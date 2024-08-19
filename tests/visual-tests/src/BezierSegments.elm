module BezierSegments exposing (main)

import Bezier
import Bezier.Spring
import Browser
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Svg
import Svg.Attributes as SvgA exposing (..)


type alias Model =
    { startPosition : Float
    , startVelocity : Float
    , endPosition : Float
    , wobble : Float
    , quickness : Float
    , settleMax : Float
    }


main =
    Browser.sandbox
        { init =
            { startPosition = 0
            , startVelocity = 0
            , endPosition = 1000
            , wobble = 0.5
            , quickness = 0
            , settleMax = 1000
            }
        , update = update
        , view = view
        }


type Msg
    = StartUpdated Float
    | StartVelocityUpdated Float
    | EndUpdated Float
    | WobbleUpdated Float
    | QuicknessUpdated Float
    | SettleMaxUpdated Float
    | SpringUpdated Model


update msg model =
    case msg of
        StartUpdated newStart ->
            { model | startPosition = newStart }

        StartVelocityUpdated newStartVelocity ->
            { model | startVelocity = newStartVelocity }

        EndUpdated newEnd ->
            { model | endPosition = newEnd }

        WobbleUpdated newWobble ->
            { model | wobble = newWobble }

        QuicknessUpdated newQuickness ->
            { model | quickness = newQuickness }

        SettleMaxUpdated newSettleMax ->
            { model | settleMax = newSettleMax }

        SpringUpdated newModel ->
            newModel


isCriticallyDamped : Bezier.Spring.Parameters -> Bool
isCriticallyDamped { stiffness, damping, mass } =
    let
        cCritical =
            criticalDamping stiffness mass
    in
    round damping == round cCritical


isOverDamped : Bezier.Spring.Parameters -> Bool
isOverDamped spring =
    spring.damping > criticalDamping spring.stiffness spring.mass


criticalDamping : Float -> Float -> Float
criticalDamping k m =
    2 * sqrt (k * m)


boolToString : Bool -> String
boolToString bool =
    if bool then
        "Yes"

    else
        "No"


row : List (Attribute msg) -> List (Html msg) -> Html msg
row attrs children =
    div [ Attr.style "display" "flex", Attr.style "flex-direction" "row", Attr.style "gap" "10px" ] children


inlineLabel : String -> Html msg
inlineLabel str =
    div [ Attr.style "width" "150px" ] [ text str ]


needsDirectPathing : Bezier.Spring.Parameters -> Bool
needsDirectPathing spring =
    (criticalDamping spring.stiffness spring.mass - spring.damping) < 12



-- row : List (Attribute msg) -> List (Html msg) -> Html msg
-- row attrs children =
--     div
--         [ Attr.style "display" "flex"
--         , Attr.style "flex-direction" "row"
--         , Attr.style "gap" "10px"
--         ]
--         children


toDampingRatio : Bezier.Spring.Parameters -> Float
toDampingRatio { stiffness, damping, mass } =
    damping / (2 * sqrt (stiffness * mass))


view model =
    let
        params =
            Bezier.Spring.new
                { wobble = model.wobble
                , quickness = model.quickness
                , settleMax = model.settleMax
                }
    in
    div [ Attr.style "padding" "40px" ]
        [ h1 [] [ text "Spring Playground" ]
        , row [ Attr.style "gap" "80px" ]
            [ div
                [ Attr.style "width" "350px"
                , Attr.style "position" "relative"
                , Attr.style "flex-shrink" "0"
                ]
                [ viewSlider "Start Position" StartUpdated model.startPosition { min = -1000, max = 1000, step = 10 }
                , viewSlider "Start Velocity" StartVelocityUpdated model.startVelocity { min = -10000, max = 10000, step = 10 }
                , viewSlider "End Position" EndUpdated model.endPosition { min = -1000, max = 1000, step = 10 }
                , viewSlider "Wobble" WobbleUpdated model.wobble { min = 0, max = 1, step = 0.01 }
                , viewSlider "Quickness" QuicknessUpdated model.quickness { min = 0, max = 1, step = 0.01 }
                , viewSlider "Settle Max" SettleMaxUpdated model.settleMax { min = 0, max = 1000, step = 10 }
                , div []
                    [ row [] [ inlineLabel "Stiffness: ", div [] [ text (String.fromFloat params.stiffness) ] ]
                    , row [] [ inlineLabel "Damping: ", div [] [ text (String.fromFloat params.damping) ] ]
                    , row [] [ inlineLabel "Mass: ", div [] [ text (String.fromFloat params.mass) ] ]
                    , row [] [ inlineLabel "Ratio: ", div [] [ text (String.fromFloat (toDampingRatio params)) ] ]
                    , row [] [ inlineLabel "Settle time: ", div [] [ text (String.fromFloat (Bezier.Spring.settlesAt params)) ] ]
                    , row [] [ inlineLabel "Critically Damped: ", div [] [ text (boolToString (isCriticallyDamped params)) ] ]
                    , row [] [ inlineLabel "Overdamped : ", div [] [ text (boolToString (isOverDamped params)) ] ]
                    , row [] [ inlineLabel "Direct Path : ", div [] [ text (boolToString (needsDirectPathing params)) ] ]
                    ]
                ]
            , Svg.svg
                [ SvgA.width "1400px"
                , SvgA.height "800px"
                , SvgA.viewBox "0 -1500 1500 3000"
                , SvgA.style "border: 4px dashed #eee;"
                ]
                [ --viewHorizontalBars
                  -- ,
                  viewSpringIndividualPoints model params
                , viewSegments model params
                , if needsDirectPathing params then
                    Html.text ""

                  else
                    viewPeaks model params
                , let
                    endsAt =
                        Bezier.Spring.settlesAt params
                  in
                  solidLine { color = "black", stroke = 3 }
                    { x = endsAt
                    , y = 0
                    }
                    { x = endsAt
                    , y = 1000
                    }

                -- Full wobble
                , Svg.line
                    [ SvgA.x1 "0"
                    , SvgA.y1 "0"
                    , SvgA.x2 "1000"
                    , SvgA.y2 "0"
                    , SvgA.stroke "black"
                    , SvgA.strokeWidth "3"
                    ]
                    []
                ]
            ]
        ]


viewSpline : { dashed : Bool, color : String } -> Bezier.Spline -> Svg.Svg msg
viewSpline style spline =
    let
        { one, two, three, four } =
            Bezier.toPoints spline
    in
    Svg.g []
        [ Svg.path
            [ d (Bezier.toPath spline)
            , fill "none"
            , stroke style.color
            , strokeWidth "5"
            , if style.dashed then
                strokeDasharray "20,20"

              else
                strokeDasharray "none"
            ]
            []
        , line { color = "black" } one two
        , line { color = "black" } two three
        , line { color = "black" } three four
        , Svg.circle
            [ cx (String.fromFloat one.x)
            , cy (String.fromFloat one.y)
            , r "5"
            , fill "red"
            , stroke "none"
            ]
            []
        , Svg.circle
            [ cx (String.fromFloat four.x)
            , cy (String.fromFloat four.y)
            , r "5"
            , fill "red"
            , stroke "none"
            ]
            []
        , Svg.circle
            [ cx (String.fromFloat two.x)
            , cy (String.fromFloat two.y)
            , r "10"
            , fill "red"
            , stroke "black"
            , strokeWidth "2"
            , strokeDasharray "5,5"
            ]
            []
        , Svg.circle
            [ cx (String.fromFloat three.x)
            , cy (String.fromFloat three.y)
            , r "10"
            , fill "red"
            , stroke "black"
            , strokeWidth "2"
            , strokeDasharray "5,5"
            ]
            []
        ]


solidLine : { color : String, stroke : Int } -> Bezier.Point -> Bezier.Point -> Svg.Svg msg
solidLine style one two =
    Svg.line
        [ SvgA.x1 (String.fromFloat one.x)
        , SvgA.y1 (String.fromFloat one.y)
        , SvgA.x2 (String.fromFloat two.x)
        , SvgA.y2 (String.fromFloat two.y)
        , SvgA.stroke style.color
        , SvgA.strokeWidth (String.fromInt style.stroke)
        ]
        []


line : { color : String } -> Bezier.Point -> Bezier.Point -> Svg.Svg msg
line style one two =
    Svg.line
        [ SvgA.x1 (String.fromFloat one.x)
        , SvgA.y1 (String.fromFloat one.y)
        , SvgA.x2 (String.fromFloat two.x)
        , SvgA.y2 (String.fromFloat two.y)
        , SvgA.stroke style.color
        , SvgA.strokeWidth "3"
        , SvgA.strokeDasharray "5,5"
        ]
        []



{- Welcome to the Spring Playground!

   The main goal is to efficiently render a spring system into a List CubicBezier

   We have the constraint of always working on underdamped springs (meaning they will always settle).


   A secondary goal(though this one isnt nearly as critical as the first), is can we calculate the position and velocity of a spring without having to use fold?



   Basically, can we implement this function:

       stepOver :
           Milliseconds
           -> Parameters
           -> Float
           ->
               { velocity : Float
               , position : Float
               }
           ->
               { velocity : Float
               , position : Float
               }

   without having to fold over a bunch of tiny steps to find the result

-}


{-| Create a basic spring that is half-wobbly, and settles in 1000ms
-}
basic : Bezier.Spring.Parameters
basic =
    Bezier.Spring.new
        { wobble = 0.5
        , quickness = 0
        , settleMax = 1000
        }


full : Bezier.Spring.Parameters
full =
    Bezier.Spring.new
        { wobble = 1
        , quickness = 0
        , settleMax = 1000
        }


null : Bezier.Spring.Parameters
null =
    Bezier.Spring.new
        { wobble = 0
        , quickness = 0
        , settleMax = 1000
        }


noWobble : Bezier.Spring.Parameters
noWobble =
    { stiffness = 170
    , damping = 26
    , mass = 1
    }


gentle : Bezier.Spring.Parameters
gentle =
    { stiffness = 120
    , damping = 14
    , mass = 1
    }


wobbly : Bezier.Spring.Parameters
wobbly =
    { stiffness = 180
    , damping = 12
    , mass = 1
    }


stiff : Bezier.Spring.Parameters
stiff =
    { stiffness = 210
    , damping = 20
    , mass = 1
    }



-- export default {
--   noWobble: {stiffness: 170, damping: 26}, // the default, if nothing provided
--   gentle: {stiffness: 120, damping: 14},
--   wobbly: {stiffness: 180, damping: 12},
--   stiff: {stiffness: 210, damping: 20},
-- };


viewSlider : String -> (Float -> msg) -> Float -> { min : Float, max : Float, step : Float } -> Html.Html msg
viewSlider label tagger value options =
    div [ Attr.style "display" "flex", Attr.style "flex-direction" "row", Attr.style "gap" "10px" ]
        [ div [ Attr.style "width" "120px", Attr.style "position" "relative" ] [ text label ]
        , input
            [ Attr.type_ "range"
            , Attr.min (String.fromFloat options.min)
            , Attr.max (String.fromFloat options.max)
            , Attr.step (String.fromFloat options.step)
            , Attr.value (String.fromFloat value)
            , Events.onInput (String.toFloat >> Maybe.withDefault 0 >> tagger)
            ]
            []
        , text (String.fromFloat value)
        ]


{-| Create a basic spring that is half-wobbly, and settles in 1000ms
-}
basic2 : Bezier.Spring.Parameters
basic2 =
    Bezier.Spring.new
        { wobble = 0.5
        , quickness = 1
        , settleMax = 1000
        }


full2 : Bezier.Spring.Parameters
full2 =
    Bezier.Spring.new
        { wobble = 1
        , quickness = 1
        , settleMax = 1000
        }


null2 : Bezier.Spring.Parameters
null2 =
    Bezier.Spring.new
        { wobble = 0
        , quickness = 1
        , settleMax = 1000
        }


standard : { x : Float, y : Float } -> Bezier.Spline
standard { x, y } =
    Bezier.fromPoints
        { x = 0
        , y = 0
        }
        { x = 0.4 * x
        , y = 0
        }
        { x = 0.2 * x
        , y = y
        }
        { x = x
        , y = y
        }


viewHorizontalBars : Svg.Svg msg
viewHorizontalBars =
    Svg.g []
        (List.range 0 5
            |> List.map ((*) 200)
            |> List.map
                (\x ->
                    line { color = "black" }
                        { x = toFloat x
                        , y = 0
                        }
                        { x = toFloat x
                        , y = 1000
                        }
                )
        )


viewPeaks : Model -> Bezier.Spring.Parameters -> Svg.Svg msg
viewPeaks model params =
    let
        peaks =
            Bezier.Spring.peaks params
                model.startPosition
                model.endPosition
                (toInitial model)
    in
    Svg.g []
        (peaks
            |> List.map
                (\peakAtX ->
                    line { color = "red" }
                        { x = peakAtX
                        , y = -100
                        }
                        { x = peakAtX
                        , y = 1200
                        }
                )
        )


viewSegments : Model -> Bezier.Spring.Parameters -> Svg.Svg msg
viewSegments model params =
    let
        segments =
            Bezier.Spring.segments params
                (toInitial model)
                model.endPosition
    in
    Svg.g [ SvgA.class "segments" ]
        [ Svg.g []
            (List.indexedMap
                (\index spline ->
                    let
                        style =
                            if remainderBy 2 index == 0 then
                                { dashed = False, color = "blue" }

                            else
                                { dashed = True, color = "black" }
                    in
                    viewSpline style
                        spline
                )
                segments
            )
        , if needsDirectPathing params then
            let
                top =
                    -400

                bottom =
                    1400
            in
            Svg.g []
                (List.indexedMap
                    (\index spline ->
                        let
                            { one, two, three, four } =
                                Bezier.toPoints spline
                        in
                        if index == 0 then
                            [ solidLine { color = "black", stroke = 1 }
                                { x = one.x
                                , y = top
                                }
                                { x = one.x
                                , y = bottom
                                }
                            , solidLine { color = "black", stroke = 1 }
                                { x = four.x
                                , y = top
                                }
                                { x = four.x
                                , y = bottom
                                }
                            ]

                        else
                            [ solidLine { color = "black", stroke = 1 }
                                { x = four.x
                                , y = top
                                }
                                { x = four.x
                                , y = bottom
                                }
                            ]
                    )
                    segments
                    |> List.concat
                )

          else
            Html.text ""
        ]


toInitial : Model -> { velocity : Float, position : Float }
toInitial model =
    { velocity = model.startVelocity
    , position = model.startPosition
    }


viewSpringIndividualPoints : Model -> Bezier.Spring.Parameters -> Svg.Svg msg
viewSpringIndividualPoints model spring =
    Svg.g []
        (List.range 0 100
            |> List.map (\t -> t * 10)
            |> List.map
                (\t ->
                    let
                        stepped =
                            Bezier.Spring.stepOver
                                { spring = spring
                                , target = model.endPosition
                                , stepSize = 16 / 100
                                , initial = toInitial model
                                }
                                (toFloat t)

                        new =
                            Bezier.Spring.at
                                { spring = spring
                                , target = model.endPosition
                                , initial = toInitial model
                                }
                                (toFloat t)

                        -- _ =
                        --     Debug.log "at" ( t, tAsTime, new )
                    in
                    Svg.g []
                        [ Svg.circle
                            [ SvgA.cx (String.fromInt t)
                            , SvgA.cy
                                (new.position
                                    |> String.fromFloat
                                )
                            , SvgA.r "10"
                            , SvgA.fill "black"
                            , SvgA.opacity "1"
                            ]
                            []
                        , Svg.circle
                            [ SvgA.cx (String.fromInt t)
                            , SvgA.cy
                                (stepped.position
                                    |> String.fromFloat
                                )
                            , SvgA.r "15"
                            , SvgA.stroke "red"
                            , SvgA.fill "none"
                            , SvgA.strokeWidth "1"
                            ]
                            []

                        -- , Svg.circle
                        --     [ SvgA.cx (String.fromInt t)
                        --     , SvgA.cy
                        --         ((new.velocity / 10)
                        --             |> String.fromFloat
                        --         )
                        --     , SvgA.r "20"
                        --     , SvgA.fill "blue"
                        --     , SvgA.opacity "0.2"
                        --     ]
                        --     []
                        ]
                 -- []
                )
        )


dot point =
    Svg.circle
        [ SvgA.cx (String.fromFloat point.x)
        , SvgA.cy
            (point.y
                |> String.fromFloat
            )
        , SvgA.r "24"
        , SvgA.fill "red"
        ]
        []



{- SPRING INTERPOLATION from ELM ANIMATOR -}
{- I kept the comments in here though they might be overkill.

   Main insights:

      - We only deal with underdamped springs.
      - We do this by only defining springs by "wobbliness and duration".
      - Wobble is essentally a damping ratio, but clamped to a certain range of values.
      - Settling time of a spring can be pretty easily be calculated for a spring if you know it's underdamped.
      - Stiffness and mass are constants here as they don't meaningfully affect the personality of the spring's movement.

-}
