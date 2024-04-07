module Playground exposing (main)

import Bezier
import Bezier.Spring
import Html exposing (..)
import Svg
import Svg.Attributes as SvgA exposing (..)


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
    Bezier.Spring.select
        { wobble = 0.5
        , stiffness = 0
        }
        1000


full : Bezier.Spring.Parameters
full =
    Bezier.Spring.select
        { wobble = 1
        , stiffness = 0
        }
        1000


null : Bezier.Spring.Parameters
null =
    Bezier.Spring.select
        { wobble = 0
        , stiffness = 0
        }
        1000


{-| Create a basic spring that is half-wobbly, and settles in 1000ms
-}
basic2 : Bezier.Spring.Parameters
basic2 =
    Bezier.Spring.select
        { wobble = 0.5
        , stiffness = 1
        }
        1000


full2 : Bezier.Spring.Parameters
full2 =
    Bezier.Spring.select
        { wobble = 1
        , stiffness = 1
        }
        1000


null2 : Bezier.Spring.Parameters
null2 =
    Bezier.Spring.select
        { wobble = 0
        , stiffness = 1
        }
        1000


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


criticalDamping : Bezier.Spring.Parameters -> Float
criticalDamping params =
    Bezier.Spring.criticalDamping params.stiffness params.mass


status : Bezier.Spring.Parameters -> Status
status params =
    let
        crit =
            Bezier.Spring.criticalDamping params.stiffness params.mass
    in
    if params.damping < crit then
        UnderDamped crit

    else if params.damping == crit then
        Critical

    else
        OverDamped crit


type Status
    = UnderDamped Float
    | Critical
    | OverDamped Float


initial =
    { velocity = 0
    , position = 0
    }


main : Html msg
main =
    let
        _ =
            Debug.log "basic" ( basic, status basic )

        _ =
            Debug.log "full" ( full, status full )

        _ =
            Debug.log "null" ( null, status null )

        _ =
            Debug.log "basic2" ( basic2, status basic2 )

        _ =
            Debug.log "full2" ( full2, status full2 )

        _ =
            Debug.log "null2" ( null2, status null2 )
    in
    div []
        [ h1 [] [ text "Spring Playground" ]
        , Svg.svg
            [ SvgA.width "1400px"
            , SvgA.height "800px"
            , SvgA.viewBox "0 -300 1500 1600"
            , SvgA.style "border: 4px dashed #eee;"
            ]
            [ --viewHorizontalBars
              -- ,
              --   viewSpring basic
              -- , viewSegments basic
              -- , viewPeaks basic
              -- , viewZeros basic
              --
              viewSpring full
            , viewSegments full
            , viewPeaks full
            , viewZeros full

            -- --
            -- , viewSpring null
            -- , viewSegments null
            -- , viewPeaks null
            -- , viewZeros null
            , viewSpline { color = "green", dashed = False }
                (standard
                    { y = 1000
                    , x = 1000
                    }
                )
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
        , Svg.svg
            [ SvgA.width "1400px"
            , SvgA.height "800px"
            , SvgA.viewBox "0 -300 1500 1600"
            , SvgA.style "border: 4px dashed #eee;"
            ]
            [ --viewHorizontalBars
              -- ,
              viewSpring basic2
            , viewSegments basic2

            -- , viewPeaks basic
            , viewSpring full2
            , viewSegments full2

            -- , viewPeaks full
            , viewSpring null2
            , viewSegments null2

            -- , viewPeaks null
            --
            , viewSpline { color = "green", dashed = False }
                (standard
                    { y = 1000
                    , x = 1000
                    }
                )
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
        , Html.h1 [] [ Html.text "Splitting Curves" ]
        , let
            spline =
                standard
                    { y = 1000
                    , x = 1000
                    }

            ( before, after ) =
                Bezier.splitAt 0.5 spline
          in
          Svg.svg
            [ SvgA.width "1400px"
            , SvgA.height "800px"
            , SvgA.viewBox "0 -300 1500 1600"
            , SvgA.style "border: 4px dashed #eee;"
            ]
            [ viewSpline { color = "#f5f5f5", dashed = False }
                spline
            , viewSpline { color = "red", dashed = True }
                before
            , viewSpline { color = "black", dashed = True }
                after
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
        , Html.h1 [] [ Html.text "Splitting Curves (AT X)" ]
        , let
            spline =
                standard
                    { y = 1000
                    , x = 1000
                    }

            ( before, after ) =
                Bezier.splitAtX 500 spline
          in
          Svg.svg
            [ SvgA.width "1400px"
            , SvgA.height "800px"
            , SvgA.viewBox "0 -300 1500 1600"
            , SvgA.style "border: 4px dashed #eee;"
            ]
            [ viewSpline { color = "#f5f5f5", dashed = False }
                spline
            , viewSpline { color = "red", dashed = True }
                before
            , viewSpline { color = "black", dashed = True }
                after
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
        , Html.h1 [] [ Html.text "Splitting Curves (0.8)" ]
        , let
            spline =
                standard
                    { y = 1000
                    , x = 1000
                    }

            ( before, after ) =
                Bezier.splitAt 0.8 spline
          in
          Svg.svg
            [ SvgA.width "1400px"
            , SvgA.height "800px"
            , SvgA.viewBox "0 -300 1500 1600"
            , SvgA.style "border: 4px dashed #eee;"
            ]
            [ viewSpline { color = "#f5f5f5", dashed = False }
                spline
            , viewSpline { color = "red", dashed = True }
                before
            , viewSpline { color = "black", dashed = True }
                after
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


viewPeaks : Bezier.Spring.Parameters -> Svg.Svg msg
viewPeaks params =
    let
        peaks =
            Bezier.Spring.peaks params
                0
                1000
                initial
    in
    Svg.g []
        (peaks
            |> List.map
                (\peakAtX ->
                    line { color = "red" }
                        { x = peakAtX
                        , y = 0
                        }
                        { x = peakAtX
                        , y = 1000
                        }
                )
        )


viewZeros : Bezier.Spring.Parameters -> Svg.Svg msg
viewZeros params =
    let
        zeroPoints =
            Bezier.Spring.zeroPoints params
                0
                1000
                initial
    in
    Svg.g []
        (zeroPoints
            |> List.map
                (\peakAtX ->
                    line { color = "blue" }
                        { x = peakAtX
                        , y = 0
                        }
                        { x = peakAtX
                        , y = 1000
                        }
                )
        )


viewSegments : Bezier.Spring.Parameters -> Svg.Svg msg
viewSegments params =
    let
        segments =
            Bezier.Spring.segments params
                initial
                1000
    in
    Svg.g []
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


viewSpring : Bezier.Spring.Parameters -> Svg.Svg msg
viewSpring spring =
    Svg.g []
        (List.range 0 100
            |> List.map ((*) 10)
            |> List.map
                (\t ->
                    let
                        stepped =
                            Bezier.Spring.stepOver
                                { spring = spring
                                , target = 1000
                                , stepSize = 16
                                , initial = initial
                                }
                                (toFloat t)

                        new =
                            Bezier.Spring.toPosition
                                { spring = spring
                                , target = 1000
                                , initial = initial
                                }
                                (toFloat t)
                    in
                    Svg.g []
                        [ Svg.circle
                            [ SvgA.cx (String.fromInt t)
                            , SvgA.cy
                                (new.position
                                    |> String.fromFloat
                                )
                            , SvgA.r "12"
                            , SvgA.fill "red"
                            , SvgA.opacity "0.5"
                            ]
                            []
                        , Svg.circle
                            [ SvgA.cx (String.fromInt t)
                            , SvgA.cy
                                (stepped.position
                                    |> String.fromFloat
                                )
                            , SvgA.r "5"
                            , SvgA.fill "blue"
                            , SvgA.opacity "0.5"
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
