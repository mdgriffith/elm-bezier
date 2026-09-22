module SpringTest exposing (tests)

import Bezier
import Bezier.Spring as Spring
import Expect
import Test exposing (Test, describe, test)


tests : Test
tests =
    describe "Spring regressions"
        [ describe "segments use time, independently of the target position"
            (List.map
                (\( name, spring ) ->
                    describe name
                        (List.map
                            (\target ->
                                test ("target " ++ String.fromFloat target) <|
                                    \_ ->
                                        let
                                            curves =
                                                Spring.segments spring { position = 25, velocity = 40 } target

                                            end =
                                                Spring.settlesAt spring
                                        in
                                        case ( List.head curves, List.head (List.reverse curves) ) of
                                            ( Just first, Just last ) ->
                                                Expect.all
                                                    [ \_ -> Expect.within (Expect.Absolute 0.000001) 0 (Bezier.first first).x
                                                    , \_ -> Expect.within (Expect.Absolute 0.000001) end (Bezier.last last).x
                                                    , \_ ->
                                                        curves
                                                            |> List.all (\curve -> (Bezier.first curve).x < (Bezier.last curve).x)
                                                            |> Expect.equal True
                                                    ]
                                                    ()

                                            _ ->
                                                Expect.fail "Expected a nonempty spring trace"
                            )
                            [ -100, 0, 1, 100, 10000 ]
                        )
                )
                springs
            )
        , describe "at preserves the supplied initial state"
            (List.map
                (\( name, spring ) ->
                    test name <|
                        \_ ->
                            Spring.at { spring = spring, target = 100, initial = { position = 25, velocity = -40 } } 0
                                |> expectMotion 0.000001 { position = 25, velocity = -40 }
                )
                springs
            )
        , describe "underdamped velocity is the derivative of position (units per second)"
            (List.concatMap
                (\velocity ->
                    List.map
                        (\time ->
                            test (String.fromFloat velocity ++ " units/s at " ++ String.fromFloat time ++ "ms") <|
                                \_ ->
                                    let
                                        at =
                                            Spring.at { spring = Spring.gentle, target = 100, initial = { position = 25, velocity = velocity } }

                                        derivative =
                                            ((at (time + 0.001)).position - (at (time - 0.001)).position) / 0.000002
                                    in
                                    Expect.within (Expect.Absolute 0.00001) derivative (at time).velocity
                        )
                        [ 10, 100, 300 ]
                )
                [ -200, 0, 200 ]
            )
        , test "underdamped motion agrees with fine numerical integration" <|
            \_ ->
                let
                    options =
                        { spring = Spring.gentle, target = 100, initial = { position = 25, velocity = -200 } }

                    reference =
                        Spring.stepOver
                            { spring = options.spring, target = options.target, initial = options.initial, stepSize = 0.001 }
                            200
                in
                Spring.at options 200 |> expectMotion 0.01 reference
        , test "restarting an underdamped spring preserves its future motion" <|
            \_ ->
                let
                    options =
                        { spring = Spring.gentle, target = 100, initial = { position = 25, velocity = -200 } }

                    midway =
                        Spring.at options 100
                in
                Spring.at { options | initial = midway } 200
                    |> expectMotion 0.000001 (Spring.at options 300)
        ]


springs : List ( String, Spring.Parameters )
springs =
    [ ( "underdamped", Spring.gentle )
    , ( "near critical preset", Spring.noWobble )
    , ( "critical", { stiffness = 100, damping = 20, mass = 1 } )
    , ( "overdamped", { stiffness = 100, damping = 30, mass = 1 } )
    ]


expectMotion : Float -> { position : Float, velocity : Float } -> { position : Float, velocity : Float } -> Expect.Expectation
expectMotion tolerance expected actual =
    Expect.all
        [ \_ -> Expect.within (Expect.Absolute tolerance) expected.position actual.position
        , \_ -> Expect.within (Expect.Absolute tolerance) expected.velocity actual.velocity
        ]
        ()
