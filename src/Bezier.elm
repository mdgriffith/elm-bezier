module Bezier exposing
    ( fromPoints, Spline, Point, toPoints, standard
    , fromCatmullRom, toCatmullRom
    , atX, pointOn
    , first, controlOne, controlTwo, last
    , firstDerivative, secondDerivative
    , normalize, trace
    , splitAt, splitAtX, splitList
    , takeBefore, takeAfter
    , scale, translateBy, withVelocities
    , toPath, toCss
    )

{-| Bézier (pronounced bez-E-ey) curves are super cool and commonly used in animation!

[Here is an excellent video on them](https://www.youtube.com/watch?v=aVwxzDHniEw).

This module borrows a lot of code from [Elm Geometry](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry), but is much more focused on animation needs for [Elm Animator](https://package.elm-lang.org/packages/mdgriffith/elm-animator).

@docs fromPoints, Spline, Point, toPoints, standard

@docs fromCatmullRom, toCatmullRom

@docs atX, pointOn

@docs first, controlOne, controlTwo, last

@docs firstDerivative, secondDerivative

@docs normalize, trace


## Splitting

@docs splitAt, splitAtX, splitList

@docs takeBefore, takeAfter


## Modification

@docs scale, translateBy, withVelocities


## Serialization

@docs toPath, toCss

-}


{-| Number betwen 0 and 1
-}
type alias Proportion =
    Float


{-| A very common Bézier curve that goes from (0,0) to (1,1) with control points at (0.4,0) and (0.2,1).

This is the standard transition curve for a lot of animation systems.

-}
standard : Spline
standard =
    fromPoints
        { x = 0
        , y = 0
        }
        { x = 0.4
        , y = 0
        }
        { x = 0.2
        , y = 1
        }
        { x = 1
        , y = 1
        }


{-| -}
toPoints :
    Spline
    ->
        { one : Point
        , two : Point
        , three : Point
        , four : Point
        }
toPoints (Spline one two three four) =
    { one = one
    , two = two
    , three = three
    , four = four
    }


{-| _Note_ This only approximates a Catmull-Rom spline, it won't successsfully round trip with `toCatmullRom` because the control points are lost.
-}
toCatmullRom :
    Spline
    ->
        { one : Point
        , two : Point
        , three : Point
        , four : Point
        }
toCatmullRom (Spline b0 b1 b2 b3) =
    let
        p0 =
            { x = b0.x - (b1.x - b0.x) * 6
            , y = b0.y - (b1.y - b0.y) * 6
            }

        p1 =
            b0

        p2 =
            b3

        p3 =
            { x = b3.x + (b3.x - b2.x) * 6
            , y = b3.y + (b3.y - b2.y) * 6
            }
    in
    { one = p0, two = p1, three = p2, four = p3 }


{-| -}
fromCatmullRom : Point -> Point -> Point -> Point -> Spline
fromCatmullRom p0 p1 p2 p3 =
    let
        b0 =
            p1

        b1 =
            { x = p1.x + (p2.x - p0.x) / 6
            , y = p1.y + (p2.y - p0.y) / 6
            }

        b2 =
            { x = p2.x - (p3.x - p1.x) / 6
            , y = p2.y - (p3.y - p1.y) / 6
            }

        b3 =
            p2
    in
    Spline b0 b1 b2 b3


{-| -}
fromPoints : Point -> Point -> Point -> Point -> Spline
fromPoints =
    Spline


{-| -}
type Spline
    = Spline Point Point Point Point


{-| -}
type alias Point =
    { x : Float
    , y : Float
    }


{-| Render a spline to a string that can be used in SVG

e.g. (M100,250 C100,100 400,100 400,250)

This uses absolute coordinates.

-}
toPath : Spline -> String
toPath (Spline one two three four) =
    String.join " "
        [ "M" ++ pathPoint one
        , "C" ++ pathPoint two
        , pathPoint three
        , pathPoint four
        ]


pathPoint : Point -> String
pathPoint point =
    String.fromFloat point.x
        ++ ("," ++ String.fromFloat point.y)


{-| Normalize and render to a string that can be used in CSS

e.g. cubic-bezier(0.1, 0.7, 1.0, 0.1)

-}
toCss : Spline -> String
toCss spline =
    let
        (Spline c0 c1 c2 c3) =
            normalize spline
    in
    "cubic-bezier("
        ++ (String.fromFloat (roundFloat c1.x) ++ comma)
        ++ (String.fromFloat (roundFloat c1.y) ++ comma)
        ++ (String.fromFloat (roundFloat c2.x) ++ comma)
        ++ String.fromFloat (roundFloat c2.y)
        ++ ")"


roundFloat : Float -> Float
roundFloat f =
    toFloat (round (f * 100)) / 100


comma : String
comma =
    ","


dash : String
dash =
    "-"


{-| -}
first : Spline -> Point
first (Spline f _ _ _) =
    f


{-| -}
controlOne : Spline -> Point
controlOne (Spline _ c _ _) =
    c


{-| -}
controlTwo : Spline -> Point
controlTwo (Spline _ _ c _) =
    c


{-| -}
last : Spline -> Point
last (Spline _ _ _ l) =
    l


{-| -}
scale : Float -> Point -> Point
scale n { x, y } =
    { x = x * n
    , y = y * n
    }


{-| -}
translateBy : Point -> Point -> Point
translateBy delta { x, y } =
    { x = x + delta.x
    , y = y + delta.y
    }


interpolatePoints : Point -> Point -> Float -> Point
interpolatePoints p1 p2 t =
    if t <= 0.5 then
        { x = p1.x + t * (p2.x - p1.x)
        , y = p1.y + t * (p2.y - p1.y)
        }

    else
        { x = p2.x + (1 - t) * (p1.x - p2.x)
        , y = p2.y + (1 - t) * (p1.y - p2.y)
        }


interpolateValue : Float -> Float -> Float -> Float
interpolateValue start end t =
    if t <= 0.5 then
        start + t * (end - start)

    else
        end + (1 - t) * (start - end)


{-| Given a spline and a proportion, return the point on the spline at that proportion.
-}
pointOn : Spline -> Proportion -> Point
pointOn ((Spline p1 p2 p3 p4) as s) proportion =
    -- Borrowed from: <https://github.com/ianmackenzie/elm-geometry/blob/3.1.0/src/CubicSpline2d.elm#L370>
    let
        q1 =
            interpolatePoints p1 p2 proportion

        q2 =
            interpolatePoints p2 p3 proportion

        q3 =
            interpolatePoints p3 p4 proportion

        r1 =
            interpolatePoints q1 q2 proportion

        r2 =
            interpolatePoints q2 q3 proportion
    in
    interpolatePoints r1 r2 proportion


{-| -}
firstDerivative : Spline -> Proportion -> Point
firstDerivative (Spline p1 p2 p3 p4) proportion =
    -- Borrowed from: <https://github.com/ianmackenzie/elm-geometry/blob/3.1.0/src/CubicSpline2d.elm#L778>
    let
        vx1 =
            p2.x - p1.x

        vy1 =
            p2.y - p1.y

        vx2 =
            p3.x - p2.x

        vy2 =
            p3.y - p2.y

        vx3 =
            p4.x - p3.x

        vy3 =
            p4.y - p3.y

        wx1 =
            interpolateValue vx1 vx2 proportion

        wy1 =
            interpolateValue vy1 vy2 proportion

        wx2 =
            interpolateValue vx2 vx3 proportion

        wy2 =
            interpolateValue vy2 vy3 proportion
    in
    { x =
        3 * interpolateValue wx1 wx2 proportion
    , y =
        3 * interpolateValue wy1 wy2 proportion
    }


{-| -}
secondDerivative : Spline -> Proportion -> Point
secondDerivative (Spline p1 p2 p3 p4) proportion =
    -- Borrowed from: <https://github.com/ianmackenzie/elm-geometry/blob/3.1.0/src/CubicSpline2d.elm#L858>
    let
        u1 =
            { x = p2.x - p1.x
            , y = p2.y - p1.y
            }

        u2 =
            { x = p3.x - p2.x
            , y = p3.y - p2.y
            }

        u3 =
            { x = p4.x - p3.x
            , y = p4.y - p3.y
            }

        v1 =
            { x = u2.x - u1.x
            , y = u2.y - u1.y
            }

        v2 =
            { x = u3.x - u2.x
            , y = u3.y - u2.y
            }
    in
    scale 6 (interpolatePoints v1 v2 proportion)


{-| -}
atX :
    Float
    -> Spline
    ->
        { point : { x : Float, y : Float }
        , t : Float
        }
atX x spline =
    atXHelper spline x 0.25 (guessTime x spline) 0


guessTime : Float -> Spline -> Float
guessTime now (Spline one two three four) =
    if (four.x - one.x) == 0 then
        0.5

    else
        (now - one.x) / (four.x - one.x)


atXTolerance : Float
atXTolerance =
    0.0005


{-| Once we have a bezier curve, we need to find the value of y at a given x.

A simple way to do this is just a binary search, which is what this does.

However we could use Newton's Method:
<https://en.wikipedia.org/wiki/Newton%27s_method>
<http://greweb.me/2012/02/bezier-curve-based-easing-functions-from-concept-to-implementation/>

OR (and I'm not 100% on this one), we could use Cardano's method:

as explained here:
<https://stackoverflow.com/questions/51879836/cubic-bezier-curves-get-y-for-given-x-special-case-where-x-of-control-points/51883347#51883347>

-}
atXHelper : Spline -> Float -> Float -> Float -> Int -> { point : { x : Float, y : Float }, t : Float }
atXHelper ((Spline p1 p2 p3 p4) as spline) desiredX jumpSize t depth =
    let
        point =
            if t <= 0.5 then
                let
                    q1 =
                        { x = p1.x + t * (p2.x - p1.x)
                        , y = p1.y + t * (p2.y - p1.y)
                        }

                    q2 =
                        { x = p2.x + t * (p3.x - p2.x)
                        , y = p2.y + t * (p3.y - p2.y)
                        }

                    q3 =
                        { x = p3.x + t * (p4.x - p3.x)
                        , y = p3.y + t * (p4.y - p3.y)
                        }

                    r1 =
                        { x = q1.x + t * (q2.x - q1.x)
                        , y = q1.y + t * (q2.y - q1.y)
                        }

                    r2 =
                        { x = q2.x + t * (q3.x - q2.x)
                        , y = q2.y + t * (q3.y - q2.y)
                        }
                in
                { x = r1.x + t * (r2.x - r1.x)
                , y = r1.y + t * (r2.y - r1.y)
                }

            else
                let
                    q1 =
                        { x = p2.x + (1 - t) * (p1.x - p2.x)
                        , y = p2.y + (1 - t) * (p1.y - p2.y)
                        }

                    q2 =
                        { x = p3.x + (1 - t) * (p2.x - p3.x)
                        , y = p3.y + (1 - t) * (p2.y - p3.y)
                        }

                    q3 =
                        { x = p4.x + (1 - t) * (p3.x - p4.x)
                        , y = p4.y + (1 - t) * (p3.y - p4.y)
                        }

                    r1 =
                        { x = q2.x + (1 - t) * (q1.x - q2.x)
                        , y = q2.y + (1 - t) * (q1.y - q2.y)
                        }

                    r2 =
                        { x = q3.x + (1 - t) * (q2.x - q3.x)
                        , y = q3.y + (1 - t) * (q2.y - q3.y)
                        }
                in
                { x = r2.x + (1 - t) * (r1.x - r2.x)
                , y = r2.y + (1 - t) * (r1.y - r2.y)
                }
    in
    if depth == 10 then
        { point = point
        , t = t
        }

    else if abs (point.x - desiredX) < atXTolerance && abs (point.x - desiredX) >= 0 then
        { point = point
        , t = t
        }

    else if (point.x - desiredX) > 0 then
        atXHelper spline desiredX (jumpSize / 2) (t - jumpSize) (depth + 1)

    else
        atXHelper spline desiredX (jumpSize / 2) (t + jumpSize) (depth + 1)


{-| Trace out a curve by sampling it at regular intervals.

Return a list of splines that approximates the curve.

-}
trace :
    { toPoint : Float -> Point
    , steps : Int
    , start : Float
    , end : Float
    }
    -> List Spline
trace options =
    if options.steps == 0 then
        []

    else
        let
            stepSize =
                (options.end - options.start) / toFloat options.steps
        in
        traceHelper options.toPoint options.steps options.start stepSize []


{-| -}
traceHelper :
    (Float -> Point)
    -> Int
    -> Float
    -> Float
    -> List Spline
    -> List Spline
traceHelper toPoint stepsRemaining t stepSize captured =
    let
        smallStepSize =
            stepSize * 0.8

        atPast =
            if t == 0 then
                toPoint (-1 * (stepSize * 0.6))

            else
                toPoint (t - smallStepSize)

        atT =
            toPoint t

        nextT =
            t + stepSize

        atNext =
            toPoint nextT

        atFuture =
            toPoint (t + stepSize + smallStepSize)
    in
    if stepsRemaining <= 0 then
        List.reverse
            captured

    else
        traceHelper toPoint
            (stepsRemaining - 1)
            nextT
            stepSize
            (fromCatmullRom
                atPast
                atT
                atNext
                atFuture
                :: captured
            )


{-| Takes a bezier and normalizes it so that it goes from (0,0) to (1,1).
-}
normalize : Spline -> Spline
normalize ((Spline c0 c1 c2 c3) as untouched) =
    if c0.x == 0 && c0.y == 0 && c3.x == 1 && c3.y == 1 then
        untouched

    else
        let
            factorX =
                c3.x - c0.x

            factorY =
                c3.y - c0.y
        in
        Spline
            { x = 0
            , y = 0
            }
            { x = (c1.x - c0.x) / factorX
            , y = (c1.y - c0.y) / factorY
            }
            { x = (c2.x - c0.x) / factorX
            , y = (c2.y - c0.y) / factorY
            }
            { x = 1
            , y = 1
            }


withinX : Float -> Spline -> Bool
withinX x (Spline p1 p2 p3 p4) =
    x >= p1.x && x <= p4.x


{-| -}
splitAtX : Float -> Spline -> ( Spline, Spline )
splitAtX x spline =
    let
        { t } =
            atX x spline
    in
    splitAt t spline


{-| Split a spline at a particular parameter value, resulting in two smaller splines.

Note, this is _not_ the intuitive version you're thinking of.

-}
splitAt : Float -> Spline -> ( Spline, Spline )
splitAt parameterValue (Spline p1 p2 p3 p4) =
    let
        q1 =
            interpolatePoints p1 p2 parameterValue

        q2 =
            interpolatePoints p2 p3 parameterValue

        q3 =
            interpolatePoints p3 p4 parameterValue

        r1 =
            interpolatePoints q1 q2 parameterValue

        r2 =
            interpolatePoints q2 q3 parameterValue

        s =
            interpolatePoints r1 r2 parameterValue
    in
    ( Spline p1 q1 r1 s
    , Spline s r2 q3 p4
    )


{-| -}
takeAfter : Float -> List Spline -> List Spline
takeAfter cutoff splines =
    takeAfterHelper cutoff splines


takeAfterHelper : Float -> List Spline -> List Spline
takeAfterHelper cutoff splines =
    case splines of
        [] ->
            []

        spline :: upcoming ->
            if withinX cutoff spline then
                let
                    ( _, after ) =
                        splitAtX cutoff spline
                in
                after :: upcoming

            else
                takeAfterHelper cutoff upcoming


{-| -}
takeBefore : Float -> List Spline -> List Spline
takeBefore cutoff splines =
    takeBeforeHelper cutoff splines []


takeBeforeHelper : Float -> List Spline -> List Spline -> List Spline
takeBeforeHelper cutoff splines captured =
    case splines of
        [] ->
            List.reverse captured

        spline :: upcoming ->
            if withinX cutoff spline then
                let
                    parameter =
                        0.5

                    ( before, _ ) =
                        splitAtX cutoff spline
                in
                List.reverse (before :: captured)

            else
                takeBeforeHelper cutoff upcoming (spline :: captured)


{-| -}
splitList :
    Float
    -> List Spline
    ->
        { before : List Spline
        , after : List Spline
        }
splitList at splines =
    splitListHelper at splines []


{-| -}
splitListHelper :
    Float
    -> List Spline
    -> List Spline
    ->
        { before : List Spline
        , after : List Spline
        }
splitListHelper at splines passed =
    case splines of
        [] ->
            { before = List.reverse passed
            , after = []
            }

        top :: remain ->
            if withinX at top then
                let
                    ( before, after ) =
                        splitAtX at top
                in
                { before = List.reverse (before :: passed)
                , after = after :: remain
                }

            else
                splitListHelper at remain (top :: passed)


{-| The math here comes the `fromEndpoints` constructor in elm-geometry

<https://github.com/ianmackenzie/elm-geometry/blob/3.9.0/src/CubicSpline2d.elm#L174>

-}
withVelocities : Float -> Float -> Spline -> Spline
withVelocities intro exit ((Spline one two three four) as full) =
    if intro == 0 && exit == 0 then
        full

    else
        let
            ctrl1 =
                if intro == 0 then
                    two

                else
                    { x = one.x + (1 / 3)
                    , y = one.y + ((1 / 3) * intro)
                    }

            ctrl2 =
                if exit == 0 then
                    three

                else
                    { x = four.x + (-1 / 3)
                    , y = four.y + ((-1 / 3) * exit)
                    }
        in
        Spline
            one
            ctrl1
            ctrl2
            four
