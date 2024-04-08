module Bezier.Spring exposing
    ( Parameters, new
    , toPosition, stepOver
    , settlesAt
    , segments
    , peaks, zeroPoints
    )

{-| Spring calculations!

This module really does 3 things.

1.  Find a spring that will settle in a certain amount of time.
2.  Calculate the position of an item at a certain time using a spring.
3.  Calculate the Bezier segments that will approximate the spring motion.

@docs Parameters, new


# Preselected Springs

@docs noWobble, gentle, wobbly, stiff

@docs toPosition, stepOver

@docs settlesAt

@docs segments

@docs peaks, zeroPoints

-}

import Bezier


{-| A duration in milliseconds.
-}
type alias Duration =
    Float


type alias Parameters =
    { stiffness : Float
    , damping : Float
    , mass : Float
    }


{-| Calculate the spring's current position and velocity given a spring, a duration, a target position, and an initial state.
-}
toPosition :
    { spring : Parameters
    , target : Float
    , initial :
        { velocity : Float
        , position : Float
        }
    }
    -> Duration
    ->
        { velocity : Float
        , position : Float
        }
toPosition { spring, target, initial } duration =
    -- Calculate position and velocity analytically instead of stepping through
    -- https://ellie-app.com/bNgBDt7GspVa1
    -- However can be more inaccurate.
    let
        c1 =
            -- offset
            target - initial.position

        t =
            duration * magicNumber / 1000

        magicNumber =
            Basics.sqrt (spring.stiffness / spring.mass)

        dampingRatio =
            spring.damping / (2 * Basics.sqrt (spring.mass * spring.stiffness))

        inner =
            Basics.sqrt
                (1 - (dampingRatio ^ 2))

        cosinElement =
            c1
                * Basics.cos (inner * t)

        c2 =
            dampingRatio * (c1 / inner)

        sinElement =
            c2 * Basics.sin (inner * t)

        newPosition =
            (Basics.e ^ ((-1 * dampingRatio) * t))
                * (cosinElement + sinElement)

        -- Calculate velocity
        attentuationFactor =
            Basics.e ^ ((-1 * dampingRatio) * t)

        firstFactor =
            ((dampingRatio * c1) - (c2 * inner))
                * (Basics.cos <| inner * t)

        secondFactor =
            ((dampingRatio * c2) + (c1 * inner))
                * (Basics.sin <| inner * t)

        newVelocity =
            attentuationFactor
                * (firstFactor + secondFactor)
    in
    { position = target - newPosition
    , velocity = newVelocity * magicNumber
    }


toCatmullSegments :
    (Float -> Bezier.Point)
    -> Float
    -> Float
    -> List Bezier.Spline
    -> List Bezier.Spline
toCatmullSegments toPoint t stepSize captured =
    let
        smallStepSize =
            stepSize * 0.8

        pastT =
            if t == 0 then
                0

            else
                t - smallStepSize

        nextT =
            t + stepSize

        futureT =
            t + stepSize + smallStepSize
    in
    if t >= 1 then
        List.reverse
            captured

    else
        toCatmullSegments toPoint
            nextT
            stepSize
            (Bezier.fromCatmullRom
                (toPoint pastT)
                (toPoint t)
                (toPoint nextT)
                (toPoint futureT)
                :: captured
            )


{-| Given a spring, a starting position and velocity, and a target position, calculate the list of Bezier segments that will approximate the spring motion.

This does assume that the spring settles. It will return a maximum of 10 segments.

-}
segments :
    Parameters
    ->
        { position : Float
        , velocity : Float
        }
    -> Float
    -> List Bezier.Spline
segments spring initialState targetPos =
    let
        pks =
            peaks spring
                0
                targetPos
                initialState

        crit =
            criticalDamping spring.stiffness spring.mass

        needsDirectPathing =
            -- If the spring is criticall damped, overdamped, or only kinda underdamped, then we need direct pathing.
            (criticalDamping spring.stiffness spring.mass - spring.damping) < 12
    in
    if needsDirectPathing then
        let
            onSpring =
                toPosition
                    { spring = spring
                    , target = targetPos
                    , initial = initialState
                    }

            toPoint factor =
                let
                    x =
                        factor * targetPos
                in
                { x = x
                , y =
                    onSpring x
                        |> .position
                }
        in
        toCatmullSegments toPoint 0 0.125 []

    else
        let
            toPos =
                toPosition
                    { spring = spring
                    , target = targetPos
                    , initial = initialState
                    }
        in
        List.map2
            (\one two ->
                let
                    posOne =
                        toPos one

                    posTwo =
                        toPos two

                    factor =
                        -- 0.06
                        0

                    spread =
                        -- -0.15
                        0

                    offsetOne =
                        (two - one)
                            -- * 0.257
                            * ((0.33 - factor) - spread)

                    ctrlOne =
                        -- + 90
                        Bezier.Point (one + offsetOne) posOne.position

                    offsetTwo =
                        (two - one)
                            -- * 0.55182845698119
                            * ((0.55 + factor) - spread)

                    ctrlTwo =
                        Bezier.Point (two - offsetTwo) posTwo.position

                    result =
                        Bezier.fromPoints
                            (Bezier.Point one posOne.position)
                            ctrlOne
                            ctrlTwo
                            (Bezier.Point two posTwo.position)
                in
                if two > targetPos then
                    -- This bezier segment is going to overshoot the target
                    result
                        |> Bezier.splitAtX targetPos
                        |> Tuple.first

                else
                    result
            )
            pks
            (List.drop 1 pks)


{-| -}
zeroPoints :
    Parameters
    -> Duration
    -> Float
    ->
        { velocity : Float
        , position : Float
        }
    -> List Float
zeroPoints spring ms target initial =
    let
        inner =
            Basics.sqrt
                (1 - (dampingRatio ^ 2))

        c1 =
            -- offset
            target - initial.position

        c2 =
            dampingRatio * (c1 / inner)

        dampingRatio =
            spring.damping / (2 * Basics.sqrt (spring.mass * spring.stiffness))

        t k =
            -1 * (1 / Basics.sqrt (1 - (dampingRatio ^ 2))) * (Basics.atan (c1 / c2) - k * pi)

        magicNumber =
            Basics.sqrt (spring.stiffness / spring.mass)
    in
    magicNumberHelper
        { magicNumber = magicNumber
        , t = t
        , target = target
        }
        0
        []


isCriticallyDamped : Parameters -> Bool
isCriticallyDamped { stiffness, damping, mass } =
    let
        cCritical =
            criticalDamping stiffness mass
    in
    round damping == round cCritical


isOverDamped : Parameters -> Bool
isOverDamped spring =
    spring.damping > criticalDamping spring.stiffness spring.mass


{-| -}
peaks :
    Parameters
    -> Duration
    -> Float
    ->
        { velocity : Float
        , position : Float
        }
    -> List Float
peaks spring ms target initial =
    if isCriticallyDamped spring then
        [ 0
        , target
        ]

    else
        let
            inner =
                Basics.sqrt
                    (1 - (dampingRatio ^ 2))

            c1 =
                -- offset
                target - initial.position

            c2 =
                dampingRatio * (c1 / inner)

            dampingRatio =
                spring.damping / (2 * Basics.sqrt (spring.mass * spring.stiffness))

            magicNumber =
                Basics.sqrt (spring.stiffness / spring.mass)

            t k =
                (-1 / inner)
                    * (Basics.atan
                        (top / bottom)
                        - k
                        * Basics.pi
                      )

            top =
                (dampingRatio * c1) - (c2 * inner)

            bottom =
                (dampingRatio * c2) + (c1 * inner)
        in
        magicNumberHelper
            { magicNumber = magicNumber
            , t = t
            , target = target
            }
            0
            []


magicNumberHelper :
    { magicNumber : Float
    , t : Float -> Float
    , target : Float
    }
    -> Float
    -> List Float
    -> List Float
magicNumberHelper options i captured =
    let
        newNum =
            options.t i * 1000 / options.magicNumber
    in
    if newNum >= options.target || i > 8 then
        -- The i > 10 check is to prevent infinite loops
        -- Most springs resolve in 4-5 iterations unless they're springing forever.
        List.reverse (newNum :: captured)

    else
        magicNumberHelper options (i + 1) (newNum :: captured)


{-| Whew, math. Exciting isn't it?

Specifically, springs.

Let's first get a bunch of equations out here so we have some facts about how things relate.

But first, definitions

    - k    -> the spring constant, also known as stiffness
    - c    -> damping factor
    - c*   -> critical damping factor
    - m    -> mass (usually 1 for us)
    - z    -> damping ratio
    - ts   -> time to settle
    - w(n) -> the natural frequency of the system

A spring is usally described as the sum of 2 forces, the spring force and the damping force.

The spring force is what causes somethig to wiggle around.
The damping is what causes the spring to finally come to rest.

    F =
        (-k * x) + (-c * v)

    w(n) =
        sqrt (k / m)

    (m * ddx) + (c * dx) + (k * x) = 0

we can calculate the settling time.

    ts = 4 / (z * w(n))

    z = c / c*

    c* = 2 * sqrt (k * m)

    z = c / 2 * sqrt (m * k)

Expanding our ts equation:

    ts =
        4 / ((c / 2 * sqrt (m * k)) * sqrt (k / m))


# Our Usecase!

So, we have a lot of equations here. What is our ideal scenario?

When interpolating a timeline and heading towards a resting state, we want to use a spring.


## Cauchy problems:

Differential equations with bounding conditions!
<https://en.wikipedia.org/wiki/Cauchy_boundary_condition>

-}
step :
    Parameters
    -> Float
    -> Float
    ->
        { velocity : Float
        , position : Float
        }
    ->
        { velocity : Float
        , position : Float
        }
step { stiffness, damping, mass } target dtms motion =
    let
        dt =
            dtms / 1000

        fspring =
            stiffness * (target - motion.position)

        fdamper =
            (-1 * damping) * motion.velocity

        a =
            (fspring + fdamper) / mass

        newVelocity =
            motion.velocity + (a * dt)

        newPos =
            motion.position + (newVelocity * dt)
    in
    { position = newPos
    , velocity = newVelocity
    }


{-| Iteratively step through a spring to get the final position and velocity.

toPosition is faster, but possibly less accurate?

-}
stepOver :
    { spring : Parameters
    , target : Float
    , stepSize : Float
    , initial :
        { velocity : Float
        , position : Float
        }
    }
    -> Float
    ->
        { velocity : Float
        , position : Float
        }
stepOver options durMs =
    stepTo options 0 durMs options.initial


stepTo :
    { spring : Parameters
    , target : Float
    , stepSize : Float
    , initial :
        { velocity : Float
        , position : Float
        }
    }
    -> Float
    -> Float
    -> { velocity : Float, position : Float }
    -> { velocity : Float, position : Float }
stepTo options t targetT current =
    let
        isLastStep =
            t + options.stepSize >= targetT

        stepSize =
            if isLastStep then
                targetT - t

            else
                options.stepSize
    in
    if stepSize == 0 then
        current

    else if isLastStep then
        step options.spring options.target stepSize current

    else
        stepTo options
            (t + stepSize)
            targetT
            (step options.spring options.target stepSize current)


toleranceForSpringSettleTimeCalculation : Float
toleranceForSpringSettleTimeCalculation =
    -- this is about 4 for 2%
    -- however, we want a smaller tolerance
    -- this is 0.5% tolerance
    -1 * logBase e 0.005


{-|

    We can detect when a spring will settle if it is underdamped (meaning it oscillates before resting)
    <https://en.wikipedia.org/wiki/Settling_time>

    However for critcally and overdamped systems it gets a lot more complicated.
    Fortunately, I'm not sure that that's an issue as I don't think we want to model overdamped spring systems for animation.
    https://electronics.stackexchange.com/questions/296567/over-and-critically-damped-systems-settling-time

-}
settlesAt : Parameters -> Float
settlesAt { stiffness, damping, mass } =
    let
        k =
            stiffness

        c =
            damping

        cCritical =
            criticalDamping k m

        m =
            mass

        springAspect =
            sqrt (k / m)
    in
    if round c == round cCritical then
        --critically damped
        -- less tolerant
        -- 1000 * (5.8335 / springAspect)
        -- more tolerant
        1000 * (8.5 / springAspect)

    else if (c - cCritical) > 0 then
        -- overdamped
        -- *NOTE* this branch is definitely not correct.
        -- this equation only applies to underdamped springs
        -- as far as I understand, this calculation for overdamped springs gets much more complicated
        -- However, I'm not sure how useful overdamped springs are in animation
        -- so we can prevent this branch by constraining the API.
        let
            dampingAspect =
                -- c / sqrt (m * k)
                c / cCritical
        in
        1000 * (toleranceForSpringSettleTimeCalculation / (dampingAspect * springAspect))

    else
        -- underdamped
        -- meaning it's wobbly
        let
            dampingAspect =
                -- c / sqrt (m * k)
                c / cCritical
        in
        1000
            * (toleranceForSpringSettleTimeCalculation
                / (dampingAspect * springAspect)
              )


{-| -}
noWobble : Parameters
noWobble =
    { stiffness = 170
    , damping = 26
    , mass = 1
    }


{-| -}
gentle : Parameters
gentle =
    { stiffness = 120
    , damping = 14
    , mass = 1
    }


{-| -}
wobbly : Parameters
wobbly =
    { stiffness = 180
    , damping = 12
    , mass = 1
    }


{-| -}
stiff : Parameters
stiff =
    { stiffness = 210
    , damping = 20
    , mass = 1
    }


{-| Normally when working with springs you have to choose a stiffness and a damping and they'll have seemingly arbitrary values like 216 or 14. This can be very hard to develop an intuition for!

Your first option is to use one of the preselected springs.

The next option could be using this function.

It takes:

  - `wobble` (0-1) - How wobbly you want the spring to be.
  - `quickness` (0-1) - How quickly you want the spring to move.
  - `settleMax` (ms) - The maximum time you want the spring to take to settle.

-}
new :
    { wobble : Float
    , quickness : Float
    , settleMax : Float
    }
    -> Parameters
new options =
    let
        -- Vary stiffness
        k =
            clampToPortion options.quickness
                |> mapToRange 120 210

        damping =
            wobble2Damping options.wobble k 1 options.settleMax

        initiallySettlesAt =
            settlesAt
                { stiffness = k
                , damping = damping
                , mass = 1
                }
    in
    if initiallySettlesAt > options.settleMax then
        -- Scale mass if it's going to take too long to settle
        -- Lol, physics
        { stiffness = k
        , damping = damping
        , mass = options.settleMax / initiallySettlesAt
        }

    else
        { stiffness = k
        , damping = damping
        , mass = 1
        }


criticalDamping : Float -> Float -> Float
criticalDamping k m =
    2 * sqrt (k * m)


{-| Wobble is essentally a damping ratio, but clamped to a certain range of values that are nice.

We take in the expected settlign time of the spring because at low settling times,
the range of the nice values changes

-}
wobble2Damping : Float -> Float -> Float -> Duration -> Float
wobble2Damping wobble k m duration =
    wobble2Ratio wobble duration * criticalDamping k m


clampToPortion : Float -> Float
clampToPortion x =
    max 0 (min 1 x)


{-| wobble:

    0 -> no wobble
    0.5 -> some wobble
    1 -> all the wobble

ratio:

    0.8 -> basically no wobble
    0.43 -> max wobble (arbitrarily chosen)


    overdampening happens when the duration is short and the wobble is low.

    if duration is below 350ms, then a high ratio can lead to overdamping, which we don't want.

    So, we linearly scale the top bound if the duration is below 350ms.

-}
wobble2Ratio : Float -> Duration -> Float
wobble2Ratio wobble msDuration =
    let
        bounded =
            clampToPortion wobble

        scalingBelowDur =
            msDuration / 350

        top =
            max 0.43 (0.9 * min 1 scalingBelowDur)
    in
    (1 - bounded)
        |> mapToRange 0.43 top


mapToRange : Float -> Float -> Float -> Float
mapToRange minimum maximum x =
    let
        total =
            maximum - minimum
    in
    minimum + (x * total)


status : Parameters -> Status
status params =
    let
        crit =
            criticalDamping params.stiffness params.mass
    in
    if params.damping < crit then
        UnderDamped (crit - params.damping)

    else if params.damping == crit then
        Critical

    else
        OverDamped (crit - params.damping)


type Status
    = UnderDamped Float
    | Critical
    | OverDamped Float
