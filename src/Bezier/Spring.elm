module Bezier.Spring exposing
    ( Parameters, new
    , noWobble, gentle, wobbly, stiff
    , at, stepOver
    , settlesAt
    , segments
    )

{-| Spring calculations!

This module really does 3 things.

1.  Find a spring that will settle in a certain amount of time.
2.  Calculate the position of an item at a certain time using a spring.
3.  Calculate the Bezier segments that will approximate the spring motion.

@docs Parameters, new


# Preselected Springs

@docs noWobble, gentle, wobbly, stiff

@docs at, stepOver

@docs settlesAt

@docs segments

-}

import Bezier


{-| A duration in milliseconds.
-}
type alias Duration =
    Float


{-| The parameters that define a spring.

Check out the presets or `new` if you want some help constructing these values.

-}
type alias Parameters =
    { stiffness : Float
    , damping : Float
    , mass : Float
    }


{-|

    Calculate the spring's current position and velocity given a spring, a duration, a target position, and an initial state.


    Duration is in milliseconds.

-}
at :
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
at { spring, target, initial } durationMs =
    let
        { stiffness, damping, mass } =
            spring

        -- Convert milliseconds to seconds
        t =
            durationMs / 1000

        omega0 =
            sqrt (stiffness / mass)

        zeta =
            damping / (2 * sqrt (mass * stiffness))

        x0 =
            initial.position - target

        v0 =
            initial.velocity

        omega1 =
            omega0 * sqrt (1 - zeta ^ 2)

        expTerm =
            e ^ (-zeta * omega0 * t)

        x0Term =
            x0
                * cos (omega1 * t)
                + ((zeta * omega0 * x0 + v0) / omega1)
                * sin (omega1 * t)
    in
    if zeta < 1 then
        -- Underdamped case
        { position =
            target
                + expTerm
                * x0Term
        , velocity =
            expTerm
                * (((v0 + zeta * omega0 * x0) * cos (omega1 * t))
                    - (omega0 / sqrt (1 - zeta ^ 2))
                    * (x0 + (zeta / omega0) * v0)
                    * sin (omega1 * t)
                  )
                - zeta
                * omega0
                * expTerm
                * x0Term
        }

    else if zeta > 1 then
        -- Overdamped case
        -- let
        --     alpha =
        --         omega0 * (zeta + sqrt (zeta ^ 2 - 1))
        --     beta =
        --         omega0 * (zeta - sqrt (zeta ^ 2 - 1))
        -- in
        -- { position =
        --     target
        --         + (alpha * x0 - v0)
        --         / (alpha - beta)
        --         * (e ^ (-beta * t))
        --         - (beta * x0 - v0)
        --         / (alpha - beta)
        --         * (e ^ (-alpha * t))
        -- , velocity =
        --     (v0 - alpha * x0)
        --         / (alpha - beta)
        --         * alpha
        --         * (e ^ (-beta * t))
        --         - (v0 - beta * x0)
        --         / (alpha - beta)
        --         * beta
        --         * (e ^ (-alpha * t))
        -- }
        -- The above math doesn't work, so we're going to use a different approach
        stepOver
            { spring = spring
            , target = target
            , initial = initial
            , stepSize = 20
            }
            durationMs

    else
        -- -- Critically damped case
        -- { position =
        --     target + (x0 + (v0 + omega0 * x0) * t) * expTerm
        -- , velocity =
        --     ((v0 + omega0 * x0) - omega0 * (x0 + (v0 + omega0 * x0) * t))
        --         * expTerm
        -- }
        -- let
        --     freq =
        --         fundamentalFrequency spring
        --     period =
        --         -- ((1 / freq) * 1000) / 2
        --         -- Which is equivalent to
        --         500 / freq
        -- in
        -- The above math doesn't work, so we're going to use a different approach
        stepOver
            { spring = spring
            , target = target
            , initial = initial
            , stepSize =
                10
            }
            durationMs


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
                end
                targetPos
                initialState

        end =
            settlesAt spring
    in
    let
        onSpring =
            at
                { spring = spring
                , target = targetPos
                , initial = initialState
                }

        toPoint x =
            { x = x
            , y =
                onSpring x
                    |> .position
            }
    in
    case pks of
        first :: second :: [] ->
            Bezier.trace
                { toPoint = toPoint
                , steps = 8
                , start = first
                , end = second
                }

        first :: second :: remaining ->
            let
                firstSegments =
                    Bezier.trace
                        { toPoint = toPoint
                        , steps = 4
                        , start = first
                        , end = second
                        }

                tail =
                    List.map2
                        (\one two ->
                            let
                                posOne =
                                    onSpring one

                                posTwo =
                                    onSpring two

                                factor =
                                    0.1

                                spread =
                                    -0.03

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
                            if two > end then
                                -- This bezier segment is going to overshoot the target
                                result
                                    |> Bezier.splitAtX end
                                    |> Tuple.first

                            else
                                result
                        )
                        (List.drop 1 pks)
                        (List.drop 2 pks)
            in
            firstSegments ++ tail

        _ ->
            Bezier.trace
                { toPoint = toPoint
                , steps = 8
                , start = 0
                , end = end
                }


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


fundamentalFrequency : Parameters -> Float
fundamentalFrequency parameters =
    let
        naturalFrequency =
            sqrt (parameters.stiffness / parameters.mass)

        dampingRatio =
            parameters.damping / (2 * sqrt (parameters.stiffness * parameters.mass))

        dampedNaturalFrequency =
            naturalFrequency * sqrt (1 - dampingRatio ^ 2)
    in
    dampedNaturalFrequency / (2 * pi)


findFirstPeak :
    Parameters
    ->
        { velocity : Float
        , position : Float
        }
    -> Float
findFirstPeak params initial =
    let
        -- Calculate angular frequency (rad/s)
        omega0 =
            sqrt (params.stiffness / params.mass)

        -- Calculate damping ratio (dimensionless)
        zeta =
            params.damping / (2 * sqrt (params.mass * params.stiffness))

        -- Calculate damped angular frequency (rad/s)
        omegaD =
            omega0 * sqrt (1 - zeta ^ 2)

        -- Calculate phase angle (rad)
        phi =
            atan2 (initial.velocity + zeta * omega0 * initial.position) (omegaD * initial.position)

        -- Calculate time to first peak (s)
        t =
            (pi - phi) / omegaD
    in
    if zeta < 1 then
        -- For underdamped system, return calculated time
        -- ms
        t * 1000

    else
        -- For critically damped or overdamped systems, there's no oscillation
        0


{-| -}
peaks :
    Parameters
    -> Duration
    -> Float
    ->
        { velocity : Float
        , position : Float
        }
    -> List Duration
peaks spring endMs xTarget initial =
    if isCriticallyDamped spring || isOverDamped spring then
        [ 0
        , xTarget
        ]

    else
        let
            freq =
                fundamentalFrequency spring

            period =
                -- ((1 / freq) * 1000) / 2
                -- Which is equivalent to
                500 / freq

            offset =
                firstPeak
                    (\t ->
                        at
                            { spring = spring
                            , target = xTarget
                            , initial = initial
                            }
                            t
                            |> .velocity
                    )
                    0
                    5
                    period
                    |> Maybe.withDefault 0
        in
        capturePeriodicValues offset
            period
            0
            endMs
            []


hasChangedSign : Float -> Float -> Bool
hasChangedSign a b =
    (a < 0 && b > 0) || (a > 0 && b < 0)


firstPeak : (Float -> Float) -> Float -> Float -> Float -> Maybe Float
firstPeak toVelocity t stepSize maxT =
    if t > maxT then
        Nothing

    else if hasChangedSign (toVelocity t) (toVelocity (t + stepSize)) then
        let
            velocityT =
                toVelocity t

            velocityTPlusStep =
                toVelocity (t + stepSize)

            velocityTPlusStep2 =
                toVelocity (t + stepSize + stepSize)

            factor =
                -- percent of the step where 0 occurs
                abs velocityT
                    / (abs velocityT + abs velocityTPlusStep)
        in
        -- This should be Just (t + (stepSize * factor))
        -- But when I plot it out, it's wrong, which is frustrating.
        -- The below looks closer.
        -- Ugh, save me.
        Just (t + stepSize + stepSize + stepSize + (stepSize * factor))

    else
        firstPeak toVelocity (t + stepSize) stepSize maxT


previousPeaksImplementation :
    Parameters
    -> Duration
    -> Float
    ->
        { velocity : Float
        , position : Float
        }
    -> List Float
previousPeaksImplementation spring ms target initial =
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


{-|

    This function captures all values that are periodic with a given period and offset.

    It will only capture positive numbers

    and always include 0 as the first value and target as the last.

    Sorta weird, but convenient for our usecase.

-}
capturePeriodicValues : Float -> Float -> Int -> Float -> List Float -> List Float
capturePeriodicValues offset period n target captured =
    let
        next =
            offset + (toFloat n * period)
    in
    if next > target then
        List.reverse (target :: captured)

    else if next < 0 then
        -- Skip, we only care about positive values
        capturePeriodicValues offset period (n + 1) target captured

    else if next == 0 then
        -- We landed directly on 0, so we don't need to add it again
        capturePeriodicValues offset period (n + 1) target (next :: captured)

    else
        case captured of
            [] ->
                capturePeriodicValues offset period (n + 1) target (next :: 0 :: captured)

            _ ->
                capturePeriodicValues offset period (n + 1) target (next :: captured)


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

[`at`](#at) is faster, but possibly less accurate?

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
    -- this is 0.05% tolerance
    -1 * logBase e 0.0005


{-| We can detect when a spring will settle if it is underdamped (meaning it oscillates before resting).
<https://en.wikipedia.org/wiki/Settling_time>

However for critically and overdamped systems it gets a lot more complicated.
Fortunately, I'm not sure that that's an issue as I don't think we want to model overdamped spring systems for animation.
<https://electronics.stackexchange.com/questions/296567/over-and-critically-damped-systems-settling-time>

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
                -- |> mapToRange 120 210
                |> mapToRange 120 300

        damping =
            -- Wobble is essentally a damping ratio, but clamped to a certain range of values that are nice.
            -- We take in the expected settlign time of the spring because at low settling times,
            -- the range of the nice values changes
            wobble2Ratio options.wobble options.settleMax
                * criticalDamping k 1

        initiallySettlesAt =
            settlesAt
                { stiffness = k
                , damping = damping
                , mass = 1
                }
    in
    if initiallySettlesAt > options.settleMax then
        let
            scale =
                options.settleMax / initiallySettlesAt
        in
        -- Scale mass if it's going to take too long to settle
        -- Lol, physics
        if scale < 0.5 then
            { stiffness = k + ((1 - (scale / 0.5)) * 500)
            , damping = damping
            , mass =
                0.5
            }

        else
            { stiffness = k
            , damping = damping
            , mass =
                -- max 0.47 scale
                scale
            }

    else
        { stiffness = k
        , damping = damping
        , mass = 1
        }


criticalDamping : Float -> Float -> Float
criticalDamping k m =
    2 * sqrt (k * m)


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


    overdamping happens when the duration is short and the wobble is low.

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
            max topRatio (0.9 * min 1 scalingBelowDur)
    in
    (1 - bounded)
        |> mapToRange topRatio top


topRatio : Float
topRatio =
    0.43


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
