# Elm Bezier

This package is for working with cubic Bezier curves and is specifically made as a dependency for [Elm Animator](https://package.elm-lang.org/packages/mdgriffith/elm-animator/1.1.1/).

A good amount of this code was borrowed from [Elm Geometry](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry), though the `Bezier.Spring` module is largely unique (though it was written with a considerable amount of help from Ian Mackenzie, for which I am very grateful!)

## Tests

Run the automated spring regressions from the repository root:

```sh
elm-test tests/SpringTest.elm
```

Use an explicit test path: `tests/visual-tests` is a separate Elm application,
and default recursive test discovery also picks up its playground modules.

### Visual playgrounds

From `tests/visual-tests`, run `elm reactor`, then open:

- <http://localhost:8000/src/BezierSegments.elm> — interactive spring controls,
  sampled positions, fitted segments, and segment time boundaries.
- <http://localhost:8000/src/Playground.elm> — preset spring comparisons and
  Bezier splitting examples.

Both use the local library source. The graphs use milliseconds horizontally and
position vertically (positive downward). Each page includes a color legend.
The numerical reference uses 0.16ms integration steps and is still approximate.

In the interactive playground, vary the end position through positive, zero,
and negative values: the trace should still end at the estimated settling-time
marker. Try both signs of initial velocity and compare the sampled positions
with the numerical reference and fitted curves. These are static plots, not
animated previews; samples cover 0–1000ms and large excursions can leave the
fixed viewport. The automated tests check velocity and restart continuity.

## Spring integration notes

- Time arguments and segment x coordinates are in **milliseconds**; initial and
  returned velocities are in **position units per second**. Converting a segment's
  slope (position per millisecond) to a spring velocity requires multiplying by 1000.
- `segments` traces from zero to `settlesAt spring`, not to a caller-supplied
  duration. Its endpoint is a sample of the spring, not an exact snap to the target.
  `settlesAt` is an estimate based on spring parameters, independent of the initial
  position and velocity. In particular, its overdamped estimate is known to be
  inaccurate. `new`'s `settleMax` should not be treated as a hard timing guarantee.
- Segment fitting is approximate. Peak detection uses 5ms sampling and an existing
  15ms offset; fitted curves do not guarantee exact velocity continuity. The number
  of segments depends on the spring and is not capped at ten.
- `at` uses an analytical solution for underdamped springs, but numerical stepping
  for critical and overdamped springs (10ms and 20ms steps respectively). Those
  branches have step-size-dependent accuracy and stability. `stepOver` needs a
  positive step size when advancing time.
- Use finite, positive mass, stiffness, and damping for settling traces. Parameters
  are not validated; undamped springs do not settle and are unsuitable for
  `segments`.

### Regression history

In tag `1.0.0`, segment generation confuses target position with elapsed time, and
`at` ignores initial velocity. The `2.0.0` rewrite uses initial velocity, but retained
the time/position mix-up for critical, overdamped, and near-critical springs. It
also introduced an underdamped velocity formula inconsistent with its position
formula. The automated suite covers the corrected time bounds, initial state,
position derivative, numerical reference motion, and restart continuity.
