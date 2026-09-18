# Align Survival Curve to Evaluation Times

Aligns a survival curve (defined by time points and survival
probabilities) to a new set of evaluation times using constant
interpolation (last value carried forward). Ensures \\S(0) = 1\\ and
monotonicity.

## Usage

``` r
align_survival_curve(curve_times, curve_surv, eval_times)
```

## Arguments

- curve_times:

  Numeric vector of time points from the survival curve.

- curve_surv:

  Numeric vector of survival probabilities corresponding to
  `curve_times`.

- eval_times:

  Numeric vector of new time points to evaluate at.

## Value

A numeric vector of survival probabilities at `eval_times`.
