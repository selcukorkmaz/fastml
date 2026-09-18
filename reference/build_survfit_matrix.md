# Build Survival Matrix from survfit Object

Extracts survival probabilities from a `survfit` object and aligns them
to a common set of evaluation times, creating a matrix.

## Usage

``` r
build_survfit_matrix(fit_obj, eval_times, n_obs)
```

## Arguments

- fit_obj:

  A `survfit` object.

- eval_times:

  Numeric vector of evaluation times.

- n_obs:

  Expected number of observations (rows).

## Value

A matrix (rows=subjects, cols=eval_times) of survival probabilities, or
`NULL` on failure.
