# Compute Survival Matrix from survreg Model

Generates a matrix of survival probabilities (rows=subjects, cols=times)
from a fitted `survreg` model for new data.

## Usage

``` r
compute_survreg_matrix(fit_obj, new_data, eval_times)
```

## Arguments

- fit_obj:

  A fitted `survreg` object.

- new_data:

  A data frame with predictor variables.

- eval_times:

  Numeric vector of evaluation times.

## Value

A matrix of survival probabilities, or `NULL` on failure.
