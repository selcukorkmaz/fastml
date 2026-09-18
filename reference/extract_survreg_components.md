# Extract survreg Linear Predictor and Scale

Computes the linear predictor (lp) and scale parameter(s) for new data
from a fitted `survreg` model.

## Usage

``` r
extract_survreg_components(fit_obj, new_data)
```

## Arguments

- fit_obj:

  A fitted `survreg` object.

- new_data:

  A data frame with predictor variables.

## Value

A list with elements `lp` (numeric vector) and `scale` (numeric vector),
or `NULL` on failure.
