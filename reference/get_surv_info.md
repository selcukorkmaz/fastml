# Extract Time and Status from Survival Matrix

Helper function to extract "time" and "status" columns from a matrix
(like one returned by
[`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html)),
falling back to defaults.

## Usage

``` r
get_surv_info(surv_matrix_vals, default_time, default_status)
```

## Arguments

- surv_matrix_vals:

  A matrix, typically from `Surv(time, status)`.

- default_time:

  Default time vector if not found.

- default_status:

  Default status vector if not found.

## Value

A list with elements `time` and `status`.
