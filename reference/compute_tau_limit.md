# Compute Tau Limit (t_max)

Finds the latest time point \\t\_{max}\\ such that at least a certain
proportion of subjects remain at risk.

## Usage

``` r
compute_tau_limit(times, threshold)
```

## Arguments

- times:

  Numeric vector of survival times.

- threshold:

  Minimum proportion of subjects that must remain at risk.

## Value

The computed \\t\_{max}\\ value, or `NA_real_` if no valid times are
provided.
