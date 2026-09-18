# Map Brier Curve Values to Specific Horizons

Extracts Brier score values from a pre-computed curve at specific time
horizons by finding the closest matching evaluation time.

## Usage

``` r
map_brier_values(curve, eval_times, horizons)
```

## Arguments

- curve:

  Numeric vector of Brier scores from `compute_ibrier`.

- eval_times:

  Numeric vector of times corresponding to `curve`.

- horizons:

  Numeric vector of target time horizons to extract.

## Value

A numeric vector of Brier scores corresponding to `horizons`.
