# Assign Risk Groups

Dichotomizes a continuous risk vector into "low" and "high" risk groups
based on the median.

## Usage

``` r
assign_risk_group(risk_vec)
```

## Arguments

- risk_vec:

  Numeric vector of predicted risk scores.

## Value

A character vector of "low", "high", or `NA`.
