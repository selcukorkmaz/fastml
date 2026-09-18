# Determine rounding digits for time horizons

Computes a sensible number of decimal digits to round time horizons
based on the minimal positive separation between unique finite times.

## Usage

``` r
determine_round_digits(times)
```

## Arguments

- times:

  Numeric vector of times.

## Value

Integer number of digits between 0 and 6.

## Details

Uses the smallest strictly positive difference among sorted unique
finite times, then returns `ceiling(-log10(min_diff))` truncated to
\\\[0, 6\]\\.

## Examples

``` r
# Not run: determine_round_digits(c(0.1, 0.12, 0.125))
NULL
#> NULL
```
