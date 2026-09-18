# Guarded Resampling Utilities

Internal helpers that enforce the Guarded Resampling Principle by
fitting preprocessing pipelines independently within each resampling
split. These functions are not exported.

## Usage

``` r
fastml_guard_validate_indices(indices, label)
```

## Arguments

- indices:

  Numeric vector of row indices for a resample split.

- label:

  Character string used to identify the index source in errors.
