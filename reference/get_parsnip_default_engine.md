# Get Parsnip Default Engine for an Algorithm

Returns the default engine that parsnip would use for a given algorithm,
allowing comparison with fastml's optimized defaults.

## Usage

``` r
get_parsnip_default_engine(algo, task = NULL)
```

## Arguments

- algo:

  Character string specifying the algorithm name.

- task:

  Character string specifying the task type ("classification",
  "regression", or "survival").

## Value

Character string of the parsnip default engine, or NULL if unknown.

## Details

This function documents parsnip's default engine choices as of
tidymodels 1.x. These may change with future parsnip versions.
