# Get All Default Differences Summary

Returns a summary of all differences between fastml and parsnip defaults
for the specified algorithms.

## Usage

``` r
get_default_differences(algorithms, task = "classification")
```

## Arguments

- algorithms:

  Character vector of algorithm names.

- task:

  Task type ("classification", "regression", or "survival").

## Value

A data frame summarizing the differences.
