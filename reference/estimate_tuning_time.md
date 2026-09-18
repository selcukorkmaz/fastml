# Estimate Tuning Time

Provides a rough estimate of tuning time based on the configuration.

## Usage

``` r
estimate_tuning_time(
  n_params,
  n_folds = 10,
  n_rows = 1000,
  complexity = "balanced",
  tuning_strategy = "grid",
  base_fit_time = 1
)
```

## Arguments

- n_params:

  Number of parameters being tuned.

- n_folds:

  Number of cross-validation folds.

- n_rows:

  Number of rows in training data.

- complexity:

  Tuning complexity level.

- tuning_strategy:

  Tuning strategy ("grid" or "bayes").

- base_fit_time:

  Estimated time for a single model fit in seconds.

## Value

A list with estimated total time and breakdown.
