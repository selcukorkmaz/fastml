# Recommend Tuning Configuration

Provides recommendations for tuning configuration based on dataset
characteristics and time constraints.

## Usage

``` r
recommend_tuning_config(
  n_rows,
  n_predictors,
  n_algorithms = 1,
  max_time_minutes = 30,
  tuning_strategy = "grid"
)
```

## Arguments

- n_rows:

  Number of rows in training data.

- n_predictors:

  Number of predictor variables.

- n_algorithms:

  Number of algorithms to tune.

- max_time_minutes:

  Maximum acceptable tuning time in minutes.

- tuning_strategy:

  Preferred tuning strategy.

## Value

A list with recommended configuration.
