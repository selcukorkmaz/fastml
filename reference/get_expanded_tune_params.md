# Expanded Default Tuning Parameters

Returns expanded tuning parameter ranges that provide better coverage
than the minimal defaults. These are used when `tuning_complexity` is
set to "thorough" or "exhaustive".

## Usage

``` r
get_expanded_tune_params(algo, train_data, label, engine)
```

## Arguments

- algo:

  Algorithm name.

- train_data:

  Training data frame.

- label:

  Outcome variable name.

- engine:

  Engine name.

## Value

A list of expanded tuning parameter ranges.
