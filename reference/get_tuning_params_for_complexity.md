# Get Tuning Parameters for Complexity Level

Returns algorithm-specific tuning parameter ranges adjusted for the
specified complexity level.

## Usage

``` r
get_tuning_params_for_complexity(
  algo,
  train_data,
  label,
  engine,
  complexity = "balanced"
)
```

## Arguments

- algo:

  Character string specifying the algorithm name.

- train_data:

  Data frame containing the training data.

- label:

  Character string specifying the outcome variable name.

- engine:

  Character string specifying the engine.

- complexity:

  Character string specifying tuning complexity level.

## Value

A list of tuning parameter ranges.

## Details

Parameter ranges are scaled based on the complexity level: - `quick`:
Narrower ranges (70 - `balanced`: Standard ranges (100 - `thorough`:
Wider ranges (130 - `exhaustive`: Very wide ranges (150
