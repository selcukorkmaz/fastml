# Get Default Parameters with Transparency Warnings

Wrapper around `get_default_params` that optionally warns when fastml's
default parameters differ from parsnip/engine defaults.

## Usage

``` r
get_default_params_with_warnings(
  algo,
  task,
  num_predictors = NULL,
  engine = NULL,
  warn_param_defaults = TRUE,
  verbose = FALSE
)
```

## Arguments

- algo:

  Algorithm name.

- task:

  Task type.

- num_predictors:

  Number of predictors (optional).

- engine:

  Engine name (optional).

- warn_param_defaults:

  Logical; if TRUE, warn about parameter differences.

- verbose:

  Logical; if TRUE, print parameter selections.

## Value

A list of default parameters.
