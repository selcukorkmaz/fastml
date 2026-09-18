# Resolve the positive class for binary classification

Determines the positive class respecting event_class settings from
fastml(). This ensures consistency across all explainer functions.

## Usage

``` r
resolve_positive_class(prep, y_levels)
```

## Arguments

- prep:

  A list from fastml_prepare_explainer_inputs containing:
  positive_class, event_class, label_levels

- y_levels:

  Character vector of actual levels from the target variable

## Value

Character string of the resolved positive class name
