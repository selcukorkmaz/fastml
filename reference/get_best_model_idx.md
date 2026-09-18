# Get Best Model Indices by Metric and Group

Get Best Model Indices by Metric and Group

## Usage

``` r
get_best_model_idx(df, metric, group_cols = c("Model", "Engine"))
```

## Arguments

- df:

  A data frame containing model performance summaries.

- metric:

  A single character string naming the column in \`df\` used to identify
  the best model.

- group_cols:

  Character vector of column names used to define model groups before
  selecting the best metric value. Defaults to \`c("Model", "Engine")\`.

## Value

Integer vector of row indices in \`df\` corresponding to the best model
group or groups.
