# Generate DALEX explanations for a fastml model

Creates a DALEX explainer and computes permutation based variable
importance, partial dependence (model profiles) and Shapley values.

## Usage

``` r
explain_dalex(
  object,
  data = c("train", "test"),
  features = NULL,
  grid_size = 20,
  shap_sample = 5,
  vi_iterations = 10,
  seed = 123,
  loss_function = NULL
)
```

## Arguments

- object:

  A `fastml` object.

- data:

  Character string specifying which data to use for explanations:
  `"train"` (default) uses training data, `"test"` uses held-out test
  data. Using test data provides explanations that better reflect model
  generalization, while training data explanations may be influenced by
  overfitting.

- features:

  Character vector of feature names for partial dependence (model
  profiles). Default NULL.

- grid_size:

  Number of grid points for partial dependence. Default 20.

- shap_sample:

  Integer number of observations from the selected data source to
  compute SHAP values for. Default 5.

- vi_iterations:

  Integer. Number of permutations for variable importance (B). Default
  10.

- seed:

  Integer. A value specifying the random seed.

- loss_function:

  Function. The loss function for `model_parts`.

  - If `NULL` and task = 'classification', defaults to
    [`DALEX::loss_cross_entropy`](https://modeloriented.github.io/DALEX/reference/loss_functions.html).

  - If `NULL` and task = 'regression', defaults to
    [`DALEX::loss_root_mean_square`](https://modeloriented.github.io/DALEX/reference/loss_functions.html).

## Value

Invisibly returns a list with variable importance, optional model
profiles and SHAP values.
