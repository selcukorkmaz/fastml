# Explain a fastml model using various techniques

Provides model explainability across several backends. With
`method = "dalex"` it:

- Creates a DALEX explainer from the trained model.

- Computes permutation-based variable importance with `vi_iterations`
  permutations and displays the table and plot.

- Computes partial dependence-like model profiles when `features` are
  supplied.

- Computes Shapley values (SHAP) for `shap_sample` training rows,
  displays the SHAP table, and plots a canonical SHAP summary (beeswarm)
  plot colored by raw feature values and ordered by \\\text{mean}(\vert
  \text{SHAP value} \vert)\\ per feature. For classification, separate
  panels per class are shown.

## Usage

``` r
fastexplain(
  object,
  method = "dalex",
  data = c("train", "test"),
  features = NULL,
  n_features = 5,
  variables = NULL,
  observation = NULL,
  grid_size = 20,
  shap_sample = 5,
  vi_iterations = 10,
  seed = 123,
  loss_function = NULL,
  protected = NULL,
  ...
)
```

## Arguments

- object:

  A `fastml` object.

- method:

  Character string specifying the explanation method. Supported values
  are `"dalex"`, `"lime"`, `"ice"`, `"ale"`, `"surrogate"`,
  `"interaction"`, `"studio"`, `"fairness"`, `"breakdown"`, and
  `"counterfactual"`. Defaults to `"dalex"`.

- data:

  Character string specifying which data to use for explanations:
  `"train"` (default) uses training data, `"test"` uses held-out test
  data. Using test data provides explanations that better reflect model
  generalization, while training data explanations may be influenced by
  overfitting.

- features:

  Character vector of feature names for partial dependence (model
  profiles). Default NULL.

- n_features:

  Number of features to show in the explanation (used for lime). Default
  5.

- variables:

  Character vector. Variable names to compute explanations for (used for
  counterfactuals).

- observation:

  A single observation for methods that need a new data point
  (`method = "lime"`, `method = "counterfactual"`, or
  `method = "breakdown"`). Default NULL.

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

- protected:

  Character or factor vector of protected attribute(s) required for
  `method = "fairness"`. Default NULL.

- ...:

  Additional arguments passed to the underlying helper functions for the
  chosen `method`.

## Value

For DALEX-based methods, prints variable importance, model profiles, and
SHAP summaries. Other methods return their respective explainer objects
(e.g., LIME explanations, ALE plot, surrogate tree, interaction
strengths, modelStudio dashboard, fairmodels object, breakdown object,
or counterfactual results), usually invisibly after plotting or
printing.

## Details

- **Data source selection:** By default, explanations are computed on
  training data (`data = "train"`), which reflects in-sample model
  behavior and may be influenced by overfitting. Set `data = "test"` to
  compute explanations on held-out test data for a more realistic
  assessment of how the model uses features on unseen data.

- **Method dispatch:** `method` can route to LIME, ICE, ALE, surrogate
  tree, interaction strengths, DALEX/modelStudio dashboards, fairness
  diagnostics, iBreakDown contributions, or counterfactual search.

- **Variable importance controls:** Use `vi_iterations` to tune
  permutation stability and `loss_function` to override the default
  DALEX loss (cross-entropy for classification, RMSE for regression).

- **Fairness and breakdown support:** Provide `protected` for
  `method = "fairness"` and an `observation` for `method = "breakdown"`
  or `method = "counterfactual"`. Observations are aligned to the
  explainer data before scoring.

## Note

By default, explanations use training data. For unbiased feature
importance estimates that better reflect model generalization, use
`data = "test"` to compute explanations on held-out test data.
