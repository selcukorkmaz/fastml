# Evaluate Models Function

Evaluates the trained models on the test data and computes performance
metrics.

## Usage

``` r
fastml_compute_holdout_results(
  models,
  train_data,
  test_data,
  label,
  start_col = NULL,
  time_col = NULL,
  status_col = NULL,
  task,
  metric = NULL,
  event_class,
  class_threshold = 0.5,
  eval_times = NULL,
  bootstrap_ci = TRUE,
  bootstrap_samples = 500,
  bootstrap_seed = 1234,
  at_risk_threshold = 0.1,
  survival_metric_convention = "fastml",
  precomputed_predictions = NULL,
  summaryFunction = NULL,
  multiclass_auc = "macro"
)
```

## Arguments

- models:

  A list of trained model objects.

- train_data:

  Preprocessed training data frame.

- test_data:

  Preprocessed test data frame.

- label:

  Name of the target variable. For survival analysis this should be a
  character vector of length two giving the names of the time and status
  columns.

- start_col:

  Optional string. The name of the column specifying the start time in
  counting process (e.g., \`(start, stop, event)\`) survival data. Only
  used when `task = "survival"`.

- time_col:

  String. The name of the column specifying the event or censoring time
  (the "stop" time in counting process data). Only used when
  `task = "survival"`.

- status_col:

  String. The name of the column specifying the event status (e.g., 0
  for censored, 1 for event). Only used when `task = "survival"`.

- task:

  Type of task: "classification", "regression", or "survival".

- metric:

  The performance metric to optimize (e.g., "accuracy", "rmse").

- event_class:

  A single string. Either "first" or "second" to specify which level of
  truth to consider as the "event".

- class_threshold:

  For binary classification, controls how class probabilities are
  converted into hard class predictions. The default is \`0.5\`
  (standard threshold). Numeric values in (0, 1) set a fixed threshold.
  Use \`"auto"\` to tune a threshold on training data to maximize F1;
  use \`"model"\` to keep the model's default predictions.

- eval_times:

  Optional numeric vector of evaluation horizons for survival metrics.
  Passed through to `process_model`.

- bootstrap_ci:

  Logical indicating whether bootstrap confidence intervals should be
  computed for the evaluation metrics.

- bootstrap_samples:

  Number of bootstrap resamples used when `bootstrap_ci = TRUE`.

- bootstrap_seed:

  Optional integer seed for the bootstrap procedure used in metric
  estimation.

- at_risk_threshold:

  Minimum proportion of subjects that must remain at risk to define
  \\t\_{max}\\ when computing survival metrics such as the integrated
  Brier score.

- survival_metric_convention:

  Character string specifying which survival metric conventions to
  follow. \`"fastml"\` (default) uses fastml's internal defaults for
  evaluation horizons and t_max. \`"tidymodels"\` uses \`eval_times\` as
  the explicit evaluation grid and applies yardstick-style Brier/IBS
  normalization; when \`eval_times\` is \`NULL\`, time-dependent Brier
  metrics are omitted.

- precomputed_predictions:

  Optional data frame or nested list of previously generated predictions
  (per algorithm/engine) to reuse instead of recomputing. This is mainly
  used when combining results across engines.

- summaryFunction:

  Optional custom classification metric function passed through to
  `process_model` for holdout evaluation.

- multiclass_auc:

  For multiclass ROC AUC, the averaging method to use: \`"macro"\`
  (default, tidymodels) or \`"macro_weighted"\`. Macro weights each
  class equally, while macro_weighted weights by class prevalence and
  can change model rankings on imbalanced data.

## Value

A list with two elements:

- performance:

  A named list of performance metric tibbles for each model.

- predictions:

  A named list of data frames with columns including truth, predictions,
  and probabilities per model.
