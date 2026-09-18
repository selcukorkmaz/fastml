# Summary Function for fastml (Using yardstick for ROC Curves)

Summarizes the results of machine learning models trained using the
\`fastml\` package. Depending on the task type (classification or
regression), it provides customized output such as performance metrics,
best hyperparameter settings, and confusion matrices. It is designed to
be informative and readable, helping users quickly interpret model
results.

## Usage

``` r
# S3 method for class 'fastml'
summary(
  object,
  algorithm = "best",
  type = c("all", "metrics", "params", "grid", "conf_mat"),
  sort_metric = NULL,
  show_ci = FALSE,
  brier_times = NULL,
  ...
)
```

## Arguments

- object:

  An object of class `fastml`.

- algorithm:

  A vector of algorithm names to display summary. Default is `"best"`.

- type:

  Character vector indicating which outputs to produce. Options are
  `"all"` (all available outputs), `"metrics"` (performance metrics),
  `"params"` (best hyperparameters), and `"conf_mat"` (confusion
  matrix). Default is `"all"`.

- sort_metric:

  The metric to sort by. Default uses optimized metric.

- show_ci:

  Logical indicating whether to display 95% confidence intervals for
  performance metrics in survival models. Defaults to `FALSE`.

- brier_times:

  Optional numeric or character vector that selects which time-specific
  Brier scores to display for survival models. When `NULL` (the
  default), time-specific Brier scores are omitted from the summary.

- ...:

  Additional arguments.

## Value

Prints summary of fastml models.

## Details

For classification tasks, the summary includes metrics such as Accuracy,
F1 Score, Kappa, Precision, ROC AUC, Sensitivity, and Specificity. A
confusion matrix is also provided for the best model(s). For regression
tasks, the summary reports RMSE, two forms of R-squared, and MAE.
\`rsq\` is squared Pearson correlation and is invariant to location and
scale; \`rsq_trad\` is one minus the residual sum of squares over the
total sum of squares. The two agree when predictions are unbiased and
correctly scaled, and diverge when they are not, so a large gap between
them is a sign of miscalibration rather than of weak association.

Users can control the type of output with the \`type\` argument:
\`metrics\` displays model performance metrics. \`params\` shows the
best hyperparameter settings. \`conf_mat\` prints confusion matrices
(only for classification). \`all\` includes all of the above.

If multiple algorithms are trained, the summary highlights the best
model based on the optimized metric. For survival tasks, Harrell's
C-index, Uno's C-index, the integrated Brier score, and (when available)
the RMST difference are shown by default. Specific Brier(t) horizons can
be requested through the `brier_times` argument.
