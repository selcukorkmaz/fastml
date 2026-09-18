# Train Specified Machine Learning Algorithms on the Training Data

Trains specified machine learning algorithms on the preprocessed
training data.

## Usage

``` r
train_models(
  train_data,
  label,
  task,
  algorithms,
  resampling_method,
  folds,
  repeats,
  group_cols = NULL,
  block_col = NULL,
  strata_cols = NULL,
  block_size = NULL,
  initial_window = NULL,
  assess_window = NULL,
  skip = 0,
  outer_folds = NULL,
  resamples = NULL,
  tune_params,
  engine_params = list(),
  metric,
  summaryFunction = NULL,
  seed = 123,
  recipe,
  use_default_tuning = FALSE,
  tuning_strategy = "grid",
  tuning_iterations = 10,
  tuning_complexity = "balanced",
  grid_levels = 3L,
  early_stopping = FALSE,
  adaptive = FALSE,
  algorithm_engines = NULL,
  use_parsnip_defaults = FALSE,
  warn_engine_defaults = TRUE,
  n_cores = 1,
  engine_threads = 1,
  verbose = FALSE,
  event_class = "first",
  class_threshold = 0.5,
  start_col = NULL,
  time_col = NULL,
  status_col = NULL,
  eval_times = NULL,
  at_risk_threshold = 0.1,
  survival_metric_convention = "fastml",
  audit_env = NULL,
  multiclass_auc = "macro",
  store_fold_models = FALSE
)
```

## Arguments

- train_data:

  Preprocessed training data frame.

- label:

  Name of the target variable.

- task:

  Type of task: "classification", "regression", or "survival".

- algorithms:

  Vector of algorithm names to train.

- resampling_method:

  Resampling method for cross-validation. Supported options include
  standard `"cv"`, `"repeatedcv"`, and `"boot"`, as well as grouped
  resampling via `"grouped_cv"`, blocked/rolling schemes via
  `"blocked_cv"` or `"rolling_origin"`, nested resampling via
  `"nested_cv"`, and the passthrough `"none"` option.

- folds:

  Number of folds for cross-validation.

- repeats:

  Number of times to repeat cross-validation (only applicable for
  methods like "repeatedcv").

- group_cols:

  Optional character vector of grouping columns used with
  \`resampling_method = "grouped_cv"\`. For classification problems the
  outcome column is used to request grouped stratification where
  supported; if class imbalance prevents stratification, grouped folds
  are still created and a warning is emitted to document the limitation.

- block_col:

  Optional name of the ordering column used with blocked or rolling
  resampling.

- strata_cols:

  Character vector naming the stratification columns used by
  `"stratified_cox"`. Columns whose names begin with `"strata"` are also
  recognised for backward compatibility. Naming a column that is not
  present in the data is an error, and requesting `"stratified_cox"`
  with no strata at all is an error rather than a silent skip. The
  stratifier is read from the training frame rather than from the
  preprocessed frame, so a numeric column is not affected by a step that
  rescales it. Default is `NULL`.

- block_size:

  Optional integer specifying the block size for \`resampling_method =
  "blocked_cv"\`.

- initial_window:

  Optional integer specifying the initial window size for rolling
  resampling.

- assess_window:

  Optional integer specifying the assessment window size for rolling
  resampling.

- skip:

  Optional integer number of resamples to skip between rolling
  resamples.

- outer_folds:

  Optional integer specifying the number of outer folds for
  \`resampling_method = "nested_cv"\`.

- resamples:

  Optional rsample object. If provided, custom resampling splits will be
  used instead of those created internally.

- tune_params:

  A named list of tuning ranges. For each algorithm, supply a list of
  engine-specific parameter values, e.g.
  `list(rand_forest = list(ranger = list(mtry = c(1, 3)))).`

- engine_params:

  A named list of fixed engine-level arguments passed directly to the
  model fitting call for each algorithm/engine combination. Use this to
  control options like `ties = "breslow"` for Cox models or
  `importance = "impurity"` for ranger. Unlike `tune_params`, these
  values are not tuned over a grid.

- metric:

  The performance metric to optimize. For classification, options
  include `"accuracy"`, `"roc_auc"`, `"logloss"`, `"brier_score"`, and
  `"ece"` (plus other class metrics).

- summaryFunction:

  A custom summary function for model evaluation. Default is `NULL`.

- seed:

  An integer value specifying the random seed for reproducibility.

- recipe:

  A recipe object for preprocessing.

- use_default_tuning:

  Logical; if `TRUE` and `tune_params` is `NULL`, tuning is performed
  using default grids. Tuning also occurs when custom `tune_params` are
  supplied. When `FALSE` and no custom parameters are given, the model
  is fitted once with default settings.

- tuning_strategy:

  A string specifying the tuning strategy. Must be one of `"grid"`,
  `"bayes"`, or `"none"`. Adaptive methods may be used with `"grid"`. If
  `"none"` is selected, the workflow is fitted directly without tuning.
  If custom `tune_params` are supplied with `tuning_strategy = "none"`,
  they will be ignored with a warning.

- tuning_iterations:

  Number of iterations for Bayesian tuning. Ignored when
  `tuning_strategy` is not `"bayes"`; validation occurs only for the
  Bayesian strategy.

- tuning_complexity:

  Character string specifying tuning complexity preset. One of "quick",
  "balanced", "thorough", or "exhaustive". Controls both grid density
  and parameter range width.

- grid_levels:

  Integer specifying number of levels per parameter for grid search.
  Higher values create denser grids but increase computation
  exponentially (grid size = levels^n_params).

- early_stopping:

  Logical for early stopping in Bayesian tuning.

- adaptive:

  Logical indicating whether to use adaptive/racing methods.

- algorithm_engines:

  A named list specifying the engine to use for each algorithm.

- use_parsnip_defaults:

  Logical. If `TRUE`, use parsnip's default engines instead of fastml's
  optimized defaults. Default is `FALSE`.

- warn_engine_defaults:

  Logical. If `TRUE` (default), warn when fastml's default engine
  differs from parsnip's default.

- n_cores:

  Integer number of parallel worker processes used to evaluate resamples
  and tuning candidates. Sizes the worker pool only, and decides whether
  tuning/resampling runs in parallel.

- engine_threads:

  Integer number of threads passed to engines that accept a thread count
  (`num.threads`, `num_threads`, `nthread`). Total CPU demand is
  approximately `n_cores * engine_threads`.

- verbose:

  Logical. If `TRUE`, print informational messages about engine
  selection and parameter overrides.

- event_class:

  Character string identifying the positive class when computing
  classification metrics ("first" or "second").

- class_threshold:

  For binary classification, controls how class probabilities are
  converted into hard class predictions during evaluation. The default
  is \`0.5\` (standard threshold). Numeric values in (0, 1) set a fixed
  threshold. Use \`"auto"\` to tune a threshold on training data to
  maximize F1; use \`"model"\` to keep the model's default predictions.

- start_col:

  Optional name of the survival start time column passed through to
  downstream evaluation helpers.

- time_col:

  Optional name of the survival stop time column.

- status_col:

  Optional name of the survival status/event column.

- eval_times:

  Optional numeric vector of time horizons for survival metrics.

- at_risk_threshold:

  Numeric cutoff used to determine the evaluation window for survival
  metrics within guarded resampling.

- survival_metric_convention:

  Character string specifying which survival metric conventions to
  follow. \`"fastml"\` (default) uses fastml's internal defaults for
  evaluation horizons and t_max. \`"tidymodels"\` uses \`eval_times\` as
  the explicit evaluation grid and applies yardstick-style Brier/IBS
  normalization; when \`eval_times\` is \`NULL\`, time-dependent Brier
  metrics are omitted.

- audit_env:

  Internal environment that tracks security audit findings when custom
  preprocessing hooks are executed. Typically supplied by
  [`fastml()`](https://selcukorkmaz.github.io/fastml/reference/fastml.md)
  and should be left as `NULL` when calling `train_models()` directly.

- multiclass_auc:

  For multiclass ROC AUC, the averaging method to use: \`"macro"\`
  (default, tidymodels) or \`"macro_weighted"\`. Macro weights each
  class equally, while macro_weighted weights by class prevalence and
  can change model rankings on imbalanced data.

- store_fold_models:

  Logical. If `TRUE`, store the fitted fold models during resampling for
  later inspection or stability analysis.

## Value

A list of trained model objects.
