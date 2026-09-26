# Fast Machine Learning Function

Trains and evaluates multiple classification or regression models
automatically detecting the task based on the target variable type.

## Usage

``` r
fastml(
  data = NULL,
  train_data = NULL,
  test_data = NULL,
  label,
  algorithms = "all",
  task = "auto",
  test_size = 0.2,
  test_size_tolerance = 0.05,
  resampling_method = if (identical(task, "survival")) "none" else "cv",
  folds = ifelse(grepl("cv", resampling_method), 10, 25),
  repeats = NULL,
  group_cols = NULL,
  block_col = NULL,
  strata_cols = NULL,
  block_size = NULL,
  initial_window = NULL,
  assess_window = NULL,
  skip = 0,
  outer_folds = NULL,
  event_class = "first",
  exclude = NULL,
  recipe = NULL,
  tune_params = NULL,
  engine_params = list(),
  metric = NULL,
  class_threshold = 0.5,
  algorithm_engines = NULL,
  use_parsnip_defaults = FALSE,
  warn_engine_defaults = TRUE,
  n_cores = 1,
  engine_threads = 1,
  stratify = TRUE,
  impute_method = "error",
  encode_categoricals = TRUE,
  scaling_methods = c("center", "scale"),
  balance_method = "none",
  resamples = NULL,
  summaryFunction = NULL,
  use_default_tuning = FALSE,
  tuning_strategy = "grid",
  tuning_iterations = 10,
  tuning_complexity = "balanced",
  grid_levels = NULL,
  early_stopping = FALSE,
  adaptive = FALSE,
  learning_curve = FALSE,
  seed = 123,
  verbose = FALSE,
  eval_times = NULL,
  survival_metric_convention = "fastml",
  bootstrap_ci = FALSE,
  bootstrap_samples = 500,
  bootstrap_seed = NULL,
  at_risk_threshold = 0.1,
  audit_mode = FALSE,
  multiclass_auc = "macro",
  store_fold_models = FALSE
)
```

## Arguments

- data:

  A data frame containing the complete dataset. If both \`train_data\`
  and \`test_data\` are \`NULL\`, \`fastml()\` will split this into
  training and testing sets according to \`test_size\` and \`stratify\`.
  When \`group_cols\` is supplied, the holdout keeps groups intact; when
  \`block_col\` is supplied, the holdout uses the last rows in time
  order; when both are supplied, the holdout is cut at the latest point
  that keeps every group intact, so both guarantees hold at once.
  Defaults to \`NULL\`.

- train_data:

  A data frame pre-split for model training. If provided, \`test_data\`
  must also be supplied, and no internal splitting will occur. Defaults
  to \`NULL\`.

- test_data:

  A data frame pre-split for model evaluation. If provided,
  \`train_data\` must also be supplied, and no internal splitting will
  occur. Defaults to \`NULL\`.

- label:

  A string specifying the name of the target variable. For survival
  analysis, supply a character vector with the names of the time and
  status columns.

- algorithms:

  A vector of algorithm names to use. Default is `"all"` to run all
  supported algorithms.

- task:

  Character string specifying model type selection. Use "auto" to let
  the function detect whether the target is for classification,
  regression, or survival based on the data. Survival is detected when
  \`label\` is a character vector of length 2 that matches time and
  status columns in the data. You may also explicitly set to
  "classification", "regression", or "survival".

- test_size:

  A numeric value between 0 and 1 indicating the proportion of the data
  to use for testing. For grouped holdout, this is applied to groups;
  for time-ordered holdout, it selects the final proportion of rows; for
  grouped time-ordered holdout it is a target that is met as closely as
  an intact-group cut allows, with a warning when the realized
  proportion differs from it by more than `test_size_tolerance`. Default
  is `0.2`.

- test_size_tolerance:

  A numeric value giving how far the realized test proportion may fall
  from `test_size` before a warning is issued. This applies only to the
  grouped time-ordered holdout, where the cut must fall between whole
  groups and the requested proportion is therefore a target rather than
  a constraint. There is no principled value for it, since how much
  departure matters depends on the sample size and on how the test
  estimate will be used, so it is exposed rather than fixed. The default
  of `0.05` is a reporting threshold chosen to be small enough to catch
  a materially different split while not warning on the rounding that
  whole group boundaries inevitably produce. Set it to `0` to be told
  the realized proportion whenever it differs at all.

- resampling_method:

  A string specifying the resampling method for model evaluation.
  Default is `"cv"` (cross-validation) for classification/regression.
  Other options include `"none"`, `"boot"`, `"repeatedcv"`,
  `"grouped_cv"`, `"blocked_cv"`, `"rolling_origin"`, and `"nested_cv"`.
  For survival tasks, resampling is supported for parsnip-compatible
  engines (e.g., censored/ranger, glmnet). Native survival engines
  (flexsurv/rstpm2/custom xgboost) ignore resampling and will error if
  custom resamples are supplied. When the task auto-detects survival and
  `resampling_method` is omitted, it defaults to `"none"` so native
  engines continue to run; set it explicitly to enable resampling for
  parsnip survival fits.

- folds:

  An integer specifying the number of folds for cross-validation
  (default `10` for methods containing "cv", `25` otherwise). When
  `resampling_method = "boot"`, this controls the number of bootstrap
  resamples. When `resampling_method = "validation_split"`, the
  proportion held out for validation is derived as `1 - 1/folds`.

- repeats:

  Number of times to repeat cross-validation (only applicable for
  methods like "repeatedcv").

- group_cols:

  Character vector naming one or more grouping columns used when
  `resampling_method = "grouped_cv"` or when grouped nested
  cross-validation is desired. All rows that share the same combination
  of values remain together in every fold. Columns must exist in the
  training data and cannot contain missing values. The default recipe
  gives these columns the non-predictor role `"grouping"` and removes
  them before preprocessing, so group identity is never used as a
  feature; they are not required in the data passed to
  [`predict()`](https://rdrr.io/r/stats/predict.html). A user-supplied
  `recipe` controls roles itself, and `fastml()` warns when it leaves a
  grouping column as a predictor.

- block_col:

  Single column name that defines the ordering variable for
  `resampling_method = "blocked_cv"` or `"rolling_origin"`. Data must
  already be sorted in ascending order by this column to avoid leakage
  from future observations. The default recipe gives this column the
  non-predictor role `"ordering"` and removes it before preprocessing,
  so it is not required in the data passed to
  [`predict()`](https://rdrr.io/r/stats/predict.html); to model a time
  trend, supply a `recipe` that derives the features you want. When
  `group_cols` is also supplied, the holdout split is cut at the
  admissible point nearest the requested `test_size` at which no group
  spans the cut, so that every training row precedes every test row and
  no group appears on both sides; if the groups are interleaved in time
  so that no such point exists, `fastml()` stops rather than relaxing
  either guarantee.

- strata_cols:

  Optional character vector naming the columns that define strata for
  `stratified_cox`. When `NULL` (the default), columns whose names begin
  with `"strata"` are used, which is the historical behaviour.

- block_size:

  Positive integer specifying the block size for `"blocked_cv"`.

- initial_window:

  Positive integer giving the number of observations in the initial
  training window for `"rolling_origin"` resampling.

- assess_window:

  Positive integer giving the number of observations in each assessment
  window for `"rolling_origin"` resampling.

- skip:

  Non-negative integer specifying how many potential rolling windows to
  skip between successive resamples when
  `resampling_method = "rolling_origin"`.

- outer_folds:

  Positive integer giving the number of outer folds to use when
  `resampling_method = "nested_cv"` and no custom `resamples` object is
  supplied.

- event_class:

  A single string. Either "first" or "second" to specify which level of
  the binary outcome factor to treat as the positive class (the
  "event"). For binary classification, "first" treats the first factor
  level as the positive class, "second" treats the second level as
  positive. Use `levels(your_data$outcome)` to check level order before
  training. Default is "first".

- exclude:

  A character vector specifying the names of the columns to be excluded
  from the training process. Exclusion is applied to `data` before it is
  split. Columns also named in `group_cols` or `block_col` are retained,
  with a message, because splitting and resampling need them; they are
  already kept out of the default recipe's predictors.

- recipe:

  A user-defined `recipe` object for custom preprocessing. If provided,
  internal recipe steps (imputation, encoding, scaling) are skipped. The
  column types the recipe recorded are preserved: character and integer
  columns the recipe covers are not converted to factors or doubles. A
  classification outcome must already be a factor when the recipe is
  built.

- tune_params:

  A named list of candidate tuning values for each algorithm and engine
  pair. Example:
  `list(rand_forest = list(ranger = list(mtry = c(1, 3))))` searches
  `mtry` at 1 and 3 for the ranger engine.

  Every value supplied here is treated as a candidate to be searched,
  not as the endpoint of an interval, whatever the number of values
  given. Writing `min_n = c(20, 50)` searches exactly 20 and 50, and
  does not interpolate between them. The grid is widened where necessary
  so that each requested value is used, even when it lies outside the
  package default range for that parameter.

  Values are given on the parameter's natural scale. A learning rate of
  `0.05` means 0.05, and a penalty of `0.01` means 0.01, even though
  dials stores these parameters logarithmically. The grid that was
  actually searched is recorded in the `tuning_grid` component of the
  returned object and printed by
  [`summary()`](https://rdrr.io/r/base/summary.html), so the
  specification supplied here can be checked against the one that was
  used.

  Default is `NULL`, in which case the package defaults documented in
  [`availableMethods`](https://selcukorkmaz.github.io/fastml/reference/availableMethods.md)
  and the package vignette are used.

- engine_params:

  A named list of engine-level arguments to pass directly to the
  underlying model fitting functions. Use this for fixed settings that
  should apply whenever an engine is fitted (for example,
  `list(royston_parmar = list(rstpm2 = list(link = "PO")))`,
  `list(cox_ph = list(survival = list(ties = "breslow")))`, or
  `list(rand_forest = list(ranger = list(importance = "impurity")))`).
  These arguments are distinct from `tune_params`, which define ranges
  of hyperparameters to explore during tuning. Default is an empty list.

- metric:

  The performance metric to optimize during training. For
  classification, options include `"accuracy"`, `"roc_auc"`,
  `"logloss"`, `"brier_score"`, and `"ece"` (plus other class metrics).

- class_threshold:

  For binary classification, controls how class probabilities are
  converted into hard class predictions during holdout evaluation. The
  default is `0.5` (standard threshold). Numeric values in (0, 1) set a
  fixed threshold. Use `"auto"` to tune a threshold on training data to
  maximize F1, or specify a metric name (e.g., `"sens"`, `"spec"`,
  `"youden"`) to optimize for that metric. Use `"model"` to keep the
  model's default predictions.

- algorithm_engines:

  A named list specifying the engine to use for each algorithm.

- use_parsnip_defaults:

  Logical. If `TRUE`, fastml uses parsnip's default engines instead of
  fastml's optimized defaults. This provides compatibility with standard
  tidymodels behavior. If `FALSE` (default), fastml uses its own curated
  engine defaults which may differ from parsnip. When engines differ, a
  warning is issued unless `algorithm_engines` explicitly specifies the
  engine. Use
  [`print_default_differences()`](https://selcukorkmaz.github.io/fastml/reference/print_default_differences.md)
  to see all differences.

- warn_engine_defaults:

  Logical. If `TRUE` (default), warns when fastml's default engine
  differs from parsnip's default. Set to `FALSE` to suppress these
  warnings. Warnings are only shown once per algorithm per session.

- n_cores:

  An integer specifying the number of parallel worker processes used to
  evaluate resamples and tuning candidates. Default is `1` (sequential).
  This sizes the worker pool only; it does not change how many threads
  an individual engine uses, which is controlled by `engine_threads`.

- engine_threads:

  An integer specifying the number of threads passed to engines that
  accept a thread count (for example `num.threads` for ranger,
  `num_threads` for lightgbm, `nthread` for xgboost). Default is `1`.
  Total CPU demand is approximately `n_cores * engine_threads`, so
  raising both above the number of available cores will oversubscribe
  the machine and can be slower than either alone. Values greater than
  `1` may also make some engines nondeterministic; a warning is issued
  where that applies.

- stratify:

  Logical indicating whether to use stratified sampling when splitting
  the data. Only applied to random holdout splitting. Default is `TRUE`
  for classification and `FALSE` for regression.

- impute_method:

  Method for handling missing values. Options include:

  `"medianImpute"`

  :   Impute missing values using median imputation (recipe-based).

  `"knnImpute"`

  :   Impute missing values using k-nearest neighbors (recipe-based).

  `"bagImpute"`

  :   Impute missing values using bagging (recipe-based).

  `"remove"`

  :   Remove rows with missing values from the data (recipe-based).

  `"error"`

  :   Do not perform imputation; if missing values are detected, stop
      execution with an error.

  `NULL`

  :   Equivalent to `"error"`. No imputation is performed, and the
      function will stop if missing values are present.

  All imputation occurs inside the recipe so the same trained
  preprocessing can be applied at prediction time. Default is `"error"`.

- encode_categoricals:

  Logical indicating whether to encode categorical variables. Default is
  `TRUE`.

- scaling_methods:

  Vector of scaling methods to apply. Default is `c("center", "scale")`.

- balance_method:

  Method to handle class imbalance. One of `"none"`, `"upsample"`, or
  `"downsample"`. Applied inside the preprocessing recipe so each
  resampling split is balanced independently (requires the `themis`
  package when enabled). Default is `"none"`.

- resamples:

  Optional rsample object providing custom resampling splits. If
  supplied, `resampling_method`, `folds`, and `repeats` are ignored.

- summaryFunction:

  A custom summary function for model evaluation. Default is `NULL`.

- use_default_tuning:

  Logical. Tuning only runs when resamples are supplied and
  `tuning_strategy` is not `"none"`. If `TRUE` and `tune_params` is
  `NULL`, default grids are used; if `tune_params` is provided, those
  values override/extend defaults. When `FALSE` and no custom parameters
  are given, models are fitted once with default settings. If no
  resamples are available or `tuning_strategy = "none"`, tuning requests
  are ignored with a warning. Default is `FALSE`.

- tuning_strategy:

  A string specifying the tuning strategy. Must be one of `"grid"`,
  `"bayes"`, or `"none"`. Default is `"grid"`. If custom `tune_params`
  are provided while `tuning_strategy = "none"`, they will be ignored
  with a warning.

- tuning_iterations:

  Number of iterations for Bayesian tuning. Ignored when
  `tuning_strategy` is not `"bayes"`. Validation of this argument only
  occurs for the Bayesian strategy. Default is `10`.

- tuning_complexity:

  Character string specifying a tuning complexity preset that controls
  grid density and parameter range width. One of:

  `"quick"`

  :   Minimal tuning (2 levels/param, ~32 combinations for 5 params).
      Best for: prototyping, debugging, time-constrained scenarios.

  `"balanced"`

  :   Standard tuning (3 levels/param, ~243 combinations). Best for:
      most production use cases. This is the default.

  `"thorough"`

  :   Comprehensive tuning (5 levels/param, ~3,125 combinations). Best
      for: final model selection, publications.

  `"exhaustive"`

  :   Maximum coverage (7 levels/param, ~16,807 combinations). Best for:
      research, competitions. Consider Bayesian tuning instead.

  See
  [`print_tuning_presets`](https://selcukorkmaz.github.io/fastml/reference/print_tuning_presets.md)
  for detailed comparison. Ignored if `grid_levels` is explicitly set.

- grid_levels:

  Integer specifying the number of levels per parameter for grid search.
  Higher values create denser grids but increase computation time
  exponentially (grid size = levels^n_params). Typical values:

  - 2: Very fast, minimal coverage

  - 3: Balanced (default via `tuning_complexity = "balanced"`)

  - 5: Thorough coverage

  - 7+: Exhaustive (consider Bayesian tuning instead)

  If `NULL` (default), determined by `tuning_complexity`.

- early_stopping:

  Logical indicating whether to use early stopping in Bayesian tuning
  methods (if supported). Default is `FALSE`.

- adaptive:

  Logical indicating whether to use adaptive/racing methods for tuning.
  Default is `FALSE`.

- learning_curve:

  Logical. If TRUE, generate learning curves (performance vs. training
  size).

- seed:

  An integer value specifying the random seed for reproducibility.
  fastml also configures parallel backends for deterministic RNG streams
  when possible; some external engines (e.g., h2o, spark, keras) may
  still be nondeterministic and will emit a warning.

- verbose:

  Logical; if TRUE, prints progress messages during the training and
  evaluation process.

- eval_times:

  Optional numeric vector of evaluation horizons for survival models.
  When `NULL`, defaults to the median and 75th percentile of the
  observed follow-up times (rounded to the dataset's time unit).

- survival_metric_convention:

  Character string specifying which survival metric conventions to
  follow. \`"fastml"\` (default) uses fastml's internal defaults for
  evaluation horizons and t_max. \`"tidymodels"\` uses \`eval_times\` as
  the explicit evaluation grid and applies yardstick-style Brier/IBS
  normalization; when \`eval_times\` is \`NULL\`, time-dependent Brier
  metrics are omitted.

- bootstrap_ci:

  Logical indicating whether bootstrap confidence intervals should be
  computed for performance metrics. Applies to all task types. Defaults
  to `FALSE`.

  The estimand is narrow. The model is fitted once and the bootstrap
  resamples the fixed vector of held-out predictions, so the interval
  describes uncertainty from the evaluation sample *conditional on that
  fitted model*. It does not propagate uncertainty from training-data
  variability, from tuning, or from any other part of model development,
  and it is therefore narrower than an interval that refitted the whole
  pipeline within each replicate. Read as ordinary uncertainty about
  model performance it overstates precision, which is why it is not
  computed unless requested. Computing it also dominates run time,
  accounting for about 85 percent of elapsed time on a Cox fit to the
  `rotterdam` data, which falls from roughly 19 seconds to roughly 2.9
  seconds with it disabled.

- bootstrap_samples:

  Integer giving the number of bootstrap resamples to use when
  `bootstrap_ci = TRUE`. Defaults to 500.

- bootstrap_seed:

  Optional seed passed to the bootstrap procedure used to estimate
  confidence intervals. When omitted, defaults to \`seed\` for
  reproducible intervals; set to \`NULL\` to allow random bootstrap
  draws.

- at_risk_threshold:

  Numeric value between 0 and 1 used for survival metrics to determine
  the last follow-up time (\\t\_{max}\\). The maximum time is set to the
  largest observed time where at least this proportion of subjects
  remain at risk.

- audit_mode:

  Logical; if `TRUE`, enables runtime auditing of custom preprocessing
  hooks and records potentially unsafe behaviour (such as global
  environment access or file I/O) while flagging the run as potentially
  unsafe.

- multiclass_auc:

  For multiclass ROC AUC, the averaging method to use: \`"macro"\`
  (default, tidymodels) or \`"macro_weighted"\`. Macro weights each
  class equally, while macro_weighted weights by class prevalence and
  can change model rankings on imbalanced data.

- store_fold_models:

  Logical. If `TRUE`, stores the models trained on each cross-validation
  fold (memory intensive). This enables
  [`explain_stability`](https://selcukorkmaz.github.io/fastml/reference/explain_stability.md)
  to compute feature importance across folds and assess explanation
  stability. Default is `FALSE`.

## Value

An object of class `fastml` containing the best model, performance
metrics, and other information. Among its components, `resampling_plan`
records the resampling design that was used, `preprocessor` the
preprocessing specification, and `tuning_grid` the tuning grid that was
actually searched, listed per algorithm and engine with each parameter
marked as coming from the user or from the package defaults.

## Details

Fast Machine Learning Function

Trains and evaluates multiple classification or regression models. The
function automatically detects the task based on the target variable
type and can perform advanced hyperparameter tuning using various tuning
strategies.

Model selection is based exclusively on resampling metrics
(cross-validation or nested CV). The holdout split is reserved for final
performance estimation and is never used to choose the best model,
mirroring `tidymodels::last_fit()` semantics.

For multiclass ROC AUC, fastml defaults to macro averaging (tidymodels).
Macro treats each class equally, while macro_weighted weights by class
prevalence and can change model rankings on imbalanced data. Keep the
same setting when comparing runs.

\## Tuning: Speed vs Robustness Trade-offs

Hyperparameter tuning involves a fundamental trade-off between
computational cost and the likelihood of finding optimal
hyperparameters. fastml provides presets via `tuning_complexity` to make
this trade-off explicit:

|            |                 |          |             |                        |
|------------|-----------------|----------|-------------|------------------------|
| **Level**  | **Grid Size\*** | **Time** | **Quality** | **Use Case**           |
| quick      | ~32             | ~1x      | Low         | Prototyping, debugging |
| balanced   | ~243            | ~10x     | Medium      | Most production use    |
| thorough   | ~3,125          | ~100x    | High        | Final models, papers   |
| exhaustive | ~16,807         | ~1000x   | Very High   | Research, competitions |

\*Grid size shown for 5 tunable parameters (levels^5)

\*\*Recommendations:\*\*

- Start with `tuning_complexity = "quick"` during development

- Use `"balanced"` (default) for most production pipelines

- Switch to `"thorough"` for final model selection

- Consider `tuning_strategy = "bayes"` instead of exhaustive grid search

- Enable `adaptive = TRUE` for early stopping of poor configurations

Use
[`print_tuning_presets`](https://selcukorkmaz.github.io/fastml/reference/print_tuning_presets.md)
to see all presets and
[`estimate_tuning_time`](https://selcukorkmaz.github.io/fastml/reference/estimate_tuning_time.md)
to estimate runtime before starting.

## Factor Level Warning

For binary classification, the interpretation of metrics like
sensitivity, specificity, and ROC AUC depends on which factor level is
treated as the "positive" class (the event of interest). The
`event_class` parameter controls this:

- `"first"` (default): The first factor level is treated as positive

- `"second"`: The second factor level is treated as positive

**Important:** Recipe preprocessing steps like `step_other()` or
`step_unknown()` can modify factor levels, potentially changing which
level is "first" or "second". Always verify factor levels after
preprocessing.

To ensure consistent behavior, explicitly set factor levels before
calling fastml:


    # Ensure "positive" is the second level (event_class = "second")
    data$outcome <- factor(data$outcome, levels = c("negative", "positive"))

    # Or ensure "positive" is the first level (event_class = "first")
    data$outcome <- factor(data$outcome, levels = c("positive", "negative"))

## Examples

``` r
# \donttest{
# Example 1: Using the iris dataset for binary classification (excluding 'setosa')
data(iris)
iris <- iris[iris$Species != "setosa", ]  # Binary classification
iris$Species <- factor(iris$Species)

# Define a custom tuning grid for the ranger engine
tune <- list(
  rand_forest = list(
    ranger = list(mtry = c(1, 3))
  )
)

# Train models with custom tuning
model <- fastml(
  data = iris,
  label = "Species",
  algorithms = "rand_forest",
  tune_params = tune,
  use_default_tuning = TRUE
)

# View model summary
summary(model)
#> 
#> ===== fastml Model Summary =====
#> Task: classification 
#> Number of Models Trained: 1 
#> 
#> -- Table 1: Model Selection (Cross-Validation) --
#> Note: This table determines the best model.
#> 
#> -------------------------------------------------------- 
#> Model         Engine  ROC AUC (CV mean)  ROC AUC (CV SD) 
#> -------------------------------------------------------- 
#> rand_forest†  ranger  0.9938             0.0198          
#> -------------------------------------------------------- 
#> † Selected based on mean ROC AUC across CV folds
#> 
#> -- Table 2: Final Evaluation (Test Set) --
#> Note: For reporting only; selection was based on CV above.
#> 
#> ------------------------------------------------------------------------------------------------------------------------- 
#> Model        Engine  Accuracy  F1 Score  Kappa  Precision  Sensitivity  Specificity  ROC AUC  Logloss  Brier Score  ECE   
#> ------------------------------------------------------------------------------------------------------------------------- 
#> rand_forest  ranger  0.900     0.889     0.800  1.000      0.800        1.000        1.000    0.166    0.049        0.118 
#> ------------------------------------------------------------------------------------------------------------------------- 
#> 
#> Best Model hyperparameters:
#> 
#> Model: rand_forest (ranger) 
#>   mtry: 1
#>   trees: 150
#>   min_n: 2
#> 
#> Tuning grid searched (values as passed to the engine):
#> 
#> rand_forest (ranger), search = grid
#>   mtry             [user]   1, 3
#>   trees            [default] 100, 150, 200
#>   min_n            [default] 2, 3, 5
#> 
#> 
#> ===========================
#> Confusion Matrices by Model
#> ===========================
#> 
#> Model: rand_forest (ranger) 
#> ---------------------------
#>             Truth
#> Prediction   versicolor virginica
#>   versicolor          8         0
#>   virginica           2        10
#> 


  # }
```
