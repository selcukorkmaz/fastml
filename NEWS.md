# fastml version 0.7.8

## New features

* **Validation Split Resampling**: Added `resampling_method = "validation_split"` to `fastml()` and `train_models()`. The holdout proportion is derived from `folds` as `1 - 1 / folds`, with stratification support where applicable.
* **Explicit Save Helper**: Added exported `save_fastml()` as the primary helper for persisting fitted `fastml` objects.

## Improvements

* **Nested CV Parameter Tracking**: Improved nested cross-validation selection so the chosen outer split better follows the final hyperparameter configuration selected from inner results.
* **Survival Holdout Plumbing**: Holdout evaluation now forwards survival-specific column metadata (`start_col`, `time_col`, `status_col`) through the evaluation path.
* **Prediction Model Validation**: `predict.fastml()` now treats native survival and Royston-Parmar model objects as valid prediction targets when flattening and selecting fitted models.
* **Safer Task Detection**: Survival auto-detection now ignores missing status values when checking for two-level event coding, and numeric auto-detection only upgrades clearly binary numeric outcomes to classification.
* **RNG State Restoration**: `fastml()`, `train_models()`, and bootstrap confidence interval computations now restore the caller's `.Random.seed` after execution.
* **Documentation Updates**: Expanded documentation for `folds`, `flatten_and_rename_models()`, and `get_best_model_idx()` for clearer usage and cleaner package checks.

## Bug fixes

* Fixed `event_class` validation in both `fastml()` and `train_models()` so invalid values are rejected consistently.
* Fixed multiclass handling so `logistic_reg` is converted to `multinom_reg` before the training loop, avoiding per-iteration mutation and preserving engine parameter transfer.
* Fixed discriminant model specification helpers to use `parsnip::discrim_linear()` and `parsnip::discrim_quad()`, resolving dependency warnings caused by referencing unexported `discrim` objects.
* Fixed default engine resolution by removing duplicate switch entries for survival algorithms such as `survreg` and `royston_parmar`.
* Removed package-owned restoration of deleted objects into `.GlobalEnv` inside sandboxed preprocessing guards, resolving the corresponding `R CMD check` NOTE about global environment assignments.
* Deprecated `save.fastml()` in favour of `save_fastml()` to avoid confusion with a non-generic S3-style naming pattern.
* Removed dead internal statements in model evaluation and selection paths, including unused holdout label handling and stray performance-value expressions.
* Added missing Rd argument documentation for `flatten_and_rename_models()` and `get_best_model_idx()`, resolving `R CMD check` `\usage` warnings.
* Added regression tests covering event class validation, engine lookup, nested CV parameter selection, multiclass algorithm swapping, and sandbox global-environment protections.

---

# fastml version 0.7.7

## New features

* **Feature Importance Stability Analysis**: Added `explain_stability()` function to analyze feature importance stability across cross-validation folds. This helps identify features that are consistently important vs. those whose importance varies across different data subsets.
* **Store Fold Models**: Added `store_fold_models` parameter to `fastml()` to optionally store models trained on each CV fold, enabling stability analysis with `explain_stability()`.
* **S3 Methods for Stability Objects**: Added `print.fastml_stability()` and `plot.fastml_stability()` methods for convenient display of stability analysis results.

## Improvements

* **Unified Explainer Infrastructure**: Added `fastml_prepare_explainer_inputs()` helper function providing consistent data preparation across all explainer methods (`explain_dalex()`, `explain_ale()`, `plot_ice()`, `interaction_strength()`, `surrogate_tree()`).
* **Positive Class Resolution**: Added `resolve_positive_class()` helper for consistent positive class handling across explainer functions, respecting `event_class` settings.
* **Enhanced `explain_dalex()`**: Major refactoring with robust preprocessing ("baking") helper that handles three scenarios: no preprocessor, successful baking, and fallback validation for already-processed data.
* **Enhanced `plot_ice()`**: Added `target_class` parameter for classification, improved feature validation with informative error messages, and added warnings for multiclass problems.
* **Improved Resampling Metrics Aggregation**: Resampling results now properly compute CV statistics (mean and SD across folds) instead of pooled metrics. Fixed grouping attributes that could carry over from fold processing.
* **Better Model Validation in Predictions**: Added `valid_model()` helper to properly validate workflow and native survival model types during prediction.

## Bug fixes

* Fixed algorithm name matching in `predict.fastml()` to correctly resolve base algorithm names to their full "algorithm (engine)" format.
* Fixed fold metrics aggregation in guarded resampling to properly ungroup and convert results to plain tibbles.
* Fixed various edge cases in explainer functions when preprocessing pipelines are absent or data is already processed.
* Fixed unit tests across multiple test files for improved reliability and stability.
* Prevented `Rplots.pdf` files from being created during test execution by adding graphics device suppression to plotting tests.
* Added `Rplots.pdf` to `.gitignore` to prevent accidental tracking.

---

# fastml version 0.7.5

## Breaking changes

* Removed incomplete or unstable survival backends where correct, leakage-safe behavior could not be guaranteed.

## New features

* **Full Survival Analysis Support**: Added training, resampling, prediction, metric computation, and model summarization for time-to-event outcomes.
* **Guarded Survival Resampling**: Introduced a workflow enforcing leakage-safe preprocessing, imputation, and model fitting within each resampling split.
* **Integrated Brier Score (IBS)**: Added IBS and expanded survival metric support with flexible time handling and user-configurable summary outputs.
* **New Survival Engines**: Added support for parametric and semi-parametric models, including Cox, penalized Cox, Royston–Parmar, and flexible parametric survival models.
* **Advanced Resampling Strategies**: Implemented grouped, blocked, rolling, stratified, and unbiased nested cross-validation.
* **Fold-wise Imputation**: Added support for advanced imputation during resampling while preventing outcome leakage.
* **Engine Parameters**: Introduced an `engine_params` argument to allow passing engine-specific options in a consistent way.
* **S3 Methods**: Added explicit S3 method annotations for `fastml` generics.

## Improvements

* Multiclass ROC AUC now defaults to macro averaging (tidymodels) and can be configured via `multiclass_auc` to use macro_weighted class-prevalence weighting.
* Improved robustness of survival predictions, including risk scores, survival probabilities, quantiles, medians, and time estimates.
* Enhanced survival summary outputs with clearer metric alignment and better handling of stratified and time-varying Cox models.
* Improved extraction of predictions and summaries for parametric survival engines.
* Strengthened recipe validation and sandboxing to harden preprocessing isolation and reduce user-induced leakage.
* Improved handling of novel and missing categorical levels during prediction.
* Integrated resampling metadata more tightly into training workflows and summaries.
* Added `survival_metric_convention` to align survival evaluation defaults with tidymodels conventions when desired.
* Parallel tuning now uses explicit RNG seeding to keep results stable across core counts.

## Bug fixes

* Fixed multiple issues in survival label validation, prediction post-processing, and metric computation.
* Corrected survival risk and probability calculations for several engines and model types.
* Fixed log-rank calculation for time-varying Cox models.
* Fixed summary formatting when confidence intervals are unavailable.
* Removed inappropriate confusion matrix warnings for non-classification tasks.
* Fixed edge cases leading to `NA` survival predictions and early exits during survival time computation.
* Addressed naming collisions and alignment issues in tuning grids and metric selection.
