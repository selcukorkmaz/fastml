# fastml version 0.7.9

## New features

* **`n_cores` and engine threads are now separate**: `n_cores` previously sized both the parallel worker pool *and* the thread count passed to engines that accept one, so a request for `k` cores could demand up to `k^2` threads and contend with itself on engines that thread aggressively. `n_cores` now controls the worker pool only, and a new `engine_threads` argument (default `1`) controls the per-engine thread count for `num.threads` (ranger), `num_threads` (LightGBM), and `nthread` (xgboost). Total CPU demand is therefore the product the user specifies. Behaviour at the defaults (`n_cores = 1`, `engine_threads = 1`) is unchanged; a previous `n_cores = k` run is now written `n_cores = k, engine_threads = k` if the old thread allocation is wanted. Determinism warnings for multithreaded engines are keyed to `engine_threads` rather than `n_cores`.

## Bug fixes

* **Prediction no longer falls back to unpreprocessed data**: in `process_model()`, a failure to apply a native survival model's stored recipe to the evaluation data silently returned the raw, unbaked data, so the model predicted on features it was not trained on and reported plausible but meaningless metrics. The failure is now fatal, with an error naming the model and the underlying cause.
* **Engine defaults are no longer discarded when engine arguments are applied**: `parsnip::set_engine()` replaces the engine arguments already attached to a specification rather than adding to them, so the second application in `train_models()` silently dropped every default the spec builder had set — LightGBM's `counts`, `bagging_freq` and `verbose`, an xgboost early-stopping configuration (`early_stop`, `validation`), and `sparsediscrim`'s `regularization_method`. Arguments are now merged through the new internal `fastml_merge_engine_args()`, with pipeline values winning only on the keys they define. Case Study B's reported metrics are unchanged, since the dropped LightGBM arguments were inert for that configuration; the defect is live wherever `mtry` or `sample_size` are tuned for LightGBM, xgboost early stopping is requested, or `sparsediscrim` is used.
* **Parallel execution restored**: `fastml_setup_parallel()` called `doFuture::registerDoFuture(flavor = "%dofuture%")`, but `registerDoFuture()` accepts no arguments, so any call with `n_cores > 1` failed with "unused argument". The argument has been dropped. Parallel execution was unusable in 0.7.8 with current doFuture.
* **Parallel cleanup no longer corrupts foreach**: when no foreach backend was registered before a `fastml()` call, the previous backend's `data`/`info` hooks are `NULL`, and passing them back to `foreach::setDoPar()` on exit left `foreach::getDoParName()` erroring for the rest of the session. The sequential default is now registered instead.
* **Silent preprocessing fallbacks removed package-wide**: the same "fall back to unprocessed data" pattern appeared in eight further places. On prediction paths (`predict_risk()` and `predict_survival()` for native survival fits) it is now fatal via the shared helper `fastml_bake_or_abort()`. Where aborting would discard an otherwise complete result — assembling `processed_test_data` in `fastml()`, and the explanation utilities (`explain_ale()`, `explain_stability()`, `interaction_strength()`, `plot_ice()`, `surrogate_tree()`) — the fallback remains but now warns and states that the output describes raw rather than preprocessed features.
* **Risk-prediction errors are no longer discarded**: the error handler around the survival risk-prediction block dropped the original condition, leaving users with NA risk scores and no diagnostic. The original message is now surfaced as a warning; if the fallback prediction path also fails, both messages are reported.
* **Recipe scanner now detects embedded external data**: `fastml_detect_leaky_recipe_steps()` tests for `data.frame`/`tbl_df` components before the generic list branch. Data frames are themselves lists, so the previous ordering recursed into their columns and the check was unreachable; a custom recipe step carrying an external lookup table was silently accepted. Such steps are now flagged and training is aborted.
* **Pretrained recipes are now actually rejected**: `fastml_validate_user_recipe()` determined trained state from `recipe$trained`, a field prepped recipes do not carry, so the check never fired. Trained state is now read via `recipes::fully_trained()`, with the `tr_info` training-set record as a fallback for prepped recipes without steps.

## Improvements

* **Parallel path is now tested outside CRAN gating**: the two parallel defects above shipped precisely because the only test touching that path was CRAN-gated. A new `tests/testthat/test-parallel-setup.R` covers `fastml_setup_parallel()` directly — backend registration, plan and `future.seed` restoration, recovery of a caller-registered backend, and the `n_cores`/`engine_threads` separation — and runs in an ordinary CRAN check, costing roughly four seconds because it exercises the helper rather than fitting models. Reintroducing each defect was confirmed to make these tests fail. The end-to-end multicore run stays behind `skip_on_cran()`, since it spends about twenty seconds on multisession worker startup to reach the same defects.
* **Survival execution path is announced**: under `verbose = TRUE`, `train_models()` now reports whether each algorithm is fitted through a parsnip workflow (the guarded resampling path) or through its native engine, so users can tell which path a given method took rather than inferring it.
* **Tuning finalization extracted**: the block in `process_model()` that selected the best configuration from a `tune_results` object (with metric fallback) and refit the finalized workflow is now the internal helper `finalize_tuned_model()`. Behaviour is unchanged, including the `NULL`-with-warning return when no configuration can be selected.
* **Process model test coverage**: added `tests/testthat/test-process-model.R` covering the recipe-application abort contract, its diagnostic message, `finalize_tuned_model()`'s graceful `NULL` return, and a clean end-to-end native survival evaluation.
* **Recipe guard test coverage**: added `tests/testthat/test-security-guards.R` with behavioural tests for the recipe scanner, covering safe-listed and non-safe-listed standard steps (false-positive checks), embedded data frames and tibbles, global-environment and parent-frame references, abort-on-rejection behaviour, and pretrained/non-recipe input.

---

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
