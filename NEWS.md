# fastml version 0.7.10

## Breaking changes

* **`time_varying_cox` has been withdrawn.** It required start-stop interval input, which the evaluation path does not accept, so the model could be fitted but never scored. It no longer appears in `availableMethods("survival")`, and a request for it by name now stops with an error naming the reason and pointing to `cox_ph` and `stratified_cox`. Advertising a learner that cannot produce a result is worse than not offering it. Code that named it previously received a fitted model with no metrics.

* **Requesting resampling or tuning for `xgboost_aft` is now an error rather than a warning.** parsnip exposes no `censored regression` mode for xgboost, so the family cannot be fitted through a workflow. Previously the request warned and then fitted natively with resampling silently disabled, which returned a `fastml` object carrying no fold-level results for a user who had explicitly asked for them. The request is now refused. This is the same contract every other natively fitted survival family already followed.

* **Values in `tune_params` are now candidate values, not the endpoints of a range.** A vector of length two was previously read as an interval and interpolated across `grid_levels`, while a vector of length three or more was read as an explicit set. Nothing but the length distinguished them, so a single call could contain `mixture = c(0, 0.5, 1)` meaning three values and `min_n = c(20, 50)` meaning an interval, which is not what the code appears to say. Every value supplied through `tune_params` is now searched as written, whatever the number of values, and the parameter's range is widened where a requested value falls outside a package default. The package's own default ranges are unchanged and are still interpolated, so results obtained without a `tune_params` argument are unaffected.

* **`bootstrap_ci` now defaults to `FALSE`.** The interval it produces resamples the held-out predictions of a single fitted model, so it describes uncertainty from the evaluation sample conditional on that fit and excludes training, tuning and model-selection variability. Read as ordinary uncertainty about model performance, which is how a reader of a `.lower`/`.upper` column will read it, it overstates precision. Computing it also dominated run time, accounting for about 85% of the elapsed time of a Cox fit to the `rotterdam` data, which falls from roughly 19 seconds to roughly 2.9 seconds with it disabled, and `summary()` did not display the result unless `show_ci = TRUE` was passed, so the cost was paid on every fit for output that was not shown. Set `bootstrap_ci = TRUE` to restore the previous behaviour. With it off, `.lower` and `.upper` are `NA` and `.n_boot` is zero, so nothing in the performance table reads as an interval that was not computed. When intervals are displayed, `summary()` now prints the estimand above the table so the qualification travels with the numbers.

* **Regression results now report `rsq_trad` alongside `rsq`.** `yardstick::rsq()` is squared Pearson correlation and is invariant to location and scale, so it cannot see a prediction that is biased or mis-dispersed. `rsq_trad()` is one minus the residual sum of squares over the total sum of squares, which is the quantity most readers assume when they see an R-squared in a results table. The two agree when predictions are unbiased and correctly scaled and diverge when they are not, so the gap between them is a calibration diagnostic. Code that reads the regression metrics table by column position will need updating.

## New features

* **The tuning grid that was actually searched is recorded and printed.** The fitted object gains a `tuning_grid` component listing, per algorithm and engine, each tuned parameter, the values that were searched on the parameter's natural scale, and whether the parameter came from the user or from the package defaults. `summary()` prints it under the new `type = "grid"`, which is included in the default `type = "all"`. Reporting only the selected configuration cannot reveal a grid that differs from the one requested, because a selected value looks identical whether the user named it or the package interpolated it, which is how the scale defect below survived. `?fastml` documents the scale and the range semantics of `tune_params` explicitly rather than leaving them to be inferred.

* **`stratified_cox` strata are now named through a `strata_cols` argument.** Strata were previously identified by a naming convention, any column whose name began with `strata`. A convention that determines model structure but appears nowhere in the function signature is not discoverable, and a user who named a column `stratum_site` got an unstratified fit with no indication. `fastml(..., strata_cols = "site")` is now the documented interface; the `strata` prefix is still recognised, so existing code is unaffected. Naming a column that is not present in the data is an error, and requesting `stratified_cox` with no strata at all is an error rather than a silent skip.

## Bug fixes

* **Tuning values supplied through `tune_params` are no longer reinterpreted as logarithms.** `dials` stores quantitative parameters on their transformed scale, which for `penalty`, `learn_rate`, `loss_reduction`, `cost`, `cost_complexity` and `rbf_sigma` is base-10 logarithmic. User-supplied values were passed to `dials::range_set()` on the natural scale without being mapped, so `learn_rate = c(0.05, 0.1)` was fitted as `10^0.05 = 1.122` to `10^0.1 = 1.259`, and `penalty = c(0.01, 0.1)` as `1.023` to `1.259`. Nothing in the output reported the substitution. For boosting the consequence is severe, since a learning rate above one overshoots at every step: on an NHANES blood-pressure regression the fitted LightGBM predictions had 1.43 times the standard deviation of the outcome and spanned 36 to 294 mmHg, giving an RMSE of 23.1 against 17.3 for predicting the mean. The reported `rsq` of 0.19 did not reveal this, because `yardstick::rsq()` is squared Pearson correlation and is invariant to location and scale; the traditional R-squared was -0.79. Values supplied by the user are now mapped through the parameter's own transform, and a value outside a transformed parameter's domain is refused with an error naming the parameter. Values are now mapped through the parameter's own transform before they reach `dials`, and a negative value for a positive-domain parameter is refused with an error naming the parameter. A requested zero, which is meaningful for `loss_reduction` but has no logarithm, maps to the parameter's lower bound and the resulting value is visible in the recorded grid.

  The same route is taken by the fixed defaults from `get_default_params()`, which are also written on the natural scale and were also being exponentiated. A user who supplied any `tune_params` for LightGBM without naming `learn_rate` was given a learning rate of `10^0.1 = 1.2589` in place of the intended default of `0.1`. Only the default *tuning ranges* from `get_default_tune_params()` are written on the scale `dials` stores, and those are left alone, so results obtained without a `tune_params` argument are unaffected. Regression tests are in `tests/testthat/test-tuning-grid-scale.R`.

* **Default `loss_reduction` ranges no longer search implausible values.** The default range was written as `c(0, 5)` and read as log10, so the searched values were 1, 316 and 100000 rather than the documented [0, 5]. The corresponding entry in the wider registry, `c(0, 10)`, searched up to 1e10. Both are now written on the scale the registry uses and search the documented range. Selection is by cross-validation, so a degenerate grid point was never chosen; the effect was a wasted search budget and a default grid that never explored small values.

* **Candidate values are no longer dropped when they outnumber `grid_levels`.** `dials::grid_regular()` takes at most `levels` values per parameter, and `grid_levels` defaults to three, so a user supplying four candidate values silently lost one. Levels are now raised per parameter to the number of values requested, leaving parameters at their defaults untouched so the grid does not grow except where asked.

* **Default tuning ranges are reachable for survival tasks.** `get_default_tune_params()` dropped the outcome columns with `sym()`, which accepts a single symbol, so a survival label naming two or three columns raised "Can't convert a character vector to a symbol" and no default range was ever returned for any survival family. The columns are now dropped by name. Note that `penalized_cox` has no default range and fits at a fixed penalty of 0.01 with a pure lasso mixture; its penalty is not selected from the data, and a user wanting selection should supply candidates through `tune_params` or call `glmnet::cv.glmnet()` directly.

* **Probability metrics no longer return `NA` inside a metric set.** `yardstick::metric_set()` calls its members with `na_rm` and `case_weights`, and passes `event_level` to metrics that accept it. The wrappers for `roc_auc`, `logloss`, `brier_score` and `ece` did not declare those as formals, so they arrived in `...`, where the wrappers read `...` as a selection of probability columns. Each metric then returned `NA` rather than erroring. All four failed together while the six class metrics, whose wrappers already declared these arguments, were unaffected. The four wrappers now declare `na_rm`, `event_level` and `case_weights`, and an `event_level` supplied by the caller takes precedence over the one captured when the metric was configured. Results obtained without tuning are unchanged, including every figure in the article's case studies, because the affected call path is the one `tune_grid()` uses.

* **Nested cross-validation no longer degrades silently when tuning is requested.** Because `roc_auc` is the default selection metric and probability metrics were empty under tuning (above), `select_best()` could not choose a configuration. The unfinalized workflow was then fitted anyway, so `tune()` placeholders reached the engine and the failure surfaced as a message about a hyperparameter, for example "Invalid value for num.trees". Every outer fold failed, the run fell back to ordinary resampling, and the returned object recorded `selection$source` as `"none"` with no indication that no nested estimate had been produced. Nested cross-validation consequently worked only when it was not tuning, which is the one case in which it has no purpose. Three changes fix this. Probability metrics compute (above); an outer split whose inner tuning selects nothing is now skipped with a warning naming the selection metric, rather than fitting a workflow that still contains `tune()` markers; and configuration selection now identifies the configuration by `.config` and recovers its hyperparameter values from rows that carry them, since `tune` attaches those columns only to the class-metric rows.

* **The nested selection rule is documented and reported.** The configuration carried into the returned model is the one with the best mean inner-fold score for the primary metric, pooled across all outer folds, with ties broken by smaller standard error and then a deterministic ordering. It is neither the modal winner across outer folds nor the configuration attached to the best outer score. `summary()` now prints, alongside a nested estimate, that the figure describes the tuning and fitting procedure rather than the returned model.

* **A numeric stratification column no longer discards the entire sample.** The `stratified_cox` path read the stratifier's *values* from the preprocessed frame but its *levels* from the training frame. Any recipe step that rewrites the column's values, `step_normalize()` being the ordinary case, therefore produced a factor whose levels matched nothing, every row became `NA`, and `coxph()` stopped with "No (non-missing) observations". A factor stratifier is untouched by normalisation and so was unaffected, which is why the defect survived hand testing. The stratifier is now read from the training frame, since a grouping label is not a modelled covariate and must not be transformed. If preprocessing changes the number of training rows, the alignment is now refused with an error rather than attempted.

* **`royston_parmar` no longer fails when `rstpm2` is not attached.** `rstpm2::stpm2()` resolves an internal helper from the search path, so calling it through the namespace failed with an object-not-found error whenever the user had not attached the package. The fit now runs inside `withr::with_package("rstpm2", ...)`, which attaches the package for the duration of the call and restores the search path afterwards. The attach is a no-op when the package is already attached. This is a side effect on global state, and it is documented as such; the fit is native and unresampled, so it does not execute in a parallel worker.

* **A survival registry test suite.** `tests/testthat/test-survival-registry.R` asserts that every family in `availableMethods("survival")` fits on a canonical right-censored frame under a holdout, and that every combination the documentation records as unavailable raises an error rather than degrading silently. All four defects above were found by hand and would all have been caught by these tests.

* **XGBoost AFT interval bounds are now supplied on the original time scale (breaking for results, not for the API).** The native `xgboost_aft` path passed `log(time)` as `label_lower_bound` and `label_upper_bound`. XGBoost's AFT objective models `log(Y) = T(x) + sigma * Z` and applies the logarithm to those bounds internally, so the previous code applied the transformation twice and optimised the wrong likelihood. The effect was silent on data whose survival times all exceed one, where it degraded the fit, and fatal below one, where the doubly-logged bound is undefined and every risk score came back non-finite. On a simulated dataset with 351 of 400 times below one, the previous code produced 0 of 400 usable risk scores; it now produces 400 of 400 with a concordance of 0.92. Concordance on the `lung`, `rotterdam` and `flchain` benchmarks changes by roughly 0.03, 0.03 and 0.10 respectively. Any previously reported `xgboost_aft` result should be regenerated. Regression tests pinning the bounds against a direct xgboost reference are added in `tests/testthat/test-xgboost-aft-bounds.R`.

* **The redundant `seed` parameter is no longer passed to `xgb.train()`.** The R xgboost package ignores a `seed` entry in `params` and warns about it on every fit, so survival runs emitted a spurious warning. Seeding is and was handled by `fastml_with_seed()`, which calls `set.seed()` around the fit, so predictions are unchanged.

* **`base_score` is now anchored to the observed time scale for XGBoost AFT.** XGBoost initialises the AFT margin at `log(base_score)`, whose default of `0.5` sits far below survival times measured in days or months. With the logistic AFT loss the objective saturates at that distance, the gradients underflow, no tree splits, and every prediction is identical. The previous `log(time)` bounds masked this by placing the labels near the default starting point. `base_score` is now set to the median observed time, which starts the optimiser on the scale of the data. Without it, concordance on `lung` and `rotterdam` collapses to exactly 0.5.

* **Grouped and time-ordered holdout constraints are now enforced together.** Supplying both `block_col` and `group_cols` previously warned that grouping would be ignored and cut the holdout purely by time, so a group could contribute rows to both the training and the test set — the exact contamination `group_cols` is meant to prevent. The holdout is now cut at the admissible point nearest the requested `test_size`, where a point is admissible only if no group spans it, so every training row precedes every test row *and* no group appears on both sides. Because such a point need not fall at the requested proportion, the realized test fraction is reported through a warning when it differs by more than five percentage points. When groups are interleaved in time no admissible point exists, and `fastml()` now stops with an explanatory error instead of silently dropping a guarantee. Fold construction under `blocked_cv` and `rolling_origin` remains purely positional and is unaffected.

* **Continuous integration and a prediction-orientation test suite.** The package now runs `R CMD check` on GitHub Actions across macOS, Windows, and Linux on R release, devel, and oldrel-1, with `NOT_CRAN=true` so the suite that `skip_on_cran()` withholds from CRAN is exercised on every push; a second workflow reports test coverage. A new `tests/testthat/test-prediction-orientation.R` pins the direction of the prediction scale for all seven survival engines (`cox_ph`, `survreg`, `penalized_cox`, `parametric_surv`, `piecewise_exp`, `rand_forest_survival`, `xgboost_aft`), for classification probabilities, and for regression predictions. Survival engines use conflicting sign conventions -- a Cox linear predictor is already a risk, AFT models predict log-time and must be negated, and a survival curve must be converted -- so an inverted conversion returns finite, plausible-looking numbers while ranking subjects backwards. That is the failure the 0.7.10 xgboost AFT scale fix produced, and it is now caught by assertion rather than by inspection.

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
