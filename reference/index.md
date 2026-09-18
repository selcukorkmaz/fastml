# Package index

## Fit and compare

The main entry points for training and benchmarking models.

- [`fastml()`](https://selcukorkmaz.github.io/fastml/reference/fastml.md)
  : Fast Machine Learning Function
- [`train_models()`](https://selcukorkmaz.github.io/fastml/reference/train_models.md)
  : Train Specified Machine Learning Algorithms on the Training Data
- [`availableMethods()`](https://selcukorkmaz.github.io/fastml/reference/availableMethods.md)
  : Get Available Methods
- [`process_model()`](https://selcukorkmaz.github.io/fastml/reference/process_model.md)
  : Process and Evaluate a Model Workflow

## Explore and explain

Data exploration before fitting, and model-agnostic explanation
afterwards (SHAP, LIME, ALE, ICE, surrogates, counterfactuals).

- [`fastexplore()`](https://selcukorkmaz.github.io/fastml/reference/fastexplore.md)
  : Lightweight exploratory helper
- [`fastexplain()`](https://selcukorkmaz.github.io/fastml/reference/fastexplain.md)
  : Explain a fastml model using various techniques
- [`explain_dalex()`](https://selcukorkmaz.github.io/fastml/reference/explain_dalex.md)
  : Generate DALEX explanations for a fastml model
- [`explain_lime()`](https://selcukorkmaz.github.io/fastml/reference/explain_lime.md)
  : Generate LIME explanations for a fastml model
- [`explain_ale()`](https://selcukorkmaz.github.io/fastml/reference/explain_ale.md)
  : Compute Accumulated Local Effects (ALE) for a fastml model
- [`explain_stability()`](https://selcukorkmaz.github.io/fastml/reference/explain_stability.md)
  : Analyze Feature Importance Stability Across Cross-Validation Folds
- [`plot_ice()`](https://selcukorkmaz.github.io/fastml/reference/plot_ice.md)
  : Plot ICE curves for a fastml model
- [`surrogate_tree()`](https://selcukorkmaz.github.io/fastml/reference/surrogate_tree.md)
  : Fit a surrogate decision tree for a fastml model
- [`counterfactual_explain()`](https://selcukorkmaz.github.io/fastml/reference/counterfactual_explain.md)
  : Generate counterfactual explanations for a fastml model
- [`interaction_strength()`](https://selcukorkmaz.github.io/fastml/reference/interaction_strength.md)
  : Compute feature interaction strengths for a fastml model

## Predict

- [`predict(`*`<fastml>`*`)`](https://selcukorkmaz.github.io/fastml/reference/predict.fastml.md)
  : Predict method for fastml objects
- [`predict_model.model_fit()`](https://selcukorkmaz.github.io/fastml/reference/predict_model.model_fit.md)
  : Internal predict_model method for parsnip fits
- [`predict_risk()`](https://selcukorkmaz.github.io/fastml/reference/predict_risk.md)
  : Predict Risk Scores from a Survival Model
- [`predict_survival()`](https://selcukorkmaz.github.io/fastml/reference/predict_survival.md)
  : Predict survival probabilities from a survival model

## Inspect results

Summaries, plots, and accessors for a fitted fastml object.

- [`summary(`*`<fastml>`*`)`](https://selcukorkmaz.github.io/fastml/reference/summary.fastml.md)
  : Summary Function for fastml (Using yardstick for ROC Curves)

- [`plot(`*`<fastml>`*`)`](https://selcukorkmaz.github.io/fastml/reference/plot.fastml.md)
  :

  Plot Methods for `fastml` Objects

- [`plot(`*`<fastml_stability>`*`)`](https://selcukorkmaz.github.io/fastml/reference/plot.fastml_stability.md)
  : Plot method for fastml_stability objects

- [`print(`*`<fastml_stability>`*`)`](https://selcukorkmaz.github.io/fastml/reference/print.fastml_stability.md)
  : Print method for fastml_stability objects

- [`get_best_model_idx()`](https://selcukorkmaz.github.io/fastml/reference/get_best_model_idx.md)
  : Get Best Model Indices by Metric and Group

- [`get_best_model_names()`](https://selcukorkmaz.github.io/fastml/reference/get_best_model_names.md)
  : Get Best Model Names

- [`get_best_workflows()`](https://selcukorkmaz.github.io/fastml/reference/get_best_workflows.md)
  : Get Best Workflows

- [`fastml_compute_holdout_results()`](https://selcukorkmaz.github.io/fastml/reference/fastml_compute_holdout_results.md)
  : Evaluate Models Function

## Defaults and tuning configuration

Inspect and control the engine defaults and tuning grids fastml applies,
including where they differ from the parsnip defaults.

- [`compare_defaults()`](https://selcukorkmaz.github.io/fastml/reference/compare_defaults.md)
  : Compare fastml and parsnip defaults
- [`get_default_differences()`](https://selcukorkmaz.github.io/fastml/reference/get_default_differences.md)
  : Get All Default Differences Summary
- [`print_default_differences()`](https://selcukorkmaz.github.io/fastml/reference/print_default_differences.md)
  : Print Default Differences Table
- [`get_default_engine()`](https://selcukorkmaz.github.io/fastml/reference/get_default_engine.md)
  : Get Default Engine
- [`get_default_params()`](https://selcukorkmaz.github.io/fastml/reference/get_default_params.md)
  : Get Default Parameters for an Algorithm
- [`get_default_params_with_warnings()`](https://selcukorkmaz.github.io/fastml/reference/get_default_params_with_warnings.md)
  : Get Default Parameters with Transparency Warnings
- [`get_default_tune_params()`](https://selcukorkmaz.github.io/fastml/reference/get_default_tune_params.md)
  : Get Default Tuning Parameters
- [`get_engine_names()`](https://selcukorkmaz.github.io/fastml/reference/get_engine_names.md)
  : Get Engine Names from Model Workflows
- [`get_expanded_tune_params()`](https://selcukorkmaz.github.io/fastml/reference/get_expanded_tune_params.md)
  : Expanded Default Tuning Parameters
- [`get_model_engine_names()`](https://selcukorkmaz.github.io/fastml/reference/get_model_engine_names.md)
  : Get Model Engine Names
- [`get_parsnip_default_engine()`](https://selcukorkmaz.github.io/fastml/reference/get_parsnip_default_engine.md)
  : Get Parsnip Default Engine for an Algorithm
- [`get_parsnip_default_params()`](https://selcukorkmaz.github.io/fastml/reference/get_parsnip_default_params.md)
  : Get Parsnip Default Parameters for an Algorithm
- [`get_tuning_complexity()`](https://selcukorkmaz.github.io/fastml/reference/get_tuning_complexity.md)
  : Tuning Complexity Presets
- [`get_tuning_params_for_complexity()`](https://selcukorkmaz.github.io/fastml/reference/get_tuning_params_for_complexity.md)
  : Get Tuning Parameters for Complexity Level
- [`estimate_tuning_time()`](https://selcukorkmaz.github.io/fastml/reference/estimate_tuning_time.md)
  : Estimate Tuning Time
- [`recommend_tuning_config()`](https://selcukorkmaz.github.io/fastml/reference/recommend_tuning_config.md)
  : Recommend Tuning Configuration
- [`print_tuning_presets()`](https://selcukorkmaz.github.io/fastml/reference/print_tuning_presets.md)
  : Print Tuning Presets Summary
- [`tuning_config`](https://selcukorkmaz.github.io/fastml/reference/tuning_config.md)
  : Tuning Configuration and Complexity Presets
- [`defaults_registry`](https://selcukorkmaz.github.io/fastml/reference/defaults_registry.md)
  : Defaults Registry for Engine and Parameter Transparency
- [`validate_defaults_registry()`](https://selcukorkmaz.github.io/fastml/reference/validate_defaults_registry.md)
  : Validate Defaults Registry Against Parsnip
- [`warn_default_override()`](https://selcukorkmaz.github.io/fastml/reference/warn_default_override.md)
  : Warn About Default Overrides
- [`reset_default_warnings()`](https://selcukorkmaz.github.io/fastml/reference/reset_default_warnings.md)
  : Reset Default Override Warnings

## Save and load

- [`save_fastml()`](https://selcukorkmaz.github.io/fastml/reference/save_fastml.md)
  : Save Model Function
- [`save.fastml()`](https://selcukorkmaz.github.io/fastml/reference/save.fastml.md)
  : Save Model Function (Deprecated)
- [`load_model()`](https://selcukorkmaz.github.io/fastml/reference/load_model.md)
  : Load Model Function
