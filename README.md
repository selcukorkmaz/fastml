<img src="man/figures/fastml_hex.png" align="right" width="85"/>

# fastml: Guarded Resampling Workflows for Safe and Automated Machine Learning in R

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/fastml)](https://CRAN.R-project.org/package=fastml)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/fastml)](https://CRAN.R-project.org/package=fastml)
[![CRAN monthly](https://cranlogs.r-pkg.org/badges/fastml)](https://CRAN.R-project.org/package=fastml)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R](https://img.shields.io/badge/R%20%3E%3D-4.1.0-blue.svg)](https://cran.r-project.org/)
<!-- badges: end -->

**Guarded Resampling Workflows for Safe and Automated Machine Learning in R**

fastml is an R package for training, evaluating, and comparing machine learning models with a guarded resampling workflow. Rather than introducing new learning algorithms, fastml focuses on **reducing leakage risk** by keeping preprocessing, model fitting, and evaluation aligned within supported resampling paths.

In fastml, *fast* refers to the rapid construction of statistically valid workflows, not to computational shortcuts. By eliminating entire classes of user-induced errors — most notably preprocessing leakage — fastml allows practitioners to obtain reliable performance estimates with minimal configuration.

## Core Principles

- **Guarded resampling workflow** — When the guarded resampling path is used, preprocessing and model fitting are re-estimated independently within each resampling split. This reduces leakage risk, but does not prevent users from supplying preprocessed inputs.

- **Leakage risk reduction, not guarantees** — fastml can mitigate common leakage modes (e.g., global scaling or imputation before resampling) when workflows are fit within resamples. It does not universally prevent all leakage scenarios.

- **Single, unified interface** — Multiple models can be trained and benchmarked through a single call, while internally coordinating resampling, preprocessing, and evaluation in supported paths.

- **Compatibility with established engines** — fastml orchestrates existing modeling infrastructure (recipes, rsample, parsnip, yardstick) without modifying their statistical behavior.

## Installation

### From CRAN

```r
install.packages("fastml")
```

### From GitHub (development version)

```r
# install.packages("devtools")
devtools::install_github("selcukorkmaz/fastml")
```

### Optional Dependencies

fastml uses a lightweight core. Feature-specific packages are installed only when needed:

```r
# Explainability (SHAP, LIME, ALE, ICE, etc.)
install.packages(c("DALEX", "iml", "lime", "iBreakDown", "pdp"))

# Additional model engines
install.packages(c("ranger", "glmnet", "kernlab", "kknn", "lightgbm", "C50"))

# Discriminant analysis and bagged trees
install.packages(c("discrim", "baguette"))

# Survival analysis extensions
install.packages(c("censored", "flexsurv", "rstpm2", "survRM2", "aorsf"))

# Fairness and interactive dashboards
install.packages(c("fairmodels", "modelStudio"))
```

## Quick Start

```r
library(fastml)
library(dplyr)

data(iris)

iris_binary <- iris %>%
  filter(Species != "setosa") %>%
  mutate(Species = factor(Species))

# Train and evaluate models in one call
fit <- fastml(
  data = iris_binary,
  label = "Species",
  algorithms = c("rand_forest", "logistic_reg")
)

# Inspect results
summary(fit)

# Visualize
plot(fit, type = "bar")
plot(fit, type = "roc")
plot(fit, type = "calibration")

# Predict on new data
predict(fit, newdata = iris_binary[1:5, ])
```

## Supported Tasks

fastml auto-detects the task type based on the target variable:

| Task | Detection | Metrics |
|------|-----------|---------|
| **Classification** | Factor or character target (binary/multiclass) | Accuracy, ROC AUC, Sensitivity, Specificity, Precision, F1, Kappa, Logloss |
| **Regression** | Numeric target | RMSE, R-squared, MAE |
| **Survival** | Two-column label (time + status) | C-index, Integrated Brier Score, time-dependent Brier, RMST |

## Supported Algorithms

### Classification

| Algorithm | Default Engine | Alternative Engines |
|-----------|---------------|---------------------|
| `logistic_reg` | glm | glmnet, brulee, stan, keras, LiblineaR |
| `multinom_reg` | nnet | glmnet, brulee, keras |
| `rand_forest` | ranger | randomForest, h2o, partykit |
| `xgboost` | xgboost | — |
| `lightgbm` | lightgbm | — |
| `decision_tree` | rpart | C5.0, partykit |
| `bag_tree` | rpart | — |
| `svm_rbf` | kernlab | — |
| `svm_linear` | kernlab | LiblineaR |
| `nearest_neighbor` | kknn | — |
| `naive_Bayes` | klaR | naivebayes, h2o |
| `mlp` | nnet | brulee, keras, h2o |
| `discrim_linear` | MASS | — |
| `discrim_quad` | sparsediscrim | — |

### Regression

| Algorithm | Default Engine | Alternative Engines |
|-----------|---------------|---------------------|
| `linear_reg` | lm | — |
| `ridge_reg` | glmnet | — |
| `lasso_reg` | glmnet | — |
| `elastic_net` | glmnet | — |
| `rand_forest` | ranger | randomForest, h2o |
| `xgboost` | xgboost | — |
| `lightgbm` | lightgbm | — |
| `decision_tree` | rpart | — |
| `svm_rbf` | kernlab | — |
| `svm_linear` | kernlab | — |
| `nearest_neighbor` | kknn | — |
| `mlp` | nnet | brulee, keras, h2o |
| `pls` | mixOmics | — |
| `bayes_glm` | stan | — |

### Survival

| Algorithm | Default Engine |
|-----------|---------------|
| `cox_ph` | survival |
| `penalized_cox` | glmnet |
| `stratified_cox` | survival |
| `time_varying_cox` | survival |
| `survreg` | survival |
| `rand_forest` | aorsf / ranger |
| `xgboost` / `xgboost_aft` | xgboost |
| `parametric_surv` | flexsurv |
| `piecewise_exp` | flexsurv |
| `royston_parmar` | rstpm2 |

## Resampling Methods

```r
fit <- fastml(
  data = df,
  label = "target",
  resampling_method = "cv",  # default
  cv_folds = 10
)
```

| Method | Description |
|--------|-------------|
| `cv` | K-fold cross-validation (default) |
| `repeatedcv` | Repeated cross-validation |
| `boot` | Bootstrap resampling |
| `grouped_cv` | Grouped cross-validation (keeps groups intact) |
| `blocked_cv` | Blocked/time-series CV (respects temporal ordering) |
| `rolling_origin` | Rolling window resampling |
| `nested_cv` | Nested cross-validation (unbiased tuning) |
| `validation_split` | Simple train/validation split |
| `none` | No resampling (single holdout) |

## Hyperparameter Tuning

```r
fit <- fastml(
  data = iris_binary,
  label = "Species",
  algorithms = c("rand_forest", "xgboost"),
  tuning_strategy = "bayes",
  tuning_complexity = "balanced",
  tuning_iterations = 25
)
```

| Strategy | Description |
|----------|-------------|
| `grid` | Grid search (default) |
| `bayes` | Bayesian optimization |
| `none` | No tuning, use defaults |

**Tuning complexity presets** control search breadth:

| Preset | Grid Levels | Use Case |
|--------|------------|----------|
| `quick` | 2 | Prototyping, debugging |
| `balanced` | 3 | Most production use (default) |
| `thorough` | 5 | Final model selection, publications |
| `exhaustive` | 7 | Research, competitions |

## Preprocessing

fastml isolates preprocessing inside the resampling loop to prevent leakage:

```r
fit <- fastml(
  data = df,
  label = "target",
  impute_method = "knnImpute",   # medianImpute, bagImpute, remove, error
  scale = c("center", "scale"),
  balance = "upsample"           # downsample, or none (default)
)
```

Transformations applied per fold: imputation, dummy encoding, centering, scaling, zero-variance removal, novel/unknown level handling.

## Explainability

Model explainability is provided through `fastexplain()` with 10 methods:

```r
# DALEX-based: variable importance, SHAP values, partial dependence
fastexplain(fit, method = "dalex")

# LIME local explanations
fastexplain(fit, method = "lime", observation = df[1, ])

# Accumulated Local Effects
fastexplain(fit, method = "ale", features = "Sepal.Length")

# Individual Conditional Expectation curves
fastexplain(fit, method = "ice", features = "Sepal.Length")

# Surrogate decision tree
fastexplain(fit, method = "surrogate")

# Feature interaction strength
fastexplain(fit, method = "interaction")

# iBreakDown contributions
fastexplain(fit, method = "breakdown", observation = df[1, ])

# Counterfactual explanations
fastexplain(fit, method = "counterfactual", observation = df[1, ])

# Interactive modelStudio dashboard
fastexplain(fit, method = "studio")

# Fairness diagnostics
fastexplain(fit, method = "fairness", protected = df$gender)
```

### Feature Importance Stability

Analyze how feature importance varies across cross-validation folds:

```r
fit <- fastml(data = df, label = "target", store_fold_models = TRUE)
stability <- explain_stability(fit)
print(stability)
plot(stability)
```

## Exploratory Data Analysis

`fastexplore()` provides read-only diagnostics prior to model training:

```r
fastexplore(iris, label = "Species")
```

Generates: summary statistics, distribution plots (histograms, boxplots, bar charts), correlation heatmaps, Q-Q plots, missingness analysis, and more — without invoking any modeling.

## Visualization

```r
# Performance comparison
plot(fit, type = "bar")

# ROC curves (classification)
plot(fit, type = "roc")

# Calibration plot (classification)
plot(fit, type = "calibration")

# Residual diagnostics (regression)
plot(fit, type = "residual")

# All plots at once
plot(fit, type = "all")
```

## Parallel Processing

```r
fit <- fastml(
  data = df,
  label = "target",
  algorithms = c("rand_forest", "xgboost", "svm_rbf"),
  n_cores = 4
)
```

## Model Persistence

```r
# Save
save.fastml(fit, path = "my_model.rds")

# Load
fit <- load_model("my_model.rds")
```

## Survival Analysis

```r
library(survival)
data(lung)

fit_surv <- fastml(
  data = lung,
  label = c("time", "status"),
  algorithms = c("cox_ph", "rand_forest"),
  eval_times = c(180, 365, 730)
)

summary(fit_surv)

# Predict survival probabilities
predict_survival(fit_surv, newdata = lung[1:5, ], eval_times = c(365, 730))

# Predict risk scores
predict_risk(fit_surv, newdata = lung[1:5, ])
```

## Advanced Options

```r
fit <- fastml(
  data = df,
  label = "target",

  # Algorithms and engines
  algorithms = c("rand_forest", "xgboost"),
  algorithm_engines = list(rand_forest = "ranger", xgboost = "xgboost"),
  engine_params = list(xgboost = list(nthread = 4)),

  # Resampling
  resampling_method = "cv",
  cv_folds = 10,
  store_fold_models = TRUE,

  # Tuning
  tuning_strategy = "bayes",
  tuning_complexity = "thorough",
  tuning_iterations = 30,

  # Classification-specific
  class_threshold = "auto",          # auto-tune threshold
  multiclass_auc = "macro_weighted", # prevalence-weighted AUC

  # Bootstrap confidence intervals
  bootstrap_ci = TRUE,
  bootstrap_samples = 500,

  # Reproducibility
  seed = 42
)
```

## Scope

fastml is intended for users who require reliable performance estimation under cross-validation, particularly in:

- Multi-site or grouped data
- High-dimensional biomedical applications
- Workflows prone to preprocessing leakage

It prioritizes correctness-oriented defaults and workflow clarity over maximum flexibility.

## Links

- [Tutorial & Documentation](https://selcukorkmaz.github.io/fastml-tutorial/)
- [GitHub Repository](https://github.com/selcukorkmaz/fastml)
- [Bug Reports](https://github.com/selcukorkmaz/fastml/issues)
- [CRAN Page](https://CRAN.R-project.org/package=fastml)

## License

MIT License. See [LICENSE](LICENSE.md) for details.
