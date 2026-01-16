<img src="man/figures/fastml_hex.png" align="right" width="95"/>

# fastml: Guarded Resampling Workflows for Safe and Automated Machine Learning in R

**fastml** is an R package for training, evaluating, and comparing machine learning models with a guarded resampling workflow.  
Rather than introducing new learning algorithms, fastml focuses on **reducing leakage risk** by keeping preprocessing, model fitting, and evaluation aligned within supported resampling paths and resampling configurations.

In fastml, *“fast” refers to the rapid construction of statistically valid workflows*, not to computational shortcuts. By eliminating entire classes of user-induced errors—most notably preprocessing leakage—fastml allows practitioners to obtain reliable performance estimates with minimal configuration.

## Core Principles

- **Guarded resampling workflow**  
  When the guarded resampling path is used, preprocessing and model fitting are re-estimated independently within each resampling split. This reduces leakage risk, but does not prevent users from supplying preprocessed inputs.

- **Leakage risk reduction, not guarantees**  
  fastml can mitigate common leakage modes (e.g., global scaling or imputation before resampling) when workflows are fit within resamples. It does not universally prevent all leakage scenarios.

- **Single, unified interface**  
  Multiple models can be trained and benchmarked through a single call, while internally coordinating resampling, preprocessing, and evaluation in supported paths.

- **Compatibility with established engines**  
  fastml orchestrates existing modeling infrastructure (recipes, rsample, parsnip, yardstick) without modifying their statistical behavior.

## Features

- **Preprocessing isolation within resampling**  
  Transformations (scaling, imputation, encoding, feature construction) are learned from training folds and applied to assessment folds when the guarded resampling path is used.

- **Support for multiple algorithms**  
  Includes tree-based models, linear and penalized models, kernel methods, neural networks, and boosting approaches via established engines.

- **Hyperparameter tuning within guarded resampling**  
  Grid and Bayesian tuning run inside the resampling loop when resampling is enabled.

- **Consistent performance evaluation**  
  Metrics such as Accuracy, ROC AUC, Sensitivity, Specificity, Precision, and F1 are computed from evaluation outputs and reflect the configured resampling or holdout strategy.

- **Visualization and comparison tools**  
  Built-in plots facilitate comparison across models while preserving statistical validity.

- **Resampling options**  
  Supports common resampling schemes (e.g., CV, repeated CV, grouped, blocked, rolling origin), with task-specific limitations noted in the function documentation.

## Installation

### From CRAN

You can install the latest stable version of **fastml** from CRAN using:

```r
install.packages("fastml")
```

You can install all dependencies (additional models) using:
```r
# install all dependencies - recommended
install.packages("fastml", dependencies = TRUE)
```

### From GitHub
For the development version, install directly from GitHub using the devtools package:

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install fastml from GitHub
devtools::install_github("selcukorkmaz/fastml")
```

### Quick Start
Here's a simple workflow to get you started with fastml:

```r
library(fastml)

# Example dataset
data(iris)

iris_binary <- iris %>%
  filter(Species != "setosa") %>%
  mutate(Species = factor(Species))

# Train models
fit <- fastml(
  data = iris_binary,
  label = "Species",
  algorithms = c("rand_forest", "logistic_reg")
)

# View model summary
summary(fit)

# Plot the performance metrics
plot(fit, type = "bar")

# Plot ROC curves
plot(fit, type = "roc")

# Plot model calibration
plot(fit, type = "calibration")
```

## Tuning Strategies

Hyperparameter tuning is supported via:

- `grid` — regular grid search

- `bayes` — Bayesian optimization

```r
fastml(
  data = iris_binary,
  label = "Species",
  algorithms = c("rand_forest", "logistic_reg"),
  tuning_strategy = "bayes",
  tuning_iterations = 20
)
```

`tuning_iterations` is used only for Bayesian optimization.

## Explainability

Model explainability tools are provided through `fastexplain()`:

```r
# Prepare data
library(survival)
data(pbc, package = "survival")
  
# The pbc dataset has two parts; we only want the baseline data (rows 1-312)
pbc_baseline <- pbc[1:312, -c(1:4)]

# Train a regression model
fit_reg <- fastml(
  data = pbc_baseline,
  label = "albumin",
  algorithms = c("xgboost"),
  metric = "rmse",
  impute_method = "medianImpute"
)
  
# Feature importance and SHAP values based on DALEX
fastexplain(fit_reg, method = "dalex")

# Breakdown profile
fastexplain(fit_reg, method = "breakdown", observation = pbc_baseline[1, -9])

# Counterfactual explanation (Ceteris Paribus profile)
fastexplain(fit_reg, method = "counterfactual", observation = pbc_baseline[1, -9])

```

Explainability is performed on trained models and does not interfere with resampling or preprocessing.

## Exploratory Diagnostics

`fastexplore()` provides read-only exploratory diagnostics prior to model training.
It summarizes distributions, missingness, correlations, and basic structure without invoking resampling, preprocessing, or model fitting.

```r
fastexplore(iris, label = "Species")
```

This function is decoupled from fastml’s guarded resampling core and does not influence model evaluation unless its outputs are explicitly used in later modeling calls.

## Scope

fastml is intended for users who require reliable performance estimation under cross-validation, particularly in:

- multi-site or grouped data

- high-dimensional biomedical applications

- workflows prone to preprocessing leakage

It prioritizes correctness-oriented defaults and workflow clarity over maximum flexibility.

## License

MIT License
See `LICENSE` for details.
