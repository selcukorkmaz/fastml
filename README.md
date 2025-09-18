<img src="man/figures/fastml_hex.png" align="right" width="95"/>


# fastml: Fast Machine Learning Model Training and Evaluation

**fastml** is a streamlined R package designed to simplify the training, evaluation, and comparison of multiple machine learning models. It offers comprehensive data preprocessing, supports a wide range of algorithms with hyperparameter tuning, and provides performance metrics alongside visualization tools to facilitate efficient and effective machine learning workflows.

## Features

- **Comprehensive Data Preprocessing:** Handle missing values, encode categorical variables, and apply various scaling methods with minimal code.
- **Support for Multiple Algorithms:** Train a wide array of machine learning models including XGBoost, Random Forest, SVMs, KNN, Neural Networks, and more.
- **Hyperparameter Tuning:** Customize and automate hyperparameter tuning for each algorithm to optimize model performance.
- **Performance Evaluation:** Evaluate models using metrics like Accuracy, Kappa, Sensitivity, Specificity, Precision, F1 Score, and ROC AUC.
- **Visualization Tools:** Generate comparison plots to visualize and compare the performance of different models effortlessly.
- **Easy Integration:** Designed to integrate seamlessly into your existing R workflows with intuitive function interfaces.

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
iris <- iris[iris$Species != "setosa", ]  # Binary classification
iris$Species <- factor(iris$Species)

# Train models
model <- fastml(
  data = iris,
  label = "Species"
)

# View model summary
summary(model)
```

## Tuning Strategies

fastml supports both grid search and Bayesian optimization through the
`tuning_strategy` argument. Use `"grid"` for a regular parameter grid or
`"bayes"` for Bayesian hyperparameter search. The `tuning_iterations`
parameter controls the number of iterations **only** when
`tuning_strategy = "bayes"` and is ignored otherwise.

## Explainability

`fastexplain()` provides several ways to understand trained models. Set the
`method` argument to choose an approach:

```r
# LIME explanations
explain_lime(model)

# ICE curves
fastexplain(model, method = "ice", features = "Sepal.Length")

# Accumulated Local Effects
fastexplain(model, method = "ale", features = "Sepal.Length")

# Surrogate tree
fastexplain(model, method = "surrogate")

# Interaction strength
fastexplain(model, method = "interaction")

# Counterfactual explanation for a single observation
fastexplain(model, method = "counterfactual", observation = iris[1, ])
```


