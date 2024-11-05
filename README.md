# fastml: Fast Machine Learning Model Training and Evaluation

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/fastml)](https://CRAN.R-project.org/package=fastml)
[![License](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)

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

You can install the latest stable version of **fastml** from [CRAN](https://CRAN.R-project.org/package=fastml) using:

```r
install.packages("fastml")
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

