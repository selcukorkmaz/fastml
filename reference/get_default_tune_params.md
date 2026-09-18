# Get Default Tuning Parameters

Returns a list of default tuning parameter ranges for a specified
algorithm based on the provided training data, outcome label, and
engine.

## Usage

``` r
get_default_tune_params(algo, train_data, label, engine)
```

## Arguments

- algo:

  A character string specifying the algorithm name. Supported values
  include: `"rand_forest"`, `"C5_rules"`, `"xgboost"`, `"lightgbm"`,
  `"logistic_reg"`, `"multinom_reg"`, `"decision_tree"`, `"svm_linear"`,
  `"svm_rbf"`, `"nearest_neighbor"`, `"naive_Bayes"`, `"mlp"`,
  `"deep_learning"`, `"discrim_linear"`, `"discrim_quad"`, `"bag_tree"`,
  `"elastic_net"`, `"bayes_glm"`, `"pls"`, `"linear_reg"`,
  `"ridge_reg"`, and `"lasso_reg"`.

- train_data:

  A data frame containing the training data.

- label:

  A character string specifying the name of the outcome variable in
  `train_data`. This column is excluded when calculating the number of
  predictors.

- engine:

  A character string specifying the engine to be used for the algorithm.
  Different engines may have different tuning parameter ranges.

## Value

A list of tuning parameter ranges for the specified algorithm. If no
tuning parameters are defined for the given algorithm, the function
returns `NULL`.

## Details

The function first determines the number of predictors by removing the
outcome variable (specified by `label`) from `train_data`. It then uses
a `switch` statement to select a list of default tuning parameter ranges
tailored for the specified algorithm and engine. The tuning ranges have
been adjusted for efficiency and may include parameters such as `mtry`,
`trees`, `min_n`, and others depending on the algorithm.
