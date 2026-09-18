# Get Available Methods

Returns a character vector of algorithm names available for
classification, regression or survival tasks.

## Usage

``` r
availableMethods(type = c("classification", "regression", "survival"), ...)
```

## Arguments

- type:

  A character string specifying the type of task. Must be one of
  `"classification"`, `"regression"`, or `"survival"`. Defaults to
  `c("classification", "regression", "survival")` and uses
  [`match.arg`](https://rdrr.io/r/base/match.arg.html) to select one.

- ...:

  Additional arguments (currently not used).

## Value

A character vector containing the names of the available algorithms for
the specified task type.

## Details

Depending on the specified `type`, the function returns a different set
of algorithm names:

- For `"classification"`, it returns algorithms such as
  `"logistic_reg"`, `"multinom_reg"`, `"decision_tree"`, `"C5_rules"`,
  `"rand_forest"`, `"xgboost"`, `"lightgbm"`, `"svm_linear"`,
  `"svm_rbf"`, `"nearest_neighbor"`, `"naive_Bayes"`, `"mlp"`,
  `"discrim_linear"`, `"discrim_quad"`, and `"bag_tree"`.

- For `"regression"`, it returns algorithms such as `"linear_reg"`,
  `"ridge_reg"`, `"lasso_reg"`, `"elastic_net"`, `"decision_tree"`,
  `"rand_forest"`, `"xgboost"`, `"lightgbm"`, `"svm_linear"`,
  `"svm_rbf"`, `"nearest_neighbor"`, `"mlp"`, `"pls"`, and
  `"bayes_glm"`.

- For `"survival"`, it returns algorithms such as `"rand_forest"`,
  `"cox_ph"`, `"penalized_cox"`, `"stratified_cox"`,
  `"time_varying_cox"`, `"survreg"`, `"royston_parmar"`,
  `"parametric_surv"`, `"piecewise_exp"`, and `"xgboost"`.
