# Get Default Parameters for an Algorithm

Returns a list of default tuning parameters for the specified algorithm
based on the task type, number of predictors, and engine.

## Usage

``` r
get_default_params(algo, task, num_predictors = NULL, engine = NULL)
```

## Arguments

- algo:

  A character string specifying the algorithm name. Supported values
  include: `"rand_forest"`, `"C5_rules"`, `"xgboost"`, `"lightgbm"`,
  `"logistic_reg"`, `"multinom_reg"`, `"decision_tree"`, `"svm_linear"`,
  `"svm_rbf"`, `"nearest_neighbor"`, `"naive_Bayes"`, `"mlp"`,
  `"deep_learning"`, `"discrim_linear"`, `"discrim_quad"`, `"bag_tree"`,
  `"elastic_net"`, `"bayes_glm"`, `"pls"`, `"linear_reg"`,
  `"ridge_reg"`, `"lasso_reg"`, and `"penalized_cox"`.

- task:

  A character string specifying the task type, typically
  `"classification"` or `"regression"`.

- num_predictors:

  An optional numeric value indicating the number of predictors. This
  value is used to compute default values for parameters such as `mtry`.
  Defaults to `NULL`.

- engine:

  An optional character string specifying the engine to use. If not
  provided, a default engine is chosen where applicable.

## Value

A list of default parameter settings for the specified algorithm. If the
algorithm is not recognized, the function returns `NULL`.

## Details

The function employs a `switch` statement to select and return a list of
default parameters tailored for the given algorithm, task, and engine.
The defaults vary by algorithm and, in some cases, by engine. For
example:

- For `"rand_forest"`, if `engine` is not provided, it defaults to
  `"ranger"`. The parameters such as `mtry`, `trees`, and `min_n` are
  computed based on the task and the number of predictors.

- For `"C5_rules"`, the defaults include `trees`, `min_n`, and
  `sample_size`.

- For `"xgboost"` and `"lightgbm"`, default values are provided for
  parameters like tree depth, learning rate, and sample size.

- For `"logistic_reg"` and `"multinom_reg"`, the function returns
  defaults for regularization parameters (`penalty` and `mixture`) that
  vary with the specified engine.

- For `"decision_tree"`, the parameters (such as `tree_depth`, `min_n`,
  and `cost_complexity`) are set based on the engine (e.g., `"rpart"`,
  `"C5.0"`, `"partykit"`, `"spark"`).

- Other algorithms, including `"svm_linear"`, `"svm_rbf"`,
  `"nearest_neighbor"`, `"naive_Bayes"`, `"mlp"`, `"deep_learning"`,
  `"elastic_net"`, `"bayes_glm"`, `"pls"`, `"linear_reg"`,
  `"ridge_reg"`, and `"lasso_reg"`, have their respective default
  parameter lists.
