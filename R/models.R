#' Define LightGBM Model Specification
#'
#' @inheritParams define_random_forest_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip boost_tree set_mode set_engine
#' @importFrom dplyr select all_of
#' @import bonsai
#' @noRd
define_lightgbm_spec <- function(task, train_data, label, tune = FALSE) {
  if (!requireNamespace("lightgbm", quietly = TRUE)) {
    stop("The 'lightgbm' package is required but is not installed.")
  }
  num_predictors <- ncol(train_data %>% select(-all_of(label)))
  defaults <- get_default_params("lightgbm", num_predictors)

  if (tune) {
    model_spec <- boost_tree(
      trees = tune(),
      tree_depth = tune(),
      learn_rate = tune(),
      mtry = tune(),
      min_n = tune(),
      loss_reduction = tune(),
      sample_size = tune()
    ) %>%
      set_mode(task) %>%
      set_engine("lightgbm")
  } else {
    model_spec <- boost_tree(
      trees = defaults$trees,
      tree_depth = defaults$tree_depth,
      learn_rate = defaults$learn_rate,
      mtry = defaults$mtry,
      min_n = defaults$min_n,
      loss_reduction = defaults$loss_reduction,
      sample_size = defaults$sample_size
    ) %>%
      set_mode(task) %>%
      set_engine("lightgbm")
  }
  list(model_spec = model_spec)
}


#' Define XGBoost Model Specification
#'
#' @inheritParams define_random_forest_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip boost_tree set_mode set_engine
#' @importFrom dplyr select all_of
#' @noRd
define_xgboost_spec <- function(task, train_data, label, tune = FALSE) {
  num_predictors <- ncol(train_data %>% select(-all_of(label)))
  defaults <- get_default_params("xgboost", num_predictors)

  if (tune) {
    model_spec <- boost_tree(
      trees = tune(),
      tree_depth = tune(),
      learn_rate = tune(),
      mtry = tune(),
      min_n = tune(),
      loss_reduction = tune(),
      sample_size = tune()
    ) %>%
      set_mode(task) %>%
      set_engine("xgboost")
  } else {
    model_spec <- boost_tree(
      trees = defaults$trees,
      tree_depth = defaults$tree_depth,
      learn_rate = defaults$learn_rate,
      mtry = defaults$mtry,
      min_n = defaults$min_n,
      loss_reduction = defaults$loss_reduction,
      sample_size = defaults$sample_size
    ) %>%
      set_mode(task) %>%
      set_engine("xgboost")
  }
  list(model_spec = model_spec)
}


#' Define C5.0 Model Specification
#'
#' @inheritParams define_random_forest_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip boost_tree set_mode set_engine
#' @noRd
define_c5_0_spec <- function(task, tune = FALSE) {
  if (task != "classification") {
    stop("C5.0 is only applicable for classification tasks.")
  }
  defaults <- get_default_params("c5.0")

  if (tune) {
    model_spec <- boost_tree(
      trees = tune(),
      min_n = tune(),
      sample_size = tune()
    ) %>%
      set_mode("classification") %>%
      set_engine("C5.0")
  } else {
    model_spec <- boost_tree(
      trees = defaults$trees,
      min_n = defaults$min_n,
      sample_size = defaults$sample_size
    ) %>%
      set_mode("classification") %>%
      set_engine("C5.0")
  }
  list(model_spec = model_spec)
}


#' Define Random Forest Model Specification
#'
#' @param task Character string specifying the task type: "classification" or "regression".
#' @param train_data Data frame containing the training data.
#' @param label Character string specifying the name of the target variable.
#' @param tune Logical indicating whether to use tuning parameters.
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip rand_forest set_mode set_engine
#' @importFrom dplyr select all_of
#' @noRd
define_random_forest_spec <- function(task, train_data, label, tune = FALSE) {
  num_predictors <- ncol(train_data %>% select(-all_of(label)))
  defaults <- get_default_params("random_forest", num_predictors)

  if (tune) {
    model_spec <- rand_forest(
      mtry = tune(),
      trees = tune(),
      min_n = tune()
    ) %>%
      set_mode(task) %>%
      set_engine("ranger")
  } else {
    model_spec <- rand_forest(
      mtry = defaults$mtry,
      trees = defaults$trees,
      min_n = defaults$min_n
    ) %>%
      set_mode(task) %>%
      set_engine("ranger")
  }
  list(model_spec = model_spec)
}


#' Define Ranger Model Specification
#'
#' @inheritParams define_random_forest_spec
#' @return List containing the model specification (`model_spec`).
#' @noRd
define_ranger_spec <- function(task, train_data, label, tune = FALSE) {
  define_random_forest_spec(task, train_data, label, tune)
}

#' Define Lasso Regression Model Specification
#'
#' @inheritParams define_elastic_net_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip linear_reg set_mode set_engine
#' @noRd
define_lasso_regression_spec <- function(task, tune = FALSE) {
  if (task != "regression") {
    stop("Lasso regression is only applicable for regression tasks.")
  }
  defaults <- get_default_params("lasso_regression")

  if (tune) {
    model_spec <- linear_reg(
      penalty = tune(),
      mixture = defaults$mixture  # Fixed mixture for Lasso
    ) %>%
      set_mode("regression") %>%
      set_engine("glmnet")
  } else {
    model_spec <- linear_reg(
      penalty = defaults$penalty,
      mixture = defaults$mixture
    ) %>%
      set_mode("regression") %>%
      set_engine("glmnet")
  }
  list(model_spec = model_spec)
}


#' Define Ridge Regression Model Specification
#'
#' @inheritParams define_elastic_net_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip linear_reg set_mode set_engine
#' @noRd
define_ridge_regression_spec <- function(task, tune = FALSE) {
  if (task != "regression") {
    stop("Ridge regression is only applicable for regression tasks.")
  }
  defaults <- get_default_params("ridge_regression")

  if (tune) {
    model_spec <- linear_reg(
      penalty = tune(),
      mixture = defaults$mixture  # Fixed mixture for Ridge
    ) %>%
      set_mode("regression") %>%
      set_engine("glmnet")
  } else {
    model_spec <- linear_reg(
      penalty = defaults$penalty,
      mixture = defaults$mixture
    ) %>%
      set_mode("regression") %>%
      set_engine("glmnet")
  }
  list(model_spec = model_spec)
}


#' Define Linear Regression Model Specification
#'
#' @inheritParams define_elastic_net_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip linear_reg set_mode set_engine
#' @noRd
define_linear_regression_spec <- function(task) {
  if (task != "regression") {
    stop("Linear regression is only applicable for regression tasks.")
  }
  model_spec <- linear_reg() %>%
    set_mode("regression") %>%
    set_engine("lm")
  list(model_spec = model_spec)
}


#' Define Bayesian GLM Model Specification
#'
#' @inheritParams define_elastic_net_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip linear_reg set_mode set_engine
#' @noRd
define_bayes_glm_spec <- function(task) {
  if (!requireNamespace("rstanarm", quietly = TRUE)) {
    stop("The 'rstanarm' package is required but is not installed.")
  }
  if (task != "regression") {
    stop("Bayesian GLM is only applicable for regression tasks.")
  }
  model_spec <- linear_reg() %>%
    set_mode("regression") %>%
    set_engine("stan")
  list(model_spec = model_spec)
}


#' Define Elastic Net Model Specification
#'
#' @param task Character string specifying the task type ("regression").
#' @inheritParams define_random_forest_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip linear_reg set_mode set_engine
#' @noRd
define_elastic_net_spec <- function(task, tune = FALSE) {
  if (task != "regression") {
    stop("Elastic Net is only applicable for regression tasks.")
  }
  defaults <- get_default_params("elastic_net")
  if (tune) {
    model_spec <- linear_reg(
      penalty = tune(),
      mixture = tune()
    ) %>%
      set_mode("regression") %>%
      set_engine("glmnet")
  } else {
    model_spec <- linear_reg(
      penalty = defaults$penalty,
      mixture = defaults$mixture
    ) %>%
      set_mode("regression") %>%
      set_engine("glmnet")
  }
  list(model_spec = model_spec)
}


#' Define Bagging Model Specification
#'
#' @inheritParams define_decision_tree_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip bag_tree set_mode set_engine
#' @import baguette
#' @noRd
define_bagging_spec <- function(task, tune = FALSE) {
  defaults <- get_default_params("bagging")
  if (tune) {
    model_spec <- bag_tree(
      min_n = tune()
    ) %>%
      set_mode(task) %>%
      set_engine("rpart", times = 25)
  } else {
    model_spec <- bag_tree(
      min_n = defaults$min_n
    ) %>%
      set_mode(task) %>%
      set_engine("rpart", times = 25)
  }
  list(model_spec = model_spec)
}


#' Define Quadratic Discriminant Analysis Model Specification
#'
#' @inheritParams define_logistic_regression_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip set_mode set_engine discrim_quad
#' @noRd
define_qda_spec <- function(task) {
  if (task != "classification") {
    stop("QDA is only applicable for classification tasks.")
  }
  model_spec <- discrim_quad() %>%
    set_mode("classification") %>%
    set_engine("MASS")
  list(model_spec = model_spec)
}


#' Define Linear Discriminant Analysis Model Specification
#'
#' @inheritParams define_logistic_regression_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip set_mode set_engine discrim_linear
#' @noRd
define_lda_spec <- function(task) {
  if (task != "classification") {
    stop("LDA is only applicable for classification tasks.")
  }
  model_spec <- discrim_linear() %>%
    set_mode("classification") %>%
    set_engine("MASS")
  list(model_spec = model_spec)
}


#' Define Neural Network Model Specification (nnet)
#'
#' @inheritParams define_random_forest_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip mlp set_mode set_engine
#' @noRd
define_neural_network_spec <- function(task, tune = FALSE) {
  defaults <- get_default_params("neural_network")

  if (tune) {
    model_spec <- mlp(
      hidden_units = tune(),
      penalty = tune(),
      epochs = tune()
    ) %>%
      set_mode(task) %>%
      set_engine("nnet")
  } else {
    model_spec <- mlp(
      hidden_units = defaults$hidden_units,
      penalty = defaults$penalty,
      epochs = defaults$epochs
    ) %>%
      set_mode(task) %>%
      set_engine("nnet")
  }
  list(model_spec = model_spec)
}


#' Define Naive Bayes Model Specification
#'
#' @inheritParams define_logistic_regression_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip naive_Bayes set_mode set_engine
#' @import discrim
#' @noRd
define_naive_bayes_spec <- function(task, tune = FALSE) {
  if (task != "classification") {
    stop("Naive Bayes is only applicable for classification tasks.")
  }
  defaults <- get_default_params("naive_bayes")

  if (tune) {
    model_spec <- naive_Bayes(
      smoothness = tune(),
      Laplace = tune()
    ) %>%
      set_mode("classification") %>%
      set_engine("klaR")
  } else {
    model_spec <- naive_Bayes(
      smoothness = defaults$smoothness,
      Laplace = defaults$Laplace
    ) %>%
      set_mode("classification") %>%
      set_engine("klaR")
  }
  list(model_spec = model_spec)
}


#' Define K-Nearest Neighbors Model Specification
#'
#' @inheritParams define_random_forest_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip nearest_neighbor set_mode set_engine
#' @noRd
define_knn_spec <- function(task, tune = FALSE) {
  defaults <- get_default_params("knn")

  if (tune) {
    model_spec <- nearest_neighbor(
      neighbors = tune(),
      weight_func = tune(),
      dist_power = tune()
    ) %>%
      set_mode(task) %>%
      set_engine("kknn")
  } else {
    model_spec <- nearest_neighbor(
      neighbors = defaults$neighbors,
      weight_func = defaults$weight_func,
      dist_power = defaults$dist_power
    ) %>%
      set_mode(task) %>%
      set_engine("kknn")
  }
  list(model_spec = model_spec)
}


#' Define SVM Radial Model Specification
#'
#' @inheritParams define_svm_linear_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip svm_rbf set_mode set_engine
#' @noRd
define_svm_radial_spec <- function(task, tune = FALSE) {
  defaults <- get_default_params("svm_radial")

  if (tune) {
    model_spec <- svm_rbf(
      cost = tune(),
      rbf_sigma = tune()
    ) %>%
      set_mode(task) %>%
      set_engine("kernlab")
  } else {
    model_spec <- svm_rbf(
      cost = defaults$cost,
      rbf_sigma = defaults$rbf_sigma
    ) %>%
      set_mode(task) %>%
      set_engine("kernlab")
  }
  list(model_spec = model_spec)
}


#' Define SVM Linear Model Specification
#'
#' @inheritParams define_random_forest_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip svm_linear set_mode set_engine
#' @noRd
define_svm_linear_spec <- function(task, tune = FALSE) {
  defaults <- get_default_params("svm_linear")

  if (tune) {
    model_spec <- svm_linear(
      cost = tune()
    ) %>%
      set_mode(task) %>%
      set_engine("kernlab")
  } else {
    model_spec <- svm_linear(
      cost = defaults$cost
    ) %>%
      set_mode(task) %>%
      set_engine("kernlab")
  }
  list(model_spec = model_spec)
}

#' Define Decision Tree Model Specification
#'
#' @inheritParams define_random_forest_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip decision_tree set_mode set_engine
#' @noRd
define_decision_tree_spec <- function(task, tune = FALSE) {
  defaults <- get_default_params("decision_tree")

  if (tune) {
    model_spec <- decision_tree(
      tree_depth = tune(),
      min_n = tune(),
      cost_complexity = tune()
    ) %>%
      set_mode(task) %>%
      set_engine("rpart")
  } else {
    model_spec <- decision_tree(
      tree_depth = defaults$tree_depth,
      min_n = defaults$min_n,
      cost_complexity = defaults$cost_complexity
    ) %>%
      set_mode(task) %>%
      set_engine("rpart")
  }
  list(model_spec = model_spec)
}


#' Define Logistic Regression Model Specification
#'
#' @param task Character string specifying the task type ("classification").
#' @param tune Logical indicating whether to use tuning parameters.
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip logistic_reg set_mode set_engine
#' @noRd
define_logistic_regression_spec <- function(task, tune = FALSE) {
  if (task != "classification") {
    stop("Logistic regression is only applicable for classification tasks.")
  }
  if (tune) {
    model_spec <- logistic_reg(
      penalty = tune()
    ) %>%
      set_mode("classification") %>%
      set_engine("glmnet")
  } else {
    model_spec <- logistic_reg() %>%
      set_mode("classification") %>%
      set_engine("glm")
  }
  list(model_spec = model_spec)
}

#' Define Multinomial Logistic Regression Specification
#'
#' This function defines a multinomial logistic regression model specification.
#' It supports both a basic model and one with hyperparameter tuning for the penalty parameter.
#'
#' @param task Character. The type of task. Must be "classification".
#' @param tune Logical. If `TRUE`, includes hyperparameter tuning for the penalty parameter. Default is `FALSE`.
#' @return A list containing the model specification (`model_spec`).
#' @importFrom parsnip multinom_reg set_mode set_engine
#' @noRd
define_multinomial_regression_spec <- function(task, tune = FALSE) {
  if (task != "classification") {
    stop("Multinomial logistic regression is only applicable for classification tasks.")
  }
  if (tune) {
    model_spec <- multinom_reg(
      penalty = tune()
    ) %>%
      set_mode("classification") %>%
      set_engine("glmnet")
  } else {
    model_spec <- multinom_reg() %>%
      set_mode("classification") %>%
      set_engine("nnet")
  }
  list(model_spec = model_spec)
}

#' Define Penalized Logistic Regression Model Specification
#'
#' @inheritParams define_logistic_regression_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip logistic_reg set_engine
#' @importFrom tune finalize_model
#' @importFrom tibble tibble
#' @noRd
define_penalized_logistic_regression_spec <- function(task, tune = FALSE) {
  if (task != "classification") {
    stop("Penalized logistic regression is only applicable for classification tasks.")
  }
  defaults <- get_default_params("penalized_logistic_regression")

  if (tune) {
    model_spec <- logistic_reg(
      penalty = tune(),
      mixture = tune()
    ) %>%
      set_engine("glmnet")
  } else {
    model_spec <- logistic_reg(
      penalty = defaults$penalty,
      mixture = defaults$mixture
    ) %>%
      set_engine("glmnet")
    model_spec <- finalize_model(model_spec, parameters = tibble())
  }
  list(model_spec = model_spec)
}

#' Define Penalized Multinomial Logistic Regression Model Specification
#'
#' This function defines a multinomial logistic regression model specification
#' with optional penalty and mixture hyperparameter tuning.
#'
#' @inheritParams define_penalized_logistic_regression_spec
#' @return A list containing the model specification (`model_spec`).
#' @importFrom parsnip multinom_reg set_engine
#' @importFrom tune finalize_model
#' @importFrom tibble tibble
#' @noRd
define_penalized_multinomial_regression_spec <- function(task, tune = FALSE) {
  if (task != "classification") {
    stop("Penalized multinomial logistic regression is only applicable for classification tasks.")
  }

  # Retrieve default parameters for penalized multinomial regression
  defaults <- get_default_params("penalized_logistic_regression")

  if (tune) {
    # Model specification with tuning
    model_spec <- multinom_reg(
      penalty = tune(),
      mixture = tune()
    ) %>%
      set_engine("glmnet")
  } else {
    # Model specification with defaults
    model_spec <- multinom_reg(
      penalty = defaults$penalty,
      mixture = defaults$mixture
    ) %>%
      set_engine("glmnet")
    # Finalize model with default parameters
    model_spec <- finalize_model(model_spec, parameters = tibble())
  }

  list(model_spec = model_spec)
}

#' Define Partial Least Squares Model Specification
#'
#' @inheritParams define_elastic_net_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip pls set_mode set_engine
#' @import plsmod
#' @noRd
define_pls_spec <- function(task, tune = FALSE) {
  if (task != "regression") {
    stop("PLS is only applicable for regression tasks.")
  }
  defaults <- get_default_params("pls")

  if (tune) {
    model_spec <- pls(
      num_comp = tune()
    ) %>%
      set_mode("regression") %>%
      set_engine("mixOmics")
  } else {
    model_spec <- pls(
      num_comp = defaults$num_comp
    ) %>%
      set_mode("regression") %>%
      set_engine("mixOmics")
  }
  list(model_spec = model_spec)
}

