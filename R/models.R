#' Define LightGBM Model Specification
#'
#' @inheritParams define_rand_forest_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip boost_tree set_mode set_engine
#' @importFrom dplyr select all_of
#' @import bonsai
#' @noRd
define_lightgbm_spec <- function(task, train_data, label, tune = FALSE, engine = "lightgbm") {
  if (!requireNamespace("lightgbm", quietly = TRUE)) {
    stop("The 'lightgbm' package is required but is not installed.")
  }
  num_predictors <- ncol(train_data %>% dplyr::select(-dplyr::all_of(label)))
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
      set_engine(engine)
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
      set_engine(engine)
  }
  list(model_spec = model_spec)
}

#' Define XGBoost Model Specification
#'
#' @inheritParams define_rand_forest_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip boost_tree set_mode set_engine
#' @importFrom dplyr select all_of
#' @noRd
define_xgboost_spec <- function(task, train_data, label, tune = FALSE, engine = "xgboost") {
  num_predictors <- ncol(train_data %>% dplyr::select(-dplyr::all_of(label)))
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
      set_engine(engine)
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
      set_engine(engine)
  }
  list(model_spec = model_spec)
}

#' Define C5_rules Model Specification
#'
#' @inheritParams define_rand_forest_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip boost_tree set_mode set_engine
#' @noRd
define_C5_rules_spec <- function(task, tune = FALSE, engine = "C5.0") {
  if (task != "classification") {
    stop("C5.0 is only applicable for classification tasks.")
  }
  defaults <- get_default_params("C5_rules")

  if (tune) {
    model_spec <- boost_tree(
      trees = tune(),
      min_n = tune(),
      sample_size = tune()
    ) %>%
      set_mode("classification") %>%
      set_engine(engine)
  } else {
    model_spec <- boost_tree(
      trees = defaults$trees,
      min_n = defaults$min_n,
      sample_size = defaults$sample_size
    ) %>%
      set_mode("classification") %>%
      set_engine(engine)
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
define_rand_forest_spec <- function(task, train_data, label, tune = FALSE, engine = "ranger") {
  num_predictors <- ncol(train_data %>% dplyr::select(-dplyr::all_of(label)))
  defaults <- get_default_params("rand_forest", task, num_predictors, engine)

  if (tune) {
    model_spec <- rand_forest(
      mtry = tune(),
      trees = tune(),
      min_n = tune()
    ) %>%
      set_mode(task) %>%
      set_engine(engine)
  } else {
    model_spec <- rand_forest(
      mtry = defaults$mtry,
      trees = defaults$trees,
      min_n = defaults$min_n
    ) %>%
      set_mode(task) %>%
      set_engine(engine)
  }
  list(model_spec = model_spec)
}





#' Define Lasso Regression Model Specification
#'
#' @inheritParams define_elastic_net_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip linear_reg set_mode set_engine
#' @noRd
define_lasso_regression_spec <- function(task, tune = FALSE, engine = "glmnet") {
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
      set_engine(engine)
  } else {
    model_spec <- linear_reg(
      penalty = defaults$penalty,
      mixture = defaults$mixture
    ) %>%
      set_mode("regression") %>%
      set_engine(engine)
  }
  list(model_spec = model_spec)
}

#' Define Ridge Regression Model Specification
#'
#' @inheritParams define_elastic_net_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip linear_reg set_mode set_engine
#' @noRd
define_ridge_regression_spec <- function(task, tune = FALSE, engine = "glmnet") {
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
      set_engine(engine)
  } else {
    model_spec <- linear_reg(
      penalty = defaults$penalty,
      mixture = defaults$mixture
    ) %>%
      set_mode("regression") %>%
      set_engine(engine)
  }
  list(model_spec = model_spec)
}

#' Define Linear Regression Model Specification
#'
#' @inheritParams define_elastic_net_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip linear_reg set_mode set_engine
#' @noRd
define_linear_reg_spec <- function(task, engine = "lm") {
  if (task != "regression") {
    stop("Linear regression is only applicable for regression tasks.")
  }
  model_spec <- linear_reg() %>%
    set_mode("regression") %>%
    set_engine(engine)
  list(model_spec = model_spec)
}

#' Define Bayesian GLM Model Specification
#'
#' @inheritParams define_elastic_net_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip linear_reg set_mode set_engine
#' @noRd
define_bayes_glm_spec <- function(task, engine = "stan") {
  if (!requireNamespace("rstanarm", quietly = TRUE)) {
    stop("The 'rstanarm' package is required but is not installed.")
  }
  if (task != "regression") {
    stop("Bayesian GLM is only applicable for regression tasks.")
  }
  model_spec <- linear_reg() %>%
    set_mode("regression") %>%
    set_engine(engine)
  list(model_spec = model_spec)
}

#' Define Elastic Net Model Specification
#'
#' @param task Character string specifying the task type ("regression").
#' @inheritParams define_rand_forest_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip linear_reg set_mode set_engine
#' @noRd
define_elastic_net_spec <- function(task, tune = FALSE, engine = "glmnet") {
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
      set_engine(engine)
  } else {
    model_spec <- linear_reg(
      penalty = defaults$penalty,
      mixture = defaults$mixture
    ) %>%
      set_mode("regression") %>%
      set_engine(engine)
  }
  list(model_spec = model_spec)
}

#' Define bag_tree Model Specification
#'
#' @inheritParams define_decision_tree_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip bag_tree set_mode set_engine
#' @import baguette
#' @noRd
define_bag_tree_spec <- function(task, tune = FALSE, engine = "rpart") {
  defaults <- get_default_params("bag_tree")
  if (tune) {
    model_spec <- bag_tree(
      min_n = tune()
    ) %>%
      set_mode(task) %>%
      set_engine(engine, times = 25)
  } else {
    model_spec <- bag_tree(
      min_n = defaults$min_n
    ) %>%
      set_mode(task) %>%
      set_engine(engine, times = 25)
  }
  list(model_spec = model_spec)
}

#' Define Quadratic Discriminant Analysis Model Specification
#'
#' @inheritParams define_logistic_reg_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip set_mode set_engine discrim_quad
#' @noRd
define_discrim_quad_spec <- function(task, engine = "MASS") {
  if (task != "classification") {
    stop("discrim_quad is only applicable for classification tasks.")
  }
  model_spec <- discrim_quad() %>%
    set_mode("classification") %>%
    set_engine(engine)
  list(model_spec = model_spec)
}

#' Define Linear Discriminant Analysis Model Specification
#'
#' @inheritParams define_logistic_reg_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip set_mode set_engine discrim_linear
#' @noRd
define_discrim_linear_spec <- function(task, engine = "MASS") {
  if (task != "classification") {
    stop("discrim_linear is only applicable for classification tasks.")
  }
  model_spec <- discrim_linear() %>%
    set_mode("classification") %>%
    set_engine(engine)
  list(model_spec = model_spec)
}

#' Define Neural Network Model Specification (nnet)
#'
#' @inheritParams define_rand_forest_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip mlp set_mode set_engine
#' @noRd
define_mlp_spec <- function(task, tune = FALSE, engine = "nnet") {
  defaults <- get_default_params("mlp")

  if (tune) {
    model_spec <- mlp(
      hidden_units = tune(),
      penalty = tune(),
      epochs = tune()
    ) %>%
      set_mode(task) %>%
      set_engine(engine)
  } else {
    model_spec <- mlp(
      hidden_units = defaults$hidden_units,
      penalty = defaults$penalty,
      epochs = defaults$epochs
    ) %>%
      set_mode(task) %>%
      set_engine(engine)
  }
  list(model_spec = model_spec)
}

#' Define Naive Bayes Model Specification
#'
#' @inheritParams define_logistic_reg_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip naive_Bayes set_mode set_engine
#' @import discrim
#' @noRd
define_naive_Bayes_spec <- function(task, tune = FALSE, engine = "klaR") {
  if (task != "classification") {
    stop("Naive Bayes is only applicable for classification tasks.")
  }
  defaults <- get_default_params("naive_Bayes")

  if (tune) {
    model_spec <- naive_Bayes(
      smoothness = tune(),
      Laplace = tune()
    ) %>%
      set_mode("classification") %>%
      set_engine(engine)
  } else {
    model_spec <- naive_Bayes(
      smoothness = defaults$smoothness,
      Laplace = defaults$Laplace
    ) %>%
      set_mode("classification") %>%
      set_engine(engine)
  }
  list(model_spec = model_spec)
}

#' Define K-Nearest Neighbors Model Specification
#'
#' @inheritParams define_rand_forest_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip nearest_neighbor set_mode set_engine
#' @noRd
define_nearest_neighbor_spec <- function(task, tune = FALSE, engine = "kknn") {
  defaults <- get_default_params("nearest_neighbor")

  if (tune) {
    model_spec <- nearest_neighbor(
      neighbors = tune(),
      weight_func = tune(),
      dist_power = tune()
    ) %>%
      set_mode(task) %>%
      set_engine(engine)
  } else {
    model_spec <- nearest_neighbor(
      neighbors = defaults$neighbors,
      weight_func = defaults$weight_func,
      dist_power = defaults$dist_power
    ) %>%
      set_mode(task) %>%
      set_engine(engine)
  }
  list(model_spec = model_spec)
}

#' Define SVM Radial Model Specification
#'
#' @inheritParams define_svm_linear_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip svm_rbf set_mode set_engine
#' @noRd
define_svm_rbf_spec <- function(task, tune = FALSE, engine = "kernlab") {
  defaults <- get_default_params("svm_rbf")

  if (tune) {
    model_spec <- svm_rbf(
      cost = tune(),
      rbf_sigma = tune()
    ) %>%
      set_mode(task) %>%
      set_engine(engine)
  } else {
    model_spec <- svm_rbf(
      cost = defaults$cost,
      rbf_sigma = defaults$rbf_sigma
    ) %>%
      set_mode(task) %>%
      set_engine(engine)
  }
  list(model_spec = model_spec)
}

#' Define SVM Linear Model Specification
#'
#' @inheritParams define_rand_forest_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip svm_linear set_mode set_engine
#' @noRd
define_svm_linear_spec <- function(task, tune = FALSE, engine = "kernlab") {
  defaults <- get_default_params("svm_linear")

  if (tune) {
    model_spec <- svm_linear(
      cost = tune()
    ) %>%
      set_mode(task) %>%
      set_engine(engine)
  } else {
    model_spec <- svm_linear(
      cost = defaults$cost
    ) %>%
      set_mode(task) %>%
      set_engine(engine)
  }
  list(model_spec = model_spec)
}

#' Define Decision Tree Model Specification
#'
#' @inheritParams define_rand_forest_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip decision_tree set_mode set_engine
#' @noRd
define_decision_tree_spec <- function(task, tune = FALSE, engine = "rpart") {
  defaults <- get_default_params("decision_tree")

  if (tune) {
    model_spec <- decision_tree(
      tree_depth = tune(),
      min_n = tune(),
      cost_complexity = tune()
    ) %>%
      set_mode(task) %>%
      set_engine(engine)
  } else {
    model_spec <- decision_tree(
      tree_depth = defaults$tree_depth,
      min_n = defaults$min_n,
      cost_complexity = defaults$cost_complexity
    ) %>%
      set_mode(task) %>%
      set_engine(engine)
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
define_logistic_reg_spec <- function(task, tune = FALSE, engine = "glm") {
  if (task != "classification") {
    stop("Logistic regression is only applicable for classification tasks.")
  }

  # Retrieve default parameters for logistic_reg (includes penalty and mixture on log scale)
  defaults <- get_default_params("logistic_reg", task, engine = engine)

  if (tune) {
    if (engine %in% c("glm", "gee", "glmer", "stan", "stan_glmer")) {
      # These engines do not have tunable penalty parameters
      model_spec <- logistic_reg(
        penalty = NULL,
        mixture = NULL
      )
      if (engine == "glm") {
        model_spec <- model_spec %>%
          set_engine("glm", family = stats::binomial())
      } else if (engine == "gee") {
        model_spec <- model_spec %>%
          set_engine("gee", corstr = "exchangeable")
      } else if (engine == "glmer") {
        model_spec <- model_spec %>%
          set_engine("glmer")
      } else if (engine %in% c("stan", "stan_glmer")) {
        model_spec <- model_spec %>%
          set_engine(engine, chains = 4L, iter = 2000L, seed = 123, cores = 1L,
                     prior = NULL, prior_intercept = NULL)
      }
      model_spec <- model_spec %>% set_mode("classification")

    } else if (engine %in% c("brulee", "glmnet", "h2o", "LiblineaR", "spark")) {
      model_spec <- logistic_reg(
        penalty = tune(),
        mixture = tune()
      )
      if (engine == "brulee") {
        model_spec <- model_spec %>%
          set_engine("brulee",
                     optimizer = "SGD",
                     epochs = 50L,
                     learn_rate = -2,    # raw 0.01 -> log10(0.01) = -2
                     momentum = 0.9,
                     batch_size = 32,
                     stop_iter = 5L,
                     class_weights = NULL)
      } else if (engine == "glmnet") {
        model_spec <- model_spec %>%
          set_engine("glmnet", standardize = TRUE)
      } else if (engine == "h2o") {
        model_spec <- model_spec %>%
          set_engine("h2o", compute_p_values = FALSE, lambda_search = TRUE, nthreads = -1)
      } else if (engine == "LiblineaR") {
        model_spec <- model_spec %>%
          set_engine("LiblineaR", cost = Inf, verbose = FALSE)
      } else if (engine == "spark") {
        model_spec <- model_spec %>%
          set_engine("spark", standardization = TRUE, reg_param = 0.0, elastic_net_param = 0.0)
      }
      model_spec <- model_spec %>% set_mode("classification")

    } else if (engine == "keras") {
      model_spec <- logistic_reg(
        penalty = tune(),
        mixture = NULL
      ) %>%
        set_engine("keras", hidden_units = 1, act = "linear") %>%
        set_mode("classification")
    } else {
      stop("Unsupported engine specified for logistic regression.")
    }

  } else {
    # Not tuning; use default penalty/mixture values from our defaults.
    model_spec <- logistic_reg(
      penalty = defaults$penalty,
      mixture = defaults$mixture
    )
    if (engine == "glm") {
      model_spec <- model_spec %>%
        set_engine("glm", family = stats::binomial())
    } else if (engine == "gee") {
      model_spec <- model_spec %>%
        set_engine("gee", corstr = "exchangeable")
    } else if (engine == "glmer") {
      model_spec <- model_spec %>%
        set_engine("glmer")
    } else if (engine %in% c("stan", "stan_glmer")) {
      model_spec <- model_spec %>%
        set_engine(engine, chains = 4L, iter = 2000L, seed = 123, cores = 1L,
                   prior = NULL, prior_intercept = NULL)
    } else if (engine == "brulee") {
      model_spec <- model_spec %>%
        set_engine("brulee",
                   optimizer = "SGD",
                   epochs = 50L,
                   learn_rate = -2,
                   momentum = 0.9,
                   batch_size = 32,
                   stop_iter = 5L,
                   class_weights = NULL)
    } else if (engine == "glmnet") {
      model_spec <- model_spec %>%
        set_engine("glmnet", standardize = TRUE)
    } else if (engine == "h2o") {
      model_spec <- model_spec %>%
        set_engine("h2o", compute_p_values = FALSE, lambda_search = TRUE)
    } else if (engine == "keras") {
      model_spec <- model_spec %>%
        set_engine("keras", hidden_units = 1, act = "linear")
    } else if (engine == "LiblineaR") {
      model_spec <- model_spec %>%
        set_engine("LiblineaR", cost = Inf, verbose = FALSE)
    } else if (engine == "spark") {
      model_spec <- model_spec %>%
        set_engine("spark", standardization = TRUE, reg_param = 0.0, elastic_net_param = 0.0)
    } else {
      stop("Unsupported engine specified for logistic regression.")
    }
    model_spec <- model_spec %>% set_mode("classification")
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
define_multinomial_regression_spec <- function(task, tune = FALSE, engine) {
  if (task != "classification") {
    stop("Multinomial logistic regression is only applicable for classification tasks.")
  }
  if (missing(engine)) {
    engine <- "glm"
  }
  if (tune) {
    model_spec <- multinom_reg(
      penalty = tune(),
      mixture = tune()
    ) %>%
      set_mode("classification") %>%
      set_engine(engine)
  } else {
    model_spec <- multinom_reg() %>%
      set_mode("classification") %>%
      set_engine(engine)
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
define_pls_spec <- function(task, tune = FALSE, engine = "mixOmics") {
  if (task != "regression") {
    stop("PLS is only applicable for regression tasks.")
  }
  defaults <- get_default_params("pls")

  if (tune) {
    model_spec <- pls(
      num_comp = tune()
    ) %>%
      set_mode("regression") %>%
      set_engine(engine)
  } else {
    model_spec <- pls(
      num_comp = defaults$num_comp
    ) %>%
      set_mode("regression") %>%
      set_engine(engine)
  }
  list(model_spec = model_spec)
}
