define_lasso_regression_spec <- function(task, tuning = FALSE, engine = "glmnet") {
  if (task != "regression") {
    stop("Lasso regression is only applicable for regression tasks.")
  }
  defaults <- get_default_params("lasso_regression")

  if (tuning) {
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
#' @importFrom tune tune
#' @noRd
define_ridge_regression_spec <- function(task, tuning = FALSE, engine = "glmnet") {
  if (task != "regression") {
    stop("Ridge regression is only applicable for regression tasks.")
  }
  defaults <- get_default_params("ridge_regression")

  if (tuning) {
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
#' @importFrom tune tune
#' @noRd
define_elastic_net_spec <- function(task, tuning = FALSE, engine = "glmnet") {
  if (task != "regression") {
    stop("Elastic Net is only applicable for regression tasks.")
  }
  defaults <- get_default_params("elastic_net")
  if (tuning) {
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
#' @importFrom parsnip bag_tree set_mode set_engine logistic_reg
#' @import baguette
#' @importFrom tune tune
#' @noRd
define_logistic_reg_spec <- function(task, tuning = FALSE, engine = "glm") {
  if (task != "classification") {
    stop("Logistic regression is only applicable for classification tasks.")
  }

  defaults <- get_default_params("logistic_reg", task, engine = engine)

  if (tuning) {
    if (engine %in% c("glm", "gee", "glmer", "stan", "stan_glmer")) {
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
                     learn_rate = 1,    # raw 0.01 -> log10(0.01) = -2
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
                   learn_rate = 1,
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
#' @importFrom tune tune
#' @noRd
define_multinomial_reg_spec <- function(task, tuning = FALSE, engine) {
  if (task != "classification") {
    stop("Multinomial logistic regression is only applicable for classification tasks.")
  }

  # Use a default engine if none is specified.
  if (missing(engine)) {
    engine <- "nnet"
  }

  # Build the base model specification based on whether tuning is requested.
  if (tuning) {
    if (engine %in% c("nnet", "keras")) {
      # For nnet and keras, only the penalty tuning parameter is supported.
      model_spec <- multinom_reg(
        penalty = tune()
      )
    } else if (engine %in% c("brulee", "glmnet", "h2o", "spark")) {
      # For these engines, both penalty and mixture can be tuned.
      model_spec <- multinom_reg(
        penalty = tune(),
        mixture = tune()
      )
    } else {
      stop("Unsupported engine specified for multinom_reg")
    }
  } else {
    # When not tuning, use the default settings provided by multinom_reg()
    model_spec <- multinom_reg()
  }

  # Set the model mode.
  model_spec <- model_spec %>% set_mode("classification")

  # Engine-specific settings.
  if (engine == "nnet") {
    # nnet::multinom() uses a 'decay' parameter corresponding to penalty.
    model_spec <- model_spec %>%
      set_engine("nnet", trace = FALSE)
  } else if (engine == "brulee") {
    # brulee supports extra arguments (optimizer, epochs, learn_rate, etc.).
    model_spec <- model_spec %>%
      set_engine("brulee",
                 optimizer    = "SGD",
                 epochs       = 50L,
                 learn_rate   = 0.01,
                 momentum     = 0.9,
                 batch_size   = 32,
                 stop_iter    = 5L,
                 class_weights = NULL)
  } else if (engine == "glmnet") {
    # glmnet requires specifying standardization and family = "multinomial"
    model_spec <- model_spec %>%
      set_engine("glmnet", standardize = TRUE, family = "multinomial")
  } else if (engine == "h2o") {
    # h2o uses agua::h2o_train_glm() behind the scenes.
    # It expects penalty and mixture translated to lambda and alpha.
    model_spec <- model_spec %>%
      set_engine("h2o", lambda_search = TRUE, solver = "AUTO")
  } else if (engine == "keras") {
    # keras_mlp() fits a linear network with one hidden unit.
    model_spec <- model_spec %>%
      set_engine("keras", hidden_units = 1, act = "linear")
  } else if (engine == "spark") {
    # For spark, the underlying function (sparklyr::ml_logistic_regression) expects
    # reg_param and elastic_net_param in place of penalty and mixture.
    model_spec <- model_spec %>%
      set_engine("spark", standardization = TRUE, reg_param = 0.0, elastic_net_param = 0.0)
  } else {
    stop("Unsupported engine specified for multinom_reg")
  }

  list(model_spec = model_spec)
}


#' Define Partial Least Squares Model Specification
#'
#' @inheritParams define_elastic_net_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip pls set_mode set_engine
#' @import plsmod
#' @importFrom tune tune
#' @noRd
define_pls_spec <- function(task, tuning = FALSE, engine = "mixOmics") {
  if (task != "regression") {
    stop("PLS is only applicable for regression tasks.")
  }
  defaults <- get_default_params("pls")

  if (tuning) {
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
