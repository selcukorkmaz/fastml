#' Define LightGBM Model Specification
#'
#' @inheritParams define_rand_forest_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip boost_tree set_mode set_engine
#' @importFrom dplyr select all_of
#' @importFrom tune tune
#' @import bonsai
#' @noRd
define_lightgbm_spec <- function(task, train_data, label, tuning = FALSE, engine = "lightgbm") {
  # Ensure the lightgbm package is installed.
  if (!requireNamespace("lightgbm", quietly = TRUE)) {
    stop("The 'lightgbm' package is required but is not installed.")
  }

  # Determine the number of predictors (all columns except the label).
  num_predictors <- ncol(train_data %>% dplyr::select(-dplyr::all_of(label)))

  # Retrieve default parameters for lightgbm.
  # The helper is expected to return a list with elements:
  # trees, tree_depth, learn_rate, mtry, min_n, loss_reduction, sample_size.
  defaults <- get_default_params("lightgbm", task, num_predictors, engine)

  # Define the model specification using boost_tree().
  if (tuning) {
    model_spec <- boost_tree(
      trees           = tune(),
      tree_depth      = tune(),
      learn_rate      = tune(),
      mtry            = tune(),
      min_n           = tune(),
      loss_reduction  = tune(),
      sample_size     = tune()
    )
  } else {
    model_spec <- boost_tree(
      trees           = defaults$trees,
      tree_depth      = defaults$tree_depth,
      learn_rate      = defaults$learn_rate,
      mtry            = defaults$mtry,
      min_n           = defaults$min_n,
      loss_reduction  = defaults$loss_reduction,
      sample_size     = defaults$sample_size
    )
  }

  # Set the mode and specify engine-specific parameters.
  # - counts = TRUE means that mtry is interpreted as a count (and later converted to a proportion)
  # - bagging_freq = 1 will enable bagging at every iteration when sample_size (bagging fraction) is less than 1.
  # - verbose = -1 quiets lightgbm's output.
  # - num_threads = 0 instructs lightgbm to use all available cores.
  # - seed and deterministic aid in reproducibility.
  model_spec <- model_spec %>%
    set_mode(task) %>%
    set_engine(engine,
               counts        = TRUE,
               bagging_freq  = 1,
               verbose       = -1,
               num_threads   = 0,
               seed          = 123,
               deterministic = TRUE)

  list(model_spec = model_spec)
}

#' Define XGBoost Model Specification
#'
#' @inheritParams define_rand_forest_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip boost_tree set_mode set_engine
#' @importFrom dplyr select all_of
#' @importFrom tune tune
#' @noRd
define_xgboost_spec <- function(task, train_data, label, tuning = FALSE, engine = "xgboost") {
  num_predictors <- ncol(train_data %>% dplyr::select(-dplyr::all_of(label)))
  defaults <- get_default_params("xgboost", num_predictors)

  if (tuning) {
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
#' @importFrom tune tune
#' @noRd
define_C5_rules_spec <- function(task, tuning = FALSE, engine = "C5.0") {
  if (task != "classification") {
    stop("C5.0 is only applicable for classification tasks.")
  }
  defaults <- get_default_params("C5_rules")

  if (tuning) {
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
#' @importFrom tune tune
#' @noRd
define_rand_forest_spec <- function(task, train_data, label, tuning = FALSE, engine = "ranger") {
  # Remove the label column to get predictors and count them
  predictors <- train_data %>% dplyr::select(-dplyr::all_of(label))
  num_predictors <- ncol(predictors)

  # Retrieve default parameters for a random forest.
  # (Assumes get_default_params returns a list with mtry, trees, and min_n.)
  defaults <- get_default_params("rand_forest", task, num_predictors, engine)

  # Build the base model spec with tuning or default values.
  if (tuning) {
    model_spec <- rand_forest(
      mtry  = tune(),
      trees = tune(),
      min_n = tune()
    )
  } else {
    model_spec <- rand_forest(
      mtry  = defaults$mtry,
      trees = defaults$trees,
      min_n = defaults$min_n
    )
  }

  # Set the model mode (e.g. "classification", "regression", etc.)
  model_spec <- model_spec %>% set_mode(task)

  # Engine-specific parameter settings:
  # - For ranger (the default engine), if doing classification, we enable probability estimates.
  # - For h2o and randomForest, we pass along the number of trees (or a similar argument).
  # - For spark, a seed is provided.
  # - Other engines (e.g., aorsf, partykit) are simply set by name.
  if (engine == "ranger") {
    if (task == "classification") {
      model_spec <- model_spec %>%
        set_engine("ranger", probability = TRUE)
    } else {
      model_spec <- model_spec %>%
        set_engine("ranger")
    }
  } else if (engine == "h2o") {
    # For h2o, you might want to pass lambda_search or nthreads, etc.
    model_spec <- model_spec %>%
      set_engine("h2o")
  } else if (engine == "randomForest") {
    # For randomForest, the argument is ntree instead of trees.
    model_spec <- model_spec %>%
      set_engine("randomForest")
  } else if (engine == "spark") {
    model_spec <- model_spec %>%
      set_engine("spark", seed = 1234)
  } else if (engine == "aorsf") {
    model_spec <- model_spec %>%
      set_engine("aorsf")
  } else if (engine == "partykit") {
    model_spec <- model_spec %>%
      set_engine("partykit")
  } else {
    stop("Unsupported engine specified for rand_forest")
  }

  list(model_spec = model_spec)
}


#' Define Lasso Regression Model Specification
#'
#' @inheritParams define_elastic_net_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip linear_reg set_mode set_engine
#' @importFrom tune tune
#' @noRd
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
#' @importFrom parsnip bag_tree set_mode set_engine
#' @import baguette
#' @importFrom tune tune
#' @noRd
define_bag_tree_spec <- function(task, tuning = FALSE, engine = "rpart") {
  defaults <- get_default_params("bag_tree")
  if (tuning) {
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
#' @importFrom tune tune
#' @noRd
define_mlp_spec <- function(task, tuning = FALSE, engine = "nnet") {

  defaults <- get_default_params("mlp", task, num_predictors = NULL, engine)

  if (tuning) {
    if (engine == "nnet") {
      model_spec <- mlp(
        hidden_units = tune(),
        penalty = tune(),
        epochs = tune()
      ) %>%
        set_mode(task) %>%
        set_engine(engine)
    } else if (engine == "brulee") {
      model_spec <- mlp(
        hidden_units = tune(),
        penalty = tune(),
        epochs = tune(),
        activation = tune(),
        dropout = tune(),
        learn_rate = tune()
        # mixture = tune()
      ) %>%
        set_mode(task) %>%
        set_engine(engine)
    } else if (engine == "h2o") {
      model_spec <- mlp(
        hidden_units = tune(),
        penalty = tune(),
        dropout = tune(),
        epochs = tune(),
        activation = tune(),
        learn_rate = tune()
      ) %>%
        set_mode(task) %>%
        set_engine(engine)
    } else if (engine == "keras") {
      model_spec <- mlp(
        hidden_units = tune(),
        penalty = tune(),
        dropout = tune(),
        epochs = tune(),
        activation = tune()
      ) %>%
        set_mode(task) %>%
        set_engine(engine)
    }
  } else {
    if (engine == "nnet") {
      model_spec <- mlp(
        hidden_units = defaults$hidden_units,
        penalty = defaults$penalty,
        epochs = defaults$epochs
      ) %>%
        set_mode(task) %>%
        set_engine(engine)
    } else if (engine == "brulee") {
      model_spec <- mlp(
        hidden_units = defaults$hidden_units,
        penalty = defaults$penalty,
        epochs = defaults$epochs,
        activation = defaults$activation,
        dropout = defaults$dropout,
        learn_rate = defaults$learn_rate
        # mixture = defaults$mixture
      ) %>%
        set_mode(task) %>%
        set_engine(engine)
    } else if (engine == "h2o") {
      model_spec <- mlp(
        hidden_units = defaults$hidden_units,
        penalty = defaults$penalty,
        dropout = defaults$dropout,
        epochs = defaults$epochs,
        activation = defaults$activation,
        learn_rate = defaults$learn_rate
      ) %>%
        set_mode(task) %>%
        set_engine(engine)
    } else if (engine == "keras") {
      model_spec <- mlp(
        hidden_units = defaults$hidden_units,
        penalty = defaults$penalty,
        dropout = defaults$dropout,
        epochs = defaults$epochs,
        activation = defaults$activation
      ) %>%
        set_mode(task) %>%
        set_engine(engine)
    }
  }

  list(model_spec = model_spec)
}

#' Define Naive Bayes Model Specification
#'
#' @inheritParams define_logistic_reg_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip naive_Bayes set_mode set_engine
#' @import discrim
#' @importFrom tune tune
#' @noRd
define_naive_Bayes_spec <- function(task, tuning = FALSE, engine = "klaR") {
  if (task != "classification") {
    stop("Naive Bayes is only applicable for classification tasks.")
  }

  # Retrieve default parameters for naive_Bayes based on engine.
  defaults <- get_default_params("naive_Bayes", task, engine)

  # Build the base model spec with tuning or default values.
  if (tuning) {
    if (engine %in% c("klaR", "naivebayes")) {
      model_spec <- naive_Bayes(
        smoothness = tune(),
        Laplace = tune()
      )
    }else if (engine == "h2o") {

      model_spec <- naive_Bayes(
        Laplace = tune()
      )
    }
  } else {
    if (engine %in% c("klaR", "naivebayes")) {
      model_spec <- naive_Bayes(
        smoothness = defaults$smoothness,
        Laplace = defaults$Laplace
      )
    }else if (engine == "h2o") {
      model_spec <- naive_Bayes(
        Laplace = defaults$Laplace
      )
    }
  }

  # Set the model mode.
  model_spec <- model_spec %>% set_mode("classification")

  # Engine-specific parameter settings.
  # For the klaR and naivebayes engines, usekernel is set to TRUE by default.
  if (engine %in% c("klaR", "naivebayes")) {
    model_spec <- model_spec %>% set_engine(engine, usekernel = TRUE)
  } else if (engine == "h2o") {
    model_spec <- model_spec %>% set_engine(engine)
  } else {
    stop("Unsupported engine specified for naive_Bayes.")
  }

  list(model_spec = model_spec)
}

#' Define K-Nearest Neighbors Model Specification
#'
#' @inheritParams define_rand_forest_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip nearest_neighbor set_mode set_engine
#' @importFrom tune tune
#' @noRd
define_nearest_neighbor_spec <- function(task, tuning = FALSE, engine = "kknn") {
  defaults <- get_default_params("nearest_neighbor")

  if (tuning) {
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
#' @importFrom tune tune
#' @noRd
define_svm_rbf_spec <- function(task, tuning = FALSE, engine = "kernlab") {
  defaults <- get_default_params("svm_rbf")

  if (tuning) {
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
#' @importFrom tune tune
#' @noRd
define_svm_linear_spec <- function(task, tuning = FALSE, engine = "kernlab") {
  defaults <- get_default_params("svm_linear")

  if (tuning) {
    if (task == "regression") {
      # For regression, both cost and margin are tunable
      model_spec <- svm_linear(
        cost   = tune(),
        margin = tune()
      ) %>%
        set_mode(task) %>%
        set_engine(engine)
    } else {  # classification
      # For classification, only cost is tuned; margin is not applicable
      model_spec <- svm_linear(
        cost = tune()
      ) %>%
        set_mode(task) %>%
        set_engine(engine)
    }
  } else {
    if (task == "regression") {
      model_spec <- svm_linear(
        cost   = defaults$cost,
        margin = defaults$margin  # defaults$margin will be 0.1 for kernlab or missing for LiblineaR
      ) %>%
        set_mode(task) %>%
        set_engine(engine)
    } else {  # classification
      model_spec <- svm_linear(
        cost = defaults$cost
      ) %>%
        set_mode(task) %>%
        set_engine(engine)
    }
  }
  list(model_spec = model_spec)
}


#' Define Decision Tree Model Specification
#'
#' @inheritParams define_rand_forest_spec
#' @param task A character string indicating the mode (e.g., "classification" or "regression").
#' @param tuning Logical; if \code{TRUE}, the tuning parameters are set to \code{tune()}.
#' @param engine A character string specifying the computational engine. Supported engines include
#'   "rpart", "C5.0", and "partykit".
#'
#' @return List containing the model specification (`model_spec`).
#'
#' @details
#' The available parameters and their defaults vary by engine:
#'
#' - **rpart:**
#'   \itemize{
#'     \item \code{tree_depth}: Tree Depth (default: 30L)
#'     \item \code{min_n}: Minimal Node Size (default: 2L)
#'     \item \code{cost_complexity}: Cost-Complexity Parameter (default: 0.01)
#'   }
#'
#' - **C5.0:**
#'   \itemize{
#'     \item \code{min_n}: Minimal Node Size (default: 2L)
#'   }
#'
#' - **partykit:**
#'   \itemize{
#'     \item \code{tree_depth}: Tree Depth (default: 30L; adjust as needed)
#'     \item \code{min_n}: Minimal Node Size (default: 20L)
#'   }
#'
#' @importFrom parsnip decision_tree set_mode set_engine
#' @importFrom tune tune
#' @noRd
define_decision_tree_spec <- function(task, tuning = FALSE, engine = "rpart") {

  # Set engine-specific defaults
  defaults <- switch(engine,
                     "rpart" = list(tree_depth = 30L, min_n = 2L, cost_complexity = 0.01),
                     "C5.0" = list(min_n = 2L),
                     "partykit" = list(tree_depth = 30L, min_n = 20L),
                     # fallback: assume rpart defaults
                     list(tree_depth = 30L, min_n = 2L, cost_complexity = 0.01)
  )

  # Build the model specification based on the engine and tuning flag.
  if (engine == "rpart") {
    if (tuning) {
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
  } else if (engine == "C5.0") {
    if (tuning) {
      model_spec <- decision_tree(
        min_n = tune()
      ) %>%
        set_mode(task) %>%
        set_engine(engine)
    } else {
      model_spec <- decision_tree(
        min_n = defaults$min_n
      ) %>%
        set_mode(task) %>%
        set_engine(engine)
    }
  } else if (engine == "partykit") {
    if (tuning) {
      model_spec <- decision_tree(
        tree_depth = tune(),
        min_n = tune()
      ) %>%
        set_mode(task) %>%
        set_engine(engine)
    } else {
      model_spec <- decision_tree(
        tree_depth = defaults$tree_depth,
        min_n = defaults$min_n
      ) %>%
        set_mode(task) %>%
        set_engine(engine)
    }
  } else {
    # Fallback for unknown engine: assume rpart defaults
    if (tuning) {
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
  }

  list(model_spec = model_spec)
}


#' Define Logistic Regression Model Specification
#'
#' @param task Character string specifying the task type ("classification").
#' @param tune Logical indicating whether to use tuning parameters.
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip logistic_reg set_mode set_engine
#' @importFrom tune tune
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
