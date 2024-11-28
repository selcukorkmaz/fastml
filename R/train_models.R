#' Train Specified Machine Learning Algorithms on the Training Data
#'
#' Trains specified machine learning algorithms on the preprocessed training data.
#'
#' @param train_data Preprocessed training data frame.
#' @param label Name of the target variable.
#' @param task Type of task: "classification" or "regression".
#' @param algorithms Vector of algorithm names to train.
#' @param resampling_method Resampling method for cross-validation (e.g., "cv", "repeatedcv").
#' @param folds Number of folds for cross-validation.
#' @param repeats Number of times to repeat cross-validation (only applicable for methods like "repeatedcv").
#' @param tune_params List of hyperparameter tuning ranges.
#' @param metric The performance metric to optimize.
#' @param summaryFunction A custom summary function for model evaluation. Default is \code{NULL}.
#' @param seed An integer value specifying the random seed for reproducibility.
#' @param recipe A recipe object for preprocessing.
#' @importFrom magrittr %>%
#' @importFrom dplyr filter mutate select
#' @importFrom rlang sym
#' @importFrom dials range_set value_set grid_regular
#' @importFrom parsnip fit extract_parameter_set_dials
#' @importFrom workflows workflow add_model add_recipe
#' @importFrom tune tune_grid control_grid select_best finalize_workflow finalize_model
#' @importFrom yardstick metric_set
#' @importFrom rsample vfold_cv bootstraps
#' @return A list of trained model objects.
#' @export
train_models <- function(train_data,
                         label,
                         task,
                         algorithms,
                         resampling_method,
                         folds,
                         repeats,
                         tune_params,
                         metric,
                         summaryFunction = NULL,
                         seed = 123,
                         recipe) {
  # Load required packages
  if (!requireNamespace("tidymodels", quietly = TRUE)) {
    stop("The 'tidymodels' package is required but not installed.")
  }
  library(tidymodels)

  # Set random seed
  set.seed(seed)

  # Decide on metrics
  if (task == "classification") {
    if (metric == "roc_auc") {
      metrics <- metric_set(roc_auc)
    } else if (metric == "accuracy") {
      metrics <- metric_set(accuracy)
    } else {
      metrics <- metric_set(accuracy)
    }
  } else {
    if (metric == "rmse") {
      metrics <- metric_set(rmse)
    } else if (metric == "rsq") {
      metrics <- metric_set(rsq)
    } else {
      metrics <- metric_set(rmse)
    }
  }

  # Set up resampling
  if (resampling_method == "cv") {
    resamples <- vfold_cv(train_data, v = folds, repeats = 1, strata = if (task == "classification") label else NULL)
  } else if (resampling_method == "boot") {
    resamples <- bootstraps(train_data, times = folds, strata = if (task == "classification") label else NULL)
  } else if (resampling_method == "repeatedcv") {
    resamples <- vfold_cv(train_data, v = folds, repeats = repeats, strata = if (task == "classification") label else NULL)
  } else if (resampling_method == "none") {
    resamples <- NULL
  } else {
    stop("Unsupported resampling method.")
  }

  # Initialize models list
  models <- list()

  # Helper function to update parameters
  update_params <- function(params_model, new_params) {
    for (param_name in names(new_params)) {
      param_value <- new_params[[param_name]]

      # Locate the parameter in the parameter set
      param_row <- params_model %>% filter(id == param_name)
      if (nrow(param_row) == 0) {
        next  # Parameter not found, skip to the next one
      }

      # Get the parameter object
      param_obj <- param_row$object[[1]]

      if (length(param_value) == 2) {
        # It's a range; ensure the type matches
        if (inherits(param_obj, "integer_parameter")) {
          param_obj <- param_obj %>% range_set(as.integer(param_value))
        } else {
          param_obj <- param_obj %>% range_set(param_value)
        }
      } else {
        # Single value; ensure the type matches
        if (inherits(param_obj, "integer_parameter")) {
          param_obj <- param_obj %>% value_set(as.integer(param_value))
        } else {
          param_obj <- param_obj %>% value_set(param_value)
        }
      }

      # Update the parameter in the parameter set
      params_model <- params_model %>%
        mutate(object = ifelse(id == param_name, list(param_obj), object))
    }
    return(params_model)
  }


  # Loop over each algorithm
  for (algo in algorithms) {
    set.seed(seed)
    model <- NULL
    # Define model specification and tuning grid
    model_info <- switch(algo,
                         "random_forest" = define_random_forest_spec(task, train_data),
                         "ranger" = define_ranger_spec(task, train_data),
                         "gbm" = define_gbm_spec(task),
                         "c5.0" = define_c5_0_spec(task),
                         "xgboost" = define_xgboost_spec(task, train_data),
                         "lightgbm" = define_lightgbm_spec(task),
                         # "catboost" = define_catboost_spec(task),
                         "logistic_regression" = define_logistic_regression_spec(task),
                         "penalized_logistic_regression" = define_penalized_logistic_regression_spec(task),
                         "decision_tree" = define_decision_tree_spec(task),
                         "svm_linear" = define_svm_linear_spec(task),
                         "svm_radial" = define_svm_radial_spec(task),
                         "knn" = define_knn_spec(task),
                         "naive_bayes" = define_naive_bayes_spec(task),
                         "neural_network" = define_neural_network_spec(task),
                         "deep_learning" = define_deep_learning_spec(task),
                         "lda" = define_lda_spec(task),
                         "qda" = define_qda_spec(task),
                         "bagging" = define_bagging_spec(task),
                         # "adaboost" = define_adaboost_spec(task),
                         # "logitboost" = define_logitboost_spec(task),
                         "elastic_net" = define_elastic_net_spec(task),
                         "bayes_glm" = define_bayes_glm_spec(task),
                         "pls" = define_pls_spec(task),
                         # "glmboost" = define_glmboost_spec(task),
                         "linear_regression" = define_linear_regression_spec(task),
                         "ridge_regression" = define_ridge_regression_spec(task),
                         "lasso_regression" = define_lasso_regression_spec(task),
                         "stacking" = define_stacking_spec(task, models, metric),
                         "blending" = define_blending_spec(task, models, metric),
                         "voting" = define_voting_spec(task, models, metric),
                         {
                           warning(paste("Algorithm", algo, "is not supported or failed to train."))
                           next
                         })
    model_spec <- model_info$model_spec
    default_params <- model_info$default_params
    required_params <- model_info$required_params

    # Obtain tunable parameters
    tune_params_model <- extract_parameter_set_dials(model_spec)

    # Finalize parameters that depend on the data
    tune_params_model <- finalize(
      tune_params_model,
      x = train_data %>% select(-!!sym(label))
    )

    # Update parameter ranges with default_params
    if (!is.null(default_params) && length(default_params) > 0) {
      tune_params_model <- update_params(tune_params_model, default_params)
    }

    # Update parameter ranges with user-provided tune_params
    if (!is.null(tune_params) && !is.null(tune_params[[algo]])) {
      user_params <- tune_params[[algo]]
      tune_params_model <- update_params(tune_params_model, user_params)
    }

    # Now create the tuning grid
    if (!is.null(tune_params_model) && nrow(tune_params_model) > 0) {
      tune_grid <- grid_regular(
        tune_params_model,
        levels = 5
      )
    } else {
      # No tunable parameters
      tune_grid <- NULL
    }

    # Create a workflow
    workflow <- workflow() %>%
    add_model(model_spec) %>%
    add_recipe(recipe)

    # Perform tuning or fit the model directly if no tuning parameters
    tryCatch({
      if (!is.null(tune_grid) && !is.null(resamples)) {
        # Perform tuning
        model_tuned <- tune_grid(
          workflow,
          resamples = resamples,
          grid = tune_grid,
          metrics = metrics,
          control = control_grid(save_pred = TRUE)
        )
        # Finalize the workflow with the best parameters
        best_params <- select_best(model_tuned, metric = metric)
        final_workflow <- finalize_workflow(workflow, best_params)
        # Fit the final model on the entire training data
        model <- fit(final_workflow, data = train_data)
      } else {
        # Fit the model directly
        model <- fit(workflow, data = train_data)
      }
      models[[algo]] <- model
    }, error = function(e) {
      warning(paste(
        "Training failed for algorithm:",
        algo,
        "\nError message:",
        e$message
      ))
    })
  }
  return(models)
}


#' Define Logistic Regression Model Specification
#'
#' Creates a logistic regression model specification using the `glm` engine for classification tasks.
#'
#' @param task A character string specifying the type of task. Must be `"classification"`.
#' @return A list containing the model specification (`model_spec`), default parameters (`default_params`), and required parameters (`required_params`).
#' @importFrom magrittr %>%
#' @importFrom parsnip logistic_reg set_engine
#' @export
define_logistic_regression_spec <- function(task) {
  if (task != "classification") {
    stop("Logistic regression is only applicable for classification tasks.")
  }
  model_spec <- logistic_reg() %>%
    set_engine("glm")
  default_params <- list()
  required_params <- c()
  list(
    model_spec = model_spec,
    default_params = default_params,
    required_params = required_params
  )
}

#' Define Penalized Logistic Regression Model Specification
#'
#' Creates a penalized logistic regression model specification using the `glmnet` engine for classification tasks.
#'
#' @param task A character string specifying the type of task. Must be `"classification"`.
#' @return A list containing the model specification, default parameters, and required parameters.
#' @importFrom magrittr %>%
#' @importFrom parsnip logistic_reg set_engine
#' @importFrom tune tune
#' @export
define_penalized_logistic_regression_spec <- function(task) {
  if (task != "classification") {
    stop("Penalized logistic regression is only applicable for classification tasks.")
  }
  model_spec <- logistic_reg(penalty = tune(), mixture = tune()) %>%
    set_engine("glmnet")
  default_params <- list(
    penalty = c(0.001, 10),  # Range for penalty
    mixture = c(0, 1)        # Range for mixture
  )
  required_params <- c("penalty", "mixture")
  list(
    model_spec = model_spec,
    default_params = default_params,
    required_params = required_params
  )
}

#' Define Decision Tree Model Specification
#'
#' Creates a decision tree model specification using the `rpart` engine for classification or regression tasks.
#'
#' @param task A character string specifying the type of task: `"classification"` or `"regression"`.
#' @return A list containing the model specification, default parameters, and required parameters.
#' @importFrom magrittr %>%
#' @importFrom parsnip decision_tree set_engine set_mode
#' @importFrom tune tune
#' @export
define_decision_tree_spec <- function(task) {
  model_spec <- decision_tree(
    cost_complexity = tune(),
    tree_depth = tune(),
    min_n = tune()
  ) %>%
    set_engine("rpart") %>%
    set_mode(task)
  default_params <- list(
    cost_complexity = c(0.0, 0.1),  # Range for cost_complexity
    tree_depth = c(1, 10),          # Range for tree_depth
    min_n = c(2, 20)                # Range for min_n
  )
  required_params <- c("cost_complexity", "tree_depth", "min_n")
  list(
    model_spec = model_spec,
    default_params = default_params,
    required_params = required_params
  )
}

#' Define C5.0 Model Specification
#'
#' Creates a C5.0 decision tree model specification using the `C5.0` engine for classification tasks.
#'
#' @param task A character string specifying the type of task. Must be `"classification"`.
#' @return A list containing the model specification, default parameters, and required parameters.
#' @importFrom magrittr %>%
#' @importFrom parsnip boost_tree set_engine set_mode
#' @importFrom tune tune
#' @export
define_c5_0_spec <- function(task) {
  if (task != "classification") {
    stop("C5.0 is only applicable for classification tasks.")
  }
  if (!requireNamespace("C50", quietly = TRUE)) {
    stop("The 'C50' package is required for C5.0 but is not installed.")
  }
  model_spec <- boost_tree(
    trees = tune(),
    min_n = tune()
    # Removed tree_depth
  ) %>%
    set_engine("C5.0") %>%
    set_mode("classification")
  default_params <- list(
    trees = c(10, 100),      # Range for trees
    min_n = c(2, 20)
    # Removed tree_depth
  )
  required_params <- c("trees", "min_n")
  list(
    model_spec = model_spec,
    default_params = default_params,
    required_params = required_params
  )
}


#' Define Random Forest Model Specification
#'
#' Creates a random forest model specification using the `ranger` engine for classification or regression tasks.
#'
#' @param task A character string specifying the type of task: `"classification"` or `"regression"`.
#' @param train_data A data frame containing the training data (used to determine the number of predictors).
#' @return A list containing the model specification, default parameters, and required parameters.
#' @importFrom magrittr %>%
#' @importFrom parsnip rand_forest set_mode set_engine
#' @importFrom tune tune
#' @export
define_random_forest_spec <- function(task, train_data) {
  num_predictors <- ncol(train_data) - 1
  if (task == "classification") {
    model_spec <- rand_forest(
      trees = tune(),
      mtry = tune(),
      min_n = tune()
    ) %>%
      set_mode("classification") %>%
      set_engine("ranger")
  } else {
    model_spec <- rand_forest(
      trees = tune(),
      mtry = tune(),
      min_n = tune()
    ) %>%
      set_mode("regression") %>%
      set_engine("ranger")
  }
  default_params <- list(
    trees = c(500, 1000),                     # Range for trees
    mtry = c(1, min(10, num_predictors)),     # Range for mtry
    min_n = c(1, 10)                          # Range for min_n
  )
  required_params <- c("trees", "mtry", "min_n")
  return(list(
    model_spec = model_spec,
    default_params = default_params,
    required_params = required_params
  ))
}

#' Define Ranger Model Specification
#'
#' Creates a random forest model specification using the `ranger` engine, which is an alternative implementation of random forests.
#'
#' @param task A character string specifying the type of task: `"classification"` or `"regression"`.
#' @param train_data A data frame containing the training data (used to determine the number of predictors).
#' @return A list containing the model specification, default parameters, and required parameters.
#' @importFrom magrittr %>%
#' @importFrom parsnip rand_forest set_engine set_mode
#' @importFrom tune tune
#' @export
define_ranger_spec <- function(task, train_data) {
  num_predictors <- ncol(train_data) - 1
  model_spec <- rand_forest(
    trees = tune(),
    mtry = tune(),
    min_n = tune()
  ) %>%
    set_engine("ranger") %>%
    set_mode(task)
  default_params <- list(
    trees = c(500, 1000),
    mtry = c(1, min(10, num_predictors)),
    min_n = c(1, 10)
  )
  required_params <- c("trees", "mtry", "min_n")
  list(
    model_spec = model_spec,
    default_params = default_params,
    required_params = required_params
  )
}


# define_ranger_spec <- function(task) {
#   # Check if the 'ranger' package is installed
#   if (!requireNamespace("ranger", quietly = TRUE)) {
#     stop("The 'ranger' package is required for Ranger but is not installed.")
#   }
#
#   # Define the model specification with tunable parameters
#   if (task == "classification") {
#     model_spec <- rand_forest(
#       mtry = tune(),    # Number of variables to possibly split at each node
#       min_n = tune(),   # Minimum number of data points in a node
#       trees = tune()    # Number of trees
#     ) %>%
#       set_mode("classification") %>%
#       set_engine("ranger")
#   } else {
#     model_spec <- rand_forest(
#       mtry = tune(),
#       min_n = tune(),
#       trees = tune()
#     ) %>%
#       set_mode("regression") %>%
#       set_engine("ranger")
#   }
#
#   # Define default parameter ranges
#   default_params <- list(
#     mtry = c(2L, 5L),        # Range for mtry (integer)
#     min_n = c(1L, 10L),      # Range for min_n (integer)
#     trees = c(100L, 500L)    # Range for trees (integer)
#   )
#
#   # Specify which parameters are required to be tuned
#   required_params <- c("mtry", "min_n", "trees")
#
#   return(list(
#     model_spec = model_spec,
#     default_params = default_params,
#     required_params = required_params
#   ))
# }


#' Define Gradient Boosting Machine (GBM) Model Specification
#'
#' Creates a GBM model specification using the `xgboost` engine for classification or regression tasks.
#'
#' @param task A character string specifying the type of task: `"classification"` or `"regression"`.
#' @return A list containing the model specification, default parameters, and required parameters.
#' @importFrom magrittr %>%
#' @importFrom parsnip boost_tree set_engine set_mode
#' @importFrom tune tune
#' @export
define_gbm_spec <- function(task) {
  if (task == "classification") {
    model_spec <- boost_tree(
      trees = tune(),
      tree_depth = tune(),
      learn_rate = tune(),
      loss_reduction = tune(),
      sample_size = tune(),
      min_n = tune()
    ) %>%
      set_mode("classification") %>%
      set_engine("xgboost")
  } else {
    model_spec <- boost_tree(
      trees = tune(),
      tree_depth = tune(),
      learn_rate = tune(),
      loss_reduction = tune(),
      sample_size = tune(),
      min_n = tune()
    ) %>%
      set_mode("regression") %>%
      set_engine("xgboost")
  }
  default_params <- list(
    trees = c(50, 200),       # Reduced range for trees
    tree_depth = c(2, 6),     # Reduced range for tree_depth
    learn_rate = c(0.05, 0.3), # Range for learn_rate
    loss_reduction = c(0, 10),  # Range for loss_reduction
    sample_size = c(0.5, 1.0),  # Range for sample_size
    min_n = c(1, 10)            # Range for min_n
  )
  required_params <- c("trees", "tree_depth", "learn_rate")
  return(list(
    model_spec = model_spec,
    default_params = default_params,
    required_params = required_params
  ))
}


#' Define XGBoost Model Specification
#'
#' Creates an XGBoost model specification using the `xgboost` engine for classification or regression tasks.
#'
#' @param task A character string specifying the type of task: `"classification"` or `"regression"`.
#' @param train_data A data frame containing the training data (used to determine the number of predictors).
#' @return A list containing the model specification, default parameters, and required parameters.
#' @importFrom magrittr %>%
#' @importFrom parsnip boost_tree set_engine set_mode
#' @importFrom tune tune
#' @export
define_xgboost_spec <- function(task, train_data) {
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    stop("The 'xgboost' package is required for XGBoost but is not installed.")
  }
  num_predictors <- ncol(train_data) - 1
  model_spec <- boost_tree(
    trees = tune(),
    tree_depth = tune(),
    learn_rate = tune(),
    loss_reduction = tune(),
    sample_size = tune(),
    min_n = tune()
  ) %>%
    set_engine("xgboost") %>%
    set_mode(task)
  default_params <- list(
    trees = c(500, 1000),                   # Range for trees
    tree_depth = c(2, 10),                  # Range for tree_depth
    learn_rate = c(0.01, 0.3),              # Range for learn_rate
    loss_reduction = c(0, 10),              # Range for loss_reduction
    sample_size = c(0.5, 1.0),              # Range for sample_size
    min_n = c(1, 10)                        # Range for min_n
  )
  required_params <- c("trees", "tree_depth", "learn_rate", "loss_reduction", "sample_size", "min_n")
  list(
    model_spec = model_spec,
    default_params = default_params,
    required_params = required_params
  )
}

#' Define LightGBM Model Specification
#'
#' Creates a LightGBM model specification using the `lightgbm` engine for classification or regression tasks.
#'
#' @param task A character string specifying the type of task: `"classification"` or `"regression"`.
#' @return A list containing the model specification, default parameters, and required parameters.
#' @importFrom magrittr %>%
#' @importFrom parsnip boost_tree set_engine set_mode
#' @importFrom tune tune
#' @export
define_lightgbm_spec <- function(task) {
  if (!requireNamespace("lightgbm", quietly = TRUE)) {
    stop("The 'lightgbm' package is required for LightGBM but is not installed.")
  }
  if (task == "classification") {
    model_spec <- boost_tree(
      trees = tune(),
      tree_depth = tune(),
      learn_rate = tune(),
      min_n = tune(),
      loss_reduction = tune(),
      sample_size = tune()
    ) %>%
      set_engine("lightgbm") %>%
      set_mode("classification")
  } else {
    model_spec <- boost_tree(
      trees = tune(),
      tree_depth = tune(),
      learn_rate = tune(),
      min_n = tune(),
      loss_reduction = tune(),
      sample_size = tune()
    ) %>%
      set_engine("lightgbm") %>%
      set_mode("regression")
  }
  default_params <- list(
    trees = c(50, 200),       # Reduced range for trees
    tree_depth = c(2, 6),     # Reduced range for tree_depth
    learn_rate = c(0.05, 0.3), # Range for learn_rate
    min_n = c(1, 10),
    loss_reduction = c(0, 10),
    sample_size = c(0.5, 1.0)
  )
  required_params <- c("trees", "tree_depth", "learn_rate")
  list(
    model_spec = model_spec,
    default_params = default_params,
    required_params = required_params
  )
}

# define_catboost_spec <- function(task) {
#   if (!requireNamespace("catboost", quietly = TRUE)) {
#     stop("The 'catboost' package is required for CatBoost but is not installed.")
#   }
#   model_spec <- boost_tree(
#     trees = tune(),
#     tree_depth = tune(),
#     learn_rate = tune(),
#     min_n = tune(),
#     loss_reduction = tune(),
#     sample_size = tune()
#   ) %>%
#     set_engine("catboost") %>%
#     set_mode(task)
#   default_params <- list(
#     trees = c(500, 1000),
#     tree_depth = c(2, 10),
#     learn_rate = c(0.01, 0.3),
#     min_n = c(1, 10),
#     loss_reduction = c(0, 10),
#     sample_size = c(0.5, 1.0)
#   )
#   required_params <- c("trees", "tree_depth", "learn_rate", "min_n", "loss_reduction", "sample_size")
#   list(
#     model_spec = model_spec,
#     default_params = default_params,
#     required_params = required_params
#   )
# }

#' Define SVM with Linear Kernel Model Specification
#'
#' Creates an SVM model specification with a linear kernel using the `kernlab` engine for classification or regression tasks.
#'
#' @param task A character string specifying the type of task: `"classification"` or `"regression"`.
#' @return A list containing the model specification, default parameters, and required parameters.
#' @importFrom magrittr %>%
#' @importFrom parsnip svm_linear set_engine set_mode
#' @importFrom tune tune
#' @export
define_svm_linear_spec <- function(task) {
  model_spec <- svm_linear(
    cost = tune()
  ) %>%
    set_engine("kernlab") %>%  # Changed engine from "LiblineaR" to "kernlab"
    set_mode(task)
  default_params <- list(
    cost = c(0.001, 1000)  # Range for cost
  )
  required_params <- c("cost")
  list(
    model_spec = model_spec,
    default_params = default_params,
    required_params = required_params
  )
}


#' Define SVM with Radial Basis Function Kernel Model Specification
#'
#' Creates an SVM model specification with an RBF kernel using the `kernlab` engine for classification or regression tasks.
#'
#' @param task A character string specifying the type of task: `"classification"` or `"regression"`.
#' @return A list containing the model specification, default parameters, and required parameters.
#' @importFrom magrittr %>%
#' @importFrom parsnip svm_rbf set_engine set_mode
#' @importFrom tune tune
#' @export
define_svm_radial_spec <- function(task) {
  model_spec <- svm_rbf(
    cost = tune(),
    rbf_sigma = tune()
  ) %>%
    set_engine("kernlab") %>%
    set_mode(task)
  default_params <- list(
    cost = c(0.001, 1000),  # Range for cost
    rbf_sigma = c(0.001, 1) # Range for rbf_sigma
  )
  required_params <- c("cost", "rbf_sigma")
  list(
    model_spec = model_spec,
    default_params = default_params,
    required_params = required_params
  )
}

#' Define k-Nearest Neighbors Model Specification
#'
#' Creates a k-NN model specification using the `kknn` engine for classification or regression tasks.
#'
#' @param task A character string specifying the type of task: `"classification"` or `"regression"`.
#' @return A list containing the model specification, default parameters, and required parameters.
#' @importFrom magrittr %>%
#' @importFrom parsnip nearest_neighbor set_engine set_mode
#' @importFrom tune tune
#' @export
define_knn_spec <- function(task) {
  model_spec <- nearest_neighbor(
    neighbors = tune(),
    weight_func = "rectangular",
    dist_power = 2
  ) %>%
    set_engine("kknn") %>%
    set_mode(task)
  default_params <- list(
    neighbors = c(1, 20)   # Range for neighbors
  )
  required_params <- c("neighbors")
  list(
    model_spec = model_spec,
    default_params = default_params,
    required_params = required_params
  )
}

#' Define Naive Bayes Model Specification
#'
#' Creates a Naive Bayes model specification using the `klaR` engine for classification tasks.
#'
#' @param task A character string specifying the type of task. Must be `"classification"`.
#' @return A list containing the model specification, default parameters, and required parameters.
#' @importFrom magrittr %>%
#' @importFrom parsnip naive_Bayes set_engine
#' @importFrom tune tune
#' @export
define_naive_bayes_spec <- function(task) {
  if (task != "classification") {
    stop("Naive Bayes is only applicable for classification tasks.")
  }
  model_spec <- naive_Bayes(
    smoothness = tune(),
    Laplace = tune()
  ) %>%
    set_engine("klaR")
  default_params <- list(
    smoothness = c(0, 1),  # Range for smoothness
    Laplace = c(0, 1)      # Range for Laplace
  )
  required_params <- c("smoothness", "Laplace")
  list(
    model_spec = model_spec,
    default_params = default_params,
    required_params = required_params
  )
}

#' Define Neural Network Model Specification
#'
#' Creates a neural network model specification using the `nnet` engine for classification or regression tasks.
#'
#' @param task A character string specifying the type of task: `"classification"` or `"regression"`.
#' @return A list containing the model specification, default parameters, and required parameters.
#' @importFrom magrittr %>%
#' @importFrom parsnip mlp set_engine set_mode
#' @importFrom tune tune
#' @export
define_neural_network_spec <- function(task) {
  model_spec <- mlp(
    hidden_units = tune(),
    penalty = tune(),
    epochs = tune()
  ) %>%
    set_engine("nnet") %>%
    set_mode(task)
  default_params <- list(
    hidden_units = c(1, 10),          # Range for hidden_units
    penalty = c(0.001, 1),            # Range for penalty
    epochs = c(50, 200)               # Range for epochs
  )
  required_params <- c("hidden_units", "penalty", "epochs")
  list(
    model_spec = model_spec,
    default_params = default_params,
    required_params = required_params
  )
}

#' Define Deep Learning Model Specification with Keras
#'
#' Creates a deep learning model specification using the `keras` engine for classification or regression tasks.
#'
#' @param task A character string specifying the type of task: `"classification"` or `"regression"`.
#' @return A list containing the model specification, default parameters, and required parameters.
#' @importFrom magrittr %>%
#' @importFrom parsnip mlp set_engine set_mode
#' @importFrom tune tune
#' @export
define_deep_learning_spec <- function(task) {
  if (!requireNamespace("keras", quietly = TRUE)) {
    stop("The 'keras' package is required for deep learning but is not installed.")
  }
  model_spec <- mlp(
    hidden_units = tune(),
    penalty = tune(),
    epochs = tune()
  ) %>%
    set_engine("keras") %>%
    set_mode(task)
  default_params <- list(
    hidden_units = c(10, 100),        # Range for hidden_units
    penalty = c(0.001, 1),            # Range for penalty
    epochs = c(50, 200)               # Range for epochs
  )
  required_params <- c("hidden_units", "penalty", "epochs")
  list(
    model_spec = model_spec,
    default_params = default_params,
    required_params = required_params
  )
}

#' Define Linear Discriminant Analysis Model Specification
#'
#' Creates an LDA model specification using the `MASS` engine for classification tasks.
#'
#' @param task A character string specifying the type of task. Must be `"classification"`.
#' @return A list containing the model specification, default parameters, and required parameters.
#' @importFrom magrittr %>%
#' @importFrom parsnip discrim_linear set_engine
#' @export
define_lda_spec <- function(task) {
  if (task != "classification") {
    stop("LDA is only applicable for classification tasks.")
  }
  model_spec <- discrim_linear() %>%
    set_engine("MASS")
  default_params <- list()
  required_params <- c()
  list(
    model_spec = model_spec,
    default_params = default_params,
    required_params = required_params
  )
}

#' Define Quadratic Discriminant Analysis Model Specification
#'
#' Creates a QDA model specification using the `MASS` engine for classification tasks.
#'
#' @param task A character string specifying the type of task. Must be `"classification"`.
#' @return A list containing the model specification, default parameters, and required parameters.
#' @importFrom magrittr %>%
#' @importFrom parsnip discrim_quad set_engine
#' @export
define_qda_spec <- function(task) {
  if (task != "classification") {
    stop("QDA is only applicable for classification tasks.")
  }
  model_spec <- discrim_quad() %>%
    set_engine("MASS")
  default_params <- list()
  required_params <- c()
  list(
    model_spec = model_spec,
    default_params = default_params,
    required_params = required_params
  )
}

#' Define Bagging Model Specification
#'
#' Creates a bagged decision tree model specification using the `rpart` engine for classification or regression tasks.
#'
#' @param task A character string specifying the type of task: `"classification"` or `"regression"`.
#' @return A list containing the model specification, default parameters, and required parameters.
#' @importFrom magrittr %>%
#' @importFrom parsnip bag_tree set_engine set_mode
#' @export
define_bagging_spec <- function(task) {
  model_spec <- bag_tree() %>%
    set_engine("rpart") %>%
    set_mode(task)
  default_params <- list()
  required_params <- c()
  list(
    model_spec = model_spec,
    default_params = default_params,
    required_params = required_params
  )
}


# define_adaboost_spec <- function(task) {
#   if (!requireNamespace("fastAdaboost", quietly = TRUE)) {
#     stop("The 'fastAdaboost' package is required for AdaBoost but is not installed.")
#   }
#   model_spec <- boost_tree(
#     trees = tune(),
#     learn_rate = tune()
#   ) %>%
#     set_engine("adaboost") %>%
#     set_mode(task)
#   default_params <- list(
#     trees = c(50, 500),          # Range for trees
#     learn_rate = c(0.01, 0.3)    # Range for learn_rate
#   )
#   required_params <- c("trees", "learn_rate")
#   list(
#     model_spec = model_spec,
#     default_params = default_params,
#     required_params = required_params
#   )
# }


# define_logitboost_spec <- function(task) {
#   if (task != "classification") {
#     stop("LogitBoost is only applicable for classification tasks.")
#   }
#   if (!requireNamespace("caTools", quietly = TRUE)) {
#     stop("The 'caTools' package is required for LogitBoost but is not installed.")
#   }
#   model_spec <- boost_tree(
#     trees = tune(),
#     learn_rate = tune()
#   ) %>%
#     set_engine("LogitBoost") %>%
#     set_mode("classification")
#   default_params <- list(
#     trees = c(50, 500),          # Range for trees
#     learn_rate = c(0.01, 0.3)    # Range for learn_rate
#   )
#   required_params <- c("trees", "learn_rate")
#   list(
#     model_spec = model_spec,
#     default_params = default_params,
#     required_params = required_params
#   )
# }

#' Define Elastic Net Model Specification
#'
#' Creates an elastic net model specification using the `glmnet` engine for regression tasks.
#'
#' @param task A character string specifying the type of task. Must be `"regression"`.
#' @return A list containing the model specification, default parameters, and required parameters.
#' @importFrom magrittr %>%
#' @importFrom parsnip linear_reg set_engine set_mode
#' @importFrom tune tune
#' @export
define_elastic_net_spec <- function(task) {
  if (task != "regression") {
    stop("Elastic Net is only applicable for regression tasks.")
  }
  model_spec <- linear_reg(penalty = tune(), mixture = tune()) %>%
    set_engine("glmnet") %>%
    set_mode("regression")
  default_params <- list(
    penalty = c(0.001, 10),  # Range for penalty
    mixture = c(0, 1)        # Range for mixture
  )
  required_params <- c("penalty", "mixture")
  list(
    model_spec = model_spec,
    default_params = default_params,
    required_params = required_params
  )
}

#' Define Bayesian GLM Model Specification
#'
#' Creates a Bayesian GLM model specification using the `stan` engine for regression tasks.
#'
#' @param task A character string specifying the type of task. Must be `"regression"`.
#' @return A list containing the model specification, default parameters, and required parameters.
#' @importFrom magrittr %>%
#' @importFrom parsnip linear_reg set_engine set_mode
#' @export
define_bayes_glm_spec <- function(task) {
  if (!requireNamespace("rstanarm", quietly = TRUE)) {
    stop("The 'rstanarm' package is required for Bayesian GLM but is not installed.")
  }
  if (task != "regression") {
    stop("Bayesian GLM is only applicable for regression tasks.")
  }
  model_spec <- linear_reg() %>%
    set_engine("stan") %>%
    set_mode("regression")
  default_params <- list()
  required_params <- c()
  list(
    model_spec = model_spec,
    default_params = default_params,
    required_params = required_params
  )
}

#' Define Partial Least Squares Model Specification
#'
#' Creates a PLS model specification using the `mixOmics` engine for regression tasks.
#'
#' @param task A character string specifying the type of task. Must be `"regression"`.
#' @return A list containing the model specification, default parameters, and required parameters.
#' @importFrom magrittr %>%
#' @importFrom parsnip linear_reg set_engine set_mode
#' @importFrom tune tune
#' @export
define_pls_spec <- function(task) {
  if (task != "regression") {
    stop("Partial Least Squares is only applicable for regression tasks.")
  }
  model_spec <- pls(num_comp = tune()) %>%
    set_engine("mixOmics") %>%
    set_mode("regression")
  default_params <- list(
    num_comp = c(1, 10)  # Range for num_comp
  )
  required_params <- c("num_comp")
  list(
    model_spec = model_spec,
    default_params = default_params,
    required_params = required_params
  )
}

#' Define GLMBoost Model Specification
#'
#' Creates a GLMBoost model specification using the `glmboost` engine for regression tasks.
#'
#' @param task A character string specifying the type of task. Must be `"regression"`.
#' @return A list containing the model specification, default parameters, and required parameters.
#' @importFrom magrittr %>%
#' @importFrom parsnip boost_tree set_engine set_mode
#' @importFrom tune tune
#' @export
define_glmboost_spec <- function(task) {
  if (!requireNamespace("mboost", quietly = TRUE)) {
    stop("The 'mboost' package is required for GLMBoost but is not installed.")
  }
  if (task != "regression") {
    stop("GLMBoost is only applicable for regression tasks.")
  }
  model_spec <- boost_tree(
    trees = tune(),
    learn_rate = tune()
  ) %>%
    set_engine("glmboost") %>%
    set_mode("regression")
  default_params <- list(
    trees = c(50, 500),          # Range for trees
    learn_rate = c(0.01, 0.3)    # Range for learn_rate
  )
  required_params <- c("trees", "learn_rate")
  list(
    model_spec = model_spec,
    default_params = default_params,
    required_params = required_params
  )
}

#' Define Linear Regression Model Specification
#'
#' Creates a linear regression model specification using the `lm` engine for regression tasks.
#'
#' @param task A character string specifying the type of task. Must be `"regression"`.
#' @return A list containing the model specification, default parameters, and required parameters.
#' @importFrom magrittr %>%
#' @importFrom parsnip linear_reg set_engine set_mode
#' @export
define_linear_regression_spec <- function(task) {
  if (task != "regression") {
    stop("Linear regression is only applicable for regression tasks.")
  }
  model_spec <- linear_reg() %>%
    set_engine("lm")
  default_params <- list()
  required_params <- c()
  list(
    model_spec = model_spec,
    default_params = default_params,
    required_params = required_params
  )
}

#' Define Ridge Regression Model Specification
#'
#' Creates a ridge regression model specification using the `glmnet` engine for regression tasks.
#'
#' @param task A character string specifying the type of task. Must be `"regression"`.
#' @return A list containing the model specification, default parameters, and required parameters.
#' @importFrom magrittr %>%
#' @importFrom parsnip linear_reg set_engine set_mode
#' @importFrom tune tune
#' @export
define_ridge_regression_spec <- function(task) {
  if (task != "regression") {
    stop("Ridge regression is only applicable for regression tasks.")
  }
  model_spec <- linear_reg(penalty = tune(), mixture = 0) %>%
    set_engine("glmnet") %>%
    set_mode("regression")
  default_params <- list(
    penalty = c(0.001, 10)  # Range for penalty
  )
  required_params <- c("penalty")
  list(
    model_spec = model_spec,
    default_params = default_params,
    required_params = required_params
  )
}

#' Define Lasso Regression Model Specification
#'
#' Creates a lasso regression model specification using the `glmnet` engine for regression tasks.
#'
#' @param task A character string specifying the type of task. Must be `"regression"`.
#' @return A list containing the model specification, default parameters, and required parameters.
#' @importFrom magrittr %>%
#' @importFrom parsnip linear_reg set_engine set_mode
#' @importFrom tune tune
#' @export
define_lasso_regression_spec <- function(task) {
  if (task != "regression") {
    stop("Lasso regression is only applicable for regression tasks.")
  }
  model_spec <- linear_reg(penalty = tune(), mixture = 1) %>%
    set_engine("glmnet") %>%
    set_mode("regression")
  default_params <- list(
    penalty = c(0.001, 10)  # Range for penalty
  )
  required_params <- c("penalty")
  list(
    model_spec = model_spec,
    default_params = default_params,
    required_params = required_params
  )
}

# # Helper function for Stacking Ensemble
# define_stacking_spec <- function(task, models, metric) {
#   if (length(models) < 2) {
#     stop("Stacking requires at least two base models.")
#   }
#   if (!requireNamespace("stacks", quietly = TRUE)) {
#     stop("The 'stacks' package is required for stacking but is not installed.")
#   }
#   stack <- stacks::stacks()
#   for (model_name in names(models)) {
#     stack <- stacks::add_candidates(stack, models[[model_name]])
#   }
#   stack <- stacks::blend_predictions(stack, metric = metric)
#   stack <- stacks::fit_members(stack)
#   model_spec <- NULL  # Placeholder
#   default_params <- list()
#   required_params <- c()
#   list(model_spec = stack, default_params = default_params, required_params = required_params)
# }
#
#
# # Helper function for Blending Ensemble
# define_blending_spec <- function(task, models, metric) {
#   stop("Blending is not yet implemented.")
# }
#
#
# # Helper function for Voting Ensemble
# define_voting_spec <- function(task, models, metric) {
#   stop("Voting ensemble is not yet implemented.")
# }
#
