#' Train Specified Machine Learning Algorithms on the Training Data
#'
#' Trains specified machine learning algorithms on the preprocessed training data.
#'
#' @param train_data Preprocessed training data frame.
#' @param train_data Preprocessed training data frame.
#' @param label Name of the target variable.
#' @param task Type of task: "classification" or "regression".
#' @param algorithms Vector of algorithm names to train.
#' @param resampling_method Resampling method for cross-validation (e.g., "cv", "repeatedcv", "boot", "none").
#' @param folds Number of folds for cross-validation.
#' @param repeats Number of times to repeat cross-validation (only applicable for methods like "repeatedcv").
#' @param tune_params List of hyperparameter tuning ranges.
#' @param metric The performance metric to optimize.
#' @param summaryFunction A custom summary function for model evaluation. Default is \code{NULL}.
#' @param seed An integer value specifying the random seed for reproducibility.
#' @param recipe A recipe object for preprocessing.
#' @param use_default_tuning Logical indicating whether to use default tuning grids when \code{tune_params} is \code{NULL}.
#' @param tuning_strategy A string specifying the tuning strategy ("grid", "bayes", or "none"), possibly with adaptive methods.
#' @param tuning_iterations Number of iterations for iterative tuning methods.
#' @param early_stopping Logical for early stopping in Bayesian tuning.
#' @param adaptive Logical indicating whether to use adaptive/racing methods.
#' @importFrom magrittr %>%
#' @importFrom dplyr filter mutate select if_else
#' @importFrom tibble tibble
#' @importFrom rlang sym
#' @importFrom dials range_set value_set grid_regular grid_latin_hypercube finalize
#' @importFrom parsnip fit extract_parameter_set_dials
#' @importFrom workflows workflow add_model add_recipe
#' @importFrom tune tune_grid control_grid select_best finalize_workflow finalize_model tune_bayes control_grid control_bayes
#' @importFrom yardstick metric_set accuracy kap roc_auc sens spec precision f_meas rmse rsq mae
#' @importFrom rsample vfold_cv bootstraps validation_split
#' @importFrom recipes all_nominal_predictors all_numeric_predictors all_outcomes all_predictors
#' @importFrom finetune control_race tune_race_anova
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
                         recipe,
                         use_default_tuning = FALSE,
                         tuning_strategy = "grid",
                         tuning_iterations = 10,
                         early_stopping = FALSE,
                         adaptive = FALSE) {

  set.seed(seed)

  if (task == "classification") {
    metrics <- metric_set(
      accuracy,
      kap,
      sens,
      spec,
      precision,
      f_meas,
      roc_auc
    )
  } else {
    metrics <- metric_set(rmse, rsq, mae)
  }

  if (resampling_method == "cv") {
    resamples <- vfold_cv(train_data, v = folds, repeats = 1, strata = if (task == "classification") all_of(label) else NULL)
  } else if (resampling_method == "boot") {
    resamples <- bootstraps(train_data, times = folds, strata = if (task == "classification") all_of(label) else NULL)
  } else if (resampling_method == "repeatedcv") {
    resamples <- vfold_cv(train_data, v = folds, repeats = repeats, strata = if (task == "classification") all_of(label) else NULL)
  } else if (resampling_method == "none") {
    resamples <- NULL
  } else {
    stop("Unsupported resampling method.")
  }

  models <- list()

  update_params <- function(params_model, new_params) {
    for (param_name in names(new_params)) {
      param_value <- new_params[[param_name]]

      param_row <- params_model %>% filter(id == param_name)
      if (nrow(param_row) == 0) {
        next
      }

      param_obj <- param_row$object[[1]]

      if (length(param_value) == 2) {
        if (inherits(param_obj, "integer_parameter")) {
          param_obj <- param_obj %>% range_set(c(as.integer(param_value[1]), as.integer(param_value[2])))
        } else {
          param_obj <- param_obj %>% range_set(param_value)
        }
      } else {
        if (inherits(param_obj, "integer_parameter")) {
          param_obj <- param_obj %>% value_set(as.integer(param_value))
        } else {
          param_obj <- param_obj %>% value_set(param_value)
        }
      }

      params_model <- params_model %>%
        mutate(object = if_else(id == param_name, list(param_obj), object))
    }
    return(params_model)
  }

  n_class <- length(levels(train_data[[label]]))

  for (algo in algorithms) {
    set.seed(seed)
    model <- NULL

    algo_tune_params <- if (!is.null(tune_params)) tune_params[[algo]] else NULL

    if (is.null(algo_tune_params) && use_default_tuning) {
      algo_tune_params <- get_default_tune_params(algo, train_data, label)
    }

    perform_tuning <- !is.null(algo_tune_params) && !is.null(resamples)

    if(n_class >2){
      logistic_regression = define_multinomial_regression_spec(task, tune = perform_tuning)
      penalized_logistic_regression = define_penalized_multinomial_regression_spec(task, tune = perform_tuning)
      } else {
        logistic_regression =  define_logistic_regression_spec(task, tune = perform_tuning)
        penalized_logistic_regression =  define_penalized_logistic_regression_spec(task, tune = perform_tuning)

      }

    model_info <- switch(algo,
                         "random_forest" = define_random_forest_spec(task, train_data, label, tune = perform_tuning),
                         "ranger" = define_ranger_spec(task, train_data, label, tune = perform_tuning),
                         "c5.0" = define_c5_0_spec(task, tune = perform_tuning),
                         "xgboost" = define_xgboost_spec(task, train_data, label, tune = perform_tuning),
                         "lightgbm" = define_lightgbm_spec(task, train_data, label, tune = perform_tuning),
                         "logistic_regression" = logistic_regression,
                         "penalized_logistic_regression" = penalized_logistic_regression,
                         "decision_tree" = define_decision_tree_spec(task, tune = perform_tuning),
                         "svm_linear" = define_svm_linear_spec(task, tune = perform_tuning),
                         "svm_radial" = define_svm_radial_spec(task, tune = perform_tuning),
                         "knn" = define_knn_spec(task, tune = perform_tuning),
                         "naive_bayes" = define_naive_bayes_spec(task, tune = perform_tuning),
                         "neural_network" = define_neural_network_spec(task, tune = perform_tuning),
                         "lda" = define_lda_spec(task),
                         "qda" = define_qda_spec(task),
                         "bagging" = define_bagging_spec(task, tune = perform_tuning),
                         "elastic_net" = define_elastic_net_spec(task, tune = perform_tuning),
                         "bayes_glm" = define_bayes_glm_spec(task),
                         "pls" = define_pls_spec(task, tune = perform_tuning),
                         "linear_regression" = define_linear_regression_spec(task),
                         "ridge_regression" = define_ridge_regression_spec(task, tune = perform_tuning),
                         "lasso_regression" = define_lasso_regression_spec(task, tune = perform_tuning),
                         { warning(paste("Algorithm", algo, "is not supported or failed to train.")); next }
    )
    model_spec <- model_info$model_spec

    if (perform_tuning) {
      tune_params_model <- extract_parameter_set_dials(model_spec)
      tune_params_model <- finalize(
        tune_params_model,
        x = train_data %>% select(-all_of(label))
      )

      if (!is.null(algo_tune_params)) {
        tune_params_model <- update_params(tune_params_model, algo_tune_params)
      }

      if (nrow(tune_params_model) > 0) {
        # If tuning_strategy is grid, we create a grid.
        # For bayes, we do not create a full grid upfront.
        # For adaptive (racing), we rely on tune_race_anova.
        if (tuning_strategy == "grid" && !adaptive) {
          tune_grid <- grid_regular(
            tune_params_model,
            levels = 3
          )
        } else {
          # For bayes or adaptive methods, we won't predefine a full grid like this.
          tune_grid <- NULL
        }
      } else {
        tune_grid <- NULL
      }
    } else {
      tune_grid <- NULL
    }

    workflow <- workflow() %>%
      add_model(model_spec) %>%
      add_recipe(recipe)

    tryCatch({
      if (perform_tuning) {
        # Control objects
        ctrl_grid <- control_grid(save_pred = TRUE)
        ctrl_bayes <- control_bayes(save_pred = TRUE)
        ctrl_race <- control_race(save_pred = TRUE)

        # Adjust control for early_stopping in bayes (if desired)
        # There's no direct early_stopping parameter, but we can use no_improve in control_bayes
        if (early_stopping && tuning_strategy == "bayes") {
          # Stop if no improvement after a few iterations
          ctrl_bayes <- control_bayes(save_pred = TRUE, no_improve = 5)
        }

        if (is.null(resamples)) {
          stop("Tuning cannot be performed without resamples.")
        }

        # Select tuning function based on strategy
        if (tuning_strategy == "bayes") {
          # Bayesian optimization
          model_tuned <- tune_bayes(
            workflow,
            resamples = resamples,
            param_info = tune_params_model,
            iter = tuning_iterations,
            metrics = metrics,
            control = ctrl_bayes
          )
        } else if (adaptive) {
          # Adaptive/racing methods
          # Use tune_race_anova as an example adaptive method
          model_tuned <- tune_race_anova(
            workflow,
            resamples = resamples,
            param_info = tune_params_model,
            grid = if (is.null(tune_grid)) 20 else tune_grid, # If no predefined grid, choose something
            metrics = metrics,
            control = ctrl_race
          )
        } else if (tuning_strategy == "grid") {
          # Grid search
          if (is.null(tune_grid)) {
            # If no tuning parameters ended up defined, fallback to some default grid
            tune_grid <- grid_regular(
              tune_params_model,
              levels = 3
            )
          }
          model_tuned <- tune_grid(
            workflow,
            resamples = resamples,
            grid = tune_grid,
            metrics = metrics,
            control = ctrl_grid
          )
        } else {
          # No recognized strategy, fallback to tune_grid with minimal grid
          model_tuned <- tune_grid(
            workflow,
            resamples = resamples,
            grid = if (is.null(tune_grid)) 5 else tune_grid,
            metrics = metrics,
            control = ctrl_grid
          )
        }

        best_params <- select_best(model_tuned, metric = metric)
        final_workflow <- finalize_workflow(workflow, best_params)
        model <- fit(final_workflow, data = train_data)
      } else {
        # No tuning parameters, fit the model directly
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

  if (length(models) == 0) {
    stop("No models were successfully trained. Please check your data and parameters.")
  }

  return(models)
}

# Declare global variables
utils::globalVariables(c(
  "id", "object", "estimate", ".metric", ".estimate", ".pred", ".pred_class",
  "rmse", "rsq", "min_n", "num_comp"
))

# Central repository for default parameters
get_default_params <- function(algo, num_predictors = NULL) {
  switch(algo,
         # 1. Random Forest
         "random_forest" = list(
           mtry = if (!is.null(num_predictors)) max(1, floor(sqrt(num_predictors))) else 2,
           trees = 100,
           min_n = 5
         ),
         # 2. Ranger (same as Random Forest)
         "ranger" = list(
           mtry = if (!is.null(num_predictors)) max(1, floor(sqrt(num_predictors))) else 2,
           trees = 100,
           min_n = 5
         ),
         # 4. C5.0
         "c5.0" = list(
           trees = 50,
           min_n = 5,
           sample_size = 0.5
         ),
         # 5. XGBoost
         "xgboost" = list(
           trees = 100,
           tree_depth = 3,
           learn_rate = 0.1,
           loss_reduction = 0,
           min_n = 5,
           sample_size = 0.5,
           mtry = if (!is.null(num_predictors)) max(1, floor(sqrt(num_predictors))) else 2
         ),
         # 6. LightGBM
         "lightgbm" = list(
           trees = 100,
           tree_depth = 3,
           learn_rate = 0.1,
           loss_reduction = 0,
           min_n = 5,
           sample_size = 0.5,
           mtry = if (!is.null(num_predictors)) max(1, floor(sqrt(num_predictors))) else 2
         ),
         # 7. Logistic Regression
         "logistic_regression" = list(),
         # 8. Penalized Logistic Regression
         "penalized_logistic_regression" = list(
           penalty = 0.01,
           mixture = 0.5
         ),
         # 9. Decision Tree
         "decision_tree" = list(
           cost_complexity = 0.01,
           tree_depth = 5,
           min_n = 5
         ),
         # 10. SVM Linear
         "svm_linear" = list(
           cost = 1
         ),
         # 11. SVM Radial
         "svm_radial" = list(
           cost = 1,
           rbf_sigma = 0.1
         ),
         # 12. KNN
         "knn" = list(
           neighbors = 5,
           weight_func = "rectangular",
           dist_power = 2
         ),
         # 13. Naive Bayes
         "naive_bayes" = list(
           smoothness = 1,
           Laplace = 0
         ),
         # 14. Neural Network (nnet)
         "neural_network" = list(
           hidden_units = 5,
           penalty = 0.01,
           epochs = 100
         ),
         # 15. Deep Learning (keras)
         "deep_learning" = list(
           hidden_units = 10,
           penalty = 0.001,
           epochs = 50
         ),
         # 16. LDA
         "lda" = list(),
         # 17. QDA
         "qda" = list(),
         # 18. Bagging
         "bagging" = list(
           min_n = 5
         ),
         # 19. Elastic Net
         "elastic_net" = list(
           penalty = 0.01,
           mixture = 0.5
         ),
         # 20. Bayesian GLM
         "bayes_glm" = list(),
         # 21. PLS
         "pls" = list(
           num_comp = 2
         ),
         # 22. Linear Regression
         "linear_regression" = list(),
         # 23. Ridge Regression
         "ridge_regression" = list(
           penalty = 0.01,
           mixture = 0
         ),
         # 24. Lasso Regression
         "lasso_regression" = list(
           penalty = 0.01,
           mixture = 1
         ),
         NULL)
}

# Function to get default tuning parameters
# Function to get default tuning parameters
get_default_tune_params <- function(algo, train_data, label) {
  # Determine the number of predictors
  num_predictors <- ncol(train_data %>% select(-!!sym(label)))

  switch(algo,
         # 1. Random Forest
         "random_forest" = list(
           mtry = c(1, max(1, floor(sqrt(num_predictors)))),
           trees = c(100, 200),  # Reduced upper limit for efficiency
           min_n = c(2, 5)
         ),

         # 2. Ranger (same as Random Forest)
         "ranger" = list(
           mtry = c(1, max(1, floor(sqrt(num_predictors)))),
           trees = c(100, 200),
           min_n = c(2, 5)
         ),

         # 4. C5.0
         "c5.0" = list(
           trees = c(1, 50),  # Reduced upper limit for efficiency
           min_n = c(2, 5)
         ),

         # 5. XGBoost
         "xgboost" = list(
           trees = c(50, 150),  # Reduced range for efficiency
           tree_depth = c(1, 5),  # Reduced maximum depth
           learn_rate = c(-2, -1),  # log scale
           loss_reduction = c(0, 5),  # Reduced upper limit
           min_n = c(2, 5),
           sample_size = c(0.5, 0.99),
           mtry = c(1, num_predictors)
         ),

         # 6. LightGBM
         "lightgbm" = list(
           trees = c(50, 150),  # Reduced range for efficiency
           tree_depth = c(1, 5),  # Reduced maximum depth
           learn_rate = c(-2, -1),  # log scale
           loss_reduction = c(0, 5),  # Reduced upper limit
           min_n = c(2, 5),
           sample_size = c(0.5, 0.99),
           mtry = c(1, num_predictors)
         ),

         # 7. Logistic Regression
         "logistic_regression" = list(
           penalty = c(-5, 0),  # log scale
           mixture = c(0, 1)
         ),

         # 8. Penalized Logistic Regression
         "penalized_logistic_regression" = list(
           penalty = c(-5, 0),  # log scale
           mixture = c(0, 1)
         ),

         # 9. Decision Tree
         "decision_tree" = list(
           cost_complexity = c(-5, 0),  # log scale
           tree_depth = c(1, 5),  # Reduced maximum depth
           min_n = c(2, 5)
         ),

         # 10. SVM Linear
         "svm_linear" = list(
           cost = c(-3, 3)  # log scale
         ),

         # 11. SVM Radial
         "svm_radial" = list(
           cost = c(-3, 3),  # log scale
           rbf_sigma = c(-9, -1)  # log scale
         ),

         # 12. KNN
         "knn" = list(
           neighbors = c(3, 7),  # Narrowed range for efficiency
           dist_power = c(1, 2)
         ),

         # 13. Naive Bayes
         "naive_bayes" = list(
           smoothness = c(0, 1),
           Laplace = c(0, 1)
         ),

         # 14. Neural Network (nnet)
         "neural_network" = list(
           hidden_units = c(1, 5),  # Reduced upper limit
           penalty = c(-5, -1),  # log scale
           epochs = c(100, 150)  # Reduced upper limit
         ),

         # 15. Deep Learning (keras)
         "deep_learning" = list(
           hidden_units = c(10, 30),  # Reduced upper limit
           penalty = c(-5, -1),  # log scale
           epochs = c(50, 100)  # Reduced upper limit
         ),

         # 16. LDA
         "lda" = NULL,

         # 17. QDA
         "qda" = NULL,

         # 18. Bagging
         "bagging" = list(
           cost_complexity = c(-5, 0),  # log scale
           tree_depth = c(1, 5),  # Reduced maximum depth
           min_n = c(2, 5)
         ),

         # 19. Elastic Net
         "elastic_net" = list(
           penalty = c(-5, 0),  # log scale
           mixture = c(0, 1)
         ),

         # 20. Bayesian GLM
         "bayes_glm" = NULL,

         # 21. PLS
         "pls" = list(
           num_comp = c(1, min(5, num_predictors))  # Reduced upper limit
         ),

         # 22. Linear Regression
         "linear_regression" = NULL,

         # 23. Ridge Regression
         "ridge_regression" = list(
           penalty = c(-5, 0)  # log scale
         ),

         # 24. Lasso Regression
         "lasso_regression" = list(
           penalty = c(-5, 0)  # log scale
         ),

         # Default case
         NULL)
}




