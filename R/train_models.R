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
#' @param algorithm_engines A named list specifying the engine to use for each algorithm.
#' @importFrom magrittr %>%
#' @importFrom dplyr filter mutate select if_else
#' @importFrom tibble tibble
#' @importFrom rlang sym
#' @importFrom dials range_set value_set grid_regular grid_latin_hypercube finalize
#' @importFrom parsnip fit extract_parameter_set_dials
#' @importFrom workflows workflow add_model add_recipe
#' @importFrom tune tune_grid control_grid select_best finalize_workflow finalize_model tune_bayes control_grid control_bayes
#' @importFrom yardstick metric_set accuracy kap roc_auc sens spec precision f_meas rmse rsq mae new_class_metric
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
                         adaptive = FALSE,
                         algorithm_engines = NULL) {

  set.seed(seed)

  if (task == "classification") {

    if(is.null(summaryFunction)){
      metrics <- metric_set(
        accuracy,
        kap,
        sens,
        spec,
        precision,
        f_meas,
        roc_auc
      )
    }else{

      newClassMetric <- new_class_metric(summaryFunction, "maximize")

      assign(metric, newClassMetric)

      metrics <- metric_set(
        accuracy,
        kap,
        sens,
        spec,
        precision,
        f_meas,
        roc_auc,
        !!sym(metric)
      )


    }
  } else {
    metrics <- metric_set(rmse, rsq, mae)
  }

  if (resampling_method == "cv") {
    if (nrow(train_data) < folds) {
      stop(
        sprintf(
          "You requested %d-fold cross-validation, but your training set only has %d rows. \nThis prevents each fold from having at least one row. \nEither reduce 'folds', increase data, or use a different resampling method (e.g. 'boot').",
          folds,
          nrow(train_data)
        )
      )
    }
    resamples <- vfold_cv(
      train_data,
      v = folds,
      repeats = 1,
      strata = if (task == "classification")
        all_of(label)
      else
        NULL
    )
  } else if (resampling_method == "boot") {
    resamples <- bootstraps(train_data,
                            times = folds,
                            strata = if (task == "classification")
                              all_of(label)
                            else
                              NULL)
  } else if (resampling_method == "repeatedcv") {

    if (nrow(train_data) < folds) {
      stop(
        sprintf(
          "You requested %d-fold cross-validation, but your training set only has %d rows. \nThis prevents each fold from having at least one row. \nEither reduce 'folds', increase data, or use a different resampling method (e.g. 'boot').",
          folds, nrow(train_data)
        )
      )
    }
    resamples <- vfold_cv(
      train_data,
      v = folds,
      repeats = repeats,
      strata = if (task == "classification")
        all_of(label)
      else
        NULL
    )
  } else if (resampling_method == "none") {
    resamples <- NULL
  } else {
    stop("Unsupported resampling method.")
  }

  models <- list()

  # A helper function to choose the engine for an algorithm
  get_engine <- function(algo, default_engine) {
    if (!is.null(algorithm_engines) && !is.null(algorithm_engines[[algo]])) {
      return(algorithm_engines[[algo]])
    } else {
      return(default_engine)
    }
  }

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

    # Assume that get_engine() now may return multiple engine names.

    engines <- get_engine(algo, get_default_engine(algo))


    # Create a nested list to store models by algorithm and engine.
    models[[algo]] <- list()

    # Loop over each engine provided
    for (engine in engines) {

      # Get the tuning parameters for this engine.
      if (use_default_tuning) {
        engine_tune_params <- get_default_tune_params(algo,
                                                      train_data,
                                                      label,
                                                      engine)
      } else {
        engine_tune_params <- get_default_params(algo, task, num_predictors = ncol(train_data %>% dplyr::select(-!!sym(label))), engine = engine)
      }

      if(algo == "logistic_reg" && engine %in% c("glm", "gee" ,"glmer" , "stan" , "stan_glmer")){

        perform_tuning = FALSE
      }else{

      perform_tuning <- !all(vapply(engine_tune_params, is.null, logical(1))) && !is.null(resamples)
      }

       # For other algorithms, use a switch that uses the current engine
        model_info <- switch(algo,
                             "rand_forest" = {
                               define_rand_forest_spec(task,
                                                       train_data,
                                                       label,
                                                       tuning = perform_tuning,
                                                       engine = engine)
                             },

                             "logistic_reg" = {
                               if(n_class == 2){
                                 define_logistic_reg_spec(
                                   task,
                                   tuning = perform_tuning,
                                   engine = engine)
                               }
                             },

                             "multinom_reg" = {
                               if(n_class > 2){
                                 define_multinomial_reg_spec(
                                   task,
                                   tuning = perform_tuning,
                                   engine = engine)
                               }
                             },


                             "C5_rules" = {
                               define_C5_rules_spec(task,
                                                    tuning = perform_tuning,
                                                    engine = engine)
                             },
                             "xgboost" = {
                               define_xgboost_spec(task,
                                                   train_data,
                                                   label,
                                                   tuning = perform_tuning,
                                                   engine = engine)
                             },
                             "lightgbm" = {
                               define_lightgbm_spec(task,
                                                    train_data,
                                                    label,
                                                    tuning = perform_tuning,
                                                    engine = engine)
                             },
                             "decision_tree" = {
                               define_decision_tree_spec(task,
                                                         tuning = perform_tuning,
                                                         engine = engine)
                             },
                             "svm_linear" = {
                               define_svm_linear_spec(task,
                                                      tuning = perform_tuning,
                                                      engine = engine)
                             },
                             "svm_rbf" = {
                               define_svm_rbf_spec(task,
                                                   tuning = perform_tuning,
                                                   engine = engine)
                             },
                             "nearest_neighbor" = {
                               define_nearest_neighbor_spec(task,
                                                            tuning = perform_tuning,
                                                            engine = engine)
                             },
                             "naive_Bayes" = {
                               define_naive_Bayes_spec(task,
                                                       tuning = perform_tuning,
                                                       engine = engine)
                             },
                             "mlp" = {
                               define_mlp_spec(task,
                                               tuning = perform_tuning,
                                               engine = engine)
                             },
                             "discrim_linear" = {
                               define_discrim_linear_spec(task,
                                                          engine = engine)
                             },
                             "discrim_quad" = {
                               define_discrim_quad_spec(task,
                                                        engine = engine)
                             },
                             "bag_tree" = {
                               define_bag_tree_spec(task,
                                                    tuning = perform_tuning,
                                                    engine = engine)
                             },
                             "elastic_net" = {
                               define_elastic_net_spec(task,
                                                       tuning = perform_tuning,
                                                       engine = engine)
                             },
                             "bayes_glm" = {
                               define_bayes_glm_spec(task,
                                                     engine = engine)
                             },
                             "pls" = {
                               define_pls_spec(task,
                                               tuning = perform_tuning,
                                               engine = engine)
                             },
                             "linear_reg" = {
                               define_linear_reg_spec(task,
                                                      engine = engine)
                             },
                             "ridge_regression" = {
                               define_ridge_regression_spec(task,
                                                            tuning = perform_tuning,
                                                            engine = engine)
                             },
                             "lasso_regression" = {
                               define_lasso_regression_spec(task,
                                                            tuning = perform_tuning,
                                                            engine = engine)
                             },
                             {
                               warning(paste("Algorithm", algo, "is not supported or failed to train."))
                               next
                             }
        )

        # Assume the model specification is stored in model_info$model_spec
        model_spec <- model_info$model_spec

      if(!is.null(model_spec)){

      # Set up tuning parameters and grid (if needed)
      if (perform_tuning) {

        if(inherits(model_spec, "model_spec")){
          tune_params_model <- extract_parameter_set_dials(model_spec)
        }else{
        tune_params_model <- extract_parameter_set_dials(model_spec[[1]])
        }
        tune_params_model <- finalize(
          tune_params_model,
          x = train_data %>% dplyr::select(-dplyr::all_of(label))
        )

        if (!is.null(engine_tune_params)) {
          tune_params_model <- update_params(tune_params_model, engine_tune_params)
        }

        if (nrow(tune_params_model) > 0) {
          if (tuning_strategy == "grid" && !adaptive) {
            tune_grid <- grid_regular(tune_params_model, levels = 3)
          } else {
            tune_grid <- NULL
          }
        } else {
          tune_grid <- NULL
        }
      } else {
        tune_grid <- NULL
      }

      # Create the workflow
      workflow_spec <- workflow() %>%
        add_model(if(inherits(model_spec,"model_spec")) model_spec else model_spec[[1]]) %>%
        add_recipe(recipe)

      # Fit the model (with tuning if requested)
      tryCatch({
        if (perform_tuning && !all(vapply(engine_tune_params, is.null, logical(1)))) {
          # Set up control objects for tuning

          if (algo == "rand_forest" && engine == "h2o") {

            roc_auc_h2o <- function(data, truth, ...) {
              # Rename probability columns from ".pred_p0"/".pred_p1" to ".pred_0"/".pred_1"
              data <- data %>%
                rename_with(~ sub("^\\.pred_p", ".pred_", .x), starts_with(".pred_p"))

              # Call the built-in roc_auc() with the renamed columns
              yardstick::roc_auc(data, truth = {{truth}}, ...)
            }

            # Assign the same class as roc_auc()
            class(roc_auc_h2o) <- class(roc_auc)
            attr(roc_auc_h2o, "direction") <- "maximize"

            my_metrics <- metric_set(accuracy, kap, sens, spec, precision, f_meas, roc_auc_h2o)

            allow_par = FALSE
          }

          else if(engine == "LiblineaR"){

            my_metrics <- metric_set(accuracy, kap, sens, spec, precision, f_meas)
            allow_par = TRUE

          }else{
            allow_par = TRUE
            my_metrics = NULL
          }

          ctrl_grid <- control_grid(save_pred = TRUE, allow_par = allow_par)
          ctrl_bayes <- control_bayes(save_pred = TRUE)
          ctrl_race <- control_race(save_pred = TRUE)

          if (early_stopping && tuning_strategy == "bayes") {
            ctrl_bayes <- control_bayes(save_pred = TRUE, no_improve = 5)
          }

          if (is.null(resamples)) {
            stop("Tuning cannot be performed without resamples.")
          }

          # Select tuning function based on strategy
          if (tuning_strategy == "bayes") {
            model_tuned <- tune_bayes(
              workflow_spec,
              resamples = resamples,
              param_info = tune_params_model,
              iter = tuning_iterations,
              metrics = if(!is.null(my_metrics)) my_metrics else metrics,
              control = ctrl_bayes
            )
          } else if (adaptive) {
            model_tuned <- tune_race_anova(
              workflow_spec,
              resamples = resamples,
              param_info = tune_params_model,
              grid = if (is.null(tune_grid)) 20 else tune_grid,
              metrics = if(!is.null(my_metrics)) my_metrics else metrics,
              control = ctrl_race
            )
          } else if (tuning_strategy == "grid") {
            if (is.null(tune_grid)) {
              tune_grid <- grid_regular(tune_params_model, levels = 3)
            }
            model_tuned <- tune_grid(
              workflow_spec,
              resamples = resamples,
              grid = tune_grid,
              metrics = if(!is.null(my_metrics)) my_metrics else metrics,
              control = ctrl_grid
            )
          } else {
            model_tuned <- tune_grid(
              workflow_spec,
              resamples = resamples,
              grid = if (is.null(tune_grid)) 5 else tune_grid,
              metrics = if(!is.null(my_metrics)) my_metrics else metrics,
              control = ctrl_grid
            )
          }

          best_params <- select_best(model_tuned, metric = metric)
          final_workflow <- finalize_workflow(workflow_spec, best_params)
          model <- fit(final_workflow, data = train_data)
        } else {
          # If no tuning is required, simply fit the workflow.
          model <- fit(workflow_spec, data = train_data)
        }
        # Save the fitted model in the nested list under the current engine
        models[[algo]][[engine]] <- model
      }, error = function(e) {
        warning(paste("Training failed for algorithm:", algo, "with engine:", engine,
                      "\nError message:", e$message))
      })

      }else{

        models[[algo]] = NULL
      }

    }  # end of loop over engines

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




