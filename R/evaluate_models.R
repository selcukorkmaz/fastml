#' Evaluate Models Function
#'
#' Evaluates the trained models on the test data and computes performance metrics.
#'
#' @param models A list of trained model objects.
#' @param train_data Preprocessed training data frame.
#' @param test_data Preprocessed test data frame.
#' @param label Name of the target variable.
#' @param task Type of task: "classification" or "regression".
#' @param metric The performance metric to optimize (e.g., "accuracy", "rmse").
#' @param event_class A single string. Either "first" or "second" to specify which level of truth to consider as the "event".
#' @importFrom dplyr filter bind_rows pull mutate select bind_cols
#' @importFrom yardstick metric_set accuracy kap roc_auc sens spec precision f_meas rmse rsq mae
#' @importFrom workflows pull_workflow_spec pull_workflow_preprocessor workflow add_model add_recipe
#' @importFrom parsnip fit predict.model_fit
#' @importFrom tune select_best finalize_model
#' @importFrom rlang sym syms
#' @importFrom tibble tibble
#' @return A list with two elements:
#'   \describe{
#'     \item{performance}{A named list of performance metric tibbles for each model.}
#'     \item{predictions}{A named list of data frames with columns including truth, predictions, and probabilities per model.}
#'   }
#' @export
evaluate_models <- function(models, train_data, test_data, label, task, metric = NULL, event_class) {
  # Load required packages
  if (!requireNamespace("yardstick", quietly = TRUE)) {
    stop("The 'yardstick' package is required but not installed.")
  }
  if (!requireNamespace("parsnip", quietly = TRUE)) {
    stop("The 'parsnip' package is required but not installed.")
  }
  if (!requireNamespace("tune", quietly = TRUE)) {
    stop("The 'tune' package is required but not installed.")
  }
  if (!requireNamespace("workflows", quietly = TRUE)) {
    stop("The 'workflows' package is required but not installed.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("The 'dplyr' package is required but not installed.")
  }
  if (!requireNamespace("rlang", quietly = TRUE)) {
    stop("The 'rlang' package is required but not installed.")
  }
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("The 'tibble' package is required but not installed.")
  }

  # Initialize performance list
  performance <- list()
  # Initialize predictions list
  predictions_list <- list()

  # Extract true labels
  true_labels <- test_data[[label]]

  for (model_name in names(models)) {
    model <- models[[model_name]]
    metrics_list <- list()

    # For regular models
    if (inherits(model, "tune_results")) {
      # Finalize the model specification with the best hyperparameters
      best_params <- tryCatch({
        select_best(model, metric = metric)
      }, error = function(e) {
        warning(paste("Could not select best parameters for model", model_name, ":", e$message))
        return(NULL)
      })
      if (is.null(best_params)) {
        next
      }

      # Extract the model specification and recipe from the tuning results
      model_spec <- pull_workflow_spec(model)
      model_recipe <- pull_workflow_preprocessor(model)

      # Finalize the model specification with the best parameters
      final_model_spec <- finalize_model(model_spec, best_params)

      # Create a new workflow with the finalized model specification and the recipe
      final_workflow <- workflow() %>%
        add_recipe(model_recipe) %>%
        add_model(final_model_spec)

      # Fit the final model on the entire training data
      final_model <- fit(final_workflow, data = train_data)
    } else {
      # The model is already a fitted workflow
      final_model <- model
    }

    # Make predictions on the test data
    if (task == "classification") {
      pred_class <- predict(final_model, new_data = test_data, type = "class")$.pred_class
      pred_prob <- predict(final_model, new_data = test_data, type = "prob")
      data_metrics <- test_data %>%
        select(truth = !!sym(label)) %>%
        mutate(estimate = pred_class) %>%
        bind_cols(pred_prob)

      # Determine number of classes
      num_classes <- length(unique(data_metrics$truth))
      if (num_classes == 2) {
        # Binary classification
        if(event_class == "first"){
         positive_class <- levels(data_metrics$truth)[1]
        }

        if(event_class == "second"){
          positive_class <- levels(data_metrics$truth)[2]
        }


        # Compute metrics
        metrics_class <- metric_set(
          accuracy,
          kap,
          sens,
          spec,
          precision,
          f_meas
        )

        perf_class <- metrics_class(data_metrics, truth = truth, estimate = estimate, event_level = event_class)

        # Compute ROC AUC
        roc_auc_value <- roc_auc(
          data_metrics,
          truth = truth,
          !!sym(paste0(".pred_", positive_class)),
          event_level = "second"
        )

        if(roc_auc_value$.estimate < 0.50){

          roc_auc_value <- roc_auc(
            data_metrics,
            truth = truth,
            !!sym(paste0(".pred_", positive_class)),
            event_level = "first"
          )
        }

        # Combine metrics
        perf <- bind_rows(perf_class, roc_auc_value)
      } else {
        # Multiclass classification
        # Compute metrics with macro averaging
        metrics_class <- metric_set(
          accuracy,
          kap,
          sens,
          spec,
          precision,
          f_meas
        )
        perf_class <- metrics_class(
          data_metrics,
          truth = truth,
          estimate = estimate,
          estimator = "macro"
        )

        # Compute ROC AUC with macro averaging
        prob_cols <- names(pred_prob)
        perf_roc_auc <- roc_auc(
          data_metrics,
          truth = truth,
          !!!syms(prob_cols),
          estimator = "macro_weighted"
        )

        # Combine all metrics
        perf <- bind_rows(
          perf_class,
          perf_roc_auc
        )
      }
    } else {
      # Regression task
      predictions <- predict(final_model, new_data = test_data)
      pred <- predictions$.pred
      truth <- true_labels
      data_metrics <- tibble(truth = truth, estimate = pred)
      # Compute metrics
      metrics_set <- metric_set(rmse, rsq, mae)
      perf <- metrics_set(data_metrics, truth = truth, estimate = estimate)
    }
    metrics_list <- perf

    # Add metrics to performance list
    performance[[model_name]] <- metrics_list

    # Store the predictions for this model
    predictions_list[[model_name]] <- data_metrics
  }

  # Return both performance and predictions
  return(list(performance = performance, predictions = predictions_list))
}
