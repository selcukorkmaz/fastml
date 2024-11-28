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
#' @importFrom dplyr filter bind_rows pull mutate select bind_cols
#' @importFrom yardstick metric_set accuracy kap roc_auc
#' @importFrom workflows pull_workflow_spec pull_workflow_preprocessor workflow add_model add_recipe fit
#' @importFrom tune select_best finalize_model
#' @importFrom rlang sym
#' @importFrom tibble tibble
#' @return A list of performance metrics for each model.
#' @export
evaluate_models <- function(models, train_data, test_data, label, task, metric = NULL) {
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

  # Extract true labels
  true_labels <- test_data[[label]]

  for (model_name in names(models)) {
    model <- models[[model_name]]
    metrics_list <- list()

    # Check if the model is a model_stack object (from stacks package)
    if (inherits(model, "model_stack")) {
      # Handle stacking models if implemented
      # (Your stacking code here)
      next
    } else if (model_name %in% c("blending", "voting")) {
      # Handle blending and voting ensembles if implemented
      warning(paste("The ensemble method", model_name, "is not implemented. Skipping."))
      next
    } else {
      # For regular models
      if (inherits(model, "tune_results")) {
        # Finalize the model specification with the best hyperparameters
        best_params <- tryCatch({
          tune::select_best(model, metric = metric)
        }, error = function(e) {
          warning(paste("Could not select best parameters for model", model_name, ":", e$message))
          return(NULL)
        })
        if (is.null(best_params)) {
          next
        }

        # Extract the model specification and recipe from the tuning results
        model_spec <- workflows::pull_workflow_spec(model)
        model_recipe <- workflows::pull_workflow_preprocessor(model)

        # Finalize the model specification with the best parameters
        final_model_spec <- tune::finalize_model(model_spec, best_params)

        # Create a new workflow with the finalized model specification and the recipe
        final_workflow <- workflows::workflow() %>%
          workflows::add_recipe(model_recipe) %>%
          workflows::add_model(final_model_spec)

        # Fit the final model on the entire training data
        final_model <- workflows::fit(final_workflow, data = train_data)
      } else {
        # The model is already a fitted workflow
        final_model <- model
      }

      # Make predictions on the test data
      if (task == "classification") {
        pred_class <- predict(final_model, new_data = test_data, type = "class")$.pred_class
        pred_prob <- predict(final_model, new_data = test_data, type = "prob")
        data_metrics <- test_data %>%
          dplyr::select(truth = !!rlang::sym(label)) %>%
          dplyr::mutate(estimate = pred_class) %>%
          dplyr::bind_cols(pred_prob)

        # Determine number of classes
        num_classes <- length(unique(data_metrics$truth))
        if (num_classes == 2) {
          # Binary classification
          positive_class <- levels(data_metrics$truth)[2]

          # Compute accuracy and kappa
          metrics_class <- yardstick::metric_set(yardstick::accuracy, yardstick::kap)
          perf_class <- metrics_class(data_metrics, truth = truth, estimate = estimate)

          # Compute ROC AUC
          roc_auc_value <- yardstick::roc_auc(
            data_metrics,
            truth = truth,
            !!rlang::sym(paste0(".pred_", positive_class)),
            event_level = "second"
          )

          # Combine metrics
          perf <- dplyr::bind_rows(perf_class, roc_auc_value)
        } else {
          # Multiclass classification
          metrics_set <- yardstick::metric_set(yardstick::accuracy, yardstick::kap)
          perf <- metrics_set(data_metrics, truth = truth, estimate = estimate)
        }
      } else {
        # Regression task
        predictions <- predict(final_model, new_data = test_data)
        pred <- predictions$.pred
        truth <- true_labels
        data_metrics <- tibble::tibble(truth = truth, estimate = pred)
        # Compute metrics
        metrics_set <- yardstick::metric_set(yardstick::rmse, yardstick::rsq, yardstick::mae)
        perf <- metrics_set(data_metrics, truth = truth, estimate = estimate)
      }
      metrics_list <- perf
    }
    # Add metrics to performance list
    performance[[model_name]] <- metrics_list
  }
  return(performance)
}
