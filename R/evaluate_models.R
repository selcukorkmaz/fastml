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
  required_pkgs <- c("yardstick", "parsnip", "tune", "workflows",
                     "dplyr", "rlang", "tibble")
  for(pkg in required_pkgs){
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("The '%s' package is required but not installed.", pkg))
    }
  }

  # Initialize performance and predictions lists
  performance <- list()
  predictions_list <- list()

  # Extract true labels from the test set
  true_labels <- test_data[[label]]


  # Iterate over the models object. Check if the model is nested (i.e. a list of engines)
  for (algo in names(models)) {
    performance[[algo]] <- list()
    predictions_list[[algo]] <- list()

    # If the model object has names, assume it is nested by engine.
    if (is.list(models[[algo]]) && !inherits(models[[algo]], "workflow") && !inherits(models[[algo]], "tune_results")) {
      for (eng in names(models[[algo]])) {
        model_obj <- models[[algo]][[eng]]
        result <- process_model(model_obj, model_id = paste(algo, eng, sep = "_"),
                                task = task, test_data = test_data, label = label,
                                event_class = event_class, engine = eng)
        if (!is.null(result)) {
          performance[[algo]][[eng]] <- result$performance
          predictions_list[[algo]][[eng]] <- result$predictions
        }
      }
    } else {
      # Otherwise, assume a single model (not nested)
      result <- process_model(models[[algo]], algo)
      if (!is.null(result)) {
        performance[[algo]] <- result$performance
        predictions_list[[algo]] <- result$predictions
      }
    }
  }

  # Return both performance and predictions as a named list
  return(list(performance = performance, predictions = predictions_list))
}
