#' Evaluate Models Function
#'
#' Evaluates the trained models on the test data and computes performance metrics.
#'
#' @param models A list of trained model objects.
#' @param test_data Preprocessed test data frame.
#' @param label Name of the target variable.
#' @param metric The performance metric to optimize (e.g., "Accuracy", "ROC").
#' @return A list of performance metrics for each model.
#'
#' @importFrom caret confusionMatrix
#' @importFrom pROC roc
#' @importFrom stats predict
#' @export
evaluate_models <- function(models, test_data, label, metric = "Accuracy") {
  # Load required packages
  if (!requireNamespace("caret", quietly = TRUE)) {
    stop("The 'caret' package is required but not installed.")
  }
  if (metric == "ROC") {
    if (!requireNamespace("pROC", quietly = TRUE)) {
      stop("The 'pROC' package is required for ROC metric but not installed.")
    }
  }

  # Initialize performance list
  performance <- list()

  # Extract true labels
  true_labels <- test_data[[label]]

  # Remove the label from test_data
  test_features <- test_data[, !(names(test_data) %in% label), drop = FALSE]

  # Loop over each model
  for (model_name in names(models)) {
    model <- models[[model_name]]

    # Initialize a list to store metrics
    metrics <- list()

    # Predict on test data
    predictions <- predict(model, test_features)

    # Compute confusion matrix
    cm <- confusionMatrix(predictions, true_labels)

    # Extract overall metrics
    overall_metrics <- cm$overall
    # Extract per-class metrics
    class_metrics <- cm$byClass

    # Store overall accuracy and kappa
    metrics$Accuracy <- overall_metrics['Accuracy']
    metrics$Kappa <- overall_metrics['Kappa']

    # For multiclass, compute macro-averaged metrics
    if (nlevels(true_labels) > 2) {
      # Compute macro-averaged sensitivity (Recall), specificity, precision, and F1 score
      metrics$Sensitivity <- mean(class_metrics[, 'Sensitivity'], na.rm = TRUE)
      metrics$Specificity <- mean(class_metrics[, 'Specificity'], na.rm = TRUE)
      metrics$Precision <- mean(class_metrics[, 'Precision'], na.rm = TRUE)
      metrics$F1 <- mean(class_metrics[, 'F1'], na.rm = TRUE)
    } else {
      # For binary classification, use existing metrics
      metrics$Sensitivity <- class_metrics['Sensitivity']
      metrics$Specificity <- class_metrics['Specificity']
      metrics$Precision <- class_metrics['Precision']
      metrics$F1 <- class_metrics['F1']
    }

    # Compute ROC AUC if required
    if (metric == "ROC") {
      # Try to get predicted probabilities
      prob_predictions <- tryCatch({
        predict(model, test_features, type = "prob")
      }, error = function(e) {
        NULL
      })

      if (!is.null(prob_predictions)) {
        # For binary classification
        if (nlevels(true_labels) == 2) {
          positive_class <- levels(true_labels)[2]  # Assuming the second level is the positive class
          roc_obj <- roc(
            response = true_labels,
            predictor = prob_predictions[[positive_class]],
            levels = levels(true_labels),
            direction = "<"
          )
          metrics$ROC <- as.numeric(roc_obj$auc)
        } else {
          # For multiclass, compute average ROC AUC using one-vs-all approach
          roc_list <- list()
          for (class in levels(true_labels)) {
            binary_labels <- ifelse(true_labels == class, class, paste0("not_", class))
            roc_obj <- roc(
              response = binary_labels,
              predictor = prob_predictions[[class]],
              levels = c(class, paste0("not_", class)),
              direction = "<"
            )
            roc_list[[class]] <- as.numeric(roc_obj$auc)
          }
          metrics$ROC <- mean(unlist(roc_list), na.rm = TRUE)
        }
      } else {
        warning(
          paste(
            "Model",
            model_name,
            "does not support probability predictions required for ROC calculation."
          )
        )
        metrics$ROC <- NA
      }
    }

    # Add metrics to performance list
    performance[[model_name]] <- metrics
  }

  return(performance)
}
