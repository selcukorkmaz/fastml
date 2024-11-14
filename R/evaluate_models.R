#' Evaluate Models Function
#'
#' Evaluates the trained models on the test data and computes performance metrics.
#'
#' @param models A list of trained model objects.
#' @param test_data Preprocessed test data frame.
#' @param label Name of the target variable.
#' @param task Type of task: "classification" or "regression".
#' @param metric The performance metric to optimize (e.g., "Accuracy", "RMSE").
#' @return A list of performance metrics for each model.
#'
#' @importFrom caret confusionMatrix
#' @importFrom pROC roc auc
#' @importFrom stats predict
#' @export
evaluate_models <- function(models, test_data, label, task, metric = "Accuracy") {
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

    # Check if the model is an ensemble method
    if (!inherits(model, "train") && is.list(model) && !is.null(model$method)) {
      method <- model$method

      if (method == "stacking" || method == "blending") {
        # Generate predictions from base models
        base_preds <- lapply(model$base_models, function(m) {
          predict(m, newdata = test_features)
        })

        # Combine predictions into a data frame
        pred_df <- as.data.frame(base_preds)
        names(pred_df) <- paste0("pred_", names(model$base_models))

        # Predict using meta-model
        predictions <- predict(model$meta_model, newdata = pred_df)

        if (task == "classification" && is.factor(true_labels)) {
          predictions <- factor(predictions, levels = levels(true_labels))
        }

      } else if (method == "voting") {
        # Generate predictions from base models
        base_preds <- lapply(model$base_models, function(m) {
          predict(m, newdata = test_features)
        })

        # Voting mechanism
        if (task == "classification") {
          # Majority vote
          predictions <- apply(do.call(cbind, base_preds), 1, function(x) {
            votes <- table(x)
            winner <- names(votes)[which.max(votes)]
            return(winner)
          })
          predictions <- factor(predictions, levels = levels(true_labels))
        } else {
          # Average predictions
          predictions <- rowMeans(do.call(cbind, base_preds))
        }

      } else {
        warning(paste("Unknown ensemble method:", method))
        next
      }

    } else {
      # Regular model prediction
      predictions <- predict(model, newdata = test_features)

      if (task == "classification" && is.factor(true_labels)) {
        predictions <- factor(predictions, levels = levels(true_labels))
      }
    }

    # Proceed with evaluation
    if (task == "classification") {
      # Compute confusion matrix
      cm <- caret::confusionMatrix(predictions, true_labels)

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
        if (requireNamespace("pROC", quietly = TRUE)) {
          if (is.list(model) && !is.null(model$method)) {
            # For ensemble methods, handle ROC calculation
            if (method == "stacking" || method == "blending") {
              # For stacking/blending, use predicted probabilities from meta-model if available
              if (any(grepl("prob", methods(predict)))) {
                prob_predictions <- predict(model$meta_model, newdata = pred_df, type = "prob")
                roc_obj <- pROC::roc(response = true_labels, predictor = prob_predictions[, 2])
                metrics$ROC <- pROC::auc(roc_obj)
              } else {
                metrics$ROC <- NA
                warning(paste("ROC cannot be computed for model:", model_name))
              }
            } else if (method == "voting") {
              # For voting, ROC calculation may not be applicable
              metrics$ROC <- NA
              warning(paste("ROC cannot be computed for voting ensemble:", model_name))
            }
          } else {
            # For regular models
            if (any(grepl("prob", methods(predict)))) {
              prob_predictions <- predict(model, newdata = test_features, type = "prob")
              roc_obj <- pROC::roc(response = true_labels, predictor = prob_predictions[, 2])
              metrics$ROC <- pROC::auc(roc_obj)
            } else {
              metrics$ROC <- NA
              warning(paste("ROC cannot be computed for model:", model_name))
            }
          }
        } else {
          metrics$ROC <- NA
          warning("The 'pROC' package is required for ROC metric but not installed.")
        }
      }

    } else if (task == "regression") {
      # Compute regression metrics
      residuals <- true_labels - predictions
      metrics$RMSE <- sqrt(mean(residuals^2))
      metrics$MAE <- mean(abs(residuals))
      metrics$Rsquared <- cor(true_labels, predictions, use = "complete.obs")^2
    }

    # Add metrics to performance list
    performance[[model_name]] <- metrics
  }

  return(performance)
}
