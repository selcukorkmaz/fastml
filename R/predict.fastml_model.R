#' Predict Function for fastml_model
#'
#' Makes predictions on new data using the trained model.
#'
#' @param object An object of class \code{fastml_model}.
#' @param newdata A data frame containing new data for prediction.
#' @param type Type of prediction. Default is \code{"auto"}, which returns class labels for classification and numeric predictions for regression.
#'             Other options include \code{"prob"} for class probabilities (classification only).
#' @param ... Additional arguments (not used).
#' @return A vector of predictions.
#'
#' @importFrom recipes bake
#' @importFrom tibble is_tibble
#' @importFrom stats predict
#'
#' @export
predict.fastml_model <- function(object, newdata, type = "auto", ...) {
  # Check if newdata is provided
  if (missing(newdata)) {
    stop("Please provide new data for prediction.")
  }

  # Retrieve the label name from the model object
  label <- object$label

  # Ensure the label is not NULL
  if (is.null(label)) {
    stop("The label name is missing from the model object. Please check the training process.")
  }

  # Remove label from newdata if present
  if (label %in% names(newdata)) {
    newdata[[label]] <- NULL
  }

  # Apply preprocessing to newdata using the recipe
  if (!is.null(object$recipe)) {
    newdata_processed <- recipes::bake(object$recipe, new_data = newdata)
  } else {
    stop("Preprocessing recipe is missing from the model object.")
  }

  # Use the best model for prediction
  best_model <- object$best_model

  # Determine prediction type
  if (type == "auto") {
    if (object$task == "classification") {
      predict_type <- "class"
    } else {
      predict_type <- "numeric"
    }
  } else if (type == "prob") {
    if (object$task != "classification") {
      stop("Probability predictions are only available for classification tasks.")
    }
    predict_type <- "prob"
  } else {
    predict_type <- type
  }

  # Generate predictions
  predictions <- predict(best_model, new_data = newdata_processed, type = predict_type)

  # If predictions are in a tibble, extract the vector
  if (is.data.frame(predictions) || is_tibble(predictions)) {
    if (predict_type == "class") {
      predictions <- predictions$.pred_class
    } else if (predict_type == "prob") {
      # Return the entire probability tibble
      predictions <- predictions
    } else if (predict_type == "numeric") {
      predictions <- predictions$.pred
    }
  }

  return(predictions)
}
