#' Predict Function for fastml_model
#'
#' Makes predictions on new data using the trained model.
#'
#' @param object An object of class \code{fastml_model}.
#' @param newdata A data frame containing new data for prediction.
#' @param ... Additional arguments (not used).
#' @return A vector of predictions.
#'
#' @importFrom stats predict
#'
#' @export
predict.fastml_model <- function(object, newdata, ...) {
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

  # Apply preprocessing to newdata
  newdata_processed <- predict(object$preprocessor, newdata)

  # Use the best model for prediction
  best_model <- object$best_model

  # Generate predictions
  predictions <- predict(best_model, newdata = newdata_processed)

  return(predictions)
}
