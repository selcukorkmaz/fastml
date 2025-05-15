#' Predict method for fastml_model objects
#'
#' Generates predictions from a trained `fastml_model` object on new data.
#' Supports both single-model and multi-model workflows, and handles classification
#' and regression tasks with optional post-processing and verbosity.
#'
#' @param object A fitted `fastml_model` object created by the `fastml()` function.
#' @param newdata A data frame or tibble containing new predictor data for which to generate predictions.
#' @param type Type of prediction to return. One of `"auto"` (default), `"class"`, `"prob"`, or `"numeric"`.
#'   - `"auto"`: chooses `"class"` for classification and `"numeric"` for regression.
#'   - `"prob"`: returns class probabilities (only for classification).
#'   - `"class"`: returns predicted class labels.
#'   - `"numeric"`: returns predicted numeric values (for regression).
#' @param model_name (Optional) Name of a specific model to use when `object$best_model` contains multiple models.
#' @param verbose Logical; if `TRUE`, prints progress messages showing which models are used during prediction.
#' @param postprocess_fn (Optional) A function to apply to the final predictions (e.g., inverse transforms, thresholding).
#' @param ... Additional arguments (currently unused).
#'
#' @return A vector of predictions, or a named list of predictions (if multiple models are used).
#'         If `postprocess_fn` is supplied, its output will be returned instead.
#' @export
#'
#' @examples
#' \dontrun{
#'   model <- fastml(iris, label = "Species")
#'   test_data <- sanitize(iris[1:4])
#'   preds <- predict(model, newdata = test_data)
#'   probs <- predict(model, newdata = test_data, type = "prob")
#'   single_model_preds <- predict(model, newdata = test_data, model_name = "rand_forest (ranger)")
#' }
predict.fastml_model <- function(object, newdata, type = "auto", model_name = NULL,
                                 verbose = FALSE, postprocess_fn = NULL, ...) {
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
  if (!is.null(object$preprocessor)) {
    newdata_processed <- bake(object$preprocessor, new_data = newdata)
  } else {
    stop("Preprocessing recipe is missing from the model object.")
  }

  # Optional: validate features after preprocessing
  if (!is.null(object$feature_names)) {
    expected_vars <- object$feature_names
    missing_vars <- setdiff(expected_vars, names(newdata_processed))
    if (length(missing_vars) > 0) {
      stop("The following variables are missing from new data after preprocessing: ",
           paste(missing_vars, collapse = ", "))
    }
  }

  # Use the best model(s) for prediction
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
      warning("Probability output not supported for regression tasks. Using numeric predictions instead.")
      predict_type <- "numeric"
    } else {
      predict_type <- "prob"
    }
  } else {
    predict_type <- type
  }

  # Case 1: Single model
  if (inherits(best_model, "workflow")) {
    preds <- predict(best_model, new_data = newdata_processed, type = predict_type)

    # Extract useful columns
    if (is.data.frame(preds) || is_tibble(preds)) {
      if (predict_type == "class") {
        preds <- preds$.pred_class
      } else if (predict_type == "numeric") {
        preds <- preds$.pred
      }
    }

    # Optional post-processing
    if (!is.null(postprocess_fn) && is.function(postprocess_fn)) {
      preds <- postprocess_fn(preds)
    }

    return(preds)
  }

  # Case 2: List of models
  if (is.list(best_model) && all(sapply(best_model, inherits, "workflow"))) {

    # Allow selection of one specific model by name
    if (!is.null(model_name)) {
      if (!model_name %in% names(best_model)) {
        stop("Requested model name not found in best_model.")
      }
      best_model <- best_model[[model_name]]
      preds <- predict(best_model, new_data = newdata_processed, type = predict_type)

      if (is.data.frame(preds) || is_tibble(preds)) {
        if (predict_type == "class") {
          preds <- preds$.pred_class
        } else if (predict_type == "numeric") {
          preds <- preds$.pred
        }
      }

      if (!is.null(postprocess_fn) && is.function(postprocess_fn)) {
        preds <- postprocess_fn(preds)
      }

      return(preds)
    }

    # Otherwise, loop through all models
    predictions <- lapply(names(best_model), function(name) {
      model <- best_model[[name]]
      if (isTRUE(verbose)) {
        message("Generating predictions using model: ", name)
      }
      preds <- predict(model, new_data = newdata_processed, type = predict_type)

      if (is.data.frame(preds) || is_tibble(preds)) {
        if (predict_type == "class") {
          preds <- preds$.pred_class
        } else if (predict_type == "numeric") {
          preds <- preds$.pred
        }
      }

      if (!is.null(postprocess_fn) && is.function(postprocess_fn)) {
        preds <- postprocess_fn(preds)
      }

      return(preds)
    })

    names(predictions) <- names(best_model)
    class(predictions) <- "fastml_prediction"
    return(predictions)
  }

  stop("Unsupported model structure in 'best_model'.")
}

