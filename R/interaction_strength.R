#' Compute feature interaction strengths for a fastml model
#'
#' Uses the `iml` package to quantify the strength of feature interactions.
#'
#' @param object A `fastml` object.
#' @param ... Additional arguments passed to `iml::Interaction`.
#'
#' @param data Character string specifying which data to use: \code{"train"} (default) or \code{"test"}.
#' @return An `iml::Interaction` object.
#' @importFrom iml Predictor Interaction
#' @importFrom recipes bake
#' @export
#' @examples
#' \dontrun{
#' data(iris)
#' iris <- iris[iris$Species != "setosa", ]
#' iris$Species <- factor(iris$Species)
#' model <- fastml(data = iris, label = "Species")
#' interaction_strength(model)
#' }
interaction_strength <- function(object, data = c("train", "test"), ...) {
  if (!inherits(object, "fastml")) {
    stop("The input must be a 'fastml' object.")
  }
  if (!requireNamespace("iml", quietly = TRUE)) {
    stop("The 'iml' package is required for interaction strength.")
  }

  data <- match.arg(data)
  prep <- fastml_prepare_explainer_inputs(object, data = data)
  train_data <- prep$train_data
  if (is.null(train_data) || !(prep$label %in% names(train_data))) {
    stop("Training data not available for interaction strength.")
  }
  x <- prep$x_raw
  y <- prep$y_raw

  # Handle classification targets for iml
  # Determine if this is multiclass (>2 classes)
  is_multiclass <- FALSE
  positive_class <- NULL
  if (is.factor(y) || is.character(y)) {
    y_factor <- if (is.factor(y)) y else factor(y)
    n_classes <- length(levels(y_factor))
    is_multiclass <- n_classes > 2

    if (is_multiclass) {
      # Multiclass: keep y as factor, iml will handle it properly
      # when predict function returns all class probabilities
      y <- y_factor
    } else {
      # Binary classification: convert to numeric (0/1) for iml
      # Use resolve_positive_class to respect event_class settings from fastml()
      positive_class <- resolve_positive_class(prep, levels(y_factor))
      y <- as.numeric(y_factor == positive_class)
    }
  } else {
    positive_class <- prep$positive_class
  }

  parsnip_fit <- prep$fits[[1]]

  # Custom predict function
  # For multiclass: returns data.frame with all class probabilities
  # For binary: returns numeric probability vector for the positive class
  predict_fun <- function(model, newdata) {
    newdata_processed <- tryCatch(
      {
        baked <- recipes::bake(prep$preprocessor, new_data = newdata)
        if (!is.null(prep$label) && prep$label %in% names(baked)) {
          baked[[prep$label]] <- NULL
        }
        baked
      },
      error = function(e) newdata
    )

    prob <- tryCatch(
      predict(model, new_data = newdata_processed, type = "prob"),
      error = function(e) NULL
    )
    if (!is.null(prob)) {
      # Multiclass: return full probability data.frame (iml handles this)
      if (is_multiclass) {
        # Clean column names: remove .pred_ prefix
        colnames(prob) <- sub("^\\.pred_", "", colnames(prob))
        return(as.data.frame(prob))
      }
      # Binary: return probability for positive class
      prob_col <- paste0(".pred_", positive_class)
      if (!is.null(positive_class) && prob_col %in% names(prob)) {
        return(as.numeric(prob[[prob_col]]))
      }
      num_cols <- names(prob)[vapply(prob, is.numeric, logical(1))]
      if (length(num_cols) > 0) {
        return(as.numeric(prob[[num_cols[1]]]))
      }
    }

    preds <- predict(model, new_data = newdata_processed)
    as.numeric(preds[[1]])
  }

  predictor <- iml::Predictor$new(parsnip_fit, data = x, y = y, predict.fun = predict_fun)
  ia <- iml::Interaction$new(predictor, ...)
  p <- plot(ia)
  print(p)
  invisible(ia)
}
