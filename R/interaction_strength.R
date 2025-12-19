#' Compute feature interaction strengths for a fastml model
#'
#' Uses the `iml` package to quantify the strength of feature interactions.
#'
#' @param object A `fastml` object.
#' @param ... Additional arguments passed to `iml::Interaction`.
#'
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
interaction_strength <- function(object, ...) {
  if (!inherits(object, "fastml")) {
    stop("The input must be a 'fastml' object.")
  }
  if (!requireNamespace("iml", quietly = TRUE)) {
    stop("The 'iml' package is required for interaction strength.")
  }

  prep <- fastml_prepare_explainer_inputs(object)
  train_data <- prep$train_data
  if (is.null(train_data) || !(prep$label %in% names(train_data))) {
    stop("Training data not available for interaction strength.")
  }
  x <- prep$x_raw
  y <- prep$y_raw

  # Convert factor/character targets to numeric (0/1) for iml
  positive_class <- prep$positive_class
  if (is.factor(y) || is.character(y)) {
    y_factor <- if (is.factor(y)) y else factor(y)
    if (is.null(positive_class) || !(positive_class %in% levels(y_factor))) {
      positive_class <- levels(y_factor)[1]
    }
    y <- as.numeric(y_factor == positive_class)
  }

  parsnip_fit <- prep$fits[[1]]

  # Custom predict function returning numeric probabilities (or numeric predictions)
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
