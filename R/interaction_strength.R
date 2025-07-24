#' Compute feature interaction strengths for a fastml model
#'
#' Uses the `iml` package to quantify the strength of feature interactions.
#'
#' @param object A `fastml` object.
#' @param ... Additional arguments passed to `iml::Interaction`.
#'
#' @return An `iml::Interaction` object.
#' @importFrom iml Predictor Interaction
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

  train_data <- object$processed_train_data
  if (is.null(train_data) || !(object$label %in% names(train_data))) {
    stop("Processed training data not available for interaction strength.")
  }
  x <- train_data[, setdiff(names(train_data), object$label), drop = FALSE]
  y <- train_data[[object$label]]

  parsnip_fit <- tryCatch(tune::extract_fit_parsnip(object$best_model[[1]]),
                          error = function(e) NULL)
  if (is.null(parsnip_fit) && inherits(object$best_model, "model_fit")) {
    parsnip_fit <- object$best_model
  }
  if (is.null(parsnip_fit)) {
    stop("Unable to extract parsnip model for interaction strength.")
  }

  predictor <- iml::Predictor$new(parsnip_fit, data = x, y = y)
  ia <- iml::Interaction$new(predictor, ...)
  plot(ia)
  invisible(ia)
}
