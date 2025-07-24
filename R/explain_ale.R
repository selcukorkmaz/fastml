#' Compute Accumulated Local Effects (ALE) for a fastml model
#'
#' Uses the `iml` package to calculate ALE for the specified feature.
#'
#' @param object A `fastml` object.
#' @param feature Character string specifying the feature name.
#' @param ... Additional arguments passed to `iml::FeatureEffect`.
#'
#' @return An `iml` object containing ALE results.
#' @importFrom iml Predictor FeatureEffect
#' @export
#' @examples
#' \dontrun{
#' data(iris)
#' iris <- iris[iris$Species != "setosa", ]
#' iris$Species <- factor(iris$Species)
#' model <- fastml(data = iris, label = "Species")
#' explain_ale(model, feature = "Sepal.Length")
#' }
explain_ale <- function(object, feature, ...) {
  if (!inherits(object, "fastml")) {
    stop("The input must be a 'fastml' object.")
  }
  if (missing(feature)) {
    stop("'feature' must be provided for ALE.")
  }
  if (!requireNamespace("iml", quietly = TRUE)) {
    stop("The 'iml' package is required for ALE explanations.")
  }

  train_data <- object$processed_train_data
  if (is.null(train_data) || !(object$label %in% names(train_data))) {
    stop("Processed training data not available for ALE.")
  }
  x <- train_data[, setdiff(names(train_data), object$label), drop = FALSE]
  y <- train_data[[object$label]]

  parsnip_fit <- tryCatch(tune::extract_fit_parsnip(object$best_model[[1]]),
                          error = function(e) NULL)
  if (is.null(parsnip_fit) && inherits(object$best_model, "model_fit")) {
    parsnip_fit <- object$best_model
  }
  if (is.null(parsnip_fit)) {
    stop("Unable to extract parsnip model for ALE.")
  }

  predictor <- iml::Predictor$new(parsnip_fit, data = x, y = y)
  fe <- iml::FeatureEffect$new(predictor, feature = feature, method = "ale", ...)
  plot(fe)
  invisible(fe)
}
