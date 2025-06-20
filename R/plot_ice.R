#' Plot ICE curves for a fastml model
#'
#' Generates Individual Conditional Expectation (ICE) plots for selected features
#' using the `pdp` package.
#'
#' @param object A `fastml` object.
#' @param features Character vector of feature names to plot.
#' @param ... Additional arguments passed to `pdp::partial`.
#'
#' @return A `ggplot` object displaying ICE curves.
#' @importFrom pdp partial
#' @export
#' @examples
#' \dontrun{
#' data(iris)
#' iris <- iris[iris$Species != "setosa", ]
#' iris$Species <- factor(iris$Species)
#' model <- fastml(data = iris, label = "Species")
#' plot_ice(model, features = "Sepal.Length")
#' }
plot_ice <- function(object, features, ...) {
  if (!inherits(object, "fastml")) {
    stop("The input must be a 'fastml' object.")
  }
  if (missing(features)) {
    stop("'features' must be specified for ICE plots.")
  }
  if (!requireNamespace("pdp", quietly = TRUE)) {
    stop("The 'pdp' package is required for ICE plots.")
  }

  train_data <- object$processed_train_data
  if (is.null(train_data) || !(object$label %in% names(train_data))) {
    stop("Processed training data not available for ICE plots.")
  }

  x <- train_data[, setdiff(names(train_data), object$label), drop = FALSE]

  parsnip_fit <- tryCatch(tune::extract_fit_parsnip(object$best_model[[1]]),
                          error = function(e) NULL)
  if (is.null(parsnip_fit) && inherits(object$best_model, "model_fit")) {
    parsnip_fit <- object$best_model
  }
  if (is.null(parsnip_fit)) {
    stop("Unable to extract parsnip model for ICE plots.")
  }

  pd <- pdp::partial(parsnip_fit, pred.var = features, ice = TRUE, train = x, ...)
  p <- plot(pd)
  print(p)
  invisible(p)
}
