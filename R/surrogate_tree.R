#' Fit a surrogate decision tree for a fastml model
#'
#' Builds an interpretable tree approximating the behaviour of the underlying
#' model using the `iml` package.
#'
#' @param object A `fastml` object.
#' @param maxdepth Maximum depth of the surrogate tree. Default 3.
#' @param ... Additional arguments passed to `iml::TreeSurrogate`.
#'
#' @return An `iml::TreeSurrogate` object.
#' @importFrom iml Predictor TreeSurrogate
#' @export
#' @examples
#' \dontrun{
#' data(iris)
#' iris <- iris[iris$Species != "setosa", ]
#' iris$Species <- factor(iris$Species)
#' model <- fastml(data = iris, label = "Species")
#' surrogate_tree(model)
#' }
surrogate_tree <- function(object, maxdepth = 3, ...) {
  if (!inherits(object, "fastml")) {
    stop("The input must be a 'fastml' object.")
  }
  if (!requireNamespace("iml", quietly = TRUE)) {
    stop("The 'iml' package is required for surrogate models.")
  }

  train_data <- object$processed_train_data
  if (is.null(train_data) || !(object$label %in% names(train_data))) {
    stop("Processed training data not available for surrogate models.")
  }
  x <- train_data[, setdiff(names(train_data), object$label), drop = FALSE]
  y <- train_data[[object$label]]

  parsnip_fit <- tryCatch(tune::extract_fit_parsnip(object$best_model[[1]]),
                          error = function(e) NULL)
  if (is.null(parsnip_fit) && inherits(object$best_model, "model_fit")) {
    parsnip_fit <- object$best_model
  }
  if (is.null(parsnip_fit)) {
    stop("Unable to extract parsnip model for surrogate tree.")
  }

  predictor <- iml::Predictor$new(parsnip_fit, data = x, y = y)
  tree <- iml::TreeSurrogate$new(predictor, maxdepth = maxdepth, ...)
  plot(tree)
  invisible(tree)
}
