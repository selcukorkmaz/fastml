#' Generate LIME explanations for a fastml model
#'
#' Creates a `lime` explainer using the processed training data stored in the
#' `fastml` object and returns feature explanations for new observations.
#'
#' @param object A `fastml` object.
#' @param n_features Number of features to show in the explanation. Default 5.
#' @param n_labels Number of labels to explain (classification only). Default 1.
#' @param ... Additional arguments passed to `lime::explain`.
#'
#' @return An object produced by `lime::explain`.
#' @importFrom lime lime plot_features
#' @export
#' @examples
#' \dontrun{
#' data(iris)
#' iris <- iris[iris$Species != "setosa", ]
#' iris$Species <- factor(iris$Species)
#' model <- fastml(data = iris, label = "Species")
#' explain_lime(model)
#' }
explain_lime <- function(object, n_features = 5, n_labels = 1, ...) {
  if (!inherits(object, "fastml")) {
    stop("The input must be a 'fastml' object.")
  }
  if (!requireNamespace("lime", quietly = TRUE)) {
    stop("The 'lime' package is required for LIME explanations.")
  }

  prep <- fastml_prepare_explainer_inputs(object)
  x <- prep$x
  if (length(prep$fits) == 0) {
    stop("Unable to extract parsnip model for LIME.")
  }

  explainer <- lime::lime(x, prep$fits[[1]])
  explanation <- lime::explain(x, explainer,
                               n_features = n_features,
                               n_labels = n_labels, ...)
  print(lime::plot_features(explanation))
  invisible(explanation)
}
