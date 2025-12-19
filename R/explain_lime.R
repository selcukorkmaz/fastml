#' Internal predict_model method for parsnip fits
#'
#' Shim for parsnip model objects so that lime's predict_model generic ignores
#' unused arguments passed via `...`.
#' @keywords internal
#' @export
predict_model.model_fit <- function(x, newdata, type, ...) {
  # LIME passes extra args that parsnip doesn't like; ignore them
  predict(x, new_data = newdata, type = type)
}

#' Generate LIME explanations for a fastml model
#'
#' Creates a `lime` explainer using the processed training data stored in the
#' `fastml` object and returns feature explanations for new observations.
#' @param object A `fastml` object.
#' @param new_observation A data frame containing the new observation(s) to explain.
#' @param n_features Number of features to show in the explanation. Default 5.
#' @param n_labels Number of labels to explain (classification only). Default 1.
#' @param ... Additional arguments passed to `lime::explain`.
#'
#' @return An object produced by `lime::explain`.
#' @importFrom lime lime plot_features
#' @importFrom recipes bake
#' @export
#' @examples
#' \dontrun{
#' data(iris)
#' iris <- iris[iris$Species != "setosa", ]
#' iris$Species <- factor(iris$Species)
#' model <- fastml(data = iris, label = "Species")
#' explain_lime(model, new_observation = iris[1, ])
#' }
explain_lime <- function(object, new_observation, n_features = 5, n_labels = 1, ...) {
  if (!inherits(object, "fastml")) {
    stop("The input must be a 'fastml' object.")
  }
  if (missing(new_observation)) {
    stop("'new_observation' must be provided for LIME.")
  }
  if (!requireNamespace("lime", quietly = TRUE)) {
    stop("The 'lime' package is required for LIME explanations.")
  }

  # 1. Get processed training data to build the explainer
  prep <- fastml_prepare_explainer_inputs(object)
  train_x <- prep$x
  if (length(prep$fits) == 0) {
    stop("Unable to extract parsnip model for LIME.")
  }

  # 2. Preprocess the new observation to match training predictors
  if (!is.null(object$preprocessor)) {
    to_explain <- tryCatch(
      recipes::bake(object$preprocessor, new_data = new_observation),
      error = function(e) {
        warning("LIME preprocessing failed; attempting to use raw data.")
        new_observation
      }
    )
    if (!is.null(object$label) && object$label %in% names(to_explain)) {
      to_explain[[object$label]] <- NULL
    }
  } else {
    to_explain <- new_observation
  }

  # 3. Build explainer and generate explanations
  explainer <- lime::lime(train_x, prep$fits[[1]])
  explanation <- lime::explain(
    x = to_explain,
    explainer = explainer,
    n_features = n_features,
    n_labels = n_labels,
    ...
  )
  print(lime::plot_features(explanation))
  invisible(explanation)
}
