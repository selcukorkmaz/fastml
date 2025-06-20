#' Generate counterfactual explanations for a fastml model
#'
#' Uses the `ceterisParibus` package to compute counterfactuals for a given
#' observation.
#'
#' @param object A `fastml` object.
#' @param observation A single observation (data frame with one row) to compute
#'   counterfactuals for.
#' @param ... Additional arguments passed to `ceterisParibus::calculate_counterfactuals`.
#'
#' @return A counterfactual explanation object.
#' @importFrom ceterisParibus calculate_counterfactuals
#' @importFrom DALEX explain
#' @export
#' @examples
#' \dontrun{
#' data(iris)
#' iris <- iris[iris$Species != "setosa", ]
#' iris$Species <- factor(iris$Species)
#' model <- fastml(data = iris, label = "Species")
#' counterfactual_explain(model, iris[1, ])
#' }
counterfactual_explain <- function(object, observation, ...) {
  if (!inherits(object, "fastml")) {
    stop("The input must be a 'fastml' object.")
  }
  if (!requireNamespace("ceterisParibus", quietly = TRUE)) {
    stop("The 'ceterisParibus' package is required for counterfactuals.")
  }
  if (missing(observation)) {
    stop("'observation' must be provided for counterfactual explanations.")
  }
  if (nrow(observation) != 1) {
    stop("'observation' must contain exactly one row.")
  }

  train_data <- object$processed_train_data
  if (is.null(train_data) || !(object$label %in% names(train_data))) {
    stop("Processed training data not available for counterfactual explanations.")
  }
  x <- train_data[, setdiff(names(train_data), object$label), drop = FALSE]

  parsnip_fit <- tryCatch(tune::extract_fit_parsnip(object$best_model[[1]]),
                          error = function(e) NULL)
  if (is.null(parsnip_fit) && inherits(object$best_model, "model_fit")) {
    parsnip_fit <- object$best_model
  }
  if (is.null(parsnip_fit)) {
    stop("Unable to extract parsnip model for counterfactuals.")
  }

  explainer <- DALEX::explain(parsnip_fit, data = x,
                             y = train_data[[object$label]])
  cf <- ceterisParibus::calculate_counterfactuals(explainer,
                                                  observation = observation, ...)
  plot(cf)
  invisible(cf)
}
