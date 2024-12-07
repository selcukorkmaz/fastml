#' Define Penalized Logistic Regression Model Specification
#'
#' @inheritParams define_logistic_regression_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip logistic_reg set_engine
#' @importFrom tune finalize_model
define_penalized_logistic_regression_spec <- function(task, tune = FALSE) {
  if (task != "classification") {
    stop("Penalized logistic regression is only applicable for classification tasks.")
  }
  defaults <- get_default_params("penalized_logistic_regression")

  if (tune) {
    model_spec <- logistic_reg(
      penalty = tune(),
      mixture = tune()
    ) %>%
      set_engine("glmnet")
  } else {
    model_spec <- logistic_reg(
      penalty = defaults$penalty,
      mixture = defaults$mixture
    ) %>%
      set_engine("glmnet")
    model_spec <- finalize_model(model_spec, parameters = tibble::tibble())
  }
  list(model_spec = model_spec)
}
