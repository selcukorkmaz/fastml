#' Define Ridge Regression Model Specification
#'
#' @inheritParams define_elastic_net_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip linear_reg set_mode set_engine
define_ridge_regression_spec <- function(task, tune = FALSE) {
  if (task != "regression") {
    stop("Ridge regression is only applicable for regression tasks.")
  }
  defaults <- get_default_params("ridge_regression")

  if (tune) {
    model_spec <- linear_reg(
      penalty = tune(),
      mixture = defaults$mixture  # Fixed mixture for Ridge
    ) %>%
      set_mode("regression") %>%
      set_engine("glmnet")
  } else {
    model_spec <- linear_reg(
      penalty = defaults$penalty,
      mixture = defaults$mixture
    ) %>%
      set_mode("regression") %>%
      set_engine("glmnet")
  }
  list(model_spec = model_spec)
}
