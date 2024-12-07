#' Define Elastic Net Model Specification
#'
#' @param task Character string specifying the task type ("regression").
#' @inheritParams define_random_forest_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip linear_reg set_mode set_engine
define_elastic_net_spec <- function(task, tune = FALSE) {
  if (task != "regression") {
    stop("Elastic Net is only applicable for regression tasks.")
  }
  defaults <- get_default_params("elastic_net")
  if (tune) {
    model_spec <- linear_reg(
      penalty = tune(),
      mixture = tune()
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
