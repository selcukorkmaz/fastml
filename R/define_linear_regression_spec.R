#' Define Linear Regression Model Specification
#'
#' @inheritParams define_elastic_net_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip linear_reg set_mode set_engine
define_linear_regression_spec <- function(task) {
  if (task != "regression") {
    stop("Linear regression is only applicable for regression tasks.")
  }
  model_spec <- linear_reg() %>%
    set_mode("regression") %>%
    set_engine("lm")
  list(model_spec = model_spec)
}
