#' Define Bayesian GLM Model Specification
#'
#' @inheritParams define_elastic_net_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip linear_reg set_mode set_engine
define_bayes_glm_spec <- function(task) {
  if (!requireNamespace("rstanarm", quietly = TRUE)) {
    stop("The 'rstanarm' package is required but is not installed.")
  }
  if (task != "regression") {
    stop("Bayesian GLM is only applicable for regression tasks.")
  }
  model_spec <- linear_reg() %>%
    set_mode("regression") %>%
    set_engine("stan")
  list(model_spec = model_spec)
}
