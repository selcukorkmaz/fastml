#' Define Partial Least Squares Model Specification
#'
#' @inheritParams define_elastic_net_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip pls set_mode set_engine
define_pls_spec <- function(task, tune = FALSE) {
  if (task != "regression") {
    stop("PLS is only applicable for regression tasks.")
  }
  defaults <- get_default_params("pls")

  if (tune) {
    model_spec <- pls(
      num_comp = tune()
    ) %>%
      set_mode("regression") %>%
      set_engine("mixOmics")
  } else {
    model_spec <- pls(
      num_comp = defaults$num_comp
    ) %>%
      set_mode("regression") %>%
      set_engine("mixOmics")
  }
  list(model_spec = model_spec)
}
