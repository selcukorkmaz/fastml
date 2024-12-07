#' Define SVM Radial Model Specification
#'
#' @inheritParams define_svm_linear_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip svm_rbf set_mode set_engine
define_svm_radial_spec <- function(task, tune = FALSE) {
  defaults <- get_default_params("svm_radial")

  if (tune) {
    model_spec <- svm_rbf(
      cost = tune(),
      rbf_sigma = tune()
    ) %>%
      set_mode(task) %>%
      set_engine("kernlab")
  } else {
    model_spec <- svm_rbf(
      cost = defaults$cost,
      rbf_sigma = defaults$rbf_sigma
    ) %>%
      set_mode(task) %>%
      set_engine("kernlab")
  }
  list(model_spec = model_spec)
}
