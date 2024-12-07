#' Define SVM Linear Model Specification
#'
#' @inheritParams define_random_forest_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip svm_linear set_mode set_engine
define_svm_linear_spec <- function(task, tune = FALSE) {
  defaults <- get_default_params("svm_linear")

  if (tune) {
    model_spec <- svm_linear(
      cost = tune()
    ) %>%
      set_mode(task) %>%
      set_engine("kernlab")
  } else {
    model_spec <- svm_linear(
      cost = defaults$cost
    ) %>%
      set_mode(task) %>%
      set_engine("kernlab")
  }
  list(model_spec = model_spec)
}
