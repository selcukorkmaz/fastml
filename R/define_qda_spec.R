#' Define Quadratic Discriminant Analysis Model Specification
#'
#' @inheritParams define_logistic_regression_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip set_mode set_engine discrim_quad
define_qda_spec <- function(task) {
  if (task != "classification") {
    stop("QDA is only applicable for classification tasks.")
  }
  model_spec <- discrim_quad() %>%
    set_mode("classification") %>%
    set_engine("MASS")
  list(model_spec = model_spec)
}
