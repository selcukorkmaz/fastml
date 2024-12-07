#' Define Ranger Model Specification
#'
#' @inheritParams define_random_forest_spec
#' @return List containing the model specification (`model_spec`).
define_ranger_spec <- function(task, train_data, label, tune = FALSE) {
  define_random_forest_spec(task, train_data, label, tune)
}
