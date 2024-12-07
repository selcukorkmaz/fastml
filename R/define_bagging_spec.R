#' Define Bagging Model Specification
#'
#' @inheritParams define_decision_tree_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip bag_tree set_mode set_engine
define_bagging_spec <- function(task, tune = FALSE) {
  defaults <- get_default_params("bagging")
  if (tune) {
    model_spec <- bag_tree(
      min_n = tune()
    ) %>%
      set_mode(task) %>%
      set_engine("rpart", times = 25)
  } else {
    model_spec <- bag_tree(
      min_n = defaults$min_n
    ) %>%
      set_mode(task) %>%
      set_engine("rpart", times = 25)
  }
  list(model_spec = model_spec)
}
