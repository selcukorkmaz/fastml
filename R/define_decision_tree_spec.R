#' Define Decision Tree Model Specification
#'
#' @inheritParams define_random_forest_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip decision_tree set_mode set_engine
define_decision_tree_spec <- function(task, tune = FALSE) {
  defaults <- get_default_params("decision_tree")

  if (tune) {
    model_spec <- decision_tree(
      tree_depth = tune(),
      min_n = tune(),
      cost_complexity = tune()
    ) %>%
      set_mode(task) %>%
      set_engine("rpart")
  } else {
    model_spec <- decision_tree(
      tree_depth = defaults$tree_depth,
      min_n = defaults$min_n,
      cost_complexity = defaults$cost_complexity
    ) %>%
      set_mode(task) %>%
      set_engine("rpart")
  }
  list(model_spec = model_spec)
}
