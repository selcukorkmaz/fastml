#' Define XGBoost Model Specification
#'
#' @inheritParams define_random_forest_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip boost_tree set_mode set_engine
#' @importFrom dplyr select all_of
define_xgboost_spec <- function(task, train_data, label, tune = FALSE) {
  num_predictors <- ncol(train_data %>% select(-all_of(label)))
  defaults <- get_default_params("xgboost", num_predictors)

  if (tune) {
    model_spec <- boost_tree(
      trees = tune(),
      tree_depth = tune(),
      learn_rate = tune(),
      mtry = tune(),
      min_n = tune(),
      loss_reduction = tune(),
      sample_size = tune()
    ) %>%
      set_mode(task) %>%
      set_engine("xgboost")
  } else {
    model_spec <- boost_tree(
      trees = defaults$trees,
      tree_depth = defaults$tree_depth,
      learn_rate = defaults$learn_rate,
      mtry = defaults$mtry,
      min_n = defaults$min_n,
      loss_reduction = defaults$loss_reduction,
      sample_size = defaults$sample_size
    ) %>%
      set_mode(task) %>%
      set_engine("xgboost")
  }
  list(model_spec = model_spec)
}
