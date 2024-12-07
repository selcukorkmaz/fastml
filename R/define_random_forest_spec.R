#' Define Random Forest Model Specification
#'
#' @param task Character string specifying the task type: "classification" or "regression".
#' @param train_data Data frame containing the training data.
#' @param label Character string specifying the name of the target variable.
#' @param tune Logical indicating whether to use tuning parameters.
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip rand_forest set_mode set_engine
#' @importFrom dplyr select all_of
define_random_forest_spec <- function(task, train_data, label, tune = FALSE) {
  num_predictors <- ncol(train_data %>% select(-all_of(label)))
  defaults <- get_default_params("random_forest", num_predictors)

  if (tune) {
    model_spec <- rand_forest(
      mtry = tune(),
      trees = tune(),
      min_n = tune()
    ) %>%
      set_mode(task) %>%
      set_engine("ranger")
  } else {
    model_spec <- rand_forest(
      mtry = defaults$mtry,
      trees = defaults$trees,
      min_n = defaults$min_n
    ) %>%
      set_mode(task) %>%
      set_engine("ranger")
  }
  list(model_spec = model_spec)
}
