#' Define Neural Network Model Specification (nnet)
#'
#' @inheritParams define_random_forest_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip mlp set_mode set_engine
define_neural_network_spec <- function(task, tune = FALSE) {
  defaults <- get_default_params("neural_network")

  if (tune) {
    model_spec <- mlp(
      hidden_units = tune(),
      penalty = tune(),
      epochs = tune()
    ) %>%
      set_mode(task) %>%
      set_engine("nnet")
  } else {
    model_spec <- mlp(
      hidden_units = defaults$hidden_units,
      penalty = defaults$penalty,
      epochs = defaults$epochs
    ) %>%
      set_mode(task) %>%
      set_engine("nnet")
  }
  list(model_spec = model_spec)
}
