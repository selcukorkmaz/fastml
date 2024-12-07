#' Define K-Nearest Neighbors Model Specification
#'
#' @inheritParams define_random_forest_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip nearest_neighbor set_mode set_engine
define_knn_spec <- function(task, tune = FALSE) {
  defaults <- get_default_params("knn")

  if (tune) {
    model_spec <- nearest_neighbor(
      neighbors = tune(),
      weight_func = tune(),
      dist_power = tune()
    ) %>%
      set_mode(task) %>%
      set_engine("kknn")
  } else {
    model_spec <- nearest_neighbor(
      neighbors = defaults$neighbors,
      weight_func = defaults$weight_func,
      dist_power = defaults$dist_power
    ) %>%
      set_mode(task) %>%
      set_engine("kknn")
  }
  list(model_spec = model_spec)
}
