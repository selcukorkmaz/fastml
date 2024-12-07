#' Define C5.0 Model Specification
#'
#' @inheritParams define_random_forest_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip boost_tree set_mode set_engine
define_c5_0_spec <- function(task, tune = FALSE) {
  if (task != "classification") {
    stop("C5.0 is only applicable for classification tasks.")
  }
  defaults <- get_default_params("c5.0")

  if (tune) {
    model_spec <- boost_tree(
      trees = tune(),
      min_n = tune(),
      sample_size = tune()
    ) %>%
      set_mode("classification") %>%
      set_engine("C5.0")
  } else {
    model_spec <- boost_tree(
      trees = defaults$trees,
      min_n = defaults$min_n,
      sample_size = defaults$sample_size
    ) %>%
      set_mode("classification") %>%
      set_engine("C5.0")
  }
  list(model_spec = model_spec)
}
