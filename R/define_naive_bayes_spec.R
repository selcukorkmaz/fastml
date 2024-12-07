#' Define Naive Bayes Model Specification
#'
#' @inheritParams define_logistic_regression_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip naive_Bayes set_mode set_engine
define_naive_bayes_spec <- function(task, tune = FALSE) {
  if (task != "classification") {
    stop("Naive Bayes is only applicable for classification tasks.")
  }
  defaults <- get_default_params("naive_bayes")

  if (tune) {
    model_spec <- naive_Bayes(
      smoothness = tune(),
      Laplace = tune()
    ) %>%
      set_mode("classification") %>%
      set_engine("klaR")
  } else {
    model_spec <- naive_Bayes(
      smoothness = defaults$smoothness,
      Laplace = defaults$Laplace
    ) %>%
      set_mode("classification") %>%
      set_engine("klaR")
  }
  list(model_spec = model_spec)
}
