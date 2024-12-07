#' Define Logistic Regression Model Specification
#'
#' @param task Character string specifying the task type ("classification").
#' @param tune Logical indicating whether to use tuning parameters.
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip logistic_reg set_mode set_engine
define_logistic_regression_spec <- function(task, tune = FALSE) {
  if (task != "classification") {
    stop("Logistic regression is only applicable for classification tasks.")
  }
  if (tune) {
    model_spec <- logistic_reg(
      penalty = tune()
    ) %>%
      set_mode("classification") %>%
      set_engine("glmnet")
  } else {
    model_spec <- logistic_reg() %>%
      set_mode("classification") %>%
      set_engine("glm")
  }
  list(model_spec = model_spec)
}
