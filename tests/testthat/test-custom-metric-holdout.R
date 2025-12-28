library(testthat)

test_that("custom summaryFunction metric is included in holdout performance", {
  skip_on_cran()
  required <- c("parsnip", "workflows", "recipes", "yardstick", "tune", "rsample", "dials")
  lapply(required, skip_if_not_installed)

  data <- iris[iris$Species != "setosa", , drop = FALSE]
  data$Species <- factor(data$Species)

  custom_metric <- function(data, truth, estimate, ...) {
    truth <- rlang::enquo(truth)
    estimate <- rlang::enquo(estimate)
    truth_vec <- dplyr::pull(data, !!truth)
    estimate_vec <- dplyr::pull(data, !!estimate)
    tibble::tibble(
      .metric = "custom_metric",
      .estimator = "binary",
      .estimate = mean(truth_vec == estimate_vec, na.rm = TRUE)
    )
  }

  fit <- fastml(
    data = data,
    label = "Species",
    algorithms = "logistic_reg",
    resampling_method = "none",
    use_default_tuning = FALSE,
    tuning_strategy = "none",
    summaryFunction = custom_metric,
    metric = "custom_metric",
    seed = 123
  )

  perf <- fit$performance[[1]]
  if (is.list(perf) && !is.data.frame(perf)) {
    perf <- perf[[1]]
  }
  expect_true("custom_metric" %in% perf$.metric)
})
