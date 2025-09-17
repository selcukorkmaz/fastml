library(testthat)
library(survival)

test_that("available survival methods include rand_forest and cox_ph", {
  expect_true(all(c("rand_forest", "cox_ph") %in% availableMethods("survival")))
})

test_that("get_default_engine works for survival algorithms", {
  expect_identical(get_default_engine("rand_forest", task = "survival"), "aorsf")
  expect_identical(get_default_engine("cox_ph", task = "survival"), "survival")
})

test_that("cox_ph survival model trains and evaluates", {
  data(cancer, package = "survival")
  res <- fastml(
    data = cancer,
    label = c("time", "status"),
    algorithms = c("cox_ph"),
    task = "survival",
    resampling_method = "none",
    test_size = 0.3
  )
  expect_s3_class(res, "fastml")
  # Ensure performance contains survival metrics
  perf <- res$performance[[1]]
  expect_true(all(c("c_index", "brier_score", "logrank_p") %in% perf$.metric))
  # Summary should not error and should print
  expect_no_error(capture.output(summary(res)))
  # If censored is installed, surv_time should be present and numeric
  if (requireNamespace("censored", quietly = TRUE)) {
    preds <- res$predictions[[1]]
    expect_true("surv_time" %in% names(preds))
    expect_type(preds$surv_time, "double")
  }
})

test_that("coxnet survival model returns finite risk predictions", {
  skip_if_not_installed("glmnet")
  data(cancer, package = "survival")
  cancer <- cancer[complete.cases(cancer), ]
  set.seed(42)
  res <- fastml(
    data = cancer,
    label = c("time", "status"),
    algorithms = "coxnet",
    task = "survival",
    resampling_method = "none",
    test_size = 0.3,
    metric = "c_index"
  )
  expect_s3_class(res, "fastml")
  perf <- res$performance[[1]]
  c_index <- perf$.estimate[perf$.metric == "c_index"]
  expect_equal(length(c_index), 1)
  expect_true(is.finite(c_index))
  preds <- res$predictions[[1]]
  expect_true("risk" %in% names(preds))
  expect_gt(sum(is.finite(preds$risk)), 0)
})

test_that("survival random forest with aorsf engine trains when available", {
  skip_if_not_installed("aorsf")
  skip_if_not_installed("censored")
  data(cancer, package = "survival")
  res <- fastml(
    data = cancer,
    label = c("time", "status"),
    algorithms = c("rand_forest"),
    task = "survival",
    resampling_method = "none",
    test_size = 0.3
  )
  expect_s3_class(res, "fastml")
  # Check performance exists for the (aorsf) engine
  nm <- names(res$performance)[[1]]
  pf <- res$performance[[nm]]
  if (is.list(pf)) {
    expect_true("aorsf" %in% names(pf))
    expect_true(all(c("c_index", "brier_score", "logrank_p") %in% pf$aorsf$.metric))
  } else {
    # Single engine path
    expect_true(all(c("c_index", "brier_score", "logrank_p") %in% pf$.metric))
  }
})

test_that("survival random forest can run with ranger engine when censored is available", {
  skip_if_not_installed("censored")
  skip_if_not_installed("ranger")
  data(cancer, package = "survival")
  res <- fastml(
    data = cancer,
    label = c("time", "status"),
    algorithms = c("rand_forest"),
    task = "survival",
    resampling_method = "none",
    test_size = 0.3,
    algorithm_engines = list(rand_forest = "ranger")
  )
  expect_s3_class(res, "fastml")
  # Check performance exists for the (ranger) engine
  nm <- names(res$performance)[[1]]
  pf <- res$performance[[nm]]
  if (is.list(pf)) {
    expect_true("ranger" %in% names(pf))
    expect_true(all(c("c_index", "brier_score", "logrank_p") %in% pf$ranger$.metric))
  } else {
    expect_true(all(c("c_index", "brier_score", "logrank_p") %in% pf$.metric))
  }
})
