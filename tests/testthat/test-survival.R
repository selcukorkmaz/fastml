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
  brier_val <- perf$.estimate[perf$.metric == "brier_score"]
  expect_true(length(brier_val) > 0)
  expect_true(all(is.finite(brier_val)))
  expect_true(all(brier_val >= 0 & brier_val <= 1))
  expect_identical(res$engine_names$cox_ph, "survival")
  # Summary should not error and should print
  expect_no_error(capture.output(summary(res)))
  # If censored is installed, surv_time should be present and numeric
  if (requireNamespace("censored", quietly = TRUE)) {
    preds <- res$predictions[[1]]
    expect_true("surv_time" %in% names(preds))
    expect_type(preds$surv_time, "double")
  }
  preds <- res$predictions[[1]]
  expect_true("surv_prob_curve" %in% names(preds))
  eval_times_attr <- attr(preds$surv_prob_curve, "eval_times")
  expect_true(is.numeric(eval_times_attr))
  if (length(preds$surv_prob_curve) > 0) {
    first_curve <- preds$surv_prob_curve[[1]]
    if (!is.null(first_curve)) {
      expect_true(is.numeric(first_curve))
    }
  }
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
    brier_val <- pf$aorsf$.estimate[pf$aorsf$.metric == "brier_score"]
    expect_true(length(brier_val) > 0)
    expect_true(all(is.finite(brier_val)))
    expect_true(all(brier_val >= 0 & brier_val <= 1))
  } else {
    # Single engine path
    expect_true(all(c("c_index", "brier_score", "logrank_p") %in% pf$.metric))
    brier_val <- pf$.estimate[pf$.metric == "brier_score"]
    expect_true(length(brier_val) > 0)
    expect_true(all(is.finite(brier_val)))
    expect_true(all(brier_val >= 0 & brier_val <= 1))
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
