library(testthat)
library(survival)

test_that("available survival methods include core algorithms", {
  expect_true(all(c("rand_forest", "cox_ph", "penalized_cox") %in% availableMethods("survival")))
  expect_true("xgboost" %in% availableMethods("survival"))
})

test_that("get_default_engine works for survival algorithms", {
  expect_identical(get_default_engine("rand_forest", task = "survival"), "aorsf")
  expect_identical(get_default_engine("cox_ph", task = "survival"), "survival")
  expect_identical(get_default_engine("penalized_cox", task = "survival"), "glmnet")
  expect_identical(get_default_engine("xgboost", task = "survival"), "aft")
})

test_that("xgboost Cox survival model trains and predicts", {
  skip_if_not_installed("xgboost")

  data(lung, package = "survival")
  lung <- lung[complete.cases(lung[, c("time", "status", "age", "sex", "ph.ecog")]), ]

  set.seed(123)
  res <- fastml(
    data = lung,
    label = c("time", "status"),
    algorithms = "xgboost",
    task = "survival",
    resampling_method = "none",
    test_size = 0.3,
    impute_method = "remove",
    algorithm_engines = list(xgboost = "cox")
  )

  expect_s3_class(res, "fastml")
  perf <- res$performance[[1]]
  expect_setequal(perf$.metric, c("c_index", "uno_c"))
  harrell <- perf$.estimate[perf$.metric == "c_index"]
  uno <- perf$.estimate[perf$.metric == "uno_c"]
  expect_true(all(is.finite(harrell)) && all(harrell >= 0 & harrell <= 1))
  expect_true(all(is.finite(uno)) && all(uno >= 0 & uno <= 1))

  spec <- res$models[[1]]
  newdata <- lung[1:5, setdiff(names(lung), c("time", "status")), drop = FALSE]
  risk_vals <- predict_risk(spec, newdata = newdata)
  expect_length(risk_vals, nrow(newdata))
  expect_true(all(is.finite(risk_vals)))

  expect_error(
    predict_survival(spec, newdata = newdata, times = c(100, 200)),
    "risk ranking only"
  )
})

test_that("xgboost AFT survival model trains and predicts", {
  skip_if_not_installed("xgboost")

  data(heart, package = "survival")
  heart <- heart[complete.cases(heart[, c("start", "stop", "event")]), ]

  set.seed(321)
  res <- fastml(
    data = heart,
    label = c("stop", "event"),
    algorithms = "xgboost",
    task = "survival",
    resampling_method = "none",
    test_size = 0.3,
    impute_method = "remove",
    algorithm_engines = list(xgboost = "aft"),
    engine_params = list(xgboost = list(aft = list(aft_loss_distribution = "normal", aft_loss_distribution_scale = 1.2)))
  )

  expect_s3_class(res, "fastml")
  perf <- res$performance[[1]]
  expect_true(all(c("c_index", "uno_c", "ibs") %in% perf$.metric))
  harrell <- perf$.estimate[perf$.metric == "c_index"]
  uno <- perf$.estimate[perf$.metric == "uno_c"]
  ibs_val <- perf$.estimate[perf$.metric == "ibs"]
  expect_true(all(is.finite(harrell)) && all(harrell >= 0 & harrell <= 1))
  expect_true(all(is.finite(uno)) && all(uno >= 0 & uno <= 1))
  expect_true(all(is.finite(ibs_val)) && all(ibs_val >= 0))

  spec <- res$models[[1]]
  newdata <- heart[1:4, setdiff(names(heart), c("stop", "event")), drop = FALSE]
  risk_vals <- predict_risk(spec, newdata = newdata)
  expect_length(risk_vals, nrow(newdata))
  expect_true(all(is.finite(risk_vals)))

  eval_times <- seq(10, 40, length.out = 3)
  surv_mat <- predict_survival(spec, newdata = newdata, times = eval_times)
  expect_equal(dim(surv_mat), c(nrow(newdata), length(eval_times)))
  expect_true(all(is.finite(surv_mat)))
  expect_true(all(surv_mat >= 0 & surv_mat <= 1))
  apply(surv_mat, 1, function(row) expect_true(all(diff(row) <= 1e-8)))
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
  expect_true(all(c("c_index", "uno_c", "ibs", "rmst_diff") %in% perf$.metric))
  expect_true(all(c(".lower", ".upper", ".n_boot") %in% names(perf)))
  brier_metrics <- perf$.metric[grepl("^brier_t", perf$.metric)]
  expect_true(length(brier_metrics) >= 1)
  brier_val <- perf$.estimate[perf$.metric %in% brier_metrics]
  expect_true(length(brier_val) > 0)
  expect_true(all(is.finite(brier_val)))
  expect_true(all(brier_val >= 0 & brier_val <= 1))
  ibs_val <- perf$.estimate[perf$.metric == "ibs"]
  expect_true(all(is.finite(ibs_val)))
  expect_true(all(ibs_val >= 0 & ibs_val <= 1))
  expect_true(length(res$survival_brier_times) >= 1)
  expect_true(all(names(res$survival_brier_times) %in% brier_metrics))
  expect_identical(res$engine_names$cox_ph, "survival")
  # Summary should not error and should print
  expect_no_error(capture.output(summary(res)))
  metrics_only <- capture.output(summary(res, type = "metrics"))
  expect_false(any(grepl("Brier\\(t=", metrics_only, fixed = TRUE)))
  if (length(res$survival_brier_times) > 0) {
    first_time <- unname(res$survival_brier_times[1])
    metrics_with_brier <- capture.output(summary(res, type = "metrics", brier_times = first_time))
    expect_true(any(grepl("Brier\\(t=", metrics_with_brier, fixed = TRUE)))
  }
  metrics_no_ci <- capture.output(summary(res, type = "metrics", show_ci = FALSE))
  expect_false(any(grepl("\([0-9.]+, [0-9.]+\)", metrics_no_ci)))
  metrics_with_ci <- capture.output(summary(res, type = "metrics", show_ci = TRUE))
  expect_true(any(grepl("\([0-9.]+, [0-9.]+\)", metrics_with_ci)))
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

test_that("penalized Cox survival model trains and evaluates", {
  skip_if_not_installed("censored")
  skip_if_not_installed("glmnet")

  data(cancer, package = "survival")
  res <- fastml(
    data = cancer,
    label = c("time", "status"),
    algorithms = c("penalized_cox"),
    task = "survival",
    resampling_method = "none",
    test_size = 0.3
  )

  expect_s3_class(res, "fastml")
  perf <- res$performance[[1]]
  expect_true(all(c("c_index", "uno_c", "ibs", "rmst_diff") %in% perf$.metric))
  brier_metrics <- perf$.metric[grepl("^brier_t", perf$.metric)]
  expect_true(length(brier_metrics) >= 1)
  brier_val <- perf$.estimate[perf$.metric %in% brier_metrics]
  expect_true(length(brier_val) > 0)
  expect_true(all(is.finite(brier_val)))
  expect_true(all(brier_val >= 0 & brier_val <= 1))
  expect_identical(res$engine_names$penalized_cox, "glmnet")

  preds <- res$predictions[[1]]
  expect_true("surv_prob_curve" %in% names(preds))
  expect_true(is.numeric(attr(preds$surv_prob_curve, "eval_times")))
  if (requireNamespace("censored", quietly = TRUE)) {
    expect_true("surv_time" %in% names(preds))
    expect_type(preds$surv_time, "double")
  }
})

test_that("stratified Cox summary reports strata without coefficients", {
  data(cancer, package = "survival")
  cancer$strata_inst <- factor(cancer$inst)
  set.seed(123)
  res <- fastml(
    data = cancer,
    label = c("time", "status"),
    algorithms = c("stratified_cox"),
    task = "survival",
    resampling_method = "none",
    test_size = 0.3,
    impute_method = "remove"
  )
  expect_s3_class(res, "fastml")
  summary_lines <- capture.output(summary(res))
  expect_true(any(grepl("^  Stratified by:", summary_lines)))
  expect_false(any(grepl("^\\s*inst\\s+<NA>", summary_lines)))
  spec <- res$models[[1]]
  expect_true(inherits(spec, "fastml_native_survival"))
  strata_info <- spec$strata_info[["strata_inst"]]
  expect_true(is.list(strata_info))
  expected_levels <- strata_info$levels
  expected_levels <- expected_levels[!is.na(expected_levels) & expected_levels != ""]
  strat_line <- summary_lines[grepl("inst \\(", summary_lines)]
  expect_true(length(strat_line) >= 1)
  if (length(expected_levels) > 0) {
    printed_levels <- sub("^.*strata: ", "", strat_line[1])
    printed_levels <- sub("\\)$", "", printed_levels)
    printed_levels <- trimws(strsplit(printed_levels, ",")[[1]])
    expect_setequal(printed_levels, expected_levels)
  }
  perf_tbl <- res$performance[[1]]
  expect_true(is.data.frame(perf_tbl))
  c_index <- perf_tbl$.estimate[perf_tbl$.metric == "c_index"][1]
  conc_line <- summary_lines[grepl("^  Concordance \\(Harrell C-index\\):", summary_lines)]
  expect_true(length(conc_line) == 1)
  conc_value <- as.numeric(trimws(sub("^  Concordance \\(Harrell C-index\\):", "", conc_line[1])))
  expect_true(is.finite(conc_value))
  expect_equal(conc_value, signif(c_index, 4))
})

test_that("survreg survival model returns Brier scores", {
  data(cancer, package = "survival")
  res <- fastml(
    data = cancer,
    label = c("time", "status"),
    algorithms = c("survreg"),
    task = "survival",
    resampling_method = "none",
    test_size = 0.3
  )
  expect_s3_class(res, "fastml")
  perf <- res$performance[[1]]
  expect_true(all(c("c_index", "uno_c", "ibs", "rmst_diff") %in% perf$.metric))
  brier_metrics <- perf$.metric[grepl("^brier_t", perf$.metric)]
  expect_true(length(brier_metrics) >= 1)
  brier_val <- perf$.estimate[perf$.metric %in% brier_metrics]
  expect_true(length(brier_val) > 0)
  expect_true(all(is.finite(brier_val)))
  expect_true(all(brier_val >= 0 & brier_val <= 1))
  preds <- res$predictions[[1]]
  expect_true("surv_prob_curve" %in% names(preds))
  expect_true(is.numeric(attr(preds$surv_prob_curve, "eval_times")))
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
    expect_true(all(c("c_index", "uno_c", "ibs", "rmst_diff") %in% pf$aorsf$.metric))
    brier_metrics <- pf$aorsf$.metric[grepl("^brier_t", pf$aorsf$.metric)]
    expect_true(length(brier_metrics) >= 1)
    brier_val <- pf$aorsf$.estimate[pf$aorsf$.metric %in% brier_metrics]
    expect_true(length(brier_val) > 0)
    expect_true(all(is.finite(brier_val)))
    expect_true(all(brier_val >= 0 & brier_val <= 1))
  } else {
    # Single engine path
    expect_true(all(c("c_index", "uno_c", "ibs", "rmst_diff") %in% pf$.metric))
    brier_metrics <- pf$.metric[grepl("^brier_t", pf$.metric)]
    expect_true(length(brier_metrics) >= 1)
    brier_val <- pf$.estimate[pf$.metric %in% brier_metrics]
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
    expect_true(all(c("c_index", "uno_c", "ibs", "rmst_diff") %in% pf$ranger$.metric))
  } else {
    expect_true(all(c("c_index", "uno_c", "ibs", "rmst_diff") %in% pf$.metric))
  }
})
