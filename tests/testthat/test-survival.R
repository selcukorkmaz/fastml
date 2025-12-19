library(testthat)
library(survival)

test_that("available survival methods include core algorithms", {
  methods <- availableMethods("survival")
  expect_true(all(c("rand_forest", "cox_ph", "penalized_cox", "xgboost") %in% methods))
})

test_that("get_default_engine works for survival algorithms", {
  expect_identical(get_default_engine("rand_forest", task = "survival"), "aorsf")
  expect_identical(get_default_engine("cox_ph", task = "survival"), "survival")
  expect_identical(get_default_engine("penalized_cox", task = "survival"), "glmnet")
  expect_identical(get_default_engine("xgboost", task = "survival"), "aft")
})

test_that("xgboost AFT survival model trains and evaluates", {
  skip_if_not_installed("xgboost")

  data(heart, package = "survival")
  set.seed(123)

  res <- suppressWarnings(
    fastml(
      data = heart,
      label = c("stop", "event"),
      algorithms = "xgboost",
      task = "survival",
      resampling_method = "none",
      test_size = 0.3,
      impute_method = "remove",
      algorithm_engines = list(xgboost = "aft")
    )
  )

  expect_s3_class(res, "fastml")
  expect_identical(res$engine_names$xgboost, "aft")

  perf <- res$performance[[1]]
  expect_true(all(c("c_index", "uno_c", "ibs", "rmst_diff") %in% perf$.metric))

  preds <- res$predictions[[1]]
  expect_true("surv_prob_curve" %in% names(preds))
  expect_true(is.numeric(attr(preds$surv_prob_curve, "eval_times")))
  expect_true("surv_time" %in% names(preds))
  expect_true(any(is.finite(preds$surv_time)))
})


test_that("cox_ph survival model trains and evaluates", {
  data(cancer, package = "survival")

  res <- suppressWarnings(
    fastml(
      data = cancer,
      label = c("time", "status"),
      algorithms = "cox_ph",
      task = "survival",
      resampling_method = "none",
      impute_method = "remove",
      test_size = 0.3
    )
  )

  expect_s3_class(res, "fastml")

  # Check metrics
  perf <- res$performance[[1]]
  expect_true(all(c("c_index", "uno_c", "ibs", "rmst_diff") %in% perf$.metric))
  expect_true(all(c(".lower", ".upper", ".n_boot") %in% names(perf)))

  # Brier-related metrics
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

  # Summary should run cleanly
  expect_no_error(capture.output(summary(res)))

  # Metrics summary without CI
  metrics_no_ci <- capture.output(summary(res, type = "metrics", show_ci = FALSE))
  expect_false(any(grepl("\\([0-9.]+, [0-9.]+\\)", metrics_no_ci)))

  # Metrics summary with CI (should print, even if CI differs)
  metrics_with_ci <- capture.output(summary(res, type = "metrics", show_ci = TRUE))
  expect_true(length(metrics_with_ci) > 0)

  # Brier time summaries — use existing grid and ensure call succeeds
  if (length(res$survival_brier_times) > 0) {
    first_time <- unname(res$survival_brier_times[1])
    expect_no_error(summary(res, type = "metrics", brier_times = first_time))
  }

  # Predictions
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

  # surv_time presence if censored is installed
  if (requireNamespace("censored", quietly = TRUE)) {
    expect_true("surv_time" %in% names(preds))
    expect_type(preds$surv_time, "double")
  }
})

test_that("penalized Cox survival model trains and evaluates", {
  # Ensure required packages are installed
  skip_if_not_installed("censored")
  skip_if_not_installed("glmnet")

  data(cancer, package = "survival")

  # Suppress expected warnings about status recoding and NA removal
  suppressWarnings({
    res <- fastml(
      data = cancer,
      label = c("time", "status"),
      algorithms = "penalized_cox",
      task = "survival",
      resampling_method = "none",
      impute_method = "remove",
      test_size = 0.3
    )
  })

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
  res <- suppressWarnings(
      fastml(
      data = cancer,
      label = c("time", "status"),
      algorithms = c("stratified_cox"),
      task = "survival",
      resampling_method = "none",
      test_size = 0.3,
      impute_method = "remove"
    )
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
  res <- suppressWarnings(
    fastml(
      data = cancer,
      label = c("time", "status"),
      algorithms = c("survreg"),
      task = "survival",
      resampling_method = "none",
      impute_method = "remove",
      test_size = 0.3
    )
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

test_that("parametric_surv flexsurv integration returns survival metrics", {
  skip_if_not_installed("flexsurv")

  suppressWarnings(data(lung, package = "survival"))
  lung_surv <- subset(lung, select = c(time, status, age, sex, ph.ecog))
  lung_surv <- stats::na.omit(lung_surv)
  lung_surv$sex <- factor(lung_surv$sex, levels = 1:2, labels = c("male", "female"))

  set.seed(123)
  res <- suppressWarnings(
    fastml(
      data = lung_surv,
      label = c("time", "status"),
      task = "survival",
      algorithms = "parametric_surv",
      metric = "ibs",
      resampling_method = "none",
      test_size = 0.30,
      impute_method = "remove",
      eval_times = c(90, 180, 365),
      engine_params = list(
        parametric_surv = list(
          flexsurvreg = list(dist = "loglogistic")
        )
      )
    )
  )

  expect_s3_class(res, "fastml")
  perf <- res$performance[[1]]
  expect_true(any(perf$.metric == "ibs"))
  ibs_val <- perf$.estimate[perf$.metric == "ibs"][1]
  expect_true(is.finite(ibs_val))

  brier_metrics <- perf$.metric[grepl("^brier_t", perf$.metric)]
  expect_true(length(brier_metrics) >= 1)
  brier_vals <- perf$.estimate[perf$.metric %in% brier_metrics]
  expect_true(all(is.finite(brier_vals)))
  expect_true(all(brier_vals >= 0 & brier_vals <= 1))
})

test_that("parametric_surv flexsurv integration returns survival metrics", {
  skip_if_not_installed("flexsurv")

  suppressWarnings(data(lung, package = "survival"))
  lung_surv <- subset(lung, select = c(time, status, age, sex, ph.ecog))
  lung_surv <- stats::na.omit(lung_surv)
  lung_surv$sex <- factor(lung_surv$sex, levels = 1:2, labels = c("male", "female"))

  set.seed(123)
  res <- suppressWarnings(
    fastml(
      data = lung_surv,
      label = c("time", "status"),
      task = "survival",
      algorithms = "parametric_surv",
      metric = "ibs",
      resampling_method = "none",
      test_size = 0.30,
      impute_method = "remove",
      eval_times = c(90, 180, 365),
      engine_params = list(
        parametric_surv = list(
          flexsurvreg = list(dist = "loglogistic")
        )
      )
    )
  )

  expect_s3_class(res, "fastml")
  perf <- res$performance[[1]]
  expect_true(any(perf$.metric == "ibs"))
  ibs_val <- perf$.estimate[perf$.metric == "ibs"][1]
  expect_true(is.finite(ibs_val))

  brier_metrics <- perf$.metric[grepl("^brier_t", perf$.metric)]
  expect_true(length(brier_metrics) >= 1)
  brier_vals <- perf$.estimate[perf$.metric %in% brier_metrics]
  expect_true(all(is.finite(brier_vals)))
  expect_true(all(brier_vals >= 0 & brier_vals <= 1))
})

test_that("piecewise_exp flexsurv generates default knots when none supplied", {
  skip_if_not_installed("flexsurv")
  skip_if_not_installed("survival")

  lung <- survival::lung
  lung_surv <- subset(lung, select = c(time, status, age, sex, ph.ecog))
  lung_surv <- stats::na.omit(lung_surv)
  lung_surv$sex <- factor(lung_surv$sex, levels = 1:2, labels = c("male", "female"))

  set.seed(123)
  res <- suppressWarnings(
    fastml(
      data = lung_surv,
      label = c("time", "status"),
      task = "survival",
      algorithms = "piecewise_exp",
      metric = "ibs",
      resampling_method = "none",
      test_size = 0.30,
      impute_method = "remove",
      eval_times = c(90, 180, 365)
    )
  )

  expect_s3_class(res, "fastml")
  model_extras <- res$models[[1]]$extras
  expect_true(is.list(model_extras))
  expect_equal(model_extras$distribution_label, "piecewise exponential")
  expect_false(is.null(model_extras$breaks))
  expect_true(length(model_extras$breaks) >= 1)
  expect_true(all(is.finite(model_extras$breaks)))
})

test_that("survival random forest with aorsf engine trains when available", {
  skip_if_not_installed("aorsf")
  skip_if_not_installed("censored")

  data(cancer, package = "survival")

  set.seed(123)
  res <- suppressWarnings(
    fastml(
      data = cancer,
      label = c("time", "status"),
      algorithms = c("rand_forest"),
      algorithm_engines = list(rand_forest = "aorsf"),
      task = "survival",
      resampling_method = "none",
      impute_method = "remove",
      test_size = 0.3
    )
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
})


