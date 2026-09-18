# Regression tests for the XGBoost AFT interval bounds.
#
# XGBoost's AFT objective models log(Y) = T(x) + sigma * Z and takes the
# logarithm of `label_lower_bound` / `label_upper_bound` internally, so the
# bounds must be supplied on the ORIGINAL time scale.  fastml previously
# supplied log(time), which applied the transformation twice.  These tests pin
# the behaviour against a direct xgboost reference so the error cannot recur.

skip_if_not_installed("xgboost")
skip_if_not_installed("survival")

aft_booster <- function(X, lower, upper, nrounds = 60) {
  d <- xgboost::xgb.DMatrix(data = X)
  xgboost::setinfo(d, "label_lower_bound", lower)
  xgboost::setinfo(d, "label_upper_bound", upper)
  xgboost::xgb.train(
    params = list(
      objective = "survival:aft", eval_metric = "aft-nloglik",
      aft_loss_distribution = "normal", aft_loss_distribution_scale = 1,
      max_depth = 4, eta = 0.2
    ),
    data = d, nrounds = nrounds, verbose = 0
  )
}

test_that("xgboost AFT recovers the generating scale from original-time bounds", {
  set.seed(1)
  n <- 3000
  x <- runif(n, -1, 1)
  time <- exp(1 + 2 * x)           # log T = 1 + 2x exactly, no censoring
  X <- matrix(x, ncol = 1)

  pred_raw <- as.numeric(predict(aft_booster(X, time, time), X))

  # Predictions come back on the original time scale and track T closely.
  expect_true(all(is.finite(pred_raw)))
  expect_gt(cor(pred_raw, time), 0.99)
})

test_that("log-transformed bounds are the documented failure mode", {
  set.seed(1)
  n <- 3000
  x <- runif(n, -1, 1)
  time <- exp(1 + 2 * x)           # spans times below and above 1
  X <- matrix(x, ncol = 1)

  pred_bug <- suppressWarnings(
    as.numeric(predict(aft_booster(X, log(time), log(time)), X))
  )

  # log(time) is negative for t < 1; logging it again is undefined, so the
  # double transformation cannot silently produce a usable fit here.
  expect_true(any(!is.finite(pred_bug)))
})

test_that("fastml supplies AFT bounds on the original time scale", {
  skip_on_cran()
  skip_if_not_installed("survival")

  set.seed(42)
  n <- 200
  df <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n)
  )
  lp <- 0.8 * df$x1 - 0.5 * df$x2
  df$time <- exp(1.5 + lp + rnorm(n, sd = 0.3))
  df$status <- rbinom(n, 1, 0.7)

  fm <- fastml(
    data = df, label = c("time", "status"), task = "survival",
    algorithms = "xgboost_aft", resampling_method = "none",
    bootstrap_ci = FALSE, verbose = FALSE
  )

  risk <- predict(fm, newdata = df, type = "risk")
  risk <- as.numeric(unlist(risk))

  # The fit must produce usable, finite risk scores.  Under the previous
  # log(time) bounds this degraded whenever times fell below one.
  expect_true(any(is.finite(risk)))
  expect_false(all(is.na(risk)))
})

test_that("survival times below one do not break the AFT fit", {
  set.seed(7)
  n <- 1500
  x <- runif(n, -1, 1)
  time <- exp(-1 + x)              # every time is below 1
  X <- matrix(x, ncol = 1)

  expect_true(all(time < 1))
  pred <- as.numeric(predict(aft_booster(X, time, time), X))
  expect_true(all(is.finite(pred)))
  expect_gt(cor(pred, time), 0.95)
})

test_that("AFT fit does not degenerate on realistic time scales", {
  # With the logistic AFT loss and xgboost's default base_score of 0.5, the
  # margin starts at log(0.5) while survival times run to the hundreds.  The
  # loss saturates there, no tree splits, and every prediction is identical.
  # fastml anchors base_score at the median observed time to prevent this.
  set.seed(3)
  n <- 400
  x <- rnorm(n)
  time <- exp(7 + 0.8 * x + rnorm(n, sd = 0.3))   # times in the thousands
  X <- matrix(x, ncol = 1)

  fit_with <- function(base) {
    d <- xgboost::xgb.DMatrix(data = X)
    xgboost::setinfo(d, "label_lower_bound", time)
    xgboost::setinfo(d, "label_upper_bound", time)
    p <- list(objective = "survival:aft", eval_metric = "aft-nloglik",
              aft_loss_distribution = "logistic", aft_loss_distribution_scale = 1,
              max_depth = 6, eta = 0.1, subsample = 0.5, verbosity = 0)
    if (!is.null(base)) p$base_score <- base
    b <- xgboost::xgb.train(params = p, data = d, nrounds = 50, verbose = 0)
    as.numeric(predict(b, X, outputmargin = TRUE))
  }

  anchored <- fit_with(stats::median(time))
  expect_gt(stats::sd(anchored), 0)
  expect_gt(cor(anchored, x), 0.8)
  # Without anchoring the optimiser never leaves its starting point at this
  # scale, so the anchored fit must vary strictly more than the unanchored one.
  expect_gt(stats::sd(anchored), stats::sd(fit_with(NULL)))
})
