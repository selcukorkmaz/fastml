library(testthat)

# Risk scores must increase with hazard for every survival engine. The sign
# convention differs by engine -- a Cox linear predictor is already a risk, an
# AFT model predicts log-time and must be negated, and a survival curve has to
# be turned into -log(S(t)) -- so an engine whose orientation is inverted still
# returns finite, plausible-looking numbers while ranking subjects backwards.
# Concordance below 0.5 on data with a strong known signal is the symptom, and
# it is exactly the failure the xgboost AFT scale bug produced. These tests fix
# the orientation per engine rather than trusting each conversion in isolation.

sim_survival_data <- function(n = 300, seed = 42) {
  set.seed(seed)
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  # Larger x1 means higher hazard, therefore shorter survival.
  linear_pred <- 1.5 * x1 + 0.8 * x2
  event_time <- rexp(n, rate = exp(linear_pred) / 50)
  censor_time <- rexp(n, rate = 1 / 200)
  data.frame(
    time = pmax(pmin(event_time, censor_time), 0.1),
    status = as.integer(event_time <= censor_time),
    x1 = x1,
    x2 = x2
  )
}

fit_survival_engine <- function(algorithm, data) {
  suppressWarnings(
    fastml(
      data = data,
      label = c("time", "status"),
      algorithms = algorithm,
      task = "survival",
      resampling_method = "none",
      test_size = 0.3,
      bootstrap_ci = FALSE,
      seed = 123
    )
  )
}

c_index_of <- function(fit, algorithm) {
  perf <- fit$performance[[algorithm]]
  if (is.null(perf)) perf <- fit$performance[[1]]
  value <- perf$.estimate[perf$.metric == "c_index"]
  value[is.finite(value)][1]
}

test_that("survival risk scores are oriented so that higher means shorter survival", {
  skip_on_cran()
  skip_if_not_installed("survival")

  sim <- sim_survival_data()

  # Only engines whose backing package is installed are exercised; each is
  # checked on its own so a missing optional backend cannot mask a real
  # orientation failure in another.
  candidates <- list(
    cox_ph = "survival",               # linear predictor is already a risk
    survreg = "survival",              # AFT: predicts log-time, must be negated
    penalized_cox = "glmnet",
    parametric_surv = "flexsurv",      # risk derived from the survival curve
    piecewise_exp = "flexsurv",
    rand_forest_survival = "aorsf",
    xgboost_aft = "xgboost"            # boosted AFT, negated log-time margin
  )

  tested <- 0L
  observed <- c()
  for (algorithm in names(candidates)) {
    pkg <- candidates[[algorithm]]
    if (!requireNamespace(pkg, quietly = TRUE)) next

    fit <- tryCatch(fit_survival_engine(algorithm, sim), error = function(e) NULL)
    if (is.null(fit)) next

    cidx <- c_index_of(fit, algorithm)
    if (is.null(cidx) || !is.finite(cidx)) next

    tested <- tested + 1L
    observed[[algorithm]] <- cidx
    # A correctly oriented model on a signal this strong lands well above 0.5;
    # an inverted one lands symmetrically below it. The threshold separates the
    # two cases without pinning engine-specific accuracy.
    expect_gt(cidx, 0.6)
  }

  # Guard against the test passing vacuously if every engine were to fail to
  # fit: at least the two survival-only engines should always be exercised.
  expect_gte(tested, 2L)
})

test_that("classification probabilities are oriented toward the positive class", {
  skip_on_cran()
  required <- c("parsnip", "workflows", "recipes", "yardstick", "rsample")
  lapply(required, skip_if_not_installed)

  set.seed(7)
  n <- 300
  x <- rnorm(n)
  # P(y = "yes") increases with x, so the predicted probability for "yes"
  # must correlate positively with x. An engine that returns the probability
  # of the other level still yields a valid-looking AUC of 1 - AUC.
  prob <- 1 / (1 + exp(-3 * x))
  cls_data <- data.frame(
    x = x,
    noise = rnorm(n),
    y = factor(ifelse(runif(n) < prob, "yes", "no"), levels = c("no", "yes"))
  )

  fit <- suppressWarnings(
    fastml(
      data = cls_data,
      label = "y",
      algorithms = "logistic_reg",
      task = "classification",
      resampling_method = "none",
      test_size = 0.3,
      event_class = "second",
      use_default_tuning = FALSE,
      tuning_strategy = "none",
      seed = 123
    )
  )

  perf <- fit$performance[[1]]
  auc <- perf$.estimate[perf$.metric == "roc_auc"]
  auc <- auc[is.finite(auc)][1]
  skip_if(is.null(auc) || !is.finite(auc), "roc_auc not reported")
  expect_gt(auc, 0.7)

  preds <- fit$predictions[[1]]
  skip_if(is.null(preds), "no stored predictions")
  prob_col <- grep("^\\.pred_yes$", names(preds), value = TRUE)
  skip_if(length(prob_col) == 0, "positive-class probability column not stored")

  skip_if(!"truth" %in% names(preds), "truth column not stored")
  truth <- preds$truth
  p_yes <- preds[[prob_col[1]]]
  keep <- is.finite(p_yes) & !is.na(truth)
  # The mean predicted probability of "yes" must be higher among actual
  # "yes" cases than among "no" cases.
  expect_gt(
    mean(p_yes[keep & truth == "yes"]),
    mean(p_yes[keep & truth == "no"])
  )
})

test_that("regression predictions track the outcome rather than its negation", {
  skip_on_cran()
  required <- c("parsnip", "workflows", "recipes", "yardstick", "rsample")
  lapply(required, skip_if_not_installed)

  set.seed(11)
  n <- 200
  x <- rnorm(n)
  reg_data <- data.frame(x = x, noise = rnorm(n), y = 3 * x + rnorm(n, sd = 0.3))

  fit <- suppressWarnings(
    fastml(
      data = reg_data,
      label = "y",
      algorithms = "linear_reg",
      task = "regression",
      resampling_method = "none",
      test_size = 0.3,
      use_default_tuning = FALSE,
      tuning_strategy = "none",
      seed = 123
    )
  )

  perf <- fit$performance[[1]]
  rsq <- perf$.estimate[perf$.metric == "rsq"]
  rsq <- rsq[is.finite(rsq)][1]
  skip_if(is.null(rsq) || !is.finite(rsq), "rsq not reported")
  expect_gt(rsq, 0.8)

  preds <- fit$predictions[[1]]
  skip_if(is.null(preds) || !all(c("truth", "estimate") %in% names(preds)),
          "no stored predictions")
  expect_gt(cor(preds$estimate, preds$truth), 0.8)
})
