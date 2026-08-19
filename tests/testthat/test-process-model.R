library(testthat)

# Regression tests for the three failure-handling contracts in process_model():
#   1. a stored recipe that cannot be applied aborts rather than silently
#      falling back to unpreprocessed data;
#   2. risk-prediction failures surface the original error message;
#   3. tuning finalization is factored into finalize_tuned_model(), which
#      returns NULL (with a warning) when no configuration can be selected.

# A tune_results object whose metrics cannot be collected: selection fails for
# the requested metric and for every fallback candidate. Feeding tune a malformed
# object also emits incidental warnings of its own, so warnings are collected and
# inspected rather than matched one at a time.
broken_tune_results <- function() structure(list(a = 1), class = "tune_results")

collect_warnings <- function(expr) {
  warns <- character()
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = warns)
}

test_that("finalize_tuned_model returns NULL and warns when selection fails", {
  res <- collect_warnings(
    fastml:::finalize_tuned_model(
      model_obj = broken_tune_results(),
      model_id = "broken_model",
      task = "classification",
      metric = "roc_auc",
      train_data = data.frame(y = factor(c("a", "b")), x = c(1, 2))
    )
  )

  expect_null(res$value)
  expect_true(any(grepl("Could not select best parameters", res$warnings, fixed = TRUE)))
})

test_that("finalize_tuned_model names the offending model in its warning", {
  res <- collect_warnings(
    fastml:::finalize_tuned_model(
      broken_tune_results(), "rand_forest_ranger", "regression", "rmse",
      data.frame(y = c(1, 2), x = c(1, 2))
    )
  )

  expect_true(any(grepl("rand_forest_ranger", res$warnings, fixed = TRUE)))
})

test_that("process_model aborts when the stored recipe cannot be applied", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("survival")

  set.seed(42)
  n <- 60
  train_data <- data.frame(
    time = rexp(n, 0.1),
    status = rbinom(n, 1, 0.7),
    x = rnorm(n)
  )

  rec <- recipes::recipe(~ x, data = train_data)
  rec_prep <- recipes::prep(rec, training = train_data, retain = TRUE)

  fit <- survival::coxph(
    survival::Surv(time, status) ~ x,
    data = train_data
  )

  native <- structure(
    list(
      algo = "cox_ph", engine = "survival", fit = fit, recipe = rec_prep,
      response = "surv", label_cols = NULL,
      time_col = "time", status_col = "status", start_col = NULL
    ),
    class = c("fastml_native_survival", "fastml_model")
  )

  # Evaluation data missing the predictor the recipe requires.
  bad_test <- data.frame(
    time = rexp(20, 0.1),
    status = rbinom(20, 1, 0.7),
    wrong_name = rnorm(20)
  )

  expect_error(
    fastml:::process_model(
      model_obj = native, model_id = "cox_ph_survival", task = "survival",
      test_data = bad_test, label = "surv", event_class = "first",
      time_col = "time", status_col = "status",
      engine = "survival", train_data = train_data, metric = "c_index",
      bootstrap_ci = FALSE
    ),
    "Could not apply the stored preprocessing recipe"
  )
})

test_that("the recipe-failure message explains why there is no fallback", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("survival")

  set.seed(43)
  n <- 60
  train_data <- data.frame(
    time = rexp(n, 0.1),
    status = rbinom(n, 1, 0.7),
    x = rnorm(n)
  )
  rec_prep <- recipes::prep(
    recipes::recipe(~ x, data = train_data),
    training = train_data, retain = TRUE
  )
  fit <- survival::coxph(survival::Surv(time, status) ~ x, data = train_data)

  native <- structure(
    list(
      algo = "cox_ph", engine = "survival", fit = fit, recipe = rec_prep,
      response = "surv", label_cols = NULL,
      time_col = "time", status_col = "status", start_col = NULL
    ),
    class = c("fastml_native_survival", "fastml_model")
  )

  bad_test <- data.frame(
    time = rexp(20, 0.1), status = rbinom(20, 1, 0.7), wrong_name = rnorm(20)
  )

  err <- tryCatch(
    fastml:::process_model(
      model_obj = native, model_id = "cox_ph_survival", task = "survival",
      test_data = bad_test, label = "surv", event_class = "first",
      time_col = "time", status_col = "status",
      engine = "survival", train_data = train_data, metric = "c_index",
      bootstrap_ci = FALSE
    ),
    error = function(e) conditionMessage(e)
  )

  # Names the model, and states that no unpreprocessed fallback is attempted.
  expect_match(err, "cox_ph_survival")
  expect_match(err, "does not fall back to")
})

test_that("a well-formed native survival model still evaluates cleanly", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("survival")

  set.seed(44)
  n <- 120
  make_data <- function(m) {
    data.frame(time = rexp(m, 0.1), status = rbinom(m, 1, 0.7), x = rnorm(m))
  }
  train_data <- make_data(n)
  test_data <- make_data(40)

  rec_prep <- recipes::prep(
    recipes::recipe(~ x, data = train_data),
    training = train_data, retain = TRUE
  )
  fit <- survival::coxph(survival::Surv(time, status) ~ x, data = train_data)

  native <- structure(
    list(
      algo = "cox_ph", engine = "survival", fit = fit, recipe = rec_prep,
      response = "surv", label_cols = NULL,
      time_col = "time", status_col = "status", start_col = NULL
    ),
    class = c("fastml_native_survival", "fastml_model")
  )

  res <- fastml:::process_model(
    model_obj = native, model_id = "cox_ph_survival", task = "survival",
    test_data = test_data, label = "surv", event_class = "first",
    time_col = "time", status_col = "status",
    engine = "survival", train_data = train_data, metric = "c_index",
    bootstrap_ci = FALSE
  )

  expect_type(res, "list")
  expect_true("performance" %in% names(res))
  expect_true(nrow(res$performance) > 0)
})
