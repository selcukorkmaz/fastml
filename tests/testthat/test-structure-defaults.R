library(testthat)

# When `group_cols` or `block_col` is supplied, the folds should respect the
# same structure as the holdout, or fastml() should say that they do not; and
# `exclude` should apply however the data reach fastml().

make_grouped_regression <- function(n_groups = 40, per_group = 5, seed = 1) {
  set.seed(seed)
  n <- n_groups * per_group
  d <- data.frame(
    grp = rep(sprintf("G%02d", seq_len(n_groups)), each = per_group),
    x1 = rnorm(n),
    x2 = rnorm(n)
  )
  d$y <- d$x1 + rnorm(n)
  d
}

# Run `expr`, muffling warnings and messages and returning them alongside the value.
collect_conditions <- function(expr) {
  warnings <- character()
  messages <- character()
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  list(value = value, warnings = warnings, messages = messages)
}

fold_group_overlap <- function(fit, col = "grp") {
  splits <- fastml:::fastml_resample_splits(fit$resampling_plan)$splits
  sum(vapply(splits, function(s) {
    length(intersect(rsample::analysis(s)[[col]], rsample::assessment(s)[[col]]))
  }, integer(1)))
}

test_that("group_cols without resampling_method defaults to grouped folds", {
  d <- make_grouped_regression()

  res <- collect_conditions(fastml(
    data = d,
    label = "y",
    algorithms = "linear_reg",
    group_cols = "grp"
  ))

  expect_true(any(grepl("grouped_cv", res$messages, fixed = TRUE)))
  expect_false(any(grepl("ignores the grouping", res$warnings, fixed = TRUE)))
  expect_identical(fastml:::fastml_resample_method(res$value$resampling_plan), "grouped_cv")
  expect_equal(fold_group_overlap(res$value), 0)

  # The holdout was already grouped; it still is.
  expect_length(
    intersect(unique(res$value$raw_train_data$grp), unique(res$value$raw_test_data$grp)),
    0
  )
})

test_that("default grouped folds are capped at the number of training groups", {
  d <- make_grouped_regression(n_groups = 8, per_group = 10)

  res <- collect_conditions(fastml(
    data = d,
    label = "y",
    algorithms = "linear_reg",
    group_cols = "grp"
  ))

  n_train_groups <- length(unique(res$value$raw_train_data$grp))
  splits <- fastml:::fastml_resample_splits(res$value$resampling_plan)
  expect_equal(nrow(splits), n_train_groups)
  expect_true(any(grepl("grouped folds because the training set has only", res$messages, fixed = TRUE)))
  expect_equal(fold_group_overlap(res$value), 0)
})

test_that("an explicit row-wise method with group_cols warns that folds ignore groups", {
  d <- make_grouped_regression()

  res <- collect_conditions(fastml(
    data = d,
    label = "y",
    algorithms = "linear_reg",
    group_cols = "grp",
    resampling_method = "cv",
    folds = 5
  ))

  expect_true(any(grepl("ignores the grouping", res$warnings, fixed = TRUE)))
  expect_identical(fastml:::fastml_resample_method(res$value$resampling_plan), "cv")
})

test_that("an explicit grouped_cv does not warn about the grouping", {
  d <- make_grouped_regression()

  res <- collect_conditions(fastml(
    data = d,
    label = "y",
    algorithms = "linear_reg",
    group_cols = "grp",
    resampling_method = "grouped_cv",
    folds = 5
  ))

  expect_false(any(grepl("ignores the grouping", res$warnings, fixed = TRUE)))
  expect_false(any(grepl("grouped_cv", res$messages, fixed = TRUE)))
})

test_that("block_col with the default cv warns and points to time-ordered methods", {
  set.seed(5)
  d <- data.frame(t = seq_len(120), x1 = rnorm(120))
  d$y <- d$x1 + rnorm(120)

  res <- collect_conditions(fastml(
    data = d,
    label = "y",
    algorithms = "linear_reg",
    block_col = "t"
  ))

  block_warning <- res$warnings[grepl("`block_col` orders the holdout split", res$warnings, fixed = TRUE)]
  expect_length(block_warning, 1)
  expect_match(block_warning, "blocked_cv", fixed = TRUE)
  expect_match(block_warning, "rolling_origin", fixed = TRUE)

  # Explicit time-ordered folds do not warn.
  res <- collect_conditions(fastml(
    data = d,
    label = "y",
    algorithms = "linear_reg",
    block_col = "t",
    resampling_method = "blocked_cv",
    block_size = 20,
    folds = 4
  ))
  expect_false(any(grepl("`block_col` orders the holdout split", res$warnings, fixed = TRUE)))
})

test_that("group_cols with block_col and no resampling_method warns rather than switching", {
  d <- make_grouped_regression()
  d$t <- seq_len(nrow(d))

  res <- collect_conditions(fastml(
    data = d,
    label = "y",
    algorithms = "linear_reg",
    group_cols = "grp",
    block_col = "t",
    folds = 5
  ))

  expect_true(any(grepl("`block_col` orders the holdout split", res$warnings, fixed = TRUE)))
  expect_identical(fastml:::fastml_resample_method(res$value$resampling_plan), "cv")
})

test_that("rolling_origin never puts tied block_col values on both sides of a cut", {
  set.seed(2)
  d <- data.frame(t = rep(1:30, each = 4), x1 = rnorm(120))

  rs <- fastml:::make_rolling_origin_cv(
    d,
    block_col = "t",
    initial_window = 50,
    assess_window = 10,
    skip = 9
  )

  straddles <- vapply(rs$splits, function(s) {
    max(rsample::analysis(s)$t) >= min(rsample::assessment(s)$t)
  }, logical(1))
  expect_false(any(straddles))

  # Every row still lands in the analysis or assessment set of its split.
  plain <- rsample::rolling_origin(d, initial = 50, assess = 10, skip = 9)
  for (i in seq_along(rs$splits)) {
    expect_setequal(
      c(rs$splits[[i]]$in_id, rs$splits[[i]]$out_id),
      c(plain$splits[[i]]$in_id, plain$splits[[i]]$out_id)
    )
  }
})

test_that("grouped holdout warns when the row share departs from test_size", {
  set.seed(3)
  sizes <- c(rep(2, 5), rep(30, 5))
  d <- data.frame(grp = rep(sprintf("G%02d", 1:10), times = sizes), x1 = rnorm(sum(sizes)))
  d$y <- d$x1 + rnorm(nrow(d))

  holdout_warnings <- character()
  for (s in 1:10) {
    res <- collect_conditions(fastml(
      data = d,
      label = "y",
      algorithms = "linear_reg",
      group_cols = "grp",
      resampling_method = "none",
      seed = s
    ))
    share <- nrow(res$value$raw_test_data) / nrow(d)
    msg <- res$warnings[grepl("applied to groups, not rows", res$warnings, fixed = TRUE)]
    if (abs(share - 0.2) > 0.05) {
      expect_length(msg, 1)
    } else {
      expect_length(msg, 0)
    }
    holdout_warnings <- c(holdout_warnings, msg)
  }
  # Two small groups hold 2.5% of rows, two large ones 37.5%, so some seeds warn.
  expect_gt(length(holdout_warnings), 0)
})

test_that("exclude is applied to train_data and test_data", {
  set.seed(4)
  train <- data.frame(sample_id = factor(sprintf("S%03d", 1:80)), x1 = rnorm(80))
  train$y <- train$x1 + rnorm(80)
  test <- data.frame(sample_id = factor(sprintf("S%03d", 81:100)), x1 = rnorm(20))
  test$y <- test$x1 + rnorm(20)

  res <- collect_conditions(fastml(
    train_data = train,
    test_data = test,
    label = "y",
    algorithms = "linear_reg",
    exclude = "sample_id"
  ))
  fit <- res$value

  expect_false("sample_id" %in% names(fit$raw_train_data))
  expect_false("sample_id" %in% names(fit$raw_test_data))
  expect_setequal(setdiff(names(fit$processed_train_data), "y"), "x1")
  expect_false(any(startsWith(names(fit$processed_test_data), "sample_id")))

  engine_fit <- workflows::extract_fit_engine(fit$best_model[[1]])
  expect_setequal(names(stats::coef(engine_fit)), c("(Intercept)", "x1"))
})

test_that("exclude on pre-split data validates its columns", {
  set.seed(4)
  train <- data.frame(id = 1:40, x1 = rnorm(40))
  train$y <- train$x1 + rnorm(40)
  test <- data.frame(id = 41:50, x1 = rnorm(10))
  test$y <- test$x1 + rnorm(10)

  res <- collect_conditions(fastml(
    train_data = train,
    test_data = test,
    label = "y",
    algorithms = "linear_reg",
    exclude = c("id", "not_a_column")
  ))
  expect_true(any(grepl("Variables not in data: not_a_column", res$warnings, fixed = TRUE)))
  expect_false("id" %in% names(res$value$raw_train_data))

  expect_error(
    fastml(
      train_data = train,
      test_data = test,
      label = "y",
      algorithms = "linear_reg",
      exclude = "y"
    ),
    "Label variable cannot be excluded"
  )
})

test_that("an auto-detected survival task defaults to no resampling", {
  skip_if_not_installed("survival")
  set.seed(6)
  n <- 120
  d <- data.frame(age = rnorm(n, 60, 8), sex = rbinom(n, 1, 0.5))
  d$time <- rexp(n, rate = exp(0.03 * (d$age - 60)) / 50)
  d$status <- rbinom(n, 1, 0.7)

  res <- collect_conditions(fastml(
    data = d,
    label = c("time", "status"),
    algorithms = "cox_ph"
  ))
  expect_identical(res$value$task, "survival")
  expect_null(res$value$resampling_plan)
})
