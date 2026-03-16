library(testthat)

# ============================================================================
# Unit tests for bugfix round 2
# ============================================================================

# ---------- Fix #2: event_class validation -----------------------------------

test_that("fastml rejects invalid event_class values", {
  expect_error(
    fastml(
      data = iris,
      label = "Species",
      algorithms = "logistic_reg",
      event_class = "fist"
    ),
    "'arg' should be one of"
  )
  expect_error(
    fastml(
      data = iris,
      label = "Species",
      algorithms = "logistic_reg",
      event_class = "third"
    ),
    "'arg' should be one of"
  )
})

test_that("fastml accepts valid event_class values without validation error", {
  # Valid event_class should pass validation. We trigger a DIFFERENT error
  # (missing label) to prove event_class itself was accepted.
  expect_error(
    fastml(
      data = iris,
      label = "nonexistent_col",
      algorithms = "logistic_reg",
      event_class = "first"
    ),
    "Label variable|not found|not present|does not exist"
  )
  expect_error(
    fastml(
      data = iris,
      label = "nonexistent_col",
      algorithms = "logistic_reg",
      event_class = "second"
    ),
    "Label variable|not found|not present|does not exist"
  )
})

test_that("train_models rejects invalid event_class values", {
  expect_error(
    train_models(
      train_data = iris,
      label = "Species",
      task = "classification",
      algorithms = "logistic_reg",
      event_class = "invalid"
    ),
    "'arg' should be one of"
  )
})

# ---------- Fix #6: dead perf_values statement removed -----------------------
# No behavioral test needed — this was dead code removal.
# Verified by the full test suite passing.

# ---------- Fix #7: unused true_labels removed -------------------------------
# No behavioral test needed — this was dead code removal.
# Verified by the full test suite passing.

# ---------- Fix #4: duplicate switch keys in engine_helpers ------------------

test_that("get_default_engine returns correct engines for previously-duplicated keys", {
  expect_equal(get_default_engine("survreg", "survival"), "survival")
  expect_equal(get_default_engine("royston_parmar", "survival"), "rstpm2")
  expect_equal(get_default_engine("cox_ph", "survival"), "survival")
})

test_that("get_default_engine errors for unknown algorithm", {
  expect_error(
    get_default_engine("nonexistent_algo", "classification"),
    "No default engine"
  )
})

# ---------- Fix #5: security guard avoids package-owned .GlobalEnv writes ----
# The sandbox intercepts direct rm/assign calls to .GlobalEnv. The regression
# guard here ensures the package itself does not write deleted objects back into
# the user's global environment while auditing hook side effects.

test_that("security guard does not restore deleted globals into .GlobalEnv", {
  src_path <- file.path(getwd(), "R", "security_guards.R")
  if (!file.exists(src_path)) {
    src_path <- system.file("R", "security_guards.R", package = "fastml")
  }
  if (!file.exists(src_path) || length(readLines(src_path)) == 0) {
    skip("Cannot locate security_guards.R source")
  }
  src <- readLines(src_path)

  assign_lines <- grep('assign.*pre_values.*\\.GlobalEnv', src)
  expect_length(assign_lines, 0)
})

test_that("added globals can still be cleaned up without restoring deletions", {
  env <- new.env(parent = emptyenv())
  assign("a", 1, envir = env)

  pre_names <- ls(env, all.names = TRUE)
  assign("extra", 99, envir = env)

  post_names <- ls(env, all.names = TRUE)
  added <- setdiff(post_names, pre_names)
  expect_equal(added, "extra")

  rm(list = added, envir = env)

  expect_false(exists("extra", envir = env))
  expect_true(exists("a", envir = env))
})

# ---------- Fix #3: logistic_reg/multinom_reg pre-loop swap ------------------

test_that("in-loop logistic_reg swap was removed from train_models", {
  src_path <- file.path(getwd(), "R", "train_models.R")
  if (!file.exists(src_path)) {
    src_path <- system.file("R", "train_models.R", package = "fastml")
  }
  if (!file.exists(src_path) || length(readLines(src_path)) == 0) {
    skip("Cannot locate train_models.R source")
  }
  src <- readLines(src_path)
  # The old in-loop swap line should no longer exist
  in_loop_swap <- grep(
    'n_class > 2 && algo == "logistic_reg".*algo.*=.*"multinom_reg"',
    src
  )
  expect_length(in_loop_swap, 0)
})

test_that("pre-loop multiclass swap exists in train_models", {
  src_path <- file.path(getwd(), "R", "train_models.R")
  if (!file.exists(src_path)) {
    src_path <- system.file("R", "train_models.R", package = "fastml")
  }
  if (!file.exists(src_path) || length(readLines(src_path)) == 0) {
    skip("Cannot locate train_models.R source")
  }
  src <- readLines(src_path)
  # Pre-loop swap should exist: algorithms[...logistic_reg...] <- "multinom_reg"
  pre_loop_swap <- grep(
    'algorithms\\[algorithms == "logistic_reg"\\] <- "multinom_reg"',
    src
  )
  expect_true(length(pre_loop_swap) > 0)
})

# ---------- Fix #1: selected_outer_id in nested CV --------------------------

test_that("fastml_nested_select_params returns correct best params", {
  # Create mock inner_results with two param configs across two folds
  mock_fold1 <- tibble::tibble(
    .metric = rep("rmse", 2),
    .estimator = rep("standard", 2),
    .estimate = c(1.5, 1.1),
    cost_complexity = c(0.01, 0.001)
  )
  mock_fold2 <- tibble::tibble(
    .metric = rep("rmse", 2),
    .estimator = rep("standard", 2),
    .estimate = c(1.3, 1.2),
    cost_complexity = c(0.01, 0.001)
  )
  inner_results <- list(mock_fold1, mock_fold2)

  result <- fastml_nested_select_params(
    inner_results = inner_results,
    metric = "rmse",
    lower_is_better = TRUE
  )

  expect_true(is.data.frame(result))
  expect_true("cost_complexity" %in% names(result))
  expect_equal(nrow(result), 1)
  # cost_complexity=0.001: avg rmse = (1.1+1.2)/2 = 1.15
  # cost_complexity=0.01:  avg rmse = (1.5+1.3)/2 = 1.40
  # Should pick 0.001
  expect_equal(result$cost_complexity[1], 0.001)
})

test_that("fastml_nested_select_params handles all-NULL inner results", {
  result <- fastml_nested_select_params(
    inner_results = list(NULL, NULL),
    metric = "rmse",
    lower_is_better = TRUE
  )
  expect_null(result)
})
