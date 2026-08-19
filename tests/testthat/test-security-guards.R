library(testthat)

# Behavioural tests for the recipe-scanning guard in R/security_guards.R.
# The guard skips a safe-list of standard steps and, for any remaining step,
# flags two patterns: a reference to the global environment / a parent frame,
# and an embedded external data object.

make_df <- function(n = 50) {
  data.frame(
    x = seq_len(n) / n,
    y = rep(c(0, 1), length.out = n),
    g = factor(rep(letters[1:3], length.out = n))
  )
}

# Attaches a synthetic non-standard step carrying `payload` to `rec`, so the
# scanner sees a step outside its safe-list.
add_custom_step <- function(rec, payload, id) {
  step <- rec$steps[[1]]
  class(step) <- c(paste0("step_", id), "step")
  step$payload <- payload
  step$id <- id
  rec$steps <- c(rec$steps, list(step))
  rec
}

safe_recipe <- function(df = make_df()) {
  recipes::recipe(y ~ ., data = df) |>
    recipes::step_impute_median(recipes::all_numeric_predictors()) |>
    recipes::step_normalize(recipes::all_numeric_predictors()) |>
    recipes::step_dummy(recipes::all_nominal_predictors()) |>
    recipes::step_novel(recipes::all_nominal_predictors()) |>
    recipes::step_zv(recipes::all_predictors())
}

test_that("safe-listed standard steps are not flagged", {
  skip_if_not_installed("recipes")

  expect_length(fastml:::fastml_detect_leaky_recipe_steps(safe_recipe()), 0)
  expect_silent(fastml:::fastml_validate_user_recipe(safe_recipe()))
})

test_that("standard steps outside the safe-list are not false positives", {
  skip_if_not_installed("recipes")

  # These steps are not on the safe-list, so they are scanned in full; none of
  # them embeds external data or reaches into the global environment, and the
  # scanner must not reject them.
  rec <- recipes::recipe(y ~ ., data = make_df()) |>
    recipes::step_pca(recipes::all_numeric_predictors(), num_comp = 2) |>
    recipes::step_log(x, offset = 1) |>
    recipes::step_YeoJohnson(recipes::all_numeric_predictors()) |>
    recipes::step_corr(recipes::all_numeric_predictors())

  expect_length(fastml:::fastml_detect_leaky_recipe_steps(rec), 0)
})

test_that("a step embedding an external data.frame is flagged", {
  skip_if_not_installed("recipes")

  # Regression test: data.frames are themselves lists, so a scanner that tests
  # is.list() before the data.frame check recurses into the columns and reports
  # no finding. This pattern must be flagged, not silently accepted.
  lookup <- data.frame(key = 1:3, value = c(0.1, 0.2, 0.3))
  rec <- add_custom_step(safe_recipe(), lookup, "custom_join")

  expect_identical(
    fastml:::fastml_detect_leaky_recipe_steps(rec),
    "custom_join"
  )
})

test_that("a step embedding an external tibble is flagged", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("tibble")

  rec <- add_custom_step(safe_recipe(), tibble::tibble(key = 1:3), "custom_tbl")

  expect_identical(
    fastml:::fastml_detect_leaky_recipe_steps(rec),
    "custom_tbl"
  )
})

test_that("steps reaching into the global environment are flagged", {
  skip_if_not_installed("recipes")

  global_env <- add_custom_step(
    safe_recipe(), function(z) get("external_object", envir = globalenv()),
    "custom_globalenv"
  )
  parent_frame <- add_custom_step(
    safe_recipe(), quote(parent.frame()), "custom_parent_frame"
  )
  dot_global <- add_custom_step(safe_recipe(), quote(.GlobalEnv), "custom_dot_global")

  expect_identical(
    fastml:::fastml_detect_leaky_recipe_steps(global_env), "custom_globalenv"
  )
  expect_identical(
    fastml:::fastml_detect_leaky_recipe_steps(parent_frame), "custom_parent_frame"
  )
  expect_identical(
    fastml:::fastml_detect_leaky_recipe_steps(dot_global), "custom_dot_global"
  )
})

test_that("validation aborts on a flagged recipe rather than dropping the step", {
  skip_if_not_installed("recipes")

  rec <- add_custom_step(safe_recipe(), data.frame(key = 1:3), "custom_join")

  expect_error(
    fastml:::fastml_validate_user_recipe(rec),
    "depend on external data"
  )
  # The offending step is named so the user can act on the message.
  expect_error(fastml:::fastml_validate_user_recipe(rec), "custom_join")
})

test_that("pretrained recipes are rejected", {
  skip_if_not_installed("recipes")

  df <- make_df()
  trained <- recipes::prep(safe_recipe(df), training = df)

  expect_error(
    fastml:::fastml_validate_user_recipe(trained),
    "Pretrained recipes are not allowed"
  )
})

test_that("non-recipe input is rejected", {
  expect_error(
    fastml:::fastml_validate_user_recipe(make_df()),
    "must be a recipes::recipe object",
    fixed = TRUE
  )
})
