library(testthat)

test_that("grouped holdout keeps groups intact", {
  skip_on_cran()
  required <- c("parsnip", "workflows", "recipes", "yardstick", "tune", "rsample", "dials")
  lapply(required, skip_if_not_installed)

  set.seed(123)
  grouped_data <- tibble::tibble(
    group = rep(letters[1:5], each = 4),
    x = rnorm(20),
    y = rnorm(20)
  )

  fit <- fastml(
    data = grouped_data,
    label = "y",
    algorithms = "linear_reg",
    task = "regression",
    resampling_method = "none",
    group_cols = "group",
    test_size = 0.2,
    use_default_tuning = FALSE,
    tuning_strategy = "none",
    seed = 123
  )

  train_groups <- unique(as.character(fit$raw_train_data$group))
  test_groups <- unique(as.character(fit$raw_test_data$group))
  expect_length(intersect(train_groups, test_groups), 0)
})

test_that("time-ordered holdout uses tail rows", {
  skip_on_cran()
  required <- c("parsnip", "workflows", "recipes", "yardstick", "tune", "rsample", "dials")
  lapply(required, skip_if_not_installed)

  time_data <- tibble::tibble(
    time_id = 1:20,
    x = rnorm(20),
    y = rnorm(20)
  )

  fit <- fastml(
    data = time_data,
    label = "y",
    algorithms = "linear_reg",
    task = "regression",
    resampling_method = "none",
    block_col = "time_id",
    test_size = 0.2,
    use_default_tuning = FALSE,
    tuning_strategy = "none",
    seed = 123
  )

  expect_equal(max(fit$raw_train_data$time_id), 16)
  expect_equal(min(fit$raw_test_data$time_id), 17)
})
