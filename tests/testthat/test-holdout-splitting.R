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

test_that("grouped time-ordered holdout keeps groups intact and preserves order", {
  skip_on_cran()
  required <- c("parsnip", "workflows", "recipes", "yardstick", "tune", "rsample", "dials")
  lapply(required, skip_if_not_installed)

  set.seed(321)
  # Contiguous groups of four in time order: cut points exist at every
  # multiple of four, and the one nearest 80% of 20 rows is 16.
  both_data <- tibble::tibble(
    time_id = 1:20,
    group = rep(letters[1:5], each = 4),
    x = rnorm(20),
    y = rnorm(20)
  )

  fit <- fastml(
    data = both_data,
    label = "y",
    algorithms = "linear_reg",
    task = "regression",
    resampling_method = "none",
    block_col = "time_id",
    group_cols = "group",
    test_size = 0.2,
    use_default_tuning = FALSE,
    tuning_strategy = "none",
    seed = 123
  )

  train_groups <- unique(as.character(fit$raw_train_data$group))
  test_groups <- unique(as.character(fit$raw_test_data$group))
  expect_length(intersect(train_groups, test_groups), 0)
  expect_lt(max(fit$raw_train_data$time_id), min(fit$raw_test_data$time_id))
  expect_equal(max(fit$raw_train_data$time_id), 16)
})

test_that("grouped time-ordered holdout snaps to the nearest intact-group cut", {
  # Groups of three rows do not align with the 80% target of 20 rows, so the
  # cut moves to the nearest group boundary rather than splitting a group.
  snap_data <- data.frame(
    time_id = 1:21,
    group = rep(letters[1:7], each = 3)
  )

  cut_point <- fastml:::fastml_grouped_time_cut(snap_data, "group", target_cut = 17L)
  expect_equal(cut_point, 18L)

  group_at_cut <- snap_data$group[cut_point]
  expect_false(group_at_cut %in% snap_data$group[seq.int(cut_point + 1L, nrow(snap_data))])
})

test_that("grouped time-ordered holdout errors when groups are interleaved in time", {
  interleaved <- data.frame(
    time_id = 1:20,
    group = rep(letters[1:5], times = 4)
  )

  expect_error(
    fastml:::fastml_grouped_time_cut(interleaved, "group", target_cut = 16L),
    "interleaved"
  )
})

test_that("grouped time-ordered cut resolves ties toward more training data", {
  tied <- data.frame(
    time_id = 1:10,
    group = rep(letters[1:5], each = 2)
  )

  # Target 5 is equidistant from admissible cuts 4 and 6; the later one wins.
  expect_equal(fastml:::fastml_grouped_time_cut(tied, "group", target_cut = 5L), 6L)
})
