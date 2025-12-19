library(testthat)

test_that("grouped_cv rejects missing group values", {
  skip_if_not_installed("rsample")

  dat <- tibble::tibble(
    group = c("a", NA_character_, "b", "b"),
    value = 1:4
  )

  expect_error(
    fastml:::make_grouped_cv(
      dat,
      group_cols = "group",
      v = 2,
      strata = NULL,
      repeats = 1
    ),
    "missing values",
    ignore.case = TRUE
  )
})

test_that("grouped_cv supports multiple grouping columns", {
  skip_if_not_installed("rsample")

  set.seed(1)
  dat <- tibble::tibble(
    g1 = rep(c("A", "B", "C"), each = 4),
    g2 = rep(rep(1:2, each = 2), times = 3),
    value = rnorm(12)
  )

  resamples <- fastml:::make_grouped_cv(
    dat,
    group_cols = c("g1", "g2"),
    v = 3,
    strata = NULL,
    repeats = 1
  )

  purrr::walk(resamples$splits, function(split) {
    analysis_groups <- unique(rsample::analysis(split)$.fastml_group)
    assessment_groups <- unique(rsample::assessment(split)$.fastml_group)
    expect_length(intersect(analysis_groups, assessment_groups), 0)
  })
})

test_that("blocked_cv validates ordering and block size", {
  set.seed(2)
  dat <- tibble::tibble(
    time = seq_len(10),
    value = rnorm(10)
  )

  info <- fastml:::make_blocked_cv(
    dat,
    block_col = "time",
    block_size = 3
  )

  expect_equal(info$block_ids, c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4))
  expect_equal(info$n_blocks, 4)

  unsorted <- dat[c(2, 1, 3:10), , drop = FALSE]
  expect_error(
    fastml:::make_blocked_cv(unsorted, block_col = "time", block_size = 3),
    "sorted",
    ignore.case = TRUE
  )

  expect_error(
    fastml:::make_blocked_cv(dat, block_col = "time", block_size = 10),
    "fewer than two blocks",
    ignore.case = TRUE
  )
})

test_that("rolling_origin creates leakage-safe expanding windows", {
  skip_if_not_installed("rsample")

  set.seed(3)
  dat <- tibble::tibble(
    time = seq_len(10),
    value = rnorm(10)
  )

  resamples <- fastml:::make_rolling_origin_cv(
    dat,
    block_col = "time",
    initial_window = 4,
    assess_window = 2,
    skip = 0
  )

  expect_equal(nrow(resamples), 5)

  purrr::walk(resamples$splits, function(split) {
    analysis_time <- rsample::analysis(split)$time
    assessment_time <- rsample::assessment(split)$time
    expect_true(max(analysis_time) < min(assessment_time))
    expect_true(all(diff(analysis_time) >= 0))
    expect_true(all(diff(assessment_time) >= 0))
  })

  unsorted <- dat[c(2, 1, 3:10), , drop = FALSE]
  expect_error(
    fastml:::make_rolling_origin_cv(
      unsorted,
      block_col = "time",
      initial_window = 4,
      assess_window = 2,
      skip = 0
    ),
    "sorted",
    ignore.case = TRUE
  )
})

test_that("nested_cv validates fold counts and supports grouped splits", {
  skip_if_not_installed("rsample")

  expect_error(
    fastml:::make_nested_cv(tibble::tibble(x = 1:5, y = 1:5), outer_folds = 1, inner_folds = 2),
    "outer_folds",
    ignore.case = TRUE
  )
  expect_error(
    fastml:::make_nested_cv(tibble::tibble(x = 1:5, y = 1:5), outer_folds = 2, inner_folds = 1),
    "inner_folds",
    ignore.case = TRUE
  )

  set.seed(4)
  dat <- tibble::tibble(
    id = seq_len(24),
    group = rep(paste0("g", 1:8), each = 3),
    x = rnorm(24),
    y = rnorm(24)
  )

  nested_resamples <- fastml:::make_nested_cv(
    dat,
    outer_folds = 3,
    inner_folds = 2,
    group_cols = "group",
    strata = NULL
  )

  expect_s3_class(nested_resamples, "nested_cv")
  expect_equal(nrow(nested_resamples), 3)
  expect_equal(length(nested_resamples$inner_resamples), 3)

  purrr::walk2(
    nested_resamples$splits,
    nested_resamples$inner_resamples,
    function(outer_split, inner_set) {
      outer_analysis <- rsample::analysis(outer_split)
      outer_assessment <- rsample::assessment(outer_split)

      expect_length(
        intersect(unique(outer_analysis$group), unique(outer_assessment$group)),
        0
      )

      purrr::walk(inner_set$splits, function(inner_split) {
        inner_analysis <- rsample::analysis(inner_split)
        inner_assessment <- rsample::assessment(inner_split)

        expect_true(all(inner_analysis$id %in% outer_analysis$id))
        expect_true(all(inner_assessment$id %in% outer_analysis$id))
        expect_length(
          intersect(unique(inner_analysis$group), unique(inner_assessment$group)),
          0
        )
        expect_length(
          intersect(unique(inner_assessment$group), unique(outer_assessment$group)),
          0
        )
      })
    }
  )
})

