library(testthat)

test_that("bootstrap coverage does not trigger the full-data guard", {
  in_id <- seq_len(10)
  expect_true(
    fastml:::fastml_guard_detect_full_analysis(in_id, 10, is_bootstrap = FALSE)
  )
  expect_false(
    fastml:::fastml_guard_detect_full_analysis(in_id, 10, is_bootstrap = TRUE)
  )
})

test_that("guarded resampling errors when split indices are missing", {
  skip_if_not_installed("rsample")

  set.seed(101)
  binary_iris <- iris[iris$Species != "virginica", ]
  binary_iris$Species <- factor(binary_iris$Species)

  idx <- sample(seq_len(nrow(binary_iris)), size = floor(0.7 * nrow(binary_iris)))
  train_split <- binary_iris[idx, , drop = FALSE]
  test_split <- binary_iris[-idx, , drop = FALSE]

  resamples <- rsample::vfold_cv(train_split, v = 3)
  resamples$splits[[1]]$in_id <- NULL

  expect_error(
    fastml(
      train_data = train_split,
      test_data = test_split,
      label = "Species",
      algorithms = "logistic_reg",
      resamples = resamples,
      resampling_method = "cv",
      folds = 3,
      seed = 202
    ),
    "split index information",
    fixed = TRUE
  )
})

test_that("guarded resampling requires consistent index fields", {
  skip_if_not_installed("rsample")

  set.seed(202)
  binary_iris <- iris[iris$Species != "virginica", ]
  binary_iris$Species <- factor(binary_iris$Species)

  idx <- sample(seq_len(nrow(binary_iris)), size = floor(0.7 * nrow(binary_iris)))
  train_split <- binary_iris[idx, , drop = FALSE]
  test_split <- binary_iris[-idx, , drop = FALSE]

  resamples <- rsample::vfold_cv(train_split, v = 3)
  resamples$splits[[1]]$analysis_id <- resamples$splits[[1]]$in_id
  resamples$splits[[1]]$in_id <- NULL

  expect_error(
    fastml(
      train_data = train_split,
      test_data = test_split,
      label = "Species",
      algorithms = "logistic_reg",
      resamples = resamples,
      resampling_method = "cv",
      folds = 3,
      seed = 303
    ),
    "consistent index field",
    fixed = TRUE
  )
})
