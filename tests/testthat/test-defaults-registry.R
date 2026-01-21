library(testthat)

test_that("validate_defaults_registry returns a list", {
  skip_if_not_installed("parsnip")

  result <- validate_defaults_registry()

  expect_type(result, "list")
})

test_that("validate_defaults_registry mismatches have correct structure", {
  skip_if_not_installed("parsnip")

  result <- validate_defaults_registry()

  # If there are mismatches, verify their structure

  if (length(result) > 0) {
    for (mismatch in result) {
      expect_true("algorithm" %in% names(mismatch))
      expect_true("fastml_default" %in% names(mismatch))
      expect_true("parsnip_default" %in% names(mismatch))
      expect_type(mismatch$algorithm, "character")
      expect_type(mismatch$fastml_default, "character")
      expect_type(mismatch$parsnip_default, "character")
    }
  }
})

test_that("get_parsnip_default_engine returns expected values for known algorithms", {
  # Test a few known defaults that are unlikely to change
  expect_equal(get_parsnip_default_engine("logistic_reg"), "glm")
  expect_equal(get_parsnip_default_engine("linear_reg"), "lm")
  expect_equal(get_parsnip_default_engine("decision_tree"), "rpart")
  expect_equal(get_parsnip_default_engine("rand_forest"), "ranger")

  # Unknown algorithms should return NULL
  expect_null(get_parsnip_default_engine("nonexistent_algorithm"))
})

test_that("compare_defaults detects engine differences", {
  # Test with a known difference (fastml uses xgboost for boost_tree,
  # and parsnip also uses xgboost)
  comparison <- compare_defaults(
    algo = "boost_tree",
    task = "classification",
    fastml_engine = "lightgbm"  # hypothetical different engine
  )

  expect_true(comparison$engine_differs)
  expect_equal(comparison$fastml_engine, "lightgbm")
  expect_equal(comparison$parsnip_engine, "xgboost")
})

test_that("compare_defaults returns no difference when engines match", {
  comparison <- compare_defaults(
    algo = "rand_forest",
    task = "classification",
    fastml_engine = "ranger"
  )

  expect_false(comparison$engine_differs)
  expect_equal(comparison$fastml_engine, "ranger")
  expect_equal(comparison$parsnip_engine, "ranger")
})
