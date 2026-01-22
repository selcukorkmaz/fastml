library(testthat)
library(dplyr)

# ========================
# Tests for availableMethods
# ========================

test_that("availableMethods returns valid methods for classification", {
  methods <- availableMethods(type = "classification")

  expect_true(is.character(methods))
  expect_true(length(methods) > 0)
  expect_true("logistic_reg" %in% methods)
  expect_true("rand_forest" %in% methods)
})

test_that("availableMethods returns valid methods for regression", {
  methods <- availableMethods(type = "regression")

  expect_true(is.character(methods))
  expect_true(length(methods) > 0)
  expect_true("linear_reg" %in% methods)
  expect_true("rand_forest" %in% methods)
})

test_that("availableMethods returns valid methods for survival", {
  methods <- availableMethods(type = "survival")

  expect_true(is.character(methods))
  expect_true(length(methods) > 0)
})

# ========================
# Tests for get_default_engine
# ========================

test_that("get_default_engine returns correct engine for rand_forest", {
  engine <- get_default_engine("rand_forest")
  expect_equal(engine, "ranger")
})

test_that("get_default_engine returns correct engine for logistic_reg", {
  engine <- get_default_engine("logistic_reg")
  expect_equal(engine, "glm")
})

test_that("get_default_engine returns correct engine for linear_reg", {
  engine <- get_default_engine("linear_reg")
  expect_equal(engine, "lm")
})

test_that("get_default_engine returns correct engine for boost_tree", {
  engine <- get_default_engine("boost_tree")
  expect_equal(engine, "xgboost")
})

# ========================
# Tests for get_default_params
# ========================

test_that("get_default_params returns list for rand_forest", {
  params <- get_default_params("rand_forest", "classification")

  expect_true(is.list(params))
})

test_that("get_default_params returns list for linear_reg", {
  params <- get_default_params("linear_reg", "regression")

  expect_true(is.list(params))
})

# ========================
# Tests for get_best_model_idx
# ========================

test_that("get_best_model_idx returns correct index for higher-better metric", {
  df <- data.frame(
    Model = c("A", "B", "C"),
    Engine = c("e1", "e1", "e1"),
    accuracy = c(0.8, 0.9, 0.7)
  )

  idx <- get_best_model_idx(df, "accuracy")
  expect_equal(idx, 2)  # B has highest accuracy
})

test_that("get_best_model_idx returns correct index for lower-better metric", {
  df <- data.frame(
    Model = c("A", "B", "C"),
    Engine = c("e1", "e1", "e1"),
    rmse = c(0.5, 0.3, 0.8)
  )

  idx <- get_best_model_idx(df, "rmse")
  expect_equal(idx, 2)  # B has lowest rmse
})

test_that("get_best_model_idx handles NA values", {
  df <- data.frame(
    Model = c("A", "B", "C"),
    Engine = c("e1", "e1", "e1"),
    accuracy = c(0.8, NA, 0.7)
  )

  idx <- get_best_model_idx(df, "accuracy")
  expect_equal(idx, 1)  # A has highest non-NA accuracy
})

# ========================
# Tests for get_best_model_names
# ========================

test_that("get_best_model_names returns named character vector", {
  skip_on_cran()

  set.seed(123)
  data(iris)
  iris_binary <- iris[iris$Species != "setosa", ]
  iris_binary$Species <- factor(iris_binary$Species)

  model <- fastml(
    data = iris_binary,
    label = "Species",
    algorithms = c("logistic_reg", "rand_forest"),
    resampling_method = "none",
    use_default_tuning = FALSE
  )

  names <- get_best_model_names(model$models)

  expect_true(is.character(names))
  expect_true(length(names) > 0)
})

# ========================
# Tests for get_best_workflows
# ========================

test_that("get_best_workflows returns list of workflows", {
  skip_on_cran()

  set.seed(123)
  data(iris)
  iris_binary <- iris[iris$Species != "setosa", ]
  iris_binary$Species <- factor(iris_binary$Species)

  model <- fastml(
    data = iris_binary,
    label = "Species",
    algorithms = c("logistic_reg", "rand_forest"),
    resampling_method = "none",
    use_default_tuning = FALSE
  )

  workflows <- get_best_workflows(model$models, model$best_model_name)

  expect_true(is.list(workflows))
  expect_true(length(workflows) > 0)
})

# ========================
# Tests for flatten_and_rename_models
# ========================

test_that("flatten_and_rename_models works on fastml models", {
  skip_on_cran()

  set.seed(123)
  data(iris)
  iris_binary <- iris[iris$Species != "setosa", ]
  iris_binary$Species <- factor(iris_binary$Species)

  model <- fastml(
    data = iris_binary,
    label = "Species",
    algorithms = c("logistic_reg", "rand_forest"),
    resampling_method = "none",
    use_default_tuning = FALSE
  )

  flattened <- flatten_and_rename_models(model$models)

  expect_true(is.list(flattened))
  expect_true(length(flattened) > 0)
})

# ========================
# Tests for get_engine_names
# ========================

test_that("get_engine_names extracts engine names from models", {
  skip_on_cran()

  set.seed(123)
  data(iris)
  iris_binary <- iris[iris$Species != "setosa", ]
  iris_binary$Species <- factor(iris_binary$Species)

  model <- fastml(
    data = iris_binary,
    label = "Species",
    algorithms = c("logistic_reg", "rand_forest"),
    resampling_method = "none",
    use_default_tuning = FALSE
  )

  engines <- get_engine_names(model$models)

  expect_true(is.list(engines) || is.character(engines))
  expect_true(length(engines) > 0)
})

# ========================
# Tests for get_tuning_complexity
# ========================

test_that("get_tuning_complexity returns valid config for quick", {
  config <- get_tuning_complexity("quick")

  expect_true(is.list(config))
  expect_true("grid_size" %in% names(config))
  expect_true("folds" %in% names(config))
})

test_that("get_tuning_complexity returns valid config for balanced", {
  config <- get_tuning_complexity("balanced")

  expect_true(is.list(config))
  expect_true(config$grid_size > get_tuning_complexity("quick")$grid_size)
})

test_that("get_tuning_complexity returns valid config for thorough", {
  config <- get_tuning_complexity("thorough")

  expect_true(is.list(config))
  expect_true(config$grid_size > get_tuning_complexity("balanced")$grid_size)
})

test_that("get_tuning_complexity returns valid config for exhaustive", {
  config <- get_tuning_complexity("exhaustive")

  expect_true(is.list(config))
  expect_true(config$grid_size > get_tuning_complexity("thorough")$grid_size)
})

# ========================
# Tests for estimate_tuning_time
# ========================

test_that("estimate_tuning_time returns numeric estimate", {
  estimate <- estimate_tuning_time(n_params = 10, n_folds = 5, n_rows = 1000)

  expect_true(is.numeric(estimate))
  expect_true(estimate > 0)
})

test_that("estimate_tuning_time increases with more params", {
  est1 <- estimate_tuning_time(n_params = 10)
  est2 <- estimate_tuning_time(n_params = 50)

  expect_true(est2 > est1)
})

# ========================
# Tests for print_tuning_presets
# ========================

test_that("print_tuning_presets runs without error", {
  output <- capture.output(print_tuning_presets())

  expect_true(length(output) > 0)
  expect_true(any(grepl("quick|balanced|thorough|exhaustive", output, ignore.case = TRUE)))
})

# ========================
# Tests for validate_defaults_registry
# ========================

test_that("validate_defaults_registry returns list", {
  skip_if_not_installed("parsnip")

  result <- validate_defaults_registry()

  expect_true(is.list(result))
})

# ========================
# Tests for get_default_differences
# ========================

test_that("get_default_differences returns list", {
  result <- get_default_differences(c("rand_forest", "logistic_reg"), task = "classification")

  expect_true(is.list(result))
})

# ========================
# Tests for compare_defaults
# ========================

test_that("compare_defaults identifies matching engines", {
  comparison <- compare_defaults(
    algo = "rand_forest",
    task = "classification",
    fastml_engine = "ranger"
  )

  expect_false(comparison$engine_differs)
})

test_that("compare_defaults identifies differing engines", {
  comparison <- compare_defaults(
    algo = "boost_tree",
    task = "classification",
    fastml_engine = "lightgbm"
  )

  expect_true(comparison$engine_differs)
})
