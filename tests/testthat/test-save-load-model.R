library(testthat)
library(dplyr)

# small dataset for tests
set.seed(123)
data(iris)
iris_binary <- iris[iris$Species != "setosa", ]
iris_binary$Species <- factor(iris_binary$Species)

test_that("save.fastml and load_model work correctly for classification", {
  skip_on_cran()

  model <- fastml(
    data = iris_binary,
    label = "Species",
    algorithms = "rand_forest",
    resampling_method = "cv",
    folds = 2
  )

  temp_file <- tempfile(fileext = ".rds")
  on.exit(unlink(temp_file), add = TRUE)

  # Test save
  expect_error(save.fastml(model, temp_file), regexp = NA)
  expect_true(file.exists(temp_file))

  # Test load
  loaded_model <- load_model(temp_file)
  expect_true(inherits(loaded_model, "fastml"))
  expect_equal(loaded_model$label, model$label)
  expect_equal(loaded_model$task, model$task)
})

test_that("save.fastml and load_model work correctly for regression", {
  skip_on_cran()

  set.seed(42)
  data(mtcars)

  model <- fastml(
    data = mtcars,
    label = "mpg",
    algorithms = "rand_forest",
    resampling_method = "cv",
    folds = 2
  )

  temp_file <- tempfile(fileext = ".rds")
  on.exit(unlink(temp_file), add = TRUE)

  save.fastml(model, temp_file)
  loaded_model <- load_model(temp_file)

  expect_true(inherits(loaded_model, "fastml"))
  expect_equal(loaded_model$label, "mpg")
  expect_equal(loaded_model$task, "regression")
})

test_that("loaded model can make predictions", {
  skip_on_cran()

  model <- fastml(
    data = iris_binary,
    label = "Species",
    algorithms = "rand_forest",
    resampling_method = "cv",
    folds = 2
  )

  temp_file <- tempfile(fileext = ".rds")
  on.exit(unlink(temp_file), add = TRUE)

  save.fastml(model, temp_file)
  loaded_model <- load_model(temp_file)

  # Test that predictions work on loaded model
  new_data <- iris_binary[1:5, ]
  new_data$Species <- NULL

  preds_original <- suppressWarnings(predict(model, new_data))
  preds_loaded <- suppressWarnings(predict(loaded_model, new_data))

  expect_equal(length(preds_original), 5)
  expect_equal(length(preds_loaded), 5)
})

test_that("save.fastml overwrites existing file", {
  skip_on_cran()

  model1 <- fastml(
    data = iris_binary,
    label = "Species",
    algorithms = "rand_forest",
    resampling_method = "cv",
    folds = 2
  )

  set.seed(42)
  data(mtcars)
  model2 <- fastml(
    data = mtcars,
    label = "mpg",
    algorithms = "rand_forest",
    resampling_method = "cv",
    folds = 2
  )

  temp_file <- tempfile(fileext = ".rds")
  on.exit(unlink(temp_file), add = TRUE)

  save.fastml(model1, temp_file)
  save.fastml(model2, temp_file)

  loaded <- load_model(temp_file)
  expect_equal(loaded$label, "mpg")
})
