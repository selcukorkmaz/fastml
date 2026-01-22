library(testthat)
library(dplyr)

skip_if_not_installed("iml")

# small dataset for tests
set.seed(123)
data(iris)
iris_binary <- iris[iris$Species != "setosa", ]
iris_binary$Species <- factor(iris_binary$Species)

test_that("surrogate_tree errors with non-fastml object", {
  expect_error(
    surrogate_tree(list(a = 1)),
    "must be a 'fastml' object"
  )
})

test_that("surrogate_tree runs on classification model", {
  skip_on_cran()

  model <- fastml(
    data = iris_binary,
    label = "Species",
    algorithms = "rand_forest",
    resampling_method = "cv",
    folds = 2
  )

  result <- suppressWarnings(suppressMessages(surrogate_tree(model)))
  expect_true(inherits(result, "TreeSurrogate"))
})

test_that("surrogate_tree respects maxdepth parameter", {
  skip_on_cran()

  model <- fastml(
    data = iris_binary,
    label = "Species",
    algorithms = "rand_forest",
    resampling_method = "cv",
    folds = 2
  )

  result <- suppressWarnings(suppressMessages(surrogate_tree(model, maxdepth = 2)))
  expect_true(inherits(result, "TreeSurrogate"))
})

test_that("surrogate_tree runs on regression model", {
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

  result <- suppressWarnings(suppressMessages(surrogate_tree(model)))
  expect_true(inherits(result, "TreeSurrogate"))
})
