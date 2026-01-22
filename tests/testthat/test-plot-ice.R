library(testthat)
library(dplyr)

skip_if_not_installed("pdp")

# small dataset for tests
set.seed(123)
data(iris)
iris_binary <- iris[iris$Species != "setosa", ]
iris_binary$Species <- factor(iris_binary$Species)

test_that("plot_ice errors with non-fastml object", {
  expect_error(
    plot_ice(list(a = 1), features = "Sepal.Length"),
    "must be a 'fastml' object"
  )
})

test_that("plot_ice errors when features is missing", {
  skip_on_cran()

  model <- fastml(
    data = iris_binary,
    label = "Species",
    algorithms = "rand_forest",
    resampling_method = "cv",
    folds = 2
  )

  expect_error(
    plot_ice(model),
    "'features' must be specified"
  )
})

test_that("plot_ice runs on classification model", {
  skip_on_cran()

  model <- fastml(
    data = iris_binary,
    label = "Species",
    algorithms = "rand_forest",
    resampling_method = "cv",
    folds = 2
  )

  result <- suppressWarnings(suppressMessages(plot_ice(model, features = "Sepal.Length")))
  expect_true(is.list(result))
  expect_true("data" %in% names(result))
  expect_true("plot" %in% names(result))
  expect_true(inherits(result$plot, c("gg", "ggplot")))
})

test_that("plot_ice runs on regression model", {
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

  result <- suppressWarnings(suppressMessages(plot_ice(model, features = "hp")))
  expect_true(is.list(result))
  expect_true("data" %in% names(result))
  expect_true("plot" %in% names(result))
})

test_that("plot_ice works with multiple features", {
  skip_on_cran()

  model <- fastml(
    data = iris_binary,
    label = "Species",
    algorithms = "rand_forest",
    resampling_method = "cv",
    folds = 2
  )

  result <- suppressWarnings(suppressMessages(
    plot_ice(model, features = c("Sepal.Length", "Sepal.Width"))
  ))
  expect_true(is.list(result))
})
