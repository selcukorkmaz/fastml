library(testthat)
library(dplyr)

skip_if_not_installed("iml")

# small dataset for tests
set.seed(123)
data(iris)
iris_binary <- iris[iris$Species != "setosa", ]
iris_binary$Species <- factor(iris_binary$Species)

test_that("interaction_strength errors with non-fastml object", {
  expect_error(
    interaction_strength(list(a = 1)),
    "must be a 'fastml' object"
  )
})

test_that("interaction_strength runs on classification model", {
  skip_on_cran()

  model <- fastml(
    data = iris_binary,
    label = "Species",
    algorithms = "rand_forest",
    resampling_method = "cv",
    folds = 2
  )

  expect_error(
    suppressWarnings(suppressMessages(interaction_strength(model))),
    regexp = NA
  )
})

test_that("interaction_strength runs on regression model", {
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

  result <- suppressWarnings(suppressMessages(interaction_strength(model)))
  expect_true(inherits(result, "Interaction"))
})
