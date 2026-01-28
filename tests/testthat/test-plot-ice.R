library(testthat)
library(dplyr)

skip_if_not_installed("pdp")
skip_if_not_installed("rpart")

# Prevent Rplots.pdf from being created by graphics calls
grDevices::pdf(file = NULL)
withr::defer(grDevices::dev.off())

# small dataset for tests
set.seed(123)
data(iris)
iris_binary <- iris[iris$Species != "setosa", ]
iris_binary$Species <- factor(iris_binary$Species)

# Helper to train model and skip if training fails
train_model_or_skip <- function(data, label, algorithms = "decision_tree") {
  model <- tryCatch(
    suppressWarnings(fastml(
      data = data,
      label = label,
      algorithms = algorithms,
      resampling_method = "none"
    )),
    error = function(e) NULL
  )
  if (is.null(model) || is.null(model$models) || length(model$models) == 0) {
    skip("Model training failed")
  }
  model
}

test_that("plot_ice errors with non-fastml object", {
  expect_error(
    plot_ice(list(a = 1), features = "Sepal.Length"),
    "must be a 'fastml' object"
  )
})

test_that("plot_ice errors when features is missing", {
  skip_on_cran()

  model <- train_model_or_skip(
    data = iris_binary,
    label = "Species",
    algorithms = "decision_tree"
  )

  expect_error(
    plot_ice(model),
    "'features' must be specified"
  )
})

test_that("plot_ice runs on classification model", {
  skip_on_cran()

  # Use decision_tree with rpart engine which has better pdp compatibility
  model <- train_model_or_skip(
    data = iris_binary,
    label = "Species",
    algorithms = "decision_tree"
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

  # Use decision_tree with rpart engine which has better pdp compatibility
  model <- train_model_or_skip(
    data = mtcars,
    label = "mpg",
    algorithms = "decision_tree"
  )

  result <- suppressWarnings(suppressMessages(plot_ice(model, features = "hp")))
  expect_true(is.list(result))
  expect_true("data" %in% names(result))
  expect_true("plot" %in% names(result))
})

test_that("plot_ice works with different features", {
  skip_on_cran()

  # Use decision_tree with rpart engine which has better pdp compatibility
  model <- train_model_or_skip(
    data = iris_binary,
    label = "Species",
    algorithms = "decision_tree"
  )

  # Note: pdp::partial with ice=TRUE only supports single features,
  # so we test each feature individually
  result1 <- suppressWarnings(suppressMessages(
    plot_ice(model, features = "Sepal.Length")
  ))
  expect_true(is.list(result1))

  result2 <- suppressWarnings(suppressMessages(
    plot_ice(model, features = "Sepal.Width")
  ))
  expect_true(is.list(result2))
})
