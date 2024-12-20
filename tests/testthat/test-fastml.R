library(dplyr)

data(iris)
iris <- iris[iris$Species != "setosa", ]  # Binary classification
iris$Species <- factor(iris$Species)


test_that("'label' is not available in the data", {
  expect_error({
    # Train models with Bayesian optimization
    model <- fastml(
      data = iris,
      label = "Unknown",
      algorithms = c("random_forest")
    )
  })
})

test_that("special characters in column names are removed", {
  tmp <- iris %>%
    rename(
      "Sepal.Length*[/" = Sepal.Length
    )

  # Train models with Bayesian optimization
  expect_true({
    model <- fastml(
      data = tmp,
      label = "Species",
      algorithms = c("random_forest")
    )

    "sepal_length" %in% colnames(model$processed_train_data)
  })
})


test_that("model fails if reponse variable is not of supported type", {
  tmp <- iris %>%
    mutate(
      Date = as.Date("2024-12-19")
    )

  expect_error({
    # Train models with Bayesian optimization
    model <- fastml(
      data = iris,
      label = "Date",
      algorithms = c("random_forest")
    )
  })
})


test_that("stop if requested metric is not allowed.", {
  expect_error({
    # Train models with Bayesian optimization
    model <- fastml(
      data = iris,
      label = "Species",
      algorithms = c("random_forest"),
      metric = "unknown"
    )
  })

  expect_error({
    # Example 2: Using the mtcars dataset for regression
    # Train models
    model <- fastml(
      data = mtcars,
      label = "mpg",
      algorithms = c("random_forest"),
      metric = "unknown"
    )
  })
})


# Last line checked: 180 (fastml.R), will continue from this point forward.
