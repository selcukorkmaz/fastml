library(dplyr)
library(testthat)

data(iris)
iris <- iris[iris$Species != "setosa", ]  # Binary classification
iris$Species <- factor(iris$Species)


test_that("'label' is not available in the data", {
  expect_error({
    # Train models with Bayesian optimization
    model <- fastml(
      data = iris,
      label = "Unknown",
      algorithms = c("rand_forest")
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
      algorithms = c("rand_forest")
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
      algorithms = c("rand_forest")
    )
  })
})


test_that("stop if requested metric is not allowed.", {
  expect_error({
    # Train models with Bayesian optimization
    model <- fastml(
      data = iris,
      label = "Species",
      algorithms = c("rand_forest"),
      metric = "unknown"
    )
  })

  expect_error({
    # Example 2: Using the mtcars dataset for regression
    # Train models
    model <- fastml(
      data = mtcars,
      label = "mpg",
      algorithms = c("rand_forest"),
      metric = "unknown"
    )
  })
})

test_that("check for supported algorithms", {
  expect_warning({
    # Train models with Bayesian optimization
    fastml(
      data = iris,
      label = "Species",
      algorithms = c("rand_forest", "unknown")
    )
  })

  expect_error({
    # Train models with Bayesian optimization
    fastml(
      data = iris,
      label = "Species",
      algorithms = c("unknown")
    )
  })
})

test_that("variables successfuly excluded", {
  expect_error({
    fastml(
      data = iris,
      label = "Species",
      algorithms = c("rand_forest"),
      exclude = "Species"
    )
  })

  expect_warning({
    fastml(
      data = iris,
      label = "Species",
      algorithms = c("rand_forest"),
      exclude = c("Sepal.Length", "unknown")
    )
  })
})

test_that("checks for impute_method", {
  expect_warning(
    fastml(
      data = iris,
      label = "Species",
      algorithms = c("rand_forest"),
      impute_method = "medianImpute"
    ),
    "Missing values in numeric predictors are being imputed using the median."
  )

  expect_warning(
    fastml(
      data = iris,
      label = "Species",
      algorithms = c("rand_forest"),
      impute_method = "knnImpute"
    ),
    "Missing values are being imputed using KNN"
  )

  expect_warning(
    fastml(
      data = iris,
      label = "Species",
      algorithms = c("rand_forest"),
      impute_method = "bagImpute"
    ),
    "Missing values are being imputed using bagging"
  )

  expect_warning(
    fastml(
      data = iris,
      label = "Species",
      algorithms = c("rand_forest"),
      impute_method = "remove"
    ),
    "Rows with missing values in predictors are being removed."
  )

  expect_error(
    fastml(
      data = iris,
      label = "Species",
      algorithms = c("rand_forest"),
      impute_method = "unknown"
    ),
    "Invalid impute_method specified."
  )
})

test_that("stop if recipe is not correctly specified.", {
  expect_error({
    fastml(
      data = iris,
      label = "Species",
      algorithms = c("rand_forest"),
      recipe = "unknown"
    )
  })
})

# test_that("regression model successful.", {
#   expect_no_error({
#     fastml(
#       data = iris[,-5],
#       label = "Sepal.Length",
#       algorithms = c("linear_reg")
#     )
#   })
# })

# test_that("multicore tasks successful.", {
#   expect_no_error({
#     fastml(
#       data = iris[,-5],
#       label = "Sepal.Length",
#       algorithms = c("linear_reg"),
#       n_cores = 2
#     )
#   })
# })

# test_that("stop if unsupported metric is selected.", {
#   expect_error({
#     fastml(
#       data = iris[,-5],
#       label = "Sepal.Length",
#       algorithms = c("linear_reg"),
#       n_cores = 2,
#       metric = "unkown"
#     )
#   })
#
#   expect_no_error({
#     fastml(
#       data = iris[,-5],
#       label = "Sepal.Length",
#       algorithms = c("linear_reg"),
#       n_cores = 2,
#       metric = "rmse"
#     )
#   })
# })

