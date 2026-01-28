library(testthat)
library(dplyr)

# small dataset for tests
set.seed(123)
data(iris)
iris_binary <- iris[iris$Species != "setosa", ]
iris_binary$Species <- factor(iris_binary$Species)

test_that("plot.fastml errors with non-fastml object", {
  expect_error(
    plot.fastml(list(a = 1)),
    "must be a 'fastml' object"
  )
})

test_that("plot.fastml type='bar' works for classification", {
  skip_on_cran()

  model <- suppressWarnings(fastml(
    data = iris_binary,
    label = "Species",
    algorithms = "logistic_reg",
    resampling_method = "none",
    use_default_tuning = FALSE
  ))

  # Use pdf device to capture plot
  grDevices::pdf(file = NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_error(suppressWarnings(plot(model, type = "bar")), NA)
})

test_that("plot.fastml type='bar' works for regression", {
  skip_on_cran()

  data(mtcars)
  model <- suppressWarnings(fastml(
    data = mtcars,
    label = "mpg",
    algorithms = "linear_reg",
    resampling_method = "none",
    use_default_tuning = FALSE
  ))

  grDevices::pdf(file = NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_error(suppressWarnings(plot(model, type = "bar")), NA)
})

test_that("plot.fastml type='roc' works for binary classification", {
  skip_on_cran()

  model <- suppressWarnings(fastml(
    data = iris_binary,
    label = "Species",
    algorithms = "logistic_reg",
    resampling_method = "none",
    use_default_tuning = FALSE
  ))

  grDevices::pdf(file = NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_error(suppressWarnings(plot(model, type = "roc")), NA)
})

test_that("plot.fastml type='calibration' works for classification", {
  skip_on_cran()
  skip_if_not_installed("probably")

  model <- suppressWarnings(fastml(
    data = iris_binary,
    label = "Species",
    algorithms = "logistic_reg",
    resampling_method = "none",
    use_default_tuning = FALSE
  ))

  grDevices::pdf(file = NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_error(suppressWarnings(plot(model, type = "calibration")), NA)
})

test_that("plot.fastml type='residual' works for regression", {
  skip_on_cran()

  data(mtcars)
  model <- suppressWarnings(fastml(
    data = mtcars,
    label = "mpg",
    algorithms = "linear_reg",
    resampling_method = "none",
    use_default_tuning = FALSE
  ))

  grDevices::pdf(file = NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  output <- capture.output(suppressWarnings(plot(model, type = "residual")))
  expect_true(TRUE)  # If we get here without error, test passes
})

test_that("plot.fastml type='all' generates multiple plots", {
  skip_on_cran()

  model <- suppressWarnings(fastml(
    data = iris_binary,
    label = "Species",
    algorithms = "logistic_reg",
    resampling_method = "none",
    use_default_tuning = FALSE
  ))

  grDevices::pdf(file = NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_error(suppressWarnings(plot(model, type = "all")), NA)
})

test_that("plot.fastml returns invisible object", {
  skip_on_cran()

  model <- suppressWarnings(fastml(
    data = iris_binary,
    label = "Species",
    algorithms = "logistic_reg",
    resampling_method = "none",
    use_default_tuning = FALSE
  ))

  grDevices::pdf(file = NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  result <- suppressWarnings(plot(model, type = "bar"))
  expect_s3_class(result, "fastml")
})

test_that("plot.fastml handles multiple algorithms", {
  skip_on_cran()

  model <- suppressWarnings(fastml(
    data = iris_binary,
    label = "Species",
    algorithms = c("logistic_reg", "rand_forest"),
    resampling_method = "none",
    use_default_tuning = FALSE
  ))

  grDevices::pdf(file = NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_error(suppressWarnings(plot(model, type = "bar")), NA)
})

test_that("plot.fastml algorithm parameter filters ROC curves", {
  skip_on_cran()

  model <- suppressWarnings(fastml(
    data = iris_binary,
    label = "Species",
    algorithms = c("logistic_reg", "rand_forest"),
    resampling_method = "none",
    use_default_tuning = FALSE
  ))

  grDevices::pdf(file = NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_error(suppressWarnings(plot(model, type = "roc", algorithm = "logistic_reg")), NA)
})

test_that("plot.fastml prints message for residual on classification", {
  skip_on_cran()

  model <- suppressWarnings(fastml(
    data = iris_binary,
    label = "Species",
    algorithms = "logistic_reg",
    resampling_method = "none",
    use_default_tuning = FALSE
  ))

  grDevices::pdf(file = NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  output <- capture.output(suppressWarnings(plot(model, type = "residual")))
  expect_true(any(grepl("regression", output, ignore.case = TRUE)))
})

test_that("plot.fastml prints message for calibration on regression", {
  skip_on_cran()

  data(mtcars)
  model <- suppressWarnings(fastml(
    data = mtcars,
    label = "mpg",
    algorithms = "linear_reg",
    resampling_method = "none",
    use_default_tuning = FALSE
  ))

  grDevices::pdf(file = NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  output <- capture.output(suppressWarnings(plot(model, type = "calibration")))
  expect_true(any(grepl("classification", output, ignore.case = TRUE)))
})

test_that("plot.fastml handles learning_curve type gracefully", {
  skip_on_cran()

  model <- suppressWarnings(fastml(
    data = iris_binary,
    label = "Species",
    algorithms = "logistic_reg",
    resampling_method = "none",
    use_default_tuning = FALSE
  ))

  grDevices::pdf(file = NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  output <- capture.output(suppressWarnings(plot(model, type = "learning_curve")))
  expect_true(any(grepl("learning curve", output, ignore.case = TRUE)))
})
