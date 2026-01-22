library(testthat)
library(dplyr)

# small dataset for tests
set.seed(123)
data(iris)
iris_binary <- iris[iris$Species != "setosa", ]
iris_binary$Species <- factor(iris_binary$Species)

test_that("summary.fastml errors with non-fastml object", {
  expect_error(
    summary.fastml(list(a = 1)),
    "must be a 'fastml' object"
  )
})

test_that("summary.fastml runs for classification without error", {
  skip_on_cran()

  model <- fastml(
    data = iris_binary,
    label = "Species",
    algorithms = "logistic_reg",
    resampling_method = "none",
    use_default_tuning = FALSE
  )

  output <- capture.output(summary(model))
  expect_true(length(output) > 0)
  expect_true(any(grepl("fastml Model Summary", output)))
  expect_true(any(grepl("classification", output, ignore.case = TRUE)))
})

test_that("summary.fastml runs for regression without error", {
  skip_on_cran()

  data(mtcars)
  model <- fastml(
    data = mtcars,
    label = "mpg",
    algorithms = "linear_reg",
    resampling_method = "none",
    use_default_tuning = FALSE
  )

  output <- capture.output(summary(model))
  expect_true(length(output) > 0)
  expect_true(any(grepl("fastml Model Summary", output)))
  expect_true(any(grepl("regression", output, ignore.case = TRUE)))
})

test_that("summary.fastml type='metrics' shows only metrics", {
  skip_on_cran()

  model <- fastml(
    data = iris_binary,
    label = "Species",
    algorithms = "logistic_reg",
    resampling_method = "none",
    use_default_tuning = FALSE
  )

  output <- capture.output(summary(model, type = "metrics"))
  expect_true(length(output) > 0)
  expect_true(any(grepl("Metric|Performance", output, ignore.case = TRUE)))
})

test_that("summary.fastml type='params' shows hyperparameters", {
  skip_on_cran()

  model <- fastml(
    data = iris_binary,
    label = "Species",
    algorithms = "logistic_reg",
    resampling_method = "none",
    use_default_tuning = FALSE
  )

  output <- capture.output(summary(model, type = "params"))
  expect_true(length(output) > 0)
  expect_true(any(grepl("hyperparameter|param", output, ignore.case = TRUE)))
})

test_that("summary.fastml type='conf_mat' shows confusion matrix", {
  skip_on_cran()

  model <- fastml(
    data = iris_binary,
    label = "Species",
    algorithms = "logistic_reg",
    resampling_method = "none",
    use_default_tuning = FALSE
  )

  output <- capture.output(summary(model, type = "conf_mat"))
  expect_true(length(output) > 0)
  expect_true(any(grepl("Confusion|Matrix", output, ignore.case = TRUE)))
})

test_that("summary.fastml type='all' shows all sections", {
  skip_on_cran()

  model <- fastml(
    data = iris_binary,
    label = "Species",
    algorithms = "logistic_reg",
    resampling_method = "none",
    use_default_tuning = FALSE
  )

  output <- capture.output(summary(model, type = "all"))
  expect_true(length(output) > 0)
  expect_true(any(grepl("Metric|Performance", output, ignore.case = TRUE)))
  expect_true(any(grepl("hyperparameter|param", output, ignore.case = TRUE)))
})

test_that("summary.fastml algorithm parameter filters models", {
  skip_on_cran()

  model <- fastml(
    data = iris_binary,
    label = "Species",
    algorithms = c("logistic_reg", "rand_forest"),
    resampling_method = "none",
    use_default_tuning = FALSE
  )

  output <- capture.output(summary(model, algorithm = "logistic_reg", type = "metrics"))
  expect_true(length(output) > 0)
  # Should mention logistic_reg in output
  expect_true(any(grepl("logistic", output, ignore.case = TRUE)))
})

test_that("summary.fastml errors on invalid algorithm", {
  skip_on_cran()

  model <- fastml(
    data = iris_binary,
    label = "Species",
    algorithms = "logistic_reg",
    resampling_method = "none",
    use_default_tuning = FALSE
  )

  expect_error(
    summary(model, algorithm = "nonexistent"),
    "algorithms entered correctly"
  )
})

test_that("summary.fastml returns invisible object", {
  skip_on_cran()

  model <- fastml(
    data = iris_binary,
    label = "Species",
    algorithms = "logistic_reg",
    resampling_method = "none",
    use_default_tuning = FALSE
  )

  result <- capture.output(res <- summary(model))
  expect_s3_class(res, "fastml")
})

test_that("summary.fastml handles multiple algorithms", {
  skip_on_cran()

  model <- fastml(
    data = iris_binary,
    label = "Species",
    algorithms = c("logistic_reg", "rand_forest"),
    resampling_method = "none",
    use_default_tuning = FALSE
  )

  output <- capture.output(summary(model))
  expect_true(any(grepl("logistic", output, ignore.case = TRUE)))
  expect_true(any(grepl("rand_forest|forest", output, ignore.case = TRUE)))
})

test_that("summary.fastml shows correct number of models", {
  skip_on_cran()

  model <- fastml(
    data = iris_binary,
    label = "Species",
    algorithms = c("logistic_reg", "rand_forest"),
    resampling_method = "none",
    use_default_tuning = FALSE
  )

  output <- capture.output(summary(model))
  expect_true(any(grepl("2", output)))  # 2 models
})

test_that("summary.fastml works with sort_metric parameter", {
  skip_on_cran()

  model <- fastml(
    data = iris_binary,
    label = "Species",
    algorithms = c("logistic_reg", "rand_forest"),
    resampling_method = "none",
    use_default_tuning = FALSE
  )

  output <- capture.output(summary(model, sort_metric = "accuracy"))
  expect_true(length(output) > 0)
})

test_that("summary.fastml errors on invalid sort_metric", {
  skip_on_cran()

  model <- fastml(
    data = iris_binary,
    label = "Species",
    algorithms = "logistic_reg",
    resampling_method = "none",
    use_default_tuning = FALSE
  )

  expect_error(
    summary(model, sort_metric = "nonexistent_metric"),
    "Invalid sort_metric"
  )
})
