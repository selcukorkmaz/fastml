library(testthat)
library(dplyr)

# small dataset for tests
set.seed(123)
data(iris)
iris_binary <- iris[iris$Species != "setosa", ]
iris_binary$Species <- factor(iris_binary$Species)

test_that("predict.fastml errors when newdata is missing", {
  skip_on_cran()

  model <- fastml(
    data = iris_binary,
    label = "Species",
    algorithms = "logistic_reg",
    resampling_method = "none",
    use_default_tuning = FALSE
  )

  expect_error(
    predict(model),
    "Please provide newdata for prediction"
  )
})

test_that("predict.fastml returns class predictions for classification", {
  skip_on_cran()

  model <- fastml(
    data = iris_binary,
    label = "Species",
    algorithms = "logistic_reg",
    resampling_method = "none",
    use_default_tuning = FALSE
  )

  new_data <- iris_binary[1:10, ]
  new_data$Species <- NULL

  preds <- predict(model, newdata = new_data, type = "class")

  expect_true(is.factor(preds) || is.character(preds))
  expect_equal(length(preds), 10)
})

test_that("predict.fastml returns probabilities for classification", {
  skip_on_cran()

  model <- fastml(
    data = iris_binary,
    label = "Species",
    algorithms = "logistic_reg",
    resampling_method = "none",
    use_default_tuning = FALSE
  )

  new_data <- iris_binary[1:10, ]
  new_data$Species <- NULL

  preds <- predict(model, newdata = new_data, type = "prob")

  expect_true(is.data.frame(preds) || tibble::is_tibble(preds))
  expect_equal(nrow(preds), 10)
  expect_true(any(grepl("^\\.pred_", names(preds))))
})

test_that("predict.fastml returns numeric predictions for regression", {
  skip_on_cran()

  data(mtcars)
  model <- fastml(
    data = mtcars,
    label = "mpg",
    algorithms = "linear_reg",
    resampling_method = "none",
    use_default_tuning = FALSE
  )

  new_data <- mtcars[1:5, ]
  new_data$mpg <- NULL

  preds <- predict(model, newdata = new_data, type = "numeric")

  expect_true(is.numeric(preds))
  expect_equal(length(preds), 5)
})

test_that("predict.fastml auto type selects appropriately for classification", {
  skip_on_cran()

  model <- fastml(
    data = iris_binary,
    label = "Species",
    algorithms = "logistic_reg",
    resampling_method = "none",
    use_default_tuning = FALSE
  )

  new_data <- iris_binary[1:5, ]
  new_data$Species <- NULL

  preds <- predict(model, newdata = new_data, type = "auto")

  expect_true(is.factor(preds) || is.character(preds))
})

test_that("predict.fastml auto type selects appropriately for regression", {
  skip_on_cran()

  data(mtcars)
  model <- fastml(
    data = mtcars,
    label = "mpg",
    algorithms = "linear_reg",
    resampling_method = "none",
    use_default_tuning = FALSE
  )

  new_data <- mtcars[1:5, ]
  new_data$mpg <- NULL

  preds <- predict(model, newdata = new_data, type = "auto")

  expect_true(is.numeric(preds))
})

test_that("predict.fastml applies postprocess_fn", {
  skip_on_cran()

  data(mtcars)
  model <- fastml(
    data = mtcars,
    label = "mpg",
    algorithms = "linear_reg",
    resampling_method = "none",
    use_default_tuning = FALSE
  )

  new_data <- mtcars[1:5, ]
  new_data$mpg <- NULL

  # Postprocess function that rounds predictions
  postprocess <- function(x) round(x)

  preds <- predict(model, newdata = new_data, postprocess_fn = postprocess)

  expect_true(all(preds == round(preds)))
})

test_that("predict.fastml handles model_name parameter", {
  skip_on_cran()

  model <- fastml(
    data = iris_binary,
    label = "Species",
    algorithms = c("logistic_reg", "rand_forest"),
    resampling_method = "none",
    use_default_tuning = FALSE
  )

  new_data <- iris_binary[1:5, ]
  new_data$Species <- NULL

  # Predict using specific model
  preds <- predict(model, newdata = new_data, model_name = "logistic_reg")

  expect_true(is.factor(preds) || is.character(preds))
  expect_equal(length(preds), 5)
})

test_that("predict.fastml errors on invalid model_name", {
  skip_on_cran()

  model <- fastml(
    data = iris_binary,
    label = "Species",
    algorithms = "logistic_reg",
    resampling_method = "none",
    use_default_tuning = FALSE
  )

  new_data <- iris_binary[1:5, ]
  new_data$Species <- NULL

  expect_error(
    predict(model, newdata = new_data, model_name = "nonexistent_model"),
    "not found"
  )
})

test_that("predict.fastml verbose prints messages", {
  skip_on_cran()

  model <- fastml(
    data = iris_binary,
    label = "Species",
    algorithms = "logistic_reg",
    resampling_method = "none",
    use_default_tuning = FALSE
  )

  new_data <- iris_binary[1:5, ]
  new_data$Species <- NULL

  expect_message(
    predict(model, newdata = new_data, verbose = TRUE),
    "Predicting with"
  )
})

test_that("predict.fastml warns when requesting prob for regression", {
  skip_on_cran()

  data(mtcars)
  model <- fastml(
    data = mtcars,
    label = "mpg",
    algorithms = "linear_reg",
    resampling_method = "none",
    use_default_tuning = FALSE
  )

  new_data <- mtcars[1:5, ]
  new_data$mpg <- NULL

  expect_warning(
    predict(model, newdata = new_data, type = "prob"),
    "Probabilities only for classification"
  )
})

test_that("predict.fastml handles integer columns in newdata", {
  skip_on_cran()

  # Create dataset with integer columns
  df <- data.frame(
    x1 = as.integer(1:50),
    x2 = as.integer(50:1),
    y = factor(rep(c("A", "B"), 25))
  )

  model <- fastml(
    data = df,
    label = "y",
    algorithms = "logistic_reg",
    resampling_method = "none",
    use_default_tuning = FALSE
  )

  new_data <- data.frame(
    x1 = as.integer(1:5),
    x2 = as.integer(5:1)
  )

  preds <- predict(model, newdata = new_data)

  expect_equal(length(preds), 5)
})
