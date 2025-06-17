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

test_that("evaluate_models works with a single workflow", {
  rec <- recipes::recipe(Species ~ ., data = iris)
  spec <- parsnip::logistic_reg() %>% parsnip::set_engine("glm")
  wf <- workflows::workflow() %>%
    workflows::add_model(spec) %>%
    workflows::add_recipe(rec)
  fitted_wf <- parsnip::fit(wf, data = iris)
  models <- list(log_reg = fitted_wf)
  eval_res <- evaluate_models(models, iris, iris,
                              label = "Species", task = "classification",
                              metric = "accuracy", event_class = "second")
  expect_true("log_reg" %in% names(eval_res$performance))
})

test_that("process_model works without global variables", {
  rec <- recipes::recipe(Species ~ ., data = iris)
  spec <- parsnip::logistic_reg() %>% parsnip::set_engine("glm")
  wf <- workflows::workflow() %>%
    workflows::add_model(spec) %>%
    workflows::add_recipe(rec)
  fitted_wf <- parsnip::fit(wf, data = iris)

  res <- process_model(fitted_wf, model_id = "log_reg", task = "classification",
                       test_data = iris, label = "Species",
                       event_class = "second", engine = "glm",
                       train_data = iris, metric = "accuracy")
  expect_s3_class(res$performance, "tbl_df")
  expect_equal(nrow(res$predictions), nrow(iris))
})

test_that("regression model successful.", {
  res <- fastml(
    data = iris[, -5],
    label = "Sepal.Length",
    algorithms = c("linear_reg")
  )
  expect_s3_class(res, "fastml")
})



test_that("multicore tasks successful.", {
  res <- fastml(
    data = iris[, -5],
    label = "Sepal.Length",
    algorithms = c("linear_reg"),
    n_cores = 2
  )
  expect_s3_class(res, "fastml")
})

test_that("stop if unsupported metric is selected.", {
  expect_error({
    fastml(
      data = iris[,-5],
      label = "Sepal.Length",
      algorithms = c("linear_reg"),
      n_cores = 2,
      metric = "unkown"
    )
  })

  expect_no_error({
    fastml(
      data = iris[,-5],
      label = "Sepal.Length",
      algorithms = c("linear_reg"),
      n_cores = 2,
      metric = "rmse"
    )
  })
})

test_that("invalid tuning_strategy triggers error", {
  expect_error(
    fastml(
      data = iris,
      label = "Species",
      algorithms = c("rand_forest"),
      tuning_strategy = "invalid"
    ),
    "should be one of"
  )
})

test_that("grid tuning executes successfully", {
  res <- fastml(
    data = iris,
    label = "Species",
    algorithms = c("rand_forest"),
    use_default_tuning = TRUE,
    tuning_strategy = "grid",
    resampling_method = "cv",
    folds = 2
  )
  expect_s3_class(res, "fastml")
  expect_true(length(res$models) > 0)
})

test_that("Bayesian tuning executes successfully", {
  res <- fastml(
    data = iris,
    label = "Species",
    algorithms = c("rand_forest"),
    use_default_tuning = TRUE,
    tuning_strategy = "bayes",
    tuning_iterations = 2,
    resampling_method = "cv",
    folds = 2
  )
  expect_s3_class(res, "fastml")
  expect_true(length(res$models) > 0)
})

