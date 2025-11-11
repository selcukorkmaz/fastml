library(dplyr)
library(testthat)
library(survival)

data(iris)
iris <- iris[iris$Species != "setosa", ]  # Binary classification
iris$Species <- factor(iris$Species)


data(cancer)
test_that("fastml errors when data contains NAs and impute_method = 'error'", {
  expect_error(
    fastml(
      data = cancer,
      label = c("time", "status"),
      algorithms = "rand_forest",
      task = "survival",
      test_size = 0.3
    ),
    "Data contains NAs"
  )
})


test_that("rand_forest defaults to aorsf engine for survival", {
  expect_identical(get_default_engine("rand_forest", "survival"), "aorsf")
})

test_that("survival task works with mice imputation", {
  skip_if_not_installed("aorsf")
  skip_if_not_installed("censored")
  skip_if_not_installed("mice")
  res <- suppressWarnings(
    fastml(
      data = cancer,
      label = c("time", "status"),
      algorithms = c("rand_forest"),
      task = "survival",
      test_size = 0.3,
      impute_method = "mice",
      resampling_method = "none"
    )
  )
  expect_s3_class(res, "fastml")
})

test_that("advanced imputation is guarded when resampling is active", {
  expect_error(
    fastml(
      data = iris,
      label = "Species",
      algorithms = c("rand_forest"),
      impute_method = "mice",
      resampling_method = "cv"
    ),
    "Advanced imputation methods ('mice', 'missForest', 'custom') must be trained within each resample."
  )
})

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

test_that("engine_params list does not break ranger training", {
  skip_if_not_installed("ranger")

  rf_recipe <- recipes::recipe(Species ~ ., data = iris)

  expect_error(
    train_models(
      train_data = iris,
      label = "Species",
      task = "classification",
      algorithms = "rand_forest",
      resampling_method = "none",
      folds = 3,
      repeats = NA,
      resamples = NULL,
      tune_params = NULL,
      engine_params = list(rand_forest = list(ranger = list(importance = "impurity"))),
      metric = "accuracy",
      summaryFunction = NULL,
      seed = 123,
      recipe = rf_recipe,
      use_default_tuning = FALSE,
      tuning_strategy = "none",
      tuning_iterations = 10,
      early_stopping = FALSE,
      adaptive = FALSE,
      algorithm_engines = list(rand_forest = "ranger")
    ),
    regexp = NA
  )
})


test_that("engine_params configure Cox model ties", {
  skip_if_not_installed("survival")

  data(cancer, package = "survival")
  cancer$status <- ifelse(cancer$status == 2, 1, 0)
  cancer <- na.omit(cancer)  # remove NAs to prevent imputation error

  cox_result <- suppressWarnings(
    fastml(
      data = cancer,
      label = c("time", "status"),
      algorithms = "cox_ph",
      task = "survival",
      test_size = 0.3,
      resampling_method = "none",
      tune_params = NULL,
      engine_params = list(
        cox_ph = list(survival = list(ties = "breslow"))
      ),
      use_default_tuning = FALSE,
      tuning_strategy = "none",
      bootstrap_ci = FALSE
    )
  )

  expect_s3_class(cox_result$models$cox_ph$fit, "coxph")
  expect_identical(cox_result$models$cox_ph$fit$method, "breslow")
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
    folds = 5
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
    folds = 5
  )
  expect_s3_class(res, "fastml")
  expect_true(length(res$models) > 0)
})

test_that("adaptive tuning executes successfully", {
  res <- fastml(
    data = iris,
    label = "Species",
    algorithms = c("rand_forest"),
    use_default_tuning = TRUE,
    tuning_strategy = "grid",
    adaptive = TRUE,
    resampling_method = "cv",
    folds = 5
  )
  expect_s3_class(res, "fastml")
})

test_that("early_stopping does not warn with grid tuning", {
  expect_warning(
    fastml(
      data = iris,
      label = "Species",
      algorithms = c("rand_forest"),
      use_default_tuning = TRUE,
      tuning_strategy = "grid",
      early_stopping = TRUE,
      resampling_method = "cv",
      folds = 5
    ),
    regexp = NA
  )
})

test_that("early stopping with bayesian tuning works", {
  res <- fastml(
    data = iris,
    label = "Species",
    algorithms = c("rand_forest"),
    use_default_tuning = TRUE,
    tuning_strategy = "bayes",
    tuning_iterations = 2,
    early_stopping = TRUE,
    resampling_method = "cv",
    folds = 5
  )
  expect_s3_class(res, "fastml")
})

test_that("invalid tuning_iterations triggers error", {
  expect_error(
    fastml(
      data = iris,
      label = "Species",
      algorithms = c("rand_forest"),
      tuning_strategy = "bayes",
      tuning_iterations = 0,
      resampling_method = "cv",
      folds = 5
    ),
    "tuning_iterations"
  )
})

test_that("tuning_iterations ignored for non-bayesian strategies", {
  expect_error(
    fastml(
      data = iris,
      label = "Species",
      algorithms = "rand_forest",
      use_default_tuning = TRUE,
      tuning_strategy = "grid",
      tuning_iterations = 0,
      resampling_method = "cv",
      folds = 5
    ),
    regexp = NA
  )

  expect_error(
    fastml(
      data = iris,
      label = "Species",
      algorithms = "rand_forest",
      tuning_strategy = "none",
      tuning_iterations = -1,
      resampling_method = "none"
    ),
    regexp = NA
  )
})


test_that("adaptive ignored with bayesian tuning", {
  expect_warning(
    fastml(
      data = iris,
      label = "Species",
      algorithms = c("rand_forest"),
      use_default_tuning = TRUE,
      tuning_strategy = "bayes",
      adaptive = TRUE,
      tuning_iterations = 1,
      resampling_method = "cv",
      folds = 5
    ),
    "adaptive"
  )
})

# test_that("warning when tune_params ignored with no tuning", {
#   tune <- list(rand_forest = list(ranger = list(mtry = c(1, 2))))
#   expect_warning(
#     fastml(
#       data = iris,
#       label = "Species",
#       algorithms = c("rand_forest"),
#       tune_params = tune,
#       tuning_strategy = "none",
#       use_default_tuning = TRUE,
#       resampling_method = "none"
#     ),
#     "tune_params"
#   )
# })

