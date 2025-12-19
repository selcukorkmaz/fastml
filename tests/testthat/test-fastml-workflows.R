library(dplyr)
library(testthat)
library(survival)

# Basic workflow smoke tests to cover core paths

test_that("fastml runs a minimal classification path", {
  skip_on_cran()
  required <- c("parsnip", "workflows", "recipes", "yardstick", "tune", "rsample", "dials")
  lapply(required, skip_if_not_installed)

  data <- iris[iris$Species != "setosa", , drop = FALSE]
  data$Species <- factor(data$Species)

  set.seed(123)
  fit <- fastml(
    data = data,
    label = "Species",
    algorithms = "logistic_reg",
    resampling_method = "none",
    stratify = FALSE,
    use_default_tuning = FALSE,
    tuning_strategy = "none",
    verbose = FALSE
  )

  expect_s3_class(fit, "fastml")
  expect_true("logistic_reg (glm)" %in% names(fit$models))

  grDevices::pdf(file = tempfile())
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_error(plot(fit, type = "bar"), NA)
})

test_that("train_models returns fitted models for classification", {
  skip_on_cran()
  required <- c("parsnip", "workflows", "recipes", "yardstick", "tune", "rsample", "dials")
  lapply(required, skip_if_not_installed)

  train_dat <- iris[iris$Species != "setosa", , drop = FALSE]
  train_dat$Species <- factor(train_dat$Species)
  train_dat <- train_dat[seq_len(40), ]

  recipe_obj <- recipes::recipe(Species ~ ., data = train_dat)

  models <- train_models(
    train_data = train_dat,
    label = "Species",
    task = "classification",
    algorithms = "logistic_reg",
    resampling_method = "none",
    folds = 3,
    repeats = NULL,
    group_cols = NULL,
    block_col = NULL,
    block_size = NULL,
    initial_window = NULL,
    assess_window = NULL,
    skip = 0,
    outer_folds = NULL,
    resamples = NULL,
    tune_params = NULL,
    engine_params = list(),
    metric = "accuracy",
    summaryFunction = NULL,
    seed = 123,
    recipe = recipe_obj,
    use_default_tuning = FALSE,
    tuning_strategy = "none",
    tuning_iterations = 10,
    early_stopping = FALSE,
    adaptive = FALSE,
    algorithm_engines = NULL,
    event_class = "first",
    start_col = NULL,
    time_col = NULL,
    status_col = NULL,
    eval_times = NULL,
    at_risk_threshold = 0.1,
    audit_env = NULL
  )

  expect_true(is.list(models))
  expect_true("logistic_reg" %in% names(models))
})

test_that("process_model evaluates fitted workflow", {
  skip_on_cran()
  required <- c("parsnip", "workflows", "recipes", "yardstick", "tune", "rsample", "dials")
  lapply(required, skip_if_not_installed)

  data <- iris[iris$Species != "setosa", , drop = FALSE]
  data$Species <- factor(data$Species)

  wf <- workflows::workflow() |>
    workflows::add_model(parsnip::logistic_reg() |> parsnip::set_engine("glm")) |>
    workflows::add_recipe(recipes::recipe(Species ~ ., data = data))

  fitted <- parsnip::fit(wf, data = data)

  res <- process_model(
    model_obj = fitted,
    model_id = "logit",
    task = "classification",
    test_data = data,
    label = "Species",
    event_class = "first",
    start_col = NULL,
    time_col = NULL,
    status_col = NULL,
    engine = "glm",
    train_data = data,
    metric = "accuracy",
    eval_times_user = NULL,
    bootstrap_ci = FALSE,
    bootstrap_samples = 10,
    bootstrap_seed = 123,
    at_risk_threshold = 0.1,
    precomputed_predictions = NULL
  )

  expect_true(is.list(res))
  expect_true(all(c("performance", "predictions") %in% names(res)))
  expect_s3_class(res$performance, "tbl_df")
})

test_that("fastml fits a simple survival model", {
  skip_on_cran()

  required <- c(
    "parsnip", "workflows", "recipes", "yardstick",
    "tune", "rsample", "dials", "survival", "censored"
  )
  lapply(required, skip_if_not_installed)

  lung <- survival::lung
  lung <- lung[complete.cases(lung),]

  # fastml expects: 0 = censored, 1 = event
  lung$status <- ifelse(lung$status == 2, 1, 0)

  fit <- fastml(
      data = lung,
      label = c("time", "status"),
      algorithms = "cox_ph",
      resampling_method = "none"
    )

  expect_s3_class(fit, "fastml")
  expect_true("cox_ph (survival)" %in% names(fit$models))
  expect_true(length(fit$performance) >= 1)
})
