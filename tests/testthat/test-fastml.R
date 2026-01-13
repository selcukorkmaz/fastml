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

test_that("bootstrap CI columns are returned for classification and regression", {
  # Reduce separation risk in glm for stable bootstrap fits.
  iris_bootstrap <- iris[, c("Sepal.Length", "Sepal.Width", "Species")]

  fit_class <- fastml(
    data = iris_bootstrap,
    label = "Species",
    algorithms = "logistic_reg",
    task = "classification",
    resampling_method = "none",
    use_default_tuning = FALSE,
    test_size = 0.3,
    seed = 123,
    bootstrap_ci = TRUE,
    bootstrap_samples = 10,
    bootstrap_seed = 123
  )

  perf_class <- fit_class$performance[[1]]
  expect_true(all(c(".lower", ".upper", ".n_boot") %in% names(perf_class)))
  expect_true(all(perf_class$.n_boot == 10))
  expect_true(any(is.finite(perf_class$.lower)))
  expect_true(any(is.finite(perf_class$.upper)))

  fit_reg <- fastml(
    data = mtcars,
    label = "mpg",
    algorithms = "linear_reg",
    task = "regression",
    resampling_method = "none",
    use_default_tuning = FALSE,
    test_size = 0.3,
    seed = 123,
    bootstrap_ci = TRUE,
    bootstrap_samples = 10,
    bootstrap_seed = 123
  )

  perf_reg <- fit_reg$performance[[1]]
  expect_true(all(c(".lower", ".upper", ".n_boot") %in% names(perf_reg)))
  expect_true(all(perf_reg$.n_boot == 10))
  expect_true(any(is.finite(perf_reg$.lower)))
  expect_true(any(is.finite(perf_reg$.upper)))
})



test_that("advanced imputation options are rejected", {
  expect_error(
    fastml(
      data = iris,
      label = "Species",
      algorithms = "decision_tree",
      impute_method = "mice",
      resampling_method = "none",
      use_default_tuning = FALSE
    ),
    "no longer supported",
    ignore.case = TRUE
  )

  expect_error(
    fastml(
      data = iris,
      label = "Species",
      algorithms = "decision_tree",
      impute_method = "missForest",
      resampling_method = "none",
      use_default_tuning = FALSE
    ),
    "no longer supported",
    ignore.case = TRUE
  )

  expect_error(
    fastml(
      data = iris,
      label = "Species",
      algorithms = "decision_tree",
      impute_method = "custom",
      resampling_method = "none",
      use_default_tuning = FALSE
    ),
    "no longer supported",
    ignore.case = TRUE
  )
})

test_that("multinom_reg falls back to logistic_reg for binary outcomes", {
  binary_iris <- iris[, c("Sepal.Length", "Sepal.Width", "Species")]

  expect_warning(
    fastml(
      data = binary_iris,
      label = "Species",
      algorithms = "multinom_reg",
      resampling_method = "none",
      use_default_tuning = FALSE,
      seed = 123
    ) -> fit,
    "not applicable to two-class outcomes"
  )

  expect_s3_class(fit, "fastml")
  expect_true(any(startsWith(names(fit$models), "logistic_reg")))
  expect_false(any(startsWith(names(fit$models), "multinom_reg")))
})

test_that("grouped_cv requires grouping columns", {
  binary_iris <- iris[iris$Species != "virginica", ]
  binary_iris$Species <- droplevels(binary_iris$Species)

  expect_error(
    fastml(
      data = binary_iris,
      label = "Species",
      algorithms = c("logistic_reg"),
      resampling_method = "grouped_cv",
      folds = 3,
      use_default_tuning = FALSE
    ),
    "group_cols",
    ignore.case = TRUE
  )
})


test_that("blocked_cv enforces ordered data", {
  set.seed(99)
  blocked_df <- data.frame(
    time_id = sample(seq_len(60)),
    x1 = rnorm(60),
    response = factor(sample(c("yes", "no"), 60, replace = TRUE))
  )

  expect_error(
    fastml(
      data = blocked_df,
      label = "response",
      algorithms = c("logistic_reg"),
      resampling_method = "blocked_cv",
      block_col = "time_id",
      block_size = 5,
      folds = 4,
      use_default_tuning = FALSE
    ),
    "sorted",
    ignore.case = TRUE
  )
})


test_that("rolling_origin validates configuration", {
  rolling_df <- data.frame(
    time_id = seq_len(30),
    x = rnorm(30),
    y = rnorm(30)
  )

  expect_error(
    fastml(
      data = rolling_df,
      label = "y",
      algorithms = c("linear_reg"),
      resampling_method = "rolling_origin",
      block_col = "time_id",
      assess_window = 5,
      use_default_tuning = FALSE
    ),
    "initial_window",
    ignore.case = TRUE
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
      algorithm_engines = list(rand_forest = "ranger"),
      event_class = "first"
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
  skip_if_not_installed("lme4")

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

test_that("fold-unsafe preprocessing is blocked during resampling", {
  skip_if_not_installed("rsample")

  set.seed(101)
  binary_iris <- iris[iris$Species != "virginica", ]
  binary_iris$Species <- factor(binary_iris$Species)

  idx <- sample(seq_len(nrow(binary_iris)), size = floor(0.7 * nrow(binary_iris)))
  train_split <- binary_iris[idx, , drop = FALSE]
  test_split <- binary_iris[-idx, , drop = FALSE]

  guard_resamples <- rsample::apparent(train_split)

  expect_error(
    fastml(
      train_data = train_split,
      test_data = test_split,
      label = "Species",
      algorithms = "logistic_reg",
      resamples = guard_resamples,
      resampling_method = "cv",
      folds = 5,
      seed = 202
    ),
    "Detected preprocessing on the full training set",
    fixed = TRUE
  )
})

test_that("guarded folds avoid performance inflation", {
  set.seed(2024)
  noise_df <- data.frame(
    x1 = rnorm(80),
    x2 = rnorm(80),
    x3 = rnorm(80),
    y = factor(sample(c("A", "B"), 80, replace = TRUE))
  )

  res_noise <- fastml(
    data = noise_df,
    label = "y",
    algorithms = "logistic_reg",
    resampling_method = "cv",
    folds = 4,
    seed = 77
  )

  guard_metrics <- res_noise$resampling_results[["logistic_reg (glm)"]]$aggregated
  guard_folds <- res_noise$resampling_results[["logistic_reg (glm)"]]$folds
  noise_accuracy <- guard_metrics[guard_metrics$.metric == "accuracy", ".estimate", drop = TRUE]

  expect_true(is.numeric(noise_accuracy))
  expect_lt(noise_accuracy, 0.8)
  expect_true(all(c("n", "std_dev", "std_err") %in% names(guard_metrics)))
  expect_true(is.numeric(guard_metrics$std_dev))
  expect_true(is.numeric(guard_metrics$std_err))
  expect_false(any(c(".lower", ".upper", ".n_boot") %in% names(guard_folds)))
})

test_that("guarded resampling is reproducible with fixed seeds", {
  set.seed(404)
  synth_df <- data.frame(
    x1 = rnorm(60),
    x2 = rnorm(60),
    y = factor(sample(c("yes", "no"), 60, replace = TRUE))
  )

  fit_one <- fastml(
    data = synth_df,
    label = "y",
    algorithms = "logistic_reg",
    resampling_method = "cv",
    folds = 3,
    seed = 55
  )

  set.seed(404)
  fit_two <- fastml(
    data = synth_df,
    label = "y",
    algorithms = "logistic_reg",
    resampling_method = "cv",
    folds = 3,
    seed = 55
  )

  expect_equal(
    fit_one$resampling_results[["logistic_reg (glm)"]]$aggregated,
    fit_two$resampling_results[["logistic_reg (glm)"]]$aggregated
  )
})

test_that("warning when tune_params ignored with no tuning", {
  tune <- list(rand_forest = list(ranger = list(mtry = c(1, 2))))
  expect_warning(
    fastml(
      data = iris,
      label = "Species",
      algorithms = c("rand_forest"),
      tune_params = tune,
      tuning_strategy = "none",
      use_default_tuning = TRUE,
      resampling_method = "none"
    ),
    "tune_params"
  )
})
