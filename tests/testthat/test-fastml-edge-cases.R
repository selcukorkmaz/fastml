library(testthat)
library(dplyr)

# small dataset for tests
set.seed(123)
data(iris)
iris_binary <- iris[iris$Species != "setosa", ]
iris_binary$Species <- factor(iris_binary$Species)

# ========================
# Input Validation Tests
# ========================

test_that("fastml errors when label column doesn't exist", {
  expect_error(
    fastml(
      data = iris_binary,
      label = "nonexistent_column",
      algorithms = "logistic_reg"
    ),
    "Label variable must exist|not found|not present|does not exist"
  )
})

test_that("fastml errors with empty data", {
  expect_error(
    fastml(
      data = iris_binary[0, ],
      label = "Species",
      algorithms = "logistic_reg"
    )
  )
})

test_that("fastml errors with invalid algorithm name", {
  expect_error(
    fastml(
      data = iris_binary,
      label = "Species",
      algorithms = "nonexistent_algorithm"
    )
  )
})

test_that("fastml errors with invalid resampling method", {
  expect_error(
    fastml(
      data = iris_binary,
      label = "Species",
      algorithms = "logistic_reg",
      resampling_method = "invalid_method"
    )
  )
})

# ========================
# Task Detection Tests
# ========================

test_that("fastml detects classification task from factor label", {
  skip_on_cran()

  model <- suppressWarnings(fastml(
    data = iris_binary,
    label = "Species",
    algorithms = "logistic_reg",
    resampling_method = "none",
    use_default_tuning = FALSE
  ))

  expect_equal(model$task, "classification")
})

test_that("fastml detects regression task from numeric label", {
  skip_on_cran()

  data(mtcars)
  model <- fastml(
    data = mtcars,
    label = "mpg",
    algorithms = "linear_reg",
    resampling_method = "none",
    use_default_tuning = FALSE
  )

  expect_equal(model$task, "regression")
})

test_that("fastml uses explicitly specified task", {
  skip_on_cran()

  model <- suppressWarnings(fastml(
    data = iris_binary,
    label = "Species",
    algorithms = "logistic_reg",
    task = "classification",
    resampling_method = "none",
    use_default_tuning = FALSE
  ))

  expect_equal(model$task, "classification")
})

# ========================
# Data Splitting Tests
# ========================

test_that("fastml respects test_size parameter", {
  skip_on_cran()

  model <- suppressWarnings(fastml(
    data = iris_binary,
    label = "Species",
    algorithms = "logistic_reg",
    resampling_method = "none",
    use_default_tuning = FALSE,
    test_size = 0.3
  ))

  # Check that raw_train_data and raw_test_data exist
  expect_true(!is.null(model$raw_train_data))
  expect_true(!is.null(model$raw_test_data))
})

test_that("fastml uses provided train_data and test_data", {
  skip_on_cran()

  set.seed(123)
  idx <- sample(nrow(iris_binary), floor(0.7 * nrow(iris_binary)))
  train_data <- iris_binary[idx, ]
  test_data <- iris_binary[-idx, ]

  model <- suppressWarnings(fastml(
    train_data = train_data,
    test_data = test_data,
    label = "Species",
    algorithms = "logistic_reg",
    resampling_method = "none",
    use_default_tuning = FALSE
  ))

  expect_equal(nrow(model$raw_train_data), nrow(train_data))
  expect_equal(nrow(model$raw_test_data), nrow(test_data))
})

# ========================
# Resampling Tests
# ========================

test_that("fastml works with cv resampling", {
  skip_on_cran()

  model <- suppressWarnings(fastml(
    data = iris_binary,
    label = "Species",
    algorithms = "logistic_reg",
    resampling_method = "cv",
    folds = 3,
    use_default_tuning = FALSE
  ))

  expect_true(inherits(model, "fastml"))
})

test_that("fastml works with none resampling", {
  skip_on_cran()

  model <- suppressWarnings(fastml(
    data = iris_binary,
    label = "Species",
    algorithms = "logistic_reg",
    resampling_method = "none",
    use_default_tuning = FALSE
  ))

  expect_true(inherits(model, "fastml"))
})

# ========================
# Multiple Algorithms Tests
# ========================

test_that("fastml trains multiple algorithms", {
  skip_on_cran()

  model <- suppressWarnings(fastml(
    data = iris_binary,
    label = "Species",
    algorithms = c("logistic_reg", "rand_forest"),
    resampling_method = "none",
    use_default_tuning = FALSE
  ))

  expect_true(length(model$models) >= 2)
})

test_that("fastml returns best model among multiple", {
  skip_on_cran()

  model <- suppressWarnings(fastml(
    data = iris_binary,
    label = "Species",
    algorithms = c("logistic_reg", "rand_forest"),
    resampling_method = "none",
    use_default_tuning = FALSE
  ))

  expect_true(!is.null(model$best_model))
  expect_true(!is.null(model$best_model_name))
})

# ========================
# Metric Tests
# ========================

test_that("fastml uses specified metric for classification", {
  skip_on_cran()

  model <- suppressWarnings(fastml(
    data = iris_binary,
    label = "Species",
    algorithms = "logistic_reg",
    metric = "accuracy",
    resampling_method = "none",
    use_default_tuning = FALSE
  ))

  expect_equal(model$metric, "accuracy")
})

test_that("fastml uses specified metric for regression", {
  skip_on_cran()

  data(mtcars)
  model <- fastml(
    data = mtcars,
    label = "mpg",
    algorithms = "linear_reg",
    metric = "rmse",
    resampling_method = "none",
    use_default_tuning = FALSE
  )

  expect_equal(model$metric, "rmse")
})

# ========================
# Output Structure Tests
# ========================

test_that("fastml returns complete object structure", {
  skip_on_cran()

  model <- suppressWarnings(fastml(
    data = iris_binary,
    label = "Species",
    algorithms = "logistic_reg",
    resampling_method = "none",
    use_default_tuning = FALSE
  ))

  expect_true("models" %in% names(model))
  expect_true("performance" %in% names(model))
  expect_true("predictions" %in% names(model))
  expect_true("best_model" %in% names(model))
  expect_true("label" %in% names(model))
  expect_true("task" %in% names(model))
  expect_true("preprocessor" %in% names(model))
})

test_that("fastml performance contains expected metrics for classification", {
  skip_on_cran()

  model <- suppressWarnings(fastml(
    data = iris_binary,
    label = "Species",
    algorithms = "logistic_reg",
    resampling_method = "none",
    use_default_tuning = FALSE
  ))

  perf <- model$performance[[1]]
  if (is.list(perf) && !is.data.frame(perf)) {
    perf <- perf[[1]]
  }

  expect_true("accuracy" %in% perf$.metric || "roc_auc" %in% perf$.metric)
})

test_that("fastml performance contains expected metrics for regression", {
  skip_on_cran()

  data(mtcars)
  model <- fastml(
    data = mtcars,
    label = "mpg",
    algorithms = "linear_reg",
    resampling_method = "none",
    use_default_tuning = FALSE
  )

  perf <- model$performance[[1]]
  if (is.list(perf) && !is.data.frame(perf)) {
    perf <- perf[[1]]
  }

  expect_true("rmse" %in% perf$.metric || "rsq" %in% perf$.metric)
})

# ========================
# Seed Reproducibility Tests
# ========================

test_that("fastml results are reproducible with same seed", {
  skip_on_cran()

  model1 <- suppressWarnings(fastml(
    data = iris_binary,
    label = "Species",
    algorithms = "logistic_reg",
    resampling_method = "none",
    use_default_tuning = FALSE,
    seed = 42
  ))

  model2 <- suppressWarnings(fastml(
    data = iris_binary,
    label = "Species",
    algorithms = "logistic_reg",
    resampling_method = "none",
    use_default_tuning = FALSE,
    seed = 42
  ))

  # Performance should be identical with same seed
  perf1 <- model1$performance[[1]]
  perf2 <- model2$performance[[1]]

  if (is.list(perf1) && !is.data.frame(perf1)) {
    perf1 <- perf1[[1]]
  }
  if (is.list(perf2) && !is.data.frame(perf2)) {
    perf2 <- perf2[[1]]
  }

  expect_equal(perf1$.estimate, perf2$.estimate)
})

# ========================
# Special Data Types Tests
# ========================

test_that("fastml handles character columns by converting to factor", {
  skip_on_cran()

  df <- iris_binary
  df$char_col <- sample(c("A", "B"), nrow(df), replace = TRUE)

  model <- suppressWarnings(fastml(
    data = df,
    label = "Species",
    algorithms = "logistic_reg",
    resampling_method = "none",
    use_default_tuning = FALSE
  ))

  expect_true(inherits(model, "fastml"))
})

test_that("fastml handles data with NAs via imputation", {
  skip_on_cran()

  df <- iris_binary
  df$Sepal.Length[1:5] <- NA

  model <- suppressWarnings(fastml(
    data = df,
    label = "Species",
    algorithms = "logistic_reg",
    resampling_method = "none",
    use_default_tuning = FALSE,
    impute_method = "medianImpute"
  ))

  expect_true(inherits(model, "fastml"))
})

# ========================
# Event Class Tests
# ========================

test_that("fastml uses specified event_class", {
  skip_on_cran()

  model <- suppressWarnings(fastml(
    data = iris_binary,
    label = "Species",
    algorithms = "logistic_reg",
    event_class = "first",
    resampling_method = "none",
    use_default_tuning = FALSE
  ))

  expect_equal(model$event_class, "first")
})

# ========================
# Verbose Tests
# ========================

test_that("fastml verbose=TRUE produces output", {
  skip_on_cran()

  output <- capture.output(type = "message", {
    model <- suppressWarnings(fastml(
      data = iris_binary,
      label = "Species",
      algorithms = "logistic_reg",
      resampling_method = "none",
      use_default_tuning = FALSE,
      verbose = TRUE
    ))
  })

  expect_true(length(output) > 0)
})

test_that("fastml verbose=FALSE suppresses output", {
  skip_on_cran()

  output <- capture.output({
    model <- suppressMessages(suppressWarnings(fastml(
      data = iris_binary,
      label = "Species",
      algorithms = "logistic_reg",
      resampling_method = "none",
      use_default_tuning = FALSE,
      verbose = FALSE
    )))
  })

  # Verbose=FALSE should produce minimal/no output
  expect_true(length(output) == 0 || all(output == ""))
})
