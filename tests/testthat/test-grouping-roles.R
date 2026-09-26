library(testthat)

# Columns that define the resampling structure (`group_cols`, `block_col`) must
# shape the folds without ever reaching the model as predictors.

make_grouped_data <- function(seed = 3) {
  set.seed(seed)
  d <- data.frame(
    grp = rep(sprintf("G%02d", 1:40), each = 5),
    x1 = rnorm(200),
    x2 = rnorm(200)
  )
  d$y <- factor(ifelse(d$x1 + rnorm(200) > 0, "a", "b"))
  d
}

# Run `expr`, muffling warnings and returning them alongside the value.
collect_warnings <- function(expr) {
  warnings <- character()
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = warnings)
}

mentions_col <- function(names, col) {
  any(names == col | startsWith(names, paste0(col, "_")))
}

test_that("grouped_cv keeps group_cols out of the default recipe's predictors", {
  d <- make_grouped_data()

  fit <- collect_warnings(fastml(
    data = d,
    label = "y",
    algorithms = "logistic_reg",
    resampling_method = "grouped_cv",
    group_cols = "grp",
    folds = 5
  ))$value

  expect_false(mentions_col(names(fit$processed_train_data), "grp"))
  expect_false(mentions_col(names(fit$processed_test_data), "grp"))
  expect_setequal(setdiff(names(fit$processed_train_data), "y"), c("x1", "x2"))

  rec_info <- summary(fit$preprocessor)
  expect_false(any(rec_info$role == "predictor" & rec_info$variable == "grp"))

  engine_fit <- workflows::extract_fit_engine(fit$best_model[[1]])
  expect_setequal(names(stats::coef(engine_fit)), c("(Intercept)", "x1", "x2"))

  # With group identity as a feature the fit was degenerate (AUC 0.5).
  auc <- fit$performance[[1]]
  auc <- auc$.estimate[auc$.metric == "roc_auc"]
  expect_gt(auc, 0.6)
})

test_that("predict() works on new data with or without the grouping column", {
  d <- make_grouped_data()
  fit <- collect_warnings(fastml(
    data = d,
    label = "y",
    algorithms = "logistic_reg",
    resampling_method = "grouped_cv",
    group_cols = "grp",
    folds = 5
  ))$value

  without_grp <- d[, c("x1", "x2")]
  with_new_grp <- d[, c("grp", "x1", "x2")]
  with_new_grp$grp <- "UNSEEN"

  res_without <- collect_warnings(predict(fit, without_grp))
  res_with <- collect_warnings(predict(fit, with_new_grp))

  expect_length(res_without$value, nrow(d))
  expect_equal(res_with$value, res_without$value)
  expect_false(any(grepl("grp", c(res_without$warnings, res_with$warnings))))

  probs <- predict(fit, without_grp, type = "prob")
  expect_equal(nrow(probs), nrow(d))
})

test_that("multiple group_cols are all kept out of the predictors", {
  set.seed(11)
  d <- data.frame(
    site = rep(c("s1", "s2", "s3", "s4"), each = 30),
    subject = rep(sprintf("P%02d", 1:40), each = 3),
    x1 = rnorm(120),
    x2 = rnorm(120)
  )
  d$y <- d$x1 * 2 + rnorm(120)

  fit <- collect_warnings(fastml(
    data = d,
    label = "y",
    algorithms = "linear_reg",
    resampling_method = "grouped_cv",
    group_cols = c("site", "subject"),
    folds = 4
  ))$value

  processed <- names(fit$processed_train_data)
  expect_false(mentions_col(processed, "site"))
  expect_false(mentions_col(processed, "subject"))
  expect_length(predict(fit, d[, c("x1", "x2")]), nrow(d))
})

test_that("block_col is an ordering column, not a predictor", {
  set.seed(5)
  d <- data.frame(t = seq_len(120), x1 = rnorm(120), x2 = rnorm(120))
  d$y <- d$x1 + rnorm(120)

  fit <- collect_warnings(fastml(
    data = d,
    label = "y",
    algorithms = "linear_reg",
    resampling_method = "blocked_cv",
    block_col = "t",
    block_size = 20,
    folds = 5
  ))$value

  expect_false("t" %in% names(fit$processed_train_data))
  expect_length(predict(fit, d[, c("x1", "x2")]), nrow(d))
})

test_that("excluding a grouping column retains it for resampling", {
  d <- make_grouped_data()

  expect_message(
    fit <- collect_warnings(fastml(
      data = d,
      label = "y",
      algorithms = "logistic_reg",
      resampling_method = "grouped_cv",
      group_cols = "grp",
      folds = 5,
      exclude = "grp"
    ))$value,
    "retained for splitting and resampling"
  )

  expect_true("grp" %in% names(fit$raw_train_data))
  expect_false(mentions_col(names(fit$processed_train_data), "grp"))
})

test_that("a user recipe that uses a grouping column as a predictor warns", {
  d <- make_grouped_data()
  d$grp <- factor(d$grp)

  leaky <- recipes::recipe(y ~ ., data = d) |>
    recipes::step_dummy(recipes::all_nominal_predictors())
  res <- collect_warnings(fastml(
    data = d,
    label = "y",
    algorithms = "logistic_reg",
    recipe = leaky,
    resampling_method = "grouped_cv",
    group_cols = "grp",
    folds = 5
  ))
  expect_true(any(grepl("Grouping column(s) grp are used as predictors", res$warnings, fixed = TRUE)))

  removed <- recipes::recipe(y ~ ., data = d) |> recipes::step_rm(grp)
  res <- collect_warnings(fastml(
    data = d,
    label = "y",
    algorithms = "logistic_reg",
    recipe = removed,
    resampling_method = "grouped_cv",
    group_cols = "grp",
    folds = 5
  ))
  expect_false(any(grepl("are used as predictors", res$warnings, fixed = TRUE)))

  reroled <- recipes::recipe(y ~ ., data = d) |>
    recipes::update_role(grp, new_role = "grouping") |>
    recipes::update_role_requirements("grouping", bake = FALSE)
  res <- collect_warnings(fastml(
    data = d,
    label = "y",
    algorithms = "logistic_reg",
    recipe = reroled,
    resampling_method = "grouped_cv",
    group_cols = "grp",
    folds = 5
  ))
  expect_false(any(grepl("are used as predictors", res$warnings, fixed = TRUE)))
  expect_length(predict(res$value, d[, c("x1", "x2")]), nrow(d))
})

test_that("explainer inputs do not treat the grouping column as a feature", {
  d <- make_grouped_data()
  fit <- collect_warnings(fastml(
    data = d,
    label = "y",
    algorithms = "logistic_reg",
    resampling_method = "grouped_cv",
    group_cols = "grp",
    folds = 5
  ))$value

  prep <- fastml:::fastml_prepare_explainer_inputs(fit)
  expect_false("grp" %in% names(prep$x_raw))
  expect_false(mentions_col(names(prep$x_processed), "grp"))
})
