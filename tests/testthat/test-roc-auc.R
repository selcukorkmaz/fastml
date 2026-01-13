library(testthat)

test_that("binary roc_auc uses available probability columns", {
  required <- c("parsnip", "workflows", "recipes", "yardstick", "rsample")
  lapply(required, skip_if_not_installed)

  data(iris)
  df <- iris[iris$Species != "setosa", c("Sepal.Length", "Sepal.Width", "Species")]
  df$Species <- factor(df$Species)
  levels(df$Species) <- c("negative class", "positive class")

  fit <- fastml(
    data = df,
    label = "Species",
    algorithms = "logistic_reg",
    task = "classification",
    resampling_method = "none",
    use_default_tuning = FALSE,
    test_size = 0.3,
    seed = 123,
    bootstrap_ci = FALSE,
    event_class = "second"
  )

  perf <- fit$performance[[1]]
  if (is.list(perf) && !is.data.frame(perf)) {
    perf <- perf[[1]]
  }

  expect_true("roc_auc" %in% perf$.metric)
  roc_val <- perf$.estimate[perf$.metric == "roc_auc"][1]
  expect_true(is.finite(roc_val))

  preds <- fit$predictions[[1]]
  if (is.list(preds) && !is.data.frame(preds)) {
    preds <- preds[[1]]
  }
  expect_true(any(grepl("^\\.pred_", names(preds))))
})

test_that("binary probability column resolver handles sanitized and fallback names", {
  res <- fastml:::fastml_resolve_binary_prob_column(
    prob_cols = c(".pred_pos.class", ".pred_neg.class"),
    truth_levels = c("pos class", "neg class"),
    event_class = "first"
  )
  expect_identical(res$prob_col, ".pred_pos.class")
  expect_false(isTRUE(res$used_fallback))

  res_fallback <- fastml:::fastml_resolve_binary_prob_column(
    prob_cols = c(".prob0", ".prob1"),
    truth_levels = c("no", "yes"),
    event_class = "second"
  )
  expect_identical(res_fallback$prob_col, ".prob1")
  expect_true(isTRUE(res_fallback$used_fallback))
})

test_that("configured roc_auc accepts explicit estimator", {
  required <- c("yardstick")
  lapply(required, skip_if_not_installed)

  data(iris)
  df <- iris[iris$Species != "setosa", ]
  df$Species <- factor(df$Species)

  set.seed(123)
  df$.pred_versicolor <- runif(nrow(df))

  roc_fun <- fastml:::fastml_configured_roc_auc("macro")

  expect_silent(
    roc_fun(df, truth = Species, .pred_versicolor, estimator = "binary")
  )
})
