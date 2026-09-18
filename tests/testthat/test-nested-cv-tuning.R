# Nested cross-validation exists to estimate the performance of a tuning and
# fitting procedure. It previously produced an estimate only when it was not
# tuning, which is the one case in which it has no purpose.
#
# The chain was: probability metrics returned NA inside the metric set, so
# select_best() could not choose a configuration; the unfinalized workflow was
# fitted anyway and failed inside the engine; every outer fold failed; and the
# run fell back to ordinary resampling, recording selection$source as "none"
# with nothing to indicate that no nested estimate had been produced.

skip_if_not_installed("ranger")

toy <- function(n = 150) {
  set.seed(3)
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
  d$y <- factor(ifelse(d$x1 + rnorm(n, sd = 0.6) > 0, "pos", "neg"))
  d
}

nested <- function(...) {
  invisible(utils::capture.output(suppressWarnings(suppressMessages(
    fm <- fastml(data = toy(), label = "y", algorithms = "rand_forest",
                 resampling_method = "nested_cv", folds = 3, outer_folds = 3,
                 verbose = FALSE, ...)
  ))))
  fm
}

test_that("nested cross-validation reports a nested estimate when tuning", {
  skip_on_cran()
  for (args in list(list(), list(use_default_tuning = TRUE),
                    list(tune_params = list(rand_forest = list(ranger = list(mtry = c(1, 2))))))) {
    fm <- do.call(nested, args)
    expect_identical(fm$selection$source, "nested_cv",
                     info = paste(names(args), collapse = ","))
    expect_false(is.null(fm$nested_cv))
  }
})

test_that("the nested estimate is computed from the outer folds", {
  skip_on_cran()
  fm <- nested(use_default_tuning = TRUE)
  inner <- fm$nested_cv[[1]]
  expect_false(is.null(inner$outer_performance))
  auc <- inner$outer_performance[inner$outer_performance$.metric == "roc_auc", ]
  # One score per outer fold, and all of them usable. Empty probability
  # metrics were what made this impossible.
  expect_equal(nrow(auc), 3L)
  expect_true(all(is.finite(auc$.estimate)))
})

test_that("the configuration carried into the returned model is fully specified", {
  skip_on_cran()
  fm <- nested(use_default_tuning = TRUE)
  final <- fm$nested_cv[[1]]$final_params
  expect_false(is.null(final))
  # tune attaches hyperparameter columns only to the class-metric rows, so a
  # configuration chosen on roc_auc must have its values recovered rather than
  # read directly. NA here is what previously reached the engine.
  expect_false(any(vapply(final, function(v) all(is.na(v)), logical(1))))
})

test_that("summary reports that the nested estimate is not of the returned model", {
  skip_on_cran()
  fm <- nested(use_default_tuning = TRUE)
  shown <- paste(utils::capture.output(summary(fm, type = "metrics")), collapse = " ")
  expect_match(shown, "not of the returned model", fixed = TRUE)
  expect_match(shown, "best mean inner score", fixed = TRUE)
})
