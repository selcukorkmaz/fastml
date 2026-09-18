# The bootstrap interval resamples the held-out predictions of a single fitted
# model. It therefore describes uncertainty from the evaluation sample
# conditional on that fit, and excludes training, tuning and model-selection
# variability. A reader of a `.lower`/`.upper` column will not read it that way,
# so it is not computed unless asked for, and when it is displayed the estimand
# is printed with it.

toy <- function(n = 200) {
  set.seed(7)
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  d$y <- factor(ifelse(d$x1 + rnorm(n, sd = 0.5) > 0, "pos", "neg"))
  d
}

fit <- function(...) {
  invisible(utils::capture.output(suppressWarnings(suppressMessages(
    fm <- fastml(data = toy(), label = "y", algorithms = "logistic_reg",
                 resampling_method = "none", verbose = FALSE, ...)
  ))))
  fm
}

test_that("bootstrap intervals are not computed unless requested", {
  skip_on_cran()
  fm <- fit()
  expect_false(isTRUE(fm$metric_bootstrap$enabled))
  perf <- fm$performance[[1]]
  # The columns remain, so the shape of the table does not depend on the
  # option, but nothing in them reads as an interval that was not computed.
  expect_true(all(is.na(perf$.lower)))
  expect_true(all(is.na(perf$.upper)))
  expect_true(all(perf$.n_boot == 0))
})

test_that("requesting bootstrap intervals produces them", {
  skip_on_cran()
  fm <- fit(bootstrap_ci = TRUE, bootstrap_samples = 50)
  expect_true(isTRUE(fm$metric_bootstrap$enabled))
  perf <- fm$performance[[1]]
  expect_true(any(!is.na(perf$.lower)))
  expect_true(all(perf$.lower[!is.na(perf$.lower)] <=
                    perf$.estimate[!is.na(perf$.lower)] + 1e-8))
  expect_true(all(perf$.upper[!is.na(perf$.upper)] >=
                    perf$.estimate[!is.na(perf$.upper)] - 1e-8))
})

test_that("displayed intervals carry their estimand", {
  skip_on_cran()
  fm <- fit(bootstrap_ci = TRUE, bootstrap_samples = 50)
  shown <- utils::capture.output(summary(fm, show_ci = TRUE, type = "metrics"))
  shown <- paste(shown, collapse = " ")
  expect_match(shown, "single fitted model", fixed = TRUE)
  expect_match(shown, "exclude training, tuning and model-selection", fixed = TRUE)
  # The qualification appears only where intervals are actually displayed.
  quiet <- paste(utils::capture.output(summary(fm, type = "metrics")), collapse = " ")
  expect_false(grepl("single fitted model", quiet, fixed = TRUE))
})
