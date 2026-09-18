# Registry smoke tests for the classification and regression surfaces.
#
# Coverage of the model-specification registries was the weakest part of the
# suite, at 0 to 35 percent, which is precisely where the package's defaults
# live. The defects those registries can carry are not subtle, they are "this
# advertised family does not fit at all", and they were previously found by
# fitting families by hand while writing documentation rather than by the
# suite. Every family named in availableMethods() is therefore fitted here on a
# canonical frame under a single holdout, and every one must produce a model.
#
# The families are fitted in one call per task rather than one call per family.
# That is roughly six times faster than looping, and it exercises the
# multi-algorithm path that ordinary use takes.
#
# The survival surface is covered by test-survival-registry.R, which also
# asserts the refusal contracts specific to it.

skip_if_not_installed("ranger")
skip_if_not_installed("xgboost")
skip_if_not_installed("lightgbm")
skip_if_not_installed("bonsai")
skip_if_not_installed("C50")
skip_if_not_installed("kernlab")
skip_if_not_installed("kknn")
skip_if_not_installed("klaR")
skip_if_not_installed("nnet")
skip_if_not_installed("discrim")
skip_if_not_installed("sparsediscrim")
skip_if_not_installed("baguette")
skip_if_not_installed("glmnet")
skip_if_not_installed("plsmod")
skip_if_not_installed("rstanarm")

canonical_binary <- function(n = 160) {
  set.seed(42)
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n), x4 = rnorm(n))
  d$y <- factor(ifelse(d$x1 + d$x2 + rnorm(n, sd = 0.5) > 0, "pos", "neg"))
  d
}

canonical_multiclass <- function(n = 180) {
  set.seed(43)
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n), x4 = rnorm(n))
  d$y <- factor(sample(c("a", "b", "c"), n, replace = TRUE))
  d
}

canonical_regression <- function(n = 160) {
  set.seed(44)
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n), x4 = rnorm(n))
  d$y <- 2 * d$x1 + d$x2 + rnorm(n)
  d
}

fit_all <- function(d, algos, task) {
  invisible(utils::capture.output(
    suppressWarnings(suppressMessages(
      fm <- fastml(
        data = d, label = "y", algorithms = algos, task = task,
        resampling_method = "none", bootstrap_ci = FALSE, verbose = FALSE
      )
    ))
  ))
  fm
}

# Model names are stored as "algo (engine)".
fitted_algorithms <- function(fm) unique(sub(" \\(.*$", "", names(fm$models)))

test_that("every advertised classification family fits on a canonical frame", {
  skip_on_cran()
  library(bonsai)
  # multinom_reg is deliberately redirected to logistic_reg on a two-class
  # outcome, so it is exercised separately on a multiclass frame below.
  algos <- setdiff(availableMethods("classification"), "multinom_reg")
  fm <- fit_all(canonical_binary(), algos, "classification")
  expect_s3_class(fm, "fastml")
  expect_setequal(fitted_algorithms(fm), algos)
  expect_length(attr(fm$models, "failed_models"), 0)
})

test_that("multinom_reg fits a multiclass outcome", {
  skip_on_cran()
  fm <- fit_all(canonical_multiclass(), "multinom_reg", "classification")
  expect_setequal(fitted_algorithms(fm), "multinom_reg")
})

test_that("multinom_reg is redirected to logistic_reg on a two-class outcome", {
  skip_on_cran()
  # The substitution is a documented convenience, but it is a substitution, so
  # it must be announced rather than leaving the requested family missing.
  expect_warning(
    invisible(utils::capture.output(suppressMessages(
      fm <- fastml(data = canonical_binary(), label = "y",
                   algorithms = "multinom_reg", task = "classification",
                   resampling_method = "none", bootstrap_ci = FALSE,
                   verbose = FALSE)
    ))),
    "not applicable to two-class"
  )
  expect_setequal(fitted_algorithms(fm), "logistic_reg")
})

test_that("every advertised regression family fits on a canonical frame", {
  skip_on_cran()
  library(bonsai)
  algos <- availableMethods("regression")
  fm <- fit_all(canonical_regression(), algos, "regression")
  expect_s3_class(fm, "fastml")
  expect_setequal(fitted_algorithms(fm), algos)
  expect_length(attr(fm$models, "failed_models"), 0)
})

test_that("the advertised registry has the size the documentation states", {
  # The article quotes a total number of supported families. Withdrawing
  # time_varying_cox changed it once already, and the prose did not follow, so
  # the count is asserted rather than left to be re-counted by hand.
  expect_length(availableMethods("classification"), 15L)
  expect_length(availableMethods("regression"), 14L)
  expect_length(availableMethods("survival"), 11L)
  total <- sum(vapply(
    c("classification", "regression", "survival"),
    function(t) length(availableMethods(t)), integer(1)
  ))
  expect_identical(total, 40L)
})
