# Registry tests for the survival surface.
#
# These encode Table 2 of the article as a regression-tested artifact rather
# than a snapshot. Every advertised survival family must fit on a canonical
# right-censored frame under a single holdout, and every combination the
# article records as unavailable must raise an error rather than degrade
# silently. The four defects these tests were written for were all found by
# hand and would all have been caught here.

skip_if_not_installed("survival")
skip_if_not_installed("censored")

canonical <- function() {
  utils::data("cancer", package = "survival", envir = environment())
  d <- get("lung", envir = environment())
  d <- d[stats::complete.cases(d), ]
  d$status <- ifelse(d$status == 2, 1, 0)
  d
}

fit_holdout <- function(d, algo, ...) {
  suppressWarnings(fastml(
    data = d, label = c("time", "status"), task = "survival",
    algorithms = algo, resampling_method = "none",
    bootstrap_ci = FALSE, verbose = FALSE, ...
  ))
}

test_that("every advertised survival family fits under a holdout", {
  skip_on_cran()
  library(censored)
  d <- canonical()
  for (algo in availableMethods(type = "survival")) {
    extra <- if (identical(algo, "stratified_cox")) list(strata_cols = "sex") else list()
    fm <- do.call(fit_holdout, c(list(d = d, algo = algo), extra))
    expect_s3_class(fm, "fastml")
    expect_true(length(fm$models) > 0, info = algo)
  }
})

test_that("families outside the guarded path refuse resampling", {
  skip_on_cran()
  library(censored)
  d <- canonical()
  native <- c("parametric_surv", "piecewise_exp", "royston_parmar", "xgboost_aft", "xgboost")
  for (algo in native) {
    expect_error(
      suppressWarnings(fastml(data = d, label = c("time", "status"), task = "survival",
                              algorithms = algo, resampling_method = "cv", folds = 3,
                              bootstrap_ci = FALSE, verbose = FALSE)),
      info = algo
    )
  }
})

test_that("xgboost_aft refuses rather than silently disabling resampling", {
  skip_on_cran()
  library(censored)
  d <- canonical()
  expect_error(
    suppressWarnings(fastml(data = d, label = c("time", "status"), task = "survival",
                            algorithms = "xgboost_aft", resampling_method = "cv", folds = 3,
                            bootstrap_ci = FALSE, verbose = FALSE)),
    "Resampling and tuning are not available"
  )
  # and the supported configuration still works
  expect_s3_class(fit_holdout(d, "xgboost_aft"), "fastml")
})

test_that("stratified_cox requires declared strata and accepts strata_cols", {
  skip_on_cran()
  library(censored)
  d <- canonical()
  expect_error(fit_holdout(d, "stratified_cox"), "requires at least one stratification column")
  expect_s3_class(fit_holdout(d, "stratified_cox", strata_cols = "sex"), "fastml")
  expect_error(fit_holdout(d, "stratified_cox", strata_cols = "not_a_column"),
               "names columns not present")
  # the historical name-prefix rule still resolves
  d2 <- d; d2$strata_sex <- factor(d2$sex)
  expect_s3_class(fit_holdout(d2, "stratified_cox"), "fastml")
})

test_that("time_varying_cox is withdrawn and says why", {
  expect_false("time_varying_cox" %in% availableMethods(type = "survival"))
  skip_on_cran()
  library(censored)
  expect_error(fit_holdout(canonical(), "time_varying_cox"), "not currently available")
})

test_that("royston_parmar leaves the search path as it found it", {
  skip_on_cran()
  skip_if_not_installed("rstpm2")
  library(censored)
  d <- canonical()

  # case 1: rstpm2 not attached beforehand, must not be attached afterwards
  if ("package:rstpm2" %in% search()) detach("package:rstpm2", character.only = TRUE)
  expect_false("package:rstpm2" %in% search())
  expect_s3_class(fit_holdout(d, "royston_parmar"), "fastml")
  expect_false("package:rstpm2" %in% search())

  # case 2: already attached, must still be attached afterwards
  library(rstpm2)
  expect_true("package:rstpm2" %in% search())
  expect_s3_class(fit_holdout(d, "royston_parmar"), "fastml")
  expect_true("package:rstpm2" %in% search())
  detach("package:rstpm2", character.only = TRUE)
})

test_that("a numeric stratifier survives preprocessing that rescales it", {
  skip_on_cran()
  library(censored)
  d <- canonical()
  # 'sex' is numeric on the canonical frame, so normalisation rewrites its
  # values. The stratifier must still be read from the training frame, which
  # is what keeps its levels and its values in the same coding.
  fm <- fit_holdout(d, "stratified_cox", strata_cols = "sex")
  expect_s3_class(fm, "fastml")
  spec <- fm$models[["stratified_cox (survival)"]]
  expect_s3_class(spec, "fastml_native_survival")
  expect_identical(spec$strata_cols, "sex")
  fit <- spec$fit
  terms_txt <- paste(deparse(stats::formula(fit)), collapse = " ")
  expect_match(terms_txt, "strata(sex)", fixed = TRUE)
  # Every training row must reach the fit. Before the stratifier was read from
  # the training frame, normalisation left it entirely missing and coxph
  # dropped the whole sample.
  expect_equal(fit$n, nrow(recipes::bake(spec$recipe, new_data = NULL)))
  # The strata levels must still carry the original coding rather than the
  # normalised values.
  expect_equal(fit$xlevels[["strata(sex)"]], c("1", "2"))
})
