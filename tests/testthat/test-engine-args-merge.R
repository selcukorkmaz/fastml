library(testthat)

# Regression tests for fastml_merge_engine_args().
# parsnip::set_engine() replaces engine arguments rather than adding to them, so
# applying pipeline arguments in a second pass used to discard the defaults a spec
# builder had set (LightGBM's counts/bagging_freq/verbose, xgboost's early-stopping
# configuration, sparsediscrim's regularization_method).

test_that("spec defaults survive a second application of engine args", {
  existing <- list(counts = rlang::quo(TRUE), verbose = rlang::quo(-1L))
  merged <- fastml:::fastml_merge_engine_args(
    existing, list(num_threads = 4L, seed = 2025L)
  )
  expect_setequal(names(merged), c("counts", "verbose", "num_threads", "seed"))
  expect_true(merged$counts)
  expect_identical(merged$verbose, -1L)
})

test_that("pipeline args override spec defaults on shared keys", {
  existing <- list(num_threads = rlang::quo(0L), seed = rlang::quo(123L))
  merged <- fastml:::fastml_merge_engine_args(
    existing, list(num_threads = 4L, seed = 2025L)
  )
  expect_identical(merged$num_threads, 4L)
  expect_identical(merged$seed, 2025L)
})

test_that("empty or absent inputs are handled", {
  expect_identical(
    fastml:::fastml_merge_engine_args(NULL, list(a = 1)), list(a = 1)
  )
  expect_identical(
    fastml:::fastml_merge_engine_args(list(), list(a = 1)), list(a = 1)
  )
  expect_length(fastml:::fastml_merge_engine_args(NULL, NULL), 0)
})

test_that("quosures that cannot be evaluated are dropped, not fatal", {
  existing <- list(good = rlang::quo(1L), bad = rlang::quo(stop("boom")))
  merged <- fastml:::fastml_merge_engine_args(existing, list(z = 2L))
  expect_identical(merged$good, 1L)
  expect_false("bad" %in% names(merged))
  expect_identical(merged$z, 2L)
})
