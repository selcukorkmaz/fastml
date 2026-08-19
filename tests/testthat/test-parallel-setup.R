library(testthat)

# Regression tests for fastml_setup_parallel(). These are intentionally not
# gated behind skip_on_cran(): they call the setup/restore helper directly
# rather than fitting models, so they are fast enough to run everywhere, and
# they cover two defects that shipped in 0.7.8 precisely because the only test
# touching the parallel path was CRAN-gated.
#
#   1. registerDoFuture() was called with a `flavor` argument it does not
#      accept, so every n_cores > 1 call failed with "unused argument".
#   2. When no foreach backend existed beforehand, the cleanup handler passed
#      NULL data/info hooks back to setDoPar(), after which getDoParName()
#      errored for the rest of the session.

test_that("sequential setup registers and restores without error", {
  skip_if_not_installed("future")

  handle <- fastml:::fastml_setup_parallel(n_cores = 1, seed = 42)
  expect_false(handle$use_parallel)
  expect_type(handle$restore, "closure")

  expect_silent(handle$restore())
})

test_that("parallel setup registers a doFuture backend", {
  skip_if_not_installed("future")
  skip_if_not_installed("doFuture")
  skip_if_not_installed("foreach")

  handle <- fastml:::fastml_setup_parallel(n_cores = 2, seed = 42)
  on.exit(handle$restore(), add = TRUE)

  expect_true(handle$use_parallel)
  expect_identical(foreach::getDoParName(), "doFuture")
})

test_that("restore leaves foreach in a usable state", {
  skip_if_not_installed("future")
  skip_if_not_installed("doFuture")
  skip_if_not_installed("foreach")

  handle <- fastml:::fastml_setup_parallel(n_cores = 2, seed = 42)
  handle$restore()

  # The failure mode this guards against is getDoParName() raising
  # "attempt to apply non-function" rather than returning a backend name.
  expect_error(foreach::getDoParName(), NA)
  expect_true(is.character(foreach::getDoParName()))
})

test_that("restore returns the future plan and future.seed option", {
  skip_if_not_installed("future")

  old_seed_option <- getOption("future.seed")
  handle <- fastml:::fastml_setup_parallel(n_cores = 1, seed = 7)
  handle$restore()

  expect_identical(getOption("future.seed"), old_seed_option)
  expect_true(inherits(future::plan(), "future"))
})

test_that("a foreach backend registered by the caller is restored", {
  skip_if_not_installed("future")
  skip_if_not_installed("doFuture")
  skip_if_not_installed("foreach")

  foreach::registerDoSEQ()
  before <- foreach::getDoParName()

  handle <- fastml:::fastml_setup_parallel(n_cores = 2, seed = 42)
  handle$restore()

  expect_identical(foreach::getDoParName(), before)
})

# n_cores sizes the worker pool; engine_threads sizes each engine's thread pool.
# Driving both from n_cores meant a request for k cores could demand k^2 threads.

test_that("engine_threads, not n_cores, sets the engine thread count", {
  for (et in c(1L, 2L, 4L)) {
    ea <- fastml:::fastml_apply_engine_seed(
      list(), "rand_forest", "ranger", seed = 2025, n_cores = et,
      task = "regression"
    )
    expect_identical(as.integer(ea$num.threads), et)
  }
})

test_that("both arguments are exposed with sequential defaults", {
  f <- formals(fastml::fastml)
  expect_true("engine_threads" %in% names(f))
  expect_identical(eval(f$n_cores), 1)
  expect_identical(eval(f$engine_threads), 1)
  expect_true("engine_threads" %in% names(formals(fastml::train_models)))
})

test_that("results are invariant to the worker/thread split", {
  # Gated: this fits three models to check a property, where the tests above
  # catch the defects. It costs ~20s against ~4s for the whole rest of the file.
  skip_on_cran()
  skip_if_not_installed("ranger")
  skip_if_not_installed("future")

  d <- iris[iris$Species != "virginica", ]
  d$Species <- factor(d$Species)
  est <- function(nc, et) {
    set.seed(7)
    m <- suppressWarnings(suppressMessages(fastml(
      data = d, label = "Species", algorithms = "rand_forest",
      n_cores = nc, engine_threads = et, seed = 42,
      verbose = FALSE, bootstrap_ci = FALSE
    )))
    p <- as.data.frame(m$performance[[1]])
    p$.estimate[p$.metric == "roc_auc"][1]
  }
  base <- est(1, 1)
  expect_equal(est(2, 1), base, tolerance = 0)
  expect_equal(est(1, 2), base, tolerance = 0)
})
