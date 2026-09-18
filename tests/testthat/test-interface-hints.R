# Two interface behaviours that exist to remove silent decisions rather than
# to remove typing.

test_that("event_class is not inferred, but an informative hint is given", {
  skip_on_cran()
  set.seed(1); n <- 120
  mk <- function(l1, l2) {
    d <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
    d$y <- factor(ifelse(d$x1 + rnorm(n, sd = 0.5) > 0, l2, l1), levels = c(l1, l2))
    d
  }
  hints <- function(d, ...) {
    msgs <- character()
    withCallingHandlers(
      invisible(utils::capture.output(suppressWarnings(
        fastml(data = d, label = "y", algorithms = "logistic_reg",
               resampling_method = "none", verbose = FALSE, ...)
      ))),
      message = function(m) {
        msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage")
      }
    )
    grep("event_class", msgs, value = TRUE)
  }
  # Level names that look like a negative/positive pair prompt the hint.
  expect_length(hints(mk("neg", "pos")), 1L)
  # Supplying the argument silences it, since there is nothing to point out.
  expect_length(hints(mk("neg", "pos"), event_class = "second"), 0L)
  # Uninformative level names produce no guess.
  expect_length(hints(mk("alpha", "beta")), 0L)
  # The event actually used is still the documented default, not an inference.
  invisible(utils::capture.output(suppressWarnings(suppressMessages(
    fm <- fastml(data = mk("neg", "pos"), label = "y", algorithms = "logistic_reg",
                 resampling_method = "none", verbose = FALSE)
  ))))
  expect_identical(fm$positive_class, "neg")
})

test_that("test_size_tolerance is an argument rather than a fixed constant", {
  expect_true("test_size_tolerance" %in% names(formals(fastml)))
  expect_equal(eval(formals(fastml)$test_size_tolerance), 0.05)
})
