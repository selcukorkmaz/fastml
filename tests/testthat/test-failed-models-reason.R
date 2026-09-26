library(testthat)

# rlang and cli errors keep only the header line in `e$message`; the bullet
# lines that name the offending columns are added by conditionMessage(). A
# failure summary built from `e$message` therefore loses the part of the error
# a user needs to diagnose it.

bullet_error_data <- function() {
  set.seed(1)
  data.frame(
    y = factor(rep(c("a", "b"), 30)),
    x = rnorm(60),
    z = factor(sample(c("u", "v"), 60, replace = TRUE))
  )
}

test_that("a recipes prep failure reports the column named in its bullets", {
  skip_on_cran()
  d <- bullet_error_data()
  # step_normalize() on a factor fails inside prep() with a cli error whose
  # `$message` is empty and whose bullets name the column `z`.
  rec <- recipes::recipe(y ~ ., data = d) |>
    recipes::step_normalize(z)

  warnings <- character()
  messages <- character()
  expect_error(
    withCallingHandlers(
      fastml(data = d, label = "y", algorithms = "logistic_reg",
             recipe = rec, resampling_method = "none", verbose = FALSE),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      },
      message = function(m) {
        messages <<- c(messages, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    ),
    "No models were successfully trained"
  )

  training_warning <- grep("Training failed for algorithm", warnings, value = TRUE)
  expect_length(training_warning, 1)
  expect_match(training_warning, "factor variable found: `z`", fixed = TRUE)

  summary_text <- paste(messages, collapse = "")
  expect_match(summary_text, "1 model(s) failed to train", fixed = TRUE)
  expect_match(summary_text, "factor variable found: `z`", fixed = TRUE)
})

test_that("a cli error with bullets is reported in full", {
  skip_on_cran()
  skip_if_not_installed("cli")
  # cli_abort() stores only the header in `$message`; the bullets reach the
  # user through conditionMessage().
  local_mocked_bindings(
    fit = function(object, ...) {
      cli::cli_abort(c(
        "The following variable has the wrong class:",
        "x" = "{.var bad_col} must be numeric, not character.",
        "i" = "Convert {.var bad_col} before training."
      ))
    },
    .package = "parsnip"
  )

  good <- bullet_error_data()[, c("y", "x")]
  warnings <- character()
  captured <- NULL
  expect_error(
    withCallingHandlers(
      fastml(data = good, label = "y", algorithms = "logistic_reg",
             resampling_method = "none", verbose = FALSE),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      },
      message = function(m) {
        captured <<- c(captured, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    ),
    "No models were successfully trained"
  )

  training_warning <- grep("Training failed for algorithm", warnings, value = TRUE)
  expect_length(training_warning, 1)
  expect_match(training_warning, "The following variable has the wrong class:", fixed = TRUE)
  expect_match(training_warning, "`bad_col` must be numeric, not character.", fixed = TRUE)
  expect_match(training_warning, "Convert `bad_col` before training.", fixed = TRUE)

  summary_text <- paste(captured, collapse = "")
  expect_match(summary_text, "`bad_col` must be numeric, not character.", fixed = TRUE)
})

test_that("the failure summary indents continuation lines of a reason", {
  skip_if_not_installed("cli")
  reason <- conditionMessage(tryCatch(
    cli::cli_abort(c(
      "The following variable has the wrong class:",
      "x" = "{.var bad_col} must be numeric, not character."
    )),
    error = function(e) e
  ))
  failed <- list(
    list(algorithm = "logistic_reg", engine = "glm", reason = reason),
    list(algorithm = "rand_forest", engine = "ranger", reason = "single line")
  )

  out <- character()
  withCallingHandlers(
    fastml_report_failed_models(failed),
    message = function(m) {
      out <<- c(out, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  lines <- strsplit(paste(out, collapse = ""), "\n", fixed = TRUE)[[1]]

  header <- grep("^  - logistic_reg \\(glm\\): ", lines)
  expect_length(header, 1)
  expect_match(lines[[header]], "The following variable has the wrong class:", fixed = TRUE)
  # The bullet line follows the header, indented beneath the entry.
  expect_match(lines[[header + 1]], "^      .*`bad_col` must be numeric, not character\\.$")
  expect_true(any(lines == "  - rand_forest (ranger): single line"))
  expect_true(any(grepl("WARNING: 2 model(s) failed to train:", lines, fixed = TRUE)))
})
