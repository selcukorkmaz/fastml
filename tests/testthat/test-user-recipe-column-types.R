library(testthat)

# A user recipe records the class of every column it was built on and
# recipes::prep() rejects training data whose classes differ. fastml coerces
# character columns to factors and integers to doubles before splitting, which
# previously made every model fail with "The following variable has the wrong
# class" when the recipe had been built on the caller's unmodified data.

make_typed_data <- function(n = 200) {
  set.seed(3)
  d <- data.frame(
    grp = rep(c("G1", "G2", "G3", "G4"), length.out = n),
    cnt = sample(1:50, n, replace = TRUE),
    x1 = rnorm(n),
    x2 = rnorm(n),
    stringsAsFactors = FALSE
  )
  d$y <- factor(ifelse(d$x1 + rnorm(n) > 0, "a", "b"))
  d
}

test_that("a user recipe built on character and integer predictors trains", {
  skip_on_cran()
  d <- make_typed_data()
  expect_type(d$grp, "character")
  expect_type(d$cnt, "integer")

  rec <- recipes::recipe(y ~ ., data = d) |>
    recipes::step_dummy(recipes::all_nominal_predictors())

  res <- fastml(
    data = d,
    label = "y",
    algorithms = "logistic_reg",
    recipe = rec,
    resampling_method = "cv",
    folds = 5,
    seed = 3
  )

  expect_s3_class(res, "fastml")
  expect_true(length(res$models) >= 1)
  perf <- res$performance[[1]]
  if (!is.data.frame(perf)) perf <- perf[[1]]
  expect_true("accuracy" %in% perf$.metric)

  # The recorded column types are what the fitted recipe was trained on, so
  # prediction on data in the caller's original format must also work.
  preds <- predict(res, newdata = d[1:10, ])
  expect_length(preds, 10)
})

test_that("an integer regression outcome in a user recipe is preserved", {
  skip_on_cran()
  d <- make_typed_data()
  d$y <- d$cnt
  d$cnt <- NULL

  rec <- recipes::recipe(y ~ ., data = d) |>
    recipes::step_dummy(recipes::all_nominal_predictors())

  res <- fastml(
    data = d,
    label = "y",
    algorithms = "linear_reg",
    recipe = rec,
    resampling_method = "cv",
    folds = 5,
    seed = 3
  )

  expect_s3_class(res, "fastml")
  perf <- res$performance[[1]]
  if (!is.data.frame(perf)) perf <- perf[[1]]
  expect_true("rmse" %in% perf$.metric)
})

test_that("a character classification outcome in a user recipe gives a clear error", {
  d <- make_typed_data()
  d$y <- as.character(d$y)
  rec <- recipes::recipe(y ~ ., data = d)

  expect_error(
    fastml(
      data = d,
      label = "y",
      algorithms = "logistic_reg",
      recipe = rec,
      folds = 5
    ),
    "classification requires a factor outcome"
  )
})

test_that("alignment undoes only fastml's own coercions", {
  d <- make_typed_data()
  d$flag <- d$x2 > 0
  rec <- recipes::recipe(y ~ ., data = d)

  coerced <- d
  coerced$grp <- factor(coerced$grp)
  coerced$cnt <- as.numeric(coerced$cnt)
  coerced$flag <- as.numeric(coerced$flag)

  aligned <- fastml:::fastml_align_to_recipe_ptype(coerced, rec, "y", "classification")
  expect_identical(aligned$grp, d$grp)
  expect_identical(aligned$cnt, d$cnt)
  # A mismatch fastml did not introduce is left for recipes to report.
  expect_identical(aligned$flag, coerced$flag)
  expect_identical(aligned$y, d$y)
})
