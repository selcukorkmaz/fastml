# Property tests for the article's central structural claim.
#
# The claim is that within a fastml() call a leaky preprocessing
# configuration cannot be expressed. Concretely, every preprocessing
# argument compiles to an UNTRAINED recipes step, so the specification that
# reaches the resampling loop carries no parameters estimated from the full
# dataset, and the one channel that could carry such parameters, a recipe
# already passed through prep(), is rejected before training begins.
#
# These tests assert that property mechanically across the supported values
# of every preprocessing argument, so that a future change which silently
# pre-estimated a step would fail here rather than pass unnoticed.

# Capture the recipe handed to train_models() without forcing the other
# arguments, some of which are unbound outside the survival path.
capture_recipe <- function(...) {
  captured <- new.env(parent = emptyenv())
  orig <- fastml::train_models
  wrapper <- function(recipe, ...) {
    captured$recipe <- recipe
    orig(recipe = recipe, ...)
  }
  assignInNamespace("train_models", wrapper, ns = "fastml")
  on.exit(assignInNamespace("train_models", orig, ns = "fastml"), add = TRUE)
  invisible(utils::capture.output(suppressWarnings(fastml(...))))
  captured$recipe
}

untrained <- function(rec) {
  !isTRUE(rec$trained) &&
    !recipes::fully_trained(rec) &&
    !any(vapply(rec$steps, function(s) isTRUE(s$trained), logical(1)))
}

# Load into an explicit environment rather than a lazily evaluated
# environment(), and skip rather than error when the dataset does not
# materialise. utils::data() only warns when it cannot resolve a name, so
# without this check the failure surfaces later as an opaque "object not
# found" from get(). skip_if_not_installed() cannot cover that case: it
# reports whether the package is installed, not whether its data is reachable.
tox <- function(n = 120) {
  skip_if_not_installed("modeldata")
  env <- new.env(parent = emptyenv())
  suppressWarnings(
    utils::data("steroidogenic_toxicity", package = "modeldata", envir = env)
  )
  if (!exists("steroidogenic_toxicity", envir = env, inherits = FALSE)) {
    skip("modeldata::steroidogenic_toxicity is not available")
  }
  d <- as.data.frame(get("steroidogenic_toxicity", envir = env, inherits = FALSE))
  stats::na.omit(d)[seq_len(n), ]
}

test_that("every preprocessing argument compiles to an untrained recipe", {
  skip_on_cran()
  d <- tox()

  cases <- list(
    scaling_center_scale = list(scaling_methods = c("center", "scale")),
    scaling_center       = list(scaling_methods = "center"),
    impute_median        = list(impute_method = "medianImpute"),
    impute_knn           = list(impute_method = "knnImpute"),
    encode_on            = list(encode_categoricals = TRUE),
    encode_off           = list(encode_categoricals = FALSE),
    balance_upsample     = list(balance_method = "upsample"),
    balance_downsample   = list(balance_method = "downsample")
  )

  for (nm in names(cases)) {
    args <- c(list(data = d, label = "class", algorithms = "logistic_reg",
                   resampling_method = "cv", folds = 3,
                   bootstrap_ci = FALSE, verbose = FALSE),
              cases[[nm]])
    rec <- do.call(capture_recipe, args)
    expect_false(is.null(rec), info = nm)
    expect_true(untrained(rec), info = nm)
  }
})

test_that("a user-supplied recipe also reaches the loop untrained", {
  skip_on_cran()
  d <- tox()
  user_rec <- recipes::recipe(class ~ ., data = d) |>
    recipes::step_impute_median(recipes::all_numeric_predictors()) |>
    recipes::step_normalize(recipes::all_numeric_predictors())

  rec <- capture_recipe(data = d, label = "class", algorithms = "logistic_reg",
                        recipe = user_rec, resampling_method = "cv", folds = 3,
                        bootstrap_ci = FALSE, verbose = FALSE)
  expect_true(untrained(rec))
})

test_that("a prepped recipe is rejected rather than used", {
  skip_on_cran()
  d <- tox()
  prepped <- recipes::prep(
    recipes::recipe(class ~ ., data = d) |>
      recipes::step_normalize(recipes::all_numeric_predictors()),
    training = d
  )
  expect_true(recipes::fully_trained(prepped))

  expect_error(
    suppressWarnings(fastml(data = d, label = "class", algorithms = "logistic_reg",
                            recipe = prepped, resampling_method = "cv", folds = 3,
                            bootstrap_ci = FALSE, verbose = FALSE))
  )
})

# This is the only test in the file that runs on CRAN, so it deliberately
# builds its own data. The property under test is about recipe state, not
# about any particular dataset, so depending on one only adds a way to fail.
test_that("the guard recognises a trained recipe by any of its markers", {
  set.seed(42)
  n <- 60
  d <- data.frame(
    x1 = stats::rnorm(n),
    x2 = stats::rnorm(n),
    class = factor(rep(c("toxic", "nontoxic"), length.out = n))
  )
  raw <- recipes::recipe(class ~ ., data = d) |>
    recipes::step_normalize(recipes::all_numeric_predictors())
  expect_false(fastml:::fastml_recipe_is_trained(raw))
  expect_true(fastml:::fastml_recipe_is_trained(recipes::prep(raw, training = d)))
})

# ------------------------------------------------------------------
# The boundary of the property asserted above.
#
# Everything before this point concerns the package's own preprocessing
# arguments, which compile to untrained steps that are estimated inside the
# resampling loop. The `recipe` argument is different in kind. A user-supplied
# recipe is prepped inside the loop like any other, so its placement is
# guarded, but a custom step can resolve a value from its enclosing environment
# at bake time, and that value may have been computed on the whole dataset.
#
# This is a known and documented limitation, not a defect to be fixed by a
# check. Detecting it in general means deciding what an arbitrary closure reads
# at evaluation time. The test exists so that the limitation stays visible and
# the claim in the article is not quietly widened.
# ------------------------------------------------------------------

test_that("a user recipe can carry full-data information past every guard", {
  skip_on_cran()
  set.seed(11); n <- 400
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  d$grp <- factor(sample(paste0("g", 1:40), n, replace = TRUE))
  d$y <- factor(ifelse(plogis(0.4 * d$x1) > runif(n), "pos", "neg"),
                levels = c("neg", "pos"))

  # Built inside a function, so the encoding lives in that function's
  # environment and carries no global-environment reference.
  build_leaky <- function(full_data) {
    enc <- tapply(as.numeric(full_data$y) - 1, full_data$grp, mean)
    recipes::recipe(y ~ ., data = full_data) |>
      recipes::step_mutate(grp_enc = unname(enc[as.character(grp)])) |>
      recipes::step_rm(grp)
  }
  rec <- build_leaky(d)

  # It is untrained, so the pretrained-recipe check does not apply.
  expect_false(isTRUE(rec$trained) || recipes::fully_trained(rec))
  # The scanner finds nothing, because there is nothing of its kind to find.
  flagged <- fastml_detect_leaky_recipe_steps(rec)
  expect_true(is.null(flagged) || length(flagged) == 0)
  # And the validator accepts it.
  expect_silent(fastml_validate_user_recipe(rec))
})
