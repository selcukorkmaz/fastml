# The values a user writes in `tune_params` must be the values that reach the
# engine.
#
# dials stores quantitative parameters on their transformed scale, which for
# `penalty`, `learn_rate`, `loss_reduction`, `cost` and `cost_complexity` is
# base-10 logarithmic. Passing a user's natural-scale value straight to
# `dials::range_set()` therefore reinterprets it as a logarithm, so a requested
# learning rate of 0.05 is fitted as 10^0.05 = 1.122. Nothing in the output
# reports the substitution, and for boosting a learning rate above one produces
# predictions that are wildly overdispersed while a scale-invariant metric such
# as `rsq` still looks unremarkable.
#
# The package's own default ranges are written on the scale dials stores, so
# they must NOT be mapped. Both halves are asserted here.

skip_if_not_installed("bonsai")
skip_if_not_installed("lightgbm")
skip_if_not_installed("glmnet")

toy <- function(n = 200) {
  set.seed(1)
  data.frame(y = rnorm(n), x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
}

test_that("a user-supplied learn_rate reaches the engine unchanged", {
  skip_on_cran()
  library(bonsai)
  fm <- fastml(
    data = toy(), label = "y", algorithms = "lightgbm", metric = "rmse",
    resampling_method = "cv", folds = 3, verbose = FALSE,
    tune_params = list(lightgbm = list(lightgbm = list(
      tree_depth = c(2, 4), learn_rate = c(0.05, 0.1))))
  )
  lr <- workflows::extract_fit_parsnip(fm$models$lightgbm)$fit$params$learning_rate
  expect_gte(lr, 0.05 - 1e-8)
  expect_lte(lr, 0.1 + 1e-8)
  # The specific value the defect produced. A learning rate above one is the
  # visible symptom, so it is pinned directly.
  expect_false(isTRUE(all.equal(lr, 10^0.05)))
  expect_lt(lr, 1)
})

test_that("a user-supplied penalty reaches the engine unchanged", {
  skip_on_cran()
  fm <- fastml(
    data = toy(), label = "y", algorithms = "elastic_net", metric = "rmse",
    resampling_method = "cv", folds = 3, verbose = FALSE,
    tune_params = list(elastic_net = list(glmnet = list(
      penalty = c(0.01, 0.1), mixture = c(0, 0.5, 1))))
  )
  pen <- workflows::extract_fit_parsnip(fm$models$elastic_net)$spec$args$penalty
  pen <- rlang::eval_tidy(pen)
  expect_gte(pen, 0.01 - 1e-8)
  expect_lte(pen, 0.1 + 1e-8)
})

test_that("the package's own default ranges are left on the scale they are written in", {
  skip_on_cran()
  library(bonsai)
  fm <- fastml(
    data = toy(), label = "y", algorithms = "lightgbm", metric = "rmse",
    resampling_method = "cv", folds = 3, verbose = FALSE
  )
  lr <- workflows::extract_fit_parsnip(fm$models$lightgbm)$fit$params$learning_rate
  # The documented default range is [1e-2, 1e-1]. Mapping the defaults a second
  # time would push this to 10^-2 .. 10^-1 of itself and fail here.
  expect_gte(lr, 0.01 - 1e-8)
  expect_lte(lr, 0.1 + 1e-8)
})

test_that("scale mapping handles the domain boundary and refuses invalid values", {
  lr <- dials::learn_rate()
  # A value of zero is meaningful for parameters such as loss_reduction but has
  # no logarithm. It maps to the parameter's lower bound, which is the closest
  # value the parameter can represent, and the result is reported in the
  # recorded grid rather than being hidden.
  mapped <- fastml_to_param_scale(lr, c(0, 0.1))
  expect_equal(mapped[[1]], lr$range$lower)
  expect_equal(mapped[[2]], log10(0.1))
  # A negative value is not a boundary case and is refused.
  expect_error(fastml_to_param_scale(lr, -1), "outside the domain")
  # An untransformed parameter is returned unchanged.
  expect_equal(fastml_to_param_scale(dials::mixture(), c(0, 1)), c(0, 1))
})

test_that("fixed defaults are not exponentiated when another parameter is tuned", {
  skip_on_cran()
  library(bonsai)
  # Supplying any `tune_params` for an algorithm pins the remaining parameters
  # at the fixed defaults from get_default_params(), which are written on the
  # natural scale. Those defaults reach dials by the same route as user values
  # and must be mapped the same way. Before this was fixed, a user who tuned
  # only tree_depth was given a learning rate of 10^0.1 = 1.2589 in place of
  # the intended default of 0.1.
  fm <- fastml(
    data = toy(300), label = "y", algorithms = "lightgbm", metric = "rmse",
    resampling_method = "cv", folds = 3, verbose = FALSE,
    tune_params = list(lightgbm = list(lightgbm = list(tree_depth = c(2, 4))))
  )
  lr <- workflows::extract_fit_parsnip(fm$models$lightgbm)$fit$params$learning_rate
  expect_equal(lr, 0.1, tolerance = 1e-8)
  expect_lt(lr, 1)
})

test_that("the grid that was searched is recorded and marks the source of each value", {
  skip_on_cran()
  library(bonsai)
  fm <- fastml(
    data = toy(300), label = "y", algorithms = "lightgbm", metric = "rmse",
    resampling_method = "cv", folds = 3, verbose = FALSE,
    tune_params = list(lightgbm = list(lightgbm = list(
      tree_depth = c(2, 4), learn_rate = c(0.05, 0.1))))
  )
  grid <- fm$tuning_grid$lightgbm$lightgbm
  expect_false(is.null(grid))
  pars <- grid$parameters
  expect_true(all(c("parameter", "source", "values") %in% names(pars)))
  expect_setequal(unique(pars$source), c("user", "default"))
  lr_row <- pars[pars$parameter == "learn_rate", ]
  expect_identical(lr_row$source, "user")
  # The realized values, not the request, are what is recorded. This is the
  # view under which a substituted grid becomes visible.
  expect_match(lr_row$values, "0.05")
  expect_match(lr_row$values, "0.1")
  td_row <- pars[pars$parameter == "tree_depth", ]
  expect_identical(td_row$values, "2, 4")
})

test_that("more candidate values than grid_levels are all searched", {
  skip_on_cran()
  # grid_levels defaults to 3, and dials truncates to `levels` values, so four
  # requested values would silently lose one without per-parameter levels.
  fm <- fastml(
    data = toy(300), label = "y", algorithms = "elastic_net", metric = "rmse",
    resampling_method = "cv", folds = 3, verbose = FALSE,
    tune_params = list(elastic_net = list(glmnet = list(
      penalty = c(0.001, 0.01, 0.1, 1))))
  )
  vals <- fm$tuning_grid$elastic_net$glmnet$parameters
  pen <- vals[vals$parameter == "penalty", "values"]
  expect_equal(length(strsplit(pen, ",")[[1]]), 4L)
})
