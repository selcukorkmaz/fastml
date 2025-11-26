library(testthat)
library(dplyr)

skip_if_not_installed("lime")
skip_if_not_installed("pdp")
skip_if_not_installed("iml")
skip_if_not_installed("ceterisParibus")

# small dataset for tests
set.seed(123)
data(iris)
iris <- iris[iris$Species != "setosa", ]
iris$Species <- factor(iris$Species)

model <- fastml(
  data = iris,
  label = "Species",
  algorithms = c("rand_forest"),
  resampling_method = "cv",
  folds = 2
)


test_that("LIME explanation works", {
  expl <- fastexplain(model, method = "lime", new_observation = iris[1, ])
  expect_s3_class(expl, "data.frame")
})

test_that("ICE plot runs", {
  expect_error(
    fastexplain(model, method = "ice", features = "Sepal.Length"),
    regexp = NA
  )
})

test_that("ALE explanation runs", {
  expect_error(
    fastexplain(model, method = "ale", features = "Sepal.Length"),
    regexp = NA
  )
})

test_that("DALEX explanation runs cleanly", {
  expect_error(
    suppressMessages(suppressWarnings(fastexplain(model))),
    regexp = NA
  )
})

test_that("Counterfactual explanation computes counterfactuals", {
  skip_if(!"calculate_counterfactuals" %in% getNamespaceExports("ceterisParibus"),
          "calculate_counterfactuals not available in ceterisParibus")
  cf <- suppressWarnings(fastexplain(model, method = "counterfactual", observation = iris[1, ]))
  expect_true(is.list(cf))
  expect_true("counterfactuals" %in% class(cf$counterfactuals))
})

