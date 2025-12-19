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
  cf <- suppressWarnings(fastexplain(model, method = "counterfactual", observation = iris[1, ]))
  expect_true(is.list(cf))
})

