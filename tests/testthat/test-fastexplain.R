library(testthat)
library(dplyr)

skip_if_not_installed("lime")
skip_if_not_installed("pdp")
skip_if_not_installed("iml")

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
  expl <- fastexplain(model, method = "lime")
  expect_s3_class(expl, "data.frame")
})

# test_that("ICE plot runs", {
#   expect_silent(fastexplain(model, method = "ice", features = "Sepal.Length"))
# })

# test_that("ALE explanation runs", {
#   expect_silent(fastexplain(model, method = "ale", features = "Sepal.Length"))
# })

test_that("DALEX explanation runs cleanly", {
  expect_error(
    suppressMessages(suppressWarnings(fastexplain(model))),
    regexp = NA
  )
})

