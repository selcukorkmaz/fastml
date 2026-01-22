library(testthat)
library(dplyr)

test_that("sanitize errors with invalid input type", {
  expect_error(
    sanitize(123),
    "Input must be a data frame or a character vector"
  )

  expect_error(
    sanitize(list(a = 1, b = 2)),
    "Input must be a data frame or a character vector"
  )
})

test_that("sanitize cleans data frame column names with colons", {
  df <- data.frame(a = 1, `b:c` = 2, check.names = FALSE)

  result <- sanitize(df)

  expect_false("b:c" %in% names(result))
  expect_true("bc" %in% names(result) || "b_c" %in% names(result))
})

test_that("sanitize cleans data frame column names with slashes", {
  df <- data.frame(a = 1, `b/c` = 2, check.names = FALSE)

  result <- sanitize(df)

  expect_false("b/c" %in% names(result))
  expect_true(any(grepl("b_c", names(result))))
})

test_that("sanitize cleans data frame column names with spaces", {
  df <- data.frame(a = 1, `b c` = 2, check.names = FALSE)

  result <- sanitize(df)

  expect_false("b c" %in% names(result))
  expect_true(any(grepl("b_c", names(result))))
})

test_that("sanitize leaves clean column names unchanged", {
  df <- data.frame(a = 1, b_c = 2, normal_name = 3)

  result <- sanitize(df)

  expect_equal(names(result), names(df))
})

test_that("sanitize handles mixed column names", {
  df <- data.frame(
    clean_name = 1,
    `dirty:name` = 2,
    `another/dirty` = 3,
    `with spaces` = 4,
    check.names = FALSE
  )

  result <- sanitize(df)

  expect_equal(names(result)[1], "clean_name")
  expect_false(any(grepl("[: /]", names(result))))
})

test_that("sanitize works with character vectors", {
  vec <- c("clean_name", "dirty:name", "with/slash", "with space")

  result <- sanitize(vec)

  expect_true(is.character(result))
  expect_equal(length(result), 4)
  expect_equal(result[1], "clean_name")
  expect_false(any(grepl("[: /]", result)))
})

test_that("sanitize returns data frame for data frame input", {
  df <- data.frame(a = 1, `b:c` = 2, check.names = FALSE)

  result <- sanitize(df)

  expect_true(is.data.frame(result))
})

test_that("sanitize returns character vector for character input", {
  vec <- c("a", "b:c")

  result <- sanitize(vec)

  expect_true(is.character(result))
  expect_false(is.data.frame(result))
})

test_that("sanitize preserves data in data frame", {
  df <- data.frame(`value:col` = c(1, 2, 3), check.names = FALSE)

  result <- sanitize(df)

  expect_equal(nrow(result), 3)
  expect_equal(result[[1]], c(1, 2, 3))
})

test_that("sanitize handles empty data frame", {
  df <- data.frame()

  result <- sanitize(df)

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), 0)
})

test_that("sanitize handles empty character vector", {
  vec <- character(0)

  result <- sanitize(vec)

  expect_true(is.character(result))
  expect_equal(length(result), 0)
})

test_that("sanitize handles data frame with no special characters", {
  df <- data.frame(a = 1, b = 2, c_d = 3)

  result <- sanitize(df)

  expect_equal(names(result), names(df))
})

test_that("sanitize handles complex special character combinations", {
  vec <- c("a:b/c d", "x::y", "m/n/o")

  result <- sanitize(vec)

  expect_false(any(grepl("[:/]", result)))
  expect_false(any(grepl(" ", result)))
})
