library(testthat)
library(dplyr)

# Prevent Rplots.pdf from being created by graphics calls
grDevices::pdf(file = NULL)
withr::defer(grDevices::dev.off())

# small dataset for tests
set.seed(123)
data(iris)

test_that("fastexplore errors with non-data.frame input", {
  expect_error(
    fastexplore("not a data frame"),
    "'data' must be a data.frame"
  )
})

test_that("fastexplore errors with empty data", {
  expect_error(
    fastexplore(iris[0, ]),
    "'data' has no rows to explore"
  )
})

test_that("fastexplore errors with missing label column", {
  expect_error(
    fastexplore(iris, label = "nonexistent"),
    "Label column 'nonexistent' is not present in data"
  )
})

test_that("fastexplore returns expected structure", {
  result <- fastexplore(iris, label = "Species", pairwise_matrix = FALSE)

  expect_true(is.list(result))
  expect_true("data_overview" %in% names(result))
  expect_true("summary_stats" %in% names(result))
  expect_true("missing_data" %in% names(result))
  expect_true("correlation_matrix" %in% names(result))
  expect_true("plots" %in% names(result))
})

test_that("fastexplore data_overview contains expected elements", {
  result <- fastexplore(iris, pairwise_matrix = FALSE)

  expect_true("dimensions" %in% names(result$data_overview))
  expect_true("types" %in% names(result$data_overview))
  expect_true("unique_values" %in% names(result$data_overview))
  expect_true("head" %in% names(result$data_overview))

  expect_equal(result$data_overview$dimensions$Rows, 150)
  expect_equal(result$data_overview$dimensions$Columns, 5)
})

test_that("fastexplore computes summary statistics", {
  result <- fastexplore(iris, pairwise_matrix = FALSE)

  expect_true(!is.null(result$summary_stats))
  expect_true("Column" %in% names(result$summary_stats))
  expect_true("Mean" %in% names(result$summary_stats))
  expect_true("Median" %in% names(result$summary_stats))
  expect_true("SD" %in% names(result$summary_stats))
})

test_that("fastexplore detects missing data", {
  iris_with_na <- iris
  iris_with_na$Sepal.Length[1:10] <- NA

  result <- fastexplore(iris_with_na, pairwise_matrix = FALSE, use_upset_missing = FALSE)

  missing_row <- result$missing_data[result$missing_data$Column == "Sepal.Length", ]
  expect_equal(missing_row$Missing[[1]], 10)
})

test_that("fastexplore computes correlation matrix", {
  result <- fastexplore(iris, pairwise_matrix = FALSE)

  expect_true(!is.null(result$correlation_matrix))
  expect_true(is.matrix(result$correlation_matrix))
  expect_equal(nrow(result$correlation_matrix), 4)  # 4 numeric columns
})

test_that("fastexplore detects high correlations", {
  result <- fastexplore(iris, corr_threshold = 0.8, pairwise_matrix = FALSE)

  expect_true(!is.null(result$high_corr_pairs))
  # Sepal.Length and Petal.Length should be highly correlated
  expect_true(nrow(result$high_corr_pairs) > 0)
})

test_that("fastexplore generates frequency tables for factors", {
  result <- fastexplore(iris, pairwise_matrix = FALSE)

  expect_true(!is.null(result$freq_tables))
  expect_true("Species" %in% names(result$freq_tables))
  expect_equal(nrow(result$freq_tables$Species), 3)  # 3 species
})

test_that("fastexplore detects class imbalance when label is provided", {
  result <- fastexplore(iris, label = "Species", pairwise_matrix = FALSE)

  expect_true(!is.null(result$class_imbalance))
  expect_equal(nrow(result$class_imbalance), 3)
  expect_equal(sum(result$class_imbalance$count), 150)
})

test_that("fastexplore detects duplicated rows", {
  # Create a dataset without pre-existing duplicates
  df_base <- data.frame(
    a = 1:100,
    b = rnorm(100),
    c = letters[rep(1:10, 10)]
  )
  df_with_dups <- rbind(df_base, df_base[1:5, ])

  result <- fastexplore(df_with_dups, pairwise_matrix = FALSE)

  expect_equal(result$duplicated_rows, 5)
  expect_true(!is.null(result$duplicated_examples))
})

test_that("fastexplore performs IQR outlier detection", {
  result <- fastexplore(iris, outlier_method = "iqr", pairwise_matrix = FALSE)

  expect_true(!is.null(result$outlier_summary))
  expect_true(is.numeric(result$outlier_summary))
})

test_that("fastexplore performs zscore outlier detection", {
  result <- fastexplore(iris, outlier_method = "zscore", pairwise_matrix = FALSE)

  expect_true(!is.null(result$outlier_summary))
  expect_true(is.numeric(result$outlier_summary))
})

test_that("fastexplore runs normality tests", {
  result <- fastexplore(iris, run_distribution_checks = TRUE, pairwise_matrix = FALSE)

  expect_true(!is.null(result$normality_tests))
  expect_true("Column" %in% names(result$normality_tests))
  expect_true("P_Value" %in% names(result$normality_tests))
})

test_that("fastexplore generates histogram plots", {
  result <- fastexplore(iris, visualize = "histogram", pairwise_matrix = FALSE)

  expect_true(!is.null(result$plots$histograms))
  expect_true(length(result$plots$histograms) == 4)  # 4 numeric columns
})

test_that("fastexplore generates boxplot plots", {
  result <- fastexplore(iris, visualize = "boxplot", pairwise_matrix = FALSE)

  expect_true(!is.null(result$plots$boxplots))
  expect_true(length(result$plots$boxplots) == 4)
})

test_that("fastexplore generates bar plots for factors", {
  result <- fastexplore(iris, visualize = "barplot", pairwise_matrix = FALSE)

  expect_true(!is.null(result$plots$barplots))
  expect_true("Species" %in% names(result$plots$barplots))
})

test_that("fastexplore generates correlation heatmap", {
  result <- fastexplore(iris, visualize = "heatmap", pairwise_matrix = FALSE)

  expect_true(!is.null(result$plots$correlation_heatmap))
  expect_true(inherits(result$plots$correlation_heatmap, c("gg", "ggplot")))
})

test_that("fastexplore generates scatterplots", {
  result <- fastexplore(iris, visualize = "scatterplot", pairwise_matrix = FALSE)

  expect_true(!is.null(result$plots$scatterplots))
})

test_that("fastexplore respects sample_size parameter", {
  result <- fastexplore(iris, sample_size = 50, pairwise_matrix = FALSE)

  # The result should still work

  expect_true(!is.null(result$data_overview))
})

test_that("fastexplore auto_convert_numeric converts numeric-like factors", {
  df <- data.frame(
    a = factor(c("1", "2", "3", "4", "5", "6", "7", "8")),
    b = c(1, 2, 3, 4, 5, 6, 7, 8)
  )

  result <- fastexplore(df, auto_convert_numeric = TRUE, pairwise_matrix = FALSE)

  # Column 'a' should be converted and appear in summary_stats
  expect_true("a" %in% result$summary_stats$Column)
})

test_that("fastexplore auto_convert_dates converts date-like strings", {
  df <- data.frame(
    date_col = c("2023-01-01", "2023-01-02", "2023-01-03"),
    value = c(1, 2, 3)
  )

  result <- fastexplore(df, auto_convert_dates = TRUE, pairwise_matrix = FALSE)

  # The function should run without error
  expect_true(!is.null(result))
})

test_that("fastexplore feature_engineering creates date features", {
  df <- data.frame(
    date_col = as.Date(c("2021-01-15", "2022-06-20", "2023-12-25")),
    value = c(1, 2, 3)
  )

  result <- fastexplore(df, feature_engineering = TRUE, pairwise_matrix = FALSE, run_distribution_checks = FALSE)

  # date features should be created
  expect_true("date_col_day" %in% result$data_overview$types$Column)
  expect_true("date_col_month" %in% result$data_overview$types$Column)
  expect_true("date_col_year" %in% result$data_overview$types$Column)
})

test_that("fastexplore detects zero variance columns", {
  df <- data.frame(
    constant = rep(5, 100),
    variable = rnorm(100)
  )

  # Suppress expected warning about zero standard deviation in correlation
  result <- suppressWarnings(fastexplore(df, pairwise_matrix = FALSE, run_distribution_checks = FALSE))

  expect_true("constant" %in% result$zero_variance_cols)
})

test_that("fastexplore detects potential ID columns", {
  df <- data.frame(
    id = 1:100,
    value = rnorm(100)
  )

  result <- fastexplore(df, pairwise_matrix = FALSE)

  expect_true("id" %in% result$potential_id_cols)
})

test_that("fastexplore generates grouped plots when label is factor", {
  result <- fastexplore(
    iris,
    label = "Species",
    visualize = c("histogram", "boxplot"),
    grouped_plots = TRUE,
    pairwise_matrix = FALSE
  )

  # Plots should exist
  expect_true(!is.null(result$plots$histograms))
  expect_true(!is.null(result$plots$boxplots))
})
