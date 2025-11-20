#' Explore and Summarize a Dataset Quickly
#'
#' \code{fastexplore} provides a fast and comprehensive exploratory data analysis (EDA) workflow.
#' It automatically detects variable types, checks for missing and duplicated data,
#' suggests potential ID columns, and provides a variety of plots (histograms, boxplots,
#' scatterplots, correlation heatmaps, etc.). It also includes optional outlier detection,
#' normality testing, and feature engineering.
#'
#' @param data A \code{data.frame}. The dataset to analyze.
#' @param label A character string specifying the name of the target or label column (optional).
#'   If provided, certain grouped plots and class imbalance checks will be performed.
#' @param visualize A character vector specifying which visualizations to produce.
#'   Possible values: \code{c("histogram", "boxplot", "barplot", "heatmap", "scatterplot")}.
#' @param save_results Logical. If \code{TRUE}, saves plots and a rendered report (HTML) into
#'   a timestamped \code{EDA_Results_} folder inside \code{output_dir}.
#' @param output_dir A character string specifying the output directory for saving results
#'   (if \code{save_results = TRUE}). Defaults to current working directory.
#' @param sample_size An integer specifying a random sample size for the data to be used in
#'   visualizations. If \code{NULL}, uses the entire dataset.
#' @param interactive Logical. If \code{TRUE}, attempts to produce interactive Plotly heatmaps
#'   and other interactive elements. If required packages are not installed, falls back to static plots.
#' @param corr_threshold Numeric. Threshold above which correlations (in absolute value)
#'   are flagged as high. Defaults to \code{0.9}.
#' @param auto_convert_numeric Logical. If \code{TRUE}, automatically converts factor/character
#'   columns that look numeric (only digits, minus sign, or decimal point) to numeric.
#' @param visualize_missing Logical. If \code{TRUE}, attempts to visualize missingness patterns
#'   (e.g., via an \code{UpSet} plot, if \pkg{UpSetR} is available, or \pkg{VIM}, \pkg{naniar}).
#' @param imputation_suggestions Logical. If \code{TRUE}, prints simple text suggestions for imputation strategies.
#' @param report_duplicate_details Logical. If \code{TRUE}, shows top duplicated rows and their frequency.
#' @param detect_near_duplicates Logical. Placeholder for near-duplicate (fuzzy) detection.
#'   Currently not implemented.
#' @param auto_convert_dates Logical. If \code{TRUE}, attempts to detect and convert date-like
#'   strings (\code{YYYY-MM-DD}) to \code{Date} format.
#' @param feature_engineering Logical. If \code{TRUE}, automatically engineers derived features
#'   (day, month, year) from any date/time columns, and identifies potential ID columns.
#' @param outlier_method A character string indicating which outlier detection method(s) to apply.
#'   One of \code{c("iqr", "zscore", "dbscan", "lof")}. Only the first match will be used in the code
#'   (though the function is designed to handle multiple).
#' @param run_distribution_checks Logical. If \code{TRUE}, runs normality tests (e.g., Shapiro-Wilk)
#'   on numeric columns.
#' @param normality_tests A character vector specifying which normality tests to run.
#'   Possible values include \code{"shapiro"} or \code{"ks"} (Kolmogorov-Smirnov).
#'   Only used if \code{run_distribution_checks = TRUE}.
#' @param pairwise_matrix Logical. If \code{TRUE}, produces a scatterplot matrix (using \pkg{GGally})
#'   for numeric columns.
#' @param max_scatter_cols Integer. Maximum number of numeric columns to include in the pairwise matrix.
#' @param grouped_plots Logical. If \code{TRUE}, produce grouped histograms, violin plots,
#'   and density plots by label (if the label is a factor).
#' @param use_upset_missing Logical. If \code{TRUE}, attempts to produce an UpSet plot for missing data
#'   if \pkg{UpSetR} is available.
#'
#' @details
#' This function automates many steps of EDA:
#' \enumerate{
#'   \item Automatically detects numeric vs. categorical variables.
#'   \item Auto-converts columns that look numeric (and optionally date-like).
#'   \item Summarizes data structure, missingness, duplication, and potential ID columns.
#'   \item Computes correlation matrix and flags highly correlated pairs.
#'   \item (Optional) Outlier detection using IQR, Z-score, DBSCAN, or LOF methods.
#'   \item (Optional) Normality tests on numeric columns.
#'   \item Saves all results and an R Markdown report if \code{save_results = TRUE}.
#' }
#'
#' @return A (silent) list containing:
#' \itemize{
#'   \item \code{data_overview} - A basic overview (head, unique values, skim summary).
#'   \item \code{summary_stats} - Summary statistics for numeric columns.
#'   \item \code{freq_tables} - Frequency tables for factor columns.
#'   \item \code{missing_data} - Missing data overview (count, percentage).
#'   \item \code{duplicated_rows} - Count of duplicated rows.
#'   \item \code{class_imbalance} - Class distribution if \code{label} is provided and is categorical.
#'   \item \code{correlation_matrix} - The correlation matrix for numeric variables.
#'   \item \code{zero_variance_cols} - Columns with near-zero variance.
#'   \item \code{potential_id_cols} - Columns with unique values in every row.
#'   \item \code{date_time_cols} - Columns recognized as date/time.
#'   \item \code{high_corr_pairs} - Pairs of variables with correlation above \code{corr_threshold}.
#'   \item \code{outlier_method} - The chosen method for outlier detection.
#'   \item \code{outlier_summary} - Outlier proportions or metrics (if computed).
#' }
#' If \code{save_results = TRUE}, additional side effects include saving figures, a correlation heatmap,
#' and an R Markdown report in the specified directory.
#'
#' @importFrom dplyr mutate across summarise n row_number filter group_by ungroup summarise_all select arrange case_when bind_cols bind_rows rename everything left_join right_join inner_join full_join distinct tibble rowwise n_distinct add_row
#' @importFrom tidyr pivot_longer
#' @importFrom skimr skim
#' @importFrom DT datatable formatStyle styleEqual formatRound datatable
#' @importFrom ggplot2 ggplot aes aes_string geom_histogram geom_boxplot geom_bar labs theme_minimal scale_fill_gradient2 coord_fixed geom_tile geom_point stat_function stat_qq_line stat_qq geom_violin geom_density after_stat ggsave
#' @importFrom broom tidy
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling
#' @importFrom rmarkdown render
#' @importFrom gridExtra grid.arrange
#' @importFrom ggpubr ggarrange
#' @importFrom naniar vis_miss
#' @importFrom moments skewness kurtosis
#' @importFrom UpSetR upset
#' @importFrom VIM aggr
#' @importFrom plotly plot_ly
#' @importFrom reshape2 melt
#' @importFrom htmlwidgets saveWidget
#' @importFrom dbscan dbscan lof
#' @importFrom scales percent_format percent
#' @importFrom knitr opts_chunk
#' @importFrom grDevices colorRamp
#' @importFrom stats cor density dnorm ks.test median na.omit p.adjust quantile sd shapiro.test var
#' @importFrom utils head
#'
#' @export

fastexplore <- function(
    data,
    label = NULL,
    visualize = c("histogram", "boxplot", "barplot", "heatmap", "scatterplot"),
    save_results = TRUE,
    output_dir = NULL,
    sample_size = NULL,
    interactive = FALSE,
    corr_threshold = 0.9,
    auto_convert_numeric = TRUE,   # Auto-convert numeric-like columns from factor/char to numeric
    visualize_missing = TRUE,  # Visualize missingness patterns
    imputation_suggestions = FALSE,  # Provide simple imputation suggestions
    report_duplicate_details = TRUE,# Show top duplicate rows and frequency
    detect_near_duplicates = TRUE,  # Placeholder for near-duplicate (fuzzy) detection
    auto_convert_dates = FALSE,  # If TRUE, tries to detect date strings (YYYY-MM-DD) and convert them to Date
    feature_engineering = FALSE,  # If TRUE, create derived columns (day, month, year) from date/time + ID col suggestions
    outlier_method = c("iqr", "zscore", "dbscan", "lof"),
    run_distribution_checks = TRUE,  # If TRUE, run normality tests (Shapiro-Wilk, etc.)
    normality_tests = c("shapiro"), # Which normality tests to run if run_distribution_checks=TRUE
    pairwise_matrix = TRUE,  # If TRUE, produce a scatterplot matrix for numeric columns
    max_scatter_cols = 5,      # Limit how many numeric columns to include in the pairwise matrix
    grouped_plots = TRUE,  # If TRUE, produce grouped histograms, violin plots, density plots by label (if factor)
    use_upset_missing = TRUE   # If TRUE, try to produce an UpSet plot for missing data if UpSetR is available
) {
  outlier_method <- match.arg(outlier_method)

  ## ------------------------------------------------------------------------
  ## 1. Error Handling and Basic Checks
  ## ------------------------------------------------------------------------
  if (!is.data.frame(data)) {
    stop("Error: The input 'data' must be a data frame.")
  }

  if (nrow(data) == 0) {
    stop("Error: The data frame is empty. No rows to analyze.")
  }


  data <- data %>%
    mutate(
      across(where(is.character), as.factor),
      across(where(is.integer), as.numeric)
    )

  # Convert numeric columns with <5 unique values to factor
  data <- data %>%
    mutate(across(where(is.numeric) &
                    where(~ n_distinct(.) < 6),
                  as.factor))


  target_var <- data[[label]]


  if (is.numeric(target_var) && length(unique(target_var)) <= 5) {
    # Convert target_var to factor
    target_var <- as.factor(target_var)
    data[[label]] = as.factor(data[[label]])

    # Issue a warning to inform the user about the change
    warning(sprintf("The target variable '%s' is numeric with %d unique values. It has been converted to a factor.",
                    label, length(unique(target_var))))
  }

  # Identify numeric and categorical columns
  numeric_cols <- names(data)[vapply(data, is.numeric, logical(1))]
  factor_cols  <- names(data)[vapply(data, function(x)
    is.factor(x) || is.character(x), logical(1))]

  if (length(numeric_cols) == 0 & length(factor_cols) == 0) {
    stop("Error: The dataset has no numeric or categorical columns.")
  }

  # Check label (formerly target)
  if (!is.null(label) && !(label %in% colnames(data))) {
    stop(paste0(
      "Error: The specified label column '",
      label,
      "' does not exist in the data."
    ))
  }

  # Optionally sample the data if sample_size is specified
  if (!is.null(sample_size)) {
    if (sample_size < nrow(data)) {
      set.seed(123)  # For reproducibility
      data_vis <- data[sample(nrow(data), sample_size), ]
    } else {
      data_vis <- data
    }
  } else {
    data_vis <- data
  }

  ## Create output folder if save_results = TRUE
  if (save_results) {
    timestamp       <- format(Sys.time(), "%Y%m%d_%H%M%S")

     if(is.null(output_dir)){

      output_dir = getwd()
    }

    results_folder  <- file.path(output_dir, paste0("EDA_Results_", timestamp))
    dir.create(results_folder,
               showWarnings = FALSE,
               recursive = TRUE)

    dir.create(paste0(results_folder, "/Boxplot"),
               showWarnings = FALSE,
               recursive = TRUE)

    dir.create(paste0(results_folder, "/Histogram"),
               showWarnings = FALSE,
               recursive = TRUE)

    dir.create(paste0(results_folder, "/Barplot"),
               showWarnings = FALSE,
               recursive = TRUE)

    dir.create(paste0(results_folder, "/ScatterPlot"),
               showWarnings = FALSE,
               recursive = TRUE)

    dir.create(paste0(results_folder, "/Heatmap"),
               showWarnings = FALSE,
               recursive = TRUE)
  }

  ## ------------------------------------------------------------------------
  ## 1a. Mixed Data Types / Validation
  ## ------------------------------------------------------------------------
  # Attempt to auto-convert factor/character columns that appear numeric
  if (auto_convert_numeric) {
    for (col in factor_cols) {
      x <- data[[col]]
      # Check if all non-NA values match a numeric pattern (digits, decimal, minus sign)
      if (all(grepl("^[0-9.-]+$", x[!is.na(x)])) & length(unique(x)) >= 6) {
        new_x <- suppressWarnings(as.numeric(as.character(x)))
        old_na_count <- sum(is.na(x))
        new_na_count <- sum(is.na(new_x))
        if (new_na_count <= old_na_count) {
          data[[col]] <- new_x
          message(paste(
            "Column",
            col,
            "was auto-converted from factor/char to numeric."
          ))
        }
      }
    }

    # Re-identify numeric_cols and factor_cols after conversions
    numeric_cols <- names(data)[vapply(data, is.numeric, logical(1))]
    factor_cols  <- names(data)[vapply(data, function(x)
      is.factor(x) || is.character(x), logical(1))]
  }

  # (NEW) Auto-convert date-like character columns (e.g., YYYY-MM-DD)
  if (auto_convert_dates) {
    for (col in factor_cols) {
      x <- data[[col]]
      # Simple regex for YYYY-MM-DD. Could add more checks for time, etc.
      if (all(grepl("^\\d{4}-\\d{2}-\\d{2}$", x[!is.na(x)]))) {
        converted_date <- as.Date(x, format = "%Y-%m-%d")
        if (!all(is.na(converted_date))) {
          data[[col]] <- converted_date
          message(paste(
            "Column",
            col,
            "was auto-converted from character to Date."
          ))
        }
      }
    }

    # Re-identify numeric_cols and factor_cols after date conversions
    numeric_cols <- names(data)[vapply(data, is.numeric, logical(1))]
    factor_cols  <- names(data)[vapply(data, function(x)
      is.factor(x) || is.character(x), logical(1))]
  }

  # Detect factor columns that may be incorrectly coded (e.g., too many distinct levels)
  if (length(factor_cols) > 0) {
    for (col in factor_cols) {
      n_levels <- length(unique(data[[col]]))
      if (n_levels > nrow(data) * 0.8) {
        message(
          paste(
            "Column",
            col,
            "has a large number of distinct levels (",
            n_levels,
            ") and may be incorrectly coded as factor."
          )
        )
      }
    }
  }

  ## ------------------------------------------------------------------------
  ## 2. Data Overview
  ## ------------------------------------------------------------------------
  # Enhanced Data Overview

  # 1. Basic Dimensions and Structure
  data_dimensions <- tibble::tibble(
    Rows = nrow(data),
    Columns = ncol(data)
  )

  data_column_names <- names(data)

  data_types <- data %>%
    summarise_all(class) %>%
    pivot_longer(cols = everything(), names_to = "Column", values_to = "Data_Type")

  # 2. Missing Values
  missing_values <- data %>%
    summarise_all(~ sum(is.na(.))) %>%
    pivot_longer(cols = everything(), names_to = "Column", values_to = "Missing_Values")

  # 3. Unique Values per Column
  unique_values <- data %>%
    summarise_all(~ n_distinct(.)) %>%
    pivot_longer(cols = everything(), names_to = "Column", values_to = "Unique_Values")

  # 4. Summary Statistics using skimr
  data_summary <- skim(data)

  # 5. Head of Data
  data_head <- head(data, 6)

  # Combine all components into a list
  data_overview <- list(
    Head = data_head,
    Unique_Values = unique_values,
    Summary = data_summary
  )


  ## ------------------------------------------------------------------------
  ## 3. Detection of Potential ID Columns and Date/Time Columns
  ## ------------------------------------------------------------------------
  potential_id_cols <- character()
  date_time_cols    <- character()

  for (col in names(data)) {
    # Potential ID columns: unique values in every row
    if (length(unique(data[[col]])) == nrow(data)) {
      potential_id_cols <- c(potential_id_cols, col)
    }

    # Detect date/time class
    if (inherits(data[[col]], "Date") ||
        inherits(data[[col]], "POSIXct") ||
        inherits(data[[col]], "POSIXt")) {
      date_time_cols <- c(date_time_cols, col)
    }
  }



  # (NEW) Feature Engineering Suggestions
  if (feature_engineering) {

    if (length(date_time_cols) > 0) {
      for (dc in date_time_cols) {
        data[[paste0(dc, "_day")]]   <- as.numeric(format(data[[dc]], "%d"))
        data[[paste0(dc, "_month")]] <- as.numeric(format(data[[dc]], "%m"))
        data[[paste0(dc, "_year")]]  <- as.numeric(format(data[[dc]], "%Y"))
      }
      # Update numeric columns (the newly added day/month/year are numeric)
      numeric_cols <- names(data)[vapply(data, is.numeric, logical(1))]
    }
  }

  ## ------------------------------------------------------------------------
  ## 4. Summary Statistics (Numeric) and Zero/Near-Zero Variance
  ## ------------------------------------------------------------------------
  summary_stats      <- NULL
  zero_variance_cols <- character()

  if (length(numeric_cols) > 0) {

    if (!requireNamespace("moments", quietly = TRUE)) {
      message("Package 'moments' not installed. Skewness and kurtosis won't be computed.")
      summary_numeric <- data.frame(
        Mean   = sapply(data[numeric_cols], mean, na.rm = TRUE),
        Median = sapply(data[numeric_cols], median, na.rm = TRUE),
        SD     = sapply(data[numeric_cols], sd, na.rm = TRUE),
        Min    = sapply(data[numeric_cols], min, na.rm = TRUE),
        Max    = sapply(data[numeric_cols], max, na.rm = TRUE),
        NAs    = sapply(data[numeric_cols], function(x)
          sum(is.na(x)))
      )
    } else {
      summary_numeric <- data.frame(
        Mean      = sapply(data[numeric_cols], mean, na.rm = TRUE),
        Median    = sapply(data[numeric_cols], median, na.rm = TRUE),
        SD        = sapply(data[numeric_cols], sd, na.rm = TRUE),
        Min       = sapply(data[numeric_cols], min, na.rm = TRUE),
        Max       = sapply(data[numeric_cols], max, na.rm = TRUE),
        Skewness  = sapply(data[numeric_cols], skewness, na.rm = TRUE),
        Kurtosis  = sapply(data[numeric_cols], kurtosis, na.rm = TRUE),
        NAs       = sapply(data[numeric_cols], function(x)
          sum(is.na(x)))
      )
    }

    # Check for zero or near-zero variance columns
    variances <- sapply(data[numeric_cols], var, na.rm = TRUE)
    epsilon   <- 1e-8
    zero_variance_cols <- names(variances[variances <= epsilon |
                                            is.na(variances)])


    summary_stats <- summary_numeric
  }
  ## (NEW) Extended Descriptive Statistics: Distribution Checks
  if (run_distribution_checks && length(numeric_cols) > 0) {


    # Perform normality tests
    distribution_tests_results_df <- perform_normality_tests(
      data = data,
      numeric_cols = numeric_cols,
      normality_tests = normality_tests
    )

    if (!is.null(distribution_tests_results_df)) {

      # Create an interactive datatable
      normality_table <- datatable(
        distribution_tests_results_df,
        extensions = 'Buttons',
        options = list(
          pageLength = 10,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          order = list(list(0, 'asc'), list(1, 'asc'))
        ),
        caption = 'Normality Test Results for Numeric Columns'
      ) %>%
        formatRound(c('P_Value', 'Adjusted_P_Value'), digits = 4) %>%
        formatStyle(
          'Normal',
          target = 'row',
          backgroundColor = styleEqual(c("Yes", "No"), c('lightgreen', 'salmon'))
        )

    }

    normality_plots <- generate_normality_plots(data, numeric_cols)


  }

  ## ------------------------------------------------------------------------
  ## 5. Frequency Tables (Categorical)
  ## ------------------------------------------------------------------------
  freq_tables <- NULL
  if (length(factor_cols) > 0) {
    freq_list <- lapply(factor_cols, function(col) {
      tbl <- table(data[[col]], useNA = "ifany")
      return(tbl)
    })
    names(freq_list) <- factor_cols

    freq_tables <- freq_list
  }

  ## ------------------------------------------------------------------------
  ## 6. Missing Data Analysis
  ## ------------------------------------------------------------------------
  missing_data <- data.frame(
    Column       = names(data),
    MissingCount = sapply(data, function(x)
      sum(is.na(x))),
    MissingPct   = sapply(data, function(x)
      mean(is.na(x)) * 100)
  )

  # (NEW) Additional Missing Data Visualization (UpSetR)
  # if (use_upset_missing) {
  #   if (!requireNamespace("UpSetR", quietly = TRUE)) {
  #     message(
  #       "Package 'UpSetR' is not installed. Skipping UpSet-based missingness visualization."
  #     )
  #   } else {
      # Attempt to produce an upset plot of missingness
      # We'll construct a logical matrix of missingness

  #   }
  # }

  if (visualize_missing) {

    ## Upset Plot
    missing_matrix <- sapply(data, is.na)

    if (any(missing_matrix)) {

      # Convert to data.frame
      missing_df <- as.data.frame(missing_matrix)
      # Because UpSetR uses sets, we convert each column to factor(0/1)
      for (cname in names(missing_df)) {
        missing_df[[cname]] <- ifelse(missing_df[[cname]] == TRUE, 1, 0)
      }
      # The typical usage is upset(missing_df, sets=names(missing_df)), but for large sets this might be big
      # We'll do a minimal version:
      if (ncol(missing_df) > 20) {
        message("Too many columns for a neat UpSetR plot. Consider subsetting.")
      } else {
        tryCatch({
          upsetPlot <- upset(missing_df)
        }, error = function(e) {
          message("Error generating UpSet plot: ", e$message)
          upsetPlot <<- NULL  # başarısızsa yine de NULL olarak tut
        })
      }
    }else{
      missing_df = NULL
    }

    miss_plot <- vis_miss(data)


  }

  ## ------------------------------------------------------------------------
  ## 7. Duplicated Rows
  ## ------------------------------------------------------------------------
  dup_count <- sum(duplicated(data))

  if (report_duplicate_details && dup_count > 0) {
    duplicates_all <- data[duplicated(data) |
                             duplicated(data, fromLast = TRUE), ]
    duplicates_all$dup_key <- apply(duplicates_all, 1, function(row)
      paste(row, collapse = "_SEP_"))
    freq_table <- table(duplicates_all$dup_key)
    freq_table <- sort(freq_table, decreasing = TRUE)
    duplicates_all$dup_key <- NULL
  }

  # if (detect_near_duplicates) {
  #   message("Near-duplicate detection is not implemented. Consider fuzzy matching, string distance, etc.")
  # }

  ## ------------------------------------------------------------------------
  ## 8. Class Imbalance (if label is provided)
  ## ------------------------------------------------------------------------
  class_imbalance <- NULL
  if (!is.null(label)) {
    # If label is in factor_cols, print table
    if (label %in% factor_cols) {
      tbl_label <- table(data[[label]], useNA = "ifany")
      class_imbalance <- tbl_label
    }
  }

  ## ------------------------------------------------------------------------
  ## 9. Correlation Matrix, Heatmap & High-Correlation Pairs
  ## ------------------------------------------------------------------------
  correlation_matrix <- NULL
  high_corr_pairs    <- NULL

  if (length(numeric_cols) > 1 && "heatmap" %in% visualize) {

    correlation_matrix <- cor(data[numeric_cols], use = "pairwise.complete.obs")

    corr_mat_upper <- correlation_matrix
    corr_mat_upper[lower.tri(corr_mat_upper, diag = TRUE)] <- NA
    corr_df <- as.data.frame(as.table(corr_mat_upper))
    corr_df <- corr_df[!is.na(corr_df$Freq), ]
    names(corr_df) <- c("Var1", "Var2", "Corr")
    high_corr_pairs <- subset(corr_df, abs(Corr) > corr_threshold)


    if (interactive) {
      if (!requireNamespace("plotly", quietly = TRUE)) {
        message("Package 'plotly' not installed. Falling back to static ggplot2 heatmap.")
      } else {
        heatmap_plot <- plot_ly(
          x = colnames(correlation_matrix),
          y = rownames(correlation_matrix),
          z = correlation_matrix,
          type = "heatmap",
          colors = colorRamp(c("blue", "white", "red"))
        )
        if (save_results) {
          saveWidget(heatmap_plot,
                                  file = file.path(results_folder, "correlation_heatmap.html"))
        }
      }
    }

    if (!interactive ||
        !requireNamespace("plotly", quietly = TRUE)) {
      if (!requireNamespace("reshape2", quietly = TRUE)) {
        stop("Package 'reshape2' must be installed to create static heatmap.")
      }
      corr_melt <- melt(correlation_matrix)
      heatmap_plot <- ggplot(
        corr_melt,
        aes(x = .data$Var1, y = .data$Var2, fill = .data$value)
      ) +
        geom_tile(color = "white") +
        scale_fill_gradient2(
          low = "blue",
          mid = "white",
          high = "red",
          midpoint = 0
        ) +
        theme_minimal() +
        coord_fixed() +
        labs(title = "Correlation Heatmap", x = "", y = "")

      if(save_results){
      save_plot(heatmap_plot, results_folder, "Heatmap", "correlation_heatmap.png")
      }
    }
  }

  ## ------------------------------------------------------------------------
  ## 10. Outlier Detection (Proportion) in Numeric Columns
  ## ------------------------------------------------------------------------
  # (NEW) Additional outlier detection methods
  # iqr  : existing simple method [Q1-1.5*IQR, Q3+1.5*IQR]
  # zscore : absolute zscore > 3
  # dbscan : density-based
  # lof    : local outlier factor
  # none   : skip

  outlier_proportions <- NULL
  outlier_summary <- list()

  if (length(numeric_cols) > 0) {

    outlier_summary <- perform_outlier_detection(data = data,
                                                 numeric_cols = numeric_cols,
                                                 outlier_methods = outlier_method)
  }

  ## ------------------------------------------------------------------------
  ## 11. Visualizations (Histograms, Boxplots, Barplots, Scatterplots)
  ## ------------------------------------------------------------------------
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    message("Package 'ggplot2' is required for plots. Please install it to view plots.")
  } else {

    # 11a. Histograms for numeric variables
    if ("histogram" %in% visualize && length(numeric_cols) > 0) {
      for (col in numeric_cols) {
        # Skip histogram for label column if numeric
        if (!is.null(label) && col == label) {
          next
        }
        p <- ggplot(data_vis, aes_string(x = col)) +
          geom_histogram(
            bins = 30,
            fill = "steelblue",
            color = "white"
          ) +
          labs(title = paste("Histogram of", col),
               x = col,
               y = "Count") +
          theme_minimal()

        # If label is provided and factor, we can facet wrap
        if (!is.null(label) &&
            label %in% factor_cols && grouped_plots) {
          p <- p + facet_wrap(as.formula(paste("~", label)))
        }

        if(save_results){
        save_plot(p, results_folder, "Histogram", paste0("histogram_", col, ".png"))
        }
      }
    }

    # 11b. Boxplots for numeric variables
    if ("boxplot" %in% visualize && length(numeric_cols) > 0) {
      for (col in numeric_cols) {
        if (!is.null(label) && col == label) {
          next
        }
        p <- ggplot(data_vis, aes_string(x = "1", y = col)) +
          geom_boxplot(fill = "tomato") +
          labs(title = paste("Boxplot of", col),
               x = "",
               y = col) +
          theme_minimal()

        if (!is.null(label) &&
            label %in% factor_cols && grouped_plots) {
          p <- ggplot(data_vis, aes_string(
            x = label,
            y = col,
            fill = label
          )) +
            geom_boxplot() +
            labs(
              title = paste("Boxplot of", col, "by", label),
              x = label,
              y = col
            ) +
            theme_minimal()
        }

        if(save_results){
        save_plot(p, results_folder, "Boxplot", paste0("boxplot_", col, ".png"))
        }
      }
    }

    # 11c. Bar plots for categorical columns
    if ("barplot" %in% visualize && length(factor_cols) > 0) {
      for (col in factor_cols) {
        if (!is.null(label) && col == label) {
          next
        }
        p <- ggplot(data_vis, aes_string(x = col)) +
          geom_bar(fill = "forestgreen") +
          labs(title = paste("Bar Plot of", col),
               x = col,
               y = "Count") +
          theme_minimal()

        if(save_results){
        save_plot(p, results_folder, "Barplot",paste0("barplot_", col, ".png"))
        }
      }
    }

    # 11d. Pairwise scatterplots for top correlated features (original)
    if ("scatterplot" %in% visualize && length(numeric_cols) > 1) {
      if (!is.null(correlation_matrix)) {
        upper_tri <- correlation_matrix
        upper_tri[lower.tri(upper_tri, diag = TRUE)] <- NA
        corr_vals <- as.data.frame(as.table(upper_tri))
        colnames(corr_vals) <- c("Var1", "Var2", "Correlation")
        corr_vals <- corr_vals[!is.na(corr_vals$Correlation), ]
        top_pairs <- head(corr_vals[order(abs(corr_vals$Correlation), decreasing =
                                            TRUE), ], 3)

        for (i in seq_len(nrow(top_pairs))) {
          varx <- as.character(top_pairs$Var1[i])
          vary <- as.character(top_pairs$Var2[i])
          if (!is.null(label) && (varx == label || vary == label)) {
            next
          }
          p <- ggplot(data_vis, aes_string(x = varx, y = vary)) +
            geom_point(color = "darkblue") +
            labs(
              title = paste("Scatterplot of", varx, "vs", vary),
              subtitle = paste("Correlation =", round(top_pairs$Correlation[i], 3))
            ) +
            theme_minimal()

          if (!is.null(label) &&
              label %in% factor_cols && grouped_plots) {
            p <- ggplot(data_vis, aes_string(
              x = varx,
              y = vary,
              color = label
            )) +
              geom_point() +
              labs(
                title = paste("Scatterplot of", varx, "vs", vary, "by", label),
                subtitle = paste("Correlation =", round(top_pairs$Correlation[i], 3))
              ) +
              theme_minimal()
          }

          if(save_results){
          save_plot(p, results_folder, "ScatterPlot", paste0("scatterplot_", varx, "_vs_", vary, ".png"))
          }
        }
      } else {
      }
    }

  # (NEW) 11e. Pairwise Scatterplot Matrix using GGally or base pairs
  if (pairwise_matrix && length(numeric_cols) > 1) {
      if (!requireNamespace("GGally", quietly = TRUE)) {
        message("Package 'GGally' is required for pairwise scatterplot matrices.")
      } else {
        # Limit how many columns to show
        numeric_subset <- numeric_cols[1:min(length(numeric_cols), max_scatter_cols)]
        spm <- GGally::ggpairs(data_vis[, numeric_subset, drop = FALSE])
        if(save_results){
        save_plot(spm, results_folder, "ScatterPlot", "pairwise_scatterplot_matrix.png")
        }
      }

  }

    # (NEW) 11f. Categorical vs. Numeric Plots (grouped histograms, violin, density) if grouped_plots=TRUE
    if (grouped_plots &&
        !is.null(label) &&
        label %in% factor_cols && length(numeric_cols) > 0) {
      # Example: grouped violin or density plots for each numeric col by label
      for (col in numeric_cols) {
        if (col == label)
          next
        # Grouped Violin
        p_violin <- ggplot(data_vis, aes_string(x = label, y = col, fill = label)) +
          geom_violin(trim = FALSE) +
          theme_minimal() +
          labs(title = paste("Violin Plot of", col, "by", label))
        if(save_results){
        save_plot(p_violin, results_folder, "Boxplot", paste0("violin_", col, ".png"))
        }

        # Grouped Density
        p_density <- ggplot(data_vis, aes_string(x = col, fill = label)) +
          geom_density(alpha = 0.4) +
          theme_minimal() +
          labs(title = paste("Density Plot of", col, "by", label))
        if(save_results){
        save_plot(p_density, results_folder, "Histogram", paste0("density_", col, ".png"))
        }
      }
    }
  }

  ## ------------------------------------------------------------------------
  ## 12. Compile Results
  ## ------------------------------------------------------------------------
  results_list <- list(
    data_overview             = data_overview,
    summary_stats             = summary_stats,
    freq_tables               = freq_tables,
    missing_data              = missing_data,
    duplicated_rows           = dup_count,
    class_imbalance           = class_imbalance,
    correlation_matrix        = correlation_matrix,
    zero_variance_cols        = zero_variance_cols,
    potential_id_cols         = potential_id_cols,
    date_time_cols            = date_time_cols,
    high_corr_pairs           = high_corr_pairs,
    outlier_method            = outlier_method,
    outlier_summary           = outlier_summary
  )

  # Create a final R Markdown report file if save_results=TRUE
  if (save_results) {
    # Create the Rmd content on-the-fly
    report_rmd_content <- c(
      "---",
      "title: \"fastexplore EDA Report\"",
      "author: \"fastexplore\"",
      "date: \"`r Sys.Date()`\"",
      "output:",
      "  html_document:",
      "    toc: true",
      "    number_sections: true",
      "  pdf_document:",
      "    toc: true",
      "    number_sections: true",
      "---",
      "",
      "```{r setup, include=FALSE}",
      "opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE)",
      "```",
      "",
      "## Data Overview",
      "",
      "```{r data-overview}",
      "results_list$data_overview",
      "```",
      "",

      "## Distribution Checks",
      # "",
      # "```{r normality-tests}",
      # "if (run_distribution_checks && length(numeric_cols) > 0) {",
      # "  normality_table",
      # "} else {",
      # "  \"No distribution tests available.\"",
      # "}",
      # "```",
      # "",

      "",
      "```{r distribution-plots}",
      "if (run_distribution_checks && length(numeric_cols) > 0) {",
      "  normality_plots",
      "} else {",
      "  \"No distribution plots available.\"",
      "}",
      "```",
      "",

      # "## Frequency Tables",
      # "",
      # "```{r freqeuncy-tables}",
      # "if (!is.null(results_list$freq_tables)) {",
      # "  results_list$freq_tables",
      # "} else {",
      # "  \"No frequency tables available.\"",
      # "}",
      # "```",
      # "",

      "## Missing Data Analysis",
      "",
      "**UpSet Plot**",
      "```{r missingness-patterns_upsetPlot}",
      "if (visualize_missing && sum(missing_df) > 0) {",
      "  upsetPlot",
      "} else {",
      "  \"No missingness patterns available.\"",
      "}",
      "```",
      "",
      "**Aggregations Plot**",
      "```{r missingness-patterns_aggr}",
      "if (visualize_missing && sum(missing_df) > 0) {",
      "     miss_plot <- vis_miss(data)",
      "} else {",
      "  \"No missingness patterns available.\"",
      "}",
      "```",
      "",

      # "## Duplicated Rows",
      # "",
      # "```{r duplicated-rows}",
      # "if (!is.null(results_list$duplicated_rows)) {",
      # "  results_list$duplicated_rows",
      # "} else {",
      # "  \"No duplicated rows available.\"",
      # "}",
      # "```",
      # "",

      # "## Class Imbalance (if label is provided)",
      # "",
      # "```{r class-imbalance}",
      # "if (!is.null(results_list$class_imbalance)) {",
      # "  results_list$class_imbalance",
      # "} else {",
      # "  \"No class imbalance info available.\"",
      # "}",
      # "```",
      # "",

      "## Correlation Matrix",
      "",
      "```{r correlation-matrix}",
      "if (!is.null(results_list$correlation_matrix)) {",
      "  results_list$correlation_matrix",
      "} else {",
      "  \"No correlation matrix available.\"",
      "}",
      "```",
      "",

      "```{r heatmap}",
      "if (!is.null(results_list$correlation_matrix)) {",
      "  heatmap_plot",
      "} else {",
      "  \"No heatmap available.\"",
      "}",
      "```",
      "",

      "## High-Correlation Pairs",
      "",
      "```{r high-corr-pairs}",
      "if (!is.null(results_list$high_corr_pairs)) {",
      "  results_list$high_corr_pairs",
      "} else {",
      "  \"No high correlation pairs available.\"",
      "}",
      "```",
      "",

      # "## Zero/Near-Zero Variance",
      # "",
      # "```{r zero-variance}",
      # "if (!is.null(results_list$zero_variance_cols)) {",
      # "  results_list$zero_variance_cols",
      # "} else {",
      # "  \"No zero variance variables available.\"",
      # "}",
      # "```",
      # "",

      # "## Potential ID Columns",
      # "",
      # "```{r id-columns}",
      # "if (!is.null(results_list$potential_id_cols)) {",
      # "  results_list$potential_id_cols",
      # "} else {",
      # "  \"No ID columns available.\"",
      # "}",
      # "```",
      # "",

      # "## Potential Date Time Columns",
      # "",
      # "```{r date-columns}",
      # "if (!is.null(results_list$date_time_cols)) {",
      # "  results_list$date_time_cols",
      # "} else {",
      # "  \"No date time columns available.\"",
      # "}",
      # "```",
      # "",


      "## Outlier Summary",
      "",
      "```{r outlier-summary}",
      "if (!is.null(outlier_summary)) {",
      "  outlier_summary",
      "} else {",
      "  \"No outlier summary available.\"",
      "}",
      "```",
      "",

      "## Histograms",
      "",
      "```{r histogram}",
      "if ('histogram' %in% visualize && length(numeric_cols) > 0) {",
        "if (grouped_plots && !is.null(label) && label %in% factor_cols && length(numeric_cols) > 0) {",
        "  for (col in numeric_cols) {",
        "    p <- ggplot(data_vis, aes_string(x = col)) +",
        "      geom_histogram(",
        "        bins = 30,",
        "        fill = 'steelblue',",
        "        color = 'white'",
        "      ) +",
        "      labs(title = paste('Histogram of', col),",
        "           x = col,",
        "           y = 'Count') +",
        "      theme_minimal()",
        "",
        "    # If label is provided and factor, we can facet wrap",
        "    if (!is.null(label) && label %in% factor_cols && grouped_plots) {",
        "      p <- p + facet_wrap(as.formula(paste('~', label)))",
        "    }",
        "",
        "    print(p)",
        "  }",
        "}",
      "}  else {",
      "  cat('No histogram plots available\\n')",
      "}",
      "```",
      "",

      "## Density Plots",
            "```{r density-plots}",
      "if (grouped_plots &&",
      "    !is.null(label) &&",
      "    label %in% factor_cols && length(numeric_cols) > 0) {",
      "",
      "  for (col in numeric_cols) {",
      "    if (col == label)",
      "      next",
      "",
      "    p_density <- ggplot(data_vis, aes_string(x = col, fill = label)) +",
      "      geom_density(alpha = 0.4) +",
      "      theme_minimal() +",
      "      labs(title = paste('Density Plot of', col, 'by', label))",
      "    print(p_density)",
      "",
      "  }",
      "}  else {",
      "  cat('No density plots available\\n')",
      "}",
      "```",
      "",

      "## Boxplots",
      "```{r boxplot-visualization}",
      "if ('boxplot' %in% visualize && length(numeric_cols) > 0) {",
      "  for (col in numeric_cols) {",
      "    if (!is.null(label) && col == label) {",
      "      next",
      "    }",
      "    p <- ggplot(data_vis, aes_string(x = '1', y = col)) +",
      "      geom_boxplot(fill = 'tomato') +",
      "      labs(title = paste('Boxplot of', col),",
      "           x = '',",
      "           y = col) +",
      "      theme_minimal()",
      "",
      "    if (!is.null(label) &&",
      "        label %in% factor_cols && grouped_plots) {",
      "      p <- ggplot(data_vis, aes_string(",
      "        x = label,",
      "        y = col,",
      "        fill = label",
      "      )) +",
      "        geom_boxplot() +",
      "        labs(",
      "          title = paste('Boxplot of', col, 'by', label),",
      "          x = label,",
      "          y = col",
      "        ) +",
      "        theme_minimal()",
      "    }",
      "",
      "    print(p)",
      "  }",
      "}  else {",
      "  cat('No box plots available\\n')",
      "}",
      "```",
      "",

      "## Violin Plots",

      "```{r violin-plots}",
      "if (grouped_plots &&",
      "    !is.null(label) &&",
      "    label %in% factor_cols && length(numeric_cols) > 0) {",
      "",
      "  for (col in numeric_cols) {",
      "    if (col == label)",
      "      next",
      "",
      "    # Grouped Violin",
      "    p_violin <- ggplot(data_vis, aes_string(x = label, y = col, fill = label)) +",
      "      geom_violin(trim = FALSE) +",
      "      theme_minimal() +",
      "      labs(title = paste('Violin Plot of', col, 'by', label))",
      "    print(p_violin)",
      "",
      "  }",
      "}  else {",
      "  cat('No violin plots available\\n')",
      "}",
      "```",
      "",


      "## Bar Plots",
      "```{r barplot-visualization}",
      "if ('barplot' %in% visualize && length(factor_cols) > 0) {",
      "  for (col in factor_cols) {",
      "    if (!is.null(label) && col == label) {",
      "      next",
      "    }",
      "    p <- ggplot(data_vis, aes_string(x = col)) +",
      "      geom_bar(fill = 'forestgreen') +",
      "      labs(title = paste('Bar Plot of', col),",
      "           x = col,",
      "           y = 'Count') +",
      "      theme_minimal()",
      "",
      "    print(p)",
      "  }",
      "}  else {",
      "  cat('No bar plots available\\n')",
      "}",
      "```",
      "",

      "## Scatter Plots",
      "```{r scatterplot-visualization}",
      "if ('scatterplot' %in% visualize && length(numeric_cols) > 1) {",
      "  if (!is.null(correlation_matrix)) {",
      "    upper_tri <- correlation_matrix",
      "    upper_tri[lower.tri(upper_tri, diag = TRUE)] <- NA",
      "    corr_vals <- as.data.frame(as.table(upper_tri))",
      "    colnames(corr_vals) <- c('Var1', 'Var2', 'Correlation')",
      "    corr_vals <- corr_vals[!is.na(corr_vals$Correlation), ]",
      "    top_pairs <- head(corr_vals[order(abs(corr_vals$Correlation), decreasing = TRUE), ], 3)",
      "",
      "    for (i in seq_len(nrow(top_pairs))) {",
      "      varx <- as.character(top_pairs$Var1[i])",
      "      vary <- as.character(top_pairs$Var2[i])",
      "      if (!is.null(label) && (varx == label || vary == label)) {",
      "        next",
      "      }",
      "      p <- ggplot(data_vis, aes_string(x = varx, y = vary)) +",
      "        geom_point(color = 'darkblue') +",
      "        labs(",
      "          title = paste('Scatterplot of', varx, 'vs', vary),",
      "          subtitle = paste('Correlation =', round(top_pairs$Correlation[i], 3))",
      "        ) +",
      "        theme_minimal()",
      "",
      "      if (!is.null(label) &&",
      "          label %in% factor_cols && grouped_plots) {",
      "        p <- ggplot(data_vis, aes_string(",
      "          x = varx,",
      "          y = vary,",
      "          color = label",
      "        )) +",
      "          geom_point() +",
      "          labs(",
      "            title = paste('Scatterplot of', varx, 'vs', vary, 'by', label),",
      "            subtitle = paste('Correlation =', round(top_pairs$Correlation[i], 3))",
      "          ) +",
      "          theme_minimal()",
      "      }",
      "",
      "      print(p)",
      "    }",
      "  } else {",
      "    cat('Correlation matrix not computed or insufficient numeric columns.\\n')",
      "  }",
      "}",
      "```",
      "",

      "## Pairwise Scatter Plots",
      "```{r pairwise-scatterplot-matrix}",
      "if (pairwise_matrix && length(numeric_cols) > 1) {",
      "  numeric_subset <- numeric_cols[1:min(length(numeric_cols), max_scatter_cols)]",
      "",
      "  spm <- GGally::ggpairs(data_vis[, numeric_subset, drop = FALSE])",
      "  print(spm)",
      "}  else {",
      "  cat('No pairwise scatter plots available\\n')",
      "}",
      "```",
      ""

    )

    # Write the Rmd file to the results folder
    rmd_file_path <- file.path(results_folder, "fastexplore_report.Rmd")
    writeLines(report_rmd_content, con = rmd_file_path)

    # Render into HTML or PDF (quietly, no console output)
    render(
      input         = rmd_file_path,
      output_format = "html_document",
      output_file   = file.path(results_folder, "fastexplore_report.html"),
      quiet = TRUE
    )
  }


  if(save_results){

    message("All fastexplore results saved at: ", results_folder)

  }

  # Return the results list invisibly (no console output)
  invisible(results_list)
}



## Helper functions
save_plot <- function(plot_obj, results_folder, plotname, filename) {

    # Attempt ggsave if it's a ggplot. If it's a plotly or base, might need different approach.
    if (inherits(plot_obj, "ggplot") || inherits(plot_obj, "gg")) {
      ggsave(
        filename = file.path(paste0(results_folder, "/" ,plotname), filename),
        plot     = plot_obj,
        width    = 7,
        height   = 5
      )
    } else {
      message("Object provided is not a ggplot. Skipping ggsave for ",
              filename)
    }

}


perform_normality_tests <- function(data, numeric_cols, normality_tests) {

  if (length(numeric_cols) == 0) {
    message("No numeric columns available for distribution checks.")
    return(NULL)
  }

  # Initialize an empty list to store results
  distribution_tests_results <- list()

  # Initialize a data frame to collect results
  results_df <- data.frame(
    Column = character(),
    Test = character(),
    P_Value = numeric(),
    Adjusted_P_Value = numeric(),
    Normal = character(),
    stringsAsFactors = FALSE
  )

  for (col in numeric_cols) {
    col_data <- data[[col]] %>% na.omit()
    pvals <- list()

    # Shapiro-Wilk Test
    if ("shapiro" %in% normality_tests) {
      if (length(col_data) >= 3 && length(col_data) <= 5000) {
        sw <- shapiro.test(col_data)
        pvals$Shapiro_Wilk <- sw$p.value
      } else {
        pvals$Shapiro_Wilk <- NA
      }
    }

    # Kolmogorov-Smirnov Test
    if ("ks" %in% normality_tests) {
      # Ensure data is standardized
      if (length(col_data) > 0) {
        ks <- suppressWarnings(ks.test(x = scale(col_data), y = "pnorm"))
        pvals$Kolmogorov_Smirnov <- ks$p.value
      } else {
        pvals$Kolmogorov_Smirnov <- NA
      }
    }

    distribution_tests_results[[col]] <- pvals

    # Append results to the data frame
    for (test_name in names(pvals)) {
      results_df <- results_df %>%
        add_row(
          Column = col,
          Test = test_name,
          P_Value = pvals[[test_name]],
          Adjusted_P_Value = NA,  # To be filled after multiple testing correction
          Normal = NA  # To be determined
        )
    }
  }

  # Multiple Testing Correction (e.g., Benjamini-Hochberg)
  results_df <- results_df %>%
    dplyr::group_by(.data$Test) %>%
    dplyr::mutate(Adjusted_P_Value = stats::p.adjust(.data$P_Value, method = "BH")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Normal = dplyr::case_when(
      .data$Adjusted_P_Value < 0.05 ~ "No",
      .data$Adjusted_P_Value >= 0.05 ~ "Yes",
      TRUE ~ NA_character_
    ))

  return(results_df)
}


generate_normality_plots <- function(data, numeric_cols) {
  plot_list <- list()

  for (col in numeric_cols) {
    col_data <- data[[col]] %>% na.omit()

    if(length(col_data) == 0){
      next  # Skip columns with no data
    }

    # Histogram with Normal Distribution Overlay
    hist_plot <- ggplot(data, aes_string(x = col)) +
      geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
      stat_function(fun = dnorm, args = list(mean = mean(col_data), sd = sd(col_data)),
                    color = "red", size = 1) +
      theme_minimal() +
      labs(title = paste("Histogram of", col),
           x = col,
           y = "Density")

    # Q-Q Plot
    qq_plot <- ggplot(data, aes_string(sample = col)) +
      stat_qq(color = "darkgreen") +
      stat_qq_line(color = "red") +
      theme_minimal() +
      labs(title = paste("Q-Q Plot of", col),
           x = "Theoretical Quantiles",
           y = "Sample Quantiles")

    # Density Plot with Normal Curve
    # density_plot <- ggplot(data, aes_string(x = col)) +
    #   geom_density(fill = "lightblue", alpha = 0.5) +
    #   stat_function(fun = dnorm, args = list(mean = mean(col_data), sd = sd(col_data)),
    #                 color = "darkred", size = 1) +
    #   theme_minimal() +
    #   labs(title = paste("Density Plot of", col),
    #        x = col,
    #        y = "Density")

    # Combine the three plots into one using ggpubr
    combined_plot <- ggarrange(hist_plot, qq_plot, ncol = 2, nrow = 1,
                               common.legend = FALSE,
                               labels = c("A", "B"))

    plot_list[[col]] <- combined_plot
  }

  return(plot_list)
}

# Function for IQR-based Outlier Detection
detect_outliers_iqr <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr_val <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr_val
  upper_bound <- q3 + 1.5 * iqr_val
  outliers <- sum(x < lower_bound | x > upper_bound)
  proportion <- outliers / length(x)
  return(proportion)
}

# Function for Z-score-based Outlier Detection
detect_outliers_zscore <- function(x, threshold = 3) {
  z <- scale(x)
  outliers <- sum(abs(z) > threshold, na.rm = TRUE)
  proportion <- outliers / length(x)
  return(proportion)
}

# Function for DBSCAN-based Outlier Detection
detect_outliers_dbscan <- function(data, eps = 0.5, minPts = 5) {
  db_res <- dbscan(data, eps = eps, minPts = minPts)
  outlier_idx <- which(db_res$cluster == 0)
  outlier_prop <- length(outlier_idx) / nrow(data)
  return(outlier_prop)
}

# Function for LOF-based Outlier Detection
detect_outliers_lof <- function(data, minPts = 5, threshold = 1.5) {
  lof_vals <- lof(data, minPts = minPts)
  outlier_idx <- which(lof_vals > threshold)
  outlier_prop <- length(outlier_idx) / nrow(data)
  return(outlier_prop)
}

perform_outlier_detection <- function(data, numeric_cols, outlier_methods = c("iqr", "zscore", "dbscan", "lof")) {
  outlier_summary <- list()

  for (method in outlier_methods) {
    if (method == "iqr") {
      outlier_proportions <- sapply(numeric_cols, function(col) {
        x <- data[[col]]
        x <- x[!is.na(x)]
        detect_outliers_iqr(x)
      })
      outlier_summary$iqr <- outlier_proportions

    } else if (method == "zscore") {
      outlier_proportions <- sapply(numeric_cols, function(col) {
        x <- data[[col]]
        x <- x[!is.na(x)]
        detect_outliers_zscore(x)
      })
      outlier_summary$zscore <- outlier_proportions

    } else if (method == "dbscan") {
      if (!requireNamespace("dbscan", quietly = TRUE)) {
        message("Package 'dbscan' not installed. Skipping DBSCAN-based outlier detection.")
      } else {
        # Select only numeric columns and remove rows with NA
        numeric_data <- data[, numeric_cols, drop = FALSE]
        numeric_data <- na.omit(numeric_data)
        outlier_prop <- detect_outliers_dbscan(numeric_data)
        outlier_summary$dbscan <- outlier_prop
      }

    } else if (method == "lof") {
      if (!requireNamespace("dbscan", quietly = TRUE)) {
        message("Package 'dbscan' not installed. Skipping LOF-based outlier detection.")
      } else {
        # Select only numeric columns and remove rows with NA
        numeric_data <- data[, numeric_cols, drop = FALSE]
        numeric_data <- na.omit(numeric_data)
        outlier_prop <- detect_outliers_lof(numeric_data)
        outlier_summary$lof <- outlier_prop
      }
    }
  }

  return(outlier_summary)
}



