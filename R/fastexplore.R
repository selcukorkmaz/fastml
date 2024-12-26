fastexplore <- function(
    data,
    label = NULL,
    visualize = c("histogram", "boxplot", "heatmap"),
    save_results = TRUE,
    output_dir = getwd(),
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
    normality_tests = c("shapiro", "ks"), # Which normality tests to run if run_distribution_checks=TRUE
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
    results_folder  <- file.path(output_dir, paste0("EDA_Results_", timestamp))
    dir.create(results_folder,
               showWarnings = FALSE,
               recursive = TRUE)
  }

  ## Helper function to save plots if needed
  maybe_save_plot <- function(plot_obj, filename) {
    if (save_results) {
      # Attempt ggsave if it's a ggplot. If it's a plotly or base, might need different approach.
      if (inherits(plot_obj, "ggplot")) {
        ggsave(
          filename = file.path(results_folder, filename),
          plot     = plot_obj,
          width    = 7,
          height   = 5
        )
      } else {
        message("Object provided is not a ggplot. Skipping ggsave for ",
                filename)
      }
    }
  }

  ## ------------------------------------------------------------------------
  ## 1a. Mixed Data Types / Validation
  ## ------------------------------------------------------------------------
  # Attempt to auto-convert factor/character columns that appear numeric
  if (auto_convert_numeric) {
    for (col in factor_cols) {
      x <- data[[col]]
      # Check if all non-NA values match a numeric pattern (digits, decimal, minus sign)
      if (all(grepl("^[0-9.-]+$", x[!is.na(x)]))) {
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
  cat("--------------------------------------------------\n")
  cat("1. DATA OVERVIEW\n")
  cat("--------------------------------------------------\n")
  cat("Dimensions: ", paste(dim(data), collapse = " x "), "\n")
  cat("Column Names:\n")
  print(names(data))
  cat("Data Types:\n")
  print(sapply(data, class))
  cat("Preview of the first 6 rows:\n")
  print(head(data, 6))
  cat("\n\n")

  data_overview <- list(
    dimensions   = dim(data),
    column_names = names(data),
    data_types   = sapply(data, class),
    head_of_data = head(data, 6)
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

  if (length(potential_id_cols) > 0) {
    cat("Potential ID columns (unique for every row):\n")
    print(potential_id_cols)
  } else {
    cat("No potential ID columns found.\n")
  }
  cat("\n")

  if (length(date_time_cols) > 0) {
    cat("Date/Time columns detected:\n")
    print(date_time_cols)
  } else {
    cat("No date/time columns detected.\n")
  }
  cat("\n\n")

  # (NEW) Feature Engineering Suggestions
  if (feature_engineering) {
    if (length(potential_id_cols) > 0) {
      cat(
        "Feature Engineering Note: Potential ID columns might be used for joining or removed from modeling.\n"
      )
    }
    if (length(date_time_cols) > 0) {
      cat("Creating derived date/time features (day, month, year) from date/time columns.\n")
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
    cat("--------------------------------------------------\n")
    cat("2. SUMMARY STATISTICS FOR NUMERIC COLUMNS\n")
    cat("--------------------------------------------------\n")

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
      library(moments)
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

    print(round(summary_numeric, 3))
    cat("\n")

    if (length(zero_variance_cols) > 0) {
      cat("Columns with zero/near-zero variance (variance <= 1e-8):\n")
      print(zero_variance_cols)
    } else {
      cat("No zero/near-zero variance columns detected.\n")
    }
    cat("\n\n")

    summary_stats <- summary_numeric
  } else {
    cat("No numeric columns detected.\n")
    cat("\n\n")
  }

  ## (NEW) Extended Descriptive Statistics: Distribution Checks
  distribution_tests_results <- NULL
  if (run_distribution_checks && length(numeric_cols) > 0) {
    distribution_tests_results <- list()
    # Perform normality tests (Shapiro-Wilk, Kolmogorov-Smirnov) on numeric columns
    # Typically, these tests are sensitive to large sample sizes, but let's demonstrate
    cat("--------------------------------------------------\n")
    cat("DISTRIBUTION CHECKS (Normality Tests)\n")
    cat("--------------------------------------------------\n")
    for (col in numeric_cols) {
      col_data <- data[[col]]
      col_data <- col_data[!is.na(col_data)]
      # We'll store p-values
      pvals <- list()
      if ("shapiro" %in% normality_tests) {
        # Shapiro-Wilk test usually limited to n < 5000 in R
        if (length(col_data) >= 3 && length(col_data) <= 5000) {
          sw <- shapiro.test(col_data)
          pvals$shapiro_pvalue <- sw$p.value
        } else {
          pvals$shapiro_pvalue <- NA
        }
      }
      if ("ks" %in% normality_tests) {
        # One-sample KS test against normal with same mean/sd
        ks <- suppressWarnings(ks.test(x = scale(col_data), y = "pnorm"))
        pvals$ks_pvalue <- ks$p.value
      }
      distribution_tests_results[[col]] <- pvals
      cat(
        "Normality test p-values for",
        col,
        ":",
        paste(
          names(pvals),
          unlist(pvals),
          sep = "=",
          collapse = ", "
        ),
        "\n"
      )

    }
    cat("\n")
  }

  ## ------------------------------------------------------------------------
  ## 5. Frequency Tables (Categorical)
  ## ------------------------------------------------------------------------
  freq_tables <- NULL
  if (length(factor_cols) > 0) {
    cat("--------------------------------------------------\n")
    cat("3. FREQUENCY TABLES FOR CATEGORICAL COLUMNS\n")
    cat("--------------------------------------------------\n")
    freq_list <- lapply(factor_cols, function(col) {
      tbl <- table(data[[col]], useNA = "ifany")
      return(tbl)
    })
    names(freq_list) <- factor_cols

    for (col in factor_cols) {
      cat(paste0("Frequency table for: ", col, "\n"))
      print(freq_list[[col]])
      cat("\n")
    }
    freq_tables <- freq_list
    cat("\n\n")
  } else {
    cat("No categorical columns detected.\n")
    cat("\n\n")
  }

  ## ------------------------------------------------------------------------
  ## 6. Missing Data Analysis
  ## ------------------------------------------------------------------------
  cat("--------------------------------------------------\n")
  cat("4. MISSING DATA ANALYSIS\n")
  cat("--------------------------------------------------\n")
  missing_data <- data.frame(
    Column       = names(data),
    MissingCount = sapply(data, function(x)
      sum(is.na(x))),
    MissingPct   = sapply(data, function(x)
      mean(is.na(x)) * 100)
  )
  print(missing_data)
  cat("\n\n")

  # (NEW) Additional Missing Data Visualization (UpSetR)
  if (use_upset_missing) {
    if (!requireNamespace("UpSetR", quietly = TRUE)) {
      message(
        "Package 'UpSetR' is not installed. Skipping UpSet-based missingness visualization."
      )
    } else {
      # Attempt to produce an upset plot of missingness
      # We'll construct a logical matrix of missingness
      missing_matrix <- sapply(data, is.na)

      if (any(missing_matrix)) {
        cat("Attempting an UpSet plot of missing data...\n")

        # Convert to data.frame
        missing_df <- as.data.frame(missing_matrix)
        # Because UpSetR uses sets, we convert each column to factor(0/1)
        library(UpSetR)
        for (cname in names(missing_df)) {
          missing_df[[cname]] <- ifelse(missing_df[[cname]] == TRUE, 1, 0)
        }
        # The typical usage is upset(missing_df, sets=names(missing_df)), but for large sets this might be big
        # We'll do a minimal version:
        if (ncol(missing_df) > 15) {
          message("Too many columns for a neat UpSetR plot. Consider subsetting.")
        } else {
          tryCatch({
            upsetPlot <- upset(missing_df, sets = names(missing_df))
          }, error = function(e) {
            message("Error generating UpSet plot: ", e$message)
          })
        }
      }else{
        missing_df = NULL
      }
    }
  }

  if (visualize_missing && sum(missing_df) > 0) {
    # Attempt to visualize missingness patterns
    if (requireNamespace("naniar", quietly = TRUE)) {
      library(naniar)
      cat("Visualizing missing data with naniar::vis_miss...\n")
      miss_plot <- vis_miss(data)
      print(miss_plot)
      if (save_results) {
        maybe_save_plot(miss_plot, "missingness_pattern.png")
      }
    } else if (requireNamespace("VIM", quietly = TRUE)) {
      library(VIM)
      cat("Visualizing missing data with VIM::aggr...\n")
      aggr_plot <- aggr(
        data,
        col = c('navyblue', 'red'),
        numbers = TRUE,
        sortVars = TRUE,
        labels = names(data),
        cex.axis = .7,
        gap = 3,
        ylab = c("Histogram of missing data", "Pattern")
      )
    } else {
      message("Neither 'naniar' nor 'VIM' is installed. Skipping missingness visualization.")
    }
  }

  if (imputation_suggestions) {
    cat("--------------------------------------------------\n")
    cat("Imputation Suggestions\n")
    cat("--------------------------------------------------\n")
    cat("Simple strategies:\n")
    cat("1) Numeric columns: Impute missing values with the mean or median.\n")
    cat(
      "2) Categorical columns: Impute missing values with the most frequent category (mode).\n"
    )
    cat(
      "Advanced approaches may include 'mice', 'missForest', or other predictive methods.\n\n"
    )
  }

  ## ------------------------------------------------------------------------
  ## 7. Duplicated Rows
  ## ------------------------------------------------------------------------
  cat("--------------------------------------------------\n")
  cat("5. DUPLICATED ROWS\n")
  cat("--------------------------------------------------\n")
  dup_count <- sum(duplicated(data))
  cat("Number of duplicated rows:", dup_count, "\n\n")

  if (report_duplicate_details && dup_count > 0) {
    cat("Reporting top duplicated rows (by frequency):\n")
    duplicates_all <- data[duplicated(data) |
                             duplicated(data, fromLast = TRUE), ]
    duplicates_all$dup_key <- apply(duplicates_all, 1, function(row)
      paste(row, collapse = "_SEP_"))
    freq_table <- table(duplicates_all$dup_key)
    freq_table <- sort(freq_table, decreasing = TRUE)
    print(head(freq_table, 10))
    cat("\n")
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
      cat("--------------------------------------------------\n")
      cat("6. CLASS IMBALANCE FOR LABEL VARIABLE\n")
      cat("--------------------------------------------------\n")
      tbl_label <- table(data[[label]], useNA = "ifany")
      class_imbalance <- tbl_label
      print(tbl_label)
      cat("\n\n")
    }
  }

  ## ------------------------------------------------------------------------
  ## 9. Correlation Matrix, Heatmap & High-Correlation Pairs
  ## ------------------------------------------------------------------------
  correlation_matrix <- NULL
  high_corr_pairs    <- NULL

  if (length(numeric_cols) > 1 && "heatmap" %in% visualize) {
    cat("--------------------------------------------------\n")
    cat("7. CORRELATION MATRIX\n")
    cat("--------------------------------------------------\n")

    correlation_matrix <- cor(data[numeric_cols], use = "pairwise.complete.obs")
    print(round(correlation_matrix, 3))
    cat("\n\n")

    corr_mat_upper <- correlation_matrix
    corr_mat_upper[lower.tri(corr_mat_upper, diag = TRUE)] <- NA
    corr_df <- as.data.frame(as.table(corr_mat_upper))
    corr_df <- corr_df[!is.na(corr_df$Freq), ]
    names(corr_df) <- c("Var1", "Var2", "Corr")
    high_corr_pairs <- subset(corr_df, abs(Corr) > corr_threshold)

    if (nrow(high_corr_pairs) > 0) {
      cat("Highly correlated pairs above threshold:\n")
      print(high_corr_pairs[order(-abs(high_corr_pairs$Corr)), ])
    } else {
      cat("No highly correlated pairs found above the threshold.\n")
    }
    cat("\n")

    if (interactive) {
      if (!requireNamespace("plotly", quietly = TRUE)) {
        message("Package 'plotly' not installed. Falling back to static ggplot2 heatmap.")
      } else {
        library(plotly)
        heatmap_plot <- plot_ly(
          x = colnames(correlation_matrix),
          y = rownames(correlation_matrix),
          z = correlation_matrix,
          type = "heatmap",
          colors = colorRamp(c("blue", "white", "red"))
        )
        print(heatmap_plot)
        if (save_results) {
          htmlwidgets::saveWidget(heatmap_plot,
                                  file = file.path(results_folder, "correlation_heatmap.html"))
        }
      }
    }

    if (!interactive ||
        !requireNamespace("plotly", quietly = TRUE)) {
      if (!requireNamespace("reshape2", quietly = TRUE)) {
        stop("Package 'reshape2' must be installed to create static heatmap.")
      }
      library(reshape2)
      library(ggplot2)
      corr_melt <- melt(correlation_matrix)
      heatmap_plot <- ggplot(corr_melt, aes(x = Var1, y = Var2, fill = value)) +
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

      print(heatmap_plot)
      maybe_save_plot(heatmap_plot, "correlation_heatmap.png")
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
    cat("--------------------------------------------------\n")
    cat("8. OUTLIER DETECTION\n")
    cat("--------------------------------------------------\n")
    if ("iqr" %in% outlier_method) {
      outlier_proportions <- sapply(numeric_cols, function(col) {
        x <- data[[col]]
        x <- x[!is.na(x)]
        q1 <- quantile(x, 0.25)
        q3 <- quantile(x, 0.75)
        iqr_val <- q3 - q1
        lower_bound <- q1 - 1.5 * iqr_val
        upper_bound <- q3 + 1.5 * iqr_val
        outliers <- sum(x < lower_bound | x > upper_bound)
        outliers / length(x)
      })
      print("Outlier proportions (IQR method):")
      print(round(outlier_proportions, 3))
      cat("\n")
      outlier_summary$iqr <- outlier_proportions
    } else if ("zscore" %in% outlier_method) {
      # robust Z-score or standard Z-score
      outlier_proportions <- sapply(numeric_cols, function(col) {
        x <- data[[col]]
        x <- x[!is.na(x)]
        z <- scale(x)
        # threshold at abs(z) > 3
        outliers <- sum(abs(z) > 3)
        outliers / length(x)
      })
      print("Outlier proportions (Z-score method):")
      print(round(outlier_proportions, 3))
      cat("\n")
      outlier_summary$zscore <- outlier_proportions
    } else if ("dbscan" %in% outlier_method) {
      if (!requireNamespace("dbscan", quietly = TRUE)) {
        message("Package 'dbscan' not installed. Skipping DBSCAN-based outlier detection.")
      } else {
        # Attempt DBSCAN on numeric columns (multi-dimensional).
        # This is a simplistic approach with fixed eps, minPts
        numeric_data <- data[, numeric_cols, drop = FALSE]
        numeric_data <- na.omit(numeric_data) # remove NAs
        # You'd normally choose eps, minPts with domain knowledge:
        db_res <- dbscan::dbscan(numeric_data, eps = 0.5, minPts = 5)
        # label outliers as cluster=0
        outlier_idx <- which(db_res$cluster == 0)
        outlier_prop <- length(outlier_idx) / nrow(numeric_data)
        cat(
          "DBSCAN-based outliers proportion (multidimensional):",
          round(outlier_prop, 3),
          "\n"
        )
        outlier_proportions <- rep(NA, length(numeric_cols))
        names(outlier_proportions) <- numeric_cols
        outlier_proportions[1] <- outlier_prop
        outlier_summary$dbscan <- outlier_prop
      }
    } else if ("lof" %in% outlier_method) {
      if (!requireNamespace("dbscan", quietly = TRUE)) {
        message("Package 'dbscan' not installed. Skipping LOF-based outlier detection.")
      } else {
        numeric_data <- data[, numeric_cols, drop = FALSE]
        numeric_data <- na.omit(numeric_data)
        lof_vals <- dbscan::lof(numeric_data, minPts = 5)
        # Typically, LOF > 1.5 or 2 could be considered outliers. We'll pick 1.5:
        outlier_idx <- which(lof_vals > 1.5)
        outlier_prop <- length(outlier_idx) / nrow(numeric_data)
        cat(
          "LOF-based outliers proportion (multidimensional):",
          round(outlier_prop, 3),
          "\n"
        )
        outlier_proportions <- rep(NA, length(numeric_cols))
        names(outlier_proportions) <- numeric_cols
        outlier_proportions[1] <- outlier_prop
        outlier_summary$lof <- outlier_prop
      }
    }
  } else if (outlier_method == "none") {
    cat("Skipping outlier detection (method='none').\n")
  }

  ## ------------------------------------------------------------------------
  ## 11. Visualizations (Histograms, Boxplots, Barplots, Scatterplots)
  ## ------------------------------------------------------------------------
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    message("Package 'ggplot2' is required for plots. Please install it to view plots.")
  } else {
    library(ggplot2)

    # 11a. Histograms for numeric variables
    if ("histogram" %in% visualize && length(numeric_cols) > 0) {
      cat("--------------------------------------------------\n")
      cat("Histogram Plots\n")
      cat("--------------------------------------------------\n")
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

        print(p)
        maybe_save_plot(p, paste0("histogram_", col, ".png"))
      }
    }

    # 11b. Boxplots for numeric variables
    if ("boxplot" %in% visualize && length(numeric_cols) > 0) {
      cat("--------------------------------------------------\n")
      cat("Boxplot Plots\n")
      cat("--------------------------------------------------\n")
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

        print(p)
        maybe_save_plot(p, paste0("boxplot_", col, ".png"))
      }
    }

    # 11c. Bar plots for categorical columns
    if ("barplot" %in% visualize && length(factor_cols) > 0) {
      cat("--------------------------------------------------\n")
      cat("Barplot Plots\n")
      cat("--------------------------------------------------\n")
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

        print(p)
        maybe_save_plot(p, paste0("barplot_", col, ".png"))
      }
    }

    # 11d. Pairwise scatterplots for top correlated features (original)
    if ("scatterplot" %in% visualize && length(numeric_cols) > 1) {
      cat("--------------------------------------------------\n")
      cat("Pairwise Scatterplots for Top Correlated Features\n")
      cat("--------------------------------------------------\n")
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

          print(p)
          maybe_save_plot(p, paste0("scatterplot_", varx, "_vs_", vary, ".png"))
        }
      } else {
        cat("Correlation matrix not computed or insufficient numeric columns.\n")
      }
    }

    # (NEW) 11e. Pairwise Scatterplot Matrix using GGally or base pairs
    if (pairwise_matrix && length(numeric_cols) > 1) {
      # Limit how many columns to show
      numeric_subset <- numeric_cols[1:min(length(numeric_cols), max_scatter_cols)]
      cat("--------------------------------------------------\n")
      cat("PAIRWISE SCATTERPLOT MATRIX\n")
      cat("--------------------------------------------------\n")
      if (requireNamespace("GGally", quietly = TRUE)) {
        library(GGally)
        cat("Using GGally::ggpairs for pairwise scatterplot matrix...\n")
        spm <- ggpairs(data_vis[, numeric_subset, drop = FALSE])
        print(spm)
        maybe_save_plot(spm, "pairwise_scatterplot_matrix.png")
      } else {
        cat("Package 'GGally' not installed. Falling back to base R 'pairs()'.\n")
        pairs(data_vis[, numeric_subset, drop = FALSE])
        # Base R pairs cannot easily be saved via ggsave
      }
    }

    # (NEW) 11f. Categorical vs. Numeric Plots (grouped histograms, violin, density) if grouped_plots=TRUE
    if (grouped_plots &&
        !is.null(label) &&
        label %in% factor_cols && length(numeric_cols) > 0) {
      cat("--------------------------------------------------\n")
      cat("CATEGORICAL vs. NUMERIC PLOTS\n")
      cat("--------------------------------------------------\n")
      # Example: grouped violin or density plots for each numeric col by label
      for (col in numeric_cols) {
        if (col == label)
          next
        # Grouped Violin
        p_violin <- ggplot(data_vis, aes_string(x = label, y = col, fill = label)) +
          geom_violin(trim = FALSE) +
          theme_minimal() +
          labs(title = paste("Violin Plot of", col, "by", label))
        print(p_violin)
        maybe_save_plot(p_violin, paste0("violin_", col, ".png"))

        # Grouped Density
        p_density <- ggplot(data_vis, aes_string(x = col, fill = label)) +
          geom_density(alpha = 0.4) +
          theme_minimal() +
          labs(title = paste("Density Plot of", col, "by", label))
        print(p_density)
        maybe_save_plot(p_density, paste0("density_", col, ".png"))
      }
    }
  }

  ## ------------------------------------------------------------------------
  ## 12. Compile Results
  ## ------------------------------------------------------------------------
  results_list <- list(
    data_overview             = data_overview,
    summary_stats             = summary_stats,
    distribution_tests        = distribution_tests_results,
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
      "knitr::opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE)",
      "```",
      "",
      "## Data Overview",
      "",
      "```{r data-overview}",
      "results_list$data_overview",
      "```",
      "",

      "## Summary Statistics",
      "",
      "```{r summary-stats}",
      "if (!is.null(results_list$summary_stats)) {",
      "  results_list$summary_stats",
      "} else {",
      "  \"No numeric columns or summary statistics available.\"",
      "}",
      "```",
      "",

      "## Distribution Checks (Normality Tests)",
      "",
      "```{r distribution-checks}",
      "if (!is.null(results_list$distribution_tests)) {",
      "  results_list$distribution_tests",
      "} else {",
      "  \"No distribution tests available.\"",
      "}",
      "```",
      "",

      "## Frequency Tables",
      "",
      "```{r freqeuncy-tables}",
      "if (!is.null(results_list$freq_tables)) {",
      "  results_list$freq_tables",
      "} else {",
      "  \"No frequency tables available.\"",
      "}",
      "```",
      "",

      "## Missing Data Analysis",
      "",
      "```{r missing-data-analysis}",
      "if (!is.null(results_list$missing_data)) {",
      "  results_list$missing_data",
      "} else {",
      "  \"No missing data info available.\"",
      "}",
      "```",
      "",

      "## Missingness Patterns",
      "",
      "```{r missingness-patterns}",
      "if (visualize_missing && sum(missing_df) > 0) {",
      "  upsetPlot",
      "} else {",
      "  \"No missingness patterns available.\"",
      "}",
      "```",
      "",

      "## Duplicated Rows",
      "",
      "```{r duplicated-rows}",
      "if (!is.null(results_list$duplicated_rows)) {",
      "  results_list$duplicated_rows",
      "} else {",
      "  \"No duplicated rows available.\"",
      "}",
      "```",
      "",

      "## Class Imbalance (if label is provided)",
      "",
      "```{r class-imbalance}",
      "if (!is.null(results_list$class_imbalance)) {",
      "  results_list$class_imbalance",
      "} else {",
      "  \"No class imbalance info available.\"",
      "}",
      "```",
      "",

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

      "## Zero/Near-Zero Variance",
      "",
      "```{r zero-variance}",
      "if (!is.null(results_list$zero_variance_cols)) {",
      "  results_list$zero_variance_cols",
      "} else {",
      "  \"No zero variance variables available.\"",
      "}",
      "```",
      "",

      "## Potential ID Columns",
      "",
      "```{r id-columns}",
      "if (!is.null(results_list$potential_id_cols)) {",
      "  results_list$potential_id_cols",
      "} else {",
      "  \"No ID columns available.\"",
      "}",
      "```",
      "",

      "## Potential Date Time Columns",
      "",
      "```{r date-columns}",
      "if (!is.null(results_list$date_time_cols)) {",
      "  results_list$date_time_cols",
      "} else {",
      "  \"No date time columns available.\"",
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


      "## Outlier Summary",
      "",
      "```{r outlier-summary}",
      "if (!is.null(results_list$outlier_summary)) {",
      "  results_list$outlier_summary",
      "} else {",
      "  \"No outlier summary available.\"",
      "}",
      "```",
      ""


    )


    # Write the Rmd file to the results folder
    rmd_file_path <- file.path(results_folder, "fastexplore_report.Rmd")
    writeLines(report_rmd_content, con = rmd_file_path)

    # Render into HTML or PDF (quietly, no console output)
    rmarkdown::render(
      input         = rmd_file_path,
      output_format = "html_document",
      output_file   = file.path(results_folder, "fastexplore_report.html"),
      quiet = TRUE
    )
  }

  # Return the results list invisibly (no console output)
  invisible(results_list)
}
