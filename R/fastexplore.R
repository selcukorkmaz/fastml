#' Lightweight exploratory helper
#'
#' `fastexplore()` is an optional, lightweight exploratory data analysis (EDA)
#' helper. It returns summary tables and plot objects; it only writes to disk or
#' renders a report when you explicitly request it via `save_results` or
#' `render_report`.
#'
#' This helper is intentionally decoupled from the core modeling workflow. Most
#' of its heavy dependencies are treated as optional and loaded via
#' `requireNamespace()` when requested features are used.
#'
#' @param data A `data.frame` to explore.
#' @param label Optional column name of the target/label. If supplied and
#'   categorical, grouped plots and class balance summaries are produced.
#' @param visualize Character vector indicating which plot families to build.
#'   Defaults to `c("histogram", "boxplot", "barplot", "heatmap", "scatterplot")`.
#' @param save_results Logical; if `TRUE`, plots/results are saved under
#'   `output_dir` (defaults to the working directory). Default is `FALSE`.
#' @param render_report Logical; if `TRUE`, a short HTML report is rendered via
#'   `rmarkdown` (if available). Default is `FALSE`.
#' @param output_dir Directory to save results/report when `save_results` or
#'   `render_report` is `TRUE`.
#' @param sample_size Optional integer; if supplied, visualizations are produced
#'   on a random sample of this size.
#' @param interactive Logical; if `TRUE` and `plotly` is available, an
#'   interactive correlation heatmap is produced. Falls back to static ggplot
#'   output otherwise.
#' @param corr_threshold Absolute correlation threshold for flagging high
#'   correlations.
#' @param auto_convert_numeric Logical; convert factor/character columns that
#'   look numeric into numeric.
#' @param visualize_missing Logical; if `TRUE`, include simple missingness
#'   visualizations.
#' @param imputation_suggestions Logical; if `TRUE`, prints lightweight
#'   suggestions based on missingness patterns.
#' @param report_duplicate_details Logical; if `TRUE`, returns a small sample of
#'   duplicated rows when present.
#' @param detect_near_duplicates Placeholder for future fuzzy duplicate checks.
#' @param auto_convert_dates Logical; convert YYYY-MM-DD strings to `Date`.
#' @param feature_engineering Logical; if `TRUE`, derive day/month/year from date
#'   columns to aid inspection of temporal structure.
#' @param outlier_method One of `"iqr"`, `"zscore"`, `"dbscan"`, `"lof"`.
#' @param run_distribution_checks Logical; if `TRUE`, run normality tests on
#'   numeric columns.
#' @param normality_tests Character vector of normality tests to run; currently
#'   supports `"shapiro"` and `"ks"`.
#' @param pairwise_matrix Logical; if `TRUE` and `GGally` is available, returns a
#'   ggpairs scatterplot matrix for a subset of numeric columns.
#' @param max_scatter_cols Maximum number of numeric columns to include in the
#'   pairwise matrix.
#' @param grouped_plots Logical; if `TRUE` and `label` is a factor, group
#'   histograms/boxplots/density plots by label.
#' @param use_upset_missing Logical; retained for compatibility. When `TRUE` and
#'   `UpSetR` is installed, an UpSet plot of missingness is returned; otherwise a
#'   simpler missingness heatmap is used.
#'
#' @return A list of summaries (tables/tibbles) and plot objects (ggplot/plotly),
#'   plus any saved file paths when `save_results`/`render_report` are enabled.
#'
#' @importFrom stats na.omit
#' @export
fastexplore <- function(
  data,
  label = NULL,
  visualize = c("histogram", "boxplot", "barplot", "heatmap", "scatterplot"),
  save_results = FALSE,
  render_report = FALSE,
  output_dir = NULL,
  sample_size = NULL,
  interactive = FALSE,
  corr_threshold = 0.9,
  auto_convert_numeric = TRUE,
  visualize_missing = TRUE,
  imputation_suggestions = FALSE,
  report_duplicate_details = TRUE,
  detect_near_duplicates = FALSE,
  auto_convert_dates = FALSE,
  feature_engineering = FALSE,
  outlier_method = c("iqr", "zscore", "dbscan", "lof"),
  run_distribution_checks = TRUE,
  normality_tests = c("shapiro"),
  pairwise_matrix = TRUE,
  max_scatter_cols = 5,
  grouped_plots = TRUE,
  use_upset_missing = TRUE
) {
  outlier_method <- match.arg(outlier_method)
  visualize <- match.arg(visualize, several.ok = TRUE)
  normality_tests <- match.arg(normality_tests, several.ok = TRUE)

  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame")
  }

  if (nrow(data) == 0) {
    stop("'data' has no rows to explore")
  }

  if (!is.null(label) && !label %in% colnames(data)) {
    stop(sprintf("Label column '%s' is not present in data", label))
  }

  data <- tibble::as_tibble(data)

  data <- dplyr::mutate(
    data,
    dplyr::across(where(is.integer), as.numeric),
    dplyr::across(where(is.character), as.factor)
  )

  if (auto_convert_numeric) {
    factor_like <- names(Filter(function(x) is.factor(x) || is.character(x), data))
    for (col in factor_like) {
      x <- data[[col]]
      if (all(grepl("^[0-9.-]+$", x[!is.na(x)])) && length(unique(x)) >= 6) {
        new_x <- suppressWarnings(as.numeric(as.character(x)))
        if (sum(is.na(new_x)) <= sum(is.na(x))) {
          data[[col]] <- new_x
        }
      }
    }
  }

  if (auto_convert_dates) {
    factor_like <- names(Filter(function(x) is.factor(x) || is.character(x), data))
    for (col in factor_like) {
      x <- data[[col]]
      if (all(grepl("^\\d{4}-\\d{2}-\\d{2}$", x[!is.na(x)]))) {
        converted_date <- as.Date(x, format = "%Y-%m-%d")
        if (!all(is.na(converted_date))) {
          data[[col]] <- converted_date
        }
      }
    }
  }

  if (!is.null(label)) {
    target_var <- data[[label]]
    if (is.numeric(target_var) && length(unique(target_var)) <= 5) {
      data[[label]] <- as.factor(target_var)
      warning(sprintf("Label '%s' converted to factor for grouped plots", label))
    }
  }

  if (feature_engineering) {
    date_cols <- names(Filter(function(x) inherits(x, c("Date", "POSIXct", "POSIXt")), data))
    for (col in date_cols) {
      data[[paste0(col, "_day")]] <- as.numeric(format(data[[col]], "%d"))
      data[[paste0(col, "_month")]] <- as.numeric(format(data[[col]], "%m"))
      data[[paste0(col, "_year")]] <- as.numeric(format(data[[col]], "%Y"))
    }
  }

  numeric_cols <- names(Filter(is.numeric, data))
  factor_cols <- names(Filter(function(x) is.factor(x) || is.character(x), data))

  if (is.null(sample_size) || sample_size >= nrow(data)) {
    data_vis <- data
  } else {
    set.seed(123)
    idx <- sample.int(nrow(data), sample_size)
    data_vis <- data[idx, , drop = FALSE]
  }

  data_dimensions <- tibble::tibble(Rows = nrow(data), Columns = ncol(data))
  data_types <- tibble::tibble(
    Column = names(data),
    Data_Type = vapply(data, function(x) paste(class(x), collapse = ","), character(1))
  )
  unique_values <- dplyr::summarise(data, dplyr::across(dplyr::everything(), dplyr::n_distinct)) |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "Column", values_to = "Unique_Values")

  missing_data <- tibble::tibble(
    Column = names(data),
    Missing = vapply(data, function(x) sum(is.na(x)), integer(1)),
    Missing_Percent = round(Missing / nrow(data) * 100, 2)
  )

  data_summary <- NULL
  if (safe_require("skimr")) {
    data_summary <- skimr::skim(data)
  }

  data_overview <- list(
    dimensions = data_dimensions,
    types = data_types,
    unique_values = unique_values,
    summary = data_summary,
    head = utils::head(data, 6)
  )

  potential_id_cols <- names(Filter(function(col) dplyr::n_distinct(data[[col]]) == nrow(data), names(data)))
  date_time_cols <- names(Filter(function(x) inherits(data[[x]], c("Date", "POSIXct", "POSIXt")), names(data)))

  summary_stats <- NULL
  if (length(numeric_cols) > 0) {
    base_stats <- tibble::tibble(
      Column = numeric_cols,
      Mean = vapply(data[numeric_cols], function(x) mean(x, na.rm = TRUE), numeric(1)),
      Median = vapply(data[numeric_cols], function(x) stats::median(x, na.rm = TRUE), numeric(1)),
      SD = vapply(data[numeric_cols], function(x) stats::sd(x, na.rm = TRUE), numeric(1)),
      Min = vapply(data[numeric_cols], function(x) min(x, na.rm = TRUE), numeric(1)),
      Max = vapply(data[numeric_cols], function(x) max(x, na.rm = TRUE), numeric(1)),
      NAs = vapply(data[numeric_cols], function(x) sum(is.na(x)), integer(1))
    )

    if (safe_require("moments")) {
      base_stats$Skewness <- vapply(data[numeric_cols], function(x) moments::skewness(x, na.rm = TRUE), numeric(1))
      base_stats$Kurtosis <- vapply(data[numeric_cols], function(x) moments::kurtosis(x, na.rm = TRUE), numeric(1))
    }

    summary_stats <- base_stats
  }

  zero_variance_cols <- character()
  if (length(numeric_cols) > 0) {
    variances <- vapply(data[numeric_cols], function(x) stats::var(x, na.rm = TRUE), numeric(1))
    zero_variance_cols <- names(variances[is.na(variances) | variances <= 1e-8])
  }

  freq_tables <- NULL
  if (length(factor_cols) > 0) {
    freq_tables <- lapply(factor_cols, function(col) {
      tab <- table(data[[col]], useNA = "ifany")
      tibble::tibble(value = names(tab), count = as.integer(tab))
    })
    names(freq_tables) <- factor_cols
  }

  duplicated_rows <- sum(duplicated(data))
  duplicated_examples <- NULL
  if (report_duplicate_details && duplicated_rows > 0) {
    duplicated_examples <- utils::head(unique(data[duplicated(data), , drop = FALSE]), 10)
  }

  class_imbalance <- NULL
  if (!is.null(label) && is.factor(data[[label]])) {
    tab <- table(data[[label]], useNA = "ifany")
    class_imbalance <- tibble::tibble(
      class = names(tab),
      count = as.integer(tab),
      percent = round(100 * as.integer(tab) / sum(tab), 2)
    )
  }

  correlation_matrix <- NULL
  high_corr_pairs <- NULL
  if (length(numeric_cols) > 1) {
    correlation_matrix <- stats::cor(data[numeric_cols], use = "pairwise.complete.obs")
    high_pairs_df <- reshape2::melt(correlation_matrix, varnames = c("Var1", "Var2"), value.name = "Correlation")
    high_pairs_df <- high_pairs_df[high_pairs_df$Var1 != high_pairs_df$Var2, ]
    high_corr_pairs <- subset(high_pairs_df, abs(Correlation) >= corr_threshold)
  }

  outlier_summary <- perform_outlier_detection(data, numeric_cols, outlier_method)

  normality_results <- NULL
  normality_plots <- NULL
  if (run_distribution_checks && length(numeric_cols) > 0) {
    normality_results <- perform_normality_tests(data, numeric_cols, normality_tests)
    normality_plots <- generate_normality_plots(data, numeric_cols)
  }

  missingness_plot <- NULL
  upset_plot <- NULL
  if (visualize_missing) {
    missingness_plot <- build_missingness_heatmap(data)
    has_missing <- any(is.na(data))
    if (use_upset_missing && has_missing && safe_require("UpSetR")) {
      missing_df <- data
      missing_df[] <- lapply(missing_df, function(x) as.integer(is.na(x)))
      upset_plot <- UpSetR::upset(as.data.frame(missing_df), nsets = min(ncol(missing_df), 6), keep.order = TRUE)
    }
  }

  heatmap_plot <- NULL
  if ("heatmap" %in% visualize && !is.null(correlation_matrix)) {
    heatmap_plot <- build_heatmap_plot(correlation_matrix, interactive)
  }

  histogram_plots <- NULL
  if ("histogram" %in% visualize && length(numeric_cols) > 0) {
    histogram_plots <- lapply(numeric_cols, function(col) {
      p <- ggplot2::ggplot(data_vis, ggplot2::aes(x = .data[[col]])) +
        ggplot2::geom_histogram(bins = 30, fill = "steelblue", color = "white") +
        ggplot2::labs(title = paste("Histogram of", col), x = col, y = "Count") +
        ggplot2::theme_minimal()
      if (!is.null(label) && label %in% factor_cols && grouped_plots) {
        p <- ggplot2::ggplot(data_vis, ggplot2::aes(x = .data[[col]], fill = .data[[label]])) +
          ggplot2::geom_histogram(position = "identity", alpha = 0.6, bins = 30, color = "white") +
          ggplot2::labs(title = paste("Histogram of", col, "by", label), x = col, y = "Count", fill = label) +
          ggplot2::theme_minimal()
      }
      p
    })
    names(histogram_plots) <- numeric_cols
  }

  boxplot_plots <- NULL
  if ("boxplot" %in% visualize && length(numeric_cols) > 0) {
    boxplot_plots <- lapply(numeric_cols, function(col) {
      p <- ggplot2::ggplot(data_vis, ggplot2::aes(y = .data[[col]])) +
        ggplot2::geom_boxplot(fill = "tomato") +
        ggplot2::labs(title = paste("Boxplot of", col), x = NULL, y = col) +
        ggplot2::theme_minimal()
      if (!is.null(label) && label %in% factor_cols && grouped_plots) {
        p <- ggplot2::ggplot(data_vis, ggplot2::aes(x = .data[[label]], y = .data[[col]], fill = .data[[label]])) +
          ggplot2::geom_boxplot() +
          ggplot2::labs(title = paste("Boxplot of", col, "by", label), x = label, y = col, fill = label) +
          ggplot2::theme_minimal()
      }
      p
    })
    names(boxplot_plots) <- numeric_cols
  }

  bar_plots <- NULL
  if ("barplot" %in% visualize && length(factor_cols) > 0) {
    bar_plots <- lapply(factor_cols, function(col) {
      ggplot2::ggplot(data_vis, ggplot2::aes(x = .data[[col]])) +
        ggplot2::geom_bar(fill = "forestgreen") +
        ggplot2::labs(title = paste("Bar plot of", col), x = col, y = "Count") +
        ggplot2::theme_minimal()
    })
    names(bar_plots) <- factor_cols
  }

  scatter_plots <- NULL
  if ("scatterplot" %in% visualize && length(numeric_cols) > 1) {
    pairs_df <- NULL
    if (!is.null(correlation_matrix)) {
      upper_tri <- correlation_matrix
      upper_tri[lower.tri(upper_tri, diag = TRUE)] <- NA
      pairs_df <- reshape2::melt(upper_tri, varnames = c("Var1", "Var2"), value.name = "Correlation")
      pairs_df <- pairs_df[!is.na(pairs_df$Correlation), ]
      pairs_df <- pairs_df[order(abs(pairs_df$Correlation), decreasing = TRUE), ]
    }

    if (!is.null(pairs_df) && nrow(pairs_df) > 0) {
      top_pairs <- utils::head(pairs_df, 3)
    } else {
      combos <- utils::combn(numeric_cols, 2)
      top_pairs <- tibble::tibble(Var1 = combos[1, ], Var2 = combos[2, ], Correlation = NA_real_)
      top_pairs <- utils::head(top_pairs, 3)
    }

    scatter_plots <- apply(top_pairs, 1, function(row) {
      varx <- row[["Var1"]]
      vary <- row[["Var2"]]
      p <- ggplot2::ggplot(data_vis, ggplot2::aes(x = .data[[varx]], y = .data[[vary]])) +
        ggplot2::geom_point(color = "darkblue", alpha = 0.7) +
        ggplot2::labs(title = paste("Scatterplot:", varx, "vs", vary),
                      subtitle = if (!is.na(row[["Correlation"]])) paste("Correlation =", round(as.numeric(row[["Correlation"]]), 3)) else NULL) +
        ggplot2::theme_minimal()
      if (!is.null(label) && label %in% factor_cols && grouped_plots) {
        p <- ggplot2::ggplot(data_vis, ggplot2::aes(x = .data[[varx]], y = .data[[vary]], color = .data[[label]])) +
          ggplot2::geom_point(alpha = 0.7) +
          ggplot2::labs(title = paste("Scatterplot:", varx, "vs", vary, "by", label), color = label,
                        subtitle = if (!is.na(row[["Correlation"]])) paste("Correlation =", round(as.numeric(row[["Correlation"]]), 3)) else NULL) +
          ggplot2::theme_minimal()
      }
      p
    })
    names(scatter_plots) <- paste0("scatter_", seq_along(scatter_plots))
  }

  pairwise_plot <- NULL
  if (pairwise_matrix && length(numeric_cols) > 1 && safe_require("GGally")) {
    numeric_subset <- numeric_cols[seq_len(min(length(numeric_cols), max_scatter_cols))]
    pairwise_plot <- GGally::ggpairs(data_vis[, numeric_subset, drop = FALSE])
  }

  results_list <- list(
    data_overview = data_overview,
    summary_stats = summary_stats,
    freq_tables = freq_tables,
    missing_data = missing_data,
    duplicated_rows = duplicated_rows,
    duplicated_examples = duplicated_examples,
    class_imbalance = class_imbalance,
    correlation_matrix = correlation_matrix,
    high_corr_pairs = high_corr_pairs,
    zero_variance_cols = zero_variance_cols,
    potential_id_cols = potential_id_cols,
    date_time_cols = date_time_cols,
    outlier_method = outlier_method,
    outlier_summary = outlier_summary,
    normality_tests = normality_results,
    plots = list(
      correlation_heatmap = heatmap_plot,
      histograms = histogram_plots,
      boxplots = boxplot_plots,
      barplots = bar_plots,
      scatterplots = scatter_plots,
      pairwise_matrix = pairwise_plot,
      missingness = missingness_plot,
      upset_missing = upset_plot,
      normality = normality_plots
    ),
    saved_paths = list()
  )

  if (imputation_suggestions && visualize_missing) {
    message("Imputation suggestion: consider median/mean for numeric columns with low missingness and mode/most frequent for factors.")
  }

  if (detect_near_duplicates) {
    message("Near-duplicate detection is not implemented yet.")
  }

  results_folder <- NULL
  if (save_results || render_report) {
    base_dir <- if (is.null(output_dir)) getwd() else output_dir
    results_folder <- file.path(base_dir, paste0("fastexplore_", format(Sys.time(), "%Y%m%d_%H%M%S")))
    dir.create(results_folder, showWarnings = FALSE, recursive = TRUE)
    results_list$saved_paths$results_folder <- results_folder
  }

  if (save_results && !is.null(results_folder)) {
    saveRDS(results_list, file.path(results_folder, "fastexplore_results.rds"))

    save_plot_collection(histogram_plots, file.path(results_folder, "histograms"))
    save_plot_collection(boxplot_plots, file.path(results_folder, "boxplots"))
    save_plot_collection(bar_plots, file.path(results_folder, "barplots"))
    save_plot_collection(scatter_plots, file.path(results_folder, "scatterplots"))

    if (!is.null(heatmap_plot)) {
      save_plot_collection(list(correlation_heatmap = heatmap_plot), file.path(results_folder, "heatmap"))
    }

    if (!is.null(pairwise_plot)) {
      save_plot_collection(list(pairwise_matrix = pairwise_plot), file.path(results_folder, "pairwise"))
    }

    if (!is.null(missingness_plot)) {
      save_plot_collection(list(missingness = missingness_plot), file.path(results_folder, "missingness"))
    }

    if (!is.null(normality_plots)) {
      save_plot_collection(normality_plots, file.path(results_folder, "normality"))
    }
  }

  if (render_report && !is.null(results_folder)) {
    if (!safe_require("rmarkdown")) {
      message("Package 'rmarkdown' is not installed; skipping report rendering.")
    } else {
      report_path <- file.path(results_folder, "fastexplore_report.Rmd")
      results_rds <- file.path(results_folder, "fastexplore_results.rds")
      if (!file.exists(results_rds)) {
        saveRDS(results_list, results_rds)
      }

      report_lines <- c(
        "---",
        "title: 'fastexplore EDA Report'",
        "output: html_document",
        "---",
        "",
        "```{r setup, include=FALSE}",
        "results <- readRDS('fastexplore_results.rds')",
        "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
        "```",
        "",
        "## Data Overview",
        "```{r}",
        "results$data_overview$dimensions",
        "results$data_overview$types",
        "head(results$data_overview$unique_values)",
        "```",
        "",
        "## Summary Stats",
        "```{r}",
        "if (!is.null(results$summary_stats)) knitr::kable(results$summary_stats)",
        "```",
        "",
        "## Missing Data",
        "```{r}",
        "knitr::kable(results$missing_data)",
        "```",
        "",
        "## Correlation",
        "```{r, fig.height=5, fig.width=6}",
        "if (!is.null(results$plots$correlation_heatmap)) results$plots$correlation_heatmap",
        "```"
      )

      writeLines(report_lines, con = report_path)
      rendered <- rmarkdown::render(report_path, output_dir = results_folder, quiet = TRUE)
      results_list$saved_paths$report <- rendered
    }
  }

  results_list
}

safe_require <- function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
}

save_plot_collection <- function(plot_list, folder) {
  if (is.null(plot_list) || length(plot_list) == 0) {
    return(invisible(NULL))
  }
  dir.create(folder, showWarnings = FALSE, recursive = TRUE)
  for (nm in names(plot_list)) {
    plt <- plot_list[[nm]]
    file_path <- file.path(folder, paste0(nm, ".png"))
    if (inherits(plt, c("gg", "ggplot"))) {
      ggplot2::ggsave(filename = file_path, plot = plt, width = 7, height = 5)
    } else if (inherits(plt, "plotly") && safe_require("htmlwidgets")) {
      htmlwidgets::saveWidget(plt, file = sub("\\.png$", ".html", file_path))
    }
  }
  invisible(NULL)
}

perform_normality_tests <- function(data, numeric_cols, normality_tests) {
  if (length(numeric_cols) == 0) {
    return(NULL)
  }
  results_df <- tibble::tibble(
    Column = character(),
    Test = character(),
    P_Value = numeric(),
    Adjusted_P_Value = numeric(),
    Normal = character()
  )

  for (col in numeric_cols) {
    col_data <- na.omit(data[[col]])
    if (length(col_data) == 0) next

    if ("shapiro" %in% normality_tests && length(col_data) >= 3 && length(col_data) <= 5000) {
      sw <- stats::shapiro.test(col_data)
      results_df <- dplyr::bind_rows(results_df, tibble::tibble(
        Column = col,
        Test = "Shapiro-Wilk",
        P_Value = sw$p.value
      ))
    }

    if ("ks" %in% normality_tests && length(col_data) > 0) {
      ks <- suppressWarnings(stats::ks.test(x = scale(col_data), y = "pnorm"))
      results_df <- dplyr::bind_rows(results_df, tibble::tibble(
        Column = col,
        Test = "Kolmogorov-Smirnov",
        P_Value = ks$p.value
      ))
    }
  }

  if (nrow(results_df) == 0) {
    return(NULL)
  }

  results_df <- dplyr::group_by(results_df, .data$Test) |>
    dplyr::mutate(Adjusted_P_Value = stats::p.adjust(.data$P_Value, method = "BH")) |>
    dplyr::ungroup() |>
    dplyr::mutate(Normal = dplyr::if_else(.data$Adjusted_P_Value < 0.05, "No", "Yes"))

  results_df
}

generate_normality_plots <- function(data, numeric_cols) {
  if (length(numeric_cols) == 0) {
    return(NULL)
  }
  plots <- list()
  for (col in numeric_cols) {
    col_data <- data[[col]]
    if (all(is.na(col_data))) next
    hist_plot <- ggplot2::ggplot(data, ggplot2::aes(sample = .data[[col]])) +
      ggplot2::geom_histogram(ggplot2::aes(x = .data[[col]], y = ..density..), bins = 30, fill = "skyblue", color = "white") +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = paste("Histogram of", col), x = col, y = "Density")

    qq_plot <- ggplot2::ggplot(data, ggplot2::aes(sample = .data[[col]])) +
      ggplot2::stat_qq() +
      ggplot2::stat_qq_line(color = "red") +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = paste("Q-Q plot of", col), x = "Theoretical Quantiles", y = "Sample Quantiles")

    plots[[col]] <- list(histogram = hist_plot, qq = qq_plot)
  }
  plots
}

build_heatmap_plot <- function(correlation_matrix, interactive = FALSE) {
  if (is.null(correlation_matrix)) {
    return(NULL)
  }
  cor_df <- reshape2::melt(correlation_matrix, varnames = c("Var1", "Var2"), value.name = "Correlation")
  static_plot <- ggplot2::ggplot(cor_df, ggplot2::aes(x = Var1, y = Var2, fill = Correlation)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_fill_gradient2(low = "#2563eb", high = "#dc2626", mid = "white", midpoint = 0, limits = c(-1, 1)) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(title = "Correlation heatmap", x = NULL, y = NULL)

  if (interactive && safe_require("plotly")) {
    return(plotly::ggplotly(static_plot))
  }
  static_plot
}

build_missingness_heatmap <- function(data) {
  missing_long <- dplyr::mutate(data, .row = dplyr::row_number()) |>
    tidyr::pivot_longer(
      cols = - .row,
      names_to = "variable",
      values_to = "value",
      values_transform = list(value = as.character)
    ) |>
    dplyr::mutate(missing = is.na(.data$value))

  ggplot2::ggplot(missing_long, ggplot2::aes(x = variable, y = .row, fill = missing)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_fill_manual(values = c(`TRUE` = "#ef4444", `FALSE` = "#16a34a"), name = "Missing") +
    ggplot2::labs(title = "Missingness heatmap", x = NULL, y = "Row") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

detect_outliers_iqr <- function(x) {
  q1 <- stats::quantile(x, 0.25, na.rm = TRUE)
  q3 <- stats::quantile(x, 0.75, na.rm = TRUE)
  iqr_val <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr_val
  upper_bound <- q3 + 1.5 * iqr_val
  proportion <- mean(x < lower_bound | x > upper_bound, na.rm = TRUE)
  proportion
}

detect_outliers_zscore <- function(x, threshold = 3) {
  z <- scale(x)
  proportion <- mean(abs(z) > threshold, na.rm = TRUE)
  proportion
}

detect_outliers_dbscan <- function(data, eps = 0.5, minPts = 5) {
  db_res <- dbscan::dbscan(data, eps = eps, minPts = minPts)
  outlier_idx <- which(db_res$cluster == 0)
  length(outlier_idx) / nrow(data)
}

detect_outliers_lof <- function(data, minPts = 5, threshold = 1.5) {
  lof_vals <- dbscan::lof(data, minPts = minPts)
  outlier_idx <- which(lof_vals > threshold)
  length(outlier_idx) / nrow(data)
}

perform_outlier_detection <- function(data, numeric_cols, outlier_method) {
  if (length(numeric_cols) == 0) {
    return(NULL)
  }

  if (outlier_method == "iqr") {
    return(vapply(data[numeric_cols], detect_outliers_iqr, numeric(1)))
  }

  if (outlier_method == "zscore") {
    return(vapply(data[numeric_cols], detect_outliers_zscore, numeric(1)))
  }

  if (outlier_method %in% c("dbscan", "lof")) {
    if (!safe_require("dbscan")) {
      message(sprintf("Package 'dbscan' not installed; cannot run %s-based outlier detection.", outlier_method))
      return(NULL)
    }
    numeric_data <- data[, numeric_cols, drop = FALSE]
    numeric_data <- na.omit(numeric_data)
    if (nrow(numeric_data) == 0) {
      return(NULL)
    }
    if (outlier_method == "dbscan") {
      return(detect_outliers_dbscan(numeric_data))
    }
    if (outlier_method == "lof") {
      return(detect_outliers_lof(numeric_data))
    }
  }

  NULL
}
