utils::globalVariables(c("Model", "value", "variable", "for", "Measure"))

#' Summary Function for fastml_model
#'
#' Provides a detailed summary of the models' performances.
#'
#' @param object An object of class \code{fastml_model}.
#' @param sort_metric A string specifying which metric to sort the models by.
#'                    Default is \code{NULL}, which prioritizes the optimized metric.
#' @param ... Additional arguments (not used).
#' @return Prints a summary of the models' performances and displays comparison plots.
#'
#' @importFrom ggplot2 ggplot aes geom_bar facet_wrap theme_bw theme element_text labs
#' @importFrom reshape2 melt
#' @export
summary.fastml_model <- function(object, sort_metric = NULL, ...) {
  # Ensure object is a valid fastml_model
  if (!inherits(object, "fastml_model")) {
    stop("The input must be a 'fastml_model' object.")
  }

  # Extract performance metrics
  performance <- object$performance

  # Define all possible metrics
  if (object$task == "classification") {
    all_metric_names <- c("Accuracy", "Kappa", "Sensitivity", "Specificity", "Precision", "F1", "ROC")
  } else {
    all_metric_names <- c("RMSE", "MAE", "Rsquared")
  }

  # Determine which metrics are available (have at least one non-NA value)
  available_metrics <- all_metric_names[sapply(all_metric_names, function(metric) {
    any(sapply(performance, function(x) !is.null(x[[metric]]) && !is.na(x[[metric]])))
  })]

  if (length(available_metrics) == 0) {
    stop("No performance metrics available to summarize.")
  }

  # Collect metrics into a data frame
  performance_df <- data.frame(Model = names(performance), stringsAsFactors = FALSE)

  for (metric in available_metrics) {
    performance_df[[metric]] <- sapply(performance, function(x) {
      if (is.list(x) && !is.null(x[[metric]])) {
        return(as.numeric(x[[metric]]))
      } else {
        return(NA)
      }
    })
  }

  # Determine the main metric used for sorting
  # Prioritize sort_metric if provided and available, else use the optimized metric
  if (!is.null(sort_metric)) {
    if (!(sort_metric %in% available_metrics)) {
      stop(paste("Invalid sort_metric. Choose from:", paste(available_metrics, collapse = ", ")))
    }
    main_metric <- sort_metric
  } else {
    optimized_metric <- object$metric
    if (optimized_metric %in% available_metrics) {
      main_metric <- optimized_metric
    } else {
      main_metric <- available_metrics[1]
      warning(paste("Optimized metric", optimized_metric, "is not available. Using", main_metric, "as the sorting metric."))
    }
  }

  # Ensure main_metric exists in performance_df
  if (!(main_metric %in% names(performance_df))) {
    stop(paste("The main metric", main_metric, "is not present in the performance metrics."))
  }

  # Sort by the main metric, handling NA values
  if (object$task == "regression") {
    # For regression, lower metrics like RMSE are better
    performance_df <- performance_df[order(performance_df[[main_metric]], na.last = TRUE), ]
  } else {
    # For classification, higher metrics like Accuracy are better
    performance_df <- performance_df[order(-performance_df[[main_metric]], na.last = TRUE), ]
  }

  # Print the best model
  best_model_name <- object$best_model_name
  cat("\n===== fastml Model Summary =====\n")
  cat("Best Model:", best_model_name, "\n\n")

  # Print the performance table
  cat("Performance Metrics for All Models:\n\n")
  print(performance_df, row.names = FALSE)

  # Print the hyperparameters of the best model
  cat("\nBest Model Hyperparameters:\n\n")
  print(object$best_model$bestTune, row.names = FALSE)

  # Generate comparison plots
  cat("\nGenerating performance comparison plots...\n")

  # Load ggplot2 and reshape2 for plotting
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The 'ggplot2' package is required but not installed.")
  }
  if (!requireNamespace("reshape2", quietly = TRUE)) {
    stop("The 'reshape2' package is required but not installed.")
  }

  # Melt the data frame for plotting
  performance_melt <- reshape2::melt(performance_df, id.vars = "Model")

  # Remove rows where the metric value is NA to clean the plot
  performance_melt <- performance_melt[!is.na(performance_melt$value), ]
  colnames(performance_melt)[2] <- "Measure"

  # Plot performance metrics
  p <- ggplot2::ggplot(performance_melt,
                       ggplot2::aes(x = Model, y = value, fill = Measure)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::facet_wrap(~ Measure, scales = "free_y") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(title = "Model Performance Comparison", x = "Model", y = "Metric Value")

  print(p)

  cat("\nTo make predictions, use the 'predict' function.\n")
  cat("=================================\n")
}
