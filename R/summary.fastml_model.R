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
#' @importFrom reshape2 melt dcast
#' @export
summary.fastml_model <- function(object, sort_metric = NULL, ...) {
  # Ensure object is a valid fastml_model
  if (!inherits(object, "fastml_model")) {
    stop("The input must be a 'fastml_model' object.")
  }

  # Extract performance metrics
  performance <- object$performance

  # Initialize a list to collect metrics
  metrics_list <- list()

  # Iterate over each model's performance
  for (model_name in names(performance)) {
    model_metrics <- performance[[model_name]]
    # Convert the tibble to a data frame
    model_metrics_df <- as.data.frame(model_metrics)
    # Add the Model name to the data frame
    model_metrics_df$Model <- model_name
    # Append to the metrics_list
    metrics_list[[model_name]] <- model_metrics_df
  }

  # Combine all metrics into one data frame
  performance_df <- do.call(rbind, metrics_list)

  # Reorder columns so 'Model' is first
  performance_df <- performance_df[, c("Model", names(performance_df)[names(performance_df) != "Model"])]

  # Define all possible metrics
  all_metric_names <- unique(performance_df$.metric)

  # Determine the main metric used for sorting
  # Prioritize sort_metric if provided and available, else use the optimized metric
  if (!is.null(sort_metric)) {
    if (!(sort_metric %in% all_metric_names)) {
      stop(paste("Invalid sort_metric. Choose from:", paste(all_metric_names, collapse = ", ")))
    }
    main_metric <- sort_metric
  } else {
    optimized_metric <- object$metric
    if (optimized_metric %in% all_metric_names) {
      main_metric <- optimized_metric
    } else {
      main_metric <- all_metric_names[1]
      warning(paste("Optimized metric", optimized_metric, "is not available. Using", main_metric, "as the sorting metric."))
    }
  }

  # Create a summary table by pivoting the data
  performance_wide <- dcast(performance_df, Model ~ .metric, value.var = ".estimate")

  # Ensure main_metric exists in performance_wide
  if (!(main_metric %in% names(performance_wide))) {
    stop(paste("The main metric", main_metric, "is not present in the performance metrics."))
  }

  # Sort by the main metric, handling NA values
  if (object$task == "regression") {
    # For regression, lower metrics like RMSE are better
    performance_wide <- performance_wide[order(performance_wide[[main_metric]], na.last = TRUE), ]
  } else {
    # For classification, higher metrics like Accuracy are better
    performance_wide <- performance_wide[order(-performance_wide[[main_metric]], na.last = TRUE), ]
  }

  # Print the best model
  best_model_name <- object$best_model_name
  cat("\n===== fastml Model Summary =====\n")
  cat("Best Model:", best_model_name, "\n\n")

  # Print the performance table
  cat("Performance Metrics for All Models:\n\n")
  print(performance_wide, row.names = FALSE)

  # Check if best model has hyperparameters
  best_model <- object$best_model
  if ("spec" %in% names(best_model$fit$actions$model)) {
    # Extract the hyperparameters
    best_params <- best_model$fit$actions$model$spec$args
    cat("\nBest Model Hyperparameters:\n\n")
    print(best_params, row.names = FALSE)
  } else {
    cat("\nBest Model Hyperparameters are not available.\n")
  }

  # Generate comparison plots
  cat("\nGenerating performance comparison plots...\n")

  # Load ggplot2 and reshape2 for plotting
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The 'ggplot2' package is required but not installed.")
  }
  if (!requireNamespace("reshape2", quietly = TRUE)) {
    stop("The 'reshape2' package is required but not installed.")
  }

  # Melt the wide data frame for plotting
  performance_melt <- melt(performance_wide, id.vars = "Model", variable.name = "Measure", value.name = "Value")

  # Remove rows where the metric value is NA to clean the plot
  performance_melt <- performance_melt[!is.na(performance_melt$Value), ]

  # Plot performance metrics
  p <- ggplot(performance_melt,
                       aes(x = Model, y = Value, fill = Measure)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~ Measure, scales = "free_y") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Model Performance Comparison", x = "Model", y = "Metric Value")

  print(p)

  cat("\nTo make predictions, use the 'predict' function.\n")
  cat("=================================\n")
}
