utils::globalVariables(c("Model", "Value", "Measure"))

#' Plot Function for fastml_model
#'
#' Generates plots to compare the performance of different models.
#'
#' @param x An object of class \code{fastml_model}.
#' @param ... Additional arguments (not used).
#' @return Displays comparison plots of model performances.
#' @importFrom ggplot2 ggplot aes geom_bar facet_wrap theme_bw theme element_text labs
#' @export
plot.fastml_model <- function(x, ...) {
  # Ensure x is a valid fastml_model
  if (!inherits(x, "fastml_model")) {
    stop("The input must be a 'fastml_model' object.")
  }

  # Extract performance metrics
  performance <- x$performance

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

  # Remove any rows with NA values in .estimate
  performance_df <- performance_df[!is.na(performance_df$.estimate), ]

  # Load ggplot2 for plotting
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The 'ggplot2' package is required but not installed.")
  }

  # Plot performance metrics
  p <- ggplot2::ggplot(performance_df,
                       ggplot2::aes(x = Model, y = .estimate, fill = Model)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::facet_wrap(~ .metric, scales = "free_y") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   legend.position = "none") +
    ggplot2::labs(title = "Model Performance Comparison", x = "Model", y = "Metric Value")

  # Display the plot
  print(p)
}
