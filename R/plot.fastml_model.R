utils::globalVariables(c("Model", "value", "variable", "for"))

#' Plot Function for fastml_model
#'
#' Generates plots to compare the performance of different models.
#'
#' @param x An object of class \code{fastml_model}.
#' @param ... Additional arguments (not used).
#' @return Displays comparison plots of model performances.
#'
#' @importFrom ggplot2 ggplot aes geom_bar facet_wrap theme_bw theme element_text labs
#' @importFrom reshape2 melt
#' @export
plot.fastml_model <- function(x, ...) {
  # Ensure x is a valid fastml_model
  if (!inherits(x, "fastml_model")) {
    stop("The input must be a 'fastml_model' object.")
  }

  # Extract performance metrics
  performance <- x$performance

  # Define the metrics to plot
  metric_names <- c("Accuracy", "Kappa", "Sensitivity", "Specificity", "Precision", "F1")

  # Initialize performance data frame
  performance_df <- data.frame(Model = names(performance),
                               stringsAsFactors = FALSE)

  # Populate the data frame with metric values
  for (metric in metric_names) {
    performance_df[[metric]] <- sapply(performance, function(perf) {
      if (!is.null(perf[[metric]])) {
        return(perf[[metric]])
      } else {
        return(NA)
      }
    })
  }

  # Load ggplot2 and reshape2 for plotting
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The 'ggplot2' package is required but not installed.")
  }
  if (!requireNamespace("reshape2", quietly = TRUE)) {
    stop("The 'reshape2' package is required but not installed.")
  }

  # Melt the data frame for plotting
  performance_melt <- melt(performance_df, id.vars = "Model")

  # Generate the plot
  p <- ggplot(performance_melt,
              aes(x = Model, y = value, fill = variable)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~ variable, scales = "free_y") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Model Performance Comparison", x = "Model", y = "Metric Value")

  # Display the plot
  print(p)
}
