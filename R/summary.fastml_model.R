#' Summary Function for fastml_model
#'
#' Provides a concise and user-friendly summary of the models' performances.
#' If \code{plot = TRUE}, also generates a bar plot of metrics using ggplot2.
#' For classification tasks, if \code{plot = TRUE} and \code{combined_roc = TRUE},
#' a single ROC plot with all curves is displayed using pROC. If \code{combined_roc = FALSE},
#' separate ROC plots are displayed for each algorithm using pROC.
#'
#' @param object An object of class \code{fastml_model}.
#' @param sort_metric A string specifying which metric to sort the models by.
#'                    Default is \code{NULL}, which prioritizes the optimized metric.
#' @param plot Logical. If \code{TRUE}, produce a bar plot of metrics and also ROC curves for binary classification tasks.
#' @param combined_roc Logical. If \code{TRUE}, shows a single combined ROC plot for all models.
#'                     If \code{FALSE}, shows separate ROC curves for each model.
#' @param ... Additional arguments (not used).
#' @return Prints a concise and user-friendly summary of the models' performances, hyperparameters,
#'         and optionally plots performance bar charts and ROC curves.
#'
#' @importFrom dplyr filter select mutate bind_rows
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
#' @importFrom reshape2 melt dcast
#' @importFrom tune extract_fit_parsnip
#' @importFrom ggplot2 ggplot aes geom_line geom_bar facet_wrap theme_bw theme element_text labs scale_color_discrete
#' @importFrom rlang sym syms get_expr get_env
#' @importFrom pROC roc coords plot.roc
#' @importFrom RColorBrewer brewer.pal
#' @export
summary.fastml_model <- function(object, sort_metric = NULL, plot = TRUE, combined_roc = TRUE, ...) {
  # Ensure object is a valid fastml_model
  if (!inherits(object, "fastml_model")) {
    stop("The input must be a 'fastml_model' object.")
  }

  performance <- object$performance
  predictions_list <- object$predictions

  # Combine all performance metrics into one data frame
  metrics_list <- list()
  for (model_name in names(performance)) {
    model_metrics <- performance[[model_name]]
    model_metrics_df <- as.data.frame(model_metrics)
    model_metrics_df$Model <- model_name
    metrics_list[[model_name]] <- model_metrics_df
  }

  performance_df <- do.call(rbind, metrics_list)

  # Determine main metric for sorting
  all_metric_names <- unique(performance_df$.metric)
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

  # Pivot wider for performance metrics
  performance_wide <- reshape2::dcast(performance_df, Model ~ .metric, value.var = ".estimate")

  # Sort by main_metric
  if (object$task == "regression") {
    performance_wide <- performance_wide[order(performance_wide[[main_metric]], na.last = TRUE), ]
  } else {
    performance_wide <- performance_wide[order(-performance_wide[[main_metric]], na.last = TRUE), ]
  }

  best_model_name <- object$best_model_name

  cat("\n===== fastml Model Summary =====\n")
  cat("Best Model:", best_model_name, "\n\n")

  cat("Performance Metrics for All Models:\n\n")
  print(performance_wide, row.names = FALSE)
  cat("\n")

  cat("Best Model Hyperparameters:\n\n")

  # Attempt to extract the final fitted parsnip model
  parsnip_fit <- tryCatch({
    tune::extract_fit_parsnip(object$best_model)
  }, error = function(e) {
    NULL
  })

  if (is.null(parsnip_fit)) {
    cat("Could not extract final fitted model details.\n")
  } else {
    if ("spec" %in% names(parsnip_fit) && "args" %in% names(parsnip_fit$spec)) {
      params <- parsnip_fit$spec$args
      if (length(params) > 0) {
        cleaned_params <- list()
        for (pname in names(params)) {
          val <- params[[pname]]
          if (inherits(val, "quosure")) {
            evaluated_val <- tryCatch(eval(rlang::get_expr(val), envir = rlang::get_env(val)),
                                      error = function(e) val)
            cleaned_params[[pname]] <- evaluated_val
          } else {
            cleaned_params[[pname]] <- val
          }
        }

        printed_any <- FALSE
        for (pname in names(cleaned_params)) {
          val <- cleaned_params[[pname]]
          if (is.numeric(val)) {
            val <- as.character(val)
          }
          cat(pname, ": ", val, "\n", sep = "")
          printed_any <- TRUE
        }

        if (!printed_any) {
          cat("No hyperparameters found.\n")
        }
      } else {
        cat("No hyperparameters found.\n")
      }
    } else {
      cat("No hyperparameters found.\n")
    }
  }

  cat("\nTo make predictions, use the 'predict' function.\n")
  cat("=================================\n")

  # If plot = FALSE, do not generate plots
  if (!plot) {
    return(invisible(object))
  }

  # Create a bar plot of performance metrics using ggplot2
  performance_melt <- reshape2::melt(performance_wide, id.vars = "Model", variable.name = "Metric", value.name = "Value")
  performance_melt <- performance_melt[!is.na(performance_melt$Value), ]

  p_bar <- ggplot2::ggplot(performance_melt, ggplot2::aes(x = Model, y = Value, fill = Model)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::facet_wrap(~ Metric, scales = "free_y") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "none"
    ) +
    ggplot2::labs(title = "Model Performance Comparison", x = "Model", y = "Metric Value")

  # Print the bar plot
  print(p_bar)

  # If classification and binary, produce ROC curves using pROC base plotting
  if (object$task == "classification") {
    any_model_name <- names(predictions_list)[1]
    df_example <- predictions_list[[any_model_name]]
    truth <- df_example$truth
    num_classes <- length(unique(truth))

    if (num_classes == 2) {
      positive_class <- levels(truth)[2]
      roc_objs <- list()

      # Create roc object for each model
      for (model_name in names(predictions_list)) {
        df <- predictions_list[[model_name]]
        prob_cols <- grep("^\\.pred_", names(df), value = TRUE)
        if (length(prob_cols) == 2) {
          pred_col <- paste0(".pred_", positive_class)
          if (pred_col %in% prob_cols) {
            roc_obj <- pROC::roc(response = df$truth,
                                 predictor = df[[pred_col]],
                                 levels = levels(df$truth),
                                 positive = positive_class)
            roc_objs[[model_name]] <- roc_obj
          }
        }
      }

      if (length(roc_objs) > 0) {
        # Colors: Use RColorBrewer for up to 16 curves
        num_curves <- length(roc_objs)
        if (num_curves <= 8) {
          colors <- RColorBrewer::brewer.pal(num_curves, "Set1")
        } else if (num_curves <= 12) {
          colors <- RColorBrewer::brewer.pal(num_curves, "Set3")
        } else {
          # More than 12, create a color ramp from Set3
          colors <- colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))(num_curves)
        }

        # Open a new device if supported, so that ROC curves appear separately
        if (capabilities("X11") || capabilities("cairo") || capabilities("quartz")) {
          dev.new()
        }

        model_names <- names(roc_objs)

        if (combined_roc) {
          # Combined ROC: plot the first ROC, then add others
          first_model <- model_names[1]
          pROC::plot.roc(roc_objs[[first_model]], col = colors[1], main = "Combined ROC Curves for All Models")
          if (num_curves > 1) {
            for (i in 2:num_curves) {
              pROC::plot.roc(roc_objs[[model_names[i]]], col = colors[i], add = TRUE)
            }
          }
          legend("bottomright", legend = model_names, col = colors, lwd = 2, cex = 0.8)
        } else {
          # Separate ROC plots for each model
          # If multiple models, it might overwrite. The user can navigate back to previous plots if needed.
          # Or open a new device for each curve.
          i <- 1
          for (model_name in model_names) {
            if (capabilities("X11") || capabilities("cairo") || capabilities("quartz")) {
              dev.new()
            }
            pROC::plot.roc(roc_objs[[model_name]],
                           main = paste("ROC Curve:", model_name),
                           col = colors[i])
            i <- i + 1
          }
        }
      } else {
        cat("\nCould not generate ROC curves. No suitable probability predictions were returned by the models.\n")
      }
    } else {
      cat("\nROC curves are only generated for binary classification tasks.\n")
    }
  }

  invisible(object)
}
