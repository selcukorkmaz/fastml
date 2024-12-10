#' Summary Function for fastml_model
#'
#' Provides a concise, user-friendly summary of model performances.
#' For classification, shows Accuracy, F1 Score, Kappa, Precision, ROC AUC, Sensitivity, Specificity.
#' Produces a bar plot of these metrics and ROC curves in the same RStudio device.
#'
#' @param object An object of class \code{fastml_model}.
#' @param sort_metric The metric to sort by. Default uses optimized metric.
#' @param plot Logical. If TRUE, produce bar plot and ROC curves.
#' @param combined_roc Logical. If TRUE, combined ROC plot; else separate ROC plots.
#' @param notes User-defined commentary.
#' @param ... Not used.
#' @return Prints summary and plots if requested.
#'
#' @importFrom dplyr filter select mutate bind_rows
#' @importFrom magrittr %>%
#' @importFrom reshape2 melt dcast
#' @importFrom tune extract_fit_parsnip
#' @importFrom ggplot2 ggplot aes geom_bar facet_wrap theme_bw theme element_text labs
#' @importFrom pROC roc plot.roc
#' @importFrom RColorBrewer brewer.pal
#' @export
summary.fastml_model <- function(object,
                                 sort_metric = NULL,
                                 plot = TRUE,
                                 combined_roc = TRUE,
                                 notes = "",
                                 ...) {
  if (!inherits(object, "fastml_model")) {
    stop("The input must be a 'fastml_model' object.")
  }

  performance <- object$performance
  predictions_list <- object$predictions
  task <- object$task
  best_model_name <- object$best_model_name
  optimized_metric <- object$metric
  model_count <- length(object$models)

  # Combine performance metrics
  metrics_list <- lapply(names(performance), function(mn) {
    df <- as.data.frame(performance[[mn]])
    df$Model <- mn
    df
  })
  performance_df <- do.call(rbind, metrics_list)

  all_metric_names <- unique(performance_df$.metric)
  if (is.null(sort_metric)) {
    if (optimized_metric %in% all_metric_names) {
      main_metric <- optimized_metric
    } else {
      main_metric <- all_metric_names[1]
      warning("Optimized metric not available; using first metric.")
    }
  } else {
    if (!(sort_metric %in% all_metric_names)) {
      stop("Invalid sort_metric. Available: ", paste(all_metric_names, collapse = ", "))
    }
    main_metric <- sort_metric
  }

  # Desired metrics for classification
  if (task == "classification") {
    desired_metrics <- c("accuracy", "f_meas", "kap", "precision", "sens", "spec", "roc_auc")
  } else {
    desired_metrics <- c("rmse", "rsq", "mae")
  }
  desired_metrics <- intersect(desired_metrics, all_metric_names)
  if (length(desired_metrics) == 0) desired_metrics <- main_metric

  performance_sub <- performance_df[performance_df$.metric %in% desired_metrics, ]
  performance_wide <- reshape2::dcast(performance_sub, Model ~ .metric, value.var = ".estimate")

  if (task == "regression") {
    performance_wide <- performance_wide[order(performance_wide[[main_metric]], na.last = TRUE), ]
  } else {
    performance_wide <- performance_wide[order(-performance_wide[[main_metric]], na.last = TRUE), ]
  }

  display_names <- c(
    accuracy = "Accuracy",
    f_meas = "F1 Score",
    kap = "Kappa",
    precision = "Precision",
    roc_auc = "ROC AUC",
    sens = "Sensitivity",
    spec = "Specificity",
    rsq = "R-squared",
    mae = "MAE",
    rmse = "RMSE"
  )

  cat("\n===== fastml Model Summary =====\n")
  cat("Task:", task, "\n")
  cat("Number of Models Trained:", model_count, "\n")
  best_val <- performance_wide[performance_wide$Model == best_model_name, main_metric]
  cat("Best Model:", best_model_name, sprintf("(%s: %.3f)", main_metric, best_val), "\n\n")

  cat("Performance Metrics (Sorted by", main_metric, "):\n\n")

  # Prepare table
  # Columns: Model and desired_metrics
  # Use format to align columns nicely
  # 3 decimal places for numeric
  metrics_to_print <- c("Model", desired_metrics)
  best_idx <- which(performance_wide$Model == best_model_name)

  # Convert numeric to strings with 3 decimals
  for (m in desired_metrics) {
    performance_wide[[m]] <- format(performance_wide[[m]], digits = 3, nsmall = 3)
  }

  # Rename metrics in header
  header <- c("Model", sapply(desired_metrics, function(m) {
    if (m %in% names(display_names)) display_names[[m]] else m
  }))

  # Compute column widths based on the widest entry in each column
  data_str <- performance_wide
  data_str$Model <- as.character(data_str$Model)
  if (length(best_idx) == 1) data_str$Model[best_idx] <- paste0(data_str$Model[best_idx], "*")

  col_widths <- sapply(seq_along(header), function(i) {
    col_name <- header[i]
    col_data <- data_str[[c("Model", desired_metrics)[i]]] # corresponding column in data_str
    max(nchar(col_name), max(nchar(col_data)))
  })

  # Print header line
  header_line <- paste(mapply(function(h, w) format(h, width = w, justify = "left"), header, col_widths), collapse = "  ")
  # Single line of dashes
  line_sep <- paste(rep("-", sum(col_widths) + 2*(length(col_widths)-1)), collapse = "")

  cat(line_sep, "\n")
  cat(header_line, "\n")
  cat(line_sep, "\n")

  # Rows
  for (i in seq_len(nrow(data_str))) {
    row_line <- paste(mapply(function(v, w) format(v, width = w, justify = "left"),
                             data_str[i, c("Model", desired_metrics), drop=FALSE], col_widths),
                      collapse = "  ")
    cat(row_line, "\n")
  }

  cat(line_sep, "\n")
  cat("(* Best model)\n\n")

  # Best model hyperparams
  cat("Best Model Hyperparameters:\n\n")
  parsnip_fit <- tryCatch(tune::extract_fit_parsnip(object$best_model), error = function(e) NULL)
  if (is.null(parsnip_fit)) {
    cat("Could not extract final fitted model details.\n")
  } else if ("spec" %in% names(parsnip_fit) && "args" %in% names(parsnip_fit$spec)) {
    params <- parsnip_fit$spec$args
    if (length(params) > 0) {
      cleaned_params <- list()
      for (pname in names(params)) {
        val <- params[[pname]]
        if (inherits(val, "quosure")) {
          val <- tryCatch(eval(rlang::get_expr(val), envir = rlang::get_env(val)), error = function(e) val)
        }
        cleaned_params[[pname]] <- val
      }
      if (length(cleaned_params) == 0) {
        cat("No hyperparameters found.\n")
      } else {
        for (pname in names(cleaned_params)) {
          val <- cleaned_params[[pname]]
          if (is.numeric(val)) val <- as.character(val)
          cat(pname, ": ", val, "\n", sep = "")
        }
      }
    } else {
      cat("No hyperparameters found.\n")
    }
  } else {
    cat("No hyperparameters found.\n")
  }

  # Interpretation
  cat("\nInterpretation:\n")
  if (task == "classification") {
    cat("The best model shows strong discriminative ability. Close performances suggest ensemble or tuning.\n")
  } else {
    cat("The best model shows superior accuracy on key metrics.\n")
  }

  cat("\n(Optional) Additional diagnostics (calibration, residuals) for deeper insights.\n")

  # User notes
  if (nzchar(notes)) {
    cat("\nUser Notes:\n", notes, "\n", sep = "")
  }

  cat("=================================\n")

  if (!plot) return(invisible(object))

  # Bar plot
  performance_melt <- reshape2::melt(performance_wide, id.vars = "Model", variable.name = "Metric", value.name = "Value")
  performance_melt <- performance_melt[!is.na(performance_melt$Value), ]

  # Convert Value back to numeric for plotting
  performance_melt$Value <- as.numeric(performance_melt$Value)


  performance_melt$Metric <- as.character(performance_melt$Metric)

  # Rename metrics in performance_melt for plotting
  performance_melt$Metric <- ifelse(
    performance_melt$Metric %in% names(display_names),
    display_names[performance_melt$Metric],
    performance_melt$Metric
  )

  # if (task == "regression") {
  #   # Desired order for regression metrics:
  #   desired_order <- c("RMSE", "R-squared", "MAE")
  #   present_metrics <- unique(performance_melt$Metric)
  #   final_order <- intersect(desired_order, present_metrics)
  #   if (length(final_order) > 0) {
  #     performance_melt$Metric <- factor(performance_melt$Metric, levels = final_order)
  #   }
  # }

  if (task == "classification") {
    # Original order: accuracy, f_meas, kap, precision, sens, spec, roc_auc
    # Display names: Accuracy, F1 Score, Kappa, Precision, Sensitivity, Specificity, ROC AUC
    class_order <- c("Accuracy", "F1 Score", "Kappa", "Precision", "Sensitivity", "Specificity", "ROC AUC")

    # Intersect with present metrics to avoid errors if some are missing
    present_class_metrics <- intersect(class_order, unique(performance_melt$Metric))
    if (length(present_class_metrics) > 0) {
      performance_melt$Metric <- factor(performance_melt$Metric, levels = present_class_metrics)
    }

  } else if (task == "regression") {
    # Desired order: rmse, rsq, mae
    # Display names: RMSE, R-squared, MAE
    reg_order <- c("RMSE", "R-squared", "MAE")

    present_reg_metrics <- intersect(reg_order, unique(performance_melt$Metric))
    if (length(present_reg_metrics) > 0) {
      performance_melt$Metric <- factor(performance_melt$Metric, levels = present_reg_metrics)
    }
  }

  p_bar <- ggplot2::ggplot(performance_melt, ggplot2::aes(x = Model, y = Value, fill = Model)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::facet_wrap(~ Metric, scales = "free_y") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "none"
    ) +
    ggplot2::labs(title = "Model Performance Comparison", x = "Model", y = "Metric Value")

  print(p_bar)

  # ROC curves for binary classification
  if (task == "classification" && !is.null(predictions_list) && length(predictions_list) > 0) {
    model_names_pred <- names(predictions_list)
    if (length(model_names_pred) > 0) {
      any_model_name <- model_names_pred[1]
      df_example <- predictions_list[[any_model_name]]
      if (!is.null(df_example) && "truth" %in% names(df_example)) {
        unique_classes <- unique(df_example$truth)
        if (length(unique_classes) == 2) {
          positive_class <- levels(df_example$truth)[2]
          roc_objs <- list()

          for (model_name in model_names_pred) {
            df <- predictions_list[[model_name]]
            prob_cols <- grep("^\\.pred_", names(df), value = TRUE)
            if (length(prob_cols) == 2) {
              pred_col <- paste0(".pred_", positive_class)
              if (pred_col %in% prob_cols) {
                # direction = "auto"
                roc_obj <- suppressMessages(
                                    pROC::roc(response = df$truth,
                                     predictor = df[[pred_col]],
                                     levels = levels(df$truth),
                                     positive = positive_class,
                                     direction = "auto")
                          )
                  roc_objs[[model_name]] <- roc_obj

              }
            }
          }

          if (length(roc_objs) > 0) {
            num_curves <- length(roc_objs)
            if (num_curves <= 8) {
              colors <- RColorBrewer::brewer.pal(num_curves, "Set1")
            } else if (num_curves <= 12) {
              colors <- RColorBrewer::brewer.pal(num_curves, "Set3")
            } else {
              colors <- colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))(num_curves)
            }

            model_names <- names(roc_objs)
            if (combined_roc) {
              first_model <- model_names[1]
              pROC::plot.roc(roc_objs[[first_model]], col = colors[1], lwd = 2,
                             main = "Combined ROC Curves for All Models",
                             legacy.axes = TRUE)
              if (num_curves > 1) {
                for (i in 2:num_curves) {
                  pROC::plot.roc(roc_objs[[model_names[i]]], col = colors[i], add = TRUE, lwd = 2, legacy.axes = TRUE)
                }
              }
              legend("bottomright", legend = model_names, col = colors, lwd = 2, cex = 0.8, bty = "n")
            } else {
              # Separate ROC curves
              for (i in seq_along(model_names)) {
                pROC::plot.roc(roc_objs[[model_names[i]]], col = colors[i], lwd = 2,
                               main = paste("ROC Curve:", model_names[i]),
                               legacy.axes = TRUE)
              }
            }
          } else {
            cat("\nNo suitable probability predictions for ROC curves.\n")
          }
        } else {
          cat("\nROC curves are only generated for binary classification tasks.\n")
        }
      } else {
        cat("\nNo predictions available to generate ROC curves.\n")
      }
    } else {
      cat("\nNo predictions available to generate ROC curves.\n")
    }
  }

  invisible(object)
}
