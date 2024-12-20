utils::globalVariables(c("truth", "residual", "sensitivity", "specificity", "FalsePositiveRate", "TruePositiveRate"))

#' Summary Function for fastml_model (Using yardstick for ROC Curves)
#'
#' Provides a concise, user-friendly summary of model performances.
#' For classification:
#' - Shows Accuracy, F1 Score, Kappa, Precision, ROC AUC, Sensitivity, Specificity.
#' - Produces a bar plot of these metrics.
#' - Shows ROC curves for binary classification using yardstick::roc_curve().
#' - Displays a confusion matrix and a calibration plot if probabilities are available.
#'
#' For regression:
#' - Shows RMSE, R-squared, and MAE.
#' - Produces a bar plot of these metrics.
#' - Displays residual diagnostics (truth vs predicted, residual distribution).
#'
#'
#' @param object An object of class \code{fastml_model}.
#' @param algorithm A vector of algorithm names to display summary. Default is \code{"best"}.
#' @param sort_metric The metric to sort by. Default uses optimized metric.
#' @param plot Logical. If TRUE, produce bar plot, yardstick-based ROC curves (for binary classification),
#'   confusion matrix (classification), smooth calibration plot (if probabilities),
#'   and residual plots (regression).
#' @param combined_roc Logical. If TRUE, combined ROC plot; else separate ROC plots.
#' @param notes User-defined commentary.
#' @param ... Additional arguments.
#' @return Prints summary and plots if requested.
#'
#' @importFrom dplyr filter select mutate bind_rows group_by summarise n
#' @importFrom magrittr %>%
#' @importFrom reshape2 melt dcast
#' @importFrom tune extract_fit_parsnip
#' @importFrom ggplot2 ggplot aes geom_bar geom_path facet_wrap theme_bw theme element_text labs geom_point geom_line geom_histogram geom_abline coord_equal scale_color_manual theme_minimal element_blank
#' @importFrom RColorBrewer brewer.pal
#' @importFrom yardstick conf_mat
#' @importFrom pROC roc auc
#' @importFrom probably cal_plot_breaks
#' @importFrom rlang get_expr get_env sym
#' @importFrom viridisLite viridis
#'
#' @export
summary.fastml_model <- function(object,
                                 algorithm = "best",
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
  positive_class <- object$positive_class

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

  if (task == "classification") {
    desired_metrics <- c("accuracy", "f_meas", "kap", "precision", "sens", "spec", "roc_auc")
  } else {
    desired_metrics <- c("rmse", "rsq", "mae")
  }
  desired_metrics <- intersect(desired_metrics, all_metric_names)
  if (length(desired_metrics) == 0) desired_metrics <- main_metric

  performance_sub <- performance_df[performance_df$.metric %in% desired_metrics, ]
  performance_wide <- dcast(performance_sub, Model ~ .metric, value.var = ".estimate")

  if (task == "regression" && main_metric != "rsq") {
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
  best_val <- unique(performance_wide[performance_wide$Model %in% best_model_name, main_metric])
  cat("Best Model(s):", best_model_name, sprintf("(%s: %.7f)", main_metric, best_val), "\n\n")

  cat("Performance Metrics (Sorted by", main_metric, "):\n\n")

  metrics_to_print <- c("Model", desired_metrics)

  best_model_idx <- which(performance_wide$Model %in% best_model_name)


  if(length(algorithm) == 1 && algorithm == "best"){
    selected_model_idx <- best_model_idx
    desired_models <- object$best_model
  }else{

    if(all(algorithm %in% names(object$models))){

      selected_model_idx <- which(performance_wide$Model %in% algorithm)
      desired_models <- object$models[algorithm]

    }else{

      stop("Not all specified algorithms entered correctly.")
    }
  }

  desired_model_name <- names(desired_models)

  for (m in desired_metrics) {
    performance_wide[[m]] <- format(performance_wide[[m]], digits = 7, nsmall = 7)
  }

  header <- c("Model", sapply(desired_metrics, function(m) {
    if (m %in% names(display_names)) display_names[[m]] else m
  }))

  data_str <- performance_wide
  data_str$Model <- as.character(data_str$Model)
  data_str$Model[best_model_idx] <- paste0(data_str$Model[best_model_idx], "*")

  col_widths <- sapply(seq_along(header), function(i) {
    col_name <- header[i]
    col_data <- data_str[[c("Model", desired_metrics)[i]]]
    max(nchar(col_name), max(nchar(col_data)))
  })

  header_line <- paste(mapply(function(h, w) format(h, width = w, justify = "left"), header, col_widths), collapse = "  ")
  line_sep <- paste(rep("-", sum(col_widths) + 2*(length(col_widths)-1)), collapse = "")

  cat(line_sep, "\n")
  cat(header_line, "\n")
  cat(line_sep, "\n")

  for (i in seq_len(nrow(data_str))) {
    row_line <- paste(mapply(function(v, w) format(v, width = w, justify = "left"),
                             data_str[i, c("Model", desired_metrics), drop=FALSE], col_widths),
                      collapse = "  ")
    cat(row_line, "\n")
  }

  cat(line_sep, "\n")
  cat("(*Best model)\n\n")


  if(length(algorithm) == 1 && algorithm == "best"){
    cat("Best Model Hyperparameters:\n\n")

  }else{

    cat("Selected Model Hyperparameters:\n\n")

  }

  if(length(desired_models) == 1){
    parsnip_fit <- tryCatch(extract_fit_parsnip(desired_models[[1]]), error = function(e) NULL)
    nms_parsnip_fit <- names(parsnip_fit)
    nms_parsnip_spec <- names(parsnip_fit$spec)
  } else {
    parsnip_fit <- tryCatch(lapply(desired_models, extract_fit_parsnip), error = function(e) NULL)
    nms_parsnip_fit <- unique(unlist(lapply(parsnip_fit, names)))
    nms_parsnip_spec <- unique(unlist(lapply(lapply(parsnip_fit, function(model) model$spec), names)))
  }

  if (is.null(parsnip_fit)) {
    cat("Could not extract final fitted model details.\n")
  } else if ("spec" %in% nms_parsnip_fit && "args" %in% nms_parsnip_spec) {

    if(length(desired_models) == 1){
      params <- parsnip_fit$spec$args
    }else{
      params <- lapply(parsnip_fit, function(model) model$spec$args)
    }
    if (length(params) > 0) {
      cleaned_params <- list()
      if(length(desired_models) == 1){
          for (pname in names(params)) {
        val <- params[[pname]]
        if (inherits(val, "quosure")) {
          val <- tryCatch(eval(get_expr(val), envir = get_env(val)), error = function(e) val)
        }
        cleaned_params[[pname]] <- val
      }
      }else{

        # Initialize a list to store cleaned parameters for each model
        cleaned_params_list <- list()

        # Process each model's parameters
        for (model_name in names(params)) {
          model_params <- params[[model_name]]
          cleaned_params <- list()

          for (pname in names(model_params)) {
            val <- model_params[[pname]]

            # Check if the parameter is a quosure
            if (inherits(val, "quosure")) {
              val <- tryCatch(
                eval(rlang::get_expr(val), envir = rlang::get_env(val)),
                error = function(e) val # Retain the quosure if evaluation fails
              )
            }

            # Add the cleaned value to the cleaned_params list
            cleaned_params[[pname]] <- val
          }

          # Store the cleaned parameters for the current model
          cleaned_params_list[[model_name]] <- cleaned_params
        }

      }
      if (length(cleaned_params) == 0) {
        cat("No hyperparameters found.\n")
      } else {

        if(length(desired_models) == 1){
          cat("Model:", desired_model_name, "\n")
          for (pname in names(cleaned_params)) {
            val <- cleaned_params[[pname]]
            if (is.numeric(val)) val <- as.character(val)
            cat("  ", pname, ": ", rlang::eval_tidy(val), "\n", sep = "")
          }
        }else{

          # Loop through the cleaned parameters for each model
          for (model_name in names(cleaned_params_list)) {
            cat("Model:", model_name, "\n")

            # Extract cleaned parameters for the current model
            cleaned_params <- cleaned_params_list[[model_name]]

            # Process and print each parameter
            for (pname in names(cleaned_params)) {
              val <- cleaned_params[[pname]]

              # Convert numeric values to character for consistent output
              if (is.numeric(val)) val <- as.character(val)

              # Print parameter name and value
              cat("  ", pname, ": ", val, "\n", sep = "")
            }

            # Add a separator for readability between models
            cat("\n")
          }
        }
      }
    } else {
      cat("No hyperparameters found.\n")
    }
  } else {
    cat("No hyperparameters found.\n")
  }

  if (nzchar(notes)) {
    cat("\nUser Notes:\n", notes, "\n", sep = "")
  }

  cat("=================================\n")

  if (!plot) return(invisible(object))

  performance_melt <- melt(performance_wide, id.vars = "Model", variable.name = "Metric", value.name = "Value")
  performance_melt <- performance_melt[!is.na(performance_melt$Value), ]
  performance_melt$Value <- as.numeric(performance_melt$Value)
  performance_melt$Metric <- as.character(performance_melt$Metric)
  performance_melt$Metric <- ifelse(
    performance_melt$Metric %in% names(display_names),
    display_names[performance_melt$Metric],
    performance_melt$Metric
  )

  if (task == "classification") {
    class_order <- c("Accuracy", "F1 Score", "Kappa", "Precision", "Sensitivity", "Specificity", "ROC AUC")
    present_class_metrics <- intersect(class_order, unique(performance_melt$Metric))
    if (length(present_class_metrics) > 0) {
      performance_melt$Metric <- factor(performance_melt$Metric, levels = present_class_metrics)
    }
  } else if (task == "regression") {
    reg_order <- c("RMSE", "R-squared", "MAE")
    present_reg_metrics <- intersect(reg_order, unique(performance_melt$Metric))
    if (length(present_reg_metrics) > 0) {
      performance_melt$Metric <- factor(performance_melt$Metric, levels = present_reg_metrics)
    }
  }

  p_bar <- ggplot(performance_melt, aes(x = Model, y = Value, fill = Model)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~ Metric, scales = "free_y") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    ) +
    labs(title = "Model Performance Comparison", x = "Model", y = "Metric Value")

  print(p_bar)

  # ROC curves for binary classification using yardstick
  if (task == "classification" && !is.null(predictions_list) && length(predictions_list) > 0) {
    model_names_pred <- names(predictions_list)
    if (length(model_names_pred) > 0) {
      any_model_name <- model_names_pred[1]
      df_example <- predictions_list[[any_model_name]]
      if (!is.null(df_example) && "truth" %in% names(df_example)) {
        unique_classes <- unique(df_example$truth)
        if (length(unique_classes) == 2) {
          # We'll create a combined data frame for ROC curves from all models
          dfs = list()
          for (model_name in model_names_pred) {
            df <- predictions_list[[model_name]]
            prob_cols <- grep("^\\.pred_", names(df), value = TRUE)
            if (length(prob_cols) == 2) {
              pred_col <- paste0(".pred_", positive_class)
              if (pred_col %in% prob_cols) {
                df$Model <- model_name
                dfs[[model_name]] <- df
              }
            }
          }

          if (length(dfs) > 0) {
            dfs_roc <- bind_rows(dfs)

            # Convert 'truth' and 'Model' to factors if they aren't already
            dfs_roc$truth <- factor(dfs_roc$truth)
            dfs_roc$Model <- as.factor(dfs_roc$Model)

            # Get the list of unique models
            models <- levels(dfs_roc$Model)

            # Initialize an empty list to store ROC objects
            roc_list <- list()
            pred_col <- paste0(".pred_", positive_class)


            # Compute ROC curves for each model
            for (model in models) {
              # Subset data for the current model
              data_model <- subset(dfs_roc, Model == model)

              # Compute ROC using .pred_1 as the predictor
              roc_obj <- roc(response = data_model$truth,
                             predictor = data_model[[pred_col]],
                             direction = "auto",
                             quiet = TRUE)  # Suppress messages

              # Store the ROC object in the list
              roc_list[[model]] <- roc_obj
            }

            # Plotting using base pROC
            # Initialize the plot with the first model
            # plot(roc_list[[1]], col = 1, lwd = 2, main = "ROC Curves for Models")

            # Add ROC curves for the remaining models
            # if (length(models) > 1) {
            #   for (i in 2:length(models)) {
            #     plot(roc_list[[i]], col = i, lwd = 2, add = TRUE)
            #   }
            # }

            # Add a legend
            # legend("bottomright",
            #        legend = paste(models, " (AUC =",
            #                       sapply(roc_list, function(x) sprintf("%.3f", auc(x))),
            #                       ")"),
            #        col = 1:length(models),
            #        lwd = 2,
            #        cex = 0.8)

            # Alternatively, for a more polished plot, use ggplot2 with pROC's ggroc function
            # Combine all ROC curves into a single data frame for ggplot2
            roc_data <- data.frame()

            for (model in models) {
              roc_obj <- roc_list[[model]]
              roc_df <- data.frame(
                FalsePositiveRate = rev(roc_obj$specificities),
                TruePositiveRate = rev(roc_obj$sensitivities),
                Model = model
              )
              roc_data <- rbind(roc_data, roc_df)
            }

            # Create the ggplot
            roc_curve_plot <- ggplot(data = roc_data, aes(x = 1 - FalsePositiveRate, y = TruePositiveRate, color = Model)) +
              geom_line(linewidth = 1) +
              geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey") +
              theme_minimal() +
              labs(title = "ROC Curves for Models",
                   x = "1 - Specificity",
                   y = "Sensitivity") +
              theme(plot.title = element_text(hjust = 0.5)) +
              # Optionally add AUC to the legend
              scale_color_manual(values = 1:length(models),
                                 labels = paste0(models, " (AUC = ",
                                                 sapply(roc_list, function(x) sprintf("%.3f", auc(x))), ")")) +
              theme(legend.title = element_blank())

            print(roc_curve_plot)


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

  # Additional Diagnostics
  if (plot && !is.null(predictions_list) && all(desired_model_name %in% names(predictions_list))) {

    if(length(desired_models) == 1){
      df_best <- predictions_list[[desired_model_name]]
      names_df_best <- names(df_best)
    }else{
      df_best <- predictions_list[desired_model_name]
      names_df_best <- unique(unlist(lapply(df_best, names)))
      }

    if (task == "classification") {
      if (!is.null(df_best) && "truth" %in% names_df_best && "estimate" %in% names_df_best) {
        if(length(desired_models) == 1){
          cm <- conf_mat(df_best, truth = truth, estimate = estimate)
          cat("\nConfusion Matrix for",desired_model_name, "\n")
          print(cm)
        }else{

          # Iterate through the models in df_best
          for (model_name in names(df_best)) {
            cat("Confusion Matrix for", model_name, "\n")

            # Extract predictions for the current model
            model_predictions <- df_best[[model_name]]

            # Check if `truth` and `estimate` columns are available
            if (!("truth" %in% colnames(model_predictions)) || !("estimate" %in% colnames(model_predictions))) {
              cat("Error: Missing `truth` or `estimate` columns in predictions for model:", model_name, "\n")
              next
            }

            # Compute confusion matrix
            cm <- conf_mat(model_predictions, truth = truth, estimate = estimate)
            print(cm)
          }
        }

        # Calibration Plot
        if (requireNamespace("probably", quietly = TRUE)) {
          if(length(desired_models) == 1){
          prob_cols <- grep("^\\.pred_", names(df_best), value = TRUE)

          }else{

            prob_cols <- grep("^\\.pred_", names_df_best, value = TRUE)

          }
          if (length(prob_cols) > 1) {
            pred_col <- paste0(".pred_", positive_class)
            if (pred_col %in% prob_cols) {

              if(length(desired_models) == 1){
                p_cal <- cal_plot_breaks(
                  df_best,
                  truth = truth,
                  estimate = !!sym(pred_col),
                  event_level = object$event_class
                ) +
                  labs(title = paste("Calibration Plot", desired_model_name))
              print(p_cal)
              }else{

                # Loop through each model in df_best
                for (model_name in names(df_best)) {

                  # Extract the predictions for the current model
                  model_predictions <- df_best[[model_name]]

                  # Ensure the pred_col exists in the data frame
                  if (!pred_col %in% colnames(model_predictions)) {
                    cat("Error: Column", pred_col, "not found in model predictions for", model_name, "\n")
                    next
                  }

                  # Create the calibration plot
                  p_cal <- cal_plot_breaks(
                    model_predictions,
                    truth = truth,
                    estimate = !!sym(pred_col),
                    event_level = object$event_class
                  ) +
                    labs(title = paste("Calibration Plot for", model_name))

                  print(p_cal)
                }
              }
            }
          }
        } else {
          cat("\nInstall the 'probably' package for a calibration plot.\n")
        }
      }
    } else if (task == "regression") {
      if (!is.null(df_best) && "truth" %in% names(df_best) && "estimate" %in% names(df_best)) {
        df_best <- df_best %>% mutate(residual = truth - estimate)
        cat("\nResidual Diagnostics for Best Model:\n")

        p_truth_pred <- ggplot(df_best, aes(x = estimate, y = truth)) +
          geom_point(alpha = 0.6) +
          geom_abline(linetype = "dashed", color = "red") +
          labs(title = "Truth vs Predicted", x = "Predicted", y = "Truth") +
          theme_bw()

        print(p_truth_pred)

        p_resid_hist <- ggplot(df_best, aes(x = residual)) +
          geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.7) +
          labs(title = "Residual Distribution", x = "Residual", y = "Count") +
          theme_bw()

        print(p_resid_hist)
      }
    }
  }

  invisible(object)
}
