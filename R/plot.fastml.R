utils::globalVariables(c("Model", "Value", "Measure"))

#' Plot Methods for \code{fastml} Objects
#'
#' \code{plot.fastml} produces visual diagnostics for a trained \code{fastml} object.
#'
#' @param x A \code{fastml} object (output of \code{\link{fastml}()}).
#' @param algorithm Character vector specifying which algorithm(s) to include when
#'   generating certain plots (e.g., ROC curves). Defaults to \code{"best"}.
#' @param type Character vector indicating which plot(s) to produce. Options are:
#'   \describe{
#'     \item{\code{"bar"}}{Bar plot of performance metrics across all models/engines.}
#'     \item{\code{"roc"}}{ROC curve(s) for binary classification models.}
#'     \item{\code{"confusion"}}{Confusion matrix for the best model(s).}
#'     \item{\code{"calibration"}}{Calibration plot for the best model(s).}
#'     \item{\code{"residual"}}{Residual diagnostics for the best model.}
#'     \item{\code{"all"}}{Produce all available plots.}
#'   }
#' @param ... Additional arguments (currently unused).
#'
#' @details
#' When \code{type = "all"}, \code{plot.fastml} will produce a bar plot of metrics,
#' ROC curves (classification), confusion matrix, calibration plot, and residual
#' diagnostics (regression).  If you specify a subset of types, only those will be drawn.
#'
#' @method plot fastml
#' @importFrom graphics plot
#' @export
plot.fastml <- function(x,
                        algorithm = "best",
                        type = c("all", "bar", "roc", "confusion", "calibration", "residual"),
                        ...) {
  x <- object

  if (!inherits(object, "fastml")) {
    stop("The input must be a 'fastml' object.")
  }

  # Validate 'type' argument
  type <- match.arg(type, several.ok = TRUE)
  if ("all" %in% type) {
    type <- c("bar", "roc", "confusion", "calibration", "residual")
  }

  performance      <- object$performance
  predictions_list <- object$predictions
  task             <- object$task
  best_model_name  <- object$best_model_name
  optimized_metric <- object$metric
  positive_class   <- object$positive_class
  engine_names     <- object$engine_names

  # Rebuild performance_wide (same logic as in summary.fastml)
  metrics_list <- lapply(names(performance), function(model_name) {
    engine_dfs <- lapply(names(performance[[model_name]]), function(engine_name) {
      df <- as.data.frame(performance[[model_name]][[engine_name]])
      df$Engine <- engine_name
      df
    })
    combined_engines      <- do.call(rbind, engine_dfs)
    combined_engines$Model <- model_name
    combined_engines
  })
  performance_df <- do.call(rbind, metrics_list)

  all_metric_names <- unique(performance_df$.metric)
  if (optimized_metric %in% all_metric_names) {
    main_metric <- optimized_metric
  } else {
    main_metric <- all_metric_names[1]
    warning("Optimized metric not available; using first metric.")
  }

  if (task == "classification") {
    desired_metrics <- c("accuracy", "f_meas", "kap", "precision", "sens", "spec", "roc_auc")
  } else {
    desired_metrics <- c("rmse", "rsq", "mae")
  }
  desired_metrics <- intersect(desired_metrics, all_metric_names)
  if (length(desired_metrics) == 0) {
    desired_metrics <- main_metric
  }

  performance_sub <- performance_df[
    performance_df$.metric %in% desired_metrics,
  ] %>%
    dplyr::select(-dplyr::any_of(".estimator"))

  if (length(engine_names) == 1 && "LiblineaR" %in% engine_names) {
    performance_wide <- tidyr::pivot_wider(
      performance_sub,
      names_from  = .metric,
      values_from = .estimate
    ) %>%
      dplyr::select(Model, Engine, accuracy, kap, sens, spec, precision, f_meas)
  } else {
    if (task == "classification") {
      performance_wide <- tidyr::pivot_wider(
        performance_sub,
        names_from  = .metric,
        values_from = .estimate
      ) %>%
        dplyr::select(Model, Engine, accuracy, kap, sens, spec, precision, f_meas, roc_auc)
    } else {
      performance_wide <- tidyr::pivot_wider(
        performance_sub,
        names_from  = .metric,
        values_from = .estimate
      ) %>%
        dplyr::select(Model, Engine, rmse, rsq, mae)
    }
  }

  if (task == "regression" && main_metric != "rsq") {
    performance_wide <- performance_wide[order(performance_wide[[main_metric]], na.last = TRUE), ]
  } else {
    performance_wide <- performance_wide[order(-performance_wide[[main_metric]], na.last = TRUE), ]
  }

  display_names <- c(
    accuracy  = "Accuracy",
    f_meas    = "F1 Score",
    kap       = "Kappa",
    precision = "Precision",
    roc_auc   = "ROC AUC",
    sens      = "Sensitivity",
    spec      = "Specificity",
    rsq       = "R-squared",
    mae       = "MAE",
    rmse      = "RMSE"
  )

  # ============================
  # 1. Bar plot of metrics
  # ============================
  if ("bar" %in% type) {
    performance_melt <- performance_wide %>%
      tidyr::pivot_longer(
        cols      = -c(Model, Engine),
        names_to  = "Metric",
        values_to = "Value"
      )
    performance_melt$Value <- as.numeric(performance_melt$Value)
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

    p_bar <- ggplot2::ggplot(
      performance_melt,
      ggplot2::aes(x = Model, y = Value, fill = Engine)
    ) +
      ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge()) +
      ggplot2::facet_wrap(~ Metric, scales = "free_y") +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
      ggplot2::labs(
        title = "Model Performance Comparison",
        x     = "Model",
        y     = "Metric Value"
      )

    if (task == "classification") {
      p_bar <- p_bar + ggplot2::ylim(0, 1)
    }

    print(p_bar)
  }

  # ============================
  # 2. ROC curves (classification)
  # ============================
  if ("roc" %in% type) {
    if (task == "classification" && !is.null(predictions_list) && length(predictions_list) > 0) {

      # Create a list to collect data frames from all models/engines
      dfs <- list()

      # Loop over each algorithm (e.g., "rand_forest", "logistic_reg")

      if(all(algorithm == "best")){
        pred_list = predictions_list
      }else{

        pred_list = predictions_list[algorithm]

      }

      for (algo in names(pred_list)) {
        # Loop over each engine in the current algorithm group
        for (eng in names(predictions_list[[algo]])) {
          if(eng == "LiblineaR"){
            warning("Engine 'LiblineaR' does not provide probability predictions; no ROC curve (roc_auc) will be computed.")

          }else{
            df <- predictions_list[[algo]][[eng]]
            if (!is.null(df) && "truth" %in% names(df)) {
              # Get probability columns matching ".pred_"

              if(eng == "h2o"){
                prob_cols <- grep("^\\.pred_p", names(df), value = TRUE)
              } else {
                prob_cols <- grep("^\\.pred_", names(df), value = TRUE)
              }

              if (length(prob_cols) >= 2) {  # binary classification should have two probability columns
                # Determine the predictor column for the positive class
                if(eng == "h2o"){
                  pred_col <- paste0(".pred_p", positive_class)

                }else{
                  pred_col <- paste0(".pred_", positive_class)
                }

                if (pred_col %in% prob_cols) {
                  # Add columns for algorithm and engine
                  df$Model  <- algo
                  df$Engine <- eng
                  # Use a compound key to store in the list
                  key <- paste(algo, eng, sep = "_")
                  dfs[[key]] <- df
                }
              }
            }
          }
        }
      }


      if (length(dfs) > 0) {
        # Combine all prediction data frames

        dfs_standardized <- lapply(dfs, function(df) {
          # Check if any column starts with ".pred_p"
          if (any(grepl("^\\.pred_p", names(df)))) {
            df <- df %>%
              rename_with(~ sub("^\\.pred_p", ".pred_", .x), starts_with(".pred_p"))
          }
          df
        })

        dfs_roc <-  bind_rows(dfs_standardized)

        # Ensure 'truth', 'Model', and 'Engine' are factors
        dfs_roc$truth  <- factor(dfs_roc$truth)
        dfs_roc$Model  <- as.factor(dfs_roc$Model)
        dfs_roc$Engine <- as.factor(dfs_roc$Engine)

        # We'll compute ROC curves for each unique combination of Model and Engine.
        roc_list <- list()


        pred_col <- paste0(".pred_", positive_class)



        # Get the unique combinations by splitting the compound key back into Model and Engine
        groups <- unique(dfs_roc[, c("Model", "Engine")])
        for (i in seq_len(nrow(groups))) {
          mod <- as.character(groups$Model[i])
          eng <- as.character(groups$Engine[i])
          data_model <- subset(dfs_roc, Model == mod & Engine == eng)

          # Compute the ROC curve (using pROC::roc)
          if(length(levels(data_model$truth)) > 2){
            roc_obj <- multiclass.roc(response = data_model$truth,
                                      predictor = data_model[[pred_col]],
                                      direction = "auto",
                                      quiet = TRUE)
          }else {
            roc_obj <- roc(response = data_model$truth,
                           predictor = data_model[[pred_col]],
                           direction = "auto",
                           quiet = TRUE)
          }
          # Use a combined label for the ROC list
          key <- paste(mod, eng, sep = " - ")
          roc_list[[key]] <- roc_obj
        }

        # Compute AUC values for each model/engine combination
        auc_values <- sapply(roc_list, function(x) auc(x))

        # Sort keys by AUC in descending order
        sorted_keys <- auc_values[order(names(auc_values))] #names(sort(auc_values, decreasing = TRUE))

        # Create a data frame for ROC curves by combining the ROC objects

        if(length(levels(dfs_roc$truth)) != 2){
          cat("\nROC curves are only generated for binary classification tasks.\n\n")
        }else{

          roc_data <- data.frame()
          for (key in names(sorted_keys)) {
            roc_obj <- roc_list[[key]]
            roc_df <- data.frame(
              FalsePositiveRate = rev(roc_obj$specificities),
              TruePositiveRate  = rev(roc_obj$sensitivities),
              ModelEngine       = key
            )
            roc_data <- rbind(roc_data, roc_df)
          }


          # Create the ROC curve plot, using the compound ModelEngine label for color
          roc_curve_plot <- ggplot(data = roc_data,
                                   aes(x = 1 - FalsePositiveRate,
                                       y = TruePositiveRate,
                                       color = ModelEngine)) +
            geom_line(linewidth = 1) +
            geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey") +
            theme_minimal() +
            labs(title = "ROC Curves for Models",
                 x = "1 - Specificity",
                 y = "Sensitivity") +
            theme(plot.title = element_text(hjust = 0.5)) +
            # Use sorted keys to label the legend with AUC values
            scale_color_manual(values = 1:length(sorted_keys),
                               labels = paste0(names(sorted_keys), " (AUC = ",
                                               sprintf("%.3f", auc_values[names(sorted_keys)]), ")")) +
            theme(legend.title = element_blank())

          print(roc_curve_plot)
        }

      } else {
        cat("\nNo suitable probability predictions for ROC curves.\n\n")
      }

    } else {
      if (is.null(predictions_list) || length(predictions_list) == 0) {
        cat("\nNo predictions available to generate ROC curves.\n\n")
      }
    }

  }

  # ============================
  # 4. Calibration Plot (classification)
  # ============================
  if ("calibration" %in% type) {
    if (task == "classification") {
      if (requireNamespace("probably", quietly = TRUE)) {
        df_best <- list()
        for (model in names(best_model_name)) {
          engine <- best_model_name[model]
          name_combined <- paste(model, "(", engine, ")", sep = "")

          if (!is.null(predictions_list[[model]]) && !is.null(predictions_list[[model]][[engine]])) {
            df_best[[name_combined]] <- predictions_list[[model]][[engine]]
          } else {
            cat("\nNo predictions found for", model, "with engine", engine, "\n")
          }
        }

        names_df_best <- unique(unlist(lapply(df_best, names)))

        prob_cols <- grep("^\\.pred", names_df_best, value = TRUE)
        if (length(prob_cols) > 1) {
          for (model_name in names(df_best)) {
            model_predictions <- df_best[[model_name]]
            if (grepl("h2o", model_name)) {
              names(model_predictions) <- sub("^\\.pred_p", ".pred_", names(model_predictions))
            }
            pred_col <- paste0(".pred_", positive_class)
            if (!pred_col %in% colnames(model_predictions)) {
              cat("Error: Column", pred_col, "not found in model predictions for", model_name, "\n")
              next
            }
            p_cal <- probably::cal_plot_breaks(
              model_predictions,
              truth    = truth,
              estimate = !!rlang::sym(pred_col),
              event_level = object$event_class
            ) +
              ggplot2::labs(title = paste("Calibration Plot for", model_name))
            print(p_cal)
          }
        } else {
          cat("\nNot enough probability columns for calibration plots.\n\n")
        }
      } else {
        cat("\nInstall the 'probably' package to generate calibration plots.\n")
      }
    } else {
      cat("\nCalibration plots are only available for classification tasks.\n\n")
    }
  }

  # ============================
  # 5. Residual Diagnostics (regression)
  # ============================
  if ("residual" %in% type) {
    if (task == "regression") {
      df_best <- list()
      # Determine the single best model engine
      for (model in names(best_model_name)) {
        engine <- best_model_name[model]
        name_combined <- paste(model, "(", engine, ")", sep = "")

        if (!is.null(predictions_list[[model]]) && !is.null(predictions_list[[model]][[engine]])) {
          df_best[[name_combined]] <- predictions_list[[model]][[engine]]
        } else {
          cat("\nNo predictions found for", model, "with engine", engine, "\n")
        }
      }


      names_df_best <- unique(unlist(lapply(df_best, names)))
      if (!is.null(df_best) && "truth" %in% names_df_best && "estimate" %in% names_df_best) {
        df_best_all <- dplyr::bind_rows(df_best, .id = "ModelEngine") %>%
          dplyr::mutate(residual = truth - estimate)

        cat("\nResidual Diagnostics for Best Model:\n")

        p_truth_pred <- ggplot2::ggplot(df_best_all, ggplot2::aes(x = estimate, y = truth)) +
          ggplot2::geom_point(alpha = 0.6) +
          ggplot2::geom_abline(linetype = "dashed", color = "red") +
          ggplot2::labs(title = "Truth vs Predicted", x = "Predicted", y = "Truth") +
          ggplot2::theme_bw()
        print(p_truth_pred)

        p_resid_hist <- ggplot2::ggplot(df_best_all, ggplot2::aes(x = residual)) +
          ggplot2::geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.7) +
          ggplot2::labs(title = "Residual Distribution", x = "Residual", y = "Count") +
          ggplot2::theme_bw()
        print(p_resid_hist)
      } else {
        cat("\nNo valid predictions to compute residual diagnostics.\n\n")
      }
    } else {
      cat("\nResidual diagnostics are only available for regression tasks.\n\n")
    }
  }

  invisible(object)
}
