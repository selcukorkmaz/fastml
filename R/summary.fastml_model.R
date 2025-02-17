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
#' @param notes User-defined commentary.
#' @param ... Additional arguments.
#' @return Prints summary and plots if requested.
#'
#' @importFrom dplyr filter select mutate bind_rows group_by summarise n
#' @importFrom magrittr %>%
#' @importFrom reshape2 melt dcast
#' @importFrom tune extract_fit_parsnip
#' @importFrom ggplot2 ggplot aes geom_bar geom_path facet_wrap theme_bw theme element_text labs geom_point geom_line geom_histogram geom_abline coord_equal scale_color_manual theme_minimal element_blank ylim position_dodge
#' @importFrom RColorBrewer brewer.pal
#' @importFrom yardstick conf_mat
#' @importFrom pROC roc auc multiclass.roc
#' @importFrom probably cal_plot_breaks
#' @importFrom rlang get_expr get_env sym
#' @importFrom viridisLite viridis
#' @importFrom tidyr pivot_wider
#'
#' @export
summary.fastml_model <- function(object,
                                 algorithm = "best",
                                 sort_metric = NULL,
                                 plot = TRUE,
                                 notes = "",
                                 ...) {
  if (!inherits(object, "fastml_model")) {
    stop("The input must be a 'fastml_model' object.")
  }

  performance <- object$performance
  predictions_list <- object$predictions
  task <- object$task
  best_model <- object$best_model
  best_model_name <- object$best_model_name
  optimized_metric <- object$metric
  model_count <- length(object$models)
  positive_class <- object$positive_class
  engine_names <- object$engine_names

  # Loop over the top-level names (e.g. "rand_forest", "logistic_reg")
  metrics_list <- lapply(names(performance), function(model_name) {

    # For each model, loop over its engines
    engine_dfs <- lapply(names(performance[[model_name]]), function(engine_name) {

      # Get the tibble for this engine and convert it to a data.frame
      df <- as.data.frame(performance[[model_name]][[engine_name]])

      # Add an "Engine" column
      df$Engine <- engine_name
      df
    })

    # Combine the tibbles from different engines (row-wise)
    combined_engines <- do.call(rbind, engine_dfs)

    # Add a "Model" column
    combined_engines$Model <- model_name
    combined_engines
  })

  # Combine all model groups into one data.frame
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

  performance_sub <- performance_df[performance_df$.metric %in% desired_metrics, ]%>%
    dplyr::select(-dplyr::any_of(".estimator"))


  performance_wide <- pivot_wider(
    performance_sub,
    names_from = .metric,
    values_from = .estimate
  ) %>%
    dplyr::select(Model, Engine, accuracy, kap, sens, spec, precision, f_meas, roc_auc)
  # performance_wide$Engine <- engine_names[match(performance_wide$Model, names(engine_names))]
  # performance_wide <- performance_wide[, c("Model", "Engine", setdiff(colnames(performance_wide), c("Model", "Engine")))]

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

  # Filter rows where the Model is in best_model_name and its Engine equals best_model_name[Model]
  best_val_df <- performance_wide %>%
    filter(Model %in% names(best_model_name)) %>%
    filter(mapply(function(m, e) e == best_model_name[[m]], Model, Engine))

  # If you just want the unique metric values:
  best_val <- best_val_df %>%
    pull(!!sym(main_metric)) %>%
    unique()
  cat("Best Model(s):", paste0(names(best_model_name), " (", best_model_name, ")"), sprintf("(%s: %.7f)", main_metric, best_val), "\n\n")

  cat("Performance Metrics (Sorted by", main_metric,"):\n\n")

  metrics_to_print <- c("Model", "Engine", desired_metrics)

  best_model_idx <- which(performance_wide$Engine == best_model_name[performance_wide$Model])


  if(length(algorithm) == 1 && algorithm == "best"){
    selected_model_idx <- best_model_idx
    desired_models <- object$best_model
  }else{

    clean_names <- sub(" \\(.*\\)", "", names(object$models))

    if(all(algorithm %in% clean_names)){

      selected_model_idx <- which(performance_wide$Model %in% algorithm)

      clean_model_names <- sub(" \\(.*\\)", "", names(object$models))
      matching_index <- match(algorithm, clean_model_names)

      if (!is.na(matching_index)) {
        desired_models <- object$models[matching_index]
      } else {
        desired_models <- NULL  # Handle case where algorithm is not found
      }

    }else{

      stop("Not all specified algorithms entered correctly.")
    }
  }

  desired_model_name <- names(desired_models)

  for (m in desired_metrics) {
    performance_wide[[m]] <- format(performance_wide[[m]], digits = 7, nsmall = 7)
  }

  if(algorithm != "best"){

    performance_wide = performance_wide %>% filter(Model %in% algorithm)
  }

  header <- c("Model", "Engine", sapply(desired_metrics, function(m) {
    if (m %in% names(display_names)) display_names[[m]] else m
  }))

  data_str <- performance_wide
  data_str$Model <- as.character(data_str$Model)

  if(algorithm == "best"){
    data_str$Model[best_model_idx] <- paste0(data_str$Model[best_model_idx], "*")
  }

  col_widths <- sapply(seq_along(header), function(i) {
    col_name <- header[i]
    col_data <- data_str[[c("Model", "Engine", desired_metrics)[i]]]
    max(nchar(col_name), max(nchar(col_data)))
  })

  header_line <- paste(mapply(function(h, w) format(h, width = w, justify = "left"), header, col_widths), collapse = "  ")
  line_sep <- paste(rep("-", sum(col_widths) + 2*(length(col_widths)-1)), collapse = "")

  cat(line_sep, "\n")
  cat(header_line, "\n")
  cat(line_sep, "\n")

  for (i in seq_len(nrow(data_str))) {
    row_line <- paste(mapply(function(v, w) format(v, width = w, justify = "left"),
                             data_str[i, c("Model", "Engine", desired_metrics), drop=FALSE], col_widths),
                      collapse = "  ")
    cat(row_line, "\n")
  }

  cat(line_sep, "\n")

  if(algorithm == "best"){
   cat("(*Best model)\n\n")
  }


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
    } else {
      params <- lapply(parsnip_fit, function(model) model$spec$args)
    }

    if (length(params) > 0) {
      cleaned_params_list <- list()  # Initialize here to prevent missing object error

      if(length(desired_models) == 1){
        cleaned_params <- list()
        for (pname in names(params)) {
          val <- params[[pname]]
          if (inherits(val, "quosure")) {
            val <- tryCatch(eval(get_expr(val), envir = get_env(val)), error = function(e) val)
          }
          cleaned_params[[pname]] <- val
        }
        cleaned_params_list[[desired_model_name]] <- cleaned_params  # Store in the list to ensure availability
      } else {
        # Process each model's parameters
        for (model_name in names(params)) {
          model_params <- params[[model_name]]
          cleaned_params <- list()

          for (pname in names(model_params)) {
            val <- model_params[[pname]]

            if (inherits(val, "quosure")) {
              val <- tryCatch(
                eval(rlang::get_expr(val), envir = rlang::get_env(val)),
                error = function(e) val # Retain quosure if evaluation fails
              )
            }

            cleaned_params[[pname]] <- val
          }
          cleaned_params_list[[model_name]] <- cleaned_params
        }
      }

      if (length(cleaned_params_list) == 0) {
        cat("No hyperparameters found.\n")
      } else {
        for (model_name in names(cleaned_params_list)) {
          cat("Model:", model_name, "\n")

          cleaned_params <- cleaned_params_list[[model_name]]

          for (pname in names(cleaned_params)) {
            val <- cleaned_params[[pname]]

            if (is.numeric(val)) val <- as.character(val)

            cat("  ", pname, ": ", val, "\n", sep = "")
          }

          cat("\n")
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

  performance_melt <- performance_wide %>%
    pivot_longer(
      cols = -c(Model, Engine),  # keep Model and Engine as identifiers
      names_to = "Metric",
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

  p_bar <- ggplot(performance_melt, aes(x = Model, y = Value, fill = Engine)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    facet_wrap(~ Metric, scales = "free_y") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    labs(title = "Model Performance Comparison",
         x = "Model",
         y = "Metric Value")

  # Adjust y-axis limits if it's a classification task
  if (task == "classification") {
    p_bar <- p_bar + ylim(0, 1)
  }

  print(p_bar)


  # ROC curves for binary classification using yardstick
  if (task == "classification" && !is.null(predictions_list) && length(predictions_list) > 0) {

    # Create a list to collect data frames from all models/engines
    dfs <- list()

    # Loop over each algorithm (e.g., "rand_forest", "logistic_reg")

    if(algorithm == "best"){
      pred_list = predictions_list
    }else{

      pred_list = predictions_list[algorithm]

    }

    for (algo in names(pred_list)) {
      # Loop over each engine in the current algorithm group
      for (eng in names(predictions_list[[algo]])) {
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
      sorted_keys <- names(sort(auc_values, decreasing = TRUE))

      # Create a data frame for ROC curves by combining the ROC objects

      if(length(levels(dfs_roc$truth)) != 2){
        cat("\nROC curves are only generated for binary classification tasks.\n\n")
      }else{

      roc_data <- data.frame()
      for (key in sorted_keys) {
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
                           labels = paste0(sorted_keys, " (AUC = ",
                                           sprintf("%.3f", auc_values[sorted_keys]), ")")) +
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


  # Additional Diagnostics
  if (plot && !is.null(predictions_list) &&  length(predictions_list) > 0) {

      df_best <- list()
      for (x in desired_model_name) {
        parts <- strsplit(x, " \\(")[[1]]
        model <- parts[1]
        engine <- gsub("\\)", "", parts[2])
        if (!is.null(predictions_list[[model]]) && !is.null(predictions_list[[model]][[engine]])) {
          # Use the original compound name as the list element name.
          df_best[[ x ]] <- predictions_list[[model]][[engine]]
        } else {
          cat("No predictions found for", model, "with engine", engine, "\n")
        }
      }
      names_df_best <- unique(unlist(lapply(df_best, names)))



    if (task == "classification") {
      if (!is.null(df_best) && "truth" %in% names_df_best && "estimate" %in% names_df_best) {

          # Iterate through the models in df_best
          for (model_name in names(df_best)) {
            cat("Confusion Matrix for", model_name, "\n\n")

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


        # Calibration Plot
        if (requireNamespace("probably", quietly = TRUE)) {


            prob_cols <- grep("^\\.pred", names_df_best, value = TRUE)


          if (length(prob_cols) > 1) {

                 # Loop through each model in df_best
                for (model_name in names(df_best)) {

                  # Extract the predictions for the current model
                  model_predictions <- df_best[[model_name]]

                  if(grepl("h2o", model_name)){

                    names(model_predictions) <- sub("^\\.pred_p", ".pred_", names(model_predictions))

                  }


                    pred_col <- paste0(".pred_", positive_class)




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

