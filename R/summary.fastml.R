utils::globalVariables(c("truth", "residual", "sensitivity", "specificity", "FalsePositiveRate", "TruePositiveRate", "Engine", "ModelEngine"))

#' Summary Function for fastml (Using yardstick for ROC Curves)
#'
#' Provides a concise, user-friendly summary of model performances.
#' For classification:
#' - Shows Accuracy, F1 Score, Kappa, Precision, ROC AUC, Sensitivity, Specificity.
#' - Displays a confusion matrix.
#'
#' For regression:
#' - Shows RMSE, R-squared, and MAE.
#'
#'
#' @param object An object of class \code{fastml}.
#' @param algorithm A vector of algorithm names to display summary. Default is \code{"best"}.
#' @param sort_metric The metric to sort by. Default uses optimized metric.
#' @param ... Additional arguments.
#' @return Prints summary of fastml models.
#'
#' @importFrom dplyr filter select mutate bind_rows group_by summarise n starts_with
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
summary.fastml <- function(object,
                                 algorithm = "best",
                                 type = c("all", "metrics", "hyperparameters", "confusion_matrix"),
                                 sort_metric = NULL,
                                 ...) {

  if (!inherits(object, "fastml")) {
    stop("The input must be a 'fastml' object.")
  }

  # Validate 'type' argument
  type <- match.arg(type, several.ok = TRUE)
  if ("all" %in% type) {
    type <- c("metrics", "hyperparameters", "confusion_matrix")
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



    if(length(engine_names) == 1 && "LiblineaR" %in% engine_names){

        performance_wide <- pivot_wider(
          performance_sub,
          names_from = .metric,
          values_from = .estimate
        ) %>%
          dplyr::select(Model, Engine, accuracy, kap, sens, spec, precision, f_meas)

    }else{

      if(task == "classification"){

      performance_wide <- pivot_wider(
        performance_sub,
        names_from = .metric,
        values_from = .estimate
      ) %>%
        dplyr::select(Model, Engine, accuracy, kap, sens, spec, precision, f_meas, roc_auc)

      }else{

        performance_wide <- pivot_wider(
          performance_sub,
          names_from = .metric,
          values_from = .estimate
        ) %>%
          dplyr::select(Model, Engine, rmse, rsq, mae)
      }

    }



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

  if ("metrics" %in% type) {

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
  cat("Best Model(s):",
      paste0(names(best_model_name), " (", best_model_name, ")"),
      sprintf("(%s: %.7f)", main_metric, as.numeric(best_val)),
      "\n\n")

  cat("Performance Metrics (Sorted by", main_metric,"):\n\n")

  metrics_to_print <- c("Model", "Engine", desired_metrics)

  best_model_idx <- get_best_model_idx(performance_wide, optimized_metric)


  if(length(algorithm) == 1 && algorithm == "best"){
    selected_model_idx <- best_model_idx
    desired_models <- object$best_model
  }else{

    clean_names <- sub(" \\(.*\\)", "", names(object$models))

    if(all(algorithm %in% clean_names)){

      selected_model_idx <- which(performance_wide$Model %in% algorithm)

      clean_model_names <- sub(" \\(.*\\)", "", names(object$models))
      matching_index <- match(algorithm, clean_model_names)

      if (all(!is.na(matching_index))) {
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

  if(all(algorithm != "best")){

    performance_wide = performance_wide %>% filter(Model %in% algorithm)
  }

  header <- c("Model", "Engine", sapply(desired_metrics, function(m) {
    if (m %in% names(display_names)) display_names[[m]] else m
  }))

  data_str <- performance_wide
  data_str$Model <- as.character(data_str$Model)

  if(all(algorithm == "best")){
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

}

  if ("hypermarameters" %in% type) {

    if(length(algorithm) == 1 && all(algorithm == "best")){
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
            # val <- cleaned_params[[pname]]
            #
            # if (is.numeric(val)) val <- as.character(val)
            #
            # cat("  ", pname, ": ", val, "\n", sep = "")

            val <- cleaned_params[[pname]]
            if (rlang::is_quosure(val)) {
              val <- rlang::quo_text(val)
            } else if (is.numeric(val)) {
              val <- as.character(val)
            }
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
}

  if ("confusion_matrix" %in% type) {

    if (task == "classification") {
      df_best <- list()

      for (model in names(best_model_name)) {
        engine <- best_model_name[model]
        name_combined <- paste(model, engine, sep = "_")

        if (!is.null(predictions_list[[model]]) && !is.null(predictions_list[[model]][[engine]])) {
          df_best[[name_combined]] <- predictions_list[[model]][[engine]]
        } else {
          cat("\nNo predictions found for", model, "with engine", engine, "\n")
        }
      }

      names_df_best <- unique(unlist(lapply(df_best, names)))

      if (!is.null(df_best) && "truth" %in% names_df_best && "estimate" %in% names_df_best) {
        cat("\n===========================\n")
        cat("Confusion Matrices by Model\n")
        cat("===========================\n\n")

        for (model_name in names(df_best)) {
          cat("Model:", model_name, "\n")
          cat("---------------------------\n")

          model_predictions <- df_best[[model_name]]
          if (!("truth" %in% colnames(model_predictions)) || !("estimate" %in% colnames(model_predictions))) {
            cat("Missing `truth` or `estimate` columns in predictions for model:", model_name, "\n\n")
            next
          }

          cm <- yardstick::conf_mat(model_predictions, truth = truth, estimate = estimate)
          print(cm)
          cat("\n")  # Extra space between models
        }
      } else {
        cat("\nNo valid predictions to compute confusion matrix.\n\n")
      }
    } else {
      cat("\nConfusion matrix is only available for classification tasks.\n\n")
    }

  }


  invisible(object)
}

