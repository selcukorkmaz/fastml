utils::globalVariables(c("truth", "residual", "sensitivity", "specificity", "FalsePositiveRate", "TruePositiveRate", "Engine", "ModelEngine"))

#' Summary Function for fastml (Using yardstick for ROC Curves)
#'
#' @description
#' Summarizes the results of machine learning models trained using the `fastml` package.
#' Depending on the task type (classification or regression), it provides customized output such as
#' performance metrics, best hyperparameter settings, and confusion matrices.
#' It is designed to be informative and readable, helping users quickly interpret model results.

#' @details
#' For classification tasks, the summary includes metrics such as Accuracy, F1 Score, Kappa,
#' Precision, ROC AUC, Sensitivity, and Specificity. A confusion matrix is also provided for the best model(s).
#' For regression tasks, the summary reports RMSE, R-squared, and MAE.
#'
#' Users can control the type of output with the `type` argument:
#' `metrics` displays model performance metrics.
#' `params` shows the best hyperparameter settings.
#' `conf_mat` prints confusion matrices (only for classification).
#' `all` includes all of the above.
#'
#' If multiple algorithms are trained, the summary highlights the best model based on the optimized metric.
#'
#'
#' @param object An object of class \code{fastml}.
#' @param algorithm A vector of algorithm names to display summary. Default is \code{"best"}.
#' @param type Character vector indicating which outputs to produce.
#'   Options are \code{"all"} (all available outputs),
#'   \code{"metrics"} (performance metrics),
#'   \code{"params"} (best hyperparameters), and
#'   \code{"conf_mat"} (confusion matrix).
#'   Default is \code{"all"}.
#' @param sort_metric The metric to sort by. Default uses optimized metric.
#' @param ... Additional arguments.
#' @return Prints summary of fastml models.
#'
#' @importFrom dplyr filter select mutate bind_rows group_by summarise n starts_with distinct
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
                                 type = c("all", "metrics", "params", "conf_mat"),
                                 sort_metric = NULL,
                                 ...) {

  if (!inherits(object, "fastml")) {
    stop("The input must be a 'fastml' object.")
  }

  # Validate 'type' argument
  type <- match.arg(type, several.ok = TRUE)
  if ("all" %in% type) {
    type <- c("metrics", "params", "conf_mat")
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

    perf_entry <- performance[[model_name]]

    # Two possible structures:
    # 1) A list of engines, each containing a tibble with .metric/.estimate
    # 2) A single tibble (no engine nesting), typical when only one engine was used

    # Helper to resolve engine label for a model
    resolve_engine <- function(default_engine = NA_character_) {
      # Prefer provided engine_names if available
      if (!is.null(engine_names) && !is.null(engine_names[[model_name]]) && !is.na(engine_names[[model_name]])) {
        return(engine_names[[model_name]])
      }
      # Fallback to best_model_name mapping if present
      if (!is.null(best_model_name) && model_name %in% names(best_model_name)) {
        return(as.character(best_model_name[[model_name]]))
      }
      default_engine
    }

    if (is.list(perf_entry) && !is.data.frame(perf_entry)) {
      # Multi-engine structure
      engine_dfs <- lapply(names(perf_entry), function(engine_name) {
        df <- as.data.frame(perf_entry[[engine_name]])
        df$Engine <- engine_name
        df
      })
      combined_engines <- do.call(rbind, engine_dfs)
    } else {
      # Single tibble structure (no engine-level nesting)
      combined_engines <- as.data.frame(perf_entry)
      combined_engines$Engine <- resolve_engine()
    }

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
  } else if (task == "regression") {
    desired_metrics <- c("rmse", "rsq", "mae")
  } else if (task == "survival") {
    brier_metrics <- sort(grep("^brier_t", all_metric_names, value = TRUE))
    desired_metrics <- c("c_index", "uno_c", "ibs", "rmst_diff", brier_metrics)
  } else {
    # Fallback for any other task types
    desired_metrics <- unique(performance_df$.metric)
  }
  desired_metrics <- intersect(desired_metrics, all_metric_names)
  if (length(desired_metrics) == 0) desired_metrics <- main_metric

  performance_sub <- performance_df[performance_df$.metric %in% desired_metrics, ] %>%
    dplyr::select(-dplyr::any_of(".estimator"))

  has_ci_cols <- all(c(".lower", ".upper") %in% colnames(performance_sub))
  if (has_ci_cols) {
    performance_sub <- performance_sub %>%
      dplyr::mutate(
        metric_display = dplyr::case_when(
          is.na(.estimate) ~ NA_character_,
          !is.na(.lower) & !is.na(.upper) ~ sprintf("%.3f (%.3f, %.3f)", .estimate, .lower, .upper),
          TRUE ~ sprintf("%.3f", .estimate)
        )
      )
  } else {
    performance_sub <- performance_sub %>%
      dplyr::mutate(
        metric_display = dplyr::case_when(
          is.na(.estimate) ~ NA_character_,
          TRUE ~ sprintf("%.3f", .estimate)
        )
      )
  }



  keep_metrics <- desired_metrics
  if (length(engine_names) == 1 && "LiblineaR" %in% engine_names) {
    keep_metrics <- intersect(keep_metrics, c("accuracy", "kap", "sens", "spec", "precision", "f_meas"))
  }
  performance_numeric <- performance_sub %>%
    dplyr::select(Model, Engine, .metric, .estimate) %>%
    dplyr::distinct()

  performance_wide <- tidyr::pivot_wider(
    performance_numeric,
    names_from = .metric,
    values_from = .estimate
  )

  performance_display <- performance_sub %>%
    dplyr::select(Model, Engine, .metric, metric_display) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(
      names_from = .metric,
      values_from = metric_display
    )
  select_cols <- c("Model", "Engine", keep_metrics)
  select_cols <- intersect(select_cols, colnames(performance_wide))
  performance_wide <- dplyr::select(performance_wide, dplyr::all_of(select_cols))
  performance_display <- dplyr::select(performance_display, dplyr::all_of(select_cols))
  desired_metrics <- intersect(desired_metrics, select_cols[!(select_cols %in% c("Model", "Engine"))])



  # performance_wide$Engine <- engine_names[match(performance_wide$Model, names(engine_names))]
  # performance_wide <- performance_wide[, c("Model", "Engine", setdiff(colnames(performance_wide), c("Model", "Engine")))]

  # Sort direction: lower-better metrics vs higher-better metrics
  brier_cols <- grep("^brier_t", colnames(performance_wide), value = TRUE)
  ascending_metrics <- unique(c("rmse", "mae", "ibs", "logloss", "mse", brier_cols))
  if (main_metric %in% ascending_metrics) {
    order_idx <- order(performance_wide[[main_metric]], na.last = TRUE)
  } else {
    order_idx <- order(-performance_wide[[main_metric]], na.last = TRUE)
  }
  performance_wide <- performance_wide[order_idx, , drop = FALSE]
  performance_display <- performance_display[order_idx, , drop = FALSE]

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
    rmse = "RMSE",
    c_index = "Harrell C-index",
    uno_c = "Uno's C-index",
    ibs = "Integrated Brier Score",
    rmst_diff = "RMST diff (t_max)"
  )
  t_max_val <- object$survival_t_max
  if (!is.null(t_max_val) && length(t_max_val) == 1 && is.finite(t_max_val) && t_max_val > 0) {
    display_names[["rmst_diff"]] <- sprintf(
      "RMST diff (t<=%s)",
      format(t_max_val, trim = TRUE, digits = 4)
    )
  }

  if (!is.null(object$survival_brier_times)) {
    for (nm in names(object$survival_brier_times)) {
      if (!is.null(object$survival_brier_times[[nm]]) && is.finite(object$survival_brier_times[[nm]])) {
        time_label <- format(object$survival_brier_times[[nm]], trim = TRUE, digits = 4)
      } else {
        time_label <- nm
      }
      display_names[[nm]] <- sprintf("Brier(t=%s)", time_label)
    }
  }
  auto_brier_cols <- setdiff(grep("^brier_t", colnames(performance_display), value = TRUE), names(display_names))
  if (length(auto_brier_cols) > 0) {
    display_names[auto_brier_cols] <- auto_brier_cols
  }



  # Filter rows where the Model is in best_model_name and its Engine equals best_model_name[Model]
  best_val_df <- performance_wide %>%
    filter(Model %in% names(best_model_name)) %>%
    filter(mapply(function(m, e) e == best_model_name[[m]], Model, Engine))



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

  if ("metrics" %in% type) {

  cat("\n===== fastml Model Summary =====\n")
  cat("Task:", task, "\n")
  cat("Number of Models Trained:", model_count, "\n")

  # If you just want the unique metric values:
  best_val <- best_val_df %>%
    pull(!!sym(main_metric)) %>%
    unique()
  cat("Best Model(s):",
      paste0(names(best_model_name), " (", best_model_name, ")"),
      sprintf("(%s: %.7f)", main_metric, as.numeric(best_val)),
      "\n\n")

  cat("Performance Metrics (Sorted by ", main_metric,"):\n\n", sep = "")

  metrics_to_print <- c("Model", "Engine", desired_metrics)

  if(all(algorithm != "best")){

    performance_wide <- performance_wide %>% dplyr::filter(Model %in% algorithm)
    performance_display <- performance_display %>% dplyr::filter(Model %in% algorithm)
    best_model_idx <- get_best_model_idx(performance_wide, optimized_metric)
  }

  header <- c("Model", "Engine", sapply(desired_metrics, function(m) {
    if (m %in% names(display_names)) display_names[[m]] else m
  }))

  data_str <- performance_display
  data_str$Model <- as.character(data_str$Model)

  if(all(algorithm == "best")){
    data_str$Model[best_model_idx] <- paste0(data_str$Model[best_model_idx], "*")
  }

  # Compute safe column widths (handle NA and non-character values)
  safe_nchar <- function(x) {
    x_chr <- as.character(x)
    x_chr[is.na(x_chr)] <- ""
    nchar(x_chr)
  }

  col_widths <- sapply(seq_along(header), function(i) {
    col_name <- header[i]
    col_key  <- c("Model", "Engine", desired_metrics)[i]
    col_data <- data_str[[col_key]]
    name_w   <- safe_nchar(col_name)
    data_w   <- if (length(col_data)) max(safe_nchar(col_data), na.rm = TRUE) else 0
    max(name_w, data_w, na.rm = TRUE)
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

  if (all(algorithm == "best")) {
    cat("(*Best model)\n\n")
  }

}

  if ("params" %in% type) {

    if(length(algorithm) == 1 && all(algorithm == "best")){
      cat("Best Model hyperparameters:\n\n")

    }else{

      cat("Selected Model hyperparameters:\n\n")

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

  if ("conf_mat" %in% type) {

    if (task == "classification") {
      df_best <- list()

      for (i in seq_along(best_model_name)) {
        model_name <- names(best_model_name)[i]
        engine     <- best_model_name[i]
        name_combined <- sprintf("%s (%s)", model_name, engine)

        if (!is.null(predictions_list[[model_name]]) &&
            !is.null(predictions_list[[model_name]][[engine]])) {
          df_best[[name_combined]] <- predictions_list[[model_name]][[engine]]
        } else {
          cat("No predictions found for", model_name, "with engine", engine, "\n")
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
