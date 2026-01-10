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
#' For survival tasks, Harrell's C-index, Uno's C-index, the integrated Brier
#' score, and (when available) the RMST difference are shown by default. Specific
#' Brier(t) horizons can be requested through the \code{brier_times} argument.
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
#' @param show_ci Logical indicating whether to display 95\% confidence intervals
#'   for performance metrics in survival models. Defaults to \code{FALSE}.
#' @param brier_times Optional numeric or character vector that selects which
#'   time-specific Brier scores to display for survival models. When \code{NULL}
#'   (the default), time-specific Brier scores are omitted from the summary.
#' @param ... Additional arguments.
#' @return Prints summary of fastml models.
#'
#' @method summary fastml
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
#' @importFrom broom tidy glance
#' @importFrom rlang get_expr get_env sym
#' @importFrom viridisLite viridis
#' @importFrom tidyr pivot_wider
#' @importFrom stats coef quantile approx AIC BIC logLik nobs setNames
#' @importFrom utils capture.output head tail
#'
#' @export
summary.fastml <- function(object,
                                 algorithm = "best",
                                 type = c("all", "metrics", "params", "conf_mat"),
                                 sort_metric = NULL,
                                 show_ci = FALSE,
                                 brier_times = NULL,
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
  brier_time_lookup <- object$survival_brier_times
  resampling_plan <- object$resampling_plan
  resampling_desc <- fastml_describe_resampling(resampling_plan)

  # cat(sprintf("Resampling strategy: %s\n", resampling_desc))

  resolve_engine_name <- function(model_name, default_engine = NA_character_) {
    if (!is.null(engine_names) && !is.null(engine_names[[model_name]])) {
      eng_candidates <- engine_names[[model_name]]
      if (length(eng_candidates) > 0) {
        eng_candidates <- as.character(eng_candidates)
        eng_candidates <- eng_candidates[!is.na(eng_candidates) & eng_candidates != ""]
        if (length(eng_candidates) > 0) {
          return(eng_candidates[[1]])
        }
      }
    }
    if (!is.null(best_model_name) && model_name %in% names(best_model_name)) {
      eng <- best_model_name[[model_name]]
      if (!is.null(eng) && !is.na(eng) && eng != "") {
        return(as.character(eng))
      }
    }
    default_engine
  }

  # Loop over the top-level names (e.g. "rand_forest", "logistic_reg")
  metrics_list <- lapply(names(performance), function(model_name) {

    perf_entry <- performance[[model_name]]

    # Two possible structures:
    # 1) A list of engines, each containing a tibble with .metric/.estimate
    # 2) A single tibble (no engine nesting), typical when only one engine was used

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
      combined_engines$Engine <- resolve_engine_name(model_name)
    }

    # Add a "Model" column
    combined_engines$Model <- model_name
    combined_engines
  })

  # Combine all model groups into one data.frame
  performance_df <- do.call(rbind, metrics_list)

  if ("Model" %in% names(performance_df)) {
    performance_df$Model <- as.character(performance_df$Model)
  }
  if ("Engine" %in% names(performance_df)) {
    performance_df$Engine <- as.character(performance_df$Engine)
  }

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
    current_metric_names <- unique(performance_df$.metric)
    base_surv_metrics <- c("c_index", "uno_c", "ibs")
    if ("rmst_diff" %in% current_metric_names) {
      base_surv_metrics <- c(base_surv_metrics, "rmst_diff")
    }
    tau_max <- object$survival_t_max

    compute_brier_helper <- function(perf_df, time_lookup) {
      available <- sort(grep("^brier_t", unique(perf_df$.metric), value = TRUE))
      if (length(available) == 0) {
        return(list(
          available = character(0),
          time_map = stats::setNames(numeric(0), character(0)),
          numeric_map = numeric(0)
        ))
      }
      if (is.null(time_lookup) || length(time_lookup) == 0) {
        time_map <- stats::setNames(rep(NA_real_, length(available)), available)
      } else {
        time_map <- time_lookup[intersect(names(time_lookup), available)]
        missing_names <- setdiff(available, names(time_lookup))
        if (length(missing_names) > 0) {
          time_map <- c(time_map, stats::setNames(rep(NA_real_, length(missing_names)), missing_names))
        }
        time_map <- time_map[match(available, names(time_map))]
        names(time_map) <- available
      }
      numeric_map <- suppressWarnings(as.numeric(time_map))
      names(numeric_map) <- names(time_map)
      list(available = available, time_map = time_map, numeric_map = numeric_map)
    }

    order_brier_metrics <- function(metrics, numeric_map) {
      if (length(metrics) <= 1) {
        return(metrics)
      }
      idx <- match(metrics, names(numeric_map))
      times <- numeric_map[idx]
      ord <- order(ifelse(is.finite(times), times, Inf), metrics)
      metrics[ord]
    }

    match_metric_by_time <- function(target_time, numeric_map, tau_limit) {
      if (length(target_time) != 1 || !is.finite(target_time) || target_time <= 0) {
        return(NA_character_)
      }
      if (is.finite(tau_limit)) {
        target_time <- min(target_time, tau_limit)
      }
      valid_idx <- which(is.finite(numeric_map))
      if (length(valid_idx) == 0) {
        return(NA_character_)
      }
      diffs <- abs(numeric_map[valid_idx] - target_time)
      best_idx <- which.min(diffs)
      idx <- valid_idx[best_idx]
      best_diff <- diffs[best_idx]
      if (length(idx) != 1 || !is.finite(best_diff)) {
        return(NA_character_)
      }
      names(numeric_map)[idx]
    }

    default_brier_selection <- function(available_metrics, numeric_map) {
      if (length(available_metrics) == 0) {
        return(character(0))
      }
      numeric_vals <- numeric_map[is.finite(numeric_map)]
      if (length(numeric_vals) > 0) {
        quantile_targets <- stats::quantile(
          numeric_vals,
          probs = c(0.25, 0.5, 0.75),
          names = FALSE,
          na.rm = TRUE
        )
        quantile_targets <- unique(as.numeric(quantile_targets))
        quantile_targets <- quantile_targets[is.finite(quantile_targets)]
        if (length(quantile_targets) > 0) {
          matched <- vapply(
            quantile_targets,
            function(x) match_metric_by_time(x, numeric_map, tau_max),
            character(1)
          )
          matched <- matched[!is.na(matched) & nzchar(matched)]
          matched <- matched[!duplicated(matched)]
          if (length(matched) > 0) {
            matched <- order_brier_metrics(matched, numeric_map)
            return(matched[seq_len(min(3, length(matched)))])
          }
        }
      }
      fallback <- order_brier_metrics(available_metrics, numeric_map)
      fallback[seq_len(min(3, length(fallback)))]
    }

    make_combo_key <- function(model, engine) {
      engine_val <- ifelse(is.null(engine) || is.na(engine) || engine == "", "<NA>", engine)
      paste(model, engine_val, sep = "||")
    }

    compute_brier_from_curve <- function(curve_df, time_point) {
      if (is.null(curve_df) || nrow(curve_df) == 0) {
        return(NA_real_)
      }
      if (!all(c("eval_time", "brier") %in% names(curve_df))) {
        return(NA_real_)
      }
      times <- as.numeric(curve_df$eval_time)
      values <- as.numeric(curve_df$brier)
      valid <- is.finite(times) & is.finite(values)
      times <- times[valid]
      values <- values[valid]
      if (length(times) == 0) {
        return(NA_real_)
      }
      ord <- order(times)
      times <- times[ord]
      values <- values[ord]
      tol <- max(1e-08, 1e-06 * max(1, abs(time_point)))
      min_time <- min(times)
      max_time <- max(times)
      if (time_point < min_time - tol || time_point > max_time + tol) {
        return(NA_real_)
      }
      clamped_time <- time_point
      if (clamped_time < min_time) {
        clamped_time <- min_time
      }
      if (clamped_time > max_time) {
        clamped_time <- max_time
      }
      approx_res <- stats::approx(times, values, xout = clamped_time, ties = "ordered", rule = 1)
      val <- approx_res$y
      if (!is.finite(val)) {
        idx <- which.min(abs(times - clamped_time))
        if (length(idx) == 0) {
          return(NA_real_)
        }
        val <- values[idx]
      }
      pmax(pmin(val, 1), 0)
    }

    make_metric_name <- function(time_val, existing_names) {
      time_label <- format(time_val, trim = TRUE, scientific = FALSE)
      time_label <- gsub("[^0-9]+", "_", time_label)
      time_label <- gsub("_+", "_", time_label)
      time_label <- gsub("^_+|_+$", "", time_label)
      base <- if (nzchar(time_label)) {
        paste0("brier_t_at_", time_label)
      } else {
        "brier_t_custom"
      }
      candidate <- base
      counter <- 1L
      while (candidate %in% existing_names) {
        candidate <- paste0(base, "_", counter)
        counter <- counter + 1L
      }
      candidate
    }

    augment_performance_with_custom_brier <- function(perf_df, predictions, target_times, time_lookup, tau_limit) {
      if (length(target_times) == 0) {
        return(list(performance_df = perf_df, time_lookup = time_lookup, failed_times = numeric(0)))
      }
      combos <- unique(perf_df[, c("Model", "Engine")])
      combos$Model <- as.character(combos$Model)
      combos$Engine <- as.character(combos$Engine)
      if (nrow(combos) == 0) {
        return(list(performance_df = perf_df, time_lookup = time_lookup, failed_times = target_times))
      }
      brier_curve_lookup <- list()
      if (!is.null(predictions) && length(predictions) > 0) {
        for (model_name in names(predictions)) {
          pred_entry <- predictions[[model_name]]
          if (is.list(pred_entry) && !is.data.frame(pred_entry)) {
            for (engine_name in names(pred_entry)) {
              curve <- attr(pred_entry[[engine_name]], "brier_curve")
              if (!is.null(curve)) {
                brier_curve_lookup[[make_combo_key(model_name, engine_name)]] <- curve
              }
            }
          } else if (is.data.frame(pred_entry)) {
            engine_name <- resolve_engine_name(model_name)
            curve <- attr(pred_entry, "brier_curve")
            if (!is.null(curve)) {
              brier_curve_lookup[[make_combo_key(model_name, engine_name)]] <- curve
            }
          }
        }
      }
      existing_names <- unique(perf_df$.metric)
      failed <- numeric(0)
      new_rows <- list()
      for (time_val in target_times) {
        if (!is.finite(time_val) || time_val <= 0) {
          next
        }
        tol <- max(1e-08, 1e-06 * max(1, abs(time_val)))
        if (is.finite(tau_limit) && time_val > tau_limit + tol) {
          failed <- c(failed, time_val)
          next
        }
        values <- vapply(seq_len(nrow(combos)), function(idx) {
          key <- make_combo_key(combos$Model[idx], combos$Engine[idx])
          curve <- brier_curve_lookup[[key]]
          compute_brier_from_curve(curve, time_val)
        }, numeric(1))
        if (!any(is.finite(values))) {
          failed <- c(failed, time_val)
          next
        }
        metric_name <- make_metric_name(time_val, existing_names)
        existing_names <- c(existing_names, metric_name)
        row_count <- nrow(combos)
        new_df <- data.frame(
          .metric = rep(metric_name, row_count),
          .estimate = values,
          stringsAsFactors = FALSE
        )
        if (".lower" %in% names(perf_df)) new_df$.lower <- NA_real_
        if (".upper" %in% names(perf_df)) new_df$.upper <- NA_real_
        if (".n_boot" %in% names(perf_df)) new_df$.n_boot <- 0
        other_cols <- setdiff(colnames(perf_df), names(new_df))
        for (col_nm in other_cols) {
          if (col_nm == "Engine") {
            new_df[[col_nm]] <- combos$Engine
          } else if (col_nm == "Model") {
            new_df[[col_nm]] <- combos$Model
          } else {
            new_df[[col_nm]] <- NA
          }
        }
        new_df <- new_df[, colnames(perf_df), drop = FALSE]
        new_rows[[length(new_rows) + 1]] <- new_df
        time_lookup <- c(time_lookup, stats::setNames(time_val, metric_name))
      }
      if (length(new_rows) > 0) {
        perf_df <- rbind(perf_df, do.call(rbind, new_rows))
      }
      list(performance_df = perf_df, time_lookup = time_lookup, failed_times = failed)
    }

    helper <- compute_brier_helper(performance_df, brier_time_lookup)
    available_brier_metrics <- helper$available
    available_time_map <- helper$time_map
    numeric_time_map <- helper$numeric_map
    selected_brier_metrics <- character(0)
    matched_metrics <- character(0)
    unmatched_numeric <- numeric(0)
    failure_times <- numeric(0)

    if (!is.null(brier_times) && length(brier_times) > 0) {
      input_list <- as.list(brier_times)
      for (bt in input_list) {
        if (is.character(bt) && length(bt) == 1 && bt %in% available_brier_metrics) {
          matched_metrics <- c(matched_metrics, bt)
        } else {
          bt_numeric <- suppressWarnings(as.numeric(bt))
          if (length(bt_numeric) == 1 && is.finite(bt_numeric) && bt_numeric > 0) {
            metric_name <- match_metric_by_time(bt_numeric, numeric_time_map, tau_max)
            if (!is.na(metric_name)) {
              matched_metrics <- c(matched_metrics, metric_name)
            } else {
              unmatched_numeric <- c(unmatched_numeric, bt_numeric)
            }
          }
        }
      }
      unmatched_numeric <- unique(unmatched_numeric[is.finite(unmatched_numeric) & unmatched_numeric > 0])
      matched_metrics <- matched_metrics[matched_metrics %in% available_brier_metrics]
      matched_metrics <- matched_metrics[!duplicated(matched_metrics)]

      if (length(unmatched_numeric) > 0) {
        augment_result <- augment_performance_with_custom_brier(
          performance_df,
          predictions_list,
          unmatched_numeric,
          brier_time_lookup,
          tau_max
        )
        performance_df <- augment_result$performance_df
        brier_time_lookup <- augment_result$time_lookup
        failure_times <- c(failure_times, augment_result$failed_times)

        helper <- compute_brier_helper(performance_df, brier_time_lookup)
        available_brier_metrics <- helper$available
        available_time_map <- helper$time_map
        numeric_time_map <- helper$numeric_map

        to_retry <- setdiff(unmatched_numeric, failure_times)
        if (length(to_retry) > 0) {
          for (bt_numeric in to_retry) {
            metric_name <- match_metric_by_time(bt_numeric, numeric_time_map, tau_max)
            if (!is.na(metric_name)) {
              matched_metrics <- c(matched_metrics, metric_name)
            } else {
              failure_times <- c(failure_times, bt_numeric)
            }
          }
        }
        matched_metrics <- matched_metrics[matched_metrics %in% available_brier_metrics]
        matched_metrics <- matched_metrics[!duplicated(matched_metrics)]
      }

      if (length(matched_metrics) == 0) {
        defaults <- default_brier_selection(available_brier_metrics, numeric_time_map)
        if (length(defaults) > 0) {
          message(
            "None of the requested brier_times overlapped with the available follow-up; ",
            "using default horizons based on observed quartiles."
          )
        }
        selected_brier_metrics <- defaults
      } else {
        selected_brier_metrics <- order_brier_metrics(matched_metrics, numeric_time_map)
      }

      failure_times <- sort(unique(failure_times[is.finite(failure_times)]))
      if (length(failure_times) > 0) {
        warning(
          sprintf(
            "Some requested Brier time points were not available and were omitted: %s",
            paste(format(failure_times, trim = TRUE, digits = 6), collapse = ", ")
          )
        )
      }
    } else {
      selected_brier_metrics <- default_brier_selection(available_brier_metrics, numeric_time_map)
    }

    all_metric_names <- unique(performance_df$.metric)
    desired_metrics <- unique(c(base_surv_metrics, selected_brier_metrics))
  } else {
    # Fallback for any other task types
    desired_metrics <- unique(performance_df$.metric)
  }
  desired_metrics <- intersect(desired_metrics, all_metric_names)
  if (length(desired_metrics) == 0) desired_metrics <- main_metric

  metrics_for_sub <- unique(c(desired_metrics, main_metric))
  metrics_for_sub <- intersect(metrics_for_sub, all_metric_names)

  performance_sub <- performance_df[performance_df$.metric %in% metrics_for_sub, ] %>%
    dplyr::select(-dplyr::any_of(".estimator"))

  has_ci_cols <- all(c(".lower", ".upper") %in% colnames(performance_sub))
  show_ci_available <- isTRUE(show_ci) && has_ci_cols

  if (show_ci_available) {
    performance_sub <- performance_sub %>%
      dplyr::mutate(
        metric_display = dplyr::case_when(
          is.na(.data$.estimate) ~ NA_character_,
          !is.na(.data$.lower) & !is.na(.data$.upper) ~
            sprintf("%.3f (%.3f, %.3f)", .data$.estimate, .data$.lower, .data$.upper),
          TRUE ~ sprintf("%.3f", .data$.estimate)
        )
      )
  } else {
    performance_sub <- performance_sub %>%
      dplyr::mutate(
        metric_display = dplyr::case_when(
          is.na(.data$.estimate) ~ NA_character_,
          TRUE ~ sprintf("%.3f", .data$.estimate)
        )
      )
  }
  metrics_for_numeric <- metrics_for_sub
  keep_metrics <- metrics_for_numeric
  if (length(engine_names) == 1 && "LiblineaR" %in% engine_names) {
    allowed_liblinear <- c("accuracy", "kap", "sens", "spec", "precision", "f_meas")
    keep_metrics <- intersect(keep_metrics, allowed_liblinear)
    if (!(main_metric %in% keep_metrics) && main_metric %in% metrics_for_numeric) {
      keep_metrics <- c(keep_metrics, main_metric)
    }
  }
  keep_metrics <- unique(keep_metrics)
  performance_numeric <- performance_sub %>%
    dplyr::select(dplyr::all_of(c("Model", "Engine", ".metric", ".estimate"))) %>%
    dplyr::distinct()

  performance_wide <- tidyr::pivot_wider(
    performance_numeric,
    names_from = ".metric",
    values_from = ".estimate"
  )

  performance_display <- performance_sub %>%
    dplyr::select(dplyr::all_of(c("Model", "Engine", ".metric", "metric_display"))) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(
      names_from = ".metric",
      values_from = "metric_display"
    )
  select_cols_numeric <- c("Model", "Engine", keep_metrics)
  select_cols_numeric <- intersect(select_cols_numeric, colnames(performance_wide))
  performance_wide <- dplyr::select(performance_wide, dplyr::all_of(select_cols_numeric))
  select_cols_display <- c("Model", "Engine", desired_metrics)
  select_cols_display <- intersect(select_cols_display, colnames(performance_display))
  performance_display <- dplyr::select(performance_display, dplyr::all_of(select_cols_display))
  desired_metrics <- intersect(desired_metrics, select_cols_display[!(select_cols_display %in% c("Model", "Engine"))])



  # performance_wide$Engine <- engine_names[match(performance_wide$Model, names(engine_names))]
  # performance_wide <- performance_wide[, c("Model", "Engine", setdiff(colnames(performance_wide), c("Model", "Engine")))]

  # Sort direction: lower-better metrics vs higher-better metrics
  brier_cols <- grep("^brier_t", colnames(performance_wide), value = TRUE)
  ascending_metrics <- unique(c("rmse", "mae", "ibs", "logloss", "mse", brier_cols))
  if (main_metric %in% colnames(performance_wide)) {
    if (main_metric %in% ascending_metrics) {
      order_idx <- order(performance_wide[[main_metric]], na.last = TRUE)
    } else {
      order_idx <- order(-performance_wide[[main_metric]], na.last = TRUE)
    }
  } else {
    warning(sprintf("Main metric '%s' not available for sorting; retaining original order.", main_metric))
    order_idx <- seq_len(nrow(performance_wide))
  }
  performance_wide <- performance_wide[order_idx, , drop = FALSE]
  performance_display <- performance_display[order_idx, , drop = FALSE]

  performance_lookup <- performance_wide
  if (all(algorithm != "best")) {
    performance_lookup <- performance_lookup %>% dplyr::filter(Model %in% algorithm)
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

  if (!is.null(brier_time_lookup) && length(brier_time_lookup) > 0) {
    for (nm in names(brier_time_lookup)) {
      if (!is.null(brier_time_lookup[[nm]]) && is.finite(brier_time_lookup[[nm]])) {
        time_label <- format(brier_time_lookup[[nm]], trim = TRUE, digits = 4)
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
    filter({
      if (length(Model) == 0) {
        rep(FALSE, length(Model))
      } else {
        mapply(function(m, e) {
          bm <- best_model_name[[m]]
          if (is.null(bm) || length(bm) == 0 || is.na(bm)) {
            return(FALSE)
          }
          e == bm
        }, Model, Engine)
      }
    })



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
    performance_lookup <- performance_wide
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

    format_numeric_vec <- function(x, digits = 4) {
      if (is.null(x) || length(x) == 0) {
        return(character())
      }
      x <- suppressWarnings(as.numeric(x))
      if (length(x) == 0) {
        return(character())
      }
      out <- vapply(x, function(val) {
        if (is.na(val) || !is.finite(val)) {
          "<NA>"
        } else {
          format(signif(val, digits), trim = TRUE, scientific = FALSE)
        }
      }, character(1))
      out
    }

    format_numeric_single <- function(x, digits = 4) {
      vec <- format_numeric_vec(x, digits = digits)
      if (length(vec) == 0) {
        "<NA>"
      } else {
        vec[[1]]
      }
    }

    format_param_value <- function(val) {
      if (rlang::is_quosure(val)) {
        return(rlang::quo_text(val))
      }
      if (is.numeric(val)) {
        return(paste(format_numeric_vec(val), collapse = ", "))
      }
      if (is.logical(val)) {
        return(paste(ifelse(val, "TRUE", "FALSE"), collapse = ", "))
      }
      if (is.character(val)) {
        return(paste(val, collapse = ", "))
      }
      if (is.factor(val)) {
        return(paste(as.character(val), collapse = ", "))
      }
      if (length(val) == 1) {
        return(as.character(val))
      }
      paste(as.character(val), collapse = ", ")
    }

    parse_model_label <- function(label) {
      if (is.null(label)) {
        return(list(algorithm = NA_character_, engine = NA_character_))
      }
      algo <- trimws(sub("\\s*\\(.*$", "", label))
      engine <- if (grepl("\\(", label, fixed = FALSE)) {
        sub("^.*\\(([^()]*)\\)\\s*$", "\\1", label)
      } else {
        NA_character_
      }
      list(algorithm = algo, engine = trimws(engine))
    }

    extract_survival_fit <- function(label, model_obj) {
      candidates <- list(object$models[[label]], model_obj)
      for (candidate in candidates) {
        if (is.null(candidate)) {
          next
        }
        fit_candidate <- tryCatch(candidate$fit$fit$fit, error = function(e) NULL)
        if (inherits(fit_candidate, c("survreg", "coxph", "stpm2", "pstpm2", "flexsurvreg", "fastml_xgb_survival"))) {
          return(fit_candidate)
        }
        fit_candidate <- tryCatch(candidate$fit$fit, error = function(e) NULL)
        if (inherits(fit_candidate, c("survreg", "coxph", "stpm2", "pstpm2", "flexsurvreg", "fastml_xgb_survival"))) {
          return(fit_candidate)
        }
        fit_candidate <- tryCatch(candidate$fit, error = function(e) NULL)
        if (inherits(fit_candidate, c("survreg", "coxph", "stpm2", "pstpm2", "flexsurvreg", "fastml_xgb_survival"))) {
          return(fit_candidate)
        }
      }
      NULL
    }

    fastml_survival_tidy_df <- function(fit_obj) {
      tidy_df <- tryCatch(broom::tidy(fit_obj), error = function(e) NULL)
      if (!is.null(tidy_df)) {
        return(as.data.frame(tidy_df, stringsAsFactors = FALSE))
      }
      summary_fit <- tryCatch(suppressWarnings(summary(fit_obj)), error = function(e) NULL)
      if (is.null(summary_fit)) {
        return(NULL)
      }
      candidate_names <- c("coefficients", "coef", "coef.table", "coef3")
      for (nm in candidate_names) {
        comp <- tryCatch(summary_fit[[nm]], error = function(e) NULL)
        if (is.null(comp)) {
          next
        }
        if (is.matrix(comp)) {
          df <- as.data.frame(comp, stringsAsFactors = FALSE)
        } else if (is.data.frame(comp)) {
          df <- comp
        } else {
          next
        }
        if (nrow(df) == 0) {
          next
        }
        if (!"term" %in% names(df)) {
          df <- rownames(df)
        }
        if (!"term" %in% names(df)) {
          df <- seq_len(nrow(df))
        }
        df <- df[, c("term", setdiff(names(df), "term")), drop = FALSE]
        return(df)
      }
      NULL
    }

    fastml_survival_glance_df <- function(fit_obj) {
      glance_df <- tryCatch(broom::glance(fit_obj), error = function(e) NULL)
      if (!is.null(glance_df)) {
        return(as.data.frame(glance_df, stringsAsFactors = FALSE))
      }
      safe_stat <- function(expr) {
        val <- tryCatch(expr, error = function(e) NA_real_)
        if (length(val) == 0) {
          return(NA_real_)
        }
        as.numeric(val[1])
      }
      data.frame(
        logLik = safe_stat(stats::logLik(fit_obj)),
        AIC = safe_stat(stats::AIC(fit_obj)),
        BIC = safe_stat(stats::BIC(fit_obj)),
        nobs = safe_stat(stats::nobs(fit_obj)),
        stringsAsFactors = FALSE
      )
    }

    fastml_print_survival_summary <- function(fit_obj, heading = NULL) {
      if (!is.null(heading)) {
        cat("  ", heading, "\n", sep = "")
      }
      model_formula <- tryCatch(stats::formula(fit_obj), error = function(e) NULL)
      if (!is.null(model_formula)) {
        cat("  Formula: ", paste(deparse(model_formula), collapse = " "), "\n", sep = "")
      }
      glance_df <- fastml_survival_glance_df(fit_obj)
      if (!is.null(glance_df)) {
        cat("  Fit statistics:\n")
        print(glance_df, row.names = FALSE)
      }
      tidy_df <- fastml_survival_tidy_df(fit_obj)
      if (!is.null(tidy_df)) {
        cat("  Coefficients:\n")
        print(tidy_df, row.names = FALSE)
      }
      TRUE
    }

    print_stpm2_details <- function(fit_obj, model_info = NULL) {
      if (is.function(fit_obj) && !is.null(model_info)) {
        alt_fit <- tryCatch(model_info, error = function(e) NULL)
        if (inherits(alt_fit, c("stpm2", "pstpm2"))) {
          fit_obj <- alt_fit
        }
      }
      if (!inherits(fit_obj, c("stpm2", "pstpm2"))) {
        return(FALSE)
      }
      fastml_print_survival_summary(fit_obj, heading = "Royston-Parmar (rstpm2)")
    }
    print_survreg_details <- function(fit_obj) {
      coef_vec <- tryCatch(stats::coef(fit_obj), error = function(e) NULL)
      if (!is.null(coef_vec) && length(coef_vec) > 0) {
        cat("  Coefficients (coef):\n")
        coef_fmt <- format_numeric_vec(coef_vec)
        if (!is.null(names(coef_vec))) {
          names(coef_fmt) <- names(coef_vec)
        }
        coef_mat <- matrix(coef_fmt, ncol = 1)
        rownames(coef_mat) <- if (!is.null(names(coef_fmt))) names(coef_fmt) else rownames(coef_mat)
        colnames(coef_mat) <- "coef"
        print(coef_mat, quote = FALSE)
      } else {
        cat("  Coefficients (coef): <unavailable>\n")
      }

      scale_val <- tryCatch(fit_obj$scale, error = function(e) NA_real_)
      cat("  Scale:", format_numeric_single(scale_val), "\n")

      dist_val <- tryCatch(fit_obj$dist, error = function(e) NA_character_)
      dist_str <- if (!is.null(dist_val) && length(dist_val) > 0 && nzchar(as.character(dist_val)[1])) {
        as.character(dist_val)[1]
      } else {
        "<NA>"
      }
      cat("  Distribution:", dist_str, "\n")

      loglik_val <- tryCatch(fit_obj$loglik, error = function(e) NULL)
      loglik_last <- if (!is.null(loglik_val) && length(loglik_val) > 0) {
        loglik_val[length(loglik_val)]
      } else {
        NA_real_
      }
      cat("  Log-likelihood:", format_numeric_single(loglik_last), "\n")
      TRUE
    }

    print_flexsurv_details <- function(fit_obj, model_info = NULL) {
      dist_val <- tryCatch(fit_obj$dist, error = function(e) NULL)
      dist_label <- NULL
      if (!is.null(model_info)) {
        dist_label <- tryCatch(model_info$distribution_label, error = function(e) NULL)
      }
      if (is.null(dist_label) || !nzchar(as.character(dist_label)[1])) {
        if (is.character(dist_val) && length(dist_val) > 0) {
          dist_label <- dist_val[1]
        } else if (is.list(dist_val) && !is.null(dist_val$dist)) {
          dist_label <- dist_val$dist
        }
      }
      if (is.null(dist_label) || !nzchar(as.character(dist_label)[1])) {
        dist_label <- "<unknown>"
      }
      cat("  Distribution:", dist_label, "\n")

      coef_vec <- tryCatch({
        stats::coef(fit_obj)
      }, error = function(e) {
        tryCatch(fit_obj$coefficients, error = function(e2) NULL)
      })
      if (!is.null(coef_vec) && length(coef_vec) > 0) {
        cat("  Coefficients (link scale):\n")
        coef_fmt <- format_numeric_vec(coef_vec)
        coef_mat <- matrix(coef_fmt, ncol = 1)
        rownames(coef_mat) <- if (!is.null(names(coef_vec))) names(coef_vec) else rownames(coef_mat)
        colnames(coef_mat) <- "coef"
        print(coef_mat, quote = FALSE)
      }

      res_mat <- tryCatch(as.matrix(fit_obj$res), error = function(e) NULL)
      if (is.matrix(res_mat) && nrow(res_mat) > 0) {
        cat("  Parameter estimates:\n")
        col_names <- colnames(res_mat)
        fmt_mat <- apply(res_mat, 2, format_numeric_vec)
        if (is.vector(fmt_mat)) {
          fmt_mat <- matrix(fmt_mat, ncol = 1)
          colnames(fmt_mat) <- col_names[1]
        }
        rownames(fmt_mat) <- rownames(res_mat)
        print(fmt_mat, quote = FALSE)
      }

      loglik_val <- tryCatch({
        ll <- fit_obj$loglik
        if (length(ll) > 0) ll[length(ll)] else NA_real_
      }, error = function(e) NA_real_)
      if (is.finite(loglik_val)) {
        cat("  Log-likelihood:", format_numeric_single(loglik_val), "\n")
      }

      aic_val <- tryCatch(as.numeric(fit_obj$AIC), error = function(e) NA_real_)
      if (is.finite(aic_val)) {
        cat("  AIC:", format_numeric_single(aic_val), "\n")
      }

      bic_val <- tryCatch(as.numeric(fit_obj$BIC), error = function(e) NA_real_)
      if (is.finite(bic_val)) {
        cat("  BIC:", format_numeric_single(bic_val), "\n")
      }

      train_times <- NULL
      train_status <- NULL
      train_size <- NULL
      if (!is.null(model_info)) {
        train_times <- tryCatch(model_info$train_times, error = function(e) NULL)
        train_status <- tryCatch(model_info$train_status, error = function(e) NULL)
        train_size <- tryCatch(model_info$train_size, error = function(e) NULL)
      }
      n_obs <- tryCatch(as.numeric(fit_obj$N), error = function(e) NA_real_)
      events <- NA_real_
      censored <- NA_real_
      if (is.null(train_status) && is.numeric(train_size) && length(train_size) == 1 && is.finite(train_size)) {
        n_obs <- train_size
      }
      if (!is.null(train_status)) {
        status_norm <- tryCatch({
          fastml_normalize_survival_status(train_status, reference_length = length(train_status))$status
        }, error = function(e) NULL)
        if (!is.null(status_norm)) {
          n_obs <- length(status_norm)
          events <- sum(status_norm == 1, na.rm = TRUE)
          censored <- sum(status_norm == 0, na.rm = TRUE)
        }
      } else if (!is.null(train_times)) {
        times_num <- as.numeric(train_times)
        times_num <- times_num[is.finite(times_num)]
        if (length(times_num) > 0) {
          n_obs <- length(times_num)
        }
      }
      if (is.finite(n_obs)) {
        msg <- paste0("  Sample size: ", format_numeric_single(n_obs, digits = 0))
        details <- character(0)
        if (is.finite(events)) {
          details <- c(details, paste0("events = ", format_numeric_single(events, digits = 0)))
        }
        if (is.finite(censored)) {
          details <- c(details, paste0("censored = ", format_numeric_single(censored, digits = 0)))
        }
        if (length(details) > 0) {
          msg <- paste0(msg, " (", paste(details, collapse = ", "), ")")
        }
        cat(msg, "\n")
      }

      breaks_info <- NULL
      if (!is.null(model_info)) {
        breaks_info <- tryCatch(model_info$breaks, error = function(e) NULL)
      }
      if (!is.null(breaks_info) && length(breaks_info) > 0) {
        cat("  Piecewise breaks:", paste(format_numeric_vec(breaks_info), collapse = ", "), "\n")
      }

      TRUE
    }

    print_coxph_details <- function(fit_obj, model_info = NULL, performance_row = NULL) {
      summary_fit <- tryCatch(summary(fit_obj), error = function(e) NULL)
      if (is.null(summary_fit)) {
        return(FALSE)
      }

      remove_terms <- character()
      strata_details <- list()
      if (!is.null(model_info)) {
        strata_cols <- tryCatch(model_info$strata_cols, error = function(e) NULL)
        strata_dummy <- tryCatch(model_info$strata_dummy_cols, error = function(e) NULL)
        strata_base <- tryCatch(model_info$strata_base_cols, error = function(e) NULL)
        remove_terms <- unique(c(
          remove_terms,
          if (!is.null(strata_cols)) strata_cols else character(),
          if (!is.null(strata_dummy)) strata_dummy else character(),
          if (!is.null(strata_base)) strata_base else character()
        ))
        info_list <- tryCatch(model_info$strata_info, error = function(e) NULL)
        if (is.list(info_list) && length(info_list) > 0) {
          for (nm in names(info_list)) {
            entry <- info_list[[nm]]
            if (is.null(entry)) {
              next
            }
            col_nm <- if (!is.null(entry$column)) entry$column else nm
            display_nm <- entry$display
            if (is.null(display_nm) || length(display_nm) == 0 || !nzchar(as.character(display_nm)[1])) {
              display_nm <- col_nm
            }
            level_vals <- entry$levels
            if (is.null(level_vals)) {
              level_vals <- character()
            }
            level_vals <- as.character(level_vals)
            strata_details[[length(strata_details) + 1]] <- list(
              column = col_nm,
              display = display_nm,
              levels = level_vals
            )
          }
        }
      }
      remove_terms <- remove_terms[nzchar(remove_terms)]
      remove_terms <- unique(remove_terms)

      filter_matrix <- function(mat, terms_to_remove) {
        if (!is.null(mat) && is.matrix(mat) && nrow(mat) > 0) {
          rn <- rownames(mat)
          if (!is.null(rn) && length(terms_to_remove) > 0) {
            drop_idx <- rep(FALSE, length(rn))
            for (term in terms_to_remove) {
              drop_idx <- drop_idx |
                rn == term |
                startsWith(rn, paste0(term, ":")) |
                startsWith(rn, paste0(term, "=")) |
                rn == paste0("strata(", term, ")")
            }
            mat <- mat[!drop_idx, , drop = FALSE]
          }
        }
        mat
      }

      coef_mat_raw <- summary_fit$coefficients
      coef_mat <- filter_matrix(coef_mat_raw, remove_terms)
      coef_raw_is_matrix <- is.matrix(coef_mat_raw)
      removed_all_coef <- coef_raw_is_matrix && nrow(coef_mat_raw) > 0 &&
        (!is.matrix(coef_mat) || nrow(coef_mat) == 0)
      no_coef_rows <- coef_raw_is_matrix && nrow(coef_mat_raw) == 0

      if (is.matrix(coef_mat) && nrow(coef_mat) > 0 && "coef" %in% colnames(coef_mat)) {
        cat("  Coefficients (coef):\n")
        coef_fmt <- format_numeric_vec(coef_mat[, "coef"])
        if (!is.null(rownames(coef_mat))) {
          names(coef_fmt) <- rownames(coef_mat)
        }
        coef_out <- matrix(coef_fmt, ncol = 1)
        rownames(coef_out) <- if (!is.null(names(coef_fmt))) names(coef_fmt) else rownames(coef_out)
        colnames(coef_out) <- "coef"
        print(coef_out, quote = FALSE)
      } else if (removed_all_coef || no_coef_rows) {
        cat("  Coefficients (coef): <none>\n")
      } else {
        cat("  Coefficients (coef): <unavailable>\n")
      }

      if (is.matrix(coef_mat) && nrow(coef_mat) > 0 && "exp(coef)" %in% colnames(coef_mat)) {
        cat("  exp(coef):\n")
        exp_fmt <- format_numeric_vec(coef_mat[, "exp(coef)"])
        if (!is.null(rownames(coef_mat))) {
          names(exp_fmt) <- rownames(coef_mat)
        }
        exp_out <- matrix(exp_fmt, ncol = 1)
        rownames(exp_out) <- if (!is.null(names(exp_fmt))) names(exp_fmt) else rownames(exp_out)
        colnames(exp_out) <- "exp(coef)"
        print(exp_out, quote = FALSE)
      } else if (removed_all_coef || no_coef_rows) {
        cat("  exp(coef): <none>\n")
      } else {
        cat("  exp(coef): <unavailable>\n")
      }

      conf_int_raw <- summary_fit$conf.int
      conf_int <- filter_matrix(conf_int_raw, remove_terms)
      conf_raw_is_matrix <- is.matrix(conf_int_raw)
      removed_all_conf <- conf_raw_is_matrix && nrow(conf_int_raw) > 0 &&
        (!is.matrix(conf_int) || nrow(conf_int) == 0)
      no_conf_rows <- conf_raw_is_matrix && nrow(conf_int_raw) == 0

      if (is.matrix(conf_int) && nrow(conf_int) > 0 &&
          all(c("exp(coef)", "lower .95", "upper .95") %in% colnames(conf_int))) {
        cat("  Hazard Ratios (95% CI):\n")
        hr_fmt <- format_numeric_vec(conf_int[, "exp(coef)"])
        lower_fmt <- format_numeric_vec(conf_int[, "lower .95"])
        upper_fmt <- format_numeric_vec(conf_int[, "upper .95"])
        if (!is.null(rownames(conf_int))) {
          names(hr_fmt) <- rownames(conf_int)
          names(lower_fmt) <- rownames(conf_int)
          names(upper_fmt) <- rownames(conf_int)
        }
        hr_mat <- cbind(
          "HR" = hr_fmt,
          "Lower 95%" = lower_fmt,
          "Upper 95%" = upper_fmt
        )
        rownames(hr_mat) <- if (!is.null(rownames(conf_int))) rownames(conf_int) else rownames(hr_mat)
        print(hr_mat, quote = FALSE)
      } else if (removed_all_conf || removed_all_coef || no_conf_rows) {
        cat("  Hazard Ratios (95% CI): <none>\n")
      } else {
        cat("  Hazard Ratios: <unavailable>\n")
      }

      if (length(strata_details) == 0 && !is.null(model_info)) {
        fallback_cols <- tryCatch(model_info$strata_cols, error = function(e) NULL)
        if (!is.null(fallback_cols) && length(fallback_cols) > 0) {
          for (sc in fallback_cols) {
            strata_details[[length(strata_details) + 1]] <- list(
              column = sc,
              display = sc,
              levels = character()
            )
          }
        }
      }
      if (length(strata_details) > 0) {
        xlevels <- tryCatch(fit_obj$xlevels, error = function(e) NULL)
        if (!is.null(xlevels)) {
          for (i in seq_along(strata_details)) {
            col_nm <- strata_details[[i]]$column
            if (!is.null(col_nm) && col_nm %in% names(xlevels)) {
              level_vals <- as.character(xlevels[[col_nm]])
              level_vals <- level_vals[!is.na(level_vals)]
              if (length(level_vals) > 0) {
                strata_details[[i]]$levels <- unique(level_vals)
              }
            }
          }
        }
        cat("  Stratified by:\n")
        for (entry in strata_details) {
          if (is.null(entry)) {
            next
          }
          display_nm <- entry$display
          if (is.null(display_nm) || length(display_nm) == 0 || !nzchar(as.character(display_nm)[1])) {
            display_nm <- entry$column
          }
          level_vals <- entry$levels
          if (is.null(level_vals)) {
            level_vals <- character()
          }
          level_vals <- as.character(level_vals)
          level_vals <- level_vals[!is.na(level_vals) & nzchar(level_vals)]
          level_vals <- unique(level_vals)
          n_strata <- length(level_vals)
          if (n_strata > 0) {
            cat(sprintf("    %s (%d strata: %s)", display_nm, n_strata, paste(level_vals, collapse = ", ")), "\n", sep = "")
          } else {
            cat(sprintf("    %s (0 strata)", display_nm), "\n", sep = "")
          }
        }
      }

      log_test <- summary_fit$logtest
      if (!is.null(log_test) && length(log_test) >= 3) {
        lr_text <- paste0(
          format_numeric_single(log_test[1]),
          " on ",
          format_numeric_single(log_test[2], digits = 0),
          " df (p = ",
          format_numeric_single(log_test[3]),
          ")"
        )
        cat("  Likelihood ratio test:", lr_text, "\n")
      } else {
        cat("  Likelihood ratio test: <unavailable>\n")
      }

      concord_val <- NA_real_
      if (!is.null(performance_row) && nrow(performance_row) >= 1 && "c_index" %in% colnames(performance_row)) {
        concord_val <- suppressWarnings(as.numeric(performance_row$c_index[1]))
      }
      if (!is.finite(concord_val)) {
        concord_vec <- summary_fit$concordance
        if (!is.null(concord_vec) && length(concord_vec) >= 1) {
          concord_val <- suppressWarnings(as.numeric(concord_vec[1]))
        }
      }
      if (is.finite(concord_val)) {
        cat("  Concordance (Harrell C-index):", format_numeric_single(concord_val), "\n")
      } else {
        cat("  Concordance (Harrell C-index): <unavailable>\n")
      }
      TRUE
    }

    print_xgb_survival_details <- function(fit_obj) {
      if (!inherits(fit_obj, "fastml_xgb_survival")) {
        return(FALSE)
      }
      objective <- tryCatch(fit_obj$objective, error = function(e) NULL)
      objective <- if (!is.null(objective) && length(objective) > 0 && nzchar(as.character(objective)[1])) {
        as.character(objective)[1]
      } else {
        "<unknown>"
      }
      if (identical(objective, "survival:cox")) {
        cat("  Objective: survival:cox  risk ranking only\n")
      } else if (identical(objective, "survival:aft")) {
        cat("  Objective: survival:aft  full survival modeling\n")
      } else {
        cat("  Objective:", objective, "\n")
      }
      booster <- tryCatch(fit_obj$booster, error = function(e) NULL)
      rounds <- NA_real_
      if (!is.null(booster)) {
        rounds <- tryCatch(as.numeric(booster$niter), error = function(e) NA_real_)
      }
      if (is.finite(rounds) && rounds > 0) {
        cat("  Boosting rounds:", format_numeric_single(rounds, digits = 0), "\n")
      }
      feat_count <- length(tryCatch(fit_obj$feature_names, error = function(e) NULL))
      if (feat_count > 0) {
        cat("  Predictors:", format_numeric_single(feat_count, digits = 0), "\n")
      }
      if (identical(objective, "survival:aft")) {
        dist_val <- tryCatch(fit_obj$aft_distribution, error = function(e) NULL)
        dist_str <- if (!is.null(dist_val) && length(dist_val) > 0 && nzchar(as.character(dist_val)[1])) {
          as.character(dist_val)[1]
        } else {
          "<unknown>"
        }
        cat("  Distribution:", dist_str, "\n")
        scale_val <- tryCatch(as.numeric(fit_obj$aft_scale), error = function(e) NA_real_)
        if (is.finite(scale_val)) {
          cat("  Scale:", format_numeric_single(scale_val), "\n")
        } else {
          cat("  Scale: <unavailable>\n")
        }
      }
      TRUE
    }

    print_parsnip_params <- function(model_obj) {
      parsnip_fit <- tryCatch(extract_fit_parsnip(model_obj), error = function(e) NULL)
      if (is.null(parsnip_fit)) {
        return(FALSE)
      }
      params <- tryCatch(parsnip_fit$spec$args, error = function(e) NULL)
      if (is.null(params) || length(params) == 0) {
        cat("  No hyperparameters found.\n")
        return(TRUE)
      }
      for (pname in names(params)) {
        val <- params[[pname]]
        if (inherits(val, "quosure")) {
          val <- tryCatch(
            eval(rlang::get_expr(val), envir = rlang::get_env(val)),
            error = function(e) val
          )
        }
        cat("  ", pname, ": ", format_param_value(val), "\n", sep = "")
      }
      TRUE
    }

    if (is.null(desired_models) || length(desired_models) == 0) {
      cat("Could not extract final fitted model details.\n")
    } else {
      model_labels <- names(desired_models)
      if (is.null(model_labels) || length(model_labels) == 0) {
        model_labels <- paste0("Model_", seq_along(desired_models))
      }

      any_success <- FALSE
      failure_detected <- FALSE

      for (idx in seq_along(desired_models)) {
        label <- model_labels[idx]
        model_obj <- desired_models[[idx]]
        cat("Model:", label, "\n")

        components <- parse_model_label(label)
        algo_name <- components$algorithm

        handled <- FALSE
        success <- FALSE

        if (!is.na(algo_name) && algo_name %in% c("survreg", "cox_ph", "penalized_cox", "stratified_cox", "time_varying_cox", "royston_parmar", "parametric_surv", "piecewise_exp", "xgboost")) {
          fit_obj <- extract_survival_fit(label, model_obj)
          if (inherits(fit_obj, "survreg")) {
            success <- isTRUE(print_survreg_details(fit_obj))
            handled <- TRUE
          } else if (inherits(fit_obj, "coxph")) {
            perf_row <- NULL
            if (!is.null(performance_lookup) &&
                nrow(performance_lookup) > 0 &&
                !is.na(algo_name) &&
                "Model" %in% colnames(performance_lookup)) {
              perf_idx <- which(performance_lookup$Model == algo_name)
              if (length(perf_idx) > 0 && "Engine" %in% colnames(performance_lookup)) {
                engine_val <- components$engine
                if (!is.na(engine_val)) {
                  perf_idx <- perf_idx[performance_lookup$Engine[perf_idx] == engine_val]
                }
              }
              if (length(perf_idx) == 0) {
                perf_idx <- which(performance_lookup$Model == algo_name)
              }
              if (length(perf_idx) > 0) {
                perf_row <- performance_lookup[perf_idx[1], , drop = FALSE]
              }
            }
            success <- isTRUE(print_coxph_details(
              fit_obj,
              model_info = model_obj,
              performance_row = perf_row
            ))
            handled <- TRUE
          } else if (inherits(fit_obj, c("stpm2", "pstpm2"))) {
            success <- isTRUE(print_stpm2_details(
              fit_obj,
              model_info = model_obj
            ))
            handled <- TRUE
          } else if (inherits(fit_obj, "flexsurvreg")) {
            success <- isTRUE(print_flexsurv_details(
              fit_obj,
              model_info = model_obj
            ))
            handled <- TRUE
          } else if (inherits(fit_obj, "fastml_xgb_survival")) {
            success <- isTRUE(print_xgb_survival_details(fit_obj))
            handled <- TRUE
          } else if (!is.null(fit_obj)) {
            # Unexpected fit type; treat as handled to avoid duplicate warnings.
            handled <- TRUE
          }
        }

        if (!handled) {
          success <- isTRUE(print_parsnip_params(model_obj))
        }

        if (!success) {
          cat("Could not extract final fitted model details.\n")
          failure_detected <- TRUE
        } else {
          any_success <- TRUE
        }

        cat("\n")
      }

      if (!any_success && failure_detected) {
        # All attempts failed; ensure user sees the generic warning once more.
        cat("Could not extract final fitted model details.\n")
      }
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
    }

  }


  invisible(object)
}
