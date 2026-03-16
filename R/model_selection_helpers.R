#' Get Best Model Names
#'
#' Extracts and returns the best engine names from a named list of model workflows.
#'
#' @param models A named list where each element corresponds to an algorithm and contains a list of model workflows.
#'   Each workflow should be compatible with \code{tune::extract_fit_parsnip}.
#'
#' @return A named character vector. The names of the vector correspond to the algorithm names, and the values represent the chosen best engine name for that algorithm.
#'
#' @details For each algorithm, the function extracts the engine names from the model workflows using \code{tune::extract_fit_parsnip}.
#'   It then chooses \code{"randomForest"} if it is available; otherwise, it selects the first non-\code{NA} engine.
#'   If no engine names can be extracted for an algorithm, \code{NA_character_} is returned.
#'
#' @importFrom tune extract_fit_parsnip
#'
#' @export
get_best_model_names <- function(models) {
  bests <- sapply(names(models), function(algo) {
    model_list <- models[[algo]]
    engines <- sapply(model_list, function(mod) {
      fit_obj <- tryCatch(
        extract_fit_parsnip(mod),
        error = function(e) NULL
      )
      if (!is.null(fit_obj)) {
        fit_obj$spec$engine
      } else {
        NA_character_
      }
    })
    if ("randomForest" %in% engines) {
      "randomForest"
    } else {
      non_na_engines <- engines[!is.na(engines)]
      if (length(non_na_engines) > 0) non_na_engines[1] else NA_character_
    }
  })
  bests
}

#' Get Best Workflows
#'
#' Extracts the best workflows from a nested list of model workflows based on the provided best model names.
#'
#' @param models A nested list of model workflows.
#' @param best_model_name A named character vector of chosen best engines.
#'
#' @export
get_best_workflows <- function(models, best_model_name) {
  best_list <- lapply(seq_along(best_model_name), function(i) {
    algo   <- names(best_model_name)[i]
    engine <- best_model_name[i]
    if (!is.null(models[[algo]]) && !is.null(models[[algo]][[engine]])) {
      models[[algo]][[engine]]
    } else {
      warning(paste("No workflow found for", algo, "with engine", engine))
      NULL
    }
  })
  names(best_list) <- paste0(names(best_model_name), " (", best_model_name, ")")
  best_list
}

#' Flatten and Rename Models
#'
#' @importFrom stats setNames
#' @export
flatten_and_rename_models <- function(models) {
  flattened <- list()
  for (outer_name in names(models)) {
    inner <- models[[outer_name]]
    if (is.list(inner) && !is.null(names(inner))) {
      new_names <- paste0(outer_name, " (", names(inner), ")")
      flattened <- c(flattened, setNames(inner, new_names))
    } else {
      flattened[[outer_name]] <- inner
    }
  }
  flattened
}

#' Get Best Model Indices by Metric and Group
#'
#' @importFrom stats ave
#' @export
get_best_model_idx <- function(df, metric, group_cols = c("Model", "Engine")) {
  metric_values <- as.numeric(as.character(df[[metric]]))
  group_values <- interaction(df[, group_cols], drop = TRUE)

  lower_is_better <- metric %in% c("rmse", "mae", "ibs", "logloss", "mse",
                                   "brier_score", "ece") ||
    grepl("^brier_t", metric) ||
    grepl("loss", metric)

  if (lower_is_better) {
    group_val <- ave(metric_values, group_values, FUN = min)
    overall_val <- min(metric_values)
  } else {
    group_val <- ave(metric_values, group_values, FUN = max)
    overall_val <- max(metric_values)
  }

  best_groups <- unique(group_values[group_val == overall_val])
  which(group_values %in% best_groups)
}

fastml_aggregate_resample_metrics <- function(perf, task) {
  if (is.null(perf) || !is.data.frame(perf) || nrow(perf) == 0) {
    return(NULL)
  }

  perf <- tibble::as_tibble(as.data.frame(perf))

  if (!".estimate" %in% names(perf)) {
    return(NULL)
  }

  perf$.estimate <- suppressWarnings(as.numeric(perf$.estimate))
  if (!".estimator" %in% names(perf)) {
    perf$.estimator <- NA_character_
  }

  group_cols <- ".metric"
  if (!identical(task, "survival") && any(!is.na(perf$.estimator))) {
    group_cols <- c(group_cols, ".estimator")
  }

  aggregated <- perf %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarise(
      mean_est = mean(.estimate, na.rm = TRUE),
      n_valid  = sum(is.finite(.estimate)),
      sd_est   = stats::sd(.estimate, na.rm = TRUE),
      .groups  = "drop"
    ) %>%
    dplyr::mutate(
      std_err = ifelse(n_valid > 0, sd_est / sqrt(n_valid), NA_real_)
    ) %>%
    dplyr::rename(
      .estimate = mean_est,
      n = n_valid,
      std_dev = sd_est
    )

  if (!".estimator" %in% names(aggregated)) {
    aggregated$.estimator <- NA_character_
  }

  aggregated
}

fastml_extract_selection_performance <- function(resampling_results,
                                                 nested_results,
                                                 task) {
  selection_perf <- list()
  selection_source <- NULL

  if (!is.null(nested_results) && length(nested_results) > 0) {
    selection_perf <- lapply(nested_results, function(entry) {
      outer_perf <- if (is.list(entry) && !is.null(entry$outer_performance)) {
        entry$outer_performance
      } else if (is.data.frame(entry)) {
        entry
      } else {
        NULL
      }
      fastml_aggregate_resample_metrics(outer_perf, task)
    })
    selection_perf <- selection_perf[!vapply(selection_perf, is.null, logical(1))]
    if (length(selection_perf) > 0) {
      selection_source <- "nested_cv"
    }
  }

  if (is.null(selection_source) &&
      !is.null(resampling_results) &&
      length(resampling_results) > 0) {
    selection_perf <- lapply(resampling_results, function(entry) {
      if (is.list(entry) && !inherits(entry, "data.frame")) {
        folds_tbl <- entry$folds
        if (!is.null(folds_tbl) && is.data.frame(folds_tbl) && nrow(folds_tbl) > 0) {
          return(fastml_aggregate_resample_metrics(folds_tbl, task))
        }
        NULL
      } else if (is.data.frame(entry)) {
        fastml_aggregate_resample_metrics(entry, task)
      } else {
        NULL
      }
    })
    selection_perf <- selection_perf[!vapply(selection_perf, is.null, logical(1))]
    if (length(selection_perf) > 0) {
      selection_source <- "resampling"
    }
  }

  list(performance = selection_perf, source = selection_source)
}
