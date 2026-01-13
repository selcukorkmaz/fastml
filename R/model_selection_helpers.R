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
    # Choose "randomForest" if available; otherwise, take the first non-NA engine.
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
#' @param models A nested list of model workflows. Each element should correspond to an algorithm and contain sublists keyed by engine names.
#' @param best_model_name A named character vector where the names represent algorithm names and the values represent the chosen best engine for each algorithm.
#'
#' @return A named list of workflows corresponding to the best engine for each algorithm. Each list element is named in the format \code{"algorithm (engine)"}.
#'
#' @details The function iterates over each element in \code{best_model_name} and attempts to extract the corresponding workflow from \code{models} using the specified engine. If the workflow for an algorithm-engine pair is not found, a warning is issued and \code{NULL} is returned for that entry.
#'
#' @export
get_best_workflows <- function(models, best_model_name) {
  # For each element in best_model_name, extract the corresponding workflow
  best_list <- lapply(seq_along(best_model_name), function(i) {
    algo   <- names(best_model_name)[i]  # e.g., "rand_forest"
    engine <- best_model_name[i]         # e.g., "ranger" or "randomForest"
    if (!is.null(models[[algo]]) && !is.null(models[[algo]][[engine]])) {
      models[[algo]][[engine]]
    } else {
      warning(paste("No workflow found for", algo, "with engine", engine))
      NULL
    }
  })

  # Name each element in the output using a combined label (e.g., "rand_forest (ranger)")
  names(best_list) <- paste0(names(best_model_name), " (", best_model_name, ")")
  best_list
}

#' Flatten and Rename Models
#'
#' Flattens a nested list of models and renames the elements by combining the outer and inner list names.
#'
#' @param models A nested list of models. The outer list should have names. If an inner element is a named list, the names will be combined with the outer name in the format \code{"outer_name (inner_name)"}.
#'
#' @return A flattened list with each element renamed according to its original outer and inner list names.
#'
#' @details The function iterates over each element of the outer list. For each element, if it is a list with names, the function concatenates the outer list name and the inner names using \code{paste0} and \code{setNames}. If an element is not a list or does not have names, it is included in the result without modification.
#'
#' @importFrom stats setNames
#'
#' @export
flatten_and_rename_models <- function(models) {
  # Initialize an empty list to store the flattened results
  flattened <- list()

  # Loop over each element in the outer list
  for (outer_name in names(models)) {
    inner <- models[[outer_name]]

    # If the inner element is a list with names, then combine names
    if (is.list(inner) && !is.null(names(inner))) {
      # Create new names by combining outer and inner names
      new_names <- paste0(outer_name, " (", names(inner), ")")
      # Set the names and append to the flattened list
      flattened <- c(flattened, setNames(inner, new_names))
    } else {
      # If the outer element is not a list (or not named), keep it as is
      flattened[[outer_name]] <- inner
    }
  }

  return(flattened)
}
#' Get Best Model Indices by Metric and Group
#'
#' Identifies and returns the indices of rows in a data frame where the specified metric reaches the overall maximum within groups defined by one or more columns.
#'
#' @param df A data frame containing model performance metrics and grouping columns.
#' @param metric A character string specifying the name of the metric column in \code{df}. The metric values are converted to numeric for comparison.
#' @param group_cols A character vector of column names used for grouping. Defaults to \code{c("Model", "Engine")}.
#'
#' @return A numeric vector of row indices in \code{df} corresponding to groups whose maximum metric equals the overall best metric value.
#'
#' @details The function converts the metric values to numeric and creates a combined grouping factor using the specified \code{group_cols}. It then computes the maximum metric value within each group and determines the overall best metric value across the entire data frame. Finally, it returns the indices of rows belonging to groups that achieve this overall maximum.
#'
#' @importFrom stats ave
#'
#' @export
#'
get_best_model_idx <- function(df, metric, group_cols = c("Model", "Engine")) {
  # Convert the metric to numeric in case it's not already
  metric_values <- as.numeric(as.character(df[[metric]]))

  # Create a combined grouping factor from the specified columns
  group_values <- interaction(df[, group_cols], drop = TRUE)

  # Compute the maximum metric for each group
  lower_is_better <- metric %in% c("rmse", "mae", "ibs", "logloss", "mse", "brier_score") ||
    grepl("^brier_t", metric) ||
    grepl("loss", metric)
  if(lower_is_better){

    group_val <- ave(metric_values, group_values, FUN = min)
    overall_val <- min(metric_values)


  }else{

    group_val <- ave(metric_values, group_values, FUN = max)
    overall_val <- max(metric_values)


  }


  # Identify groups whose maximum equals the overall maximum
  best_groups <- unique(group_values[group_val == overall_val])

  # Return indices where the group is one of the best groups
  idx <- which(group_values %in% best_groups)
  return(idx)
}

fastml_aggregate_resample_metrics <- function(perf, task) {
  if (is.null(perf) || !is.data.frame(perf) || nrow(perf) == 0) {
    return(NULL)
  }
  if (!".estimate" %in% names(perf)) {
    return(NULL)
  }
  if (!".estimator" %in% names(perf)) {
    perf$.estimator <- NA_character_
  }

  if (identical(task, "survival")) {
    dplyr::summarise(
      dplyr::group_by(perf, .data$.metric),
      .estimate = mean(.data$.estimate, na.rm = TRUE),
      n = sum(is.finite(.data$.estimate)),
      std_dev = stats::sd(.data$.estimate, na.rm = TRUE),
      std_err = ifelse(n > 0, std_dev / sqrt(n), NA_real_),
      .groups = "drop"
    )
  } else {
    dplyr::summarise(
      dplyr::group_by(perf, .data$.metric, .data$.estimator),
      .estimate = mean(.data$.estimate, na.rm = TRUE),
      n = sum(is.finite(.data$.estimate)),
      std_dev = stats::sd(.data$.estimate, na.rm = TRUE),
      std_err = ifelse(n > 0, std_dev / sqrt(n), NA_real_),
      .groups = "drop"
    )
  }
}

fastml_extract_selection_performance <- function(resampling_results,
                                                 nested_results,
                                                 task) {
  selection_perf <- list()
  selection_source <- NULL

  if (!is.null(nested_results) && length(nested_results) > 0) {
    selection_perf <- lapply(nested_results, function(entry) {
      outer_perf <- NULL
      if (is.list(entry) && !is.null(entry$outer_performance)) {
        outer_perf <- entry$outer_performance
      } else if (is.data.frame(entry)) {
        outer_perf <- entry
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
        entry$aggregated
      } else if (is.data.frame(entry)) {
        entry
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
