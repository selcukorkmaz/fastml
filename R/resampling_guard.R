#' Guarded Resampling Utilities
#'
#' Internal helpers that enforce the Guarded Resampling Principle by
#' fitting preprocessing pipelines independently within each resampling
#' split. These functions are not exported.
#' @param split An `rsample` split object representing a single resample.
#' @param total_rows Integer; total number of rows in the original dataset.
#'
#' @importFrom dplyr bind_rows group_by summarise
#' @importFrom rsample analysis assessment
fastml_guard_validate_indices <- function(indices, label) {
  if (!is.atomic(indices) || length(dim(indices)) > 0) {
    stop(sprintf("Guarded resampling requires '%s' to be a vector of indices.", label))
  }
  if (!is.numeric(indices)) {
    stop(sprintf("Guarded resampling requires '%s' to be numeric indices.", label))
  }
  if (length(indices) == 0) {
    stop(sprintf("Guarded resampling requires '%s' to contain at least one index.", label))
  }
  if (anyNA(indices)) {
    stop(sprintf("Guarded resampling requires '%s' to contain no missing values.", label))
  }
  if (any(indices < 1)) {
    stop(sprintf("Guarded resampling requires '%s' to contain positive indices.", label))
  }
  if (any(indices != floor(indices))) {
    stop(sprintf("Guarded resampling requires '%s' to contain integer indices.", label))
  }
  as.integer(indices)
}

fastml_guard_index_candidates <- function() {
  c("in_id", "analysis_id", "analysis_ids", "analysis_index", "analysis_indices", "analysis")
}

fastml_guard_split_index_candidates <- function(split) {
  candidates <- fastml_guard_index_candidates()
  valid <- character(0)
  for (candidate in candidates) {
    value <- split[[candidate]]
    if (is.null(value)) {
      next
    }
    if (!is.atomic(value) || length(dim(value)) > 0 || !is.numeric(value)) {
      next
    }
    ok <- tryCatch({
      fastml_guard_validate_indices(value, candidate)
      TRUE
    }, error = function(e) FALSE)
    if (ok) {
      valid <- c(valid, candidate)
    }
  }
  valid
}

fastml_guard_select_index_name <- function(splits) {
  candidates <- fastml_guard_index_candidates()
  valid_names <- lapply(splits, fastml_guard_split_index_candidates)
  has_valid <- vapply(valid_names, length, integer(1)) > 0
  if (!all(has_valid)) {
    stop(
      "Guarded resampling requires split index information (e.g., `in_id`) for every resample."
    )
  }
  common <- Reduce(intersect, valid_names)
  if (length(common) == 0) {
    stop(
      "Guarded resampling requires a consistent index field (e.g., `in_id`) across resamples."
    )
  }
  for (candidate in candidates) {
    if (candidate %in% common) {
      return(candidate)
    }
  }
  common[[1]]
}

fastml_guard_extract_in_id <- function(split, index_name = NULL) {
  if (!is.null(index_name)) {
    value <- split[[index_name]]
    if (is.null(value)) {
      stop(
        sprintf(
          "Guarded resampling requires '%s' index information for every resample.",
          index_name
        )
      )
    }
    return(fastml_guard_validate_indices(value, index_name))
  }
  candidates <- fastml_guard_index_candidates()
  for (candidate in candidates) {
    value <- split[[candidate]]
    if (is.null(value)) {
      next
    }
    if (!is.atomic(value) || length(dim(value)) > 0 || !is.numeric(value)) {
      next
    }
    return(fastml_guard_validate_indices(value, candidate))
  }
  NULL
}

fastml_guard_is_bootstrap_split <- function(split) {
  inherits(split, "boot_split")
}

fastml_guard_is_bootstrap_resamples <- function(resamples, plan = NULL) {
  if (!is.null(plan)) {
    method <- fastml_resample_method(plan)
    if (!is.null(method)) {
      method <- tolower(as.character(method))
      if (!is.na(method) && method %in% c("boot", "bootstrap", "bootstraps")) {
        return(TRUE)
      }
    }
  }
  if (!is.null(resamples) && inherits(resamples, "bootstraps")) {
    return(TRUE)
  }
  FALSE
}

fastml_guard_detect_full_analysis <- function(in_id, total_rows, is_bootstrap = FALSE) {
  if (is_bootstrap) {
    return(FALSE)
  }
  if (length(in_id) < total_rows) {
    return(FALSE)
  }
  sorted_unique <- sort(unique(in_id))
  if (length(sorted_unique) != total_rows) {
    return(FALSE)
  }
  all(sorted_unique == seq_len(total_rows))
}

fastml_guarded_resample_fit <- function(workflow_spec,
                                        resamples,
                                        original_train_rows,
                                        task,
                                        label,
                                        metric,
                                        event_class,
                                        engine,
                                        start_col = NULL,
                                        time_col = NULL,
                                        status_col = NULL,
                                        eval_times = NULL,
                                        at_risk_threshold = 0.1,
                                        engine_args = list(),
                                        summaryFunction = NULL,
                                        multiclass_auc = "macro") {
  plan <- NULL
  if (fastml_is_resample_plan(resamples)) {
    plan <- fastml_resample_validate(resamples)
    resamples <- fastml_resample_splits(plan)
  }

  if (!inherits(resamples, "rset")) {
    stop("'resamples' must be an 'rset' object for guarded resampling.")
  }

  splits <- resamples$splits
  if (length(splits) == 0) {
    return(NULL)
  }

  index_name <- fastml_guard_select_index_name(splits)
  bootstrap_resamples <- fastml_guard_is_bootstrap_resamples(resamples, plan)

  fold_metrics <- vector("list", length(splits))

  for (i in seq_along(splits)) {
    split <- splits[[i]]

    in_id <- fastml_guard_extract_in_id(split, index_name = index_name)
    is_bootstrap_split <- bootstrap_resamples || fastml_guard_is_bootstrap_split(split)

    if (fastml_guard_detect_full_analysis(in_id, original_train_rows, is_bootstrap_split)) {
      stop(
        paste(
          "Detected preprocessing on the full training set during resampling.",
          "Each fold must train preprocessing exclusively on its analysis subset."
        )
      )
    }

    analysis_data <- rsample::analysis(split)
    assessment_data <- rsample::assessment(split)

    fit_args <- list(object = workflow_spec, data = analysis_data)
    if (!is.null(engine_args) && length(engine_args) > 0) {
      fold_fit <- call_with_engine_params(parsnip::fit, fit_args, engine_args)
    } else {
      fold_fit <- parsnip::fit(workflow_spec, data = analysis_data)
    }

    fold_result <- process_model(
      model_obj = fold_fit,
      model_id = paste0("fold_", i),
      task = task,
      test_data = assessment_data,
      label = label,
      event_class = event_class,
      start_col = start_col,
      time_col = time_col,
      status_col = status_col,
      engine = engine,
      train_data = analysis_data,
      metric = metric,
      eval_times_user = eval_times,
      bootstrap_ci = FALSE,
      bootstrap_samples = 0,
      bootstrap_seed = NULL,
      at_risk_threshold = at_risk_threshold,
      summaryFunction = summaryFunction,
      multiclass_auc = multiclass_auc
    )

    fold_metrics[[i]] <- fold_result$performance

    rm(fold_fit)
    rm(analysis_data)
    rm(assessment_data)
    gc(verbose = FALSE)
  }

  fold_metrics_df <- dplyr::bind_rows(fold_metrics, .id = "fold")
  ci_cols <- c(".lower", ".upper", ".n_boot")
  keep_cols <- setdiff(names(fold_metrics_df), ci_cols)
  fold_metrics_df <- fold_metrics_df[, keep_cols, drop = FALSE]

  if (identical(task, "survival")) {
    fold_metrics_df <- fold_metrics_df[, setdiff(names(fold_metrics_df), ".estimator"), drop = FALSE]
    aggregated <- fold_metrics_df %>%
      dplyr::group_by(.data$.metric) %>%
      dplyr::summarise(
        .estimate = mean(.data$.estimate, na.rm = TRUE),
        n = sum(is.finite(.data$.estimate)),
        std_dev = stats::sd(.data$.estimate, na.rm = TRUE),
        std_err = ifelse(n > 0, std_dev / sqrt(n), NA_real_),
        .groups = "drop"
      )
  } else {
    if (!".estimator" %in% names(fold_metrics_df)) {
      fold_metrics_df$.estimator <- NA_character_
    }
    aggregated <- fold_metrics_df %>%
      dplyr::group_by(.data$.metric, .data$.estimator) %>%
      dplyr::summarise(
        .estimate = mean(.data$.estimate, na.rm = TRUE),
        n = sum(is.finite(.data$.estimate)),
        std_dev = stats::sd(.data$.estimate, na.rm = TRUE),
        std_err = ifelse(n > 0, std_dev / sqrt(n), NA_real_),
        .groups = "drop"
      )
  }

  result <- list(
    aggregated = aggregated,
    folds = fold_metrics_df
  )

  if (!is.null(plan)) {
    result$metadata <- fastml_resample_metadata(plan)
  }

  result
}
