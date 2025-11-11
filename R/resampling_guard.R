#' Guarded Resampling Utilities
#'
#' Internal helpers that enforce the Guarded Resampling Principle by
#' fitting preprocessing pipelines independently within each resampling
#' split. These functions are not exported.
#'
#' @importFrom dplyr bind_rows group_by summarise
#' @importFrom rsample analysis assessment
fastml_guard_detect_full_analysis <- function(split, total_rows) {
  in_id <- split$in_id
  if (is.null(in_id)) {
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
                                        at_risk_threshold = 0.1) {
  if (!inherits(resamples, "rset")) {
    stop("'resamples' must be an 'rset' object for guarded resampling.")
  }

  splits <- resamples$splits
  if (length(splits) == 0) {
    return(NULL)
  }

  fold_metrics <- vector("list", length(splits))

  for (i in seq_along(splits)) {
    split <- splits[[i]]

    if (fastml_guard_detect_full_analysis(split, original_train_rows)) {
      stop(
        paste(
          "Detected preprocessing on the full training set during resampling.",
          "Each fold must train preprocessing exclusively on its analysis subset."
        )
      )
    }

    analysis_data <- rsample::analysis(split)
    assessment_data <- rsample::assessment(split)

    fold_fit <- parsnip::fit(workflow_spec, data = analysis_data)

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
      at_risk_threshold = at_risk_threshold
    )

    fold_metrics[[i]] <- fold_result$performance

    rm(fold_fit)
    rm(analysis_data)
    rm(assessment_data)
    gc(verbose = FALSE)
  }

  fold_metrics_df <- dplyr::bind_rows(fold_metrics, .id = "fold")

  aggregated <- fold_metrics_df %>%
    dplyr::group_by(.metric, .estimator) %>%
    dplyr::summarise(.estimate = mean(.estimate, na.rm = TRUE), .groups = "drop")

  list(
    aggregated = aggregated,
    folds = fold_metrics_df
  )
}
