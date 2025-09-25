#' Helper functions for flexsurv-based survival models
#'
#' These utilities are internal to fastml and support training-time summaries
#' and prediction helpers for models fitted with flexsurv::flexsurvreg().
#'
#' @noRd
NULL

fastml_flexsurv_survival_matrix <- function(fit, newdata, times) {
  if (!inherits(fit, "flexsurvreg")) {
    return(NULL)
  }
  if (!requireNamespace("flexsurv", quietly = TRUE)) {
    return(NULL)
  }

  times <- as.numeric(times)
  times <- times[is.finite(times) & times >= 0]
  if (length(times) == 0) {
    n_rows <- if (!is.null(newdata)) nrow(newdata) else 0L
    return(matrix(numeric(0), nrow = n_rows, ncol = 0))
  }

  if (is.null(newdata)) {
    return(matrix(numeric(), nrow = 0, ncol = length(times)))
  }

  newdata <- as.data.frame(newdata)
  n_obs <- nrow(newdata)
  if (n_obs == 0) {
    return(matrix(numeric(), nrow = 0, ncol = length(times)))
  }

  align_curve <- function(curve_times, curve_surv, eval_times) {
    if (length(eval_times) == 0) {
      return(numeric(0))
    }
    if (length(curve_times) == 0 || length(curve_surv) == 0) {
      return(rep(NA_real_, length(eval_times)))
    }
    curve_times <- as.numeric(curve_times)
    curve_surv <- as.numeric(curve_surv)
    ord <- order(curve_times)
    curve_times <- curve_times[ord]
    curve_surv <- curve_surv[ord]
    idx <- findInterval(eval_times, curve_times)
    res <- rep(NA_real_, length(eval_times))
    if (any(idx == 0)) {
      res[idx == 0] <- 1
    }
    pos_idx <- which(idx > 0)
    if (length(pos_idx) > 0) {
      mapped <- pmin(idx[pos_idx], length(curve_surv))
      res[pos_idx] <- curve_surv[mapped]
    }
    if (any(idx > length(curve_surv))) {
      last_val <- curve_surv[length(curve_surv)]
      res[idx > length(curve_surv)] <- last_val
    }
    res <- pmin(pmax(res, 0), 1)
    res
  }

  summary_list <- tryCatch(
    flexsurv::summary(
      fit,
      type = "survival",
      t = times,
      newdata = newdata,
      ci = FALSE
    ),
    error = function(e) NULL
  )

  if (is.null(summary_list)) {
    return(NULL)
  }

  if (!is.list(summary_list)) {
    summary_list <- list(summary_list)
  }

  if (length(summary_list) == 1L && n_obs > 1L) {
    summary_list <- rep(summary_list, length.out = n_obs)
  }

  res <- matrix(NA_real_, nrow = n_obs, ncol = length(times))

  max_iter <- min(length(summary_list), n_obs)
  for (i in seq_len(max_iter)) {
    df <- summary_list[[i]]
    if (is.null(df)) {
      next
    }
    time_col <- intersect(c("time", "t", ".eval_time"), names(df))
    surv_col <- intersect(c("est", "survival", "S", ".pred_survival", ".pred"), names(df))
    if (length(time_col) == 0 || length(surv_col) == 0) {
      next
    }
    curve_times <- df[[time_col[1]]]
    curve_surv <- df[[surv_col[1]]]
    res[i, ] <- align_curve(curve_times, curve_surv, times)
  }

  if (n_obs > length(summary_list) && length(summary_list) >= 1) {
    filled <- res[seq_len(max_iter), , drop = FALSE]
    valid_row <- which(rowSums(!is.na(filled)) > 0)
    if (length(valid_row) >= 1) {
      template <- filled[valid_row[1], , drop = TRUE]
      for (i in seq((max_iter + 1), n_obs)) {
        res[i, ] <- template
      }
    }
  }

  res <- pmin(pmax(res, 0), 1)
  res
}
