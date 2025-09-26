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

  extract_curves <- function(summary_list, times, n_obs) {
    if (inherits(summary_list, "data.frame")) {
      row_cols <- intersect(c(".row", "row", "obs", "case", "id", "ID", "index"),
                            names(summary_list))
      if (length(row_cols) > 0) {
        split_col <- summary_list[[row_cols[1]]]
        if (!is.null(split_col)) {
          split_keys <- unique(split_col)
          split_map <- split(summary_list, split_col)
          summary_list <- lapply(seq_along(split_keys), function(idx) {
            key_chr <- as.character(split_keys[idx])
            if (!is.null(names(split_map)) && key_chr %in% names(split_map)) {
              df <- split_map[[key_chr]]
            } else {
              df <- split_map[[1]]
            }
            drop_cols <- intersect(row_cols, names(df))
            if (length(drop_cols) > 0) {
              df <- df[, setdiff(names(df), drop_cols), drop = FALSE]
            }
            df
          })
          summary_list <- unname(summary_list)
        }
      }
    }

    if (!is.list(summary_list)) {
      summary_list <- list(summary_list)
    }

    summary_list <- lapply(summary_list, function(df) {
      if (is.data.frame(df)) {
        drop_cols <- intersect(c(".row", "row", "obs", "case", "id", "ID", "index"), names(df))
        if (length(drop_cols) > 0) {
          df <- df[, setdiff(names(df), drop_cols), drop = FALSE]
        }
      }
      df
    })

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

  compute_rowwise <- function(newdata, times) {
    res <- matrix(NA_real_, nrow = nrow(newdata), ncol = length(times))
    if (nrow(newdata) == 0 || length(times) == 0) {
      return(res)
    }
    for (i in seq_len(nrow(newdata))) {
      row_df <- newdata[i, , drop = FALSE]
      row_summary <- tryCatch(
        flexsurv::summary(
          fit,
          type = "survival",
          t = times,
          newdata = row_df,
          ci = FALSE
        ),
        error = function(e) NULL
      )
      if (is.null(row_summary)) {
        next
      }
      if (is.list(row_summary) && length(row_summary) >= 1) {
        row_summary <- row_summary[[1]]
      }
      if (!is.data.frame(row_summary)) {
        next
      }
      time_col <- intersect(c("time", "t", ".eval_time"), names(row_summary))
      surv_col <- intersect(c("est", "survival", "S", ".pred_survival", ".pred"), names(row_summary))
      if (length(time_col) == 0 || length(surv_col) == 0) {
        next
      }
      res[i, ] <- align_curve(row_summary[[time_col[1]]],
                              row_summary[[surv_col[1]]],
                              times)
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

  if (!is.null(summary_list)) {
    res <- extract_curves(summary_list, times, n_obs)
    has_finite <- any(rowSums(is.finite(res)) > 0)
    if (!has_finite) {
      res <- compute_rowwise(newdata, times)
    }
  } else {
    res <- compute_rowwise(newdata, times)
  }

  if (!all(dim(res) == c(n_obs, length(times)))) {
    res <- compute_rowwise(newdata, times)
  }

  res <- pmin(pmax(res, 0), 1)
  res
}
