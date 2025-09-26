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

  coerce_to_dataframe <- function(x) {
    if (is.null(x)) {
      return(NULL)
    }
    if (is.data.frame(x)) {
      return(x)
    }
    if (inherits(x, "tbl_df")) {
      return(as.data.frame(x))
    }
    if (is.matrix(x)) {
      return(as.data.frame(x))
    }
    if (is.list(x) && !is.null(x$time) && !is.null(x$survival)) {
      df <- data.frame(time = x$time, survival = x$survival)
      other_cols <- setdiff(names(x), c("time", "survival"))
      for (nm in other_cols) {
        if (is.atomic(x[[nm]]) && length(x[[nm]]) == nrow(df)) {
          df[[nm]] <- x[[nm]]
        }
      }
      return(df)
    }
    NULL
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
      df <- coerce_to_dataframe(df)
      if (!is.null(df) && nrow(df) > 0) {
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
      if (!is.null(df) && !is.data.frame(df)) {
        df <- coerce_to_dataframe(df)
      }
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
      if (is.list(row_summary) && length(row_summary) >= 1 && !is.data.frame(row_summary)) {
        row_summary <- row_summary[[1]]
      }
      row_summary <- coerce_to_dataframe(row_summary)
      if (is.null(row_summary)) {
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

  summary_tidy <- tryCatch(
    flexsurv::summary(
      fit,
      type = "survival",
      t = times,
      newdata = newdata,
      ci = FALSE,
      tidy = TRUE
    ),
    error = function(e) NULL
  )

  convert_tidy_summary <- function(df, times, n_obs) {
    if (!is.data.frame(df) || !(".row" %in% names(df))) {
      return(NULL)
    }
    time_col <- intersect(c("time", "t", ".eval_time"), names(df))
    surv_col <- intersect(c("est", "survival", "S", ".pred_survival", ".pred"), names(df))
    if (length(time_col) == 0 || length(surv_col) == 0) {
      return(NULL)
    }
    df$.row <- as.integer(df$.row)
    df <- df[is.finite(df$.row) & df$.row >= 1, , drop = FALSE]
    if (nrow(df) == 0) {
      return(NULL)
    }
    res <- matrix(NA_real_, nrow = n_obs, ncol = length(times))
    split_map <- split(df, df$.row)
    max_iter <- min(length(split_map), n_obs)
    for (i in seq_len(max_iter)) {
      entry <- split_map[[i]]
      res[i, ] <- align_curve(entry[[time_col[1]]], entry[[surv_col[1]]], times)
    }
    res
  }

  res <- convert_tidy_summary(summary_tidy, times, n_obs)

  if (is.null(res) || !is.matrix(res)) {
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
      has_finite <- is.matrix(res) && any(rowSums(is.finite(res)) > 0)
      if (!has_finite) {
        res <- compute_rowwise(newdata, times)
      }
    } else {
      res <- compute_rowwise(newdata, times)
    }
  }

  expected_dim <- c(n_obs, length(times))
  if (!is.matrix(res)) {
    res <- matrix(as.numeric(res), nrow = expected_dim[1], ncol = expected_dim[2])
  }

  if (!all(dim(res) == expected_dim)) {
    res <- compute_rowwise(newdata, times)
    if (!is.matrix(res)) {
      res <- matrix(as.numeric(res), nrow = expected_dim[1], ncol = expected_dim[2])
    }
  }

  if (!is.matrix(res) || !all(dim(res) == expected_dim)) {
    res <- matrix(NA_real_, nrow = expected_dim[1], ncol = expected_dim[2])
  }

  if (length(times) > 0) {
    res[is.nan(res)] <- NA_real_
    res <- pmin(pmax(res, 0), 1)
  }
  res
}

fastml_parametric_surv_predict <- function(fit,
                                           newdata,
                                           eval_times,
                                           risk_time = NULL) {
  n_obs <- if (!is.null(newdata)) nrow(newdata) else 0L

  if (!inherits(fit, "flexsurvreg") ||
      !requireNamespace("flexsurv", quietly = TRUE)) {
    empty_surv <- if (length(eval_times) > 0) {
      matrix(NA_real_, nrow = n_obs, ncol = length(eval_times))
    } else {
      matrix(numeric(), nrow = n_obs, ncol = 0)
    }
    return(list(surv = empty_surv, risk = rep(NA_real_, n_obs)))
  }

  eval_times <- as.numeric(eval_times)
  eval_times <- sort(unique(eval_times[is.finite(eval_times) & eval_times >= 0]))

  if (is.null(newdata)) {
    newdata <- data.frame(matrix(nrow = 0, ncol = 0))
  }

  newdata <- as.data.frame(newdata)
  n_obs <- nrow(newdata)

  if (n_obs == 0) {
    surv_mat <- if (length(eval_times) > 0) {
      matrix(numeric(), nrow = 0, ncol = length(eval_times))
    } else {
      matrix(numeric(), nrow = 0, ncol = 0)
    }
    attr(surv_mat, "eval_times") <- eval_times
    return(list(surv = surv_mat, risk = numeric(0)))
  }

  if (length(eval_times) == 0) {
    surv_mat <- matrix(NA_real_, nrow = n_obs, ncol = 0)
    attr(surv_mat, "eval_times") <- eval_times
    return(list(surv = surv_mat, risk = rep(NA_real_, n_obs)))
  }

  surv_mat <- fastml_flexsurv_survival_matrix(fit, newdata, eval_times)
  if (is.null(surv_mat) || !is.matrix(surv_mat) ||
      nrow(surv_mat) != n_obs || ncol(surv_mat) != length(eval_times)) {
    surv_mat <- matrix(NA_real_, nrow = n_obs, ncol = length(eval_times))
  }

  if (ncol(surv_mat) == length(eval_times)) {
    colnames(surv_mat) <- format(eval_times, trim = TRUE, scientific = FALSE)
  }
  attr(surv_mat, "eval_times") <- eval_times

  risk_vec <- rep(NA_real_, n_obs)
  valid_cols <- which(colSums(is.finite(surv_mat)) > 0)
  if (length(valid_cols) > 0) {
    if (is.null(risk_time) || !is.finite(risk_time) || risk_time <= 0) {
      med_time <- stats::median(eval_times)
      risk_time <- if (is.finite(med_time) && med_time > 0) med_time else max(eval_times)
    }
    if (!is.finite(risk_time) || risk_time <= 0) {
      risk_time <- eval_times[valid_cols[length(valid_cols)]]
    }
    idx <- which.min(abs(eval_times - risk_time))
    idx <- max(1L, min(idx, ncol(surv_mat)))
    surv_vals <- as.numeric(surv_mat[, idx])
    surv_vals[!is.finite(surv_vals)] <- NA_real_
    risk_vec <- -log(pmax(surv_vals, .Machine$double.eps))
    risk_vec[!is.finite(risk_vec)] <- NA_real_
  }

  list(surv = surv_mat, risk = risk_vec)
}
