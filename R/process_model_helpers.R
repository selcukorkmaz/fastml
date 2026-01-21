#' Determine rounding digits for time horizons
#'
#' Computes a sensible number of decimal digits to round time horizons based on the minimal
#' positive separation between unique finite times.
#'
#' @param times Numeric vector of times.
#'
#' @return Integer number of digits between 0 and 6.
#'
#' @details Uses the smallest strictly positive difference among sorted unique finite times,
#' then returns \code{ceiling(-log10(min_diff))} truncated to \eqn{[0, 6]}.
#'
#' @keywords internal
#' @examples
#' # Not run: determine_round_digits(c(0.1, 0.12, 0.125))
#' NULL
#'
#' @name determine_round_digits
#' @rdname determine_round_digits
#' @noMd
#'
determine_round_digits <- function(times) {
  times <- times[is.finite(times) & times > 0]
  if (length(times) <= 1) {
    return(0)
  }
  diffs <- diff(sort(unique(times)))
  diffs <- diffs[diffs > 0]
  if (length(diffs) == 0) {
    return(0)
  }
  min_diff <- min(diffs)
  if (!is.finite(min_diff) || min_diff <= 0) {
    return(0)
  }
  digits <- ceiling(-log10(min_diff))
  digits <- max(0, digits)
  digits <- min(digits, 6)
  digits
}

#' Compute Tau Limit (t_max)
#'
#' Finds the latest time point \eqn{t_{max}} such that at least a certain proportion
#' of subjects remain at risk.
#'
#' @param times Numeric vector of survival times.
#' @param threshold Minimum proportion of subjects that must remain at risk.
#'
#' @return The computed \eqn{t_{max}} value, or \code{NA_real_} if no valid
#'   times are provided.
#' @keywords internal
#' @noMd
compute_tau_limit <- function(times, threshold) {
  times_valid <- sort(unique(times[is.finite(times) & times > 0]))
  if (length(times_valid) == 0) {
    return(NA_real_)
  }
  n <- length(times)
  props <- vapply(times_valid, function(t) {
    mean(times >= t, na.rm = TRUE)
  }, numeric(1))
  idx <- which(props >= threshold)
  if (length(idx) == 0) {
    max(times_valid)
  } else {
    max(times_valid[idx])
  }
}

#' Create Censoring Distribution Evaluator
#'
#' Creates a function to evaluate the survival function of the censoring
#' distribution, \eqn{G(t) = P(C > t)}, using a Kaplan-Meier estimator.
#'
#' @param time_vec Numeric vector of survival/censoring times.
#' @param status_vec Numeric vector of event statuses (1=event, 0=censored).
#'
#' @return A function that takes a numeric vector of times and returns the
#'   estimated censoring survival probabilities \eqn{G(t)} at those times.
#'
#' @importFrom survival survfit Surv
#' @importFrom utils head
#' @keywords internal
#' @noMd
create_censor_eval <- function(time_vec, status_vec) {
  time_vec <- as.numeric(time_vec)
  status_vec <- ifelse(is.na(status_vec), 0, ifelse(status_vec > 0, 1, 0))
  valid_idx <- which(is.finite(time_vec) & time_vec >= 0)
  if (length(valid_idx) == 0) {
    return(function(times)
      rep(NA_real_, length(times)))
  }
  censor_indicator <- 1 - status_vec[valid_idx]
  fit <- tryCatch({
    survival::survfit(survival::Surv(time_vec[valid_idx], censor_indicator) ~ 1)
  }, error = function(e)
    NULL)
  if (is.null(fit)) {
    return(function(times)
      rep(NA_real_, length(times)))
  }
  function(times) {
    if (length(times) == 0)
      return(numeric(0))
    if (length(fit$time) == 0 || length(fit$surv) == 0) {
      return(rep(1, length(times)))
    }
    align_survival_curve(fit$time, fit$surv, times)
  }
}

#' Assign Risk Groups
#'
#' Dichotomizes a continuous risk vector into "low" and "high" risk groups
#' based on the median.
#'
#' @param risk_vec Numeric vector of predicted risk scores.
#'
#' @return A character vector of "low", "high", or \code{NA}.
#'
#' @importFrom stats median
#' @keywords internal
assign_risk_group <- function(risk_vec) {
  group <- rep(NA_character_, length(risk_vec))
  valid <- is.finite(risk_vec)
  if (sum(valid) == 0) {
    return(group)
  }
  risk_vals <- risk_vec[valid]
  if (length(unique(risk_vals)) > 1) {
    threshold <- stats::median(risk_vals)
    group[valid] <- ifelse(risk_vals > threshold, "high", "low")
  } else {
    group[valid] <- "low"
  }
  group
}

#' Compute Uno's C-index (Time-Dependent AUC)
#'
#' Calculates Uno's C-index (a time-dependent AUC measure) for survival data,
#' weighted by the inverse probability of censoring (IPCW).
#'
#' @param train_time Numeric vector of training times (used for censor model).
#' @param train_status Numeric vector of training statuses (used for censor model).
#' @param test_time Numeric vector of test times.
#' @param test_status Numeric vector of test statuses.
#' @param risk_vec Numeric vector of predicted risk scores for test data.
#' @param tau The time horizon \eqn{\tau} for evaluation. If \code{NA} or
#'   \code{<= 0}, the maximum finite test time is used.
#' @param censor_eval_fn A function (from \code{create_censor_eval}) that
#'   evaluates the censoring survival function \eqn{G(t)}.
#'
#' @return The computed Uno's C-index, or \code{NA_real_} on failure.
#' @keywords internal
#' @noMd
compute_uno_c_index <- function(train_time,
                                train_status,
                                test_time,
                                test_status,
                                risk_vec,
                                tau,
                                censor_eval_fn) {
  if (!is.function(censor_eval_fn)) {
    return(NA_real_)
  }
  if (!is.finite(tau) || tau <= 0) {
    tau <- suppressWarnings(max(test_time[is.finite(test_time)], na.rm = TRUE))
  }
  valid <- which(is.finite(test_time) & is.finite(risk_vec))
  if (length(valid) <= 1) {
    return(NA_real_)
  }
  time_val <- test_time[valid]
  status_val <- ifelse(is.na(test_status[valid]), 0, ifelse(test_status[valid] > 0, 1, 0))
  risk_val <- risk_vec[valid]
  G_vec <- censor_eval_fn(time_val)
  if (length(G_vec) == 0) {
    return(NA_real_)
  }
  G_vec[!is.finite(G_vec) | G_vec <= 0] <- NA_real_
  event_idx <- which(status_val == 1 &
                       is.finite(G_vec) & time_val <= tau)
  if (length(event_idx) == 0) {
    return(NA_real_)
  }
  numerator <- 0
  denominator <- 0
  for (ii in event_idx) {
    w_i <- 1 / (G_vec[ii]^2)
    if (!is.finite(w_i) || w_i <= 0)
      next
    later <- which(time_val > time_val[ii] &
                     time_val <= tau & is.finite(G_vec))
    later <- later[G_vec[later] > 0]
    if (length(later) == 0)
      next
    denominator <- denominator + w_i * length(later)
    diff_scores <- risk_val[ii] - risk_val[later]
    concordant <- sum(diff_scores > 0) + 0.5 * sum(diff_scores == 0)
    numerator <- numerator + w_i * concordant
  }
  if (denominator <= 0) {
    return(NA_real_)
  }
  val <- numerator / denominator
  val <- min(max(val, 0), 1)
  val
}

#' Compute Difference in Restricted Mean Survival Time (RMST)
#'
#' Calculates the difference in RMST between "low" and "high" risk groups up to
#' a time horizon \eqn{\tau}. Groups are defined by median-splitting the
#' \code{risk_vec}.
#'
#' @param time_vec Numeric vector of test times.
#' @param status_vec Numeric vector of test statuses.
#' @param risk_vec Numeric vector of predicted risk scores for test data.
#' @param tau The time horizon \eqn{\tau} for integration.
#' @param surv_mat Optional. A matrix of individual survival predictions
#'   (rows=subjects, cols=times) used for model-based RMST calculation.
#' @param eval_times_full Optional. A numeric vector of time points
#'   corresponding to the columns of \code{surv_mat}.
#' @param model_type Optional string (e.log., "rstpm2", "flexsurv") indicating
#'   if a model-based RMST calculation should be attempted.
#'
#' @return The RMST difference (RMST_low - RMST_high), or \code{NA_real_}.
#'
#' @importFrom survival survfit Surv
#' @importFrom stats approxfun integrate median
#' @keywords internal
#' @noMd
#' @importFrom utils head
#' @importFrom utils tail
#' @importFrom survRM2 rmst2
compute_rmst_difference <- function(time_vec,
                                    status_vec,
                                    risk_vec,
                                    tau,
                                    surv_mat = NULL,
                                    eval_times_full = NULL,
                                    model_type = "other") {
  if (!is.finite(tau) || tau <= 0) {
    return(NA_real_)
  }

  risk_group <- assign_risk_group(risk_vec)
  valid <- which(is.finite(time_vec) & !is.na(risk_group))
  if (length(valid) < 2) {
    return(NA_real_)
  }

  time_val <- time_vec[valid]
  status_val <- ifelse(is.na(status_vec[valid]), 0, ifelse(status_vec[valid] > 0, 1, 0))
  group_val <- risk_group[valid]
  if (length(unique(group_val)) < 2) {
    return(NA_real_)
  }

  compute_rmst_from_curve <- function(curve_times, curve_surv) {
    if (length(curve_times) == 0 || length(curve_surv) == 0) {
      return(NA_real_)
    }
    ord <- order(curve_times)
    curve_times <- curve_times[ord]
    curve_surv <- curve_surv[ord]
    keep <- is.finite(curve_times) &
      curve_times >= 0 & is.finite(curve_surv)
    curve_times <- curve_times[keep]
    curve_surv <- curve_surv[keep]
    if (length(curve_times) == 0) {
      return(NA_real_)
    }
    if (curve_times[1] > 0) {
      curve_times <- c(0, curve_times)
      curve_surv <- c(1, curve_surv)
    } else {
      curve_surv[1] <- 1
    }
    if (curve_times[length(curve_times)] < tau) {
      tail_val <- tail(curve_surv, 1)
      curve_times <- c(curve_times, tau)
      curve_surv <- c(curve_surv, tail_val)
    }
    curve_surv <- pmin(pmax(curve_surv, 0), 1)
    curve_surv <- cummin(curve_surv)
    fn <- stats::approxfun(
      curve_times,
      curve_surv,
      method = "linear",
      yleft = 1,
      yright = tail(curve_surv, 1)
    )
    rmst_val <- tryCatch(
      stats::integrate(fn, lower = 0, upper = tau)$value,
      error = function(e)
        NA_real_
    )
    rmst_val
  }

  if ((identical(model_type, "rstpm2") ||
       identical(model_type, "flexsurv")) && !is.null(surv_mat) &&
      !is.null(eval_times_full) &&
      length(eval_times_full) > 0 &&
      is.matrix(surv_mat)) {
    surv_mat_use <- surv_mat
    if (nrow(surv_mat_use) == length(time_vec)) {
      surv_mat_use <- surv_mat_use[valid, , drop = FALSE]
    } else if (nrow(surv_mat_use) != length(valid)) {
      surv_mat_use <- tryCatch(
        surv_mat_use[valid, , drop = FALSE],
        error = function(e)
          NULL
      )
    }

    if (!is.null(surv_mat_use) &&
        nrow(surv_mat_use) == length(valid)) {
      eval_times_full <- as.numeric(eval_times_full)
      prepare_group_curve <- function(group_label) {
        idx <- which(group_val == group_label)
        if (length(idx) == 0) {
          return(NULL)
        }
        curves <- surv_mat_use[idx, , drop = FALSE]
        curve_mean <- if (nrow(curves) == 1) {
          as.numeric(curves[1, ])
        } else {
          colMeans(curves, na.rm = TRUE)
        }
        if (all(is.na(curve_mean))) {
          return(NULL)
        }
        times_full <- eval_times_full
        keep_full <- is.finite(times_full)
        times_full <- times_full[keep_full]
        curve_full <- curve_mean[keep_full]
        keep_vals <- is.finite(curve_full)
        times_full <- times_full[keep_vals]
        curve_full <- curve_full[keep_vals]
        if (length(times_full) == 0) {
          return(NULL)
        }
        sel <- times_full <= tau + 1e-08
        if (!any(sel)) {
          sel <- seq_along(times_full)
        }
        times_sel <- times_full[sel]
        surv_sel <- curve_full[sel]
        if (length(times_sel) == 0) {
          return(NULL)
        }
        if (max(times_sel) < tau) {
          surv_tau <- align_survival_curve(times_full, curve_full, tau)
          if (is.finite(surv_tau)) {
            times_sel <- c(times_sel, tau)
            surv_sel <- c(surv_sel, surv_tau)
          }
        }
        if (length(times_sel) == 0) {
          return(NULL)
        }
        unique_times <- sort(unique(times_sel))
        surv_unique <- vapply(unique_times, function(tt) {
          vals <- surv_sel[abs(times_sel - tt) < 1e-12]
          vals[length(vals)]
        }, numeric(1))
        surv_unique <- pmin(pmax(surv_unique, 0), 1)
        surv_unique <- cummin(surv_unique)
        if (unique_times[1] > 0) {
          unique_times <- c(0, unique_times)
          surv_unique <- c(1, surv_unique)
        } else {
          surv_unique[1] <- 1
        }
        list(times = unique_times,
             surv = surv_unique,
             size = length(idx))
      }

      build_pseudo_data <- function(curve_info, arm_label) {
        if (is.null(curve_info) || length(curve_info$times) < 1) {
          return(NULL)
        }
        times_vec <- curve_info$times
        surv_vec <- curve_info$surv
        if (length(times_vec) < 2) {
          total_n <- max(curve_info$size, 10L)
          return(data.frame(
            time = rep(tau, total_n),
            status = rep(0L, total_n),
            arm = rep.int(as.integer(arm_label), total_n)
          ))
        }
        total_n <- max(curve_info$size, 10L)
        surv_vec <- pmin(pmax(surv_vec, 0), 1)
        surv_vec <- cummin(surv_vec)
        interval_surv_prev <- head(surv_vec, -1)
        interval_surv_next <- tail(surv_vec, -1)
        interval_times <- tail(times_vec, -1)
        expected_counts <- pmax(0,
                                (interval_surv_prev - interval_surv_next) * total_n)
        base_counts <- floor(expected_counts + 1e-08)
        base_counts <- pmin(base_counts, total_n)
        remainder <- expected_counts - base_counts
        leftover <- total_n - sum(base_counts)
        if (leftover > 0 && any(remainder > 0)) {
          order_idx <- order(remainder, decreasing = TRUE)
          add_idx <- order_idx[seq_len(min(leftover, length(order_idx)))]
          base_counts[add_idx] <- base_counts[add_idx] + 1L
          leftover <- total_n - sum(base_counts)
        }
        event_times <- rep(interval_times, base_counts)
        status_vec <- rep(1L, length(event_times))
        if (leftover > 0) {
          event_times <- c(event_times, rep(tau, leftover))
          status_vec <- c(status_vec, rep(0L, leftover))
        }
        data.frame(
          time = as.numeric(event_times),
          status = as.integer(status_vec),
          arm = rep.int(as.integer(arm_label), length(event_times))
        )
      }

      low_curve <- prepare_group_curve("low")
      high_curve <- prepare_group_curve("high")

      if (!is.null(low_curve) && !is.null(high_curve)) {
        if (requireNamespace("survRM2", quietly = TRUE)) {
          pseudo_low <- build_pseudo_data(low_curve, arm_label = 1L)
          pseudo_high <- build_pseudo_data(high_curve, arm_label = 0L)
          if (!is.null(pseudo_low) && !is.null(pseudo_high) &&
              nrow(pseudo_low) > 0 && nrow(pseudo_high) > 0) {
            pseudo_df <- rbind(pseudo_low, pseudo_high)
            rmst_obj <- tryCatch(
              survRM2::rmst2(
                time = pseudo_df$time,
                status = pseudo_df$status,
                arm = pseudo_df$arm,
                tau = tau
              ),
              error = function(e)
                NULL
            )
            if (!is.null(rmst_obj) &&
                !is.null(rmst_obj$unadjusted.result)) {
              diff_row <- rmst_obj$unadjusted.result
              if (is.matrix(diff_row)) {
                rmst_row <- diff_row[grepl("RMST (arm=1)-(arm=0)",
                                           rownames(diff_row),
                                           fixed = TRUE), , drop = FALSE]
                if (nrow(rmst_row) >= 1 &&
                    "Est." %in% colnames(rmst_row)) {
                  diff_val <- rmst_row[1, "Est."]
                  if (is.finite(diff_val)) {
                    return(as.numeric(diff_val))
                  }
                }
              }
            }
          }
        }

        rmst_low <- compute_rmst_from_curve(low_curve$times, low_curve$surv)
        rmst_high <- compute_rmst_from_curve(high_curve$times, high_curve$surv)
        if (is.finite(rmst_low) && is.finite(rmst_high)) {
          return(rmst_low - rmst_high)
        }
      }
    }
  }

  rmst_group <- function(times, status) {
    fit <- tryCatch({
      survival::survfit(survival::Surv(times, status) ~ 1)
    }, error = function(e)
      NULL)
    if (is.null(fit)) {
      return(NA_real_)
    }
    if (length(fit$time) == 0 || length(fit$surv) == 0) {
      return(tau)
    }
    idx <- fit$time < tau
    surv_vals <- fit$surv[idx]
    time_vals <- fit$time[idx]
    surv_at_tau <- align_survival_curve(fit$time, fit$surv, tau)
    times_step <- c(0, time_vals, tau)
    surv_step <- c(1, surv_vals, surv_at_tau)
    sum(surv_step[-length(surv_step)] * diff(times_step))
  }

  low_idx <- group_val == "low"
  high_idx <- group_val == "high"
  if (!any(low_idx) || !any(high_idx)) {
    return(NA_real_)
  }

  rmst_low <- rmst_group(time_val[low_idx], status_val[low_idx])
  rmst_high <- rmst_group(time_val[high_idx], status_val[high_idx])
  if (!is.finite(rmst_low) || !is.finite(rmst_high)) {
    return(NA_real_)
  }
  rmst_low - rmst_high
}

#' Extract Time and Status from Survival Matrix
#'
#' Helper function to extract "time" and "status" columns from a matrix
#' (like one returned by \code{survival::Surv()}), falling back to defaults.
#'
#' @param surv_matrix_vals A matrix, typically from \code{Surv(time, status)}.
#' @param default_time Default time vector if not found.
#' @param default_status Default status vector if not found.
#'
#' @return A list with elements \code{time} and \code{status}.
#' @keywords internal
get_surv_info <- function(surv_matrix_vals,
                          default_time,
                          default_status) {
  time_out <- default_time
  status_out <- default_status
  if (!is.null(surv_matrix_vals)) {
    if ("time" %in% colnames(surv_matrix_vals)) {
      time_out <- as.numeric(surv_matrix_vals[, "time"])
    } else if ("time2" %in% colnames(surv_matrix_vals)) {
      time_out <- as.numeric(surv_matrix_vals[, "time2"])
    }
    if ("status" %in% colnames(surv_matrix_vals)) {
      status_out <- as.numeric(surv_matrix_vals[, "status"])
    }
  }
  list(time = time_out, status = status_out)
}

#' Map Brier Curve Values to Specific Horizons
#'
#' Extracts Brier score values from a pre-computed curve at specific
#' time horizons by finding the closest matching evaluation time.
#'
#' @param curve Numeric vector of Brier scores from \code{compute_ibrier}.
#' @param eval_times Numeric vector of times corresponding to \code{curve}.
#' @param horizons Numeric vector of target time horizons to extract.
#'
#' @return A numeric vector of Brier scores corresponding to \code{horizons}.
#' @keywords internal
map_brier_values <- function(curve, eval_times, horizons) {
  if (length(horizons) == 0) {
    return(numeric(0))
  }
  if (length(curve) != length(eval_times)) {
    return(rep(NA_real_, length(horizons)))
  }
  sapply(horizons, function(h) {
    if (!is.finite(h))
      return(NA_real_)
    idx <- which.min(abs(eval_times - h))
    if (length(idx) == 0)
      return(NA_real_)
    if (abs(eval_times[idx] - h) > max(1e-08, 1e-06 * max(1, abs(h)))) {
      return(NA_real_)
    }
    curve[idx]
  })
}

#' Clamp Values to [0, 1]
#'
#' Truncates a numeric vector so all values lie within the [0, 1] interval.
#'
#' @param x A numeric vector.
#'
#' @return The clamped numeric vector.
#' @keywords internal
clamp01 <- function(x) {
  x <- pmax(pmin(x, 1), 0)
  x
}

#' Align Survival Curve to Evaluation Times
#'
#' Aligns a survival curve (defined by time points and survival probabilities)
#' to a new set of evaluation times using constant interpolation (last value
#' carried forward). Ensures \eqn{S(0) = 1} and monotonicity.
#'
#' @param curve_times Numeric vector of time points from the survival curve.
#' @param curve_surv Numeric vector of survival probabilities corresponding to
#'   \code{curve_times}.
#' @param eval_times Numeric vector of new time points to evaluate at.
#'
#' @return A numeric vector of survival probabilities at \code{eval_times}.
#'
#' @importFrom stats approx
#' @keywords internal
#' @noMd
align_survival_curve <- function(curve_times, curve_surv, eval_times) {
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
  keep <- is.finite(curve_times) & is.finite(curve_surv)
  curve_times <- curve_times[keep]
  curve_surv <- curve_surv[keep]
  if (length(curve_times) == 0) {
    return(rep(NA_real_, length(eval_times)))
  }
  if (curve_times[1] > 0) {
    curve_times <- c(0, curve_times)
    curve_surv <- c(1, curve_surv)
  } else {
    curve_times[1] <- 0
    curve_surv[1] <- 1
  }
  if (length(curve_times) > 1) {
    curve_surv <- cummin(pmin(pmax(curve_surv, 0), 1))
  } else {
    curve_surv <- pmin(pmax(curve_surv, 0), 1)
  }
  approx_res <- stats::approx(
    x = curve_times,
    y = curve_surv,
    xout = eval_times,
    method = "constant",
    f = 0,
    rule = 2,
    ties = "ordered"
  )
  res <- approx_res$y
  res <- pmin(pmax(res, 0), 1)
  res
}

#' Build Survival Matrix from survfit Object
#'
#' Extracts survival probabilities from a \code{survfit} object and aligns
#' them to a common set of evaluation times, creating a matrix.
#'
#' @param fit_obj A \code{survfit} object.
#' @param eval_times Numeric vector of evaluation times.
#' @param n_obs Expected number of observations (rows).
#'
#' @return A matrix (rows=subjects, cols=eval_times) of survival
#'   probabilities, or \code{NULL} on failure.
#' @keywords internal
build_survfit_matrix <- function(fit_obj, eval_times, n_obs) {
  if (is.null(fit_obj) || length(eval_times) == 0 || n_obs == 0) {
    return(NULL)
  }
  surv_times <- fit_obj$time
  surv_vals <- fit_obj$surv
  if (is.null(surv_times) || is.null(surv_vals)) {
    return(NULL)
  }
  if (is.matrix(surv_vals)) {
    n_curves <- ncol(surv_vals)
    res <- matrix(NA_real_,
                  nrow = n_curves,
                  ncol = length(eval_times))
    for (j in seq_len(n_curves)) {
      res[j, ] <- align_survival_curve(surv_times, surv_vals[, j], eval_times)
    }
    if (n_curves != n_obs) {
      if (n_curves == 1 && n_obs > 1) {
        res <- matrix(
          res[1, ],
          nrow = n_obs,
          ncol = length(eval_times),
          byrow = TRUE
        )
      } else {
        res <- res[seq_len(min(n_curves, n_obs)), , drop = FALSE]
        if (n_curves < n_obs) {
          res <- rbind(res,
                       matrix(
                         NA_real_,
                         nrow = n_obs - n_curves,
                         ncol = length(eval_times)
                       ))
        }
      }
    }
    return(res)
  }
  if (is.numeric(surv_vals)) {
    curve <- align_survival_curve(surv_times, surv_vals, eval_times)
    return(matrix(
      rep(curve, each = n_obs),
      nrow = n_obs,
      ncol = length(eval_times)
    ))
  }
  NULL
}

#' Extract survreg Linear Predictor and Scale
#'
#' Computes the linear predictor (lp) and scale parameter(s) for new data
#' from a fitted \code{survreg} model.
#'
#' @param fit_obj A fitted \code{survreg} object.
#' @param new_data A data frame with predictor variables.
#'
#' @return A list with elements \code{lp} (numeric vector) and \code{scale}
#'   (numeric vector), or \code{NULL} on failure.
#'
#' @importFrom stats delete.response model.frame model.offset model.matrix na.pass
#' @importFrom survival untangle.specials strata
#' @keywords internal
extract_survreg_components <- function(fit_obj, new_data) {
  if (is.null(fit_obj) || is.null(new_data)) {
    return(NULL)
  }

  Terms <- fit_obj$terms
  if (is.null(Terms) || !inherits(Terms, "terms")) {
    return(NULL)
  }

  Terms_noy <- stats::delete.response(Terms)

  model_frame <- tryCatch({
    stats::model.frame(
      Terms_noy,
      data = new_data,
      na.action = stats::na.pass,
      xlev = fit_obj$xlevels
    )
  }, error = function(e)
    NULL)

  if (is.null(model_frame) || nrow(model_frame) == 0) {
    return(NULL)
  }

  offset_vals <- tryCatch(
    stats::model.offset(model_frame),
    error = function(e)
      NULL
  )
  if (length(offset_vals) == 0 || all(is.na(offset_vals))) {
    offset_vals <- rep(0, nrow(model_frame))
  } else {
    offset_vals <- as.numeric(offset_vals)
    offset_vals[is.na(offset_vals)] <- 0
  }

  model_matrix <- tryCatch({
    stats::model.matrix(fit_obj, model_frame)
  }, error = function(e)
    NULL)

  if (is.null(model_matrix)) {
    return(NULL)
  }

  coefs <- fit_obj$coefficients
  mm_cols <- colnames(model_matrix)
  if (!is.null(mm_cols) && !is.null(names(coefs))) {
    missing_cols <- setdiff(mm_cols, names(coefs))
    if (length(missing_cols) > 0) {
      complete_coefs <- numeric(length(mm_cols))
      names(complete_coefs) <- mm_cols
      overlap <- intersect(mm_cols, names(coefs))
      if (length(overlap) > 0) {
        complete_coefs[overlap] <- coefs[overlap]
      }
      coefs <- complete_coefs
    } else {
      coefs <- coefs[mm_cols]
    }
  } else if (length(coefs) != ncol(model_matrix)) {
    return(NULL)
  }

  coefs[!is.finite(coefs)] <- 0
  lp <- as.numeric(model_matrix %*% coefs) + offset_vals

  strata_special <- attr(Terms, "specials")$strata
  if (!is.null(strata_special) && length(strata_special) > 0) {
    temp <- survival::untangle.specials(Terms, "strata", 1)
    if (length(temp$vars) == 1) {
      strata_vals <- model_frame[[temp$vars]]
    } else {
      strata_vals <- survival::strata(model_frame[, temp$vars], shortlabel = TRUE)
    }
    scale_lookup <- fit_obj$scale
    if (is.null(scale_lookup)) {
      scale_vec <- rep(1, length(lp))
    } else {
      scale_names <- names(scale_lookup)
      if (!is.null(scale_names)) {
        strata_index <- match(as.character(strata_vals), scale_names)
        strata_index[is.na(strata_index)] <- 1L
      } else {
        strata_index <- as.integer(factor(strata_vals))
        strata_index[!is.finite(strata_index)] <- 1L
        if (length(scale_lookup) < max(strata_index, na.rm = TRUE)) {
          scale_lookup <- rep(scale_lookup,
                              length.out = max(strata_index, na.rm = TRUE))
        }
      }
      scale_vec <- scale_lookup[strata_index]
    }
  } else {
    scale_lookup <- fit_obj$scale
    if (length(scale_lookup) <= 1) {
      scale_vec <- rep(scale_lookup, length(lp))
    } else {
      scale_vec <- scale_lookup[rep(1L, length(lp))]
    }
  }

  scale_vec <- as.numeric(scale_vec)
  scale_vec[!is.finite(scale_vec) | scale_vec <= 0] <- NA_real_

  list(lp = lp, scale = scale_vec)
}

#' Compute Survival Matrix from survreg Model
#'
#' Generates a matrix of survival probabilities (rows=subjects, cols=times)
#' from a fitted \code{survreg} model for new data.
#'
#' @param fit_obj A fitted \code{survreg} object.
#' @param new_data A data frame with predictor variables.
#' @param eval_times Numeric vector of evaluation times.
#'
#' @return A matrix of survival probabilities, or \code{NULL} on failure.
#'
#' @importFrom survival psurvreg
#' @importFrom stats median
#' @keywords internal
compute_survreg_matrix <- function(fit_obj, new_data, eval_times) {
  if (!inherits(fit_obj, "survreg") || length(eval_times) == 0) {
    return(NULL)
  }

  components <- extract_survreg_components(fit_obj, new_data)
  if (is.null(components)) {
    return(NULL)
  }

  lp <- components$lp
  scale_vec <- components$scale
  n_obs <- length(lp)

  if (n_obs == 0) {
    return(matrix(
      numeric(0),
      nrow = 0,
      ncol = length(eval_times)
    ))
  }

  finite_scale <- scale_vec[is.finite(scale_vec) &
                              scale_vec > 0]
  fallback_scale <- if (length(finite_scale) > 0)
    stats::median(finite_scale)
  else
    1
  if (length(scale_vec) != n_obs ||
      any(!is.finite(scale_vec) | scale_vec <= 0)) {
    scale_vec <- rep(fallback_scale, n_obs)
  }

  dist_name <- fit_obj$dist
  if (is.list(dist_name) && !is.null(dist_name$dist)) {
    dist_name <- dist_name$dist
  }
  if (is.null(dist_name)) {
    dist_name <- "weibull"
  }
  dist_name <- as.character(dist_name)
  parms <- fit_obj$parms

  res <- matrix(NA_real_, nrow = n_obs, ncol = length(eval_times))
  for (j in seq_along(eval_times)) {
    t_val <- eval_times[j]
    if (!is.finite(t_val)) {
      next
    }
    if (t_val <= 0) {
      res[, j] <- 1
      next
    }
    q_vec <- rep(t_val, n_obs)
    surv_vals <- tryCatch({
      if (is.null(parms)) {
        survival::psurvreg(
          q_vec,
          mean = lp,
          scale = scale_vec,
          distribution = dist_name
        )
      } else {
        survival::psurvreg(
          q_vec,
          mean = lp,
          scale = scale_vec,
          distribution = dist_name,
          parms = parms
        )
      }
    }, error = function(e)
      rep(NA_real_, n_obs))

    surv_vals <- 1 - surv_vals
    surv_vals[!is.finite(surv_vals)] <- NA_real_
    res[, j] <- pmin(pmax(surv_vals, 0), 1)
  }

  res
}

#' Convert Various Prediction Formats to Survival Matrix
#'
#' Attempts to convert various survival prediction formats (e.g., list of
#' data frames from \code{predict.model_fit} with type "survival", matrices)
#' into a standardized \code{[n_obs, n_eval_times]} matrix.
#'
#' @param pred_obj The prediction object.
#' @param eval_times Numeric vector of evaluation times.
#' @param n_obs Expected number of observations (rows).
#'
#' @return A standardized matrix of survival probabilities, or \code{NULL}
#'   on failure.
#' @keywords internal
convert_survival_predictions <- function(pred_obj, eval_times, n_obs) {
  if (is.null(pred_obj) || length(eval_times) == 0 || n_obs == 0) {
    return(NULL)
  }

  if (is.matrix(pred_obj)) {
    if (nrow(pred_obj) == n_obs &&
        ncol(pred_obj) == length(eval_times)) {
      return(as.matrix(pred_obj))
    }
    if (ncol(pred_obj) == n_obs &&
        nrow(pred_obj) == length(eval_times)) {
      return(t(pred_obj))
    }
  }

  if (is.numeric(pred_obj) &&
      length(pred_obj) == n_obs * length(eval_times)) {
    return(matrix(
      pred_obj,
      nrow = n_obs,
      ncol = length(eval_times)
    ))
  }

  extract_list <- NULL
  if (is.data.frame(pred_obj)) {
    if (".pred_survival" %in% names(pred_obj)) {
      extract_list <- pred_obj$.pred_survival
    } else if (".pred" %in% names(pred_obj)) {
      extract_list <- pred_obj$.pred
    } else {
      extract_list <- pred_obj[[1]]
    }
  } else if (is.list(pred_obj)) {
    extract_list <- pred_obj
  }

  if (is.null(extract_list)) {
    return(NULL)
  }

  if (!is.list(extract_list)) {
    if (is.numeric(extract_list) &&
        length(extract_list) == n_obs * length(eval_times)) {
      return(matrix(
        extract_list,
        nrow = n_obs,
        ncol = length(eval_times)
      ))
    }
    if (is.numeric(extract_list) &&
        length(extract_list) == n_obs) {
      return(matrix(extract_list, nrow = n_obs, ncol = 1))
    }
    return(NULL)
  }

  res <- matrix(NA_real_, nrow = n_obs, ncol = length(eval_times))
  max_iter <- min(length(extract_list), n_obs)
  for (i in seq_len(max_iter)) {
    entry <- extract_list[[i]]
    if (is.null(entry))
      next
    if (is.data.frame(entry)) {
      time_col <- intersect(c(".eval_time", ".time", "time"), names(entry))
      surv_col <- intersect(c(".survival", "survival", ".pred_survival"),
                            names(entry))
      if (length(time_col) > 0 && length(surv_col) > 0) {
        res[i, ] <- align_survival_curve(entry[[time_col[1]]], entry[[surv_col[1]]], eval_times)
        next
      }
      if (ncol(entry) == length(eval_times)) {
        vals <- as.numeric(entry[1, , drop = TRUE])
        if (length(vals) == length(eval_times)) {
          res[i, ] <- vals
          next
        }
      }
    }
    if (is.numeric(entry)) {
      vals <- as.numeric(entry)
      if (length(vals) == length(eval_times)) {
        res[i, ] <- vals
        next
      }
      if (length(vals) == 1 && length(eval_times) == 1) {
        res[i, ] <- rep(vals, length(eval_times))
        next
      }
      if (length(vals) > 1) {
        take <- min(length(vals), length(eval_times))
        res[i, seq_len(take)] <- vals[seq_len(take)]
        next
      }
    }
    if (is.list(entry) && length(entry) > 0) {
      inner <- entry[[1]]
      if (is.data.frame(inner)) {
        time_col <- intersect(c(".eval_time", ".time", "time"), names(inner))
        surv_col <- intersect(c(".survival", "survival", ".pred_survival"),
                              names(inner))
        if (length(time_col) > 0 && length(surv_col) > 0) {
          res[i, ] <- align_survival_curve(inner[[time_col[1]]], inner[[surv_col[1]]], eval_times)
          next
        }
      }
      if (is.numeric(inner)) {
        vals <- as.numeric(inner)
        if (length(vals) == length(eval_times)) {
          res[i, ] <- vals
        }
      }
    }
  }
  res
}

#' Compute Integrated Brier Score and Curve
#'
#' Calculates the Brier score at specified evaluation times and the
#' Integrated Brier Score (IBS) up to \eqn{\tau}, using IPCW to handle
#' censoring.
#'
#' @param eval_times Numeric vector of evaluation time points.
#' @param surv_mat Matrix of predicted survival probabilities
#'   (rows=subjects, cols=eval_times).
#' @param time_vec Numeric vector of test times.
#' @param status_vec Numeric vector of test statuses.
#' @param tau The time horizon \eqn{\tau} for integration.
#' @param censor_eval_fn A function (from \code{create_censor_eval}) that
#'   evaluates the censoring survival function \eqn{G(t)}.
#' @param normalize_by Character string specifying how to normalize Brier
#'   scores. Use \code{"non_missing"} (default) to divide by the number of
#'   non-missing contributions or \code{"n"} to divide by the total sample size.
#' @param include_zero Logical; if \code{TRUE}, integrates from time 0 with a
#'   Brier score of 0 when the evaluation grid starts after 0.
#'
#' @return A list with \code{ibs} (the scalar IBS value) and \code{curve} (a
#'   numeric vector of Brier scores at \code{eval_times}).
#' @keywords internal
#' @noMd
#' @importFrom utils head
#' @importFrom utils tail
compute_ibrier <- function(eval_times,
                           surv_mat,
                           time_vec,
                           status_vec,
                           tau,
                           censor_eval_fn,
                           normalize_by = c("non_missing", "n"),
                           include_zero = TRUE) {
  normalize_by <- match.arg(normalize_by)
  include_zero <- isTRUE(include_zero)
  n <- length(time_vec)
  m <- length(eval_times)
  if (n == 0 ||
      m == 0 ||
      is.null(surv_mat) || nrow(surv_mat) != n || ncol(surv_mat) != m) {
    return(list(ibs = NA_real_, curve = rep(NA_real_, m)))
  }

  status_vec <- ifelse(is.na(status_vec), 0, status_vec)
  status_vec <- ifelse(status_vec > 0, 1, 0)
  time_vec <- as.numeric(time_vec)

  valid_idx <- which(!is.na(time_vec) & !is.na(status_vec))
  if (length(valid_idx) == 0) {
    return(list(ibs = NA_real_, curve = rep(NA_real_, m)))
  }

  if (!is.function(censor_eval_fn)) {
    return(list(ibs = NA_real_, curve = rep(NA_real_, m)))
  }

  G_t <- censor_eval_fn(eval_times)
  G_t[!is.finite(G_t) | G_t <= 0] <- NA_real_

  time_minus <- pmax(time_vec - 1e-08, 0)
  G_time_minus <- censor_eval_fn(time_minus)
  G_time_minus[!is.finite(G_time_minus) |
                 G_time_minus <= 0] <- NA_real_

  weights <- matrix(NA_real_, nrow = n, ncol = m)
  for (j in seq_len(m)) {
    t_val <- eval_times[j]
    g_t <- G_t[j]
    for (i in seq_len(n)) {
      ti <- time_vec[i]
      if (!is.finite(ti)) {
        next
      }
      if (ti <= t_val && status_vec[i] == 1) {
        denom <- G_time_minus[i]
        if (is.finite(denom) && denom > 0) {
          weights[i, j] <- 1 / denom
        }
      } else if (ti > t_val) {
        if (is.finite(g_t) && g_t > 0) {
          weights[i, j] <- 1 / g_t
        }
      }
    }
  }

  indicator <- matrix(0, nrow = n, ncol = m)
  for (j in seq_len(m)) {
    indicator[, j] <- as.numeric(time_vec > eval_times[j])
  }

  residual <- indicator - surv_mat
  residual[!is.finite(residual)] <- NA_real_
  weights[!is.finite(weights) | weights <= 0] <- NA_real_
  weighted <- weights * residual^2
  weighted[!is.finite(weighted)] <- NA_real_
  denom_counts <- colSums(!is.na(weighted))
  bs_t <- colSums(weighted, na.rm = TRUE)
  if (normalize_by == "n") {
    bs_t <- bs_t / n
  } else if (length(denom_counts) == length(bs_t)) {
    valid_cols <- denom_counts > 0
    bs_t[valid_cols] <- bs_t[valid_cols] / denom_counts[valid_cols]
    bs_t[!valid_cols] <- NA_real_
  } else {
    bs_t <- bs_t / n
  }
  bs_t[!is.finite(bs_t)] <- NA_real_

  if (!is.finite(tau) || tau <= 0) {
    tau <- suppressWarnings(max(eval_times[is.finite(eval_times)], na.rm = TRUE))
  }
  if (is.finite(tau)) {
    bs_t[eval_times > tau] <- NA_real_
  }

  valid_bs <- which(is.finite(bs_t) &
                      is.finite(eval_times) & eval_times <= tau + 1e-08)
  if (length(valid_bs) >= 2) {
    times_valid <- eval_times[valid_bs]
    bs_valid <- bs_t[valid_bs]
    ord <- order(times_valid)
    times_valid <- times_valid[ord]
    bs_valid <- bs_valid[ord]
    if (include_zero && length(times_valid) > 0 && times_valid[1] > 0) {
      times_aug <- c(0, times_valid)
      bs_aug <- c(0, bs_valid)
    } else {
      times_aug <- times_valid
      bs_aug <- bs_valid
    }
    area <- sum(diff(times_aug) * (head(bs_aug, -1) + tail(bs_aug, -1)) / 2)
    tau <- max(times_valid)
    ibs <- if (tau > 0)
      area / tau
    else
      NA_real_
  } else if (length(valid_bs) == 1) {
    ibs <- bs_t[valid_bs]
  } else {
    ibs <- NA_real_
  }

  list(ibs = ibs, curve = bs_t)
}

fastml_normalize_class_name <- function(x) {
  tolower(trimws(as.character(x)))
}

fastml_truth_levels <- function(truth_vec) {
  if (is.factor(truth_vec)) {
    levels(truth_vec)
  } else {
    unique(as.character(truth_vec))
  }
}

fastml_prob_cols_from_dots <- function(data, dots) {
  if (length(dots) == 0) {
    return(grep("^\\.pred", names(data), value = TRUE))
  }
  cols <- vapply(dots, rlang::as_name, character(1))
  cols <- cols[cols %in% names(data)]
  if (length(cols) == 0) {
    cols <- grep("^\\.pred", names(data), value = TRUE)
  }
  cols
}

fastml_prepare_prob_matrix <- function(data, prob_cols) {
  prob_cols <- intersect(as.character(prob_cols), names(data))
  if (length(prob_cols) == 0) {
    return(NULL)
  }
  num_cols <- prob_cols[vapply(data[prob_cols], is.numeric, logical(1))]
  if (length(num_cols) == 0) {
    return(NULL)
  }
  list(mat = as.matrix(data[num_cols]), cols = num_cols)
}

fastml_align_prob_matrix <- function(prob_mat, prob_cols, truth_levels) {
  if (is.null(prob_mat) || length(prob_cols) == 0 || length(truth_levels) == 0) {
    return(NULL)
  }
  prob_labels <- sub("^\\.pred_p", "", sub("^\\.pred_", "", prob_cols))
  prob_norm <- fastml_normalize_class_name(make.names(prob_labels))
  truth_norm <- fastml_normalize_class_name(make.names(truth_levels))
  match_idx <- match(truth_norm, prob_norm)
  if (any(is.na(match_idx))) {
    return(NULL)
  }
  list(mat = prob_mat[, match_idx, drop = FALSE], cols = prob_cols[match_idx])
}

fastml_binary_logloss <- function(truth_vec, prob_vec, positive_class) {
  if (is.null(positive_class) || length(prob_vec) == 0) {
    return(NA_real_)
  }
  y <- as.character(truth_vec) == as.character(positive_class)
  valid <- is.finite(prob_vec) & !is.na(y)
  if (!any(valid)) {
    return(NA_real_)
  }
  p <- prob_vec[valid]
  y <- as.numeric(y[valid])
  eps <- 1e-15
  p <- pmin(pmax(p, eps), 1 - eps)
  -mean(y * log(p) + (1 - y) * log(1 - p))
}

fastml_binary_brier <- function(truth_vec, prob_vec, positive_class) {
  if (is.null(positive_class) || length(prob_vec) == 0) {
    return(NA_real_)
  }
  y <- as.character(truth_vec) == as.character(positive_class)
  valid <- is.finite(prob_vec) & !is.na(y)
  if (!any(valid)) {
    return(NA_real_)
  }
  p <- prob_vec[valid]
  y <- as.numeric(y[valid])
  mean((p - y)^2)
}

fastml_binary_ece <- function(truth_vec, prob_vec, positive_class, n_bins = 10) {
  if (is.null(positive_class) || length(prob_vec) == 0) {
    return(NA_real_)
  }
  n_bins <- as.integer(n_bins)
  if (!is.finite(n_bins) || n_bins < 1) {
    n_bins <- 10L
  }
  y <- as.character(truth_vec) == as.character(positive_class)
  valid <- is.finite(prob_vec) & !is.na(y)
  if (!any(valid)) {
    return(NA_real_)
  }
  p <- pmin(pmax(prob_vec[valid], 0), 1)
  y <- as.numeric(y[valid])
  bins <- cut(p,
              breaks = seq(0, 1, length.out = n_bins + 1L),
              include.lowest = TRUE,
              labels = FALSE)
  n <- length(p)
  ece <- 0
  for (b in seq_len(n_bins)) {
    idx <- which(bins == b)
    if (length(idx) == 0) {
      next
    }
    conf <- mean(p[idx])
    acc <- mean(y[idx])
    ece <- ece + abs(acc - conf) * (length(idx) / n)
  }
  ece
}

fastml_multiclass_logloss <- function(truth_vec, prob_mat, truth_levels) {
  n <- length(truth_vec)
  if (is.null(prob_mat) || n == 0 || nrow(prob_mat) != n) {
    return(NA_real_)
  }
  idx <- match(as.character(truth_vec), truth_levels)
  p_true <- prob_mat[cbind(seq_len(n), idx)]
  valid <- !is.na(idx) & is.finite(p_true)
  if (!any(valid)) {
    return(NA_real_)
  }
  eps <- 1e-15
  p_true <- pmin(pmax(p_true, eps), 1 - eps)
  -mean(log(p_true[valid]))
}

fastml_multiclass_brier <- function(truth_vec, prob_mat, truth_levels) {
  n <- length(truth_vec)
  if (is.null(prob_mat) || n == 0 || nrow(prob_mat) != n) {
    return(NA_real_)
  }
  idx <- match(as.character(truth_vec), truth_levels)
  sum_sq <- rowSums(prob_mat^2)
  p_true <- prob_mat[cbind(seq_len(n), idx)]
  valid <- !is.na(idx) & is.finite(p_true) & is.finite(sum_sq)
  if (!any(valid)) {
    return(NA_real_)
  }
  mean(sum_sq[valid] - 2 * p_true[valid] + 1)
}

fastml_multiclass_ece <- function(truth_vec, prob_mat, truth_levels, n_bins = 10) {
  n <- length(truth_vec)
  if (is.null(prob_mat) || n == 0 || nrow(prob_mat) != n) {
    return(NA_real_)
  }
  n_bins <- as.integer(n_bins)
  if (!is.finite(n_bins) || n_bins < 1) {
    n_bins <- 10L
  }
  prob_mat <- pmin(pmax(prob_mat, 0), 1)
  row_has_finite <- apply(prob_mat, 1, function(x) any(is.finite(x)))
  safe_mat <- prob_mat
  safe_mat[!is.finite(safe_mat)] <- -Inf
  max_idx <- max.col(safe_mat, ties.method = "first")
  conf <- prob_mat[cbind(seq_len(n), max_idx)]
  conf[!row_has_finite] <- NA_real_
  pred_class <- truth_levels[max_idx]
  correct <- as.character(truth_vec) == as.character(pred_class)
  valid <- is.finite(conf) & !is.na(correct)
  if (!any(valid)) {
    return(NA_real_)
  }
  conf <- conf[valid]
  correct <- as.numeric(correct[valid])
  bins <- cut(conf,
              breaks = seq(0, 1, length.out = n_bins + 1L),
              include.lowest = TRUE,
              labels = FALSE)
  n_valid <- length(conf)
  ece <- 0
  for (b in seq_len(n_bins)) {
    idx <- which(bins == b)
    if (length(idx) == 0) {
      next
    }
    conf_mean <- mean(conf[idx])
    acc_mean <- mean(correct[idx])
    ece <- ece + abs(acc_mean - conf_mean) * (length(idx) / n_valid)
  }
  ece
}

fastml_class_calibration_metrics <- function(truth_vec, prob_mat, prob_cols, event_class) {
  truth_levels <- fastml_truth_levels(truth_vec)
  n_classes <- length(truth_levels)
  if (n_classes < 2 || is.null(prob_mat) || length(prob_cols) == 0) {
    return(list(logloss = NA_real_,
                brier_score = NA_real_,
                ece = NA_real_,
                estimator = NA_character_))
  }
  if (n_classes == 2) {
    if (is.null(event_class) || !event_class %in% c("first", "second")) {
      event_class <- "first"
    }
    prob_meta <- fastml_resolve_binary_prob_column(prob_cols, truth_levels, event_class)
    if (is.null(prob_meta$prob_col)) {
      return(list(logloss = NA_real_,
                  brier_score = NA_real_,
                  ece = NA_real_,
                  estimator = NA_character_))
    }
    col_idx <- match(prob_meta$prob_col, prob_cols)
    if (is.na(col_idx)) {
      return(list(logloss = NA_real_,
                  brier_score = NA_real_,
                  ece = NA_real_,
                  estimator = NA_character_))
    }
    prob_vec <- prob_mat[, col_idx]
    logloss <- fastml_binary_logloss(truth_vec, prob_vec, prob_meta$positive_class)
    brier <- fastml_binary_brier(truth_vec, prob_vec, prob_meta$positive_class)
    ece <- fastml_binary_ece(truth_vec, prob_vec, prob_meta$positive_class)
    return(list(logloss = logloss,
                brier_score = brier,
                ece = ece,
                estimator = NA_character_))
  }
  aligned <- fastml_align_prob_matrix(prob_mat, prob_cols, truth_levels)
  if (is.null(aligned)) {
    return(list(logloss = NA_real_,
                brier_score = NA_real_,
                ece = NA_real_,
                estimator = NA_character_))
  }
  prob_mat <- pmin(pmax(aligned$mat, 0), 1)
  logloss <- fastml_multiclass_logloss(truth_vec, prob_mat, truth_levels)
  brier <- fastml_multiclass_brier(truth_vec, prob_mat, truth_levels)
  ece <- fastml_multiclass_ece(truth_vec, prob_mat, truth_levels)
  list(logloss = logloss, brier_score = brier, ece = ece, estimator = "macro")
}

fastml_wrap_event_level_metric <- function(metric, event_class) {
  if (is.null(event_class) || !event_class %in% c("first", "second")) {
    return(metric)
  }

  metric_fun <- function(data, truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {
    truth_quo <- rlang::enquo(truth)
    estimate_quo <- rlang::enquo(estimate)
    case_weights_quo <- rlang::enquo(case_weights)
    if (rlang::quo_is_missing(truth_quo) || rlang::quo_is_missing(estimate_quo)) {
      metric_name <- attr(metric, "metric_name")
      if (is.null(metric_name) || !nzchar(metric_name)) {
        metric_name <- "metric"
      }
      return(tibble::tibble(
        .metric = metric_name,
        .estimator = NA_character_,
        .estimate = NA_real_
      ))
    }
    dots <- rlang::enquos(...)
    dots$event_level <- NULL
    call <- rlang::expr(
      metric(
        data = data,
        truth = !!truth_quo,
        estimate = !!estimate_quo,
        na_rm = na_rm,
        case_weights = !!case_weights_quo,
        event_level = event_class,
        !!!dots
      )
    )
    rlang::eval_tidy(call)
  }

  class(metric_fun) <- class(metric)
  attr(metric_fun, "direction") <- attr(metric, "direction")
  attr(metric_fun, "metric_name") <- attr(metric, "metric_name")
  attr(metric_fun, "range") <- attr(metric, "range")
  static <- attr(metric, "static", exact = TRUE)
  if (is.null(static)) {
    static <- "event_level"
  } else {
    static <- unique(c(static, "event_level"))
  }
  attr(metric_fun, "static") <- static
  metric_fun
}

fastml_resolve_binary_prob_column <- function(prob_cols,
                                              truth_levels,
                                              event_class,
                                              allow_fallback = FALSE) {
  if (!event_class %in% c("first", "second")) {
    stop("Invalid event_class argument. It should be either 'first' or 'second'.")
  }

  prob_cols <- as.character(prob_cols)
  truth_levels <- as.character(truth_levels)

  result <- list(
    prob_col = NULL,
    positive_class = NULL,
    candidates = character(),
    used_fallback = FALSE
  )

  if (length(prob_cols) == 0 || length(truth_levels) < 2) {
    return(result)
  }

  pos_idx <- if (event_class == "first") 1L else 2L
  positive_class <- truth_levels[pos_idx]
  result$positive_class <- positive_class

  candidate_levels <- unique(c(positive_class, make.names(positive_class)))
  candidates <- unique(c(
    paste0(".pred_", candidate_levels),
    paste0(".pred_p", candidate_levels)
  ))
  event_idx <- if (event_class == "first") 0L else 1L
  candidates <- unique(c(candidates, paste0(".pred_p", event_idx)))
  result$candidates <- candidates

  prob_col <- candidates[candidates %in% prob_cols][1]
  if (!is.na(prob_col) && length(prob_col) > 0) {
    result$prob_col <- prob_col
    return(result)
  }

  prob_levels <- sub("^\\.pred_p", "", sub("^\\.pred_", "", prob_cols))
  prob_levels_norm <- fastml_normalize_class_name(make.names(prob_levels))
  candidate_norm <- fastml_normalize_class_name(make.names(candidate_levels))
  match_idx <- match(candidate_norm, prob_levels_norm)
  match_idx <- match_idx[!is.na(match_idx)][1]
  if (!is.na(match_idx)) {
    result$prob_col <- prob_cols[match_idx]
    return(result)
  }

  if (isTRUE(allow_fallback) && length(prob_cols) == 2) {
    result$prob_col <- prob_cols[pos_idx]
    result$used_fallback <- TRUE
  }

  result
}

fastml_normalize_multiclass_auc <- function(multiclass_auc) {
  if (is.null(multiclass_auc) || length(multiclass_auc) == 0 || is.na(multiclass_auc[[1]])) {
    return("macro")
  }

  multiclass_auc <- tolower(as.character(multiclass_auc[[1]]))
  allowed <- c("macro", "macro_weighted")
  if (!multiclass_auc %in% allowed) {
    stop(
      sprintf(
        "Invalid multiclass_auc '%s'. Choose one of: %s.",
        multiclass_auc,
        paste(allowed, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  multiclass_auc
}

fastml_auc_estimator_from_truth <- function(truth, multiclass_auc) {
  multiclass_auc <- fastml_normalize_multiclass_auc(multiclass_auc)
  if (is.factor(truth)) {
    n_levels <- length(levels(truth))
  } else {
    truth_vals <- truth[!is.na(truth)]
    n_levels <- length(unique(truth_vals))
  }
  if (n_levels > 2) multiclass_auc else "binary"
}

fastml_configured_roc_auc <- function(multiclass_auc, event_class = NULL) {
  multiclass_auc <- fastml_normalize_multiclass_auc(multiclass_auc)

  roc_fun <- function(data, truth, ..., estimator = NULL) {
    na_result <- function(estimator_value) {
      tibble::tibble(
        .metric = "roc_auc",
        .estimator = estimator_value,
        .estimate = NA_real_
      )
    }

    # Extract the truth column name from the argument
    truth_name <- NULL
    if (!missing(truth) && !is.null(truth)) {
      truth_name <- tryCatch({
        truth_sym <- rlang::ensym(truth)
        rlang::as_string(truth_sym)
      }, error = function(e) NULL)
    }

    if (is.null(truth_name) || !nzchar(truth_name) || !is.data.frame(data)) {
      return(na_result(NA_character_))
    }

    if (!truth_name %in% names(data)) {
      return(na_result(NA_character_))
    }

    truth_col <- data[[truth_name]]
    if (is.null(truth_col)) {
      return(na_result(NA_character_))
    }

    # Determine estimator type
    if (missing(estimator) || is.null(estimator) || length(estimator) == 0 || is.na(estimator[[1]])) {
      estimator <- fastml_auc_estimator_from_truth(truth_col, multiclass_auc)
    }
    estimator <- as.character(estimator)[1]
    if (is.null(estimator) || is.na(estimator) || !nzchar(estimator)) {
      estimator <- NA_character_
    }

    truth_vals <- truth_col[!is.na(truth_col)]
    if (length(unique(truth_vals)) < 2) {
      return(na_result(estimator))
    }

    dots <- rlang::enquos(...)
    prob_cols <- fastml_prob_cols_from_dots(data, dots)
    prob_info <- fastml_prepare_prob_matrix(data, prob_cols)
    if (is.null(prob_info)) {
      return(na_result(estimator))
    }

    prob_cols <- prob_info$cols
    if (length(prob_cols) == 0) {
      return(na_result(estimator))
    }

    # Use the vector-based approach to avoid NSE issues
    if (identical(estimator, "binary")) {
      truth_levels <- fastml_truth_levels(truth_col)
      event_use <- if (!is.null(event_class) && event_class %in% c("first", "second")) {
        event_class
      } else {
        "first"
      }
      prob_meta <- fastml_resolve_binary_prob_column(
        prob_cols,
        truth_levels,
        event_use,
        allow_fallback = FALSE
      )
      prob_col <- prob_meta$prob_col
      if (is.null(prob_col) || !prob_col %in% prob_cols) {
        return(na_result(estimator))
      }

      result <- tryCatch({
        # Use yardstick::roc_auc_vec for vector-based computation (no NSE)
        truth_vec <- truth_col
        prob_vec <- data[[prob_col]]

        if (!is.factor(truth_vec)) {
          truth_vec <- factor(truth_vec)
        }

        event_level_use <- if (!is.null(event_class) && event_class %in% c("first", "second")) {
          event_class
        } else {
          "first"
        }

        auc_value <- yardstick::roc_auc_vec(
          truth = truth_vec,
          estimate = prob_vec,
          event_level = event_level_use
        )

        tibble::tibble(
          .metric = "roc_auc",
          .estimator = "binary",
          .estimate = auc_value
        )
      }, error = function(e) {
        na_result("binary")
      })

      return(result)
    } else {
      # Multiclass case
      truth_levels <- fastml_truth_levels(truth_col)
      aligned <- fastml_align_prob_matrix(prob_info$mat, prob_cols, truth_levels)
      if (is.null(aligned) || length(aligned$cols) == 0) {
        return(na_result(estimator))
      }

      result <- tryCatch({
        truth_vec <- truth_col
        if (!is.factor(truth_vec)) {
          truth_vec <- factor(truth_vec, levels = truth_levels)
        }

        # Create probability matrix for multiclass
        prob_mat <- as.matrix(data[, aligned$cols, drop = FALSE])

        auc_value <- yardstick::roc_auc_vec(
          truth = truth_vec,
          estimate = prob_mat,
          estimator = estimator
        )

        tibble::tibble(
          .metric = "roc_auc",
          .estimator = estimator,
          .estimate = auc_value
        )
      }, error = function(e) {
        na_result(estimator)
      })

      return(result)
    }
  }

  class(roc_fun) <- class(yardstick::roc_auc)
  attr(roc_fun, "direction") <- attr(yardstick::roc_auc, "direction")
  attr(roc_fun, "metric_name") <- attr(yardstick::roc_auc, "metric_name")
  roc_fun
}

fastml_configured_logloss <- function(event_class = NULL) {
  logloss_fun <- function(data, truth, ..., estimator = NULL) {
    truth_sym <- NULL
    if (!missing(truth) && !is.null(truth)) {
      truth_sym <- tryCatch(rlang::ensym(truth), error = function(e) NULL)
    }
    truth_col <- if (!is.null(truth_sym)) {
      tryCatch(rlang::as_string(truth_sym), error = function(e) NULL)
    } else {
      NULL
    }
    if (is.null(truth_col) || !nzchar(truth_col) || !is.data.frame(data)) {
      return(tibble::tibble(
        .metric = "logloss",
        .estimator = NA_character_,
        .estimate = NA_real_
      ))
    }
    truth_vec <- data[[truth_col]]
    dots <- rlang::enquos(...)
    prob_cols <- fastml_prob_cols_from_dots(data, dots)
    prob_info <- fastml_prepare_prob_matrix(data, prob_cols)
    if (is.null(prob_info)) {
      estimate <- NA_real_
      estimator_out <- NA_character_
    } else {
      metrics <- fastml_class_calibration_metrics(truth_vec,
                                                  prob_info$mat,
                                                  prob_info$cols,
                                                  event_class)
      estimate <- metrics$logloss
      estimator_out <- metrics$estimator
    }
    tibble::tibble(
      .metric = "logloss",
      .estimator = estimator_out,
      .estimate = as.numeric(estimate)
    )
  }

  class(logloss_fun) <- class(yardstick::roc_auc)
  attr(logloss_fun, "direction") <- "minimize"
  attr(logloss_fun, "metric_name") <- "logloss"
  logloss_fun
}

fastml_configured_brier_score <- function(event_class = NULL) {
  brier_fun <- function(data, truth, ..., estimator = NULL) {
    truth_sym <- NULL
    if (!missing(truth) && !is.null(truth)) {
      truth_sym <- tryCatch(rlang::ensym(truth), error = function(e) NULL)
    }
    truth_col <- if (!is.null(truth_sym)) {
      tryCatch(rlang::as_string(truth_sym), error = function(e) NULL)
    } else {
      NULL
    }
    if (is.null(truth_col) || !nzchar(truth_col) || !is.data.frame(data)) {
      return(tibble::tibble(
        .metric = "brier_score",
        .estimator = NA_character_,
        .estimate = NA_real_
      ))
    }
    truth_vec <- data[[truth_col]]
    dots <- rlang::enquos(...)
    prob_cols <- fastml_prob_cols_from_dots(data, dots)
    prob_info <- fastml_prepare_prob_matrix(data, prob_cols)
    if (is.null(prob_info)) {
      estimate <- NA_real_
      estimator_out <- NA_character_
    } else {
      metrics <- fastml_class_calibration_metrics(truth_vec,
                                                  prob_info$mat,
                                                  prob_info$cols,
                                                  event_class)
      estimate <- metrics$brier_score
      estimator_out <- metrics$estimator
    }
    tibble::tibble(
      .metric = "brier_score",
      .estimator = estimator_out,
      .estimate = as.numeric(estimate)
    )
  }

  class(brier_fun) <- class(yardstick::roc_auc)
  attr(brier_fun, "direction") <- "minimize"
  attr(brier_fun, "metric_name") <- "brier_score"
  brier_fun
}

fastml_configured_ece <- function(event_class = NULL) {
  ece_fun <- function(data, truth, ..., estimator = NULL) {
    truth_sym <- NULL
    if (!missing(truth) && !is.null(truth)) {
      truth_sym <- tryCatch(rlang::ensym(truth), error = function(e) NULL)
    }
    truth_col <- if (!is.null(truth_sym)) {
      tryCatch(rlang::as_string(truth_sym), error = function(e) NULL)
    } else {
      NULL
    }
    if (is.null(truth_col) || !nzchar(truth_col) || !is.data.frame(data)) {
      return(tibble::tibble(
        .metric = "ece",
        .estimator = NA_character_,
        .estimate = NA_real_
      ))
    }
    truth_vec <- data[[truth_col]]
    dots <- rlang::enquos(...)
    prob_cols <- fastml_prob_cols_from_dots(data, dots)
    prob_info <- fastml_prepare_prob_matrix(data, prob_cols)
    if (is.null(prob_info)) {
      estimate <- NA_real_
      estimator_out <- NA_character_
    } else {
      metrics <- fastml_class_calibration_metrics(truth_vec,
                                                  prob_info$mat,
                                                  prob_info$cols,
                                                  event_class)
      estimate <- metrics$ece
      estimator_out <- metrics$estimator
    }
    tibble::tibble(
      .metric = "ece",
      .estimator = estimator_out,
      .estimate = as.numeric(estimate)
    )
  }

  class(ece_fun) <- class(yardstick::roc_auc)
  attr(ece_fun, "direction") <- "minimize"
  attr(ece_fun, "metric_name") <- "ece"
  ece_fun
}
