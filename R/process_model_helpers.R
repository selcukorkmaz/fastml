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

clamp01 <- function(x) {
  x <- pmax(pmin(x, 1), 0)
  x
}
