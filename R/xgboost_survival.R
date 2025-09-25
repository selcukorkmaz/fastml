#' Internal helpers for xgboost-based survival models
#'
#' These utilities provide training-time summaries (e.g. baseline hazards)
#' and prediction helpers shared between the Cox and AFT objectives exposed
#' by the native xgboost survival integration.  They are not exported.
#'
#' @noRd
NULL

fastml_prepare_native_survival_predictors <- function(model, baked_newdata, original_newdata) {
  if (is.null(baked_newdata)) {
    baked_newdata <- data.frame()
  }
  pred_predictors <- as.data.frame(baked_newdata)
  keep_cols <- names(pred_predictors)
  drop_cols <- c(model$response, model$time_col, model$status_col, model$start_col)
  drop_cols <- unique(drop_cols[!is.na(drop_cols)])
  drop_cols <- intersect(drop_cols, keep_cols)
  if (length(drop_cols) > 0) {
    keep_cols <- setdiff(keep_cols, drop_cols)
  }
  if (!is.null(model$strata_dummy_cols) && length(model$strata_dummy_cols) > 0) {
    keep_cols <- setdiff(keep_cols, model$strata_dummy_cols)
  }
  if (!is.null(model$strata_base_cols) && length(model$strata_base_cols) > 0) {
    keep_cols <- setdiff(keep_cols, model$strata_base_cols)
  }
  if (length(keep_cols) == 0) {
    pred_predictors <- pred_predictors[, 0, drop = FALSE]
  } else {
    pred_predictors <- pred_predictors[, keep_cols, drop = FALSE]
  }
  extra_cols <- unique(c(model$start_col, model$time_col, model$status_col))
  extra_cols <- extra_cols[!is.na(extra_cols)]
  if (length(extra_cols) > 0) {
    for (ec in extra_cols) {
      if (!(ec %in% names(pred_predictors)) && ec %in% names(original_newdata)) {
        pred_predictors[[ec]] <- original_newdata[[ec]]
      }
    }
  }
  if (!is.null(model$strata_cols) && length(model$strata_cols) > 0) {
    strata_levels <- NULL
    fit_obj <- tryCatch(model$fit, error = function(e) NULL)
    if (!is.null(fit_obj)) {
      strata_levels <- tryCatch(fit_obj$xlevels, error = function(e) NULL)
    }
    for (sc in model$strata_cols) {
      if (!(sc %in% names(pred_predictors)) && sc %in% names(original_newdata)) {
        pred_predictors[[sc]] <- original_newdata[[sc]]
      }
      if (sc %in% names(pred_predictors)) {
        pred_predictors[[sc]] <- as.factor(pred_predictors[[sc]])
        if (!is.null(strata_levels) && sc %in% names(strata_levels)) {
          pred_predictors[[sc]] <- factor(pred_predictors[[sc]], levels = strata_levels[[sc]])
        }
      }
    }
  }
  pred_predictors
}

fastml_prepare_xgb_matrix <- function(predictors, feature_names) {
  if (is.null(predictors)) {
    predictors <- data.frame()
  }
  predictors <- as.data.frame(predictors)
  if (length(feature_names) == 0) {
    return(matrix(numeric(), nrow = nrow(predictors), ncol = 0))
  }
  if (ncol(predictors) == 0) {
    mat <- matrix(0, nrow = nrow(predictors), ncol = length(feature_names))
    colnames(mat) <- feature_names
    return(mat)
  }
  for (nm in names(predictors)) {
    col <- predictors[[nm]]
    if (is.factor(col)) {
      predictors[[nm]] <- as.numeric(col)
    } else if (is.logical(col)) {
      predictors[[nm]] <- as.numeric(col)
    }
  }
  mat <- as.matrix(predictors)
  if (is.null(colnames(mat))) {
    colnames(mat) <- paste0("V", seq_len(ncol(mat)))
  }
  present <- intersect(feature_names, colnames(mat))
  if (length(present) > 0) {
    mat <- mat[, present, drop = FALSE]
  } else {
    mat <- matrix(numeric(), nrow = nrow(mat), ncol = 0)
  }
  missing <- setdiff(feature_names, colnames(mat))
  if (length(missing) > 0) {
    if (nrow(mat) == 0) {
      add_cols <- matrix(numeric(), nrow = 0, ncol = length(missing))
    } else {
      add_cols <- matrix(0, nrow = nrow(mat), ncol = length(missing))
    }
    colnames(add_cols) <- missing
    if (ncol(mat) == 0) {
      mat <- add_cols
    } else {
      mat <- cbind(mat, add_cols)
    }
  }
  mat <- mat[, feature_names, drop = FALSE]
  storage.mode(mat) <- "numeric"
  mat
}

fastml_compute_breslow_baseline <- function(time_vec, status_vec, lp_vec) {
  df <- data.frame(
    time = as.numeric(time_vec),
    status = ifelse(is.na(status_vec), 0, ifelse(status_vec > 0, 1, 0)),
    lp = as.numeric(lp_vec)
  )
  df <- df[is.finite(df$time) & df$time >= 0, , drop = FALSE]
  if (nrow(df) == 0) {
    return(NULL)
  }
  suppressWarnings({
    fit <- survival::coxph(
      survival::Surv(time, status) ~ offset(lp),
      data = df,
      ties = "breslow"
    )
  })
  bh <- tryCatch(survival::basehaz(fit, centered = FALSE), error = function(e) NULL)
  if (is.null(bh) || nrow(bh) == 0) {
    return(NULL)
  }
  bh <- bh[order(bh$time), , drop = FALSE]
  bh$cumhaz <- as.numeric(bh$hazard)
  bh$surv <- exp(-bh$cumhaz)
  bh <- bh[, c("time", "cumhaz", "surv"), drop = FALSE]
  bh
}

fastml_xgb_predict_lp <- function(fit_obj, predictors) {
  booster <- fit_obj$booster
  if (is.null(booster)) {
    return(rep(NA_real_, nrow(predictors)))
  }
  feature_names <- fit_obj$feature_names
  mat <- fastml_prepare_xgb_matrix(predictors, feature_names)
  if (nrow(mat) == 0 && length(feature_names) > 0) {
    return(rep(NA_real_, nrow(predictors)))
  }
  pred <- tryCatch({
    xgboost::predict(booster, newdata = mat, outputmargin = TRUE)
  }, error = function(e) rep(NA_real_, nrow(mat)))
  as.numeric(pred)
}

fastml_xgb_survival_matrix_cox <- function(fit_obj, lp_vec, times) {
  baseline <- fit_obj$baseline
  if (is.null(baseline) || nrow(baseline) == 0 || length(times) == 0) {
    return(matrix(NA_real_, nrow = length(lp_vec), ncol = length(times)))
  }
  times <- as.numeric(times)
  times[!is.finite(times) | times < 0] <- NA_real_
  base_time <- baseline$time
  base_cumhaz <- baseline$cumhaz
  lookup <- function(t) {
    if (is.na(t)) {
      return(NA_real_)
    }
    if (t <= base_time[1]) {
      return(0)
    }
    idx <- findInterval(t, base_time)
    if (idx <= 0) {
      return(0)
    }
    if (idx > length(base_time)) {
      idx <- length(base_time)
    }
    base_cumhaz[idx]
  }
  cumhaz_vals <- vapply(times, lookup, numeric(1))
  cumhaz_vals[!is.finite(cumhaz_vals) | cumhaz_vals < 0] <- NA_real_
  if (length(lp_vec) == 0) {
    return(matrix(numeric(), nrow = 0, ncol = length(times)))
  }
  mat <- matrix(NA_real_, nrow = length(lp_vec), ncol = length(times))
  for (j in seq_along(times)) {
    ch <- cumhaz_vals[j]
    if (!is.finite(ch) || ch < 0) {
      next
    }
    mat[, j] <- exp(-ch * exp(lp_vec))
  }
  mat[mat < 0] <- 0
  mat[mat > 1] <- 1
  if (ncol(mat) > 1) {
    mat <- t(apply(mat, 1, function(row) {
      row <- cummin(pmin(pmax(row, 0), 1))
      row
    }))
  }
  mat
}

fastml_aft_survival_prob <- function(times, mu_vec, dist, scale) {
  if (length(times) == 0) {
    return(matrix(numeric(), nrow = length(mu_vec), ncol = 0))
  }
  scale <- ifelse(is.finite(scale) && scale > 0, scale, 1)
  times <- as.numeric(times)
  times[times <= 0 | !is.finite(times)] <- NA_real_
  if (length(mu_vec) == 0) {
    return(matrix(numeric(), nrow = 0, ncol = length(times)))
  }
  mat <- matrix(NA_real_, nrow = length(mu_vec), ncol = length(times))
  dist <- tolower(dist)
  for (j in seq_along(times)) {
    tval <- times[j]
    if (!is.finite(tval) || tval <= 0) {
      next
    }
    z <- (log(tval) - mu_vec) / scale
    surv <- switch(dist,
                   "logistic" = 1 / (1 + exp(z)),
                   "extreme" = 1 - exp(-exp(-z)),
                   "normal" = 1 - stats::pnorm(z),
                   1 - stats::pnorm(z))
    surv[!is.finite(surv)] <- NA_real_
    mat[, j] <- surv
  }
  mat[mat < 0] <- 0
  mat[mat > 1] <- 1
  if (ncol(mat) > 1) {
    mat <- t(apply(mat, 1, function(row) cummin(pmin(pmax(row, 0), 1))))
  }
  mat
}

fastml_aft_quantile_times <- function(probs, mu_vec, dist, scale) {
  if (length(mu_vec) == 0 || length(probs) == 0) {
    return(matrix(numeric(), nrow = length(mu_vec), ncol = length(probs)))
  }
  scale <- ifelse(is.finite(scale) && scale > 0, scale, 1)
  probs <- as.numeric(probs)
  probs <- probs[is.finite(probs) & probs > 0 & probs < 1]
  if (length(probs) == 0) {
    return(matrix(numeric(), nrow = length(mu_vec), ncol = 0))
  }
  n <- length(mu_vec)
  k <- length(probs)
  z_vals <- switch(tolower(dist),
                   "logistic" = log(probs / (1 - probs)),
                   "extreme" = -log(-log(probs)),
                   "normal" = stats::qnorm(probs),
                   stats::qnorm(probs))
  z_vals[!is.finite(z_vals)] <- NA_real_
  log_time <- outer(mu_vec, rep(1, k)) + scale * matrix(z_vals, nrow = n, ncol = k, byrow = TRUE)
  times <- exp(log_time)
  times[!is.finite(times) | times <= 0] <- NA_real_
  colnames(times) <- format(probs, trim = TRUE, scientific = FALSE)
  times
}

fastml_aft_survival_from_quantiles <- function(times, quantile_mat, probs) {
  if (length(times) == 0) {
    return(matrix(numeric(), nrow = nrow(quantile_mat), ncol = 0))
  }
  n <- if (is.null(quantile_mat)) 0 else nrow(quantile_mat)
  if (n == 0) {
    return(matrix(numeric(), nrow = 0, ncol = length(times)))
  }
  if (is.null(probs) || length(probs) == 0) {
    return(matrix(NA_real_, nrow = n, ncol = length(times)))
  }
  probs <- as.numeric(probs)
  ord <- order(probs)
  probs_ord <- probs[ord]
  quant_ord <- quantile_mat[, ord, drop = FALSE]
  res <- matrix(NA_real_, nrow = n, ncol = length(times))
  for (i in seq_len(n)) {
    q_row <- as.numeric(quant_ord[i, ])
    valid <- is.finite(q_row)
    if (!any(valid)) {
      next
    }
    q_vals <- q_row[valid]
    p_vals <- probs_ord[valid]
    if (length(q_vals) < 1) {
      next
    }
    if (length(q_vals) > 1) {
      q_vals <- cummax(q_vals)
    }
    for (j in seq_along(times)) {
      tval <- times[j]
      if (!is.finite(tval) || tval <= 0) {
        next
      }
      idx <- findInterval(tval, q_vals)
      if (idx <= 0) {
        surv <- 1
      } else if (idx >= length(q_vals)) {
        surv <- 1 - tail(p_vals, 1)
      } else {
        q_low <- q_vals[idx]
        q_high <- q_vals[idx + 1]
        p_low <- p_vals[idx]
        p_high <- p_vals[idx + 1]
        if (!is.finite(q_low) || !is.finite(q_high) || q_high <= q_low) {
          surv <- 1 - p_high
        } else {
          weight <- (tval - q_low) / (q_high - q_low)
          weight <- min(max(weight, 0), 1)
          p_interp <- p_low + weight * (p_high - p_low)
          surv <- 1 - p_interp
        }
      }
      res[i, j] <- surv
    }
  }
  res[res < 0] <- 0
  res[res > 1] <- 1
  if (ncol(res) > 1) {
    res <- t(apply(res, 1, function(row) {
      row[!is.finite(row)] <- NA_real_
      cummin(pmin(pmax(row, 0), 1))
    }))
  }
  res
}

fastml_xgb_aft_predict <- function(fit_obj, predictors, eval_times = NULL, quantile_probs = NULL) {
  if (is.null(fit_obj) || is.null(fit_obj$booster)) {
    return(list(mu = numeric(0), quantiles = NULL, probs = numeric(0), surv = NULL))
  }
  mu_vec <- fastml_xgb_predict_lp(fit_obj, predictors = predictors)
  dist <- tryCatch(fit_obj$aft_distribution, error = function(e) "logistic")
  scale <- tryCatch(as.numeric(fit_obj$aft_scale), error = function(e) 1)
  if (!is.finite(scale) || scale <= 0) {
    scale <- 1
  }
  if (is.null(quantile_probs)) {
    quantile_probs <- tryCatch(fit_obj$aft_quantiles, error = function(e) NULL)
  }
  quantile_probs <- quantile_probs[is.finite(quantile_probs) & quantile_probs > 0 & quantile_probs < 1]
  if (length(quantile_probs) == 0) {
    quantile_probs <- c(0.25, 0.5, 0.75)
  }
  quantile_probs <- sort(unique(as.numeric(quantile_probs)))
  quantile_mat <- fastml_aft_quantile_times(quantile_probs, mu_vec, dist, scale)
  surv_mat <- NULL
  if (!is.null(eval_times)) {
    surv_mat <- fastml_aft_survival_from_quantiles(eval_times, quantile_mat, quantile_probs)
    if (is.null(surv_mat) || ncol(surv_mat) != length(eval_times)) {
      surv_mat <- fastml_aft_survival_prob(eval_times, mu_vec, dist, scale)
    }
  }
  list(mu = mu_vec, quantiles = quantile_mat, probs = quantile_probs, surv = surv_mat)
}

#' Predict risk scores from a survival model
#'
#' Provides a uniform interface for obtaining linear predictors / risk scores
#' from both native survival engines used by fastml (e.g. Cox PH, xgboost Cox)
#' and parsnip/workflow objects.  The function is exported for end users.
#'
#' @param fit A fitted survival model object.
#' @param newdata A data frame of predictors for which to compute risk scores.
#' @param ... Additional arguments passed to methods.
#'
#' @return A numeric vector of risk scores (higher implies greater risk).
#'
#' @export
predict_risk <- function(fit, newdata, ...) {
  UseMethod("predict_risk")
}

#' @export
predict_risk.fastml_native_survival <- function(fit, newdata, ...) {
  if (missing(newdata)) {
    stop("Please supply 'newdata' when calling predict_risk().")
  }
  baked <- tryCatch(recipes::bake(fit$recipe, new_data = newdata), error = function(e) newdata)
  predictors <- fastml_prepare_native_survival_predictors(fit, baked, newdata)
  if (inherits(fit$fit, "coxph")) {
    res <- tryCatch(stats::predict(fit$fit, newdata = predictors, type = "lp"), error = function(e) NULL)
    return(as.numeric(res))
  }
  if (inherits(fit$fit, "survreg")) {
    res <- tryCatch(stats::predict(fit$fit, newdata = predictors, type = "lp"), error = function(e) NULL)
    return(as.numeric(res))
  }
  if (inherits(fit$fit, "glmnet")) {
    if (!requireNamespace("glmnet", quietly = TRUE)) {
      return(rep(NA_real_, nrow(predictors)))
    }
    feature_names <- fit$feature_names
    x_terms <- fit$x_terms
    x_contrasts <- fit$x_contrasts
    prepare_glmnet_newx <- function(new_data) {
      if (is.null(new_data) || nrow(new_data) == 0) {
        return(matrix(0, nrow = ifelse(is.null(new_data), 0, nrow(new_data)), ncol = length(feature_names)))
      }
      mm <- tryCatch({
        if (!is.null(x_terms)) {
          stats::model.matrix(x_terms, new_data, contrasts.arg = x_contrasts)
        } else {
          stats::model.matrix(~ . - 1, data = new_data)
        }
      }, error = function(e) NULL)
      if (is.null(mm)) {
        mm <- tryCatch(stats::model.matrix(~ . - 1, data = new_data), error = function(e) NULL)
      }
      if (is.null(mm)) {
        mm <- matrix(0, nrow = nrow(new_data), ncol = 0)
      }
      mm <- as.matrix(mm)
      if (!is.null(colnames(mm)) && any(colnames(mm) == "(Intercept)")) {
        keep_cols <- colnames(mm) != "(Intercept)"
        mm <- mm[, keep_cols, drop = FALSE]
      }
      mm_full <- matrix(0, nrow = nrow(mm), ncol = length(feature_names))
      colnames(mm_full) <- feature_names
      overlap <- intersect(feature_names, colnames(mm))
      if (length(overlap) > 0) {
        mm_full[, overlap] <- mm[, overlap, drop = FALSE]
      }
      mm_full
    }
    mm <- prepare_glmnet_newx(predictors)
    penalty <- fit$penalty
    res <- tryCatch(glmnet::predict(fit$fit, newx = mm, s = penalty, type = "link"), error = function(e) NULL)
    if (!is.null(res) && length(dim(res)) >= 2) {
      res <- as.matrix(res)[, 1, drop = TRUE]
    }
    if (!is.null(res)) {
      return(as.numeric(res))
    }
    coef_mat <- tryCatch(glmnet::coef(fit$fit, s = penalty), error = function(e) NULL)
    if (is.null(coef_mat)) {
      return(rep(NA_real_, nrow(predictors)))
    }
    coef_dense <- as.matrix(coef_mat)
    intercept <- 0
    if ("(Intercept)" %in% rownames(coef_dense)) {
      intercept <- coef_dense["(Intercept)", 1]
      coef_dense <- coef_dense[setdiff(rownames(coef_dense), "(Intercept)"), , drop = FALSE]
    }
    coef_vec <- numeric(length(feature_names))
    names(coef_vec) <- feature_names
    overlap <- intersect(feature_names, rownames(coef_dense))
    if (length(overlap) > 0) {
      coef_vec[overlap] <- coef_dense[overlap, 1]
    }
    drop(as.numeric(mm %*% coef_vec + intercept))
  } else if (inherits(fit$fit, c("stpm2", "pstpm2"))) {
    if (!requireNamespace("rstpm2", quietly = TRUE)) {
      return(rep(NA_real_, nrow(predictors)))
    }
    base_newdata <- predictors
    time_var <- fit$time_col
    if (!is.null(time_var)) {
      if (!(time_var %in% names(base_newdata)) && time_var %in% names(newdata)) {
        base_newdata[[time_var]] <- newdata[[time_var]]
      }
      if (!(time_var %in% names(base_newdata))) {
        base_newdata[[time_var]] <- rep(1, nrow(base_newdata))
      }
    }
    return(as.numeric(rstpm2::predict(fit$fit, newdata = base_newdata, type = "lp")))
  } else if (inherits(fit$fit, "fastml_xgb_survival")) {
    lp <- fastml_xgb_predict_lp(fit$fit, predictors)
    if (identical(fit$fit$objective, "survival:aft")) {
      return(as.numeric(-lp))
    }
    return(as.numeric(lp))
  }
  rep(NA_real_, nrow(predictors))
}

#' @export
predict_risk.workflow <- function(fit, newdata, ...) {
  if (missing(newdata)) {
    stop("Please supply 'newdata' when calling predict_risk().")
  }
  pred_lp <- tryCatch(predict(fit, new_data = newdata, type = "linear_pred"), error = function(e) NULL)
  if (is.null(pred_lp)) {
    pred_lp <- tryCatch(predict(fit, new_data = newdata, type = "lp"), error = function(e) NULL)
  }
  if (is.null(pred_lp)) {
    pred_lp <- tryCatch(predict(fit, new_data = newdata, type = "risk"), error = function(e) NULL)
  }
  if (is.null(pred_lp)) {
    stop("Unable to compute risk predictions for this workflow. Ensure the engine supports 'linear_pred' or 'lp'.")
  }
  if (is.data.frame(pred_lp)) {
    if (".pred" %in% names(pred_lp)) {
      return(as.numeric(pred_lp$.pred))
    }
    if (".pred_linear_pred" %in% names(pred_lp)) {
      return(as.numeric(pred_lp$.pred_linear_pred))
    }
  }
  as.numeric(pred_lp)
}

#' @export
predict_risk.default <- function(fit, newdata, ...) {
  stop("predict_risk() is not implemented for objects of class ", paste(class(fit), collapse = ", "), ".")
}

#' Predict survival probabilities from a survival model
#'
#' @param fit A fitted survival model.
#' @param newdata A data frame of predictors for which to compute survival curves.
#' @param times Numeric vector of evaluation times.
#' @param ... Additional arguments passed to methods.
#'
#' @return A numeric matrix with one row per observation and one column per time.
#'
#' @export
predict_survival <- function(fit, newdata, times, ...) {
  UseMethod("predict_survival")
}

fastml_align_survival_output <- function(pred_obj, times, n) {
  if (is.null(pred_obj)) {
    return(matrix(NA_real_, nrow = n, ncol = length(times)))
  }
  if (is.matrix(pred_obj)) {
    mat <- pred_obj
    if (nrow(mat) != n) {
      stop("Returned matrix has an unexpected number of rows.")
    }
    if (ncol(mat) != length(times)) {
      if (length(times) == 1 && ncol(mat) == 1) {
        colnames(mat) <- NULL
      } else if (!is.null(colnames(mat))) {
        idx <- match(times, suppressWarnings(as.numeric(colnames(mat))))
        if (anyNA(idx)) {
          stop("Could not align survival predictions with requested times.")
        }
        mat <- mat[, idx, drop = FALSE]
      }
    }
    return(as.matrix(mat))
  }
  if (is.data.frame(pred_obj) && ".pred_survival" %in% names(pred_obj)) {
    lst <- pred_obj$.pred_survival
    res <- matrix(NA_real_, nrow = n, ncol = length(times))
    for (i in seq_along(lst)) {
      entry <- lst[[i]]
      if (is.null(entry)) next
      time_col <- intersect(c(".eval_time", "eval_time", "time"), names(entry))
      surv_col <- intersect(c(".pred", ".survival", "survival", "surv"), names(entry))
      if (length(time_col) == 0 || length(surv_col) == 0) next
      res[i, ] <- stats::approx(entry[[time_col[1]]], entry[[surv_col[1]]], xout = times, method = "constant", rule = 2, f = 0, ties = "ordered")$y
    }
    return(res)
  }
  stop("Unsupported survival prediction output structure.")
}

#' @export
predict_survival.fastml_native_survival <- function(fit, newdata, times, ...) {
  if (missing(newdata)) {
    stop("Please supply 'newdata' when calling predict_survival().")
  }
  if (missing(times)) {
    stop("Please supply 'times' when calling predict_survival().")
  }
  times <- as.numeric(times)
  times <- times[is.finite(times) & times >= 0]
  baked <- tryCatch(recipes::bake(fit$recipe, new_data = newdata), error = function(e) newdata)
  predictors <- fastml_prepare_native_survival_predictors(fit, baked, newdata)
  n <- nrow(newdata)
  if (inherits(fit$fit, "coxph")) {
    base <- tryCatch(survival::basehaz(fit$fit, centered = FALSE), error = function(e) NULL)
    if (is.null(base) && !is.null(fit$baseline)) {
      base <- fit$baseline
      colnames(base) <- c("time", "hazard", "surv")
    }
    lp <- tryCatch(stats::predict(fit$fit, newdata = predictors, type = "lp"), error = function(e) rep(NA_real_, n))
    if (!is.null(base) && nrow(base) > 0) {
      bh <- data.frame(time = base$time, cumhaz = ifelse("hazard" %in% names(base), base$hazard, base$cumhaz))
      helper <- list(baseline = data.frame(time = bh$time, cumhaz = bh$cumhaz), feature_names = colnames(predictors))
      class(helper) <- "fastml_xgb_survival"
      helper$baseline$surv <- exp(-helper$baseline$cumhaz)
      helper$booster <- NULL
      return(fastml_xgb_survival_matrix_cox(helper, lp, times))
    }
    return(matrix(NA_real_, nrow = n, ncol = length(times)))
  }
  if (inherits(fit$fit, "survreg")) {
    lp <- tryCatch(stats::predict(fit$fit, newdata = predictors, type = "lp"), error = function(e) rep(NA_real_, n))
    scale <- tryCatch(fit$fit$scale, error = function(e) 1)
    dist <- tryCatch(fit$fit$dist, error = function(e) "weibull")
    if (identical(dist, "weibull")) {
      lambda <- exp(-lp / scale)
      k <- 1 / scale
      mat <- outer(lambda, times^k, FUN = function(l, t) exp(-l * t))
      mat[mat < 0] <- 0
      mat[mat > 1] <- 1
      if (ncol(mat) > 1) {
        mat <- t(apply(mat, 1, cummin))
      }
      return(mat)
    }
    return(matrix(NA_real_, nrow = n, ncol = length(times)))
  }
  if (inherits(fit$fit, "fastml_xgb_survival")) {
    if (identical(fit$fit$objective, "survival:cox")) {
      stop("XGBoost survival:cox provides risk ranking only; survival curve predictions are unavailable.")
    }
    aft_pred <- fastml_xgb_aft_predict(fit$fit, predictors, eval_times = times)
    if (is.null(aft_pred$surv)) {
      n_obs <- if (is.null(predictors)) 0L else nrow(predictors)
      return(matrix(NA_real_, nrow = n_obs, ncol = length(times)))
    }
    return(aft_pred$surv)
  }
  stop("Survival prediction not implemented for this native engine.")
}

#' @export
predict_survival.workflow <- function(fit, newdata, times, ...) {
  if (missing(newdata)) {
    stop("Please supply 'newdata' when calling predict_survival().")
  }
  if (missing(times)) {
    stop("Please supply 'times' when calling predict_survival().")
  }
  times <- as.numeric(times)
  times <- times[is.finite(times) & times >= 0]
  if (length(times) == 0) {
    stop("No valid evaluation times supplied.")
  }
  pred <- tryCatch(predict(fit, new_data = newdata, type = "survival", eval_time = times), error = function(e) NULL)
  if (is.null(pred) && length(times) == 1) {
    pred <- tryCatch(predict(fit, new_data = newdata, type = "survival", eval_time = times[1]), error = function(e) NULL)
  }
  if (is.null(pred)) {
    stop("Underlying engine does not provide survival curve predictions.")
  }
  fastml_align_survival_output(pred, times, nrow(newdata))
}

#' @export
predict_survival.default <- function(fit, newdata, times, ...) {
  stop("predict_survival() is not implemented for objects of class ", paste(class(fit), collapse = ", "), ".")
}
