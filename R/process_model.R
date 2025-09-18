#' Process and Evaluate a Model Workflow
#'
#' This function processes a fitted model or a tuning result, finalizes the model if tuning was used,
#' makes predictions on the test set, and computes performance metrics depending on the task type
#' (classification or regression). It supports binary and multiclass classification, and handles
#' probabilistic outputs when supported by the modeling engine.
#'
#' @param model_obj A fitted model or a tuning result (`tune_results` object).
#' @param model_id A character identifier for the model (used in warnings).
#' @param task Type of task, either `"classification"`, `"regression"`, or
#'   `"survival"`.
#' @param test_data A data frame containing the test data.
#' @param label The name of the outcome variable (as a character string).
#' @param event_class For binary classification, specifies which class is considered the positive class:
#'   `"first"` or `"second"`.
#' @param engine A character string indicating the model engine (e.g., `"xgboost"`, `"randomForest"`). Used
#'   to determine if class probabilities are supported. If `NULL`, probabilities are skipped.
#' @param train_data A data frame containing the training data, required to refit finalized workflows.
#' @param metric The name of the metric (e.g., `"roc_auc"`, `"accuracy"`, `"rmse"`) used for selecting the best tuning result.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{performance}{A tibble with computed performance metrics.}
#'   \item{predictions}{A tibble with predicted values and corresponding truth values, and probabilities (if applicable).}
#' }
#'
#' @details
#' - If the input `model_obj` is a `tune_results` object, the function finalizes the model using the
#'   best hyperparameters according to the specified `metric`, and refits the model on the full training data.
#'
#' - For classification tasks, performance metrics include accuracy, kappa, sensitivity, specificity, precision,
#'   F1-score, and ROC AUC (if probabilities are available).
#'
#' - For regression tasks, RMSE, R-squared, and MAE are returned.
#'
#' - For models with missing prediction lengths, a helpful imputation error is thrown to guide data preprocessing.
#'
#' @export

process_model <- function(model_obj, model_id, task, test_data, label, event_class,
                          engine, train_data, metric) {
  # If the model object is a tuning result, finalize the workflow
  if (inherits(model_obj, "tune_results")) {
    best_params <- tryCatch({
      tune::select_best(model_obj, metric = metric)
    }, error = function(e) {
      warning(paste("Could not select best parameters for model", model_id, ":", e$message))
      return(NULL)
    })
    if (is.null(best_params)) return(NULL)

    model_spec <- workflows::pull_workflow_spec(model_obj)
    model_recipe <- workflows::pull_workflow_preprocessor(model_obj)

    final_model_spec <- tune::finalize_model(model_spec, best_params)
    final_workflow <- workflows::workflow() %>%
      workflows::add_recipe(model_recipe) %>%
      workflows::add_model(final_model_spec)

    final_model <- parsnip::fit(final_workflow, data = train_data)
  } else if (inherits(model_obj, "fastml_native_survival")) {
    # Native survival model fitted outside parsnip/workflows
    final_model <- model_obj
  } else {
    # Otherwise, assume the model is already a fitted workflow
    final_model <- model_obj
  }

  # Make predictions and compute performance metrics
  if (task == "classification") {

    pred_class <- predict(final_model, new_data = test_data, type = "class")$.pred_class


    if (!is.null(engine) && !is.na(engine) && engine != "LiblineaR") {
      pred_prob <- predict(final_model, new_data = test_data, type = "prob")
    }



    if(nrow(test_data) != length(pred_class)) {
      stop('The dataset has missing values. To handle this, set impute_method = "remove" to delete rows with missing values,
             or use an imputation method such as "medianImpute" to fill missing values with the column median, "knnImpute" to
             estimate missing values using k-Nearest Neighbors, "bagImpute" to apply bagging for imputation, "mice" to use
             Multiple Imputation by Chained Equations, or "missForest" to use random forests for imputation.')
    }

    data_metrics <- test_data %>%
      dplyr::select(truth = !!rlang::sym(label)) %>%
      dplyr::mutate(estimate = pred_class) %>%
      {
        if (!is.null(engine) && !is.na(engine) && engine != "LiblineaR") {
          dplyr::bind_cols(., pred_prob)
        } else {
          .
        }
      }

    if(all(grepl("^\\.pred_p", names(data_metrics)[3:4]))){

      pred_name = ".pred_p"
    }else{

      pred_name = ".pred_"
    }

    num_classes <- length(unique(data_metrics$truth))

    if (num_classes == 2) {
      # Determine the positive class based on event_class parameter
      if(event_class == "first"){
        positive_class <- levels(data_metrics$truth)[1]
      } else if(event_class == "second"){
        positive_class <- levels(data_metrics$truth)[2]
      } else {
        stop("Invalid event_class argument. It should be either 'first' or 'second'.")
      }

      # Compute standard classification metrics
      metrics_class <- yardstick::metric_set(
        yardstick::accuracy,
        yardstick::kap,
        yardstick::sens,
        yardstick::spec,
        yardstick::precision,
        yardstick::f_meas
      )
      perf_class <- metrics_class(data_metrics, truth = truth, estimate = estimate, event_level = event_class)

      if (!is.null(engine) && !is.na(engine) && engine != "LiblineaR") {
        # Compute ROC AUC using the probability column for the positive class
        roc_auc_value <- yardstick::roc_auc(
          data_metrics,
          truth = truth,
          !!rlang::sym(paste0(pred_name, positive_class)),
          event_level = "second"
        )
        if(roc_auc_value$.estimate < 0.50) {
          roc_auc_value <- yardstick::roc_auc(
            data_metrics,
            truth = truth,
            !!rlang::sym(paste0(pred_name, positive_class)),
            event_level = "first"
          )
        }
        perf <- dplyr::bind_rows(perf_class, roc_auc_value)
      }else{

        perf <- perf_class
      }
    }else {
      # Multiclass classification (using macro averaging)
      metrics_class <- yardstick::metric_set(
        yardstick::accuracy,
        yardstick::kap,
        yardstick::sens,
        yardstick::spec,
        yardstick::precision,
        yardstick::f_meas
      )
      perf_class <- metrics_class(
        data_metrics,
        truth = truth,
        estimate = estimate,
        estimator = "macro"
      )

      if (!is.null(engine) && !is.na(engine) && engine != "LiblineaR") {
        prob_cols <- names(pred_prob)
        perf_roc_auc <- yardstick::roc_auc(
          data_metrics,
          truth = truth,
          !!!rlang::syms(prob_cols),
          estimator = "macro_weighted"
        )
        perf <- dplyr::bind_rows(perf_class, perf_roc_auc)
      } else {
        perf <- perf_class
      }
    }
  } else if (task == "survival") {
    time_col <- label[1]
    status_col <- label[2]
    surv_obj <- survival::Surv(test_data[[time_col]], test_data[[status_col]])

    # Prepare data for prediction depending on model type
    pred_new_data <- test_data
    if (inherits(final_model, "fastml_native_survival")) {
      # Use baked test data from the stored recipe
      pred_new_data <- tryCatch(
        recipes::bake(final_model$recipe, new_data = test_data),
        error = function(e) test_data
      )
    }

    pred_predictors <- NULL
    if (inherits(final_model, "fastml_native_survival")) {
      pred_predictors <- as.data.frame(pred_new_data)
      drop_cols <- c(final_model$response, final_model$time_col,
                     final_model$status_col, final_model$start_col)
      drop_cols <- unique(drop_cols[!is.na(drop_cols)])
      drop_cols <- intersect(drop_cols, names(pred_predictors))
      if (length(drop_cols) > 0) {
        keep_cols <- setdiff(names(pred_predictors), drop_cols)
        if (is.null(keep_cols)) keep_cols <- character(0)
        pred_predictors <- pred_predictors[, keep_cols, drop = FALSE]
      }
    }

    extract_pred <- function(pred) {
      # Return early if NULL
      if (is.null(pred)) {
        return(pred)
      }

      # If a tibble/data.frame, pick the appropriate column first
      if (is.data.frame(pred)) {
        if (".pred" %in% names(pred)) {
          pred <- pred[[".pred"]]
        } else if (".pred_survival" %in% names(pred)) {
          pred <- pred[[".pred_survival"]]
        } else {
          pred <- pred[[1]]
        }
      }

      # If a list (including list-column extracted from tibble), unlist robustly
      # Engines for survival often return a list-column where each element is a
      # numeric of length 1 when a single eval_time is provided.
      if (is.list(pred)) {
        pred <- vapply(pred, function(x) {
          if (is.null(x)) return(NA_real_)
          # If it's a scalar numeric, take it; otherwise try first element
          if (is.numeric(x) && length(x) == 1L) return(as.numeric(x))
          if (is.atomic(x) && length(x) >= 1L) return(as.numeric(x[[1L]]))
          # If it's a list wrapping a scalar, try to dig one level
          if (is.list(x) && length(x) >= 1L) {
            x1 <- x[[1L]]
            if (is.numeric(x1) && length(x1) >= 1L) return(as.numeric(x1[[1L]]))
          }
          NA_real_
        }, numeric(1))
      }

      # Coerce remaining vector-like objects to numeric
      as.numeric(pred)
    }

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
        res <- matrix(NA_real_, nrow = n_curves, ncol = length(eval_times))
        for (j in seq_len(n_curves)) {
          res[j, ] <- align_survival_curve(surv_times, surv_vals[, j], eval_times)
        }
        if (n_curves != n_obs) {
          if (n_curves == 1 && n_obs > 1) {
            res <- matrix(res[1, ], nrow = n_obs, ncol = length(eval_times), byrow = TRUE)
          } else {
            res <- res[seq_len(min(n_curves, n_obs)), , drop = FALSE]
            if (n_curves < n_obs) {
              res <- rbind(res, matrix(NA_real_, nrow = n_obs - n_curves, ncol = length(eval_times)))
            }
          }
        }
        return(res)
      }
      if (is.numeric(surv_vals)) {
        curve <- align_survival_curve(surv_times, surv_vals, eval_times)
        return(matrix(rep(curve, each = n_obs), nrow = n_obs, ncol = length(eval_times)))
      }
      NULL
    }

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
      }, error = function(e) NULL)

      if (is.null(model_frame) || nrow(model_frame) == 0) {
        return(NULL)
      }

      offset_vals <- tryCatch(stats::model.offset(model_frame), error = function(e) NULL)
      if (length(offset_vals) == 0 || all(is.na(offset_vals))) {
        offset_vals <- rep(0, nrow(model_frame))
      } else {
        offset_vals <- as.numeric(offset_vals)
        offset_vals[is.na(offset_vals)] <- 0
      }

      model_matrix <- tryCatch({
        stats::model.matrix(fit_obj, model_frame)
      }, error = function(e) NULL)

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
              scale_lookup <- rep(scale_lookup, length.out = max(strata_index, na.rm = TRUE))
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
        return(matrix(numeric(0), nrow = 0, ncol = length(eval_times)))
      }

      finite_scale <- scale_vec[is.finite(scale_vec) & scale_vec > 0]
      fallback_scale <- if (length(finite_scale) > 0) stats::median(finite_scale) else 1
      if (length(scale_vec) != n_obs || any(!is.finite(scale_vec) | scale_vec <= 0)) {
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
            survival::psurvreg(q_vec, mean = lp, scale = scale_vec, distribution = dist_name)
          } else {
            survival::psurvreg(q_vec, mean = lp, scale = scale_vec, distribution = dist_name, parms = parms)
          }
        }, error = function(e) rep(NA_real_, n_obs))

        surv_vals <- 1 - surv_vals
        surv_vals[!is.finite(surv_vals)] <- NA_real_
        res[, j] <- pmin(pmax(surv_vals, 0), 1)
      }

      res
    }

    convert_survival_predictions <- function(pred_obj, eval_times, n_obs) {
      if (is.null(pred_obj) || length(eval_times) == 0 || n_obs == 0) {
        return(NULL)
      }

      if (is.matrix(pred_obj)) {
        if (nrow(pred_obj) == n_obs && ncol(pred_obj) == length(eval_times)) {
          return(as.matrix(pred_obj))
        }
        if (ncol(pred_obj) == n_obs && nrow(pred_obj) == length(eval_times)) {
          return(t(pred_obj))
        }
      }

      if (is.numeric(pred_obj) && length(pred_obj) == n_obs * length(eval_times)) {
        return(matrix(pred_obj, nrow = n_obs, ncol = length(eval_times)))
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
        if (is.numeric(extract_list) && length(extract_list) == n_obs * length(eval_times)) {
          return(matrix(extract_list, nrow = n_obs, ncol = length(eval_times)))
        }
        if (is.numeric(extract_list) && length(extract_list) == n_obs) {
          return(matrix(extract_list, nrow = n_obs, ncol = 1))
        }
        return(NULL)
      }

      res <- matrix(NA_real_, nrow = n_obs, ncol = length(eval_times))
      max_iter <- min(length(extract_list), n_obs)
      for (i in seq_len(max_iter)) {
        entry <- extract_list[[i]]
        if (is.null(entry)) next
        if (is.data.frame(entry)) {
          time_col <- intersect(c(".eval_time", ".time", "time"), names(entry))
          surv_col <- intersect(c(".survival", "survival", ".pred_survival"), names(entry))
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
            surv_col <- intersect(c(".survival", "survival", ".pred_survival"), names(inner))
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

    compute_ibrier <- function(eval_times, surv_mat, time_vec, status_vec) {
      n <- length(time_vec)
      m <- length(eval_times)
      if (n == 0 || m == 0 || is.null(surv_mat) || nrow(surv_mat) != n || ncol(surv_mat) != m) {
        return(list(ibs = NA_real_, curve = rep(NA_real_, m)))
      }

      status_vec <- ifelse(is.na(status_vec), 0, status_vec)
      status_vec <- ifelse(status_vec > 0, 1, 0)
      time_vec <- as.numeric(time_vec)

      valid_idx <- which(!is.na(time_vec) & !is.na(status_vec))
      if (length(valid_idx) == 0) {
        return(list(ibs = NA_real_, curve = rep(NA_real_, m)))
      }

      censor_indicator <- 1 - status_vec[valid_idx]
      censor_fit <- tryCatch({
        survival::survfit(survival::Surv(time_vec[valid_idx], censor_indicator) ~ 1)
      }, error = function(e) NULL)
      if (is.null(censor_fit)) {
        return(list(ibs = NA_real_, curve = rep(NA_real_, m)))
      }

      censor_eval <- function(times) {
        if (length(times) == 0) return(numeric(0))
        if (length(censor_fit$time) == 0 || length(censor_fit$surv) == 0) {
          return(rep(1, length(times)))
        }
        align_survival_curve(censor_fit$time, censor_fit$surv, times)
      }

      G_t <- censor_eval(eval_times)
      G_t[!is.finite(G_t) | G_t <= 0] <- NA_real_

      time_minus <- pmax(time_vec - 1e-08, 0)
      G_time_minus <- censor_eval(time_minus)
      G_time_minus[!is.finite(G_time_minus) | G_time_minus <= 0] <- NA_real_

      weights <- matrix(0, nrow = n, ncol = m)
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
      weighted <- weights * residual^2
      weighted[!is.finite(weighted)] <- 0
      bs_t <- colSums(weighted, na.rm = TRUE) / n
      bs_t[!is.finite(bs_t)] <- NA_real_

      valid_bs <- which(is.finite(bs_t) & is.finite(eval_times))
      if (length(valid_bs) >= 2) {
        times_valid <- eval_times[valid_bs]
        bs_valid <- bs_t[valid_bs]
        times_aug <- c(0, times_valid)
        bs_aug <- c(0, bs_valid)
        area <- sum(diff(times_aug) * (head(bs_aug, -1) + tail(bs_aug, -1)) / 2)
        tau <- max(times_valid)
        ibs <- if (tau > 0) area / tau else NA_real_
      } else if (length(valid_bs) == 1) {
        ibs <- bs_t[valid_bs]
      } else {
        ibs <- NA_real_
      }

      list(ibs = ibs, curve = bs_t)
    }

    risk <- tryCatch({
      if (inherits(final_model, "fastml_native_survival")) {
        # Prepare predictor-only data for native survival fits
        if (is.null(pred_predictors)) {
          pred_predictors <- data.frame()
        }
        if (inherits(final_model$fit, "coxph")) {
          as.numeric(stats::predict(final_model$fit, newdata = pred_predictors, type = "lp"))
        } else if (inherits(final_model$fit, "survreg")) {
          as.numeric(stats::predict(final_model$fit, newdata = pred_predictors, type = "lp"))
        } else if (inherits(final_model$fit, "glmnet")) {
          if (!requireNamespace("glmnet", quietly = TRUE)) {
            rep(NA_real_, nrow(test_data))
          } else {
            feature_names <- final_model$feature_names
            x_terms <- final_model$x_terms
            x_contrasts <- final_model$x_contrasts
            n_obs <- if (is.null(pred_predictors)) 0L else nrow(pred_predictors)

            if (is.null(feature_names) || length(feature_names) == 0) {
              rep(NA_real_, nrow(test_data))
            } else if (n_obs == 0) {
              numeric(0)
            } else {
              prepare_glmnet_newx <- function(new_data) {
                if (is.null(new_data)) {
                  new_data <- data.frame()
                }
                n_local <- nrow(new_data)
                if (n_local == 0) {
                  out <- matrix(0, nrow = 0, ncol = length(feature_names))
                  colnames(out) <- feature_names
                  return(out)
                }
                if (ncol(new_data) == 0) {
                  mm <- matrix(0, nrow = n_local, ncol = 0)
                } else {
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
                    mm <- matrix(0, nrow = n_local, ncol = 0)
                  }
                }
                mm <- as.matrix(mm)
                mm_colnames <- colnames(mm)
                if (!is.null(mm_colnames) && any(mm_colnames == "(Intercept)")) {
                  keep_cols <- mm_colnames != "(Intercept)"
                  mm <- mm[, keep_cols, drop = FALSE]
                  mm_colnames <- colnames(mm)
                }
                mm_full <- matrix(0, nrow = n_local, ncol = length(feature_names))
                colnames(mm_full) <- feature_names
                if (ncol(mm) > 0) {
                  overlap <- intersect(feature_names, mm_colnames)
                  if (length(overlap) > 0) {
                    mm_full[, overlap] <- mm[, overlap, drop = FALSE]
                  }
                }
                mm_full[!is.finite(mm_full)] <- 0
                mm_full
              }

              mm_full <- prepare_glmnet_newx(pred_predictors)
              n_obs <- nrow(mm_full)
              penalty <- final_model$penalty
              pred_lp <- tryCatch({
                glmnet::predict(final_model$fit, newx = mm_full,
                                 s = penalty, type = "link")
              }, error = function(e) NULL)
              if (!is.null(pred_lp) && length(dim(pred_lp)) >= 2) {
                pred_lp <- as.matrix(pred_lp)[, 1, drop = TRUE]
              }
              if (!is.null(pred_lp)) {
                pred_lp <- as.numeric(pred_lp)
              }

              needs_manual_lp <- is.null(pred_lp) || length(pred_lp) == 0 || any(!is.finite(pred_lp))

              if (needs_manual_lp) {
                coef_mat <- tryCatch({
                  glmnet::coef(final_model$fit, s = penalty)
                }, error = function(e) NULL)

                if (!is.null(coef_mat)) {
                  coef_dense <- as.matrix(coef_mat)
                  intercept <- 0
                  if ("(Intercept)" %in% rownames(coef_dense)) {
                    intercept <- coef_dense["(Intercept)", 1]
                    coef_dense <- coef_dense[setdiff(rownames(coef_dense), "(Intercept)"), , drop = FALSE]
                  }

                  coef_vec <- numeric(length(feature_names))
                  names(coef_vec) <- feature_names
                  coef_overlap <- intersect(feature_names, rownames(coef_dense))
                  if (length(coef_overlap) > 0) {
                    coef_vals <- coef_dense[coef_overlap, 1]
                    coef_vals[!is.finite(coef_vals)] <- 0
                    coef_vec[coef_overlap] <- coef_vals
                  }
                  if (!is.finite(intercept)) {
                    intercept <- 0
                  }
                  pred_lp <- as.numeric(mm_full %*% coef_vec + intercept)
                } else {
                  pred_lp <- rep(NA_real_, n_obs)
                }
              }

              pred_lp
            }
          }
        } else {
          rep(NA_real_, nrow(test_data))
        }
      } else {
        extract_pred(predict(final_model, new_data = test_data, type = "linear_pred"))
      }
    }, error = function(e) {
      if (inherits(final_model, "fastml_native_survival")) {
        rep(NA_real_, nrow(test_data))
      } else {
        extract_pred(predict(final_model, new_data = test_data))
      }
    })

    train_times <- as.numeric(train_data[[time_col]])
    test_times <- as.numeric(test_data[[time_col]])
    t0 <- stats::median(train_times, na.rm = TRUE)

    combined_times <- c(train_times, test_times)
    combined_times <- combined_times[is.finite(combined_times) & combined_times > 0]
    if (length(combined_times) > 0) {
      eval_times <- sort(unique(combined_times))
      if (length(eval_times) > 200) {
        probs <- seq(0, 1, length.out = 200)
        eval_times <- sort(unique(as.numeric(stats::quantile(eval_times, probs = probs, na.rm = TRUE, type = 1))))
      }
    } else {
      eval_times <- numeric(0)
    }
    if (is.finite(t0) && t0 > 0) {
      eval_times <- sort(unique(c(eval_times, t0)))
    }

    normalize_status <- function(status_vec) {
      if (is.null(status_vec)) {
        return(rep(0, length(test_times)))
      }
      if (is.factor(status_vec) || is.character(status_vec)) {
        numeric_version <- suppressWarnings(as.numeric(as.character(status_vec)))
        if (!all(is.na(numeric_version))) {
          status_vec <- numeric_version
        } else {
          status_levels <- unique(status_vec[!is.na(status_vec)])
          if (length(status_levels) <= 1) {
            return(rep(0, length(status_vec)))
          }
          event_level <- status_levels[length(status_levels)]
          return(ifelse(is.na(status_vec), 0, ifelse(status_vec == event_level, 1, 0)))
        }
      }
      status_vec <- as.numeric(status_vec)
      unique_vals <- sort(unique(status_vec[!is.na(status_vec)]))
      if (length(unique_vals) == 0) {
        rep(0, length(status_vec))
      } else if (min(unique_vals) <= 0) {
        ifelse(is.na(status_vec), 0, ifelse(status_vec > 0, 1, 0))
      } else {
        event_val <- max(unique_vals)
        ifelse(is.na(status_vec), 0, ifelse(status_vec == event_val, 1, 0))
      }
    }

    surv_matrix_vals <- tryCatch(as.matrix(surv_obj), error = function(e) NULL)
    if (!is.null(surv_matrix_vals)) {
      if ("time" %in% colnames(surv_matrix_vals)) {
        obs_time <- as.numeric(surv_matrix_vals[, "time"])
      } else if ("time2" %in% colnames(surv_matrix_vals)) {
        obs_time <- as.numeric(surv_matrix_vals[, "time2"])
      } else {
        obs_time <- test_times
      }
      if ("status" %in% colnames(surv_matrix_vals)) {
        status_event <- as.numeric(surv_matrix_vals[, "status"])
      } else {
        status_event <- normalize_status(test_data[[status_col]])
      }
    } else {
      obs_time <- test_times
      status_event <- normalize_status(test_data[[status_col]])
    }
    status_event[is.na(status_event)] <- 0

    n_obs <- nrow(test_data)
    surv_prob_mat <- NULL
    if (length(eval_times) > 0 && n_obs > 0) {
      if (inherits(final_model, "fastml_native_survival")) {
        newdata_survfit <- pred_predictors
        if (is.null(newdata_survfit) || nrow(newdata_survfit) != n_obs) {
          newdata_survfit <- tryCatch({
            if (is.null(pred_new_data)) {
              NULL
            } else {
              drop_cols <- c(final_model$response, final_model$time_col, final_model$status_col, final_model$start_col)
              drop_cols <- drop_cols[!is.na(drop_cols)]
              keep_cols <- setdiff(names(pred_new_data), drop_cols)
              pred_new_data[, keep_cols, drop = FALSE]
            }
          }, error = function(e) NULL)
        }
        if (inherits(final_model$fit, "survreg")) {
          surv_prob_mat <- compute_survreg_matrix(final_model$fit, newdata_survfit, eval_times)
          if (is.null(surv_prob_mat)) {
            surv_fit <- tryCatch(
              survival::survfit(final_model$fit, newdata = newdata_survfit),
              error = function(e) NULL
            )
            surv_prob_mat <- build_survfit_matrix(surv_fit, eval_times, n_obs)
          }
        } else {
          surv_fit <- tryCatch(
            survival::survfit(final_model$fit, newdata = newdata_survfit),
            error = function(e) NULL
          )
          surv_prob_mat <- build_survfit_matrix(surv_fit, eval_times, n_obs)
        }
      } else {
        surv_pred <- tryCatch(
          predict(final_model, new_data = test_data, type = "survival", eval_time = eval_times),
          error = function(e) NULL
        )
        if (is.null(surv_pred) && length(eval_times) == 1) {
          surv_pred <- tryCatch(
            predict(final_model, new_data = test_data, type = "survival", eval_time = eval_times[1]),
            error = function(e) NULL
          )
        }
        surv_prob_mat <- convert_survival_predictions(surv_pred, eval_times, n_obs)
      }
    }

    surv_prob <- rep(NA_real_, n_obs)
    brier <- NA_real_
    brier_curve <- rep(NA_real_, length(eval_times))
    if (!is.null(surv_prob_mat) && nrow(surv_prob_mat) == n_obs && ncol(surv_prob_mat) == length(eval_times) && length(eval_times) > 0) {
      idx_t0 <- 1L
      if (is.finite(t0) && t0 > 0) {
        idx_t0 <- which.min(abs(eval_times - t0))
      }
      idx_t0 <- max(1L, min(idx_t0, ncol(surv_prob_mat)))
      surv_prob <- as.numeric(surv_prob_mat[, idx_t0])
      ibs_res <- compute_ibrier(eval_times, surv_prob_mat, obs_time, status_event)
      brier <- ibs_res$ibs
      brier_curve <- ibs_res$curve
    }

    surv_curve_list <- vector("list", n_obs)
    if (!is.null(surv_prob_mat) && nrow(surv_prob_mat) == n_obs && ncol(surv_prob_mat) == length(eval_times) && length(eval_times) > 0) {
      formatted_times <- format(eval_times, trim = TRUE, scientific = FALSE)
      for (i in seq_len(n_obs)) {
        row_vals <- as.numeric(surv_prob_mat[i, ])
        names(row_vals) <- formatted_times
        surv_curve_list[[i]] <- row_vals
      }
    }
    attr(surv_curve_list, "eval_times") <- eval_times

    # Try to obtain predicted survival time where supported
    surv_time <- tryCatch({
      if (inherits(final_model, "fastml_native_survival") && requireNamespace("censored", quietly = TRUE)) {
        if (inherits(final_model$fit, "coxph")) {
          as.numeric(censored::survival_time_coxph(final_model$fit, pred_predictors))
        } else if (inherits(final_model$fit, "survbagg")) {
          as.numeric(censored::survival_time_survbagg(final_model$fit, pred_predictors))
        } else if (inherits(final_model$fit, "mboost")) {
          as.numeric(censored::survival_time_mboost(final_model$fit, pred_predictors))
        } else {
          rep(NA_real_, nrow(test_data))
        }
      } else {
        # For workflow-based models, rely on engine-specific predict; not all expose time directly
        rep(NA_real_, nrow(test_data))
      }
    }, error = function(e) rep(NA_real_, nrow(test_data)))


    risk_valid <- is.finite(risk)
    c_index <- NA_real_
    logrank_p <- NA_real_
    risk_group <- rep(NA_character_, length(risk))

    if (any(risk_valid)) {
      risk_vals <- risk[risk_valid]
      surv_valid <- surv_obj[risk_valid]
      if (length(unique(risk_vals)) > 1) {
        c_index <- tryCatch(
          survival::concordance(surv_valid ~ risk_vals)$concordance,
          error = function(e) NA_real_
        )
        risk_threshold <- stats::median(risk_vals)
        risk_group_vals <- ifelse(risk_vals > risk_threshold, "high", "low")
        risk_group[risk_valid] <- risk_group_vals
        if (length(unique(risk_group_vals)) > 1) {
          lr <- tryCatch(
            survival::survdiff(surv_valid ~ factor(risk_group_vals, levels = c("low", "high"))),
            error = function(e) NULL
          )
          if (!is.null(lr)) {
            logrank_p <- 1 - stats::pchisq(lr$chisq, length(lr$n) - 1)
          }
        }
      } else {
        risk_group[risk_valid] <- "low"
      }
    } else {
      warning(sprintf("Model %s produced no finite risk predictions; survival metrics set to NA.", model_id))
    }

    perf <- tibble::tibble(
      .metric = c("c_index", "brier_score", "logrank_p"),
      .estimate = c(c_index, brier, logrank_p)
    )

    data_metrics <- tibble::tibble(
      time = test_data[[time_col]],
      status = test_data[[status_col]],
      risk = risk,
      surv_prob = surv_prob,
      surv_time = surv_time,
      surv_prob_curve = surv_curve_list
    )
    attr(data_metrics, "eval_times") <- eval_times
    attr(data_metrics, "brier_curve") <- tibble::tibble(eval_time = eval_times, brier = brier_curve)

  } else {
    # Regression task
    predictions <- predict(final_model, new_data = test_data)
    pred <- predictions$.pred
    data_metrics <- tibble::tibble(truth = test_data[[label]], estimate = pred)
    metrics_set <- yardstick::metric_set(yardstick::rmse, yardstick::rsq, yardstick::mae)
    perf <- metrics_set(data_metrics, truth = truth, estimate = estimate)
  }

  return(list(performance = perf, predictions = data_metrics))
}

