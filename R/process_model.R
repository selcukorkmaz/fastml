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
#' @param eval_times_user Optional numeric vector of time horizons at which to
#'   evaluate survival Brier scores. When `NULL`, sensible defaults based on the
#'   observed follow-up distribution are used.
#' @param bootstrap_ci Logical; if `TRUE`, bootstrap confidence intervals are
#'   estimated for survival performance metrics.
#' @param bootstrap_samples Integer giving the number of bootstrap resamples
#'   used when computing confidence intervals.
#' @param bootstrap_seed Optional integer seed applied before bootstrap
#'   resampling to make interval estimates reproducible.
#' @param at_risk_threshold Numeric value between 0 and 1 defining the minimum
#'   proportion of subjects required to remain at risk when determining the
#'   maximum follow-up time used in survival metrics.
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

process_model <- function(model_obj,
                          model_id,
                          task,
                          test_data,
                          label,
                          event_class,
                          start_col = NULL,
                          time_col = NULL,
                          status_col = NULL,
                          engine,
                          train_data,
                          metric,
                          eval_times_user = NULL,
                          bootstrap_ci = TRUE,
                          bootstrap_samples = 500,
                          bootstrap_seed = 1234,
                          at_risk_threshold = 0.1) {
  # If the model object is a tuning result, finalize the workflow
  if (inherits(model_obj, "tune_results")) {
    best_params <- tryCatch({
      tune::select_best(model_obj, metric = metric)
    }, error = function(e) {
      warning(paste(
        "Could not select best parameters for model",
        model_id,
        ":",
        e$message
      ))
      return(NULL)
    })
    if (is.null(best_params))
      return(NULL)

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


    if (!is.null(engine) &&
        !is.na(engine) && engine != "LiblineaR") {
      pred_prob <- predict(final_model, new_data = test_data, type = "prob")
    }



    if (nrow(test_data) != length(pred_class)) {
      stop(
        'The dataset has missing values. To handle this, set impute_method = "remove" to delete rows with missing values,
             or use an imputation method such as "medianImpute" to fill missing values with the column median, "knnImpute" to
             estimate missing values using k-Nearest Neighbors, "bagImpute" to apply bagging for imputation, "mice" to use
             Multiple Imputation by Chained Equations, or "missForest" to use random forests for imputation.'
      )
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

    if (all(grepl("^\\.pred_p", names(data_metrics)[3:4]))) {
      pred_name = ".pred_p"
    } else{
      pred_name = ".pred_"
    }

    num_classes <- length(unique(data_metrics$truth))

    if (num_classes == 2) {
      # Determine the positive class based on event_class parameter
      if (event_class == "first") {
        positive_class <- levels(data_metrics$truth)[1]
      } else if (event_class == "second") {
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
      perf_class <- metrics_class(
        data_metrics,
        truth = truth,
        estimate = estimate,
        event_level = event_class
      )

      if (!is.null(engine) &&
          !is.na(engine) && engine != "LiblineaR") {
        # Compute ROC AUC using the probability column for the positive class
        roc_auc_value <- yardstick::roc_auc(
          data_metrics,
          truth = truth,!!rlang::sym(paste0(pred_name, positive_class)),
          event_level = "second"
        )
        if (roc_auc_value$.estimate < 0.50) {
          roc_auc_value <- yardstick::roc_auc(
            data_metrics,
            truth = truth,!!rlang::sym(paste0(pred_name, positive_class)),
            event_level = "first"
          )
        }
        perf <- dplyr::bind_rows(perf_class, roc_auc_value)
      } else{
        perf <- perf_class
      }
    } else {
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

      if (!is.null(engine) &&
          !is.na(engine) && engine != "LiblineaR") {
        prob_cols <- names(pred_prob)
        perf_roc_auc <- yardstick::roc_auc(
          data_metrics,
          truth = truth,!!!rlang::syms(prob_cols),
          estimator = "macro_weighted"
        )
        perf <- dplyr::bind_rows(perf_class, perf_roc_auc)
      } else {
        perf <- perf_class
      }
    }
  } else if (task == "survival") {
    status_warning_emitted <- FALSE
    normalize_status <- function(status_vec, reference_length) {
      res <- fastml_normalize_survival_status(status_vec, reference_length)
      if (res$recoded && !status_warning_emitted) {
        warning(
          "Detected non-standard survival status coding; recoding to 0 = censored and 1 = event.",
          call. = FALSE
        )
        status_warning_emitted <<- TRUE
      }
      res$status
    }
    # if (length(label) == 3) {
    #   start_col <- label[1]
    #   time_col <- label[2]
    #   status_col <- label[3]
    # } else {
    #   start_col <- NULL
    #   time_col <- label[1]
    #   status_col <- label[2]
    # }
    test_status_clean <- normalize_status(test_data[[status_col]], nrow(test_data))
    train_status_clean <- normalize_status(train_data[[status_col]], nrow(train_data))
    test_data[[status_col]] <- test_status_clean
    train_data[[status_col]] <- train_status_clean
    if (!is.null(start_col)) {
      surv_obj <- survival::Surv(test_data[[start_col]], test_data[[time_col]], test_status_clean)
    } else {
      surv_obj <- survival::Surv(test_data[[time_col]], test_status_clean)
    }

    # Prepare data for prediction depending on model type
    pred_new_data <- test_data
    if (inherits(final_model, "fastml_native_survival")) {
      pred_new_data <- tryCatch(
        recipes::bake(final_model$recipe, new_data = test_data),
        error = function(e)
          test_data
      )
    }

    pred_predictors <- NULL
    if (inherits(final_model, "fastml_native_survival")) {
      pred_predictors <- fastml_prepare_native_survival_predictors(final_model, pred_new_data, test_data)
    }

    identify_survival_model <- function(obj) {
      fit_obj <- NULL
      if (inherits(obj, "fastml_native_survival")) {
        fit_obj <- tryCatch(
          obj$fit,
          error = function(e)
            NULL
        )
      }
      if (is.null(fit_obj)) {
        fit_obj <- tryCatch({
          workflows::extract_fit_engine(obj)
        }, error = function(e)
          NULL)
      }
      if (is.null(fit_obj)) {
        fit_obj <- tryCatch({
          workflows::extract_fit_parsnip(obj)$fit
        }, error = function(e)
          NULL)
      }
      if (is.null(fit_obj)) {
        fit_obj <- tryCatch({
          parsnip::extract_fit_engine(obj)
        }, error = function(e)
          NULL)
      }
      if (is.null(fit_obj)) {
        fit_obj <- tryCatch(
          obj$fit,
          error = function(e)
            NULL
        )
      }

      model_type <- "other"
      if (inherits(fit_obj, "coxph")) {
        model_type <- "coxph"
      } else if (inherits(fit_obj, c("stpm2", "pstpm2"))) {
        model_type <- "rstpm2"
      } else if (inherits(fit_obj, "survreg")) {
        model_type <- "survreg"
      } else if (inherits(fit_obj, "flexsurvreg")) {
        model_type <- "flexsurv"
      } else if (inherits(fit_obj, "glmnet")) {
        model_type <- "glmnet"
      } else if (inherits(fit_obj, "fastml_xgb_survival")) {
        objective <- tryCatch(
          fit_obj$objective,
          error = function(e)
            NULL
        )
        if (identical(objective, "survival:aft")) {
          model_type <- "xgboost_aft"
        } else if (identical(objective, "survival:cox")) {
          model_type <- "xgboost_cox"
        } else {
          model_type <- "xgboost_survival"
        }
      }

      list(type = model_type, fit = fit_obj)
    }

    survival_model_info <- identify_survival_model(final_model)
    survival_model_type <- survival_model_info$type

    default_time_val <- NA_real_
    time_candidates <- list()
    if (!is.null(final_model$time_col) &&
        final_model$time_col %in% names(train_data)) {
      time_candidates[[length(time_candidates) + 1]] <- train_data[[final_model$time_col]]
    }
    if (time_col %in% names(train_data)) {
      time_candidates[[length(time_candidates) + 1]] <- train_data[[time_col]]
    }
    if (time_col %in% names(test_data)) {
      time_candidates[[length(time_candidates) + 1]] <- test_data[[time_col]]
    }
    if (length(time_candidates) > 0) {
      med_vals <- vapply(time_candidates, function(x) {
        stats::median(as.numeric(x), na.rm = TRUE)
      }, numeric(1))
      default_time_val <- med_vals[is.finite(med_vals) &
                                     med_vals > 0]
      if (length(default_time_val) > 0) {
        default_time_val <- default_time_val[1]
      } else {
        default_time_val <- NA_real_
      }
    }
    if (!is.finite(default_time_val) || default_time_val <= 0) {
      default_time_val <- 1
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
          if (is.null(x))
            return(NA_real_)
          # If it's a scalar numeric, take it; otherwise try first element
          if (is.numeric(x) &&
              length(x) == 1L)
            return(as.numeric(x))
          if (is.atomic(x) &&
              length(x) >= 1L)
            return(as.numeric(x[[1L]]))
          # If it's a list wrapping a scalar, try to dig one level
          if (is.list(x) && length(x) >= 1L) {
            x1 <- x[[1L]]
            if (is.numeric(x1) &&
                length(x1) >= 1L)
              return(as.numeric(x1[[1L]]))
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

    compute_ibrier <- function(eval_times,
                               surv_mat,
                               time_vec,
                               status_vec,
                               tau,
                               censor_eval_fn) {
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
      if (length(denom_counts) == length(bs_t)) {
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
        times_aug <- c(0, times_valid)
        bs_aug <- c(0, bs_valid)
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

    risk <- tryCatch({
      if (inherits(final_model, "fastml_native_survival")) {
        # Prepare predictor-only data for native survival fits
        if (is.null(pred_predictors)) {
          pred_predictors <- data.frame()
        }
        if (inherits(final_model$fit, "coxph")) {
          as.numeric(stats::predict(
            final_model$fit,
            newdata = pred_predictors,
            type = "lp"
          ))
        } else if (inherits(final_model$fit, "survreg")) {
          as.numeric(stats::predict(
            final_model$fit,
            newdata = pred_predictors,
            type = "lp"
          ))
        } else if (inherits(final_model$fit, "flexsurvreg")) {
          tryCatch({
            # Use the original (unprocessed) test data for flexsurvreg predictions
            mm <- model.matrix(formula(final_model$fit), data = test_data)
            coefs <- final_model$fit$res.t[, "est"]
            common <- intersect(names(coefs), colnames(mm))
            lp <- as.numeric(mm[, common, drop = FALSE] %*% coefs[common])
            if ("(Intercept)" %in% names(coefs)) {
              lp <- lp + coefs["(Intercept)"]
            }
            lp
          }, error = function(e) {
            warning("Failed to compute linear predictors for flexsurvreg: ", e$message)
            rep(NA_real_, nrow(test_data))
          })
        } else if (inherits(final_model$fit, "glmnet")) {
          if (!requireNamespace("glmnet", quietly = TRUE)) {
            rep(NA_real_, nrow(test_data))
          } else {
            feature_names <- final_model$feature_names
            x_terms <- final_model$x_terms
            x_contrasts <- final_model$x_contrasts
            n_obs <- if (is.null(pred_predictors))
              0L
            else
              nrow(pred_predictors)

            if (is.null(feature_names) ||
                length(feature_names) == 0) {
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
                  out <- matrix(0,
                                nrow = 0,
                                ncol = length(feature_names))
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
                      stats::model.matrix( ~ . - 1, data = new_data)
                    }
                  }, error = function(e)
                    NULL)
                  if (is.null(mm)) {
                    mm <- tryCatch(
                      stats::model.matrix( ~ . - 1, data = new_data),
                      error = function(e)
                        NULL
                    )
                  }
                  if (is.null(mm)) {
                    mm <- matrix(0, nrow = n_local, ncol = 0)
                  }
                }
                mm <- as.matrix(mm)
                mm_colnames <- colnames(mm)
                if (!is.null(mm_colnames) &&
                    any(mm_colnames == "(Intercept)")) {
                  keep_cols <- mm_colnames != "(Intercept)"
                  mm <- mm[, keep_cols, drop = FALSE]
                  mm_colnames <- colnames(mm)
                }
                mm_full <- matrix(0,
                                  nrow = n_local,
                                  ncol = length(feature_names))
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
                glmnet::predict(
                  final_model$fit,
                  newx = mm_full,
                  s = penalty,
                  type = "link"
                )
              }, error = function(e)
                NULL)
              if (!is.null(pred_lp) && length(dim(pred_lp)) >= 2) {
                pred_lp <- as.matrix(pred_lp)[, 1, drop = TRUE]
              }
              if (!is.null(pred_lp)) {
                pred_lp <- as.numeric(pred_lp)
              }

              needs_manual_lp <- is.null(pred_lp) ||
                length(pred_lp) == 0 || any(!is.finite(pred_lp))

              if (needs_manual_lp) {
                coef_mat <- tryCatch({
                  glmnet::coef(final_model$fit, s = penalty)
                }, error = function(e)
                  NULL)

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
        } else if (inherits(final_model$fit, "fastml_xgb_survival")) {
          lp_vals <- fastml_xgb_predict_lp(final_model$fit, pred_predictors)
          if (identical(final_model$fit$objective, "survival:aft")) {
            as.numeric(-lp_vals)
          } else {
            as.numeric(lp_vals)
          }
        } else if (inherits(final_model$fit, c("stpm2", "pstpm2"))) {
          if (!requireNamespace("rstpm2", quietly = TRUE)) {
            rep(NA_real_, nrow(test_data))
          } else {
            rp_newdata <- pred_predictors
            if (is.null(rp_newdata)) {
              rp_newdata <- data.frame()
            }
            time_var <- final_model$time_col
            if (!is.null(time_var)) {
              if (!(time_var %in% names(rp_newdata)) &&
                  time_var %in% names(test_data)) {
                rp_newdata[[time_var]] <- test_data[[time_var]]
              }
              if (!(time_var %in% names(rp_newdata))) {
                rp_newdata[[time_var]] <- rep(default_time_val, nrow(test_data))
              } else {
                rp_newdata[[time_var]][!is.finite(rp_newdata[[time_var]])] <- default_time_val
              }
            }
            if (!is.null(final_model$start_col) &&
                !(final_model$start_col %in% names(rp_newdata)) &&
                final_model$start_col %in% names(test_data)) {
              rp_newdata[[final_model$start_col]] <- test_data[[final_model$start_col]]
            }
            as.numeric(rstpm2::predict(
              final_model$fit,
              newdata = rp_newdata,
              type = "lp"
            ))
          }
        } else {
          rep(NA_real_, nrow(test_data))
        }
      } else {
        pred_vals <- NULL
        if (identical(survival_model_type, "rstpm2")) {
          pred_vals <- tryCatch(
            predict(final_model, new_data = test_data, type = "lp"),
            error = function(e)
              NULL
          )
        }
        if (is.null(pred_vals)) {
          pred_vals <- tryCatch(
            predict(final_model, new_data = test_data, type = "linear_pred"),
            error = function(e)
              NULL
          )
        }
        extract_pred(pred_vals)
      }
    }, error = function(e) {
      if (inherits(final_model, "fastml_native_survival")) {
        rep(NA_real_, nrow(test_data))
      } else {
        extract_pred(predict(final_model, new_data = test_data))
      }
    })

    if (length(risk) == 0) {
      risk <- rep(NA_real_, nrow(test_data))
    } else if (length(risk) == 1 && nrow(test_data) > 1) {
      risk <- rep(risk, nrow(test_data))
    }
    risk <- as.numeric(risk)

    train_surv_matrix <- tryCatch(
      as.matrix(train_data$surv_obj),
      error = function(e)
        NULL
    )
    train_times_raw <- as.numeric(train_data[[time_col]])
    train_status_default <- normalize_status(train_data[[status_col]], length(train_times_raw))
    train_info <- get_surv_info(train_surv_matrix, train_times_raw, train_status_default)
    train_times <- train_info$time
    train_status_event <- ifelse(is.na(train_info$status),
                                 0,
                                 ifelse(train_info$status > 0, 1, 0))

    test_surv_matrix <- tryCatch(
      as.matrix(surv_obj),
      error = function(e)
        NULL
    )
    test_times_raw <- as.numeric(test_data[[time_col]])
    test_status_default <- normalize_status(test_data[[status_col]], length(test_times_raw))
    test_info <- get_surv_info(test_surv_matrix, test_times_raw, test_status_default)
    obs_time <- test_info$time
    status_event <- ifelse(is.na(test_info$status),
                           0,
                           ifelse(test_info$status > 0, 1, 0))

    t0 <- stats::median(train_times, na.rm = TRUE)
    threshold <- min(max(at_risk_threshold, 0.01), 0.5)
    tau_max <- compute_tau_limit(obs_time, threshold)
    if (!is.finite(tau_max) || tau_max <= 0) {
      tau_max <- suppressWarnings(max(obs_time[is.finite(obs_time)], na.rm = TRUE))
    }
    if (!is.finite(tau_max) || tau_max <= 0) {
      tau_max <- NA_real_
    }

    digits_round <- determine_round_digits(obs_time)
    if (is.null(eval_times_user)) {
      eval_horizons <- unique(round(c(
        stats::median(obs_time, na.rm = TRUE),
        as.numeric(
          stats::quantile(obs_time, 0.75, na.rm = TRUE, names = FALSE)
        )
      ), digits_round))
      eval_horizons <- eval_horizons[is.finite(eval_horizons) &
                                       eval_horizons > 0]
    } else {
      eval_horizons <- sort(unique(as.numeric(eval_times_user)))
      eval_horizons <- eval_horizons[is.finite(eval_horizons) &
                                       eval_horizons > 0]
    }
    if (length(eval_horizons) > 0 && is.finite(tau_max)) {
      too_late <- eval_horizons > tau_max
      if (any(too_late)) {
        eval_horizons <- eval_horizons[!too_late]
        if (length(eval_horizons) == 0) {
          warning(
            "All requested eval_times exceed t_max; horizon-specific Brier scores will be omitted."
          )
        } else {
          warning(
            "Some requested eval_times exceed t_max and were removed for Brier score computation."
          )
        }
      }
    }
    brier_times <- eval_horizons
    brier_metric_names <- if (length(brier_times) > 0) {
      paste0("brier_t", seq_along(brier_times))
    } else{
      character(0)
    }
    brier_time_map <- if (length(brier_times) > 0) {
      stats::setNames(brier_times, brier_metric_names)
    } else{
      numeric(0)
    }

    combined_times <- c(train_times, obs_time)
    combined_times <- combined_times[is.finite(combined_times) &
                                       combined_times > 0]
    if (length(combined_times) > 0) {
      eval_times <- sort(unique(combined_times))
      if (length(eval_times) > 200) {
        probs <- seq(0, 1, length.out = 200)
        eval_times <- sort(unique(as.numeric(
          stats::quantile(
            eval_times,
            probs = probs,
            na.rm = TRUE,
            type = 1
          )
        )))
      }
    } else {
      eval_times <- numeric(0)
    }
    special_points <- c(brier_times, tau_max, t0)
    special_points <- special_points[is.finite(special_points) &
                                       special_points > 0]
    eval_times <- sort(unique(c(eval_times, special_points)))
    if (is.finite(tau_max)) {
      eval_times <- eval_times[eval_times <= tau_max + 1e-08]
    }
    if (length(eval_times) == 0 &&
        is.finite(tau_max) && tau_max > 0) {
      eval_times <- tau_max
    }

    censor_eval_test <- create_censor_eval(obs_time, status_event)
    censor_eval_train <- create_censor_eval(train_times, train_status_event)

    n_obs <- nrow(test_data)
    surv_prob_mat <- NULL
    aft_quantile_mat <- NULL
    aft_probs <- numeric(0)
    aft_mu <- NULL
    if (length(eval_times) > 0 && n_obs > 0) {
      if (inherits(final_model, "fastml_native_survival")) {
        newdata_survfit <- pred_predictors
        if (is.null(newdata_survfit) ||
            nrow(newdata_survfit) != n_obs) {
          newdata_survfit <- tryCatch({
            if (is.null(pred_new_data)) {
              NULL
            } else {
              drop_cols <- c(
                final_model$response,
                final_model$time_col,
                final_model$status_col,
                final_model$start_col
              )
              drop_cols <- drop_cols[!is.na(drop_cols)]
              keep_cols <- setdiff(names(pred_new_data), drop_cols)
              pred_new_data[, keep_cols, drop = FALSE]
            }
          }, error = function(e)
            NULL)
        }
        if (is.null(newdata_survfit) && n_obs > 0) {
          newdata_survfit <- data.frame(matrix(nrow = n_obs, ncol = 0))
        }
        if (inherits(final_model$fit, "survreg")) {
          surv_prob_mat <- compute_survreg_matrix(final_model$fit, newdata_survfit, eval_times)
          if (is.null(surv_prob_mat)) {
            surv_fit <- tryCatch(
              survival::survfit(final_model$fit, newdata = newdata_survfit),
              error = function(e)
                NULL
            )
            surv_prob_mat <- build_survfit_matrix(surv_fit, eval_times, n_obs)
          }
        } else if (inherits(final_model$fit, "flexsurvreg")) {
          # flexsurvreg models expect the covariates used during fitting.
          # Prefer the preprocessed predictor set when available, but fall back
          # to the raw test data if preprocessing failed or yielded mismatched
          # rows (e.g. due to recipes that only keep the outcome columns).
          flexsurv_newdata <- newdata_survfit
          if (is.null(flexsurv_newdata) || nrow(flexsurv_newdata) != n_obs) {
            flexsurv_newdata <- test_data
          }
          surv_prob_mat <- fastml_flexsurv_survival_matrix(
            final_model$fit,
            flexsurv_newdata,
            eval_times
          )

          # If risk has not been computed yet, derive it from the survival probabilities
          if (!any(is.finite(risk)) &&
              !is.null(surv_prob_mat) &&
              ncol(surv_prob_mat) > 0) {
            mid_idx <- ceiling(ncol(surv_prob_mat) / 2)
            if (mid_idx > 0 && mid_idx <= ncol(surv_prob_mat)) {
              surv_mid <- surv_prob_mat[, mid_idx]
              risk <- -log(pmax(surv_mid, .Machine$double.eps))
            }
          }
        } else if (inherits(final_model$fit, "stpm2")) {
          if (requireNamespace("rstpm2", quietly = TRUE) &&
              length(eval_times) > 0) {
            base_newdata <- newdata_survfit
            if (is.null(base_newdata)) {
              base_newdata <- data.frame()
            }
            time_var <- final_model$time_col
            if (!is.null(time_var)) {
              if (!(time_var %in% names(base_newdata)) &&
                  time_var %in% names(test_data)) {
                base_newdata[[time_var]] <- test_data[[time_var]]
              }
              if (!(time_var %in% names(base_newdata))) {
                base_newdata[[time_var]] <- rep(default_time_val, n_obs)
              } else {
                base_newdata[[time_var]][!is.finite(base_newdata[[time_var]])] <- default_time_val
              }
            }
            if (!is.null(final_model$start_col) &&
                !(final_model$start_col %in% names(base_newdata)) &&
                final_model$start_col %in% names(test_data)) {
              base_newdata[[final_model$start_col]] <- test_data[[final_model$start_col]]
            }
            surv_prob_mat <- matrix(NA_real_,
                                    nrow = n_obs,
                                    ncol = length(eval_times))
            for (j in seq_along(eval_times)) {
              nd <- base_newdata
              if (!is.null(time_var)) {
                nd[[time_var]] <- eval_times[j]
              }
              preds <- tryCatch({
                as.numeric(rstpm2::predict(
                  final_model$fit,
                  newdata = nd,
                  type = "surv"
                ))
              }, error = function(e)
                rep(NA_real_, n_obs))
              if (length(preds) == n_obs) {
                surv_prob_mat[, j] <- preds
              }
            }
          }
        } else if (inherits(final_model$fit, "fastml_xgb_survival")) {
          if (identical(final_model$fit$objective, "survival:cox")) {
            surv_prob_mat <- NULL
          } else {
            aft_pred <- fastml_xgb_aft_predict(final_model$fit, pred_predictors, eval_times = eval_times)
            surv_prob_mat <- aft_pred$surv
            aft_quantile_mat <- aft_pred$quantiles
            aft_probs <- aft_pred$probs
            aft_mu <- aft_pred$mu
          }
        } else {
          surv_fit <- tryCatch(
            survival::survfit(final_model$fit, newdata = newdata_survfit),
            error = function(e)
              NULL
          )
          surv_prob_mat <- build_survfit_matrix(surv_fit, eval_times, n_obs)
        }
      } else {
        surv_pred <- tryCatch(
          predict(
            final_model,
            new_data = test_data,
            type = "survival",
            eval_time = eval_times
          ),
          error = function(e)
            NULL
        )
        if (is.null(surv_pred) && length(eval_times) == 1) {
          surv_pred <- tryCatch(
            predict(
              final_model,
              new_data = test_data,
              type = "survival",
              eval_time = eval_times[1]
            ),
            error = function(e)
              NULL
          )
        }
        surv_prob_mat <- convert_survival_predictions(surv_pred, eval_times, n_obs)
      }
    }

    surv_prob <- rep(NA_real_, n_obs)
    if (!is.null(surv_prob_mat) &&
        nrow(surv_prob_mat) == n_obs &&
        ncol(surv_prob_mat) == length(eval_times) &&
        length(eval_times) > 0) {
      idx_t0 <- 1L
      if (is.finite(t0) && t0 > 0) {
        idx_t0 <- which.min(abs(eval_times - t0))
      }
      idx_t0 <- max(1L, min(idx_t0, ncol(surv_prob_mat)))
      surv_prob <- as.numeric(surv_prob_mat[, idx_t0])
    }

    if (identical(survival_model_type, "rstpm2")) {
      if (length(surv_prob) == n_obs && any(is.finite(surv_prob))) {
        risk <- -log(pmax(surv_prob, .Machine$double.eps))
      }
    } else if ((!any(is.finite(risk)) ||
                all(is.na(risk))) &&
               length(surv_prob) == n_obs && any(is.finite(surv_prob))) {
      risk <- -log(pmax(surv_prob, .Machine$double.eps))
    }
    if (length(risk) != n_obs) {
      risk <- rep(risk, length.out = n_obs)
    }
    risk[!is.finite(risk)] <- NA_real_

    surv_curve_list <- vector("list", n_obs)
    if (!is.null(surv_prob_mat) &&
        nrow(surv_prob_mat) == n_obs &&
        ncol(surv_prob_mat) == length(eval_times) &&
        length(eval_times) > 0) {
      formatted_times <- format(eval_times, trim = TRUE, scientific = FALSE)
      for (i in seq_len(n_obs)) {
        row_vals <- as.numeric(surv_prob_mat[i, ])
        names(row_vals) <- formatted_times
        surv_curve_list[[i]] <- row_vals
      }
    }
    attr(surv_curve_list, "eval_times") <- eval_times

    limited_metrics <- identical(survival_model_type, "xgboost_cox")
    if (limited_metrics) {
      attr(surv_curve_list, "eval_times") <- numeric(0)
      brier_times <- numeric(0)
      brier_metric_names <- character(0)
      brier_time_map <- numeric(0)
    }

    compute_surv_median <- function(prob_mat, eval_times, n_rows) {
      if (is.null(prob_mat) ||
          nrow(prob_mat) != n_rows ||
          ncol(prob_mat) == 0 ||
          length(eval_times) == 0) {
        rep(NA_real_, n_rows)
      } else {
        median_indices <- apply(prob_mat, 1, function(surv_row) {
          idx <- which(surv_row <= 0.5)
          if (length(idx) > 0) {
            idx[1]
          } else {
            NA_integer_
          }
        })

        result <- rep(NA_real_, n_rows)
        valid_indices <- !is.na(median_indices)
        if (any(valid_indices)) {
          result[valid_indices] <- eval_times[median_indices[valid_indices]]
        }
        result
      }
    }

    align_surv_time <- function(values, n_rows) {
      if (length(values) == n_rows) {
        as.numeric(values)
      } else if (length(values) == 0) {
        rep(NA_real_, n_rows)
      } else {
        aligned <- rep(NA_real_, n_rows)
        idx <- seq_len(min(length(values), n_rows))
        if (length(idx) > 0) {
          aligned[idx] <- as.numeric(values[idx])
        }
        aligned
      }
    }

    surv_time_default <- compute_surv_median(surv_prob_mat, eval_times, n_obs)

    surv_time <- tryCatch({
      if (inherits(final_model, "fastml_native_survival")) {
        result <- surv_time_default
        missing_idx <- which(!is.finite(result))
        if (length(missing_idx) > 0) {
          fallback <- NULL
          if (inherits(final_model$fit, "flexsurvreg")) {
            quantiles_list <- quantile(
              final_model$fit,
              p = 0.5,
              newdata = test_data
            )
            fallback_vals <- vapply(quantiles_list, function(x) {
              if (is.data.frame(x) && "est" %in% names(x)) {
                as.numeric(x$est[1])
              } else {
                NA_real_
              }
            }, numeric(1))
            fallback <- align_surv_time(fallback_vals, n_obs)
          } else if (requireNamespace("censored", quietly = TRUE)) {
            if (inherits(final_model$fit, "coxph")) {
              fallback <- align_surv_time(
                censored::survival_time_coxph(final_model$fit, pred_predictors),
                n_obs
              )
            } else if (inherits(final_model$fit, "survbagg")) {
              fallback <- align_surv_time(
                censored::survival_time_survbagg(final_model$fit, pred_predictors),
                n_obs
              )
            } else if (inherits(final_model$fit, "mboost")) {
              fallback <- align_surv_time(
                censored::survival_time_mboost(final_model$fit, pred_predictors),
                n_obs
              )
            }
          }
          if (!is.null(fallback)) {
            result[missing_idx] <- fallback[missing_idx]
          }
        }
        result

      } else {
        surv_time_default
      }
    }, error = function(e) {
      warning("Computation of survival time failed: ", e$message)
      rep(NA_real_, n_obs)
    })

    if (identical(survival_model_type, "xgboost_aft")) {
      if (!is.null(aft_quantile_mat) &&
          nrow(aft_quantile_mat) == n_obs && length(aft_probs) > 0) {
        median_idx <- which.min(abs(aft_probs - 0.5))
        if (length(median_idx) == 0) {
          median_idx <- ceiling(length(aft_probs) / 2)
        } else {
          median_idx <- median_idx[1]
        }
        median_vals <- as.numeric(aft_quantile_mat[, median_idx, drop = TRUE])
        surv_time <- median_vals
        surv_time[!is.finite(surv_time)] <- NA_real_
      } else if (!is.null(aft_mu) && length(aft_mu) == n_obs) {
        surv_time <- exp(as.numeric(aft_mu))
        surv_time[!is.finite(surv_time)] <- NA_real_
      }
    } else if (identical(survival_model_type, "xgboost_cox")) {
      surv_time <- rep(NA_real_, n_obs)
    }

    risk_group <- assign_risk_group(risk)

    ibs_curve_full <- rep(NA_real_, length(eval_times))
    ibs_point <- NA_real_
    brier_time_values <- if (length(brier_metric_names) > 0)
      rep(NA_real_, length(brier_metric_names))
    else
      numeric(0)
    rmst_diff <- NA_real_
    if (!limited_metrics &&
        !is.null(surv_prob_mat) && length(eval_times) > 0) {
      ibs_res_full <- compute_ibrier(eval_times,
                                     surv_prob_mat,
                                     obs_time,
                                     status_event,
                                     tau_max,
                                     censor_eval_test)
      ibs_point <- ibs_res_full$ibs
      ibs_curve_full <- ibs_res_full$curve
      brier_time_values <- map_brier_values(ibs_curve_full, eval_times, brier_times)
      rmst_diff <- compute_rmst_difference(
        obs_time,
        status_event,
        risk,
        tau_max,
        surv_mat = surv_prob_mat,
        eval_times_full = eval_times,
        model_type = survival_model_type
      )
    }

    harrell_c <- NA_real_
    risk_valid <- is.finite(risk)
    if (any(risk_valid)) {
      risk_vals <- risk[risk_valid]
      if (length(unique(risk_vals)) > 1) {
        surv_valid <- surv_obj[risk_valid]
        harrell_c <- tryCatch(
          survival::concordance(surv_valid ~ risk_vals)$concordance,
          error = function(e)
            NA_real_
        )
      } else {
        harrell_c <- 0.5
      }
    }
    harrell_c <- clamp01(harrell_c)

    uno_c <- compute_uno_c_index(
      train_times,
      train_status_event,
      obs_time,
      status_event,
      risk,
      tau_max,
      censor_eval_train
    )
    uno_c <- clamp01(uno_c)
    if (!limited_metrics) {
      ibs_point <- clamp01(ibs_point)
      if (length(brier_time_values) > 0) {
        brier_time_values <- clamp01(brier_time_values)
      }
    }

    if (limited_metrics) {
      metric_names <- c("c_index", "uno_c")
      metrics_point <- c(harrell_c, uno_c)
    } else {
      metric_names <- c("c_index",
                        "uno_c",
                        "ibs",
                        "rmst_diff",
                        brier_metric_names)
      metrics_point <- c(harrell_c,
                         uno_c,
                         ibs_point,
                         rmst_diff,
                         brier_time_values)
    }
    names(metrics_point) <- metric_names
    metrics_point[is.nan(metrics_point)] <- NA_real_

    compute_metrics_boot <- function(idx) {
      idx <- as.integer(idx)
      obs_sub <- obs_time[idx]
      status_sub <- status_event[idx]
      risk_sub <- risk[idx]
      surv_sub <- surv_obj[idx]
      surv_mat_sub <- if (!is.null(surv_prob_mat))
        surv_prob_mat[idx, , drop = FALSE]
      else
        NULL
      harrell_sub <- NA_real_
      risk_valid_sub <- is.finite(risk_sub)
      if (any(risk_valid_sub)) {
        risk_vals_sub <- risk_sub[risk_valid_sub]
        if (length(unique(risk_vals_sub)) > 1) {
          surv_valid_sub <- surv_sub[risk_valid_sub]
          harrell_sub <- tryCatch(
            survival::concordance(surv_valid_sub ~ risk_vals_sub)$concordance,
            error = function(e)
              NA_real_
          )
        } else {
          harrell_sub <- 0.5
        }
      }
      harrell_sub <- clamp01(harrell_sub)
      uno_sub <- compute_uno_c_index(
        train_times,
        train_status_event,
        obs_sub,
        status_sub,
        risk_sub,
        tau_max,
        censor_eval_train
      )
      uno_sub <- clamp01(uno_sub)
      if (limited_metrics) {
        vals <- c(harrell_sub, uno_sub)
        names(vals) <- metric_names
        vals[is.nan(vals)] <- NA_real_
        return(vals)
      }
      ibs_sub <- NA_real_
      curve_sub <- rep(NA_real_, length(eval_times))
      if (!is.null(surv_mat_sub) &&
          nrow(surv_mat_sub) == length(idx)) {
        ibs_sub_res <- compute_ibrier(eval_times,
                                      surv_mat_sub,
                                      obs_sub,
                                      status_sub,
                                      tau_max,
                                      censor_eval_test)
        ibs_sub <- clamp01(ibs_sub_res$ibs)
        curve_sub <- ibs_sub_res$curve
      }
      brier_sub <- map_brier_values(curve_sub, eval_times, brier_times)
      if (length(brier_sub) > 0) {
        brier_sub <- clamp01(brier_sub)
      }
      rmst_sub <- compute_rmst_difference(
        obs_sub,
        status_sub,
        risk_sub,
        tau_max,
        surv_mat = surv_mat_sub,
        eval_times_full = eval_times,
        model_type = survival_model_type
      )
      vals <- c(harrell_sub, uno_sub, ibs_sub, rmst_sub, brier_sub)
      names(vals) <- metric_names
      vals[is.nan(vals)] <- NA_real_
      vals
    }

    ci_lower <- rep(NA_real_, length(metric_names))
    ci_upper <- rep(NA_real_, length(metric_names))
    n_boot_used <- 0L
    if (isTRUE(bootstrap_ci) &&
        bootstrap_samples > 1 && n_obs > 1) {
      if (!is.null(bootstrap_seed)) {
        set.seed(bootstrap_seed)
      }
      boot_matrix <- matrix(NA_real_,
                            nrow = bootstrap_samples,
                            ncol = length(metric_names))
      colnames(boot_matrix) <- metric_names
      for (b in seq_len(bootstrap_samples)) {
        idx <- sample.int(n_obs, size = n_obs, replace = TRUE)
        boot_matrix[b, ] <- compute_metrics_boot(idx)
      }
      for (j in seq_along(metric_names)) {
        vals <- boot_matrix[, j]
        vals <- vals[is.finite(vals)]
        if (length(vals) > 0) {
          ci_lower[j] <- stats::quantile(vals,
                                         probs = 0.025,
                                         names = FALSE,
                                         na.rm = TRUE)
          ci_upper[j] <- stats::quantile(vals,
                                         probs = 0.975,
                                         names = FALSE,
                                         na.rm = TRUE)
        }
      }
      clamp_idx <- metric_names %in% c("c_index", "uno_c", "ibs") |
        grepl("^brier_t", metric_names)
      if (any(clamp_idx)) {
        ci_lower[clamp_idx] <- clamp01(ci_lower[clamp_idx])
        ci_upper[clamp_idx] <- clamp01(ci_upper[clamp_idx])
      }
      n_boot_used <- bootstrap_samples
    }

    perf <- tibble::tibble(
      .metric = metric_names,
      .estimate = as.numeric(metrics_point),
      .lower = as.numeric(ci_lower),
      .upper = as.numeric(ci_upper),
      .n_boot = n_boot_used
    )

    perf$.estimate <- as.numeric(perf$.estimate)

    if (!is.null(start_col) && start_col %in% names(test_data)) {
      data_metrics <- tibble::tibble(
        start = test_data[[start_col]],
        time = test_data[[time_col]],
        status = test_data[[status_col]],
        risk = risk,
        risk_group = risk_group,
        surv_prob = surv_prob,
        surv_time = surv_time,
        surv_prob_curve = surv_curve_list
      )
    } else {
      data_metrics <- tibble::tibble(
        time = test_data[[time_col]],
        status = test_data[[status_col]],
        risk = risk,
        risk_group = risk_group,
        surv_prob = surv_prob,
        surv_time = surv_time,
        surv_prob_curve = surv_curve_list
      )
    }

    attr(perf, "brier_times") <- brier_time_map
    attr(perf, "t_max") <- tau_max
    attr(perf, "at_risk_threshold") <- threshold
    attr(data_metrics, "eval_times") <- if (limited_metrics)
      numeric(0)
    else
      eval_times
    if (limited_metrics) {
      attr(data_metrics, "brier_curve") <- NULL
      attr(data_metrics, "brier_times") <- numeric(0)
    } else {
      attr(data_metrics, "brier_curve") <- tibble::tibble(eval_time = eval_times, brier = ibs_curve_full)
      attr(data_metrics, "brier_times") <- brier_time_map
    }
    attr(data_metrics, "t_max") <- tau_max

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



