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
            n_obs <- nrow(pred_predictors)
            if (is.null(feature_names) || length(feature_names) == 0) {
              rep(NA_real_, nrow(test_data))
            } else if (n_obs == 0) {
              numeric(0)
            } else {
              if (ncol(pred_predictors) > 0) {
                mm <- stats::model.matrix(~ . - 1, data = pred_predictors)
              } else {
                mm <- matrix(0, nrow = n_obs, ncol = 0)
              }
              mm_full <- matrix(0, nrow = n_obs, ncol = length(feature_names))
              colnames(mm_full) <- feature_names
              if (ncol(mm) > 0) {
                overlap <- intersect(feature_names, colnames(mm))
                if (length(overlap) > 0) {
                  mm_full[, overlap, drop = FALSE] <- mm[, overlap, drop = FALSE]
                }
              }
              penalty <- final_model$penalty
              pred_lp <- glmnet::predict(final_model$fit, newx = mm_full,
                                         s = penalty, type = "link")
              if (is.matrix(pred_lp)) {
                pred_lp <- pred_lp[, 1, drop = TRUE]
              }
              as.numeric(pred_lp)
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

    t0 <- stats::median(train_data[[time_col]])
    # Try to obtain survival probability at t0
    surv_pred <- tryCatch({
      if (inherits(final_model, "fastml_native_survival")) {
        if (requireNamespace("censored", quietly = TRUE)) {
          if (inherits(final_model$fit, "coxph")) {
            censored::survival_prob_coxph(final_model$fit, pred_predictors, eval_time = t0)
          } else if (inherits(final_model$fit, "survreg")) {
            censored::survival_prob_survreg(final_model$fit, pred_predictors, eval_time = t0)
          } else {
            NULL
          }
        } else if (inherits(final_model$fit, "coxph") || inherits(final_model$fit, "survreg")) {
          if (nrow(pred_predictors) == 0) {
            numeric(0)
          } else {
            surv_fit <- tryCatch(
              survival::survfit(final_model$fit, newdata = pred_predictors),
              error = function(e) NULL
            )
            if (is.null(surv_fit)) {
              NULL
            } else {
              surv_summary_vals <- tryCatch({
                summary(surv_fit, times = t0, extend = TRUE)$surv
              }, error = function(e) NULL)
              if (!is.null(surv_summary_vals) && length(surv_summary_vals) == nrow(pred_predictors)) {
                as.numeric(surv_summary_vals)
              } else {
                surv_times <- surv_fit$time
                surv_vals <- surv_fit$surv
                if (length(surv_times) == 0 || is.null(surv_vals)) {
                  rep(1, nrow(pred_predictors))
                } else if (is.matrix(surv_vals)) {
                  idx <- findInterval(t0, surv_times)
                  as.numeric(apply(surv_vals, 2, function(curve) {
                    if (idx <= 0) {
                      1
                    } else {
                      curve[min(idx, length(curve))]
                    }
                  }))
                } else {
                  idx <- findInterval(t0, surv_times)
                  val <- if (idx <= 0) 1 else surv_vals[min(idx, length(surv_vals))]
                  rep(val, nrow(pred_predictors))
                }
              }
            }
          }
        } else {
          NULL
        }
      } else {
        predict(final_model, new_data = test_data, type = "survival", eval_time = t0)
      }
    }, error = function(e) NULL)
    if (!is.null(surv_pred)) {
      surv_prob <- extract_pred(surv_pred)
      brier <- mean((as.numeric(test_data[[time_col]] > t0) - surv_prob)^2)
    } else {
      surv_prob <- rep(NA_real_, nrow(test_data))
      brier <- NA_real_
    }

    # Try to obtain predicted survival time where supported
    surv_time <- tryCatch({
      if (inherits(final_model, "fastml_native_survival") && requireNamespace("censored", quietly = TRUE)) {
        if (inherits(final_model$fit, "coxph")) {
          as.numeric(censored::survival_time_coxph(final_model$fit, pred_predictors))
        } else if (inherits(final_model$fit, "survbagg")) {
          as.numeric(censored::survival_time_survbagg(final_model$fit, pred_predictors))
        } else if (inherits(final_model$fit, "mboost")) {
          as.numeric(censored::survival_time_mboost(final_model$fit, pred_predictors))
        } else if (inherits(final_model$fit, "glmnet")) {
          as.numeric(censored::survival_time_coxnet(final_model$fit, pred_predictors))
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
      surv_time = surv_time
    )

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

