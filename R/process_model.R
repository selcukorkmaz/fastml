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
#' @importFrom survival psurvreg survfit
#' @export

compute_survreg_probabilities <- function(final_model, new_data, eval_time, fit_info = NULL) {
  lp_tbl <- tryCatch(
    predict(final_model, new_data = new_data, type = "linear_pred"),
    error = function(e) NULL
  )

  if (is.null(lp_tbl) || !".pred" %in% names(lp_tbl)) {
    return(rep(NA_real_, nrow(new_data)))
  }

  lp <- lp_tbl$.pred

  if (is.null(fit_info)) {
    fit_info <- tryCatch(
      workflows::extract_fit_parsnip(final_model),
      error = function(e) NULL
    )
  }

  if (is.null(fit_info) || is.null(fit_info$fit)) {
    return(rep(NA_real_, length(lp)))
  }

  survreg_fit <- fit_info$fit
  scale_val <- if (!is.null(survreg_fit$scale)) survreg_fit$scale else 1

  dist_val <- NULL
  if (!is.null(survreg_fit$dist)) {
    dist_val <- survreg_fit$dist
  } else if (!is.null(survreg_fit$call$dist)) {
    dist_val <- as.character(survreg_fit$call$dist)
  }
  if (is.null(dist_val) || length(dist_val) == 0) {
    dist_val <- "weibull"
  }

  surv_prob <- tryCatch(
    1 - survival::psurvreg(
      rep(eval_time, length(lp)),
      mean = lp,
      scale = scale_val,
      distribution = dist_val
    ),
    error = function(e) rep(NA_real_, length(lp))
  )

  as.numeric(surv_prob)
}

get_survival_probabilities <- function(final_model, new_data, eval_time, engine) {
  raw_pred <- tryCatch(
    predict(final_model, new_data = new_data, type = "survival", eval_time = eval_time),
    error = function(e) NULL
  )

  if (!is.null(raw_pred)) {
    if (".pred" %in% names(raw_pred)) {
      return(raw_pred$.pred)
    }
    if (".pred_survival" %in% names(raw_pred)) {
      return(raw_pred$.pred_survival)
    }
    first_col <- raw_pred[[1]]
    if (is.numeric(first_col)) {
      return(first_col)
    }
  }

  fit_info <- tryCatch(
    workflows::extract_fit_parsnip(final_model),
    error = function(e) NULL
  )

  if (!is.null(fit_info)) {
    spec_class <- class(fit_info$spec)[1]
    if (!is.null(spec_class) && spec_class == "survival_reg" && engine == "survival") {
      return(compute_survreg_probabilities(final_model, new_data, eval_time, fit_info))
    }
  } else if (inherits(final_model, "model_fit")) {
    spec_class <- class(final_model$spec)[1]
    if (!is.null(spec_class) && spec_class == "survival_reg" && engine == "survival") {
      return(compute_survreg_probabilities(final_model, new_data, eval_time))
    }
  }

  rep(NA_real_, nrow(new_data))
}

compute_ipcw_brier <- function(train_time, train_status, test_time, test_status, surv_prob, eval_time) {
  if (length(surv_prob) != length(test_time) || all(is.na(surv_prob))) {
    return(NA_real_)
  }

  indicator <- as.numeric(test_time > eval_time)
  train_status <- as.numeric(train_status)
  test_status <- as.numeric(test_status)

  if (length(indicator) != length(surv_prob)) {
    return(NA_real_)
  }

  if (all(train_status == 1, na.rm = TRUE)) {
    return(mean((indicator - surv_prob)^2))
  }

  censor_fit <- tryCatch(
    survival::survfit(survival::Surv(train_time, 1 - train_status) ~ 1),
    error = function(e) NULL
  )

  if (is.null(censor_fit)) {
    return(mean((indicator - surv_prob)^2))
  }

  g_eval <- tryCatch(
    summary(censor_fit, times = eval_time, extend = TRUE)$surv,
    error = function(e) NA_real_
  )

  g_tminus <- tryCatch(
    summary(censor_fit, times = pmax(test_time - 1e-8, 0), extend = TRUE)$surv,
    error = function(e) rep(NA_real_, length(test_time))
  )

  if (is.na(g_eval) || any(is.na(g_tminus))) {
    return(mean((indicator - surv_prob)^2))
  }

  eps <- sqrt(.Machine$double.eps)
  contrib <- (indicator - surv_prob)^2
  observed_event <- (test_time <= eval_time) & (test_status == 1)
  beyond_time <- test_time > eval_time

  contrib[observed_event] <- contrib[observed_event] / pmax(g_tminus[observed_event], eps)
  contrib[beyond_time] <- contrib[beyond_time] / pmax(g_eval, eps)
  contrib[!(observed_event | beyond_time)] <- 0
  contrib[!is.finite(contrib)] <- 0

  mean(contrib)
}

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
    t0 <- stats::median(train_data[[time_col]])

    if (inherits(final_model, "fastml_royston")) {
      processed_test <- tryCatch(
        recipes::bake(final_model$recipe, new_data = test_data),
        error = function(e) NULL
      )

      if (is.null(processed_test)) {
        risk <- rep(NA_real_, nrow(test_data))
        surv_prob <- rep(NA_real_, nrow(test_data))
      } else {
        processed_new <- as.data.frame(processed_test)
        if ("surv_obj" %in% names(processed_new)) {
          processed_new$surv_obj <- NULL
        }
        risk <- tryCatch(
          as.numeric(rstpm2::predict(final_model$fit, newdata = processed_new, type = "link")),
          error = function(e) rep(NA_real_, nrow(test_data))
        )
        surv_prob <- tryCatch(
          as.numeric(rstpm2::predict(final_model$fit, newdata = processed_new, type = "surv", time = t0)),
          error = function(e) rep(NA_real_, nrow(test_data))
        )
      }
    } else {
      risk <- tryCatch(
        predict(final_model, new_data = test_data, type = "linear_pred")$.pred,
        error = function(e) {
          tryCatch(
            predict(final_model, new_data = test_data)$.pred,
            error = function(e2) rep(NA_real_, nrow(test_data))
          )
        }
      )

      surv_prob <- get_survival_probabilities(final_model, test_data, t0, engine)
    }

    train_surv <- survival::Surv(train_data[[time_col]], train_data[[status_col]])
    train_status_vec <- train_surv[, "status"]
    test_status_vec <- surv_obj[, "status"]

    brier <- if (anyNA(surv_prob)) {
      NA_real_
    } else {
      compute_ipcw_brier(
        train_time = train_data[[time_col]],
        train_status = train_status_vec,
        test_time = test_data[[time_col]],
        test_status = test_status_vec,
        surv_prob = surv_prob,
        eval_time = t0
      )
    }

    if (all(is.na(risk))) {
      c_index <- NA_real_
      risk_group <- rep(NA_character_, length(risk))
      logrank_p <- NA_real_
    } else {
      c_index <- survival::concordance(surv_obj ~ risk)$concordance
      median_risk <- stats::median(risk, na.rm = TRUE)
      if (is.finite(median_risk)) {
        risk_group <- ifelse(risk > median_risk, "high", "low")
        if (length(unique(risk_group[!is.na(risk_group)])) > 1) {
          lr <- survival::survdiff(surv_obj ~ risk_group)
          logrank_p <- 1 - stats::pchisq(lr$chisq, length(lr$n) - 1)
        } else {
          logrank_p <- NA_real_
        }
      } else {
        risk_group <- rep(NA_character_, length(risk))
        logrank_p <- NA_real_
      }
    }

    perf <- tibble::tibble(
      .metric = c("c_index", "brier_score", "logrank_p"),
      .estimate = c(c_index, brier, logrank_p)
    )

    data_metrics <- tibble::tibble(
      time = test_data[[time_col]],
      status = test_data[[status_col]],
      risk = risk,
      surv_prob = surv_prob
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
