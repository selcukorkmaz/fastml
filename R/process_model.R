#' Process and Evaluate a Model Workflow
#'
#' This function processes a fitted model or a tuning result, finalizes the model if tuning was used,
#' makes predictions on the test set, and computes performance metrics depending on the task type
#' (classification or regression). It supports binary and multiclass classification, and handles
#' probabilistic outputs when supported by the modeling engine.
#'
#' @param model_obj A fitted model or a tuning result (`tune_results` object).
#' @param model_id A character identifier for the model (used in warnings).
#' @param task Type of task, either `"classification"` or `"regression"`.
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
