#' Evaluate Models Function
#'
#' Evaluates the trained models on the test data and computes performance metrics.
#'
#' @param models A list of trained model objects.
#' @param train_data Preprocessed training data frame.
#' @param test_data Preprocessed test data frame.
#' @param label Name of the target variable.
#' @param task Type of task: "classification" or "regression".
#' @param metric The performance metric to optimize (e.g., "accuracy", "rmse").
#' @param event_class A single string. Either "first" or "second" to specify which level of truth to consider as the "event".
#' @importFrom dplyr filter bind_rows pull mutate select bind_cols
#' @importFrom yardstick metric_set accuracy kap roc_auc sens spec precision f_meas rmse rsq mae
#' @importFrom workflows pull_workflow_spec pull_workflow_preprocessor workflow add_model add_recipe
#' @importFrom parsnip fit predict.model_fit
#' @importFrom tune select_best finalize_model
#' @importFrom rlang sym syms
#' @importFrom tibble tibble
#' @return A list with two elements:
#'   \describe{
#'     \item{performance}{A named list of performance metric tibbles for each model.}
#'     \item{predictions}{A named list of data frames with columns including truth, predictions, and probabilities per model.}
#'   }
#' @export
evaluate_models <- function(models, train_data, test_data, label, task, metric = NULL, event_class) {
  # Load required packages
  required_pkgs <- c("yardstick", "parsnip", "tune", "workflows",
                     "dplyr", "rlang", "tibble")
  for(pkg in required_pkgs){
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("The '%s' package is required but not installed.", pkg))
    }
  }

  # Initialize performance and predictions lists
  performance <- list()
  predictions_list <- list()

  # Extract true labels from the test set
  true_labels <- test_data[[label]]

  # Helper function to process a single fitted model
  process_model <- function(model_obj, model_id) {
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

      final_model_spec <- parsnip::finalize_model(model_spec, best_params)
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
      pred_prob <- predict(final_model, new_data = test_data, type = "prob")

      if(nrow(test_data) != length(pred_class)) {
        stop('The dataset has missing values. To handle this, set impute_method = "remove" to delete rows with missing values,
             or use an imputation method such as "medianImpute" to fill missing values with the column median, "knnImpute" to
             estimate missing values using k-Nearest Neighbors, "bagImpute" to apply bagging for imputation, "mice" to use
             Multiple Imputation by Chained Equations, or "missForest" to use random forests for imputation.')
      }

      data_metrics <- test_data %>%
        dplyr::select(truth = !!rlang::sym(label)) %>%
        dplyr::mutate(estimate = pred_class) %>%
        dplyr::bind_cols(pred_prob)

      if(any(grepl("^\\.pred_p", names(data_metrics)))){

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

        prob_cols <- names(pred_prob)
        perf_roc_auc <- yardstick::roc_auc(
          data_metrics,
          truth = truth,
          !!!rlang::syms(prob_cols),
          estimator = "macro_weighted"
        )
        perf <- dplyr::bind_rows(perf_class, perf_roc_auc)
      }
    } else {
      # Regression task
      predictions <- predict(final_model, new_data = test_data)
      pred <- predictions$.pred
      data_metrics <- tibble::tibble(truth = true_labels, estimate = pred)
      metrics_set <- yardstick::metric_set(yardstick::rmse, yardstick::rsq, yardstick::mae)
      perf <- metrics_set(data_metrics, truth = truth, estimate = estimate)
    }

    return(list(performance = perf, predictions = data_metrics))
  }

  # Iterate over the models object. Check if the model is nested (i.e. a list of engines)
  for (algo in names(models)) {
    performance[[algo]] <- list()
    predictions_list[[algo]] <- list()

    # If the model object has names, assume it is nested by engine.
    if (is.list(models[[algo]]) && !inherits(models[[algo]], "workflow") && !inherits(models[[algo]], "tune_results")) {
      for (eng in names(models[[algo]])) {
        model_obj <- models[[algo]][[eng]]
        result <- process_model(model_obj, model_id = paste(algo, eng, sep = "_"))
        if (!is.null(result)) {
          performance[[algo]][[eng]] <- result$performance
          predictions_list[[algo]][[eng]] <- result$predictions
        }
      }
    } else {
      # Otherwise, assume a single model (not nested)
      result <- process_model(models[[algo]], algo)
      if (!is.null(result)) {
        performance[[algo]] <- result$performance
        predictions_list[[algo]] <- result$predictions
      }
    }
  }

  # Return both performance and predictions as a named list
  return(list(performance = performance, predictions = predictions_list))
}
