#' Evaluate Models Function
#'
#' Evaluates the trained models on the test data and computes performance metrics.
#'
#' @param models A list of trained model objects.
#' @param train_data Preprocessed training data frame.
#' @param test_data Preprocessed test data frame.
#' @param label Name of the target variable. For survival analysis this should
#'   be a character vector of length two giving the names of the time and status
#'   columns.
#' @param start_col Optional string. The name of the column specifying the
#'   start time in counting process (e.g., `(start, stop, event)`) survival
#'   data. Only used when \code{task = "survival"}.
#' @param time_col String. The name of the column specifying the event or
#'   censoring time (the "stop" time in counting process data). Only used
#'   when \code{task = "survival"}.
#' @param status_col String. The name of the column specifying the event
#'   status (e.g., 0 for censored, 1 for event). Only used when
#'   \code{task = "survival"}.
#' @param task Type of task: "classification", "regression", or "survival".
#' @param metric The performance metric to optimize (e.g., "accuracy", "rmse").
#' @param event_class A single string. Either "first" or "second" to specify which level of truth to consider as the "event".
#' @importFrom dplyr filter bind_rows pull mutate select bind_cols
#' @importFrom yardstick metric_set accuracy kap roc_auc sens spec precision f_meas rmse rsq mae
#' @importFrom workflows pull_workflow_spec pull_workflow_preprocessor workflow add_model add_recipe
#' @importFrom parsnip fit predict.model_fit
#' @importFrom tune select_best finalize_model
#' @importFrom rlang sym syms
#' @importFrom tibble tibble
#' @param eval_times Optional numeric vector of evaluation horizons for survival
#'   metrics. Passed through to \code{process_model}.
#' @param bootstrap_ci Logical indicating whether bootstrap confidence intervals
#'   should be computed for the evaluation metrics.
#' @param bootstrap_samples Number of bootstrap resamples used when
#'   \code{bootstrap_ci = TRUE}.
#' @param bootstrap_seed Optional integer seed for the bootstrap procedure used
#'   in metric estimation.
#' @param at_risk_threshold Minimum proportion of subjects that must remain at
#'   risk to define \eqn{t_{max}} when computing survival metrics such as the
#'   integrated Brier score.
#' @return A list with two elements:
#'   \describe{
#'     \item{performance}{A named list of performance metric tibbles for each model.}
#'     \item{predictions}{A named list of data frames with columns including truth, predictions, and probabilities per model.}
#'   }
#' @export
evaluate_models <- function(models,
                            train_data,
                            test_data,
                            label,
                            start_col,
                            time_col,
                            status_col,
                            task,
                            metric = NULL,
                            event_class,
                            eval_times = NULL,
                            bootstrap_ci = TRUE,
                            bootstrap_samples = 500,
                            bootstrap_seed = 1234,
                            at_risk_threshold = 0.1) {
  # Load required packages
  required_pkgs <- c("yardstick", "parsnip", "tune", "workflows",
                     "dplyr", "rlang", "tibble")
  if (task == "survival") {
    required_pkgs <- c(required_pkgs, "survival")
  }
  for(pkg in required_pkgs){
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("The '%s' package is required but not installed.", pkg))
    }
  }

  # Determine engine names for each algorithm
  engine_names <- get_engine_names(models)
  # Initialize performance and predictions lists
  performance <- list()
  predictions_list <- list()

  if (task != "survival") {
    true_labels <- test_data[[label]]
  }


  # Iterate over the models object. Check if the model is nested (i.e. a list of engines)
  for (algo in names(models)) {
    performance[[algo]] <- list()
    predictions_list[[algo]] <- list()

    # If the model object has names, assume it is nested by engine.
    # But skip this for native survival models stored as lists.
    if (is.list(models[[algo]]) && !inherits(models[[algo]], "workflow") && !inherits(models[[algo]], "tune_results") && !inherits(models[[algo]], "fastml_native_survival")) {
      for (eng in names(models[[algo]])) {
        model_obj <- models[[algo]][[eng]]
        result <- process_model(model_obj,
                                model_id = paste(algo, eng, sep = "_"),
                                task = task,
                                test_data = test_data,
                                label = label,
                                event_class = event_class,
                                engine = eng,
                                train_data = train_data,
                                metric = metric,
                                eval_times_user = eval_times,
                                bootstrap_ci = bootstrap_ci,
                                bootstrap_samples = bootstrap_samples,
                                bootstrap_seed = bootstrap_seed,
                                at_risk_threshold = at_risk_threshold)
        if (!is.null(result)) {
          performance[[algo]][[eng]] <- result$performance
          predictions_list[[algo]][[eng]] <- result$predictions
        }
      }
    } else {
      # Otherwise, assume a single model (not nested)
      eng <- if (inherits(models[[algo]], "fastml_native_survival")) {
        models[[algo]]$engine
      } else if (!is.null(engine_names[[algo]])) {
        engine_names[[algo]][1]
      } else if (inherits(models[[algo]], "workflow")) {
        workflows::extract_fit_parsnip(models[[algo]])$spec$engine
      } else {
        NA
      }
      result <- process_model(model_obj = models[[algo]],
                              model_id = algo,
                              task = task,
                              test_data = test_data,
                              label = label,
                              start_col = start_col,
                              time_col = time_col,
                              status_col = status_col,
                              event_class = event_class,
                              engine = eng,
                              train_data = train_data,
                              metric = metric,
                              eval_times_user = eval_times,
                              bootstrap_ci = bootstrap_ci,
                              bootstrap_samples = bootstrap_samples,
                              bootstrap_seed = bootstrap_seed,
                              at_risk_threshold = at_risk_threshold)



      if (!is.null(result)) {
        performance[[algo]] <- result$performance
        predictions_list[[algo]] <- result$predictions
      }
    }
  }

  # Return both performance and predictions as a named list
  return(list(performance = performance, predictions = predictions_list))
}
