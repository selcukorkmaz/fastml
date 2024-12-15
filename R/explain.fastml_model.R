#' Explain model (Generic)
#'
#' A generic function to provide explanations for models.
#' Different objects can have specialized methods, such as \code{explain.fastml_model}.
#'
#' @param x An object to explain.
#' @param ... Additional arguments passed to the method.
#' @export
explain <- function(x, ...) {
  UseMethod("explain")
}


#' Explain the fastml_model (Only DALEX + SHAP + Permutation-based VI)
#'
#' Provides model explainability using DALEX only. This function:
#' - Creates a DALEX explainer.
#' - Computes permutation-based variable importance with boxplots showing variability, displays the table and plot.
#' - Computes partial dependency-like model profiles if `features` are provided.
#' - Computes Shapley values (SHAP) for a sample of the training observations, displays the SHAP table,
#'   and plots a summary bar chart of mean(|SHAP value|) per feature. For classification, it shows separate bars for each class.
#'
#' The bars in the variable importance plot start at the RMSE value for the model on the original data (x-axis),
#' and the length of each bar corresponds to the RMSE loss after permutations.
#' Boxplots show how random permutations differ, indicating stability.
#'
#' For SHAP:
#' - We use the individual SHAP values from `predict_parts(..., type="shap")`.
#' - We aggregate mean absolute SHAP values by feature (and by class if classification).
#' - Class names are taken from the model predictions (the 'label' column if available).
#' - We print the SHAP table and then produce a summary plot similar to the provided example.
#'
#' @param object A \code{fastml_model} object.
#' @param method Currently only \code{"dalex"} is supported.
#' @param features Character vector of feature names for partial dependence (model profiles). Default NULL.
#' @param grid_size Number of grid points for partial dependence. Default 20.
#' @param shap_sample Integer number of observations from processed training data to compute SHAP values for. Default 5.
#' @param ... Additional arguments (not currently used).
#'
#' @return Prints DALEX explanations: variable importance table & plot, model profiles (if any), SHAP table & summary plot.
#' @export
explain.fastml_model <- function(object,
                                 method = "dalex",
                                 features = NULL,
                                 grid_size = 20,
                                 shap_sample = 5,
                                 ...) {
  if (!inherits(object, "fastml_model")) {
    stop("The input must be a 'fastml_model' object.")
  }

  if (method != "dalex") {
    stop("Only 'dalex' method is supported in this simplified version.")
  }

  best_model <- object$best_model
  task <- object$task
  label <- object$label

  if (is.null(object$processed_train_data) || !(label %in% names(object$processed_train_data))) {
    cat("\nCannot create DALEX explainer without processed training data and a target variable.\n")
    return(invisible(NULL))
  }

  train_data <- object$processed_train_data
  x <- train_data %>% dplyr::select(-!!label)
  y <- train_data[[label]]

  # Determine if classification and identify positive_class for binary
  positive_class <- NULL
  if (task == "classification") {
    if (is.factor(y) && length(levels(y)) == 2) {
      positive_class <- levels(y)[2]
    }
  }

  # Prediction function for DALEX
  pred_fun_for_pdp <- function(m, newdata) {
    if (task == "classification") {
      p <- predict(m, new_data = newdata, type = "prob")
      all_prob_cols <- grep("^\\.pred_", names(p), value = TRUE)
      if (!is.null(positive_class)) {
        pred_col_name <- paste0(".pred_", positive_class)
      } else {
        pred_col_name <- all_prob_cols[1]
      }
      if (!pred_col_name %in% names(p)) {
        stop("Prediction column ", pred_col_name, " not found. Check class names.")
      }
      as.numeric(p[[pred_col_name]])
    } else {
      # regression
      p <- predict(m, new_data = newdata, type = "numeric")
      as.numeric(p$.pred)
    }
  }

  model_info <- if (task == "classification") list(type = "classification") else list(type = "regression")

  parsnip_fit <- tryCatch(tune::extract_fit_parsnip(best_model), error = function(e) NULL)
  if (is.null(parsnip_fit) && inherits(best_model, "model_fit")) {
    parsnip_fit <- best_model
  }

  if (is.null(parsnip_fit)) {
    cat("Could not extract a parsnip model from the workflow.\nDALEX explainer may fail.\n")
    return(invisible(NULL))
  }

  if (!requireNamespace("DALEX", quietly = TRUE)) {
    stop("The 'DALEX' package is required.")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The 'ggplot2' package is required for plotting.")
  }

  predict_function <- function(m, newdata) {
    pred_fun_for_pdp(m, newdata)
  }

  exp_try <- try({
    explainer <- DALEX::explain(
      model = parsnip_fit,
      data = x,
      y = if (is.numeric(y)) y else as.numeric(y),
      label = object$best_model_name,
      predict_function = predict_function,
      model_info = model_info
    )

    cat("\n=== DALEX Variable Importance (with Boxplots) ===\n")
    vi <- DALEX::model_parts(
      explainer,
      B = 10,
      type = "raw",
      loss_function = DALEX::loss_root_mean_square
    )
    # Print the variable importance table
    cat("\nVariable Importance Table:\n")
    print(vi)

    # Plot with boxplots to show variation
    vi_plot <- plot(vi, show_boxplots = TRUE)
    print(vi_plot)

    if (!is.null(features)) {
      cat("\n=== DALEX Model Profiles (Partial Dependence) ===\n")
      mp <- DALEX::model_profile(explainer, variables = features, N = grid_size)
      print(mp)
    }

    # Compute SHAP (Shapley) values for a sample of training data
    shap_data <- train_data[1:min(shap_sample, nrow(train_data)), , drop = FALSE]
    cat("\n=== DALEX Shapley Values (SHAP) ===\n")
    shap <- DALEX::predict_parts(explainer, new_observation = train_data, type = "shap")
    # Print the shap values table
    print(shap)

    # Create a summary SHAP plot:
    # We have one row per feature-observation(-label). Compute mean(|SHAP|) by feature and label if classification
    shap$abs_contribution <- abs(shap$contribution)

    # Determine grouping variables
    if (task == "classification" && "label" %in% names(shap)) {
      group_vars <- object$processed_train_data[[object$label]] %>% levels()
    } else {
      group_vars <- c("feature")
    }

    print(plot(shap), title = "sds")

  }, silent = TRUE)

  if (inherits(exp_try, "try-error")) {
    cat("DALEX explanations not available for this model.\n")
  }

  invisible(NULL)
}

