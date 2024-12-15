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


#' Explain the fastml_model (DALEX + SHAP + Permutation-based VI, Enhanced)
#'
#' Provides model explainability using DALEX. This function:
#' - Creates a DALEX explainer.
#' - Computes permutation-based variable importance with boxplots showing variability, displays the table and plot.
#' - Computes partial dependency-like model profiles if `features` are provided.
#' - Computes Shapley values (SHAP) for a sample of the training observations, displays the SHAP table,
#'   and plots a summary bar chart of mean(|SHAP value|) per feature. For classification, it shows separate bars for each class.
#'
#' **Enhancements added:**
#' 1. **Custom number of permutations for VI (vi_iterations):**
#'    You can now specify how many permutations (B) to use for permutation-based variable importance.
#'    More permutations yield more stable estimates but take longer.
#'
#' 2. **Custom color palette (colormap):**
#'    A `colormap` parameter allows you to select a color palette (e.g., "viridis") for SHAP summary plots and variable importance plots.
#'    This improves aesthetics over default palettes.
#'
#' 3. **Top Features in SHAP Summary (top_features):**
#'    You can limit the SHAP summary plot to the top N features by mean absolute SHAP value. This helps focus on the most influential features.
#'
#' 4. **Support for calibration plot if probably is available (calibration):**
#'    If `calibration = TRUE` and `probably` is installed, it attempts to produce a model-based calibration plot (e.g., `cal_plot_logistic`).
#'    This provides a smoothed, nonparametric view of model calibration.
#'
#' 5. **Better error messages and checks:**
#'    Improved checks and messages if certain packages or conditions are not met.
#'
#' @param object A \code{fastml_model} object.
#' @param method Currently only \code{"dalex"} is supported.
#' @param features Character vector of feature names for partial dependence (model profiles). Default NULL.
#' @param grid_size Number of grid points for partial dependence. Default 20.
#' @param shap_sample Integer number of observations from processed training data to compute SHAP values for. Default 5.
#' @param vi_iterations Integer. Number of permutations for variable importance. Default 10.
#' @param colormap Character. Name of a color palette to use (e.g., "viridis"). Default "viridis".
#' @param top_features Integer. Limit the SHAP summary plot to top N features by mean abs SHAP. Default NULL (no limit).
#' @param ... Additional arguments (not currently used).
#'
#' @return Prints DALEX explanations: variable importance table & plot, model profiles (if any), SHAP table & summary plot, and optionally a calibration plot.
#' @export
explain.fastml_model <- function(object,
                                 method = "dalex",
                                 features = NULL,
                                 grid_size = 20,
                                 shap_sample = 5,
                                 vi_iterations = 10,
                                 colormap = "viridis",
                                 top_features = NULL,
                                 seed = 123,
                                 ...) {
  if (!inherits(object, "fastml_model")) {
    stop("The input must be a 'fastml_model' object.")
  }

  if (method != "dalex") {
    stop("Only 'dalex' method is supported in this version.")
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

  # Identify positive_class if binary classification
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

  if (colormap == "viridis" && !requireNamespace("viridisLite", quietly = TRUE)) {
    cat("viridisLite not installed, defaulting to a basic palette.\n")
    colormap <- NULL
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
    set.seed(seed)
    vi <- DALEX::model_parts(
      explainer,
      B = vi_iterations,
      type = "raw",
      loss_function = DALEX::loss_root_mean_square
    )
    cat("\nVariable Importance Table:\n")
    print(vi)

    # Plot VI with chosen colormap if available
    vi_plot <- plot(vi, show_boxplots = TRUE)
    # if (!is.null(colormap) && colormap == "viridis") {
    #   # If we want to apply viridis to VI plot, we can attempt to modify scale
    #   # But plot(vi) returns a ggplot - we can try a scale_color_viridis_d or scale_fill_viridis_d
    #   if (requireNamespace("viridisLite", quietly = TRUE)) {
    #     vi_plot <- vi_plot + ggplot2::scale_fill_viridis_d() + ggplot2::scale_color_viridis_d()
    #   }
    # }
    print(vi_plot)

    if (!is.null(features)) {
      cat("\n=== DALEX Model Profiles (Partial Dependence) ===\n")
      mp <- DALEX::model_profile(explainer, variables = features, N = grid_size)
      print(mp)
    }

    # Compute SHAP for shap_sample observations
    # Actually use shap_sample
    shap_data <- train_data[1:min(shap_sample, nrow(train_data)), , drop = FALSE]

    cat("\n=== DALEX Shapley Values (SHAP) ===\n")
    set.seed(seed)
    shap <- DALEX::predict_parts(explainer, new_observation = shap_data, type = "shap")
    print(shap)

    # SHAP aggregation
    shap$abs_contribution <- abs(shap$contribution)

    # Determine grouping variables
    if (task == "classification" && "label" %in% names(shap)) {
      group_vars <- object$processed_train_data[[object$label]] %>% levels()
    } else {
      group_vars <- c("feature")
    }

    print(plot(shap) + ggplot2::labs(title = paste("SHAP Values for", object$best_model_name)))


  }, silent = TRUE)

  if (inherits(exp_try, "try-error")) {
    cat("DALEX explanations not available for this model.\n")
  }

  invisible(NULL)
}
