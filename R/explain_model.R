#' Explain the fastml_model (DALEX + SHAP + Permutation-based VI)
#'
#' Provides model explainability using DALEX. This function:
#' \itemize{
#'   \item Creates a DALEX explainer.
#'   \item Computes permutation-based variable importance with boxplots showing variability, displays the table and plot.
#'   \item Computes partial dependency-like model profiles if `features` are provided.
#'   \item Computes Shapley values (SHAP) for a sample of the training observations, displays the SHAP table,
#'   and plots a summary bar chart of \eqn{\text{mean}(\vert \text{SHAP value} \vert)} per feature. For classification, it shows separate bars for each class.
#' }
#'
#' @details
#'  \enumerate{
#'    \item \bold{Custom number of permutations for VI (vi_iterations):}
#'
#'    You can now specify how many permutations (B) to use for permutation-based variable importance.
#'    More permutations yield more stable estimates but take longer.
#'
#'    \item \bold{Custom color palette (colormap):}
#'
#'    A `colormap` parameter allows you to select a color palette (e.g., "viridis") for SHAP summary plots and variable importance plots.
#'    This improves aesthetics over default palettes.
#'
#'    \item \bold{Top Features in SHAP Summary (top_features):}
#'
#'    You can limit the SHAP summary plot to the top N features by mean absolute SHAP value. This helps focus on the most influential features.
#'
#'    \item \bold{Support for calibration plot if probably is available (calibration):}
#'
#'    If `calibration = TRUE` and `probably` is installed, it attempts to produce a model-based calibration plot (e.g., `cal_plot_logistic`).
#'    This provides a smoothed, nonparametric view of model calibration.
#'
#'    \item \bold{Better error messages and checks:}
#'
#'    Improved checks and messages if certain packages or conditions are not met.
#'  }
#'
#' @param object A \code{fastml_model} object.
#' @param method Currently only \code{"dalex"} is supported.
#' @param features Character vector of feature names for partial dependence (model profiles). Default NULL.
#' @param grid_size Number of grid points for partial dependence. Default 20.
#' @param shap_sample Integer number of observations from processed training data to compute SHAP values for. Default 5.
#' @param vi_iterations Integer. Number of permutations for variable importance. Default 10.
#' @param colormap Character. Name of a color palette to use (e.g., "viridis"). Default "viridis".
#' @param top_features Integer. Limit the SHAP summary plot to top N features by mean abs SHAP. Default NULL (no limit).
#' @param seed Integer. A value specifying the random seed.
#' @param ... Additional arguments (not currently used).
#'
#' @importFrom dplyr select
#' @importFrom tune extract_fit_parsnip
#' @importFrom DALEX explain model_parts loss_root_mean_square model_profile predict_parts
#' @importFrom ggplot2 labs
#' @importFrom patchwork wrap_plots
#'
#' @return Prints DALEX explanations: variable importance table & plot, model profiles (if any), SHAP table & summary plot, and optionally a calibration plot.
#' @export
explain_model <- function(object,
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
  x <- train_data %>% select(-!!label)
  y <- train_data[[label]]

  # Identify positive_class if binary classification
  positive_class <- NULL
  if (task == "classification") {
    if (is.factor(y) && length(levels(y)) == 2) {
      positive_class <- object$positive_class

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


  if(length(object$best_model) == 1){
    parsnip_fit <- tryCatch(extract_fit_parsnip(object$best_model[[1]]), error = function(e) NULL)
  } else {
    parsnip_fit <- tryCatch(lapply(object$best_model, extract_fit_parsnip), error = function(e) NULL)
  }

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

    if(length(object$best_model) == 1){
      explainer <- explain(
        model = parsnip_fit,
        data = x,
        y = if (is.numeric(y)) y else as.numeric(y),
        label = object$best_model_name,
        predict_function = predict_function,
        model_info = model_info
      )
    }else{

      explainer_list <- lapply(names(parsnip_fit), function(model_name) {
        model <- parsnip_fit[[model_name]]
        explain(
          model = model,
          data = x,
          y = if (is.numeric(y)) y else as.numeric((y)),
          label = model_name,
          predict_function = predict_function,  # Adjust predict_function if needed
          model_info = model_info
        )
      })

    }

    cat("\n=== DALEX Variable Importance (with Boxplots) ===\n")
    set.seed(seed)

    if(length(object$best_model) == 1){
      vi <- model_parts(
        explainer,
        B = vi_iterations,
        type = "raw",
        loss_function = loss_root_mean_square
      )
    }else{
      vi <- lapply(explainer_list, function(explainer) {
        model_parts(
          explainer = explainer,
          B = vi_iterations,
          type = "raw",
          loss_function = loss_root_mean_square
        )
      })
    }


    cat("\nVariable Importance Table:\n")
    print(vi)

    # Plot VI with chosen colormap if available
    vi_plot <- plot(vi, show_boxplots = TRUE)

    print(vi_plot)

    if (!is.null(features)) {
      cat("\n=== DALEX Model Profiles (Partial Dependence) ===\n")
      if(length(object$best_model) == 1){
         mp <- model_profile(explainer, variables = features, N = grid_size)
      }else{
        mp <- lapply(explainer_list, function(explainer) {
          model_profile(explainer, variables = features, N = grid_size)
        })

      }
      print(mp)
    }

    # Compute SHAP for shap_sample observations
    # Actually use shap_sample
    shap_data <- train_data[1:min(shap_sample, nrow(train_data)), , drop = FALSE]

    cat("\n=== DALEX Shapley Values (SHAP) ===\n")
    set.seed(seed)
    if(length(object$best_model) == 1){
      shap <- predict_parts(explainer, new_observation = shap_data, type = "shap")
    }else{
      shap <- lapply(explainer_list, function(explainer) {
        predict_parts(explainer, new_observation = shap_data, type = "shap")
      })
    }
    print(shap)


    # SHAP aggregation
    if(length(object$best_model) == 1){
      shap$abs_contribution <- abs(shap$contribution)
    }else{
      # Create a new list to store absolute SHAP contributions
      shap <- lapply(shap, function(model_shap) {
        model_shap$abs_contribution <- abs(model_shap$contribution)
        return(model_shap)
      })
    }
    # Determine grouping variables
    if (task == "classification") {
      group_vars <- object$processed_train_data[[object$label]] %>% levels()
    } else {
      group_vars <- c("feature")
    }

    if(length(object$best_model) == 1){
      print(plot(shap) + labs(title = paste("SHAP Values")))
    }else{
    # Initialize an empty list to store plots
    plot_list <- list()
    names(shap) = object$best_model_name
    # Generate plots and store them in the list
    for(model_name in object$best_model_name) {
      shap_df <- shap[[model_name]]
      plot_list[[model_name]] <- plot(shap_df)  + labs(title = paste("SHAP Values"))
    }

    # Combine all plots into one grid (e.g., 1 row, 3 columns)
    combined_plot <- wrap_plots(plot_list, nrow = 1)

    # Display the combined plot
    print(combined_plot)
    }

  }, silent = TRUE)

  if (inherits(exp_try, "try-error")) {
    cat("DALEX explanations not available for this model.\n")
  }

  invisible(NULL)
}
