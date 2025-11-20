#' Generate DALEX explanations for a fastml model
#'
#' Creates a DALEX explainer and computes permutation based variable importance,
#' partial dependence (model profiles) and Shapley values.
#'
#' @inheritParams fastexplain
#' @return Invisibly returns a list with variable importance, optional model
#'   profiles and SHAP values.
#' @importFrom dplyr select
#' @importFrom tune extract_fit_parsnip
#' @importFrom DALEX explain model_parts loss_root_mean_square loss_cross_entropy model_profile predict_parts
#' @importFrom ggplot2 labs
#' @importFrom stats predict
#' @export
explain_dalex <- function(object,
                          features = NULL,
                          grid_size = 20,
                          shap_sample = 5,
                          vi_iterations = 10,
                          seed = 123,
                          loss_function = NULL) {
  if (!inherits(object, "fastml")) {
    stop("The input must be a 'fastml' object.")
  }

  prep <- fastml_prepare_explainer_inputs(object)
  task <- prep$task
  label <- prep$label
  x <- prep$x
  y <- prep$y
  train_data <- prep$train_data
  model_names <- prep$model_names
  fits <- prep$fits

  if(!is.null(features)){
    features <- sanitize(features)
  }

  pred_fun_for_pdp <- function(m, newdata) {
    if (task == "classification") {
      p <- predict(m, new_data = newdata, type = "prob")
      colnames(p) <- sub("^\\.pred_", "", colnames(p))
      return(as.data.frame(p))
    } else {
      p <- predict(m, new_data = newdata, type = "numeric")
      return(as.numeric(p$.pred))
    }
  }

  model_info <- if (task == "classification") list(type = "classification") else list(type = "regression")

  if (!requireNamespace("DALEX", quietly = TRUE)) {
    stop("The 'DALEX' package is required.")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The 'ggplot2' package is required for plotting.")
  }

  predict_function <- function(m, newdata) {
    pred_fun_for_pdp(m, newdata)
  }

  if (is.null(loss_function)) {
    if (task == "classification") {
      loss_function <- loss_cross_entropy
    } else {
      loss_function <- loss_root_mean_square
    }
  }

  exp_try <- try({
    if(length(fits) == 1){
      explainer <- explain(
        model = fits[[1]],
        data = x,
        y = if (is.numeric(y)) y else as.numeric(y),
        label = if (length(model_names) == 1) model_names[[1]] else object$best_model_name,
        predict_function = predict_function,
        model_info = model_info
      )
    } else {
      explainer_list <- lapply(seq_along(fits), function(i) {
        model <- fits[[i]]
        model_name <- if (length(model_names) >= i) model_names[[i]] else paste0("model_", i)
        explain(
          model = model,
          data = x,
          y = if (is.numeric(y)) y else as.numeric(y),
          label = model_name,
          predict_function = predict_function,
          model_info = model_info
        )
      })
    }

    cat("\n=== DALEX Variable Importance (with Boxplots) ===\n")
    set.seed(seed)

    if(length(fits) == 1){
      vi <- model_parts(
        explainer,
        B = vi_iterations,
        type = "raw",
        loss_function = loss_function
      )
    } else {
      vi <- lapply(explainer_list, function(expl) {
        model_parts(
          explainer = expl,
          B = vi_iterations,
          type = "raw",
          loss_function = loss_function
        )
      })
      names(vi) <- model_names
    }

    vi_plot <- suppressWarnings(plot(vi, show_boxplots = TRUE))
    print(vi_plot)

    mp <- NULL
    if (!is.null(features)) {
      cat("\n=== DALEX Model Profiles (Partial Dependence) ===\n")
      if(length(fits) == 1){
        mp <- model_profile(explainer, variables = features, N = grid_size)
      } else {
        mp <- lapply(explainer_list, function(expl) {
          model_profile(expl, variables = features, N = grid_size)
        })
        names(mp) <- model_names
      }
    }

    shap_data <- train_data[1:min(shap_sample, nrow(train_data)), , drop = FALSE]

    cat("\n=== DALEX Shapley Values (SHAP) ===\n")
    set.seed(seed)
    if(length(fits) == 1){
      shap <- predict_parts(explainer, new_observation = shap_data, type = "shap")
    } else {
      shap <- lapply(explainer_list, function(expl) {
        predict_parts(expl, new_observation = shap_data, type = "shap")
      })
      names(shap) <- model_names
    }

    if(length(fits) == 1){
      shap$abs_contribution <- abs(shap$contribution)
    } else {
      shap <- lapply(shap, function(model_shap) {
        model_shap$abs_contribution <- abs(model_shap$contribution)
        return(model_shap)
      })
    }

    if (task == "classification") {
      group_vars <- object$processed_train_data[[object$label]] %>% levels()
    } else {
      group_vars <- c("feature")
    }

    if(length(object$best_model) == 1){
      suppressWarnings(print(plot(shap) + labs(title = paste("SHAP Values"))))
    } else {
      plot_list <- list()
      for(model_name in names(shap)) {
        shap_df <- shap[[model_name]]
        plot_list[[model_name]] <- suppressWarnings(plot(shap_df) + labs(title = paste("SHAP Values:", model_name)))
      }
      combined_plot <- patchwork::wrap_plots(plot_list, nrow = 1)
      print(combined_plot)
    }

  }, silent = TRUE)

  if (inherits(exp_try, "try-error")) {
    cat("DALEX explanations not available for this model.\n")
  }

  if(!is.null(mp)){
    result <- list(variable_importance = vi,
                   model_profiles = mp,
                   shap_values = shap)
  } else {
    result <- list(variable_importance = vi,
                   shap_values = shap)
  }

  invisible(result)
}
