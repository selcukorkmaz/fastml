#' Generate DALEX explanations for a fastml model
#'
#' Creates a DALEX explainer and computes permutation based variable importance,
#' partial dependence (model profiles) and Shapley values.
#'
#' @inheritParams fastexplain
#' @return Invisibly returns a list with variable importance, optional model profiles and SHAP values.
#' @export
explain_dalex <- function(object,
                          features = NULL,
                          grid_size = 20,
                          shap_sample = 5,
                          vi_iterations = 10,
                          seed = 123,
                          loss_function = NULL) {
  prep <- fastml_prepare_explainer_inputs(object)
  explainers <- fastml_build_dalex_explainers(prep)$explainers
  explain_dalex_internal(
    explainers = explainers,
    prep = prep,
    features = features,
    grid_size = grid_size,
    shap_sample = shap_sample,
    vi_iterations = vi_iterations,
    seed = seed,
    loss_function = loss_function
  )
}

#' @keywords internal
fastml_build_dalex_explainers <- function(prep) {
  if (!requireNamespace("DALEX", quietly = TRUE)) {
    stop("The 'DALEX' package is required.")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The 'ggplot2' package is required for plotting.")
  }

  model_info <- if (prep$task == "classification") list(type = "classification") else list(type = "regression")

  predict_function <- function(m, newdata) {
    if (prep$task == "classification") {
      p <- predict(m, new_data = newdata, type = "prob")
      colnames(p) <- sub("^\\.pred_", "", colnames(p))
      p <- as.data.frame(p)
      # For binary classification, return only the positive class probability to avoid redundant outputs
      if (ncol(p) == 2) {
        # Choose the second column by convention (assumes .pred_[pos])
        p <- p[[2]]
      }
      return(p)
    } else {
      p <- predict(m, new_data = newdata, type = "numeric")
      return(as.numeric(p$.pred))
    }
  }

  build_one <- function(model, name) {
    DALEX::explain(
      model = model,
      data = prep$x,
      y = if (is.numeric(prep$y)) prep$y else as.numeric(prep$y),
      label = name,
      predict_function = predict_function,
      model_info = model_info
    )
  }

  explainers <- if (length(prep$fits) == 1) {
    list(build_one(prep$fits[[1]], if (length(prep$model_names)) prep$model_names[[1]] else "model"))
  } else {
    mapply(
      build_one,
      model = prep$fits,
      name = if (length(prep$model_names)) prep$model_names else paste0("model_", seq_along(prep$fits)),
      SIMPLIFY = FALSE
    )
  }

  list(explainers = explainers)
}

#' @keywords internal
explain_dalex_internal <- function(explainers,
                                   prep,
                                   features = NULL,
                                   grid_size = 20,
                                   shap_sample = 5,
                                   vi_iterations = 10,
                                   seed = 123,
                                   loss_function = NULL) {
  task <- prep$task
  train_data <- prep$train_data
  model_names <- if (length(prep$model_names)) prep$model_names else paste0("model_", seq_along(explainers))

  if (!is.null(features)) {
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

  predict_function <- function(m, newdata) {
    pred_fun_for_pdp(m, newdata)
  }

  if (is.null(loss_function)) {
    if (task == "classification") {
      loss_function <- DALEX::loss_cross_entropy
    } else {
      loss_function <- DALEX::loss_root_mean_square
    }
  }

  exp_try <- try({
    explainer <- if (length(explainers) == 1) explainers[[1]] else NULL
    explainer_list <- if (length(explainers) > 1) explainers else NULL

    cat("\n=== DALEX Variable Importance (with Boxplots) ===\n")
    set.seed(seed)

    if (length(explainers) == 1) {
      vi <- DALEX::model_parts(
        explainer,
        B = vi_iterations,
        type = "raw",
        loss_function = loss_function
      )
    } else {
      vi <- lapply(explainer_list, function(expl) {
        DALEX::model_parts(
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
      if (length(explainers) == 1) {
        mp <- DALEX::model_profile(explainer, variables = features, N = grid_size)
      } else {
        mp <- lapply(explainer_list, function(expl) {
          DALEX::model_profile(expl, variables = features, N = grid_size)
        })
        names(mp) <- model_names
      }
    }

    shap_data <- train_data[1:min(shap_sample, nrow(train_data)), , drop = FALSE]

    cat("\n=== DALEX Shapley Values (SHAP) ===\n")
    set.seed(seed)
    if (length(explainers) == 1) {
      shap <- DALEX::predict_parts(explainer, new_observation = shap_data, type = "shap")
    } else {
      shap <- lapply(explainer_list, function(expl) {
        DALEX::predict_parts(expl, new_observation = shap_data, type = "shap")
      })
      names(shap) <- model_names
    }

    if (length(explainers) == 1) {
      shap$abs_contribution <- abs(shap$contribution)
    } else {
      shap <- lapply(shap, function(model_shap) {
        model_shap$abs_contribution <- abs(model_shap$contribution)
        return(model_shap)
      })
    }

    if (length(explainers) == 1) {
      suppressWarnings(print(plot(shap) + ggplot2::labs(title = paste("SHAP Values"))))
    } else {
      plot_list <- list()
      for (model_name in names(shap)) {
        shap_df <- shap[[model_name]]
        plot_list[[model_name]] <- suppressWarnings(plot(shap_df) + ggplot2::labs(title = paste("SHAP Values:", model_name)))
      }
      combined_plot <- patchwork::wrap_plots(plot_list, nrow = 1)
      print(combined_plot)
    }

  }, silent = TRUE)

  if (inherits(exp_try, "try-error")) {
    cat("DALEX explanations not available for this model.\n")
  }

  if (!is.null(mp)) {
    result <- list(variable_importance = vi,
                   model_profiles = mp,
                   shap_values = shap)
  } else {
    result <- list(variable_importance = vi,
                   shap_values = shap)
  }

  invisible(result)
}
