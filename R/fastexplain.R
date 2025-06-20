#' Explain a fastml model using various techniques
#'
#' Provides model explainability. When `method = "dalex"` this function:
#' \itemize{
#'   \item Creates a DALEX explainer.
#'   \item Computes permutation-based variable importance with boxplots showing variability, displays the table and plot.
#'   \item Computes partial dependence-like model profiles if `features` are provided.
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
#'    \item \bold{Better error messages and checks:}
#'
#'    Improved checks and messages if certain packages or conditions are not met.
#'
#'    \item \bold{Loss Function:}
#'
#'    A \code{loss_function} argument has been added to let you pick a different performance measure (e.g., \code{loss_cross_entropy} for classification, \code{loss_root_mean_square} for regression).
#'
#'    \item \bold{Parallelization Suggestion:}
#'
#'  }
#'
#' @param object A \code{fastml} object.
#' @param method Character string specifying the explanation method.
#'   Supported values are \code{"dalex"}, \code{"lime"}, \code{"ice"},
#'   \code{"ale"}, \code{"surrogate"}, \code{"interaction"}, and
#'   \code{"counterfactual"}. Defaults to \code{"dalex"}.
#' @param features Character vector of feature names for partial dependence (model profiles). Default NULL.
#' @param observation A single observation for counterfactual explanations. Default NULL.
#' @param grid_size Number of grid points for partial dependence. Default 20.
#' @param shap_sample Integer number of observations from processed training data to compute SHAP values for. Default 5.
#' @param vi_iterations Integer. Number of permutations for variable importance (B). Default 10.
#' @param seed Integer. A value specifying the random seed.
#' @param loss_function Function. The loss function for \code{model_parts}.
#'   \itemize{
#'     \item If \code{NULL} and task = 'classification', defaults to \code{DALEX::loss_cross_entropy}.
#'     \item If \code{NULL} and task = 'regression', defaults to \code{DALEX::loss_root_mean_square}.
#'   }
#' @param ... Additional arguments passed to the underlying helper functions.
#'
#' @importFrom dplyr select
#' @importFrom tune extract_fit_parsnip
#' @importFrom DALEX explain model_parts loss_root_mean_square loss_cross_entropy model_profile predict_parts
#' @importFrom ggplot2 labs
#' @importFrom patchwork wrap_plots
#' @importFrom stats predict
#'
#' @return Prints DALEX explanations: variable importance table & plot, model profiles (if any), and SHAP table & summary plot.
#' @export
fastexplain <- function(object,
                          method = "dalex",
                          features = NULL,
                          observation = NULL,
                          grid_size = 20,
                          shap_sample = 5,
                          vi_iterations = 10,
                          seed = 123,
                          loss_function = NULL,
                          ...) {

  if (!inherits(object, "fastml")) {
    stop("The input must be a 'fastml' object.")
  }

  method <- tolower(method)

  if (method == "lime") {
    return(explain_lime(object, ...))
  } else if (method == "ice") {
    return(plot_ice(object, features = features, ...))
  } else if (method == "ale") {
    if (is.null(features) || length(features) == 0) {
      stop("'features' must contain a feature name for ALE explanations.")
    }
    return(explain_ale(object, feature = features[1], ...))
  } else if (method == "surrogate") {
    return(surrogate_tree(object, ...))
  } else if (method == "interaction") {
    return(interaction_strength(object, ...))
  } else if (method == "counterfactual") {
    if (is.null(observation)) {
      stop("'observation' must be provided for counterfactual explanations.")
    }
    return(counterfactual_explain(object, observation, ...))
  } else if (method != "dalex") {
    stop("Unknown explanation method.")
  }

  best_model <- object$best_model
  task <- object$task
  label <- object$label

  if(!is.null(features)){
    features = sanitize(features)
  }

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
      # 1) Get the probabilities for *all classes*.
      p <- predict(m, new_data = newdata, type = "prob")

      # p typically has columns like: .pred_versicolor, .pred_virginica, etc.
      # We want to return the whole data frame, not just one column.

      # 2) Optionally rename columns (remove the ".pred_" prefix, for clarity).
      #    This step helps if DALEX uses factor levels as column names internally.
      colnames(p) <- sub("^\\.pred_", "", colnames(p))

      # Return the full data frame of probabilities.
      return(as.data.frame(p))

    } else {
      # regression
      p <- predict(m, new_data = newdata, type = "numeric")
      return(as.numeric(p$.pred))
    }
  }


  model_info <- if (task == "classification") list(type = "classification") else list(type = "regression")

  # Extract parsnip fit
  if(length(object$best_model) == 1){
    parsnip_fit <- tryCatch(
      extract_fit_parsnip(object$best_model[[1]]),
      error = function(e) NULL
    )
  } else {
    parsnip_fit <- tryCatch(
      lapply(object$best_model, extract_fit_parsnip),
      error = function(e) NULL
    )
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

  # Define predict function for DALEX
  predict_function <- function(m, newdata) {
    pred_fun_for_pdp(m, newdata)
  }

  # If no loss_function provided, set defaults:
  if (is.null(loss_function)) {
    if (task == "classification") {
      # Use cross-entropy for classification by default
      loss_function <- loss_cross_entropy
    } else {
      # Use RMSE for regression by default
      loss_function <- loss_root_mean_square
    }
  }

  exp_try <- try({

    # Create explainer(s)
    if(length(object$best_model) == 1){
      explainer <- explain(
        model = parsnip_fit,
        data = x,
        y = if (is.numeric(y)) y else as.numeric(y),
        label = object$best_model_name,
        predict_function = predict_function,
        model_info = model_info
      )
    } else {
      explainer_list <- lapply(names(parsnip_fit), function(model_name) {
        model <- parsnip_fit[[model_name]]
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

    # For Permutation-Based VI
    # Note that we pass B = vi_iterations and the loss_function
    if(length(object$best_model) == 1){
      vi <- model_parts(
        explainer,
        B = vi_iterations,
        type = "raw",
        loss_function = loss_function
      )
    } else {
      vi <- lapply(explainer_list, function(explainer) {
        model_parts(
          explainer = explainer,
          B = vi_iterations,
          type = "raw",
          loss_function = loss_function
        )
      })
      # Name each result by the model name for clarity
      names(vi) <- names(best_model)
    }

    # Plot VI with boxplots
    # For multiple models, plot automatically faceted
    vi_plot <- plot(vi, show_boxplots = TRUE)
    print(vi_plot)

    # Partial Dependence (model profiles)
    mp <- NULL
    if (!is.null(features)) {
      cat("\n=== DALEX Model Profiles (Partial Dependence) ===\n")
      if(length(object$best_model) == 1){
        mp <- model_profile(explainer, variables = features, N = grid_size)
      } else {
        mp <- lapply(explainer_list, function(explainer) {
          model_profile(explainer, variables = features, N = grid_size)
        })
        names(mp) <- names(best_model)
      }
    }

    # Compute SHAP for shap_sample observations
    shap_data <- train_data[1:min(shap_sample, nrow(train_data)), , drop = FALSE]

    cat("\n=== DALEX Shapley Values (SHAP) ===\n")
    set.seed(seed)
    if(length(object$best_model) == 1){
      shap <- predict_parts(explainer, new_observation = shap_data, type = "shap")
    } else {
      shap <- lapply(explainer_list, function(explainer) {
        predict_parts(explainer, new_observation = shap_data, type = "shap")
      })
      names(shap) <- names(best_model)
    }

    # Store absolute SHAP contributions
    if(length(object$best_model) == 1){
      shap$abs_contribution <- abs(shap$contribution)
    } else {
      shap <- lapply(shap, function(model_shap) {
        model_shap$abs_contribution <- abs(model_shap$contribution)
        return(model_shap)
      })
    }

    # Determine grouping variables for classification/regression
    if (task == "classification") {
      group_vars <- object$processed_train_data[[object$label]] %>% levels()
    } else {
      group_vars <- c("feature")
    }

    # Plot SHAP
    if(length(object$best_model) == 1){
      print(plot(shap) + labs(title = paste("SHAP Values")))
    } else {
      # Initialize a list to store plots
      plot_list <- list()
      for(model_name in names(shap)) {
        shap_df <- shap[[model_name]]
        plot_list[[model_name]] <- plot(shap_df) + labs(title = paste("SHAP Values:", model_name))
      }
      combined_plot <- wrap_plots(plot_list, nrow = 1)
      print(combined_plot)
    }

  }, silent = TRUE)

  if (inherits(exp_try, "try-error")) {
    cat("DALEX explanations not available for this model.\n")
  }

  # Prepare return list
  if(!is.null(mp)){
    result <- list(variable_importance = vi,
                   model_profiles = mp,
                   shap_values = shap)
  } else {
    result <- list(variable_importance = vi,
                   shap_values = shap)
  }

  return(invisible(result))
}
