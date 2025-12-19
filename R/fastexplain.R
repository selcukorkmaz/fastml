#' Explain a fastml model using various techniques
#'
#' Provides model explainability across several backends. With \code{method = "dalex"} it:
#' \itemize{
#'   \item Creates a DALEX explainer from the trained model.
#'   \item Computes permutation-based variable importance with \code{vi_iterations} permutations and displays the table and plot.
#'   \item Computes partial dependence-like model profiles when \code{features} are supplied.
#'   \item Computes Shapley values (SHAP) for \code{shap_sample} training rows, displays the SHAP table,
#'   and plots a summary bar chart of \eqn{\text{mean}(\vert \text{SHAP value} \vert)} per feature. For classification, separate bars per class are shown.
#' }
#'
#' @details
#'  \itemize{
#'    \item \bold{Method dispatch:} \code{method} can route to LIME, ICE, ALE, surrogate tree, interaction strengths,
#'    DALEX/modelStudio dashboards, fairness diagnostics, iBreakDown contributions, or counterfactual search.
#'    \item \bold{Variable importance controls:} Use \code{vi_iterations} to tune permutation stability and \code{loss_function}
#'    to override the default DALEX loss (cross-entropy for classification, RMSE for regression).
#'    \item \bold{Fairness and breakdown support:} Provide \code{protected} for \code{method = "fairness"} and an \code{observation}
#'    for \code{method = "breakdown"} or \code{method = "counterfactual"}. Observations are aligned to the explainer data before scoring.
#'  }
#'
#' @param object A \code{fastml} object.
#' @param method Character string specifying the explanation method.
#'   Supported values are \code{"dalex"}, \code{"lime"}, \code{"ice"},
#'   \code{"ale"}, \code{"surrogate"}, \code{"interaction"}, \code{"studio"},
#'   \code{"fairness"}, \code{"breakdown"}, and \code{"counterfactual"}.
#'   Defaults to \code{"dalex"}.
#' @param features Character vector of feature names for partial dependence (model profiles). Default NULL.
#' @param variables Character vector. Variable names to compute explanations for (used for counterfactuals).
#' @param observation A single observation for methods that need a new data point
#'   (\code{method = "counterfactual"} or \code{method = "breakdown"}). Default NULL.
#' @param grid_size Number of grid points for partial dependence. Default 20.
#' @param shap_sample Integer number of observations from processed training data to compute SHAP values for. Default 5.
#' @param vi_iterations Integer. Number of permutations for variable importance (B). Default 10.
#' @param seed Integer. A value specifying the random seed.
#' @param loss_function Function. The loss function for \code{model_parts}.
#'   \itemize{
#'     \item If \code{NULL} and task = 'classification', defaults to \code{DALEX::loss_cross_entropy}.
#'     \item If \code{NULL} and task = 'regression', defaults to \code{DALEX::loss_root_mean_square}.
#'   }
#' @param protected Character or factor vector of protected attribute(s) required for
#'   \code{method = "fairness"}. Default NULL.
#' @param ... Additional arguments passed to the underlying helper functions
#'   for the chosen \code{method}.
#'
#' @importFrom dplyr select
#' @importFrom tune extract_fit_parsnip
#' @importFrom DALEX explain model_parts loss_root_mean_square loss_cross_entropy model_profile predict_parts
#' @importFrom ggplot2 labs
#' @importFrom stats predict
#'
#' @return For DALEX-based methods, prints variable importance, model profiles, and SHAP summaries.
#'   Other methods return their respective explainer objects (e.g., LIME explanations, ALE plot,
#'   surrogate tree, interaction strengths, modelStudio dashboard, fairmodels object, breakdown
#'   object, or counterfactual results), usually invisibly after plotting or printing.
#' @export
fastexplain <- function(object,
                          method = "dalex",
                          features = NULL,
                          variables = NULL,
                          observation = NULL,
                          grid_size = 20,
                          shap_sample = 5,
                          vi_iterations = 10,
                          seed = 123,
                          loss_function = NULL,
                          protected = NULL,
                          ...) {

  if (!inherits(object, "fastml")) {
    stop("The input must be a 'fastml' object.")
  }

  method <- tolower(method)

  # Helper to ensure new observations are preprocessed before scoring
  preprocess_observation <- function(obs) {
    if (is.null(obs)) return(NULL)
    if (!is.null(object$preprocessor)) {
      tryCatch(
        recipes::bake(object$preprocessor, new_data = obs),
        error = function(e) obs
      )
    } else {
      obs
    }
  }

  # Legacy/specific methods
  if (method == "lime") return(explain_lime(object, ...))
  if (method == "ice") return(plot_ice(object, features = features, ...))
  if (method == "ale") {
    if (is.null(features) || length(features) == 0) {
      stop("'features' must contain a feature name for ALE explanations.")
    }
    return(explain_ale(object, feature = features[1], ...))
  }
  if (method == "surrogate") return(surrogate_tree(object, ...))
  if (method == "interaction") return(interaction_strength(object, ...))
  # DALEX ecosystem methods reuse centralized explainer builder
  prep <- fastml_prepare_explainer_inputs(object)
  dalex_res <- fastml_build_dalex_explainers(prep)
  explainer <- dalex_res$explainers[[1]]

  if (method == "dalex") {
    return(explain_dalex_internal(
      explainers = dalex_res$explainers,
      prep = prep,
      features = features,
      grid_size = grid_size,
      shap_sample = shap_sample,
      vi_iterations = vi_iterations,
      seed = seed,
      loss_function = loss_function
    ))
  } else if (method == "studio") {
    if (!requireNamespace("modelStudio", quietly = TRUE)) {
      stop("Package 'modelStudio' required for method = 'studio'.")
    }
    return(modelStudio::modelStudio(explainer, ...))
  } else if (method == "fairness") {
    if (is.null(protected)) {
      stop("Argument 'protected' is required for method = 'fairness'.")
    }
    if (!requireNamespace("fairmodels", quietly = TRUE)) {
      stop("Package 'fairmodels' required for method = 'fairness'.")
    }
    fairness_obj <- fairmodels::fairness_check(explainer, protected = protected, ...)
    print(plot(fairness_obj))
    return(invisible(fairness_obj))
  } else if (method == "breakdown") {
    if (is.null(observation)) {
      stop("'observation' must be provided for method = 'breakdown'.")
    }
    if (!requireNamespace("iBreakDown", quietly = TRUE)) {
      stop("Package 'iBreakDown' required for method = 'breakdown'.")
    }
    if (prep$task == "survival") {
      message("Breakdown plots are skipped for survival tasks because reducing a survival curve to a single number (e.g., median time) can be undefined or misleading. Variable importance and SHAP remain available.")
      return(invisible(NULL))
    }
    # Align observation to the explainer's expected columns (raw predictors).
    obs_aligned <- as.data.frame(observation)
    if (!is.null(explainer$data)) {
      expl_cols <- colnames(explainer$data)
      # Add any missing predictors so the recipe/predict_function can handle them.
      missing_cols <- setdiff(expl_cols, colnames(obs_aligned))
      if (length(missing_cols) > 0) {
        for (nm in missing_cols) {
          tmpl <- explainer$data[[nm]]
          obs_aligned[[nm]] <- if (is.factor(tmpl)) factor(NA, levels = levels(tmpl)) else NA
        }
      }
      obs_aligned <- obs_aligned[, expl_cols, drop = FALSE]
    }
    bd <- suppressWarnings(iBreakDown::break_down(explainer, new_observation = obs_aligned, ...))
    bd_plot <- suppressWarnings(tryCatch(
      plot(bd),
      error = function(e) {
        warning("Breakdown plot could not be rendered: ", e$message)
        NULL
      }
    ))
    if (!is.null(bd_plot)) {
      print(bd_plot)
    }
    return(invisible(bd))
  } else if (method == "counterfactual") {
    if (is.null(observation)) {
      stop("'observation' must be provided for counterfactual explanations.")
    }

    # --- FIX: Suppress Warnings for GLM Rank-Deficiency ---
    return(suppressWarnings(counterfactual_explain(
      object = explainer,
      observation = observation,
      variables = variables,
      positive_class = prep$positive_class,
      event_class = prep$event_class,
      label_levels = prep$label_levels,
      ...
    )))

  } else {
    stop("Unknown explanation method.")
  }
}
