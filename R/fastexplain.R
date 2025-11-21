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
#'   \code{"counterfactual"}, \code{"studio"}. Defaults to \code{"dalex"}.
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
    obs_processed <- preprocess_observation(observation)
    bd <- iBreakDown::break_down(explainer, new_observation = obs_processed, ...)
    print(plot(bd))
    return(invisible(bd))
  } else if (method == "counterfactual") {
    if (is.null(observation)) {
      stop("'observation' must be provided for counterfactual explanations.")
    }
    return(counterfactual_explain(
      explainer,
      preprocess_observation(observation),
      positive_class = prep$positive_class,
      event_class = prep$event_class,
      label_levels = prep$label_levels,
      ...
    ))
  } else {
    stop("Unknown explanation method.")
  }
}
