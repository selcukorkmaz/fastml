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
  return(explain_dalex(object, features = features, grid_size = grid_size,
                       shap_sample = shap_sample, vi_iterations = vi_iterations,
                       seed = seed, loss_function = loss_function))

}
