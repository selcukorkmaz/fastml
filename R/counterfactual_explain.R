#' Generate counterfactual explanations for a fastml model
#'
#' Uses DALEX ceteris-paribus profiles (`predict_profile`) to compute
#' counterfactual-style what-if explanations for a given observation.
#'
#' @param object A `fastml` object.
#' @param observation A single observation (data frame with one row) to compute
#'   counterfactuals for.
#' @param ... Additional arguments passed to `DALEX::predict_profile`.
#'
#' @return A counterfactual explanation object.
#' @importFrom DALEX explain predict_profile
#' @export
#' @examples
#' \dontrun{
#' data(iris)
#' iris <- iris[iris$Species != "setosa", ]
#' iris$Species <- factor(iris$Species)
#' model <- fastml(data = iris, label = "Species")
#' counterfactual_explain(model, iris[1, ])
#' }

counterfactual_explain <- function(object,
                                   observation,
                                   positive_class = NULL,
                                   event_class = NULL,
                                   label_levels = NULL,
                                   ...) {
  if (missing(observation)) {
    stop("'observation' must be provided for counterfactual explanations.")
  }
  if (nrow(observation) != 1) {
    stop("'observation' must contain exactly one row.")
  }
  if (!requireNamespace("DALEX", quietly = TRUE)) {
    stop("The 'DALEX' package is required for counterfactual explanations.")
  }

  obs_raw <- observation
  explainer <- NULL

  if (inherits(object, "fastml")) {
    prep <- fastml_prepare_explainer_inputs(object)
    expl_list <- fastml_build_dalex_explainers(prep)$explainers
    explainer <- expl_list[[1]]
  } else if (inherits(object, "explainer")) {
    explainer <- object
  } else {
    stop("`object` must be either a fastml object or a DALEX explainer.")
  }

  # Use DALEX ceteris-paribus profiles for what-if (counterfactual-style) analysis
  profile <- DALEX::predict_profile(
    explainer = explainer,
    new_observation = obs_raw,
    ...
  )

  plot_obj <- tryCatch(suppressWarnings(plot(profile)), error = function(e) NULL)
  if (!is.null(plot_obj)) print(plot_obj)
  invisible(list(counterfactuals = profile, profile = profile, plot = plot_obj))
}
