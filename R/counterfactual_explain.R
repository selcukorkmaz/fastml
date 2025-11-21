#' Generate counterfactual explanations for a fastml model
#'
#' Uses the `ceterisParibus` package to compute counterfactuals for a given
#' observation.
#'
#' @param object A `fastml` object.
#' @param observation A single observation (data frame with one row) to compute
#'   counterfactuals for.
#' @param ... Additional arguments passed to `ceterisParibus::calculate_counterfactuals`.
#'
#' @return A counterfactual explanation object.
#' @importFrom DALEX explain
#' @importFrom ceterisParibus calculate_oscillations
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
  if (!requireNamespace("ceterisParibus", quietly = TRUE)) {
    stop("The 'ceterisParibus' package is required for counterfactuals.")
  }

  obs_processed <- observation
  explainer <- NULL

  if (inherits(object, "fastml")) {
    prep <- fastml_prepare_explainer_inputs(object)
    expl_list <- fastml_build_dalex_explainers(prep)$explainers
    explainer <- expl_list[[1]]
    if (!is.null(object$preprocessor)) {
      obs_processed <- tryCatch(
        recipes::bake(object$preprocessor, new_data = observation),
        error = function(e) observation
      )
    }
  } else if (inherits(object, "explainer")) {
    explainer <- object
  } else {
    stop("`object` must be either a fastml object or a DALEX explainer.")
  }

  # Wrap predict_function to ensure numeric vector output (use positive/event class when available)
  choose_target_col <- function(cols) {
    if (length(cols) == 0) return(NULL)
    if (!is.null(event_class) &&
        event_class %in% c("first", "second") &&
        !is.null(label_levels) &&
        length(label_levels) >= 2) {
      idx <- if (event_class == "first") 1 else 2
      lvl <- label_levels[idx]
      if (lvl %in% cols) return(lvl)
    }
    if (!is.null(positive_class) && positive_class %in% cols) {
      return(positive_class)
    }
    if (!is.null(label_levels)) {
      for (lvl in label_levels) {
        if (lvl %in% cols) return(lvl)
      }
    }
    tail(cols, 1)
  }

  original_predict <- explainer$predict_function
  adjusted_predict <- function(m, newdata) {
    res <- original_predict(m, newdata)
    if (is.data.frame(res) || is.matrix(res)) {
      cols <- colnames(res)
      if (ncol(res) >= 1) {
        if (ncol(res) >= 2) {
          target_col <- choose_target_col(cols)
          return(as.numeric(res[[target_col]]))
        } else {
          return(as.numeric(res[[1]]))
        }
      }
    }
    as.numeric(res)
  }

  explainer$predict_function <- adjusted_predict

  cf <- ceterisParibus::ceteris_paribus(
    explainer = explainer,
    observations = obs_processed,
    ...
  )
  plot_obj <- suppressWarnings(plot(cf))
  print(plot_obj)
  invisible(list(profile = cf, plot = plot_obj))
}
