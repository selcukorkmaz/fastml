#' Plot ICE curves for a fastml model
#'
#' Generates Individual Conditional Expectation (ICE) plots for selected features
#' using the `pdp` package (ggplot2 engine), and returns both the underlying data
#' and the plot object.
#'
#' @param object A `fastml` object.
#' @param features Character vector of feature names to plot.
#' @param data Character string specifying which data to use: \code{"train"} (default) or \code{"test"}.
#' @param target_class For classification, which class probability to plot. If NULL (default),
#'   uses the positive class determined by the model settings. For multiclass problems,
#'   this shows the probability of the specified class vs all others.
#' @param ... Additional arguments passed to `pdp::partial`.
#'
#' @return A list with two elements: `data` (the ICE data frame) and `plot` (the ggplot object).
#' @importFrom pdp partial
#' @importFrom ggplot2 autoplot
#' @importFrom recipes bake
#' @export
#' @examples
#' \dontrun{
#' data(iris)
#' iris <- iris[iris$Species != "setosa", ]
#' iris$Species <- factor(iris$Species)
#' model <- fastml(data = iris, label = "Species")
#' plot_ice(model, features = "Sepal.Length")
#' }
plot_ice <- function(object, features, data = c("train", "test"), target_class = NULL, ...) {
  if (!inherits(object, "fastml")) {
    stop("The input must be a 'fastml' object.")
  }
  if (missing(features)) {
    stop("'features' must be specified for ICE plots.")
  }
  if (!requireNamespace("pdp", quietly = TRUE)) {
    stop("The 'pdp' package is required for ICE plots.")
  }

  data <- match.arg(data)

  # Use the same helper as other explainer methods for consistency

  prep <- fastml_prepare_explainer_inputs(object, data = data)
  if (is.null(prep$train_data) || !(prep$label %in% names(prep$train_data))) {
    stop(sprintf("%s data not available for ICE plots.", tools::toTitleCase(data)))
  }

  # Use raw data (consistent with ALE, interaction_strength, surrogate_tree)
  x <- prep$x_raw
  y <- prep$y_raw

  # Validate that user-provided features exist in the raw data
  available_features <- colnames(x)
  invalid_features <- setdiff(features, available_features)
  if (length(invalid_features) > 0) {
    stop(
      sprintf(
        "Feature name(s) not found in data: %s\nAvailable features: %s%s\nNote: feature names must match exactly (case-sensitive).",
        paste(invalid_features, collapse = ", "),
        paste(utils::head(available_features, 10), collapse = ", "),
        if (length(available_features) > 10) " ..." else ""
      ),
      call. = FALSE
    )
  }

  parsnip_fit <- prep$fits[[1]]
  if (is.null(parsnip_fit)) {
    stop("Unable to extract parsnip model for ICE plots.")
  }

  # Determine target class for classification tasks
  # For multiclass, warn user that ICE shows probability for one class vs all others
  positive_class <- NULL
  is_multiclass <- FALSE
  if (prep$task == "classification" && (is.factor(y) || is.character(y))) {
    y_factor <- if (is.factor(y)) y else factor(y)
    y_levels <- levels(y_factor)
    is_multiclass <- length(y_levels) > 2

    if (!is.null(target_class)) {
      # User specified a target class
      if (!(target_class %in% y_levels)) {
        stop(
          sprintf(
            "target_class '%s' not found in class levels.\nAvailable classes: %s",
            target_class,
            paste(y_levels, collapse = ", ")
          ),
          call. = FALSE
        )
      }
      positive_class <- target_class
    } else {
      # Use default positive class
      positive_class <- resolve_positive_class(prep, y_levels)
    }

    if (is_multiclass) {
      message(
        sprintf(
          "Note: Multiclass ICE plot showing probability for class '%s' vs all others.\n",
          positive_class
        ),
        "Use target_class parameter to specify a different class."
      )
    }
  }

  # Custom predict function that preprocesses raw data before prediction

  # This ensures consistency with other explainer methods (ALE, interaction, surrogate)
  pred_fun <- function(object, newdata) {
    # Preprocess raw data using the stored recipe
    newdata_processed <- tryCatch(
      {
        baked <- recipes::bake(prep$preprocessor, new_data = newdata)
        if (!is.null(prep$label) && prep$label %in% names(baked)) {
          baked[[prep$label]] <- NULL
        }
        baked
      },
      error = function(e) newdata
    )

    # Try probabilities first (classification)
    prob <- tryCatch(
      predict(object, new_data = newdata_processed, type = "prob"),
      error = function(e) NULL
    )
    if (!is.null(prob)) {
      # Prefer column that matches the positive class; otherwise fall back to first numeric column
      prob_col <- paste0(".pred_", positive_class)
      if (!is.null(positive_class) && prob_col %in% names(prob)) {
        return(as.numeric(prob[[prob_col]]))
      }
      num_cols <- names(prob)[vapply(prob, is.numeric, logical(1))]
      if (length(num_cols) > 0) {
        return(as.numeric(prob[[num_cols[1]]]))
      }
    }

    # Fallback to standard predictions (regression)
    preds <- predict(object, new_data = newdata_processed)
    as.numeric(preds[[1]])
  }

  # Compute ICE data using raw data and custom predict function
  # This makes ICE consistent with ALE, interaction_strength, and surrogate_tree
  pd <- pdp::partial(
    parsnip_fit,
    pred.var = features,
    ice = TRUE,
    train = x,
    pred.fun = pred_fun,
    plot = FALSE,
    ...
  )
  p <- ggplot2::autoplot(pd)

  # Convert deprecated `size` aesthetics on line geoms to `linewidth` to avoid ggplot2 warnings
  p$layers <- lapply(p$layers, function(layer) {
    if (inherits(layer$geom, c("GeomLine", "GeomPath", "GeomSegment", "GeomStep"))) {
      mapping <- layer$mapping
      if (!is.null(mapping$size)) {
        mapping$linewidth <- mapping$size
        mapping$size <- NULL
        layer$mapping <- mapping
      }
      if (!is.null(layer$aes_params$size)) {
        layer$aes_params$linewidth <- layer$aes_params$size
        layer$aes_params$size <- NULL
      }
    }
    layer
  })
  if (!is.null(p$labels$size)) {
    p$labels$linewidth <- p$labels$size
    p$labels$size <- NULL
  }
  print(p)
  invisible(list(data = pd, plot = p))
}
