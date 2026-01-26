#' Compute Accumulated Local Effects (ALE) for a fastml model
#'
#' Uses the `iml` package to calculate ALE for the specified feature.
#'
#' @param object A `fastml` object.
#' @param feature Character string specifying the feature name.
#' @param ... Additional arguments passed to `iml::FeatureEffect`.
#'
#' @return An `iml` object containing ALE results.
#' @importFrom iml Predictor FeatureEffect
#' @importFrom ggplot2 ggplot aes geom_line geom_col geom_rug labs
#' @importFrom rlang sym
#' @importFrom recipes bake
#' @param data Character string specifying which data to use: \code{"train"} (default) or \code{"test"}.
#' @export
#' @examples
#' \dontrun{
#' data(iris)
#' iris <- iris[iris$Species != "setosa", ]
#' iris$Species <- factor(iris$Species)
#' model <- fastml(data = iris, label = "Species")
#' explain_ale(model, feature = "Sepal.Length")
#' }
explain_ale <- function(object, feature, data = c("train", "test"), ...) {
  if (!inherits(object, "fastml")) {
    stop("The input must be a 'fastml' object.")
  }
  if (missing(feature)) {
    stop("'feature' must be provided for ALE.")
  }
  if (!requireNamespace("iml", quietly = TRUE)) {
    stop("The 'iml' package is required for ALE explanations.")
  }

  data <- match.arg(data)
  prep <- fastml_prepare_explainer_inputs(object, data = data)
  train_data <- prep$train_data
  if (is.null(train_data) || !(prep$label %in% names(train_data))) {
    stop("Training data not available for ALE.")
  }
  x <- prep$x_raw
  y <- prep$y_raw

  # Validate that user-provided feature exists in the data
  available_features <- colnames(x)
  if (!(feature %in% available_features)) {
    stop(
      sprintf(
        "Feature '%s' not found in data.\nAvailable features: %s%s\nNote: feature names must match exactly (case-sensitive).",
        feature,
        paste(utils::head(available_features, 10), collapse = ", "),
        if (length(available_features) > 10) " ..." else ""
      ),
      call. = FALSE
    )
  }

  # Handle classification targets for iml

  # Determine if this is multiclass (>2 classes)
  is_multiclass <- FALSE
  positive_class <- NULL
  if (is.factor(y) || is.character(y)) {
    y_factor <- if (is.factor(y)) y else factor(y)
    n_classes <- length(levels(y_factor))
    is_multiclass <- n_classes > 2

    if (is_multiclass) {
      # Multiclass: keep y as factor, iml will handle it properly
      # when predict function returns all class probabilities
      y <- y_factor
    } else {
      # Binary classification: convert to numeric (0/1) for iml
      # Use resolve_positive_class to respect event_class settings from fastml()
      positive_class <- resolve_positive_class(prep, levels(y_factor))
      y <- as.numeric(y_factor == positive_class)
    }
  } else {
    positive_class <- prep$positive_class
  }

  parsnip_fit <- prep$fits[[1]]

  # Custom predict function
  # For multiclass: returns data.frame with all class probabilities

  # For binary: returns numeric probability vector for the positive class
  predict_fun <- function(model, newdata) {
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
      predict(model, new_data = newdata_processed, type = "prob"),
      error = function(e) NULL
    )
    if (!is.null(prob)) {
      # Multiclass: return full probability data.frame (iml handles this)
      if (is_multiclass) {
        # Clean column names: remove .pred_ prefix
        colnames(prob) <- sub("^\\.pred_", "", colnames(prob))
        return(as.data.frame(prob))
      }
      # Binary: return probability for positive class
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
    preds <- predict(model, new_data = newdata_processed)
    as.numeric(preds[[1]])
  }

  predictor <- iml::Predictor$new(parsnip_fit, data = x, y = y, predict.fun = predict_fun)
  fe <- iml::FeatureEffect$new(predictor, feature = feature, method = "ale", ...)
  plot_data <- fe$results
  feature_sym <- rlang::sym(feature)
  value_sym <- rlang::sym(".value")
  is_categorical <- is.factor(plot_data[[feature]]) || is.character(plot_data[[feature]])

  if (is_categorical) {
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = !!feature_sym, y = !!value_sym)) +
      ggplot2::geom_col() +
      ggplot2::labs(x = feature, y = "ALE")
  } else {
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = !!feature_sym, y = !!value_sym)) +
      ggplot2::geom_line() +
      ggplot2::geom_rug(data = x, ggplot2::aes(x = !!feature_sym), alpha = 0.2, inherit.aes = FALSE) +
      ggplot2::labs(x = feature, y = "ALE")
  }

  print(p)
  invisible(fe)
}
