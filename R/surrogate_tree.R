#' Fit a surrogate decision tree for a fastml model
#'
#' Builds an interpretable tree approximating the behaviour of the underlying
#' model using the `iml` package.
#'
#' @param object A `fastml` object.
#' @param maxdepth Maximum depth of the surrogate tree. Default 3.
#' @param ... Additional arguments passed to `iml::TreeSurrogate`.
#'
#' @return An `iml::TreeSurrogate` object.
#' @importFrom iml Predictor TreeSurrogate
#' @importFrom recipes bake
#' @export
#' @examples
#' \dontrun{
#' data(iris)
#' iris <- iris[iris$Species != "setosa", ]
#' iris$Species <- factor(iris$Species)
#' model <- fastml(data = iris, label = "Species")
#' surrogate_tree(model)
#' }
surrogate_tree <- function(object, maxdepth = 3, ...) {
  if (!inherits(object, "fastml")) {
    stop("The input must be a 'fastml' object.")
  }
  if (!requireNamespace("iml", quietly = TRUE)) {
    stop("The 'iml' package is required for surrogate models.")
  }

  prep <- fastml_prepare_explainer_inputs(object)
  raw_data <- prep$train_data
  if (is.null(raw_data) || !(prep$label %in% names(raw_data))) {
    stop("Training data not available for surrogate models.")
  }
  x <- prep$x_raw
  y <- prep$y_raw

  # Convert factor/character targets to numeric (0/1) for iml
  positive_class <- prep$positive_class
  if (is.factor(y) || is.character(y)) {
    y_factor <- if (is.factor(y)) y else factor(y)
    if (is.null(positive_class) || !(positive_class %in% levels(y_factor))) {
      positive_class <- levels(y_factor)[1]
    }
    y <- as.numeric(y_factor == positive_class)
  }

  parsnip_fit <- prep$fits[[1]]

  # Custom predict function returning numeric probabilities (or numeric predictions)
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

    prob <- tryCatch(
      predict(model, new_data = newdata_processed, type = "prob"),
      error = function(e) NULL
    )
    if (!is.null(prob)) {
      prob_col <- paste0(".pred_", positive_class)
      if (!is.null(positive_class) && prob_col %in% names(prob)) {
        return(as.numeric(prob[[prob_col]]))
      }
      num_cols <- names(prob)[vapply(prob, is.numeric, logical(1))]
      if (length(num_cols) > 0) {
        return(as.numeric(prob[[num_cols[1]]]))
      }
    }

    preds <- predict(model, new_data = newdata_processed)
    as.numeric(preds[[1]])
  }

  predictor <- iml::Predictor$new(parsnip_fit, data = x, y = y, predict.fun = predict_fun)
  tree <- iml::TreeSurrogate$new(predictor, maxdepth = maxdepth, ...)
  p <- plot(tree)
  print(p)
  invisible(tree)
}
