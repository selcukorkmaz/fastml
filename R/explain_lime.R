#' Internal predict_model method for parsnip fits
#'
#' Shim for parsnip model objects so that lime's predict_model generic ignores
#' unused arguments passed via `...`.
#' @keywords internal
#' @export
predict_model.model_fit <- function(x, newdata, type, ...) {
  # LIME passes extra args that parsnip doesn't like; ignore them
  predict(x, new_data = newdata, type = type)
}

#' Generate LIME explanations for a fastml model
#'
#' Creates a `lime` explainer using processed (encoded, scaled) data and returns
#' feature explanations for new observations. The new observation is automatically
#' preprocessed using the same recipe to ensure alignment with the explainer background.
#'
#' @param object A `fastml` object.
#' @param new_observation A data frame containing the new observation(s) to explain.
#'   Must contain the same columns as the original training data (before preprocessing).
#'   The function will apply the stored preprocessor to transform it.
#' @param n_features Number of features to show in the explanation. Default 5.
#' @param n_labels Number of labels to explain (classification only). Default 1.
#' @param ... Additional arguments passed to `lime::explain`.
#'
#' @param data Character string specifying which data to use for the LIME explainer background:
#'   \code{"train"} (default) or \code{"test"}.
#' @return An object produced by `lime::explain`.
#' @importFrom recipes bake
#' @export
#' @examples
#' \dontrun{
#' data(iris)
#' iris <- iris[iris$Species != "setosa", ]
#' iris$Species <- factor(iris$Species)
#' model <- fastml(data = iris, label = "Species")
#' explain_lime(model, new_observation = iris[1, ])
#' }
explain_lime <- function(object, new_observation, data = c("train", "test"), n_features = 5, n_labels = 1, ...) {
  if (!inherits(object, "fastml")) {
    stop("The input must be a 'fastml' object.")
  }
  if (missing(new_observation)) {
    stop("'new_observation' must be provided for LIME.")
  }
  if (!requireNamespace("lime", quietly = TRUE)) {
    stop("The 'lime' package is required for LIME explanations.")
  }

  # 1. Get data to build the explainer (train or test based on parameter)
  data <- match.arg(data)
  prep <- fastml_prepare_explainer_inputs(object, data = data)
  if (length(prep$fits) == 0) {
    stop("Unable to extract parsnip model for LIME.")
  }

  # 2. Use PROCESSED data for explainer background (this is what the model sees)
 background_x <- prep$x_processed
  expected_cols <- colnames(background_x)

  # 3. Preprocess the new observation to match the processed feature space
  preprocess_failed <- FALSE
  if (!is.null(object$preprocessor)) {
    to_explain <- tryCatch(
      {
        baked <- recipes::bake(object$preprocessor, new_data = new_observation)
        # Remove label if present
        if (!is.null(object$label) && object$label %in% names(baked)) {
          baked[[object$label]] <- NULL
        }
        baked
      },
      error = function(e) {
        preprocess_failed <<- TRUE
        NULL
      }
    )
  } else {
    # No preprocessor - use raw observation but remove label
    to_explain <- new_observation
    if (!is.null(object$label) && object$label %in% names(to_explain)) {
      to_explain[[object$label]] <- NULL
    }
  }

  # 4. Validate alignment between background and observation
 if (preprocess_failed || is.null(to_explain)) {
    stop(
      "LIME preprocessing failed for the new observation. ",
      "Ensure the observation has the same columns as the training data.",
      call. = FALSE
    )
  }

  # Check for column mismatch
  obs_cols <- colnames(to_explain)
  missing_in_obs <- setdiff(expected_cols, obs_cols)
  extra_in_obs <- setdiff(obs_cols, expected_cols)

  if (length(missing_in_obs) > 0 || length(extra_in_obs) > 0) {
    msg_parts <- character()
    if (length(missing_in_obs) > 0) {
      msg_parts <- c(msg_parts, paste0(
        "Missing columns in observation: ",
        paste(head(missing_in_obs, 5), collapse = ", "),
        if (length(missing_in_obs) > 5) paste0(" (and ", length(missing_in_obs) - 5, " more)") else ""
      ))
    }
    if (length(extra_in_obs) > 0) {
      msg_parts <- c(msg_parts, paste0(
        "Extra columns in observation: ",
        paste(head(extra_in_obs, 5), collapse = ", "),
        if (length(extra_in_obs) > 5) paste0(" (and ", length(extra_in_obs) - 5, " more)") else ""
      ))
    }
    stop(
      "LIME column mismatch between explainer background and observation. ",
      paste(msg_parts, collapse = ". "),
      call. = FALSE
    )
  }

  # Ensure column order matches
  to_explain <- to_explain[, expected_cols, drop = FALSE]

  # 5. Build explainer and generate explanations
  explainer <- lime::lime(background_x, prep$fits[[1]])
  explanation <- lime::explain(
    x = to_explain,
    explainer = explainer,
    n_features = n_features,
    n_labels = n_labels,
    ...
  )
  print(lime::plot_features(explanation))
  invisible(explanation)
}
