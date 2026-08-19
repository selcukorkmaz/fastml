#' Native survival prediction helpers
#'
#' The functions in this file provide shared preprocessing logic used across
#' the package whenever predictions are generated from models that operate on
#' fastml's "native" survival objects.  They are intentionally kept internal
#' (not exported) so they can be re-used without polluting the public API.
#'
#' @noRd
NULL

#' Apply a stored recipe, or abort
#'
#' Applies a fitted recipe to new data on a prediction path. Failure is fatal
#' rather than silently ignored: falling back to unpreprocessed data would make
#' the model predict on features it was not trained on, producing
#' plausible-looking but meaningless output. A `NULL` recipe means there is
#' nothing to apply, and `new_data` is returned unchanged.
#'
#' @param recipe A prepped recipe, or `NULL`.
#' @param new_data Data to bake.
#' @param context Short description of the calling path, used in the message.
#'
#' @return The baked data, or `new_data` when `recipe` is `NULL`.
#' @keywords internal
#' @noRd
fastml_bake_or_abort <- function(recipe, new_data, context) {
  if (is.null(recipe)) {
    return(new_data)
  }
  tryCatch(
    recipes::bake(recipe, new_data = new_data),
    error = function(e) {
      stop(sprintf(
        paste0(
          "Could not apply the stored preprocessing recipe in %s: %s\n",
          "The new data must contain the same columns, types, and factor levels ",
          "as the training data. fastml does not fall back to unpreprocessed ",
          "data, because doing so would return predictions from a model applied ",
          "to features it was not trained on."
        ),
        context,
        conditionMessage(e)
      ), call. = FALSE)
    }
  )
}

fastml_prepare_native_survival_predictors <- function(model, baked_newdata, original_newdata) {
  if (is.null(baked_newdata)) {
    baked_newdata <- data.frame()
  }

  pred_predictors <- as.data.frame(baked_newdata)
  keep_cols <- names(pred_predictors)

  drop_cols <- c(model$response, model$time_col, model$status_col, model$start_col)
  drop_cols <- unique(drop_cols[!is.na(drop_cols)])
  drop_cols <- intersect(drop_cols, keep_cols)
  if (length(drop_cols) > 0) {
    keep_cols <- setdiff(keep_cols, drop_cols)
  }

  if (!is.null(model$strata_dummy_cols) && length(model$strata_dummy_cols) > 0) {
    keep_cols <- setdiff(keep_cols, model$strata_dummy_cols)
  }
  if (!is.null(model$strata_base_cols) && length(model$strata_base_cols) > 0) {
    keep_cols <- setdiff(keep_cols, model$strata_base_cols)
  }

  if (length(keep_cols) == 0) {
    pred_predictors <- pred_predictors[, 0, drop = FALSE]
  } else {
    pred_predictors <- pred_predictors[, keep_cols, drop = FALSE]
  }

  extra_cols <- unique(c(model$start_col, model$time_col, model$status_col))
  extra_cols <- extra_cols[!is.na(extra_cols)]
  if (length(extra_cols) > 0) {
    for (ec in extra_cols) {
      if (!(ec %in% names(pred_predictors)) && ec %in% names(original_newdata)) {
        pred_predictors[[ec]] <- original_newdata[[ec]]
      }
    }
  }

  if (!is.null(model$strata_cols) && length(model$strata_cols) > 0) {
    strata_levels <- NULL
    fit_obj <- tryCatch(model$fit, error = function(e) NULL)
    if (!is.null(fit_obj)) {
      strata_levels <- tryCatch(fit_obj$xlevels, error = function(e) NULL)
    }
    for (sc in model$strata_cols) {
      if (!(sc %in% names(pred_predictors)) && sc %in% names(original_newdata)) {
        pred_predictors[[sc]] <- original_newdata[[sc]]
      }
      if (sc %in% names(pred_predictors)) {
        pred_predictors[[sc]] <- as.factor(pred_predictors[[sc]])
        if (!is.null(strata_levels) && sc %in% names(strata_levels)) {
          pred_predictors[[sc]] <- factor(pred_predictors[[sc]], levels = strata_levels[[sc]])
        }
      }
    }
  }

  pred_predictors
}

