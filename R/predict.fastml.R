#' Predict method for fastml objects
#'
#' Generates predictions from a trained `fastml` object on new data.
#' Supports both single-model and multi-model workflows, and handles classification
#' and regression tasks with optional post-processing and verbosity.
#'
#' @param object A fitted `fastml` object created by the `fastml()` function.
#' @param newdata A data frame or tibble containing new predictor data for which to generate predictions.
#' @param type Type of prediction to return. One of `"auto"` (default), `"class"`, `"prob"`, `"numeric"`,
#'   `"survival"`, or `"risk"`.
#'   - `"auto"`: chooses `"class"` for classification, `"numeric"` for regression, and `"survival"` for survival.
#'   - `"prob"`: returns class probabilities (only for classification).
#'   - `"class"`: returns predicted class labels.
#'   - `"numeric"`: returns predicted numeric values (for regression).
#'   - `"survival"`: returns survival probabilities at the supplied `eval_time` horizons (for survival tasks).
#'   - `"risk"`: returns risk scores on the linear predictor scale (for survival tasks).
#' @param model_name (Optional) Name of a specific model to use when `object$best_model` contains multiple models.
#' @param verbose Logical; if `TRUE`, prints progress messages showing which models are used during prediction.
#' @param postprocess_fn (Optional) A function to apply to the final predictions (e.g., inverse transforms, thresholding).
#' @param eval_time Optional numeric vector of time points (on the original time
#'   scale) at which to return survival probabilities when `type = "survival"`.
#'   Required for survival tasks when requesting survival curves.
#' @param ... Additional arguments (currently unused).
#'
#' @return A vector of predictions, or a named list of predictions (if multiple models are used).
#'         If `postprocess_fn` is supplied, its output will be returned instead.
#'
#' @method predict fastml
#' @importFrom stats predict
#' @importFrom tibble is_tibble
#' @export
predict.fastml <- function(object, newdata,
                           type = "auto",
                           model_name = NULL,
                           verbose = FALSE,
                           postprocess_fn = NULL,
                           eval_time = NULL,
                           ...) {
  # 1. input checks -------------------------------------------------------
  if (missing(newdata)) {
    stop("Please provide newdata for prediction.")
  }

  # --- FIX 1: Cast integers to numeric to prevent recipe imputation errors ---
  # Recipes are strict about type. If training data was integer but imputation
  # adds decimals (median/mean), applying it to integer test data crashes.
  if (is.data.frame(newdata)) {
    newdata[] <- lapply(newdata, function(x) {
      if (is.integer(x)) as.numeric(x) else x
    })
  }

  # 2. preprocessing via recipe -------------------------------------------
  if (is.null(object$preprocessor)) {
    stop("No preprocessing recipe found in fastml object.")
  }

  # Pass full data to bake; the recipe handles label removal via step_rm
  new_proc <- recipes::bake(object$preprocessor, new_data = newdata)

  if (!is.null(object$feature_names)) {
    miss <- setdiff(object$feature_names, names(new_proc))
    if (length(miss)) {
      stop("Missing features after preprocessing: ", paste(miss, collapse = ", "))
    }
  }

  # 3. pick prediction type ------------------------------------------------
  predict_type <- switch(type,
                         auto = if (object$task == "classification") "class" else if (object$task == "survival") "survival" else "numeric",
                         prob = {
                           if (object$task != "classification") {
                             warning("Probabilities only for classification; using numeric.")
                             "numeric"
                           } else "prob"
                         },
                         survival = "survival",
                         risk = "risk",
                         type
  )

  # Helper: standardized risk/linear predictor extraction -----------------
  fastml_predict_risk <- function(model, new_data, ...) {
    if (inherits(model, "fastml_native_survival")) {
      native_lp <- tryCatch(
        predict_risk(model, newdata = new_data, ...),
        error = function(e) NULL
      )
      if (!is.null(native_lp)) {
        return(as.numeric(native_lp))
      }
      stop("Risk prediction for this native survival model is not available.", call. = FALSE)
    }

    lp <- tryCatch(
      predict(model, new_data = new_data, type = "linear_pred", ...),
      error = function(e) NULL
    )

    if (is.null(lp)) {
      stop("This model does not support risk/linear predictor scores.", call. = FALSE)
    }

    if (is.data.frame(lp) || tibble::is_tibble(lp)) {
      if (".pred" %in% names(lp)) return(as.numeric(lp$.pred))
      if (".pred_lp" %in% names(lp)) return(as.numeric(lp$.pred_lp))
      return(as.numeric(lp[[1]]))
    }

    as.numeric(lp)
  }

  # Helper: normalize nested model lists into a flat, named list ----------
  normalize_models <- function(mods, engine_names = NULL) {
    if (!is.list(mods)) return(list())
    if (all(vapply(mods, inherits, logical(1), what = "workflow"))) return(mods)

    out <- list()
    for (algo in names(mods)) {
      entry <- mods[[algo]]
      if (inherits(entry, "workflow")) {
        eng <- tryCatch(engine_names[[algo]], error = function(e) NULL)
        eng <- if (is.null(eng) || length(eng) == 0 || is.na(eng[1])) NULL else eng[1]
        nm  <- if (!is.null(eng)) paste0(algo, " (", eng, ")") else algo
        out[[nm]] <- entry
        next
      }
      if (is.list(entry)) {
        inner_names <- names(entry)
        for (j in seq_along(entry)) {
          wf <- entry[[j]]
          if (!inherits(wf, "workflow")) next
          eng <- if (!is.null(inner_names) && !is.na(inner_names[j]) && nzchar(inner_names[j])) inner_names[j] else NULL
          nm <- if (!is.null(eng)) paste0(algo, " (", eng, ")") else algo
          out[[nm]] <- wf
        }
      }
    }
    out
  }

  resolve_model_names <- function(requested, available) {
    if (length(requested) == 0) return(character())
    avail_set <- unique(available)
    base_map <- stats::setNames(avail_set, sub(" \\(.*\\)$", "", avail_set))
    vapply(requested, function(nm) {
      if (nm %in% avail_set) nm else if (!is.null(base_map[[nm]])) base_map[[nm]] else NA_character_
    }, character(1))
  }

  # 4. choose which workflows to use ---------------------------------------
  all_mods <- normalize_models(object$models, object$engine_names)
  if (!length(all_mods) || !all(vapply(all_mods, inherits, logical(1), what = "workflow"))) {
    stop("No valid `models` slot in fastml object.")
  }

  if (!is.null(model_name)) {
    resolved <- resolve_model_names(model_name, names(all_mods))
    bad <- setdiff(model_name, model_name[!is.na(resolved)])
    if (length(bad)) {
      stop("Requested model_name not found: ", paste(bad, collapse = ", "))
    }
    to_predict <- all_mods[na.omit(resolved)]
  } else {
    bm <- object$best_model
    if (inherits(bm, "workflow")) {
      to_predict <- list(bm)
      names(to_predict) <- if (!is.null(names(bm))) names(bm) else "best_model"
    } else if (is.list(bm) && all(sapply(bm, inherits, "workflow"))) {
      to_predict <- bm
    } else {
      to_predict <- all_mods
    }
  }

  # 5. run predictions ------------------------------------------------------
  preds <- lapply(names(to_predict), function(nm) {
    wf <- to_predict[[nm]]
    if (verbose) message("Predicting with: ", nm)

    # Workflows need raw data (they bake internally). Native objects need baked data.
    new_data_for_predict <- if (inherits(wf, "workflow")) newdata else new_proc

    if (object$task == "survival") {
      if (identical(predict_type, "survival")) {
        times <- eval_time
        if (is.null(times)) {
          stop("Please supply 'eval_time' when requesting survival probabilities.")
        }
        times <- as.numeric(times)
        times <- sort(unique(times[is.finite(times) & times >= 0]))
        if (length(times) == 0) {
          stop("No valid evaluation times supplied in 'eval_time'.")
        }

        surv_pred <- tryCatch({
          # --- FIX 2: Handle Native vs Workflow Separately ---
          if (inherits(wf, "fastml_native_survival")) {
            # Native helper needs 'newdata'
            predict_survival(wf, newdata = new_data_for_predict, times = times, ...)
          } else {
            # Workflows use standard predict with 'new_data'
            # 1. Get nested tibble prediction
            raw_pred <- predict(wf, new_data = new_data_for_predict, type = "survival", eval_time = times)
            # 2. Convert nested list column to Matrix (Rows=Obs, Cols=Time)
            probs_list <- lapply(raw_pred$.pred, function(x) x$.pred_survival)
            do.call(rbind, probs_list)
          }
        }, error = function(e) {
          stop(sprintf("Failed to obtain survival predictions for model '%s': %s", nm, e$message), call. = FALSE)
        })

        if (is.matrix(surv_pred) && ncol(surv_pred) == length(times)) {
          colnames(surv_pred) <- format(times, trim = TRUE, scientific = FALSE)
        }
        if (!is.null(postprocess_fn)) {
          surv_pred <- postprocess_fn(surv_pred)
        }
        return(surv_pred)
      }

      if (identical(predict_type, "risk")) {
        risk_pred <- tryCatch({
          # --- FIX 3: Map user-facing newdata to internal new_data requirement ---
          fastml_predict_risk(wf, new_data = new_data_for_predict, ...)
        }, error = function(e) {
          stop(sprintf("Failed to obtain risk predictions for model '%s': %s", nm, e$message), call. = FALSE)
        })
        if (!is.null(postprocess_fn)) {
          risk_pred <- postprocess_fn(risk_pred)
        }
        return(as.numeric(risk_pred))
      }

      stop("Unsupported prediction type for survival task: ", predict_type)
    }

    # Standard predictions (Classification / Regression)
    p <- predict(wf, new_data = new_data_for_predict, type = predict_type, ...)
    if (is.data.frame(p) || tibble::is_tibble(p)) {
      p <- switch(predict_type,
                  class   = p$.pred_class,
                  numeric = p$.pred,
                  prob    = p
      )
    }
    if (!is.null(postprocess_fn)) {
      p <- postprocess_fn(p)
    }
    p
  })
  names(preds) <- names(to_predict)

  # 6. return simple vs. list ---------------------------------------------
  if (length(preds) == 1) {
    return(preds[[1]])
  }
  class(preds) <- "fastml_prediction"
  preds
}
