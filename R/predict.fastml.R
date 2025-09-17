#' Predict method for fastml objects
#'
#' Generates predictions from a trained `fastml` object on new data.
#' Supports both single-model and multi-model workflows, and handles classification
#' and regression tasks with optional post-processing and verbosity.
#'
#' @param object A fitted `fastml` object created by the `fastml()` function.
#' @param newdata A data frame or tibble containing new predictor data for which to generate predictions.
#' @param type Type of prediction to return. One of `"auto"` (default), `"class"`, `"prob"`, or `"numeric"`.
#'   - `"auto"`: chooses `"class"` for classification and `"numeric"` for regression.
#'   - `"prob"`: returns class probabilities (only for classification).
#'   - `"class"`: returns predicted class labels.
#'   - `"numeric"`: returns predicted numeric values (for regression).
#' @param model_name (Optional) Name of a specific model to use when `object$best_model` contains multiple models.
#' @param verbose Logical; if `TRUE`, prints progress messages showing which models are used during prediction.
#' @param postprocess_fn (Optional) A function to apply to the final predictions (e.g., inverse transforms, thresholding).
#' @param ... Additional arguments (currently unused).
#'
#' @return A vector of predictions, or a named list of predictions (if multiple models are used).
#'         If `postprocess_fn` is supplied, its output will be returned instead.
#'
#' @importFrom stats predict
#' @importFrom tibble is_tibble
#' @export
#'
#' @examples
#' \dontrun{
#'   set.seed(123)
#'   model <- fastml(iris, label = "Species")
#'   test_data <- iris[sample(1:150, 20),-5]
#'
#'   ## Best model(s) predictions
#'   preds <- predict(model, newdata = test_data)
#'
#'   ## Predicted class probabilities using best model(s)
#'   probs <- predict(model, newdata = test_data, type = "prob")
#'
#'   ## Prediction from a specific model by name
#'   single_model_preds <- predict(model, newdata = test_data, model_name = "rand_forest (ranger)")
#'
#' }
predict.fastml <- function(object, newdata,
                           type = "auto",
                           model_name = NULL,
                           verbose = FALSE,
                           postprocess_fn = NULL,
                           ...) {
  # 1. input checks & drop label from newdata -----------------------------
  if (missing(newdata)) {
    stop("Please provide newdata for prediction.")
  }
  lbl <- object$label
  if (is.null(lbl)) {
    stop("Label name is missing from the model object.")
  }
  if (lbl %in% names(newdata)) {
    newdata[[lbl]] <- NULL
  }

  # 2. preprocessing via recipe -------------------------------------------
  if (is.null(object$preprocessor)) {
    stop("No preprocessing recipe found in fastml object.")
  }
  new_proc <- bake(object$preprocessor, new_data = newdata)
  if (!is.null(object$feature_names)) {
    miss <- setdiff(object$feature_names, names(new_proc))
    if (length(miss)) {
      stop("Missing features after preprocessing: ", paste(miss, collapse = ", "))
    }
  }

  # 3. pick prediction type ------------------------------------------------
  predict_type <- switch(type,
                         auto = if (object$task == "classification") "class" else "numeric",
                         prob = {
                           if (object$task != "classification") {
                             warning("Probabilities only for classification; using numeric.")
                             "numeric"
                           } else "prob"
                         },
                         type
  )

  # 4. choose which workflows to use ---------------------------------------
  if (!is.null(model_name)) {
    # user wants a specific model → look in object$models
    all_mods <- object$models
    valid_model <- function(mod) inherits(mod, "workflow") || inherits(mod, "fastml_royston")
    if (!is.list(all_mods) || !all(sapply(all_mods, valid_model))) {
      stop("No valid `models` slot in fastml object.")
    }
    bad <- setdiff(model_name, names(all_mods))
    if (length(bad)) {
      stop("Requested model_name not found: ", paste(bad, collapse = ", "))
    }
    to_predict <- all_mods[model_name]
  } else {
    # default → best_model slot
    bm <- object$best_model
    if (inherits(bm, "workflow") || inherits(bm, "fastml_royston")) {
      to_predict <- list(bm)
      names(to_predict) <- if (!is.null(names(bm))) names(bm) else "best_model"
    } else if (is.list(bm) && all(sapply(bm, valid_model))) {
      to_predict <- bm
    } else {
      stop("No valid `best_model` found in fastml object.")
    }
  }

  # 5. run predictions ------------------------------------------------------
  preds <- lapply(names(to_predict), function(nm) {
    wf <- to_predict[[nm]]
    if (verbose) message("Predicting with: ", nm)
    if (inherits(wf, "workflow")) {
      p <- predict(wf, new_data = new_proc, type = predict_type, ...)
      if (is.data.frame(p) || tibble::is_tibble(p)) {
        p <- switch(predict_type,
                    class   = p$.pred_class,
                    numeric = p$.pred,
                    prob    = p
        )
      }
    } else if (inherits(wf, "fastml_royston")) {
      pred_data <- new_proc
      if ("surv_obj" %in% names(pred_data)) {
        pred_data$surv_obj <- NULL
      }
      p <- tryCatch(
        as.numeric(rstpm2::predict(wf$fit, newdata = pred_data, type = "link")),
        error = function(e) rep(NA_real_, nrow(pred_data))
      )
    } else {
      stop("Unsupported model type for prediction.")
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


