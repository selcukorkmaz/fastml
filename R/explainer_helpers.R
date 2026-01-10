#' Internal helper to prepare explainer inputs from a fastml object
#'
#' @keywords internal
fastml_prepare_explainer_inputs <- function(object) {
  if (!inherits(object, "fastml")) {
    stop("The input must be a 'fastml' object.")
  }

  if (is.null(object$processed_train_data) || is.null(object$label) ||
      !(object$label %in% names(object$processed_train_data))) {
    stop("Processed training data with the target column is required to build an explainer.")
  }

  raw_train_data <- if (!is.null(object$raw_train_data)) {
    as.data.frame(object$raw_train_data)
  } else {
    as.data.frame(object$processed_train_data)
  }
  processed_train_data <- as.data.frame(object$processed_train_data)

  train_data <- raw_train_data
  rownames(train_data) <- NULL

  label <- object$label
  x_raw <- train_data[, setdiff(names(train_data), label), drop = FALSE]
  x_raw <- as.data.frame(x_raw)
  rownames(x_raw) <- NULL
  y_raw <- train_data[[label]]

  # Use processed predictors for actually scoring models
  x_processed <- as.data.frame(processed_train_data[, setdiff(names(processed_train_data), label), drop = FALSE])
  rownames(x_processed) <- NULL
  y_processed <- processed_train_data[[label]]

  best_model <- object$best_model
  fits <- list()
  model_names <- character()
  label_levels <- if (is.factor(y_raw)) levels(y_raw) else NULL

  add_fit <- function(name, mod) {
    if (is.null(mod)) {
      return()
    }
    pf <- tryCatch(tune::extract_fit_parsnip(mod), error = function(e) NULL)
    if (is.null(pf) && inherits(mod, c("model_fit", "workflow"))) {
      pf <- mod
    }
    if (!is.null(pf)) {
      fits[[length(fits) + 1]] <<- pf
      model_names[[length(model_names) + 1]] <<- name
    }
  }

  combine_names <- function(parent, child) {
    if (is.null(parent) || !nzchar(parent)) return(child)
    if (is.null(child) || !nzchar(child)) return(parent)
    if (grepl(paste0("\\(", child, "\\)$"), parent)) return(parent)
    paste0(parent, " (", child, ")")
  }

  collect_fits <- function(models, parent = NULL) {
    if (is.null(models)) return()
    if (inherits(models, c("workflow", "model_fit"))) {
      add_fit(if (!is.null(parent) && nzchar(parent)) parent else "best_model", models)
      return()
    }
    if (is.list(models)) {
      model_names_local <- names(models)
      for (i in seq_along(models)) {
        nm <- if (!is.null(model_names_local) && nzchar(model_names_local[i])) {
          model_names_local[i]
        } else {
          paste0("model_", i)
        }
        combined <- combine_names(parent, nm)
        collect_fits(models[[i]], combined)
      }
    }
  }

  collect_fits(best_model)

  if (length(fits) == 0 && !is.null(object$best_model_name) && !is.null(object$models)) {
    target_names <- paste0(names(object$best_model_name), " (", object$best_model_name, ")")
    collect_fits(object$models[target_names])
  }

  if (length(fits) == 0 && !is.null(object$models)) {
    collect_fits(object$models)
  }

  if (length(fits) == 0) {
    stop("Unable to extract fitted model(s) for explanation.")
  }

  list(
    train_data = train_data,
    raw_train_data = raw_train_data,
    processed_train_data = processed_train_data,
    x = x_processed,
    x_raw = x_raw,
    x_processed = x_processed,
    y = y_raw,
    y_raw = y_raw,
    y_processed = y_processed,
    label = label,
    task = object$task,
    positive_class = object$positive_class,
    event_class = if (!is.null(object$event_class)) object$event_class else NULL,
    label_levels = label_levels,
    preprocessor = object$preprocessor,
    fits = fits,
    model_names = model_names
  )
}
