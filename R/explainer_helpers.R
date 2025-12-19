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
    pf <- tryCatch(tune::extract_fit_parsnip(mod), error = function(e) NULL)
    if (is.null(pf) && inherits(mod, "model_fit")) {
      pf <- mod
    }
    if (!is.null(pf)) {
      fits[[length(fits) + 1]] <<- pf
      model_names[[length(model_names) + 1]] <<- name
    }
  }

  if (inherits(best_model, c("workflow", "model_fit"))) {
    nm <- if (!is.null(names(best_model)) && nzchar(names(best_model))) names(best_model) else "best_model"
    add_fit(nm, best_model)
  } else if (is.list(best_model)) {
    bm_names <- names(best_model)
    for (i in seq_along(best_model)) {
      nm <- if (!is.null(bm_names) && nzchar(bm_names[i])) bm_names[i] else paste0("model_", i)
      add_fit(nm, best_model[[i]])
    }
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
