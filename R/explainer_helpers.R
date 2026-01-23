#' Internal helper to prepare explainer inputs from a fastml object
#'
#' @param object A fastml object.
#' @param data Character string specifying which data to use: "train" (default) or "test".
#' @keywords internal
fastml_prepare_explainer_inputs <- function(object, data = c("train", "test")) {
  if (!inherits(object, "fastml")) {
    stop("The input must be a 'fastml' object.")
  }

  data <- match.arg(data)

  if (is.null(object$processed_train_data) || is.null(object$label) ||
      !(object$label %in% names(object$processed_train_data))) {
    stop("Processed training data with the target column is required to build an explainer.")
  }

  # Select data source based on parameter

  if (data == "test") {
    if (is.null(object$raw_test_data)) {
      stop("Test data is not available in this fastml object. Use data = 'train' instead.")
    }
    raw_data <- as.data.frame(object$raw_test_data)
    processed_data <- if (!is.null(object$processed_test_data)) {
      as.data.frame(object$processed_test_data)
    } else {
      # Bake test data if processed version not stored
      if (!is.null(object$preprocessor)) {
        as.data.frame(recipes::bake(object$preprocessor, new_data = object$raw_test_data))
      } else {
        raw_data
      }
    }
  } else {
    raw_data <- if (!is.null(object$raw_train_data)) {
      as.data.frame(object$raw_train_data)
    } else {
      as.data.frame(object$processed_train_data)
    }
    processed_data <- as.data.frame(object$processed_train_data)
  }

  # Keep references to training data for preprocessing alignment
  raw_train_data <- if (!is.null(object$raw_train_data)) {
    as.data.frame(object$raw_train_data)
  } else {
    as.data.frame(object$processed_train_data)
  }
  processed_train_data <- as.data.frame(object$processed_train_data)

  # Use selected data source (train or test) for explanations
  selected_data <- raw_data
  rownames(selected_data) <- NULL

  label <- object$label
  x_raw <- selected_data[, setdiff(names(selected_data), label), drop = FALSE]
  x_raw <- as.data.frame(x_raw)
  rownames(x_raw) <- NULL
  y_raw <- selected_data[[label]]

  # Use processed predictors for actually scoring models
  x_processed <- as.data.frame(processed_data[, setdiff(names(processed_data), label), drop = FALSE])
  rownames(x_processed) <- NULL
  y_processed <- processed_data[[label]]

  best_model <- object$best_model
  fits <- list()
  model_names <- character()
  # Use training data labels for factor levels to ensure consistency
  train_y <- raw_train_data[[label]]
  label_levels <- if (is.factor(train_y)) levels(train_y) else if (is.factor(y_raw)) levels(y_raw) else NULL

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
    data_source = data,
    train_data = selected_data,
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

#' Resolve the positive class for binary classification
#'
#' Determines the positive class respecting event_class settings from fastml().
#' This ensures consistency across all explainer functions.
#'
#' @param prep A list from fastml_prepare_explainer_inputs containing:
#'   positive_class, event_class, label_levels
#' @param y_levels Character vector of actual levels from the target variable
#' @return Character string of the resolved positive class name
#' @keywords internal
resolve_positive_class <- function(prep, y_levels) {
  # 1. Use explicit positive_class if set and valid
  if (!is.null(prep$positive_class) && prep$positive_class %in% y_levels) {
    return(prep$positive_class)
  }

  # 2. Use label_levels with event_class to determine positive class
  if (!is.null(prep$label_levels) && length(prep$label_levels) >= 2) {
    if (!is.null(prep$event_class) && prep$event_class %in% c("first", "second")) {
      # Respect explicit event_class setting
      idx <- if (prep$event_class == "second") {
        min(2L, length(prep$label_levels))
      } else {
        1L
      }
      candidate <- prep$label_levels[idx]
      if (candidate %in% y_levels) {
        return(candidate)
      }
    } else {
      # Default: second level is typically the positive class
      # (e.g., "Yes" > "No", "True" > "False" alphabetically)
      candidate <- prep$label_levels[min(2L, length(prep$label_levels))]
      if (candidate %in% y_levels) {
        return(candidate)
      }
    }
  }

  # 3. Fallback: use event_class directly on y_levels
  if (!is.null(prep$event_class) && prep$event_class %in% c("first", "second") &&
      length(y_levels) >= 2) {
    idx <- if (prep$event_class == "second") min(2L, length(y_levels)) else 1L
    return(y_levels[idx])
  }

  # 4. Last resort: second level if available, else first
  if (length(y_levels) >= 2) {
    return(y_levels[2L])
  }
  y_levels[1L]
}
