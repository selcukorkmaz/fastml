#' Generate DALEX explanations for a fastml model
#'
#' Creates a DALEX explainer and computes permutation based variable importance,
#' partial dependence (model profiles) and Shapley values.
#'
#' @inheritParams fastexplain
#' @return Invisibly returns a list with variable importance, optional model profiles and SHAP values.
#' @export
explain_dalex <- function(object,
                          features = NULL,
                          grid_size = 20,
                          shap_sample = 5,
                          vi_iterations = 10,
                          seed = 123,
                          loss_function = NULL) {
  prep <- fastml_prepare_explainer_inputs(object)
  explainers <- fastml_build_dalex_explainers(prep)$explainers
  explain_dalex_internal(
    explainers = explainers,
    prep = prep,
    features = features,
    grid_size = grid_size,
    shap_sample = shap_sample,
    vi_iterations = vi_iterations,
    seed = seed,
    loss_function = loss_function
  )
}

#' @keywords internal
#' @keywords internal
fastml_build_dalex_explainers <- function(prep) {
  if (!requireNamespace("DALEX", quietly = TRUE)) {
    stop("The 'DALEX' package is required.")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The 'ggplot2' package is required for plotting.")
  }

  model_info <- if (prep$task == "classification") list(type = "classification") else list(type = "regression")

  pos_class <- prep$positive_class
  if (!is.null(prep$label_levels) && length(prep$label_levels) == 2) {
    if (!is.null(prep$event_class)) {
      pos_class <- if (prep$event_class == "second") prep$label_levels[2] else prep$label_levels[1]
    } else if (is.null(pos_class)) {
      pos_class <- prep$label_levels[1]
    }
  }

  # --- ROBUST BAKING HELPER ---
  bake_newdata <- function(newdata) {
    if (is.null(prep$preprocessor)) return(newdata)
    processed_cols <- colnames(prep$x_processed)
    if (!is.null(processed_cols) && length(processed_cols) && all(processed_cols %in% colnames(newdata))) {
      return(newdata[, processed_cols, drop = FALSE])
    }
    baked <- tryCatch(
      recipes::bake(prep$preprocessor, new_data = newdata),
      error = function(e) {
        warning("Baking failed: ", e$message)
        newdata
      }
    )
    if (!is.null(prep$label) && prep$label %in% names(baked)) {
      baked[[prep$label]] <- NULL
    }
    if (!is.null(processed_cols)) {
      missing_cols <- setdiff(processed_cols, colnames(baked))
      if (length(missing_cols) > 0) {
        for (col in missing_cols) baked[[col]] <- 0
      }
      baked <- baked[, processed_cols, drop = FALSE]
    }
    baked
  }

  # --- PREDICT FUNCTION ---
  predict_function <- function(m, newdata) {
    newdata_processed <- bake_newdata(newdata)

    to_numeric_vec <- function(x) {
      if (is.null(x)) return(NULL)
      drop_if_empty <- function(v) if (length(v) == 0) NULL else as.numeric(v)

      if (is.numeric(x)) return(drop_if_empty(x))
      if (is.matrix(x) && is.numeric(x)) {
        return(drop_if_empty(x[, 1, drop = TRUE]))
      }
      if (is.data.frame(x)) {
        numeric_cols <- names(x)[sapply(x, function(col) is.numeric(col) && !is.list(col))]
        if (length(numeric_cols)) return(drop_if_empty(x[[numeric_cols[1]]]))
        list_cols <- names(x)[sapply(x, is.list)]
        if (length(list_cols)) {
          lst <- x[[list_cols[1]]]
          vec <- vapply(lst, function(el) if (is.null(el) || length(el) == 0) NA_real_ else as.numeric(el[[1]]), numeric(1))
          return(drop_if_empty(vec))
        }
      }
      if (is.list(x)) {
        if (length(x) == 0) return(NULL)
        if (all(vapply(x, function(el) is.numeric(el) && length(el) == 1, logical(1)))) {
          return(drop_if_empty(unlist(x, use.names = FALSE)))
        }
        if (all(vapply(x, function(el) is.numeric(el) && length(el) > 0, logical(1)))) {
          vec <- vapply(x, function(el) as.numeric(el[[1]]), numeric(1))
          return(drop_if_empty(vec))
        }
        flat <- unlist(x, recursive = TRUE, use.names = FALSE)
        if (is.numeric(flat)) return(drop_if_empty(flat))
      }
      NULL
    }

    if (prep$task == "classification") {
      p <- predict(m, new_data = newdata_processed, type = "prob")
      colnames(p) <- sub("^\\.pred_", "", colnames(p))
      p <- as.data.frame(p)

      if (!is.null(pos_class) && pos_class %in% colnames(p)) {
        return(p[[pos_class]])
      }
      return(p[[1]]) # Fallback to first column if positive class not found

    } else if (prep$task == "survival") {
      try_type <- function(tp) {
        tryCatch(predict(m, new_data = newdata_processed, type = tp), error = function(e) NULL)
      }
      pred_vec <- NULL
      for (tp in c("time", "numeric", "risk", "linear_pred", "raw", "survival", "hazard")) {
        candidate <- to_numeric_vec(try_type(tp))
        if (!is.null(candidate)) {
          pred_vec <- candidate
          break
        }
      }
      if (is.null(pred_vec)) {
        pred_vec <- to_numeric_vec(tryCatch(predict(m, new_data = newdata_processed), error = function(e) NULL))
      }
      if (is.null(pred_vec)) stop("Unable to compute survival predictions.")
      return(pred_vec)
    } else {
      p <- predict(m, new_data = newdata_processed, type = "numeric")
      return(as.numeric(p$.pred))
    }
  }

  build_one <- function(model, name) {
    # --- FIX 2: Correctly Extract and Coerce Target for DALEX/modelStudio ---
    y_target <- prep$y_raw
    if (prep$task == "survival") {
      if (inherits(y_target, "Surv")) y_target <- y_target[, 1]
      y_target <- as.numeric(y_target)
    } else if (prep$task == "classification") {
      if (is.factor(y_target) || is.character(y_target)) {
        y_target <- as.numeric(as.factor(y_target)) - 1
      }
    } else {
      y_target <- as.numeric(y_target)
    }

    DALEX::explain(
      model = model,
      data = prep$x_raw,
      y = y_target,
      label = name,
      predict_function = predict_function,
      model_info = model_info,
      verbose = FALSE
    )
  }

  explainers <- if (length(prep$fits) == 1) {
    list(build_one(prep$fits[[1]], if (length(prep$model_names)) prep$model_names[[1]] else "model"))
  } else {
    mapply(
      build_one,
      model = prep$fits,
      name = if (length(prep$model_names)) prep$model_names else paste0("model_", seq_along(prep$fits)),
      SIMPLIFY = FALSE
    )
  }

  list(explainers = explainers)
}

#' @keywords internal
#' @keywords internal
explain_dalex_internal <- function(explainers,
                                   prep,
                                   features = NULL,
                                   grid_size = 20,
                                   shap_sample = 5,
                                   vi_iterations = 10,
                                   seed = 123,
                                   loss_function = NULL) {

  vi <- NULL; mp <- NULL; shap <- NULL
  if (!is.null(features)) features <- sanitize(features)
  model_names <- if (length(prep$model_names)) prep$model_names else paste0("model_", seq_along(explainers))

  # --- CRITICAL FIX 1: Safe Loss Function for Factors ---
  # DALEX::loss_cross_entropy requires 0/1 numerics.
  # If we feed it 1/2 (from R factors), the math is wrong.
  if (is.null(loss_function)) {
    if (prep$task == "classification") {
      loss_function <- function(observed, predicted) {
        eps <- 1e-15
        # Multiclass: take prob of the true class per row, then mean cross-entropy
        if (is.data.frame(predicted) || is.matrix(predicted)) {
          pred_mat <- as.matrix(predicted)
          obs_chr <- as.character(observed)
          # try column-name alignment first
          col_idx <- if (!is.null(colnames(pred_mat))) match(obs_chr, colnames(pred_mat)) else rep(NA_integer_, length(obs_chr))
          p_true <- numeric(length(obs_chr))
          for (i in seq_along(obs_chr)) {
            if (!is.na(col_idx[i])) {
              p_true[i] <- pred_mat[i, col_idx[i]]
            } else {
              # fallback: if observed is numeric-like and columns unnamed, treat as column index
              obs_num <- suppressWarnings(as.integer(obs_chr[i]))
              if (!is.na(obs_num) && obs_num >= 1 && obs_num <= ncol(pred_mat)) {
                p_true[i] <- pred_mat[i, obs_num]
              } else {
                p_true[i] <- NA_real_
              }
            }
          }
          p_true <- pmin(pmax(p_true, eps), 1 - eps)
          return(mean(-log(p_true), na.rm = TRUE))
        }
        # Binary: force 0/1 target, mean cross-entropy
        obs_num <- observed
        if (is.factor(obs_num)) obs_num <- as.integer(obs_num) - 1
        obs_num <- as.numeric(obs_num)
        # normalize non-binary coding to {0,1}
        if (length(unique(stats::na.omit(obs_num))) == 2 && max(obs_num, na.rm = TRUE) > 1) {
          obs_num <- ifelse(obs_num == min(obs_num, na.rm = TRUE), 0, 1)
        }
        pred <- pmin(pmax(as.numeric(predicted), eps), 1 - eps)
        -mean(obs_num * log(pred) + (1 - obs_num) * log(1 - pred))
      }
    } else {
      loss_function <- DALEX::loss_root_mean_square
    }
  }

  # --- CRITICAL FIX 2: Scientifically Valid SHAP Sampling ---
  # Head (1:5) is biased. We use random sampling.
  set.seed(seed)
  n_rows <- nrow(prep$x_raw)
  sample_indices <- sample(seq_len(n_rows), min(shap_sample, n_rows))
  shap_data <- prep$x_raw[sample_indices, , drop = FALSE]

  exp_try <- try({
    explainer <- if (length(explainers) == 1) explainers[[1]] else NULL
    explainer_list <- if (length(explainers) > 1) explainers else NULL

    cat("\n=== DALEX Variable Importance (with Boxplots) ===\n")
    set.seed(seed)

    # Use the custom safe loss_function defined above
    if (length(explainers) == 1) {
      vi <- DALEX::model_parts(explainer, B = vi_iterations, type = "raw", loss_function = loss_function)
    } else {
      vi <- lapply(explainer_list, function(expl) {
        DALEX::model_parts(expl, B = vi_iterations, type = "raw", loss_function = loss_function)
      })
      names(vi) <- model_names
    }

    # Plotting: Since we fixed the math, values are now Positive Cross Entropy.
    # We can label it clearly.
    print(plot(vi, show_boxplots = TRUE) +
            ggplot2::labs(y = "Mean Cross Entropy Loss"))

    # ... (Rest of your code for model_profile and SHAP is fine) ...
    if (!is.null(features)) {
      cat("\n=== DALEX Model Profiles (Partial Dependence) ===\n")
      if (length(explainers) == 1) {
        mp <- DALEX::model_profile(explainer, variables = features, N = grid_size)
      } else {
        mp <- lapply(explainer_list, function(expl) {
          DALEX::model_profile(expl, variables = features, N = grid_size)
        })
        names(mp) <- model_names
      }
      print(plot(mp))
    }

    cat("\n=== DALEX Shapley Values (SHAP) ===\n")
    # ... (Rest of SHAP code) ...
    if (length(explainers) == 1) {
      shap <- DALEX::predict_parts(explainer, new_observation = shap_data, type = "shap")
      suppressWarnings(print(plot(shap) + ggplot2::labs(title = "SHAP Values")))
    } else {
      shap <- lapply(explainer_list, function(expl) {
        DALEX::predict_parts(expl, new_observation = shap_data, type = "shap")
      })
      names(shap) <- model_names
      invisible(lapply(names(shap), function(nm) {
        suppressWarnings(print(plot(shap[[nm]]) + ggplot2::labs(title = paste("SHAP:", nm))))
      }))
    }

  }, silent = TRUE)

  if (inherits(exp_try, "try-error")) {
    cat("DALEX explanations not available for this model.\n")
    warning(exp_try)
  }

  invisible(list(variable_importance = vi, model_profiles = mp, shap_values = shap))
}
