#' Generate DALEX explanations for a fastml model
#'
#' Creates a DALEX explainer and computes permutation based variable importance,
#' partial dependence (model profiles) and Shapley values.
#'
#' @inheritParams fastexplain
#' @return Invisibly returns a list with variable importance, optional model profiles and SHAP values.
#' @export
explain_dalex <- function(object,
                          data = c("train", "test"),
                          features = NULL,
                          grid_size = 20,
                          shap_sample = 5,
                          vi_iterations = 10,
                          seed = 123,
                          loss_function = NULL) {
  data <- match.arg(data)
  prep <- fastml_prepare_explainer_inputs(object, data = data)
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
fastml_build_dalex_explainers <- function(prep, classification_output = c("prob", "logit")) {
  if (!requireNamespace("DALEX", quietly = TRUE)) {
    stop("The 'DALEX' package is required.")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The 'ggplot2' package is required for plotting.")
  }

  classification_output <- match.arg(classification_output)
  model_info <- if (prep$task == "classification") list(type = "classification") else list(type = "regression")

  normalize_class_name <- function(x) {
    tolower(trimws(as.character(x)))
  }

  resolve_positive_candidates <- function() {
    candidates <- character()
    if (!is.null(prep$positive_class)) {
      candidates <- c(candidates, prep$positive_class)
    }
    if (!is.null(prep$label_levels) && length(prep$label_levels) >= 2) {
      if (!is.null(prep$event_class) && prep$event_class %in% c("first", "second")) {
        idx <- if (prep$event_class == "second") min(2L, length(prep$label_levels)) else 1L
        candidates <- c(candidates, prep$label_levels[idx])
      } else {
        candidates <- c(candidates, prep$label_levels[min(2L, length(prep$label_levels))])
      }
    }
    if (!is.null(prep$label_levels)) {
      candidates <- c(candidates, prep$label_levels)
    }
    unique(candidates[!is.na(candidates) & nzchar(candidates)])
  }

  pos_candidates <- resolve_positive_candidates()
  pos_class <- if (length(pos_candidates)) pos_candidates[[1]] else NULL
  warned_missing_prob <- FALSE

  # --- ROBUST BAKING HELPER ---
  # This function ensures data is properly preprocessed before prediction.
  # It handles three scenarios:
  # 1. No preprocessor exists -> return data as-is
  # 2. Baking succeeds -> return baked data
  # 3. Baking fails -> validate if data is already processed, error if not
  bake_newdata <- function(newdata) {
    if (is.null(prep$preprocessor)) return(newdata)

    processed_cols <- colnames(prep$x_processed)

    # Always attempt to bake when preprocessor exists - column names matching
    # does NOT mean data is already transformed (e.g., centering/scaling keeps
    # names but changes values)
    bake_error <- NULL
    baked <- tryCatch(
      recipes::bake(prep$preprocessor, new_data = newdata),
      error = function(e) {
        bake_error <<- e$message
        NULL
      }
    )

    # If baking failed, we need to validate whether input can be safely used
    if (is.null(baked)) {
      input_cols <- colnames(newdata)

      # Check if input has all expected processed columns
      if (is.null(processed_cols) || !all(processed_cols %in% input_cols)) {
        missing <- if (!is.null(processed_cols)) setdiff(processed_cols, input_cols) else character(0)
        stop(
          "Preprocessing failed: ", bake_error, "\n",
          "Input data is missing expected columns: ",
          paste(utils::head(missing, 5), collapse = ", "),
          if (length(missing) > 5) paste0(" ... and ", length(missing) - 5, " more") else "",
          "\nCannot safely generate explanations with incompatible data."
        )
      }

      # Validate column types match expected processed data types
      type_mismatches <- character(0)
      for (col in processed_cols) {
        if (col %in% colnames(prep$x_processed)) {
          expected_class <- class(prep$x_processed[[col]])[1]
          actual_class <- class(newdata[[col]])[1]
          # Allow numeric/integer/double interchangeability
          numeric_types <- c("numeric", "integer", "double")
          if (expected_class %in% numeric_types && actual_class %in% numeric_types) {
            next
          }
          if (expected_class != actual_class) {
            type_mismatches <- c(type_mismatches,
              sprintf("%s (expected %s, got %s)", col, expected_class, actual_class))
          }
        }
      }

      if (length(type_mismatches) > 0) {
        stop(
          "Preprocessing failed: ", bake_error, "\n",
          "Input data column types do not match expected processed data:\n",
          paste("  -", utils::head(type_mismatches, 5), collapse = "\n"),
          if (length(type_mismatches) > 5) paste0("\n  ... and ", length(type_mismatches) - 5, " more") else "",
          "\nCannot safely generate explanations with incompatible data."
        )
      }

      # Data appears to already be in processed format - use with warning
      warning(
        "Preprocessing failed (", bake_error, "). ",
        "Input data appears to already be processed (has expected columns with compatible types). ",
        "Using input data directly."
      )
      baked <- newdata
    }

    # Remove label column if present
    if (!is.null(prep$label) && prep$label %in% names(baked)) {
      baked[[prep$label]] <- NULL
    }

    # Ensure output has exactly the expected columns in the right order
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
  logit_transform <- function(p) {
    eps <- 1e-7
    p <- pmin(pmax(p, eps), 1 - eps)
    log(p / (1 - p))
  }

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
      if (ncol(p) > 2) {
        p_mat <- as.matrix(p)
        if (classification_output == "logit") {
          p_mat <- logit_transform(p_mat)
        }
        return(p_mat)
      }

      p <- as.data.frame(p)
      prob_cols <- colnames(p)
      match_idx <- NA_integer_
      chosen_class <- NULL

      if (length(pos_candidates)) {
        for (cand in pos_candidates) {
          idx <- match(normalize_class_name(cand), normalize_class_name(prob_cols))
          if (!is.na(idx)) {
            match_idx <- idx
            chosen_class <- prob_cols[idx]
            break
          }
        }
      }

      if (is.na(match_idx) && length(prob_cols) == 2) {
        fallback_idx <- if (!is.null(prep$event_class) && prep$event_class == "second") 2L else 1L
        match_idx <- min(fallback_idx, length(prob_cols))
        chosen_class <- prob_cols[match_idx]
      }

      if (is.na(match_idx)) {
        match_idx <- 1L
        chosen_class <- prob_cols[match_idx]
      }

      if (!warned_missing_prob &&
          length(pos_candidates) &&
          all(is.na(match(normalize_class_name(pos_candidates), normalize_class_name(prob_cols))))) {
        warning(
          "Positive class '", pos_candidates[[1]], "' not found in predicted probabilities; using '",
          chosen_class, "' instead."
        )
        warned_missing_prob <<- TRUE
      }

      p_vec <- p[[match_idx]]
      if (classification_output == "logit") {
        p_vec <- logit_transform(p_vec)
      }
      return(p_vec)

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
      if (is.factor(y_target) || is.character(y_target) || is.logical(y_target)) {
        y_chr <- as.character(y_target)
        pos_for_y <- NULL
        if (!is.null(pos_class) && any(normalize_class_name(y_chr) == normalize_class_name(pos_class))) {
          pos_for_y <- pos_class
        } else if (!is.null(prep$label_levels) && length(prep$label_levels) == 2) {
          if (!is.null(prep$event_class) && prep$event_class == "first") {
            pos_for_y <- prep$label_levels[1]
          } else {
            pos_for_y <- prep$label_levels[2]
          }
        }

        if (!is.null(pos_for_y)) {
          y_target <- as.numeric(normalize_class_name(y_chr) == normalize_class_name(pos_for_y))
        } else {
          y_target <- as.numeric(as.factor(y_chr)) - 1
        }
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
explain_dalex_internal <- function(explainers,
                                   prep,
                                   features = NULL,
                                   grid_size = 20,
                                   shap_sample = 5,
                                   vi_iterations = 10,
                                   seed = 123,
                                   loss_function = NULL) {

  vi <- NULL; mp <- NULL; shap <- NULL

  # Validate that user-provided features exist in the data

  # NOTE: Do NOT sanitize feature names here - the data (prep$x_raw) uses original
  # column names. Users should pass feature names exactly as they appear in their data.
  if (!is.null(features)) {
    available_features <- colnames(prep$x_raw)
    invalid_features <- setdiff(features, available_features)
    if (length(invalid_features) > 0) {
      stop(
        sprintf(
          "Feature name(s) not found in data: %s\nAvailable features: %s\nNote: feature names must match exactly (case-sensitive).",
          paste(invalid_features, collapse = ", "),
          paste(utils::head(available_features, 10), collapse = ", "),
          if (length(available_features) > 10) " ..." else ""
        ),
        call. = FALSE
      )
    }
  }

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

        # Get unique non-NA values for validation
        unique_vals <- unique(stats::na.omit(obs_num))
        n_unique <- length(unique_vals)

        # Handle edge cases
        if (n_unique == 0) {
          warning("No valid observed values for loss calculation")
          return(NA_real_)
        }

        if (n_unique > 2) {
          # More than 2 classes but predictions are scalar - shouldn't happen
          # but fall back to RMSE to avoid garbage results
          warning("More than 2 classes detected but predictions are scalar. ",
                  "Using RMSE as fallback loss.")
          return(sqrt(mean((obs_num - as.numeric(predicted))^2, na.rm = TRUE)))
        }

        # Normalize to {0, 1} - handle any 2-value or 1-value coding
        if (n_unique == 2) {
          min_val <- min(unique_vals)
          max_val <- max(unique_vals)
          if (min_val != 0 || max_val != 1) {
            obs_num <- ifelse(obs_num == min_val, 0, 1)
          }
        } else if (n_unique == 1) {
          # Single class present - map to 0 or 1 as appropriate
          if (!unique_vals[1] %in% c(0, 1)) {
            # Arbitrary single value - treat as class 0
            obs_num <- rep(0, length(obs_num))
            obs_num[is.na(observed)] <- NA  # Preserve original NAs
          }
        }

        pred <- pmin(pmax(as.numeric(predicted), eps), 1 - eps)

        # Compute binary cross-entropy with NA handling
        ce_vals <- obs_num * log(pred) + (1 - obs_num) * log(1 - pred)
        if (all(is.na(ce_vals))) {
          warning("All cross-entropy values are NA")
          return(NA_real_)
        }
        -mean(ce_vals, na.rm = TRUE)
      }
    } else {
      loss_function <- DALEX::loss_root_mean_square
    }
  }

  # --- CRITICAL FIX 2: Scientifically Valid SHAP Sampling ---
  # Head (1:5) is biased. We use stratified sampling for classification to ensure
  # minority classes are represented. For regression/survival, use random sampling.
  set.seed(seed)
  n_rows <- nrow(prep$x_raw)
  n_sample <- min(shap_sample, n_rows)

  sample_indices <- if (prep$task == "classification" && !is.null(prep$y_raw) && n_sample > 1) {
    # Stratified sampling: ensure each class is represented proportionally
    y_vec <- as.character(prep$y_raw)
    class_levels <- unique(y_vec)
    n_classes <- length(class_levels)

    if (n_classes > 1 && n_classes <= n_sample) {
      # Calculate proportional allocation per class (at least 1 per class if possible)
      class_counts <- table(y_vec)
      class_props <- as.numeric(class_counts) / sum(class_counts)

      # Allocate samples: at least 1 per class, rest proportionally
      base_allocation <- pmin(1, as.numeric(class_counts))  # 1 per class or class size if smaller
      remaining <- n_sample - sum(base_allocation)

      if (remaining > 0) {
        # Distribute remaining samples proportionally
        extra_allocation <- floor(class_props * remaining)
        # Handle rounding: distribute leftover to largest classes
        leftover <- remaining - sum(extra_allocation)
        if (leftover > 0) {
          order_idx <- order(class_props, decreasing = TRUE)
          for (i in seq_len(leftover)) {
            extra_allocation[order_idx[i]] <- extra_allocation[order_idx[i]] + 1
          }
        }
        final_allocation <- base_allocation + extra_allocation
      } else {
        final_allocation <- base_allocation
      }

      # Sample from each class
      sampled_indices <- integer(0)
      for (i in seq_along(class_levels)) {
        cls <- class_levels[i]
        cls_indices <- which(y_vec == cls)
        n_to_sample <- min(final_allocation[i], length(cls_indices))
        if (n_to_sample > 0) {
          sampled_indices <- c(sampled_indices, sample(cls_indices, n_to_sample))
        }
      }

      # If we somehow got fewer than requested (edge case), fill with random
      if (length(sampled_indices) < n_sample) {
        remaining_indices <- setdiff(seq_len(n_rows), sampled_indices)
        n_fill <- min(n_sample - length(sampled_indices), length(remaining_indices))
        if (n_fill > 0) {
          sampled_indices <- c(sampled_indices, sample(remaining_indices, n_fill))
        }
      }

      sampled_indices
    } else {
      # Single class or more classes than samples: fall back to random
      sample(seq_len(n_rows), n_sample)
    }
  } else {
    # Regression/survival or single sample: random sampling
    sample(seq_len(n_rows), n_sample)
  }

  shap_data <- prep$x_raw[sample_indices, , drop = FALSE]

  abort_shap <- function(message) {
    cond <- structure(
      list(message = message, call = sys.call(-1)),
      class = c("fastml_shap_value_error", "error", "condition")
    )
    stop(cond)
  }

  exp_try <- tryCatch({
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
    # We can label it clearly. Add explicit whiskers based on permutation draws
    # to ensure variability is visible even when boxplot whiskers collapse.

    # Helper to fix DALEX boxplot linewidth issue (ggplot2 3.4+ uses linewidth, not size)
    fix_boxplot_linewidth <- function(p, linewidth = 0.4, colour = "#371ea3") {
      for (i in seq_along(p$layers)) {
        if (inherits(p$layers[[i]]$geom, "GeomBoxplot")) {
          p$layers[[i]]$aes_params$linewidth <- linewidth
          p$layers[[i]]$aes_params$colour <- colour
        }
      }
      p
    }

    build_vi_plot <- function(vi_obj) {
      if (is.list(vi_obj) && !inherits(vi_obj, "data.frame")) {
        do.call(plot, c(vi_obj, list(show_boxplots = TRUE)))
      } else {
        plot(vi_obj, show_boxplots = TRUE)
      }
    }

    p_vi <- build_vi_plot(vi) + ggplot2::labs(y = "Mean Cross Entropy Loss")
    p_vi <- fix_boxplot_linewidth(p_vi)

    vi_df <- if (is.list(vi) && !inherits(vi, "data.frame")) {
      do.call(rbind, vi)
    } else {
      vi
    }

    if (inherits(vi_df, "data.frame") &&
        "permutation" %in% names(vi_df) &&
        "dropout_loss" %in% names(vi_df) &&
        any(vi_df$permutation > 0, na.rm = TRUE)) {
      vi_perm <- vi_df[vi_df$permutation > 0 & !grepl("^_", vi_df$variable), , drop = FALSE]
      if (nrow(vi_perm) > 0) {
        vi_stats <- stats::aggregate(
          dropout_loss ~ variable + label,
          data = vi_perm,
          FUN = function(x) c(min = min(x, na.rm = TRUE), max = max(x, na.rm = TRUE))
        )
        vi_stats <- do.call(data.frame, vi_stats)
        names(vi_stats) <- gsub("^dropout_loss\\.", "", names(vi_stats))

        if (!is.null(p_vi$data$variable) && is.factor(p_vi$data$variable)) {
          vi_stats$variable <- factor(vi_stats$variable, levels = levels(p_vi$data$variable))
        }

        p_vi <- p_vi + ggplot2::geom_linerange(
          data = vi_stats,
          ggplot2::aes(x = variable, ymin = min, ymax = max),
          inherit.aes = FALSE,
          linewidth = 0.4,
          color = "#371ea3"
        )
      }
    }

    suppressWarnings(print(p_vi))

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
      suppressWarnings(print(plot(mp)))
    }

    cat("\n=== DALEX Shapley Values (SHAP) ===\n")
    if (length(explainers) == 1) {
      shap <- DALEX::predict_parts(explainer, new_observation = shap_data, type = "shap")
      p_shap <- plot(shap) + ggplot2::labs(title = "SHAP Values")
      p_shap <- fix_boxplot_linewidth(p_shap)
      suppressWarnings(print(p_shap))
    } else {
      shap <- lapply(explainer_list, function(expl) {
        DALEX::predict_parts(expl, new_observation = shap_data, type = "shap")
      })
      names(shap) <- model_names
      invisible(lapply(names(shap), function(nm) {
        p_shap <- plot(shap[[nm]]) + ggplot2::labs(title = paste("SHAP:", nm))
        p_shap <- fix_boxplot_linewidth(p_shap)
        suppressWarnings(print(p_shap))
      }))
    }

  }, error = function(e) {
    if (inherits(e, "fastml_shap_value_error")) {
      stop(e)
    }
    structure(e, class = c("try-error", class(e)))
  })

  if (inherits(exp_try, "try-error")) {
    cat("DALEX explanations not available for this model.\n")
    warning(exp_try)
  }

  invisible(list(variable_importance = vi, model_profiles = mp, shap_values = shap))
}
