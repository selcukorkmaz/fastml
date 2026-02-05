#' Analyze Feature Importance Stability Across Cross-Validation Folds
#'
#' Computes feature importance for each fold model and aggregates results
#' to assess the stability of feature importance rankings across resamples.
#' This helps identify features that are consistently important vs those
#' whose importance varies across different data subsets.
#'
#' @param object A \code{fastml} object trained with \code{store_fold_models = TRUE}.
#' @param model_name Character string specifying which model to analyze. If NULL,
#'   uses the best model. Should match the format "algorithm (engine)", e.g.,
#'   "rand_forest (ranger)".
#' @param vi_iterations Integer. Number of permutations for variable importance
#'   per fold. Default is 10 for faster computation across many folds.
#' @param seed Integer. Random seed for reproducibility.
#' @param plot Logical. If TRUE, displays a stability plot showing
#'   mean importance with confidence intervals. Default is FALSE.
#' @param conf_level Numeric. Confidence level for intervals. Default is 0.95.
#'
#' @return A list with class \code{"fastml_stability"} containing:
#'   \describe{
#'     \item{importance_summary}{Data frame with aggregated feature importance
#'       (mean, sd, se, lower/upper CI) across folds.}
#'     \item{fold_importance}{List of per-fold variable importance results.}
#'     \item{rank_stability}{Data frame showing how feature ranks vary across folds.}
#'     \item{n_folds}{Number of folds analyzed.}
#'     \item{model_name}{Name of the model analyzed.}
#'   }
#'
#' @details
#' This function requires that the fastml model was trained with
#' \code{store_fold_models = TRUE}, which stores the models fitted on each
#' cross-validation fold. Without stored fold models, only the final best
#' model is available, and cross-fold stability analysis is not possible.
#'
#' The stability analysis computes permutation-based variable importance
#' for each fold's model using DALEX, then aggregates across folds to show:
#' \itemize{
#'   \item Mean importance and standard deviation
#'   \item Confidence intervals for importance
#'   \item Rank stability (how consistently features rank across folds)
#' }
#'
#' Features with high mean importance but also high variance may be
#' important for some data subsets but not others, suggesting potential
#' instability in the model's reliance on those features.
#'
#' @examples
#' \donttest{
#' # Train model with fold models stored
#' model <- fastml(
#'   data = iris,
#'   label = "Species",
#'   algorithms = "rand_forest",
#'   store_fold_models = TRUE
#' )
#'
#' # Analyze stability
#' stability <- explain_stability(model)
#' print(stability)
#' plot(stability)
#' }
#'
#' @importFrom rlang .data
#' @importFrom stats aggregate qnorm sd
#' @importFrom utils head
#' @importFrom tune extract_fit_parsnip
#' @importFrom workflows extract_recipe extract_fit_parsnip
#' @importFrom recipes bake
#' @importFrom DALEX explain model_parts loss_root_mean_square
#' @importFrom ggplot2 ggplot aes geom_point geom_errorbarh labs theme_minimal theme element_text
#' @export
explain_stability <- function(object,
                              model_name = NULL,
                              vi_iterations = 10,
                              seed = 123,
                              plot = FALSE,
                              conf_level = 0.95) {
  if (!inherits(object, "fastml")) {
    stop("The input must be a 'fastml' object.")
  }

  if (!requireNamespace("DALEX", quietly = TRUE)) {
    stop("Package 'DALEX' is required for stability analysis.")
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting.")
  }

  if (!requireNamespace("workflows", quietly = TRUE)) {
    stop("Package 'workflows' is required for stability analysis.")
  }

  if (!requireNamespace("recipes", quietly = TRUE)) {
    stop("Package 'recipes' is required for stability analysis.")
  }

  # Check if fold models are available
  if (is.null(object$resampling_results)) {
    stop(
      "No resampling results found. ",
      "Ensure the model was trained with resampling (resampling_method != 'none')."
    )
  }

  # Determine which model to analyze
  if (is.null(model_name)) {
    # Use best model name
    if (!is.null(object$best_model_name)) {
      model_name <- paste0(
        names(object$best_model_name),
        " (",
        object$best_model_name,
        ")"
      )
    } else {
      model_name <- names(object$resampling_results)[1]
    }
  }

  if (!model_name %in% names(object$resampling_results)) {
    stop(
      sprintf(
        "Model '%s' not found in resampling results. Available models: %s",
        model_name,
        paste(names(object$resampling_results), collapse = ", ")
      )
    )
  }

  resample_entry <- object$resampling_results[[model_name]]

  # Check if fold models are stored
  if (is.null(resample_entry$fold_models)) {
    stop(
      "Fold models are not available for this model. ",
      "To enable stability analysis, retrain with store_fold_models = TRUE:\n",
      "  model <- fastml(..., store_fold_models = TRUE)"
    )
  }

  fold_models <- resample_entry$fold_models
  fold_train_data <- resample_entry$fold_train_data
  n_folds <- length(fold_models)

  if (n_folds < 2) {
    stop("At least 2 folds are required for stability analysis.")
  }

  message(sprintf("Computing variable importance for %d folds...", n_folds))

  # Prepare common inputs
  task <- object$task
  label <- object$label
  positive_class <- object$positive_class
  event_class <- object$event_class

  # Extract label levels from training data
  label_levels <- NULL
  if (!is.null(object$raw_train_data) && label %in% names(object$raw_train_data)) {
    y_train <- object$raw_train_data[[label]]
    if (is.factor(y_train)) {
      label_levels <- levels(y_train)
    }
  }

  # Build loss function (same as explain_dalex)
  loss_function <- if (task == "classification") {
    function(observed, predicted) {
      eps <- 1e-15
      if (is.data.frame(predicted) || is.matrix(predicted)) {
        pred_mat <- as.matrix(predicted)
        obs_chr <- as.character(observed)
        col_idx <- if (!is.null(colnames(pred_mat))) {
          match(obs_chr, colnames(pred_mat))
        } else {
          rep(NA_integer_, length(obs_chr))
        }
        p_true <- numeric(length(obs_chr))
        for (i in seq_along(obs_chr)) {
          if (!is.na(col_idx[i])) {
            p_true[i] <- pred_mat[i, col_idx[i]]
          } else {
            p_true[i] <- NA_real_
          }
        }
        p_true <- pmin(pmax(p_true, eps), 1 - eps)
        return(mean(-log(p_true), na.rm = TRUE))
      }
      obs_num <- if (is.factor(observed)) as.integer(observed) - 1 else as.numeric(observed)
      unique_vals <- unique(stats::na.omit(obs_num))
      if (length(unique_vals) == 2) {
        min_val <- min(unique_vals)
        max_val <- max(unique_vals)
        if (min_val != 0 || max_val != 1) {
          obs_num <- ifelse(obs_num == min_val, 0, 1)
        }
      }
      pred <- pmin(pmax(as.numeric(predicted), eps), 1 - eps)
      -mean(obs_num * log(pred) + (1 - obs_num) * log(1 - pred), na.rm = TRUE)
    }
  } else {
    DALEX::loss_root_mean_square
  }

  # Compute variable importance for each fold
  set.seed(seed)
  fold_vi_results <- vector("list", n_folds)

  for (i in seq_len(n_folds)) {
    fold_model <- fold_models[[i]]
    fold_data <- fold_train_data[[i]]

    if (is.null(fold_model) || is.null(fold_data)) {
      warning(sprintf("Fold %d has missing model or data, skipping.", i))
      next
    }

    # Use the workflow directly with raw data, similar to fastexplain()
    # The predict_function will handle preprocessing internally
    # This ensures DALEX permutes raw features (original feature names)
    model_fit <- fold_model

    # For non-workflow models, extract the parsnip fit
    if (!inherits(fold_model, "workflow")) {
      model_fit <- tryCatch(
        tune::extract_fit_parsnip(fold_model),
        error = function(e) {
          if (inherits(fold_model, "model_fit")) fold_model else NULL
        }
      )
    }

    if (is.null(model_fit)) {
      warning(sprintf("Could not extract model fit for fold %d, skipping.", i))
      next
    }

    # Use raw data (without label) for DALEX - it will permute original features
    x_data <- fold_data[, setdiff(names(fold_data), label), drop = FALSE]
    y_data <- fold_data[[label]]

    # Extract preprocessor for use in predict_function (workflows only)
    fold_preprocessor <- NULL
    if (inherits(fold_model, "workflow")) {
      fold_preprocessor <- tryCatch(
        workflows::extract_recipe(fold_model),
        error = function(e) NULL
      )
    }

    # Coerce target for DALEX
    y_target <- if (task == "classification") {
      if (is.factor(y_data) || is.character(y_data) || is.logical(y_data)) {
        y_chr <- as.character(y_data)
        class_levels <- if (!is.null(label_levels)) label_levels else unique(y_chr)
        n_classes <- length(unique(class_levels))

        if (n_classes > 2) {
          # Multiclass: keep labels to align with probability column names
          factor(y_chr, levels = class_levels)
        } else {
          # Binary: map to 0/1 using positive class when available
          pos_class <- NULL
          if (!is.null(positive_class)) {
            pos_class <- positive_class
          } else if (!is.null(label_levels) && length(label_levels) == 2) {
            pos_class <- if (!is.null(event_class) && event_class == "first") {
              label_levels[1]
            } else {
              label_levels[2]
            }
          }
          if (!is.null(pos_class)) {
            as.numeric(tolower(trimws(y_chr)) == tolower(trimws(pos_class)))
          } else {
            as.numeric(as.factor(y_chr)) - 1
          }
        }
      } else {
        y_num <- as.numeric(y_data)
        unique_vals <- unique(stats::na.omit(y_num))
        if (length(unique_vals) > 2) {
          as.character(y_num)
        } else {
          y_num
        }
      }
    } else {
      as.numeric(y_data)
    }

    # Build predict function that handles preprocessing internally
    # This allows DALEX to permute raw features while still getting correct predictions
    # Capture fold_preprocessor in closure for use during prediction
    local_preprocessor <- fold_preprocessor

    predict_function <- function(m, newdata) {
      # For workflows: predict handles baking internally, so pass data directly
      # For non-workflows with preprocessor: bake first, then predict
      processed_data <- newdata

      if (!inherits(m, "workflow") && !is.null(local_preprocessor)) {
        # Bake the permuted data before prediction
        processed_data <- tryCatch(
          recipes::bake(local_preprocessor, new_data = newdata),
          error = function(e) newdata  # Fall back to original if baking fails
        )
        # Remove label if present in baked data
        if (label %in% names(processed_data)) {
          processed_data[[label]] <- NULL
        }
      }

      if (task == "classification") {
        p <- predict(m, new_data = processed_data, type = "prob")
        colnames(p) <- sub("^\\.pred_", "", colnames(p))
        if (ncol(p) > 2) {
          return(as.matrix(p))
        }
        # Binary: return probability of positive class
        return(as.numeric(p[[ncol(p)]]))
      } else {
        p <- predict(m, new_data = processed_data, type = "numeric")
        return(as.numeric(p$.pred))
      }
    }

    # Create DALEX explainer
    model_info <- if (task == "classification") {
      list(type = "classification")
    } else {
      list(type = "regression")
    }

    explainer <- tryCatch(
      DALEX::explain(
        model = model_fit,
        data = x_data,
        y = y_target,
        label = paste0("fold_", i),
        predict_function = predict_function,
        model_info = model_info,
        verbose = FALSE
      ),
      error = function(e) {
        warning(sprintf("Could not create explainer for fold %d: %s", i, e$message))
        NULL
      }
    )

    if (is.null(explainer)) next

    # Compute variable importance
    fold_seed <- seed + i
    vi_result <- tryCatch({
      set.seed(fold_seed)
      DALEX::model_parts(
        explainer,
        B = vi_iterations,
        type = "raw",
        loss_function = loss_function
      )
    }, error = function(e) {
      warning(sprintf("Variable importance failed for fold %d: %s", i, e$message))
      NULL
    })

    if (!is.null(vi_result)) {
      fold_vi_results[[i]] <- vi_result
    }
  }

  # Filter out NULL results
  valid_folds <- which(!sapply(fold_vi_results, is.null))
  n_valid <- length(valid_folds)

  if (n_valid < 2) {
    stop("Insufficient valid fold results for stability analysis. Need at least 2 folds.")
  }

  message(sprintf("Successfully computed importance for %d of %d folds.", n_valid, n_folds))

  # Aggregate results across folds
  # Extract dropout_loss for each feature from each fold
  extract_vi_agg <- function(vi, fold_id) {
    if (!inherits(vi, "data.frame") ||
        !all(c("variable", "dropout_loss") %in% names(vi))) {
      return(NULL)
    }
    vi_sub <- vi[!vi$variable %in% c("_baseline_", "_full_model_"), , drop = FALSE]
    if (nrow(vi_sub) == 0) {
      return(NULL)
    }
    vi_sub <- vi_sub[
      !is.na(vi_sub$variable) &
        !is.na(vi_sub$dropout_loss) &
        is.finite(vi_sub$dropout_loss),
      ,
      drop = FALSE
    ]
    if (nrow(vi_sub) == 0) {
      return(NULL)
    }
    vi_agg <- stats::aggregate(
      dropout_loss ~ variable,
      data = vi_sub,
      FUN = mean
    )
    if (nrow(vi_agg) == 0) {
      return(NULL)
    }
    vi_agg$fold <- fold_id
    vi_agg
  }

  all_importance <- lapply(valid_folds, function(i) {
    extract_vi_agg(fold_vi_results[[i]], i)
  })
  all_importance <- Filter(Negate(is.null), all_importance)

  if (length(all_importance) < 2) {
    stop(
      "Insufficient variable-level importance results for stability analysis. ",
      "Ensure the model supports variable importance and DALEX returns per-feature values."
    )
  }

  importance_df <- do.call(rbind, all_importance)

  # Compute summary statistics per feature
  importance_summary <- stats::aggregate(
    dropout_loss ~ variable,
    data = importance_df,
    FUN = function(x) {
      c(
        mean = mean(x, na.rm = TRUE),
        sd = stats::sd(x, na.rm = TRUE),
        min = min(x, na.rm = TRUE),
        max = max(x, na.rm = TRUE)
      )
    }
  )

  # Flatten the matrix output from aggregate
  importance_summary <- data.frame(
    variable = importance_summary$variable,
    mean_importance = importance_summary$dropout_loss[, "mean"],
    sd_importance = importance_summary$dropout_loss[, "sd"],
    min_importance = importance_summary$dropout_loss[, "min"],
    max_importance = importance_summary$dropout_loss[, "max"]
  )

  # Add standard error and confidence intervals
  importance_summary$se_importance <- importance_summary$sd_importance / sqrt(n_valid)
  z_val <- stats::qnorm(1 - (1 - conf_level) / 2)
  importance_summary$lower_ci <- importance_summary$mean_importance - z_val * importance_summary$se_importance
  importance_summary$upper_ci <- importance_summary$mean_importance + z_val * importance_summary$se_importance

  # Sort by mean importance (descending)
  importance_summary <- importance_summary[order(-importance_summary$mean_importance), ]
  rownames(importance_summary) <- NULL

  # Compute rank stability
  # Get ranks for each fold
  rank_data <- lapply(valid_folds, function(i) {
    vi_agg <- extract_vi_agg(fold_vi_results[[i]], i)
    if (is.null(vi_agg)) {
      return(NULL)
    }
    vi_agg$rank <- rank(-vi_agg$dropout_loss, ties.method = "average")
    vi_agg[, c("variable", "rank", "fold")]
  })
  rank_data <- Filter(Negate(is.null), rank_data)

  if (length(rank_data) < 2) {
    stop(
      "Insufficient variable-level importance results for rank stability. ",
      "Ensure the model supports variable importance and DALEX returns per-feature values."
    )
  }

  rank_df <- do.call(rbind, rank_data)

  # Compute rank summary
  rank_summary <- stats::aggregate(
    rank ~ variable,
    data = rank_df,
    FUN = function(x) {
      c(
        mean_rank = mean(x, na.rm = TRUE),
        sd_rank = stats::sd(x, na.rm = TRUE),
        min_rank = min(x, na.rm = TRUE),
        max_rank = max(x, na.rm = TRUE)
      )
    }
  )

  rank_stability <- data.frame(
    variable = rank_summary$variable,
    mean_rank = rank_summary$rank[, "mean_rank"],
    sd_rank = rank_summary$rank[, "sd_rank"],
    min_rank = rank_summary$rank[, "min_rank"],
    max_rank = rank_summary$rank[, "max_rank"]
  )

  rank_stability <- rank_stability[order(rank_stability$mean_rank), ]
  rownames(rank_stability) <- NULL

  # Create result object
  result <- list(
    importance_summary = importance_summary,
    fold_importance = fold_vi_results[valid_folds],
    rank_stability = rank_stability,
    n_folds = n_valid,
    model_name = model_name,
    conf_level = conf_level
  )
  class(result) <- "fastml_stability"

  # Plot if requested
  if (plot) {
    p <- plot.fastml_stability(result)
    print(p)
  }

  invisible(result)
}

#' Plot method for fastml_stability objects
#'
#' @param x A fastml_stability object from explain_stability().
#' @param top_n Integer. Number of top features to display. Default is 15.
#' @param ... Additional arguments (ignored).
#'
#' @return A ggplot object.
#' @export
plot.fastml_stability <- function(x, top_n = 15, ...) {
  if (!inherits(x, "fastml_stability")) {
    stop("Input must be a 'fastml_stability' object.")
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting.")
  }

  # Select top features
  plot_data <- utils::head(x$importance_summary, top_n)

  # Reorder factor levels for plotting

  plot_data$variable <- factor(
    plot_data$variable,
    levels = rev(plot_data$variable)
  )

  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$mean_importance, y = .data$variable)) +
    ggplot2::geom_point(size = 3, color = "steelblue") +
    ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = .data$lower_ci, xmax = .data$upper_ci),
      height = 0.3,
      color = "steelblue",
      alpha = 0.7
    ) +
    ggplot2::labs(
      title = sprintf("Feature Importance Stability (%d folds)", x$n_folds),
      subtitle = sprintf("Model: %s | %d%% CI shown", x$model_name, round(x$conf_level * 100)),
      x = "Mean Dropout Loss (higher = more important)",
      y = "Feature"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      axis.text.y = ggplot2::element_text(size = 9)
    )

  p
}

#' Print method for fastml_stability objects
#'
#' @param x A fastml_stability object from explain_stability().
#' @param top_n Integer. Number of top features to display. Default is 10.
#' @param ... Additional arguments (ignored).
#'
#' @return The input object invisibly.
#' @export
print.fastml_stability <- function(x, top_n = 10, ...) {
  if (!inherits(x, "fastml_stability")) {
    stop("Input must be a 'fastml_stability' object.")
  }

  cat("\n=== Feature Importance Stability Analysis ===\n")
  cat(sprintf("Model: %s\n", x$model_name))
  cat(sprintf("Number of folds: %d\n", x$n_folds))
  cat(sprintf("Confidence level: %d%%\n\n", round(x$conf_level * 100)))

  cat("Top features by mean importance:\n")
  top_features <- utils::head(x$importance_summary, top_n)
  top_features$mean_importance <- round(top_features$mean_importance, 4)
  top_features$sd_importance <- round(top_features$sd_importance, 4)
  top_features$lower_ci <- round(top_features$lower_ci, 4)
  top_features$upper_ci <- round(top_features$upper_ci, 4)
  print(top_features[, c("variable", "mean_importance", "sd_importance", "lower_ci", "upper_ci")])

  cat("\n\nRank stability (lower SD = more stable ranking):\n")
  top_ranks <- utils::head(x$rank_stability, top_n)
  top_ranks$mean_rank <- round(top_ranks$mean_rank, 2)
  top_ranks$sd_rank <- round(top_ranks$sd_rank, 2)
  print(top_ranks[, c("variable", "mean_rank", "sd_rank", "min_rank", "max_rank")])

  invisible(x)
}
