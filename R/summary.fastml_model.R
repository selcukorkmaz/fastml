#' Summary Function for fastml_model
#'
#' Provides a concise and user-friendly summary of the model's performances.
#'
#' @param object An object of class \code{fastml_model}.
#' @param sort_metric A string specifying which metric to sort the models by.
#'                    Default is \code{NULL}, which prioritizes the optimized metric.
#' @param ... Additional arguments (not used).
#' @return Prints a concise and user-friendly summary of the models' performances.
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
#' @importFrom reshape2 dcast melt
#' @importFrom tune extract_fit_parsnip
#' @importFrom utils head
#' @export
summary.fastml_model <- function(object, sort_metric = NULL, ...) {
  # Ensure object is a valid fastml_model
  if (!inherits(object, "fastml_model")) {
    stop("The input must be a 'fastml_model' object.")
  }

  performance <- object$performance

  # Combine all performance metrics into one data frame
  metrics_list <- list()
  for (model_name in names(performance)) {
    model_metrics <- performance[[model_name]]
    model_metrics_df <- as.data.frame(model_metrics)
    model_metrics_df$Model <- model_name
    metrics_list[[model_name]] <- model_metrics_df
  }

  performance_df <- do.call(rbind, metrics_list)

  # Determine main metric for sorting
  all_metric_names <- unique(performance_df$.metric)
  if (!is.null(sort_metric)) {
    if (!(sort_metric %in% all_metric_names)) {
      stop(paste("Invalid sort_metric. Choose from:", paste(all_metric_names, collapse = ", ")))
    }
    main_metric <- sort_metric
  } else {
    optimized_metric <- object$metric
    if (optimized_metric %in% all_metric_names) {
      main_metric <- optimized_metric
    } else {
      main_metric <- all_metric_names[1]
      warning(paste("Optimized metric", optimized_metric, "is not available. Using", main_metric, "as the sorting metric."))
    }
  }

  # Pivot wider for performance metrics
  performance_wide <- reshape2::dcast(performance_df, Model ~ .metric, value.var = ".estimate")

  # Sort by main_metric
  if (object$task == "regression") {
    performance_wide <- performance_wide[order(performance_wide[[main_metric]], na.last = TRUE), ]
  } else {
    performance_wide <- performance_wide[order(-performance_wide[[main_metric]], na.last = TRUE), ]
  }

  best_model_name <- object$best_model_name

  cat("\n===== fastml Model Summary =====\n")
  cat("Best Model:", best_model_name, "\n\n")

  cat("Performance Metrics for All Models:\n\n")
  print(performance_wide, row.names = FALSE)
  cat("\n")

  cat("Best Model Hyperparameters:\n\n")

  # Attempt to extract the final fitted parsnip model
  # This should work if object$best_model is a fitted workflow
  parsnip_fit <- tryCatch({
    tune::extract_fit_parsnip(object$best_model)
  }, error = function(e) {
    NULL
  })

  if (is.null(parsnip_fit)) {
    # Could not extract parsnip fit, print a message
    cat("Could not extract final fitted model details.\n")
  } else {
    # Extract args from the parsnip model spec
    if ("spec" %in% names(parsnip_fit) && "args" %in% names(parsnip_fit$spec)) {
      params <- parsnip_fit$spec$args
      if (length(params) > 0) {
        # Evaluate quosures if needed
        cleaned_params <- list()
        for (pname in names(params)) {
          val <- params[[pname]]
          # If val is a quosure, try to evaluate it
          if (inherits(val, "quosure")) {
            evaluated_val <- tryCatch(eval(rlang::get_expr(val), envir = rlang::get_env(val)),
                                      error = function(e) val)
            cleaned_params[[pname]] <- evaluated_val
          } else {
            cleaned_params[[pname]] <- val
          }
        }

        # Print parameters
        printed_any <- FALSE
        for (pname in names(cleaned_params)) {
          val <- cleaned_params[[pname]]
          # Convert to character for nicer printing if needed
          if (is.numeric(val)) {
            val <- as.character(val)
          }
          cat(pname, ": ", val, "\n", sep = "")
          printed_any <- TRUE
        }

        if (!printed_any) {
          cat("No hyperparameters found.\n")
        }
      } else {
        cat("No hyperparameters found.\n")
      }
    } else {
      cat("No hyperparameters found.\n")
    }
  }

  cat("\nTo make predictions, use the 'predict' function.\n")
  cat("=================================\n")
}
