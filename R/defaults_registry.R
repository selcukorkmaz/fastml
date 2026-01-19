#' @title Defaults Registry for Engine and Parameter Transparency
#' @description Functions to track, compare, and warn about differences between
#'   fastml defaults and parsnip defaults, providing users with full transparency
#'   and control over model configuration.
#' @name defaults_registry
NULL

#' Get Parsnip Default Engine for an Algorithm
#'
#' Returns the default engine that parsnip would use for a given algorithm,
#' allowing comparison with fastml's optimized defaults.
#'
#' @param algo Character string specifying the algorithm name.
#' @param task Character string specifying the task type ("classification",
#'   "regression", or "survival").
#'
#' @return Character string of the parsnip default engine, or NULL if unknown.
#'
#' @details This function documents parsnip's default engine choices as of
#'   tidymodels 1.x. These may change with future parsnip versions.
#'
#' @keywords internal
#' @export
get_parsnip_default_engine <- function(algo, task = NULL) {
  # Parsnip default engines as of tidymodels 1.x

  # Source: parsnip package documentation and show_engines() output
  parsnip_defaults <- list(
    # Classification/Regression models
    logistic_reg = "glm",
    multinom_reg = "nnet",
    linear_reg = "lm",
    decision_tree = "rpart",
    rand_forest = "ranger",
    boost_tree = "xgboost",
    xgboost = "xgboost",
    lightgbm = "lightgbm",
    svm_linear = "LiblineaR",
    svm_rbf = "kernlab",
    svm_poly = "kernlab",
    nearest_neighbor = "kknn",
    naive_Bayes = "klaR",
    mlp = "nnet",
    bag_tree = "rpart",
    C5_rules = "C5.0",
    discrim_linear = "MASS",
    discrim_quad = "MASS",
    pls = "mixOmics",

    # Survival models (via censored package)
    proportional_hazards = "survival",
    survival_reg = "survival",
    cox_ph = "survival",
    bag_mars = "earth"
  )

  # Task-specific overrides for survival
  if (!is.null(task) && task == "survival") {
    survival_overrides <- list(
      rand_forest = "partykit",  # censored default for survival RF
      rand_forest_survival = "partykit"
    )
    if (algo %in% names(survival_overrides)) {
      return(survival_overrides[[algo]])
    }
  }

  if (algo %in% names(parsnip_defaults)) {
    return(parsnip_defaults[[algo]])
  }

  # Return NULL for algorithms not in parsnip
  NULL
}

#' Get Parsnip Default Parameters for an Algorithm
#'
#' Returns the default parameter values that parsnip would use for a given
#' algorithm and engine combination.
#'
#' @param algo Character string specifying the algorithm name.
#' @param engine Character string specifying the engine.
#' @param task Character string specifying the task type.
#'
#' @return A named list of default parameter values, or NULL if unknown.
#'
#' @details These defaults are based on parsnip's internal defaults and the
#'   underlying engine defaults. They may differ from fastml's optimized
#'   defaults which are tuned for better out-of-box performance.
#'
#' @keywords internal
#' @export
get_parsnip_default_params <- function(algo, engine = NULL, task = NULL) {
  # Parsnip typically uses NULL (engine defaults) for most parameters

  # These are documented defaults from parsnip model specifications
  parsnip_defaults <- list(
    rand_forest = list(
      ranger = list(mtry = NULL, trees = 500L, min_n = NULL),
      randomForest = list(mtry = NULL, trees = 500L, min_n = NULL),
      partykit = list(mtry = NULL, trees = 500L, min_n = 20L),
      aorsf = list(mtry = NULL, trees = 500L, min_n = 5L)
    ),
    xgboost = list(
      xgboost = list(
        tree_depth = 6L,
        trees = 15L,
        learn_rate = 0.3,
        mtry = NULL,
        min_n = 1L,
        loss_reduction = 0,
        sample_size = 1,
        stop_iter = NULL
      )
    ),
    lightgbm = list(
      lightgbm = list(
        trees = 100L,
        tree_depth = -1L,  # unlimited
        learn_rate = 0.1,
        min_n = 20L,
        loss_reduction = 0,
        sample_size = 1,
        mtry = NULL
      )
    ),
    logistic_reg = list(
      glm = list(penalty = NULL, mixture = NULL),
      glmnet = list(penalty = 0, mixture = 1)  # pure lasso by default
    ),
    decision_tree = list(
      rpart = list(tree_depth = 30L, min_n = 2L, cost_complexity = 0.01)
    ),
    svm_linear = list(
      LiblineaR = list(cost = 1, margin = NULL),
      kernlab = list(cost = 1, margin = NULL)
    ),
    nearest_neighbor = list(
      kknn = list(neighbors = 5L, weight_func = "optimal", dist_power = 2)
    ),
    mlp = list(
      nnet = list(hidden_units = 5L, penalty = 0, epochs = 100L)
    )
  )

  if (algo %in% names(parsnip_defaults)) {
    algo_defaults <- parsnip_defaults[[algo]]
    if (!is.null(engine) && engine %in% names(algo_defaults)) {
      return(algo_defaults[[engine]])
    }
    # Return first engine's defaults if engine not specified
    if (length(algo_defaults) > 0) {
      return(algo_defaults[[1]])
    }
  }

  NULL
}

#' Compare fastml and parsnip defaults
#'
#' Compares the default engine and parameter choices between fastml and parsnip
#' for a given algorithm.
#'
#' @param algo Character string specifying the algorithm name.
#' @param task Character string specifying the task type.
#' @param fastml_engine Character string of the fastml default engine.
#' @param fastml_params List of fastml default parameters.
#'
#' @return A list with components:
#'   \describe{
#'     \item{engine_differs}{Logical indicating if engines differ.}
#'     \item{fastml_engine}{The fastml default engine.}
#'     \item{parsnip_engine}{The parsnip default engine.}
#'     \item{param_differences}{Named list of parameters that differ.}
#'   }
#'
#' @keywords internal
#' @export
compare_defaults <- function(algo, task, fastml_engine, fastml_params = NULL) {
  parsnip_engine <- get_parsnip_default_engine(algo, task)
  parsnip_params <- get_parsnip_default_params(algo, fastml_engine, task)

  result <- list(
    engine_differs = FALSE,
    fastml_engine = fastml_engine,
    parsnip_engine = parsnip_engine,
    param_differences = list()
  )

  # Check if engines differ

if (!is.null(parsnip_engine) && !identical(fastml_engine, parsnip_engine)) {
    result$engine_differs <- TRUE
  }

  # Check parameter differences
  if (!is.null(fastml_params) && !is.null(parsnip_params)) {
    common_params <- intersect(names(fastml_params), names(parsnip_params))
    for (param in common_params) {
      fastml_val <- fastml_params[[param]]
      parsnip_val <- parsnip_params[[param]]

      # Skip if both are NULL
      if (is.null(fastml_val) && is.null(parsnip_val)) next

      # Check if values differ (handle NULL cases)
      if (is.null(fastml_val) != is.null(parsnip_val) ||
          (!is.null(fastml_val) && !isTRUE(all.equal(fastml_val, parsnip_val)))) {
        result$param_differences[[param]] <- list(
          fastml = fastml_val,
          parsnip = parsnip_val
        )
      }
    }
  }

  result
}

#' Format Default Override Warning Message
#'
#' Creates a human-readable warning message about default overrides.
#'
#' @param algo Algorithm name.
#' @param comparison Result from compare_defaults().
#' @param show_params Logical; whether to include parameter differences.
#'
#' @return Character string with the warning message.
#'
#' @keywords internal
format_default_override_warning <- function(algo, comparison, show_params = TRUE) {
  messages <- character(0)

  if (comparison$engine_differs) {
    messages <- c(messages, sprintf(
      "fastml uses '%s' engine (parsnip default: '%s')",
      comparison$fastml_engine,
      comparison$parsnip_engine
    ))
  }

  if (show_params && length(comparison$param_differences) > 0) {
    param_msgs <- vapply(names(comparison$param_differences), function(p) {
      diff <- comparison$param_differences[[p]]
      fastml_str <- if (is.null(diff$fastml)) "NULL" else as.character(diff$fastml)
      parsnip_str <- if (is.null(diff$parsnip)) "NULL/engine-default" else as.character(diff$parsnip)
      sprintf("  %s: %s (parsnip: %s)", p, fastml_str, parsnip_str)
    }, character(1))
    messages <- c(messages, param_msgs)
  }

  if (length(messages) == 0) {
    return(NULL)
  }

  paste0("[", algo, "] ", paste(messages, collapse = "; "))
}

#' Environment for Tracking Warned Defaults
#'
#' Internal environment to track which default warnings have been shown
#' in the current session to avoid duplicate warnings.
#'
#' @keywords internal
.fastml_warned_defaults <- new.env(parent = emptyenv())

#' Reset Default Override Warnings
#'
#' Resets the tracking of which default override warnings have been shown.
#' Useful for testing or when starting a new analysis session.
#'
#' @export
reset_default_warnings <- function() {
  rm(list = ls(.fastml_warned_defaults), envir = .fastml_warned_defaults)
  invisible(NULL)
}

#' Warn About Default Overrides
#'
#' Issues a warning if fastml defaults differ from parsnip defaults and
#' the warning hasn't been shown yet in this session.
#'
#' @param algo Algorithm name.
#' @param task Task type.
#' @param fastml_engine fastml's default engine for this algorithm.
#' @param fastml_params fastml's default parameters (optional).
#' @param verbose If TRUE, always show the message (as a message, not warning).
#' @param warn_once If TRUE (default), only warn once per algorithm per session.
#'
#' @return Invisibly returns the comparison result.
#'
#' @export
warn_default_override <- function(algo, task, fastml_engine, fastml_params = NULL,
                                  verbose = FALSE, warn_once = TRUE) {
  comparison <- compare_defaults(algo, task, fastml_engine, fastml_params)

  # Check if there's anything to warn about
  has_differences <- comparison$engine_differs || length(comparison$param_differences) > 0
  if (!has_differences) {
    return(invisible(comparison))
  }

  # Check if we've already warned for this algorithm
  warn_key <- paste0(algo, "_", task)
  if (warn_once && exists(warn_key, envir = .fastml_warned_defaults)) {
    return(invisible(comparison))
  }

  # Format the warning message
  msg <- format_default_override_warning(algo, comparison, show_params = TRUE)
  if (is.null(msg)) {
    return(invisible(comparison))
  }

  full_msg <- paste0(
    msg, "\n",
    "Set algorithm_engines or use_parsnip_defaults=TRUE for explicit control."
  )

  if (verbose) {
    message(full_msg)
  } else {
    warning(full_msg, call. = FALSE)
  }

  # Mark as warned
  if (warn_once) {
    assign(warn_key, TRUE, envir = .fastml_warned_defaults)
  }

  invisible(comparison)
}

#' Get All Default Differences Summary
#'
#' Returns a summary of all differences between fastml and parsnip defaults
#' for the specified algorithms.
#'
#' @param algorithms Character vector of algorithm names.
#' @param task Task type ("classification", "regression", or "survival").
#'
#' @return A data frame summarizing the differences.
#'
#' @export
get_default_differences <- function(algorithms, task = "classification") {
  results <- lapply(algorithms, function(algo) {
    fastml_engine <- tryCatch(
      get_default_engine(algo, task),
      error = function(e) NA_character_
    )
    if (is.na(fastml_engine)) {
      return(data.frame(
        algorithm = algo,
        fastml_engine = NA_character_,
        parsnip_engine = NA_character_,
        engine_differs = NA,
        param_differences = NA_character_,
        stringsAsFactors = FALSE
      ))
    }

    parsnip_engine <- get_parsnip_default_engine(algo, task)
    comparison <- compare_defaults(algo, task, fastml_engine, NULL)

    data.frame(
      algorithm = algo,
      fastml_engine = fastml_engine,
      parsnip_engine = if (is.null(parsnip_engine)) NA_character_ else parsnip_engine,
      engine_differs = comparison$engine_differs,
      param_differences = if (length(comparison$param_differences) == 0) {
        NA_character_
      } else {
        paste(names(comparison$param_differences), collapse = ", ")
      },
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, results)
}

#' Print Default Differences Table
#'
#' Prints a formatted table showing differences between fastml and parsnip
#' defaults for the specified task type.
#'
#' @param task Task type ("classification", "regression", or "survival").
#' @param algorithms Optional character vector of algorithms to check.
#'   If NULL, checks all available algorithms for the task.
#'
#' @return Invisibly returns the differences data frame.
#'
#' @export
print_default_differences <- function(task = "classification", algorithms = NULL) {
  if (is.null(algorithms)) {
    algorithms <- availableMethods(type = task)
  }

  diffs <- get_default_differences(algorithms, task)

  # Only show rows where there are differences
  diffs_only <- diffs[diffs$engine_differs == TRUE | !is.na(diffs$param_differences), ]

  if (nrow(diffs_only) == 0) {
    message("No differences between fastml and parsnip defaults for ", task, " task.")
    return(invisible(diffs))
  }

  message("\nfastml vs parsnip default differences (", task, "):\n")
  message(sprintf("%-20s %-15s %-15s", "Algorithm", "fastml", "parsnip"))
  message(paste(rep("-", 50), collapse = ""))

  for (i in seq_len(nrow(diffs_only))) {
    row <- diffs_only[i, ]
    parsnip_eng <- if (is.na(row$parsnip_engine)) "(not in parsnip)" else row$parsnip_engine
    marker <- if (row$engine_differs) " *" else ""
    message(sprintf("%-20s %-15s %-15s%s",
                    row$algorithm, row$fastml_engine, parsnip_eng, marker))
  }

  message("\n* = engine differs from parsnip default")
  message("\nTip: Use algorithm_engines parameter to explicitly set engines,")
  message("or use_parsnip_defaults=TRUE to use parsnip's default engines.\n")

  invisible(diffs)
}
