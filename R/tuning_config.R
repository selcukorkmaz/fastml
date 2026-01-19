#' @title Tuning Configuration and Complexity Presets
#' @description Functions and presets for configuring hyperparameter tuning grids
#'   with explicit speed-robustness trade-offs.
#' @name tuning_config
NULL

#' Tuning Complexity Presets
#'
#' Returns the configuration for a given tuning complexity level, including
#' grid levels, parameter ranges, and expected computational characteristics.
#'
#' @param complexity Character string specifying the tuning complexity level.
#'   One of:
#'   \describe{
#'     \item{\code{"quick"}}{Minimal tuning for fast iteration. 2-3 levels per
#'       parameter, narrow ranges. Best for: initial exploration, prototyping,
#'       small datasets, time-constrained scenarios. Typical grid size: 4-27 points.}
#'     \item{\code{"balanced"}}{Moderate tuning balancing speed and thoroughness.
#'       3-4 levels per parameter, standard ranges. Best for: most production use
#'       cases, medium datasets. Typical grid size: 27-256 points. This is the default.
#'       }
#'     \item{\code{"thorough"}}{Comprehensive tuning for maximum model quality.
#'       4-5 levels per parameter, wide ranges. Best for: final model selection,
#'       publications, competitions, when compute time is not a constraint.
#'       Typical grid size: 256-3125 points.}
#'     \item{\code{"exhaustive"}}{Maximum coverage tuning. 5-7 levels per parameter,
#'       very wide ranges. Best for: research, benchmarking, when you need to be
#'       certain you've found the best hyperparameters. Warning: Can be very slow.
#'       Typical grid size: 1000-10000+ points. Consider using Bayesian tuning instead.}
#'   }
#'
#' @return A list with components:
#'   \describe{
#'     \item{grid_levels}{Integer number of levels per parameter for grid search.}
#'     \item{bayes_iterations}{Integer number of iterations for Bayesian tuning.}
#'     \item{description}{Human-readable description of the complexity level.}
#'     \item{speed_estimate}{Relative speed estimate (1 = baseline).}
#'     \item{robustness_estimate}{Relative robustness estimate (1-5 scale).}
#'   }
#'
#' @details
#' ## Speed-Robustness Trade-offs
#'
#' Hyperparameter tuning involves a fundamental trade-off between computational
#' cost and the likelihood of finding optimal hyperparameters:
#'
#' \tabular{lllll}{
#'   \strong{Level} \tab \strong{Grid Size} \tab \strong{Time} \tab \strong{Robustness} \tab \strong{Use Case} \cr
#'   quick \tab 4-27 \tab ~1x \tab Low \tab Prototyping, debugging \cr
#'   balanced \tab 27-256 \tab ~10x \tab Medium \tab Most production use \cr
#'   thorough \tab 256-3125 \tab ~100x \tab High \tab Final models, papers \cr
#'   exhaustive \tab 1000-10000+ \tab ~1000x \tab Very High \tab Research, competitions \cr
#' }
#'
#' ### Recommendations:
#'
#' 1. **Start with "quick"** during development to iterate fast
#' 2. **Use "balanced"** for most production pipelines
#' 3. **Switch to "thorough"** for final model selection
#' 4. **Consider Bayesian tuning** (`tuning_strategy = "bayes"`) for high-dimensional
#'    parameter spaces instead of exhaustive grid search
#' 5. **Use adaptive/racing** (`adaptive = TRUE`) to early-stop poor configurations
#'
#' ### Computational Scaling:
#'
#' Grid search scales as O(L^P * F * N) where:
#' - L = number of levels per parameter
#' - P = number of parameters being tuned
#' - F = number of cross-validation folds
#' - N = dataset size
#'
#' For a model with 5 tunable parameters and 10-fold CV:
#' - quick (L=2): 2^5 * 10 = 320 model fits
#' - balanced (L=3): 3^5 * 10 = 2,430 model fits
#' - thorough (L=5): 5^5 * 10 = 31,250 model fits
#'
#' @examples
#' # Get configuration for balanced tuning
#' config <- get_tuning_complexity("balanced")
#' print(config$grid_levels)  # 3
#'
#' # See all available presets
#' print_tuning_presets()
#'
#' @export
get_tuning_complexity <- function(complexity = c("balanced", "quick", "thorough", "exhaustive")) {

  complexity <- match.arg(complexity)

  presets <- list(
    quick = list(
      grid_levels = 2L,
      bayes_iterations = 5L,
      description = "Minimal tuning for fast iteration and prototyping",
      speed_estimate = 1,
      robustness_estimate = 2,
      recommended_folds = 5L,
      param_range_scale = 0.7  # Narrower ranges
    ),
    balanced = list(
      grid_levels = 3L,
      bayes_iterations = 15L,
      description = "Balanced tuning for most production use cases",
      speed_estimate = 10,
      robustness_estimate = 3,
      recommended_folds = 10L,
      param_range_scale = 1.0  # Standard ranges
    ),
    thorough = list(
      grid_levels = 5L,
      bayes_iterations = 30L,
      description = "Comprehensive tuning for final model selection",
      speed_estimate = 100,
      robustness_estimate = 4,
      recommended_folds = 10L,
      param_range_scale = 1.3  # Wider ranges
    ),
    exhaustive = list(
      grid_levels = 7L,
      bayes_iterations = 50L,
      description = "Maximum coverage for research and competitions",
      speed_estimate = 1000,
      robustness_estimate = 5,
      recommended_folds = 10L,
      param_range_scale = 1.5  # Very wide ranges
    )
  )

  presets[[complexity]]
}

#' Print Tuning Presets Summary
#'
#' Prints a formatted summary of all available tuning complexity presets
#' with their characteristics and recommended use cases.
#'
#' @return Invisibly returns a data frame with preset information.
#'
#' @export
print_tuning_presets <- function() {
  presets <- c("quick", "balanced", "thorough", "exhaustive")

  cat("\n")
  cat("=============================================================================\n")
  cat("                    fastml Tuning Complexity Presets\n")
  cat("=============================================================================\n\n")

  cat("SPEED vs ROBUSTNESS TRADE-OFF:\n")
  cat("-----------------------------------------------------------------------------\n")
  cat(sprintf("%-12s %-8s %-12s %-12s %s\n",
              "Preset", "Levels", "Bayes Iter", "Rel. Time", "Robustness"))
  cat("-----------------------------------------------------------------------------\n")

  results <- lapply(presets, function(p) {
    config <- get_tuning_complexity(p)
    cat(sprintf("%-12s %-8d %-12d %-12s %s\n",
                p,
                config$grid_levels,
                config$bayes_iterations,
                paste0("~", config$speed_estimate, "x"),
                paste(rep("*", config$robustness_estimate), collapse = "")))
    data.frame(
      preset = p,
      grid_levels = config$grid_levels,
      bayes_iterations = config$bayes_iterations,
      speed_estimate = config$speed_estimate,
      robustness_estimate = config$robustness_estimate,
      stringsAsFactors = FALSE
    )
  })

  cat("-----------------------------------------------------------------------------\n\n")

  cat("ESTIMATED GRID SIZES (for 5 tunable parameters):\n")
  cat("  quick:      2^5 =      32 combinations\n")
  cat("  balanced:   3^5 =     243 combinations\n")
  cat("  thorough:   5^5 =   3,125 combinations\n")
  cat("  exhaustive: 7^5 =  16,807 combinations\n\n")

  cat("RECOMMENDATIONS:\n")
  cat("  - Start with 'quick' during development for fast iteration\
")
  cat("  - Use 'balanced' (default) for most production pipelines\n")
  cat("  - Switch to 'thorough' for final model selection or publications\n")
  cat("  - Consider tuning_strategy='bayes' instead of 'exhaustive' grid search\n")
  cat("  - Enable adaptive=TRUE for early stopping of poor configurations\n\n")

  cat("USAGE:\n")
  cat("  fastml(..., tuning_complexity = 'balanced')     # Use preset\n")
  cat("  fastml(..., grid_levels = 4)                    # Custom grid levels\n")
  cat("  fastml(..., tuning_strategy = 'bayes',          # Bayesian tuning\n")
  cat("              tuning_iterations = 20)\n\n")

  invisible(do.call(rbind, results))
}

#' Get Tuning Parameters for Complexity Level
#'
#' Returns algorithm-specific tuning parameter ranges adjusted for the
#' specified complexity level.
#'
#' @param algo Character string specifying the algorithm name.
#' @param train_data Data frame containing the training data.
#' @param label Character string specifying the outcome variable name.
#' @param engine Character string specifying the engine.
#' @param complexity Character string specifying tuning complexity level.
#'
#' @return A list of tuning parameter ranges.
#'
#' @details
#' Parameter ranges are scaled based on the complexity level:
#' - \code{quick}: Narrower ranges (70% of standard), fewer discrete values
#' - \code{balanced}: Standard ranges (100%)
#' - \code{thorough}: Wider ranges (130% of standard)
#' - \code{exhaustive}: Very wide ranges (150% of standard)
#'
#' @export
get_tuning_params_for_complexity <- function(algo, train_data, label, engine,
                                             complexity = "balanced") {
  # Get base parameters
  base_params <- get_default_tune_params(algo, train_data, label, engine)

  if (is.null(base_params)) {
    return(NULL)
  }

  # Get complexity configuration
  config <- get_tuning_complexity(complexity)
  scale <- config$param_range_scale

  # Scale continuous parameter ranges
  scaled_params <- lapply(names(base_params), function(param_name) {
    param_range <- base_params[[param_name]]

    if (is.null(param_range) || length(param_range) < 2) {
      return(param_range)
    }

    # Check if this is a log-scale parameter (typically negative values or very small)
    is_log_scale <- any(param_range < 0) ||
      (all(param_range > 0) && max(param_range) / min(param_range) > 100)

    if (is_log_scale) {
      # For log-scale parameters, scale the range width
      mid <- mean(param_range)
      half_width <- (param_range[2] - param_range[1]) / 2
      new_half_width <- half_width * scale
      c(mid - new_half_width, mid + new_half_width)
    } else {
      # For linear-scale parameters
      mid <- mean(param_range)
      half_width <- (param_range[2] - param_range[1]) / 2
      new_half_width <- half_width * scale
      new_range <- c(mid - new_half_width, mid + new_half_width)

      # Ensure non-negative for parameters that should be positive
      if (param_name %in% c("trees", "min_n", "neighbors", "hidden_units", "epochs", "num_comp")) {
        new_range <- pmax(new_range, 1)
      }
      if (param_name %in% c("sample_size", "mixture")) {
        new_range <- pmin(pmax(new_range, 0), 1)
      }

      new_range
    }
  })
  names(scaled_params) <- names(base_params)

  scaled_params
}

#' Expanded Default Tuning Parameters
#'
#' Returns expanded tuning parameter ranges that provide better coverage
#' than the minimal defaults. These are used when \code{tuning_complexity}
#' is set to "thorough" or "exhaustive".
#'
#' @param algo Algorithm name.
#' @param train_data Training data frame.
#' @param label Outcome variable name.
#' @param engine Engine name.
#'
#' @return A list of expanded tuning parameter ranges.
#'
#' @keywords internal
#' @export
get_expanded_tune_params <- function(algo, train_data, label, engine) {
  num_predictors <- ncol(train_data) - 1

  switch(algo,
         # Random Forest - expanded ranges
         "rand_forest" = list(
           mtry = c(1, min(num_predictors, max(2, floor(num_predictors * 0.8)))),
           trees = c(100, 1000),
           min_n = c(1, 20)
         ),

         # XGBoost - comprehensive tuning
         "xgboost" = list(
           trees = c(50, 500),
           tree_depth = c(1, 10),
           learn_rate = c(-3, -0.5),  # 0.001 to ~0.3
           loss_reduction = c(0, 10),
           min_n = c(1, 15),
           sample_size = c(0.5, 1.0),
           mtry = c(0.5, 1.0)  # As proportion for xgboost
         ),

         # LightGBM - comprehensive tuning
         "lightgbm" = list(
           trees = c(50, 500),
           tree_depth = c(2, 12),
           learn_rate = c(-3, -0.5),
           loss_reduction = c(0, 10),
           min_n = c(5, 50),
           sample_size = c(0.5, 1.0),
           mtry = c(0.5, 1.0)
         ),

         # Logistic Regression (regularized)
         "logistic_reg" = {
           if (engine %in% c("glmnet", "brulee", "LiblineaR", "spark", "h2o")) {
             list(penalty = c(-6, 1), mixture = c(0, 1))
           } else {
             list(penalty = NULL, mixture = NULL)
           }
         },

         # Multinomial Regression
         "multinom_reg" = {
           if (engine %in% c("glmnet", "brulee")) {
             list(penalty = c(-6, 1), mixture = c(0, 1))
           } else if (engine == "nnet") {
             list(penalty = c(0, 1))
           } else {
             list(penalty = c(-6, 1), mixture = c(0, 1))
           }
         },

         # Decision Tree
         "decision_tree" = list(
           cost_complexity = c(-6, -1),
           tree_depth = c(1, 15),
           min_n = c(2, 20)
         ),

         # SVM Linear
         "svm_linear" = list(
           cost = c(-4, 4),
           margin = c(0.01, 0.2)
         ),

         # SVM RBF
         "svm_rbf" = list(
           cost = c(-4, 4),
           rbf_sigma = c(-10, 0)
         ),

         # K-Nearest Neighbors
         "nearest_neighbor" = list(
           neighbors = c(1, min(50, floor(sqrt(nrow(train_data))))),
           weight_func = c("rectangular", "triangular", "epanechnikov", "gaussian"),
           dist_power = c(1, 2)
         ),

         # Naive Bayes
         "naive_Bayes" = list(
           smoothness = c(0, 2),
           Laplace = c(0, 2)
         ),

         # Neural Network (MLP)
         "mlp" = list(
           hidden_units = c(2, 20),
           penalty = c(-6, 0),
           epochs = c(50, 500)
         ),

         # Elastic Net
         "elastic_net" = list(
           penalty = c(-6, 1),
           mixture = c(0, 1)
         ),

         # Ridge Regression
         "ridge_reg" = list(
           penalty = c(-6, 2)
         ),

         # Lasso Regression
         "lasso_reg" = list(
           penalty = c(-6, 2)
         ),

         # PLS
         "pls" = list(
           num_comp = c(1, min(20, num_predictors))
         ),

         # C5 Rules
         "C5_rules" = list(
           trees = c(1, 100),
           min_n = c(2, 20)
         ),

         # Bag Tree
         "bag_tree" = list(
           cost_complexity = c(-6, -1),
           tree_depth = c(1, 15),
           min_n = c(2, 20)
         ),

         # Default - return NULL
         NULL
  )
}

#' Estimate Tuning Time
#'
#' Provides a rough estimate of tuning time based on the configuration.
#'
#' @param n_params Number of parameters being tuned.
#' @param n_folds Number of cross-validation folds.
#' @param n_rows Number of rows in training data.
#' @param complexity Tuning complexity level.
#' @param tuning_strategy Tuning strategy ("grid" or "bayes").
#' @param base_fit_time Estimated time for a single model fit in seconds.
#'
#' @return A list with estimated total time and breakdown.
#'
#' @export
estimate_tuning_time <- function(n_params, n_folds = 10, n_rows = 1000,
                                 complexity = "balanced",
                                 tuning_strategy = "grid",
                                 base_fit_time = 1) {
  config <- get_tuning_complexity(complexity)

  if (tuning_strategy == "grid") {
    n_combinations <- config$grid_levels ^ n_params
  } else if (tuning_strategy == "bayes") {
    n_combinations <- config$bayes_iterations
  } else {
    n_combinations <- 1
  }

  total_fits <- n_combinations * n_folds
  estimated_seconds <- total_fits * base_fit_time * (n_rows / 1000)

  list(
    grid_combinations = n_combinations,
    total_model_fits = total_fits,
    estimated_seconds = estimated_seconds,
    estimated_minutes = estimated_seconds / 60,
    estimated_hours = estimated_seconds / 3600,
    breakdown = sprintf(
      "%d combinations x %d folds = %d fits",
      n_combinations, n_folds, total_fits
    )
  )
}

#' Recommend Tuning Configuration
#'
#' Provides recommendations for tuning configuration based on dataset
#' characteristics and time constraints.
#'
#' @param n_rows Number of rows in training data.
#' @param n_predictors Number of predictor variables.
#' @param n_algorithms Number of algorithms to tune.
#' @param max_time_minutes Maximum acceptable tuning time in minutes.
#' @param tuning_strategy Preferred tuning strategy.
#'
#' @return A list with recommended configuration.
#'
#' @export
recommend_tuning_config <- function(n_rows, n_predictors, n_algorithms = 1,
                                    max_time_minutes = 30,
                                    tuning_strategy = "grid") {
  # Estimate base fit time (rough heuristic)
  base_fit_time <- 0.01 * sqrt(n_rows) * sqrt(n_predictors)

  # Average number of tunable parameters
  avg_params <- 4

  recommendations <- list()

  for (complexity in c("quick", "balanced", "thorough", "exhaustive")) {
    est <- estimate_tuning_time(
      n_params = avg_params,
      n_folds = 10,
      n_rows = n_rows,
      complexity = complexity,
      tuning_strategy = tuning_strategy,
      base_fit_time = base_fit_time
    )

    total_time <- est$estimated_minutes * n_algorithms

    recommendations[[complexity]] <- list(
      estimated_minutes = total_time,
      within_budget = total_time <= max_time_minutes
    )
  }

  # Find best complexity within budget
  feasible <- names(recommendations)[sapply(recommendations, function(x) x$within_budget)]
  recommended <- if (length(feasible) > 0) tail(feasible, 1) else "quick"

  list(
    recommended_complexity = recommended,
    recommendations = recommendations,
    suggestion = sprintf(
      "Recommended: '%s' complexity (~%.1f minutes for %d algorithm%s)",
      recommended,
      recommendations[[recommended]]$estimated_minutes,
      n_algorithms,
      if (n_algorithms > 1) "s" else ""
    ),
    alternative = if (recommended != "thorough" && "thorough" %in% names(recommendations)) {
      sprintf(
        "For better results, consider '%s' (~%.1f minutes) if time permits",
        "thorough",
        recommendations[["thorough"]]$estimated_minutes
      )
    } else {
      NULL
    }
  )
}
