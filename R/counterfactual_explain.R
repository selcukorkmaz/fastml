#' Generate counterfactual explanations for a fastml model
#'
#' Uses DALEX ceteris-paribus profiles (`predict_profile`) to compute
#' counterfactual-style what-if explanations for a given observation.
#'
#' @param object A `fastml` object.
#' @param observation A single observation (data frame with one row) to compute
#'   counterfactuals for.
#' @param ... Additional arguments passed to `DALEX::predict_profile`.
#'
#' @return A counterfactual explanation object.
#' @importFrom DALEX explain predict_profile
#' @export
counterfactual_explain <- function(object,
                                   observation,
                                   variables = NULL,
                                   positive_class = NULL,
                                   event_class = NULL,
                                   label_levels = NULL,
                                   ...) {
  if (missing(observation)) {
    stop("'observation' must be provided for counterfactual explanations.")
  }
  if (nrow(observation) != 1) {
    stop("'observation' must contain exactly one row.")
  }
  if (!requireNamespace("DALEX", quietly = TRUE)) {
    stop("The 'DALEX' package is required for counterfactual explanations.")
  }

  obs_raw <- observation
  explainer <- NULL
  prep <- NULL

  # 1. Build/Extract Explainer
  if (inherits(object, "fastml")) {
    prep <- fastml_prepare_explainer_inputs(object)
    # align observation with explainer input schema (raw predictors, no label)
    if (!is.null(prep$label) && prep$label %in% names(obs_raw)) {
      obs_raw[[prep$label]] <- NULL
    }
    expected_cols <- colnames(prep$x_raw)
    missing_cols <- setdiff(expected_cols, names(obs_raw))
    if (length(missing_cols)) {
      stop(
        "The observation is missing required columns: ",
        paste(missing_cols, collapse = ", "),
        call. = FALSE
      )
    }
    # keep only expected columns and coerce types to match training schema
    obs_raw <- obs_raw[, expected_cols, drop = FALSE]
    obs_raw <- as.data.frame(obs_raw, stringsAsFactors = FALSE)
    for (nm in expected_cols) {
      ref_col <- prep$x_raw[[nm]]
      if (is.factor(ref_col)) {
        obs_raw[[nm]] <- factor(obs_raw[[nm]], levels = levels(ref_col))
      } else if (is.logical(ref_col)) {
        obs_raw[[nm]] <- as.logical(obs_raw[[nm]])
      } else if (inherits(ref_col, "Date")) {
        obs_raw[[nm]] <- as.Date(obs_raw[[nm]])
      } else if (inherits(ref_col, "POSIXt")) {
        obs_raw[[nm]] <- as.POSIXct(obs_raw[[nm]])
      } else if (is.integer(ref_col)) {
        obs_raw[[nm]] <- as.integer(obs_raw[[nm]])
      } else if (is.numeric(ref_col)) {
        obs_raw[[nm]] <- as.numeric(obs_raw[[nm]])
      } else if (is.character(ref_col)) {
        obs_raw[[nm]] <- as.character(obs_raw[[nm]])
      }
    }
    expl_list <- fastml_build_dalex_explainers(prep)$explainers
    explainer <- expl_list[[1]]
  } else if (inherits(object, "explainer")) {
    explainer <- object
  } else {
    stop("`object` must be either a fastml object or a DALEX explainer.")
  }

  # 2. Filter Variables: Keep ONLY Numerics
  # Identify all numeric columns in the explainer's data
  all_vars <- colnames(explainer$data)
  numeric_vars <- names(explainer$data)[sapply(explainer$data, is.numeric)]

  # Check if user requested specific variables in '...'
  # Check if user requested specific variables in '...'
  args <- list(...)
  user_vars <- NULL

  if ("variables" %in% names(args)) {
    user_vars <- args$variables
    # Remove it from args so it isn't passed twice
    args$variables <- NULL
  }

  # Filter Variables: Keep ONLY Numerics
  all_numeric <- names(explainer$data)[sapply(explainer$data, is.numeric)]

  final_vars <- all_numeric
  if (!is.null(variables)) {
    # Intersect user request with allowed numeric variables
    final_vars <- intersect(variables, all_numeric)
    if (length(final_vars) == 0) {
      warning("No numeric variables found in the requested 'variables' list.")
    }
  }

  # Generate Profile
  profile <- DALEX::predict_profile(
    explainer = explainer,
    new_observation = obs_raw,
    variables = final_vars, # Use the filtered list
    ...
  )

  # --- FIX: ROBUST FILTERING OF LINES AND DOTS ---
  if (!is.null(positive_class)) {
    # A. Capture attributes BEFORE subsetting the main object
    obs_attr <- attr(profile, "observation") # legacy name (unused by ingredients)
    obs_attr_plural <- attr(profile, "observations") # actual name used by ingredients

    # B. Define filters
    matches_lines <- grepl(positive_class, profile$`_label_`, fixed = TRUE)

    # C. Filter the Lines (Main Object)
    if (any(matches_lines)) {
      profile <- profile[matches_lines, ]

      # D. Filter the points attached via attributes
      if (!is.null(obs_attr_plural) && "_label_" %in% names(obs_attr_plural)) {
        matches_dots <- grepl(positive_class, obs_attr_plural$`_label_`, fixed = TRUE)
        obs_attr_plural <- obs_attr_plural[matches_dots, , drop = FALSE]
      }
      if (!is.null(obs_attr) && "_label_" %in% names(obs_attr)) {
        matches_dots_legacy <- grepl(positive_class, obs_attr$`_label_`, fixed = TRUE)
        obs_attr <- obs_attr[matches_dots_legacy, , drop = FALSE]
      }

      # E. Re-attach filtered attributes (ingredients uses "observations")
      if (!is.null(obs_attr_plural)) {
        attr(profile, "observations") <- obs_attr_plural
      }
      if (!is.null(obs_attr)) {
        attr(profile, "observation") <- obs_attr
      }
    }
  }

  # 4. Plot (UPDATED: Added specific Y-axis label)
  plot_obj <- tryCatch(
    suppressWarnings(
      plot(profile) +
        ggplot2::labs(y = paste("Predicted Probability of", positive_class))
    ),
    error = function(e) NULL
  )

  if (!is.null(plot_obj)) print(plot_obj)

  invisible(list(counterfactuals = profile, profile = profile, plot = plot_obj))
}
