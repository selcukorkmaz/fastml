make_grouped_cv <- function(data, group_cols, v = 5, strata = NULL, repeats = 1) {
  if (is.null(group_cols) || length(group_cols) == 0) {
    stop("'group_cols' must be provided when using grouped resampling.")
  }
  missing_cols <- setdiff(group_cols, colnames(data))
  if (length(missing_cols) > 0) {
    stop(
      sprintf(
        "The following grouping columns were not found in the data: %s",
        paste(missing_cols, collapse = ", ")
      )
    )
  }
  if (!is.numeric(v) || length(v) != 1 || v < 2 || v != as.integer(v)) {
    stop("'folds' must be an integer greater than or equal to 2 for 'grouped_cv'.")
  }
  if (!is.numeric(repeats) || length(repeats) != 1 || repeats <= 0 ||
      repeats != as.integer(repeats)) {
    stop("'repeats' must be a positive integer when using 'grouped_cv'.")
  }

  prepared_data <- data
  if (length(group_cols) > 1) {
    prepared_data <- prepared_data %>%
      dplyr::mutate(
        .fastml_group = interaction(!!!rlang::syms(group_cols), drop = TRUE)
      )
    group_sym <- rlang::sym(".fastml_group")
  } else {
    group_sym <- rlang::sym(group_cols[[1]])
  }

  group_values <- prepared_data[[rlang::as_string(group_sym)]]
  if (any(is.na(group_values))) {
    stop("Grouping columns contain missing values, which are not supported for grouped resampling.")
  }

  n_groups <- dplyr::n_distinct(group_values)
  if (n_groups < v) {
    stop(
      sprintf(
        "'folds' cannot exceed the number of groups for 'grouped_cv'. Found %d unique groups for %d-fold CV.",
        n_groups, v
      )
    )
  }

  strata_sym <- NULL
  if (!is.null(strata)) {
    if (!strata %in% colnames(prepared_data)) {
      warning(
        sprintf(
          "Column '%s' requested for stratification was not found; proceeding without stratification.",
          strata
        )
      )
      strata <- NULL
    } else {
      strata_sym <- rlang::sym(strata)
      groups_per_stratum <- prepared_data %>%
        dplyr::distinct(!!group_sym, !!strata_sym) %>%
        dplyr::count(!!strata_sym, name = "n_groups")
      insufficient <- groups_per_stratum[groups_per_stratum$n_groups < v, , drop = FALSE]
      if (nrow(insufficient) > 0) {
        strata_values <- unique(insufficient[[rlang::as_string(strata_sym)]])
        warning(
          sprintf(
            paste0(
              "Unable to stratify grouped folds because the following strata have fewer than %d groups: %s.",
              " Proceeding without stratification."
            ),
            v,
            paste(strata_values, collapse = ", ")
          )
        )
        strata <- NULL
        strata_sym <- NULL
      }
    }
  } else {
    # Without strata, grouped CV may yield imbalanced class distributions across folds.
    # This is documented here because rsample currently lacks native grouped-stratified
    # splitting when no stratification column is supplied.
  }

  try_grouped_cv <- function(strata_column) {
    if (is.null(strata_column)) {
      rsample::group_vfold_cv(
        prepared_data,
        group = !!group_sym,
        v = v,
        repeats = repeats
      )
    } else {
      rsample::group_vfold_cv(
        prepared_data,
        group = !!group_sym,
        v = v,
        repeats = repeats,
        strata = !!strata_column
      )
    }
  }

  resamples <- tryCatch(
    try_grouped_cv(strata_sym),
    error = function(err) {
      if (!is.null(strata_sym)) {
        warning(
          sprintf(
            paste0(
              "Grouped stratification failed: %s. Proceeding without stratification."
            ),
            conditionMessage(err)
          )
        )
        return(try_grouped_cv(NULL))
      }
      stop(err)
    }
  )

  resamples
}

make_blocked_cv <- function(data, block_col, block_size) {
  if (is.null(block_col) || length(block_col) != 1) {
    stop("'block_col' must be provided when using blocked resampling.")
  }
  if (is.null(block_size)) {
    stop("'block_size' must be provided when using blocked resampling.")
  }
  if (!block_col %in% colnames(data)) {
    stop(sprintf("Column '%s' was not found in the data.", block_col))
  }
  if (!is.numeric(block_size) || length(block_size) != 1 || block_size <= 0 ||
      block_size != as.integer(block_size)) {
    stop("'block_size' must be a positive integer.")
  }
  if (any(is.na(data[[block_col]]))) {
    stop(sprintf("Column '%s' contains missing values which are not supported for blocked resampling.", block_col))
  }
  if (!identical(order(data[[block_col]]), seq_len(nrow(data)))) {
    stop(sprintf("Data must be sorted by '%s' in ascending order before applying blocked resampling.", block_col))
  }

  block_ids <- ceiling(seq_len(nrow(data)) / block_size)
  n_blocks <- length(unique(block_ids))
  if (n_blocks < 2) {
    stop("'block_size' results in fewer than two blocks, which is insufficient for blocked resampling.")
  }

  list(
    data = data,
    block_ids = block_ids,
    n_blocks = n_blocks
  )
}

make_rolling_origin_cv <- function(data,
                                   block_col,
                                   initial_window,
                                   assess_window,
                                   skip = 0) {
  if (is.null(block_col) || length(block_col) != 1) {
    stop("'block_col' must be provided when using rolling resampling.")
  }
  if (!block_col %in% colnames(data)) {
    stop(sprintf("Column '%s' was not found in the data.", block_col))
  }
  if (any(is.na(data[[block_col]]))) {
    stop(sprintf("Column '%s' contains missing values which are not supported for rolling resampling.", block_col))
  }
  if (!identical(order(data[[block_col]]), seq_len(nrow(data)))) {
    stop(sprintf("Data must be sorted by '%s' in ascending order before applying rolling resampling.", block_col))
  }

  window_args <- list(
    initial_window = initial_window,
    assess_window = assess_window,
    skip = skip
  )

  if (is.null(window_args$initial_window) || is.null(window_args$assess_window)) {
    stop("Both 'initial_window' and 'assess_window' must be provided for rolling resampling.")
  }

  for (nm in c("initial_window", "assess_window")) {
    value <- window_args[[nm]]
    if (!is.numeric(value) || length(value) != 1 || value <= 0 ||
        value != as.integer(value)) {
      stop(sprintf("'%s' must be a positive integer.", nm))
    }
  }

  if (!is.numeric(window_args$skip) || length(window_args$skip) != 1 ||
      window_args$skip < 0 || window_args$skip != as.integer(window_args$skip)) {
    stop("'skip' must be a non-negative integer.")
  }

  rsample::rolling_origin(
    data,
    initial = window_args$initial_window,
    assess = window_args$assess_window,
    skip = window_args$skip
  )
}

make_nested_cv <- function(data,
                            outer_folds = 5,
                            inner_folds = 5,
                            group_cols = NULL,
                            strata = NULL) {
  if (!is.numeric(outer_folds) || length(outer_folds) != 1 || outer_folds < 2 ||
      outer_folds != as.integer(outer_folds)) {
    stop("'outer_folds' must be an integer greater than or equal to 2.")
  }

  if (!is.numeric(inner_folds) || length(inner_folds) != 1 || inner_folds < 2 ||
      inner_folds != as.integer(inner_folds)) {
    stop("'inner_folds' must be an integer greater than or equal to 2.")
  }

  strata_sym <- if (!is.null(strata)) rlang::sym(strata) else NULL

  outer_resamples <- if (!is.null(group_cols) && length(group_cols) > 0) {
    make_grouped_cv(
      data = data,
      group_cols = group_cols,
      v = outer_folds,
      strata = strata,
      repeats = 1
    )
  } else {
    if (is.null(strata_sym)) {
      rsample::vfold_cv(data, v = outer_folds)
    } else {
      rsample::vfold_cv(data, v = outer_folds, strata = !!strata_sym)
    }
  }

  nested_tbl <- outer_resamples
  nested_tbl$inner_resamples <- vector("list", length(outer_resamples$splits))

  for (i in seq_along(outer_resamples$splits)) {
    outer_analysis <- rsample::analysis(outer_resamples$splits[[i]])
    if (!is.null(group_cols) && length(group_cols) > 0) {
      inner_resamples <- make_grouped_cv(
        data = outer_analysis,
        group_cols = group_cols,
        v = inner_folds,
        strata = strata,
        repeats = 1
      )
    } else {
      if (is.null(strata_sym)) {
        inner_resamples <- rsample::vfold_cv(outer_analysis, v = inner_folds)
      } else {
        inner_resamples <- rsample::vfold_cv(
          outer_analysis,
          v = inner_folds,
          strata = !!strata_sym
        )
      }
    }
    nested_tbl$inner_resamples[[i]] <- inner_resamples
  }

  class(nested_tbl) <- unique(c("nested_cv", class(outer_resamples)))
  nested_tbl
}

fastml_nested_strip_meta <- function(param_tbl) {
  meta_cols <- c(".metric", ".estimator", ".config", ".notes", ".iter", ".order",
                 "mean", "n", "std_err")
  keep <- setdiff(names(param_tbl), meta_cols)
  if (length(keep) == 0) {
    return(NULL)
  }
  tibble::as_tibble(param_tbl[, keep, drop = FALSE])
}

fastml_nested_signature <- function(param_tbl) {
  if (is.null(param_tbl) || nrow(param_tbl) == 0) {
    return(NA_character_)
  }
  vals <- vapply(
    param_tbl,
    function(col) {
      if (is.list(col)) {
        paste(vapply(col, function(x) paste(as.character(x), collapse = ","), character(1)), collapse = ";")
      } else {
        paste(as.character(col), collapse = ",")
      }
    },
    character(1)
  )
  paste(names(vals), vals, sep = "=", collapse = "||")
}

fastml_nested_select_params <- function(best_params_list) {
  if (length(best_params_list) == 0) {
    return(NULL)
  }
  params <- best_params_list[!vapply(best_params_list, is.null, logical(1))]
  if (length(params) == 0) {
    return(NULL)
  }
  stripped <- lapply(params, fastml_nested_strip_meta)
  stripped <- stripped[!vapply(stripped, is.null, logical(1))]
  if (length(stripped) == 0) {
    return(NULL)
  }
  signatures <- vapply(stripped, fastml_nested_signature, character(1))
  counts <- table(signatures)
  top_signature <- names(counts)[which.max(counts)]
  idx <- match(top_signature, signatures)
  stripped[[idx]]
}

fastml_run_nested_cv <- function(workflow_spec,
                                  nested_resamples,
                                  train_data,
                                  label,
                                  task,
                                 metric,
                                 metrics,
                                 engine,
                                 engine_args,
                                 tune_params_template,
                                 engine_tune_params,
                                 tuning_strategy,
                                 tuning_iterations,
                                 adaptive,
                                 early_stopping,
                                 ctrl_grid,
                                 ctrl_bayes,
                                 ctrl_race,
                                 my_metrics,
                                 do_tuning,
                                 event_class,
                                 start_col,
                                 time_col,
                                 status_col,
                                 eval_times,
                                 at_risk_threshold,
                                 seed,
                                 update_params_fn) {
  if (!inherits(nested_resamples, "rset") ||
      is.null(nested_resamples$inner_resamples)) {
    stop("'nested_resamples' must be a nested resampling object with inner resamples.")
  }

  outer_ids <- nested_resamples$id
  inner_list <- nested_resamples$inner_resamples
  outer_splits <- nested_resamples$splits

  inner_results <- vector("list", length(outer_splits))
  outer_metrics <- vector("list", length(outer_splits))
  best_params_list <- vector("list", length(outer_splits))

  metric_direction <- tryCatch({
    if (requireNamespace("yardstick", quietly = TRUE) &&
        exists(metric, envir = asNamespace("yardstick"), inherits = FALSE)) {
      attr(get(metric, envir = asNamespace("yardstick")), "direction", exact = TRUE)
    } else {
      NA_character_
    }
  }, error = function(e) NA_character_)

  lower_is_better <- if (!is.na(metric_direction) && !is.null(metric_direction)) {
    identical(metric_direction, "minimize")
  } else {
    metric %in% c("rmse", "mae", "ibs", "logloss", "mse", "brier_score") ||
      grepl("brier", metric, fixed = TRUE)
  }

  for (i in seq_along(outer_splits)) {
    current_split <- outer_splits[[i]]
    if (is.null(current_split)) {
      next
    }

    inner_resamples <- inner_list[[i]]
    if (do_tuning && (is.null(inner_resamples) || !inherits(inner_resamples, "rset"))) {
      stop("Inner resamples must be an 'rset' object when tuning is enabled.")
    }

    outer_train <- rsample::analysis(current_split)
    outer_assess <- rsample::assessment(current_split)

    current_workflow <- workflow_spec
    best_params <- NULL

    if (do_tuning) {
      params_current <- tune_params_template
      if (!is.null(params_current)) {
        params_current <- finalize(
          params_current,
          x = outer_train %>% dplyr::select(-dplyr::all_of(label))
        )
      }

      if (!is.null(params_current) && !is.null(engine_tune_params)) {
        params_current <- update_params_fn(params_current, engine_tune_params)
      }

      grid_current <- NULL
      if (!is.null(params_current) && nrow(params_current) > 0) {
        if (tuning_strategy == "grid" && !adaptive) {
          grid_current <- grid_regular(params_current, levels = 3)
        }
      }

      tuning_args <- c(
        list(
          object = workflow_spec,
          resamples = inner_resamples,
          metrics = if (!is.null(my_metrics)) my_metrics else metrics
        ),
        engine_args
      )

      if (tuning_strategy == "bayes") {
        tuning_args$param_info <- params_current
        tuning_args$iter <- tuning_iterations
        tuning_args$control <- ctrl_bayes
        model_tuned <- do.call(tune_bayes, tuning_args)
      } else if (adaptive) {
        tuning_args$param_info <- params_current
        tuning_args$grid <- if (is.null(grid_current)) 20 else grid_current
        tuning_args$control <- ctrl_race
        model_tuned <- do.call(tune_race_anova, tuning_args)
      } else {
        grid_arg <- if (!is.null(grid_current)) {
          grid_current
        } else {
          5
        }
        tuning_args$grid <- grid_arg
        tuning_args$control <- ctrl_grid
        model_tuned <- do.call(tune::tune_grid, tuning_args)
      }

      inner_results[[i]] <- model_tuned
      best_params <- tryCatch({
        select_best(model_tuned, metric = metric)
      }, error = function(e) NULL)

      if (!is.null(best_params)) {
        best_params_list[[i]] <- best_params
        current_workflow <- finalize_workflow(workflow_spec, best_params)
      }
    }

    fitted_outer <- tryCatch({
      do.call(
        parsnip::fit,
        c(list(object = current_workflow, data = outer_train), engine_args)
      )
    }, error = function(e) {
      warning(sprintf("Outer fit failed for split '%s': %s", outer_ids[[i]], e$message))
      NULL
    })

    if (!is.null(fitted_outer)) {
      eval_result <- tryCatch({
        process_model(
          model_obj = fitted_outer,
          model_id = paste0("outer_", outer_ids[[i]]),
          task = task,
          test_data = outer_assess,
          label = label,
          event_class = event_class,
          start_col = start_col,
          time_col = time_col,
          status_col = status_col,
          engine = engine,
          train_data = outer_train,
          metric = metric,
          eval_times_user = eval_times,
          bootstrap_ci = FALSE,
          bootstrap_samples = 1,
          bootstrap_seed = seed,
          at_risk_threshold = at_risk_threshold
        )
      }, error = function(e) {
        warning(sprintf("Evaluation failed for outer split '%s': %s", outer_ids[[i]], e$message))
        NULL
      })

      if (!is.null(eval_result) && !is.null(eval_result$performance)) {
        outer_metrics[[i]] <- eval_result$performance %>%
          dplyr::mutate(outer_id = outer_ids[[i]])
      }
    }
  }

  outer_metrics <- outer_metrics[!vapply(outer_metrics, is.null, logical(1))]
  outer_performance <- if (length(outer_metrics) > 0) {
    dplyr::bind_rows(outer_metrics)
  } else {
    NULL
  }

  final_params <- NULL
  selected_outer_id <- NULL
  if (do_tuning) {
    final_params <- fastml_nested_select_params(best_params_list)
  }

  final_workflow <- if (!is.null(final_params)) {
    finalize_workflow(workflow_spec, final_params)
  } else {
    workflow_spec
  }

  final_model <- do.call(
    parsnip::fit,
    c(list(object = final_workflow, data = train_data), engine_args)
  )

  list(
    final_model = final_model,
    details = list(
      inner_results = inner_results,
      outer_performance = outer_performance,
      final_params = final_params,
      selected_outer_id = selected_outer_id
    )
  )
}

#' Train Specified Machine Learning Algorithms on the Training Data
#'
#' Trains specified machine learning algorithms on the preprocessed training data.
#'
#' @param train_data Preprocessed training data frame.
#' @param train_data Preprocessed training data frame.
#' @param label Name of the target variable.
#' @param task Type of task: "classification", "regression", or "survival".
#' @param algorithms Vector of algorithm names to train.
#' @param resampling_method Resampling method for cross-validation. Supported
#'   options include standard \code{"cv"}, \code{"repeatedcv"}, and
#'   \code{"boot"}, as well as grouped resampling via \code{"grouped_cv"},
#'   blocked/rolling schemes via \code{"blocked_cv"} or \code{"rolling_origin"},
#'   nested resampling via \code{"nested_cv"}, and the passthrough
#'   \code{"none"} option.
#' @param folds Number of folds for cross-validation.
#' @param repeats Number of times to repeat cross-validation (only applicable for methods like "repeatedcv").
#' @param resamples Optional rsample object. If provided, custom resampling splits
#'   will be used instead of those created internally.
#' @param group_cols Optional character vector of grouping columns used with
#'   `resampling_method = "grouped_cv"`. For classification problems the outcome
#'   column is used to request grouped stratification where supported; if class
#'   imbalance prevents stratification, grouped folds are still created and a
#'   warning is emitted to document the limitation.
#' @param block_col Optional name of the ordering column used with blocked or
#'   rolling resampling.
#' @param block_size Optional integer specifying the block size for
#'   `resampling_method = "blocked_cv"`.
#' @param initial_window Optional integer specifying the initial window size for
#'   rolling resampling.
#' @param assess_window Optional integer specifying the assessment window size
#'   for rolling resampling.
#' @param skip Optional integer number of resamples to skip between rolling
#'   resamples.
#' @param outer_folds Optional integer specifying the number of outer folds for
#'   `resampling_method = "nested_cv"`.
#' @param tune_params A named list of tuning ranges. For each algorithm, supply a
#'   list of engine-specific parameter values, e.g.
#'   \code{list(rand_forest = list(ranger = list(mtry = c(1, 3)))).}
#' @param engine_params A named list of fixed engine-level arguments passed
#'   directly to the model fitting call for each algorithm/engine combination.
#'   Use this to control options like \code{ties = "breslow"} for Cox models or
#'   \code{importance = "impurity"} for ranger. Unlike \code{tune_params}, these
#'   values are not tuned over a grid.
#' @param metric The performance metric to optimize.
#' @param summaryFunction A custom summary function for model evaluation. Default is \code{NULL}.
#' @param seed An integer value specifying the random seed for reproducibility.
#' @param recipe A recipe object for preprocessing.
#' @param use_default_tuning Logical; if \code{TRUE} and \code{tune_params} is \code{NULL}, tuning is performed using default grids. Tuning also occurs when custom \code{tune_params} are supplied. When \code{FALSE} and no custom parameters are given, the model is fitted once with default settings.
#' @param tuning_strategy A string specifying the tuning strategy. Must be one of
#'   \code{"grid"}, \code{"bayes"}, or \code{"none"}. Adaptive methods may be
#'   used with \code{"grid"}. If \code{"none"} is selected, the workflow is fitted
#'   directly without tuning.
#'   If custom \code{tune_params} are supplied with \code{tuning_strategy = "none"},
#'   they will be ignored with a warning.
#' @param tuning_iterations Number of iterations for Bayesian tuning. Ignored
#'   when \code{tuning_strategy} is not \code{"bayes"}; validation occurs only
#'   for the Bayesian strategy.
#' @param early_stopping Logical for early stopping in Bayesian tuning.
#' @param adaptive Logical indicating whether to use adaptive/racing methods.
#' @param algorithm_engines A named list specifying the engine to use for each algorithm.
#' @param event_class Character string identifying the positive class when computing
#'   classification metrics ("first" or "second").
#' @param start_col Optional name of the survival start time column passed through
#'   to downstream evaluation helpers.
#' @param time_col Optional name of the survival stop time column.
#' @param status_col Optional name of the survival status/event column.
#' @param eval_times Optional numeric vector of time horizons for survival metrics.
#' @param at_risk_threshold Numeric cutoff used to determine the evaluation window
#'   for survival metrics within guarded resampling.
#' @param audit_env Internal environment that tracks security audit findings when
#'   custom preprocessing hooks are executed. Typically supplied by
#'   \code{fastml()} and should be left as \code{NULL} when calling
#'   \code{train_models()} directly.
#' @importFrom magrittr %>%
#' @importFrom dplyr filter mutate select if_else starts_with arrange row_number
#'   n_distinct
#' @importFrom tibble tibble
#' @importFrom rlang sym syms call2 call_modify expr as_string
#' @importFrom dials range_set value_set grid_regular grid_latin_hypercube finalize
#' @importFrom parsnip fit extract_parameter_set_dials
#' @importFrom workflows workflow add_model add_recipe
#' @importFrom tune tune_grid control_grid select_best finalize_workflow finalize_model tune_bayes control_grid control_bayes
#' @importFrom yardstick metric_set accuracy kap roc_auc sens spec precision f_meas rmse rsq mae new_class_metric
#' @importFrom rsample vfold_cv bootstraps validation_split group_vfold_cv
#'   rolling_origin nested_cv
#' @importFrom recipes all_nominal_predictors all_numeric_predictors all_outcomes all_predictors
#' @importFrom finetune control_race tune_race_anova
#' @importFrom rstpm2 stpm2
#' @return A list of trained model objects.
#' @export
train_models <- function(train_data,
                         label,
                         task,
                         algorithms,
                         resampling_method,
                         folds,
                         repeats,
                         group_cols = NULL,
                         block_col = NULL,
                         block_size = NULL,
                         initial_window = NULL,
                         assess_window = NULL,
                         skip = 0,
                         outer_folds = NULL,
                         resamples = NULL,
                         tune_params,
                         engine_params = list(),
                         metric,
                         summaryFunction = NULL,
                         seed = 123,
                         recipe,
                         use_default_tuning = FALSE,
                         tuning_strategy = "grid",
                         tuning_iterations = 10,
                         early_stopping = FALSE,
                         adaptive = FALSE,
                         algorithm_engines = NULL,
                         event_class = "first",
                         start_col = NULL,
                         time_col = NULL,
                         status_col = NULL,
                         eval_times = NULL,
                         at_risk_threshold = 0.1,
                         audit_env = NULL) {

  set.seed(seed)

  if (is.null(audit_env) || !is.environment(audit_env)) {
    audit_env <- fastml_init_audit_env(FALSE)
  }

  tuning_strategy <- match.arg(tuning_strategy, c("grid", "bayes", "none"))

  resample_data <- train_data

  supported_resampling <- c(
    "cv",
    "repeatedcv",
    "boot",
    "grouped_cv",
    "blocked_cv",
    "rolling_origin",
    "nested_cv",
    "none"
  )

  if (!resampling_method %in% supported_resampling) {
    stop("Unsupported resampling method.")
  }
  if (!is.null(repeats) && length(repeats) == 1 && is.na(repeats)) {
    repeats <- NULL
  }
  if (!is.null(repeats)) {
    if (!is.numeric(repeats) || length(repeats) != 1 ||
        !isTRUE(all.equal(repeats, round(repeats))) || repeats <= 0) {
      stop("'repeats' must be a positive integer when supplied.", call. = FALSE)
    }
    repeats <- as.integer(round(repeats))
  }

  if (tuning_strategy == "bayes" && adaptive) {
    warning("'adaptive' is not supported with Bayesian tuning. Setting adaptive = FALSE.")
    adaptive <- FALSE
  }

  if (tuning_strategy == "none" && !is.null(tune_params)) {
    warning("'tune_params' are ignored when 'tuning_strategy' is 'none'")
  }

  if (tuning_strategy == "bayes") {
    if (!is.numeric(tuning_iterations) || length(tuning_iterations) != 1 ||
        tuning_iterations <= 0 || tuning_iterations != as.integer(tuning_iterations)) {
      stop("'tuning_iterations' must be a positive integer")
    }
  }

  if (early_stopping && tuning_strategy != "bayes") {
    message("Engine-level early stopping will be applied when supported")
  }

  if (task == "survival") {
    models <- list()

    response_col <- label
    label_cols <- attr(train_data[[label]], "fastml_label_cols")
    if (is.null(label_cols)) {
      if (length(label) > 1) {
        label_cols <- label
      } else {
        stop("Survival training requires original time/status column names to be stored on the response.")
      }
    }
    start_col <- if (length(label_cols) == 3) label_cols[1] else NULL
    time_col <- if (length(label_cols) == 3) label_cols[2] else label_cols[1]
    status_col <- label_cols[length(label_cols)]

    get_engine <- function(algo, default_engine) {
      if (!is.null(algorithm_engines) && !is.null(algorithm_engines[[algo]])) {
        return(algorithm_engines[[algo]])
      } else {
        return(default_engine)
      }
    }

    rec_prep_cache <- NULL
    baked_train_cache <- NULL
    get_prepped_data <- function() {
      if (is.null(rec_prep_cache)) {
        rec_prep_cache <<- recipes::prep(recipe, training = train_data, retain = TRUE)
        baked_train_cache <<- recipes::bake(rec_prep_cache, new_data = NULL)
      }
      list(recipe = rec_prep_cache, data = baked_train_cache)
    }

    create_native_spec <- function(algo_name, engine_name, fit_obj, recipe_obj, extras = list()) {
      spec <- c(list(
        algo = algo_name,
        engine = if (!is.null(engine_name)) engine_name else NA_character_,
        fit = fit_obj,
        recipe = recipe_obj,
        response = response_col,
        label_cols = label_cols,
        time_col = time_col,
        status_col = status_col,
        start_col = start_col
      ), extras)
      class(spec) <- c("fastml_native_survival", "fastml_model")
      spec
    }

    resampling_summaries <- list()

    for (algo in algorithms) {
      engine <- get_engine(algo, get_default_engine(algo, task))
      engine_args <- resolve_engine_params(engine_params, algo, engine)
      fit_engine_args <- NULL

      if (algo == "rand_forest") {
        if (identical(engine, "aorsf") && !requireNamespace("aorsf", quietly = TRUE)) {
          if (requireNamespace("ranger", quietly = TRUE)) {
            warning("Engine 'aorsf' not installed. Falling back to 'ranger' for survival random forest.")
            engine <- "ranger"
          } else {
            warning("Neither 'aorsf' nor 'ranger' are available. Skipping survival random forest.")
            next
          }
        }
        if (!requireNamespace("censored", quietly = TRUE)) {
          warning("Package 'censored' not installed; skipping survival random forest parsnip model.")
          next
        }
        spec <- define_rand_forest_spec("survival", train_data, label,
                                       tuning = FALSE, engine = engine)$model_spec
      } else if (algo == "elastic_net") {
        warning("Survival 'elastic_net' requires censored survival model specs not available in your setup. Skipping.")
        next
      } else if (algo == "cox_ph") {
        if (!requireNamespace("survival", quietly = TRUE)) {
          stop("The 'survival' package is required for Cox PH. Please install it.")
        }
        prep_dat <- get_prepped_data()
        baked_train <- prep_dat$data
        rec_prep <- prep_dat$recipe
        fit <- call_with_engine_params(
          survival::coxph,
          list(
            formula = as.formula(paste(response_col, "~ .")),
            data = baked_train,
            ties = "efron"
          ),
          engine_args
        )
        spec <- create_native_spec("cox_ph", engine, fit, rec_prep)
      } else if (algo == "penalized_cox") {
        if (!requireNamespace("censored", quietly = TRUE)) {
          warning("Package 'censored' not installed; skipping penalized_cox.")
          next
        }
        if (!requireNamespace("glmnet", quietly = TRUE)) {
          stop("The 'glmnet' package is required for penalized_cox. Please install it.")
        }

        defaults <- get_default_params("penalized_cox", task, engine = engine)
        penalty_val <- defaults$penalty
        mixture_val <- defaults$mixture

        if (length(engine_args) > 0) {
          if ("penalty" %in% names(engine_args)) {
            penalty_val <- engine_args$penalty
            engine_args$penalty <- NULL
          }
          if ("mixture" %in% names(engine_args)) {
            mixture_val <- engine_args$mixture
            engine_args$mixture <- NULL
          }
        }

        spec_info <- define_penalized_cox_spec(
          task = task,
          penalty = penalty_val,
          mixture = mixture_val,
          engine = engine,
          engine_params = engine_args
        )
        spec <- spec_info$model_spec
        fit_engine_args <- spec_info$fit_args
      } else if (algo == "xgboost") {
        if (!requireNamespace("xgboost", quietly = TRUE)) {
          warning("Package 'xgboost' not installed; skipping xgboost survival model.")
          next
        }

        engine_name <- engine
        if (is.null(engine_name) || length(engine_name) == 0 || is.na(engine_name)) {
          engine_name <- "aft"
        }
        engine_name <- tolower(as.character(engine_name)[1])
        if (engine_name %in% c("xgboost", "default")) {
          engine_name <- "aft"
        }
        if (!identical(engine_name, "aft")) {
          warning(sprintf("XGBoost survival engine '%s' is not supported; skipping.", engine_name))
          next
        }
        engine <- engine_name

        if (!is.null(start_col)) {
          warning("XGBoost AFT does not support start-stop survival outcomes; skipping.")
          next
        }

        prep_dat <- get_prepped_data()
        baked_train <- prep_dat$data
        rec_prep <- prep_dat$recipe

        predictor_cols <- setdiff(names(baked_train), response_col)
        if (length(predictor_cols) == 0) {
          warning("No predictors available after preprocessing; skipping xgboost survival model.")
          next
        }
        predictors <- baked_train[, predictor_cols, drop = FALSE]
        feature_names <- predictor_cols
        design_mat <- fastml_prepare_xgb_matrix(predictors, feature_names)

        if (ncol(design_mat) == 0) {
          warning("Unable to construct a feature matrix for xgboost; skipping.")
          next
        }

        time_vec <- as.numeric(train_data[[time_col]])
        status_vec <- as.numeric(train_data[[status_col]])
        valid_idx <- is.finite(time_vec) & time_vec > 0 & is.finite(status_vec)
        if (!any(valid_idx)) {
          warning("No valid survival outcomes available for xgboost; skipping.")
          next
        }
        if (!all(valid_idx)) {
          design_mat <- design_mat[valid_idx, , drop = FALSE]
          time_vec <- time_vec[valid_idx]
          status_vec <- status_vec[valid_idx]
        }
        if (!any(status_vec > 0, na.rm = TRUE)) {
          warning("XGBoost AFT requires at least one observed event; skipping.")
          next
        }

        time_vec[time_vec <= 0 | !is.finite(time_vec)] <- min(time_vec[time_vec > 0 & is.finite(time_vec)])
        log_time <- log(time_vec)
        lower_bounds <- log_time
        upper_bounds <- log_time
        upper_bounds[status_vec <= 0 | !is.finite(status_vec)] <- Inf

        dtrain <- xgboost::xgb.DMatrix(data = design_mat, label = log_time)
        xgboost::setinfo(dtrain, "label_lower_bound", lower_bounds)
        xgboost::setinfo(dtrain, "label_upper_bound", upper_bounds)

        defaults <- get_default_params(
          "xgboost",
          task,
          num_predictors = ncol(design_mat),
          engine = "xgboost"
        )

        max_depth <- defaults$tree_depth
        if (is.null(max_depth) || !is.finite(max_depth)) max_depth <- 6
        max_depth <- as.integer(max(1, round(max_depth)))

        eta <- defaults$learn_rate
        if (is.null(eta) || !is.finite(eta)) eta <- 0.1

        gamma <- defaults$loss_reduction
        if (is.null(gamma) || !is.finite(gamma)) gamma <- 0

        min_child_weight <- defaults$min_n
        if (is.null(min_child_weight) || !is.finite(min_child_weight)) min_child_weight <- 2

        subsample <- defaults$sample_size
        if (is.null(subsample) || !is.finite(subsample)) subsample <- 0.5

        mtry <- defaults$mtry
        if (is.null(mtry) || !is.finite(mtry)) {
          colsample <- 1
        } else {
          colsample <- min(1, max(1, mtry) / max(1, ncol(design_mat)))
        }

        nrounds <- defaults$trees
        if (is.null(nrounds) || !is.finite(nrounds) || nrounds <= 0) nrounds <- 50
        nrounds <- as.integer(max(1, round(nrounds)))

        params <- list(
          objective = "survival:aft",
          eval_metric = "aft-nloglik",
          max_depth = max_depth,
          eta = eta,
          gamma = gamma,
          min_child_weight = min_child_weight,
          subsample = subsample,
          colsample_bytree = colsample,
          aft_loss_distribution = "logistic",
          aft_loss_distribution_scale = 1,
          verbosity = 0
        )

        aft_quantiles <- c(0.25, 0.5, 0.75)
        early_stop <- defaults$stop_iter
        if (!is.null(early_stop) && is.finite(early_stop) && early_stop <= 0) {
          early_stop <- NULL
        }

        extra_args <- list()
        if (length(engine_args) > 0) {
          if (!is.null(engine_args$params)) {
            if (!is.list(engine_args$params)) {
              stop("Engine parameters for xgboost must provide 'params' as a list.")
            }
            params <- merge_engine_args(params, engine_args$params)
            engine_args$params <- NULL
          }
          if (!is.null(engine_args$nrounds)) {
            nrounds <- as.integer(engine_args$nrounds[1])
            engine_args$nrounds <- NULL
          }
          if (!is.null(engine_args$early_stopping_rounds)) {
            early_stop <- as.integer(engine_args$early_stopping_rounds[1])
            engine_args$early_stopping_rounds <- NULL
          }
          if (!is.null(engine_args$quantiles)) {
            aft_quantiles <- engine_args$quantiles
            engine_args$quantiles <- NULL
          }
          if (!is.null(engine_args$aft_quantiles)) {
            aft_quantiles <- engine_args$aft_quantiles
            engine_args$aft_quantiles <- NULL
          }
          extra_args <- merge_engine_args(extra_args, engine_args)
        }

        aft_quantiles <- as.numeric(aft_quantiles)
        aft_quantiles <- aft_quantiles[is.finite(aft_quantiles) & aft_quantiles > 0 & aft_quantiles < 1]
        aft_quantiles <- sort(unique(aft_quantiles))

        if (length(aft_quantiles) == 0) {
          aft_quantiles <- c(0.25, 0.5, 0.75)
        }

        if (!is.finite(nrounds) || nrounds <= 0) {
          warning("xgboost survival requires 'nrounds' > 0; skipping.")
          next
        }

        train_args <- c(
          list(
            params = params,
            data = dtrain,
            nrounds = nrounds
          ),
          extra_args
        )

        if (!is.null(early_stop) && is.finite(early_stop) && early_stop > 0) {
          train_args$early_stopping_rounds <- as.integer(early_stop)
        }

        booster <- do.call(xgboost::xgb.train, train_args)

        aft_distribution <- params$aft_loss_distribution
        if (is.null(aft_distribution) || !nzchar(as.character(aft_distribution)[1])) {
          aft_distribution <- "logistic"
        }
        aft_scale <- params$aft_loss_distribution_scale
        if (is.null(aft_scale) || !is.finite(aft_scale) || aft_scale <= 0) {
          aft_scale <- 1
        }

        fit <- list(
          booster = booster,
          objective = "survival:aft",
          feature_names = colnames(design_mat),
          aft_distribution = as.character(aft_distribution)[1],
          aft_scale = as.numeric(aft_scale)[1],
          aft_quantiles = aft_quantiles
        )
        class(fit) <- "fastml_xgb_survival"

        spec <- create_native_spec("xgboost", engine, fit, rec_prep)
      } else if (algo == "stratified_cox") {
        if (!requireNamespace("survival", quietly = TRUE)) {
          stop("The 'survival' package is required for stratified Cox. Please install it.")
        }
        strata_cols <- names(train_data)[grepl("^strata", names(train_data))]
        if (length(strata_cols) == 0) {
          warning("No columns starting with 'strata' were found; skipping stratified Cox.")
          next
        }
        prep_dat <- get_prepped_data()
        baked_train <- prep_dat$data
        rec_prep <- prep_dat$recipe
        strata_cols_present <- character()
        strata_base_cols <- character()
        strata_info <- list()
        for (sc in strata_cols) {
          if (!(sc %in% names(baked_train)) && sc %in% names(train_data)) {
            baked_train[[sc]] <- train_data[[sc]]
          }
          if (!(sc %in% names(baked_train))) {
            next
          }
          source_vals <- NULL
          if (sc %in% names(train_data)) {
            source_vals <- train_data[[sc]]
          } else {
            source_vals <- baked_train[[sc]]
          }
          desired_levels <- NULL
          if (is.factor(source_vals)) {
            desired_levels <- levels(source_vals)
          } else if (!is.null(source_vals)) {
            desired_levels <- sort(unique(source_vals))
          }
          if (is.null(desired_levels) || length(desired_levels) == 0) {
            alt_vals <- baked_train[[sc]]
            if (is.factor(alt_vals)) {
              desired_levels <- levels(alt_vals)
            } else {
              desired_levels <- sort(unique(alt_vals))
            }
          }
          desired_levels <- desired_levels[!is.na(desired_levels)]
          if (length(desired_levels) == 0) {
            next
          }
          baked_train[[sc]] <- factor(baked_train[[sc]], levels = desired_levels)
          strata_cols_present <- c(strata_cols_present, sc)
          base_candidate <- gsub("^strata[._]*", "", sc)
          base_candidate <- gsub("^[._]+", "", base_candidate)
          display_name <- if (nzchar(base_candidate)) base_candidate else sc
          strata_info[[sc]] <- list(
            column = sc,
            display = display_name,
            levels = as.character(desired_levels)
          )
          if (nzchar(base_candidate) && base_candidate %in% names(baked_train)) {
            strata_base_cols <- c(strata_base_cols, base_candidate)
          }
        }
        strata_cols_present <- unique(strata_cols_present)
        strata_base_cols <- unique(strata_base_cols)
        if (length(strata_cols_present) == 0) {
          warning("Unable to identify usable strata columns after preprocessing; skipping stratified Cox.")
          next
        }
        strata_dummy_cols <- character(0)
        if (length(strata_cols_present) > 0) {
          for (sc in strata_cols_present) {
            prefix_underscore <- paste0(sc, "_")
            prefix_dot <- paste0(sc, ".")
            matches <- names(baked_train)[startsWith(names(baked_train), prefix_underscore) |
                                            startsWith(names(baked_train), prefix_dot)]
            if (length(matches) > 0) {
              strata_dummy_cols <- c(strata_dummy_cols, matches)
            }
          }
          strata_dummy_cols <- unique(strata_dummy_cols)
        }
        predictor_cols <- setdiff(names(baked_train),
                                  c(response_col, strata_cols_present, strata_dummy_cols,
                                    strata_base_cols))
        rhs_terms <- c(predictor_cols, paste0("strata(", strata_cols_present, ")"))
        formula_rhs <- if (length(rhs_terms) == 0) "1" else paste(rhs_terms, collapse = " + ")
        f <- as.formula(paste(response_col, "~", formula_rhs))
        fit <- call_with_engine_params(
          survival::coxph,
          list(
            formula = f,
            data = baked_train,
            ties = "efron"
          ),
          engine_args
        )
        spec <- create_native_spec("stratified_cox", engine, fit, rec_prep,
                                   extras = list(strata_cols = strata_cols_present,
                                                 strata_dummy_cols = strata_dummy_cols,
                                                 strata_info = strata_info,
                                                 strata_base_cols = strata_base_cols))
      } else if (algo == "time_varying_cox") {
        if (length(label_cols) != 3) {
          warning("time_varying_cox requires label = c(start, stop, status). Skipping.")
          next
        }
        if (!requireNamespace("survival", quietly = TRUE)) {
          stop("The 'survival' package is required for time-varying Cox. Please install it.")
        }
        prep_dat <- get_prepped_data()
        baked_train <- prep_dat$data
        rec_prep <- prep_dat$recipe
        fit <- call_with_engine_params(
          survival::coxph,
          list(
            formula = as.formula(paste(response_col, "~ .")),
            data = baked_train,
            ties = "efron"
          ),
          engine_args
        )
        spec <- create_native_spec("time_varying_cox", engine, fit, rec_prep)
      } else if (algo == "survreg") {
        if (!requireNamespace("survival", quietly = TRUE)) {
          stop("The 'survival' package is required for survreg. Please install it.")
        }
        prep_dat <- get_prepped_data()
        baked_train <- prep_dat$data
        rec_prep <- prep_dat$recipe
        fit <- call_with_engine_params(
          survival::survreg,
          list(
            formula = as.formula(paste(response_col, "~ .")),
            data = baked_train,
            dist = "weibull"
          ),
          engine_args
        )
        spec <- create_native_spec("survreg", engine, fit, rec_prep,
                                   extras = list(distribution = "weibull"))
      } else if (algo == "parametric_surv") {
        if (!requireNamespace("flexsurv", quietly = TRUE)) {
          warning("Package 'flexsurv' not installed; skipping parametric_surv.")
          next
        }
        prep_dat <- get_prepped_data()
        baked_train <- prep_dat$data
        rec_prep <- prep_dat$recipe
        dist_arg <- engine_args$dist
        if (!is.null(dist_arg)) {
          engine_args$dist <- NULL
        }
        if (is.null(dist_arg)) {
          dist_arg <- "weibull"
        }
        dist_arg <- as.character(dist_arg)[1]
        dist_arg <- tolower(dist_arg)
        dist_map <- c(
          weibull = "weibull",
          loglogistic = "llogis",
          llogis = "llogis",
          lognormal = "lognormal",
          exponential = "exp",
          exp = "exp"
        )
        label_map <- c(
          weibull = "weibull",
          loglogistic = "loglogistic",
          llogis = "loglogistic",
          lognormal = "lognormal",
          exponential = "exponential",
          exp = "exponential"
        )
        if (!(dist_arg %in% names(dist_map))) {
          stop("Unsupported distribution for parametric_surv. Choose one of 'weibull', 'loglogistic', 'lognormal', or 'exponential'.")
        }
        dist_flex <- dist_map[[dist_arg]]
        dist_label <- label_map[[dist_arg]]
        base_args <- list(
          formula = as.formula(paste(response_col, "~ .")),
          data = baked_train,
          dist = dist_flex
        )
        fit <- fastml_fit_flexsurvreg(base_args, engine_args)
        extras <- list(
          distribution = dist_flex,
          distribution_label = dist_label
        )
        if (!is.null(time_col) && time_col %in% names(train_data)) {
          extras$train_times <- train_data[[time_col]]
        }
        if (!is.null(status_col) && status_col %in% names(train_data)) {
          extras$train_status <- train_data[[status_col]]
        }
        extras$train_size <- nrow(baked_train)
        spec <- create_native_spec(
          "parametric_surv",
          engine,
          fit,
          rec_prep,
          extras = extras
        )
      } else if (algo == "piecewise_exp") {
        if (!requireNamespace("flexsurv", quietly = TRUE)) {
          warning("Package 'flexsurv' not installed; skipping piecewise_exp.")
          next
        }

        prep_dat <- get_prepped_data()
        baked_train <- prep_dat$data
        rec_prep <- prep_dat$recipe

        breaks_val <- NULL
        if ("breaks" %in% names(engine_args)) {
          breaks_val <- engine_args$breaks
          engine_args$breaks <- NULL
        }
        if ("cuts" %in% names(engine_args) && is.null(breaks_val)) {
          breaks_val <- engine_args$cuts
          engine_args$cuts <- NULL
        }
        if ("knots" %in% names(engine_args) && is.null(breaks_val)) {
          breaks_val <- engine_args$knots
          engine_args$knots <- NULL
        }

        breaks_val <- fastml_piecewise_normalize_breaks(breaks_val)
        if (length(breaks_val) == 0) breaks_val <- NULL

        if (is.null(breaks_val)) {
          event_times <- NULL
          if (!is.null(status_col) && status_col %in% names(train_data)) {
            status_vals <- train_data[[status_col]]
            if (!is.null(time_col) && time_col %in% names(train_data)) {
              time_vals <- train_data[[time_col]]
            } else {
              time_vals <- NULL
            }
            if (!is.null(time_vals)) {
              event_idx <- which(is.finite(status_vals) & status_vals == 1)
              if (length(event_idx) > 0) {
                event_times <- as.numeric(time_vals[event_idx])
                event_times <- event_times[is.finite(event_times) & event_times > 0]
              }
            }
          }
          if (!is.null(event_times) && length(event_times) >= 2) {
            quantiles <- stats::quantile(
              event_times,
              probs = c(1 / 3, 2 / 3),
              names = FALSE,
              type = 7
            )
            breaks_val <- fastml_piecewise_normalize_breaks(quantiles)
          }
        }

        pw_spec <- fastml_piecewise_make_distribution(breaks_val)
        base_args <- list(
          formula = as.formula(paste(response_col, "~ .")),
          data = baked_train,
          dist = pw_spec$dlist,
          dfns = pw_spec$dfns
        )
        if (length(pw_spec$aux) > 0) {
          base_args$aux <- pw_spec$aux
        }

        fit <- fastml_fit_flexsurvreg(base_args, engine_args)

        extras <- list(
          distribution = "fastml_piecewise_exponential",
          distribution_label = "piecewise exponential",
          breaks = breaks_val
        )
        if (!is.null(time_col) && time_col %in% names(train_data)) {
          extras$train_times <- train_data[[time_col]]
        }
        if (!is.null(status_col) && status_col %in% names(train_data)) {
          extras$train_status <- train_data[[status_col]]
        }

        spec <- create_native_spec(
          "piecewise_exp",
          engine,
          fit,
          rec_prep,
          extras = extras
        )

        # ensure extras are attached to top-level spec
        if (inherits(spec, "fastml_native_survival")) {
          spec$extras <- extras
        }
      }
      else if (algo == "royston_parmar") {
        if (!requireNamespace("rstpm2", quietly = TRUE)) {
          warning("Package 'rstpm2' not installed; skipping royston_parmar.")
          next
        }
        prep_dat <- get_prepped_data()
        baked_train <- as.data.frame(prep_dat$data)
        rec_prep <- prep_dat$recipe
        rp_train <- baked_train
        if (!(response_col %in% names(rp_train)) && response_col %in% names(train_data)) {
          rp_train[[response_col]] <- train_data[[response_col]]
        }
        if (length(label_cols) < 2) {
          warning("royston_parmar requires time and status columns; skipping.")
          next
        }
        for (lc in label_cols) {
          if (lc %in% names(train_data)) {
            rp_train[[lc]] <- train_data[[lc]]
          }
        }
        predictor_cols <- setdiff(names(rp_train), c(response_col, label_cols))
        predictor_terms <- if (length(predictor_cols) == 0) {
          "1"
        } else {
          paste(paste0("`", predictor_cols, "`"), collapse = " + ")
        }
        label_terms <- paste0("`", label_cols, "`")
        surv_lhs <- if (length(label_cols) == 3) {
          paste0("survival::Surv(", paste(label_terms, collapse = ", "), ")")
        } else {
          paste0("survival::Surv(", paste(label_terms[1:2], collapse = ", "), ")")
        }
        rp_formula <- as.formula(paste(surv_lhs, "~", predictor_terms))
        fit <- tryCatch({
          stpm2_fn <- get("stpm2", envir = asNamespace("rstpm2"))
          base_args <- list(
            formula = rp_formula,
            data = rp_train,
            df = 3,
            penalised = FALSE
          )
          fit_obj <- call_with_engine_params(
            stpm2_fn,
            base_args,
            engine_args
          )
          fit_obj
        }, error = function(e) e)
        if (inherits(fit, "error")) {
          warning(sprintf("royston_parmar training failed: %s", fit$message))
          next
        }
        if (!inherits(fit, c("stpm2", "pstpm2"))) {
          warning("rstpm2::stpm2 did not return an 'stpm2' compatible object; skipping royston_parmar.")
          next
        }
        loglik_val <- tryCatch({
          val <- stats::logLik(fit)
          if (length(val) > 0) as.numeric(val)[1] else NA_real_
        }, error = function(e) NA_real_)
        if (!is.finite(loglik_val)) {
          loglik_val <- tryCatch({
            val <- stats::logLik(fit)
            if (length(val) > 0) as.numeric(val)[1] else NA_real_
          }, error = function(e) NA_real_)
        }
        safe_stat_val <- function(expr) {
          val <- tryCatch(expr, error = function(e) NA_real_)
          if (length(val) == 0) {
            return(NA_real_)
          }
          as.numeric(val[1])
        }
        loglik_val <- safe_stat_val(stats::logLik(fit))
        aic_val <- safe_stat_val(stats::AIC(fit))
        bic_val <- safe_stat_val(stats::BIC(fit))
        spec <- create_native_spec("royston_parmar", engine, fit, rec_prep,
                                   extras = list(
                                     spline_df = 3,
                                     loglik = loglik_val,
                                     aic = aic_val,
                                     bic = bic_val
                                   ))
      } else if (algo == "aft") {
        warning("Survival 'aft' requires censored survival model specs not available in your setup. Skipping.")
        next
      } else {
        next
      }

      if (is.null(fit_engine_args)) {
        fit_engine_args <- engine_args
      }

      if (!is.null(resamples) && inherits(spec, "fastml_native_survival")) {
        stop(
          paste(
            "Guarded resampling for survival models is not yet implemented for native engines.",
            "Please supply parsnip-compatible specifications or disable resampling."
          )
        )
      }

      if (inherits(spec, "fastml_native_survival")) {
        models[[algo]] <- spec
      } else {
        wf <- workflows::workflow() %>%
          workflows::add_recipe(recipe) %>%
          workflows::add_model(spec)
        models[[algo]] <- do.call(
          parsnip::fit,
          c(list(object = wf, data = train_data), fit_engine_args)
        )
      }
    }

    if (length(resampling_summaries) > 0) {
      attr(models, "guarded_resampling") <- resampling_summaries
    }

    return(models)
  } else if (task == "classification") {

    if(is.null(summaryFunction)){
      metrics <- metric_set(
        accuracy,
        kap,
        sens,
        spec,
        precision,
        f_meas,
        roc_auc
      )
    }else{

      newClassMetric <- new_class_metric(summaryFunction, "maximize")

      assign(metric, newClassMetric)

      metrics <- metric_set(
        accuracy,
        kap,
        sens,
        spec,
        precision,
        f_meas,
        roc_auc,
        !!sym(metric)
      )


    }
  } else {
    metrics <- metric_set(rmse, rsq, mae)
  }

  resample_plan <- NULL

  if (!is.null(resamples)) {
    if (fastml_is_resample_plan(resamples)) {
      resample_plan <- fastml_resample_validate(resamples)
    } else if (inherits(resamples, "rset")) {
      resample_plan <- fastml_new_resample_plan(
        splits = resamples,
        method = resampling_method %||% "custom",
        params = list(source = "user_provided")
      )
    } else {
      stop("'resamples' must be an 'rset' object or a fastml resampling plan")
    }
  } else if (resampling_method == "cv") {
    if (nrow(resample_data) < folds) {
      stop(
        sprintf(
          "You requested %d-fold cross-validation, but your training set only has %d rows. \nThis prevents each fold from having at least one row. \nEither reduce 'folds', increase data, or use a different resampling method (e.g. 'boot').",
          folds,
          nrow(resample_data)
        )
      )
    }
    resamples_obj <- vfold_cv(
      resample_data,
      v = folds,
      repeats = 1,
      strata = if (task == "classification")
        all_of(label)
      else
        NULL
    )
    resample_plan <- fastml_new_resample_plan(
      splits = resamples_obj,
      method = "cv",
      params = list(v = folds, repeats = 1, strata = if (task == "classification") label else NULL)
    )
  } else if (resampling_method == "boot") {
    resamples_obj <- bootstraps(resample_data,
                                times = folds,
                                strata = if (task == "classification")
                                  all_of(label)
                                else
                                  NULL)
    resample_plan <- fastml_new_resample_plan(
      splits = resamples_obj,
      method = "boot",
      params = list(times = folds, strata = if (task == "classification") label else NULL)
    )
  } else if (resampling_method == "repeatedcv") {

    if (nrow(resample_data) < folds) {
      stop(
        sprintf(
          "You requested %d-fold cross-validation, but your training set only has %d rows. \nThis prevents each fold from having at least one row. \nEither reduce 'folds', increase data, or use a different resampling method (e.g. 'boot').",
          folds, nrow(resample_data)
        )
      )
    }
    repeats_val <- if (is.null(repeats)) 1L else repeats
    resamples_obj <- vfold_cv(
      resample_data,
      v = folds,
      repeats = repeats_val,
      strata = if (task == "classification")
        all_of(label)
      else
        NULL
    )
    resample_plan <- fastml_new_resample_plan(
      splits = resamples_obj,
      method = "repeatedcv",
      params = list(v = folds, repeats = repeats_val, strata = if (task == "classification") label else NULL)
    )
  } else if (resampling_method == "grouped_cv") {
    repeats_arg <- if (is.null(repeats)) 1L else repeats
    resamples_obj <- make_grouped_cv(
      data = resample_data,
      group_cols = group_cols,
      v = folds,
      strata = if (task == "classification") label else NULL,
      repeats = repeats_arg
    )
    resample_plan <- fastml_new_resample_plan(
      splits = resamples_obj,
      method = "grouped_cv",
      params = list(
        group_cols = group_cols,
        v = folds,
        repeats = repeats_arg,
        strata = if (task == "classification") label else NULL
      )
    )
  } else if (resampling_method %in% c("blocked_cv", "rolling_origin")) {
    if (is.null(block_col) || length(block_col) != 1) {
      stop("'block_col' must be provided when using blocked or rolling resampling.")
    }

    if (resampling_method == "blocked_cv") {
      blocked_info <- make_blocked_cv(
        data = resample_data,
        block_col = block_col,
        block_size = block_size
      )

      if (!is.numeric(folds) || length(folds) != 1 || folds < 2 || folds != as.integer(folds)) {
        stop("'folds' must be an integer greater than or equal to 2 for 'blocked_cv'.")
      }

      if (folds > blocked_info$n_blocks) {
        stop("'folds' cannot exceed the number of derived blocks.")
      }

      block_group_col <- ".fastml_block"
      blocked_data <- blocked_info$data
      blocked_data[[block_group_col]] <- blocked_info$block_ids

      resamples_obj <- rsample::group_vfold_cv(
        blocked_data,
        group = !!rlang::sym(block_group_col),
        v = folds
      )

      resamples_obj$splits <- lapply(
        resamples_obj$splits,
        function(split) {
          split$data[[block_group_col]] <- NULL
          split
        }
      )

      resample_plan <- fastml_new_resample_plan(
        splits = resamples_obj,
        method = "blocked_cv",
        params = list(
          block_col = block_col,
          block_size = block_size,
          v = folds
        )
      )
    } else {
      resamples_obj <- make_rolling_origin_cv(
        data = resample_data,
        block_col = block_col,
        initial_window = initial_window,
        assess_window = assess_window,
        skip = skip
      )
      resample_plan <- fastml_new_resample_plan(
        splits = resamples_obj,
        method = "rolling_origin",
        params = list(
          block_col = block_col,
          initial_window = initial_window,
          assess_window = assess_window,
          skip = skip
        )
      )
    }
  } else if (resampling_method == "nested_cv") {
    if (is.null(outer_folds)) {
      stop("'outer_folds' must be provided when using 'nested_cv'.")
    }
    if (!is.numeric(outer_folds) || length(outer_folds) != 1 || outer_folds < 2 ||
        outer_folds != as.integer(outer_folds)) {
      stop("'outer_folds' must be an integer greater than or equal to 2.")
    }
    inner_folds <- if (is.null(folds)) 5 else folds
    if (!is.numeric(inner_folds) || length(inner_folds) != 1 || inner_folds < 2 ||
        inner_folds != as.integer(inner_folds)) {
      stop("'folds' must be an integer greater than or equal to 2 for inner resampling.")
    }
    resamples_obj <- make_nested_cv(
      data = resample_data,
      outer_folds = outer_folds,
      inner_folds = inner_folds,
      group_cols = group_cols,
      strata = if (task == "classification") label else NULL
    )
    resample_plan <- fastml_new_resample_plan(
      splits = resamples_obj,
      method = "nested_cv",
      params = list(
        outer_folds = outer_folds,
        inner_folds = inner_folds,
        group_cols = group_cols,
        strata = if (task == "classification") label else NULL
      )
    )
  } else if (resampling_method == "none") {
    resample_plan <- NULL
  }

  resamples <- if (!is.null(resample_plan)) fastml_resample_splits(resample_plan) else NULL

  if (use_default_tuning && is.null(resamples)) {
    warning("'tune_params' are ignored when 'tuning_strategy' is 'none'")
  }

  models <- list()
  resampling_summaries <- list()
  resample_method_meta <- if (!is.null(resample_plan)) {
    fastml_resample_method(resample_plan) %||% resampling_method
  } else {
    resampling_method
  }

  nested_mode <- identical(resample_method_meta, "nested_cv")
  nested_details <- list()

  # A helper function to choose the engine for an algorithm
  get_engine <- function(algo, default_engine) {
    if (!is.null(algorithm_engines) && !is.null(algorithm_engines[[algo]])) {
      return(algorithm_engines[[algo]])
    } else {
      return(default_engine)
    }
  }

  update_params <- function(params_model, new_params) {
    for (param_name in names(new_params)) {
      param_value <- new_params[[param_name]]
      param_row <- params_model %>% dplyr::filter(id == param_name)
      if (nrow(param_row) == 0) next

      param_obj <- param_row$object[[1]]

      # Helper function to update a parameter object
      try_update <- function(obj, value) {
        if (length(value) == 2) {
          if (inherits(obj, "integer_parameter")) {
            return(obj %>% dials::range_set(c(as.integer(value[1]), as.integer(value[2]))))
          } else {
            return(obj %>% dials::range_set(value))
          }
        } else {
          if (inherits(obj, "integer_parameter")) {
            return(obj %>% dials::value_set(as.integer(value)))
          } else {
            return(obj %>% dials::value_set(value))
          }
        }
      }

      updated_obj <- tryCatch({
        try_update(param_obj, param_value)
      }, error = function(e) {
        # If update fails, expand the allowed range to include the new value.
        current_lb <- attr(param_obj, "range")$lower
        current_ub <- attr(param_obj, "range")$upper

        # Ensure new value(s) are numeric
        if (length(param_value) == 1) {
          new_val <- if (inherits(param_obj, "integer_parameter")) as.integer(param_value) else param_value
        } else {
          new_val <- c(min(param_value), max(param_value))
        }

        # Compute new bounds that include the new value(s)
        if (length(new_val) == 1) {
          new_lb <- min(current_lb, new_val)
          new_ub <- max(current_ub, new_val)
        } else {
          new_lb <- min(current_lb, new_val[1])
          new_ub <- max(current_ub, new_val[2])
        }

        # Expand the range and update the parameter object
        param_obj %>% dials::range_set(c(new_lb, new_ub))
      })

      params_model$object[params_model$id == param_name] <- list(updated_obj)
    }
    return(params_model)
  }

  n_class <- length(levels(train_data[[label]]))

  for (algo in algorithms) {
    set.seed(seed)

    # Assume that get_engine() now may return multiple engine names.

    if(n_class > 2 && algo == "logistic_reg") {algo = "multinom_reg"}

    engines <- get_engine(algo, get_default_engine(algo, task))


    # Create a nested list to store models by algorithm and engine.
    models[[algo]] <- list()

    # Loop over each engine provided
    for (engine in engines) {
      engine_args <- resolve_engine_params(engine_params, algo, engine)

      # Get default parameters for this engine
      if (use_default_tuning) {
        defaults <- get_default_tune_params(
          algo,
          train_data,
          label,
          engine
        )
      } else {
        defaults <- get_default_params(
          algo,
          task,
          num_predictors = ncol(train_data %>% dplyr::select(-!!sym(label))),
          engine = engine
        )
      }

      # User supplied tuning parameters for this algorithm/engine
      user_params <- NULL
      if (!is.null(tune_params) &&
          !is.null(tune_params[[algo]]) &&
          !is.null(tune_params[[algo]][[engine]])) {
        user_params <- tune_params[[algo]][[engine]]
      }

      # Merge defaults with user parameters
      engine_tune_params <- if (is.null(defaults)) list() else defaults
      if (!is.null(user_params)) {
        for (nm in names(user_params)) {
          engine_tune_params[[nm]] <- user_params[[nm]]
        }
      }

      if (algo == "logistic_reg" && engine %in% c("glm", "gee", "glmer", "stan", "stan_glmer")) {
        perform_tuning <- FALSE
      } else {
        has_custom <- !is.null(user_params)
        perform_tuning <- (use_default_tuning || has_custom) && !is.null(resamples)
        if (tuning_strategy == "none") {
          perform_tuning <- FALSE
        }
      }

        spec_context <- list(
          task = task,
          train_data = train_data,
          label = label,
          tuning = perform_tuning,
          engine = engine,
          early_stopping = early_stopping,
          n_class = n_class
        )
        model_info <- fastml_get_model_spec(algo, spec_context)
        if (is.null(model_info)) {
          next
        }

        # Assume the model specification is stored in model_info$model_spec
        model_spec <- model_info$model_spec

      if(!is.null(model_spec)){

      # Set up tuning parameters and grid (if needed)
      tune_params_model <- NULL
      tune_params_template <- NULL
      tune_grid_values <- NULL

      if (perform_tuning) {
        param_source <- if (inherits(model_spec, "model_spec")) {
          model_spec
        } else {
          model_spec[[1]]
        }

        param_set <- extract_parameter_set_dials(param_source)

        if (nested_mode) {
          tune_params_template <- param_set
        } else {
          tune_params_model <- finalize(
            param_set,
            x = train_data %>% dplyr::select(-dplyr::all_of(label))
          )

          if (!is.null(engine_tune_params)) {
            tune_params_model <- update_params(tune_params_model, engine_tune_params)
          }

          if (nrow(tune_params_model) > 0 && tuning_strategy == "grid" && !adaptive) {
            tune_grid_values <- grid_regular(tune_params_model, levels = 3)
          }
        }
      }

      do_tuning <- perform_tuning && !all(vapply(engine_tune_params, is.null, logical(1)))

      # Create the workflow
      workflow_spec <- workflow() %>%
        add_model(if(inherits(model_spec,"model_spec")) model_spec else model_spec[[1]]) %>%
        add_recipe(recipe)

      if (!perform_tuning && !nested_mode && !is.null(resamples)) {
        res_summary <- fastml_guarded_resample_fit(
          workflow_spec = workflow_spec,
          resamples = if (!is.null(resample_plan)) resample_plan else resamples,
          original_train_rows = nrow(train_data),
          task = task,
          label = label,
          metric = metric,
          event_class = event_class,
          engine = engine,
          start_col = start_col,
          time_col = time_col,
          status_col = status_col,
          eval_times = eval_times,
          at_risk_threshold = at_risk_threshold
        )
        if (!is.null(res_summary)) {
          if (is.null(resampling_summaries[[algo]])) {
            resampling_summaries[[algo]] <- list()
          }
          resampling_summaries[[algo]][[engine]] <- res_summary
        }
      }

      # Fit the model (with tuning if requested)
      tryCatch({
        allow_par <- TRUE
        my_metrics <- NULL

        if (do_tuning) {
          if (algo == "rand_forest" && engine == "h2o") {

            roc_auc_h2o <- function(data, truth, ...) {
              # Rename probability columns from ".pred_p0"/".pred_p1" to ".pred_0"/".pred_1"
              data <- data %>%
                rename_with(~ sub("^\\.pred_p", ".pred_", .x), starts_with(".pred_p"))

              # Call the built-in roc_auc() with the renamed columns
              yardstick::roc_auc(data, truth = {{truth}}, ...)
            }

            # Assign the same metadata as roc_auc() so metric ids stay consistent
            class(roc_auc_h2o) <- class(roc_auc)
            attr(roc_auc_h2o, "direction") <- attr(yardstick::roc_auc, "direction")
            attr(roc_auc_h2o, "metric_name") <- attr(yardstick::roc_auc, "metric_name")

            my_metrics <- metric_set(accuracy, kap, sens, spec, precision, f_meas, roc_auc_h2o)

            allow_par = FALSE
          }

          else if(engine == "LiblineaR"){

            my_metrics <- metric_set(accuracy, kap, sens, spec, precision, f_meas)
            allow_par = TRUE

          }else{
            allow_par = TRUE
            my_metrics = NULL
          }
        }

        ctrl_grid <- control_grid(save_pred = TRUE, allow_par = allow_par)
        ctrl_bayes <- control_bayes(save_pred = TRUE)
        ctrl_race <- control_race(save_pred = TRUE)

        if (early_stopping && tuning_strategy == "bayes") {
          ctrl_bayes <- control_bayes(save_pred = TRUE, no_improve = 5)
        }

        if (nested_mode) {
          nested_fit <- fastml_run_nested_cv(
            workflow_spec = workflow_spec,
            nested_resamples = resamples,
            train_data = train_data,
            label = label,
            task = task,
            metric = metric,
            metrics = metrics,
            engine = engine,
            engine_args = engine_args,
            tune_params_template = if (do_tuning) tune_params_template else NULL,
            engine_tune_params = engine_tune_params,
            tuning_strategy = tuning_strategy,
            tuning_iterations = tuning_iterations,
            adaptive = adaptive,
            early_stopping = early_stopping,
            ctrl_grid = ctrl_grid,
            ctrl_bayes = ctrl_bayes,
            ctrl_race = ctrl_race,
            my_metrics = my_metrics,
            do_tuning = do_tuning,
            event_class = event_class,
            start_col = start_col,
            time_col = time_col,
            status_col = status_col,
            eval_times = eval_times,
            at_risk_threshold = at_risk_threshold,
            seed = seed,
            update_params_fn = update_params
          )

          model <- nested_fit$final_model

          if (!is.null(nested_fit$details)) {
            if (is.null(nested_details[[algo]])) {
              nested_details[[algo]] <- list()
            }
            nested_details[[algo]][[engine]] <- nested_fit$details
          }
        } else if (do_tuning) {
          # Set up control objects for tuning
          if (is.null(resamples)) {
            stop("Tuning cannot be performed without resamples.")
          }

          # Select tuning function based on strategy
          if (tuning_strategy == "bayes") {
            model_tuned <- do.call(
              tune_bayes,
              c(list(
                object = workflow_spec,
                resamples = resamples,
                param_info = tune_params_model,
                iter = tuning_iterations,
                metrics = if(!is.null(my_metrics)) my_metrics else metrics,
                control = ctrl_bayes
              ), engine_args)
            )
          } else if (adaptive) {
            model_tuned <- do.call(
              tune_race_anova,
              c(list(
                object = workflow_spec,
                resamples = resamples,
                param_info = tune_params_model,
                grid = if (is.null(tune_grid_values)) 20 else tune_grid_values,
                metrics = if(!is.null(my_metrics)) my_metrics else metrics,
                control = ctrl_race
              ), engine_args)
            )
          } else if (tuning_strategy == "grid") {
            if (is.null(tune_grid_values)) {
              tune_grid_values <- grid_regular(tune_params_model, levels = 3)
            }
            model_tuned <- do.call(
              tune::tune_grid,
              c(list(
                object = workflow_spec,
                resamples = resamples,
                grid = tune_grid_values,
                metrics = if(!is.null(my_metrics)) my_metrics else metrics,
                control = ctrl_grid
              ), engine_args)
            )
          } else {
            model_tuned <- do.call(
              tune::tune_grid,
              c(list(
                object = workflow_spec,
                resamples = resamples,
                grid = if (is.null(tune_grid_values)) 5 else tune_grid_values,
                metrics = if(!is.null(my_metrics)) my_metrics else metrics,
                control = ctrl_grid
              ), engine_args)
            )
          }

          best_params <- select_best(model_tuned, metric = metric)
          final_workflow <- finalize_workflow(workflow_spec, best_params)
          model <- do.call(
            parsnip::fit,
            c(list(object = final_workflow, data = train_data), engine_args)
          )
        } else {
          # Rebuild the workflow with engine parameters applied correctly
          if (length(engine_args) > 0) {
            model_spec_with_engine <- do.call(
              parsnip::set_engine,
              c(list(model_spec, engine = engine), engine_args)
            )
            workflow_spec <- workflows::workflow() %>%
              workflows::add_model(model_spec_with_engine) %>%
              workflows::add_recipe(recipe)
          }

          model <- parsnip::fit(workflow_spec, data = train_data)
        }
        # Save the fitted model in the nested list under the current engine
        models[[algo]][[engine]] <- model
      }, error = function(e) {
        warning(paste("Training failed for algorithm:", algo, "with engine:", engine,
                      "\nError message:", e$message))
      })

      }else{

        models[[algo]] = NULL
      }

    }  # end of loop over engines

  }

  if (length(models) == 0) {
    stop("No models were successfully trained. Please check your data and parameters.")
  }

  if (length(resampling_summaries) > 0) {
    attr(models, "guarded_resampling") <- resampling_summaries
  }

  if (nested_mode && length(nested_details) > 0) {
    attr(models, "nested_cv_results") <- nested_details
  }

  if (!is.null(resample_plan)) {
    attr(models, "resampling_plan") <- resample_plan
  }

  return(models)
}

