#' Fast Machine Learning Function
#'
#' Trains and evaluates multiple classification or regression models automatically detecting the task based on the target variable type.
#'
#' Fast Machine Learning Function
#'
#' Trains and evaluates multiple classification or regression models. The function automatically
#' detects the task based on the target variable type and can perform advanced hyperparameter tuning
#' using various tuning strategies.
#'
#' @param data A data frame containing the complete dataset. If both `train_data` and `test_data` are `NULL`, `fastml()` will split this into training and testing sets according to `test_size` and `stratify`. Defaults to `NULL`.
#' @param train_data A data frame pre-split for model training. If provided, `test_data` must also be supplied, and no internal splitting will occur. Defaults to `NULL`.
#' @param test_data A data frame pre-split for model evaluation. If provided, `train_data` must also be supplied, and no internal splitting will occur. Defaults to `NULL`.
#' @param label A string specifying the name of the target variable. For
#'   survival analysis, supply a character vector with the names of the time and
#'   status columns.
#' @param algorithms A vector of algorithm names to use. Default is \code{"all"} to run all supported algorithms.
#' @param task Character string specifying model type selection. Use "auto" to
#'   let the function detect whether the target is for classification, regression,
#'   or survival based on the data. Survival is detected when `label` is a
#'   character vector of length 2 that matches time and status columns in the data.
#'   You may also explicitly set to "classification", "regression", or "survival".
#' @param test_size A numeric value between 0 and 1 indicating the proportion of the data to use for testing. Default is \code{0.2}.
#' @param resampling_method A string specifying the resampling method for model evaluation. Default is \code{"cv"}
#'   (cross-validation) for classification/regression. Other options include \code{"none"}, \code{"boot"},
#'   \code{"repeatedcv"}, \code{"grouped_cv"}, \code{"blocked_cv"}, \code{"rolling_origin"}, and \code{"nested_cv"}.
#'   For survival tasks, resampling is supported for parsnip-compatible engines (e.g., censored/ranger, glmnet).
#'   Native survival engines (flexsurv/rstpm2/custom xgboost) ignore resampling and will error if custom resamples
#'   are supplied. When the task auto-detects survival and \code{resampling_method} is omitted, it defaults to
#'   \code{"none"} so native engines continue to run; set it explicitly to enable resampling for parsnip survival fits.
#' @param folds An integer specifying the number of folds for cross-validation. Default is \code{10} for methods containing "cv" and \code{25} otherwise.
#' @param repeats Number of times to repeat cross-validation (only applicable for methods like "repeatedcv").
#' @param group_cols Character vector naming one or more grouping columns used when
#'   \code{resampling_method = "grouped_cv"} or when grouped nested cross-validation is desired.
#'   All rows that share the same combination of values remain together in every fold. Columns must exist
#'   in the training data and cannot contain missing values.
#' @param block_col Single column name that defines the ordering variable for
#'   \code{resampling_method = "blocked_cv"} or \code{"rolling_origin"}. Data must already be sorted in
#'   ascending order by this column to avoid leakage from future observations.
#' @param block_size Positive integer specifying the block size for \code{"blocked_cv"}.
#' @param initial_window Positive integer giving the number of observations in the initial training
#'   window for \code{"rolling_origin"} resampling.
#' @param assess_window Positive integer giving the number of observations in each assessment window for
#'   \code{"rolling_origin"} resampling.
#' @param skip Non-negative integer specifying how many potential rolling windows to skip between
#'   successive resamples when \code{resampling_method = "rolling_origin"}.
#' @param outer_folds Positive integer giving the number of outer folds to use when
#'   \code{resampling_method = "nested_cv"} and no custom \code{resamples} object is supplied.
#' @param event_class A single string. Either "first" or "second" to specify which level of truth to consider as the "event". Default is "first".
#' @param exclude A character vector specifying the names of the columns to be excluded from the training process.
#' @param recipe A user-defined \code{recipe} object for custom preprocessing. If provided, internal recipe steps (imputation, encoding, scaling) are skipped.
#' @param tune_params A named list of tuning ranges for each algorithm and engine
#'   pair. Example: \code{list(rand_forest = list(ranger = list(mtry = c(1, 3))))}
#'   will override the defaults for the ranger engine. Default is \code{NULL}.
#' @param engine_params A named list of engine-level arguments to pass directly
#'   to the underlying model fitting functions. Use this for fixed settings that
#'   should apply whenever an engine is fitted (for example,
#'   \code{list(royston_parmar = list(rstpm2 = list(link = "PO")))},
#'   \code{list(cox_ph = list(survival = list(ties = "breslow")))}, or
#'   \code{list(rand_forest = list(ranger = list(importance = "impurity")))}).
#'   These arguments are distinct from \code{tune_params}, which define ranges of
#'   hyperparameters to explore during tuning. Default is an empty list.
#' @param metric The performance metric to optimize during training.
#' @param algorithm_engines A named list specifying the engine to use for each algorithm.
#' @param n_cores An integer specifying the number of CPU cores to use for parallel processing. Default is \code{1}.
#' @param stratify Logical indicating whether to use stratified sampling when splitting the data. Default is \code{TRUE} for classification and \code{FALSE} for regression.
#' @param impute_method Method for handling missing values. Options include:
#'   \describe{
#'     \item{\code{"medianImpute"}}{Impute missing values using median imputation (recipe-based).}
#'     \item{\code{"knnImpute"}}{Impute missing values using k-nearest neighbors (recipe-based).}
#'     \item{\code{"bagImpute"}}{Impute missing values using bagging (recipe-based).}
#'     \item{\code{"remove"}}{Remove rows with missing values from the data (recipe-based).}
#'     \item{\code{"error"}}{Do not perform imputation; if missing values are detected, stop execution with an error.}
#'     \item{\code{NULL}}{Equivalent to \code{"error"}. No imputation is performed, and the function will stop if missing values are present.}
#'   }
#'   All imputation occurs inside the recipe so the same trained preprocessing
#'   can be applied at prediction time. Default is \code{"error"}.
#' @param encode_categoricals Logical indicating whether to encode categorical variables. Default is \code{TRUE}.
#' @param scaling_methods Vector of scaling methods to apply. Default is \code{c("center", "scale")}.
#' @param balance_method Method to handle class imbalance. One of \code{"none"},
#'   \code{"upsample"}, or \code{"downsample"}. Applied to the training set for
#'   classification tasks. Default is \code{"none"}.
#' @param resamples Optional rsample object providing custom resampling splits.
#'   If supplied, \code{resampling_method}, \code{folds}, and \code{repeats} are
#'   ignored.
#' @param summaryFunction A custom summary function for model evaluation. Default is \code{NULL}.
#' @param use_default_tuning Logical. Tuning only runs when resamples are supplied and
#'   \code{tuning_strategy} is not \code{"none"}. If \code{TRUE} and
#'   \code{tune_params} is \code{NULL}, default grids are used; if
#'   \code{tune_params} is provided, those values override/extend defaults. When
#'   \code{FALSE} and no custom parameters are given, models are fitted once with
#'   default settings. If no resamples are available or \code{tuning_strategy =
#'   "none"}, tuning requests are ignored with a warning. Default is \code{FALSE}.
#' @param tuning_strategy A string specifying the tuning strategy. Must be one of
#'   \code{"grid"}, \code{"bayes"}, or \code{"none"}. Default is \code{"grid"}.
#'   If custom \code{tune_params} are provided while \code{tuning_strategy = "none"},
#'   they will be ignored with a warning.
#' @param tuning_iterations Number of iterations for Bayesian tuning. Ignored when
#'   \code{tuning_strategy} is not \code{"bayes"}. Validation of this argument only
#'   occurs for the Bayesian strategy. Default is \code{10}.
#' @param early_stopping Logical indicating whether to use early stopping in Bayesian tuning methods (if supported). Default is \code{FALSE}.
#' @param adaptive Logical indicating whether to use adaptive/racing methods for tuning. Default is \code{FALSE}.
#' @param learning_curve Logical. If TRUE, generate learning curves (performance vs. training size).
#' @param seed An integer value specifying the random seed for reproducibility.
#' @param verbose Logical; if TRUE, prints progress messages during the training
#'   and evaluation process.
#' @param eval_times Optional numeric vector of evaluation horizons for survival
#'   models. When \code{NULL}, defaults to the median and 75th percentile of the
#'   observed follow-up times (rounded to the dataset's time unit).
#' @param bootstrap_ci Logical indicating whether bootstrap confidence intervals
#'   should be computed for performance metrics. Applies to all task types.
#' @param bootstrap_samples Integer giving the number of bootstrap resamples to
#'   use when \code{bootstrap_ci = TRUE}. Defaults to 500.
#' @param bootstrap_seed Optional seed passed to the bootstrap procedure used to
#'   estimate confidence intervals.
#' @param at_risk_threshold Numeric value between 0 and 1 used for survival
#'   metrics to determine the last follow-up time (\eqn{t_{max}}). The maximum
#'   time is set to the largest observed time where at least this proportion of
#'   subjects remain at risk.
#' @param audit_mode Logical; if \code{TRUE}, enables runtime auditing of custom
#'   preprocessing hooks and records potentially unsafe behaviour (such as global
#'   environment access or file I/O) while flagging the run as potentially
#'   unsafe.
#' @importFrom magrittr %>%
#' @importFrom rsample initial_split training testing
#' @importFrom recipes recipe step_impute_median step_impute_knn step_impute_bag step_naomit step_dummy step_center step_scale prep bake all_numeric_predictors all_predictors all_nominal_predictors all_outcomes step_zv step_rm step_novel step_unknown
#' @importFrom dplyr filter pull rename_with mutate across where select all_of group_by sample_n ungroup
#' @importFrom rlang sym .data
#' @importFrom stats as.formula complete.cases
#' @importFrom doFuture registerDoFuture
#' @importFrom future plan multisession sequential
#' @importFrom janitor make_clean_names
#' @importFrom stringr str_detect
#' @importFrom purrr flatten
#' @return An object of class \code{fastml} containing the best model, performance metrics, and other information.
#' @examples
#' \donttest{
#' # Example 1: Using the iris dataset for binary classification (excluding 'setosa')
#' data(iris)
#' iris <- iris[iris$Species != "setosa", ]  # Binary classification
#' iris$Species <- factor(iris$Species)
#'
#' # Define a custom tuning grid for the ranger engine
#' tune <- list(
#'   rand_forest = list(
#'     ranger = list(mtry = c(1, 3))
#'   )
#' )
#'
#' # Train models with custom tuning
#' model <- fastml(
#'   data = iris,
#'   label = "Species",
#'   algorithms = "rand_forest",
#'   tune_params = tune,
#'   use_default_tuning = TRUE
#' )
#'
#' # View model summary
#' summary(model)
#'
#'
#'   }
#'
#' @export
fastml <- function(data = NULL,
                   train_data = NULL,
                   test_data = NULL,
                   label,
                   algorithms = "all",
                   task = "auto",
                   test_size = 0.2,
                   resampling_method = if (identical(task, "survival")) "none" else "cv",
                   folds = ifelse(grepl("cv", resampling_method), 10, 25),
                   repeats = NULL,
                   group_cols = NULL,
                   block_col = NULL,
                   block_size = NULL,
                   initial_window = NULL,
                   assess_window = NULL,
                   skip = 0,
                   outer_folds = NULL,
                   event_class = "first",
                   exclude = NULL,
                   recipe = NULL,
                   tune_params = NULL,
                   engine_params = list(),
                   metric = NULL,
                   algorithm_engines = NULL,
                   n_cores = 1,
                   stratify = TRUE,
                   impute_method = "error",
                   encode_categoricals = TRUE,
                   scaling_methods = c("center", "scale"),
                   balance_method = "none",
                   resamples = NULL,
                   summaryFunction = NULL,
                   use_default_tuning = FALSE,
                   tuning_strategy = "grid",
                   tuning_iterations = 10,
                   early_stopping = FALSE,
                   adaptive = FALSE,
                   learning_curve = FALSE,
                   seed = 123,
                   verbose = FALSE,
                   eval_times = NULL,
                   bootstrap_ci = TRUE,
                   bootstrap_samples = 500,
                   bootstrap_seed = NULL,
                   at_risk_threshold = 0.1,
                   audit_mode = FALSE) {

  resampling_method_missing <- missing(resampling_method)
  audit_env <- fastml_init_audit_env(audit_mode)

  set.seed(seed)

  task <- match.arg(task, c("auto", "classification", "regression", "survival"))
  tuning_strategy <- match.arg(tuning_strategy, c("grid", "bayes", "none"))
  if (is.null(resampling_method)) {
    resampling_method <- "none"
  } else {
    resampling_method <- tolower(resampling_method)
  }
  if (task == "survival" && resampling_method_missing && !identical(resampling_method, "none")) {
    resampling_method <- "none"
  }
  custom_resamples <- !is.null(resamples)

  unsupported_imputers <- c("mice", "missforest", "custom")
  if (!is.null(impute_method) &&
      is.character(impute_method) &&
      tolower(impute_method) %in% unsupported_imputers) {
    stop(
      paste0(
        "Advanced imputation methods ('mice', 'missForest', 'custom') are no longer supported. ",
        "For leak-free predictions, specify a recipe-based imputation step such as ",
        "'medianImpute', 'knnImpute', 'bagImpute', or use `impute_method = \"remove\"`."
      ),
      call. = FALSE
    )
  }

  coerce_count <- function(value, name, allow_zero = FALSE) {
    if (is.null(value)) {
      return(NULL)
    }
    if (!is.numeric(value) || length(value) != 1 || is.na(value)) {
      stop(
        sprintf(
          "'%s' must be a %s integer.",
          name,
          if (allow_zero) "non-negative" else "positive"
        ),
        call. = FALSE
      )
    }
    if (!isTRUE(all.equal(value, round(value)))) {
      stop(
        sprintf(
          "'%s' must be supplied as a whole number.",
          name
        ),
        call. = FALSE
      )
    }
    if (!allow_zero && value <= 0) {
      stop(sprintf("'%s' must be a positive integer.", name), call. = FALSE)
    }
    if (allow_zero && value < 0) {
      stop(sprintf("'%s' must be a non-negative integer.", name), call. = FALSE)
    }
    as.integer(round(value))
  }

  ensure_columns_present <- function(df, cols, arg_label) {
    if (is.null(cols) || length(cols) == 0) {
      return(invisible(NULL))
    }
    missing_cols <- setdiff(cols, colnames(df))
    if (length(missing_cols) > 0) {
      stop(
        sprintf(
          "Column(s) %s required by %s were not found in the training data.",
          paste(missing_cols, collapse = ", "),
          arg_label
        ),
        call. = FALSE
      )
    }
    invisible(NULL)
  }

  if (!is.null(group_cols)) {
    group_cols <- unique(as.character(group_cols))
    group_cols <- group_cols[nzchar(group_cols)]
    if (length(group_cols) == 0) {
      group_cols <- NULL
    }
  }

  if (!is.null(block_col)) {
    block_col <- as.character(block_col)
    block_col <- block_col[nzchar(block_col)]
    if (length(block_col) == 0) {
      block_col <- NULL
    } else if (length(block_col) > 1) {
      stop("'block_col' must be a single column name.", call. = FALSE)
    } else {
      block_col <- block_col[1]
    }
  }

  if (!is.null(repeats) && length(repeats) == 1 && is.na(repeats)) {
    repeats <- NULL
  }
  repeats <- coerce_count(repeats, "repeats")
  block_size <- coerce_count(block_size, "block_size")
  initial_window <- coerce_count(initial_window, "initial_window")
  assess_window <- coerce_count(assess_window, "assess_window")
  outer_folds <- coerce_count(outer_folds, "outer_folds")
  skip <- coerce_count(skip, "skip", allow_zero = TRUE)
  if (is.null(skip)) {
    skip <- 0L
  }

  if (!custom_resamples && identical(resampling_method, "grouped_cv") && is.null(group_cols)) {
    stop("`group_cols` must be provided when `resampling_method = \"grouped_cv\"`.", call. = FALSE)
  }
  if (!custom_resamples && resampling_method %in% c("blocked_cv", "rolling_origin") && is.null(block_col)) {
    stop("`block_col` must be provided for blocked or rolling resampling.", call. = FALSE)
  }
  if (!custom_resamples && identical(resampling_method, "blocked_cv") && is.null(block_size)) {
    stop("`block_size` must be provided when `resampling_method = \"blocked_cv\"`.", call. = FALSE)
  }
  if (!custom_resamples && identical(resampling_method, "rolling_origin") &&
      (is.null(initial_window) || is.null(assess_window))) {
    stop("`initial_window` and `assess_window` must be provided when `resampling_method = \"rolling_origin\"`.", call. = FALSE)
  }
  if (!custom_resamples && identical(resampling_method, "nested_cv") && is.null(outer_folds)) {
    stop("`outer_folds` must be provided when `resampling_method = \"nested_cv\"` unless custom resamples are supplied.", call. = FALSE)
  }
  if (is.null(repeats) && identical(resampling_method, "repeatedcv") && !custom_resamples) {
    repeats <- 1L
  }

  # If explicit train/test provided, ensure both are given
  if (!is.null(train_data) || !is.null(test_data)) {
    if (is.null(train_data) || is.null(test_data)) {
      stop("Both 'train_data' and 'test_data' must be provided together.")
    }
    # use provided splits; require label in both
    data <- NULL  # skip full-data splitting
  } else {
    # require data for splitting
    if (is.null(data)) stop("Either 'data' or both 'train_data' and 'test_data' must be provided.")
  }


  # Determine source for target variable and track whether task detection is pending
  source_data <- if (!is.null(data)) data else train_data
  target_var <- NULL
  pending_auto_detection <- FALSE

  # Auto-detect task if requested, including survival when label has two columns
  if (task == "auto") {
    # survival detection: label is c(time_col, status_col)
    if (is.character(label) && length(label) %in% c(2, 3)) {
      if (!all(label %in% names(source_data))) {
        missing_vars <- setdiff(label, names(source_data))
        stop(paste0(
          "When task='auto' and 'label' has length ",
          length(label),
          ", the specified columns must exist in the data for survival detection. Missing: ",
          paste(missing_vars, collapse = ", ")
        ))
      }
      time_cols <- if (length(label) == 3) label[1:2] else label[1]
      status_col <- label[length(label)]
      time_ok <- all(vapply(time_cols, function(col) is.numeric(source_data[[col]]), logical(1)))
      status_vec <- source_data[[status_col]]
      uniq_status <- unique(status_vec)
      status_ok <- is.logical(status_vec) || length(uniq_status) == 2
      if (time_ok && status_ok) {
        task <- "survival"
        target_var <- NULL
      } else {
        stop("Unable to detect survival task automatically: ensure time/start/stop are numeric and status has two unique values.")
      }
    } else {
      if (!(label %in% names(source_data))) {
        stop("Label variable must exist in the data source.")
      }
      pending_auto_detection <- TRUE
    }
  } else {
    # Non-auto: validate label(s) exist and set target_var when applicable
    if (task == "survival") {
      if (!(length(label) %in% c(2, 3)) || !all(label %in% names(source_data))) {
        stop("For survival tasks, 'label' must contain the time/status columns present in the data (length 2 or 3).")
      }
      target_var <- NULL
    } else {
      if (!(label %in% names(source_data))) {
        stop("Label variable must exist in the data source.")
      }
    }
  }

  # Early check: label column must not contain missing values for non-survival tasks
  if (!is.null(label) &&
      length(label) == 1 &&
      task != "survival" &&
      label %in% names(source_data)) {
    missing_targets <- sum(is.na(source_data[[label]]))
    if (missing_targets > 0) {
      stop(
        sprintf(
          "Error: The label variable '%s' contains %d missing values (NA).
          Rows with missing targets cannot be used for training.
          Please filter these rows out before running fastml.",
          label,
          missing_targets
        )
      )
    }
  }

  # determine positive_class after data split when factor levels are available
  positive_class <- NULL
  # ---------------- END TASK DETECTION ----------------


  provisional_split_used <- FALSE
  # If initial data provided, perform exclusion and checks, then split
  if (!is.null(data)) {
    if (task == "survival") {
      if (!all(label %in% names(data))) {
        stop("The specified label(s) do not exist in the data.")
      }
    } else {
      if (!(label %in% names(data))) {
        stop("The specified label does not exist in the data.")
      }
    }
    if (!is.null(exclude)) {
      if (task == "survival") {
        if (any(label %in% exclude)) {
          stop("Label variable(s) cannot be excluded: ", paste(label[label %in% exclude], collapse = ", "))
        }
      } else {
        if (label %in% exclude) stop("Label variable cannot be excluded: ", label)
      }
      missing_vars <- setdiff(exclude, colnames(data))
      if (length(missing_vars) > 0) {
        warning("Variables not in data: ", paste(missing_vars, collapse = ", "))
        exclude <- setdiff(exclude, missing_vars)
      }
      data <- dplyr::select(data, -dplyr::all_of(exclude))
    }
    if (!is.null(impute_method) && impute_method == "error" && anyNA(data)) {
      stop("Data contains NAs and 'impute_method = \"error\"'. Handle missing values first.")
    }
    data <- data %>%
      dplyr::mutate(
        dplyr::across(where(is.character), as.factor),
        dplyr::across(where(is.integer), as.numeric)
      )

    if (verbose) message("Splitting data into training and test sets...")
    # Split into train/test
    if (pending_auto_detection && task == "auto") {
      split <- rsample::initial_split(data, prop = 1 - test_size)
      provisional_split_used <- TRUE
    } else if (stratify && task == "classification") {
      split <- rsample::initial_split(
        data,
        prop = 1 - test_size,
        strata = dplyr::all_of(label)
      )
    } else {
      split <- rsample::initial_split(data, prop = 1 - test_size)
    }

    train_data <- rsample::training(split)
    test_data  <- rsample::testing(split)
  }

  if (task == "survival") {
    if (!(length(label) %in% c(2, 3))) {
      stop("For survival tasks, 'label' must contain the time/status columns present in the data (length 2 or 3).")
    }
    outcome_cols_check <- if (length(label) == 2) label else label[1:3]
    drop_missing_outcomes <- function(df, which_set) {
      missing_idx <- !stats::complete.cases(df[, outcome_cols_check, drop = FALSE])
      if (any(missing_idx)) {
        warning(
          sprintf(
            "Removed %d row(s) with missing survival outcomes from the %s set.",
            sum(missing_idx),
            which_set
          ),
          call. = FALSE
        )
        df <- df[!missing_idx, , drop = FALSE]
      }
      df
    }
    train_data <- drop_missing_outcomes(train_data, "training")
    test_data  <- drop_missing_outcomes(test_data, "test")
    if (nrow(train_data) == 0 || nrow(test_data) == 0) {
      stop("No data remain after removing rows with missing survival outcomes.")
    }
    label_surv <- label
    status_warning_emitted <- FALSE
    normalize_status <- function(vec, reference_length) {
      res <- fastml_normalize_survival_status(vec, reference_length)
      if (res$recoded && !status_warning_emitted) {
        warning("Detected non-standard survival status coding; recoding to 0 = censored and 1 = event.", call. = FALSE)
        status_warning_emitted <<- TRUE
      }
      res$status
    }
    if (length(label) == 2) {
      start_col = NULL
      time_col <- label[1]
      status_col <- label[2]
      train_status <- normalize_status(train_data[[status_col]], nrow(train_data))
      test_status  <- normalize_status(test_data[[status_col]], nrow(test_data))
      train_data[[status_col]] <- train_status
      test_data[[status_col]]  <- test_status
      surv_train <- survival::Surv(train_data[[time_col]], train_status)
      surv_test  <- survival::Surv(test_data[[time_col]], test_status)
    } else {
      start_col <- label[1]
      stop_col <- label[2]
      status_col <- label[3]
      train_status <- normalize_status(train_data[[status_col]], nrow(train_data))
      test_status  <- normalize_status(test_data[[status_col]], nrow(test_data))
      train_data[[status_col]] <- train_status
      test_data[[status_col]]  <- test_status
      surv_train <- survival::Surv(train_data[[start_col]], train_data[[stop_col]], train_status)
      surv_test  <- survival::Surv(test_data[[start_col]], test_data[[stop_col]], test_status)
    }
    attr(surv_train, "fastml_label_cols") <- label_surv
    attr(surv_test, "fastml_label_cols") <- label_surv
    train_data$surv_obj <- surv_train
    test_data$surv_obj <- surv_test
    label <- "surv_obj"
  } else {
    label_surv <- label
  }


  if (task != "survival") {
    target_var <- train_data[[label]]
  }

  if (pending_auto_detection && task != "survival") {
    if (is.numeric(target_var) && length(unique(target_var)) <= 5) {
      train_data[[label]] <- factor(train_data[[label]])
      test_data[[label]]  <- factor(test_data[[label]], levels = levels(train_data[[label]]))
      task <- "classification"
      warning(sprintf(
        "The target variable '%s' is numeric with %d unique values. Converted to factor; task set to 'classification'.",
        label, length(unique(target_var))
      ))
    } else if (is.factor(target_var) || is.character(target_var) || is.logical(target_var)) {
      task <- "classification"
    } else if (is.numeric(target_var)) {
      task <- "regression"
    } else {
      stop("Unable to detect task type. The target variable must be numeric, factor, character, or logical.")
    }
    pending_auto_detection <- FALSE
  }

  if (provisional_split_used && stratify && task == "classification") {
    set.seed(seed)
    split <- rsample::initial_split(
      data,
      prop = 1 - test_size,
      strata = dplyr::all_of(label)
    )
    train_data <- rsample::training(split)
    test_data  <- rsample::testing(split)
    target_var <- train_data[[label]]
  }

  if (task == "classification" && balance_method != "none") {
    label_sym <- rlang::sym(label)
    class_counts <- table(train_data[[label]])
    if (balance_method == "upsample") {
      max_n <- max(class_counts)
      train_data <- train_data %>%
        dplyr::group_by(!!label_sym) %>%
        dplyr::sample_n(max_n, replace = TRUE) %>%
        dplyr::ungroup()
    } else if (balance_method == "downsample") {
      min_n <- min(class_counts)
      train_data <- train_data %>%
        dplyr::group_by(!!label_sym) %>%
        dplyr::sample_n(min_n, replace = FALSE) %>%
        dplyr::ungroup()
    }
  }

  # Set default metric now that task has been resolved and validate it
  if (is.null(metric)) {
    metric <- if (task == "classification") {
      "accuracy"
    } else if (task == "regression") {
      "rmse"
    } else {
      "ibs"
    }
  }

  allowed_metrics_classification <- c("accuracy", "kap", "sens", "spec", "precision", "f_meas", "roc_auc")
  allowed_metrics_regression <- c("rmse", "rsq", "mae")
  allowed_metrics_survival <- c("c_index", "uno_c", "ibs", "rmst_diff")

  if (task == "classification") {
    if (!(metric %in% allowed_metrics_classification) && is.null(summaryFunction)) {
      stop(paste0("Invalid metric for classification task. Choose one of: ",
                  paste(allowed_metrics_classification, collapse = ", "), "."))
    }
  } else if (task == "regression") {
    if (!(metric %in% allowed_metrics_regression) && is.null(summaryFunction)) {
      stop(paste0("Invalid metric for regression task. Choose one of: ",
                  paste(allowed_metrics_regression, collapse = ", "), "."))
    }
  } else {
    if (!(metric %in% allowed_metrics_survival || grepl("^brier_t", metric))) {
      stop(paste0("Invalid metric for survival task. Choose one of: ",
                  paste(c(allowed_metrics_survival, "brier_t*"), collapse = ", "), "."))
    }
  }

  supported_algorithms <- availableMethods(type = task)

  if ("all" %in% algorithms) {
    algorithms <- supported_algorithms
  } else {
    if (length(intersect(algorithms, supported_algorithms)) == 0) {
      stop("No valid algorithms specified.")
    }
    invalid_algos <- setdiff(algorithms, supported_algorithms)
    if (length(invalid_algos) > 0) {
      warning("Invalid algorithm(s) specified: ", paste(invalid_algos, collapse = ", "))
      algorithms <- intersect(algorithms, supported_algorithms)
    }
  }

  if (task == "survival") {
    warned_xgb <- FALSE
    warned_rf <- FALSE
    for (i in seq_along(algorithms)) {
      alg <- algorithms[[i]]
      if (identical(alg, "xgboost")) {
        if (!warned_xgb) {
          warning("Survival 'xgboost' is AFT-only; prefer 'xgboost_aft' (alias added).")
          warned_xgb <- TRUE
        }
        algorithms[[i]] <- "xgboost_aft"
      } else if (identical(alg, "rand_forest")) {
        if (!warned_rf) {
          warning("Survival 'rand_forest' uses censored RF engines; prefer 'rand_forest_survival' (alias added).")
          warned_rf <- TRUE
        }
        algorithms[[i]] <- "rand_forest_survival"
      }
    }
  }


  if(!is.null(impute_method) && impute_method == "remove") {
    if (anyNA(train_data)) {
      train_data <- train_data %>%
        filter(complete.cases(.))

      warning("Rows with missing values have been removed from the training set.")

    }

    if (anyNA(test_data)) {
      test_data <- test_data %>%
        filter(complete.cases(.))

      warning("Rows with missing values have been removed from the test set.")

    }

  }



  if (task == "classification") {
    train_data[[label]] <- as.factor(train_data[[label]])
    test_data[[label]] <- factor(test_data[[label]], levels = levels(train_data[[label]]))

    n_classes <- length(levels(train_data[[label]]))
    if (n_classes == 2 && "multinom_reg" %in% algorithms) {
      warning(
        sprintf(
          "Multinomial regression ('multinom_reg') is not applicable to two-class outcomes. Detected a binary outcome for '%s'; using logistic regression ('logistic_reg') instead.",
          label
        ),
        call. = FALSE
      )

      algorithms[algorithms == "multinom_reg"] <- "logistic_reg"
      algorithms <- algorithms[!duplicated(algorithms)]

      if (!is.null(algorithm_engines) &&
          !is.null(algorithm_engines[["multinom_reg"]]) &&
          is.null(algorithm_engines[["logistic_reg"]])) {
        allowed_logistic_engines <- c(
          "glm",
          "gee",
          "glmer",
          "stan",
          "stan_glmer",
          "brulee",
          "glmnet",
          "h2o",
          "LiblineaR",
          "spark",
          "keras"
        )
        candidate_engines <- intersect(
          as.character(algorithm_engines[["multinom_reg"]]),
          allowed_logistic_engines
        )
        if (length(candidate_engines) > 0) {
          algorithm_engines[["logistic_reg"]] <- candidate_engines
        }
      }

      if (!is.null(tune_params) &&
          !is.null(tune_params[["multinom_reg"]]) &&
          is.null(tune_params[["logistic_reg"]])) {
        tune_params[["logistic_reg"]] <- tune_params[["multinom_reg"]]
      }

      if (!is.null(engine_params) &&
          !is.null(engine_params[["multinom_reg"]]) &&
          is.null(engine_params[["logistic_reg"]])) {
        engine_params[["logistic_reg"]] <- engine_params[["multinom_reg"]]
      }
    }

    if (n_classes >= 2) {
      positive_class <- ifelse(event_class == "first",
                               levels(train_data[[label]])[1],
                               levels(train_data[[label]])[2])
    }
  } else if (task == "regression") {
    train_data[[label]] <- as.numeric(train_data[[label]])
    test_data[[label]] <- as.numeric(test_data[[label]])
    positive_class <- NULL
  } else {
    positive_class <- NULL
  }

  skip_recipe_imputation <- FALSE
  # If user has provided a custom recipe, skip internal imputation logic
  if (!is.null(recipe)) {
    fastml_validate_user_recipe(recipe, audit_env)
    skip_recipe_imputation <- TRUE
  } else {
    skip_recipe_imputation <- FALSE
  }

  if (is.null(recipe) && task == "classification" && "discrim_quad" %in% algorithms) {
    predictor_cols <- setdiff(names(train_data), label)
    numeric_predictors <- predictor_cols[vapply(train_data[predictor_cols], is.numeric, logical(1))]

    if (length(numeric_predictors) > 0) {
      zero_var_by_class <- vapply(numeric_predictors, function(col) {
        values <- train_data[[col]]
        split_vals <- split(values, train_data[[label]])
        any(vapply(split_vals, function(class_values) {
          class_values <- class_values[!is.na(class_values)]
          length(class_values) <= 1 || isTRUE(all.equal(stats::var(class_values), 0))
        }, logical(1)))
      }, logical(1))

      cols_to_drop <- names(zero_var_by_class)[zero_var_by_class]

      if (length(cols_to_drop) > 0) {
        warning(
          sprintf(
            "Removed %d predictor%s with zero variance within at least one class to stabilize Quadratic Discriminant Analysis: %s.",
            length(cols_to_drop),
            ifelse(length(cols_to_drop) == 1, "", "s"),
            paste(cols_to_drop, collapse = ", ")
          )
        )

        train_data <- dplyr::select(train_data, -dplyr::all_of(cols_to_drop))
        test_data <- dplyr::select(test_data, -dplyr::all_of(cols_to_drop))
      }
    }
  }

  reference_resample_data <- train_data
  ensure_columns_present(reference_resample_data, group_cols, "`group_cols`")
  ensure_columns_present(reference_resample_data, block_col, "`block_col`")
  ###############################################################################

  # Set up parallel processing using future
  if (n_cores > 1) {
    if (!requireNamespace("doFuture", quietly = TRUE)) {
      stop("The 'doFuture' package is required for parallel processing but is not installed.")
    }
    if (!requireNamespace("future", quietly = TRUE)) {
      stop("The 'future' package is required but is not installed.")
    }
    registerDoFuture()
    plan(multisession, workers = n_cores)
    on.exit(plan(sequential), add = TRUE)
  } else {
    if (!requireNamespace("future", quietly = TRUE)) {
      stop("The 'future' package is required but is not installed.")
    }
    plan(sequential)
  }

  ########################################################################
  # Build or use recipe if user hasn't provided it
  ########################################################################
  if (is.null(recipe)) {
    if (verbose) message("Creating preprocessing recipe...")
    # Build a default recipe, possibly skipping recipe-based imputation if advanced used
    recipe <- recipe(as.formula(paste(label, "~ .")), data = train_data)
    if (task == "survival") {
      recipe <- recipe %>% step_rm(all_of(label_surv))
    }

    # Remove zero-variance predictors before any additional transformations
    recipe <- recipe %>%
      step_zv(all_predictors())

    # Apply recipe-based imputation steps only if skip_recipe_imputation = FALSE
    if (!skip_recipe_imputation) {
      if (!is.null(impute_method) && impute_method == "medianImpute") {
        recipe <- recipe %>% step_impute_median(all_numeric_predictors())

        warning("Missing values in numeric predictors are being imputed using the median.")

      } else if (!is.null(impute_method) && impute_method == "knnImpute") {
        recipe <- recipe %>% step_impute_knn(all_predictors())

        warning("Missing values are being imputed using KNN (k-Nearest Neighbors).")

      } else if (!is.null(impute_method) && impute_method == "bagImpute") {
        recipe <- recipe %>% step_impute_bag(all_predictors())

        warning("Missing values are being imputed using bagging (bootstrap aggregation)")


      } else if (!is.null(impute_method) && impute_method == "remove") {
        recipe <- recipe %>% step_naomit(all_predictors(), skip = TRUE)

        warning("Rows with missing values in predictors are being removed.")

      } else if (impute_method == "error" || is.null(impute_method)) {
        # We'll detect if there's still NA after prep/bake, then stop
        # so do nothing special here
      } else {
        # If it's a leftover method not recognized
        stop("Invalid impute_method specified.")
      }
    } else {
      # skip_recipe_imputation = TRUE means we are using advanced or custom
      # so if "error" was set but we do have NAs after that, it won't be handled here
      # do nothing in recipe for imputation
    }

    # If encoding needed
    if (encode_categoricals) {
      recipe <- recipe %>%
        step_novel(all_nominal_predictors(), -all_outcomes()) %>%
        step_unknown(all_nominal_predictors(), -all_outcomes()) %>%
        step_dummy(all_nominal_predictors(), -all_outcomes())

      # Encoding can introduce zero-variance predictors (e.g., unused levels),
      # which would otherwise trigger warnings during subsequent scaling steps.
      recipe <- recipe %>% step_zv(all_predictors())
    }

    # scaling
    if (!is.null(scaling_methods)) {
      if ("center" %in% scaling_methods) {
        recipe <- recipe %>% step_center(all_numeric_predictors())
      }
      if ("scale" %in% scaling_methods) {
        recipe <- recipe %>% step_scale(all_numeric_predictors())
      }
    }

    # do not prep yet
  }

  if (verbose) message("Training models: ", paste(algorithms, collapse = ", "))

  models <- train_models(
    train_data = train_data,
    label = label,
    task = task,
    algorithms = algorithms,
    resampling_method = resampling_method,
    folds = folds,
    repeats = repeats,
    group_cols = group_cols,
    block_col = block_col,
    block_size = block_size,
    initial_window = initial_window,
    assess_window = assess_window,
    skip = skip,
    outer_folds = outer_folds,
    resamples = resamples,
    tune_params = tune_params,
    engine_params = engine_params,
    metric = metric,
    summaryFunction = summaryFunction,
    seed = seed,
    recipe = recipe,
    use_default_tuning = use_default_tuning,
    tuning_strategy = tuning_strategy,
    tuning_iterations = tuning_iterations,
    early_stopping = early_stopping,
    adaptive = adaptive,
    algorithm_engines = algorithm_engines,
    event_class = event_class,
    start_col = start_col,
    time_col = time_col,
    status_col = status_col,
    eval_times = eval_times,
    at_risk_threshold = at_risk_threshold,
    audit_env = audit_env
  )

  resampling_results <- attr(models, "guarded_resampling")
  attr(models, "guarded_resampling") <- NULL

  nested_results <- attr(models, "nested_cv_results")
  attr(models, "nested_cv_results") <- NULL

  resampling_plan <- attr(models, "resampling_plan")
  attr(models, "resampling_plan") <- NULL

  models <- models[sapply(models, function(x) length(x) > 0)]

  engine_names <- get_engine_names(models)

  if (length(models) == 0) {
    stop("No models were successfully trained.")
  }

  if (verbose) message("Evaluating models...")
  eval_output <- fastml_compute_holdout_results(models,
                                               train_data,
                                               test_data,
                                               label_surv,
                                               start_col,
                                               time_col,
                                               status_col,
                                               task,
                                               metric,
                                               event_class,
                                               eval_times = eval_times,
                                               bootstrap_ci = bootstrap_ci,
                                               bootstrap_samples = bootstrap_samples,
                                               bootstrap_seed = bootstrap_seed,
                                               at_risk_threshold = at_risk_threshold)
  performance <- eval_output$performance
  predictions <- eval_output$predictions

  # metric_values <- sapply(performance, function(x) x %>% filter(.metric == metric) %>% pull(.estimate))

  # Build a normalized performance list and align model names as "algorithm (engine)"
  combined_performance  <- list()
  model_map <- list()
  display_algo <- function(algo, task) {
    if (task == "survival") {
      if (algo %in% c("xgboost", "xgboost_aft")) return("xgboost (AFT)")
      if (algo %in% c("rand_forest", "rand_forest_survival")) return("rand_forest (censored)")
    }
    algo
  }
  for (alg in names(performance)) {
    perf_alg <- performance[[alg]]
    # Case 1: nested by engine (list of tibbles)
    if (is.list(perf_alg) && !inherits(perf_alg, "data.frame")) {
      for (eng in names(perf_alg)) {
        combined_name <- paste0(display_algo(alg, task), " (", eng, ")")
        combined_performance[[combined_name]] <- perf_alg[[eng]]
        # Map corresponding model
        if (is.list(models[[alg]]) && !inherits(models[[alg]], "workflow") && !inherits(models[[alg]], "tune_results")) {
          model_map[[combined_name]] <- models[[alg]][[eng]]
        } else {
          model_map[[combined_name]] <- models[[alg]]
        }
      }
    } else if (inherits(perf_alg, "data.frame")) {
      # Case 2: single workflow (tibble). Infer engine name when possible
      eng_candidates <- tryCatch(engine_names[[alg]], error = function(e) NULL)
      eng <- if (!is.null(eng_candidates) && length(eng_candidates) >= 1 && !is.na(eng_candidates[1])) {
        eng_candidates[1]
      } else {
        tryCatch(get_default_engine(alg, task), error = function(e) "unknown")
      }
      combined_name <- paste0(display_algo(alg, task), " (", eng, ")")
      combined_performance[[combined_name]] <- perf_alg
      model_map[[combined_name]] <- models[[alg]]
    }
  }

  survival_brier_times <- NULL
  survival_t_max <- NULL
  if (task == "survival") {
    for (entry in combined_performance) {
      if (is.data.frame(entry)) {
        times_attr <- attr(entry, "brier_times")
        tmax_attr <- attr(entry, "t_max")
        if (!is.null(times_attr) || !is.null(tmax_attr)) {
          survival_brier_times <- times_attr
          survival_t_max <- tmax_attr
          break
        }
      }
    }
  }

  # Replace models with normalized, consistently named map
  models <- model_map

  if (!is.null(resampling_results)) {
    resampling_named <- list()
    for (alg in names(resampling_results)) {
      alg_entry <- resampling_results[[alg]]
      if (is.list(alg_entry) && !inherits(alg_entry, "data.frame")) {
        for (eng in names(alg_entry)) {
          combined_name <- paste0(display_algo(alg, task), " (", eng, ")")
          resampling_named[[combined_name]] <- alg_entry[[eng]]
        }
      } else if (!is.null(alg_entry)) {
        eng_candidates <- tryCatch(engine_names[[alg]], error = function(e) NULL)
        eng <- if (!is.null(eng_candidates) && length(eng_candidates) >= 1 && !is.na(eng_candidates[1])) {
          eng_candidates[1]
        } else {
          tryCatch(get_default_engine(alg, task), error = function(e) "unknown")
        }
        combined_name <- paste0(display_algo(alg, task), " (", eng, ")")
        resampling_named[[combined_name]] <- alg_entry
      }
    }
    resampling_results <- resampling_named
  }

  if (!is.null(nested_results)) {
    nested_named <- list()
    for (alg in names(nested_results)) {
      alg_entry <- nested_results[[alg]]
      if (is.list(alg_entry) && !inherits(alg_entry, "data.frame")) {
        for (eng in names(alg_entry)) {
          combined_name <- paste0(alg, " (", eng, ")")
          nested_named[[combined_name]] <- alg_entry[[eng]]
        }
      } else if (!is.null(alg_entry)) {
        eng_candidates <- tryCatch(engine_names[[alg]], error = function(e) NULL)
        eng <- if (!is.null(eng_candidates) && length(eng_candidates) >= 1 && !is.na(eng_candidates[1])) {
          eng_candidates[1]
        } else {
          tryCatch(get_default_engine(alg, task), error = function(e) "unknown")
        }
        combined_name <- paste0(alg, " (", eng, ")")
        nested_named[[combined_name]] <- alg_entry
      }
    }
    nested_results <- nested_named
  }


  # Now apply the function over the flattened list
  lower_is_better_metrics <- c("rmse", "mae", "ibs", "logloss", "mse", "brier_score")
  extract_metric_values <- function(metric_name) {
    sapply(combined_performance, function(x) {
      if (is.data.frame(x)) {
        vals <- x[x$.metric == metric_name, ".estimate", drop = TRUE]
        if (length(vals) == 0) {
          NA_real_
        } else {
          as.numeric(vals[1])
        }
      } else {
        NA_real_
      }
    })
  }

  compute_lower_is_better <- function(metric_name) {
    if (is.null(metric_name) || length(metric_name) == 0 || is.na(metric_name)) {
      return(FALSE)
    }
    direction <- tryCatch({
      if (requireNamespace("yardstick", quietly = TRUE) &&
          exists(metric_name, envir = asNamespace("yardstick"), inherits = FALSE)) {
        attr(get(metric_name, envir = asNamespace("yardstick")), "direction", exact = TRUE)
      } else {
        NA_character_
      }
    }, error = function(e) NA_character_)

    if (!is.na(direction) && !is.null(direction)) {
      identical(direction, "minimize")
    } else {
      metric_name %in% lower_is_better_metrics ||
        grepl("^brier_t", metric_name) ||
        grepl("brier", metric_name, fixed = TRUE)
    }
  }

  metric_values <- extract_metric_values(metric)
  lower_is_better <- compute_lower_is_better(metric)

  if (!any(is.finite(metric_values))) {
    available_metrics <- unique(unlist(lapply(combined_performance, function(x) {
      if (is.data.frame(x)) {
        as.character(x$.metric)
      } else {
        character(0)
      }
    })))

    fallback_priority <- if (task == "classification") {
      c("roc_auc", "accuracy", "kap", "sens", "spec", "precision", "f_meas")
    } else if (task == "regression") {
      c("rmse", "mae", "rsq")
    } else {
      c("c_index", "uno_c", "ibs", "rmst_diff")
    }

    candidate_metrics <- setdiff(fallback_priority, metric)
    candidate_metrics <- candidate_metrics[candidate_metrics %in% available_metrics]
    remaining_metrics <- setdiff(available_metrics, c(metric, candidate_metrics))
    candidate_metrics <- c(candidate_metrics, remaining_metrics)

    fallback_metric <- NULL
    for (cand in candidate_metrics) {
      cand_values <- extract_metric_values(cand)
      if (any(is.finite(cand_values))) {
        fallback_metric <- cand
        metric_values <- cand_values
        lower_is_better <- compute_lower_is_better(cand)
        break
      }
    }

    if (!is.null(fallback_metric)) {
      warning(sprintf("Metric '%s' unavailable across models; falling back to '%s'.", metric, fallback_metric), call. = FALSE)
      metric <- fallback_metric
    }
  }

  if (!any(is.finite(metric_values))) {
    stop("None of the models returned the specified metric.")
  }

  if (any(is.na(metric_values))) {
    warning("Some models did not return the specified metric.")
    metric_values[is.na(metric_values)] <- if (lower_is_better) Inf else -Inf
  }

  if (all(metric_values == if (lower_is_better) Inf else -Inf)) {
    stop("None of the models returned the specified metric.")
  }

  best_model_idx <- if (lower_is_better) {
    names(metric_values[metric_values == min(metric_values)])
  } else {
    names(metric_values[metric_values == max(metric_values)])
  }

  # model_names <- get_model_engine_names(models)
  # best_model_name <- model_names[model_names %in% best_model_idx]

  # Split each compound name into algorithm and engine parts
  best_model_components <- lapply(best_model_idx, function(x) {
    parts <- strsplit(x, " \\(")[[1]]
    algo <- parts[1]
    engine <- gsub("\\)", "", parts[2])
    list(algo = algo, engine = engine)
  })

  # Build a named vector: names are the algorithm and values are the engine
  best_model_name <- sapply(best_model_components, function(comp) comp$engine)
  names(best_model_name) <- sapply(best_model_components, function(comp) comp$algo)

  if (verbose) {
    msg <- paste0(names(best_model_name), " (", best_model_name, ")", collapse = ", ")
    message("Best model selected: ", msg)
  }


  if (verbose) message("Preparing preprocessing recipe for downstream use...")
  # Now store processed training data for explainability:
  trained_recipe <- prep(recipe, training = train_data, retain = TRUE)
  processed_train_data <- bake(trained_recipe, new_data = NULL)
  raw_train_data <- train_data

  if (learning_curve) {
    # Define the fractions to test
    fractions <- c(0.1, 0.2, 0.4, 0.6, 0.8, 1.0)

    # Helper function to run the learning curve step for a given fraction
    run_curve <- function(fraction) {
      if (verbose) message(sprintf("Learning curve: using %.0f%% of training data", fraction * 100))
      set.seed(seed)

      # Select training subset based on the fraction value
      sub_train <- if (fraction >= 1.0) {
        train_data
      } else {
        sub_split <- rsample::initial_split(
          train_data,
          prop = fraction,
          strata = if (task == "classification") label else NULL
        )
        rsample::training(sub_split)
      }

      # Train models on the subset
      sub_models <- train_models(
        train_data = sub_train,
        label = label,
        task = task,
        algorithms = algorithms,
        resampling_method = resampling_method,
        folds = folds,
        repeats = repeats,
        group_cols = group_cols,
        block_col = block_col,
        block_size = block_size,
        initial_window = initial_window,
        assess_window = assess_window,
        skip = skip,
        outer_folds = outer_folds,
        resamples = resamples,
        tune_params = tune_params,
        engine_params = engine_params,
        metric = metric,
        summaryFunction = summaryFunction,
        seed = seed,
        recipe = recipe,
        use_default_tuning = use_default_tuning,
        tuning_strategy = tuning_strategy,
        tuning_iterations = tuning_iterations,
        early_stopping = early_stopping,
        adaptive = adaptive,
        algorithm_engines = algorithm_engines,
        event_class = event_class,
        start_col = start_col,
        time_col = time_col,
        status_col = status_col,
        eval_times = eval_times,
        at_risk_threshold = at_risk_threshold,
        audit_env = audit_env
      )



      # Evaluate models on the subset
      sub_eval <- evaluate_models(
        sub_models,
        sub_train,
        test_data,
        label_surv,
        task,
        metric,
        event_class,
        eval_times = eval_times,
        bootstrap_ci = bootstrap_ci,
        bootstrap_samples = bootstrap_samples,
        bootstrap_seed = bootstrap_seed,
        at_risk_threshold = at_risk_threshold
      )


      # Extract the performance metric from each model evaluation
      perf_values <- sapply(sub_eval$performance, function(engine_list) {
        perf_df <- engine_list[[1]]
        val <- perf_df[perf_df$.metric == metric, ".estimate", drop = TRUE]
        if (length(val) == 0) NA_real_ else as.numeric(val[[1]])
      })

      perf_values

      # Compute the average performance (ignoring any missing values)
      avg_perf <- mean(perf_values, na.rm = TRUE)

      # Return a data frame for this fraction
      data.frame(Fraction = fraction, Performance = avg_perf)
    }

    # Apply the helper function to each fraction and combine the results
    df_lc <- do.call(rbind, lapply(fractions, run_curve))

    # Prepare the learning-curve plot without printing (avoid side effects)
    lc_plot <- ggplot(df_lc, aes(x = .data$Fraction, y = .data$Performance)) +
      geom_line(color = "blue") +
      geom_point(color = "blue") +
      labs(
        title = "Learning Curve",
        x = "Training Set Size (fraction)",
        y = paste("Mean", metric, "across models")
      ) +
      theme_minimal()
  } else {
    df_lc <- NULL
    lc_plot <- NULL
  }


  processed_test_data <- test_data
  if (!is.null(trained_recipe)) {
    processed_test_data <- tryCatch(
      {
        baked <- recipes::bake(trained_recipe, new_data = test_data)
        if (!is.null(label) && label %in% names(baked)) {
          baked <- baked[, setdiff(names(baked), label), drop = FALSE]
        }
        baked
      },
      error = function(e) test_data
    )
  }

  result <- list(
    best_model = models[best_model_idx],
    best_model_name = best_model_name,
    performance = performance,
    predictions = predictions,
    preprocessor = trained_recipe,
    raw_train_data = raw_train_data,
    processed_train_data = processed_train_data,
    raw_test_data = test_data,
    processed_test_data = processed_test_data,
    label = label,
    task = task,
    models = models,
    metric = metric,
    positive_class = positive_class,
    event_class = event_class,
    engine_names = engine_names,
    learning_curve = list(
      data = df_lc,
      plot = lc_plot
    ),
    survival_brier_times = survival_brier_times,
    survival_t_max = survival_t_max,
    metric_bootstrap = list(enabled = bootstrap_ci, samples = bootstrap_samples, seed = bootstrap_seed),
    resampling_results = resampling_results,
    resampling_plan = resampling_plan,
    nested_cv = nested_results,
    audit = if (audit_env$enabled) list(log = audit_env$log, flagged = audit_env$unsafe) else NULL
  )
  class(result) <- "fastml"
  if (verbose) message("Training complete.")
  return(result)
}
