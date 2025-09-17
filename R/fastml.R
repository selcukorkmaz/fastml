utils::globalVariables(c("Fraction", "Performance"))

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
#' @param resampling_method A string specifying the resampling method for model evaluation. Default is \code{"cv"} (cross-validation).
#'                          Other options include \code{"none"}, \code{"boot"}, \code{"repeatedcv"}, etc.
#' @param folds An integer specifying the number of folds for cross-validation. Default is \code{10} for methods containing "cv" and \code{25} otherwise.
#' @param repeats Number of times to repeat cross-validation (only applicable for methods like "repeatedcv").
#' @param event_class A single string. Either "first" or "second" to specify which level of truth to consider as the "event". Default is "first".
#' @param exclude A character vector specifying the names of the columns to be excluded from the training process.
#' @param recipe A user-defined \code{recipe} object for custom preprocessing. If provided, internal recipe steps (imputation, encoding, scaling) are skipped.
#' @param tune_params A named list of tuning ranges for each algorithm and engine
#'   pair. Example: \code{list(rand_forest = list(ranger = list(mtry = c(1, 3))))}
#'   will override the defaults for the ranger engine. Default is \code{NULL}.
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
#'     \item{\code{"mice"}}{Impute missing values using MICE (Multiple Imputation by Chained Equations).}
#'     \item{\code{"missForest"}}{Impute missing values using the missForest algorithm.}
#'     \item{\code{"custom"}}{Use a user-provided imputation function (see `impute_custom_function`).}
#'     \item{\code{"error"}}{Do not perform imputation; if missing values are detected, stop execution with an error.}
#'     \item{\code{NULL}}{Equivalent to \code{"error"}. No imputation is performed, and the function will stop if missing values are present.}
#'   }
#'   Default is \code{"error"}.
#' @param impute_custom_function A function that takes a data.frame as input and returns an imputed data.frame. Used only if \code{impute_method = "custom"}.
#' @param encode_categoricals Logical indicating whether to encode categorical variables. Default is \code{TRUE}.
#' @param scaling_methods Vector of scaling methods to apply. Default is \code{c("center", "scale")}. 
#' @param balance_method Method to handle class imbalance. One of \code{"none"},
#'   \code{"upsample"}, or \code{"downsample"}. Applied to the training set for
#'   classification tasks. Default is \code{"none"}.
#' @param resamples Optional rsample object providing custom resampling splits.
#'   If supplied, \code{resampling_method}, \code{folds}, and \code{repeats} are
#'   ignored.
#' @param summaryFunction A custom summary function for model evaluation. Default is \code{NULL}.
#' @param use_default_tuning Logical; if \code{TRUE} and \code{tune_params} is \code{NULL}, tuning is performed using default grids. Tuning also occurs when custom \code{tune_params} are supplied. When \code{FALSE} and no custom parameters are given, models are fitted once with default settings. Default is \code{FALSE}.
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
#' @importFrom magrittr %>%
#' @importFrom rsample initial_split training testing
#' @importFrom recipes recipe step_impute_median step_impute_knn step_impute_bag step_naomit step_dummy step_center step_scale prep bake all_numeric_predictors all_predictors all_nominal_predictors all_outcomes step_zv step_rm
#' @importFrom dplyr filter pull rename_with mutate across where select all_of
#' @importFrom rlang sym
#' @importFrom stats as.formula complete.cases
#' @importFrom doFuture registerDoFuture
#' @importFrom future plan multisession sequential
#' @importFrom janitor make_clean_names
#' @importFrom stringr str_detect
#' @importFrom mice mice complete
#' @importFrom missForest missForest
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
                   resampling_method = "cv",
                   folds = ifelse(grepl("cv", resampling_method), 10, 25),
                   repeats = ifelse(resampling_method == "repeatedcv", 1, NA),
                   event_class = "first",
                   exclude = NULL,
                   recipe = NULL,
                   tune_params = NULL,
                   metric = NULL,
                   algorithm_engines = NULL,
                   n_cores = 1,
                   stratify = TRUE,
                   impute_method = "error",
                   impute_custom_function = NULL,
                   encode_categoricals = TRUE,
                   scaling_methods = c("center", "scale"),
                   balance_method = c("none", "upsample", "downsample"),
                   resamples = NULL,
                   summaryFunction = NULL,
                   use_default_tuning = FALSE,
                   tuning_strategy = "grid",
                   tuning_iterations = 10,
                   early_stopping = FALSE,
                   adaptive = FALSE,
                   learning_curve = FALSE,
                   seed = 123,
                   verbose = FALSE) {

  set.seed(seed)

  task <- match.arg(task, c("auto", "classification", "regression", "survival"))
  tuning_strategy <- match.arg(tuning_strategy, c("grid", "bayes", "none"))
  balance_method <- match.arg(balance_method)

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


  # Determine source for target variable
  source_data <- if (!is.null(data)) data else train_data

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
      # classification/regression detection with single target label
      if (!(label %in% names(source_data))) {
        stop("Label variable must exist in the data source.")
      }
      target_var <- source_data[[label]]
      if (is.numeric(target_var) && length(unique(target_var)) <= 5) {
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
      target_var <- source_data[[label]]
    }
  }

  # determine positive_class after data split when factor levels are available
  positive_class <- NULL
  # ---------------- END TASK DETECTION ----------------


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
    if (stratify && task == "classification") {
      split <- rsample::initial_split(data, prop = 1 - test_size, strata = label)
    } else {
      split <- rsample::initial_split(data, prop = 1 - test_size)
    }
    train_data <- rsample::training(split)
    test_data  <- rsample::testing(split)
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

  if (task == "survival") {
    if (!(length(label) %in% c(2, 3))) {
      stop("For survival tasks, 'label' must contain the time/status columns present in the data (length 2 or 3).")
    }
    label_surv <- label
    if (length(label) == 2) {
      time_col <- label[1]
      status_col <- label[2]
      surv_train <- survival::Surv(train_data[[time_col]], train_data[[status_col]])
      surv_test  <- survival::Surv(test_data[[time_col]], test_data[[status_col]])
    } else {
      start_col <- label[1]
      stop_col <- label[2]
      status_col <- label[3]
      surv_train <- survival::Surv(train_data[[start_col]], train_data[[stop_col]], train_data[[status_col]])
      surv_test  <- survival::Surv(test_data[[start_col]], test_data[[stop_col]], test_data[[status_col]])
    }
    attr(surv_train, "fastml_label_cols") <- label_surv
    attr(surv_test, "fastml_label_cols") <- label_surv
    train_data$surv_obj <- surv_train
    test_data$surv_obj <- surv_test
    label <- "surv_obj"
  } else {
    label_surv <- label
  }


  if (is.null(metric)) {
    metric <- if (task == "classification") {
      "accuracy"
    } else if (task == "regression") {
      "rmse"
    } else {
      "c_index"
    }
  }

  # Define allowed metrics
  allowed_metrics_classification <- c("accuracy", "kap", "sens", "spec", "precision", "f_meas", "roc_auc")
  allowed_metrics_regression <- c("rmse", "rsq", "mae")
  allowed_metrics_survival <- c("c_index", "brier_score", "logrank_p")

  # Validate the metric based on the task
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
    if (!(metric %in% allowed_metrics_survival)) {
      stop(paste0("Invalid metric for survival task. Choose one of: ",
                  paste(allowed_metrics_survival, collapse = ", "), "."))
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



  if (task != "survival") {
    # Task detection and numeric-to-factor conversion for small numeric targets
    if (is.numeric(target_var) && length(unique(target_var)) <= 5) {
      # Convert both train and test labels to factor
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
    if (length(levels(train_data[[label]])) >= 2) {
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

  ###############################################################################
  # NEW BLOCK: Advanced Imputation (MICE, missForest, or custom) if no recipe supplied
  ###############################################################################
  # We handle advanced or custom imputation outside of the recipes step.
  # We'll do it on train_data and test_data directly if requested.

  # If user has provided a custom recipe, skip internal imputation logic
  if (is.null(recipe)) {

    if (!is.null(impute_method) && impute_method %in% c("mice", "missForest", "custom")) {

      # If 'custom', user must provide a function
      if (impute_method == "custom") {
        if (!is.function(impute_custom_function)) {
          stop("You selected impute_method='custom' but did not provide a valid `impute_custom_function`.")
        }
        # Apply user function to train/test
        train_data <- impute_custom_function(train_data)
        test_data <- impute_custom_function(test_data)

        warning("Missing values in the training and test set have been imputed using the 'custom' method.")


        } else if (impute_method == "mice") {
          if (!requireNamespace("mice", quietly = TRUE)) {
            stop("impute_method='mice' requires the 'mice' package to be installed.")
          }
          # Perform MICE on train_data only. Typically, you do MICE on training and apply
          # some approach to test, but for simplicity, we'll do them separately:
          # 1) For train_data:
          train_cols <- names(train_data)
          matrix_cols_train <- vapply(train_data, is.matrix, logical(1))
          if (any(matrix_cols_train)) {
            train_matrix <- train_data[, matrix_cols_train, drop = FALSE]
            train_non_matrix <- train_data[, !matrix_cols_train, drop = FALSE]
            train_data_mice <- mice(train_non_matrix)
            train_non_matrix <- complete(train_data_mice)
            train_data <- cbind(train_non_matrix, train_matrix)
            train_data <- train_data[, train_cols]
          } else {
            train_data_mice <- mice(train_data)  # Basic usage
            train_data <- complete(train_data_mice)
          }

          warning("Missing values in the training set have been imputed using the 'mice' method.")

          # 2) For test_data:
          # There's no built-in perfect approach for test_data in MICE. For simplicity,
          # we do a quick single pass (though in practice you might combine with train).
          # We'll create a temporary combined approach or re-run MICE on test alone.
          # This is simplistic but workable for demonstration.
          if (anyNA(test_data)) {
            test_cols <- names(test_data)
            matrix_cols_test <- vapply(test_data, is.matrix, logical(1))
            if (any(matrix_cols_test)) {
              test_matrix <- test_data[, matrix_cols_test, drop = FALSE]
              test_non_matrix <- test_data[, !matrix_cols_test, drop = FALSE]
              test_data_mice <- mice(test_non_matrix)
              test_non_matrix <- complete(test_data_mice)
              test_data <- cbind(test_non_matrix, test_matrix)
              test_data <- test_data[, test_cols]
            } else {
              test_data_mice <- mice(test_data)
              test_data <- complete(test_data_mice)
            }

            warning("Missing values in the test set have been imputed using the 'mice' method.")

          }

        } else if (impute_method == "missForest") {
        if (!requireNamespace("missForest", quietly = TRUE)) {
          stop("impute_method='missForest' requires the 'missForest' package to be installed.")
        }
        # Perform missForest on train_data
        train_data_imp <- missForest(train_data, verbose = FALSE)
        train_data <- train_data_imp$ximp

        warning("Missing values in the training set have been imputed using the 'missForest' method.")


        # Similarly for test_data if needed
        if (anyNA(test_data)) {
          test_data_imp <- missForest(test_data, verbose = FALSE)
          test_data <- test_data_imp$ximp

          warning("Missing values in the test set have been imputed using the 'missForest' method.")

        }
      }

      # If user has requested advanced or custom imputation, we won't also do recipe-based steps
      # for imputation. We'll effectively skip those inside the recipes below.
      # We'll keep a note to skip recipe imputation steps:
      skip_recipe_imputation <- TRUE

    } else {
      skip_recipe_imputation <- FALSE
    }

  } else {
    # If user provided a recipe, do not do advanced or custom imputation outside
    skip_recipe_imputation <- TRUE
  }
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

    # Remove zero-variance predictors
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
      recipe <- recipe %>% step_dummy(all_nominal_predictors(), -all_outcomes())
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
  } else {
    # user provided a recipe
    if (!inherits(recipe, "recipe")) {
      stop("The provided recipe is not a valid recipe object.")
    }
    if (length(recipe$steps) > 0 && any(sapply(recipe$steps, function(x) x$trained))) {
      stop("The provided recipe is already trained. Please supply an untrained recipe.")
    }
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
    resamples = resamples,
    tune_params = tune_params,
    metric = metric,
    summaryFunction = summaryFunction,
    seed = seed,
    recipe = recipe,
    use_default_tuning = use_default_tuning,
    tuning_strategy = tuning_strategy,
    tuning_iterations = tuning_iterations,
    early_stopping = early_stopping,
    adaptive = adaptive,
    algorithm_engines = algorithm_engines
  )

  models <- models[sapply(models, function(x) length(x) > 0)]

  engine_names <- get_engine_names(models)

  if (length(models) == 0) {
    stop("No models were successfully trained.")
  }

  if (verbose) message("Evaluating models...")
  eval_output <- evaluate_models(models, train_data, test_data, label_surv, task, metric, event_class)
  performance <- eval_output$performance
  predictions <- eval_output$predictions

  # metric_values <- sapply(performance, function(x) x %>% filter(.metric == metric) %>% pull(.estimate))

  # Build a normalized performance list and align model names as "algorithm (engine)"
  combined_performance  <- list()
  model_map <- list()
  for (alg in names(performance)) {
    perf_alg <- performance[[alg]]
    # Case 1: nested by engine (list of tibbles)
    if (is.list(perf_alg) && !inherits(perf_alg, "data.frame")) {
      for (eng in names(perf_alg)) {
        combined_name <- paste0(alg, " (", eng, ")")
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
      combined_name <- paste0(alg, " (", eng, ")")
      combined_performance[[combined_name]] <- perf_alg
      model_map[[combined_name]] <- models[[alg]]
    }
  }

  # Replace models with normalized, consistently named map
  models <- model_map


  # Now apply the function over the flattened list
  metric_values <- sapply(combined_performance, function(x) {
    if (is.data.frame(x)) {
      val <- x %>% dplyr::filter(.metric == metric) %>% dplyr::pull(.estimate)
      if (length(val) == 0) NA_real_ else as.numeric(val[1])
    } else {
      NA_real_
    }
  })


  if (any(is.na(metric_values))) {
    warning("Some models did not return the specified metric.")
    metric_values[is.na(metric_values)] <- if (task == "regression") Inf else -Inf
  }

  if (all(metric_values == if (task == "regression") Inf else -Inf)) {
    stop("None of the models returned the specified metric.")
  }

  best_model_idx <- if (task == "regression" && metric != "rsq"){
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
       resamples = resamples,
       tune_params = tune_params,
        metric = metric,
        summaryFunction = summaryFunction,
        seed = seed,
        recipe = recipe,
        use_default_tuning = use_default_tuning,
        tuning_strategy = tuning_strategy,
        tuning_iterations = tuning_iterations,
        early_stopping = early_stopping,
        adaptive = adaptive,
        algorithm_engines = algorithm_engines
      )



      # Evaluate models on the subset
      sub_eval <- evaluate_models(
        sub_models,
        sub_train,
        test_data,
        label_surv,
        task,
        metric,
        event_class
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

    # Plot the learning curve

      lc_plot <- ggplot(df_lc, aes(x = Fraction, y = Performance)) +
        geom_line(color = "blue") +
        geom_point(color = "blue") +
        labs(
          title = "Learning Curve",
          x = "Training Set Size (fraction)",
          y = paste("Mean", metric, "across models")
        ) +
        theme_minimal()
      print(lc_plot)

  }


  result <- list(
    best_model = models[best_model_idx],
    best_model_name = best_model_name,
    performance = performance,
    predictions = predictions,
    preprocessor = trained_recipe,
    processed_train_data = processed_train_data,
    label = label,
    task = task,
    models =models,
    metric = metric,
    positive_class = positive_class,
    event_class = event_class,
    engine_names = engine_names
  )
  class(result) <- "fastml"
  if (verbose) message("Training complete.")
  return(result)
}
