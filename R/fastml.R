#' Fast Machine Learning Function
#'
#' Trains and evaluates multiple classification or regression models automatically detecting the task based on the target variable type.
#'
#' @param data A data frame containing the features and target variable.
#' @param label A string specifying the name of the target variable.
#' @param algorithms A vector of algorithm names to use. Default is \code{"all"} to run all supported algorithms.
#' @param test_size A numeric value between 0 and 1 indicating the proportion of the data to use for testing. Default is \code{0.2}.
#' @param resampling_method A string specifying the resampling method for cross-validation. Default is \code{"cv"} (cross-validation).
#'                          Other options include \code{"none"}, \code{"boot"}, \code{"repeatedcv"}, etc.
#' @param folds An integer specifying the number of folds for cross-validation. Default is \code{10} for methods containing "cv" and \code{25} otherwise.
#' @param repeats Number of times to repeat cross-validation (only applicable for methods like "repeatedcv").
#' @param tune_params A list specifying hyperparameter tuning ranges. Default is \code{NULL}.
#' @param metric The performance metric to optimize during training. Default depends on the task.
#' @param n_cores An integer specifying the number of CPU cores to use for parallel processing. Default is \code{1}.
#' @param stratify Logical indicating whether to use stratified sampling when splitting the data. Default is \code{TRUE} for classification and \code{FALSE} for regression.
#' @param impute_method Method for handling missing values. Options include:
#'   \describe{
#'     \item{\code{"medianImpute"}}{Impute missing values using median imputation.}
#'     \item{\code{"knnImpute"}}{Impute missing values using k-nearest neighbors.}
#'     \item{\code{"bagImpute"}}{Impute missing values using bagging.}
#'     \item{\code{"remove"}}{Remove rows with missing values from the data.}
#'     \item{\code{"error"}}{Do not perform imputation; if missing values are detected after preprocessing, stop execution with an error.}
#'     \item{\code{NULL}}{Equivalent to \code{"error"}. No imputation is performed, and the function will stop if missing values are present.}
#'   }
#'   Default is \code{"error"}.
#' @param encode_categoricals Logical indicating whether to encode categorical variables. Default is \code{TRUE}.
#' @param scaling_methods Vector of scaling methods to apply. Default is \code{c("center", "scale")}.
#' @param summaryFunction A custom summary function for model evaluation. Default is \code{NULL}.
#' @param use_default_tuning Logical indicating whether to use default tuning grids when \code{tune_params} is \code{NULL}. Default is \code{FALSE}.
#' @param seed An integer value specifying the random seed for reproducibility.
#' @param recipe A user-defined \code{recipe} object for custom preprocessing. If provided, internal recipe steps (imputation, encoding, scaling) are skipped.
#' @importFrom magrittr %>%
#' @importFrom rsample initial_split training testing
#' @importFrom recipes recipe step_impute_median step_impute_knn step_impute_bag step_naomit step_dummy step_center step_scale prep bake
#' @importFrom dplyr select
#' @importFrom foreach registerDoSEQ
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel stopCluster makeCluster
#' @importFrom stats as.formula
#' @return An object of class \code{fastml_model} containing the best model, performance metrics, and other information.
#' @examples
#'   # Example 1: Using the iris dataset for binary classification (excluding 'setosa')
#'   data(iris)
#'   iris <- iris[iris$Species != "setosa", ]  # Binary classification
#'   iris$Species <- factor(iris$Species)
#'
#'   # Train models
#'   model <- fastml(
#'     data = iris,
#'     label = "Species",
#'     algorithms = c("random_forest", "xgboost", "svm_radial")
#'   )
#'
#'   # View model summary
#'   summary(model)
#'
#'   # Example 2: Using the mtcars dataset for regression
#'   data(mtcars)
#'
#'   # Train models
#'   model <- fastml(
#'     data = mtcars,
#'     label = "mpg",
#'     algorithms = c("random_forest", "xgboost", "svm_radial")
#'   )
#'
#'   # View model summary
#'   summary(model)
#'
#' @export
fastml <- function(data,
                   label,
                   algorithms = "all",
                   test_size = 0.2,
                   resampling_method = "cv",
                   folds = ifelse(grepl("cv", resampling_method), 10, 25),
                   repeats = ifelse(resampling_method == "repeatedcv", 1, NA),
                   tune_params = NULL,
                   metric = NULL,
                   n_cores = 1,
                   stratify = NULL,
                   impute_method = "error",
                   encode_categoricals = TRUE,
                   scaling_methods = c("center", "scale"),
                   summaryFunction = NULL,
                   use_default_tuning = FALSE,
                   seed = 123,
                   recipe = NULL) {

  set.seed(seed)

  if (!(label %in% names(data))) {
    stop("The specified label does not exist in the data.")
  }

  target_var <- data[[label]]
  if (is.factor(target_var) || is.character(target_var) || is.logical(target_var)) {
    task <- "classification"
  } else if (is.numeric(target_var)) {
    task <- "regression"
  } else {
    stop("Unable to detect task type. The target variable must be numeric, factor, character, or logical.")
  }

  if (is.null(metric)) {
    metric <- if (task == "classification") "accuracy" else "rmse"
  }

  if (is.null(stratify)) {
    stratify <- (task == "classification")
  }

  supported_algorithms_classification <- c(
    "logistic_regression",
    "penalized_logistic_regression",
    "decision_tree",
    "c5.0",
    "random_forest",
    "ranger",
    "xgboost",
    "lightgbm",
    "svm_linear",
    "svm_radial",
    "knn",
    "naive_bayes",
    "neural_network",
    "lda",
    "qda",
    "bagging"
  )
  supported_algorithms_regression <- c(
    "linear_regression",
    "ridge_regression",
    "lasso_regression",
    "elastic_net",
    "decision_tree",
    "random_forest",
    "xgboost",
    "lightgbm",
    "svm_linear",
    "svm_radial",
    "knn",
    "neural_network",
    "pls",
    "bayes_glm"
  )

  if (task == "classification") {
    supported_algorithms <- supported_algorithms_classification
  } else {
    supported_algorithms <- supported_algorithms_regression
  }

  if ("all" %in% algorithms) {
    algorithms <- supported_algorithms
  } else {
    invalid_algos <- setdiff(algorithms, supported_algorithms)
    if (length(invalid_algos) > 0) {
      warning("Invalid algorithm(s) specified: ", paste(invalid_algos, collapse = ", "))
    }
    algorithms <- intersect(algorithms, supported_algorithms)
    if (length(algorithms) == 0) {
      stop("No valid algorithms specified.")
    }
  }

  if (stratify && task == "classification") {
    split <- rsample::initial_split(data, prop = 1 - test_size, strata = label)
  } else {
    split <- rsample::initial_split(data, prop = 1 - test_size)
  }

  train_data <- rsample::training(split)
  test_data <- rsample::testing(split)

  if (task == "classification") {
    train_data[[label]] <- as.factor(train_data[[label]])
    test_data[[label]] <- factor(test_data[[label]], levels = levels(train_data[[label]]))
  } else {
    train_data[[label]] <- as.numeric(train_data[[label]])
    test_data[[label]] <- as.numeric(test_data[[label]])
  }

  if (is.null(recipe)) {
    recipe <- recipes::recipe(as.formula(paste(label, "~ .")), data = train_data)

    if (impute_method == "medianImpute") {
      recipe <- recipe %>% recipes::step_impute_median(recipes::all_numeric_predictors())
    } else if (impute_method == "knnImpute") {
      recipe <- recipe %>% recipes::step_impute_knn(recipes::all_predictors())
    } else if (impute_method == "bagImpute") {
      recipe <- recipe %>% recipes::step_impute_bag(recipes::all_predictors())
    } else if (impute_method == "remove") {
      recipe <- recipe %>% recipes::step_naomit(recipes::all_predictors(), skip = TRUE)
    } else if (impute_method == "error" || is.null(impute_method)) {
      # do nothing
    } else {
      stop("Invalid impute_method specified.")
    }

    if (encode_categoricals) {
      recipe <- recipe %>% recipes::step_dummy(recipes::all_nominal_predictors(), -recipes::all_outcomes())
    }

    if (!is.null(scaling_methods)) {
      if ("center" %in% scaling_methods) {
        recipe <- recipe %>% recipes::step_center(recipes::all_numeric_predictors())
      }
      if ("scale" %in% scaling_methods) {
        recipe <- recipe %>% recipes::step_scale(recipes::all_numeric_predictors())
      }
    }

    # Do not prep, leave untrained
  } else {
    # User provided a recipe, must be untrained
    if (!inherits(recipe, "recipe")) {
      stop("The provided recipe is not a valid recipe object.")
    }
    if (length(recipe$steps) > 0 && any(sapply(recipe$steps, function(x) x$trained))) {
      stop("The provided recipe is already trained. Please supply an untrained recipe.")
    }
  }

  if (n_cores > 1) {
    if (!requireNamespace("doParallel", quietly = TRUE)) {
      stop("The 'doParallel' package is required for parallel processing but is not installed.")
    }
    cl <- parallel::makeCluster(n_cores)
    doParallel::registerDoParallel(cl)
  } else {
    foreach::registerDoSEQ()
  }

  models <- train_models(
    train_data = train_data,
    label = label,
    task = task,
    algorithms = algorithms,
    resampling_method = resampling_method,
    folds = folds,
    repeats = repeats,
    tune_params = tune_params,
    metric = metric,
    summaryFunction = summaryFunction,
    seed = seed,
    recipe = recipe,
    use_default_tuning = use_default_tuning
  )

  if (n_cores > 1) {
    parallel::stopCluster(cl)
  }

  if (length(models) == 0) {
    stop("No models were successfully trained.")
  }

  eval_output <- evaluate_models(models, train_data, test_data, label, task, metric)
  performance <- eval_output$performance
  predictions <- eval_output$predictions

  metric_values <- sapply(performance, function(x) x %>% dplyr::filter(.metric == metric) %>% dplyr::pull(.estimate))

  if (any(is.na(metric_values))) {
    warning("Some models did not return the specified metric.")
    metric_values[is.na(metric_values)] <- if (task == "regression") Inf else -Inf
  }

  if (all(metric_values == if (task == "regression") Inf else -Inf)) {
    stop("None of the models returned the specified metric.")
  }

  best_model_idx <- if (task == "regression") which.min(metric_values) else which.max(metric_values)
  best_model_name <- names(models)[best_model_idx]

  result <- list(
    best_model = models[[best_model_idx]],
    best_model_name = best_model_name,
    performance = performance,
    predictions = predictions,
    preprocessor = recipe,
    label = label,
    task = task,
    models = models,
    metric = metric
  )
  class(result) <- "fastml_model"
  return(result)
}
