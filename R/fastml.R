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
#' @importFrom magrittr %>%
#' @importFrom rsample initial_split vfold_cv bootstraps training testing
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
                   seed = 123) {

  # Set random seed
  set.seed(seed)
  # Check if label exists in data
  if (!(label %in% names(data))) {
    stop("The specified label does not exist in the data.")
  }
  # Detect task based on target variable type
  target_var <- data[[label]]
  if (is.factor(target_var) || is.character(target_var) || is.logical(target_var)) {
    task <- "classification"
  } else if (is.numeric(target_var)) {
    task <- "regression"
  } else {
    stop("Unable to detect task type. The target variable must be numeric, factor, character, or logical.")
  }
  if (is.null(metric)) {
    if (task == "classification") {
      metric <- "accuracy"
    } else {
      metric <- "rmse"
    }
  }
  # Set default stratify based on task
  if (is.null(stratify)) {
    stratify <- ifelse(task == "classification", TRUE, FALSE)
  }
  # Handle algorithms
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
    "deep_learning",
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
    # "gbm",
    "xgboost",
    "lightgbm",
    "svm_linear",
    "svm_radial",
    "knn",
    "neural_network",
    "deep_learning",
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
    invalid_algorithms <- setdiff(algorithms, supported_algorithms)
    valid_algorithms <- intersect(algorithms, supported_algorithms)
    if (length(invalid_algorithms) > 0) {
      warning(
        paste(
          "Invalid algorithm(s) specified:",
          paste(invalid_algorithms, collapse = ", "),
          "\nSupported algorithms for",
          task,
          "are:",
          paste(supported_algorithms, collapse = ", ")
        )
      )
    }
    if (length(valid_algorithms) == 0) {
      stop(
        paste(
          "No valid algorithms specified. Please choose from the supported algorithms for",
          task,
          ":\n",
          paste(supported_algorithms, collapse = ", ")
        )
      )
    } else {
      algorithms <- valid_algorithms
    }
  }
  # Split data into training and testing sets
  if (stratify && task == "classification") {
    split <- initial_split(data, prop = 1 - test_size, strata = label)
  } else {
    split <- initial_split(data, prop = 1 - test_size)
  }
  train_data <- training(split)
  test_data <- testing(split)
  # Preprocess data using recipes
  # Create a recipe
  recipe <- recipe(as.formula(paste(label, "~ .")), data = train_data)
  # Handle missing values
  if (impute_method == "medianImpute") {
    recipe <- recipe %>% step_impute_median(all_numeric_predictors())
  } else if (impute_method == "knnImpute") {
    recipe <- recipe %>% step_impute_knn(all_predictors())
  } else if (impute_method == "bagImpute") {
    recipe <- recipe %>% step_impute_bag(all_predictors())
  } else if (impute_method == "remove") {
    recipe <- recipe %>% step_naomit(all_predictors(), skip = TRUE)
  } else if (impute_method == "error" || is.null(impute_method)) {
    # Do nothing, let the recipe throw an error if there are missing values
  } else {
    stop("Invalid impute_method specified.")
  }
  # Encode categorical variables
  if (encode_categoricals) {
    recipe <- recipe %>% step_dummy(all_nominal_predictors(), -all_outcomes())
  }
  # Scaling methods
  if (!is.null(scaling_methods)) {
    if ("center" %in% scaling_methods) {
      recipe <- recipe %>% step_center(all_numeric_predictors())
    }
    if ("scale" %in% scaling_methods) {
      recipe <- recipe %>% step_scale(all_numeric_predictors())
    }
  }
  # Prepare the recipe
  recipe_prep <- prep(recipe, training = train_data)
  # Apply the recipe to training and test data
  train_processed <- bake(recipe_prep, new_data = NULL)
  test_processed <- bake(recipe_prep, new_data = test_data)
  # Adjust target variable based on task
  if (task == "classification") {
    # Adjust factor levels of the target variable in training data
    train_processed[[label]] <- as.factor(train_processed[[label]])
    # Adjust factor levels of the target variable in test data to match training data
    test_processed[[label]] <- factor(test_processed[[label]], levels = levels(train_processed[[label]]))
  } else {
    # For regression, ensure the target variable is numeric
    train_processed[[label]] <- as.numeric(train_processed[[label]])
    test_processed[[label]] <- as.numeric(test_processed[[label]])
  }
  # Check for missing values in preprocessed training data
  if (any(is.na(train_processed))) {
    # Provide detailed error message
    na_counts <- colSums(is.na(train_processed))
    na_vars <- names(na_counts[na_counts > 0])
    na_info <- paste(na_vars, "(", na_counts[na_vars], " missing)", collapse = ", ")
    stop(
      paste(
        "Preprocessed training data contains missing values in the following variables:",
        na_info,
        "\nPlease check the preprocessing steps or specify an appropriate 'impute_method'."
      )
    )
  }
  # Check for missing values in preprocessed test data
  if (any(is.na(test_processed))) {
    # Provide detailed error message
    na_counts <- colSums(is.na(test_processed))
    na_vars <- names(na_counts[na_counts > 0])
    na_info <- paste(na_vars, "(", na_counts[na_vars], " missing)", collapse = ", ")
    stop(
      paste(
        "Preprocessed test data contains missing values in the following variables:",
        na_info,
        "\nPlease check the preprocessing steps or specify an appropriate 'impute_method'."
      )
    )
  }
  # Initialize parallel processing if n_cores > 1
  if (n_cores > 1) {
    if (!requireNamespace("doParallel", quietly = TRUE)) {
      stop("The 'doParallel' package is required for parallel processing but is not installed.")
    }
    cl <- makeCluster(n_cores)
    registerDoParallel(cl)
  } else {
    registerDoSEQ()
  }
  # Train models
  models <- train_models(
    train_data = train_processed,
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
  # Stop parallel processing
  if (n_cores > 1) {
    stopCluster(cl)
  }
  # Check if any models were successfully trained
  if (length(models) == 0) {
    stop("No models were successfully trained. Please check your data and parameters.")
  }
  # Evaluate models
  performance <- evaluate_models(models, train_processed, test_processed, label, task, metric)
  # Calculate best model index using the metric specified
  metric_values <- sapply(performance, function(x) x %>% filter(.metric == metric) %>% pull(.estimate))
  # Handle possible NA values
  if (any(is.na(metric_values))) {
    warning(
      "Some models did not return the specified metric. Check the metric name and model performance."
    )
    metric_values[is.na(metric_values)] <- if (task == "regression") Inf else -Inf  # Adjust based on task
  }
  # Check if all metric_values are Inf or -Inf
  if (all(metric_values == if (task == "regression") Inf else -Inf)) {
    stop(
      "None of the models returned the specified metric. Please check the 'metric' parameter."
    )
  }
  # Identify best model
  if (task == "regression") {
    best_model_idx <- which.min(metric_values)
  } else {
    best_model_idx <- which.max(metric_values)
  }
  best_model_name <- names(models)[best_model_idx]
  # Store the result
  result <- list(
    best_model = models[[best_model_idx]],
    best_model_name = best_model_name,
    performance = performance,
    preprocessor = recipe_prep,
    label = label,
    task = task,
    models = models,
    metric = metric
   )
  class(result) <- "fastml_model"
  # Return the result
  return(result)
}
