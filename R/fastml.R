#' Fast Machine Learning Function
#'
#' Trains and evaluates multiple classification models.
#'
#' @param data A data frame containing the features and target variable.
#' @param label A string specifying the name of the target variable.
#' @param algorithms A vector of algorithm names to use. Default is \code{c("xgboost", "random_forest", "svm_radial")}.
#'                   Use \code{"all"} to run all supported algorithms.
#' @param test_size A numeric value between 0 and 1 indicating the proportion of the data to use for testing. Default is \code{0.2}.
#' @param resampling_method A string specifying the resampling method for cross-validation. Default is \code{"cv"} (cross-validation).
#'                          Other options include \code{"none"}, \code{"boot"}, \code{"repeatedcv"}, etc.
#' @param folds An integer specifying the number of folds for cross-validation. Default is \code{5}.
#' @param repeats Number of times to repeat cross-validation (only applicable for methods like "repeatedcv").
#' @param tune_params A list specifying hyperparameter tuning ranges. Default is \code{NULL}.
#' @param metric The performance metric to optimize during training. Default is \code{"Accuracy"}.
#' @param n_cores An integer specifying the number of CPU cores to use for parallel processing. Default is \code{1}.
#' @param stratify Logical indicating whether to use stratified sampling when splitting the data. Default is \code{TRUE}.
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
#' @param seed An integer value specifying the random seed for reproducibility.
#' @return An object of class \code{fastml_model} containing the best model, performance metrics, and other information.
#' @examples
#' \donttest{
#'  # Example 1: Using the iris dataset for binary classification (excluding 'setosa')
#' data(iris)
#' iris <- iris[iris$Species != "setosa", ]  # Binary classification
#' iris$Species <- factor(iris$Species)
#'
#' # Train models
#' model <- fastml(
#'   data = iris,
#'   label = "Species"
#' )
#'
#' # View model summary
#' summary(model)
#' }
#'
#' # Example 2: Using the mtcars dataset for binary classification
#' data(mtcars)
#' mtcars$am <- factor(mtcars$am)  # Convert transmission (0 = automatic, 1 = manual) to a factor
#'
#' # Train models with a different resampling method and specific algorithms
#' model2 <- fastml(
#'   data = mtcars,
#'   label = "am",
#'   algorithms = c("random_forest", "svm_radial"),
#'   resampling_method = "repeatedcv",
#'   folds = 5,
#'   repeats = 2,
#'   test_size = 0.25
#' )
#'
#' # View model performance
#' summary(model2)
#'
#' \donttest{
#' # Example 3: Using the airquality dataset with missing values
#' data(airquality)
#' airquality <- na.omit(airquality)  # Simple example to remove missing values for demonstration
#' airquality$Month <- factor(airquality$Month)
#'
#' # Train models with categorical encoding and scaling
#' model3 <- fastml(
#'   data = airquality,
#'   label = "Month",
#'   encode_categoricals = TRUE,
#'   scaling_methods = c("center", "scale")
#' )
#'
#' # Evaluate and compare models
#' summary(model3)
#' }
#'
#' # Example 4: Custom hyperparameter tuning for a random forest
#' data(iris)
#' iris <- iris[iris$Species != "setosa", ]  # Filter out 'setosa' for binary classification
#' iris$Species <- factor(iris$Species)
#' custom_tuning <- list(
#'   random_forest = expand.grid(mtry = c(1:10))
#' )
#'
#' model4 <- fastml(
#'   data = iris,
#'   label = "Species",
#'   algorithms = c("random_forest"),
#'   tune_params = custom_tuning,
#'   metric = "Accuracy"
#' )
#'
#' # View the results
#' summary(model4)
#'
#' @importFrom caret createDataPartition
#' @importFrom doParallel registerDoParallel registerDoParallel
#' @importFrom foreach registerDoSEQ
#' @importFrom parallel makeCluster stopCluster detectCores
#' @importFrom stats complete.cases
#' @export
#' Fast Machine Learning Function
#'
#' Trains and evaluates multiple classification or regression models automatically detecting the task based on the target variable type.
#'
#' @param data A data frame containing the features and target variable.
#' @param label A string specifying the name of the target variable.
#' @param algorithms A vector of algorithm names to use. Default depends on the task.
#'                   Use \code{"all"} to run all supported algorithms.
#' @param test_size A numeric value between 0 and 1 indicating the proportion of the data to use for testing. Default is \code{0.2}.
#' @param resampling_method A string specifying the resampling method for cross-validation. Default is \code{"cv"} (cross-validation).
#'                          Other options include \code{"none"}, \code{"boot"}, \code{"repeatedcv"}, etc.
#' @param folds An integer specifying the number of folds for cross-validation. Default is \code{5}.
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
#' @param seed An integer value specifying the random seed for reproducibility.
#' @return An object of class \code{fastml_model} containing the best model, performance metrics, and other information.
#' @export
fastml <- function(data,
                   label,
                   algorithms = NULL,
                   test_size = 0.2,
                   resampling_method = "cv",
                   folds = ifelse(grepl("cv", resampling_method), 10, 25),
                   repeats = ifelse(grepl("[d_]cv$", resampling_method), 1, NA),
                   tune_params = NULL,
                   metric = NULL,
                   n_cores = 1,
                   stratify = NULL,
                   impute_method = "error",
                   encode_categoricals = TRUE,
                   scaling_methods = c("center", "scale"),
                   summaryFunction = NULL,
                   seed = 123) {
  # Load required packages
  if (!requireNamespace("caret", quietly = TRUE)) {
    stop("The 'caret' package is required but not installed.")
  }
  if (!requireNamespace("doParallel", quietly = TRUE)) {
    stop("The 'doParallel' package is required but not installed.")
  }
  if (!requireNamespace("foreach", quietly = TRUE)) {
    stop("The 'foreach' package is required but not installed.")
  }
  if (!requireNamespace("parallel", quietly = TRUE)) {
    stop("The 'parallel' package is required but not installed.")
  }

  # Initialize the cluster variable
  cl <- NULL

  # Check if the label exists in the data
  if (!(label %in% names(data))) {
    stop("The specified label does not exist in the data.")
  }

  # Detect task based on the type of the target variable
  target_var <- data[[label]]

  if (is.factor(target_var) || is.character(target_var) || is.logical(target_var)) {
    task <- "classification"
  } else if (is.numeric(target_var)) {
    task <- "regression"
  } else {
    stop("Unable to detect task type. The target variable must be numeric, factor, character, or logical.")
  }

  # Set default algorithms and metric based on the task
  if (is.null(algorithms)) {
    if (task == "classification") {
      algorithms <- c("neural_network", "random_forest", "svm_radial", "xgboost", "gbm")
    } else {
      algorithms <- c("neural_network", "random_forest", "svm_radial", "xgboost", "gbm")
    }
  }

  if (is.null(metric)) {
    if (task == "classification") {
      metric <- "Accuracy"
    } else {
      metric <- "RMSE"
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
    "gbm",
    "xgboost",
    "svm_linear",
    "svm_radial",
    "knn",
    "naive_bayes",
    "neural_network",
    "lda",
    "qda",
    "bagging",
    "adaboost",
    "logitboost",
    "elastic_net",
    "bayes_glm",
    "pls",
    "glmboost"
  )

  supported_algorithms_regression <- c(
    "linear_regression",
    "ridge_regression",
    "lasso_regression",
    "elastic_net",
    "decision_tree",
    "random_forest",
    "gbm",
    "xgboost",
    "svm_linear",
    "svm_radial",
    "knn",
    "neural_network",
    "pls",
    "glmboost"
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
  set.seed(seed)  # For reproducibility
  if (stratify && task == "classification") {
    train_index <-
      caret::createDataPartition(data[[label]], p = 1 - test_size, list = FALSE)
  } else {
    train_index <-
      sample(seq_len(nrow(data)), size = floor((1 - test_size) * nrow(data)))
  }

  train_data <- data[train_index, , drop = FALSE]
  test_data <- data[-train_index, , drop = FALSE]

  # Preprocess data
  preprocessed <- prepare_data(
    train_data = train_data,
    test_data = test_data,
    label = label,
    impute_method = impute_method,
    encode_categoricals = encode_categoricals,
    scaling_methods = scaling_methods
  )

  train_processed <- preprocessed$train
  test_processed <- preprocessed$test
  preprocessor <- preprocessed$preprocessor

  # Adjust target variable based on task
  if (task == "classification") {
    # Adjust factor levels of the target variable in training data
    train_processed[[label]] <- as.factor(train_processed[[label]])
    original_levels <- levels(train_processed[[label]])
    valid_levels <- make.names(original_levels)
    if (!all(original_levels == valid_levels)) {
      levels(train_processed[[label]]) <- valid_levels
      warning("Factor levels of the target variable have been adjusted to be valid R variable names.")
    }

    # Adjust factor levels of the target variable in test data to match training data
    test_processed[[label]] <- factor(test_processed[[label]], levels = original_levels)
    levels(test_processed[[label]]) <- levels(train_processed[[label]])
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
    na_info <- paste(na_vars, "(", na_counts[na_vars], "missing)", collapse = ", ")
    stop(
      paste(
        "Preprocessed training data contains missing values in the following variables:",
        na_info,
        "\nPlease check the preprocessing steps or specify an appropriate 'impute_method'."
      )
    )
  }

  # Initialize parallel backend if n_cores > 1
  if (n_cores > 1) {
    cl <- parallel::makeCluster(n_cores)
    doParallel::registerDoParallel(cl)

    # Ensure the cluster is stopped even if an error occurs
    on.exit({
      if (!is.null(cl)) {
        parallel::stopCluster(cl)
        foreach::registerDoSEQ()
      }
    }, add = TRUE)
  }

  # Wrap the main code in a tryCatch block
  result <- tryCatch({

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
      seed = seed  # Pass seed for reproducibility
    )

    # Check if any models were successfully trained
    if (length(models) == 0) {
      stop("No models were successfully trained. Please check your data and parameters.")
    }

    # Evaluate models
    performance <-
      evaluate_models(models, test_processed, label, task, metric)

    # Calculate best model index using the metric specified
    metric_values <- sapply(performance, function(x)
      x[[metric]])

    # Handle possible NA values
    if (any(is.na(metric_values))) {
      warning(
        "Some models did not return the specified metric. Check the metric name and model performance."
      )
      metric_values[is.na(metric_values)] <-
        if (task == "regression") Inf else -Inf  # Adjust based on task
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
      preprocessor = preprocessor,
      label = label,
      task = task,
      models = models,
      metric = metric
    )

    class(result) <- "fastml_model"
    result  # Return the result

  }, error = function(e) {
    # Re-throw the error after ensuring on.exit is called
    stop(e)
  })

  # Return the result
  invisible(result)
}





