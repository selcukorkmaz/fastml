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
#' @param tune_params A list specifying hyperparameter tuning ranges. Default is \code{NULL}.
#' @param metric The performance metric to optimize during training. Default is \code{"Accuracy"}.
#' @param n_cores An integer specifying the number of CPU cores to use for parallel processing. Default is \code{1}.
#' @param stratify Logical indicating whether to use stratified sampling when splitting the data. Default is \code{TRUE}.
#' @param impute_method Method for missing value imputation. Default is \code{NULL}.
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
#'   folds = 3,
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
#'   tune_params = custom_tuning
#' )
#'
#' # View the results
#' summary(model4)
#'
#' @importFrom caret createDataPartition
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach registerDoSEQ
#' @importFrom parallel makeCluster stopCluster detectCores
#' @export
fastml <- function(data,
                   label,
                   algorithms = c("xgboost", "random_forest", "svm_radial"),
                   test_size = 0.2,
                   resampling_method = "cv",
                   folds = 5,
                   tune_params = NULL,
                   metric = "Accuracy",
                   n_cores = 1,
                   stratify = TRUE,
                   impute_method = NULL,
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

  # Supported metrics
  supported_metrics <-
    c("Accuracy",
      "Kappa",
      "Sensitivity",
      "Specificity",
      "Precision",
      "F1",
      "MCC")

  # Validate metric
  if (!(metric %in% supported_metrics)) {
    stop(paste(
      "Unsupported metric. Please choose from:",
      paste(supported_metrics, collapse = ", ")
    ))
  }

  # Check if the label exists in the data
  if (!(label %in% names(data))) {
    stop("The specified label does not exist in the data.")
  }

  # Handle algorithms
  supported_algorithms <- c(
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

  if ("all" %in% algorithms) {
    algorithms <- supported_algorithms
  } else {
    invalid_algorithms <- setdiff(algorithms, supported_algorithms)
    if (length(invalid_algorithms) > 0) {
      warning(
        paste(
          "Invalid algorithm(s) specified:",
          paste(invalid_algorithms, collapse = ", "),
          "\nSupported algorithms are:",
          paste(supported_algorithms, collapse = ", ")
        )
      )
      # Remove invalid algorithms
      algorithms <- setdiff(algorithms, invalid_algorithms)
    }
  }

  # Split data into training and testing sets
  set.seed(seed)  # For reproducibility
  if (stratify) {
    train_index <-
      createDataPartition(data[[label]], p = 1 - test_size, list = FALSE)
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

  # Check for missing values in preprocessed training data
  if (any(is.na(train_processed))) {
    stop(
      "Preprocessed training data contains missing values. Please check the preprocessing steps."
    )
  }

  # Initialize parallel backend if n_cores > 1
  if (n_cores > 1) {
    cl <- makeCluster(n_cores)
    registerDoParallel(cl)

    # Ensure the cluster is stopped even if an error occurs
    on.exit({
      stopCluster(cl)
      registerDoSEQ()
    }, add = TRUE)
  }

  # Train models within a tryCatch to handle errors and ensure cluster termination
  models <- tryCatch({
    train_models(
      train_data = train_processed,
      label = label,
      algorithms = algorithms,
      resampling_method = resampling_method,
      folds = folds,
      tune_params = tune_params,
      metric = metric,
      summaryFunction = summaryFunction
    )
  }, error = function(e) {
    # The on.exit will handle cluster termination
    stop(e)  # Re-throw the error after ensuring on.exit is called
  })

  # Check if any models were successfully trained
  if (length(models) == 0) {
    stop("No models were successfully trained. Please check your data and parameters.")
  }

  # Evaluate models (compute multiple metrics by default)
  performance <-
    evaluate_models(models, test_processed, label, metric)

  # Calculate best model index using the metric specified
  metric_values <- sapply(performance, function(x)
    x[[metric]])

  # Handle possible NA values
  if (any(is.na(metric_values))) {
    warning(
      "Some models did not return the specified metric. Check the metric name and model performance."
    )
    metric_values[is.na(metric_values)] <-
      -Inf  # Set NA to -Inf so they are not selected as best
  }

  # Check if all metric_values are -Inf (i.e., all models failed to produce the metric)
  if (all(metric_values == -Inf)) {
    stop(
      "None of the models returned the specified metric. Please check the 'metric' parameter."
    )
  }

  best_model_idx <- which.max(metric_values)

  # Store the result
  result <- list(
    best_model = models[[best_model_idx]],
    # Best model object
    best_model_name = names(models)[best_model_idx],
    # Name of the best algorithm
    performance = performance,
    # Performance metrics of all models
    preprocessor = preprocessor,
    # Preprocessing steps
    label = label,
    # Store the label name for future use
    models = models,
    # Include all models in the result
    metric = metric                                   # Store the optimized metric
  )

  class(result) <- "fastml_model"
  invisible(result)
}
