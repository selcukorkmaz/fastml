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
#' @param data A data frame containing the features and target variable.
#' @param label A string specifying the name of the target variable.
#' @param algorithms A vector of algorithm names to use. Default is \code{"all"} to run all supported algorithms.
#' @param test_size A numeric value between 0 and 1 indicating the proportion of the data to use for testing. Default is \code{0.2}.
#' @param resampling_method A string specifying the resampling method for model evaluation. Default is \code{"cv"} (cross-validation).
#'                          Other options include \code{"none"}, \code{"boot"}, \code{"repeatedcv"}, etc.
#' @param folds An integer specifying the number of folds for cross-validation. Default is \code{10} for methods containing "cv" and \code{25} otherwise.
#' @param repeats Number of times to repeat cross-validation (only applicable for methods like "repeatedcv").
#' @param event_class A single string. Either "first" or "second" to specify which level of truth to consider as the "event". Default is "first".
#' @param recipe A user-defined \code{recipe} object for custom preprocessing. If provided, internal recipe steps (imputation, encoding, scaling) are skipped.
#' @param tune_params A list specifying hyperparameter tuning ranges. Default is \code{NULL}.
#' @param metric The performance metric to optimize during training.
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
#' @param tuning_strategy A string specifying the tuning strategy. Options might include \code{"grid"}, \code{"bayes"}, or \code{"none"}. Default is \code{"grid"}.
#' @param tuning_iterations Number of tuning iterations (applicable for Bayesian or other iterative search methods). Default is \code{10}.
#' @param early_stopping Logical indicating whether to use early stopping in Bayesian tuning methods (if supported). Default is \code{FALSE}.
#' @param adaptive Logical indicating whether to use adaptive/racing methods for tuning. Default is \code{FALSE}.
#' @param seed An integer value specifying the random seed for reproducibility.
#' @importFrom magrittr %>%
#' @importFrom rsample initial_split training testing
#' @importFrom recipes recipe step_impute_median step_impute_knn step_impute_bag step_naomit step_dummy step_center step_scale prep bake all_numeric_predictors all_predictors all_nominal_predictors all_outcomes step_zv
#' @importFrom dplyr filter pull rename_with mutate across where select
#' @importFrom stats as.formula
#' @importFrom doFuture registerDoFuture
#' @importFrom future plan multisession sequential
#' @importFrom janitor make_clean_names
#' @importFrom stringr str_detect
#' @return An object of class \code{fastml_model} containing the best model, performance metrics, and other information.
#' @examples
#' # Example 1: Using the iris dataset for binary classification (excluding 'setosa')
#' data(iris)
#' iris <- iris[iris$Species != "setosa", ]  # Binary classification
#' iris$Species <- factor(iris$Species)
#'
#' # Train models with Bayesian optimization
#' model <- fastml(
#'   data = iris,
#'   label = "Species",
#'   algorithms = c("random_forest", "xgboost", "svm_radial")
#' )
#'
#' # View model summary
#' summary(model)
#'
#' # Explain model
#' explain_model(model)
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
                   event_class = "first",
                   exclude = NULL,
                   recipe = NULL,
                   tune_params = NULL,
                   metric = NULL,
                   n_cores = 1,
                   stratify = TRUE,
                   impute_method = "error",
                   encode_categoricals = TRUE,
                   scaling_methods = c("center", "scale"),
                   summaryFunction = NULL,
                   use_default_tuning = FALSE,
                   tuning_strategy = "grid",
                   tuning_iterations = 10,
                   early_stopping = FALSE,
                   adaptive = FALSE,
                   seed = 123) {
  set.seed(seed)
  if (!(label %in% names(data))) {
    stop("The specified label does not exist in the data.")
  }

  if (!is.null(exclude)){
    if (label %in% exclude){
      stop("Label variable cannot be excluded from the data: ", paste(label))
    }

    missing_vars <- setdiff(exclude, colnames(data))

    if (length(missing_vars) > 0) {
      warning("The following variables are not in the dataset: ", paste(missing_vars, collapse = ", "))
      exclude = exclude[!exclude %in% missing_vars]
      if (length(exclude) == 0) {exclude = NULL}
    }

    data <- data %>%
      select(-all_of(exclude))
  }


  data <- data %>%
    mutate(
      across(where(is.character), as.factor),
      across(where(is.integer), as.numeric)
    )

  target_var <- data[[label]]
  label_index <- which(colnames(data) == label)


  if (is.numeric(target_var) && length(unique(target_var)) <= 5) {
    # Convert target_var to factor
    target_var <- as.factor(target_var)
    data[[label]] = as.factor(data[[label]])

    task <- "classification"

    # Issue a warning to inform the user about the change
    warning(sprintf("The target variable '%s' is numeric with %d unique values. It has been converted to a factor and the task has been set to 'classification'.",
                    label, length(unique(target_var))))
  }

  # Define the function to detect special characters
  has_special_chars <- function(name) {
    # Detect any character that is not a letter, number, or underscore
    str_detect(name, "[^a-zA-Z0-9_]")
  }

  # Identify columns with special characters
  columns_with_special_chars <- names(data)[has_special_chars(names(data))]

  # Replace multiple special characters
  data <- data %>%
    rename_with(
      .fn = ~ make_clean_names(
        .x,
        replace = c(
          "\u03bc" = "u",  # Replace mu with 'u'
          ":" = "",         # Remove colons
          "/" = "_",        # Replace slashes with underscores
          " " = "_"         # Replace spaces with underscores
        )
      ),
      .cols = all_of(columns_with_special_chars)
    )

  label <- colnames(data[label_index])


  if (is.factor(target_var) || is.character(target_var) || is.logical(target_var)) {
    task <- "classification"
  } else if (is.numeric(target_var)) {
    task <- "regression"
  } else {
    stop("Unable to detect task type. The target variable must be numeric, factor, character, or logical.")
  }

  if(task == "classification"){
    positive_class <- ifelse(event_class == "first", levels(data[[label]])[1], levels(data[[label]])[2])
  }else{positive_class = NULL}

  if (is.null(metric)) {
    metric <- if (task == "classification") "accuracy" else "rmse"
  }

  # Define allowed metrics for each task
  allowed_metrics_classification <- c("accuracy", "kap", "sens", "spec", "precision", "f_meas", "roc_auc")
  allowed_metrics_regression <- c("rmse", "rsq", "mae")

  # Validate the metric based on the task
  if (task == "classification") {
    if (!(metric %in% allowed_metrics_classification)) {
      stop(paste0("Invalid metric for classification task. Choose one of: ",
                  paste(allowed_metrics_classification, collapse = ", "), "."))
    }
  } else {  # regression
    if (!(metric %in% allowed_metrics_regression)) {
      stop(paste0("Invalid metric for regression task. Choose one of: ",
                  paste(allowed_metrics_regression, collapse = ", "), "."))
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

  if (stratify && task == "classification") {
    split <- initial_split(data, prop = 1 - test_size, strata = all_of(label))
  } else {
    split <- initial_split(data, prop = 1 - test_size)
  }

  train_data <- training(split)
  test_data <- testing(split)

  if (task == "classification") {
    train_data[[label]] <- as.factor(train_data[[label]])
    test_data[[label]] <- factor(test_data[[label]], levels = levels(train_data[[label]]))
  } else {
    train_data[[label]] <- as.numeric(train_data[[label]])
    test_data[[label]] <- as.numeric(test_data[[label]])
  }

  if (is.null(recipe)) {
    recipe <- recipe(as.formula(paste(label, "~ .")), data = train_data)

    # Remove zero-variance predictors
    recipe <- recipe %>%
      step_zv(all_predictors())

    if (impute_method == "medianImpute") {
      recipe <- recipe %>% step_impute_median(all_numeric_predictors())
    } else if (impute_method == "knnImpute") {
      recipe <- recipe %>% step_impute_knn(all_predictors())
    } else if (impute_method == "bagImpute") {
      recipe <- recipe %>% step_impute_bag(all_predictors())
    } else if (impute_method == "remove") {
      recipe <- recipe %>% step_naomit(all_predictors(), skip = TRUE)
    } else if (impute_method == "error" || is.null(impute_method)) {
      # do nothing
    } else {
      stop("Invalid impute_method specified.")
    }

    if (encode_categoricals) {
      recipe <- recipe %>% step_dummy(all_nominal_predictors(), -all_outcomes())
    }

    if (!is.null(scaling_methods)) {
      if ("center" %in% scaling_methods) {
        recipe <- recipe %>% step_center(all_numeric_predictors())
      }
      if ("scale" %in% scaling_methods) {
        recipe <- recipe %>% step_scale(all_numeric_predictors())
      }
    }

    # Do not prep yet
  } else {
    # User provided a recipe, must be untrained
    if (!inherits(recipe, "recipe")) {
      stop("The provided recipe is not a valid recipe object.")
    }
    if (length(recipe$steps) > 0 && any(sapply(recipe$steps, function(x) x$trained))) {
      stop("The provided recipe is already trained. Please supply an untrained recipe.")
    }
  }



  # Set up parallel processing using future
  if (n_cores > 1) {
    if (!requireNamespace("doFuture", quietly = TRUE)) {
      stop("The 'doFuture' package is required for parallel processing but is not installed.")
    }
    if (!requireNamespace("future", quietly = TRUE)) {
      stop("The 'future' package is required for parallel processing but is not installed.")
    }
    registerDoFuture()
    plan(multisession, workers = n_cores)
  } else {
    if (!requireNamespace("future", quietly = TRUE)) {
      stop("The 'future' package is required but is not installed.")
    }
    plan(sequential)
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
    use_default_tuning = use_default_tuning,
    tuning_strategy = tuning_strategy,
    tuning_iterations = tuning_iterations,
    early_stopping = early_stopping,
    adaptive = adaptive
  )

  if (length(models) == 0) {
    stop("No models were successfully trained.")
  }

  eval_output <- evaluate_models(models, train_data, test_data, label, task, metric, event_class)
  performance <- eval_output$performance
  predictions <- eval_output$predictions

  metric_values <- sapply(performance, function(x) x %>% filter(.metric == metric) %>% pull(.estimate))

  if (any(is.na(metric_values))) {
    warning("Some models did not return the specified metric.")
    metric_values[is.na(metric_values)] <- if (task == "regression") Inf else -Inf
  }

  if (all(metric_values == if (task == "regression") Inf else -Inf)) {
    stop("None of the models returned the specified metric.")
  }

  best_model_idx <- if (task == "regression" && metric != "rsq") names(metric_values[metric_values == min(metric_values)]) else names(metric_values[metric_values == max(metric_values)])
  best_model_name <- names(models)[names(models) %in% best_model_idx]

  # Now store processed training data for explainability:
  # Prep and bake the recipe on train_data to store processed_train_data
  trained_recipe <- prep(recipe, training = train_data, retain = TRUE)
  processed_train_data <- bake(trained_recipe, new_data = NULL)

  result <- list(
    best_model = models[best_model_name],
    best_model_name = best_model_name,
    performance = performance,
    predictions = predictions,
    preprocessor = trained_recipe, # Store the trained recipe
    processed_train_data = processed_train_data, # Store processed training data
    label = label,
    task = task,
    models = models,
    metric = metric,
    positive_class = positive_class,
    event_class = event_class
  )
  class(result) <- "fastml_model"
  return(result)
}
