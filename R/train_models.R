#' Train Specified Machine Learning Algorithms on the Training Data
#'
#' Trains specified machine learning algorithms on the preprocessed training data.
#'
#' @param train_data Preprocessed training data frame.
#' @param label Name of the target variable.
#' @param algorithms Vector of algorithm names to train.
#' @param resampling_method Resampling method for cross-validation (e.g., "cv", "repeatedcv").
#' @param folds Number of folds for cross-validation.
#' @param tune_params List of hyperparameter tuning ranges.
#' @param metric The performance metric to optimize.
#' @param summaryFunction A custom summary function for model evaluation. Default is \code{NULL}.
#' @param seed An integer value specifying the random seed for reproducibility.
#' @return A list of trained model objects.
#'
#' @importFrom caret train trainControl defaultSummary
#' @importFrom stats as.formula predict binomial
#'
#' @export
train_models <-
  function(train_data,
           label,
           algorithms,
           resampling_method,
           folds,
           tune_params,
           metric,
           summaryFunction = NULL,
           seed = 123) {
    # Load required packages
    requireNamespace("caret", quietly = TRUE)

    # Ensure the target variable is a factor
    train_data[[label]] <- as.factor(train_data[[label]])

    # Decide on classProbs based on whether probabilities are needed
    # For metrics like MCC, class probabilities are not needed
    class_probs <- FALSE

    # Use the custom summary function
    if (is.null(summaryFunction)) {
      summary_func <- defaultSummary
    } else {
      summary_func <- summaryFunction
    }

    # Set up trainControl
    control <- trainControl(
      method = resampling_method,
      number = folds,
      search = "grid",
      verboseIter = FALSE,
      classProbs = class_probs,
      summaryFunction = summary_func,
      allowParallel = TRUE,
      savePredictions = "final"
    )

    # Define required tuning parameters for each algorithm
    required_tuning_params <- list(
      neural_network = c("size", "decay"),
      svm_linear = c("C"),
      svm_radial = c("sigma", "C"),
      knn = c("k"),
      gbm = c(
        "n.trees",
        "interaction.depth",
        "shrinkage",
        "n.minobsinnode"
      ),
      xgboost = c(
        "nrounds",
        "max_depth",
        "eta",
        "gamma",
        "colsample_bytree",
        "min_child_weight",
        "subsample"
      ),
      logitboost = c("nIter"),
      decision_tree = c("cp"),
      c5.0 = c("model", "trials", "winnow"),
      pls = c("ncomp"),
      glmboost = c("mstop", "prune")
      # Add other algorithms and their required parameters as needed
    )

    # Initialize model list
    models <- list()

    # Loop over each algorithm
    for (algo in algorithms) {
      set.seed(seed)  # For reproducibility
      model <- NULL  # Initialize model variable

      # Define the model formula
      formula <- as.formula(paste(label, "~ ."))

      # Retrieve tuning parameters if provided
      if (!is.null(tune_params) && !is.null(tune_params[[algo]])) {
        tuneGrid <- tune_params[[algo]]
      } else {
        tuneGrid <- NULL
      }

      # Train the model within a tryCatch block
      tryCatch({
        # Train the model based on the algorithm
        if (algo == "neural_network") {
          # Neural Network (nnet)
          # Define default tuning parameters
          default_params <- list(size = c(3, 5, 7),
                                 decay = c(0, 0.01, 0.1))

          # Validate or set tuneGrid
          tuneGrid <- validate_tuneGrid(
            tuneGrid,
            default_params,
            required_tuning_params[[algo]],
            resampling_method = (resampling_method != "none")
          )

          model <- train(
            formula,
            data = train_data,
            method = "nnet",
            trControl = control,
            tuneGrid = tuneGrid,
            metric = metric,
            trace = FALSE,
            maxit = 200  # Optional: Increase max iterations if needed
          )

        } else if (algo == "svm_linear") {
          # SVM with linear kernel
          # Define default tuning parameters
          default_params <- list(C = c(0.1, 1, 10))

          # Validate or set tuneGrid
          tuneGrid <- validate_tuneGrid(
            tuneGrid,
            default_params,
            required_tuning_params[[algo]],
            resampling_method = (resampling_method != "none")
          )

          model <- train(
            formula,
            data = train_data,
            method = "svmLinear",
            trControl = control,
            tuneGrid = tuneGrid,
            metric = metric
          )

        } else if (algo == "svm_radial") {
          # SVM with radial kernel
          # Define default tuning parameters
          default_params <- list(sigma = c(0.01, 0.1, 1),
                                 C = c(0.1, 1, 10))

          # Validate or set tuneGrid
          tuneGrid <- validate_tuneGrid(
            tuneGrid,
            default_params,
            required_tuning_params[[algo]],
            resampling_method = (resampling_method != "none")
          )

          model <- train(
            formula,
            data = train_data,
            method = "svmRadial",
            trControl = control,
            tuneGrid = tuneGrid,
            metric = metric
          )

        } else if (algo == "knn") {
          # k-Nearest Neighbors (knn)
          # Define default tuning parameters
          default_params <- list(k = 1:10)

          # Validate or set tuneGrid
          tuneGrid <- validate_tuneGrid(
            tuneGrid,
            default_params,
            required_tuning_params[[algo]],
            resampling_method = (resampling_method != "none")
          )

          model <- train(
            formula,
            data = train_data,
            method = "knn",
            trControl = control,
            tuneGrid = tuneGrid,
            metric = metric
          )

        } else if (algo == "xgboost") {
          # Extreme Gradient Boosting (xgboost)
          # Define default tuning parameters
          default_params <- list(
            nrounds = 100,
            max_depth = c(3, 5, 7),
            eta = c(0.01, 0.1, 0.3),
            gamma = c(0, 1),
            colsample_bytree = c(0.6, 0.8, 1.0),
            min_child_weight = c(1, 5, 10),
            subsample = c(0.6, 0.8, 1.0)
          )

          # Validate or set tuneGrid
          tuneGrid <- validate_tuneGrid(
            tuneGrid,
            default_params,
            required_tuning_params[[algo]],
            resampling_method = (resampling_method != "none")
          )

          model <- train(
            formula,
            data = train_data,
            method = "xgbTree",
            trControl = control,
            tuneGrid = tuneGrid,
            metric = metric,
            verbose = FALSE
          )

        } else if (algo == "random_forest") {
          # Random Forest
          # Define default tuning parameters
          default_params <-
            list(mtry = floor(sqrt(ncol(train_data) - 1)))

          # No required tuning parameters for random forest
          tuneGrid <- validate_tuneGrid(tuneGrid,
                                        default_params,
                                        NULL,
                                        resampling_method = (resampling_method != "none"))

          model <- train(
            formula,
            data = train_data,
            method = "rf",
            trControl = control,
            tuneGrid = tuneGrid,
            metric = metric
          )

        } else if (algo == "logistic_regression") {
          # Logistic Regression (GLM)
          model <- train(
            formula,
            data = train_data,
            method = "glm",
            family = binomial(),
            trControl = control,
            metric = metric
          )

        } else if (algo == "penalized_logistic_regression") {
          # Penalized Logistic Regression (glmnet)
          # Define default tuning parameters
          default_params <- list(alpha = c(0, 0.5, 1),
                                 # Elastic net mixing parameter
                                 lambda = seq(0.0001, 0.1, length = 10))

          # No specific required tuning parameters
          tuneGrid <- validate_tuneGrid(tuneGrid,
                                        default_params,
                                        NULL,
                                        resampling_method = (resampling_method != "none"))

          model <- train(
            formula,
            data = train_data,
            method = "glmnet",
            family = "binomial",
            trControl = control,
            tuneGrid = tuneGrid,
            metric = metric
          )

        } else if (algo == "decision_tree") {
          # Decision Tree (rpart)
          # Define default tuning parameters
          default_params <- list(cp = seq(0.0, 0.05, by = 0.01))

          # Required tuning parameters
          tuneGrid <- validate_tuneGrid(
            tuneGrid,
            default_params,
            required_tuning_params[[algo]],
            resampling_method = (resampling_method != "none")
          )

          model <- train(
            formula,
            data = train_data,
            method = "rpart",
            trControl = control,
            tuneGrid = tuneGrid,
            metric = metric
          )

        } else if (algo == "c5.0") {
          # Decision Tree (C5.0)
          # Define default tuning parameters
          default_params <- list(
            model = c("tree", "rules"),
            trials = c(1, 5, 10),
            winnow = c(TRUE, FALSE)
          )

          # Required tuning parameters
          tuneGrid <- validate_tuneGrid(
            tuneGrid,
            default_params,
            required_tuning_params[[algo]],
            resampling_method = (resampling_method != "none")
          )

          model <- train(
            formula,
            data = train_data,
            method = "C5.0",
            trControl = control,
            tuneGrid = tuneGrid,
            metric = metric
          )

        } else if (algo == "gbm") {
          # Gradient Boosting Machine
          # Define default tuning parameters
          default_params <- list(
            n.trees = c(100, 200),
            interaction.depth = c(1, 3, 5),
            shrinkage = c(0.1, 0.01),
            n.minobsinnode = 10
          )

          # Required tuning parameters
          tuneGrid <- validate_tuneGrid(
            tuneGrid,
            default_params,
            required_tuning_params[[algo]],
            resampling_method = (resampling_method != "none")
          )

          model <- train(
            formula,
            data = train_data,
            method = "gbm",
            trControl = control,
            tuneGrid = tuneGrid,
            metric = metric,
            verbose = FALSE
          )

        } else if (algo == "naive_bayes") {
          # Naive Bayes
          model <- train(
            formula,
            data = train_data,
            method = "nb",
            trControl = control,
            metric = metric
          )

        } else if (algo == "lda") {
          # Linear Discriminant Analysis
          model <- train(
            formula,
            data = train_data,
            method = "lda",
            trControl = control,
            metric = metric
          )

        } else if (algo == "qda") {
          # Quadratic Discriminant Analysis
          model <- train(
            formula,
            data = train_data,
            method = "qda",
            trControl = control,
            metric = metric
          )

        } else if (algo == "bagging") {
          # Bagging (treebag)
          model <- train(
            formula,
            data = train_data,
            method = "treebag",
            trControl = control,
            metric = metric
          )

        } else if (algo == "logitboost") {
          # LogitBoost
          # Define default tuning parameters
          default_params <- list(nIter = c(50, 100))

          # Required tuning parameters
          tuneGrid <- validate_tuneGrid(
            tuneGrid,
            default_params,
            required_tuning_params[[algo]],
            resampling_method = (resampling_method != "none")
          )

          model <- train(
            formula,
            data = train_data,
            method = "LogitBoost",
            trControl = control,
            tuneGrid = tuneGrid,
            metric = metric
          )

        } else if (algo == "elastic_net") {
          # Elastic Net (glmnet)
          # Define default tuning parameters
          default_params <- list(alpha = seq(0, 1, by = 0.1),
                                 lambda = seq(0.0001, 0.1, length = 10))

          # No specific required tuning parameters
          tuneGrid <- validate_tuneGrid(tuneGrid,
                                        default_params,
                                        NULL,
                                        resampling_method = (resampling_method != "none"))

          model <- train(
            formula,
            data = train_data,
            method = "glmnet",
            trControl = control,
            tuneGrid = tuneGrid,
            metric = metric
          )

        } else if (algo == "bayes_glm") {
          # Bayesian Generalized Linear Model
          model <- train(
            formula,
            data = train_data,
            method = "bayesglm",
            trControl = control,
            metric = metric
          )

        } else if (algo == "pls") {
          # Partial Least Squares
          # Define default tuning parameters
          default_params <- list(ncomp = 1:5)

          # Required tuning parameters
          tuneGrid <- validate_tuneGrid(
            tuneGrid,
            default_params,
            required_tuning_params[[algo]],
            resampling_method = (resampling_method != "none")
          )

          model <- train(
            formula,
            data = train_data,
            method = "pls",
            trControl = control,
            tuneGrid = tuneGrid,
            metric = metric
          )

        } else if (algo == "glmboost") {
          # Boosted Generalized Linear Model
          # Define default tuning parameters
          default_params <- list(mstop = c(50, 100),
                                 prune = c("yes", "no"))

          # Required tuning parameters
          tuneGrid <- validate_tuneGrid(
            tuneGrid,
            default_params,
            required_tuning_params[[algo]],
            resampling_method = (resampling_method != "none")
          )

          model <- train(
            formula,
            data = train_data,
            method = "glmboost",
            trControl = control,
            tuneGrid = tuneGrid,
            metric = metric
          )

        } else {
          warning(paste("Algorithm", algo, "is not supported or failed to train."))
          next
        }

        # Store the trained model if successful
        models[[algo]] <- model

      }, error = function(e) {
        warning(paste(
          "Training failed for algorithm:",
          algo,
          "\nError message:",
          e$message
        ))
        # Continue to the next algorithm
      })
    }

    return(models)
  }

