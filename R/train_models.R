utils::globalVariables("twoClassSummary")

#' Train Specified Machine Learning Algorithms on the Training Data
#'
#' Trains specified machine learning algorithms on the preprocessed training data.
#'
#' @param train_data Preprocessed training data frame.
#' @param label Name of the target variable.
#' @param task Type of task: "classification" or "regression".
#' @param algorithms Vector of algorithm names to train.
#' @param resampling_method Resampling method for cross-validation (e.g., "cv", "repeatedcv").
#' @param folds Number of folds for cross-validation.
#' @param repeats Number of times to repeat cross-validation (only applicable for methods like "repeatedcv").
#' @param tune_params List of hyperparameter tuning ranges.
#' @param metric The performance metric to optimize.
#' @param summaryFunction A custom summary function for model evaluation. Default is \code{NULL}.
#' @param seed An integer value specifying the random seed for reproducibility.
#' @return A list of trained model objects.
#'
#' @importFrom caret train trainControl defaultSummary
#' @importFrom stats as.formula predict binomial
#' @export
train_models <-
  function(train_data,
           label,
           task,
           algorithms,
           resampling_method,
           folds,
           repeats,
           tune_params,
           metric,
           summaryFunction = NULL,
           seed = 123) {
    # Load required packages
    if (!requireNamespace("caret", quietly = TRUE)) {
      stop("The 'caret' package is required but not installed.")
    }
    if (metric == "ROC") {
      if (!requireNamespace("pROC", quietly = TRUE)) {
        stop("The 'pROC' package is required for ROC metric but is not installed.")
      }
    }

    # Decide on classProbs based on whether probabilities are needed
    # For metrics like ROC, class probabilities are needed
    if (task == "classification" && metric == "ROC") {
      class_probs <- TRUE
      if (is.null(summaryFunction)) {
        summary_func <- caret::twoClassSummary  # Use twoClassSummary for ROC
      } else {
        summary_func <- summaryFunction
      }
    } else {
      class_probs <- FALSE
      if (is.null(summaryFunction)) {
        summary_func <- caret::defaultSummary
      } else {
        summary_func <- summaryFunction
      }
    }

    # Set up trainControl
    control_args <- list(
      method = resampling_method,
      number = folds,
      repeats = repeats,
      search = "grid",
      verboseIter = FALSE,
      classProbs = class_probs,
      summaryFunction = summary_func,
      allowParallel = TRUE,
      savePredictions = "final"
    )

    control <- do.call(caret::trainControl, control_args)

    # Determine available metrics from the summaryFunction
    available_metrics <- get_available_metrics(summary_func, train_data, label, task)

    # Validate the metric
    if (!(metric %in% available_metrics)) {
      stop(
        paste(
          "Unsupported metric. The available metrics are:",
          paste(available_metrics, collapse = ", ")
        )
      )
    }

    # Define required tuning parameters for each algorithm
    required_tuning_params <- list(
      neural_network = c("size", "decay"),
      deep_learning = c("epochs", "batch_size", "units", "activation"),
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
      lightgbm = c(
        "num_leaves",
        "learning_rate",
        "n_estimators",
        "min_data_in_leaf",
        "feature_fraction"
      ),
      catboost = c(
        "iterations",
        "learning_rate",
        "depth",
        "l2_leaf_reg"
      ),
      logitboost = c("nIter"),
      decision_tree = c("cp"),
      c5.0 = c("model", "trials", "winnow"),
      pls = c("ncomp"),
      glmboost = c("mstop", "prune"),
      stacking = NULL,
      blending = NULL,
      voting = NULL
      # Add other algorithms and their required parameters as needed
    )

    # Initialize model list
    models <- list()

    # Inform the user if cross-validation is disabled
    if (resampling_method == "none") {
      message("Cross-validation is disabled ('resampling_method' = 'none'). Only the first set of hyperparameters will be used for each model.")
    }

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
        # Train the model based on the algorithm and task
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
            resampling_method != "none"
          )

          model <- caret::train(
            formula,
            data = train_data,
            method = "nnet",
            trControl = control,
            tuneGrid = tuneGrid,
            metric = metric,
            trace = FALSE,
            maxit = 200  # Optional: Increase max iterations if needed
          )

        } else if (algo == "deep_learning") {
          # Keras (Deep Learning)
          if (!requireNamespace("keras", quietly = TRUE)) {
            stop("The 'keras' package is required for deep learning but is not installed.")
          }

          # Define default tuning parameters
          default_params <- list(
            epochs = c(10, 20),
            batch_size = c(32, 64),
            units = c(64, 128),
            activation = c("relu")
          )

          # Validate or set tuneGrid
          tuneGrid <- validate_tuneGrid(
            tuneGrid,
            default_params,
            required_tuning_params[[algo]],
            resampling_method != "none"
          )

          # Use the custom model info
          model <- caret::train(
            formula,
            data = train_data,
            method = kerasModelInfo,
            trControl = control,
            tuneGrid = tuneGrid,
            metric = metric
          )


        }  else if (algo == "lightgbm") {
          # LightGBM
          if (!requireNamespace("lightgbm", quietly = TRUE)) {
            stop("The 'lightgbm' package is required for LightGBM but is not installed.")
          }

          # Define default tuning parameters
          default_params <- list(
            num_leaves = c(31, 63),
            learning_rate = c(0.01, 0.1),
            n_estimators = c(100, 200),
            min_data_in_leaf = c(20, 50),
            feature_fraction = c(0.6, 0.8, 1.0)
          )

          # Validate or set tuneGrid
          tuneGrid <- validate_tuneGrid(
            tuneGrid,
            default_params,
            required_tuning_params[[algo]],
            resampling_method != "none"
          )

          # Use the custom model info
          model <- caret::train(
            formula,
            data = train_data,
            method = lightgbmModelInfo,
            trControl = control,
            tuneGrid = tuneGrid,
            metric = metric
          )

        } else if (algo == "catboost") {
          # CatBoost
          if (!requireNamespace("catboost", quietly = TRUE)) {
            stop("The 'catboost' package is required for CatBoost but is not installed.")
          }

          # Define default tuning parameters
          default_params <- list(
            iterations = c(100, 200),
            learning_rate = c(0.01, 0.1),
            depth = c(4, 6, 8),
            l2_leaf_reg = c(1, 3, 5)
          )

          # Validate or set tuneGrid
          tuneGrid <- validate_tuneGrid(
            tuneGrid,
            default_params,
            required_tuning_params[[algo]],
            resampling_method != "none"
          )

          # Use the custom model info
          model <- caret::train(
            formula,
            data = train_data,
            method = catboostModelInfo,
            trControl = control,
            tuneGrid = tuneGrid,
            metric = metric
          )
        } else if (algo == "stacking") {
          # Stacking Ensemble
          # Requires at least two base models
          if (length(models) < 2) {
            warning("Stacking requires at least two base models. Skipping stacking.")
            next
          }

          model <- create_stacking_model(models, train_data, label, task, metric, control, seed)

        } else if (algo == "blending") {
          # Blending Ensemble
          # Requires at least two base models
          if (length(models) < 2) {
            warning("Blending requires at least two base models. Skipping blending.")
            next
          }

          model <- create_blending_model(models, train_data, label, task, metric, control, seed)

        } else if (algo == "voting") {
          # Voting Ensemble
          # Requires at least two base models
          if (length(models) < 2) {
            warning("Voting requires at least two base models. Skipping voting.")
            next
          }

          model <- create_voting_model(models, train_data, label, task, metric, control, seed)

        } else if (algo == "svm_linear") {
          # SVM with linear kernel
          # Define default tuning parameters
          default_params <- list(C = c(0.1, 1, 10))

          # Validate or set tuneGrid
          tuneGrid <- validate_tuneGrid(
            tuneGrid,
            default_params,
            required_tuning_params[[algo]],
            resampling_method != "none"
          )

          method_name <- "svmLinear"

          model <- caret::train(
            formula,
            data = train_data,
            method = method_name,
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
            resampling_method != "none"
          )

          method_name <- "svmRadial"

          model <- caret::train(
            formula,
            data = train_data,
            method = method_name,
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
            resampling_method != "none"
          )

          method_name <- "knn"

          model <- caret::train(
            formula,
            data = train_data,
            method = method_name,
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
            resampling_method != "none"
          )

          method_name <- "xgbTree"

          model <- caret::train(
            formula,
            data = train_data,
            method = method_name,
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
                                        resampling_method != "none")

          method_name <- "rf"

          model <- caret::train(
            formula,
            data = train_data,
            method = method_name,
            trControl = control,
            tuneGrid = tuneGrid,
            metric = metric
          )

        } else if (algo == "linear_regression") {
          # Linear Regression (for regression task)
          if (task == "regression") {
            model <- caret::train(
              formula,
              data = train_data,
              method = "lm",
              trControl = control,
              metric = metric
            )
          } else {
            warning("Algorithm 'linear_regression' is not applicable for classification tasks.")
            next
          }

        } else if (algo == "ridge_regression") {
          # Ridge Regression
          if (task == "regression") {
            default_params <- list(lambda = seq(0, 1, length = 10))
            tuneGrid <- validate_tuneGrid(
              tuneGrid,
              default_params,
              NULL,
              resampling_method != "none"
            )
            model <- caret::train(
              formula,
              data = train_data,
              method = "ridge",
              trControl = control,
              tuneGrid = tuneGrid,
              metric = metric
            )
          } else {
            warning("Algorithm 'ridge_regression' is not applicable for classification tasks.")
            next
          }

        } else if (algo == "lasso_regression") {
          # Lasso Regression
          if (task == "regression") {
            default_params <- list(fraction = seq(0.1, 1, length = 10))
            tuneGrid <- validate_tuneGrid(
              tuneGrid,
              default_params,
              NULL,
              resampling_method != "none"
            )
            model <- caret::train(
              formula,
              data = train_data,
              method = "lasso",
              trControl = control,
              tuneGrid = tuneGrid,
              metric = metric
            )
          } else {
            warning("Algorithm 'lasso_regression' is not applicable for classification tasks.")
            next
          }

        } else if (algo == "elastic_net") {
          # Elastic Net
          default_params <- list(alpha = seq(0, 1, by = 0.1),
                                 lambda = seq(0.0001, 0.1, length = 10))

          # No specific required tuning parameters
          tuneGrid <- validate_tuneGrid(tuneGrid,
                                        default_params,
                                        NULL,
                                        resampling_method != "none")

          model <- caret::train(
            formula,
            data = train_data,
            method = "glmnet",
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
            resampling_method != "none"
          )

          model <- caret::train(
            formula,
            data = train_data,
            method = "rpart",
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
            resampling_method != "none"
          )

          model <- caret::train(
            formula,
            data = train_data,
            method = "gbm",
            trControl = control,
            tuneGrid = tuneGrid,
            metric = metric,
            verbose = FALSE
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
            resampling_method != "none"
          )

          model <- caret::train(
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
            resampling_method != "none"
          )

          model <- caret::train(
            formula,
            data = train_data,
            method = "glmboost",
            trControl = control,
            tuneGrid = tuneGrid,
            metric = metric
          )

        } else if (algo == "logistic_regression") {
          # Logistic Regression (for classification)
          if (task == "classification") {
            model <- caret::train(
              formula,
              data = train_data,
              method = "glm",
              family = binomial(),
              trControl = control,
              metric = metric
            )
          } else {
            warning("Algorithm 'logistic_regression' is not applicable for regression tasks.")
            next
          }

        } else if (algo == "penalized_logistic_regression") {
          # Penalized Logistic Regression (glmnet)
          if (task == "classification") {
            # Define default tuning parameters
            default_params <- list(alpha = c(0, 0.5, 1),
                                   lambda = seq(0.0001, 0.1, length = 10))

            # No specific required tuning parameters
            tuneGrid <- validate_tuneGrid(
              tuneGrid,
              default_params,
              NULL,
              resampling_method != "none"
            )

            model <- caret::train(
              formula,
              data = train_data,
              method = "glmnet",
              family = "binomial",
              trControl = control,
              tuneGrid = tuneGrid,
              metric = metric
            )
          } else {
            warning("Algorithm 'penalized_logistic_regression' is not applicable for regression tasks.")
            next
          }

        } else if (algo == "naive_bayes") {
          # Naive Bayes (classification only)
          if (task == "classification") {
            model <- caret::train(
              formula,
              data = train_data,
              method = "nb",
              trControl = control,
              metric = metric
            )
          } else {
            warning("Algorithm 'naive_bayes' is not applicable for regression tasks.")
            next
          }

        } else if (algo == "lda") {
          # Linear Discriminant Analysis (classification only)
          if (task == "classification") {
            model <- caret::train(
              formula,
              data = train_data,
              method = "lda",
              trControl = control,
              metric = metric
            )
          } else {
            warning("Algorithm 'lda' is not applicable for regression tasks.")
            next
          }

        } else if (algo == "qda") {
          # Quadratic Discriminant Analysis (classification only)
          if (task == "classification") {
            model <- caret::train(
              formula,
              data = train_data,
              method = "qda",
              trControl = control,
              metric = metric
            )
          } else {
            warning("Algorithm 'qda' is not applicable for regression tasks.")
            next
          }

        } else if (algo == "bagging") {
          # Bagging (treebag)
          model <- caret::train(
            formula,
            data = train_data,
            method = "treebag",
            trControl = control,
            metric = metric
          )

        } else if (algo == "logitboost") {
          # LogitBoost (classification only)
          if (task == "classification") {
            # Define default tuning parameters
            default_params <- list(nIter = c(50, 100))

            # Required tuning parameters
            tuneGrid <- validate_tuneGrid(
              tuneGrid,
              default_params,
              required_tuning_params[[algo]],
              resampling_method != "none"
            )

            model <- caret::train(
              formula,
              data = train_data,
              method = "LogitBoost",
              trControl = control,
              tuneGrid = tuneGrid,
              metric = metric
            )
          } else {
            warning("Algorithm 'logitboost' is not applicable for regression tasks.")
            next
          }

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

# Helper function to get available metrics from the summaryFunction
get_available_metrics <- function(summaryFunction, train_data, label, task) {
  # Create a small sample dataset
  sample_data <- train_data[1:2, , drop = FALSE]
  sample_obs <- sample_data[[label]]

  if (task == "classification") {
    sample_pred <- sample_obs  # Use the same values for simplicity

    # Create a data frame for summaryFunction
    data <- data.frame(obs = sample_obs, pred = sample_pred)

    if (inherits(summaryFunction, "function")) {
      res <- summaryFunction(data = data, lev = levels(sample_obs), model = NULL)
      return(names(res))
    } else {
      # Use defaultSummary
      res <- caret::defaultSummary(data = data)
      return(names(res))
    }
  } else if (task == "regression") {
    sample_pred <- sample_obs + rnorm(length(sample_obs))  # Add small noise

    # Create a data frame for summaryFunction
    data <- data.frame(obs = sample_obs, pred = sample_pred)

    if (inherits(summaryFunction, "function")) {
      res <- summaryFunction(data = data, lev = NULL, model = NULL)
      return(names(res))
    } else {
      # Use defaultSummary
      res <- caret::defaultSummary(data = data)
      return(names(res))
    }
  } else {
    stop("Invalid task type.")
  }
}

