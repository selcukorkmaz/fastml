#' Prepare Data Function
#'
#' Preprocesses the training and testing data with customizable steps.
#'
#' @param train_data Training data frame.
#' @param test_data Testing data frame.
#' @param label Name of the target variable.
#' @param impute_method Method for missing value imputation. Default is \code{"medianImpute"}.
#'                      Options include \code{"medianImpute"}, \code{"knnImpute"}, \code{"bagImpute"}, or \code{NULL} for no imputation.
#' @param encode_categoricals Logical indicating whether to encode categorical variables. Default is \code{TRUE}.
#' @param scaling_methods Vector of scaling methods to apply. Options include \code{"center"}, \code{"scale"}, \code{"range"}, \code{"zv"}, \code{"nzv"}, \code{"YeoJohnson"}, \code{"BoxCox"}.
#'                        Default is \code{c("center", "scale")}.
#' @return A list containing preprocessed training and testing data and the preprocessor object.
#'
#' @importFrom caret dummyVars preProcess
#' @keywords internal
prepare_data <- function(train_data,
                         test_data,
                         label,
                         impute_method = "medianImpute",
                         encode_categoricals = TRUE,
                         scaling_methods = c("center", "scale")) {
  # Load required package
  if (!requireNamespace("caret", quietly = TRUE)) {
    stop("The 'caret' package is required but not installed.")
  }

  # Identify predictor variables
  predictors <- setdiff(names(train_data), label)

  # Initialize preProcess_methods
  preProcess_methods <- c()

  # Add missing value imputation method
  if (!is.null(impute_method)) {
    if (impute_method %in% c("medianImpute", "knnImpute", "bagImpute")) {
      preProcess_methods <- c(preProcess_methods, impute_method)
    } else {
      stop(
        "Invalid impute_method. Choose from 'medianImpute', 'knnImpute', 'bagImpute', or NULL."
      )
    }
  }

  # Add scaling methods
  if (!is.null(scaling_methods)) {
    valid_scaling_methods <-
      c("center",
        "scale",
        "range",
        "zv",
        "nzv",
        "YeoJohnson",
        "BoxCox")
    invalid_methods <-
      setdiff(scaling_methods, valid_scaling_methods)
    if (length(invalid_methods) > 0) {
      stop(paste(
        "Invalid scaling method(s):",
        paste(invalid_methods, collapse = ", ")
      ))
    }
    preProcess_methods <- c(preProcess_methods, scaling_methods)
  }

  # Handle categorical encoding
  if (encode_categoricals) {
    # Convert character columns to factors
    for (col in predictors) {
      if (is.character(train_data[[col]])) {
        train_data[[col]] <- as.factor(train_data[[col]])
        test_data[[col]] <- as.factor(test_data[[col]])
      }
    }

    # Identify factor columns
    factor_cols <- sapply(train_data[predictors], is.factor)
    if (any(factor_cols)) {
      # Use dummyVars for one-hot encoding
      dummies <- dummyVars( ~ ., data = train_data[, predictors], fullRank = TRUE)
      train_data_dummies <- predict(dummies, newdata = train_data)
      test_data_dummies <- predict(dummies, newdata = test_data)

      # Convert to data frames and ensure column names are retained
      train_data_dummies <- as.data.frame(train_data_dummies)
      test_data_dummies <- as.data.frame(test_data_dummies)

      # Combine with label
      train_processed <-
        cbind(train_data_dummies, train_data[[label]])
      test_processed <- cbind(test_data_dummies, test_data[[label]])

      # Rename the label column
      colnames(train_processed)[ncol(train_processed)] <- label
      colnames(test_processed)[ncol(test_processed)] <- label
    } else {
      # No categorical variables, use the data as is
      train_processed <- train_data
      test_processed <- test_data
    }
  } else {
    # Do not encode categoricals
    train_processed <- train_data
    test_processed <- test_data
  }

  # Apply preprocessing steps
  preProcess_steps <- preProcess(train_processed[, setdiff(names(train_processed), label)],
                                 method = preProcess_methods)

  # Apply preprocessing to training data
  train_processed_pp <- predict(preProcess_steps, train_processed)

  # Apply preprocessing to testing data
  test_processed_pp <- predict(preProcess_steps, test_processed)

  return(
    list(
      train = train_processed_pp,
      test = test_processed_pp,
      preprocessor = preProcess_steps
    )
  )
}
