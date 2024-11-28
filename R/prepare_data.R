#' Prepare Data Function
#'
#' Preprocesses the training and testing data with customizable steps using the `recipes` package.
#'
#' @param train_data Training data frame.
#' @param test_data Testing data frame.
#' @param label Name of the target variable.
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
#' @param encode_categoricals Logical indicating whether to encode categorical variables using one-hot encoding. Default is \code{TRUE}.
#' @param scaling_methods Vector of scaling methods to apply. Options include \code{"center"}, \code{"scale"}, \code{"range"}, \code{"zv"}, \code{"nzv"}, \code{"YeoJohnson"}, \code{"BoxCox"}.
#'                        Default is \code{c("center", "scale")}.
#' @return A list containing preprocessed training and testing data and the recipe object.
#'
#' @importFrom magrittr %>%
#' @importFrom recipes recipe step_medianimpute step_knnimpute step_bagimpute step_naomit step_dummy step_string2factor step_center step_scale step_range step_zv step_nzv step_YeoJohnson step_BoxCox prep bake
#' @importFrom dplyr bind_rows
#' @keywords internal
prepare_data <- function(train_data,
                         test_data,
                         label,
                         impute_method = "error",
                         encode_categoricals = TRUE,
                         scaling_methods = c("center", "scale")) {
  # Load required packages
  if (!requireNamespace("recipes", quietly = TRUE)) {
    stop("The 'recipes' package is required but not installed.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("The 'dplyr' package is required but not installed.")
  }

  # Combine train and test data for consistent factor levels (if needed)
  combined_data <- dplyr::bind_rows(train_data, test_data)

  # Start recipe
  rec <- recipes::recipe(as.formula(paste(label, "~ .")), data = combined_data)

  # Handle missing value imputation method
  if (!is.null(impute_method)) {
    if (impute_method == "medianImpute") {
      rec <- rec %>% recipes::step_medianimpute(all_numeric(), -all_outcomes())
    } else if (impute_method == "knnImpute") {
      rec <- rec %>% recipes::step_knnimpute(all_predictors(), neighbors = 5)
    } else if (impute_method == "bagImpute") {
      rec <- rec %>% recipes::step_bagimpute(all_predictors())
    } else if (impute_method == "remove") {
      rec <- rec %>% recipes::step_naomit(all_predictors(), skip = TRUE)
    } else if (impute_method == "error" || is.null(impute_method)) {
      # Do not perform imputation, will check for missing values later
    } else {
      stop(
        "Invalid impute_method. Choose from 'medianImpute', 'knnImpute', 'bagImpute', 'remove', 'error', or NULL."
      )
    }
  }

  # Handle categorical encoding
  if (encode_categoricals) {
    # Convert character columns to factors
    rec <- rec %>%
      recipes::step_string2factor(all_nominal_predictors())

    # Use one-hot encoding for categorical variables
    rec <- rec %>%
      recipes::step_dummy(all_nominal_predictors(), one_hot = TRUE)
  } else {
    # Ensure character columns are converted to factors
    rec <- rec %>%
      recipes::step_string2factor(all_nominal_predictors())
  }

  # Add scaling methods
  if (!is.null(scaling_methods)) {
    valid_scaling_methods <- c("center", "scale", "range", "zv", "nzv", "YeoJohnson", "BoxCox")
    invalid_methods <- setdiff(scaling_methods, valid_scaling_methods)
    if (length(invalid_methods) > 0) {
      stop(paste(
        "Invalid scaling method(s):",
        paste(invalid_methods, collapse = ", ")
      ))
    }
    # Add the corresponding steps
    if ("center" %in% scaling_methods) {
      rec <- rec %>% recipes::step_center(all_numeric_predictors())
    }
    if ("scale" %in% scaling_methods) {
      rec <- rec %>% recipes::step_scale(all_numeric_predictors())
    }
    if ("range" %in% scaling_methods) {
      rec <- rec %>% recipes::step_range(all_numeric_predictors())
    }
    if ("zv" %in% scaling_methods) {
      rec <- rec %>% recipes::step_zv(all_predictors())
    }
    if ("nzv" %in% scaling_methods) {
      rec <- rec %>% recipes::step_nzv(all_predictors())
    }
    if ("YeoJohnson" %in% scaling_methods) {
      rec <- rec %>% recipes::step_YeoJohnson(all_numeric_predictors())
    }
    if ("BoxCox" %in% scaling_methods) {
      rec <- rec %>% recipes::step_BoxCox(all_numeric_predictors())
    }
  }

  # Prepare the recipe using the training data
  rec <- rec %>% recipes::prep(data = train_data)

  # Apply preprocessing to training data
  train_processed <- recipes::bake(rec, new_data = train_data)

  # Apply preprocessing to testing data
  test_processed <- recipes::bake(rec, new_data = test_data)

  # Check for missing values if impute_method is "error" or NULL
  if (impute_method == "error" || is.null(impute_method)) {
    if (any(is.na(train_processed))) {
      stop("Missing values detected in the training data after preprocessing. Please handle missing values or choose an imputation method.")
    }
    if (any(is.na(test_processed))) {
      stop("Missing values detected in the testing data after preprocessing. Please handle missing values or choose an imputation method.")
    }
  }

  return(
    list(
      train = train_processed,
      test = test_processed,
      recipe = rec
    )
  )
}
