#' Validate and Complete the tuneGrid
#'
#' Ensures that the tuneGrid includes all required hyperparameters and adjusts it based on cross-validation.
#'
#' @param tuneGrid User-provided tuning grid.
#' @param default_params Default hyperparameter ranges.
#' @param required_params Required hyperparameters for the algorithm.
#' @param is_cv_enabled Logical indicating whether cross-validation is enabled.
#' @return A validated and possibly modified tuneGrid.
#' @export
validate_tuneGrid <- function(tuneGrid, default_params, required_params, is_cv_enabled) {
  # If tuneGrid is NULL and default_params is provided, create tuneGrid
  if (is.null(tuneGrid) && !is.null(default_params)) {
    tuneGrid <- expand.grid(default_params)
  } else if (is.null(tuneGrid)) {
    # If no tuning parameters are required
    tuneGrid <- NULL
  } else {
    # Check for missing required parameters
    if (!is.null(required_params)) {
      missing_params <- setdiff(required_params, names(tuneGrid))
      if (length(missing_params) > 0) {
        # Fill missing required parameters with default values
        for (param in missing_params) {
          if (param %in% names(default_params)) {
            tuneGrid[[param]] <- default_params[[param]][1]
          } else {
            stop(paste("Missing required tuning parameter:", param))
          }
        }
      }
    }
  }

  # If cross-validation is disabled, use only the first set of hyperparameters
  if (!is_cv_enabled && !is.null(tuneGrid)) {
    tuneGrid <- tuneGrid[1, , drop = FALSE]
  }

  return(tuneGrid)
}
