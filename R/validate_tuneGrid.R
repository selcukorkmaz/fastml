#' Validate and Complete the tuneGrid
#'
#' Ensures that the tuneGrid includes all required hyperparameters and adjusts it based on cross-validation.
#'
#' @param tuneGrid User-provided tuning grid.
#' @param default_params Default hyperparameter ranges.
#' @param required_params Required hyperparameters for the algorithm.
#' @param resampling_method Logical indicating whether cross-validation is enabled.
#' @return A validated and possibly modified tuneGrid.
#' @export
validate_tuneGrid <- function(tuneGrid,
                              default_params,
                              required_params,
                              resampling_method) {
  if (!is.null(tuneGrid)) {
    # Check for missing parameters
    if (!is.null(required_params)) {
      missing_params <- setdiff(required_params, names(tuneGrid))
      if (length(missing_params) > 0) {
        # Add default values for missing parameters
        for (param in missing_params) {
          tuneGrid[[param]] <- default_params[[param]]
        }
      }
    }
    # Expand the grid to create all combinations
    tuneGrid <- expand.grid(tuneGrid)
  } else {
    # Use default tuning parameters
    tuneGrid <- expand.grid(default_params)
  }

  # If resampling_method is FALSE, ensure only one set of hyperparameters
  if (!resampling_method && nrow(tuneGrid) > 1) {
    warning(
      "Cross-validation is disabled. Using the first hyperparameter combination in tuneGrid."
    )
    tuneGrid <- tuneGrid[1, , drop = FALSE]
  }

  return(tuneGrid)
}
