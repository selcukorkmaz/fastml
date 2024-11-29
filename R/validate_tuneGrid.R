#' Validate or Set the Tuning Grid for a Model Specification
#'
#' Ensures that all required hyperparameters are included in the tuning grid.
#' If no tuning grid is provided, it sets a default grid based on the model's tunable parameters.
#'
#' @param tune_grid The user-provided tuning grid (can be NULL).
#' @param model_spec The model specification object.
#' @param required_params A character vector of required parameter names.
#' @param resampling_enabled Logical indicating if resampling (e.g., cross-validation) is enabled.
#' @importFrom dials grid_regular
#' @importFrom parsnip extract_parameter_set_dials
#' @return A tuning grid data frame or NULL.
validate_tuneGrid <- function(tune_grid, model_spec, required_params, resampling_enabled) {
  # If resampling is disabled, we only need the first set of hyperparameters or default parameters
  if (!resampling_enabled) {
    if (!is.null(tune_grid)) {
      # Use the first set of hyperparameters from the provided grid
      tune_grid <- tune_grid[1, , drop = FALSE]
    } else {
      # No tuning grid provided; use default parameters from the model specification
      tune_grid <- NULL  # Indicate that default parameters should be used
    }
  } else {
    if (is.null(tune_grid)) {
      # Create a tuning grid from the model's tunable parameters
      params <- extract_parameter_set_dials(model_spec)

      # If there are tunable parameters, create a grid; otherwise, set tune_grid to NULL
      if (nrow(params) > 0) {
        tune_grid <- grid_regular(
          params,
          levels = 5
        )
      } else {
        # No tunable parameters; no tuning grid needed
        tune_grid <- NULL
      }
    } else {
      # Ensure that all required parameters are included in the user-provided tuning grid
      missing_params <- setdiff(required_params, names(tune_grid))
      if (length(missing_params) > 0) {
        stop(paste("The following required tuning parameters are missing:", paste(missing_params, collapse = ", ")))
      }
    }
  }
  return(tune_grid)
}
