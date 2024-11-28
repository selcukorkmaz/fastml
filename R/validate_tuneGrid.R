#' Validate or Set the Tuning Grid for a Model Specification
#'
#' Ensures that all required hyperparameters are included in the tuning grid.
#' If no tuning grid is provided, it sets a default grid based on the model's parameters.
#'
#' @param tune_grid The user-provided tuning grid (can be NULL).
#' @param model_spec The model specification object.
#' @param default_params A list of default parameters to use if tune_grid is NULL.
#' @param required_params A character vector of required parameter names.
#' @param resampling_enabled Logical indicating if resampling (e.g., cross-validation) is enabled.
#' @importFrom dials grid_regular
#' @importFrom parsnip extract_parameter_set_dials
#' @return A tuning grid data frame.
validate_tuneGrid <- function(tune_grid, model_spec, default_params, required_params, resampling_enabled) {
  # If resampling is disabled, we only need the first set of hyperparameters
  if (!resampling_enabled) {
    if (!is.null(tune_grid)) {
      # Use the first set of hyperparameters from the provided grid
      tune_grid <- tune_grid[1, , drop = FALSE]
    } else {
      # Create a single set of default hyperparameters
      tune_grid <- as.data.frame(lapply(default_params, function(x) x[1]))
    }
  } else {
    if (is.null(tune_grid)) {
      # Create a tuning grid from default parameters
      tune_grid <- grid_regular(
        extract_parameter_set_dials(model_spec),
        levels = 5
      )
    } else {
      # Ensure that all required parameters are included
      missing_params <- setdiff(required_params, names(tune_grid))
      if (length(missing_params) > 0) {
        stop(paste("The following required tuning parameters are missing:", paste(missing_params, collapse = ", ")))
      }
    }
  }
  return(tune_grid)
}
