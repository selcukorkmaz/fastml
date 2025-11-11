#' Guard utilities for enforcing the Guarded Resampling Principle
#'
#' These helpers coordinate guard rails that ensure preprocessing and model
#' fitting do not accidentally use the full dataset when resampling is active.
#' Whenever cross-validation (or any rsample-based routine) is requested, the
#' guard is activated before any preprocessing happens. Any attempt to carry out
#' a guarded operation on the full dataset while the guard is active results in
#' an error, forcing callers to perform the work inside the fold-specific
#' analysis sets instead.
fastml_resampling_guard <- new.env(parent = emptyenv())

fastml_resampling_guard$active <- FALSE
fastml_resampling_guard$context <- NULL

#' Activate guarded mode for the provided context (e.g. "cv", "boot").
fastml_guard_activate <- function(context) {
  fastml_resampling_guard$active <- TRUE
  fastml_resampling_guard$context <- context
  invisible(TRUE)
}

#' Deactivate guarded mode after resampling has finished.
fastml_guard_deactivate <- function() {
  fastml_resampling_guard$active <- FALSE
  fastml_resampling_guard$context <- NULL
  invisible(TRUE)
}

#' Check whether the current operation is allowed under the active guard.
#'
#' @param scope Character string describing the data scope the operation is
#'   about to touch. Use "global" when the computation would involve the full
#'   training set prior to fold splitting. Use "fold" when the computation is
#'   scoped to an analysis subset.
#' @param operation Human readable description used when raising the error.
fastml_guard_check <- function(scope, operation) {
  if (!isTRUE(fastml_resampling_guard$active)) {
    return(invisible(TRUE))
  }

  if (identical(scope, "global")) {
    context <- fastml_resampling_guard$context %||% "resampling"
    stop(
      sprintf(
        paste0(
          "Guarded Resampling Principle violated: %s attempted while %s is active. ",
          "All preprocessing and model fitting must be performed on fold-specific training partitions."
        ),
        operation,
        context
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

