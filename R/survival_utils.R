#' Internal helpers for survival-specific preprocessing
#'
#' These utilities standardize survival status indicators so that downstream
#' metrics always receive the conventional coding (0 = censored, 1 = event).
#' The functions are intentionally unexported and are used across multiple
#' internal modules.

#' Normalize survival status coding to 0/1 representation
#'
#' This helper attempts to coerce a status vector into a numeric format where
#' 0 represents censoring and 1 represents the event indicator. It accepts a
#' variety of common encodings such as 1/2, logical values, factors, or
#' character labels. When the supplied values deviate from the canonical
#' coding, the function records that a recode was performed so callers can
#' communicate this to the user (once).
#'
#' @param status_vec A vector containing survival status information. May be
#'   numeric, logical, factor, or character.
#' @param reference_length Optional integer specifying the desired length of the
#'   returned vector. When `status_vec` is `NULL`, this value controls the
#'   length of the output (defaulting to 0 when not supplied).
#'
#' @return A list with two elements: `status`, the recoded numeric vector, and
#'   `recoded`, a logical flag indicating whether a non-standard encoding was
#'   detected.
fastml_normalize_survival_status <- function(status_vec, reference_length = NULL) {
  if (is.null(reference_length)) {
    reference_length <- length(status_vec)
  }

  if (is.null(status_vec)) {
    stop(
      "Survival status column is missing. Provide a 0/1 event indicator (0=censored, 1=event).",
      call. = FALSE
    )
  }

  status_raw <- status_vec

  if (is.data.frame(status_raw)) {
    status_raw <- status_raw[[1]]
  }

  if (is.factor(status_raw) || is.character(status_raw)) {
    stop(
      paste(
        "Survival status must be coded as 0/1 (0=censored, 1=event) or logical.",
        "Please recode the status column explicitly before modeling."
      ),
      call. = FALSE
    )
  }

  if (is.logical(status_raw)) {
    status_num <- as.integer(status_raw)
  } else {
    status_num <- as.numeric(status_raw)
  }

  if (length(status_num) != reference_length) {
    status_num <- rep_len(status_num, reference_length)
  }

  if (anyNA(status_num)) {
    stop(
      "Survival status contains missing values. Remove rows with missing status before modeling.",
      call. = FALSE
    )
  }

  finite_vals <- sort(unique(status_num[is.finite(status_num)]))
  if (length(finite_vals) == 0) {
    stop("Survival status is empty after cleaning. Provide a 0/1 event indicator.", call. = FALSE)
  }

  if (!all(finite_vals %in% c(0, 1))) {
    stop(
      paste(
        "Survival status must be coded as 0/1 (0=censored, 1=event) or logical.",
        "Please recode the status column explicitly before modeling."
      ),
      call. = FALSE
    )
  }

  list(status = as.integer(status_num), recoded = FALSE)
}

fastml_normalize_survival_convention <- function(convention) {
  if (is.null(convention) || length(convention) == 0 || is.na(convention[[1]])) {
    return("fastml")
  }

  convention <- tolower(as.character(convention[[1]]))
  allowed <- c("fastml", "tidymodels")
  if (!convention %in% allowed) {
    stop(
      sprintf(
        "Invalid survival_metric_convention '%s'. Choose one of: %s.",
        convention,
        paste(allowed, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  convention
}
