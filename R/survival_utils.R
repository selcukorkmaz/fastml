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
    return(list(status = rep(0, reference_length), recoded = FALSE))
  }

  status_raw <- status_vec

  if (is.data.frame(status_raw)) {
    status_raw <- status_raw[[1]]
  }

  was_recode <- FALSE

  if (is.factor(status_raw)) {
    status_raw <- as.character(status_raw)
    was_recode <- TRUE
  }

  if (is.character(status_raw)) {
    numeric_candidate <- suppressWarnings(as.numeric(status_raw))
    if (!all(is.na(numeric_candidate[!is.na(status_raw)]))) {
      status_num <- numeric_candidate
      was_recode <- TRUE
    } else {
      unique_vals <- unique(status_raw[!is.na(status_raw)])
      if (length(unique_vals) == 0) {
        status_num <- rep(NA_real_, length(status_raw))
      } else {
        lower_vals <- tolower(unique_vals)
        event_keywords <- c("event", "dead", "death", "fail", "failed", "failure",
                            "yes", "true", "deceased")
        keyword_idx <- which(lower_vals %in% event_keywords)
        if (length(keyword_idx) == 0) {
          event_label <- unique_vals[length(unique_vals)]
        } else {
          event_label <- unique_vals[keyword_idx[length(keyword_idx)]]
        }
        status_num <- ifelse(is.na(status_raw), NA_real_,
                             ifelse(tolower(status_raw) == tolower(event_label), 1, 0))
      }
      return(list(status = rep_len(ifelse(is.na(status_num), 0, status_num),
                                   reference_length),
                  recoded = TRUE))
    }
  } else {
    status_num <- as.numeric(status_raw)
  }

  if (length(status_num) != reference_length) {
    status_num <- rep_len(status_num, reference_length)
  }

  finite_vals <- sort(unique(status_num[is.finite(status_num)]))
  if (length(finite_vals) == 0) {
    return(list(status = rep(0, reference_length), recoded = FALSE))
  }

  recoded_flag <- was_recode || !all(finite_vals %in% c(0, 1))

  status_out <- rep(0, reference_length)

  if (length(finite_vals) == 1) {
    if (finite_vals > 0) {
      status_out[!is.na(status_num)] <- 1
    }
    return(list(status = status_out, recoded = recoded_flag))
  }

  if (any(finite_vals == 0)) {
    threshold <- 0
  } else {
    threshold <- finite_vals[1]
  }

  status_out[!is.na(status_num) & status_num > threshold] <- 1

  list(status = status_out, recoded = recoded_flag)
}
