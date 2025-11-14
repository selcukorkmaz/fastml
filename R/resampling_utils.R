# Internal helpers for handling resampling plans and metadata

fastml_is_resample_plan <- function(x) {
  inherits(x, "fastml_resample_plan")
}

fastml_new_resample_plan <- function(splits, method, params = list(), extras = list()) {
  if (is.null(splits)) {
    stop("'splits' must be provided when creating a resampling plan.")
  }
  if (!inherits(splits, "rset")) {
    stop("'splits' must be an 'rset' object when creating a resampling plan.")
  }
  metadata <- list(
    method = method,
    params = params
  )
  plan <- c(list(splits = splits, metadata = metadata), extras)
  class(plan) <- c("fastml_resample_plan", class(plan))
  plan
}

fastml_resample_splits <- function(plan) {
  if (fastml_is_resample_plan(plan)) {
    return(plan$splits)
  }
  plan
}

fastml_resample_metadata <- function(plan) {
  if (fastml_is_resample_plan(plan)) {
    return(plan$metadata)
  }
  NULL
}

fastml_resample_method <- function(plan) {
  meta <- fastml_resample_metadata(plan)
  if (is.null(meta)) {
    return(NULL)
  }
  meta$method
}

fastml_resample_params <- function(plan) {
  meta <- fastml_resample_metadata(plan)
  if (is.null(meta)) {
    return(list())
  }
  meta$params %||% list()
}

fastml_resample_update_splits <- function(plan, splits) {
  if (!fastml_is_resample_plan(plan)) {
    stop("Cannot update splits on a non-resampling plan object.")
  }
  if (!inherits(splits, "rset")) {
    stop("'splits' must be an 'rset' object when updating a resampling plan.")
  }
  plan$splits <- splits
  plan
}

fastml_resample_validate <- function(plan, allow_null = FALSE) {
  if (is.null(plan)) {
    if (allow_null) {
      return(NULL)
    }
    stop("Resampling plan cannot be NULL in this context.")
  }
  splits <- fastml_resample_splits(plan)
  if (is.null(splits) || !inherits(splits, "rset")) {
    stop("Resampling plan must contain an 'rset' object under the 'splits' element.")
  }
  plan
}

fastml_describe_resampling <- function(plan) {
  if (is.null(plan)) {
    return("none")
  }
  method <- fastml_resample_method(plan)
  params <- fastml_resample_params(plan)
  if (is.null(method)) {
    method <- "custom"
  }
  if (length(params) == 0) {
    return(method)
  }
  if (is.null(names(params))) {
    names(params) <- paste0("param", seq_along(params))
  }
  formatted <- vapply(
    names(params),
    function(nm) {
      value <- params[[nm]]
      if (is.null(value)) {
        value <- "NULL"
      } else if (length(value) > 1) {
        value <- paste(value, collapse = ", ")
      }
      paste0(nm, "=", value)
    },
    character(1)
  )
  paste0(method, " [", paste(formatted, collapse = ", "), "]")
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
