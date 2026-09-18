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

# Group identifier for one or more grouping columns, used by the grouped and
# grouped time-ordered splitting rules.
fastml_group_id <- function(df, group_cols) {
  group_df <- df[, group_cols, drop = FALSE]
  if (length(group_cols) == 1) {
    group_df[[1]]
  } else {
    interaction(group_df, drop = TRUE)
  }
}

# Locate the cut point for a time-ordered holdout that also keeps groups intact.
#
# `df` is assumed to be sorted in ascending order of the ordering variable. A
# cut `c` splits rows 1..c from rows (c+1)..n. A cut is admissible only when no
# group spans it, so that both guarantees hold simultaneously: every training
# row precedes every test row, and no group contributes rows to both sides.
# Groups whose occurrences are interleaved in time can make every interior cut
# inadmissible, in which case no split satisfies both constraints and we stop
# rather than silently relaxing one of them.
fastml_grouped_time_cut <- function(df, group_cols, target_cut) {
  n_rows <- nrow(df)
  group_id <- fastml_group_id(df, group_cols)
  if (any(is.na(group_id))) {
    stop("Grouping columns contain missing values, which are not supported for grouped time-ordered holdout splitting.", call. = FALSE)
  }

  # Unused factor levels would make tapply() emit NA entries for groups that
  # are not present, so drop them before the interval scan.
  if (is.factor(group_id)) {
    group_id <- droplevels(group_id)
  }

  idx <- seq_len(n_rows)
  first_idx <- tapply(idx, group_id, min)
  last_idx <- tapply(idx, group_id, max)

  # A group spanning [first, last] rules out every cut in [first, last - 1].
  # Accumulate those intervals with a difference array so the scan is linear.
  blocked <- integer(n_rows + 1L)
  spans <- which(last_idx > first_idx)
  for (g in spans) {
    blocked[first_idx[[g]]] <- blocked[first_idx[[g]]] + 1L
    blocked[last_idx[[g]]] <- blocked[last_idx[[g]]] - 1L
  }
  coverage <- cumsum(blocked)[seq_len(n_rows)]

  admissible <- which(coverage == 0L)
  admissible <- admissible[admissible >= 1L & admissible <= n_rows - 1L]

  if (length(admissible) == 0L) {
    stop(
      paste0(
        "No time-ordered holdout split keeps groups intact: the groups given by '",
        paste(group_cols, collapse = ", "),
        "' are interleaved in the order given by the ordering variable, so every ",
        "cut point would place at least one group on both sides. Supply only one ",
        "of 'block_col' and 'group_cols', or pre-split the data with 'train_data' ",
        "and 'test_data'."
      ),
      call. = FALSE
    )
  }

  distance <- abs(admissible - target_cut)
  # Ties resolve toward the later cut, which retains more training data.
  best <- admissible[which(distance == min(distance))]
  max(best)
}
