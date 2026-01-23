fastml_setup_parallel <- function(n_cores, seed) {
  if (!requireNamespace("future", quietly = TRUE)) {
    stop("The 'future' package is required but is not installed.")
  }

  old_plan <- future::plan()
  old_future_seed <- getOption("future.seed")

  n_cores_val <- fastml_normalize_threads(n_cores)
  if (is.null(n_cores_val)) {
    n_cores_val <- 1L
  }

  use_parallel <- n_cores_val > 1L
  old_do_par <- NULL

  if (use_parallel) {
    if (!requireNamespace("doFuture", quietly = TRUE)) {
      stop("The 'doFuture' package is required for parallel processing but is not installed.")
    }

    # CAPTURE: Register doFuture and save the PREVIOUS backend info.
    # registerDoFuture() returns the old backend which we restore on cleanup.
    # See: https://dofuture.futureverse.org/reference/registerDoFuture.html
    old_do_par <- doFuture::registerDoFuture(flavor = "%dofuture%")

    seed_val <- fastml_normalize_seed(seed)
    options(future.seed = if (!is.null(seed_val)) seed_val else TRUE)
    future::plan(future::multisession, workers = n_cores_val)
  } else {
    # plan(sequential) is sufficient for sequential execution.
    # doFuture handles this automatically without needing registerDoSEQ().
    future::plan(future::sequential)
  }

  # RESTORE: Define the cleanup function
  restore <- function() {
    future::plan(old_plan)
    options(future.seed = old_future_seed)

    # If we changed the foreach backend, restore the previous one
    if (!is.null(old_do_par)) {
      do.call(foreach::setDoPar, args = old_do_par)
    }
  }

  list(restore = restore, use_parallel = use_parallel)
}

fastml_muffle_foreach_warning <- function(w) {
  msg <- conditionMessage(w)
  if (is.null(msg)) {
    return(invisible())
  }
  if (grepl("executing %dopar% sequentially: no parallel backend registered", msg, fixed = TRUE)) {
    invokeRestart("muffleWarning")
  }
  invisible()
}
