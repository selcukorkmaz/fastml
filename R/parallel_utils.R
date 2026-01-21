fastml_setup_parallel <- function(n_cores, seed) {
  if (!requireNamespace("future", quietly = TRUE)) {
    stop("The 'future' package is required but is not installed.")
  }
  if (!requireNamespace("foreach", quietly = TRUE)) {
    stop("The 'foreach' package is required but is not installed.")
  }

  old_plan <- future::plan()
  old_future_seed <- getOption("future.seed")
  get_do_par <- tryCatch(
    getFromNamespace("getDoPar", "foreach"),
    error = function(e) NULL
  )
  old_do_par <- if (is.function(get_do_par)) {
    tryCatch(get_do_par(), error = function(e) NULL)
  } else {
    NULL
  }
  set_do_par <- tryCatch(
    getFromNamespace("setDoPar", "foreach"),
    error = function(e) NULL
  )
  n_cores_val <- fastml_normalize_threads(n_cores)
  if (is.null(n_cores_val)) {
    n_cores_val <- 1L
  }

  restore <- function() {
    future::plan(old_plan)
    options(future.seed = old_future_seed)
    if (!is.null(old_do_par) && is.function(set_do_par)) {
      set_do_par(old_do_par$fun, old_do_par$data, old_do_par$info)
    }
  }

  if (n_cores_val > 1L) {
    if (!requireNamespace("doFuture", quietly = TRUE)) {
      stop("The 'doFuture' package is required for parallel processing but is not installed.")
    }
    seed_val <- fastml_normalize_seed(seed)
    doFuture::registerDoFuture(flavor = "%dofuture%")
    options(future.seed = if (!is.null(seed_val)) seed_val else TRUE)
    future::plan(future::multisession, workers = n_cores_val)
  } else {
    foreach::registerDoSEQ()
    future::plan(future::sequential)
  }

  list(restore = restore)
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
