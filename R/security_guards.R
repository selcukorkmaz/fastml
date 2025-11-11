fastml_init_audit_env <- function(enabled = FALSE) {
  env <- new.env(parent = emptyenv())
  env$enabled <- isTRUE(enabled)
  env$log <- list()
  env$unsafe <- FALSE
  env$warnings_emitted <- character()
  env
}

fastml_register_audit <- function(env, message, severity = c("info", "warning"), context = NULL) {
  if (is.null(env) || !is.environment(env)) {
    return(invisible(NULL))
  }
  severity <- match.arg(severity)
  entry <- list(message = message, severity = severity, context = context)
  env$log <- c(env$log, list(entry))
  if (identical(severity, "warning")) {
    env$unsafe <- TRUE
  }
  invisible(entry)
}

fastml_audit_emit_warning <- function(env, message) {
  if (is.null(env) || !is.environment(env) || !isTRUE(env$enabled)) {
    return(invisible(NULL))
  }
  if (message %in% env$warnings_emitted) {
    return(invisible(NULL))
  }
  env$warnings_emitted <- c(env$warnings_emitted, message)
  warning(message, call. = FALSE)
  invisible(NULL)
}

fastml_env_inherits_global <- function(env) {
  if (is.null(env) || !is.environment(env)) {
    return(FALSE)
  }
  current <- env
  while (!identical(current, emptyenv())) {
    if (identical(current, .GlobalEnv)) {
      return(TRUE)
    }
    parent <- tryCatch(parent.env(current), error = function(...) NULL)
    if (is.null(parent) || identical(parent, current)) {
      break
    }
    current <- parent
  }
  FALSE
}

fastml_recipe_step_contains_external_reference <- function(step, depth = 0, max_depth = 6) {
  if (depth > max_depth) {
    return(FALSE)
  }

  if (is.environment(step)) {
    return(fastml_env_inherits_global(step))
  }

    if (is.function(step)) {
      if (fastml_env_inherits_global(environment(step))) {
        return(TRUE)
      }
      return(fastml_recipe_step_contains_external_reference(body(step), depth + 1, max_depth))
    }

  if (is.list(step)) {
      for (element in step) {
        if (fastml_recipe_step_contains_external_reference(element, depth + 1, max_depth)) {
          return(TRUE)
        }
      }
    return(FALSE)
  }

  if (inherits(step, "quosure")) {
    expr <- tryCatch(rlang::quo_get_expr(step), error = function(...) NULL)
    if (!is.null(expr)) {
      return(fastml_recipe_step_contains_external_reference(expr, depth + 1, max_depth))
    }
    return(FALSE)
  }

  if (is.language(step)) {
    names_in_expr <- all.names(step, functions = TRUE)
    if (".GlobalEnv" %in% names_in_expr) {
      return(TRUE)
    }
    return(FALSE)
  }

  if (inherits(step, "data.frame") || inherits(step, "tbl_df")) {
    return(TRUE)
  }

  FALSE
}

fastml_detect_leaky_recipe_steps <- function(recipe) {
  flagged <- character()
  if (is.null(recipe$steps) || length(recipe$steps) == 0) {
    return(flagged)
  }
  for (idx in seq_along(recipe$steps)) {
    step <- recipe$steps[[idx]]
    label <- if (!is.null(step$id)) step$id else paste0("step_", idx)
    if (fastml_recipe_step_contains_external_reference(step)) {
      flagged <- c(flagged, label)
    }
  }
  unique(flagged)
}

fastml_validate_user_recipe <- function(recipe, audit_env = NULL) {
  if (!inherits(recipe, "recipe")) {
    stop("`recipe` must be a recipes::recipe object.")
  }
  if (isTRUE(recipe$trained)) {
    stop("Pretrained recipes are not allowed; provide an untrained recipe.")
  }
  leaky_steps <- fastml_detect_leaky_recipe_steps(recipe)
  if (length(leaky_steps) > 0) {
    msg <- paste0(
      "The supplied recipe contains steps that depend on external data: ",
      paste(leaky_steps, collapse = ", "),
      ". Remove or rewrite these steps to avoid leakage."
    )
    stop(msg)
  }
  fastml_register_audit(
    audit_env,
    "Recipe validation completed with no detected leakage references.",
    severity = "info",
    context = "recipe"
  )
  invisible(recipe)
}

fastml_audit_io_functions <- function() {
  c(
    "read.csv", "read.csv2", "read.table", "readRDS", "load", "scan",
    "source", "file", "gzfile", "bzfile", "xzfile", "unz", "url",
    "readLines", "write.csv", "write.table", "save", "saveRDS"
  )
}

fastml_build_sandbox_env <- function(audit_env, context) {
  env <- new.env(parent = baseenv())

  makeActiveBinding(
    ".GlobalEnv",
    function(value) {
      stop(sprintf("Sandboxed %s cannot access .GlobalEnv.", context), call. = FALSE)
    },
    env
  )

  env$assign <- function(x, value, pos = -1, envir = as.environment(pos), inherits = FALSE, immediate = TRUE) {
    target <- if (!missing(envir)) envir else as.environment(pos)
    if (identical(target, .GlobalEnv)) {
      stop(sprintf("Sandboxed %s cannot assign into .GlobalEnv.", context), call. = FALSE)
    }
    base::assign(x, value, pos = pos, envir = envir, inherits = inherits, immediate = immediate)
  }
  environment(env$assign) <- baseenv()

  env$rm <- function(..., list = character(), pos = -1, envir = as.environment(pos), inherits = FALSE) {
    target <- if (!missing(envir)) envir else as.environment(pos)
    if (identical(target, .GlobalEnv)) {
      stop(sprintf("Sandboxed %s cannot remove objects from .GlobalEnv.", context), call. = FALSE)
    }
    base::rm(..., list = list, pos = pos, envir = envir, inherits = inherits)
  }
  environment(env$rm) <- baseenv()

  env$get <- function(x, pos = -1, envir = as.environment(pos), inherits = FALSE) {
    target <- if (!missing(envir)) envir else as.environment(pos)
    if (identical(target, .GlobalEnv)) {
      stop(sprintf("Sandboxed %s cannot read from .GlobalEnv.", context), call. = FALSE)
    }
    base::get(x, pos = pos, envir = envir, inherits = inherits)
  }
  environment(env$get) <- baseenv()

  if (!is.null(audit_env) && isTRUE(audit_env$enabled)) {
    for (fn_name in fastml_audit_io_functions()) {
      if (!exists(fn_name, envir = baseenv(), inherits = TRUE)) {
        next
      }
      base_fun <- get(fn_name, envir = baseenv())
      env[[fn_name]] <- (function(fun, name) {
        function(...) {
          msg <- sprintf("fastml audit: %s attempted to call %s(), which may access external resources.", context, name)
          fastml_register_audit(audit_env, msg, severity = "warning", context = context)
          fastml_audit_emit_warning(audit_env, msg)
          fun(...)
        }
      })(base_fun, fn_name)
      environment(env[[fn_name]]) <- baseenv()
    }
  }

  env
}

fastml_prepare_sandboxed_function <- function(fn, audit_env, context) {
  if (!is.function(fn)) {
    stop(sprintf("%s must be a function.", context), call. = FALSE)
  }
  if (is.primitive(fn)) {
    stop(sprintf("%s must not be a primitive function.", context), call. = FALSE)
  }
  sandbox_env <- fastml_build_sandbox_env(audit_env, context)
  fn_copy <- fn
  environment(fn_copy) <- sandbox_env
  fn_copy
}

fastml_audit_function_signature <- function(fn, context, audit_env) {
  if (is.null(audit_env) || !isTRUE(audit_env$enabled)) {
    return(invisible(NULL))
  }
  fn_env <- tryCatch(environment(fn), error = function(...) NULL)
  if (fastml_env_inherits_global(fn_env)) {
    msg <- sprintf("fastml audit: %s retains a reference to .GlobalEnv.", context)
    fastml_register_audit(audit_env, msg, severity = "warning", context = context)
    fastml_audit_emit_warning(audit_env, msg)
  }
  if (!is.primitive(fn)) {
    body_symbols <- tryCatch(all.names(body(fn), functions = TRUE), error = function(...) character())
    if (".GlobalEnv" %in% body_symbols) {
      msg <- sprintf("fastml audit: %s references .GlobalEnv, which is disallowed.", context)
      fastml_register_audit(audit_env, msg, severity = "warning", context = context)
      fastml_audit_emit_warning(audit_env, msg)
    }
    io_hits <- intersect(body_symbols, fastml_audit_io_functions())
    if (length(io_hits) > 0) {
      msg <- sprintf(
        "fastml audit: %s references I/O helper(s): %s. External access voids leakage guarantees.",
        context,
        paste(unique(io_hits), collapse = ", ")
      )
      fastml_register_audit(audit_env, msg, severity = "warning", context = context)
      fastml_audit_emit_warning(audit_env, msg)
    }
  }
  invisible(NULL)
}

fastml_run_user_hook <- function(fn, data, context, audit_env, extra_args = list()) {
  fn_prepared <- fastml_prepare_sandboxed_function(fn, audit_env, context)
  fastml_audit_function_signature(fn, context, audit_env)

  data_copy <- if (is.data.frame(data)) {
    data[ , , drop = FALSE]
  } else {
    data
  }

  args <- c(list(data_copy), extra_args)

  pre_globals <- ls(.GlobalEnv, all.names = TRUE)
  result <- tryCatch({
    do.call(fn_prepared, args)
  }, error = function(e) {
    stop(sprintf("Custom preprocessing step failed while executing %s: %s", context, e$message), call. = FALSE)
  })
  post_globals <- ls(.GlobalEnv, all.names = TRUE)
  added <- setdiff(post_globals, pre_globals)
  removed <- setdiff(pre_globals, post_globals)
  if (length(added) > 0) {
    rm(list = added, envir = .GlobalEnv)
  }
  if (length(added) > 0 || length(removed) > 0) {
    msg <- sprintf("Sandboxed %s attempted to modify the global environment.", context)
    fastml_register_audit(audit_env, msg, severity = "warning", context = context)
    stop(msg, call. = FALSE)
  }
  result
}

fastml_validate_custom_hook <- function(hook, context) {
  if (is.list(hook) || is.environment(hook)) {
    fit <- hook$fit
    transform <- hook$transform
  } else {
    fit <- NULL
    transform <- NULL
  }
  if (!is.function(fit) || !is.function(transform)) {
    stop(sprintf("%s must supply both 'fit' and 'transform' functions.", context), call. = FALSE)
  }
  list(fit = fit, transform = transform)
}

fastml_validate_transformed_data <- function(data, context) {
  if (!is.data.frame(data)) {
    stop(sprintf("%s must return a data.frame.", context), call. = FALSE)
  }
  data
}

fastml_process_custom_fit_result <- function(result, context) {
  if (!is.list(result) || is.null(result$state)) {
    stop(sprintf("%s fit must return a list with a 'state' element.", context), call. = FALSE)
  }
  list(
    state = result$state,
    transformed = result$transformed
  )
}
