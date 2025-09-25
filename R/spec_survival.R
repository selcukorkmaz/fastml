#' Define Penalized Cox Regression Model Specification
#'
#' Constructs a penalized Cox proportional hazards specification using
#' `parsnip::proportional_hazards()` with the glmnet engine.
#'
#' @param task Character string specifying the task type. Must be
#'   "survival".
#' @param penalty Numeric non-negative penalty value (lambda) passed to
#'   glmnet. Set to `NULL` to use the engine default.
#' @param mixture Numeric value in [0, 1] controlling the elastic-net
#'   mixing parameter (alpha). Set to `NULL` to use the engine default.
#' @param engine Character string identifying the model engine. Defaults to
#'   "glmnet".
#' @param engine_params Optional named list of additional arguments passed to
#'   `parsnip::set_engine()`. Entries named `"control"` or
#'   `"control_parsnip"` are forwarded to `parsnip::fit()` instead.
#'
#' @return A list containing the parsnip model specification (`model_spec`)
#'   and any arguments that should be forwarded to `parsnip::fit()`
#'   (`fit_args`).
#'
#' @importFrom parsnip proportional_hazards set_mode set_engine
#' @noRd
define_penalized_cox_spec <- function(task,
                                      penalty = 0.01,
                                      mixture = 1,
                                      engine = "glmnet",
                                      engine_params = list()) {
  if (!identical(task, "survival")) {
    stop("Penalized Cox regression is only applicable for survival tasks.")
  }

  if (!is.null(penalty)) {
    penalty <- as.numeric(penalty)[1]
    if (!is.finite(penalty) || penalty < 0) {
      stop("'penalty' must be a non-negative finite number or NULL.")
    }
  }

  if (!is.null(mixture)) {
    mixture <- as.numeric(mixture)[1]
    if (!is.finite(mixture) || mixture < 0 || mixture > 1) {
      stop("'mixture' must be between 0 and 1 or NULL.")
    }
  }

  if (is.null(engine_params)) {
    engine_params <- list()
  }
  if (!is.list(engine_params)) {
    stop("'engine_params' must be provided as a list when supplied.")
  }

  fit_args <- list()
  if (length(engine_params) > 0) {
    control_names <- intersect(names(engine_params), c("control", "control_parsnip"))
    if (length(control_names) > 0) {
      for (nm in control_names) {
        fit_args[[nm]] <- engine_params[[nm]]
        engine_params[[nm]] <- NULL
      }
    }
    empty_names <- names(engine_params) == ""
    if (length(empty_names) > 0 && any(empty_names, na.rm = TRUE)) {
      stop("All entries in 'engine_params' must be named.")
    }
    engine_params <- engine_params[!vapply(engine_params, is.null, logical(1))]
  }

  model_spec <- proportional_hazards(
    penalty = penalty,
    mixture = mixture
  ) %>%
    set_mode("censored regression")

  if (length(engine_params) > 0) {
    model_spec <- do.call(set_engine, c(list(model_spec, engine), engine_params))
  } else {
    model_spec <- set_engine(model_spec, engine)
  }

  list(model_spec = model_spec, fit_args = fit_args)
}

