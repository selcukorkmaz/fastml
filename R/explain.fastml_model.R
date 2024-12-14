#' Explain model (Generic)
#'
#' A generic function to provide explanations for models.
#' Different objects can have specialized methods, such as \code{explain.fastml_model}.
#'
#' @param x An object to explain.
#' @param ... Additional arguments passed to the method.
#' @export
explain <- function(x, ...) {
  UseMethod("explain")
}


#' Explain the fastml_model (More Robust Version)
#'
#' Provides model explainability for the best model in a \code{fastml_model} object.
#' Users can choose one of three methods:
#' \describe{
#'   \item{\code{"vip"} (default)}{Uses \strong{vip} for variable importance. If model-specific VI not available, tries permutation-based VI. For partial dependence, uses \strong{pdp}.}
#'   \item{\code{"dalex"}}{Uses \strong{DALEX} for model-agnostic explanations. Provides variable importance and model profiles.}
#'   \item{\code{"lime"}}{Uses \strong{lime} for local explanations of individual observations. Requires \code{newdata}.}
#' }
#'
#' **Requirements:**
#' - \code{method = "vip"}: \code{vip} package. For PDP, also \code{pdp}.
#' - \code{method = "dalex"}: \code{DALEX} package.
#' - \code{method = "lime"}: \code{lime} package.
#'
#' **Data & Model Requirements:**
#' - For PDP (vip/dalex), \code{object$processed_train_data} must exist.
#' - For lime, \code{newdata} and \code{object$processed_train_data} must exist.
#'
#' This code attempts best-effort defaults. Some models may require custom handling.
#'
#' @param object A \code{fastml_model} object.
#' @param method One of \code{"vip"}, \code{"dalex"}, or \code{"lime"}. Default \code{"vip"}.
#' @param features Character vector of feature names for PDP. Default NULL.
#' @param grid_size Grid size for PDP. Default 20.
#' @param newdata Data frame for \code{method = "lime"}. Required if \code{method="lime"}.
#' @param ... Additional arguments passed on (not currently used).
#' @return Prints explainability outputs; no return value.
#' @export
explain.fastml_model <- function(object,
                                 method = c("vip", "dalex", "lime"),
                                 features = NULL,
                                 grid_size = 20,
                                 newdata = NULL,
                                 ...) {
  if (!inherits(object, "fastml_model")) {
    stop("The input must be a 'fastml_model' object.")
  }

  method <- match.arg(method, c("vip", "dalex", "lime"))

  best_model <- object$best_model
  task <- object$task
  label <- object$label
  have_processed_data <- !is.null(object$processed_train_data)
  parsnip_fit <- tryCatch(tune::extract_fit_parsnip(best_model), error = function(e) NULL)
  if (is.null(parsnip_fit)) {
    # If extraction fails, maybe best_model is already a parsnip model
    if (inherits(best_model, "model_fit")) {
      parsnip_fit <- best_model
    }
  }

  # Determine positive_class if binary classification
  if (task == "classification" && have_processed_data) {
    y_factor <- object$processed_train_data[[label]]
    if (is.factor(y_factor) && length(levels(y_factor)) == 2) {
      positive_class <- levels(y_factor)[2]
    } else {
      positive_class <- NULL
    }
  } else {
    positive_class <- NULL
  }

  # Define prediction function for PDP and permutation VI if needed
  pred_fun_for_pdp <- NULL
  if (task == "classification") {
    # If binary, use positive_class
    # If multiclass, pick one class (last level)
    if (!is.null(positive_class)) {
      pred_fun_for_pdp <- function(m, newdata) {
        p <- predict(m, new_data = newdata, type = "prob")
        p[[positive_class]]
      }
    } else {
      # Multiclass scenario: pick the last level as "target"
      if (have_processed_data) {
        all_levels <- levels(object$processed_train_data[[label]])
        use_class <- all_levels[length(all_levels)]
        pred_fun_for_pdp <- function(m, newdata) {
          p <- predict(m, new_data = newdata, type = "prob")
          p[[use_class]]
        }
      } else {
        pred_fun_for_pdp <- function(m, newdata) {
          # Without processed data or known classes, default to numeric predictions if any
          # This may fail
          as.numeric(predict(m, new_data = newdata, type = "prob")[[1]])
        }
      }
    }
  } else {
    # regression
    pred_fun_for_pdp <- function(m, newdata) {
      predict(m, new_data = newdata, type = "numeric")$.pred
    }
  }

  if (method == "vip") {
    if (!requireNamespace("vip", quietly = TRUE)) {
      stop("The 'vip' package is required for method='vip'.")
    }

    cat("\n=== Variable Importance (VIP) ===\n")

    # Try model-specific VIP
    vi_try <- try(vip::vip(best_model), silent = TRUE)
    if (inherits(vi_try, "try-error")) {
      # Try permutation-based VI if have processed data and parsnip_fit
      if (have_processed_data && !is.null(parsnip_fit)) {
        x_train <- object$processed_train_data %>% dplyr::select(-!!label)
        y_train <- object$processed_train_data[[label]]

        # For permutation VI, we must have a pred_wrapper
        pred_wrapper <- function(object, newdata) pred_fun_for_pdp(object, newdata)

        # If classification and y is factor, for permutation importance:
        # vip supports metric = "accuracy" etc.
        # If regression, metric could be "rmse" etc.
        metric <- if (task == "classification") "accuracy" else "rmse"

        vi_perm_try <- try({
          vip::vip(parsnip_fit,
                   method = "permute",
                   train = x_train,
                   target = y_train,
                   metric = metric,
                   pred_wrapper = pred_wrapper)
        }, silent = TRUE)

        if (inherits(vi_perm_try, "try-error")) {
          cat("Unable to compute variable importance for this model (even with permutation).\n")
        }
      } else {
        cat("Model-specific VI not available and no processed data to attempt permutation VI.\n")
      }
    }

    # If features provided, do PDP
    if (!is.null(features)) {
      if (!requireNamespace("pdp", quietly = TRUE)) {
        stop("The 'pdp' package is required for partial dependence plots.")
      }

      if (!have_processed_data) {
        cat("\nNo processed training data found. Cannot produce partial dependence plots.\n")
      } else if (is.null(parsnip_fit)) {
        cat("\nCould not extract a parsnip model from the workflow. PDP may fail.\n")
      } else {
        cat("\n=== Partial Dependence Plots (VIP) ===\n")
        partial_data <- object$processed_train_data
        for (f in features) {
          pd_try <- try({
            pd <- pdp::partial(
              object = parsnip_fit,
              pred.var = f,
              train = partial_data,
              grid.resolution = grid_size,
              pred.fun = pred_fun_for_pdp
            )
            print(pdp::autoplot(pd) + ggplot2::ggtitle(paste("Partial Dependence:", f)))
          }, silent = TRUE)
          if (inherits(pd_try, "try-error")) {
            cat("Could not produce PDP for feature:", f, "\n")
          }
        }
      }
    }

  } else if (method == "dalex") {
    if (!requireNamespace("DALEX", quietly = TRUE)) {
      stop("The 'DALEX' package is required for method='dalex'.")
    }

    if (!have_processed_data || !(label %in% names(object$processed_train_data))) {
      cat("\nCannot create DALEX explainer without processed training data and target variable.\n")
      return(invisible(NULL))
    }

    train_data <- object$processed_train_data
    x <- train_data %>% dplyr::select(-!!label)
    y <- train_data[[label]]

    # Define predict_function for DALEX
    predict_function <- function(m, newdata) pred_fun_for_pdp(m, newdata)

    model_info <- if (task == "classification") list(type = "classification") else list(type = "regression")

    # Use DALEX::explain() to avoid conflicts with our explain()
    exp_try <- try({
      explainer <- DALEX::explain(
        model = parsnip_fit,
        data = x,
        y = if (is.numeric(y)) y else as.numeric(y),
        label = object$best_model_name,
        predict_function = predict_function,
        model_info = model_info
      )

      cat("\n=== DALEX Variable Importance ===\n")
      vi <- DALEX::model_parts(explainer)
      print(vi)

      if (!is.null(features)) {
        cat("\n=== DALEX Model Profiles (Partial Dependence) ===\n")
        mp <- DALEX::model_profile(explainer, variables = features, N = grid_size)
        print(mp)
      }
    }, silent = TRUE)

    if (inherits(exp_try, "try-error")) {
      cat("DALEX explanations not available for this model.\n")
    }

  } else if (method == "lime") {
    if (!requireNamespace("lime", quietly = TRUE)) {
      stop("The 'lime' package is required for method='lime'.")
    }

    if (is.null(newdata)) {
      stop("For method='lime', you must provide 'newdata'.")
    }
    if (!have_processed_data) {
      stop("No processed training data found, cannot create lime explainer.")
    }
    if (is.null(object$preprocessor)) {
      stop("Preprocessing recipe missing. Cannot preprocess newdata for lime explanations.")
    }

    train_data <- object$processed_train_data
    if (!(label %in% names(train_data))) {
      stop("Label not found in processed training data. Cannot create lime explainer.")
    }

    newdata_processed <- recipes::bake(object$preprocessor, new_data = newdata)
    x_train <- train_data %>% dplyr::select(-!!label)

    # lime may require model_type and predict_model methods for unsupported models:
    # For ksvm, define them if not defined:
    if (inherits(parsnip_fit$fit, "ksvm")) {
      if (!exists("model_type.ksvm", where = .GlobalEnv)) {
        model_type.ksvm <- function(x, ...) {
          # If factor response: classification
          if (task == "classification") "classification" else "regression"
        }
        assign("model_type.ksvm", model_type.ksvm, envir = .GlobalEnv)
      }

      if (!exists("predict_model.ksvm", where = .GlobalEnv)) {
        predict_model.ksvm <- function(x, newdata, type, ...) {
          # ksvm can predict probabilities if fitted with probability=TRUE
          p <- kernlab::predict(x, newdata, type="probabilities")
          as.data.frame(p)
        }
        assign("predict_model.ksvm", predict_model.ksvm, envir = .GlobalEnv)
      }
    }

    cat("\n=== LIME Local Explanations ===\n")
    lime_try <- try({
      explainer <- lime::lime(
        x = x_train,
        model = parsnip_fit,
        ...
      )
      explanations <- lime::explain(newdata_processed, explainer, n_features = min(5, ncol(x_train)))
      print(explanations)
    }, silent = TRUE)

    if (inherits(lime_try, "try-error")) {
      cat("LIME explanations not available for this model.\n")
    }
  }

  invisible(NULL)
}
