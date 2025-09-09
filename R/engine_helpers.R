#' Get Available Methods
#'
#' Returns a character vector of algorithm names available for classification,
#' regression or survival tasks.
#'
#' @param type A character string specifying the type of task. Must be one of
#'   \code{"classification"}, \code{"regression"}, or \code{"survival"}. Defaults to
#'   \code{c("classification", "regression", "survival")} and uses
#'   \code{\link[base]{match.arg}} to select one.
#' @param ... Additional arguments (currently not used).
#'
#' @return A character vector containing the names of the available algorithms for the specified task type.
#'
#' @details Depending on the specified \code{type}, the function returns a different set of algorithm names:
#' \itemize{
#'   \item For \code{"classification"}, it returns algorithms such as \code{"logistic_reg"}, \code{"multinom_reg"}, \code{"decision_tree"}, \code{"C5_rules"}, \code{"rand_forest"}, \code{"xgboost"}, \code{"lightgbm"}, \code{"svm_linear"}, \code{"svm_rbf"}, \code{"nearest_neighbor"}, \code{"naive_Bayes"}, \code{"mlp"}, \code{"discrim_linear"}, \code{"discrim_quad"}, and \code{"bag_tree"}.
#'   \item For \code{"regression"}, it returns algorithms such as \code{"linear_reg"}, \code{"ridge_reg"}, \code{"lasso_reg"}, \code{"elastic_net"}, \code{"decision_tree"}, \code{"rand_forest"}, \code{"xgboost"}, \code{"lightgbm"}, \code{"svm_linear"}, \code{"svm_rbf"}, \code{"nearest_neighbor"}, \code{"mlp"}, \code{"pls"}, and \code{"bayes_glm"}.
#' }
#'
#' @export
availableMethods <- function(type = c("classification", "regression", "survival"), ...){
    type <- match.arg(type)

    algorithms <- if (type == "classification"){
      c(
        "logistic_reg",
        "multinom_reg",
        "decision_tree",
        "C5_rules",
        "rand_forest",
        "xgboost",
        "lightgbm",
        "svm_linear",
        "svm_rbf",
        "nearest_neighbor",
        "naive_Bayes",
        "mlp",
        "discrim_linear",
        "discrim_quad",
        "bag_tree"
      )
    } else if (type == "regression") {
      c(
        "linear_reg",
        "ridge_reg",
        "lasso_reg",
        "elastic_net",
        "decision_tree",
        "rand_forest",
        "xgboost",
        "lightgbm",
        "svm_linear",
        "svm_rbf",
        "nearest_neighbor",
        "mlp",
        "pls",
        "bayes_glm"
      )
    } else {
      c(
        "rand_forest",
        "elastic_net"
      )
    }

    return(algorithms)
  }

#' Get Default Engine
#'
#' Returns the default engine corresponding to the specified algorithm.
#'
#' @param algo A character string specifying the name of the algorithm. The value should match one of the supported algorithm names.
#'
#' @return A character string containing the default engine name associated with the provided algorithm.
#'
#' @details The function uses a \code{switch} statement to select the default engine based on the given algorithm. If the provided algorithm does not have a defined default engine, the function terminates with an error.
#'
#'
#' @export
get_default_engine <- function(algo) {
  switch(algo,
         "lightgbm" = "lightgbm",
         "xgboost" = "xgboost",
         "C5_rules" = "C5.0",
         "rand_forest" = "ranger",
         "ranger" = "ranger",
         "logistic_reg" = "glm",
         "multinom_reg" = "nnet",
         "decision_tree" = "rpart",
         "svm_linear" = "kernlab",
         "svm_rbf" = "kernlab",
         "nearest_neighbor" = "kknn",
         "naive_Bayes" = "klaR",
         "mlp" = "nnet",
         "discrim_linear" = "MASS",
         "discrim_quad" = "MASS",
         "bag_tree" = "rpart",
         "elastic_net" = "glmnet",
         "bayes_glm" = "stan",
         "pls" = "mixOmics",
         "linear_reg" = "lm",
         "ridge_reg" = "glmnet",
         "lasso_reg" = "glmnet",
         "deep_learning" = "keras",
         stop("No default engine defined for algorithm: ", algo)
  )
}


#' Get Engine Names from Model Workflows
#'
#' Extracts and returns a list of unique engine names from a list of model workflows.
#'
#' @param models A list where each element is a list of model workflows. Each workflow is expected to contain a fitted model that can be processed with \code{tune::extract_fit_parsnip}.
#'
#' @return A list of character vectors. Each vector contains the unique engine names extracted from the corresponding element of \code{models}.
#'
#' @details The function applies \code{tune::extract_fit_parsnip} to each model workflow to extract the fitted model object. It then retrieves the engine name from the model specification (\code{spec$engine}). If the extraction fails, \code{NA_character_} is returned for that workflow. Finally, the function removes any duplicate engine names using \code{unique}.
#'
#' @importFrom tune extract_fit_parsnip
#'
#' @export
get_engine_names <- function(models) {
  lapply(models, function(model_list) {
    # Extract engine names from each workflow
    engines <- sapply(model_list, function(mod) {
      fit_obj <- tryCatch(
        extract_fit_parsnip(mod),
        error = function(e) NULL
      )
      if (!is.null(fit_obj)) {
        fit_obj$spec$engine
      } else {
        NA_character_
      }
    })
    # Remove names and duplicate entries
    unique(unname(engines))
  })
}

#' Get Model Engine Names
#'
#' Extracts and returns a named vector mapping algorithm names to engine names from a nested list of model workflows.
#'
#' @param models A nested list of model workflows. Each inner list should contain model objects from which a fitted model can be extracted using \code{tune::extract_fit_parsnip}.
#'
#' @return A named character vector where the names correspond to algorithm names (e.g., \code{"rand_forest"}, \code{"logistic_reg"}) and the values correspond to the associated engine names (e.g., \code{"ranger"}, \code{"glm"}).
#'
#' @details The function iterates over a nested list of model workflows and, for each workflow, attempts to extract the fitted model object using \code{tune::extract_fit_parsnip}. If successful, it retrieves the algorithm name from the first element of the class attribute of the model specification and the engine name from the specification. The results are combined into a named vector.
#'
#' @importFrom tune extract_fit_parsnip
#' @importFrom stats setNames
#'
#' @export
get_model_engine_names <- function(models) {
  result <- c()
  for(model_list in models) {
    for(mod in model_list) {
      fit_obj <- tryCatch(
        extract_fit_parsnip(mod),
        error = function(e) NULL
      )
      if (!is.null(fit_obj)) {
        algo   <- class(fit_obj$spec)[1]   # e.g., "rand_forest" or "logistic_reg"
        engine <- fit_obj$spec$engine       # e.g., "ranger", "randomForest", "glm"
        result <- c(result, setNames(engine, algo))
      }
    }
  }
  result
}
