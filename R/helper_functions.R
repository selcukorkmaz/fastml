availableMethods <- function(type = c("classification", "regression"), ...){
  type <- match.arg(type)

  algorithms <- if (type == "classification"){
    c(
      "logistic_reg",
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
  } else {
    c(
      "linear_reg",
      "ridge_regression",
      "lasso_regression",
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
  }

  return(algorithms)
}

# Helper function: get_default_engine
get_default_engine <- function(algo) {
  switch(algo,
         "lightgbm" = "lightgbm",
         "xgboost" = "xgboost",
         "C5_rules" = "C5.0",
         "rand_forest" = "ranger",
         "ranger" = "ranger",
         "logistic_reg" = "glm",        # default fixed-effects logistic regression
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
         "ridge_regression" = "glmnet",
         "lasso_regression" = "glmnet",
         "deep_learning" = "keras",
         stop("No default engine defined for algorithm: ", algo)
  )
}


# Helper function to extract engine names from a list of trained workflows
get_engine_names <- function(models) {
  lapply(models, function(model_list) {
    # Extract engine names from each workflow
    engines <- sapply(model_list, function(mod) {
      fit_obj <- tryCatch(
        tune::extract_fit_parsnip(mod),
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


get_model_engine_names <- function(models) {
  result <- c()
  for(model_list in models) {
    for(mod in model_list) {
      fit_obj <- tryCatch(
        tune::extract_fit_parsnip(mod),
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

get_best_model_names <- function(models) {
  bests <- sapply(names(models), function(algo) {
    model_list <- models[[algo]]
    engines <- sapply(model_list, function(mod) {
      fit_obj <- tryCatch(
        tune::extract_fit_parsnip(mod),
        error = function(e) NULL
      )
      if (!is.null(fit_obj)) {
        fit_obj$spec$engine
      } else {
        NA_character_
      }
    })
    # Choose "randomForest" if available; otherwise, take the first non-NA engine.
    if ("randomForest" %in% engines) {
      "randomForest"
    } else {
      non_na_engines <- engines[!is.na(engines)]
      if (length(non_na_engines) > 0) non_na_engines[1] else NA_character_
    }
  })

  bests
}


get_best_workflows <- function(models, best_model_name) {
  # For each element in best_model_name, extract the corresponding workflow
  best_list <- lapply(seq_along(best_model_name), function(i) {
    algo   <- names(best_model_name)[i]  # e.g., "rand_forest"
    engine <- best_model_name[i]         # e.g., "ranger" or "randomForest"
    if (!is.null(models[[algo]]) && !is.null(models[[algo]][[engine]])) {
      models[[algo]][[engine]]
    } else {
      warning(paste("No workflow found for", algo, "with engine", engine))
      NULL
    }
  })

  # Name each element in the output using a combined label (e.g., "rand_forest (ranger)")
  names(best_list) <- paste0(names(best_model_name), " (", best_model_name, ")")
  best_list
}



