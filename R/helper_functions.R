availableMethods <- function(type = c("classification", "regression"), ...){
  type <- match.arg(type)

  algorithms <- if (type == "classification"){
    c(
      "logistic_regression",
      "decision_tree",
      "c5.0",
      "random_forest",
      "ranger",
      "xgboost",
      "lightgbm",
      "svm_linear",
      "svm_radial",
      "knn",
      "naive_bayes",
      "neural_network",
      "lda",
      "qda",
      "bagging"
    )
  } else {
    c(
      "linear_regression",
      "ridge_regression",
      "lasso_regression",
      "elastic_net",
      "decision_tree",
      "random_forest",
      "xgboost",
      "lightgbm",
      "svm_linear",
      "svm_radial",
      "knn",
      "neural_network",
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
         "c5.0" = "C5.0",
         "random_forest" = "ranger",
         "ranger" = "ranger",
         "logistic_regression" = "glm",        # default fixed-effects logistic regression
         "decision_tree" = "rpart",
         "svm_linear" = "kernlab",
         "svm_radial" = "kernlab",
         "knn" = "kknn",
         "naive_bayes" = "klaR",
         "neural_network" = "nnet",
         "lda" = "MASS",
         "qda" = "MASS",
         "bagging" = "rpart",
         "elastic_net" = "glmnet",
         "bayes_glm" = "stan",
         "pls" = "mixOmics",
         "linear_regression" = "lm",
         "ridge_regression" = "glmnet",
         "lasso_regression" = "glmnet",
         "deep_learning" = "keras",
         stop("No default engine defined for algorithm: ", algo)
  )
}

