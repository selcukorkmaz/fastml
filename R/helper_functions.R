availableMethods <- function(type = c("classification", "regression"), ...){
  type <- match.arg(type)

  algorithms <- if (type == "classification"){
    c(
      "logistic_regression",
      "penalized_logistic_regression",
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
