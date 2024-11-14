#' LightGBM Model for caret
#'
#' Custom model function to integrate LightGBM with caret
#'
#' @import lightgbm
#' @export
lightgbmModelInfo <- list(
  label = "LightGBM",
  library = "lightgbm",
  type = c("Classification", "Regression"),
  parameters = data.frame(
    parameter = c("num_leaves", "learning_rate", "n_estimators", "min_data_in_leaf", "feature_fraction"),
    class = rep("numeric", 5),
    label = c("Num Leaves", "Learning Rate", "n_estimators", "Min Data in Leaf", "Feature Fraction")
  ),
  grid = function(x, y, len = NULL, search = "grid") {
    expand.grid(
      num_leaves = c(31, 63),
      learning_rate = c(0.01, 0.1),
      n_estimators = c(100, 200),
      min_data_in_leaf = c(20, 50),
      feature_fraction = c(0.6, 0.8, 1.0)
    )
  },
  loop = NULL,
  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
    if (!requireNamespace("lightgbm", quietly = TRUE)) {
      stop("The 'lightgbm' package is required but is not installed.")
    }

    if (is.factor(y)) {
      # Classification
      dtrain <- lightgbm::lgb.Dataset(data = as.matrix(x), label = as.numeric(y) - 1)
      params <- list(
        objective = "multiclass",
        num_class = length(lev),
        num_leaves = param$num_leaves,
        learning_rate = param$learning_rate,
        min_data_in_leaf = param$min_data_in_leaf,
        feature_fraction = param$feature_fraction,
        verbosity = -1
      )
    } else {
      # Regression
      dtrain <- lightgbm::lgb.Dataset(data = as.matrix(x), label = y)
      params <- list(
        objective = "regression",
        metric = "rmse",
        num_leaves = param$num_leaves,
        learning_rate = param$learning_rate,
        min_data_in_leaf = param$min_data_in_leaf,
        feature_fraction = param$feature_fraction,
        verbosity = -1
      )
    }

    model <- lightgbm::lgb.train(
      params = params,
      data = dtrain,
      nrounds = param$n_estimators,
      verbose = -1
    )

    list(
      object = model,
      lev = lev
    )
  },
  predict = function(modelFit, newdata, submodels = NULL) {
    if (is.factor(modelFit$lev)) {
      preds <- predict(modelFit$object, as.matrix(newdata))
      preds <- matrix(preds, ncol = length(modelFit$lev), byrow = TRUE)
      class_preds <- apply(preds, 1, function(x) which.max(x) - 1)
      factor(modelFit$lev[class_preds + 1], levels = modelFit$lev)
    } else {
      preds <- predict(modelFit$object, as.matrix(newdata))
      as.vector(preds)
    }
  },
  prob = function(modelFit, newdata, submodels = NULL) {
    preds <- predict(modelFit$object, as.matrix(newdata))
    preds <- matrix(preds, ncol = length(modelFit$lev), byrow = TRUE)
    colnames(preds) <- modelFit$lev
    preds
  },
  predictors = function(x, ...) {
    x$object$feature_name
  },
  varImp = NULL,
  levels = function(x) x$lev,
  tags = c("Ensemble Model", "Boosting", "Tree-Based Model"),
  sort = function(x) x
)
