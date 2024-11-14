#' CatBoost Model for caret
#'
#' Custom model function to integrate CatBoost with caret
#'
#' @import catboost
#' @export
catboostModelInfo <- list(
  label = "CatBoost",
  library = "catboost",
  type = c("Classification", "Regression"),
  parameters = data.frame(
    parameter = c("iterations", "learning_rate", "depth", "l2_leaf_reg"),
    class = c("numeric", "numeric", "numeric", "numeric"),
    label = c("Iterations", "Learning Rate", "Depth", "L2 Leaf Reg")
  ),
  grid = function(x, y, len = NULL, search = "grid") {
    expand.grid(
      iterations = c(100, 200),
      learning_rate = c(0.01, 0.1),
      depth = c(4, 6, 8),
      l2_leaf_reg = c(1, 3, 5)
    )
  },
  loop = NULL,
  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
    if (!requireNamespace("catboost", quietly = TRUE)) {
      stop("The 'catboost' package is required but is not installed.")
    }

    if (is.factor(y)) {
      # Classification
      pool <- catboost::catboost.load_pool(data = x, label = as.numeric(y) - 1)
      params <- list(
        loss_function = "MultiClass",
        iterations = param$iterations,
        learning_rate = param$learning_rate,
        depth = param$depth,
        l2_leaf_reg = param$l2_leaf_reg,
        verbose = FALSE
      )
    } else {
      # Regression
      pool <- catboost::catboost.load_pool(data = x, label = y)
      params <- list(
        loss_function = "RMSE",
        iterations = param$iterations,
        learning_rate = param$learning_rate,
        depth = param$depth,
        l2_leaf_reg = param$l2_leaf_reg,
        verbose = FALSE
      )
    }

    model <- catboost::catboost.train(
      learn_pool = pool,
      params = params
    )

    list(
      object = model,
      lev = lev
    )
  },
  predict = function(modelFit, newdata, submodels = NULL) {
    if (is.factor(modelFit$obsLevels)) {
      preds <- catboost::catboost.predict(
        modelFit$object,
        catboost::catboost.load_pool(newdata),
        prediction_type = "Class"
      )
      factor(modelFit$obsLevels[preds + 1], levels = modelFit$obsLevels)
    } else {
      preds <- catboost::catboost.predict(
        modelFit$object,
        catboost::catboost.load_pool(newdata)
      )
      as.vector(preds)
    }
  },
  prob = function(modelFit, newdata, submodels = NULL) {
    preds <- catboost::catboost.predict(
      modelFit$object,
      catboost::catboost.load_pool(newdata),
      prediction_type = "Probability"
    )
    preds <- matrix(preds, ncol = length(modelFit$obsLevels), byrow = TRUE)
    colnames(preds) <- modelFit$obsLevels
    preds
  },
  predictors = function(x, ...) {
    # Return variable names used in the model
    names(x$object$feature_importances_)
  },
  varImp = NULL,
  levels = function(x) x$lev,
  tags = c("Ensemble Model", "Boosting", "Tree-Based Model"),
  sort = function(x) x
)
