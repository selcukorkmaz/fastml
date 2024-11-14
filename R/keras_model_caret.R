#' Keras Model for caret
#'
#' Custom model function to integrate Keras with caret
#'
#' @param x Matrix or data frame of predictor variables.
#' @param y Vector or factor of response variable.
#' @param weights Optional vector of weights.
#' @param param List of tuning parameters.
#' @param lev Levels of the response variable.
#' @param last Logical indicating whether this is the final model.
#' @param classProbs Logical indicating whether to compute class probabilities.
#' @param ... Additional arguments.
#' @return A trained Keras model.
keras_model_caret <- function(x, y, weights, param, lev, last, classProbs, ...) {
  if (!requireNamespace("keras", quietly = TRUE)) {
    stop("The 'keras' package is required but is not installed.")
  }

  keras::k_clear_session()

  # Build the model
  model <- keras::keras_model_sequential()

  input_shape <- ncol(x)

  if (is.factor(y)) {
    # Classification
    num_classes <- length(lev)
    y_encoded <- keras::to_categorical(as.numeric(y) - 1, num_classes = num_classes)

    model %>%
      keras::layer_dense(units = 64, activation = 'relu', input_shape = input_shape) %>%
      keras::layer_dense(units = num_classes, activation = 'softmax')

    model %>% keras::compile(
      loss = 'categorical_crossentropy',
      optimizer = 'adam',
      metrics = 'accuracy'
    )

    history <- model %>% keras::fit(
      x = as.matrix(x),
      y = y_encoded,
      epochs = param$epochs,
      batch_size = param$batch_size,
      verbose = 0
    )
  } else {
    # Regression
    model %>%
      keras::layer_dense(units = 64, activation = 'relu', input_shape = input_shape) %>%
      keras::layer_dense(units = 1)

    model %>% keras::compile(
      loss = 'mean_squared_error',
      optimizer = 'adam',
      metrics = 'mean_squared_error'
    )

    history <- model %>% keras::fit(
      x = as.matrix(x),
      y = y,
      epochs = param$epochs,
      batch_size = param$batch_size,
      verbose = 0
    )
  }

  list(model = model)
}

kerasModelInfo <- list(
  label = "Keras Neural Network",
  library = "keras",
  type = c("Classification", "Regression"),
  parameters = data.frame(
    parameter = c("units", "activation", "epochs", "batch_size"),
    class = c("numeric", "character", "numeric", "numeric"),
    label = c("Units", "Activation", "Epochs", "Batch Size")
  ),
  grid = function(x, y, len = NULL, search = "grid") {
    expand.grid(
      units = c(32, 64),
      activation = c("relu"),
      epochs = c(10, 20),
      batch_size = c(16, 32)
    )
  },
  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
    library(keras)
    keras::k_clear_session()

    model <- keras_model_sequential()
    input_shape <- ncol(x)

    if (is.factor(y)) {
      # Classification
      num_classes <- length(lev)
      y_encoded <- to_categorical(as.numeric(y) - 1, num_classes = num_classes)

      model %>%
        layer_dense(units = param$units, activation = param$activation, input_shape = input_shape) %>%
        layer_dense(units = num_classes, activation = 'softmax')

      model %>% compile(
        loss = 'categorical_crossentropy',
        optimizer = 'adam',
        metrics = 'accuracy'
      )

      history <- model %>% fit(
        x = as.matrix(x),
        y = y_encoded,
        epochs = param$epochs,
        batch_size = param$batch_size,
        verbose = 0
      )
    } else {
      # Regression
      model %>%
        layer_dense(units = param$units, activation = param$activation, input_shape = input_shape) %>%
        layer_dense(units = 1)

      model %>% compile(
        loss = 'mean_squared_error',
        optimizer = 'adam',
        metrics = 'mean_squared_error'
      )

      history <- model %>% fit(
        x = as.matrix(x),
        y = y,
        epochs = param$epochs,
        batch_size = param$batch_size,
        verbose = 0
      )
    }

    list(
      object = model,
      lev = lev
    )
  },
  predict = function(modelFit, newdata, submodels = NULL) {
    library(keras)
    if (is.factor(modelFit$lev)) {
      # Classification
      preds <- predict(modelFit$object, as.matrix(newdata))
      class_preds <- apply(preds, 1, function(x) which.max(x) - 1)
      factor(modelFit$lev[class_preds + 1], levels = modelFit$lev)
    } else {
      # Regression
      preds <- predict(modelFit$object, as.matrix(newdata))
      as.vector(preds)
    }
  },
  prob = function(modelFit, newdata, submodels = NULL) {
    library(keras)
    preds <- predict(modelFit$object, as.matrix(newdata))
    colnames(preds) <- modelFit$lev
    preds
  },
  predictors = function(x, ...) {
    colnames(x)
  },
  varImp = NULL,
  levels = function(x) x$lev,
  tags = c("Neural Network", "Deep Learning"),
  sort = function(x) x
)
