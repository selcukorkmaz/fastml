#' Create Blending Model
#'
#' Builds a blending ensemble model using the base models.
#'
#' @param models List of base models.
#' @param train_data Training data.
#' @param label Name of the target variable.
#' @param task Task type ("classification" or "regression").
#' @param metric Performance metric to optimize.
#' @param control Training control object.
#' @param seed Random seed for reproducibility.
#' @return A blending model.
#' @export
create_blending_model <- function(models, train_data, label, task, metric, control, seed) {
  # Blending involves splitting the training data into two parts
  # Part 1: Train base models
  # Part 2: Train meta-model using predictions from base models on part 2

  # Split the training data into two parts
  set.seed(seed)
  blend_index <- caret::createDataPartition(train_data[[label]], p = 0.7, list = FALSE)

  base_train <- train_data[blend_index, ]
  blend_train <- train_data[-blend_index, ]

  # Train base models on base_train
  base_models <- list()
  for (algo in names(models)) {
    set.seed(seed)
    model <- caret::train(
      as.formula(paste(label, "~ .")),
      data = base_train,
      method = models[[algo]]$method,
      trControl = control,
      tuneGrid = models[[algo]]$bestTune,
      metric = metric
    )
    base_models[[algo]] <- model
  }

  # Generate predictions from base models on blend_train
  base_preds <- lapply(base_models, function(model) {
    predict(model, newdata = blend_train)
  })

  # Combine predictions into a data frame
  pred_df <- as.data.frame(base_preds)
  names(pred_df) <- paste0("pred_", names(base_models))

  # Add the true labels
  pred_df[[label]] <- blend_train[[label]]

  # Define formula for meta-model
  formula <- as.formula(paste(label, "~ ."))

  # Train meta-model
  set.seed(seed)
  if (task == "regression") {
    meta_model <- caret::train(
      formula,
      data = pred_df,
      method = "lm",
      trControl = control,
      metric = metric
    )
  } else {
    meta_model <- caret::train(
      formula,
      data = pred_df,
      method = "glm",
      family = binomial(),
      trControl = control,
      metric = metric
    )
  }

  # Return a list containing base models and meta-model
  list(
    base_models = base_models,
    meta_model = meta_model,
    method = "blending"
  )
}
