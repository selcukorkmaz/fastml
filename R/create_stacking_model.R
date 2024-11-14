#' Create Stacking Model
#'
#' Builds a stacking ensemble model using the base models.
#'
#' @param models List of base models.
#' @param train_data Training data.
#' @param label Name of the target variable.
#' @param task Task type ("classification" or "regression").
#' @param metric Performance metric to optimize.
#' @param control Training control object.
#' @param seed Random seed for reproducibility.
#' @return A stacking model.
create_stacking_model <- function(models, train_data, label, task, metric, control, seed) {
  # Generate predictions from base models
  base_preds <- lapply(models, function(model) {
    predict(model, train_data)
  })

  # Combine predictions into a data frame
  pred_df <- as.data.frame(base_preds)
  names(pred_df) <- paste0("pred_", names(models))

  # Add the true labels
  pred_df[[label]] <- train_data[[label]]

  # Define formula for meta-model
  formula <- as.formula(paste(label, "~ ."))

  # Train meta-model (e.g., linear regression for regression, logistic regression for classification)
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
    base_models = models,
    meta_model = meta_model,
    method = "stacking"
  )
}
