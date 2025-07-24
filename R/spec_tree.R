#' Define Random Forest Model Specification
#'
#' @param task Character string specifying the task type: "classification" or "regression".
#' @param train_data Data frame containing the training data.
#' @param label Character string specifying the name of the target variable.
#' @param tune Logical indicating whether to use tuning parameters.
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip rand_forest set_mode set_engine
#' @importFrom dplyr select all_of
#' @importFrom tune tune
#' @noRd
define_rand_forest_spec <- function(task, train_data, label, tuning = FALSE, engine = "ranger") {
  # Remove the label column to get predictors and count them
  predictors <- train_data %>% dplyr::select(-dplyr::all_of(label))
  num_predictors <- ncol(predictors)

  # Retrieve default parameters for a random forest.
  # (Assumes get_default_params returns a list with mtry, trees, and min_n.)
  defaults <- get_default_params("rand_forest", task, num_predictors, engine)

  # Build the base model spec with tuning or default values.
  if (tuning) {
    model_spec <- rand_forest(
      mtry  = tune(),
      trees = tune(),
      min_n = tune()
    )
  } else {
    model_spec <- rand_forest(
      mtry  = defaults$mtry,
      trees = defaults$trees,
      min_n = defaults$min_n
    )
  }

  # Set the model mode (e.g. "classification", "regression", etc.)
  model_spec <- model_spec %>% set_mode(task)

  # Engine-specific parameter settings:
  # - For ranger (the default engine), if doing classification, we enable probability estimates.
  # - For h2o and randomForest, we pass along the number of trees (or a similar argument).
  # - For spark, a seed is provided.
  # - Other engines (e.g., aorsf, partykit) are simply set by name.
  if (engine == "ranger") {
    if (task == "classification") {
      model_spec <- model_spec %>%
        set_engine("ranger", probability = TRUE)
    } else {
      model_spec <- model_spec %>%
        set_engine("ranger")
    }
  } else if (engine == "h2o") {
    # For h2o, you might want to pass lambda_search or nthreads, etc.
    model_spec <- model_spec %>%
      set_engine("h2o")
  } else if (engine == "randomForest") {
    # For randomForest, the argument is ntree instead of trees.
    model_spec <- model_spec %>%
      set_engine("randomForest")
  } else if (engine == "spark") {
    model_spec <- model_spec %>%
      set_engine("spark", seed = 1234)
  } else if (engine == "aorsf") {
    model_spec <- model_spec %>%
      set_engine("aorsf")
  } else if (engine == "partykit") {
    model_spec <- model_spec %>%
      set_engine("partykit")
  } else {
    stop("Unsupported engine specified for rand_forest")
  }

  list(model_spec = model_spec)
}



#' Define bag_tree Model Specification
#'
#' @inheritParams define_decision_tree_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip bag_tree set_mode set_engine
#' @import baguette
#' @importFrom tune tune
#' @noRd
define_bag_tree_spec <- function(task, tuning = FALSE, engine = "rpart") {
  defaults <- get_default_params("bag_tree")
  if (tuning) {
    model_spec <- bag_tree(
      min_n = tune()
    ) %>%
      set_mode(task) %>%
      set_engine(engine, times = 25)
  } else {
    model_spec <- bag_tree(
      min_n = defaults$min_n
    ) %>%
      set_mode(task) %>%
      set_engine(engine, times = 25)
  }
  list(model_spec = model_spec)
}


#' Define Decision Tree Model Specification
#'
#' @inheritParams define_rand_forest_spec
#' @param task A character string indicating the mode (e.g., "classification" or "regression").
#' @param tuning Logical; if \code{TRUE}, the tuning parameters are set to \code{tune()}.
#' @param engine A character string specifying the computational engine. Supported engines include
#'   "rpart", "C5.0", and "partykit".
#'
#' @return List containing the model specification (`model_spec`).
#'
#' @details
#' The available parameters and their defaults vary by engine:
#'
#' - **rpart:**
#'   \itemize{
#'     \item \code{tree_depth}: Tree Depth (default: 30L)
#'     \item \code{min_n}: Minimal Node Size (default: 2L)
#'     \item \code{cost_complexity}: Cost-Complexity Parameter (default: 0.01)
#'   }
#'
#' - **C5.0:**
#'   \itemize{
#'     \item \code{min_n}: Minimal Node Size (default: 2L)
#'   }
#'
#' - **partykit:**
#'   \itemize{
#'     \item \code{tree_depth}: Tree Depth (default: 30L; adjust as needed)
#'     \item \code{min_n}: Minimal Node Size (default: 20L)
#'   }
#'
#' @importFrom parsnip decision_tree set_mode set_engine
#' @importFrom tune tune
#' @noRd
define_decision_tree_spec <- function(task, tuning = FALSE, engine = "rpart") {

  # Set engine-specific defaults
  defaults <- switch(engine,
                     "rpart" = list(tree_depth = 30L, min_n = 2L, cost_complexity = 0.01),
                     "C5.0" = list(min_n = 2L),
                     "partykit" = list(tree_depth = 30L, min_n = 20L),
                     # fallback: assume rpart defaults
                     list(tree_depth = 30L, min_n = 2L, cost_complexity = 0.01)
  )

  # Build the model specification based on the engine and tuning flag.
  if (engine == "rpart") {
    if (tuning) {
      model_spec <- decision_tree(
        tree_depth = tune(),
        min_n = tune(),
        cost_complexity = tune()
      ) %>%
        set_mode(task) %>%
        set_engine(engine)
    } else {
      model_spec <- decision_tree(
        tree_depth = defaults$tree_depth,
        min_n = defaults$min_n,
        cost_complexity = defaults$cost_complexity
      ) %>%
        set_mode(task) %>%
        set_engine(engine)
    }
  } else if (engine == "C5.0") {
    if (tuning) {
      model_spec <- decision_tree(
        min_n = tune()
      ) %>%
        set_mode(task) %>%
        set_engine(engine)
    } else {
      model_spec <- decision_tree(
        min_n = defaults$min_n
      ) %>%
        set_mode(task) %>%
        set_engine(engine)
    }
  } else if (engine == "partykit") {
    if (tuning) {
      model_spec <- decision_tree(
        tree_depth = tune(),
        min_n = tune()
      ) %>%
        set_mode(task) %>%
        set_engine(engine)
    } else {
      model_spec <- decision_tree(
        tree_depth = defaults$tree_depth,
        min_n = defaults$min_n
      ) %>%
        set_mode(task) %>%
        set_engine(engine)
    }
  } else {
    # Fallback for unknown engine: assume rpart defaults
    if (tuning) {
      model_spec <- decision_tree(
        tree_depth = tune(),
        min_n = tune(),
        cost_complexity = tune()
      ) %>%
        set_mode(task) %>%
        set_engine(engine)
    } else {
      model_spec <- decision_tree(
        tree_depth = defaults$tree_depth,
        min_n = defaults$min_n,
        cost_complexity = defaults$cost_complexity
      ) %>%
        set_mode(task) %>%
        set_engine(engine)
    }
  }

  list(model_spec = model_spec)
}


#' Define Logistic Regression Model Specification
#'
#' @param task Character string specifying the task type ("classification").
#' @param tune Logical indicating whether to use tuning parameters.
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip logistic_reg set_mode set_engine
#' @importFrom tune tune
#' @importFrom tune tune
#' @noRd
