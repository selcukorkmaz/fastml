#' Define LightGBM Model Specification
#'
#' @inheritParams define_rand_forest_spec
#' @param early_stopping Logical. If TRUE, early stopping parameters are passed
#'   to the engine.
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip boost_tree set_mode set_engine
#' @importFrom dplyr select all_of
#' @importFrom tune tune
#' @noRd
define_lightgbm_spec <- function(task, train_data, label, tuning = FALSE, engine = "lightgbm", early_stopping = FALSE) {
  # Ensure the lightgbm package is installed.
  if (!requireNamespace("lightgbm", quietly = TRUE)) {
    stop("The 'lightgbm' package is required but is not installed.")
  }

  # Determine the number of predictors (all columns except the label).
  num_predictors <- ncol(train_data %>% dplyr::select(-dplyr::all_of(label)))

  # Retrieve default parameters for lightgbm.
  # The helper is expected to return a list with elements:
  # trees, tree_depth, learn_rate, mtry, min_n, loss_reduction, sample_size.
  defaults <- get_default_params("lightgbm", task, num_predictors, engine)

  # Define the model specification using boost_tree().
  if (tuning) {
    model_spec <- boost_tree(
      trees           = tune(),
      tree_depth      = tune(),
      learn_rate      = tune(),
      mtry            = tune(),
      min_n           = tune(),
      loss_reduction  = tune(),
      sample_size     = tune()
    )
  } else {
    model_spec <- boost_tree(
      trees           = defaults$trees,
      tree_depth      = defaults$tree_depth,
      learn_rate      = defaults$learn_rate,
      mtry            = defaults$mtry,
      min_n           = defaults$min_n,
      loss_reduction  = defaults$loss_reduction,
      sample_size     = defaults$sample_size
    )
  }

  # Set the mode and specify engine-specific parameters.
  # - counts = TRUE means that mtry is interpreted as a count (and later converted to a proportion)
  # - bagging_freq = 1 will enable bagging at every iteration when sample_size (bagging fraction) is less than 1.
  # - verbose = -1 quiets lightgbm's output.
  # - num_threads = 0 instructs lightgbm to use all available cores.
  # - seed and deterministic aid in reproducibility.
  model_spec <- model_spec %>%
    set_mode(task)

  engine_args <- list(
    counts        = TRUE,
    bagging_freq  = 1,
    verbose       = -1,
    num_threads   = 0,
    seed          = 123,
    deterministic = TRUE
  )

  if (early_stopping) {
    engine_args$early_stop <- 10
    engine_args$validation <- 0.1
  }

  model_spec <- do.call(set_engine, c(list(object = model_spec, engine = engine), engine_args))

  list(model_spec = model_spec)
}

#' Define XGBoost Model Specification
#'
#' @inheritParams define_rand_forest_spec
#' @param early_stopping Logical. If TRUE, early stopping parameters are passed
#'   to the engine.
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip boost_tree set_mode set_engine
#' @importFrom dplyr select all_of
#' @importFrom tune tune
#' @noRd
define_xgboost_spec <- function(task, train_data, label, tuning = FALSE, engine = "xgboost", early_stopping = FALSE) {
  num_predictors <- ncol(train_data %>% dplyr::select(-dplyr::all_of(label)))
  defaults <- get_default_params("xgboost", num_predictors)

  if (tuning) {
    model_spec <- boost_tree(
      trees = tune(),
      tree_depth = tune(),
      learn_rate = tune(),
      mtry = tune(),
      min_n = tune(),
      loss_reduction = tune(),
      sample_size = tune()
    )
  } else {
    model_spec <- boost_tree(
      trees = defaults$trees,
      tree_depth = defaults$tree_depth,
      learn_rate = defaults$learn_rate,
      mtry = defaults$mtry,
      min_n = defaults$min_n,
      loss_reduction = defaults$loss_reduction,
      sample_size = defaults$sample_size
    )
  }

  model_spec <- model_spec %>%
    set_mode(task)

  engine_args <- list()
  if (early_stopping) {
    engine_args$early_stop <- 10
    engine_args$validation <- 0.1
  }

  model_spec <- do.call(set_engine, c(list(object = model_spec, engine = engine), engine_args))
  list(model_spec = model_spec)
}

#' Define C5_rules Model Specification
#'
#' @inheritParams define_rand_forest_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip boost_tree set_mode set_engine
#' @importFrom tune tune
#' @noRd
define_C5_rules_spec <- function(task, tuning = FALSE, engine = "C5.0") {
  if (task != "classification") {
    stop("C5.0 is only applicable for classification tasks.")
  }
  defaults <- get_default_params("C5_rules")

  if (tuning) {
    model_spec <- boost_tree(
      trees = tune(),
      min_n = tune(),
      sample_size = tune()
    ) %>%
      set_mode("classification") %>%
      set_engine(engine)
  } else {
    model_spec <- boost_tree(
      trees = defaults$trees,
      min_n = defaults$min_n,
      sample_size = defaults$sample_size
    ) %>%
      set_mode("classification") %>%
      set_engine(engine)
  }
  list(model_spec = model_spec)
}
