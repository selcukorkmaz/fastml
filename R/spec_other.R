#' Define Quadratic Discriminant Analysis Model Specification
#'
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip set_mode set_engine discrim_quad
#' @noRd
define_discrim_quad_spec <- function(task, engine = "MASS") {
  if (task != "classification") {
    stop("discrim_quad is only applicable for classification tasks.")
  }
  model_spec <- discrim_quad() %>%
    set_mode("classification") %>%
    set_engine(engine)
  list(model_spec = model_spec)
}

#' Define Linear Discriminant Analysis Model Specification
#'
#' @inheritParams define_logistic_reg_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip set_mode set_engine discrim_linear
#' @noRd
define_discrim_linear_spec <- function(task, engine = "MASS") {
  if (task != "classification") {
    stop("discrim_linear is only applicable for classification tasks.")
  }
  model_spec <- discrim_linear() %>%
    set_mode("classification") %>%
    set_engine(engine)
  list(model_spec = model_spec)
}

#' Define Neural Network Model Specification (nnet)
#'
#' @inheritParams define_rand_forest_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip mlp set_mode set_engine
#' @importFrom tune tune
#' @noRd
define_mlp_spec <- function(task, tuning = FALSE, engine = "nnet") {

  defaults <- get_default_params("mlp", task, num_predictors = NULL, engine)

  if (tuning) {
    if (engine == "nnet") {
      model_spec <- mlp(
        hidden_units = tune(),
        penalty = tune(),
        epochs = tune()
      ) %>%
        set_mode(task) %>%
        set_engine(engine)
    } else if (engine == "brulee") {
      model_spec <- mlp(
        hidden_units = tune(),
        penalty = tune(),
        epochs = tune(),
        activation = tune(),
        dropout = tune(),
        learn_rate = tune()
        # mixture = tune()
      ) %>%
        set_mode(task) %>%
        set_engine(engine)
    } else if (engine == "h2o") {
      model_spec <- mlp(
        hidden_units = tune(),
        penalty = tune(),
        dropout = tune(),
        epochs = tune(),
        activation = tune(),
        learn_rate = tune()
      ) %>%
        set_mode(task) %>%
        set_engine(engine)
    } else if (engine == "keras") {
      model_spec <- mlp(
        hidden_units = tune(),
        penalty = tune(),
        dropout = tune(),
        epochs = tune(),
        activation = tune()
      ) %>%
        set_mode(task) %>%
        set_engine(engine)
    }
  } else {
    if (engine == "nnet") {
      model_spec <- mlp(
        hidden_units = defaults$hidden_units,
        penalty = defaults$penalty,
        epochs = defaults$epochs
      ) %>%
        set_mode(task) %>%
        set_engine(engine)
    } else if (engine == "brulee") {
      model_spec <- mlp(
        hidden_units = defaults$hidden_units,
        penalty = defaults$penalty,
        epochs = defaults$epochs,
        activation = defaults$activation,
        dropout = defaults$dropout,
        learn_rate = defaults$learn_rate
        # mixture = defaults$mixture
      ) %>%
        set_mode(task) %>%
        set_engine(engine)
    } else if (engine == "h2o") {
      model_spec <- mlp(
        hidden_units = defaults$hidden_units,
        penalty = defaults$penalty,
        dropout = defaults$dropout,
        epochs = defaults$epochs,
        activation = defaults$activation,
        learn_rate = defaults$learn_rate
      ) %>%
        set_mode(task) %>%
        set_engine(engine)
    } else if (engine == "keras") {
      model_spec <- mlp(
        hidden_units = defaults$hidden_units,
        penalty = defaults$penalty,
        dropout = defaults$dropout,
        epochs = defaults$epochs,
        activation = defaults$activation
      ) %>%
        set_mode(task) %>%
        set_engine(engine)
    }
  }

  list(model_spec = model_spec)
}

#' Define Naive Bayes Model Specification
#'
#' @inheritParams define_logistic_reg_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip naive_Bayes set_mode set_engine
#' @import discrim
#' @importFrom tune tune
#' @noRd
define_naive_Bayes_spec <- function(task, tuning = FALSE, engine = "klaR") {
  if (task != "classification") {
    stop("Naive Bayes is only applicable for classification tasks.")
  }

  # Retrieve default parameters for naive_Bayes based on engine.
  defaults <- get_default_params("naive_Bayes", task, engine)

  # Build the base model spec with tuning or default values.
  if (tuning) {
    if (engine %in% c("klaR", "naivebayes")) {
      model_spec <- naive_Bayes(
        smoothness = tune(),
        Laplace = tune()
      )
    }else if (engine == "h2o") {

      model_spec <- naive_Bayes(
        Laplace = tune()
      )
    }
  } else {
    if (engine %in% c("klaR", "naivebayes")) {
      model_spec <- naive_Bayes(
        smoothness = defaults$smoothness,
        Laplace = defaults$Laplace
      )
    }else if (engine == "h2o") {
      model_spec <- naive_Bayes(
        Laplace = defaults$Laplace
      )
    }
  }

  # Set the model mode.
  model_spec <- model_spec %>% set_mode("classification")

  # Engine-specific parameter settings.
  # For the klaR and naivebayes engines, usekernel is set to TRUE by default.
  if (engine %in% c("klaR", "naivebayes")) {
    model_spec <- model_spec %>% set_engine(engine, usekernel = TRUE)
  } else if (engine == "h2o") {
    model_spec <- model_spec %>% set_engine(engine)
  } else {
    stop("Unsupported engine specified for naive_Bayes.")
  }

  list(model_spec = model_spec)
}

#' Define K-Nearest Neighbors Model Specification
#'
#' @inheritParams define_rand_forest_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip nearest_neighbor set_mode set_engine
#' @importFrom tune tune
#' @noRd
define_nearest_neighbor_spec <- function(task, tuning = FALSE, engine = "kknn") {
  defaults <- get_default_params("nearest_neighbor")

  if (tuning) {
    model_spec <- nearest_neighbor(
      neighbors = tune(),
      weight_func = tune(),
      dist_power = tune()
    ) %>%
      set_mode(task) %>%
      set_engine(engine)
  } else {
    model_spec <- nearest_neighbor(
      neighbors = defaults$neighbors,
      weight_func = defaults$weight_func,
      dist_power = defaults$dist_power
    ) %>%
      set_mode(task) %>%
      set_engine(engine)
  }
  list(model_spec = model_spec)
}

#' Define SVM Radial Model Specification
#'
#' @inheritParams define_svm_linear_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip svm_rbf set_mode set_engine
#' @importFrom tune tune
#' @noRd
define_svm_rbf_spec <- function(task, tuning = FALSE, engine = "kernlab") {
  defaults <- get_default_params("svm_rbf")

  if (tuning) {
    model_spec <- svm_rbf(
      cost = tune(),
      rbf_sigma = tune()
    ) %>%
      set_mode(task) %>%
      set_engine(engine)
  } else {
    model_spec <- svm_rbf(
      cost = defaults$cost,
      rbf_sigma = defaults$rbf_sigma
    ) %>%
      set_mode(task) %>%
      set_engine(engine)
  }
  list(model_spec = model_spec)
}

#' Define SVM Linear Model Specification
#'
#' @inheritParams define_rand_forest_spec
#' @return List containing the model specification (`model_spec`).
#' @importFrom parsnip svm_linear set_mode set_engine
#' @importFrom tune tune
#' @noRd
define_svm_linear_spec <- function(task, tuning = FALSE, engine = "kernlab") {
  defaults <- get_default_params("svm_linear")

  if (tuning) {
    if (task == "regression") {
      # For regression, both cost and margin are tunable
      model_spec <- svm_linear(
        cost   = tune(),
        margin = tune()
      ) %>%
        set_mode(task) %>%
        set_engine(engine)
    } else {  # classification
      # For classification, only cost is tuned; margin is not applicable
      model_spec <- svm_linear(
        cost = tune()
      ) %>%
        set_mode(task) %>%
        set_engine(engine)
    }
  } else {
    if (task == "regression") {
      model_spec <- svm_linear(
        cost   = defaults$cost,
        margin = defaults$margin  # defaults$margin will be 0.1 for kernlab or missing for LiblineaR
      ) %>%
        set_mode(task) %>%
        set_engine(engine)
    } else {  # classification
      model_spec <- svm_linear(
        cost = defaults$cost
      ) %>%
        set_mode(task) %>%
        set_engine(engine)
    }
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
