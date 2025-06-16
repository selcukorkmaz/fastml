#' Get Available Methods
#'
#' Returns a character vector of algorithm names available for either classification or regression tasks.
#'
#' @param type A character string specifying the type of task. Must be either \code{"classification"} or \code{"regression"}. Defaults to \code{c("classification", "regression")} and uses \code{\link[base]{match.arg}} to select one.
#' @param ... Additional arguments (currently not used).
#'
#' @return A character vector containing the names of the available algorithms for the specified task type.
#'
#' @details Depending on the specified \code{type}, the function returns a different set of algorithm names:
#' \itemize{
#'   \item For \code{"classification"}, it returns algorithms such as \code{"logistic_reg"}, \code{"multinom_reg"}, \code{"decision_tree"}, \code{"C5_rules"}, \code{"rand_forest"}, \code{"xgboost"}, \code{"lightgbm"}, \code{"svm_linear"}, \code{"svm_rbf"}, \code{"nearest_neighbor"}, \code{"naive_Bayes"}, \code{"mlp"}, \code{"discrim_linear"}, \code{"discrim_quad"}, and \code{"bag_tree"}.
#'   \item For \code{"regression"}, it returns algorithms such as \code{"linear_reg"}, \code{"ridge_regression"}, \code{"lasso_regression"}, \code{"elastic_net"}, \code{"decision_tree"}, \code{"rand_forest"}, \code{"xgboost"}, \code{"lightgbm"}, \code{"svm_linear"}, \code{"svm_rbf"}, \code{"nearest_neighbor"}, \code{"mlp"}, \code{"pls"}, and \code{"bayes_glm"}.
#' }
#'
#' @export
availableMethods <- function(type = c("classification", "regression"), ...){
    type <- match.arg(type)

    algorithms <- if (type == "classification"){
      c(
        "logistic_reg",
        "multinom_reg",
        "decision_tree",
        "C5_rules",
        "rand_forest",
        "xgboost",
        "lightgbm",
        "svm_linear",
        "svm_rbf",
        "nearest_neighbor",
        "naive_Bayes",
        "mlp",
        "discrim_linear",
        "discrim_quad",
        "bag_tree"
      )
    } else {
      c(
        "linear_reg",
        "ridge_regression",
        "lasso_regression",
        "elastic_net",
        "decision_tree",
        "rand_forest",
        "xgboost",
        "lightgbm",
        "svm_linear",
        "svm_rbf",
        "nearest_neighbor",
        "mlp",
        "pls",
        "bayes_glm"
      )
    }

    return(algorithms)
  }

#' Get Default Engine
#'
#' Returns the default engine corresponding to the specified algorithm.
#'
#' @param algo A character string specifying the name of the algorithm. The value should match one of the supported algorithm names.
#'
#' @return A character string containing the default engine name associated with the provided algorithm.
#'
#' @details The function uses a \code{switch} statement to select the default engine based on the given algorithm. If the provided algorithm does not have a defined default engine, the function terminates with an error.
#'
#'
#' @export
get_default_engine <- function(algo) {
  switch(algo,
         "lightgbm" = "lightgbm",
         "xgboost" = "xgboost",
         "C5_rules" = "C5.0",
         "rand_forest" = "ranger",
         "ranger" = "ranger",
         "logistic_reg" = "glm",
         "multinom_reg" = "nnet",
         "decision_tree" = "rpart",
         "svm_linear" = "kernlab",
         "svm_rbf" = "kernlab",
         "nearest_neighbor" = "kknn",
         "naive_Bayes" = "klaR",
         "mlp" = "nnet",
         "discrim_linear" = "MASS",
         "discrim_quad" = "MASS",
         "bag_tree" = "rpart",
         "elastic_net" = "glmnet",
         "bayes_glm" = "stan",
         "pls" = "mixOmics",
         "linear_reg" = "lm",
         "ridge_regression" = "glmnet",
         "lasso_regression" = "glmnet",
         "deep_learning" = "keras",
         stop("No default engine defined for algorithm: ", algo)
  )
}


#' Get Engine Names from Model Workflows
#'
#' Extracts and returns a list of unique engine names from a list of model workflows.
#'
#' @param models A list where each element is a list of model workflows. Each workflow is expected to contain a fitted model that can be processed with \code{tune::extract_fit_parsnip}.
#'
#' @return A list of character vectors. Each vector contains the unique engine names extracted from the corresponding element of \code{models}.
#'
#' @details The function applies \code{tune::extract_fit_parsnip} to each model workflow to extract the fitted model object. It then retrieves the engine name from the model specification (\code{spec$engine}). If the extraction fails, \code{NA_character_} is returned for that workflow. Finally, the function removes any duplicate engine names using \code{unique}.
#'
#' @importFrom tune extract_fit_parsnip
#'
#' @export
get_engine_names <- function(models) {
  lapply(models, function(model_list) {
    # Extract engine names from each workflow
    engines <- sapply(model_list, function(mod) {
      fit_obj <- tryCatch(
        extract_fit_parsnip(mod),
        error = function(e) NULL
      )
      if (!is.null(fit_obj)) {
        fit_obj$spec$engine
      } else {
        NA_character_
      }
    })
    # Remove names and duplicate entries
    unique(unname(engines))
  })
}

#' Get Model Engine Names
#'
#' Extracts and returns a named vector mapping algorithm names to engine names from a nested list of model workflows.
#'
#' @param models A nested list of model workflows. Each inner list should contain model objects from which a fitted model can be extracted using \code{tune::extract_fit_parsnip}.
#'
#' @return A named character vector where the names correspond to algorithm names (e.g., \code{"rand_forest"}, \code{"logistic_reg"}) and the values correspond to the associated engine names (e.g., \code{"ranger"}, \code{"glm"}).
#'
#' @details The function iterates over a nested list of model workflows and, for each workflow, attempts to extract the fitted model object using \code{tune::extract_fit_parsnip}. If successful, it retrieves the algorithm name from the first element of the class attribute of the model specification and the engine name from the specification. The results are combined into a named vector.
#'
#' @importFrom tune extract_fit_parsnip
#' @importFrom stats setNames
#'
#' @export
get_model_engine_names <- function(models) {
  result <- c()
  for(model_list in models) {
    for(mod in model_list) {
      fit_obj <- tryCatch(
        extract_fit_parsnip(mod),
        error = function(e) NULL
      )
      if (!is.null(fit_obj)) {
        algo   <- class(fit_obj$spec)[1]   # e.g., "rand_forest" or "logistic_reg"
        engine <- fit_obj$spec$engine       # e.g., "ranger", "randomForest", "glm"
        result <- c(result, setNames(engine, algo))
      }
    }
  }
  result
}

#' Get Best Model Names
#'
#' Extracts and returns the best engine names from a named list of model workflows.
#'
#' @param models A named list where each element corresponds to an algorithm and contains a list of model workflows.
#'   Each workflow should be compatible with \code{tune::extract_fit_parsnip}.
#'
#' @return A named character vector. The names of the vector correspond to the algorithm names, and the values represent the chosen best engine name for that algorithm.
#'
#' @details For each algorithm, the function extracts the engine names from the model workflows using \code{tune::extract_fit_parsnip}.
#'   It then chooses \code{"randomForest"} if it is available; otherwise, it selects the first non-\code{NA} engine.
#'   If no engine names can be extracted for an algorithm, \code{NA_character_} is returned.
#'
#' @importFrom tune extract_fit_parsnip
#'
#' @export
get_best_model_names <- function(models) {
  bests <- sapply(names(models), function(algo) {
    model_list <- models[[algo]]
    engines <- sapply(model_list, function(mod) {
      fit_obj <- tryCatch(
        extract_fit_parsnip(mod),
        error = function(e) NULL
      )
      if (!is.null(fit_obj)) {
        fit_obj$spec$engine
      } else {
        NA_character_
      }
    })
    # Choose "randomForest" if available; otherwise, take the first non-NA engine.
    if ("randomForest" %in% engines) {
      "randomForest"
    } else {
      non_na_engines <- engines[!is.na(engines)]
      if (length(non_na_engines) > 0) non_na_engines[1] else NA_character_
    }
  })

  bests
}

#' Get Best Workflows
#'
#' Extracts the best workflows from a nested list of model workflows based on the provided best model names.
#'
#' @param models A nested list of model workflows. Each element should correspond to an algorithm and contain sublists keyed by engine names.
#' @param best_model_name A named character vector where the names represent algorithm names and the values represent the chosen best engine for each algorithm.
#'
#' @return A named list of workflows corresponding to the best engine for each algorithm. Each list element is named in the format \code{"algorithm (engine)"}.
#'
#' @details The function iterates over each element in \code{best_model_name} and attempts to extract the corresponding workflow from \code{models} using the specified engine. If the workflow for an algorithm-engine pair is not found, a warning is issued and \code{NULL} is returned for that entry.
#'
#' @export
get_best_workflows <- function(models, best_model_name) {
  # For each element in best_model_name, extract the corresponding workflow
  best_list <- lapply(seq_along(best_model_name), function(i) {
    algo   <- names(best_model_name)[i]  # e.g., "rand_forest"
    engine <- best_model_name[i]         # e.g., "ranger" or "randomForest"
    if (!is.null(models[[algo]]) && !is.null(models[[algo]][[engine]])) {
      models[[algo]][[engine]]
    } else {
      warning(paste("No workflow found for", algo, "with engine", engine))
      NULL
    }
  })

  # Name each element in the output using a combined label (e.g., "rand_forest (ranger)")
  names(best_list) <- paste0(names(best_model_name), " (", best_model_name, ")")
  best_list
}

#' Flatten and Rename Models
#'
#' Flattens a nested list of models and renames the elements by combining the outer and inner list names.
#'
#' @param models A nested list of models. The outer list should have names. If an inner element is a named list, the names will be combined with the outer name in the format \code{"outer_name (inner_name)"}.
#'
#' @return A flattened list with each element renamed according to its original outer and inner list names.
#'
#' @details The function iterates over each element of the outer list. For each element, if it is a list with names, the function concatenates the outer list name and the inner names using \code{paste0} and \code{setNames}. If an element is not a list or does not have names, it is included in the result without modification.
#'
#' @importFrom stats setNames
#'
#' @export
flatten_and_rename_models <- function(models) {
  # Initialize an empty list to store the flattened results
  flattened <- list()

  # Loop over each element in the outer list
  for (outer_name in names(models)) {
    inner <- models[[outer_name]]

    # If the inner element is a list with names, then combine names
    if (is.list(inner) && !is.null(names(inner))) {
      # Create new names by combining outer and inner names
      new_names <- paste0(outer_name, " (", names(inner), ")")
      # Set the names and append to the flattened list
      flattened <- c(flattened, setNames(inner, new_names))
    } else {
      # If the outer element is not a list (or not named), keep it as is
      flattened[[outer_name]] <- inner
    }
  }

  return(flattened)
}

#' Get Default Parameters for an Algorithm
#'
#' Returns a list of default tuning parameters for the specified algorithm based on the task type, number of predictors, and engine.
#'
#' @param algo A character string specifying the algorithm name. Supported values include:
#'   \code{"rand_forest"}, \code{"C5_rules"}, \code{"xgboost"}, \code{"lightgbm"},
#'   \code{"logistic_reg"}, \code{"multinom_reg"}, \code{"decision_tree"}, \code{"svm_linear"},
#'   \code{"svm_rbf"}, \code{"nearest_neighbor"}, \code{"naive_Bayes"}, \code{"mlp"},
#'   \code{"deep_learning"}, \code{"discrim_linear"}, \code{"discrim_quad"}, \code{"bag_tree"},
#'   \code{"elastic_net"}, \code{"bayes_glm"}, \code{"pls"}, \code{"linear_reg"},
#'   \code{"ridge_regression"}, and \code{"lasso_regression"}.
#'
#' @param task A character string specifying the task type, typically \code{"classification"} or \code{"regression"}.
#'
#' @param num_predictors An optional numeric value indicating the number of predictors. This value is used to compute default values for parameters such as \code{mtry}. Defaults to \code{NULL}.
#'
#' @param engine An optional character string specifying the engine to use. If not provided, a default engine is chosen where applicable.
#'
#' @return A list of default parameter settings for the specified algorithm. If the algorithm is not recognized, the function returns \code{NULL}.
#'
#' @details The function employs a \code{switch} statement to select and return a list of default parameters tailored for the given algorithm, task, and engine. The defaults vary by algorithm and, in some cases, by engine. For example:
#'   \itemize{
#'     \item For \code{"rand_forest"}, if \code{engine} is not provided, it defaults to \code{"ranger"}. The parameters such as \code{mtry}, \code{trees}, and \code{min_n} are computed based on the task and the number of predictors.
#'     \item For \code{"C5_rules"}, the defaults include \code{trees}, \code{min_n}, and \code{sample_size}.
#'     \item For \code{"xgboost"} and \code{"lightgbm"}, default values are provided for parameters like tree depth, learning rate, and sample size.
#'     \item For \code{"logistic_reg"} and \code{"multinom_reg"}, the function returns defaults for regularization parameters (\code{penalty} and \code{mixture}) that vary with the specified engine.
#'     \item For \code{"decision_tree"}, the parameters (such as \code{tree_depth}, \code{min_n}, and \code{cost_complexity}) are set based on the engine (e.g., \code{"rpart"}, \code{"C5.0"}, \code{"partykit"}, \code{"spark"}).
#'     \item Other algorithms, including \code{"svm_linear"}, \code{"svm_rbf"}, \code{"nearest_neighbor"}, \code{"naive_Bayes"}, \code{"mlp"}, \code{"deep_learning"}, \code{"elastic_net"}, \code{"bayes_glm"}, \code{"pls"}, \code{"linear_reg"}, \code{"ridge_regression"}, and \code{"lasso_regression"}, have their respective default parameter lists.
#'   }
#'
#' @export
get_default_params <- function(algo, task, num_predictors = NULL, engine = NULL) {
  switch(algo,
         # 1. Random Forest
         "rand_forest" = {
           # Set a default engine if not provided:
           if (is.null(engine)) engine <- "ranger"

           if (engine == "ranger") {
             list(
               mtry  = if (!is.null(num_predictors)) floor(sqrt(num_predictors)) else 2,
               trees = 500L,
               min_n = if (task == "regression") 5 else 10
             )
           } else if (engine == "aorsf") {
             list(
               mtry           = if (!is.null(num_predictors)) ceiling(sqrt(num_predictors)) else 2,
               trees          = 500L,
               min_n          = 5L,
               split_min_stat = 3.841459  # Engine-specific tuning parameter
             )
           } else if (engine == "h2o") {
             list(
               mtry  = if (!is.null(num_predictors)) {
                 if (task == "classification") floor(sqrt(num_predictors)) else floor(num_predictors / 3)
               } else 2,
               trees = 50L,
               min_n = 2
             )
           } else if (engine == "partykit") {
             list(
               mtry  = if (!is.null(num_predictors)) {
                 if (task == "classification") floor(sqrt(num_predictors)) else floor(num_predictors / 3)
               } else 2,
               trees = 500L,
               min_n = 20L
             )
           } else if (engine == "randomForest") {
             list(
               mtry  = if (!is.null(num_predictors)) {
                 if (task == "classification") floor(sqrt(num_predictors)) else floor(num_predictors / 3)
               } else 2,
               trees = 500L,
               min_n = if (task == "regression") 5 else 10
             )
           } else if (engine == "spark") {
             list(
               mtry  = if (!is.null(num_predictors)) {
                 if (task == "classification") floor(sqrt(num_predictors)) else floor(num_predictors / 3)
               } else 2,
               trees = 20L,
               min_n = 1L
             )
           } else {
             # Fallback defaults (similar to ranger)
             list(
               mtry  = if (!is.null(num_predictors)) floor(sqrt(num_predictors)) else 2,
               trees = 500L,
               min_n = if (task == "regression") 5 else 10
             )
           }
         },
         # 4. C5_rules
         "C5_rules" = list(
           trees = 50,
           min_n = 5,
           sample_size = 0.5
         ),
         # 5. XGBoost
         "xgboost" = list(
           tree_depth = 6L,
           trees = 15L,
           learn_rate = -1,
           mtry =  if (!is.null(num_predictors)) max(1, floor(sqrt(num_predictors))) else 2,
           min_n =  2,
           loss_reduction = 0.0,
           sample_size =  0.5,
           stop_iter =  Inf
         ),
         # 6. LightGBM
         "lightgbm" = list(
           trees = 100,
           tree_depth = 3,
           learn_rate = -1,
           loss_reduction = 0,
           min_n = 5,
           sample_size = 0.5,
           mtry = if (!is.null(num_predictors)) max(1, floor(sqrt(num_predictors))) else 2
         ),
         # 7. Logistic Regression default parameters

         "logistic_reg" = {
           if (engine %in% c("glm")) {
             list(
               penalty = NULL,
               mixture = NULL
             )
           } else if (engine %in% c("gee")) {
             list(
               penalty = NULL,
               mixture = NULL
             )
           } else if (engine %in% c("glmer")) {
             list(
               penalty = NULL,
               mixture = NULL
             )
           } else if (engine %in% c("stan", "stan_glmer")) {
             list(
               penalty = NULL,
               mixture = NULL
             )
           } else if (engine %in% c("brulee")) {
             list(
               penalty = -3,      # corresponds to a raw penalty of 0.001 (log10(0.001) = -3)
               mixture = 0.0
             )
           } else if (engine %in% c("glmnet")) {
             list(
               penalty = -2,      # corresponds to a raw penalty of 0.01 (log10(0.01) = -2)
               mixture = 1.0      # pure lasso
             )
           } else if (engine %in% c("h2o")) {
             list(
               penalty = NULL,
               mixture = NULL
             )
           } else if (engine %in% c("keras")) {
             list(
               penalty = 0.0,     # no regularization
               mixture = NULL
             )
           } else if (engine %in% c("LiblineaR")) {
             list(
               penalty = -2,      # corresponds to a raw penalty of 0.01 (log10(0.01) = -2)
               mixture = 0        # ridge regularization (mixture = 0)
             )
           } else if (engine %in% c("spark")) {
             list(
               penalty = 0.0,     # no regularization
               mixture = 0.0
             )
           } else {
             # Fallback: use engine defaults
             list(penalty = NULL, mixture = NULL)
           }
         },

         "multinom_reg" = {
           if (engine %in% c("nnet")) {
             # nnet::multinom() uses only a penalty (decay) parameter; its default is 0.0.
             list(penalty = 0.0, mixture = NULL)
           } else if (engine %in% c("brulee")) {
             # brulee defaults: penalty = 0.001 and mixture = 0.0.
             list(penalty = 0.0, mixture = 0.0)
           } else if (engine %in% c("glmnet")) {
             # glmnet: by convention, we use a penalty of 0.0 and a mixture default of 1.0 (pure lasso).
             list(penalty = 0.0, mixture = 1.0)
           } else if (engine %in% c("h2o")) {
             # h2o: if no fixed penalty is given, a heuristic is used; here we choose 0.0 with ridge (mixture = 0.0).
             # list(mixture = 0.0)
           } else if (engine %in% c("keras")) {
             # keras_mlp() only uses a penalty parameter (default 0.0); mixture is not applicable.
             list(penalty = 0.0, mixture = NULL)
           } else if (engine %in% c("spark")) {
             # sparklyr: defaults are typically 0.0 for both penalty and mixture.
             list(penalty = 0.0, mixture = 0.0)
           } else {
             # Fallback default if engine not recognized.
             list(penalty = 0.0, mixture = 0.0)
           }
         },

         # 9. Decision Tree
         "decision_tree" = {
           if (engine %in% c("rpart")) {
             # rpart uses three parameters:
             #   tree_depth: 10L, min_n: 5L, cost_complexity: -2 (on log10 scale, i.e., 10^(-2) = 0.01)
             list(tree_depth = 10L, min_n = 5L, cost_complexity = -2)
           } else if (engine %in% c("C5.0")) {
             # C5.0 uses only the minimal node size
             list(min_n = 5L)
           } else if (engine %in% c("partykit")) {
             # partykit uses two parameters:
             #   tree_depth: 10L, min_n: 20L
             list(tree_depth = 10L, min_n = 20L)
           } else if (engine %in% c("spark")) {
             # spark uses two parameters:
             #   tree_depth: 5L, min_n: 2L
             list(tree_depth = 5L, min_n = 2L)
           } else {
             # Fallback defaults (using rpart defaults)
             list(tree_depth = 10L, min_n = 5L, cost_complexity = -2)
           }
         },

         # 10. SVM Linear
         "svm_linear" = {
           if (is.null(engine)) engine <- "kernlab"  # set default engine if not provided

           if (engine %in% c("kernlab")) {
             list(cost = 1, margin = 0.1)
           } else if (engine %in% c("LiblineaR")) {
             list(cost = 1, margin = 0.1)
           }
         },


         # 11. SVM Radial
         "svm_rbf" = list(
           cost = 1,
           rbf_sigma = c(-5, -1)
         ),
         # 12. nearest_neighbor
         "nearest_neighbor" = list(
           neighbors = 5,
           weight_func = "rectangular",
           dist_power = 2
         ),

         # 13. Naive Bayes
         "naive_Bayes" = {
           if (is.null(engine)) engine <- "klaR"  # set default engine if not provided

           if (engine %in% c("klaR", "naivebayes")) {
             list(smoothness = 1.0, Laplace = 0.0)
           } else if (engine %in% c("h2o")) {
             list(Laplace = 0.0)
           } else {
             stop("Unsupported engine specified for naive_Bayes.")
           }
         },

         # 14. Neural Network (nnet)
         "mlp" = {
           if (is.null(engine)) engine <- "nnet"  # set default engine if not provided

           if (engine %in% c("nnet")) {
             list(
               hidden_units = 5L,
               penalty      = -1,
               epochs       = 100L
             )
           } else if (engine %in% c("brulee")) {
             list(
               hidden_units = 3L,
               penalty      = -1,
               epochs       = 100L,
               activation   = "relu",
               # mixture      = 0.0,
               dropout      = 0.0,
               learn_rate   = -1
             )
           } else if (engine %in% c("h2o")) {
             list(
               hidden_units = 200L,
               penalty      = 0.0,
               dropout      = 0.5,
               epochs       = 10L,
               activation   = "relu",
               learn_rate   = 0.005
             )
           } else if (engine %in% c("keras")) {
             list(
               hidden_units = 5L,
               penalty      = -1,
               dropout      = 0.0,
               epochs       = 20L,
               activation   = "softmax"
             )
           } else {
             stop("Unsupported engine specified for mlp.")
           }
         },

         # 15. Deep Learning (keras)
         "deep_learning" = list(
           hidden_units = 10,
           penalty = -1,
           epochs = 50
         ),
         # 16. discrim_linear
         "discrim_linear" = list(),
         # 17. discrim_quad
         "discrim_quad" = list(),
         # 18. bag_tree
         "bag_tree" = list(
           min_n = 5
         ),
         # 19. Elastic Net
         "elastic_net" = list(
           penalty = 0.01,
           mixture = 0.5
         ),
         # 20. Bayesian GLM
         "bayes_glm" = list(),
         # 21. PLS
         "pls" = list(
           num_comp = 2
         ),
         # 22. Linear Regression
         "linear_reg" = list(),
         # 23. Ridge Regression
         "ridge_regression" = list(
           penalty = 0.01,
           mixture = 0
         ),
         # 24. Lasso Regression
         "lasso_regression" = list(
           penalty = 0.01,
           mixture = 1
         ),
         NULL)
}

#' Get Default Tuning Parameters
#'
#' Returns a list of default tuning parameter ranges for a specified algorithm based on the provided training data, outcome label, and engine.
#'
#' @param algo A character string specifying the algorithm name. Supported values include: \code{"rand_forest"}, \code{"C5_rules"}, \code{"xgboost"}, \code{"lightgbm"}, \code{"logistic_reg"}, \code{"multinom_reg"}, \code{"decision_tree"}, \code{"svm_linear"}, \code{"svm_rbf"}, \code{"nearest_neighbor"}, \code{"naive_Bayes"}, \code{"mlp"}, \code{"deep_learning"}, \code{"discrim_linear"}, \code{"discrim_quad"}, \code{"bag_tree"}, \code{"elastic_net"}, \code{"bayes_glm"}, \code{"pls"}, \code{"linear_reg"}, \code{"ridge_regression"}, and \code{"lasso_regression"}.
#'
#' @param train_data A data frame containing the training data.
#'
#' @param label A character string specifying the name of the outcome variable in \code{train_data}. This column is excluded when calculating the number of predictors.
#'
#' @param engine A character string specifying the engine to be used for the algorithm. Different engines may have different tuning parameter ranges.
#'
#' @return A list of tuning parameter ranges for the specified algorithm. If no tuning parameters are defined for the given algorithm, the function returns \code{NULL}.
#'
#' @details The function first determines the number of predictors by removing the outcome variable (specified by \code{label}) from \code{train_data}. It then uses a \code{switch} statement to select a list of default tuning parameter ranges tailored for the specified algorithm and engine. The tuning ranges have been adjusted for efficiency and may include parameters such as \code{mtry}, \code{trees}, \code{min_n}, and others depending on the algorithm.
#'
#' @importFrom dplyr select
#' @importFrom rlang sym
#'
#' @export
get_default_tune_params <- function(algo, train_data, label, engine) {
  # Determine the number of predictors
  num_predictors <- ncol(train_data %>% select(-!!sym(label)))

  switch(algo,
         # 1. Random Forest
         "rand_forest" = list(
           mtry = c(1, max(1, floor(sqrt(num_predictors)))),
           trees = c(100, 200),  # Reduced upper limit for efficiency
           min_n = c(2, 5)
         ),

         # 4. C5_rules
         "C5_rules" = list(
           trees = c(1, 50),  # Reduced upper limit for efficiency
           min_n = c(2, 5)
         ),

         # 5. XGBoost
         "xgboost" = list(
           trees = c(50, 150),  # Reduced range for efficiency
           tree_depth = c(1, 5),  # Reduced maximum depth
           learn_rate = c(-2, -1),  # log scale
           loss_reduction = c(0, 5),  # Reduced upper limit
           min_n = c(2, 5),
           sample_size = c(0.5, 0.99),
           mtry = c(1, num_predictors)
         ),

         # 6. LightGBM
         "lightgbm" = list(
           trees = c(50, 150),  # Reduced range for efficiency
           tree_depth = c(1, 5),  # Reduced maximum depth
           learn_rate = c(-2, -1),  # log scale
           loss_reduction = c(0, 5),  # Reduced upper limit
           min_n = c(2, 5),
           sample_size = c(0.5, 0.99),
           mtry = c(1, num_predictors)
         ),

         # 7. Logistic Regression


         "logistic_reg" = {

           if (engine %in% c("glm", "gee", "glmer", "stan", "stan_glmer")) {
             list(penalty = NULL, mixture = NULL)
           } else if (engine %in% c("brulee", "glmnet", "h20", "LiblineaR", "spark")) {
             list(penalty = c(-5, 0), mixture = c(0, 1))
           } else if (engine %in% c("keras")) {
             list(penalty = c(-5, 0), mixture = NULL)
           } else {
             # Default if engine not recognized
             list(penalty = c(-5, 0), mixture = c(0, 1))
           }

         },

         "multinom_reg" = {
           if (engine %in% c("nnet")) {
             # nnet::multinom() uses only a penalty (decay) parameter; its default is 0.0.
             list(penalty = 0.0, mixture = NULL)
           } else if (engine %in% c("brulee")) {
             # brulee defaults: penalty = 0.001 and mixture = 0.0.
             list(penalty = 0.001, mixture = 0.0)
           } else if (engine %in% c("glmnet")) {
             # glmnet: by convention, we use a penalty of 0.0 and a mixture default of 1.0 (pure lasso).
             list(penalty = 0.0, mixture = 1.0)
           } else if (engine %in% c("h2o")) {
             # h2o: if no fixed penalty is given, a heuristic is used; here we choose 0.0 with ridge (mixture = 0.0).
             list(penalty = 0.0, mixture = 0.0)
           } else if (engine %in% c("keras")) {
             # keras_mlp() only uses a penalty parameter (default 0.0); mixture is not applicable.
             list(penalty = 0.0, mixture = NULL)
           } else if (engine %in% c("spark")) {
             # sparklyr: defaults are typically 0.0 for both penalty and mixture.
             list(penalty = 0.0, mixture = 0.0)
           } else {
             # Fallback default if engine not recognized.
             list(penalty = 0.0, mixture = 0.0)
           }
         },


         # 9. Decision Tree
         "decision_tree" = list(
           cost_complexity = c(-5, 0),  # log scale
           tree_depth = c(1, 5),  # Reduced maximum depth
           min_n = c(2, 5)
         ),

         # 10. SVM Linear
         "svm_linear" = list(
           cost = c(-3, 3)  # log scale
         ),

         # 11. SVM Radial
         "svm_rbf" = list(
           cost = c(-3, 3),  # log scale
           rbf_sigma = c(-9, -1)  # log scale
         ),

         # 12. nearest_neighbor
         "nearest_neighbor" = list(
           neighbors = c(3, 7),  # Narrowed range for efficiency
           dist_power = c(1, 2)
         ),

         # 13. Naive Bayes
         "naive_Bayes" = list(
           smoothness = c(0, 1),
           Laplace = c(0, 1)
         ),

         # 14. Neural Network (nnet)
         "mlp" = list(
           hidden_units = c(1, 5),  # Reduced upper limit
           penalty = c(-5, -1),  # log scale
           epochs = c(100, 150)  # Reduced upper limit
         ),

         # 15. Deep Learning (keras)
         "deep_learning" = list(
           hidden_units = c(10, 30),  # Reduced upper limit
           penalty = c(-5, -1),  # log scale
           epochs = c(50, 100)  # Reduced upper limit
         ),

         # 16. discrim_linear
         "discrim_linear" = NULL,

         # 17. discrim_quad
         "discrim_quad" = NULL,

         # 18. bag_tree
         "bag_tree" = list(
           cost_complexity = c(-5, 0),  # log scale
           tree_depth = c(1, 5),  # Reduced maximum depth
           min_n = c(2, 5)
         ),

         # 19. Elastic Net
         "elastic_net" = list(
           penalty = c(-5, 0),  # log scale
           mixture = c(0, 1)
         ),

         # 20. Bayesian GLM
         "bayes_glm" = NULL,

         # 21. PLS
         "pls" = list(
           num_comp = c(1, min(5, num_predictors))  # Reduced upper limit
         ),

         # 22. Linear Regression
         "linear_reg" = NULL,

         # 23. Ridge Regression
         "ridge_regression" = list(
           penalty = c(-5, 0)  # log scale
         ),

         # 24. Lasso Regression
         "lasso_regression" = list(
           penalty = c(-5, 0)  # log scale
         ),

         # Default case
         NULL)
}

#' Process Model and Compute Performance Metrics
#'
#' Finalizes a tuning result or utilizes an already fitted workflow to generate predictions on test data and compute performance metrics.
#'
#' @param model_obj A model object, which can be either a tuning result (an object inheriting from \code{"tune_results"}) or an already fitted workflow.
#' @param model_id A unique identifier for the model, used in warning messages if issues arise during processing.
#' @param task A character string indicating the type of task, either \code{"classification"} or \code{"regression"}.
#' @param test_data A data frame containing the test data on which predictions will be generated.
#' @param label A character string specifying the name of the outcome variable in \code{test_data}.
#' @param event_class For classification tasks, a character string specifying which event class to consider as positive (accepted values: \code{"first"} or \code{"second"}).
#' @param engine A character string specifying the modeling engine used. This parameter affects prediction types and metric computations.
#' @param train_data A data frame containing the training data used to fit tuned models.
#' @param metric A character string specifying the metric name used to select the best tuning parameters.
#'
#' @return A list with two components:
#'   \describe{
#'     \item{performance}{A data frame of performance metrics. For classification tasks, metrics include accuracy, kappa, sensitivity, specificity, precision, F-measure, and ROC AUC (when applicable). For regression tasks, metrics include RMSE, R-squared, and MAE.}
#'     \item{predictions}{A data frame containing the test data augmented with predicted classes and, when applicable, predicted probabilities.}
#'   }
#'
#' @details The function first checks if \code{model_obj} is a tuning result. If so, it attempts to:
#'   \itemize{
#'     \item Select the best tuning parameters using \code{tune::select_best} (note that the metric used for selection should be defined in the calling environment).
#'     \item Extract the model specification and preprocessor from \code{model_obj} using \code{workflows::pull_workflow_spec} and \code{workflows::pull_workflow_preprocessor}, respectively.
#'     \item Finalize the model specification with the selected parameters via \code{tune::finalize_model}.
#'     \item Rebuild the workflow using \code{workflows::workflow}, \code{workflows::add_recipe}, and \code{workflows::add_model}, and fit the finalized workflow with \code{parsnip::fit} on the supplied \code{train_data}.
#'   }
#'   If \code{model_obj} is already a fitted workflow, it is used directly.
#'
#'   For classification tasks, the function makes class predictions (and probability predictions if \code{engine} is not \code{"LiblineaR"}) and computes performance metrics using functions from the \code{yardstick} package. In binary classification, the positive class is determined based on the \code{event_class} argument and ROC AUC is computed accordingly. For multiclass classification, macro-averaged metrics and ROC AUC (using weighted estimates) are calculated.
#'
#'   For regression tasks, the function predicts outcomes and computes regression metrics (RMSE, R-squared, and MAE).
#'
#'   If the number of predictions does not match the number of rows in \code{test_data}, the function stops with an informative error message regarding missing values and imputation options.
#'
#' @importFrom tune select_best finalize_model
#' @importFrom workflows pull_workflow_spec pull_workflow_preprocessor workflow add_recipe add_model
#' @importFrom parsnip fit
#' @importFrom dplyr select mutate bind_cols bind_rows
#' @importFrom yardstick metric_set accuracy kap sens spec precision f_meas roc_auc rmse rsq mae
#' @importFrom tibble tibble
#' @importFrom rlang sym
#' @importFrom stats predict
#' @importFrom magrittr %>%
#'
#' @export
process_model <- function(model_obj, model_id, task, test_data, label, event_class,
                          engine, train_data, metric) {
  # If the model object is a tuning result, finalize the workflow
  if (inherits(model_obj, "tune_results")) {
    best_params <- tryCatch({
      tune::select_best(model_obj, metric = metric)
    }, error = function(e) {
      warning(paste("Could not select best parameters for model", model_id, ":", e$message))
      return(NULL)
    })
    if (is.null(best_params)) return(NULL)

    model_spec <- workflows::pull_workflow_spec(model_obj)
    model_recipe <- workflows::pull_workflow_preprocessor(model_obj)

    final_model_spec <- tune::finalize_model(model_spec, best_params)
    final_workflow <- workflows::workflow() %>%
      workflows::add_recipe(model_recipe) %>%
      workflows::add_model(final_model_spec)

    final_model <- parsnip::fit(final_workflow, data = train_data)
  } else {
    # Otherwise, assume the model is already a fitted workflow
    final_model <- model_obj
  }

  # Make predictions and compute performance metrics
  if (task == "classification") {

    pred_class <- predict(final_model, new_data = test_data, type = "class")$.pred_class


    if (!is.null(engine) && !is.na(engine) && engine != "LiblineaR") {
      pred_prob <- predict(final_model, new_data = test_data, type = "prob")
    }



    if(nrow(test_data) != length(pred_class)) {
      stop('The dataset has missing values. To handle this, set impute_method = "remove" to delete rows with missing values,
             or use an imputation method such as "medianImpute" to fill missing values with the column median, "knnImpute" to
             estimate missing values using k-Nearest Neighbors, "bagImpute" to apply bagging for imputation, "mice" to use
             Multiple Imputation by Chained Equations, or "missForest" to use random forests for imputation.')
    }

    data_metrics <- test_data %>%
      dplyr::select(truth = !!rlang::sym(label)) %>%
      dplyr::mutate(estimate = pred_class) %>%
      {
        if (!is.null(engine) && !is.na(engine) && engine != "LiblineaR") {
          dplyr::bind_cols(., pred_prob)
        } else {
          .
        }
      }

    if(all(grepl("^\\.pred_p", names(data_metrics)[3:4]))){

      pred_name = ".pred_p"
    }else{

      pred_name = ".pred_"
    }

    num_classes <- length(unique(data_metrics$truth))

    if (num_classes == 2) {
      # Determine the positive class based on event_class parameter
      if(event_class == "first"){
        positive_class <- levels(data_metrics$truth)[1]
      } else if(event_class == "second"){
        positive_class <- levels(data_metrics$truth)[2]
      } else {
        stop("Invalid event_class argument. It should be either 'first' or 'second'.")
      }

      # Compute standard classification metrics
      metrics_class <- yardstick::metric_set(
        yardstick::accuracy,
        yardstick::kap,
        yardstick::sens,
        yardstick::spec,
        yardstick::precision,
        yardstick::f_meas
      )
      perf_class <- metrics_class(data_metrics, truth = truth, estimate = estimate, event_level = event_class)

      if (!is.null(engine) && !is.na(engine) && engine != "LiblineaR") {
        # Compute ROC AUC using the probability column for the positive class
        roc_auc_value <- yardstick::roc_auc(
          data_metrics,
          truth = truth,
          !!rlang::sym(paste0(pred_name, positive_class)),
          event_level = "second"
        )
        if(roc_auc_value$.estimate < 0.50) {
          roc_auc_value <- yardstick::roc_auc(
            data_metrics,
            truth = truth,
            !!rlang::sym(paste0(pred_name, positive_class)),
            event_level = "first"
          )
        }
        perf <- dplyr::bind_rows(perf_class, roc_auc_value)
      }else{

        perf <- perf_class
      }
    }else {
      # Multiclass classification (using macro averaging)
      metrics_class <- yardstick::metric_set(
        yardstick::accuracy,
        yardstick::kap,
        yardstick::sens,
        yardstick::spec,
        yardstick::precision,
        yardstick::f_meas
      )
      perf_class <- metrics_class(
        data_metrics,
        truth = truth,
        estimate = estimate,
        estimator = "macro"
      )

      if (!is.null(engine) && !is.na(engine) && engine != "LiblineaR") {
        prob_cols <- names(pred_prob)
        perf_roc_auc <- yardstick::roc_auc(
          data_metrics,
          truth = truth,
          !!!rlang::syms(prob_cols),
          estimator = "macro_weighted"
        )
        perf <- dplyr::bind_rows(perf_class, perf_roc_auc)
      } else {
        perf <- perf_class
      }
    }
  } else {
    # Regression task
    predictions <- predict(final_model, new_data = test_data)
    pred <- predictions$.pred
    data_metrics <- tibble::tibble(truth = test_data[[label]], estimate = pred)
    metrics_set <- yardstick::metric_set(yardstick::rmse, yardstick::rsq, yardstick::mae)
    perf <- metrics_set(data_metrics, truth = truth, estimate = estimate)
  }

  return(list(performance = perf, predictions = data_metrics))
}

#' Get Best Model Indices by Metric and Group
#'
#' Identifies and returns the indices of rows in a data frame where the specified metric reaches the overall maximum within groups defined by one or more columns.
#'
#' @param df A data frame containing model performance metrics and grouping columns.
#' @param metric A character string specifying the name of the metric column in \code{df}. The metric values are converted to numeric for comparison.
#' @param group_cols A character vector of column names used for grouping. Defaults to \code{c("Model", "Engine")}.
#'
#' @return A numeric vector of row indices in \code{df} corresponding to groups whose maximum metric equals the overall best metric value.
#'
#' @details The function converts the metric values to numeric and creates a combined grouping factor using the specified \code{group_cols}. It then computes the maximum metric value within each group and determines the overall best metric value across the entire data frame. Finally, it returns the indices of rows belonging to groups that achieve this overall maximum.
#'
#' @importFrom stats ave
#'
#' @export
#'
get_best_model_idx <- function(df, metric, group_cols = c("Model", "Engine")) {
  # Convert the metric to numeric in case it's not already
  metric_values <- as.numeric(as.character(df[[metric]]))

  # Create a combined grouping factor from the specified columns
  group_values <- interaction(df[, group_cols], drop = TRUE)

  # Compute the maximum metric for each group
  if(metric %in% c("rmse", "mae")){

    group_val <- ave(metric_values, group_values, FUN = min)
    overall_val <- min(metric_values)


  }else{

    group_val <- ave(metric_values, group_values, FUN = max)
    overall_val <- max(metric_values)


  }


  # Identify groups whose maximum equals the overall maximum
  best_groups <- unique(group_values[group_val == overall_val])

  # Return indices where the group is one of the best groups
  idx <- which(group_values %in% best_groups)
  return(idx)
}

