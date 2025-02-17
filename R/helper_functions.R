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

# Helper function: get_default_engine
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


# Helper function to extract engine names from a list of trained workflows
get_engine_names <- function(models) {
  lapply(models, function(model_list) {
    # Extract engine names from each workflow
    engines <- sapply(model_list, function(mod) {
      fit_obj <- tryCatch(
        tune::extract_fit_parsnip(mod),
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


get_model_engine_names <- function(models) {
  result <- c()
  for(model_list in models) {
    for(mod in model_list) {
      fit_obj <- tryCatch(
        tune::extract_fit_parsnip(mod),
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

get_best_model_names <- function(models) {
  bests <- sapply(names(models), function(algo) {
    model_list <- models[[algo]]
    engines <- sapply(model_list, function(mod) {
      fit_obj <- tryCatch(
        tune::extract_fit_parsnip(mod),
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


# Central repository for default parameters
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
         "svm_linear" = list(
           cost = 1
         ),
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
         "naive_Bayes" = list(
           smoothness = 1,
           Laplace = 0
         ),
         # 14. Neural Network (nnet)
         "mlp" = list(
           hidden_units = 5,
           penalty = -1,
           epochs = 100
         ),
         # 15. Deep Learning (keras)
         "deep_learning" = list(
           hidden_units = 10,
           penalty = 0.001,
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

# Function to get default tuning parameters
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





