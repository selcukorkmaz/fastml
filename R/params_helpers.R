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
#'   \code{"ridge_reg"}, \code{"lasso_reg"}, and \code{"penalized_cox"}.
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
#'     \item Other algorithms, including \code{"svm_linear"}, \code{"svm_rbf"}, \code{"nearest_neighbor"}, \code{"naive_Bayes"}, \code{"mlp"}, \code{"deep_learning"}, \code{"elastic_net"}, \code{"bayes_glm"}, \code{"pls"}, \code{"linear_reg"}, \code{"ridge_reg"}, and \code{"lasso_reg"}, have their respective default parameter lists.
#'   }
#'
#'@importFrom utils tail
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
          learn_rate = 0.1,
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
          learn_rate = 0.1,
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
             # NOTE: the values below are written on the log10 scale, but this
             # list supplies fixed (untuned) arguments that parsnip reads on the
             # natural scale, where a negative penalty is invalid. parsnip
             # rejects such a value with an error rather than accepting it, so
             # the mismatch fails loudly; it is recorded here so that the next
             # reader does not have to rediscover it.
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
           rbf_sigma = 0.1
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
               penalty      = 0.1,
               epochs       = 100L
             )
           } else if (engine %in% c("brulee")) {
             list(
               hidden_units = 3L,
               penalty      = 0.1,
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
               penalty      = 0.1,
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
           penalty = 0.1,
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
         "ridge_reg" = list(
           penalty = 0.01,
           mixture = 0
         ),
         # 24. Lasso Regression
         "lasso_reg" = list(
           penalty = 0.01,
           mixture = 1
         ),
         "penalized_cox" = list(
           penalty = 0.01,
           mixture = 1
         ),
         NULL)
}

#' Get Default Tuning Parameters
#'
#' Returns a list of default tuning parameter ranges for a specified algorithm based on the provided training data, outcome label, and engine.
#'
#' @param algo A character string specifying the algorithm name. Supported values include: \code{"rand_forest"}, \code{"C5_rules"}, \code{"xgboost"}, \code{"lightgbm"}, \code{"logistic_reg"}, \code{"multinom_reg"}, \code{"decision_tree"}, \code{"svm_linear"}, \code{"svm_rbf"}, \code{"nearest_neighbor"}, \code{"naive_Bayes"}, \code{"mlp"}, \code{"deep_learning"}, \code{"discrim_linear"}, \code{"discrim_quad"}, \code{"bag_tree"}, \code{"elastic_net"}, \code{"bayes_glm"}, \code{"pls"}, \code{"linear_reg"}, \code{"ridge_reg"}, and \code{"lasso_reg"}.
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
  # Determine the number of predictors. A survival label names two columns and
  # sometimes three, so the columns are dropped by name rather than through
  # sym(), which accepts only a single symbol and errored on every survival
  # family before reaching the ranges below.
  num_predictors <- ncol(train_data %>% dplyr::select(-dplyr::all_of(label)))

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
           # Ranges in this registry are written on the scale dials stores,
           # which for loss_reduction is log10. The intended range is the
           # documented [0, 5]; zero has no logarithm, so the lower bound is
           # the smallest value dials represents, which is 1e-10.
           loss_reduction = c(-10, log10(5)),
           min_n = c(2, 5),
           sample_size = c(0.5, 0.99),
           mtry = c(1, num_predictors)
         ),

         # 6. LightGBM
         "lightgbm" = list(
           trees = c(50, 150),  # Reduced range for efficiency
           tree_depth = c(1, 5),  # Reduced maximum depth
           learn_rate = c(-2, -1),  # log scale
           # Ranges in this registry are written on the scale dials stores,
           # which for loss_reduction is log10. The intended range is the
           # documented [0, 5]; zero has no logarithm, so the lower bound is
           # the smallest value dials represents, which is 1e-10.
           loss_reduction = c(-10, log10(5)),
           min_n = c(2, 5),
           sample_size = c(0.5, 0.99),
           mtry = c(1, num_predictors)
         ),

         # 7. Logistic Regression


         "logistic_reg" = {

           if (engine %in% c("glm", "gee", "glmer", "stan", "stan_glmer")) {
             list(penalty = NULL, mixture = NULL)
           } else if (engine %in% c("brulee", "glmnet", "h2o", "LiblineaR", "spark")) {
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
           # dials::cost() is stored on a base-2 logarithmic scale, not base 10,
           # so the documented range of [1e-3, 1e3] is written as its base-2
           # logarithm here. Writing c(-3, 3) searched [0.125, 8] instead.
           cost = c(log2(1e-3), log2(1e3))
         ),

         # 11. SVM Radial
         "svm_rbf" = list(
           # dials::cost() is stored on a base-2 logarithmic scale (see above).
           cost = c(log2(1e-3), log2(1e3)),
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
           # log10 scale, as this registry uses throughout. The documented
           # range is [1e-5, 0.1]; writing those values directly searched
           # [10^1e-5, 10^0.1], which is roughly 1.0 to 1.26.
           penalty = c(-5, -1),
           epochs = c(100, 150)  # Reduced upper limit
         ),

         # 15. Deep Learning (keras)
         "deep_learning" = list(
           hidden_units = c(10, 30),  # Reduced upper limit
           # log10 scale, as this registry uses throughout. The documented
           # range is [1e-5, 0.1]; writing those values directly searched
           # [10^1e-5, 10^0.1], which is roughly 1.0 to 1.26.
           penalty = c(-5, -1),
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
         "ridge_reg" = list(
           penalty = c(-5, 0)  # log scale
         ),

         # 24. Lasso Regression
         "lasso_reg" = list(
           penalty = c(-5, 0)  # log scale
         ),

         # Default case
         NULL)
}

#' Get Default Parameters with Transparency Warnings
#'
#' Wrapper around \code{get_default_params} that optionally warns when fastml's
#' default parameters differ from parsnip/engine defaults.
#'
#' @param algo Algorithm name.
#' @param task Task type.
#' @param num_predictors Number of predictors (optional).
#' @param engine Engine name (optional).
#' @param warn_param_defaults Logical; if TRUE, warn about parameter differences.
#' @param verbose Logical; if TRUE, print parameter selections.
#'
#' @return A list of default parameters.
#'
#' @keywords internal
#' @export
get_default_params_with_warnings <- function(algo, task, num_predictors = NULL,
                                             engine = NULL, warn_param_defaults = TRUE,
                                             verbose = FALSE) {
  fastml_params <- get_default_params(algo, task, num_predictors, engine)

  if (is.null(fastml_params)) {
    return(fastml_params)
  }

  if (warn_param_defaults || verbose) {
    parsnip_params <- get_parsnip_default_params(algo, engine, task)

    if (!is.null(parsnip_params)) {
      differences <- list()
      common_params <- intersect(names(fastml_params), names(parsnip_params))

      for (param in common_params) {
        fastml_val <- fastml_params[[param]]
        parsnip_val <- parsnip_params[[param]]

        # Skip NULL comparisons
        if (is.null(fastml_val) && is.null(parsnip_val)) next

        # Check if different
        if (is.null(fastml_val) != is.null(parsnip_val) ||
            (!is.null(fastml_val) && !isTRUE(all.equal(fastml_val, parsnip_val)))) {
          differences[[param]] <- list(fastml = fastml_val, parsnip = parsnip_val)
        }
      }

      if (length(differences) > 0) {
        diff_msgs <- vapply(names(differences), function(p) {
          d <- differences[[p]]
          f_str <- if (is.null(d$fastml)) "NULL" else as.character(d$fastml)
          p_str <- if (is.null(d$parsnip)) "engine-default" else as.character(d$parsnip)
          sprintf("%s=%s (parsnip: %s)", p, f_str, p_str)
        }, character(1))

        msg <- sprintf("[%s/%s] fastml parameter defaults: %s",
                       algo, engine, paste(diff_msgs, collapse = ", "))

        if (verbose) {
          message(msg)
        } else if (warn_param_defaults) {
          # Only warn once per algo/engine combination
          warn_key <- paste0("params_", algo, "_", engine)
          if (!exists(warn_key, envir = .fastml_warned_defaults)) {
            warning(msg, "\nUse tune_params to override or engine_params for fixed values.", call. = FALSE)
            assign(warn_key, TRUE, envir = .fastml_warned_defaults)
          }
        }
      }
    }
  }

  fastml_params
}

#' Map tuning values onto a dials parameter's own scale
#'
#' `dials` stores quantitative parameters on their transformed scale. For
#' `penalty`, `learn_rate` and `loss_reduction` that scale is base-10
#' logarithmic, so the stored range of `learn_rate()` is `c(-10, -1)` rather
#' than `c(1e-10, 0.1)`. Users supply tuning values on the natural scale, and
#' passing them to `dials::range_set()` unchanged therefore reinterprets them
#' as logarithms. A requested learning rate of `0.05` becomes `10^0.05`, which
#' is greater than one, and a requested penalty of `0.01` becomes `10^0.01`.
#' Neither substitution is reported. This helper maps supplied values through
#' the parameter's own transform so that the value reaching the engine is the
#' value the user asked for.
#'
#' @param obj A `dials` parameter object.
#' @param value Numeric values on the parameter's natural scale.
#'
#' @return `value` on the parameter's stored scale. Returned unchanged when the
#'   parameter carries no transform.
#'
#' @keywords internal
#' @noRd
fastml_to_param_scale <- function(obj, value) {
  trans <- obj$trans
  if (is.null(trans) || !is.function(trans$transform) || !is.numeric(value)) {
    return(value)
  }
  out <- suppressWarnings(trans$transform(value))
  # Zero is a meaningful request for parameters such as loss_reduction, where
  # it means no minimum gain, but it has no logarithm. The closest value the
  # parameter can represent is its own lower bound, which is what is used. The
  # substitution is not hidden, since the value that results is reported in the
  # `tuning_grid` component of the fitted object.
  lower <- if (is.null(obj$range)) NULL else obj$range$lower
  at_zero <- is.finite(value) & value == 0 & !is.finite(out)
  if (any(at_zero) && !is.null(lower) && length(lower) == 1 &&
      !dials::is_unknown(lower) && is.finite(lower)) {
    out[at_zero] <- lower
  }
  if (any(!is.finite(out))) {
    stop(
      sprintf(
        paste(
          "Tuning value(s) %s are outside the domain of the '%s' parameter,",
          "which is defined on a transformed scale. Supply values on the",
          "natural scale of the parameter."
        ),
        paste(format(value[!is.finite(out)]), collapse = ", "),
        if (is.null(obj$label)) "requested" else as.character(obj$label)
      ),
      call. = FALSE
    )
  }
  out
}

#' Widen a dials parameter's range to admit a set of values
#'
#' `dials::value_set()` rejects values outside the parameter's declared range,
#' and several sensible requests fall outside a default range (`tree_depth`
#' defaults to `c(1, 15)`, so a requested depth of 20 is refused). Users are
#' entitled to search outside a default, so the range is widened to cover what
#' was asked for before the values are set. Unknown bounds, which `mtry` and
#' `num_comp` carry until finalization, are taken from the values themselves.
#'
#' @param obj A `dials` parameter object.
#' @param value Numeric values on the parameter's stored scale.
#'
#' @return `obj` with a range covering `value`.
#'
#' @keywords internal
#' @noRd
fastml_widen_param_range <- function(obj, value) {
  rng <- obj$range
  if (is.null(rng)) return(obj)
  lower <- rng$lower
  upper <- rng$upper
  if (length(lower) != 1 || dials::is_unknown(lower)) lower <- min(value)
  if (length(upper) != 1 || dials::is_unknown(upper)) upper <- max(value)
  lower <- min(c(lower, value))
  upper <- max(c(upper, value))
  if (inherits(obj, "integer_parameter") && is.null(obj$trans)) {
    lower <- as.integer(floor(lower))
    upper <- as.integer(ceiling(upper))
  }
  dials::range_set(obj, c(lower, upper))
}

#' Per-parameter grid levels that preserve every user-supplied value
#'
#' `dials::grid_regular()` takes at most `levels` values per parameter, so a
#' user who supplies four candidate values while `grid_levels` is three would
#' have one of them dropped without notice. Levels are therefore raised, for
#' the affected parameters only, to the number of values the user asked for.
#' Parameters left at their defaults keep `grid_levels`, so the grid does not
#' grow except where the user asked it to.
#'
#' @param param_set A `dials` parameter set.
#' @param tune_values The merged tuning specification, carrying the
#'   `fastml_user_supplied` attribute that records which names came from the
#'   user.
#' @param grid_levels The requested number of levels per parameter.
#'
#' @return A named integer vector of levels, one per parameter in `param_set`.
#'
#' @keywords internal
#' @noRd
fastml_grid_levels <- function(param_set, tune_values, grid_levels) {
  ids <- param_set$id
  if (length(ids) == 0) return(as.integer(grid_levels))
  levels_vec <- rep(as.integer(grid_levels), length(ids))
  names(levels_vec) <- ids
  user_supplied <- attr(tune_values, "fastml_user_supplied")
  if (is.null(user_supplied)) return(levels_vec)
  for (nm in intersect(ids, user_supplied)) {
    n_values <- length(tune_values[[nm]])
    if (n_values > levels_vec[[nm]]) levels_vec[[nm]] <- as.integer(n_values)
  }
  levels_vec
}

#' Describe the tuning grid that was actually searched
#'
#' `summary()` reports the selected configuration, which is the one view under
#' which a corrupted grid is invisible: a selected `min_n` of 20 looks the same
#' whether 20 was one of two values the user named or the low end of a range the
#' package interpolated. This records, per parameter, the values `grid_regular()`
#' will actually use, on the parameter's natural scale, together with whether
#' the parameter came from the user or from the package defaults.
#'
#' @param param_set A finalized and updated `dials` parameter set.
#' @param tune_values The merged tuning specification, carrying the
#'   `fastml_user_supplied` attribute.
#' @param levels_vec Levels per parameter, as returned by
#'   `fastml_grid_levels()`.
#'
#' @return A data frame with columns `parameter`, `source` and `values`, or
#'   `NULL` when nothing is tuned.
#'
#' @keywords internal
#' @noRd
fastml_realized_grid <- function(param_set, tune_values, levels_vec) {
  if (is.null(param_set) || !is.data.frame(param_set) || nrow(param_set) == 0) {
    return(NULL)
  }
  user_supplied <- attr(tune_values, "fastml_user_supplied")
  if (is.null(user_supplied)) user_supplied <- character()
  level_for <- function(id) {
    if (!is.null(names(levels_vec)) && id %in% names(levels_vec)) {
      as.integer(levels_vec[[id]])
    } else {
      as.integer(levels_vec[[1]])
    }
  }
  rows <- lapply(seq_len(nrow(param_set)), function(i) {
    id <- param_set$id[i]
    obj <- param_set$object[[i]]
    vals <- tryCatch(dials::value_seq(obj, level_for(id)), error = function(e) NULL)
    txt <- if (is.null(vals)) {
      "not resolved"
    } else {
      # Each value is formatted on its own, so that a lower bound standing in
      # for a requested zero prints compactly without forcing every other
      # value in the row into scientific notation.
      paste(
        vapply(vals, function(v) format(signif(v, 6), trim = TRUE), character(1)),
        collapse = ", "
      )
    }
    data.frame(
      parameter = id,
      source = if (id %in% user_supplied) "user" else "default",
      values = txt,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
