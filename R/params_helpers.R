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
