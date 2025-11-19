fastml_model_spec_registry <- function() {
  list(
    rand_forest = function(ctx) {
      define_rand_forest_spec(
        ctx$task,
        ctx$train_data,
        ctx$label,
        tuning = ctx$tuning,
        engine = ctx$engine
      )
    },
    logistic_reg = function(ctx) {
      define_logistic_reg_spec(
        ctx$task,
        tuning = ctx$tuning,
        engine = ctx$engine
      )
    },
    multinom_reg = function(ctx) {
      define_multinomial_reg_spec(
        ctx$task,
        tuning = ctx$tuning,
        engine = ctx$engine
      )
    },
    C5_rules = function(ctx) {
      define_C5_rules_spec(
        ctx$task,
        tuning = ctx$tuning,
        engine = ctx$engine
      )
    },
    xgboost = function(ctx) {
      define_xgboost_spec(
        ctx$task,
        ctx$train_data,
        ctx$label,
        tuning = ctx$tuning,
        engine = ctx$engine,
        early_stopping = ctx$early_stopping
      )
    },
    lightgbm = function(ctx) {
      define_lightgbm_spec(
        ctx$task,
        ctx$train_data,
        ctx$label,
        tuning = ctx$tuning,
        engine = ctx$engine,
        early_stopping = ctx$early_stopping
      )
    },
    decision_tree = function(ctx) {
      define_decision_tree_spec(
        ctx$task,
        tuning = ctx$tuning,
        engine = ctx$engine
      )
    },
    svm_linear = function(ctx) {
      define_svm_linear_spec(
        ctx$task,
        tuning = ctx$tuning,
        engine = ctx$engine
      )
    },
    svm_rbf = function(ctx) {
      define_svm_rbf_spec(
        ctx$task,
        tuning = ctx$tuning,
        engine = ctx$engine
      )
    },
    nearest_neighbor = function(ctx) {
      define_nearest_neighbor_spec(
        ctx$task,
        tuning = ctx$tuning,
        engine = ctx$engine
      )
    },
    naive_Bayes = function(ctx) {
      define_naive_Bayes_spec(
        ctx$task,
        tuning = ctx$tuning,
        engine = ctx$engine
      )
    },
    mlp = function(ctx) {
      define_mlp_spec(
        ctx$task,
        tuning = ctx$tuning,
        engine = ctx$engine
      )
    },
    discrim_linear = function(ctx) {
      define_discrim_linear_spec(
        ctx$task,
        engine = ctx$engine
      )
    },
    discrim_quad = function(ctx) {
      define_discrim_quad_spec(
        ctx$task,
        engine = ctx$engine
      )
    },
    bag_tree = function(ctx) {
      define_bag_tree_spec(
        ctx$task,
        tuning = ctx$tuning,
        engine = ctx$engine
      )
    },
    elastic_net = function(ctx) {
      define_elastic_net_spec(
        ctx$task,
        tuning = ctx$tuning,
        engine = ctx$engine
      )
    },
    bayes_glm = function(ctx) {
      define_bayes_glm_spec(
        ctx$task,
        engine = ctx$engine
      )
    },
    pls = function(ctx) {
      define_pls_spec(
        ctx$task,
        tuning = ctx$tuning,
        engine = ctx$engine
      )
    },
    linear_reg = function(ctx) {
      define_linear_reg_spec(
        ctx$task,
        engine = ctx$engine
      )
    },
    ridge_reg = function(ctx) {
      define_ridge_reg_spec(
        ctx$task,
        tuning = ctx$tuning,
        engine = ctx$engine
      )
    },
    lasso_reg = function(ctx) {
      define_lasso_reg_spec(
        ctx$task,
        tuning = ctx$tuning,
        engine = ctx$engine
      )
    }
  )
}

fastml_get_model_spec <- function(algo, context) {
  registry <- fastml_model_spec_registry()
  handler <- registry[[algo]]
  if (is.null(handler)) {
    warning(sprintf("Algorithm %s is not supported or failed to train.", algo))
    return(NULL)
  }
  handler(context)
}
