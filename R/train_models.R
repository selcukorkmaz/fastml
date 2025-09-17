#' Train Specified Machine Learning Algorithms on the Training Data
#'
#' Trains specified machine learning algorithms on the preprocessed training data.
#'
#' @param train_data Preprocessed training data frame.
#' @param train_data Preprocessed training data frame.
#' @param label Name of the target variable.
#' @param task Type of task: "classification", "regression", or "survival".
#' @param algorithms Vector of algorithm names to train.
#' @param resampling_method Resampling method for cross-validation (e.g., "cv", "repeatedcv", "boot", "none").
#' @param folds Number of folds for cross-validation.
#' @param repeats Number of times to repeat cross-validation (only applicable for methods like "repeatedcv").
#' @param resamples Optional rsample object. If provided, custom resampling splits
#'   will be used instead of those created internally.
#' @param tune_params A named list of tuning ranges. For each algorithm, supply a
#'   list of engine-specific parameter values, e.g.
#'   \code{list(rand_forest = list(ranger = list(mtry = c(1, 3)))).}
#' @param metric The performance metric to optimize.
#' @param summaryFunction A custom summary function for model evaluation. Default is \code{NULL}.
#' @param seed An integer value specifying the random seed for reproducibility.
#' @param recipe A recipe object for preprocessing.
#' @param use_default_tuning Logical; if \code{TRUE} and \code{tune_params} is \code{NULL}, tuning is performed using default grids. Tuning also occurs when custom \code{tune_params} are supplied. When \code{FALSE} and no custom parameters are given, the model is fitted once with default settings.
#' @param tuning_strategy A string specifying the tuning strategy. Must be one of
#'   \code{"grid"}, \code{"bayes"}, or \code{"none"}. Adaptive methods may be
#'   used with \code{"grid"}. If \code{"none"} is selected, the workflow is fitted
#'   directly without tuning.
#'   If custom \code{tune_params} are supplied with \code{tuning_strategy = "none"},
#'   they will be ignored with a warning.
#' @param tuning_iterations Number of iterations for Bayesian tuning. Ignored
#'   when \code{tuning_strategy} is not \code{"bayes"}; validation occurs only
#'   for the Bayesian strategy.
#' @param early_stopping Logical for early stopping in Bayesian tuning.
#' @param adaptive Logical indicating whether to use adaptive/racing methods.
#' @param algorithm_engines A named list specifying the engine to use for each algorithm.
#' @importFrom magrittr %>%
#' @importFrom dplyr filter mutate select if_else starts_with
#' @importFrom tibble tibble
#' @importFrom rlang sym
#' @importFrom dials range_set value_set grid_regular grid_latin_hypercube finalize
#' @importFrom parsnip fit extract_parameter_set_dials
#' @importFrom workflows workflow add_model add_recipe
#' @importFrom tune tune_grid control_grid select_best finalize_workflow finalize_model tune_bayes control_grid control_bayes
#' @importFrom yardstick metric_set accuracy kap roc_auc sens spec precision f_meas rmse rsq mae new_class_metric
#' @importFrom rsample vfold_cv bootstraps validation_split
#' @importFrom recipes all_nominal_predictors all_numeric_predictors all_outcomes all_predictors
#' @importFrom finetune control_race tune_race_anova
#' @return A list of trained model objects.
#' @export
train_models <- function(train_data,
                         label,
                         task,
                         algorithms,
                         resampling_method,
                         folds,
                         repeats,
                         resamples = NULL,
                         tune_params,
                         metric,
                         summaryFunction = NULL,
                         seed = 123,
                         recipe,
                         use_default_tuning = FALSE,
                         tuning_strategy = "grid",
                         tuning_iterations = 10,
                         early_stopping = FALSE,
                         adaptive = FALSE,
                         algorithm_engines = NULL) {

  set.seed(seed)

  tuning_strategy <- match.arg(tuning_strategy, c("grid", "bayes", "none"))

  if (tuning_strategy == "bayes" && adaptive) {
    warning("'adaptive' is not supported with Bayesian tuning. Setting adaptive = FALSE.")
    adaptive <- FALSE
  }

  if (tuning_strategy == "none" && !is.null(tune_params)) {
    warning("'tune_params' are ignored when 'tuning_strategy' is 'none'")
  }

  if (tuning_strategy == "bayes") {
    if (!is.numeric(tuning_iterations) || length(tuning_iterations) != 1 ||
        tuning_iterations <= 0 || tuning_iterations != as.integer(tuning_iterations)) {
      stop("'tuning_iterations' must be a positive integer")
    }
  }

  if (early_stopping && tuning_strategy != "bayes") {
    message("Engine-level early stopping will be applied when supported")
  }

  if (task == "survival") {
    models <- list()

    response_col <- label
    label_cols <- attr(train_data[[label]], "fastml_label_cols")
    if (is.null(label_cols)) {
      if (length(label) > 1) {
        label_cols <- label
      } else {
        stop("Survival training requires original time/status column names to be stored on the response.")
      }
    }
    start_col <- if (length(label_cols) == 3) label_cols[1] else NULL
    time_col <- if (length(label_cols) == 3) label_cols[2] else label_cols[1]
    status_col <- label_cols[length(label_cols)]

    get_engine <- function(algo, default_engine) {
      if (!is.null(algorithm_engines) && !is.null(algorithm_engines[[algo]])) {
        return(algorithm_engines[[algo]])
      } else {
        return(default_engine)
      }
    }

    rec_prep_cache <- NULL
    baked_train_cache <- NULL
    get_prepped_data <- function() {
      if (is.null(rec_prep_cache)) {
        rec_prep_cache <<- recipes::prep(recipe, training = train_data, retain = TRUE)
        baked_train_cache <<- recipes::bake(rec_prep_cache, new_data = NULL)
      }
      list(recipe = rec_prep_cache, data = baked_train_cache)
    }

    create_native_spec <- function(algo_name, engine_name, fit_obj, recipe_obj, extras = list()) {
      spec <- c(list(
        algo = algo_name,
        engine = if (!is.null(engine_name)) engine_name else NA_character_,
        fit = fit_obj,
        recipe = recipe_obj,
        response = response_col,
        label_cols = label_cols,
        time_col = time_col,
        status_col = status_col,
        start_col = start_col
      ), extras)
      class(spec) <- c("fastml_native_survival", "fastml_model")
      spec
    }

    for (algo in algorithms) {
      engine <- get_engine(algo, get_default_engine(algo, task))

      if (algo == "rand_forest") {
        if (identical(engine, "aorsf") && !requireNamespace("aorsf", quietly = TRUE)) {
          if (requireNamespace("ranger", quietly = TRUE)) {
            warning("Engine 'aorsf' not installed. Falling back to 'ranger' for survival random forest.")
            engine <- "ranger"
          } else {
            warning("Neither 'aorsf' nor 'ranger' are available. Skipping survival random forest.")
            next
          }
        }
        if (!requireNamespace("censored", quietly = TRUE)) {
          warning("Package 'censored' not installed; skipping survival random forest parsnip model.")
          next
        }
        spec <- define_rand_forest_spec("survival", train_data, label,
                                       tuning = FALSE, engine = engine)$model_spec
      } else if (algo == "elastic_net") {
        warning("Survival 'elastic_net' requires censored survival model specs not available in your setup. Skipping.")
        next
      } else if (algo == "cox_ph") {
        if (!requireNamespace("survival", quietly = TRUE)) {
          stop("The 'survival' package is required for Cox PH. Please install it.")
        }
        prep_dat <- get_prepped_data()
        baked_train <- prep_dat$data
        rec_prep <- prep_dat$recipe
        fit <- survival::coxph(as.formula(paste(response_col, "~ .")),
                               data = baked_train,
                               ties = "efron")
        spec <- create_native_spec("cox_ph", engine, fit, rec_prep)
      } else if (algo == "stratified_cox") {
        if (!requireNamespace("survival", quietly = TRUE)) {
          stop("The 'survival' package is required for stratified Cox. Please install it.")
        }
        strata_cols <- names(train_data)[grepl("^strata", names(train_data))]
        if (length(strata_cols) == 0) {
          warning("No columns starting with 'strata' were found; skipping stratified Cox.")
          next
        }
        prep_dat <- get_prepped_data()
        baked_train <- prep_dat$data
        rec_prep <- prep_dat$recipe
        strata_cols_present <- character()
        for (sc in strata_cols) {
          if (!(sc %in% names(baked_train)) && sc %in% names(train_data)) {
            baked_train[[sc]] <- train_data[[sc]]
          }
          if (sc %in% names(baked_train) && !is.factor(baked_train[[sc]])) {
            baked_train[[sc]] <- as.factor(baked_train[[sc]])
          }
          if (sc %in% names(baked_train)) {
            strata_cols_present <- c(strata_cols_present, sc)
          }
        }
        if (length(strata_cols_present) == 0) {
          warning("Unable to identify usable strata columns after preprocessing; skipping stratified Cox.")
          next
        }
        predictor_cols <- setdiff(names(baked_train), c(response_col, strata_cols_present))
        rhs_terms <- c(predictor_cols, paste0("strata(", strata_cols_present, ")"))
        formula_rhs <- if (length(rhs_terms) == 0) "1" else paste(rhs_terms, collapse = " + ")
        f <- as.formula(paste(response_col, "~", formula_rhs))
        fit <- survival::coxph(f, data = baked_train, ties = "efron")
        spec <- create_native_spec("stratified_cox", engine, fit, rec_prep,
                                   extras = list(strata_cols = strata_cols_present))
      } else if (algo == "time_varying_cox") {
        if (length(label_cols) != 3) {
          warning("time_varying_cox requires label = c(start, stop, status). Skipping.")
          next
        }
        if (!requireNamespace("survival", quietly = TRUE)) {
          stop("The 'survival' package is required for time-varying Cox. Please install it.")
        }
        prep_dat <- get_prepped_data()
        baked_train <- prep_dat$data
        rec_prep <- prep_dat$recipe
        fit <- survival::coxph(as.formula(paste(response_col, "~ .")),
                               data = baked_train,
                               ties = "efron")
        spec <- create_native_spec("time_varying_cox", engine, fit, rec_prep)
      } else if (algo == "survreg") {
        if (!requireNamespace("survival", quietly = TRUE)) {
          stop("The 'survival' package is required for survreg. Please install it.")
        }
        prep_dat <- get_prepped_data()
        baked_train <- prep_dat$data
        rec_prep <- prep_dat$recipe
        fit <- survival::survreg(as.formula(paste(response_col, "~ .")),
                                 data = baked_train,
                                 dist = "weibull")
        spec <- create_native_spec("survreg", engine, fit, rec_prep,
                                   extras = list(distribution = "weibull"))
      } else if (algo == "coxnet") {
        if (!requireNamespace("glmnet", quietly = TRUE)) {
          warning("Package 'glmnet' not installed; skipping coxnet.")
          next
        }
        prep_dat <- get_prepped_data()
        baked_train <- prep_dat$data
        rec_prep <- prep_dat$recipe
        predictor_df <- baked_train
        predictor_df[[response_col]] <- NULL
        if (ncol(predictor_df) == 0) {
          warning("No predictors available for coxnet; skipping.")
          next
        }
        x_mat <- stats::model.matrix(~ . - 1, data = predictor_df)
        if (ncol(x_mat) == 0) {
          warning("No predictors available for coxnet; skipping.")
          next
        }
        y <- baked_train[[response_col]]
        cv_fit <- tryCatch(glmnet::cv.glmnet(x_mat, y, family = "cox"), error = function(e) e)
        if (inherits(cv_fit, "error")) {
          warning(sprintf("coxnet training failed: %s", cv_fit$message))
          next
        }
        lambda <- cv_fit$lambda.min
        glmnet_fit <- tryCatch(glmnet::glmnet(x_mat, y, family = "cox", lambda = lambda), error = function(e) e)
        if (inherits(glmnet_fit, "error")) {
          warning(sprintf("coxnet final fit failed: %s", glmnet_fit$message))
          next
        }
        x_terms <- attr(x_mat, "terms")
        if (!is.null(x_terms)) {
          attr(x_terms, ".Environment") <- baseenv()
        }
        x_contrasts <- attr(x_mat, "contrasts")
        spec <- create_native_spec(
          "coxnet",
          engine,
          glmnet_fit,
          rec_prep,
          extras = list(
            penalty = lambda,
            feature_names = colnames(x_mat),
            x_terms = x_terms,
            x_contrasts = x_contrasts
          )
        )
      } else if (algo == "royston_parmar") {
        if (!requireNamespace("rstpm2", quietly = TRUE)) {
          warning("Package 'rstpm2' not installed; skipping royston_parmar.")
          next
        }
        prep_dat <- get_prepped_data()
        baked_train <- prep_dat$data
        rec_prep <- prep_dat$recipe
        fit <- tryCatch(rstpm2::stpm2(as.formula(paste(response_col, "~ .")),
                                      data = baked_train,
                                      df = 3),
                        error = function(e) e)
        if (inherits(fit, "error")) {
          warning(sprintf("royston_parmar training failed: %s", fit$message))
          next
        }
        spec <- create_native_spec("royston_parmar", engine, fit, rec_prep,
                                   extras = list(spline_df = 3))
      } else if (algo == "aft") {
        warning("Survival 'aft' requires censored survival model specs not available in your setup. Skipping.")
        next
      } else {
        next
      }

      if (inherits(spec, "fastml_native_survival")) {
        models[[algo]] <- spec
      } else {
        wf <- workflows::workflow() %>%
          workflows::add_recipe(recipe) %>%
          workflows::add_model(spec)
        models[[algo]] <- parsnip::fit(wf, data = train_data)
      }
    }

    return(models)
  } else if (task == "classification") {

    if(is.null(summaryFunction)){
      metrics <- metric_set(
        accuracy,
        kap,
        sens,
        spec,
        precision,
        f_meas,
        roc_auc
      )
    }else{

      newClassMetric <- new_class_metric(summaryFunction, "maximize")

      assign(metric, newClassMetric)

      metrics <- metric_set(
        accuracy,
        kap,
        sens,
        spec,
        precision,
        f_meas,
        roc_auc,
        !!sym(metric)
      )


    }
  } else {
    metrics <- metric_set(rmse, rsq, mae)
  }

  if (!is.null(resamples)) {
    if (!inherits(resamples, "rset")) {
      stop("'resamples' must be an 'rset' object")
    }
  } else if (resampling_method == "cv") {
    if (nrow(train_data) < folds) {
      stop(
        sprintf(
          "You requested %d-fold cross-validation, but your training set only has %d rows. \nThis prevents each fold from having at least one row. \nEither reduce 'folds', increase data, or use a different resampling method (e.g. 'boot').",
          folds,
          nrow(train_data)
        )
      )
    }
    resamples <- vfold_cv(
      train_data,
      v = folds,
      repeats = 1,
      strata = if (task == "classification")
        all_of(label)
      else
        NULL
    )
  } else if (resampling_method == "boot") {
    resamples <- bootstraps(train_data,
                            times = folds,
                            strata = if (task == "classification")
                              all_of(label)
                            else
                              NULL)
  } else if (resampling_method == "repeatedcv") {

    if (nrow(train_data) < folds) {
      stop(
        sprintf(
          "You requested %d-fold cross-validation, but your training set only has %d rows. \nThis prevents each fold from having at least one row. \nEither reduce 'folds', increase data, or use a different resampling method (e.g. 'boot').",
          folds, nrow(train_data)
        )
      )
    }
    resamples <- vfold_cv(
      train_data,
      v = folds,
      repeats = repeats,
      strata = if (task == "classification")
        all_of(label)
      else
        NULL
    )
  } else if (resampling_method == "none") {
    resamples <- NULL
  } else {
    stop("Unsupported resampling method.")
  }

  if (use_default_tuning && is.null(resamples)) {
    warning("Tuning is skipped because resampling is disabled")
  }

  models <- list()

  # A helper function to choose the engine for an algorithm
  get_engine <- function(algo, default_engine) {
    if (!is.null(algorithm_engines) && !is.null(algorithm_engines[[algo]])) {
      return(algorithm_engines[[algo]])
    } else {
      return(default_engine)
    }
  }

  update_params <- function(params_model, new_params) {
    for (param_name in names(new_params)) {
      param_value <- new_params[[param_name]]
      param_row <- params_model %>% dplyr::filter(id == param_name)
      if (nrow(param_row) == 0) next

      param_obj <- param_row$object[[1]]

      # Helper function to update a parameter object
      try_update <- function(obj, value) {
        if (length(value) == 2) {
          if (inherits(obj, "integer_parameter")) {
            return(obj %>% dials::range_set(c(as.integer(value[1]), as.integer(value[2]))))
          } else {
            return(obj %>% dials::range_set(value))
          }
        } else {
          if (inherits(obj, "integer_parameter")) {
            return(obj %>% dials::value_set(as.integer(value)))
          } else {
            return(obj %>% dials::value_set(value))
          }
        }
      }

      updated_obj <- tryCatch({
        try_update(param_obj, param_value)
      }, error = function(e) {
        # If update fails, expand the allowed range to include the new value.
        current_lb <- attr(param_obj, "range")$lower
        current_ub <- attr(param_obj, "range")$upper

        # Ensure new value(s) are numeric
        if (length(param_value) == 1) {
          new_val <- if (inherits(param_obj, "integer_parameter")) as.integer(param_value) else param_value
        } else {
          new_val <- c(min(param_value), max(param_value))
        }

        # Compute new bounds that include the new value(s)
        if (length(new_val) == 1) {
          new_lb <- min(current_lb, new_val)
          new_ub <- max(current_ub, new_val)
        } else {
          new_lb <- min(current_lb, new_val[1])
          new_ub <- max(current_ub, new_val[2])
        }

        # Expand the range and update the parameter object
        param_obj %>% dials::range_set(c(new_lb, new_ub))
      })

      params_model$object[params_model$id == param_name] <- list(updated_obj)
    }
    return(params_model)
  }

  n_class <- length(levels(train_data[[label]]))

  for (algo in algorithms) {
    set.seed(seed)

    # Assume that get_engine() now may return multiple engine names.

    if(n_class > 2 && algo == "logistic_reg") {algo = "multinom_reg"}

    engines <- get_engine(algo, get_default_engine(algo, task))


    # Create a nested list to store models by algorithm and engine.
    models[[algo]] <- list()

    # Loop over each engine provided
    for (engine in engines) {

      # Get default parameters for this engine
      if (use_default_tuning) {
        defaults <- get_default_tune_params(
          algo,
          train_data,
          label,
          engine
        )
      } else {
        defaults <- get_default_params(
          algo,
          task,
          num_predictors = ncol(train_data %>% dplyr::select(-!!sym(label))),
          engine = engine
        )
      }

      # User supplied tuning parameters for this algorithm/engine
      user_params <- NULL
      if (!is.null(tune_params) &&
          !is.null(tune_params[[algo]]) &&
          !is.null(tune_params[[algo]][[engine]])) {
        user_params <- tune_params[[algo]][[engine]]
      }

      # Merge defaults with user parameters
      engine_tune_params <- if (is.null(defaults)) list() else defaults
      if (!is.null(user_params)) {
        for (nm in names(user_params)) {
          engine_tune_params[[nm]] <- user_params[[nm]]
        }
      }

      if (algo == "logistic_reg" && engine %in% c("glm", "gee", "glmer", "stan", "stan_glmer")) {
        perform_tuning <- FALSE
      } else {
        has_custom <- !is.null(user_params)
        perform_tuning <- (use_default_tuning || has_custom) && !is.null(resamples)
        if (tuning_strategy == "none") {
          perform_tuning <- FALSE
        }
      }

       # For other algorithms, use a switch that uses the current engine
        model_info <- switch(algo,
                             "rand_forest" = {
                               define_rand_forest_spec(task,
                                                       train_data,
                                                       label,
                                                       tuning = perform_tuning,
                                                       engine = engine)
                             },

                             "logistic_reg" = {
                               if(n_class == 2){
                                 define_logistic_reg_spec(
                                   task,
                                   tuning = perform_tuning,
                                   engine = engine)
                               }
                             },

                             "multinom_reg" = {
                               if(n_class > 2){
                                 define_multinomial_reg_spec(
                                   task,
                                   tuning = perform_tuning,
                                   engine = engine)
                               }
                             },


                             "C5_rules" = {
                               define_C5_rules_spec(task,
                                                    tuning = perform_tuning,
                                                    engine = engine)
                             },
                             "xgboost" = {
                               define_xgboost_spec(task,
                                                   train_data,
                                                   label,
                                                   tuning = perform_tuning,
                                                   engine = engine,
                                                   early_stopping = early_stopping)
                             },
                             "lightgbm" = {
                               define_lightgbm_spec(task,
                                                    train_data,
                                                    label,
                                                    tuning = perform_tuning,
                                                    engine = engine,
                                                    early_stopping = early_stopping)
                             },
                             "decision_tree" = {
                               define_decision_tree_spec(task,
                                                         tuning = perform_tuning,
                                                         engine = engine)
                             },
                             "svm_linear" = {
                               define_svm_linear_spec(task,
                                                      tuning = perform_tuning,
                                                      engine = engine)
                             },
                             "svm_rbf" = {
                               define_svm_rbf_spec(task,
                                                   tuning = perform_tuning,
                                                   engine = engine)
                             },
                             "nearest_neighbor" = {
                               define_nearest_neighbor_spec(task,
                                                            tuning = perform_tuning,
                                                            engine = engine)
                             },
                             "naive_Bayes" = {
                               define_naive_Bayes_spec(task,
                                                       tuning = perform_tuning,
                                                       engine = engine)
                             },
                             "mlp" = {
                               define_mlp_spec(task,
                                               tuning = perform_tuning,
                                               engine = engine)
                             },
                             "discrim_linear" = {
                               define_discrim_linear_spec(task,
                                                          engine = engine)
                             },
                             "discrim_quad" = {
                               define_discrim_quad_spec(task,
                                                        engine = engine)
                             },
                             "bag_tree" = {
                               define_bag_tree_spec(task,
                                                    tuning = perform_tuning,
                                                    engine = engine)
                             },
                             "elastic_net" = {
                               define_elastic_net_spec(task,
                                                       tuning = perform_tuning,
                                                       engine = engine)
                             },
                             "bayes_glm" = {
                               define_bayes_glm_spec(task,
                                                     engine = engine)
                             },
                             "pls" = {
                               define_pls_spec(task,
                                               tuning = perform_tuning,
                                               engine = engine)
                             },
                             "linear_reg" = {
                               define_linear_reg_spec(task,
                                                      engine = engine)
                             },
                             "ridge_reg" = {
                               define_ridge_reg_spec(task,
                                                            tuning = perform_tuning,
                                                            engine = engine)
                             },
                             "lasso_reg" = {
                               define_lasso_reg_spec(task,
                                                            tuning = perform_tuning,
                                                            engine = engine)
                             },
                             {
                               warning(paste("Algorithm", algo, "is not supported or failed to train."))
                               next
                             }
        )

        # Assume the model specification is stored in model_info$model_spec
        model_spec <- model_info$model_spec

      if(!is.null(model_spec)){

      # Set up tuning parameters and grid (if needed)
      if (perform_tuning) {

        if(inherits(model_spec, "model_spec")){
          tune_params_model <- extract_parameter_set_dials(model_spec)
        }else{
        tune_params_model <- extract_parameter_set_dials(model_spec[[1]])
        }
        tune_params_model <- finalize(
          tune_params_model,
          x = train_data %>% dplyr::select(-dplyr::all_of(label))
        )

        if (!is.null(engine_tune_params)) {
          tune_params_model <- update_params(tune_params_model, engine_tune_params)
        }

        if (nrow(tune_params_model) > 0) {
          if (tuning_strategy == "grid" && !adaptive) {
            tune_grid <- grid_regular(tune_params_model, levels = 3)
          } else {
            tune_grid <- NULL
          }
        } else {
          tune_grid <- NULL
        }
      } else {
        tune_grid <- NULL
      }

      # Create the workflow
      workflow_spec <- workflow() %>%
        add_model(if(inherits(model_spec,"model_spec")) model_spec else model_spec[[1]]) %>%
        add_recipe(recipe)

      # Fit the model (with tuning if requested)
      tryCatch({
        if (perform_tuning && !all(vapply(engine_tune_params, is.null, logical(1)))) {
          # Set up control objects for tuning

          if (algo == "rand_forest" && engine == "h2o") {

            roc_auc_h2o <- function(data, truth, ...) {
              # Rename probability columns from ".pred_p0"/".pred_p1" to ".pred_0"/".pred_1"
              data <- data %>%
                rename_with(~ sub("^\\.pred_p", ".pred_", .x), starts_with(".pred_p"))

              # Call the built-in roc_auc() with the renamed columns
              yardstick::roc_auc(data, truth = {{truth}}, ...)
            }

            # Assign the same class as roc_auc()
            class(roc_auc_h2o) <- class(roc_auc)
            attr(roc_auc_h2o, "direction") <- "maximize"

            my_metrics <- metric_set(accuracy, kap, sens, spec, precision, f_meas, roc_auc_h2o)

            allow_par = FALSE
          }

          else if(engine == "LiblineaR"){

            my_metrics <- metric_set(accuracy, kap, sens, spec, precision, f_meas)
            allow_par = TRUE

          }else{
            allow_par = TRUE
            my_metrics = NULL
          }

          ctrl_grid <- control_grid(save_pred = TRUE, allow_par = allow_par)
          ctrl_bayes <- control_bayes(save_pred = TRUE)
          ctrl_race <- control_race(save_pred = TRUE)

          if (early_stopping && tuning_strategy == "bayes") {
            ctrl_bayes <- control_bayes(save_pred = TRUE, no_improve = 5)
          }

          if (is.null(resamples)) {
            stop("Tuning cannot be performed without resamples.")
          }

          # Select tuning function based on strategy
          if (tuning_strategy == "bayes") {
            model_tuned <- tune_bayes(
              workflow_spec,
              resamples = resamples,
              param_info = tune_params_model,
              iter = tuning_iterations,
              metrics = if(!is.null(my_metrics)) my_metrics else metrics,
              control = ctrl_bayes
            )
          } else if (adaptive) {
            model_tuned <- tune_race_anova(
              workflow_spec,
              resamples = resamples,
              param_info = tune_params_model,
              grid = if (is.null(tune_grid)) 20 else tune_grid,
              metrics = if(!is.null(my_metrics)) my_metrics else metrics,
              control = ctrl_race
            )
          } else if (tuning_strategy == "grid") {
            if (is.null(tune_grid)) {
              tune_grid <- grid_regular(tune_params_model, levels = 3)
            }
            model_tuned <- tune_grid(
              workflow_spec,
              resamples = resamples,
              grid = tune_grid,
              metrics = if(!is.null(my_metrics)) my_metrics else metrics,
              control = ctrl_grid
            )
          } else {
            model_tuned <- tune_grid(
              workflow_spec,
              resamples = resamples,
              grid = if (is.null(tune_grid)) 5 else tune_grid,
              metrics = if(!is.null(my_metrics)) my_metrics else metrics,
              control = ctrl_grid
            )
          }

          best_params <- select_best(model_tuned, metric = metric)
          final_workflow <- finalize_workflow(workflow_spec, best_params)
          model <- fit(final_workflow, data = train_data)
        } else {
          # If no tuning is required, simply fit the workflow.
          model <- fit(workflow_spec, data = train_data)
        }
        # Save the fitted model in the nested list under the current engine
        models[[algo]][[engine]] <- model
      }, error = function(e) {
        warning(paste("Training failed for algorithm:", algo, "with engine:", engine,
                      "\nError message:", e$message))
      })

      }else{

        models[[algo]] = NULL
      }

    }  # end of loop over engines

  }

  if (length(models) == 0) {
    stop("No models were successfully trained. Please check your data and parameters.")
  }

  return(models)
}

# Declare global variables
utils::globalVariables(c(
  "id", "object", "estimate", ".metric", ".estimate", ".pred", ".pred_class",
  "rmse", "rsq", "min_n", "num_comp"
))




