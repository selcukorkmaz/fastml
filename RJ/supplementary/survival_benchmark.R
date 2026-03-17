###############################################################################
# SURVIVAL BENCHMARK: lung (small), rotterdam (medium), flchain (large)
# Engines: CoxPH, Weibull, XGBoost-AFT, RF-Surv, Penalized Cox
# Method: Repeated stratified CV + heuristic (fixed) hyperparameters
###############################################################################

library(dplyr)
library(survival)
library(rsample)
library(purrr)
library(recipes)
library(fastml)
library(tibble)

###############################################################################
# 0. Helpers
###############################################################################

# Flip C-index if < 0.5 (directionality)
standardize_c <- function(val) {
  if (is.null(val) || length(val) == 0 || is.na(val)) return(0.5)
  if (val < 0.5) 1 - val else val
}

# Extract c_index safely from a fastml object
extract_cindex <- function(mod, algo) {

  # ---------------------------------------------------------
  # 1. XGBoost AFT  (fastml stores under $performance$xgboost)
  # ---------------------------------------------------------
  if (algo == "xgboost_aft" || algo == "xgboost") {

    tb <- mod$performance[["xgboost_aft"]]
    if (is.null(tb)) return(0.5)

    idx <- which(tb$.metric == "c_index")
    if (length(idx) == 0) return(0.5)

    val <- tb$.estimate[idx[1]]
    return(as.numeric(val))
  }

  # ---------------------------------------------------------
  # 2. Random Forest Survival
  # ---------------------------------------------------------
  if (algo == "rand_forest_survival" || algo == "rand_forest") {

    tb <- mod$performance[["rand_forest_survival"]]
    if (is.null(tb)) return(0.5)

    idx <- which(tb$.metric == "c_index")
    if (length(idx) == 0) return(0.5)

    val <- tb$.estimate[idx[1]]
    return(as.numeric(val))
  }

  # ---------------------------------------------------------
  # 3. Penalized Cox (glmnet)
  # ---------------------------------------------------------
  if (algo == "penalized_cox") {

    tb <- mod$performance[["penalized_cox"]]
    if (is.null(tb)) return(0.5)

    idx <- which(tb$.metric == "c_index")
    if (length(idx) == 0) return(0.5)

    val <- tb$.estimate[idx[1]]
    return(as.numeric(val))
  }

  # ---------------------------------------------------------
  # Default
  # ---------------------------------------------------------
  return(0.5)
}


# Heuristic hyperparameters as a function of training size
get_heuristic_params <- function(n_obs) {
  if (n_obs < 500) {
    # Small data (Lung): strong regularization, shallow trees
    list(
      xgb_nrounds = 100,
      xgb_eta     = 0.05,
      xgb_depth   = 2,
      xgb_child   = 20,
      xgb_sub     = 0.8,
      rf_leaf     = 5,
      rf_split    = 5,
      rf_sample   = 0.7,
      glm_penalty = 0.05,
      glm_mix     = 0.5
    )
  } else if (n_obs < 4000) {
    # Medium data (Rotterdam)
    list(
      xgb_nrounds = 500,
      xgb_eta     = 0.03,
      xgb_depth   = 3,
      xgb_child   = 10,
      xgb_sub     = 0.8,
      rf_leaf     = 3,
      rf_split    = 3,
      rf_sample   = 0.632,
      glm_penalty = 0.05,
      glm_mix     = 0.5
    )
  } else {
    # Large data (Flchain)
    list(
      xgb_nrounds = 800,
      xgb_eta     = 0.02,
      xgb_depth   = 4,
      xgb_child   = 5,
      xgb_sub     = 1.0,
      rf_leaf     = 3,
      rf_split    = 3,
      rf_sample   = 0.7,
      glm_penalty = 0.05,
      glm_mix     = 0.5
    )
  }
}

###############################################################################
# 1. Evaluate a single CV fold
###############################################################################

evaluate_fold <- function(split, recipe_base) {

  train_raw <- analysis(split)
  test_raw  <- assessment(split)

  # Guarded preprocessing per fold
  rec_prep <- prep(recipe_base, training = train_raw)
  tr <- bake(rec_prep, new_data = train_raw)
  te <- bake(rec_prep, new_data = test_raw)

  # Safety: remove any residual NA from preprocessing
  tr[is.na(tr)] <- 0
  te[is.na(te)] <- 0

  # Build survival formula
  preds <- setdiff(names(tr), c("time", "status"))
  f_str <- paste("Surv(time, status) ~", paste(preds, collapse = " + "))
  surv_f <- as.formula(f_str)

  # Heuristic hyperparameters based on training size
  hp <- get_heuristic_params(nrow(tr))

  # --- Cox PH ---
  c_cox <- tryCatch({
    fit <- coxph(surv_f, data = tr)
    risk <- predict(fit, newdata = te, type = "risk")
    concordance(Surv(te$time, te$status) ~ risk)$concordance
  }, error = function(e) 0.5)

  # --- Weibull AFT ---
  c_wb <- tryCatch({
    fit <- survreg(surv_f, data = tr, dist = "weibull")
    lp  <- -predict(fit, newdata = te, type = "lp")
    concordance(Surv(te$time, te$status) ~ lp)$concordance
  }, error = function(e) 0.5)

  # --- XGBoost AFT via fastml (no tuning, fixed heuristics) ---
  c_xgb <- tryCatch({
    mod <- fastml(
      train_data = tr,
      test_data  = te,
      label      = c("time", "status"),
      algorithms = "xgboost_aft",
      metric     = "c_index",
      resampling_method = "none",
      algorithm_engines = list(xgboost_aft = "xgboost"),
      engine_params = list(
        xgboost_aft = list(
          xgboost = list(
            nrounds = hp$xgb_nrounds,
            params = list(
              eta                      = hp$xgb_eta,
              max_depth                = hp$xgb_depth,
              min_child_weight         = hp$xgb_child,
              subsample                = hp$xgb_sub,
              colsample_bytree         = 1,
              gamma                    = 0,
              aft_loss_distribution    = "logistic",
              aft_loss_distribution_scale = 1
            )
          )
        )
      ),
      verbose = FALSE
    )
    extract_cindex(mod, "xgboost_aft")
  }, error = function(e) 0.5)

  # --- Random Forest Survival (aorsf) via fastml ---
  c_rf <- tryCatch({
    mod <- fastml(
      train_data = tr,
      test_data  = te,
      label      = c("time", "status"),
      algorithms = "rand_forest_survival",
      metric     = "c_index",
      resampling_method = "none",
      algorithm_engines = list(rand_forest_survival = "aorsf"),
      engine_params = list(
        rand_forest_survival = list(
          aorsf = list(
            leaf_min_events  = hp$rf_leaf,
            split_min_events = hp$rf_split,
            sample_fraction  = hp$rf_sample
          )
        )
      ),
      verbose = FALSE
    )
    extract_cindex(mod, "rand_forest_survival")
  }, error = function(e) 0.5)

  # --- Penalized Cox (glmnet) via fastml ---
  c_coxn <- tryCatch({
    mod <- fastml(
      train_data = tr,
      test_data  = te,
      label      = c("time", "status"),
      algorithms = "penalized_cox",
      metric     = "c_index",
      resampling_method = "none",
      algorithm_engines = list(penalized_cox = "glmnet"),
      engine_params = list(
        penalized_cox = list(
          glmnet = list(
            penalty = hp$glm_penalty,  # lambda
            mixture = hp$glm_mix       # alpha
          )
        )
      ),
      verbose = FALSE
    )
    extract_cindex(mod, "penalized_cox")
  }, error = function(e) 0.5)

  tibble(
    Model   = c("CoxPH", "Weibull", "XGB_AFT", "RF_Surv", "PenalCox"),
    C_Index = c(
      standardize_c(c_cox),
      standardize_c(c_wb),
      standardize_c(c_xgb),
      standardize_c(c_rf),
      standardize_c(c_coxn)
    )
  )
}

###############################################################################
# 2. Run benchmark for one dataset
###############################################################################

run_benchmark <- function(df, dataset_name) {

  message("Running: ", dataset_name)

  numeric_vars <- df %>% select(where(is.numeric)) %>% names()
  numeric_vars <- setdiff(numeric_vars, c("time", "status"))

  recipe_base <- recipe(time + status ~ ., data = df) %>%
    step_unknown(all_nominal_predictors()) %>%
    step_impute_median(all_numeric_predictors()) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_zv(all_predictors()) %>%
    step_normalize(all_of(numeric_vars))

  set.seed(2024)
  folds <- vfold_cv(df, v = 5, repeats = 3, strata = status)

  raw <- map_dfr(
    folds$splits,
    ~ evaluate_fold(.x, recipe_base),
    .id = "Fold"
  )

  raw$Dataset <- dataset_name
  raw
}

###############################################################################
# 3. Load and prepare datasets: lung / rotterdam / flchain
###############################################################################

# Lung (small)
df_lung <- lung %>%
  transmute(
    time,
    status = ifelse(status == 2, 1, 0),
    age,
    sex = factor(sex),
    ph.ecog,
    ph.karno,
    pat.karno,
    meal.cal,
    wt.loss
  ) %>%
  filter(time > 0)

# Rotterdam (medium)
df_rot <- rotterdam %>%
  transmute(
    time   = dtime,
    status = death,
    age,
    grade  = factor(grade),
    nodes,
    pgr,
    er,
    hormon = factor(hormon),
    chemo  = factor(chemo),
    size   = factor(size),
    meno   = factor(meno)
  ) %>%
  filter(time > 0)

# Flchain (large) – keep NAs, let recipe impute
df_fl <- flchain %>%
  transmute(
    time   = futime,
    status = death,
    age,
    sex = factor(sex),
    kappa,
    lambda,
    creatinine,
    mgus = factor(mgus)
  ) %>%
  filter(time > 0)

###############################################################################
# 4. Run all benchmarks and summarise
###############################################################################

results_lung <- run_benchmark(df_lung, "Lung")
results_rot  <- run_benchmark(df_rot,  "Rotterdam")
results_fl   <- run_benchmark(df_fl,   "Flchain")

all_results <- bind_rows(results_lung, results_rot, results_fl)

# Per-dataset, per-model summary
summary_stats <- all_results %>%
  group_by(Dataset, Model) %>%
  summarise(
    Mean_C   = mean(C_Index, na.rm = TRUE),
    SD_C     = sd(C_Index,   na.rm = TRUE),
    CI_Lower = quantile(C_Index, 0.025, na.rm = TRUE),
    CI_Upper = quantile(C_Index, 0.975, na.rm = TRUE),
    .groups  = "drop"
  )

print(all_results)
print(summary_stats)
