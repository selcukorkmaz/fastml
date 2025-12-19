library(testthat)

# Grouped CV tests

test_that("grouped CV keeps groups intact", {
  skip_if_not_installed("rsample")

  set.seed(123)
  grouped_data <- tibble::tibble(
    id = 1:12,
    group = rep(letters[1:4], each = 3),
    strata = rep(c("A", "B"), each = 6),
    value = rnorm(12)
  )

  resamples <- fastml:::make_grouped_cv(
    grouped_data,
    group_cols = "group",
    v = 4,
    strata = NULL,
    repeats = 1
  )

  purrr::walk(resamples$splits, function(split) {
    analysis_groups <- unique(rsample::analysis(split)$group)
    assessment_groups <- unique(rsample::assessment(split)$group)
    expect_length(intersect(analysis_groups, assessment_groups), 0)
  })
})

test_that("grouped CV errors when groups fewer than folds", {
  skip_if_not_installed("rsample")

  small_groups <- tibble::tibble(
    group = rep(letters[1:3], each = 2),
    value = runif(6)
  )

  expect_error(
    fastml:::make_grouped_cv(
      small_groups,
      group_cols = "group",
      v = 4,
      strata = NULL,
      repeats = 1
    ),
    "cannot exceed the number of groups"
  )
})

test_that("grouped CV stratification distributes strata across folds", {
  skip_if_not_installed("rsample")

  set.seed(456)
  strat_data <- tibble::tibble(
    id = 1:12,
    group = rep(paste0("g", 1:6), each = 2),
    strata = rep(c("A", "B"), each = 6),
    value = rnorm(12)
  )

  resamples <- fastml:::make_grouped_cv(
    strat_data,
    group_cols = "group",
    v = 3,
    strata = "strata",
    repeats = 1
  )

  purrr::walk(resamples$splits, function(split) {
    assessment_strata <- unique(rsample::assessment(split)$strata)
    expect_setequal(assessment_strata, unique(strat_data$strata))
  })
})

# Blocked CV tests

test_that("blocked CV preserves order and prevents leakage", {
  skip_if_not_installed("rsample")

  blocked_data <- tibble::tibble(
    time = 1:12,
    value = rnorm(12)
  )

  blocked_info <- fastml:::make_blocked_cv(
    blocked_data,
    block_col = "time",
    block_size = 3
  )

  expect_equal(blocked_info$data$time, blocked_data$time)
  expect_true(all(diff(blocked_info$block_ids) >= 0))

  blocked_plan <- blocked_info$data
  blocked_plan$.fastml_block <- blocked_info$block_ids

  resamples <- rsample::group_vfold_cv(
    blocked_plan,
    group = .fastml_block,
    v = blocked_info$n_blocks
  )

  purrr::walk(resamples$splits, function(split) {
    analysis <- rsample::analysis(split)
    assessment <- rsample::assessment(split)
    expect_true(all(diff(analysis$time) >= 0))
    expect_true(all(diff(assessment$time) >= 0))
    expect_true(all(!assessment$time %in% analysis$time))
  })
})

# Rolling origin tests

test_that("rolling origin uses expanding analysis and fixed assessment windows", {
  skip_if_not_installed("rsample")

  rolling_data <- tibble::tibble(
    time = 1:10,
    value = rnorm(10)
  )

  resamples <- fastml:::make_rolling_origin_cv(
    rolling_data,
    block_col = "time",
    initial_window = 4,
    assess_window = 2,
    skip = 0
  )

  analysis_sizes <- purrr::map_int(resamples$splits, ~ nrow(rsample::analysis(.x)))
  assessment_sizes <- purrr::map_int(resamples$splits, ~ nrow(rsample::assessment(.x)))

  expect_equal(analysis_sizes[1], 4)
  expect_true(all(diff(analysis_sizes) >= 0))
  expect_true(all(assessment_sizes == 2))
})

# Nested CV tests

test_that("nested CV inner folds remain within outer training data", {
  skip_if_not_installed("rsample")

  set.seed(789)
  nested_data <- tibble::tibble(
    id = 1:18,
    x = runif(18),
    y = rnorm(18)
  )

  nested_resamples <- fastml:::make_nested_cv(
    nested_data,
    outer_folds = 3,
    inner_folds = 2,
    group_cols = NULL,
    strata = NULL
  )

  purrr::walk2(
    nested_resamples$splits,
    nested_resamples$inner_resamples,
    function(outer_split, inner_set) {
      outer_analysis_ids <- rsample::analysis(outer_split)$id
      outer_assessment_ids <- rsample::assessment(outer_split)$id

      purrr::walk(inner_set$splits, function(inner_split) {
        inner_analysis_ids <- rsample::analysis(inner_split)$id
        inner_assessment_ids <- rsample::assessment(inner_split)$id

        expect_true(all(inner_analysis_ids %in% outer_analysis_ids))
        expect_true(all(inner_assessment_ids %in% outer_analysis_ids))
        expect_length(intersect(inner_assessment_ids, outer_assessment_ids), 0)
      })
    }
  )
})

# test_that("nested CV tuning uses inner folds and outer metrics match manual evaluation", {
#   skip_if_not_installed("rsample")
#   skip_if_not_installed("workflows")
#   skip_if_not_installed("recipes")
#   skip_if_not_installed("parsnip")
#   skip_if_not_installed("yardstick")
#   skip_if_not_installed("tune")
#   skip_if_not_installed("dials")
#   skip_if_not_installed("rpart")
#
#   set.seed(101)
#   tuning_data <- tibble::tibble(
#     id = 1:24,
#     x = runif(24, -1, 1),
#     y = 3 * x + rnorm(24, sd = 0.1)
#   )
#
#   nested_resamples <- fastml:::make_nested_cv(
#     tuning_data,
#     outer_folds = 3,
#     inner_folds = 3,
#     group_cols = NULL,
#     strata = NULL
#   )
#
#   recipe_spec <- recipes::recipe(y ~ x, data = tuning_data)
#   model_spec <- parsnip::decision_tree(
#     cost_complexity = tune::tune(),
#     tree_depth = tune::tune()
#   ) %>%
#     parsnip::set_mode("regression") %>%
#     parsnip::set_engine("rpart")
#
#   workflow_spec <- workflows::workflow() %>%
#     workflows::add_recipe(recipe_spec) %>%
#     workflows::add_model(model_spec)
#
#   metrics <- yardstick::metric_set(yardstick::rmse)
#
#   nested_fit <- fastml:::fastml_run_nested_cv(
#     workflow_spec = workflow_spec,
#     nested_resamples = nested_resamples,
#     train_data = tuning_data,
#     label = "y",
#     task = "regression",
#     metric = "rmse",
#     metrics = metrics,
#     engine = "rpart",
#     engine_args = list(),
#     tune_params_template = dials::parameters(
#       dials::cost_complexity(),
#       dials::tree_depth()
#     ),
#     engine_tune_params = list(),
#     tuning_strategy = "grid",
#     tuning_iterations = 5,
#     adaptive = FALSE,
#     early_stopping = FALSE,
#     ctrl_grid = tune::control_grid(save_pred = TRUE),
#     ctrl_bayes = tune::control_bayes(save_pred = TRUE),
#     ctrl_race = NULL,
#     my_metrics = NULL,
#     do_tuning = TRUE,
#     event_class = NULL,
#     start_col = NULL,
#     time_col = NULL,
#     status_col = NULL,
#     eval_times = NULL,
#     at_risk_threshold = 0.1,
#     seed = 202,
#     update_params_fn = function(params, new) params
#   )
#
#   inner_results <- nested_fit$details$inner_results
#
#   purrr::walk2(
#     nested_resamples$splits,
#     seq_along(inner_results),
#     function(outer_split, idx) {
#       outer_analysis_ids <- rsample::analysis(outer_split)$id
#       outer_assessment_ids <- rsample::assessment(outer_split)$id
#
#       tune_splits <- inner_results[[idx]]$splits
#       purrr::walk(tune_splits, function(inner_split) {
#         inner_analysis_ids <- rsample::analysis(inner_split)$id
#         inner_assessment_ids <- rsample::assessment(inner_split)$id
#
#         expect_true(all(inner_analysis_ids %in% outer_analysis_ids))
#         expect_true(all(inner_assessment_ids %in% outer_analysis_ids))
#         expect_length(intersect(inner_assessment_ids, outer_assessment_ids), 0)
#       })
#     }
#   )
#
#   outer_performance <- nested_fit$details$outer_performance
#   expect_equal(sort(unique(outer_performance$outer_id)), sort(nested_resamples$id))
#
#   purrr::walk2(
#     nested_resamples$splits,
#     seq_along(inner_results),
#     function(outer_split, idx) {
#       best_params <- tune::select_best(inner_results[[idx]], metric = "rmse")
#       finalized <- tune::finalize_workflow(workflow_spec, best_params)
#       outer_train <- rsample::analysis(outer_split)
#       outer_assess <- rsample::assessment(outer_split)
#       fitted <- workflows::fit(finalized, data = outer_train)
#       preds <- predict(fitted, new_data = outer_assess)$.pred
#       expected_rmse <- yardstick::rmse_vec(truth = outer_assess$y, estimate = preds)
#
#       fold_id <- nested_resamples$id[idx]
#       recorded <- outer_performance %>%
#         dplyr::filter(outer_id == fold_id, .metric == "rmse")
#
#       expect_equal(recorded$.estimate, expected_rmse)
#     }
#   )
# })
