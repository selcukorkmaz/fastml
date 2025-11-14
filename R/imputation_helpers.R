# Internal helpers for advanced imputation handling

fastml_apply_advanced_imputation <- function(train_data,
                                             test_data,
                                             outcome_cols,
                                             impute_method,
                                             impute_custom_function,
                                             warn = TRUE,
                                             audit_env = NULL) {
  advanced_methods <- c("mice", "missForest", "custom")
  if (is.null(impute_method) || !(impute_method %in% advanced_methods)) {
    return(list(train_data = train_data, test_data = test_data))
  }

  if (impute_method == "custom") {
    hook <- fastml_validate_custom_hook(impute_custom_function, "Custom imputer")

    fit_result <- fastml_run_user_hook(hook$fit, train_data, "custom imputer fit", audit_env)
    fit_processed <- fastml_process_custom_fit_result(fit_result, "Custom imputer")
    state <- fit_processed$state

    if (!is.null(fit_processed$transformed)) {
      train_data <- fastml_validate_transformed_data(fit_processed$transformed, "Custom imputer fit")
    } else {
      train_data <- fastml_validate_transformed_data(
        fastml_run_user_hook(
          hook$transform,
          train_data,
          "custom imputer transform (train)",
          audit_env,
          extra_args = list(state = state)
        ),
        "Custom imputer transform (train)"
      )
    }

    if (!is.null(test_data) && nrow(test_data) > 0) {
      test_data <- fastml_validate_transformed_data(
        fastml_run_user_hook(
          hook$transform,
          test_data,
          "custom imputer transform (test)",
          audit_env,
          extra_args = list(state = state)
        ),
        "Custom imputer transform (test)"
      )
    }

    fastml_register_audit(
      audit_env,
      "Custom imputer executed in guarded two-phase mode.",
      severity = "info",
      context = "custom_imputer"
    )

    if (warn) {
      warning("Missing values have been imputed using the 'custom' method.")
    }
    return(list(train_data = train_data, test_data = test_data))
  }

  predictor_cols_train <- setdiff(colnames(train_data), outcome_cols)

  if (impute_method == "mice") {
    if (!requireNamespace("mice", quietly = TRUE)) {
      stop("impute_method='mice' requires the 'mice' package to be installed.")
    }

    if (length(predictor_cols_train) > 0 && nrow(train_data) > 0) {
      train_predictors <- train_data[, predictor_cols_train, drop = FALSE]
      matrix_cols_train <- vapply(train_predictors, is.matrix, logical(1))
      if (any(matrix_cols_train)) {
        train_matrix <- train_predictors[, matrix_cols_train, drop = FALSE]
        train_non_matrix <- train_predictors[, !matrix_cols_train, drop = FALSE]
        if (ncol(train_non_matrix) > 0) {
          train_data_mice <- mice::mice(train_non_matrix)
          train_non_matrix <- mice::complete(train_data_mice)
        }
        train_predictors <- cbind(train_non_matrix, train_matrix)
        train_predictors <- train_predictors[, predictor_cols_train, drop = FALSE]
      } else {
        train_data_mice <- mice::mice(train_predictors)
        train_predictors <- mice::complete(train_data_mice)
      }
      train_data[, predictor_cols_train] <- train_predictors
    }

    if (!is.null(test_data) && nrow(test_data) > 0) {
      test_predictor_cols <- setdiff(colnames(test_data), outcome_cols)
      if (length(test_predictor_cols) > 0) {
        test_predictors <- test_data[, test_predictor_cols, drop = FALSE]
        matrix_cols_test <- vapply(test_predictors, is.matrix, logical(1))
        if (any(matrix_cols_test)) {
          test_matrix <- test_predictors[, matrix_cols_test, drop = FALSE]
          test_non_matrix <- test_predictors[, !matrix_cols_test, drop = FALSE]
          if (ncol(test_non_matrix) > 0) {
            if (!exists("train_data_mice")) {
              train_data_mice <- mice::mice(train_data[, predictor_cols_train, drop = FALSE])
            }
            test_data_mice <- mice::mice(
              test_non_matrix,
              maxit = train_data_mice$iteration,
              meth = train_data_mice$meth,
              predictorMatrix = train_data_mice$predictorMatrix
            )
            test_non_matrix <- mice::complete(test_data_mice, action = 1)
          }
          test_predictors <- cbind(test_non_matrix, test_matrix)
          test_predictors <- test_predictors[, test_predictor_cols, drop = FALSE]
        } else {
          if (!exists("train_data_mice")) {
            train_data_mice <- mice::mice(train_data[, predictor_cols_train, drop = FALSE])
          }
          test_data_mice <- mice::mice(
            test_predictors,
            maxit = train_data_mice$iteration,
            meth = train_data_mice$meth,
            predictorMatrix = train_data_mice$predictorMatrix
          )
          test_predictors <- mice::complete(test_data_mice, action = 1)
        }
        test_data[, test_predictor_cols] <- test_predictors
      }
    }

    if (warn) {
      warning("Missing values have been imputed using the 'mice' method.")
    }
    return(list(train_data = train_data, test_data = test_data))
  }

  if (impute_method == "missForest") {
    if (!requireNamespace("missForest", quietly = TRUE)) {
      stop("impute_method='missForest' requires the 'missForest' package to be installed.")
    }

    if (length(predictor_cols_train) > 0 && nrow(train_data) > 0) {
      train_data_imp <- missForest::missForest(train_data[, predictor_cols_train, drop = FALSE], verbose = FALSE)
      imputed_train <- train_data_imp$ximp
      train_data[, predictor_cols_train] <- imputed_train[, predictor_cols_train, drop = FALSE]
    }

    if (!is.null(test_data) && nrow(test_data) > 0) {
      test_predictor_cols <- setdiff(colnames(test_data), outcome_cols)
      if (length(test_predictor_cols) > 0) {
        test_data_imp <- missForest::missForest(test_data[, test_predictor_cols, drop = FALSE], verbose = FALSE)
        imputed_test <- test_data_imp$ximp
        test_data[, test_predictor_cols] <- imputed_test[, test_predictor_cols, drop = FALSE]
      }
    }

    if (warn) {
      warning("Missing values have been imputed using the 'missForest' method.")
    }
    return(list(train_data = train_data, test_data = test_data))
  }

  list(train_data = train_data, test_data = test_data)
}

fastml_impute_resamples <- function(resamples,
                                    impute_method,
                                    impute_custom_function,
                                    outcome_cols,
                                    audit_env = NULL) {
  if (is.null(resamples) || length(resamples$splits) == 0) {
    return(resamples)
  }

  resamples$splits <- lapply(resamples$splits, function(split) {
    data <- split$data
    analysis_idx <- split$in_id
    assessment_idx <-
      if (!is.null(split$out_id) && !all(is.na(split$out_id))) {
        split$out_id
      } else {
        rsample::complement(split)
      }

    analysis_data <- data[analysis_idx, , drop = FALSE]
    assessment_data <- data[assessment_idx, , drop = FALSE]

    imputed <- fastml_apply_advanced_imputation(
      train_data = analysis_data,
      test_data = assessment_data,
      outcome_cols = outcome_cols,
      impute_method = impute_method,
      impute_custom_function = impute_custom_function,
      warn = FALSE,
      audit_env = audit_env
    )

    data[analysis_idx, ] <- imputed$train_data
    if (length(assessment_idx) > 0) {
      data[assessment_idx, ] <- imputed$test_data
    }

    split$data <- data
    split
  })

  if (!is.null(resamples$inner_resamples)) {
    resamples$inner_resamples <- lapply(resamples$inner_resamples, function(inner) {
      if (inherits(inner, "rset")) {
        fastml_impute_resamples(
          resamples = inner,
          impute_method = impute_method,
          impute_custom_function = impute_custom_function,
          outcome_cols = outcome_cols,
          audit_env = audit_env
        )
      } else {
        inner
      }
    })
  }

  resamples
}
