# Compute Uno's C-index (Time-Dependent AUC)

Calculates Uno's C-index (a time-dependent AUC measure) for survival
data, weighted by the inverse probability of censoring (IPCW).

## Usage

``` r
compute_uno_c_index(
  train_time,
  train_status,
  test_time,
  test_status,
  risk_vec,
  tau,
  censor_eval_fn
)
```

## Arguments

- train_time:

  Numeric vector of training times (used for censor model).

- train_status:

  Numeric vector of training statuses (used for censor model).

- test_time:

  Numeric vector of test times.

- test_status:

  Numeric vector of test statuses.

- risk_vec:

  Numeric vector of predicted risk scores for test data.

- tau:

  The time horizon \\\tau\\ for evaluation. If `NA` or `<= 0`, the
  maximum finite test time is used.

- censor_eval_fn:

  A function (from `create_censor_eval`) that evaluates the censoring
  survival function \\G(t)\\.

## Value

The computed Uno's C-index, or `NA_real_` on failure.
