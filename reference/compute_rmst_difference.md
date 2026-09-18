# Compute Difference in Restricted Mean Survival Time (RMST)

Calculates the difference in RMST between "low" and "high" risk groups
up to a time horizon \\\tau\\. Groups are defined by median-splitting
the `risk_vec`.

## Usage

``` r
compute_rmst_difference(
  time_vec,
  status_vec,
  risk_vec,
  tau,
  surv_mat = NULL,
  eval_times_full = NULL,
  model_type = "other"
)
```

## Arguments

- time_vec:

  Numeric vector of test times.

- status_vec:

  Numeric vector of test statuses.

- risk_vec:

  Numeric vector of predicted risk scores for test data.

- tau:

  The time horizon \\\tau\\ for integration.

- surv_mat:

  Optional. A matrix of individual survival predictions (rows=subjects,
  cols=times) used for model-based RMST calculation.

- eval_times_full:

  Optional. A numeric vector of time points corresponding to the columns
  of `surv_mat`.

- model_type:

  Optional string (e.log., "rstpm2", "flexsurv") indicating if a
  model-based RMST calculation should be attempted.

## Value

The RMST difference (RMST_low - RMST_high), or `NA_real_`.
