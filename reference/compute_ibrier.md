# Compute Integrated Brier Score and Curve

Calculates the Brier score at specified evaluation times and the
Integrated Brier Score (IBS) up to \\\tau\\, using IPCW to handle
censoring.

## Usage

``` r
compute_ibrier(
  eval_times,
  surv_mat,
  time_vec,
  status_vec,
  tau,
  censor_eval_fn,
  normalize_by = c("non_missing", "n"),
  include_zero = TRUE
)
```

## Arguments

- eval_times:

  Numeric vector of evaluation time points.

- surv_mat:

  Matrix of predicted survival probabilities (rows=subjects,
  cols=eval_times).

- time_vec:

  Numeric vector of test times.

- status_vec:

  Numeric vector of test statuses.

- tau:

  The time horizon \\\tau\\ for integration.

- censor_eval_fn:

  A function (from `create_censor_eval`) that evaluates the censoring
  survival function \\G(t)\\.

- normalize_by:

  Character string specifying how to normalize Brier scores. Use
  `"non_missing"` (default) to divide by the number of non-missing
  contributions or `"n"` to divide by the total sample size.

- include_zero:

  Logical; if `TRUE`, integrates from time 0 with a Brier score of 0
  when the evaluation grid starts after 0.

## Value

A list with `ibs` (the scalar IBS value) and `curve` (a numeric vector
of Brier scores at `eval_times`).
