# Convert Various Prediction Formats to Survival Matrix

Attempts to convert various survival prediction formats (e.g., list of
data frames from `predict.model_fit` with type "survival", matrices)
into a standardized `[n_obs, n_eval_times]` matrix.

## Usage

``` r
convert_survival_predictions(pred_obj, eval_times, n_obs)
```

## Arguments

- pred_obj:

  The prediction object.

- eval_times:

  Numeric vector of evaluation times.

- n_obs:

  Expected number of observations (rows).

## Value

A standardized matrix of survival probabilities, or `NULL` on failure.
