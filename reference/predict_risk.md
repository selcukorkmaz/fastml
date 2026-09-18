# Predict Risk Scores from a Survival Model

Provides a consistent interface for computing linear predictors (risk
scores) across various survival modeling engines, including native
fastml models (e.g., Cox proportional hazards, XGBoost Cox) and
parsnip/workflow objects.

## Usage

``` r
predict_risk(fit, newdata, ...)

# S3 method for class 'fastml_native_survival'
predict_risk(fit, newdata, ...)

# S3 method for class 'workflow'
predict_risk(fit, newdata, ...)

# Default S3 method
predict_risk(fit, newdata, ...)
```

## Arguments

- fit:

  A fitted survival model object.

- newdata:

  A data frame containing predictor variables for which to compute risk
  scores.

- ...:

  Additional arguments passed to specific methods.

## Value

A numeric vector of risk scores, where higher values indicate greater
predicted risk.
