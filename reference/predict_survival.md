# Predict survival probabilities from a survival model

Predict survival probabilities from a survival model

## Usage

``` r
predict_survival(fit, newdata, times, ...)

# S3 method for class 'fastml_native_survival'
predict_survival(fit, newdata, times, ...)

# S3 method for class 'workflow'
predict_survival(fit, newdata, times, ...)

# Default S3 method
predict_survival(fit, newdata, times, ...)
```

## Arguments

- fit:

  A fitted survival model.

- newdata:

  A data frame of predictors for which to compute survival curves.

- times:

  Numeric vector of evaluation times.

- ...:

  Additional arguments passed to methods.

## Value

A numeric matrix with one row per observation and one column per time.
