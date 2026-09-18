# Create Censoring Distribution Evaluator

Creates a function to evaluate the survival function of the censoring
distribution, \\G(t) = P(C \> t)\\, using a Kaplan-Meier estimator.

## Usage

``` r
create_censor_eval(time_vec, status_vec)
```

## Arguments

- time_vec:

  Numeric vector of survival/censoring times.

- status_vec:

  Numeric vector of event statuses (1=event, 0=censored).

## Value

A function that takes a numeric vector of times and returns the
estimated censoring survival probabilities \\G(t)\\ at those times.
