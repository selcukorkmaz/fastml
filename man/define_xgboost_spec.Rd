% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/define_xgboost_spec.R
\name{define_xgboost_spec}
\alias{define_xgboost_spec}
\title{Define XGBoost Model Specification}
\usage{
define_xgboost_spec(task, train_data, label, tune = FALSE)
}
\arguments{
\item{task}{Character string specifying the task type: "classification" or "regression".}

\item{train_data}{Data frame containing the training data.}

\item{label}{Character string specifying the name of the target variable.}

\item{tune}{Logical indicating whether to use tuning parameters.}
}
\value{
List containing the model specification (`model_spec`).
}
\description{
Define XGBoost Model Specification
}
