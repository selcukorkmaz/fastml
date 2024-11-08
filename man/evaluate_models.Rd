% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evaluate_models.R
\name{evaluate_models}
\alias{evaluate_models}
\title{Evaluate Models Function}
\usage{
evaluate_models(models, test_data, label, metric = "Accuracy")
}
\arguments{
\item{models}{A list of trained model objects.}

\item{test_data}{Preprocessed test data frame.}

\item{label}{Name of the target variable.}

\item{metric}{The performance metric to optimize (e.g., "Accuracy", "ROC").}
}
\value{
A list of performance metrics for each model.
}
\description{
Evaluates the trained models on the test data and computes performance metrics.
}
