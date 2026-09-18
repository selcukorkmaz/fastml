# Generate LIME explanations for a fastml model

Creates a \`lime\` explainer using processed (encoded, scaled) data and
returns feature explanations for new observations. The new observation
is automatically preprocessed using the same recipe to ensure alignment
with the explainer background.

## Usage

``` r
explain_lime(
  object,
  new_observation,
  data = c("train", "test"),
  n_features = 5,
  n_labels = 1,
  ...
)
```

## Arguments

- object:

  A \`fastml\` object.

- new_observation:

  A data frame containing the new observation(s) to explain. Must
  contain the same columns as the original training data (before
  preprocessing). The function will apply the stored preprocessor to
  transform it.

- data:

  Character string specifying which data to use for the LIME explainer
  background: `"train"` (default) or `"test"`.

- n_features:

  Number of features to show in the explanation. Default 5.

- n_labels:

  Number of labels to explain (classification only). Default 1.

- ...:

  Additional arguments passed to \`lime::explain\`.

## Value

An object produced by \`lime::explain\`.

## Examples

``` r
if (FALSE) { # \dontrun{
data(iris)
iris <- iris[iris$Species != "setosa", ]
iris$Species <- factor(iris$Species)
model <- fastml(data = iris, label = "Species")
explain_lime(model, new_observation = iris[1, ])
} # }
```
