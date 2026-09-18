# Compute Accumulated Local Effects (ALE) for a fastml model

Uses the \`iml\` package to calculate ALE for the specified feature.

## Usage

``` r
explain_ale(object, feature, data = c("train", "test"), ...)
```

## Arguments

- object:

  A \`fastml\` object.

- feature:

  Character string specifying the feature name.

- data:

  Character string specifying which data to use: `"train"` (default) or
  `"test"`.

- ...:

  Additional arguments passed to \`iml::FeatureEffect\`.

## Value

An \`iml\` object containing ALE results.

## Examples

``` r
if (FALSE) { # \dontrun{
data(iris)
iris <- iris[iris$Species != "setosa", ]
iris$Species <- factor(iris$Species)
model <- fastml(data = iris, label = "Species")
explain_ale(model, feature = "Sepal.Length")
} # }
```
