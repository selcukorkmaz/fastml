# Compute feature interaction strengths for a fastml model

Uses the \`iml\` package to quantify the strength of feature
interactions.

## Usage

``` r
interaction_strength(object, data = c("train", "test"), ...)
```

## Arguments

- object:

  A \`fastml\` object.

- data:

  Character string specifying which data to use: `"train"` (default) or
  `"test"`.

- ...:

  Additional arguments passed to \`iml::Interaction\`.

## Value

An \`iml::Interaction\` object.

## Examples

``` r
if (FALSE) { # \dontrun{
data(iris)
iris <- iris[iris$Species != "setosa", ]
iris$Species <- factor(iris$Species)
model <- fastml(data = iris, label = "Species")
interaction_strength(model)
} # }
```
