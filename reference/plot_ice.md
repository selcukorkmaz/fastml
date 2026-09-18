# Plot ICE curves for a fastml model

Generates Individual Conditional Expectation (ICE) plots for selected
features using the \`pdp\` package (ggplot2 engine), and returns both
the underlying data and the plot object.

## Usage

``` r
plot_ice(object, features, data = c("train", "test"), target_class = NULL, ...)
```

## Arguments

- object:

  A \`fastml\` object.

- features:

  Character vector of feature names to plot.

- data:

  Character string specifying which data to use: `"train"` (default) or
  `"test"`.

- target_class:

  For classification, which class probability to plot. If NULL
  (default), uses the positive class determined by the model settings.
  For multiclass problems, this shows the probability of the specified
  class vs all others.

- ...:

  Additional arguments passed to \`pdp::partial\`.

## Value

A list with two elements: \`data\` (the ICE data frame) and \`plot\`
(the ggplot object).

## Examples

``` r
if (FALSE) { # \dontrun{
data(iris)
iris <- iris[iris$Species != "setosa", ]
iris$Species <- factor(iris$Species)
model <- fastml(data = iris, label = "Species")
plot_ice(model, features = "Sepal.Length")
} # }
```
