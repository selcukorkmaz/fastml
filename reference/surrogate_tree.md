# Fit a surrogate decision tree for a fastml model

Builds an interpretable tree approximating the behaviour of the
underlying model using the \`iml\` package.

## Usage

``` r
surrogate_tree(object, maxdepth = 3, data = c("train", "test"), ...)
```

## Arguments

- object:

  A \`fastml\` object.

- maxdepth:

  Maximum depth of the surrogate tree. Default 3.

- data:

  Character string specifying which data to use: `"train"` (default) or
  `"test"`.

- ...:

  Additional arguments passed to \`iml::TreeSurrogate\`.

## Value

An \`iml::TreeSurrogate\` object.

## Examples

``` r
if (FALSE) { # \dontrun{
data(iris)
iris <- iris[iris$Species != "setosa", ]
iris$Species <- factor(iris$Species)
model <- fastml(data = iris, label = "Species")
surrogate_tree(model)
} # }
```
