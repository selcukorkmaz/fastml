# Get Best Model Names

Extracts and returns the best engine names from a named list of model
workflows.

## Usage

``` r
get_best_model_names(models)
```

## Arguments

- models:

  A named list where each element corresponds to an algorithm and
  contains a list of model workflows. Each workflow should be compatible
  with
  [`tune::extract_fit_parsnip`](https://hardhat.tidymodels.org/reference/hardhat-extract.html).

## Value

A named character vector. The names of the vector correspond to the
algorithm names, and the values represent the chosen best engine name
for that algorithm.

## Details

For each algorithm, the function extracts the engine names from the
model workflows using
[`tune::extract_fit_parsnip`](https://hardhat.tidymodels.org/reference/hardhat-extract.html).
It then chooses `"randomForest"` if it is available; otherwise, it
selects the first non-`NA` engine. If no engine names can be extracted
for an algorithm, `NA_character_` is returned.
