# Get Model Engine Names

Extracts and returns a named vector mapping algorithm names to engine
names from a nested list of model workflows.

## Usage

``` r
get_model_engine_names(models)
```

## Arguments

- models:

  A nested list of model workflows. Each inner list should contain model
  objects from which a fitted model can be extracted using
  [`tune::extract_fit_parsnip`](https://hardhat.tidymodels.org/reference/hardhat-extract.html).

## Value

A named character vector where the names correspond to algorithm names
(e.g., `"rand_forest"`, `"logistic_reg"`) and the values correspond to
the associated engine names (e.g., `"ranger"`, `"glm"`).

## Details

The function iterates over a nested list of model workflows and, for
each workflow, attempts to extract the fitted model object using
[`tune::extract_fit_parsnip`](https://hardhat.tidymodels.org/reference/hardhat-extract.html).
If successful, it retrieves the algorithm name from the first element of
the class attribute of the model specification and the engine name from
the specification. The results are combined into a named vector.
