# Get Engine Names from Model Workflows

Extracts and returns a list of unique engine names from a list of model
workflows.

## Usage

``` r
get_engine_names(models)
```

## Arguments

- models:

  A list where each element is a list of model workflows. Each workflow
  is expected to contain a fitted model that can be processed with
  [`tune::extract_fit_parsnip`](https://hardhat.tidymodels.org/reference/hardhat-extract.html).

## Value

A list of character vectors. Each vector contains the unique engine
names extracted from the corresponding element of `models`.

## Details

The function applies
[`tune::extract_fit_parsnip`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
to each model workflow to extract the fitted model object. It then
retrieves the engine name from the model specification (`spec$engine`).
If the extraction fails, `NA_character_` is returned for that workflow.
Finally, the function removes any duplicate engine names using `unique`.
