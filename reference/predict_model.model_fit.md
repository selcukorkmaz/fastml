# Internal predict_model method for parsnip fits

Shim for parsnip model objects so that lime's predict_model generic
ignores unused arguments passed via \`...\`.

## Usage

``` r
predict_model.model_fit(x, newdata, type, ...)
```
