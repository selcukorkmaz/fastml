# Predict method for fastml objects

Generates predictions from a trained \`fastml\` object on new data.
Supports both single-model and multi-model workflows, and handles
classification and regression tasks with optional post-processing and
verbosity.

## Usage

``` r
# S3 method for class 'fastml'
predict(
  object,
  newdata,
  type = "auto",
  model_name = NULL,
  verbose = FALSE,
  postprocess_fn = NULL,
  eval_time = NULL,
  ...
)
```

## Arguments

- object:

  A fitted \`fastml\` object created by the \`fastml()\` function.

- newdata:

  A data frame or tibble containing new predictor data for which to
  generate predictions.

- type:

  Type of prediction to return. One of \`"auto"\` (default),
  \`"class"\`, \`"prob"\`, \`"numeric"\`, \`"survival"\`, or
  \`"risk"\`. - \`"auto"\`: chooses \`"class"\` for classification,
  \`"numeric"\` for regression, and \`"survival"\` for survival. -
  \`"prob"\`: returns class probabilities (only for classification). -
  \`"class"\`: returns predicted class labels. - \`"numeric"\`: returns
  predicted numeric values (for regression). - \`"survival"\`: returns
  survival probabilities at the supplied \`eval_time\` horizons (for
  survival tasks). - \`"risk"\`: returns risk scores on the linear
  predictor scale (for survival tasks).

- model_name:

  (Optional) Name of a specific model to use when \`object\$best_model\`
  contains multiple models.

- verbose:

  Logical; if \`TRUE\`, prints progress messages showing which models
  are used during prediction.

- postprocess_fn:

  (Optional) A function to apply to the final predictions (e.g., inverse
  transforms, thresholding).

- eval_time:

  Optional numeric vector of time points (on the original time scale) at
  which to return survival probabilities when \`type = "survival"\`.
  Required for survival tasks when requesting survival curves.

- ...:

  Additional arguments (currently unused).

## Value

A vector of predictions, or a named list of predictions (if multiple
models are used). If \`postprocess_fn\` is supplied, its output will be
returned instead.
