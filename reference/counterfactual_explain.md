# Generate counterfactual explanations for a fastml model

Uses DALEX ceteris-paribus profiles (\`predict_profile\`) to compute
counterfactual-style what-if explanations for a given observation.

## Usage

``` r
counterfactual_explain(
  object,
  observation,
  variables = NULL,
  data = c("train", "test"),
  positive_class = NULL,
  event_class = NULL,
  label_levels = NULL,
  ...
)
```

## Arguments

- object:

  A \`fastml\` object.

- observation:

  A single observation (data frame with one row) to compute
  counterfactuals for.

- variables:

  Optional character vector of candidate variables to vary. Only numeric
  variables are used for counterfactual profiling.

- data:

  Character string specifying which data to use for the explainer
  background: `"train"` (default) or `"test"`.

- positive_class:

  Optional string used to filter lines/points in the resulting profiles
  for classification tasks.

- event_class:

  Optional event class indicator propagated from
  \`fastml_prepare_explainer_inputs()\` (kept for compatibility).

- label_levels:

  Optional vector of label levels propagated from
  \`fastml_prepare_explainer_inputs()\` (kept for compatibility).

- ...:

  Additional arguments passed to \`DALEX::predict_profile\`.

## Value

A list (returned invisibly) containing the DALEX profile, filtered
lines/points when \`positive_class\` is supplied, and the plotted object
if rendering succeeds.
