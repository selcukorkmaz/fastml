# Compare fastml and parsnip defaults

Compares the default engine and parameter choices between fastml and
parsnip for a given algorithm.

## Usage

``` r
compare_defaults(algo, task, fastml_engine, fastml_params = NULL)
```

## Arguments

- algo:

  Character string specifying the algorithm name.

- task:

  Character string specifying the task type.

- fastml_engine:

  Character string of the fastml default engine.

- fastml_params:

  List of fastml default parameters.

## Value

A list with components:

- engine_differs:

  Logical indicating if engines differ.

- fastml_engine:

  The fastml default engine.

- parsnip_engine:

  The parsnip default engine.

- param_differences:

  Named list of parameters that differ.
