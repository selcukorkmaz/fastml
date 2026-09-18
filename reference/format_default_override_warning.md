# Format Default Override Warning Message

Creates a human-readable warning message about default overrides.

## Usage

``` r
format_default_override_warning(algo, comparison, show_params = TRUE)
```

## Arguments

- algo:

  Algorithm name.

- comparison:

  Result from compare_defaults().

- show_params:

  Logical; whether to include parameter differences.

## Value

Character string with the warning message.
