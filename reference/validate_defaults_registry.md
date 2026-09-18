# Validate Defaults Registry Against Parsnip

Compares the hardcoded parsnip default engines in fastml's registry
against the actual defaults reported by
[`parsnip::show_engines()`](https://parsnip.tidymodels.org/reference/show_engines.html).
Returns a list of any mismatches found, which may indicate that parsnip
has updated its defaults since fastml's registry was last updated.

## Usage

``` r
validate_defaults_registry()
```

## Value

A list of mismatches. Each element is a list with components:

- algorithm:

  The algorithm name.

- fastml_default:

  The default engine recorded in fastml's registry.

- parsnip_default:

  The actual default engine from parsnip.

Returns an empty list if no mismatches are found.

## Details

This function queries parsnip for model specifications and compares
against the hardcoded `parsnip_defaults` list in
[`get_parsnip_default_engine()`](https://selcukorkmaz.github.io/fastml/reference/get_parsnip_default_engine.md).
Mismatches may occur when:

- Parsnip updates its default engine for a model type

- New engines are added to parsnip that become the new default

- fastml's registry has not been updated after a parsnip release

This validation is intended for package maintenance and testing
purposes.

## Examples

``` r
if (FALSE) { # \dontrun{
mismatches <- validate_defaults_registry()
if (length(mismatches) > 0) {
  message("Found ", length(mismatches), " mismatch(es) with parsnip defaults")
}
} # }
```
