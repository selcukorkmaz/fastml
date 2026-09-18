# Save Model Function (Deprecated)

`save.fastml` is deprecated in favour of
[`save_fastml`](https://selcukorkmaz.github.io/fastml/reference/save_fastml.md).
The old name resembled an S3 method for
[`base::save()`](https://rdrr.io/r/base/save.html), which is not a
generic, leading to dispatch confusion.

## Usage

``` r
save.fastml(model, filepath)
```

## Arguments

- model:

  An object of class `fastml`.

- filepath:

  A string specifying the file path to save the model.

## Value

No return value, called for its side effect of saving the model object
to a file.
