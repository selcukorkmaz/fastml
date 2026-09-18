# Clean Column Names or Character Vectors by Removing Special Characters

This function can operate on either a data frame or a character vector:

- **Data frame**: Detects columns whose names contain any character that
  is not a letter, number, or underscore, removes colons, replaces
  slashes with underscores, and spaces with underscores.

- **Character vector**: Applies the same cleaning rules to every element
  of the vector.

## Usage

``` r
sanitize(x)
```

## Arguments

- x:

  A data frame or character vector to be cleaned.

## Value

- If `x` is a data frame: returns a data frame with cleaned column
  names.

- If `x` is a character vector: returns a character vector with cleaned
  elements.
