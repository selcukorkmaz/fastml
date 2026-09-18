# Get Default Engine

Returns the default engine corresponding to the specified algorithm.

## Usage

``` r
get_default_engine(algo, task = NULL)
```

## Arguments

- algo:

  A character string specifying the name of the algorithm. The value
  should match one of the supported algorithm names.

- task:

  Optional task type (e.g., `"classification"`, `"regression"`, or
  `"survival"`). Used to determine defaults that depend on the task.

## Value

A character string containing the default engine name associated with
the provided algorithm.

## Details

The function uses a `switch` statement to select the default engine
based on the given algorithm. For survival random forests, the function
defaults to `"aorsf"`. If the provided algorithm does not have a defined
default engine, the function terminates with an error.
