# Print Default Differences Table

Prints a formatted table showing differences between fastml and parsnip
defaults for the specified task type.

## Usage

``` r
print_default_differences(task = "classification", algorithms = NULL)
```

## Arguments

- task:

  Task type ("classification", "regression", or "survival").

- algorithms:

  Optional character vector of algorithms to check. If NULL, checks all
  available algorithms for the task.

## Value

Invisibly returns the differences data frame.
