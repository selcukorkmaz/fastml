# Warn About Default Overrides

Issues a warning if fastml defaults differ from parsnip defaults and the
warning hasn't been shown yet in this session.

## Usage

``` r
warn_default_override(
  algo,
  task,
  fastml_engine,
  fastml_params = NULL,
  verbose = FALSE,
  warn_once = TRUE
)
```

## Arguments

- algo:

  Algorithm name.

- task:

  Task type.

- fastml_engine:

  fastml's default engine for this algorithm.

- fastml_params:

  fastml's default parameters (optional).

- verbose:

  If TRUE, always show the message (as a message, not warning).

- warn_once:

  If TRUE (default), only warn once per algorithm per session.

## Value

Invisibly returns the comparison result.
