# Contributing to fastml

Thanks for taking the time to contribute. fastml is developed in the open, and
questions, bug reports and patches are all welcome.

Everyone participating is expected to follow the [Code of
Conduct](CODE_OF_CONDUCT.md).

## Getting help

If something is unclear rather than broken, open a [GitHub
issue](https://github.com/selcukorkmaz/fastml/issues) and label it as a
question, or work through the [tutorial
site](https://selcukorkmaz.github.io/fastml-tutorial/) first. There is no
separate mailing list or chat; the issue tracker is the place for everything,
so that answers stay searchable for the next person.

## Reporting a bug

Open an issue that includes:

* a small **reproducible example** — ideally a [reprex](https://reprex.tidyverse.org),
  using a built-in dataset such as `iris` or a simulated one rather than data
  you cannot share;
* what you expected and what happened instead, with the error or warning text
  quoted in full;
* the output of `sessionInfo()`, or at least your fastml, R and platform
  versions.

Performance estimates that look wrong are worth reporting even when nothing
errors. A silent bias is the class of bug this package exists to prevent, so a
report of the form "these numbers seem too good" is useful even if you cannot
pinpoint the cause.

## Requesting a feature

Say what you are trying to achieve, not only what interface you have in mind —
often the goal is reachable already, and where it is not, the underlying task
shapes the design.

Please note that fastml deliberately accepts a **narrow vocabulary**. Its
preprocessing arguments compile to untrained specifications that are estimated
only inside the resampling loop, and the package refuses designs it cannot
execute correctly rather than approximating them. Proposals that would allow
preprocessing to be estimated outside the loop, or that add arbitrary pipeline
topologies, are likely to be declined even when they are individually
reasonable; that constraint is the point of the package rather than an
oversight. Feature requests that widen what can be expressed *safely* are very
welcome.

## Contributing code

1. Fork the repository and create a branch from `main`.
2. Make your change, keeping the style of the surrounding code.
3. Add or update tests under `tests/testthat/`.
4. Run the checks below.
5. Open a pull request describing what changed and why. Link the issue it
   addresses, if there is one.

### Development setup

```r
install.packages("devtools")
devtools::install_deps(dependencies = TRUE)
devtools::load_all()
```

### Running the tests

Many tests are guarded with `skip_on_cran()` because they fit real models, so
set `NOT_CRAN` to run the full suite:

```r
Sys.setenv(NOT_CRAN = "true")
devtools::test()
```

Tests for optional engines call `skip_if_not_installed()`, so a missing
suggested package results in a skip rather than a failure. Before opening a
pull request, please also run:

```r
devtools::document()
devtools::check()
```

### Test conventions

* Each test file states, in a comment at the top, what behaviour it pins and —
  for regression tests — what went wrong before. A test that only asserts the
  current output without saying why is hard to maintain.
* Prefer small, fast fixtures. Where a test must fit a model, keep the data
  small and set a seed.
* When you fix a bug, add a test that fails without your fix.

### Documentation

Roxygen comments are the source of truth for `man/`; edit the `R/` files and run
`devtools::document()` rather than editing `.Rd` files directly. If your change
affects behaviour a user relies on, update `NEWS.md` as well.

## Scope of this file

fastml is maintained by a small team alongside other work, so review may take a
little time. If a pull request has had no response after a couple of weeks,
please comment on it — that is a reminder, not a nuisance.
