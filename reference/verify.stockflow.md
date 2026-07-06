# Verify unit tests against simulation results

Run all active unit tests defined on a stock-and-flow model. Use
[`unit_test()`](https://kcevers.github.io/sdbuildR/reference/unit_test.md)
to define tests; use
[`unit_tests()`](https://kcevers.github.io/sdbuildR/reference/unit_tests.md)
to display them.

## Usage

``` r
# S3 method for class 'stockflow'
verify(object, test = NULL, ...)
```

## Arguments

- object:

  An
  [`stockflow`](https://kcevers.github.io/sdbuildR/reference/stockflow.md)
  object.

- test:

  Integer vector of test number(s) to run (numbers-based, as shown by
  [`unit_tests()`](https://kcevers.github.io/sdbuildR/reference/unit_tests.md)).
  Defaults to `NULL` (run all tests).

- ...:

  Additional arguments passed to
  [`sim_settings()`](https://kcevers.github.io/sdbuildR/reference/sim_settings.md)
  (e.g., `seed`, `dt`).

## Value

An object of class `verify_stockflow`, returned invisibly. Use
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) to
extract results as a data frame and
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) to visualize
the simulations used. The object contains:

- results:

  List of test result entries, one per test (including inactive tests,
  which appear with `status = "skip"`). Each entry has `label`,
  `expr_str`, `conditions`, `status`, `error_type`, `message`, and
  `outcome`.

- object:

  The `stockflow` model the tests were run against.

- sims:

  Nested list of `simulate_stockflow` objects used internally by
  [`plot.verify_stockflow()`](https://kcevers.github.io/sdbuildR/reference/plot.verify_stockflow.md).
  Always present (never `NULL`).

- condition:

  Named integer vector mapping each test label to its condition index.
  Used internally by
  [`plot.verify_stockflow()`](https://kcevers.github.io/sdbuildR/reference/plot.verify_stockflow.md).

- n_conditions:

  Number of unique simulation conditions.

- test_indices:

  Integer vector of the original 1-based test numbers that were run (as
  shown by
  [`unit_tests()`](https://kcevers.github.io/sdbuildR/reference/unit_tests.md)).
  Equal to `seq_along(results)` when `test = NULL` (all tests run).

## Details

Calling
[`verify()`](https://kcevers.github.io/sdbuildR/reference/verify.md) on
a `stockflow` model will first simulate the model, then run all tests —
including those that require re-simulation under alternative
[conditions](https://kcevers.github.io/sdbuildR/reference/unit_test.md).
Simulations are always retained in the returned object so that
[`plot.verify_stockflow()`](https://kcevers.github.io/sdbuildR/reference/plot.verify_stockflow.md)
works without any extra arguments.

For repeated-run robustness testing use
[`ensemble()`](https://kcevers.github.io/sdbuildR/reference/ensemble.md)
instead.

## See also

[`unit_test()`](https://kcevers.github.io/sdbuildR/reference/unit_test.md),
[`unit_tests()`](https://kcevers.github.io/sdbuildR/reference/unit_tests.md),
[`simulate.stockflow()`](https://kcevers.github.io/sdbuildR/reference/simulate.stockflow.md),
[`as.data.frame.verify_stockflow()`](https://kcevers.github.io/sdbuildR/reference/as.data.frame.verify_stockflow.md),
[`plot.verify_stockflow()`](https://kcevers.github.io/sdbuildR/reference/plot.verify_stockflow.md)

## Examples

``` r
sfm <- stockflow("sir") |>
  unit_test(expr = all(susceptible >= 0)) |>
  unit_test(
    label = "recovered increases over time",
    expr = all(diff(recovered) >= 0)
  )

verify(sfm)
#> 
#> ── Stock-and-Flow Unit Test Results ────────────────────────────────────────────
#> 2/2 tests passed.
#> ✔ 1. susceptible is at least 0 (for all values)
#> ✔ 2. recovered increases over time
```
