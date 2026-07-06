# Convert verify() results to a data frame

Converts a `verify_stockflow` object to a data frame.

## Usage

``` r
# S3 method for class 'verify_stockflow'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  which = c("tests", "sims")[1],
  direction = "long",
  test = NULL,
  label = NULL,
  ignore_case = TRUE,
  status = c("pass", "fail", "error", "skip"),
  condition = NULL,
  vars = NULL,
  type = NULL,
  ...
)
```

## Arguments

- x:

  A `verify_stockflow` object (output of
  [`verify()`](https://kcevers.github.io/sdbuildR/reference/verify.md)).

- row.names:

  `NULL` or a character vector giving row names (optional).

- optional:

  Ignored; present for compatibility.

- which:

  Character. `"tests"` (default) or `"sims"`. Partial matching
  supported.

- direction:

  Character. `"long"` (default) or `"wide"`. Only used when
  `which = "sims"`.

- test:

  Integer vector of test number(s) to display (1-based). Defaults to
  `NULL` (show all tests). Can be combined with `label` (intersection).

- label:

  Character vector of regex patterns for partial, case-insensitive label
  matching. A test is included if its label matches *any* pattern. E.g.,
  `c("non-neg", "beta")` returns tests matching either fragment. Can be
  combined with `test` (intersection).

- ignore_case:

  Logical; whether `label` matching is case-insensitive. Default `TRUE`.

- status:

  Optional character vector of statuses to include (e.g.,
  `c("fail", "error")`). Defaults to all statuses.

  - `which = "tests"`: filters rows by test status.

  - `which = "sims"`: filters to conditions that have at least one test
    with a matching status.

- condition:

  Optional integer vector of condition numbers to filter by. For
  `which = "sims"`, keeps only the matching condition simulations. For
  `which = "tests"`, keeps only tests belonging to those conditions.

- vars:

  Variable names to retain in the data frame. Only applies when
  `which = "sims"`. Defaults to `NULL` to include all variables.

- type:

  Variable types to retain in the data frame. Must be one or more of
  'stock', 'flow', 'constant', 'aux', 'lookup', or 'func'. Only applies
  when `which = "sims"`. Defaults to `NULL` to include all types.

- ...:

  Additional arguments (unused).

## Value

A `data.frame`. Column set depends on `which`:

- `"tests"`: `test`, `label`, `status`, `outcome`, `expr_str`,
  `condition`, `conditions`, `message`.

- `"sims"` (long): `test`, `condition`, `conditions`, `time`,
  `variable`, `value`.

- `"sims"` (wide): `test`, `condition`, `conditions`, `time`, then one
  column per variable.

## Details

**`which = "tests"` (default)** returns one row per unit test with
columns `test`, `label`, `status`, `outcome`, `expr_str`, `conditions`,
and `message`. Use `test`, `label`, and `status` to filter.

**`which = "sims"`** returns the underlying simulation time-series in
long format with columns `test` (test number(s) that used this
simulation, as a comma-separated string), `conditions` (specified
conditions per test, if any), `time`, `variable`, and `value`. Each
unique condition generates one simulation; if multiple tests share a
condition their numbers are combined in `test` (e.g. `"1, 3"`). When
filtering with `test`, the displayed `test` value shows only the
requested matching test number(s) for the retained simulation row(s).
Use `direction = "wide"` to pivot variables into columns.

## Examples

``` r
# Create model with 2 unit tests
sfm <- stockflow("sir") |>
  unit_test(expr = all(susceptible >= 0)) |>
  # Add test with conditions
  unit_test(
    label = "lower infection rate",
    expr = all(susceptible >= 0),
    conditions = list(infection_rate = 0.1)
  )
res <- verify(sfm)

# Test results (default)
as.data.frame(res)
#>   test                                      label status outcome
#> 1    1 susceptible is at least 0 (for all values)   pass    TRUE
#> 2    2                       lower infection rate   fail   FALSE
#>                expr_str condition           conditions
#> 1 all(susceptible >= 0)         1                     
#> 2 all(susceptible >= 0)         2 infection_rate = 0.1
#>                         message
#> 1                              
#> 2 Expected: TRUE\nActual: FALSE

# Simulation time-series (long format)
as.data.frame(res, which = "sims") |> head()
#>   test condition conditions time variable    value
#> 1    1         1            0.00 infected 1.000000
#> 2    1         1            0.01 infected 1.019000
#> 3    1         1            0.02 infected 1.038361
#> 4    1         1            0.03 infected 1.058089
#> 5    1         1            0.04 infected 1.078193
#> 6    1         1            0.05 infected 1.098678

# Simulation time-series (wide format)
as.data.frame(res, which = "sims", direction = "wide") |> head()
#>   test conditions time condition.infected infected condition.recovered
#> 1    1            0.00                  1 1.000000                   1
#> 2    1            0.01                  1 1.019000                   1
#> 3    1            0.02                  1 1.038361                   1
#> 4    1            0.03                  1 1.058089                   1
#> 5    1            0.04                  1 1.078193                   1
#> 6    1            0.05                  1 1.098678                   1
#>     recovered condition.susceptible susceptible
#> 1 0.000000000                     1    99999.00
#> 2 0.001000000                     1    99998.98
#> 3 0.002019000                     1    99998.96
#> 4 0.003057360                     1    99998.94
#> 5 0.004115450                     1    99998.92
#> 6 0.005193642                     1    99998.90

# Filter to simulation for test 2 only
as.data.frame(res, which = "sims", test = 2) |> head()
#>   test condition           conditions time variable         value
#> 1    2         2 infection_rate = 0.1 0.00 infected  1.000000e+00
#> 2    2         2 infection_rate = 0.1 0.01 infected  1.009980e+02
#> 3    2         2 infection_rate = 0.1 0.02 infected  1.019050e+04
#> 4    2         2 infection_rate = 0.1 0.03 infected  9.253827e+05
#> 5    2         2 infection_rate = 0.1 0.04 infected -7.628799e+08
#> 6    2         2 infection_rate = 0.1 0.05 infected -5.820621e+14

# Only simulations for passing tests
as.data.frame(res, which = "sims", status = "pass") |> head()
#>   test condition conditions time variable    value
#> 1    1         1            0.00 infected 1.000000
#> 2    1         1            0.01 infected 1.019000
#> 3    1         1            0.02 infected 1.038361
#> 4    1         1            0.03 infected 1.058089
#> 5    1         1            0.04 infected 1.078193
#> 6    1         1            0.05 infected 1.098678
```
