# Unit Testing Stock-and-Flow Models

``` r

library(sdbuildR)
```

Stock-and-flow models can easily become complex, producing unexpected
behaviours as the model is developed. To ensure a model behaves as
intended, we may make use of unit tests, a concept from software
engineering. A unit test compares the behaviour of a small aspect of the
model (i.e., a unit) to an explicitly formulated expectation. By
implementing an exhaustive test suite, we can systematically and
routinely check the model. In this vignette, we will demonstrate how to
use `sdbuildR` to create and run unit tests for a simple model.

## Load the model

We will demonstrate unit tests on the SIR
(Susceptible-Infected-Recovered) model, a classic model in epidemiology
of infectious diseases.

``` r

sfm <- stockflow("sir")
plot(sfm)
```

A quick simulation shows the model’s behaviour.

``` r

sim <- simulate(sfm)
plot(sim)
```

## Add simple unit tests

[`unit_test()`](https://kcevers.github.io/sdbuildR/reference/unit_test.md)
adds one test at a time. A test expression should evaluate to `TRUE`
when the model behaves as expected. For example, we may test that the
susceptible population never becomes negative, as people cannot be less
than zero:

``` r

sfm <- sfm |>
  unit_test(
    expr = all(susceptible >= 0)
  )
```

Note that a test expression can refer to any variable in the model. It
is evaluated on the simulated output data, meaning that `susceptible`
refers to the entire timeseries of the susceptible population (not just
its initial value).

We assess our test with
[`verify()`](https://kcevers.github.io/sdbuildR/reference/verify.md),
which simulates the model and assess the test on the output data:

``` r

verify(sfm)
#> 
#> ── Stock-and-Flow Unit Test Results ────────────────────────────────────────────
#> 1/1 test passed.
#> ✔ 1. susceptible is at least 0 (for all values)
```

The test passed, so we can be confident that the susceptible population
does not become negative under the current model specification. As shown
above, a test label has been automatically generated. A custom label can
also be provided, which we demonstrate with a test that checks that the
total population is conserved (i.e., the sum of susceptible, infected,
and recovered equals the total population at all times):

``` r

sfm <- sfm |>
  unit_test(
    expr = all(abs(susceptible + infected + recovered - total_population) < 1e-8),
    label = "Population is conserved"
  )

verify(sfm)
#> 
#> ── Stock-and-Flow Unit Test Results ────────────────────────────────────────────
#> 2/2 tests passed.
#> ✔ 1. susceptible is at least 0 (for all values)
#> ✔ 2. Population is conserved
```

Unit tests are most useful when kept small, easily understood, and tied
to a specific model expectation.

## Test behaviour under a condition

Intended model behaviour can also be tested under a specific scenario
using `conditions`. `conditions` should be a named list, and can only
specify constants and initial conditions of stock variables.

``` r

as.data.frame(sfm,
  type = c("constant", "stock"),
  properties = c("eqn", "label")
)
#>       type             name                                eqn            label
#> 1    stock         infected                                  1         Infected
#> 2    stock        recovered                                  0        Recovered
#> 3    stock      susceptible                              99999      Susceptible
#> 4 constant     contact_rate                                  2     Contact rate
#> 5 constant   infection_rate    contact_rate / total_population   Infection rate
#> 6 constant    recovery_rate                                0.1    Recovery rate
#> 7 constant total_population susceptible + infected + recovered Total population
```

Here, we set `recovery_rate = 0`, so the recovered stock should not
increase:

``` r

sfm <- sfm |>
  unit_test(
    expr = all(diff(recovered) == 0),
    label = "Recovered does not increase when recovery_rate is zero",
    conditions = list(recovery_rate = 0)
  )

verify(sfm)
#> 
#> ── Stock-and-Flow Unit Test Results ────────────────────────────────────────────
#> 3/3 tests passed.
#> ✔ 1. susceptible is at least 0 (for all values)
#> ✔ 2. Population is conserved
#> ✔ 3. Recovered does not increase when recovery_rate is zero
```

Multiple conditions can be specified, which together define one scenario
under which the expression is evaluated. In other words, all conditions
are applied simultaneously. For example, when all stocks are initialized
at zero (except for `recovered`, as this would lead to a
`total_population` of zero and thus division by zero), they should
remain at zero:

``` r

sfm <- sfm |>
  unit_test(
    expr = all(susceptible == 0) & all(infected == 0) & all(diff(recovered) == 0),
    conditions = list(susceptible = 0, infected = 0, recovered = 1)
  )

verify(sfm)
#> 
#> ── Stock-and-Flow Unit Test Results ────────────────────────────────────────────
#> 4/4 tests passed.
#> ✔ 1. susceptible is at least 0 (for all values)
#> ✔ 2. Population is conserved
#> ✔ 3. Recovered does not increase when recovery_rate is zero
#> ✔ 4. susceptible is equal to 0 (for all values) and infected is equal to 0 (for
#>   all values) and the successive differences of recovered is equal to 0 (for
#>   all values) (susceptible = 0, infected = 0, recovered = 1)
```

## View defined tests

All defined tests can be viewed with
[`unit_tests()`](https://kcevers.github.io/sdbuildR/reference/unit_tests.md).

``` r

unit_tests(sfm)
#> 
#> ── Stock-and-Flow Unit Tests ───────────────────────────────────────────────────
#> 4 tests • 4/4 active • 2/4 include conditions
#> • 1. susceptible is at least 0 (for all values)
#>   `all(susceptible >= 0)`
#> • 2. Population is conserved
#>   `all(abs(susceptible + infected + recovered - total_population) < 1e-08)`
#> • 3. Recovered does not increase when recovery_rate is zero
#>   `all(diff(recovered) == 0)`
#>   Conditions: recovery_rate = 0
#> • 4. susceptible is equal to 0 (for all values) and infected is equal to 0 (for
#> all values) and the successive differences of recovered is equal to 0 (for all
#> values) (susceptible = 0, infected = 0, recovered = 1)
#>   `all(susceptible == 0) & all(infected == 0) & all(diff(recovered) == 0)`
#>   Conditions: susceptible = 0, infected = 0, recovered = 1
```

Each test shows its number, label, expression, and any conditions.
Inactive tests (those with `active = FALSE`) are shown with a dotted
icon and are skipped when
[`verify()`](https://kcevers.github.io/sdbuildR/reference/verify.md) is
called.

Unit tests can be filtered by their number:

``` r

unit_tests(sfm, test = 1)
#> 
#> ── Stock-and-Flow Unit Tests ───────────────────────────────────────────────────
#> 1 tests • 1/1 active • 0/1 include conditions
#> • 1. susceptible is at least 0 (for all values)
#>   `all(susceptible >= 0)`
```

Or using a regular expression for their label:

``` r

unit_tests(sfm, label = "population")
#> 
#> ── Stock-and-Flow Unit Tests ───────────────────────────────────────────────────
#> 1 tests • 1/1 active • 0/1 include conditions
#> • 2. Population is conserved
#>   `all(abs(susceptible + infected + recovered - total_population) < 1e-08)`
```

``` r

unit_tests(sfm, label = "susceptible")
#> 
#> ── Stock-and-Flow Unit Tests ───────────────────────────────────────────────────
#> 2 tests • 2/2 active • 1/2 include conditions
#> • 1. susceptible is at least 0 (for all values)
#>   `all(susceptible >= 0)`
#> • 4. susceptible is equal to 0 (for all values) and infected is equal to 0 (for
#> all values) and the successive differences of recovered is equal to 0 (for all
#> values) (susceptible = 0, infected = 0, recovered = 1)
#>   `all(susceptible == 0) & all(infected == 0) & all(diff(recovered) == 0)`
#>   Conditions: susceptible = 0, infected = 0, recovered = 1
```

By default, `ignore_case = TRUE`, so the regular expression is
case-insensitive. Set `ignore_case = FALSE` for a case-sensitive search.

## Modify an existing test

Existing tests can be modified by their numeric position (`test`):

``` r

# Modify label of test number 1
sfm <- unit_test(sfm, test = 1, label = "Susceptible is non-negative")

unit_tests(sfm, test = 1)
#> 
#> ── Stock-and-Flow Unit Tests ───────────────────────────────────────────────────
#> 1 tests • 1/1 active • 0/1 include conditions
#> • 1. Susceptible is non-negative
#>   `all(susceptible >= 0)`
```

Or their label (exact match):

``` r

sfm <- unit_test(sfm,
  expr = all(abs(susceptible + infected + recovered - total_population) < 1e-5),
  label = "Population is conserved"
)

unit_tests(sfm, label = "Population is conserved")
#> 
#> ── Stock-and-Flow Unit Tests ───────────────────────────────────────────────────
#> 1 tests • 1/1 active • 0/1 include conditions
#> • 2. Population is conserved
#>   `all(abs(susceptible + infected + recovered - total_population) < 1e-05)`
```

## Deactivate / reactivate tests

Use the `active` flag to temporarily disable a test without removing it.

``` r

sfm <- unit_test(sfm, test = 1, active = FALSE)
verify(sfm)
#> 
#> ── Stock-and-Flow Unit Test Results ────────────────────────────────────────────
#> 3/3 tests passed.
#> ℹ 1. Susceptible is non-negative
#>   Test is inactive.
#> 
#> ✔ 2. Population is conserved
#> ✔ 3. Recovered does not increase when recovery_rate is zero
#> ✔ 4. susceptible is equal to 0 (for all values) and infected is equal to 0 (for
#>   all values) and the successive differences of recovered is equal to 0 (for
#>   all values) (susceptible = 0, infected = 0, recovered = 1)
```

``` r

# Reactivate
sfm <- unit_test(sfm, test = 1, active = TRUE)
verify(sfm)
#> 
#> ── Stock-and-Flow Unit Test Results ────────────────────────────────────────────
#> 4/4 tests passed.
#> ✔ 1. Susceptible is non-negative
#> ✔ 2. Population is conserved
#> ✔ 3. Recovered does not increase when recovery_rate is zero
#> ✔ 4. susceptible is equal to 0 (for all values) and infected is equal to 0 (for
#>   all values) and the successive differences of recovered is equal to 0 (for
#>   all values) (susceptible = 0, infected = 0, recovered = 1)
```

## Remove tests

Tests can be removed by number or by their (exact) label.

``` r

sfm <- discard_unit_test(sfm, test = 3)
unit_tests(sfm)
#> 
#> ── Stock-and-Flow Unit Tests ───────────────────────────────────────────────────
#> 3 tests • 3/3 active • 1/3 include conditions
#> • 1. Susceptible is non-negative
#>   `all(susceptible >= 0)`
#> • 2. Population is conserved
#>   `all(abs(susceptible + infected + recovered - total_population) < 1e-05)`
#> • 3. susceptible is equal to 0 (for all values) and infected is equal to 0 (for
#> all values) and the successive differences of recovered is equal to 0 (for all
#> values) (susceptible = 0, infected = 0, recovered = 1)
#>   `all(susceptible == 0) & all(infected == 0) & all(diff(recovered) == 0)`
#>   Conditions: susceptible = 0, infected = 0, recovered = 1
```

## Specify simulation settings for tests

By default,
[`verify()`](https://kcevers.github.io/sdbuildR/reference/verify.md)
runs a single simulation with the default settings. Different simulation
settings (as in `sim_settings`) can be passed to `verify`:

``` r

verify(sfm, seed = 123)
#> 
#> ── Stock-and-Flow Unit Test Results ────────────────────────────────────────────
#> 3/3 tests passed.
#> ✔ 1. Susceptible is non-negative
#> ✔ 2. Population is conserved
#> ✔ 3. susceptible is equal to 0 (for all values) and infected is equal to 0 (for
#>   all values) and the successive differences of recovered is equal to 0 (for
#>   all values) (susceptible = 0, infected = 0, recovered = 1)
```

``` r

verify(sfm, dt = .1)
#> 
#> ── Stock-and-Flow Unit Test Results ────────────────────────────────────────────
#> 3/3 tests passed.
#> ✔ 1. Susceptible is non-negative
#> ✔ 2. Population is conserved
#> ✔ 3. susceptible is equal to 0 (for all values) and infected is equal to 0 (for
#>   all values) and the successive differences of recovered is equal to 0 (for
#>   all values) (susceptible = 0, infected = 0, recovered = 1)
```

## Inspecting `verify()` results

The simulations used for testing can be retrieved and inspected:

``` r

res <- verify(sfm)
plot(res)
```

Interactively browse through conditions:

``` r

plot(res, condition_display = "slider")
```

Plot the simulation used for a specific test:

``` r

plot(res, test = 1)
```

View test results as a data frame:

``` r

as.data.frame(res)
#>   test
#> 1    1
#> 2    2
#> 3    3
#>                                                                                                                                                                                                              label
#> 1                                                                                                                                                                                      Susceptible is non-negative
#> 2                                                                                                                                                                                          Population is conserved
#> 3 susceptible is equal to 0 (for all values) and infected is equal to 0 (for all values) and the successive differences of recovered is equal to 0 (for all values) (susceptible = 0, infected = 0, recovered = 1)
#>   status outcome
#> 1   pass    TRUE
#> 2   pass    TRUE
#> 3   pass    TRUE
#>                                                                  expr_str
#> 1                                                   all(susceptible >= 0)
#> 2 all(abs(susceptible + infected + recovered - total_population) < 1e-05)
#> 3  all(susceptible == 0) & all(infected == 0) & all(diff(recovered) == 0)
#>   condition                                   conditions message
#> 1         1                                                     
#> 2         1                                                     
#> 3         2 susceptible = 0, infected = 0, recovered = 1
```

Retrieve test results for a specific test by number:

``` r

as.data.frame(res, test = 1)
#>   test                       label status outcome              expr_str
#> 1    1 Susceptible is non-negative   pass    TRUE all(susceptible >= 0)
#>   condition conditions message
#> 1         1
```

Retrieve simulation data for a specific test:

``` r

head(res, which = "sims", test = 1)
#>   test condition conditions time variable    value
#> 1    1         1            0.00 infected 1.000000
#> 2    1         1            0.01 infected 1.019000
#> 3    1         1            0.02 infected 1.038361
#> 4    1         1            0.03 infected 1.058089
#> 5    1         1            0.04 infected 1.078193
#> 6    1         1            0.05 infected 1.098678
# or:
head(as.data.frame(res, which = "sims", test = 1))
#>   test condition conditions time variable    value
#> 1    1         1            0.00 infected 1.000000
#> 2    1         1            0.01 infected 1.019000
#> 3    1         1            0.02 infected 1.038361
#> 4    1         1            0.03 infected 1.058089
#> 5    1         1            0.04 infected 1.078193
#> 6    1         1            0.05 infected 1.098678
```

## Inspect failed tests

We will first deliberately break the model by making the susceptible
population negative (which is not physically possible):

``` r

sfm <- update(sfm, susceptible, eqn = -10)
```

This should lead some unit tests to fail:

``` r

res <- verify(sfm)
print(res)
#> 
#> ── Stock-and-Flow Unit Test Results ────────────────────────────────────────────
#> 1/3 tests passed.
#> ✖ 1. Susceptible is non-negative
#>   Expected: TRUE Actual: FALSE
#> 
#> ✖ 2. Population is conserved
#>   Expected: TRUE Actual: FALSE
#> 
#> ✔ 3. susceptible is equal to 0 (for all values) and infected is equal to 0 (for
#>   all values) and the successive differences of recovered is equal to 0 (for
#>   all values) (susceptible = 0, infected = 0, recovered = 1)
```

To inspect the failed tests, we can plot the simulations that were used
for testing:

``` r

plot(res, status = "fail")
```

We can also extract the simulation data for the failed tests:

``` r

head(res, which = "sims", status = "fail")
#>   test condition conditions time variable    value
#> 1 1, 2         1            0.00 infected 1.000000
#> 2 1, 2         1            0.01 infected 1.021222
#> 3 1, 2         1            0.02 infected 1.042945
#> 4 1, 2         1            0.03 infected 1.065183
#> 5 1, 2         1            0.04 infected 1.087950
#> 6 1, 2         1            0.05 infected 1.111262
```
