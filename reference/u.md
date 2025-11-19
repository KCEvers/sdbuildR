# Specify unit in equations

Flexibly use units in equations by enclosing them in `u()`. Note that
units are only supported in Julia, not in R.

## Usage

``` r
u(unit_str)
```

## Arguments

- unit_str:

  Unit string; e.g. '3 seconds'

## Value

Specified unit (only in Julia)

## Details

Unit strings are converted to their standard symbols using regular
expressions. This means that you can easily specify units without
knowing their standard symbols. For example, u('kilograms per meters
squared') will become 'kg/m^2'. You can use title-case for unit names,
but letters cannot all be uppercase if this is not the standard symbol.
For example, 'kilogram' works, but 'KILOGRAM' does not. This is to
ensure that the right unit is detected.

## See also

[`model_units()`](https://kcevers.github.io/sdbuildR/reference/model_units.md),
[`unit_prefixes()`](https://kcevers.github.io/sdbuildR/reference/unit_prefixes.md),
[`convert_u()`](https://kcevers.github.io/sdbuildR/reference/convert_u.md),
[`drop_u()`](https://kcevers.github.io/sdbuildR/reference/drop_u.md)

## Examples

``` r
# Use units in equations
sfm <- xmile() |>
  build("a", "constant",
    eqn = "u('10kilometers') - u('3meters')",
    units = "centimeters"
  )

# Units can also be set by multiplying a number with a unit
sfm <- xmile() |>
  build("a", "constant", eqn = "10 * u('kilometers') - u('3meters')")

# Addition and subtraction is only allowed between matching units
sfm <- xmile() |>
  build("a", "constant", eqn = "u('3seconds') + u('1hour')")

# Division, multiplication, and exponentiation are allowed between different units
sfm <- xmile() |>
  build("a", "constant", eqn = "u('10grams') / u('1minute')")

# Use custom units in equations
sfm <- xmile() |>
  model_units("BMI", eqn = "kilograms/meters^2", doc = "Body Mass Index") |>
  build("weight_gain", "flow", eqn = "u('2 BMI / year')", units = "BMI/year")

# Unit strings are often needed in flows to ensure dimensional consistency
sfm <- xmile() |>
  sim_specs(stop = 1, time_units = "days") |>
  build("consumed_food", "stock", eqn = "1", units = "kilocalories") |>
  build("eating", "flow",
    eqn = "u('750kilocalories') / u('6hours')",
    units = "kilocalories/day", to = "consumed_food"
  )
```
