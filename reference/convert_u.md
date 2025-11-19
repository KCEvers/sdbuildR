# Convert unit in equation

In rare cases, it may be desirable to change the units of a variable
within an equation. Use `convert_u()` to convert a variable to another
matching unit. See
[`u()`](https://kcevers.github.io/sdbuildR/reference/u.md) for more
information on the rules of specifying units. Note that units are only
supported in Julia, not in R.

## Usage

``` r
convert_u(x, unit_def)
```

## Arguments

- x:

  Variable

- unit_def:

  Unit definition, e.g. u('seconds')

## Value

Variable with new unit (only in Julia)

## See also

[`model_units()`](https://kcevers.github.io/sdbuildR/reference/model_units.md),
[`unit_prefixes()`](https://kcevers.github.io/sdbuildR/reference/unit_prefixes.md),
[`u()`](https://kcevers.github.io/sdbuildR/reference/u.md),
[`drop_u()`](https://kcevers.github.io/sdbuildR/reference/drop_u.md)

## Examples

``` r
# Change the unit of rate from minutes to hours
sfm <- xmile() |>
  build("rate", "constant", eqn = "10", units = "minutes") |>
  build("change", "flow",
    eqn = "(room_temperature - coffee_temperature) / convert_u(rate, u('hour'))"
  )
```
