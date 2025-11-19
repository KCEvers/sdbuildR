# Drop unit in equation

In rare cases, it may be desirable to drop the units of a variable
within an equation. Use `drop_u()` to render a variable unitless. See
[`u()`](https://kcevers.github.io/sdbuildR/reference/u.md) for more
information on the rules of specifying units. Note that units are only
supported in Julia, not in R.

## Usage

``` r
drop_u(x)
```

## Arguments

- x:

  Variable with unit

## Value

Unitless variable (only in Julia)

## See also

[`model_units()`](https://kcevers.github.io/sdbuildR/reference/model_units.md),
[`unit_prefixes()`](https://kcevers.github.io/sdbuildR/reference/unit_prefixes.md),
[`u()`](https://kcevers.github.io/sdbuildR/reference/u.md),
[`convert_u()`](https://kcevers.github.io/sdbuildR/reference/convert_u.md)

## Examples

``` r
# For example, the cosine function only accepts unitless arguments or
# arguments with units in radians or degrees
sfm <- xmile() |>
  build("a", "constant", eqn = "10", units = "minutes") |>
  build("b", "constant", eqn = "cos(drop_u(a))")
```
