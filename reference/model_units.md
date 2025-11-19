# Create, modify or remove custom units

A large library of units already exists, but you may want to define your
own custom units. Use `model_units()` to add, change, or erase custom
units from a stock-and-flow model. Custom units may be new base units,
or may be defined in terms of other (custom) units. See
[`u()`](https://kcevers.github.io/sdbuildR/reference/u.md) for more
information on the rules of specifying units. Note that units are only
supported in Julia, not in R.

## Usage

``` r
model_units(sfm, name, eqn = "1", doc = "", erase = FALSE, change_name = NULL)
```

## Arguments

- sfm:

  Stock-and-flow model, object of class
  [`sdbuildR_xmile`](https://kcevers.github.io/sdbuildR/reference/xmile.md).

- name:

  Name of unit. A character vector.

- eqn:

  Definition of unit. String or vector of unit definitions. Defaults to
  "1" to indicate a base unit not defined in terms of other units.

- doc:

  Documentation of unit.

- erase:

  If TRUE, remove model unit from the model. Defaults to FALSE.

- change_name:

  New name for model unit. Defaults to NULL to indicate no change.

## Value

A stock-and-flow model object of class
[`sdbuildR_xmile`](https://kcevers.github.io/sdbuildR/reference/xmile.md)

## See also

[`unit_prefixes()`](https://kcevers.github.io/sdbuildR/reference/unit_prefixes.md)

## Examples

``` r
# Units are only supported with Julia
sfm <- xmile("Crielaard2022")
sfm <- model_units(sfm, "BMI", eqn = "kg/m^2", doc = "Body Mass Index")

# You may also use words rather than symbols for the unit definition.
# The following modifies the unit BMI:
sfm <- model_units(sfm, "BMI", eqn = "kilogram/meters^2")

# Remove unit:
sfm <- model_units(sfm, "BMI", erase = TRUE)

# Unit names may be changed to be syntactically valid and avoid overlap:
sfm <- model_units(xmile(), "C0^2")
#> Warning: The custom unit name C0^2 was modified to C0_2 to comply with Julia's syntactic rules.
#> Use sfm |> model_units('old_name', change_name = 'new_name') to update the name in your model.
```
