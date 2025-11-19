# Clean variable name(s)

Clean variable name(s) to create syntactically valid, unique names for
use in R and Julia.

## Usage

``` r
clean_name(new, existing, protected = c())
```

## Arguments

- new:

  Vector of names to transform to valid names

- existing:

  Vector of existing names in model

- protected:

  Optional vector of protected names

## Value

Vector of cleaned names

## Examples

``` r
sfm <- xmile("predator_prey")
# As the variable name "predator" is already taken, clean_name() will create
# an unique name
clean_name("predator", as.data.frame(sfm)[["name"]]) # "predator_1"
#> [1] "predator_1"
```
