# Find dependencies

Find which other variables each variable is dependent on.

## Usage

``` r
dependencies(object, name = NULL, type = NULL, reverse = FALSE)
```

## Arguments

- object:

  Stock-and-flow model, object of class
  [`stockflow`](https://kcevers.github.io/sdbuildR/reference/stockflow.md).

- name:

  Variable names to find dependencies for. Defaults to `NULL` to include
  all variables.

- type:

  Variable types to find dependencies for. Must be one or more of
  'stock', 'flow', 'constant', 'aux', 'lookup', or 'func'. Defaults to
  `NULL` to include all types.

- reverse:

  If FALSE, list for each variable X which variables Y it depends on for
  its equation definition. If TRUE, don't show dependencies but
  dependents. This reverses the dependencies, such that for each
  variable X, it lists what other variables Y depend on X.

## Value

List, with for each model variable what other variables it depends on,
or if `reverse = TRUE`, which variables depend on it

## Examples

``` r
sfm <- stockflow("sir")
dependencies(sfm)
#> $infected
#> character(0)
#> 
#> $recovered
#> character(0)
#> 
#> $susceptible
#> character(0)
#> 
#> $new_infections
#> [1] "infection_rate" "susceptible"    "infected"      
#> 
#> $new_recoveries
#> [1] "recovery_rate" "infected"     
#> 
#> $contact_rate
#> character(0)
#> 
#> $infection_rate
#> [1] "contact_rate"     "total_population"
#> 
#> $recovery_rate
#> character(0)
#> 
#> $total_population
#> [1] "susceptible" "infected"    "recovered"  
#> 
```
