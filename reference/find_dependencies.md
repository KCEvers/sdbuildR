# Find dependencies

Find which other variables each variable is dependent on.

## Usage

``` r
find_dependencies(sfm, reverse = FALSE)
```

## Arguments

- sfm:

  Stock-and-flow model, object of class
  [`sdbuildR_xmile`](https://kcevers.github.io/sdbuildR/reference/xmile.md).

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
sfm <- xmile("SIR")
find_dependencies(sfm)
#> $Susceptible
#> character(0)
#> 
#> $Infected
#> character(0)
#> 
#> $Recovered
#> character(0)
#> 
#> $Beta
#> [1] "Effective_Contact_Rate" "Total_Population"      
#> 
#> $Total_Population
#> character(0)
#> 
#> $Effective_Contact_Rate
#> character(0)
#> 
#> $Delay
#> character(0)
#> 
#> $Lambda
#> [1] "Beta"     "Infected"
#> 
#> $Infection_Rate
#> [1] "Susceptible" "Lambda"     
#> 
#> $Recovery_Rate
#> [1] "Infected" "Delay"   
#> 
```
