# View all standard units

Obtain a data frame with all standard units in Julia's Unitful package
and added custom units by sdbuildR.

## Usage

``` r
get_units()
```

## Value

A character matrix with 5 columns: `description` (unit description),
`name` (unit symbol or abbreviation), `full_name` (full unit name),
`definition` (mathematical definition in terms of base units), and
`prefix` (logical indicating whether SI prefixes like kilo- or milli-
can be applied). Includes SI base units, derived units, CGS units, US
customary units, and custom units added by sdbuildR.

## Examples

``` r
x <- get_units()
head(x)
#>      description                                                  name
#> [1,] "The meter, the SI base unit of length."                     "m" 
#> [2,] "The second, the SI base unit of time."                      "s" 
#> [3,] "The ampere, the SI base unit of electric current."          "A" 
#> [4,] "The kelvin, the SI base unit of thermodynamic temperature." "K" 
#> [5,] "The candela, the SI base unit of luminous intensity."       "cd"
#> [6,] "The gram, the SI base unit for weight."                     "g" 
#>      full_name definition prefix
#> [1,] "Meter"   ""         "TRUE"
#> [2,] "Second"  ""         "TRUE"
#> [3,] "Ampere"  ""         "TRUE"
#> [4,] "Kelvin"  ""         "TRUE"
#> [5,] "Candela" ""         "TRUE"
#> [6,] "Gram"    ""         "TRUE"
```
