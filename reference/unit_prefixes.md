# Show unit prefixes

Show unit prefixes

## Usage

``` r
unit_prefixes()
```

## Value

A character matrix with 3 columns: `prefix` (prefix name like "kilo" or
"micro"), `symbol` (prefix symbol like "k"), and `scale` (power-of-ten
multiplier like "10^3" or "10^-6"). Rows are ordered from largest
(yotta, 10^24) to smallest (yocto, 10^-24).

## Examples

``` r
unit_prefixes()
#>       prefix  symbol    scale   
#>  [1,] "yotta" "Y"       "10^24" 
#>  [2,] "zetta" "Z"       "10^21" 
#>  [3,] "exa"   "E"       "10^18" 
#>  [4,] "peta"  "P"       "10^15" 
#>  [5,] "tera"  "T"       "10^12" 
#>  [6,] "giga"  "G"       "10^9"  
#>  [7,] "mega"  "M"       "10^6"  
#>  [8,] "kilo"  "k"       "10^3"  
#>  [9,] "hecto" "h"       "10^2"  
#> [10,] "deka"  "da"      "10^1"  
#> [11,] "deci"  "d"       "10^-1" 
#> [12,] "centi" "c"       "10^-2" 
#> [13,] "milli" "m"       "10^-3" 
#> [14,] "micro" "\\u03BC" "10^-6" 
#> [15,] "nano"  "n"       "10^-9" 
#> [16,] "pico"  "p"       "10^-12"
#> [17,] "femto" "f"       "10^-15"
#> [18,] "atto"  "a"       "10^-18"
#> [19,] "zepto" "z"       "10^-21"
#> [20,] "yocto" "y"       "10^-24"
```
