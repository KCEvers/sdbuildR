# Get regular expressions for time units in Julia

Get regular expressions for time units in Julia

## Usage

``` r
get_regex_time_units()
```

## Value

Named vector with regular expressions as names and units as entries

## Examples

``` r
x <- get_regex_time_units()
head(x)
#> ^[S|s]econd[s]?$ ^[M|m]inute[s]?$   ^[H|h]our[s]?$    ^[D|d]ay[s]?$ 
#>              "s"         "minute"             "hr"              "d" 
#>   ^[W|w]eek[s]?$   ^[Y|y]ear[s]?$ 
#>             "wk"             "yr" 
```
