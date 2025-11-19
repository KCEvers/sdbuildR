# Get regular expressions for units in Julia

Get regular expressions for units in Julia

## Usage

``` r
get_regex_units(sfm = NULL)
```

## Arguments

- sfm:

  Stock-and-flow model, object of class
  [`sdbuildR_xmile`](https://kcevers.github.io/sdbuildR/reference/xmile.md).

## Value

Named vector with regular expressions as names and units as entries

## Examples

``` r
x <- get_regex_units()
head(x)
#>   ^[M|m]eter[s]?$  ^[S|s]econd[s]?$  ^[A|a]mpere[s]?$  ^[K|k]elvin[s]?$ 
#>               "m"               "s"               "A"               "K" 
#> ^[C|c]andela[s]?$    ^[G|g]ram[s]?$ 
#>              "cd"               "g" 
```
