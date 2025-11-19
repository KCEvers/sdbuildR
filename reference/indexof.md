# Find index of needle in haystack

Find index of value in vector or string. Equivalent of .IndexOf() in
Insight Maker.

## Usage

``` r
indexof(haystack, needle)
```

## Arguments

- haystack:

  Vector or string to search through

- needle:

  Value to search for

## Value

Index, integer

## Examples

``` r
indexof(c("a", "b", "c"), "b") # 2
#> [1] 2
indexof("haystack", "hay") # 1
#> [1] 1
indexof("haystack", "m") # 0
#> [1] 0
```
