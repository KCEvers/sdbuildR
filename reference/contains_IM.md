# Check if needle is in haystack

Check whether value is in vector or string. Equivalent of .Contains() in
Insight Maker.

## Usage

``` r
contains_IM(haystack, needle)
```

## Arguments

- haystack:

  Vector or string to search through

- needle:

  Value to search for

## Value

Logical value

## Examples

``` r
contains_IM(c("a", "b", "c"), "d") # FALSE
#> [1] FALSE
contains_IM(c("abcdef"), "bc") # TRUE
#> [1] TRUE
```
