# Length of vector or string

Equivalent of .Length() in Insight Maker, which returns the number of
elements when performed on a vector, but returns the number of
characters when performed on a string

## Usage

``` r
length_IM(x)
```

## Arguments

- x:

  A vector or a string

## Value

The number of elements in x if x is a vector; the number of characters
in x if x is a string

## Examples

``` r
length_IM(c("a", "b", "c")) # 3
#> [1] 3
length_IM("abcdef") # 6
#> [1] 6
```
