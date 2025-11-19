# Safely check whether x is less than zero

If using Julia, units are preserved

## Usage

``` r
nonnegative(x)
```

## Arguments

- x:

  Value

## Value

x if x is greater than 0, 0 otherwise

## Examples

``` r
nonnegative(NA)
#> [1] NA
nonnegative(-1)
#> [1] 0
```
