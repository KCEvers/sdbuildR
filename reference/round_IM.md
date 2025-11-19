# Round Half-Up (as in Insight Maker)

R rounds .5 to 0, whereas Insight Maker rounds .5 to 1. This function is
the equivalent of Insight Maker's Round() function.

## Usage

``` r
round_IM(x, digits = 0)
```

## Arguments

- x:

  Value

- digits:

  Number of digits; optional, defaults to 0

## Value

Rounded value

## Examples

``` r
round_IM(.5) # 1
#> [1] 1
round(.5) # 0
#> [1] 0
round_IM(-0.5) # 0
#> [1] 0
round(-0.5) # 0
#> [1] 0
round_IM(1.5) # 2
#> [1] 2
round(1.5) # 2
#> [1] 2
```
