# Generate random number from custom distribution

Equivalent of RandDist() in Insight Maker

## Usage

``` r
rdist(a, b)
```

## Arguments

- a:

  Vector to draw sample from

- b:

  Vector of probabilities

## Value

One sample from custom distribution

## Examples

``` r
rdist(c(1, 2, 3), c(.5, .25, .25))
#> [1] 1
```
