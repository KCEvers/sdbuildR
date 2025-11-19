# Logistic function

Computes the logistic (i.e., sigmoid) function with configurable slope,
midpoint, and upper asymptote.

## Usage

``` r
logistic(x, slope = 1, midpoint = 0, upper = 1)

sigmoid(x, slope = 1, midpoint = 0, upper = 1)
```

## Arguments

- x:

  Value at which to evaluate the function

- slope:

  Slope of logistic function at the midpoint. Defaults to 1.

- midpoint:

  Midpoint of logistic function where the output is `upper/2`. Defaults
  to 0.

- upper:

  Upper asymptote (maximal value) of the logistic function. Defaults to
  1.

## Value

Numeric value given by \$\$f(x) = \frac{upper}{1 + e^{-slope \cdot (x -
midpoint)}}\$\$

## Details

The logistic function is a smooth S-shaped curve bounded between 0 and
`upper`. It transitions from near 0 to near `upper` around the
`midpoint`, with the steepness of this transition controlled by `slope`.

## Examples

``` r
logistic(0)
#> [1] 0.5
# equivalent:
sigmoid(0)
#> [1] 0.5
logistic(1, slope = 5, midpoint = 0.5, upper = 10)
#> [1] 9.241418

# Visualize different slopes
x <- seq(-5, 5, length.out = 1000)
plot(x, logistic(x, slope = 1), type = "l", ylab = "f(x)", ylim = c(0, 1))
lines(x, logistic(x, slope = 5), col = "blue")
lines(x, logistic(x, slope = 50), col = "red")
legend("topleft", legend = c("slope = 1", "slope = 5", "slope = 50"),
       col = c("black", "blue", "red"), lty = 1)
```
