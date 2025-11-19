# Remainder and modulus

Remainder and modulus operators. The modulus and remainder are not the
same in case either a or b is negative. If you work with negative
numbers, modulus is always non-negative (it matches the sign of the
divisor).

## Usage

``` r
rem(a, b)

mod(a, b)

a %REM% b
```

## Arguments

- a:

  Dividend

- b:

  Divisor

## Value

Remainder

## Examples

``` r
# Modulus and remainder are the same when a and b are positive
a <- 7
b <- 3
rem(a, b)
#> [1] 1
mod(a, b)
#> [1] 1
# Modulus and remainder are NOT when either a or b is negative
a <- -7
b <- 3
rem(a, b)
#> [1] -1
mod(a, b)
#> [1] 2
a <- 7
b <- -3
rem(a, b)
#> [1] 1
mod(a, b)
#> [1] -2
# Modulus and remainder are the same when both a and b are negative
a <- -7
b <- -3
rem(a, b)
#> [1] -1
mod(a, b)
#> [1] -1

# Alternative way of computing the remainder:
a %REM% b
#> [1] -1
```
