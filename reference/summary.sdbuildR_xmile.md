# Print overview of stock-and-flow model

Print summary of stock-and-flow model, including number of stocks,
flows, constants, auxiliaries, graphical functions, macros, and custom
model units, as well as simulation specifications and use of delay
functions.

## Usage

``` r
# S3 method for class 'sdbuildR_xmile'
summary(object, ...)
```

## Arguments

- object:

  A stock-and-flow model object of class
  [`sdbuildR_xmile`](https://kcevers.github.io/sdbuildR/reference/xmile.md)

- ...:

  Optional arguments

## Value

Summary object of class summary.sdbuildR_xmile

## See also

[`build()`](https://kcevers.github.io/sdbuildR/reference/build.md)

## Examples

``` r
sfm <- xmile("SIR")
summary(sfm)
#> Your model contains:
#> * 3 Stocks: Susceptible, Infected, Recovered
#> * 2 Flows: Infection_Rate, Recovery_Rate
#> * 4 Constants: Beta, Total_Population, Effective_Contact_Rate, Delay
#> * 1 Auxiliaries: Lambda
#> * 0 Graphical Functions
#> * 0 Custom model units
#> * 0 Macros
#> 
#> Simulation time: 0.0 to 20.0 weeks (dt = 0.01)
#> Simulation settings: solver euler in R
```
