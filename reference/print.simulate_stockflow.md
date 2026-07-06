# Print simulation of a stock-and-flow model

Prints the first rows of the simulation results in wide format. For a
statistical summary per variable use
[summary()](https://kcevers.github.io/sdbuildR/reference/summary.simulate_stockflow.md).

## Usage

``` r
# S3 method for class 'simulate_stockflow'
print(x, ...)
```

## Arguments

- x:

  A simulation result of class
  [`simulate_stockflow`](https://kcevers.github.io/sdbuildR/reference/simulate.stockflow.md)

- ...:

  Additional arguments (unused)

## Value

Invisibly returns `x`

## See also

[`simulate.stockflow()`](https://kcevers.github.io/sdbuildR/reference/simulate.stockflow.md),
[`summary.simulate_stockflow()`](https://kcevers.github.io/sdbuildR/reference/summary.simulate_stockflow.md),
[`plot.simulate_stockflow()`](https://kcevers.github.io/sdbuildR/reference/plot.simulate_stockflow.md),
[`as.data.frame.simulate_stockflow()`](https://kcevers.github.io/sdbuildR/reference/as.data.frame.simulate_stockflow.md)

## Examples

``` r
sfm <- stockflow("sir")
sim <- simulate(sfm)
print(sim)
#> 
#> ── Stock-and-Flow Simulation: Susceptible-Infected-Recovered (SIR) ─────────────
#> 
#> ── Data (first rows) ──
#>   time infected  recovered susceptible
#> 1 0.00 1.000000 0.00000000    99999.00
#> 2 0.01 1.019000 0.00100000    99998.98
#> 3 0.02 1.038361 0.00201900    99998.96
#> 4 0.03 1.058089 0.00305736    99998.94
#> 5 0.04 1.078193 0.00411545    99998.92
#> 
#> ℹ Access with `as.data.frame()` • Visualise with `plot()`
```
