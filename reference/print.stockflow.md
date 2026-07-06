# Print overview of stock-and-flow model

Prints a descriptive overview of the model structure, including
stock-flow topology, variable names, and simulation settings. For model
diagnostics, use
[`summary()`](https://kcevers.github.io/sdbuildR/reference/summary.stockflow.md).

## Usage

``` r
# S3 method for class 'stockflow'
print(x, ...)
```

## Arguments

- x:

  A stock-and-flow model object of class
  [`stockflow`](https://kcevers.github.io/sdbuildR/reference/stockflow.md)

- ...:

  Additional arguments (unused)

## Value

Invisibly returns `x`

## See also

[`summary.stockflow()`](https://kcevers.github.io/sdbuildR/reference/summary.stockflow.md),
[`dependencies()`](https://kcevers.github.io/sdbuildR/reference/dependencies.md)

## Examples

``` r
sfm <- stockflow("sir")
print(sfm)
#> 
#> ── Stock-and-Flow Model: Susceptible-Infected-Recovered (SIR) ──────────────────
#> 3 stocks • 2 flows • 4 constants
#> 
#> ── Stock-Flow Structure ──
#> infected: + new_infections - new_recoveries
#> recovered: + new_recoveries
#> susceptible: - new_infections
#> 
#> ── Other Variables ──
#> Constants: `contact_rate`, `infection_rate`, `recovery_rate`, and
#> `total_population`
#> 
#> ── Simulation Settings ──
#> Time: 0.0 to 20.0 weeks (dt = 0.01) • euler • R
#> Simulation output: stocks only
```
