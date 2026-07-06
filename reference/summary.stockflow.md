# Run model diagnostics

Check for common formulation problems in a stock-and-flow model.

## Usage

``` r
# S3 method for class 'stockflow'
summary(object, ...)
```

## Arguments

- object:

  Stock-and-flow model, object of class
  [`stockflow`](https://kcevers.github.io/sdbuildR/reference/stockflow.md).

- ...:

  Additional arguments (currently unused).

## Value

Object of class `summary_stockflow`. A flat named list with one entry
per check. Each entry contains a `problem` field (`"none"`, `"warning"`,
or `"error"`) and type-specific data fields.

## Details

The following problems are detected:

- An absence of stocks

- Flows without a source (`from`) or target (`to`)

- Flows connected to a stock that does not exist

- Undefined variable references in equations

- Circularity in equations

The following potential problems are detected:

- Absence of flows

- Stocks without inflows or outflows

- Equations with a value of 0

## Examples

``` r
# No issues
sfm <- stockflow("sir")
summary(sfm)
#> 
#> ── Stock-and-Flow Model Diagnostics ────────────────────────────────────────────
#> 
#> ── Potential problem (1) ──
#> • `recovered` has an equation of 0.

# Detect absence of stocks or flows
sfm <- stockflow()
summary(sfm)
#> 
#> ── Stock-and-Flow Model Diagnostics ────────────────────────────────────────────
#> 
#> ── Problem (1) ──
#> ! Model has no stocks.
#> → Add at least one stock with `stock()` or `update()`.
#> 
#> ── Potential problem (1) ──
#> • Model has no flows.
#> → Add flows with `flow()` or `update()`.

# Detect stocks without inflows or outflows
sfm <- stockflow() |> update("Prey", "stock")
summary(sfm)
#> 
#> ── Stock-and-Flow Model Diagnostics ────────────────────────────────────────────
#> 
#> ── Potential problems (3) ──
#> • Model has no flows.
#> → Add flows with `flow()` or `update()`.
#> • Stock not connected to any flow: `Prey`.
#> • `Prey` has an equation of 0.

# Detect circularity in equation definitions
sfm <- stockflow() |>
  update("Prey", "stock", eqn = "Predator") |>
  update("Predator", "stock", eqn = "Prey")
#> Warning: Could not order static equations.
#> ! Topological sorting of static equations failed.
#> ℹ Circular dependencies detected involving variables: Prey, Predator - Prey
#>   depends on Predator - Predator depends on Prey
#> → Check constant and stock definitions for circular dependencies.
summary(sfm)
#> 
#> ── Stock-and-Flow Model Diagnostics ────────────────────────────────────────────
#> 
#> ── Problem (1) ──
#> ! Circular dependency in static equations.
#>   Variables involved: `Prey` and `Predator`.
#>   `Prey` depends on `Predator`.
#>   `Predator` depends on `Prey`.
#> 
#> ── Potential problems (2) ──
#> • Model has no flows.
#> → Add flows with `flow()` or `update()`.
#> • Stocks not connected to any flow: `Predator` and `Prey`.
```
