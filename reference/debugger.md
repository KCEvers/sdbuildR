# Debug stock-and-flow model

Check for common formulation problems in a stock-and-flow model.

## Usage

``` r
debugger(sfm, quietly = FALSE)
```

## Arguments

- sfm:

  Stock-and-flow model, object of class
  [`sdbuildR_xmile`](https://kcevers.github.io/sdbuildR/reference/xmile.md).

- quietly:

  If TRUE, don't print problems. Defaults to FALSE.

## Value

If `quietly = FALSE`, list with problems and potential problems.

## Details

The following problems are detected:

- An absence of stocks

- Flows without a source (`from`) or target (`to`)

- Flows connected to a stock that does not exist

- Undefined variable references in equations

- Circularity in equations

- Connected stocks and flows without both having units or no units

- Missing unit definitions

The following potential problems are detected:

- Absence of flows

- Stocks without inflows or outflows

- Equations with a value of 0

## Examples

``` r
# No issues
sfm <- xmile("SIR")
debugger(sfm)
#> No problems detected!
#> Potentially problematic:
#> * These variables have an equation of 0:
#> - Recovered

# Detect absence of stocks or flows
sfm <- xmile()
debugger(sfm)
#> Problems:
#> * Your model has no stocks.
#> 
#> Potentially problematic:
#> * Your model has no flows.

# Detect stocks without inflows or outflows
sfm <- xmile() |> build("Prey", "stock")
debugger(sfm)
#> No problems detected!
#> Potentially problematic:
#> * Your model has no flows.
#> 
#> * These variables have an equation of 0:
#> - Prey

# Detect circularity in equation definitions
sfm <- xmile() |>
  build("Prey", "stock", eqn = "Predator") |>
  build("Predator", "stock", eqn = "Prey")
debugger(sfm)
#> Problems:
#> * Circular dependencies detected involving variables: Predator, Prey
#> - Predator depends on Prey
#> - Prey depends on Predator
#> 
#> Potentially problematic:
#> * Your model has no flows.
```
