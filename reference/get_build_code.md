# Generate code to build stock-and-flow model

Create R code to rebuild an existing stock-and-flow model. This may help
to understand how a model is built, or to modify an existing one.

## Usage

``` r
get_build_code(sfm)
```

## Arguments

- sfm:

  Stock-and-flow model, object of class
  [`sdbuildR_xmile`](https://kcevers.github.io/sdbuildR/reference/xmile.md).

## Value

String with code to build stock-and-flow model from scratch.

## Examples

``` r
sfm <- xmile("SIR")
get_build_code(sfm)
#> Warning: Could not use `colored = TRUE`, as the package prettycode is not installed.
#> Please install it if you want to see colored output or see
#> `?styler::print.vertical()` for more information.
#> sfm <- xmile() |>
#>   sim_specs(method = "euler", start = "0.0", stop = "20.0", dt = "0.01", save_at = "0.01", save_from = "0.0", seed = NULL, time_units = "wk", language = "R") |>
#>   header(name = "Susceptible-Infected-Recovered (SIR)", created = "2025-11-22 11:08:26.682638") |>
#>   build(name = "Susceptible", type = "stock", eqn = "99999", label = "Susceptible") |>
#>   build(name = "Infected", type = "stock", eqn = "1", label = "Infected") |>
#>   build(name = "Recovered", type = "stock", label = "Recovered") |>
#>   build(name = "Beta", type = "constant", eqn = "Effective_Contact_Rate / Total_Population", label = "Beta") |>
#>   build(name = "Total_Population", type = "constant", eqn = "100000", label = "Total_Population") |>
#>   build(name = "Effective_Contact_Rate", type = "constant", eqn = "2", label = "Effective_Contact_Rate") |>
#>   build(name = "Delay", type = "constant", eqn = "2", label = "Delay") |>
#>   build(name = "Lambda", type = "aux", eqn = "Beta * Infected", label = "Lambda") |>
#>   build(name = "Infection_Rate", type = "flow", eqn = "Susceptible * Lambda", to = "Infected", from = "Susceptible", label = "Infection_Rate") |>
#>   build(name = "Recovery_Rate", type = "flow", eqn = "Infected / Delay", to = "Recovered", from = "Infected", label = "Recovery_Rate")
```
