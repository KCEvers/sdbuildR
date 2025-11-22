# Get started with sdbuildR

``` r
library(sdbuildR)
```

### Get started with a template

Stock-and-flow models can be created in three ways in sdbuildR. Firstly,
dozens of example models can be loaded using
[`xmile()`](https://kcevers.github.io/sdbuildR/reference/xmile.md). Plot
the stock-and-flow diagram to get an overview of the model:

``` r
sfm <- xmile("SIR")
plot(sfm)
```

The model can be simulated using
[`simulate()`](https://kcevers.github.io/sdbuildR/reference/simulate.md):

``` r
sim <- simulate(sfm)
plot(sim)
```

### Import models from Insight Maker

Secondly, an [Insight Maker](https://insightmaker.com/) model can be
imported to R using its URL:

``` r
URL <- "https://insightmaker.com/insight/5LxQr0waZGgBcPJcNTC029/Crielaard-2022"
sfm <- insightmaker_to_sfm(URL = URL)
```

``` r
sim <- simulate(sfm)
```

``` r
plot(sim)
```

### Build a model from scratch

Lastly, a stock-and-flow model can be created from scratch. We first
initialize an empty stock and flow model with
[`xmile()`](https://kcevers.github.io/sdbuildR/reference/xmile.md), and
then use
[`build()`](https://kcevers.github.io/sdbuildR/reference/build.md) to
create a logistic model of population growth. The simulation
specifications, such as the start time, stop time, simulation time step
(`dt`), and time units, are set with
[`sim_specs()`](https://kcevers.github.io/sdbuildR/reference/sim_specs.md).
Below, we make use of the convenient pipe operator `|>`, which simply
passes the result of an expression to the next expression as a first
argument.

``` r
sfm <- xmile() |>
  header(name = "Population growth") |>
  build("X", "stock", eqn = "100", label = "Population size") |>
  build("change", "flow",
        eqn = "r * (1 - X/K)", to = "X",
        label = "Births and Deaths"
  ) |>
  build("r", "constant", eqn = "5", label = "Growth rate") |>
  build("K", "constant", eqn = "10000", label = "Carrying capacity") |>
  sim_specs(stop = 10000, time_units = "days")
```

``` r
sim <- simulate(sfm)
```

``` r
plot(sim)
```

An overview of the model components and simulation specifications can be
accessed with [`summary()`](https://rdrr.io/r/base/summary.html):

``` r
summary(sfm)
#> Your model contains:
#> * 1 Stocks: X
#> * 1 Flows: change
#> * 2 Constants: r, K
#> * 0 Auxiliaries
#> * 0 Graphical Functions
#> * 0 Custom model units
#> * 0 Macros
#> 
#> Simulation time: 0.0 to 10000.0 days (dt = 0.01)
#> Simulation settings: solver euler in R
```

To quickly view all model variable properties, use
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html):

``` r
as.data.frame(sfm)
#>       type   name           eqn units             label   to non_negative
#> 1    stock      X           100     1   Population size <NA>        FALSE
#> 2 constant      r             5     1       Growth rate <NA>        FALSE
#> 3 constant      K         10000     1 Carrying capacity <NA>        FALSE
#> 4     flow change r * (1 - X/K)     1 Births and Deaths    X        FALSE
#>              eqn_julia
#> 1                100.0
#> 2                  5.0
#> 3              10000.0
#> 4 r .* (1.0 .- X ./ K)
```

## Start building your own models!

See the vignettes to learn more about all features of sdbuildR:

- [Build](https://kcevers.github.io/sdbuildR/articles/build.html): Learn
  how to build, modify, and debug stock-and-flow models.
- [Julia
  setup](https://kcevers.github.io/sdbuildR/articles/julia-setup.html):
  Install and set up the Julia environment for running ensemble
  simulations and using units.
- [Ensemble](https://kcevers.github.io/sdbuildR/articles/ensemble.html):
  Learn how to assess a model’s sensitivity, uncertainty and robustness
  with ensemble simulations.
- [Units](https://kcevers.github.io/sdbuildR/articles/units.html): Learn
  why and how to use (custom) units.

## Citation

To cite sdbuildR, please use:

``` r
citation("sdbuildR")
#> To cite package 'sdbuildR' in publications use:
#> 
#>   Evers K (2025). _sdbuildR: Easily Build, Simulate, and Visualise
#>   Stock-and-Flow Models_. R package version 1.0.8.9000,
#>   <https://kcevers.github.io/sdbuildR/>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {sdbuildR: Easily Build, Simulate, and Visualise Stock-and-Flow Models},
#>     author = {Kyra Caitlin Evers},
#>     year = {2025},
#>     note = {R package version 1.0.8.9000},
#>     url = {https://kcevers.github.io/sdbuildR/},
#>   }
```
