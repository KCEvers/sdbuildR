
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sdbuildR: Stock-and-flow modelling with R

<!-- badges: start -->
<!-- badges: end -->

sdbuildR makes it easy to create and simulate stock-and-flow models in
R. With Julia as a backend, it combines fast performance with
flexibility in a user-friendly R environment.

## Installation

You can install the development version of sdbuildR like so:

sdbuildR primarily runs by calling Julia as a backend, with limited
support for simulating models in R. It is thus highly recommended to
install Julia (and the necessary packages), which is supported by the
package JuliaCall. The first call to `sdbuildR_setup()` may take around
5-15 minutes:

``` r
library(insightmakeR1)
#> 
#> Attaching package: 'insightmakeR1'
#> The following objects are masked from 'package:stats':
#> 
#>     simulate, step
#> The following object is masked from 'package:utils':
#> 
#>     debugger
sdbuildR_setup()
#> Julia version 1.11.5 at location C:\Users\kevers1\AppData\Local\R\WIN-LI~1\4.4\INSIGH~1\julia\111~1.5\JULIA-~1.5\bin will be used.
#> Loading setup script for JuliaCall...
#> Finish loading setup script for JuliaCall.
#> Setting up Julia environment for sdbuildR...
```

## Quick start

In each R session, the Julia environment for sdbuildR needs to be set
up:

``` r
library(insightmakeR1)
sdbuildR_setup()
#> Setting up Julia environment for sdbuildR...
```

Stock-and-flow models can be created in three ways. Firstly, dozens of
example models can be loaded using `xmile()`. To get an overview of the
model, plot the stock-and-flow diagram, with stocks as boxes, flows as
arrows, and the model boundary as double circles.

``` r
sfm = xmile("SIR")
plot(sfm)
```

![](man/figures/README-SIR_diagram-1.png)<!-- -->

![](man/figures/README-SIR_diagram-1.png)

The model can be simulated using `simulate()`:

``` r
sim = simulate(sfm)
plot(sim)
```

![](man/figures/README-SIR_plot-1.png)<!-- -->

![](man/figures/README-SIR_plot-1.png)

Secondly, an [Insight Maker](https://insightmaker.com/) model can be
imported to R using its URL:

``` r
sfm = insightmaker_to_sfm(URL = "https://insightmaker.com/insight/5LxQr0waZGgBcPJcNTC029/Crielaard-2022")
plot(simulate(sfm))
```

![](man/figures/README-Crielaard2022_plot-1.png)<!-- -->

![](man/figures/README-Crielaard2022_plot-1.png)

And lastly, a stock-and-flow model can be created from scratch. We make
use of the convenient pipe operator `%\>%` from the magrittr package,
which simply passes the result of an expression to the next expression
as a first argument. We first initialize an empty stock and flow model
with `xmile()`, and then use `build()` to create a logistic model of
population growth. The simulation specifications, such as the start
time, stop time, and simulation time step (`dt`), are set with
`sim_specs()`. Custom units such as people can be added with
`model_units()`.

``` r
sfm = xmile() %>%
  header(name = "Population growth") %>%
  build("X", "stock", eqn = ".01", label = "Population size", 
        units = "people") %>%
  build("inflow", "flow", eqn = "r * X / u('day')", to = "X", 
        label = "Births", units = "people/day") %>%
  build("outflow", "flow", eqn = "r * X^2 / K / u('day')", from = "X", 
        label = "Deaths", units = "people/day") %>%
  build("r", "constant", eqn = "0.1", label = "Growth rate") %>%
  build("K", "constant", eqn = "1", label = "Carrying capacity") %>%
  sim_specs(time_units = "days", start = 0, stop = 200, dt = 0.01) %>%
  model_units("people")
plot(simulate(sfm))
```

![](man/figures/README-logistic_plot-1.png)<!-- -->

![](man/figures/README-logistic_plot-1.png)

An overview of the model components and simulation specifications can be
accessed with `summary()`:

``` r
summary(sfm)
#> Your model contains:
#> * 1 Stocks: X
#> * 2 Flows: inflow, outflow
#> * 2 Constants: r, K
#> * 0 Auxiliaries
#> * 0 Graphical Functions
#> * 1 Custom model units: people
#> * 0 Macros
#> 
#> The model will be simulated from 0 to 200 d (dt = 0.01) with solver euler in Julia.
```

To quickly view all model variable properties, use `as.data.frame()`:

``` r
as.data.frame(sfm)
#>          type    name                  eqn    units             label   to from
#> 1       stock       X                  .01   people   Population size <NA> <NA>
#> 2    constant       r                  0.1        1       Growth rate <NA> <NA>
#> 3    constant       K                    1        1 Carrying capacity <NA> <NA>
#> 4        flow  inflow       r * X / u('d') people/d            Births    X <NA>
#> 5        flow outflow r * X^2 / K / u('d') people/d            Deaths <NA>    X
#> 6 model_units  people                    1     <NA>              <NA> <NA> <NA>
#>   non_negative conveyor                  eqn_Julia
#> 1        FALSE    FALSE                        .01
#> 2        FALSE       NA                        0.1
#> 3        FALSE       NA                        1.0
#> 4        FALSE       NA             r .* X ./ u"d"
#> 5        FALSE       NA r .* X .^ 2.0 ./ K ./ u"d"
#> 6           NA       NA                       <NA>
```

## Features

- **Accessibility**: Get started with stock-and-flow models with limited
  knowledge.
- **Flexibility**: Modify your models easily and safely.
- **Insight Maker integration**: Import models from Insight Maker.
- **Unit support**: Use standard or custom units to verify and interpret
  your model.
- **Julia backend**: Leverage Julia for high-performance simulations.

All package capabilities are described in the vignettes:

1.  **Building Blocks**: Learn how to build, modify, and debug
    stock-and-flow models.
2.  **Units**: Learn why and how to use (custom) units.
    <!-- 3. **Delays**: Not all dynamics are instantaneous. Learn about delays (fixed, smooth, material and information) and how to implement them with sdbuildR. -->
    <!-- 4. **Sensitivity**:  -->

## Limitations

Unlike other System Dynamics software, sdbuildR makes a distinction
between static and dynamic variables. This means that static variables -
the initial values of stocks, constants, and graphical functions -
cannot depend on dynamic variables - auxiliaries and flows. If this is
the case, an error will be thrown with the problematic dependencies and
the model will not be simulated.

Non-negative stocks and flows are only minimally supported, where flows
are not adjusted in case of non-negative stocks. Note that setting
stocks and flows to be non-negative is not recommended \[@sterman2000\].
If a stock or flow can logically never be negative, for example in the
case of animals or deaths, this should ideally arise from the model
equations and parameters itself. If they unintentionally turn negative,
it is likely due to model misspecification and including hard-coded
constraints can lead to unexpected results \[@sterman2000\].

In addition, sdbuildR does not support vectorized operations.

## Other System Dynamics software

sdbuildR is heavily based on common System Dynamics software such as
[Vensim](https://vensim.com/), [Powersim](https://powersim.com/),
[Stella](https://www.iseesystems.com/), and [Insight
Maker](https://insightmaker.com/). To translate xmile models to R, see
the R package [readsdr](https://github.com/jandraor/readsdr). To build
stock-and-flow models with the R package
[deSolve](https://cran.r-project.org/web/packages/deSolve/index.html),
the book [System Dynamics Modelling with
R](https://link.springer.com/book/10.1007/978-3-319-34043-2) by Jim
Duggan will prove useful. In Python, stock-and-flow models are supported
by [PySD](https://pysd.readthedocs.io/en/master/).

## Troubleshooting

sdbuildR is under active development, leveraging Julia for
high-performance simulations via custom R-to-Julia translations. While
thoroughly tested, the package may have bugs, particularly in complex
model translations. We encourage users to report issues on GitHub
(<https://github.com/KCEvers/sdbuildR/issues>) - your input helps the
package improve! Use `debugger(sfm)` to diagnose model errors, and use
the vignettes for guidance.
