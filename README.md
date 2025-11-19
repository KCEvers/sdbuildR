
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sdbuildR: Easily Build, Simulate, and Visualise Stock-and-Flow Models in R

<!-- badges: start -->

[![R-CMD-check](https://github.com/KCEvers/sdbuildR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/KCEvers/sdbuildR/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/KCEvers/sdbuildR/graph/badge.svg)](https://app.codecov.io/gh/KCEvers/sdbuildR)
[![CRAN
status](https://www.r-pkg.org/badges/version/sdbuildR)](https://CRAN.R-project.org/package=sdbuildR)
<!-- badges: end -->

Model systems as stock-and-flow models in R. Stock-and-flow models are
foundational to system dynamics, and help to understand systems
complicated by nonlinearities, delays, and feedback loops. sdbuildR aims
to make stock-and-flow modeling accessible and effortless, enabling you
to dedicate your expertise to what matters most: building insightful,
high-quality models. Get started at
<https://kcevers.github.io/sdbuildR/>!

## Features

- **Accessibility**: Get started with stock-and-flow models with limited
  knowledge.
- **Flexibility**: Modify your models easily and robustly.
- **Insight Maker integration**: Import models from [Insight
  Maker](https://insightmaker.com/).
- **Julia backend**: Use Julia for high-performance simulations, without
  any knowledge of Julia.
- **Unit support**: Use standard or custom units to verify and interpret
  your model.

All package capabilities are described in the vignettes:

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

## Installation

The release version can be installed from CRAN:

``` r
install.packages("sdbuildR")
```

The development version can be installed from GitHub:

``` r
if (!require("remotes")) install.packages("remotes")
remotes::install_github("KCEvers/sdbuildR")
```

sdbuildR offers two simulation engines: R and Julia (supported by
[JuliaConnectoR](https://github.com/stefan-m-lenz/JuliaConnectoR/)). If
you would like to run ensemble simulations and use units, you will need
to install and set up the Julia environment. See [this
vignette](https://kcevers.github.io/sdbuildR/articles/julia-setup.html)
for guidance.

## Citation

To cite sdbuildR, please use:

``` r
citation("sdbuildR")
#> To cite package 'sdbuildR' in publications use:
#> 
#>   Evers K (2025). _sdbuildR: Easily Build, Simulate, and Visualise
#>   Stock-and-Flow Models_. R package version 1.0.7.9000,
#>   https://github.com/KCEvers/sdbuildR,
#>   <https://kcevers.github.io/sdbuildR/>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {sdbuildR: Easily Build, Simulate, and Visualise Stock-and-Flow Models},
#>     author = {Kyra Caitlin Evers},
#>     year = {2025},
#>     note = {R package version 1.0.7.9000, https://github.com/KCEvers/sdbuildR},
#>     url = {https://kcevers.github.io/sdbuildR/},
#>   }
```

## Limitations

- Unlike in other system dynamics software, sdbuildR provides only
  minimal support for non-negative stocks and flows. Specifically,
  setting stocks to non-negative will constrain the stocks to remain
  non-negative, but will not adjust the corresponding flows. In any
  case, enforcing either stocks or flows to be non-negative is not
  recommended, as it may mask model misspecification. Stocks and flows
  that logically cannot be negative (e.g., animals or deaths) should
  ideally remain non-negative as a result of the model’s equations and
  parameters, rather than by forcing them to be non-negative.

- sdbuildR does not support vectorized operations, destructuring
  assignment, or minimum and maximum constraints for variables.

- sdbuildR does not support the Insight Maker functions Stop(),
  Prompt(), Confirm(), Pause(), Fix(), Map(), Filter(), and Repeat(),
  nor the delay and past functions. A message is issued if any of these
  are detected.

## Other system dynamics software

sdbuildR is heavily based on common system dynamics software such as
[Vensim](https://en.wikipedia.org/wiki/Vensim),
[Powersim](https://powersim.com/),
[Stella](https://www.iseesystems.com/), and [Insight
Maker](https://insightmaker.com/). To translate xmile models to R, see
the R package [readsdr](https://CRAN.R-project.org/package=readsdr). To
build stock-and-flow models with the R package
[deSolve](https://CRAN.R-project.org/package=deSolve), the book [System
Dynamics Modeling with
R](https://link.springer.com/book/10.1007/978-3-319-34043-2) by Jim
Duggan will prove useful. In Python, stock-and-flow models are supported
by [PySD](https://doi.org/10.21105/joss.04329).

## Troubleshooting

sdbuildR is under active development. While thoroughly tested, the
package may have bugs, particularly in complex model translations. We
encourage users to report [issues on
GitHub](https://github.com/KCEvers/sdbuildR/issues) - your input helps
the package improve! Use `debugger()` to diagnose model errors, and use
the vignettes for guidance.
