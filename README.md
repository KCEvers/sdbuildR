
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sdbuildR: Easily build and simulate stock-and-flow models in R

<!-- badges: start -->

[![R-CMD-check](https://github.com/KCEvers/sdbuildR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/KCEvers/sdbuildR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Model systems as stock-and-flow models in R. Stock-and-flow models are
foundational to system dynamics, and help to understand systems
complicated by nonlinearities, delays, and feedback loops. sdbuildR aims
to make stock-and-flow modelling accessible and effortless, enabling you
to dedicate your expertise to what matters most: building insightful,
high-quality models. Get started at
<https://kcevers.github.io/sdbuildR/>.

## Features

- **Accessibility**: Get started with stock-and-flow models with limited
  knowledge.
- **Flexibility**: Modify your models easily and robustly.
- **Insight Maker integration**: Import models from Insight Maker.
- **Julia backend**: Leverage Julia for high-performance simulations,
  without any knowledge of Julia.
- **Unit support**: Use standard or custom units to verify and interpret
  your model.

All package capabilities are described in the vignettes:

- `vignette("build")` Learn how to build, modify, and debug
  stock-and-flow models.
- `vignette("units")` Learn why and how to use (custom) units.
  <!-- 3. **Delays**: Not all dynamics are instantaneous. Learn about delays (fixed, smooth, material and information) and how to implement them with sdbuildR. -->
  <!-- 4. **Sensitivity**:  -->

## Installation

You can install the development version of sdbuildR like so:

``` r
# install.packages("remotes")
remotes::install_github("KCEvers/sdbuildR")
```

sdbuildR offers two simulation engines: R and Julia. If you would like
to use units and delay functions, you will need to set up the Julia
environment. Run `use_julia()` to install Julia and required packages
(initial setup may take 5–15 minutes):

``` r
sdbuildR::use_julia()
```

## Limitations

<!-- Unlike other System Dynamics software, sdbuildR makes a distinction between static and dynamic variables. This means that static variables - the initial values of stocks, constants, and graphical functions - cannot depend on dynamic variables - auxiliaries and flows. If this is the case, an error will be thrown with the problematic dependencies and the model will not be simulated.  -->

Non-negative stocks and flows are minimally supported, Setting stocks to
non-negative will not adjust flows. Hard-coding non-negativity is not
recommended, as it may mask model misspecification. Stocks and flows
that logically cannot be negative (e.g., animals or deaths) should
remain non-negative through appropriate model equations and parameters
to avoid unexpected results. In addition, sdbuildR does not support
vectorized operations.

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

sdbuildR is under active development. While thoroughly tested, the
package may have bugs, particularly in complex model translations. We
encourage users to report issues on GitHub
(<https://github.com/KCEvers/sdbuildR/issues>) - your input helps the
package improve! Use `debugger()` to diagnose model errors, and use the
vignettes for guidance.
