
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sdbuildR: Stock-and-flow modelling with R

<!-- badges: start -->
<!-- badges: end -->

Easily build and simulate stock-and-flow models with sdbuildR in R. With
Julia as a backend, sdbuildR combines fast performance with flexibility
in a user-friendly R environment.

## Installation

You can install the development version of sdbuildR like so:

``` r
# install.packages("remotes")
remotes::install_github("KCEvers/sdbuildR")
```

sdbuildR uses Julia as its primary backend, with limited R-based
simulation support. To enable full functionality, install Julia and
required packages via JuliaCall. Run `sdbuildR_setup()` to configure
Julia (initial setup may take 5–15 minutes):

``` r
sdbuildR::sdbuildR_setup()
```
