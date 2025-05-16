
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

sdbuildR offers two simulation engines: R and Julia. If you would like
to use units and delay functions, you will need to set up the Julia
environment. Run `use_julia()` to install Julia and required packages
(initial setup may take 5–15 minutes):

``` r
sdbuildR::use_julia()
```
