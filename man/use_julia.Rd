% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setup_julia.R
\name{use_julia}
\alias{use_julia}
\title{Set up Julia environment for sdbuildR}
\usage{
use_julia(
  stop = FALSE,
  version = "latest",
  JULIA_HOME = NULL,
  dir = NULL,
  force = FALSE,
  force_install = FALSE,
  ...
)
}
\arguments{
\item{stop}{If TRUE, stop active Julia session. Defaults to FALSE.}

\item{version}{Julia version. Default is "latest", which will install the most recent stable release.}

\item{JULIA_HOME}{Path to Julia installation. Defaults to NULL to locate Julia automatically.}

\item{dir}{Directory to install Julia. Defaults to NULL to use default location.}

\item{force}{If TRUE, force Julia setup to execute again.}

\item{force_install}{If TRUE, force Julia installation even if existing version is found. Defaults to FALSE.}

\item{...}{Optional arguments passed to JuliaCall::julia_setup()}
}
\description{
Create Julia environment to simulate stock-and-flow models. \code{use_julia()} looks for a Julia installation, and will install Julia as well as some required packages if not found. Keep in mind that the installation can take around 5-15 minutes. In every R session, \code{use_julia()} needs to be run once, which can take around 20 seconds.
}
\examples{
# Start Julia session
use_julia()
# Stop Julia session
use_julia(stop = TRUE)
}
