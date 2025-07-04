% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/past.R
\name{setup_smoothN}
\alias{setup_smoothN}
\title{Setup a smoothN accumulator}
\usage{
setup_smoothN(initial, length, order, name)
}
\arguments{
\item{initial}{Initial value for the accumulator. If NULL, the first value of the variable to smooth is used. Defaults to NULL.}

\item{length}{Length of the smoothing.}

\item{order}{Order of the smoothing.}

\item{name}{Name of the accumulator.}
}
\value{
A named vector of initial values for the accumulator.
}
\description{
Internal function to set up a smoothN accumulator.
}
