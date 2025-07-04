% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/past.R
\name{smoothN}
\alias{smoothN}
\title{Smoothing of order N}
\usage{
smoothN(variable, length, order, initial = NULL)
}
\arguments{
\item{variable}{Variable to smooth}

\item{length}{Length of smoothing.}

\item{order}{Order of smoothing.}

\item{initial}{Value to initialize the accumulator with. If NULL, the first value of the variable to smooth is used. Defaults to NULL.}
}
\value{
Smoothed variable
}
\description{
Smoothing of order N
}
