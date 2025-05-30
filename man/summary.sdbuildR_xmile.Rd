% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_xmile.R
\name{summary.sdbuildR_xmile}
\alias{summary.sdbuildR_xmile}
\title{Print overview of stock-and-flow model}
\usage{
\method{summary}{sdbuildR_xmile}(object, ...)
}
\arguments{
\item{object}{Stock-and-flow model of class sdbuildR_xmile}

\item{...}{Optional arguments}
}
\description{
Print summary of stock-and-flow model, including number of stocks, flows, constants, auxiliaries, graphical functions, macros, custom model units, and use of delay functions. Also prints simulation specifications.
}
\examples{
sfm = xmile("SIR")
summary(sfm)
}
\seealso{
\code{\link[=build]{build()}}
}
