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

Other build: 
\code{\link{as.data.frame.sdbuildR_sim}()},
\code{\link{as.data.frame.sdbuildR_xmile}()},
\code{\link{build}()},
\code{\link{debugger}()},
\code{\link{get_build_code}()},
\code{\link{header}()},
\code{\link{macro}()},
\code{\link{plot.sdbuildR_xmile}()},
\code{\link{xmile}()}
}
\concept{build}
