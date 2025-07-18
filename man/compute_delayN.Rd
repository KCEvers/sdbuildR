% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/delay.R
\name{compute_delayN}
\alias{compute_delayN}
\title{Compute the delayN accumulator}
\usage{
compute_delayN(input, accumulator, length, order)
}
\arguments{
\item{input}{Input value to process.}

\item{accumulator}{Accumulator values.}

\item{length}{Length of the delay.}

\item{order}{Order of the delay.}
}
\value{
A list containing the outflow value and the updated accumulator.
}
\description{
Internal function to compute the delayN accumulator.
}
\seealso{
Other internal: 
\code{\link{compare_sim}()},
\code{\link{compute_smoothN}()},
\code{\link{nonnegative}()},
\code{\link{saveat_func}()},
\code{\link{setup_delayN}()},
\code{\link{setup_smoothN}()}
}
\concept{internal}
