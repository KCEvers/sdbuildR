% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/custom_func.R
\name{substr_i}
\alias{substr_i}
\title{Extract characters from string by index}
\usage{
substr_i(string, idxs)
}
\arguments{
\item{string}{String to extract from}

\item{idxs}{Integer or vector of integers indexing which characters to extract}
}
\value{
Substring
}
\description{
Equivalent of .Range() in Insight Maker
}
\examples{
substr_i("InsightMaker", 3) # "s"
substr_i("InsightMaker", c(1, 5, 10)) # "Igk"
}
