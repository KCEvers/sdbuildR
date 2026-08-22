# Documentation-only pages ("hub" topics) that carry an \alias for the base
# generics sdbuildR provides methods for, so that ?plot, ?print, ?summary,
# ?head, ?tail and ?as.data.frame reach an index of the relevant methods.
# An alias can only point at one topic, hence one overview page per generic
# rather than an alias on an arbitrary method page.


#' Plot methods for sdbuildR objects
#'
#' sdbuildR provides a `plot()` method for each of its object classes:
#'
#' \describe{
#'   \item{[plot.stockflow()]}{Stock-and-flow diagram of a model.}
#'   \item{[plot.simulate_stockflow()]}{Timeseries of a simulation.}
#'   \item{[plot.ensemble_stockflow()]}{Timeseries of an ensemble simulation.}
#'   \item{[plot.verify_stockflow()]}{Results of [verify()].}
#' }
#'
#' @name sdbuildR-plot
#' @aliases plot
#' @concept methods
#' @seealso [stockflow()], [`simulate()`][simulate.stockflow()], [ensemble()], [verify()]
NULL


#' Print methods for sdbuildR objects
#'
#' sdbuildR provides a `print()` method for each of its object classes, called
#' automatically when an object is shown at the console:
#'
#' \describe{
#'   \item{[print.stockflow()]}{Overview of a stock-and-flow model.}
#'   \item{[print.simulate_stockflow()]}{Overview of a simulation.}
#'   \item{[print.summary_stockflow()]}{Model diagnostics from [`summary()`][summary.stockflow()].}
#'   \item{[print.compare_stockflow()]}{Comparison of two models from [compare_models()].}
#' }
#'
#' `print()` methods are also defined for [`ensemble_stockflow`][ensemble()],
#' [`verify_stockflow`][verify()] and [`unit_tests_stockflow`][unit_tests()]
#' objects.
#'
#' @name sdbuildR-print
#' @aliases print
#' @concept methods
NULL


#' Summary methods for sdbuildR objects
#'
#' sdbuildR provides a `summary()` method for each of its object classes:
#'
#' \describe{
#'   \item{[summary.stockflow()]}{Run diagnostics on a stock-and-flow model.}
#'   \item{[summary.simulate_stockflow()]}{Statistical summary per variable of a simulation.}
#' }
#'
#' A `summary()` method is also defined for [`ensemble_stockflow`][ensemble()]
#' objects, returning the summary statistics computed across runs.
#'
#' @name sdbuildR-summary
#' @aliases summary
#' @concept methods
NULL


#' First and last rows of sdbuildR results
#'
#' `head()` and `tail()` return the first or last rows of the data frame
#' produced by `as.data.frame()`:
#'
#' \describe{
#'   \item{[head.simulate_stockflow()], [tail.simulate_stockflow()]}{Rows of a simulation.}
#'   \item{[head.verify_stockflow()], [tail.verify_stockflow()]}{Rows of [verify()] results.}
#' }
#'
#' `head()` and `tail()` methods are also defined for
#' [`ensemble_stockflow`][ensemble()] objects.
#'
#' @name sdbuildR-head-tail
#' @aliases head tail
#' @concept methods
#' @seealso \link[=sdbuildR-as.data.frame]{as.data.frame()} methods
NULL


#' Data frame methods for sdbuildR objects
#'
#' `as.data.frame()` extracts the contents of an sdbuildR object as a data
#' frame:
#'
#' \describe{
#'   \item{[as.data.frame.stockflow()]}{Structure of a model: its stocks, flows, constants and auxiliaries.}
#'   \item{[as.data.frame.simulate_stockflow()]}{Results of a simulation.}
#'   \item{[as.data.frame.ensemble_stockflow()]}{Results of an ensemble simulation.}
#'   \item{[as.data.frame.verify_stockflow()]}{Results of [verify()].}
#' }
#'
#' @name sdbuildR-as.data.frame
#' @aliases as.data.frame
#' @concept methods
#' @seealso \link[=sdbuildR-head-tail]{head()} and \link[=sdbuildR-head-tail]{tail()} methods
NULL
