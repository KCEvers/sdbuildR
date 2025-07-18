% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compile.R
\name{simulate}
\alias{simulate}
\title{Simulate stock-and-flow model}
\usage{
simulate(
  sfm,
  format_code = TRUE,
  keep_nonnegative_flow = TRUE,
  keep_nonnegative_stock = FALSE,
  keep_unit = TRUE,
  only_stocks = TRUE,
  verbose = FALSE,
  ...
)
}
\arguments{
\item{sfm}{Stock-and-flow model, object of class sdbuildR_xmile.}

\item{format_code}{If TRUE, format the R script with the styler package; only works if language is set to "R" in sim_specs. Defaults to TRUE.}

\item{keep_nonnegative_flow}{If TRUE, keeps original non-negativity setting of flows. Defaults to TRUE.}

\item{keep_nonnegative_stock}{If TRUE, keeps original non-negativity setting of stocks Defaults to TRUE.}

\item{keep_unit}{If TRUE, keeps units of variables. Defaults to TRUE.}

\item{only_stocks}{If TRUE, only save stocks. If FALSE, auxiliaries and flows are saved using a callback function. Only applies if language is set to "Julia" in sim_specs() and no delay functions are used. Defaults to FALSE.}

\item{verbose}{If TRUE, update on progress. Defaults to FALSE.}

\item{...}{Optional arguments}
}
\value{
Object of class sdbuildR_sim, which is a list containing:
\describe{
\item{df}{Dataframe, timeseries of computed variables in the ODE}
\item{init}{Initial value of stocks}
\item{constants}{Constant parameters}
\item{script}{Simulation script}
\item{duration}{Duration of simulation}
\item{success}{If TRUE, simulation was successful. If FALSE, simulation failed.}
\item{...}{Other parameters passed to simulate}
}
}
\description{
Simulate a stock-and-flow model with simulation specifications defined by \code{sim_specs()}. If not already run, the Julia environment will be set up with \code{use_julia()}. If any problems are detected by \code{debugger()}, the model cannot be simulated.
}
\examples{
sfm = xmile("SIR")
sim = simulate(sfm)
plot(sim)

# Obtain all model variables
sim = simulate(sfm, only_stocks = FALSE)
plot(sim, add_constants = TRUE)

# Use Julia for models with units or delay functions
sfm = xmile("coffee_cup") \%>\% sim_specs(language = "Julia")
use_julia()
sim = simulate(sfm)
plot(sim)

# Close Julia session
use_julia(stop = TRUE)

}
\seealso{
\code{\link[=build]{build()}}, \code{\link[=xmile]{xmile()}}, \code{\link[=debugger]{debugger()}}, \code{\link[=sim_specs]{sim_specs()}}, \code{\link[=use_julia]{use_julia()}}

Other simulate: 
\code{\link{ensemble}()},
\code{\link{plot.sdbuildR_ensemble}()},
\code{\link{plot.sdbuildR_sim}()},
\code{\link{sim_specs}()},
\code{\link{use_julia}()}
}
\concept{simulate}
