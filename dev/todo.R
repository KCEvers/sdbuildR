### Kyra Evers, University of Amsterdam, kyra.c.evers@gmail.com, 25.07.2024
#
# devtools::load_all(".")
# devtools::check(vignettes = F)
# devtools::install()
#
# library(dplyr)
# library(microbenchmark)
# library(profvis)
# # Set working directory
# # unit_df = readRDS("unit_df.RDS")
# filepath_base = dirname(rstudioapi::getActiveDocumentContext()$path)
# setwd(filepath_base)
# #regex_units = insightmakeR1::regex_units
# #regex_units_Julia = insightmakeR1::regex_units_Julia
# #source("globals.R")
# #source("func_detection.R")
# #source("past.R")
# #source("custom_func.R")
# #source("build_xmile.R")
# #source('insightmaker_conv.R')
# #source('insightmaker_conv_eqn.R')
# #source('Julia_conv_eqn.R')
# # source("compile.R")
# #source("compile_R.R")
# #source('compile_Julia.R')
#
# # Function parameters
# seed_number = 123 %>% round() %>% as.character()
# keep_nonnegative_flow = T
# keep_nonnegative_stock = F
# keep_unit = T
# keep_solver = F
# include_plot = FALSE
# debug = T
# filepath_script = NULL
# format_code = TRUE
# filepath_IM=NULL
# URL = NULL
# directory = "C:/Users/kevers1/Documents/PhD/insightmakeR1/validate/models"
# only_stocks = FALSE
# InsightMaker_version = 37
# verbose = TRUE
# regex_units = get_regex_units()
# name = "a"; type = "stock"; sfm = xmile(); var_names= get_model_var(sfm)
# format_label = TRUE; wrap_width = 25
#
# label = name
# new_name = NULL
# eqn = ""
# inflow = ""
# outflow = ""
# # Stock conveyor arguments
# conveyor = FALSE
# len = ""
# # leakage = .1, leakage_type = "linear", **to do
# to = ""
# from = ""
# units = "1"
# non_negative = FALSE
# min = NULL
# max = NULL
# xpts = NULL
# ypts = NULL
# interpolation = "linear"
# extrapolation = "nearest"
# doc = ""
#
# to do: ** #**[compat] in project.toml

# microbenchmark(simulate(template("SIR"), verbose = F),
#                simulate(template("predator-prey"), verbose = F),
#                simulate(template("Crielaard2022"), verbose = F))

# microbenchmark::microbenchmark(
#   simulate(template("SIR"), only_stocks = FALSE),
#   simulate(template("SIR"), only_stocks = TRUE),
#   simulate(template("Crielaard2022"), only_stocks = FALSE),
#   simulate(template("Crielaard2022"), only_stocks = TRUE),
#   times = 100
# )

# ** names of macros should als be protected names
# read hystersis macro name F
# no {} around functions
# **statements arent converted well

# ** nonnegativity of states in Julia:
#
#   cb2 = PositiveDomain()
#   callback = CallbackSet(callback,cb2)
#   but only for use with adaptive = false
#   ERROR: ArgumentError: domain callback can only be applied to adaptive algorithms
# Secondly, isoutofdomain doesn't work with adaptive=false either I think, because it rejects the timestep.

# ** to do: insight maker does make_pulse up to and including, so e.g. pulse starting at time 5 of width 1 goes up until 6, making the pulse have a width of 101 (with dt = .01). that's inaccurate, so don;t put that in my function but add +dt


# **to do: in insightmaker, at # at start_t_ramp, the signal is still 0,  but from me it already starts at start_h_ramp
# **to do: vectorized models https://insightmaker.com/docs/vectors, e.g. https://insightmaker.com/insight/4EQ4EUaFAZXRbs7nCoOAAd/North-America-Population-Growth-Model
# **to do: include all units https://github.com/scottfr/simulation/blob/main/src/formula/Units.js


# **to do: capacity

# **to do: recognize vectorized models "https://insightmaker.com/insight/cH7FKRLiz1ZvQpYuhGwDE/3-K-rper-Problem-mit-Reibung"


# **to do: make compatible with readsdr -> add function that writes script from readsdr output!


# **to do: keep_nonnegative_stock -> This cannot be fully reproduced with rk4 because InsightMaker applies the non-negativity correction after all rk4 steps, and we don't have access to that point with deSolve. The non-negativity correction is applied using "modifier" in the  apply(timeChange, oldTime) function
# https://github.com/scottfr/simulation/blob/main/src/Primitives.js
# doRK4Aggregation() {
#   this.blendedRate = div(plus(plus(plus(this.RKPrimary[0], mult(new Material(2), this.RKPrimary[1])), mult(new Material(2), this.RKPrimary[2])), this.RKPrimary[3]), new Material(6));
#
#   this.blendedRate = this.checkRate(this.blendedRate);
# }

# https://github.com/scottfr/simulation/blob/main/src/Primitives.js
# in apply() function:
#   if (this.omega !== null && this.omega.dna.nonNegative) {
#     let modifier;
#     try {
#       // Omega + Flow: value(s) will be negative if we need to make
#       // an adjustment to maintain non-negativity
#       modifier = plus(toNum(this.omega.level), rate);

# **nonnegative stocks are not done correctly, need to adjust outflow, see story of ED,
# triage.and.register both affects dPre.Triage and dDelay.for.Area..or.EDMO and because pre.triage turns negative it needs to be adjusted

# **converters have inverse units of their inputs? could make a function that returns it like that
# URL = "https://insightmaker.com/insight/2k4n9nSLWwZ2faP9DgUbnS/Clone-of-Z602-Population-with-four-age-groups"


# **asteroid different digits pi. from simulation:
# simulate.varBank.set("e", new Material(2.71828182845904523536));
# simulate.varBank.set("pi", new Material(3.14159265358979323846264338));
# simulate.varBank.set("phi", new Material(1.61803399))
# PI = 3.14159265358979323846264338
# from java sim: 3.14159265358979
# in R: 3.141592653589793115998



# # option 1 (then you should also put utils in Imports)
# utils::globalVariables(c("P", "regex_units"))

# ** constraints in Julia

# optional:
# **to do: check for doubles (e.g. step or delay functions)
# **to do: remove unused model elements as an option
# It shouldn't be about connected elements, but whether parameters/stocks/flows are being used in the ODE...
# include_unconnected_elements

# **notes on julia setup in diffeqr
# https://github.com/SciML/diffeqr/blob/master/R/diffeqr.R
# in julia_pkg_import() and environment is created, and then all DifferentialEquations.jl functions are assigned to that environment. That's how de$ODEProblem() works.

# ** creating a system image in Julia with PackageCompiler
# https://julialang.github.io/PackageCompiler.jl/stable/sysimages.html

# ** parallel computing in julia
# EnsembleProblem()
# https://docs.sciml.ai/DiffEqDocs/dev/features/ensemble/

# **sensitivity analysis
# https://github.com/SciML/GlobalSensitivity.jl

# "C:\\Users\\kevers1\\.julia\\juliaup\\julia-1.11.3+0.x64.w64.mingw32\\bin"
# Package creation

# # Declare dependencies
# pkgs = c('dplyr', 'igraph', 'magrittr', 'purrr', 'stringr', 'textutils', 'tibble', 'tidyr', 'rlang', 'rstudioapi', 'stringi', 'rvest', 'xml2', 'gtools', 'units', 'fs', "styler", "data.table", "DiagrammeR", "deSolve", "viridis", "ggplot2", "JuliaCall")
# for (p in pkgs){
#   usethis::use_package(p)
# }
#
# Use pipe operator
# usethis::use_pipe(export = TRUE)
#
#
# to print diagrammr in pdf output:
# install.packages('webshot')
# webshot::install_phantomjs()
# Show deprecation warnings as soon as they happen, not every 8 hours
# https://forum.posit.co/t/this-warning-is-displayed-once-every-8-hours-how-to-not-wait-8-hours-to-verify-fix/75135/2
# # Set lifecycle_verbosity to 'warning', ensuring deprecated messages are shown.
# options(lifecycle_verbosity = "warning")
#
# # Deprecation signal is given as a warning, even within 8 hours
# lifecycle::deprecate_soft("0.1.0", "deprecated_function()")
# #> Warning: `deprecated_function()` is deprecated as of lifecycle 0.1.0.


# rjtools::create_article(
#   name = "sdbuildR_paper",
#   create_dir = TRUE)

# usethis::use_testthat()
# usethis::use_test("apa_t_pair") # create file
# test_that("defaults", {
#   x <- c(1,2,3,4,5)
#   y <- c(2,3,2,5,6)
#
#   result <- apa_t_pair(x, y)
#   expected <- "A paired-samples t-test was conducted to compare the DV between level 1 (M = 3.0, SD = 1.6) and level 2 (M = 3.6, SD = 1.8). There was a non-significant difference; t(4) = -1.50, p = 0.208."
#   expect_equal(result, expected)
# })
# devtools::test()


#**find way to build in scenarios, like Wang's model

# **to do: aliases for model_units

# **debugger # **add stop() in debugger() - if no initial value is specified by the delayN depends on an aux or flow, initial value must be specified to ensure no dependencies of static on dynamic variables
# **to do: debugger detect undefined functions
# ** to do: debugger use of gf without source in equations

# https://www.iseesystems.com/resources/help/v3/Content/08-Reference/07-Builtins/Delay_builtins.html
# Note: The dynamic behavior of SMTHN is identical to DELAYN, except when the averaging time changes. SMTHN is conceptually a stock, and DELAYN is conceptually a flow.
#
# Note: If n is large, and the time delay is small, the intermediate transition times between the stocks can become smaller than DT. To prevent unstable behavior in this situation, the length of the delay will be increased to n*DT. Normally, n should be much smaller than delay_duration/DT.
# **check order rounds to integer

# ** Dimensional consistency


# ** to do: verbal rmarkdown equations
# ** to do: document() like vensim

# **get rid of replacement_func
# **check whether aux are constants, may show lack of understanding or model misspecification if they expect somehting to change




