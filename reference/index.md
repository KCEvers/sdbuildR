# Package index

## Building stock-and-flow models

- [`as.data.frame(`*`<sdbuildR_sim>`*`)`](https://kcevers.github.io/sdbuildR/reference/as.data.frame.sdbuildR_sim.md)
  : Create data frame of simulation results
- [`as.data.frame(`*`<sdbuildR_xmile>`*`)`](https://kcevers.github.io/sdbuildR/reference/as.data.frame.sdbuildR_xmile.md)
  : Convert stock-and-flow model to data frame
- [`build()`](https://kcevers.github.io/sdbuildR/reference/build.md) :
  Create, modify or remove variables
- [`debugger()`](https://kcevers.github.io/sdbuildR/reference/debugger.md)
  : Debug stock-and-flow model
- [`find_dependencies()`](https://kcevers.github.io/sdbuildR/reference/find_dependencies.md)
  : Find dependencies
- [`get_build_code()`](https://kcevers.github.io/sdbuildR/reference/get_build_code.md)
  : Generate code to build stock-and-flow model
- [`header()`](https://kcevers.github.io/sdbuildR/reference/header.md) :
  Modify header of stock-and-flow model
- [`macro()`](https://kcevers.github.io/sdbuildR/reference/macro.md) :
  Create, modify or remove a global variable or function
- [`plot(`*`<sdbuildR_xmile>`*`)`](https://kcevers.github.io/sdbuildR/reference/plot.sdbuildR_xmile.md)
  : Plot stock-and-flow diagram
- [`print(`*`<summary.sdbuildR_xmile>`*`)`](https://kcevers.github.io/sdbuildR/reference/print.summary.sdbuildR_xmile.md)
  : Print method for summary.sdbuildR_xmile
- [`summary(`*`<sdbuildR_xmile>`*`)`](https://kcevers.github.io/sdbuildR/reference/summary.sdbuildR_xmile.md)
  : Print overview of stock-and-flow model
- [`xmile()`](https://kcevers.github.io/sdbuildR/reference/xmile.md) :
  Create a new stock-and-flow model

## Simulate stock-and-flow models

- [`ensemble()`](https://kcevers.github.io/sdbuildR/reference/ensemble.md)
  : Run ensemble simulations
- [`export_plot()`](https://kcevers.github.io/sdbuildR/reference/export_plot.md)
  : Save plot to a file
- [`plot(`*`<sdbuildR_ensemble>`*`)`](https://kcevers.github.io/sdbuildR/reference/plot.sdbuildR_ensemble.md)
  : Plot timeseries of ensemble
- [`plot(`*`<sdbuildR_sim>`*`)`](https://kcevers.github.io/sdbuildR/reference/plot.sdbuildR_sim.md)
  : Plot timeseries of simulation
- [`sim_specs()`](https://kcevers.github.io/sdbuildR/reference/sim_specs.md)
  : Modify simulation specifications
- [`simulate()`](https://kcevers.github.io/sdbuildR/reference/simulate.md)
  : Simulate stock-and-flow model
- [`solvers()`](https://kcevers.github.io/sdbuildR/reference/solvers.md)
  : Check or translate between deSolve and Julia DifferentialEquations
  solvers

## Set-up and use Julia

- [`install_julia_env()`](https://kcevers.github.io/sdbuildR/reference/install_julia_env.md)
  : Install, update, or remove Julia environment
- [`julia_status()`](https://kcevers.github.io/sdbuildR/reference/julia_status.md)
  : Check status of Julia installation and environment
- [`use_julia()`](https://kcevers.github.io/sdbuildR/reference/use_julia.md)
  : Start Julia and activate environment
- [`use_threads()`](https://kcevers.github.io/sdbuildR/reference/use_threads.md)
  : Set up threaded ensemble simulations

## Insight Maker

- [`insightmaker_to_sfm()`](https://kcevers.github.io/sdbuildR/reference/insightmaker_to_sfm.md)
  : Import Insight Maker model
- [`url_to_IM()`](https://kcevers.github.io/sdbuildR/reference/url_to_IM.md)
  : Extract Insight Maker model from URL

## Input functions

- [`pulse()`](https://kcevers.github.io/sdbuildR/reference/pulse.md) :
  Create pulse function
- [`ramp()`](https://kcevers.github.io/sdbuildR/reference/ramp.md) :
  Create ramp function
- [`seasonal()`](https://kcevers.github.io/sdbuildR/reference/seasonal.md)
  : Create a seasonal wave function
- [`step()`](https://kcevers.github.io/sdbuildR/reference/step.md) :
  Create step function

## Custom functions

- [`contains_IM()`](https://kcevers.github.io/sdbuildR/reference/contains_IM.md)
  : Check if needle is in haystack
- [`expit()`](https://kcevers.github.io/sdbuildR/reference/expit.md) :
  Expit function
- [`indexof()`](https://kcevers.github.io/sdbuildR/reference/indexof.md)
  : Find index of needle in haystack
- [`length_IM()`](https://kcevers.github.io/sdbuildR/reference/length_IM.md)
  : Length of vector or string
- [`logistic()`](https://kcevers.github.io/sdbuildR/reference/logistic.md)
  [`sigmoid()`](https://kcevers.github.io/sdbuildR/reference/logistic.md)
  : Logistic function
- [`logit()`](https://kcevers.github.io/sdbuildR/reference/logit.md) :
  Logit function
- [`rbool()`](https://kcevers.github.io/sdbuildR/reference/rbool.md) :
  Generate random logical value
- [`rdist()`](https://kcevers.github.io/sdbuildR/reference/rdist.md) :
  Generate random number from custom distribution
- [`rem()`](https://kcevers.github.io/sdbuildR/reference/rem_mod.md)
  [`mod()`](https://kcevers.github.io/sdbuildR/reference/rem_mod.md)
  [`` `%REM%` ``](https://kcevers.github.io/sdbuildR/reference/rem_mod.md)
  : Remainder and modulus
- [`round_IM()`](https://kcevers.github.io/sdbuildR/reference/round_IM.md)
  : Round Half-Up (as in Insight Maker)

## Delay functions

## Units

- [`convert_u()`](https://kcevers.github.io/sdbuildR/reference/convert_u.md)
  : Convert unit in equation
- [`drop_u()`](https://kcevers.github.io/sdbuildR/reference/drop_u.md) :
  Drop unit in equation
- [`get_regex_time_units()`](https://kcevers.github.io/sdbuildR/reference/get_regex_time_units.md)
  : Get regular expressions for time units in Julia
- [`get_regex_units()`](https://kcevers.github.io/sdbuildR/reference/get_regex_units.md)
  : Get regular expressions for units in Julia
- [`get_units()`](https://kcevers.github.io/sdbuildR/reference/get_units.md)
  : View all standard units
- [`model_units()`](https://kcevers.github.io/sdbuildR/reference/model_units.md)
  : Create, modify or remove custom units
- [`u()`](https://kcevers.github.io/sdbuildR/reference/u.md) : Specify
  unit in equations
- [`unit_prefixes()`](https://kcevers.github.io/sdbuildR/reference/unit_prefixes.md)
  : Show unit prefixes

## Internal functions

- [`clean_name()`](https://kcevers.github.io/sdbuildR/reference/clean_name.md)
  : Clean variable name(s)
- [`has_internet()`](https://kcevers.github.io/sdbuildR/reference/has_internet.md)
  : Check if user has internet
- [`nonnegative()`](https://kcevers.github.io/sdbuildR/reference/nonnegative.md)
  : Safely check whether x is less than zero
- [`saveat_func()`](https://kcevers.github.io/sdbuildR/reference/saveat_func.md)
  : Internal function to save data frame at specific times
