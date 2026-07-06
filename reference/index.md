# Package index

## Building stock-and-flow models

- [`as.data.frame(`*`<simulate_stockflow>`*`)`](https://kcevers.github.io/sdbuildR/reference/as.data.frame.simulate_stockflow.md)
  : Create data frame of simulation results
- [`as.data.frame(`*`<stockflow>`*`)`](https://kcevers.github.io/sdbuildR/reference/as.data.frame.stockflow.md)
  : Convert stock-and-flow model to data frame
- [`auxiliary()`](https://kcevers.github.io/sdbuildR/reference/auxiliary.md)
  [`aux()`](https://kcevers.github.io/sdbuildR/reference/auxiliary.md) :
  Add or modify auxiliaries
- [`change_name()`](https://kcevers.github.io/sdbuildR/reference/change_name.md)
  : Change name of variable
- [`change_type()`](https://kcevers.github.io/sdbuildR/reference/change_type.md)
  : Change variable type
- [`compare_models()`](https://kcevers.github.io/sdbuildR/reference/compare_models.md)
  : Compare two stock-and-flow models
- [`constant()`](https://kcevers.github.io/sdbuildR/reference/constant.md)
  : Add or modify constants
- [`custom_func()`](https://kcevers.github.io/sdbuildR/reference/custom_func.md)
  : Create or modify custom variables or functions
- [`dependencies()`](https://kcevers.github.io/sdbuildR/reference/dependencies.md)
  : Find dependencies
- [`discard()`](https://kcevers.github.io/sdbuildR/reference/discard.md)
  : Remove variable(s)
- [`flow()`](https://kcevers.github.io/sdbuildR/reference/flow.md) : Add
  or modify flows
- [`lookup()`](https://kcevers.github.io/sdbuildR/reference/lookup.md) :
  Add or modify lookup variables (graphical functions)
- [`meta()`](https://kcevers.github.io/sdbuildR/reference/meta.md) :
  Modify meta of stock-and-flow model
- [`plot(`*`<stockflow>`*`)`](https://kcevers.github.io/sdbuildR/reference/plot.stockflow.md)
  : Plot stock-and-flow diagram
- [`print(`*`<compare_stockflow>`*`)`](https://kcevers.github.io/sdbuildR/reference/print.compare_stockflow.md)
  : Print comparison of two stock-and-flow models
- [`print(`*`<simulate_stockflow>`*`)`](https://kcevers.github.io/sdbuildR/reference/print.simulate_stockflow.md)
  : Print simulation of a stock-and-flow model
- [`print(`*`<stockflow>`*`)`](https://kcevers.github.io/sdbuildR/reference/print.stockflow.md)
  : Print overview of stock-and-flow model
- [`print(`*`<summary_stockflow>`*`)`](https://kcevers.github.io/sdbuildR/reference/print.summary_stockflow.md)
  : Print method for summary_stockflow
- [`stock()`](https://kcevers.github.io/sdbuildR/reference/stock.md) :
  Add or modify stocks
- [`stockflow()`](https://kcevers.github.io/sdbuildR/reference/stockflow.md)
  : Create a new stock-and-flow model
- [`summary(`*`<stockflow>`*`)`](https://kcevers.github.io/sdbuildR/reference/summary.stockflow.md)
  : Run model diagnostics
- [`update(`*`<stockflow>`*`)`](https://kcevers.github.io/sdbuildR/reference/update.stockflow.md)
  : Create or modify variables

## Simulate stock-and-flow models

- [`head(`*`<simulate_stockflow>`*`)`](https://kcevers.github.io/sdbuildR/reference/head.simulate_stockflow.md)
  : Print first rows of a simulation
- [`plot(`*`<simulate_stockflow>`*`)`](https://kcevers.github.io/sdbuildR/reference/plot.simulate_stockflow.md)
  : Plot timeseries of simulation
- [`sim_methods()`](https://kcevers.github.io/sdbuildR/reference/sim_methods.md)
  : Translate between deSolve and DifferentialEquations.jl solver names
- [`sim_settings()`](https://kcevers.github.io/sdbuildR/reference/sim_settings.md)
  : Modify simulation specifications
- [`simulate(`*`<stockflow>`*`)`](https://kcevers.github.io/sdbuildR/reference/simulate.stockflow.md)
  : Simulate stock-and-flow model
- [`summary(`*`<simulate_stockflow>`*`)`](https://kcevers.github.io/sdbuildR/reference/summary.simulate_stockflow.md)
  : Summarize simulation results
- [`tail(`*`<simulate_stockflow>`*`)`](https://kcevers.github.io/sdbuildR/reference/tail.simulate_stockflow.md)
  : Print last rows of a simulation

## Ensemble simulations

- [`as.data.frame(`*`<ensemble_stockflow>`*`)`](https://kcevers.github.io/sdbuildR/reference/as.data.frame.ensemble_stockflow.md)
  : Create data frame of simulation results
- [`ensemble()`](https://kcevers.github.io/sdbuildR/reference/ensemble.md)
  : Run ensemble simulation
- [`plot(`*`<ensemble_stockflow>`*`)`](https://kcevers.github.io/sdbuildR/reference/plot.ensemble_stockflow.md)
  : Plot timeseries of ensemble simulation

## Unit testing

- [`as.data.frame(`*`<verify_stockflow>`*`)`](https://kcevers.github.io/sdbuildR/reference/as.data.frame.verify_stockflow.md)
  : Convert verify() results to a data frame
- [`discard_unit_test()`](https://kcevers.github.io/sdbuildR/reference/discard_unit_test.md)
  : Remove a unit test from a stock-and-flow model
- [`head(`*`<verify_stockflow>`*`)`](https://kcevers.github.io/sdbuildR/reference/head.verify_stockflow.md)
  : Print first rows of verify results
- [`plot(`*`<verify_stockflow>`*`)`](https://kcevers.github.io/sdbuildR/reference/plot.verify_stockflow.md)
  : Plot verify results
- [`tail(`*`<verify_stockflow>`*`)`](https://kcevers.github.io/sdbuildR/reference/tail.verify_stockflow.md)
  : Print last rows of verify results
- [`unit_test()`](https://kcevers.github.io/sdbuildR/reference/unit_test.md)
  : Add or modify unit tests
- [`unit_tests()`](https://kcevers.github.io/sdbuildR/reference/unit_tests.md)
  : Display unit tests defined on a stock-and-flow model
- [`verify()`](https://kcevers.github.io/sdbuildR/reference/verify.md) :
  Verify model behavior with unit tests
- [`verify(`*`<stockflow>`*`)`](https://kcevers.github.io/sdbuildR/reference/verify.stockflow.md)
  : Verify unit tests against simulation results

## Set-up and use Julia

- [`install_julia_env()`](https://kcevers.github.io/sdbuildR/reference/install_julia_env.md)
  : Install, update, or remove Julia environment
- [`use_julia()`](https://kcevers.github.io/sdbuildR/reference/use_julia.md)
  : Start Julia and activate environment

## Importing and exporting models

- [`export_model()`](https://kcevers.github.io/sdbuildR/reference/export_model.md)
  : Export a stock-and-flow model
- [`import_desolve()`](https://kcevers.github.io/sdbuildR/reference/import_desolve.md)
  : Import a deSolve model
- [`import_insightmaker()`](https://kcevers.github.io/sdbuildR/reference/import_insightmaker.md)
  : Import Insight Maker model
- [`insightmaker_to_json()`](https://kcevers.github.io/sdbuildR/reference/insightmaker_to_json.md)
  : Convert .InsightMaker file to .json file
- [`url_to_insightmaker()`](https://kcevers.github.io/sdbuildR/reference/url_to_insightmaker.md)
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

## Convenience functions

- [`contains_IM()`](https://kcevers.github.io/sdbuildR/reference/contains_IM.md)
  : Check whether value is in vector or string
- [`expit()`](https://kcevers.github.io/sdbuildR/reference/expit.md) :
  Expit function
- [`export_plot()`](https://kcevers.github.io/sdbuildR/reference/export_plot.md)
  : Save plot to a file
- [`hill()`](https://kcevers.github.io/sdbuildR/reference/hill.md) :
  Hill function
- [`indexof()`](https://kcevers.github.io/sdbuildR/reference/indexof.md)
  : Find index of value in vector or string
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
- [`ricker()`](https://kcevers.github.io/sdbuildR/reference/ricker.md) :
  Generalized Ricker function
- [`round_IM()`](https://kcevers.github.io/sdbuildR/reference/round_IM.md)
  : Round values half-up (as in Insight Maker)

## Internal functions

- [`clean_name()`](https://kcevers.github.io/sdbuildR/reference/clean_name.md)
  : Clean variable name(s)
- [`has_internet()`](https://kcevers.github.io/sdbuildR/reference/has_internet.md)
  : Check if user has internet
- [`nonnegative()`](https://kcevers.github.io/sdbuildR/reference/nonnegative.md)
  : Check whether x is less than zero
- [`saveat_func()`](https://kcevers.github.io/sdbuildR/reference/saveat_func.md)
  : Internal function to save data frame at specific times
