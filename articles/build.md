# Building stock-and-flow models

``` r
library(sdbuildR)
```

Stock-and-flow models represent systems as states (stocks) that
accumulate over time with processes (flows) that change these variables.
Foundational to the field of system dynamics, stock-and-flow models are
widely used in areas such as epidemiology, economics, and environmental
science to model systems complicated by nonlinearities, delays, and
feedback loops. In this vignette, we will explore how to build
stock-and-flow models from scratch using sdbuildR. We will first go
through the basics: What are the building blocks of a stock-and-flow
model? How can we combine them to create a model of our system? To give
a quick overview of how sdbuildR works, we will recreate a well-known
predator-prey model. We will then develop a model of our own to show how
sdbuildR facilitates iterative model development.

## Stock-and-flow models

Stock-and-flow models are composed of stocks, which are the variables
that determine the state of the system, and flows, which are the way
stocks change. Stocks accumulate material or information over time, such
as people or products. Stocks are variables that can increase and
decrease, and can be measured at a single moment in time. Despite what
their name may imply, stocks need not be tangible, and can also be
variables such as beliefs, feelings, or knowledge.

The value of a stock is increased or decreased by flows. Flows represent
the rates at which stocks change, measured in units per time (e.g.,
people per year, dollars per month). Flows transport whatever is in the
stock, and have to come and go somewhere. The source and sink of flows
may be another stock, or may be left unspecified as outside the model
boundary. For instance, in a population model, births are an inflow, and
deaths are an outflow. Usually, we don’t specify where the outflow of
deaths is going to, meaning it is outside of the model boundary.

The net change in a stock is the sum of inflows minus outflows. A stock
may have multiple inflows and multiple outflows. This means you can
separate different processes that affect your stock. If two stocks are
connected by a flow, the flow directly takes the material from the first
stock to the second stock (i.e. a material flow). For example, in the
Susceptible-Infected-Recovered model, susceptible individuals move to
the infected stock through the flow of infection, and infected
individuals move to the recovered stock through the flow of recovery.

To help build a stock-and-flow model, you can also use three other types
of variables: constants, auxiliaries, and graphical functions. Constants
are variables that do not change over the course of the simulation -
they are time-independent. Think of fixed birth rates, time constants,
or thresholds. Conversely, auxiliaries are dynamic, and help as
intermediate computations for flows. To create custom interpolation
functions, use graphical functions (also called converters, or table or
lookup functions).

| Building Block     | Purpose                                                                         | Example                                                                     |
|:-------------------|:--------------------------------------------------------------------------------|:----------------------------------------------------------------------------|
| Stock              | Defines the state of the system                                                 | People, products, beliefs, blood sugar, CO2 in atmosphere                   |
| Flow               | Moves material or information to and from stocks and outside the model boundary | Birth rates, product order rates, eating, revenue, sales                    |
| Constant           | Specifies static parameters                                                     | Fixed prices, constants, coefficients, parameters                           |
| Auxiliary          | Computes intermediate computations dynamically in the model                     | Sum of all Stocks, ratios of flows, averages                                |
| Graphical Function | Allows custom input-output interpolation functions                              | Interventions or events occurring at known times such as vaccines or storms |

Stock-and-flow building blocks

## A Simple Example: Predator-Prey

To create a stock-and-flow model from scratch, we first create an XMILE
object using
[`xmile()`](https://kcevers.github.io/sdbuildR/reference/xmile.md),
which yields a nested list in the structure of the XMILE standard. This
model is still empty, but has default simulation specifications. We add
a model name with
[`header()`](https://kcevers.github.io/sdbuildR/reference/header.md).

``` r
sfm <- xmile() |> header(name = "Predator-Prey Model")
summary(sfm)
#> Your model contains:
#> * 0 Stocks
#> * 0 Flows
#> * 0 Constants
#> * 0 Auxiliaries
#> * 0 Graphical Functions
#> * 0 Custom model units
#> * 0 Macros
#> 
#> Simulation time: 0.0 to 100.0 seconds (dt = 0.01)
#> Simulation settings: solver euler in R
```

We add two stocks, predators and prey.
[`build()`](https://kcevers.github.io/sdbuildR/reference/build.md) takes
the variable name, building block type (`stock`, `flow`, `constant`,
`aux`, or `gf`), and type-specific properties (e.g., `eqn` for equation,
`label` for plotting). Here, we start the simulation with 10 predators
and 50 prey. We use the pipe operator `|>` to pass the result of an
expression to the next expression as a first argument.

``` r
sfm <- build(sfm, "predator", "stock", eqn = 10, label = "Predator") |>
  build("prey", "stock", eqn = 50, label = "Prey")
```

How does this model evolve over time?

``` r
sim <- simulate(sfm)
plot(sim)
```

The stocks are static! Stocks can only change through flows, and we have
not yet added any flows to the model. We could also have noticed this by
running
[`debugger()`](https://kcevers.github.io/sdbuildR/reference/debugger.md):

``` r
debugger(sfm)
#> No problems detected!
#> Potentially problematic:
#> * Your model has no flows.
```

We create four flows: the births and deaths of both the predators and
prey. The rate of the flow is indicated in `eqn`. In addition, flows
have to be connected to at least one stock: a stock the flow is coming
from (`from`) and/or a stock the flow is flowing to (`to`). Inflows add
to the stock, and outflows subtract from the stock.

``` r
sfm <- build(sfm, "predator_births", "flow",
  eqn = "delta*prey*predator",
  label = "Births", to = "predator"
) |>
  build("predator_deaths", "flow",
    eqn = "gamma*predator",
    label = "Deaths", from = "predator"
  ) |>
  build("prey_births", "flow",
    eqn = "alpha*prey",
    label = "Births", to = "prey"
  ) |>
  build("prey_deaths", "flow",
    eqn = "beta*prey*predator",
    label = "Deaths", from = "prey"
  )
plot(sfm)
```

The flows make use of four other variables: `delta`, `gamma`, `alpha`,
and `beta`. Below, we add these constants in a vectorized manner for
efficiency. In addition, we document the meaning of these variables in
the optional `doc` property.

``` r
sfm <- build(sfm, c("delta", "gamma", "alpha", "beta"), "constant",
  eqn = c(.025, .5, .5, .05),
  label = c("Delta", "Gamma", "Alpha", "Beta"),
  doc = c(
    "Birth rate of predators", "Death rate of predators",
    "Birth rate of prey", "Death rate of prey by predators"
  )
)
```

We now have a complete predator-prey model, ready to be simulated!

``` r
sim <- simulate(sfm)
plot(sim)
```

The model shows classic predator-prey dynamics, with oscillations in the
populations of both species. As the number of prey rises, the population
of predators grows, consuming the prey. With fewer prey, the larger
number of predators don’t have enough to eat, such that the predators
start to dwindle. The population of prey can grow again, leading to
another cycle.

## Building models in practice: The art of iteration

Of course, in reality, creating a stock-and-flow model isn’t a one-shot
process. The variables, connections, and equations will have to be
experimented with and tweaked to accurately represent the system’s
behavior. Let’s explore how to iteratively build stock-and-flow models
by creating a simplified model of burnout, inspired by [classic system
dynamics
models](https://ocw.mit.edu/courses/15-988-system-dynamics-self-study-fall-1998-spring-1999/e8bd0c07ef2848b39e55fc8ff52dcb88_generic3.pdf).

Let’s say Maya has just switched from a part-time to a full-time job,
and she wants to impress her supervisor. She is working hard and putting
in extra hours. However, the more she works, the more projects,
meetings, and emails pile on - work breeds work. This comes at the cost
of her sleep, which she can only sustain for a limited time. Maya ends
up working far beyond her capacity, to the point where she is unable to
work at all.

As a first step in creating this stock-and-flow model, let’s start by
setting up our model. We change the name, and specify that we want to
see Maya’s burnout develop over the course of 12 months:

``` r
sfm <- xmile() |>
  header(name = "Maya's Burnout") |>
  sim_specs(stop = 12, time_units = "month")
```

We add a stock for Maya’s work, which is the number of hours she works
per day. She starts from 4 hours per day, her part-time job. We add a
flow that increases Maya’s daily work hours as a function of the work
she’s doing and a multiplier.

``` r
sfm <- build(sfm, "workload", "stock", eqn = 4) |>
  build("new_tasks", "flow", eqn = "workload * work_growth", to = "workload") |>
  build("work_growth", "constant", eqn = .1)

plot(sfm)
```

``` r
sim <- simulate(sfm)
plot(sim)
```

Maya’s work day is increasing exponentially! We’re clearly missing
something. We’ve only added an inflow, but no outflow yet. In order to
keep Maya’s work hours in check, we need to add a balancing feedback
loop, so that her work hours can also decrease. Maya tries to adjust her
hours to the normal workday, which is 8 hours. We add a new constant and
an outflow:

``` r
sfm <- build(sfm, "normal_workday", "constant", eqn = 8) |>
  build("back_to_normal", "flow", eqn = "workload - normal_workday", from = "workload")

sim <- simulate(sfm)
plot(sim, add_constants = TRUE)
```

Despite adding a balancing feedback loop, Maya’s daily work hours settle
above the normal work day, reflecting the interplay of the reinforcing
and balancing feedback loop.

Let’s now add sleep to our model. We create a stock for the daily hours
of sleep Maya is getting and a constant for the sleep she needs. To
specify that the more she works, the less Maya sleeps, we add a outflow
from sleep that depends on how much she is working and a multiplier.

``` r
sfm <- build(sfm, "sleep", "stock", eqn = "necessary_sleep") |>
  build("necessary_sleep", "constant", eqn = 8) |>
  build("worry_about_work", "flow",
    eqn = "workload * worry_factor", from = "sleep"
  ) |>
  build("worry_factor", "constant", eqn = .1)

plot(sfm)
```

The model now contains two stocks and three flows.

``` r
sim <- simulate(sfm)
plot(sim)
```

Maya’s sleep is decreasing, but we haven’t yet added the effect of her
sleep deficit on how much she is working. The further Maya’s sleep is
from what she needs, the more she tries to reduce her work. We change
the outflow from work to increase the less she sleeps, and increase the
amount of work that piles on, for good measure.

``` r
sfm <- build(sfm, "back_to_normal",
  change_name = "need_for_rest",
  eqn = "workload * necessary_sleep / sleep"
) |>
  build("work_growth", eqn = 1.5)
```

``` r
sim <- simulate(sfm)
plot(sim)
```

We’ve now created burnout in our model: Maya’s work hours increase far
beyond what is sustainable, to the point where she stops working
completely. This is a classic example of overshoot and collapse, in
which tasks beyond an individual’s capacity deplete their resources,
making the behaviour unsustainable in the long-term.

But what if rather than only decreasing, Maya’s daily sleep can also
recover? To do so, we change the outflow from sleep to not always
increase with the hours worked per day, but to only increase when the
hours worked per day exceed a normal work day. When the hours worked per
day are less than a normal work day, Maya’s sleep will improve. What
will this look like?

``` r
sfm <- build(sfm, "worry_about_work",
  eqn = "(workload - normal_workday) * worry_factor"
) |>
  build("worry_factor", eqn = .5)

sim <- simulate(sfm)
plot(sim, add_constants = TRUE)
```

We’ve created oscillations in the system! Maya is now trapped in a cycle
of overworking, which causes her sleep to deteriorate to the point of
collapse. As she takes more time off work, her sleep recovers, but this
only starts the cycle anew. What’s worse, the oscillations are growing
in amplitude, with each cycle becoming more extreme. Of course, the
model becomes untenable beyond this time horizon, as Maya would work
more than 24 hours a day.

## Overview of package functions

| Function                                                                                       | Purpose                                           |
|:-----------------------------------------------------------------------------------------------|:--------------------------------------------------|
| [`xmile()`](https://kcevers.github.io/sdbuildR/reference/xmile.md)                             | Create object of class xmile or load template     |
| [`insightmaker_to_sfm()`](https://kcevers.github.io/sdbuildR/reference/insightmaker_to_sfm.md) | Import Insight Maker model                        |
| [`build()`](https://kcevers.github.io/sdbuildR/reference/build.md)                             | Create, modify, or remove variable from the model |
| [`simulate()`](https://kcevers.github.io/sdbuildR/reference/simulate.md)                       | Simulate model                                    |
| [`plot()`](https://rdrr.io/r/graphics/plot.default.html)                                       | Plot model diagram or simulation                  |
| [`summary()`](https://rdrr.io/r/base/summary.html)                                             | Print summary of building blocks and variables    |
| [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)                                 | Get model properties in a dataframe               |
| [`debugger()`](https://kcevers.github.io/sdbuildR/reference/debugger.md)                       | Find problems in your model formulation           |
| [`get_build_code()`](https://kcevers.github.io/sdbuildR/reference/get_build_code.md)           | Get code to build model from scratch              |
| [`sim_specs()`](https://kcevers.github.io/sdbuildR/reference/sim_specs.md)                     | Modify simulation specifications                  |
| [`model_units()`](https://kcevers.github.io/sdbuildR/reference/model_units.md)                 | Create, modify, or remove custom units            |
| [`macro()`](https://kcevers.github.io/sdbuildR/reference/macro.md)                             | Create, modify, or remove global variables        |
| [`header()`](https://kcevers.github.io/sdbuildR/reference/header.md)                           | Modify model header                               |

Main functions in *sdbuildR*

| Variable | Description                  | Use case                                           |
|:---------|:-----------------------------|:---------------------------------------------------|
| `times`  | Vector with simulation times | Use in equations, e.g. pulse(times, 5, width = dt) |
| `t`      | Current time in the ODE      | Use in equations, e.g. input(t)                    |
| `dt`     | Time step of the simulation  | Use in equations, e.g. pulse(times, 5, width = dt) |

Global variables in *sdbuildR*

### Simulation specifications

We may want to observe the system over a longer time period, or with a
different time step. To change the simulation specifications, use
[`sim_specs()`](https://kcevers.github.io/sdbuildR/reference/sim_specs.md).
Below, we simulate from 0 to 250 days. To decrease the size of the
resulting dataframe, the simulation time step `dt` should *not* be
lowered, as this can affect the accuracy of the simulation. Rather, we
can save the simulation results only every 0.1 days, which preserves
accuracy. We additionally don’t save the first 100 days. The method is
set to `"euler"`, which is the simplest numerical integration method.
For more complex models or when higher accuracy is needed, consider
other methods like `"rk4"`. See \[`?solvers()`\] for available methods.

``` r
sfm <- xmile("predator_prey") |>
  sim_specs(
    time_units = "days",
    start = 0,
    stop = 250,
    method = "euler",
    dt = 0.001,
    save_at = 0.1,
    save_from = 100
  )
```

In case the simulation contains stochastic elements, we may want to set
a seed to ensure that the simulation is reproducible. For example, the
initial value of the predator and prey stocks could be a random number.
The seed needs to be an integer, and can also be set with
[`sim_specs()`](https://kcevers.github.io/sdbuildR/reference/sim_specs.md).

``` r
sfm <- build(sfm, c("predator", "prey"), eqn = "runif(1, 20, 50)") |>
  sim_specs(seed = 1)
```

In case a seed was set, this will appear in
[`summary.sdbuildR_xmile()`](https://kcevers.github.io/sdbuildR/reference/summary.sdbuildR_xmile.md).
The seed can also be removed to ensure variation in the simulation. This
can be useful to for example test the sensitivity of the model to
changes in the initial condition.

``` r
sfm <- sim_specs(sfm, seed = NULL)
```

### Documenting

To document meta-properties of the model, use
[`header()`](https://kcevers.github.io/sdbuildR/reference/header.md).
Some standard XMILE properties specified in the header are `name`,
`caption`, `created` (recording the date it was created), `author`
(recording the creator of the model), and `version`. In addition, any
other properties useful for documenting the model may be added, such as
`URL` or `doi`.

``` r
sfm <- header(sfm, author = "Kyra Evers", affiliation = "University of Amsterdam")
```

### Deconstructing a model

To understand how a model was built, use
[`get_build_code()`](https://kcevers.github.io/sdbuildR/reference/get_build_code.md).
This will return the R code used to build the model, omitting default
specifications.

``` r
get_build_code(xmile("logistic_model"))
#> Warning: Could not use `colored = TRUE`, as the package prettycode is not installed.
#> Please install it if you want to see colored output or see
#> `?styler::print.vertical()` for more information.
#> sfm <- xmile() |>
#>   sim_specs(method = "euler", start = "0.0", stop = "200.0", dt = "0.01", save_at = "0.01", save_from = "0.0", seed = NULL, time_units = "s", language = "R") |>
#>   header(name = "Logistic model", created = "2025-11-19 11:26:27.175056") |>
#>   build(name = "X", type = "stock", eqn = ".01", label = "Population size") |>
#>   build(name = "r", type = "constant", eqn = "0.1", label = "Growth rate") |>
#>   build(name = "K", type = "constant", eqn = "1", label = "Carrying capacity") |>
#>   build(name = "inflow", type = "flow", eqn = "r * X", to = "X", label = "Births") |>
#>   build(name = "outflow", type = "flow", eqn = "r * X^2 / K", from = "X", label = "Deaths")
```

### Changing variable names

To rename a variable, use `change_name` in
[`build()`](https://kcevers.github.io/sdbuildR/reference/build.md):

``` r
sfm <- xmile("SIR") |> build("Infection_Rate", change_name = "Infections")
```

This will ensure that all references to `'Infection_Rate'` are changed
to `'Infections'`. Both when creating variables and changing variable
names, you may get a warning that the name was modified to be
syntactically valid and unique. For example, you cannot use the name
`'t'`, as this is the current time step in the ODE. Similarly, names
cannot contain spaces or special characters. Use `change_name` to adjust
the name to your liking.

### Erasing variables

To remove a variable from the model, use `erase` in
[`build()`](https://kcevers.github.io/sdbuildR/reference/build.md):

``` r
sfm <- build(sfm, "Susceptible", erase = TRUE)
```

Note that this cannot be undone!

### Changing variable types

During the process of building a model, you may realize that a variable
is not what you thought it was - a constant may actually be a stock, or
a stock may turn out to be a flow on second thought. For example, a
birth rate may not be constant throughout the simulation, but depend on
other variables in the model. You could erase the variable and create a
new one, but you may also use `change_type` in
[`build()`](https://kcevers.github.io/sdbuildR/reference/build.md):

``` r
sfm <- xmile() |>
  build("birth_rate", "constant") |>
  build("birth_rate", change_type = "aux")
```

### Graphical functions

Graphical functions (`gf`), also known as table or lookup functions, are
interpolation functions used to create custom input-output functions,
where we define the desired output (y) for a specified input (x). They
are defined by a set of x- and y-domain points. The interpolation method
defines the behaviour of the graphical function between x-points, and
the extrapolation method defines the behaviour outside of the x-points.
For example, a simple graphical function called “graph” may look like
this:

``` r
sfm <- xmile() |>
  build("graph", "gf",
    xpts = c(0, 1, 2), ypts = c(0.5, 1, 1),
    interpolation = "linear", extrapolation = "nearest"
  )
```

The function can now be used in any equation in the model like so:

``` r
sfm <- build(sfm, "x", "constant", eqn = "graph(1)")
```

### Macros

Macros are global variables or functions that can be used anywhere in
the model. They are useful for defining constants or functions that are
used repeatedly. For example, if the
[`logistic()`](https://kcevers.github.io/sdbuildR/reference/logistic.md)
function did not exist, you could create it yourself:

``` r
sfm <- xmile() |>
  macro("func", eqn = "function(x, slope = 1, midpoint = .5) 1 / (1 + exp(-slope*(x-midpoint)))")
```

This will create a function `func()` that can be used in any equation in
the model like so:

``` r
sfm <- build(sfm, "x", "constant", eqn = "func(0)")
```
