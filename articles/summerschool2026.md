# Theory Building in Psychology (Summer School 2026)

## Formalizing Theory with Stock-and-Flow Models

### Inflows Increase a Stock

| Time | People Waiting in Queue | Arrivals |
|------|-------------------------|----------|
| 0.00 | 0.00                    | 1        |
| 0.01 | 0.01                    | 1        |
| 0.02 | 0.02                    | 1        |
| 0.03 | 0.03                    | 1        |
| 0.04 | 0.04                    | 1        |
| 0.05 | 0.05                    | 1        |

### Outflows Decrease a Stock

What **processes** cause the state to change?

| Time | People Waiting in Queue | Arrivals | Service |
|------|-------------------------|----------|---------|
| 0.00 | 0.000                   | 1        | 0.5     |
| 0.01 | 0.005                   | 1        | 0.5     |
| 0.02 | 0.010                   | 1        | 0.5     |
| 0.03 | 0.015                   | 1        | 0.5     |
| 0.04 | 0.020                   | 1        | 0.5     |
| 0.05 | 0.025                   | 1        | 0.5     |

### What Happens When the Outflow Is Larger Than the Inflow?

| Time | People Waiting in Queue | Arrivals | Service |
|------|-------------------------|----------|---------|
| 0.00 | 0.00                    | 1        | 2       |
| 0.01 | -0.01                   | 1        | 2       |
| 0.02 | -0.02                   | 1        | 2       |
| 0.03 | -0.03                   | 1        | 2       |
| 0.04 | -0.04                   | 1        | 2       |
| 0.05 | -0.05                   | 1        | 2       |

### Functional Forms

**Functional form**: mathematical relationship specifying how one
variable affects another.

constant

linear

`eqn = `

### Making the Outflow Stock-Dependent

This is a **negative feedback loop**: the more people, the greater the
outflow, which reduces the amount of people in the queue.

A **positive** feedback loop **amplifies**, whereas a **negative**
feedback loop **dampens** or **balances**.

### Multiple Outflows

### Multiple Stocks

### Add A Constant to the Model

Add a constant for service rate:

### Add An Auxiliary Variable to the Model

Add an auxiliary variable for satisfaction:

### Recap

Stock-and-flow models **formalize the processes by which states change**
over time.

## Applied Examples of Stock-and-Flow Models

This framework can be applied to model a wide range of phenomena.

## Stock-and-Flow Models to Build Psychological Theory

## Package Setup

The workshop materials are available at
<https://kcevers.github.io/sdbuildR/summerschool2026>.

``` r

library(sdbuildR)
```

Load the stock-and-flow model of people waiting in a queue from the
package’s model library:

``` r

sfm <- stockflow("queue")
```

`sfm` stands for **s**tock-and-**f**low **m**odel.

Look at the model’s structure:

``` r

print(sfm)
#> 
#> ── Stock-and-Flow Model ────────────────────────────────────────────────────────
#> 2 stocks • 3 flows • 1 constant • 1 auxiliary
#> 
#> ── Stock-Flow Structure ──
#> queue: + arrivals - leave - service
#> served: + service
#> 
#> ── Other Variables ──
#> Constants: `service_rate`
#> Auxiliaries: `satisfaction`
#> 
#> ── Simulation Settings ──
#> Time: 0.0 to 10.0 hours (dt = 0.01) • euler • R
#> Simulation output: all variables
```

Or inspect it in data frame format:

``` r

as.data.frame(sfm, properties = "eqn")
#>       type         name                         eqn
#> 1    stock        queue                           0
#> 2    stock       served                           0
#> 3     flow     arrivals                           1
#> 4     flow        leave               0.1 * queue^2
#> 5     flow      service        service_rate * queue
#> 6 constant service_rate                         0.5
#> 7      aux satisfaction service / (service + leave)
```

Plot the stock-and-flow diagram:

``` r

plot(sfm, show_constants = TRUE)
```

Simulate the model and plot the resulting time series:

``` r

sfm |> simulate() |> plot()
```

The pipe operator `|>` passes the result on its left into the function
on its right. For example, `x |> f(y)` is interpreted as `f(x, y)`.

The timeseries can also be inspected in data frame format:

``` r

sfm |> simulate() |> as.data.frame() |> head()
#>   time variable      value
#> 1 0.00    queue 0.00000000
#> 2 0.01    queue 0.01000000
#> 3 0.02    queue 0.01994990
#> 4 0.03    queue 0.02984975
#> 5 0.04    queue 0.03969961
#> 6 0.05    queue 0.04949954
```

## A Simplified Model of Burnout

Draw the **target phenomenon**:

    #> Warning: No trace type specified and no positional attributes specified

### Building a Model from Scratch

``` r

# Initialize stock-and-flow model
sfm <- stockflow()
print(sfm)
#> 
#> ── Stock-and-Flow Model ────────────────────────────────────────────────────────
#> ℹ Empty model without any variables.
#> 
#> ── Simulation Settings ──
#> Time: 0 to 100 seconds (dt = 0.01) • euler • R
#> Simulation output: stocks only

# Change name
sfm <- meta(sfm, name = "Burnout Model")
```

### Defining the Time Horizon and Time Unit

Change simulation settings:

``` r

sfm <- sim_settings(sfm, 
  # Run simulation for 6 months (~ 180 days)
  stop = round(365/2), time_unit = "days", 
  # Return all variables in output (not just stocks)
  only_stocks = FALSE)

print(sfm)
#> 
#> ── Stock-and-Flow Model: Burnout Model ─────────────────────────────────────────
#> ℹ Empty model without any variables.
#> 
#> ── Simulation Settings ──
#> Time: 0.0 to 182.0 days (dt = 0.01) • euler • R
#> Simulation output: all variables
```

### Adding a Stock

``` r

sfm <- stock(sfm, name = engagement, eqn = .3)
```

``` r

# Plot stock-and-flow diagram
sfm |> plot()
```

``` r

# Simulate and visualise timeseries
sfm |> simulate() |> plot()
```

### Adding an Outflow

``` r

sfm <- constant(sfm, decay_rate, eqn = .05) |>
       flow(decay, eqn = decay_rate, from = engagement)

# Plot stock-and-flow diagram
sfm |> plot()
```

``` r


# Simulate and visualise timeseries
sfm |> simulate() |> plot()
```

Animate the simulation over time:

``` r

sfm |> simulate() |> plot(animation = "time")
```

### Stock-Dependent Outflow

``` r

sfm <- update(sfm, decay, eqn = decay_rate * engagement)
sfm |> plot()
```

``` r

sfm |> simulate() |> plot()
```

### Adding an Inflow

``` r

sfm <- constant(sfm, enjoyment, eqn = .3) |>
       flow(motivation, eqn = enjoyment, to = engagement)
sfm |> plot()
```

``` r

sfm |> simulate() |> plot()
```

The stock settles at a steady state where inflow == outflow.

### Dynamic Inflow Rate

What if work enjoyment is not static, but erodes over time? Put
differently, what if work enjoyment is not a constant, but a stock?

``` r

sfm <- change_type(sfm, enjoyment, new_type = "stock")
sfm |> plot()
```

### Adding an Outflow from Enjoyment

``` r

sfm <- flow(sfm, overcommitment, eqn = enjoyment * new_projects, 
            from = enjoyment) |>
       aux(new_projects, eqn = .1 * engagement)
sfm |> plot()
```

``` r

sfm |> simulate() |> plot()
```

### Recap

Iteratively, we have built a stock-and-flow model with:

- A stock for engagement
- A constant outflow, updated to a stock-dependent outflow
- A constant inflow, updated to an inflow with a dynamic rate

In other words, the inflow rate changed from an **exogenous to an
endogenous variable**; from a constant to a stock that erodes over time.

## Dependence on Initial Condition

If you did not complete all steps above, load the updated model from the
model library:

``` r

sfm <- stockflow("burnout")
```

Initialize the engagement stock with a random value between 0 and 4:

``` r

sfm <- update(sfm, engagement, eqn = runif(1, min = 0, max = 4)) 
```

Equations are stored as expressions:

``` r

plot(sfm)
```

This means that each simulation will have a different initial value for
engagement:

``` r

sfm |> simulate() |> plot()
```

## Systematically Investigating Dependence on Initial Conditions

Update simulation settings to prepare for ensemble simulations:

``` r

sfm <- sfm |>
  # Save only 50 time points 
  sim_settings(save_length = 50, 
  # Save only stocks (not flows or auxiliaries)
  only_stocks = TRUE, 
  # Save individual simulation runs (not just summary statistics)
  save_sims = TRUE)
```

Run 50 simulations with different initial conditions:

``` r

sims <- ensemble(sfm, n = 50)
#> Starting ensemble simulation in "R" with 50 simulations.
#> ✔ Ensemble simulation completed in 11.4668 seconds.
```

``` r

# Summary statistics
plot(sims)
```

``` r


# Individual simulation runs
plot(sims, which = "sims")
```

## Assignment: Expressing Theoretical Ideas with Equations

### Set-up

Let’s return to the simpler model with a static recovery rate:

``` r

sfm <- sfm |>
 # Remove the outflow and auxiliary
 discard(c(overcommitment, new_projects)) |>
 # Change recovery_rate back to a constant
 change_type(enjoyment, new_type = "constant")

sfm |> plot(show_constants = TRUE)
```

This model has a constant inflow. The functional form of the motivation
inflow can be changed to explore different theoretical assumptions about
how motivation depends on engagement.

### Functional Forms

constant

linear

hill

ricker

`eqn = `

Explore the functional forms above:

1.  What theoretical ideas does each functional form express?

2.  How do they differ in terms of the dynamics they produce?

3.  Is the behaviour dependent on the initial condition of energy?

### Hint

The motivation inflow can be updated to a different functional form
using the [`update()`](https://rdrr.io/r/stats/update.html) function.
For example, to change the inflow to a linear function of engagement:

``` r

sfm_linear <- update(sfm, motivation, eqn = enjoyment * engagement)

sfm_linear |> simulate() |> plot()
```

## Bonus Material

### Parallel Simulations

If you have a multi-core machine, you can run simulations in parallel to
speed up simulations:

``` r

# Set up parallel execution (3 cores less than the total number of available cores)
n_workers <- max(c(1, parallel::detectCores() - 3))
future::plan(future::multisession, workers = n_workers)
```

If configured,
[`ensemble()`](https://kcevers.github.io/sdbuildR/reference/ensemble.md)
will automatically run simulations in parallel. Otherwise, it will run
sequentially.

``` r

sfm <- sim_settings(sfm, save_sims = TRUE)

# Run 100 simulations
sims <- ensemble(sfm, n = 100)
#> Starting ensemble simulation in "R" with 100 simulations.
#> ✔ Ensemble simulation completed in 21.0217 seconds.

# Individual simulation runs
plot(sims, which = "sims")
```

### Parameter Sensitivity

Ensemble simulations can also be used to explore the sensitivity of the
model to changes in parameter values.

``` r

# Reload model from library to reset any changes made above
sfm <- stockflow("burnout", version = 2) |>
  update(engagement, eqn = runif(1, min = 0, max = 4)) |>
  sim_settings(save_length = 50, only_stocks = TRUE, save_sims = TRUE)

# Specify a range of values of decay_rate to vary
conditions <- list(decay_rate = c(0.005, 0.01, 0.05, 0.1, 0.2))

# Run 50 simulations for each condition
sims <- ensemble(sfm, n = 50, conditions = conditions)
#> Starting ensemble simulation in "R" with 250 simulations in total.
#> ℹ 5 conditions x 50 simulations per condition.
#> ✔ Ensemble simulation completed in 56.9931 seconds.
```

``` r

plot(sims, which = "sims", condition_display = "slider")
```

If parallel execution was configured above, restore sequential
execution:

``` r

future::plan(future::sequential)
```
