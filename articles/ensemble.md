# Ensemble simulations

``` r

library(sdbuildR)
```

After having built a stock-and-flow model, you may want to explore how
different parameter values affect the model’s behaviour. Running
multiple simulations with varying parameters is also called an ensemble,
which provides insight into the range of possible outcomes and
uncertainty associated with your model. In this vignette, we will
explore how to set up and run ensemble simulations using the sdbuildR
package.

## Setting up the model

For this example, we will use Crielaard et al.’s (2022) model of eating
behaviour, including the stocks hunger, eating, and compensatory
behaviour (i.e., disordered eating behaviour such as purging and
overexercising). For more details, see Crielaard et al. (2022). We can
load this example from the model library and look what is inside:

``` r

sfm <- stockflow("crielaard2022")
print(sfm)
#> 
#> ── Stock-and-Flow Model: Eating Behaviour (Crielaard et al., 2022) ─────────────
#> 3 stocks • 8 flows • 3 constants
#> 
#> ── Stock-Flow Structure ──
#> Compensatory_behaviour: + Compensating_for_having_eaten -
#> Satisfaction_with_hungry_feeling
#> Food_intake: + Effect_of_eating_triggers + Feeling_hunger -
#> Effect_of_compensatory_behavior - Satiety
#> Hunger: + Losing_energy_by_compensatory_behavior - Food_intake_reduces_hunger
#> 
#> ── Other Variables ──
#> Constants: `a0`, `a1`, and `a2`
#> 
#> ── Simulation Settings ──
#> Time: 0.0 to 100.0 days (dt = 0.01) • euler • R
#> Simulation output: stocks only
```

Simulations run in R by default. If you want to use Julia for faster
execution or additional parallelization options, you can activate the
Julia environment for sdbuildR and change the simulation language to
Julia (see below). For guidance on installing and setting up the Julia
environment, see [this
vignette](https://kcevers.github.io/sdbuildR/articles/julia-setup.html).

Without changing the parameters, we can run a single simulation to see
how the model behaves:

``` r

sfm |>
  simulate() |>
  plot()
```

As the model has random initial conditions, another run will be
different:

``` r

sfm |>
  simulate() |>
  plot()
```

To explore this more systematically, we can run an ensemble simulation
using the
[`ensemble()`](https://kcevers.github.io/sdbuildR/reference/ensemble.md)
function.

## Running ensemble simulations

Ensemble simulations create multiple runs of the model, which only makes
sense if the model either has some random elements or if parameters are
being varied. Our model already has random initial conditions, but if it
did not, we could create these:

``` r

sfm <- update(sfm, c(Food_intake, Hunger, Compensatory_behaviour),
  eqn = runif(1)
)
```

With random initial conditions, multiple runs of the same model will be
different. As running ensemble simulations can be quite memory
intensive, it is highly recommended to reduce the size of the returned
timeseries. This will save memory and speed up the simulation. For
example, we can save only 50 evenly-spaced time points per run with
`save_length`:

``` r

sfm <- sim_settings(sfm, save_length = 50)
```

With random initial conditions and reduced output, the model is now
ready for running ensemble simulations. We complete 100 runs using the
[`ensemble()`](https://kcevers.github.io/sdbuildR/reference/ensemble.md)
function:

``` r

sims <- ensemble(sfm, n = 100)
#> Starting ensemble simulation in "R" with 100 simulations.
#> ✔ Ensemble simulation completed in 18.8506 seconds.
```

``` r

plot(sims)
```

The plot shows the mean and confidence interval of the stocks (mean with
95% confidence interval). We can also plot the individual runs, for
which we first have to rerun the simulation with `save_sims = TRUE`:

``` r

sfm <- sim_settings(sfm, save_sims = TRUE)
# or pass `save_sims = TRUE` directly to `ensemble()`:
sims <- ensemble(sfm, n = 30, save_sims = TRUE)
#> Starting ensemble simulation in "R" with 30 simulations.
#> ✔ Ensemble simulation completed in 5.6697 seconds.
```

``` r

plot(sims, which = "sims")
```

We can change which simulations we plot by specifying the `sim`
argument:

``` r

plot(sims, which = "sims", sim = 15:30)
```

### Selecting variables to save

By default, only the stocks are saved. We can save all variables by
updating the simulation settings:

``` r

sfm <- sim_settings(sfm, only_stocks = FALSE)
```

Alternatively, we can specify which variables to save with the `vars`
argument. For example, to save only the stocks Hunger and Food_intake,
we can run:

``` r

sfm <- sim_settings(sfm, vars = c("Hunger", "Food_intake"))
```

``` r

sims <- ensemble(sfm, n = 100)
#> Starting ensemble simulation in "R" with 100 simulations.
#> ✔ Ensemble simulation completed in 18.6912 seconds.
```

``` r

head(sims, direction = "wide", which = "sims", n = 1)
#>   time sim condition Food_intake    Hunger
#> 1    0   1         1   0.8075164 0.3849424
```

Resetting `vars` to `NULL` will save all variables again:

``` r

sfm <- sim_settings(sfm, vars = NULL, only_stocks = TRUE)
```

### Selecting summary statistics

By default,
[`ensemble()`](https://kcevers.github.io/sdbuildR/reference/ensemble.md)
computes as summary statistics the mean and median, together with a 95%
interval (the 2.5% and 97.5% quantiles), across runs at each time point.
The quantile levels can be changed as follows:

``` r

sims <- ensemble(sfm,
  n = 100,
  quantiles = c(0.1, 0.9)
)
#> Starting ensemble simulation in "R" with 100 simulations.
#> ✔ Ensemble simulation completed in 18.7362 seconds.
head(sims)
#>   condition               variable      time      mean    median missing_count
#> 1         1 Compensatory_behaviour  0.000000 0.4750875 0.4036320             0
#> 2         1 Compensatory_behaviour  2.040816 0.6611736 0.6447433             0
#> 3         1 Compensatory_behaviour  4.081633 0.6901401 0.6983019             0
#> 4         1 Compensatory_behaviour  6.122449 0.6484665 0.6473452             0
#> 5         1 Compensatory_behaviour  8.163265 0.5759548 0.5646513             0
#> 6         1 Compensatory_behaviour 10.204082 0.5068495 0.4974462             0
#>       quant1    quant2
#> 1 0.09538729 0.8620636
#> 2 0.49913587 0.8756194
#> 3 0.52347853 0.8442506
#> 4 0.50208329 0.7992062
#> 5 0.46242760 0.6944000
#> 6 0.43265309 0.5799470
```

Here, quantiles appear as columns `quant1`, `quant2` which correspond to
`sims$quantiles`.

To change which summary statistics are computed, use `central` (defining
the central tendency) and `spread` (defining the measure of dispersion).
For example, to summarize each run by its median together with a
standard deviation:

``` r

sims <- ensemble(sfm,
  n = 100,
  central = "median",
  spread = "sd"
)
#> Starting ensemble simulation in "R" with 100 simulations.
#> ✔ Ensemble simulation completed in 18.5931 seconds.
head(sims)
#>   condition               variable      time    median         sd missing_count
#> 1         1 Compensatory_behaviour  0.000000 0.4036320 0.29498048             0
#> 2         1 Compensatory_behaviour  2.040816 0.6447433 0.14195140             0
#> 3         1 Compensatory_behaviour  4.081633 0.6983019 0.11711189             0
#> 4         1 Compensatory_behaviour  6.122449 0.6473452 0.11086144             0
#> 5         1 Compensatory_behaviour  8.163265 0.5646513 0.09414421             0
#> 6         1 Compensatory_behaviour 10.204082 0.4974462 0.06660461             0
```

All available summary statistics can be computed with:

``` r

sims <- ensemble(sfm,
  n = 100,
  central = c("mean", "median"),
  spread = c("quantile", "sd", "range")
)
#> Starting ensemble simulation in "R" with 100 simulations.
#> ✔ Ensemble simulation completed in 18.4981 seconds.
head(sims, n = 1)
#>   condition               variable time      mean   median        sd
#> 1         1 Compensatory_behaviour    0 0.4750875 0.403632 0.2949805
#>           min       max missing_count     quant1    quant2
#> 1 0.008115848 0.9553296             0 0.01931528 0.9259952
```

Plots can then switch between summary statistics:

``` r

plot(sims, central = "median", spread = "quantile")
```

``` r

plot(sims, central = "median", spread = "range")
```

``` r

plot(sims, central = "none", spread = "range")
```

### Computational efficiency

#### Parallel simulations (R)

By default, R ensemble simulations run sequentially. To run simulations
in parallel, use the `future` package to control parallel execution:

``` r

# Set up parallel execution with 4 workers
future::plan(future::multisession, workers = 4)

# Run 100 simulations in parallel
sims <- ensemble(sfm, n = 100)

# Restore sequential execution
future::plan(future::sequential)
```

The `workers` argument specifies how many parallel processes to use;
adjust this based on your system’s capabilities. On Windows,
`multisession` is the recommended backend. On POSIX systems (Linux,
macOS), you can also use `multicore` for potentially better performance,
though `multisession` will work on those systems as well.

#### Ensemble simulations (Julia)

Ensemble simulations can also be run in Julia, which is typically faster
than R for large simulations. To use the Julia backend, first follow the
instructions in the [Julia setup
vignette](https://kcevers.github.io/sdbuildR/articles/julia-setup.html)
to install and set up the Julia environment for sdbuildR.

Activate the Julia environment for sdbuildR:

``` r

use_julia()
#> ℹ Activating Julia environment for sdbuildR at
#>   /home/runner/.local/share/R/sdbuildR/julia...
#> ✔ Julia environment ready.
```

Then, set the simulation language to Julia:

``` r

sfm <- sim_settings(sfm, language = "julia")
```

We can also enable parallel execution in Julia by setting the number of
threads:

``` r

use_julia(nthreads = 4)
```

To stop using threaded simulations, run:

``` r

use_julia(restart = TRUE)
```

## Specifying conditions

Instead of generating an ensemble with random initial conditions, we can
also specify exact constant and initial stock values to vary. For
example, we could vary the `a2` parameter, which determines how strongly
having eaten increases compensatory behaviour.

``` r

sims <- ensemble(sfm,
  n = 100,
  conditions = list(a2 = c(0.2, 0.4, 0.6, 0.8))
)
#> Starting ensemble simulation in "Julia" with 400 simulations in total.
#> ℹ 4 conditions x 100 simulations per condition.
#> ✔ Ensemble simulation completed in 9.9206 seconds.
```

``` r

plot(sims)
```

### Varying multiple parameters

We can also vary multiple parameters at once. For example, we can vary
both `a2` and `a1`, where the latter influences how strongly food intake
leads to more food intake. `n` now specifies the number of simulations
per condition. By default, `cross = TRUE`, which means that all possible
combinations of parameters are simulated.

``` r

sims <- ensemble(sfm,
  conditions = list(
    a2 = c(0.2, 0.8),
    a1 = c(1.3, 1.5)
  ),
  n = 100
)
#> Starting ensemble simulation in "Julia" with 400 simulations in total.
#> ℹ 4 conditions x 100 simulations per condition.
#> ✔ Ensemble simulation completed in 2.2957 seconds.
```

``` r

plot(sims)
```

The plot shows similarity within columns but differences between
columns. As `a1` differs between columns, it appears that `a1` has a
larger effect than `a2`.

To view the parameter combination corresponding to each condition, view
`conditions` in `sims`:

``` r

sims$conditions
#>      condition  a1  a2
#> [1,]         1 1.3 0.2
#> [2,]         2 1.5 0.2
#> [3,]         3 1.3 0.8
#> [4,]         4 1.5 0.8
```

To explore the effect of each parameter interactively, use
`condition_display = "slider"` or `condition_display = "dropdown"`:

``` r

plot(sims, condition_display = "slider")
```

### Crossed vs non-crossed designs

To generate a non-crossed designed, set `cross = FALSE`. In this case,
the length of each vector in `conditions` needs to be the same.

``` r

sims <- ensemble(sfm,
  conditions = list(
    a2 = c(0.4, 0.5, 0.6),
    a1 = c(1.3, 1.4, 1.5)
  ),
  n = 100, cross = FALSE, save_sims = TRUE
)
#> Starting ensemble simulation in "Julia" with 300 simulations in total.
#> ℹ 3 conditions x 100 simulations per condition.
#> ✔ Ensemble simulation completed in 2.3044 seconds.
```

``` r

plot(sims, nrows = 1)
```

We can select specific conditions to compare, where here we plot the
first fifteen simulations of the first two conditions:

``` r

plot(sims, sim = 1:15, condition = 1:2, which = "sims", nrows = 1)
```

### Accessing simulation results

The results of the ensemble simulation are stored in the `sims` object.

You can access the summary statistics per condition and per time point,
such as the mean and confidence intervals, using:

``` r

head(sims, n = 1)
#>   condition time               variable      mean    median missing_count
#> 1         1    0 Compensatory_behaviour 0.4783079 0.4449562             0
#>       quant1    quant2
#> 1 0.02306451 0.9746829
# or
# sims |> as.data.frame() |> head()
```

By default, simulations are returned in long format, but can also be
shaped in wide format as well:

``` r

head(sims, n = 1, direction = "wide")
#>   condition time mean.Compensatory_behaviour median.Compensatory_behaviour
#> 1         1    0                   0.4783079                     0.4449562
#>   missing_count.Compensatory_behaviour quant1.Compensatory_behaviour
#> 1                                    0                    0.02306451
#>   quant2.Compensatory_behaviour mean.Food_intake median.Food_intake
#> 1                     0.9746829        0.4766333          0.4552623
#>   missing_count.Food_intake quant1.Food_intake quant2.Food_intake mean.Hunger
#> 1                         0         0.01626855          0.9624419    0.478489
#>   median.Hunger missing_count.Hunger quant1.Hunger quant2.Hunger
#> 1     0.4548582                    0    0.01419136     0.9650377
```

With `save_sims = TRUE`, the individual simulation runs can be accessed
by specifying `which = "sims"`. The dataframe contains the value of each
variable, for each time point, for each simulation, for each condition.

``` r

head(sims, n = 1, which = "sims", direction = "long")
#>   condition sim time               variable      value
#> 1         1   1    0 Compensatory_behaviour 0.09211427
```

Finally, to access the parameters (i.e., constants) of each simulation
per condition, run:

``` r

head(sims, n = 1, which = "sims", type = "constant")
#>   condition sim variable value
#> 1         1   1       a0  1.31
```

To view their summary statistics, run:

``` r

head(sims, n = 1, which = "summary", type = "constant")
#>   condition variable mean median missing_count quant1 quant2
#> 1         1       a0 1.31   1.31             0   1.31   1.31
```

## Close Julia session

``` r

use_julia(stop = TRUE)
#> ✔ Closed Julia session.
```
