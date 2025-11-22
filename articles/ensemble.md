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
behaviour (i.e. disordered eating behaviour such as purging and
overexercising). For more details, see Crielaard et al. (2022). We can
load this example from the model library and look what is inside:

``` r
sfm <- xmile("Crielaard2022")
summary(sfm)
#> Your model contains:
#> * 3 Stocks: Food_intake, Hunger, Compensatory_behaviour
#> * 8 Flows: Losing_energy_by_compensatory_behavior, Feeling_hunger, Satiety, Food_intake_reduces_hunger, Compensating_for_having_eaten, Satisfaction_with_hungry_feeling, Effect_of_eating_triggers, Effect_of_compensatory_behavior
#> * 3 Constants: a0, a1, a2
#> * 0 Auxiliaries
#> * 0 Graphical Functions
#> * 0 Custom model units
#> * 0 Macros
#> 
#> Simulation time: 0.0 to 100.0 days (dt = 0.01)
#> Simulation settings: solver euler in R
```

Ensemble simulations are only supported in Julia, so we need to activate
the Julia environment for sdbuildR and change the simulation language to
Julia. For guidance on installing and setting up the Julia environment,
see [this
vignette](https://kcevers.github.io/sdbuildR/articles/julia-setup.html).

``` r
use_julia()
#> Starting Julia ...
#> Connecting to Julia TCP server at localhost:11980 ...
#> Setting up Julia environment for sdbuildR...
sfm <- sim_specs(sfm, language = "Julia")
```

Without changing the parameters, we can run a single simulation to see
how the model behaves:

``` r
sim <- simulate(sfm)
plot(sim)
```

As the model has random initial conditions, another run will be
different:

``` r
sim <- simulate(sfm)
plot(sim)
```

To explore this more systematically, we can run an ensemble simulation
using the
[`ensemble()`](https://kcevers.github.io/sdbuildR/reference/ensemble.md)
function.

## Running ensemble simulations

Ensemble simulations create multiple runs of the model, which only makes
sense if the model either has some random elements or if parameters are
being varied. Our model already has random initial conditions, but if it
had not, we could create these:

``` r
sfm <- build(sfm, c("Food_intake", "Hunger", "Compensatory_behaviour"),
  eqn = "runif(1)"
)
```

With random initial conditions, multiple runs of the same model will be
different. As running ensemble simulations can be quite memory
intensive, it is highly recommended to reduce the size of the returned
timeseries. This will save memory and speed up the simulation. For
example, we can only save the timeseries every 1 time units, and only
save from time 10:

``` r
sfm <- sim_specs(sfm, save_at = 1, save_from = 10)
```

The model is now ready for running ensemble simulations. We complete 100
runs using the
[`ensemble()`](https://kcevers.github.io/sdbuildR/reference/ensemble.md)
function:

``` r
sims <- ensemble(sfm, n = 100)
#> Running a total of 100 simulations
#> Simulation took 5.6572 seconds
```

``` r
plot(sims)
```

The plot shows the mean and confidence interval of the stocks (mean with
95% confidence interval). We can also plot the individual runs, for
which we first have to rerun the simulation and set
`return_sims = TRUE`:

``` r
sims <- ensemble(sfm, n = 30, return_sims = TRUE)
#> Running a total of 30 simulations
#> Simulation took 0.5922 seconds
```

``` r
plot(sims, type = "sims")
```

This automatically only plots the first ten simulations, as plotting a
large number of simulations can be quite slow. We can change which
simulations we plot by specifying the `i` argument:

``` r
plot(sims, type = "sims", i = 15:30)
```

By default, only the stocks are saved. To save all variables, set
`only_stocks = FALSE`:

``` r
sims <- ensemble(sfm, n = 100, only_stocks = FALSE)
#> Running a total of 100 simulations
#> Simulation took 2.0171 seconds
```

``` r
plot(sims)
```

### Parallel simulations

To run simulations in parallel, set the number of threads you would like
to use:

``` r
use_threads(n = 4)
#> Warning in use_threads(n = 4): n is set to 4, which is higher than the number
#> of available cores minus 1. Setting it to 3.
```

To stop using parallel simulations, run `use_threads(stop = TRUE)`.

## Specifying ranges

Instead of generating an ensemble with random initial conditions, we can
also specify ensembles with exact parameter values. For example, we
could vary the a_2 parameter, which determines how strongly having eaten
increases compensatory behaviour.

``` r
sims <- ensemble(sfm,
  n = 100,
  range = list("a2" = c(0.2, 0.4, 0.6, 0.8))
)
#> Running a total of 400 simulations for 4 conditions (100 simulations per condition)
#> Simulation took 2.7312 seconds
```

``` r
plot(sims)
```

We can also vary multiple parameters at once. For example, we can vary
both a_2 and a_1, where the latter influences how strongly food intake
leads to more food intake. `n` now specifies the number of simulations
per condition. By default, `cross = TRUE`, which means that all possible
combinations of parameters are simulated.

``` r
sims <- ensemble(sfm,
  range = list(
    "a2" = c(0.2, 0.8),
    "a1" = c(1.3, 1.5)
  ),
  n = 100
)
#> Running a total of 400 simulations for 4 conditions (100 simulations per condition)
#> Simulation took 1.5521 seconds
```

``` r
plot(sims)
```

The plot shows similarity within columns but differences between
columns. As a_1 differs between columns, it appears that a_1 has a
larger effect than a_2. To view the parameter combination corresponding
to each condition j, view `conditions` in `sims`:

``` r
sims$conditions
#>      j  a1  a2
#> [1,] 1 1.3 0.2
#> [2,] 2 1.5 0.2
#> [3,] 3 1.3 0.8
#> [4,] 4 1.5 0.8
```

To generate a non-crossed designed, set `cross = FALSE`. In this case,
the length of each range needs to be the same.

``` r
sims <- ensemble(sfm,
  range = list(
    "a2" = c(0.4, 0.5, 0.6),
    "a1" = c(1.3, 1.4, 1.5)
  ),
  n = 100, cross = FALSE, return_sims = TRUE
)
#> Running a total of 300 simulations for 3 conditions (100 simulations per condition)
#> Simulation took 1.1042 seconds
```

``` r
plot(sims, nrows = 1)
```

We can select specific conditions to compare, where here we plot the
first fifteen simulations of the first two conditions:

``` r
plot(sims, i = 1:15, j = 1:2, type = "sims", nrows = 1)
```

### Accessing simulation results

The results of the ensemble simulation are stored in the `sims` object,
which is a list containing, among others: - `summary`: summary
statistics across all simulations per condition - `df`: individual
simulation data (if `return_sims = TRUE`) - `init`: initial values of
stocks - `constants`: parameter values used - `conditions`: matrix
showing parameter combinations for each j

You can access the summary statistics per condition j and per time
point, such as the mean and confidence intervals, using:

``` r
head(sims$summary)
#>   j time               variable        mean       median     variance
#> 1 1   10 Compensatory_behaviour 0.497633309 0.4980184654 0.0043764182
#> 2 1   10            Food_intake 0.009710898 0.0003722831 0.0007950528
#> 3 1   10                 Hunger 0.878490005 0.8684765325 0.0020621324
#> 4 1   11 Compensatory_behaviour 0.471891398 0.4713064932 0.0025858800
#> 5 1   11            Food_intake 0.008401243 0.0002708180 0.0006582843
#> 6 1   11                 Hunger 0.895091512 0.8865459415 0.0014092172
#>   missing_count         q025       q975
#> 1             0 4.034236e-01 0.64982041
#> 2             0 3.264930e-06 0.07285534
#> 3             0 7.868477e-01 0.97541233
#> 4             0 4.004288e-01 0.58318899
#> 5             0 2.581467e-06 0.05773706
#> 6             0 8.245510e-01 0.97825365
```

If you have set have set `return_sims = TRUE`, you can find the
individual simulation runs in `sims$df`. These are in long format,
containing the value of each variable for each time point in each
simulation i per condition j.

``` r
head(sims$df)
#>   j i time               variable        value
#> 1 1 1   10 Compensatory_behaviour 0.5392242687
#> 2 1 1   10            Food_intake 0.0005835991
#> 3 1 1   10                 Hunger 0.8337453364
#> 4 1 1   11 Compensatory_behaviour 0.5042458898
#> 5 1 1   11            Food_intake 0.0003910558
#> 6 1 1   11                 Hunger 0.8578220356
```

To view the initial values of each simulation i per condition j, run:

``` r
head(sims$init$df)
#>   j i               variable      value
#> 1 1 1 Compensatory_behaviour 0.09211427
#> 2 1 1            Food_intake 0.69842600
#> 3 1 1                 Hunger 0.49628470
#> 4 1 2 Compensatory_behaviour 0.95141725
#> 5 1 2            Food_intake 0.79604914
#> 6 1 2                 Hunger 0.79654026
```

To view their summary statistics, run:

``` r
head(sims$init$summary)
#>   j               variable      mean    median   variance missing_count
#> 1 1 Compensatory_behaviour 0.4783079 0.4449562 0.08892955             0
#> 2 1            Food_intake 0.4766333 0.4552623 0.07613286             0
#> 3 1                 Hunger 0.4784890 0.4548582 0.10593726             0
#> 4 2 Compensatory_behaviour 0.4902884 0.4747643 0.09696997             0
#> 5 2            Food_intake 0.4677841 0.4762301 0.08076779             0
#> 6 2                 Hunger 0.4799398 0.4828765 0.08724472             0
#>         q025      q975
#> 1 0.02306451 0.9746829
#> 2 0.01626855 0.9624419
#> 3 0.01419136 0.9650377
#> 4 0.03388623 0.9769073
#> 5 0.02314142 0.9178641
#> 6 0.01777176 0.9714317
```

Finally, to access the parameters (i.e. constants) of each simulation i
per condition j, run:

``` r
head(sims$constants$df)
#>   j i variable value
#> 1 1 1       a0  1.31
#> 2 1 1       a1  1.30
#> 3 1 1       a2  0.40
#> 4 1 2       a0  1.31
#> 5 1 2       a1  1.30
#> 6 1 2       a2  0.40
```

To view their summary statistics, run:

``` r
head(sims$constants$summary)
#>   j variable mean median     variance missing_count q025 q975
#> 1 1       a0 1.31   1.31 0.000000e+00             0 1.31 1.31
#> 2 1       a1 1.30   1.30 4.980182e-32             0 1.30 1.30
#> 3 1       a2 0.40   0.40 0.000000e+00             0 0.40 0.40
#> 4 2       a0 1.31   1.31 0.000000e+00             0 1.31 1.31
#> 5 2       a1 1.40   1.40 0.000000e+00             0 1.40 1.40
#> 6 2       a2 0.50   0.50 0.000000e+00             0 0.50 0.50
```
