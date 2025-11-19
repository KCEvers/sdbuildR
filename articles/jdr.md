# Formalizing Job Demands-Resources Theory

This is the online supplemental material accompanying the paper
*Formalizing Psychological Theories with sdbuildR: A System Dynamics
Modelling Tutorial in R* by Evers et al. (submitted). It contains the
complete code to build, test, and simulate a formal model of Job
Demands-Resources (JD-R) theory. To reproduce the figures in the paper,
please see the corresponding .Rmd file.

To follow the entire tutorial, it is necessary to install Julia as well
as set up the Julia environment. See [this
vignette](https://kcevers.github.io/sdbuildR/articles/julia-setup.html)
for guidance.

``` r
library(sdbuildR)
```

## Simulating Stock-and-Flow Models in R

``` r
sfm <- xmile() |>
  header(name = "Job Resources and Demands Theory") |>
  # Add a seed for reproducibility:
  sim_specs(seed = 123) |>
  sim_specs(time_units = "day", stop = 365 / 2) |>
  # Make save_at larger to reduce image file size
  sim_specs(save_at = .1)
```

``` r
sfm <- build(sfm, "E", "stock", eqn = .5, label = "Engagement") |>
  build("R", "stock", eqn = .7, label = "Job Resources")

plot(sfm)
```

``` r
sim <- simulate(sfm)
plot(sim)
```

``` r
debugger(sfm)
#> No problems detected!
#> Potentially problematic:
#> * Your model has no flows.
```

``` r
sfm <- build(sfm, "r_E_R", "constant", eqn = 0.2, label = "Motivation Rate") |>
  build("E_R", "flow", eqn = "r_E_R * R", to = "E", label = "Motivation")

sim <- simulate(sfm)
plot(sim)
```

``` r
sfm <- build(sfm, "K_E", "constant", eqn = 1, label = "Engagement Capacity") |>
  build("E_R", eqn = "r_E_R * R * (1 - E/K_E)")

sim <- simulate(sfm)
plot(sim)
```

``` r
sfm <- build(sfm, c("r_A", "K_R"), "constant",
  eqn = c(0.2, 1),
  label = c("Proactive Behaviour Rate", "Resource Capacity")
) |>
  build("A_to_R", "flow",
    eqn = "r_A * E * (1 - R/K_R)", to = "R",
    label = "Proactive Behaviour"
  )

sim <- simulate(sfm)
plot(sim)
```

``` r
sfm <- build(sfm, "r_R_X", "constant", eqn = 0.05, label = "Energy Decay Rate") |>
  build("R_X", "flow", eqn = "r_R_X * R", from = "R", label = "Decay")

sim <- simulate(sfm)
plot(sim)
```

``` r
sfm <- build(sfm, "D", "stock", eqn = .2, label = "Job Demands") |>
  build("X", "stock", eqn = .5, label = "Energy") |>
  build("P", "aux", eqn = "E + X", label = "Job Performance")
```

``` r
sfm <- build(sfm, "r_X_D", "constant",
  eqn = 0.4,
  label = "Fatigue from Demand Rate"
) |>
  build("K_X", "constant", eqn = 1, label = "Energy Capacity") |>
  build("X_D", "flow",
    eqn = "r_X_D * X * D / (1 + R)",
    from = "X", label = "Effort", doc = "Buffer of resources"
  ) |>
  build("E_R",
    eqn = "r_E_R * X * R * (1 + D) * (1 - E/K_E)",
    doc = "Boost of demands"
  )

sim <- simulate(sfm, only_stocks = FALSE)

# Specify variables to plot
vars <- c("E", "D", "R", "X", "P")
plot(sim, vars = vars)
```

``` r
sfm <- build(sfm, "r_X_X", "constant",
  eqn = 0.15,
  label = "Restoration Rate"
) |>
  build("X_X", "flow",
    eqn = "r_X_X * X * (1 - X/K_X)",
    to = "X", label = "Restoration"
  ) |>
  build("r_E_X", "constant",
    eqn = 0.1,
    label = "Energy-Based Disengagement Rate"
  ) |>
  build("E_X", "flow",
    eqn = "r_E_X * E * (1 - X/K_X)", from = "E",
    label = "Energy-Based Disengagement"
  )

sim <- simulate(sfm, only_stocks = FALSE)
plot(sim, vars = vars)
```

``` r
sfm <- build(sfm, "r_U", "constant",
  eqn = 0.15,
  label = "Self-undermining Rate"
) |>
  build("K_D", "constant", eqn = 1, label = "Demand Capacity") |>
  build("A_from_D", "flow",
    eqn = "r_A * E * D", from = "D",
    label = "Proactive Behaviour"
  ) |>
  build("U", "flow",
    eqn = "r_U * (1 - X/K_X)", to = "D",
    label = "Self-undermining"
  )

sim <- simulate(sfm, only_stocks = FALSE)
plot(sim, vars = vars)
```

``` r
sfm <- build(sfm, "r_D", "constant",
  eqn = 0.2,
  label = "Demand Regulation Rate"
) |>
  build("D_D", "flow",
    eqn = "r_D * (1 - D/K_D)", to = "D",
    label = "Demand Regulation"
  )

sim <- simulate(sfm, only_stocks = FALSE)
plot(sim, vars = vars)
```

``` r
plot(sfm, minlen = 1.5, font_size = 22)
```

We save this version of the stock-and-flow model for later use:

``` r
sfm0 <- sfm |> sim_specs(save_at = .1)
```

Note that this is identical to the version stored in the model library,
which can be loaded using
[`xmile()`](https://kcevers.github.io/sdbuildR/reference/xmile.md):

``` r
sfm0 <- xmile("JDR") |> sim_specs(save_at = .1)
```

## Interrogating Your Model

### Reality Check

``` r
# Rerun simulation with only_stocks = FALSE to get all model variables
sfm <- sfm0
sim <- simulate(sfm0, only_stocks = FALSE)
df <- as.data.frame(sim, direction = "wide")
head(df)
#>   time         D         E         R         X        P        E_R     A_to_R
#> 1  0.0 0.2000000 0.5000000 0.7000000 0.5000000 1.000000 0.04200000 0.03000000
#> 2  0.1 0.2211999 0.5017299 0.6995080 0.5012808 1.003011 0.04267322 0.03015316
#> 3  0.2 0.2417467 0.5035218 0.6990338 0.5023083 1.005830 0.04329440 0.03030861
#> 4  0.3 0.2616619 0.5053691 0.6985776 0.5030902 1.008459 0.04386466 0.03046591
#> 5  0.4 0.2809659 0.5072652 0.6981394 0.5036344 1.010900 0.04438522 0.03062467
#> 6  0.5 0.2996788 0.5092040 0.6977193 0.5039491 1.013153 0.04485744 0.03078450
#>          R_X        X_D        X_X        E_X   A_from_D          U       D_D
#> 1 0.03500000 0.02352941 0.03750000 0.02500000 0.02000000 0.07500000 0.1600000
#> 2 0.03497540 0.02609773 0.03749975 0.02502223 0.02219652 0.07480787 0.1557600
#> 3 0.03495169 0.02858834 0.03749920 0.02505986 0.02434495 0.07465376 0.1516507
#> 4 0.03492888 0.03099994 0.03749857 0.02511228 0.02644716 0.07453647 0.1476676
#> 5 0.03490697 0.03333156 0.03749802 0.02517890 0.02850485 0.07445484 0.1438068
#> 6 0.03488597 0.03558253 0.03749766 0.02525911 0.03051953 0.07440764 0.1400642
```

``` r
# Job demands and energy should negatively correlate
cor(df[["D"]], df[["X"]])
#> [1] -0.9110299
stopifnot(cor(df[["D"]], df[["X"]]) < -.5)

# Job resources and energy should positive correlate
cor(df[["R"]], df[["E"]])
#> [1] 0.8849693
stopifnot(cor(df[["R"]], df[["E"]]) > .5)

# Job performance should positively correlate with work engagement
cor(df[["P"]], df[["E"]])
#> [1] 0.9823004
stopifnot(cor(df[["P"]], df[["E"]]) > .5)

# Job performance should positively correlate with energy
cor(df[["P"]], df[["X"]])
#> [1] 0.94537
stopifnot(cor(df[["P"]], df[["X"]]) > .5)
```

``` r
# Behaviours should always be positive
stopifnot(all(df[["U"]] >= 0))
stopifnot(all(df[["A_to_R"]] >= 0))
stopifnot(all(df[["A_from_D"]] >= 0))
```

With zero job demands, engagement, energy, and performance can still be
high.

``` r
sfm2 <- build(sfm0, c("D", "r_D", "r_U"), eqn = 0)

sim <- simulate(sfm2, only_stocks = FALSE)
plot(sim, vars = vars)
```

``` r
# Adjust model
sfm0 <- build(sfm0, "P", eqn = "D * (E + X)")
```

We include some additional reality checks that are omitted from the
paper for brevity. First, we would expect that when resources are zero,
engagement decays to zero, which is indeed what we find.

``` r
sfm2 <- build(sfm0, c("R", "r_A"), eqn = 0)

sim <- simulate(sfm2, only_stocks = FALSE)
plot(sim, vars = vars)
```

When engagement is zero, decent job performance is still possible (see
beginning peak).

``` r
sfm2 <- build(sfm0, c("E", "r_E_R"), eqn = 0)

sim <- simulate(sfm2, only_stocks = FALSE)
plot(sim, vars = vars)
```

Similarly, when energy is set to zero, performance can still be okay
(see beginning peak).

``` r
# Set energy to zero
sfm2 <- build(sfm0, c("X", "r_E_R"), eqn = 0)

sim <- simulate(sfm2, only_stocks = FALSE)
plot(sim, vars = vars)
```

As an extreme condition test, we can initialize all stocks with zero. In
this case, only job demands rises.

``` r
sfm2 <- build(sfm0, c("E", "R", "X", "D"), eqn = 0)

sim <- simulate(sfm2, only_stocks = FALSE)
plot(sim, vars = vars)
```

As a robustness check, we initialize all stocks above their carrying
capacity, which the model is able to handle.

``` r
sfm2 <- build(sfm0, c("E", "R", "X", "D"),
  eqn = c("K_E + 5", "K_R + 5", "K_X + 5", "K_D + 5")
)

sim <- simulate(sfm2, only_stocks = FALSE)
plot(sim, vars = vars)
```

### Interpretability

To add units, the Julia environment needs to be set up:

``` r
use_julia()
#> Starting Julia ...
#> Connecting to Julia TCP server at localhost:11980 ...
#> Setting up Julia environment for sdbuildR...
```

Change the simulation engine to Julia:

``` r
sfm <- sim_specs(sfm0, language = "Julia")
```

``` r
sfm <- model_units(sfm, "UWES", doc = "Utrecht Work Engagement Scale") |>
  build("E", units = "UWES")
```

``` r
sfm <- model_units(sfm, "UWES", erase = TRUE) |>
  model_units("uE") |>
  build("E", units = "uE")
```

We can add similar placeholder units for all other stocks, as well as
for performance:

``` r
sfm <- model_units(sfm, c("uE", "uR", "uX", "uD", "uP"),
  doc = c(
    "Engagement", "Job Resources", "Energy",
    "Job Demands", "Job Performance"
  )
) |>
  build(c("R", "X", "D", "P"), units = c("uR", "uX", "uD", "uP"))
```

### Dimensional Consistency

To ensure dimensional consistency, we add units for all other variables
in the model.

``` r
sfm <- build(sfm, "r_E_R", units = "uE/uR/day") |>
  build("E_R", units = "uE/day") |>
  build("K_E", units = "uE") |>
  build(c("r_A", "K_R"), units = c("uR/uE/day", "uR")) |>
  build("A_to_R", units = "uR/day") |>
  build("r_R_X", units = "1/day") |>
  build("R_X", units = "uR/day") |>
  build("r_X_D", units = "1/day") |>
  build("K_X", units = "uX") |>
  build("X_D", units = "uX/day") |>
  build("r_X_X", units = "1/day") |>
  build("X_X", units = "uX/day") |>
  build("r_E_X", units = "1/day") |>
  build("E_X", units = "uE/day") |>
  build("r_U", units = "uD/day") |>
  build("K_D", units = "uD") |>
  build("A_from_D", units = "uD/day") |>
  build("U", units = "uD/day") |>
  build(c("r_D", "D_D"), units = "uD/day")
```

Some equations need to be adjusted. In order to make job performance
dimensionally consistent, two new constants are added:

``` r
sfm <- build(sfm, "r_P_E", "constant",
  eqn = 1,
  label = "Influence Engagement on Performance", units = "uP/(uE*uD)"
) |>
  build("r_P_X", "constant",
    eqn = 1,
    label = "Influence Energy on Performance", units = "uP/(uX*uD)"
  ) |>
  build("P", eqn = "D * (r_P_E * E + r_P_X * X)")
```

In order to give $r_{X_{D}}$ the unit $\frac{1}{\text{day}}$, we need to
add an additional factor, $r_{R_{D}}$ with the units $\frac{uR}{uD}$,
indicating how many unit resources are needed per unit demand to buffer
energy depletion.

``` r
sfm <- build(sfm, "r_R_D", "constant",
  eqn = 1,
  label = "Resources Needed per Unit Demand for Buffer", units = "uR/uD"
)
```

We use the unit function
[`u()`](https://kcevers.github.io/sdbuildR/reference/u.md) to add one to
resources:

``` r
sfm <- build(sfm, "X_D", eqn = "r_X_D * r_R_D * X * D / (u('1uR') + R)")
```

Proactive behaviour now increases resources and decreases demands with
the same constant $r_{A}$, creating dimensional inconsistency. We create
a new constant for the outflow from job demands:

``` r
sfm <- build(sfm, "r_A_D", "constant",
  eqn = .1,
  label = "Proactive Behaviour Rate", units = "1/(day*uE)"
) |>
  build("A_from_D", eqn = "r_A_D * E * D")
```

We verify whether our model is dimensionally consistent by simulation:

``` r
sim <- simulate(sfm, only_stocks = FALSE)
plot(sim, vars = vars)
```

Because units can slow down simulations, we only modify our original
model with the missing variables identified by the dimensional
consistency test:

``` r
sfm0 <- build(sfm0, "r_P_E", "constant",
  eqn = 1,
  label = "Influence Engagement on Performance"
) |>
  build("r_P_X", "constant",
    eqn = 1,
    label = "Influence Energy on Performance"
  ) |>
  build("P", eqn = "D * (r_P_E * E + r_P_X * X)") |>
  build("r_R_D", "constant",
    eqn = 1,
    label = "Resources Needed per Unit Demand for Buffer"
  ) |>
  build("X_D", eqn = "r_X_D * r_R_D * X * D / (1 + R)") |>
  build("r_A_D", "constant", eqn = .1, label = "Proactive Behaviour Rate") |>
  build("A_from_D", eqn = "r_A_D * E * D")

# Check that simulation runs without errors
sim <- simulate(sfm0)
```

### Functional Forms

``` r
# Grab parameter values from dataframe
r_E_X <- as.numeric(as.data.frame(sfm, name = "r_E_X")[["eqn"]])
K_X <- as.numeric(as.data.frame(sfm, name = "K_X")[["eqn"]])

# Range of energy to evaluate functional form over
X <- seq(0, 1, by = .01)

# Create dataframe with two functional forms
df <- rbind(
  data.frame(
    X = X,
    # Add a minus sign as it is an outflow
    value = -(1 - X / K_X),
    type = "Linear"
  ),
  data.frame(
    X = X,
    value = -logistic(X, slope = -100, midpoint = 0.2),
    type = "Logistic"
  )
)

# Pick two colours for linear and logistic functional form
colors <- grDevices::hcl.colors(n = 50, palette = "Roma")[c(20, 8)]

# Plot functional form
pl <- plotly::plot_ly() |>
  plotly::add_trace(
    data = df, x = ~X,
    y = ~value,
    colors = colors,
    color = ~type,
    legendgroup = ~type,
    type = "scatter",
    mode = "lines"
  ) |>
  plotly::add_trace(
    x = 0.2, y = -0.5, type = "scatter", mode = "markers",
    marker = list(size = 10, color = colors[2]),
    showlegend = FALSE
  ) |>
  plotly::layout(
    xaxis = list(title = "Energy (X)"),
    yaxis = list(title = "Effect on Work Engagement (E)"),
    font = list(family = "Times New Roman")
  )

pl
```

Simulate two different models.

``` r
sfm2 <- build(sfm0, "E_X",
  eqn = "r_E_X * E * logistic(X, slope = -100, midpoint = 0.2)"
)

sim1 <- simulate(sfm0, only_stocks = FALSE)
pl1 <- plot(sim1, xlim = c(0, 65), vars = c("D", "E", "P"), showlegend = FALSE)

sim2 <- simulate(sfm2, only_stocks = FALSE)
pl2 <- plot(sim2, xlim = c(0, 65), vars = c("D", "E", "P"))

pl <- plotly::subplot(pl1, pl2, nrows = 1) |>
  plotly::layout(title = "Linear versus Logistic Functional Form")
pl
```

### Noise

Pink noise implementation following Sterman (2000) Appendix B, p. 918.

``` r
sfm <- sfm0 |>
  build("pink", "stock", label = "Pink Noise") |>
  build("tau", "constant", eqn = 7, label = "Correlation Time of Noise") |>
  build("mu", "constant", eqn = 0, label = "Mean of Noise") |>
  build("sd", "constant", eqn = .125, label = "SD of Noise") |>
  build("white", "aux",
    eqn = "mu + sd * ( (24 * tau/dt)^0.5 ) * runif(1, -.5, .5)",
    label = "White Noise"
  ) |>
  build("to_pink", "flow",
    eqn = "(white - pink) / tau", to = "pink",
    label = "Change Pink Noise"
  ) |>
  build("D_D", eqn = "r_D * (K_D - D)/K_D + pink")

sim <- simulate(sfm, only_stocks = FALSE)
plot(sim, vars = vars)
```

### Removing Variables

``` r
sim1 <- simulate(sfm0)
pl1 <- plot(sim1)

sfm2 <- build(sfm0, "U", erase = TRUE)
sim2 <- simulate(sfm2)
pl2 <- plot(sim2, showlegend = FALSE)

pl <- plotly::subplot(pl1, pl2, nrows = 1)
pl
```

### Challenging the Model Boundary

``` r
# Make performance feedback to the system
sfm2 <- build(sfm0, "r_P", "constant",
  eqn = .2,
  label = "Effect Job Performance on Work Engagement"
) |>
  build("E_R", eqn = "(r_P * P + r_E_R * X * R * (1 + D)) * (K_E - E)/K_E")

sim <- simulate(sfm0, only_stocks = FALSE)
plot(sim, vars = vars)
```

``` r


sim2 <- simulate(sfm2, only_stocks = FALSE)
plot(sim2, vars = vars)
```

## Learning From Your Model

Set up threaded simulation:

``` r
use_threads(n = 4)
#> Warning in use_threads(n = 4): n is set to 4, which is higher than the number
#> of available cores minus 1. Setting it to 3.
```

### Establishing Boundary Conditions of Phenomena

``` r
# Simulation study
sfm0 <- sfm0 |> sim_specs(save_at = 5, language = "Julia") |>
  # Random initial conditions
  build(c("D", "R", "E", "X"), eqn = "runif(1, 0.01, 1)")
sfm <- sfm0

sims <- ensemble(sfm, n = 1000)
#> Running a total of 1000 simulations
#> Simulation took 9.0683 seconds
plot(sims, vars = c("E", "D"))
```

#### Parameter Variation

Vary $r_{X_{D}}$: the rate of energy depletion from demand.

``` r
sims <- ensemble(sfm,
  n = 100,
  range = list("r_X_D" = c(0.175, 0.15, 0.1)),
  return_sims = TRUE
)
#> Running a total of 300 simulations for 3 conditions (100 simulations per condition)
#> Simulation took 1.9272 seconds

plot(sims,
  type = "sims", i = 1:100, alpha = .25,
  nrows = 1, central_tendency = FALSE, vars = c("E", "D")
)
```

### “What If” Scenarios: Developing Interventions

``` r
# Early intervention
sfm <- sfm0 |>
  build("start", "constant", eqn = 14) |>
  build("input", "constant", 
        eqn = "pulse(times, start, height = .1, width = 14)") |>
  build("r_X_D", change_type = "stock") |>
  build("intervention", "flow", from = "r_X_D", 
        eqn = "r_X_D * input(t)", label = "Intervention")

# Add different seed to show improvement from intervention
sim <- simulate(sfm |> sim_specs(seed = 1234))
plot(sim)
```

``` r
# Simulation study
sims <- ensemble(sfm, n = 100, return_sims = TRUE)
#> Running a total of 100 simulations
#> Simulation took 1.1262 seconds
plot(sims,
  type = "sims", i = 1:100, alpha = .25, central_tendency = FALSE,
  vars = c("E", "D", "r_X_D")
)
```

We increase the simulation length and only save the last timepoint to
compute the effectiveness of the intervention.

``` r
sfm <- sfm |> sim_specs(stop = 1000, save_from = 1000)
sims <- ensemble(sfm, n = 1000, return_sims = TRUE)
#> Running a total of 1000 simulations
#> Simulation took 20.5202 seconds
y <- sims$df[sims$df$variable == "X", "value"]
tab <- table(round(y, 4)) |>
  prop.table() |>
  as.data.frame()
colnames(tab) <- c("Energy", "Proportion")
tab
#>   Energy Proportion
#> 1      0      0.414
#> 2 0.6786      0.586
```

A later intervention is not effective.

``` r
sfm <- build(sfm, "start", eqn = 28)
sims <- ensemble(sfm, n = 1000, return_sims = TRUE)
#> Running a total of 1000 simulations
#> Simulation took 20.6512 seconds

y <- sims$df[sims$df$variable == "X", "value"]
tab <- table(round(y, 4)) |>
  prop.table() |>
  as.data.frame()
colnames(tab) <- c("Energy", "Proportion")
tab
#>   Energy Proportion
#> 1      0          1
```

### Deriving Testable Hypotheses

``` r
# What treatment works after burnout has occurred?
sfm <- sfm0 |>
  sim_specs(stop = 500, save_at = 10) |>
  build("start", "constant", eqn = 200) |>
  build("input", "constant", eqn = "step(times, start)")

sfm1 <- build(sfm, "reduce_demands", "flow", from = "D", eqn = "D * input(t)")
sfm2 <- build(sfm, "add_resources", "flow", to = "R", eqn = ".05 * input(t)")
sfm3 <- build(sfm, "add_engagement", "flow", to = "E", eqn = ".1 * input(t)")
sfm4 <- build(sfm, "add_energy", "flow", to = "X", eqn = ".1 * input(t)")

sim1 <- simulate(sfm1)
sim2 <- simulate(sfm2)
sim3 <- simulate(sfm3)
sim4 <- simulate(sfm4)

pl1 <- plot(sim1)
pl2 <- plot(sim2, showlegend = FALSE)
pl3 <- plot(sim3, showlegend = FALSE)
pl4 <- plot(sim4, showlegend = FALSE)

pl <- plotly::subplot(pl1, pl2, pl3, pl4,
  nrows = 2,
  margin = c(0.05, 0.1, 0.05, 0.05)
)
pl
```

## Session Information

``` r
sessionInfo()
#> R version 4.5.2 (2025-10-31)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.3 LTS
#> 
#> Matrix products: default
#> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
#> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
#> 
#> locale:
#>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
#>  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
#>  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
#> [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
#> 
#> time zone: UTC
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] sdbuildR_1.0.7.9001
#> 
#> loaded via a namespace (and not attached):
#>  [1] styler_1.11.0        DiagrammeR_1.0.11    tidyr_1.3.1         
#>  [4] plotly_4.11.0        sass_0.4.10          generics_0.1.4      
#>  [7] stringi_1.8.7        digest_0.6.38        magrittr_2.0.4      
#> [10] evaluate_1.0.5       grid_4.5.2           RColorBrewer_1.1-3  
#> [13] fastmap_1.2.0        R.oo_1.27.1          R.cache_0.17.0      
#> [16] jsonlite_2.0.0       R.utils_2.13.0       deSolve_1.40        
#> [19] httr_1.4.7           purrr_1.2.0          crosstalk_1.2.2     
#> [22] viridisLite_0.4.2    scales_1.4.0         lazyeval_0.2.2      
#> [25] textshaping_1.0.4    jquerylib_0.1.4      cli_3.6.5           
#> [28] rlang_1.1.6          R.methodsS3_1.8.2    visNetwork_2.1.4    
#> [31] cachem_1.1.0         yaml_2.3.10          parallel_4.5.2      
#> [34] tools_4.5.2          JuliaConnectoR_1.1.4 dplyr_1.1.4         
#> [37] ggplot2_4.0.1        vctrs_0.6.5          R6_2.6.1            
#> [40] lifecycle_1.0.4      stringr_1.6.0        fs_1.6.6            
#> [43] htmlwidgets_1.6.4    ragg_1.5.0           pkgconfig_2.0.3     
#> [46] desc_1.4.3           pkgdown_2.2.0        pillar_1.11.1       
#> [49] bslib_0.9.0          gtable_0.3.6         data.table_1.17.8   
#> [52] glue_1.8.0           systemfonts_1.3.1    xfun_0.54           
#> [55] tibble_3.3.0         tidyselect_1.2.1     rstudioapi_0.17.1   
#> [58] knitr_1.50           farver_2.1.2         htmltools_0.5.8.1   
#> [61] igraph_2.2.1         rmarkdown_2.30       compiler_4.5.2      
#> [64] S7_0.2.1
```
