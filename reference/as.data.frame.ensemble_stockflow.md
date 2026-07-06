# Create data frame of simulation results

Convert simulation results to a data.frame.

## Usage

``` r
# S3 method for class 'ensemble_stockflow'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  which = c("summary", "sims")[1],
  direction = "long",
  sim = NULL,
  condition = NULL,
  vars = NULL,
  type = NULL,
  ...
)
```

## Arguments

- x:

  Output of
  [`simulate()`](https://kcevers.github.io/sdbuildR/reference/simulate.stockflow.md).

- row.names:

  NULL or a character vector giving the row names for the data frame.
  Missing values are not allowed.

- optional:

  Ignored parameter.

- which:

  Type of data to return. Either `"summary"` for a summary statistics,
  or `"sims"` for individual simulation trajectories. Defaults to
  `"summary"`.

- direction:

  Format of data frame, either "long" (default) or "wide".

- sim:

  Indices of the individual trajectories to include if which = `"sims"`.
  Defaults to `NULL`, which includes all trajectories. Including a high
  number of trajectories will create a large object.

- condition:

  Indices of the conditions to include. Defaults to `NULL`, which
  includes all conditions.

- vars:

  Variables to plot. Defaults to `NULL` to plot all variables.

- type:

  Variable types to retain in the data frame. Must be one or more of
  'stock', 'flow', 'constant', 'aux', 'lookup', or 'func'. Defaults to
  `NULL` to include all types.

- ...:

  Optional parameters

## Value

A data.frame with simulation results. For `direction = "long"`
(default), the data frame has three columns: `time`, `variable`, and
`value`. For `direction = "wide"`, the data frame has columns `time`
followed by one column per variable.

## See also

[`ensemble()`](https://kcevers.github.io/sdbuildR/reference/ensemble.md),
[`stockflow()`](https://kcevers.github.io/sdbuildR/reference/stockflow.md)

## Examples

``` r
sfm <- stockflow("sir")
sims <- ensemble(sfm, n = 10)
#> Starting ensemble simulation in "R" with 10 simulations.
#> ✔ Ensemble simulation completed in 0.39 seconds.
df <- as.data.frame(sims)
head(df)
#>   condition variable time     mean   median missing_count   quant1   quant2
#> 1         1 infected 0.00 1.000000 1.000000             0 1.000000 1.000000
#> 2         1 infected 0.01 1.019000 1.019000             0 1.019000 1.019000
#> 3         1 infected 0.02 1.038361 1.038361             0 1.038361 1.038361
#> 4         1 infected 0.03 1.058089 1.058089             0 1.058089 1.058089
#> 5         1 infected 0.04 1.078193 1.078193             0 1.078193 1.078193
#> 6         1 infected 0.05 1.098678 1.098678             0 1.098678 1.098678

# Get results in wide format
df_wide <- as.data.frame(sims, direction = "wide")
head(df_wide)
#>   condition time mean.infected median.infected missing_count.infected
#> 1         1 0.00      1.000000        1.000000                      0
#> 2         1 0.01      1.019000        1.019000                      0
#> 3         1 0.02      1.038361        1.038361                      0
#> 4         1 0.03      1.058089        1.058089                      0
#> 5         1 0.04      1.078193        1.078193                      0
#> 6         1 0.05      1.098678        1.098678                      0
#>   quant1.infected quant2.infected mean.recovered median.recovered
#> 1        1.000000        1.000000    0.000000000      0.000000000
#> 2        1.019000        1.019000    0.001000000      0.001000000
#> 3        1.038361        1.038361    0.002019000      0.002019000
#> 4        1.058089        1.058089    0.003057360      0.003057360
#> 5        1.078193        1.078193    0.004115450      0.004115450
#> 6        1.098678        1.098678    0.005193642      0.005193642
#>   missing_count.recovered quant1.recovered quant2.recovered mean.susceptible
#> 1                       0      0.000000000      0.000000000         99999.00
#> 2                       0      0.001000000      0.001000000         99998.98
#> 3                       0      0.002019000      0.002019000         99998.96
#> 4                       0      0.003057360      0.003057360         99998.94
#> 5                       0      0.004115450      0.004115450         99998.92
#> 6                       0      0.005193642      0.005193642         99998.90
#>   median.susceptible missing_count.susceptible quant1.susceptible
#> 1           99999.00                         0           99999.00
#> 2           99998.98                         0           99998.98
#> 3           99998.96                         0           99998.96
#> 4           99998.94                         0           99998.94
#> 5           99998.92                         0           99998.92
#> 6           99998.90                         0           99998.90
#>   quant2.susceptible
#> 1           99999.00
#> 2           99998.98
#> 3           99998.96
#> 4           99998.94
#> 5           99998.92
#> 6           99998.90
```
