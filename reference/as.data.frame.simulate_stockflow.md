# Create data frame of simulation results

Convert simulation results to a data.frame.

## Usage

``` r
# S3 method for class 'simulate_stockflow'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  direction = "long",
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

- direction:

  Format of data frame, either "long" (default) or "wide".

- vars:

  Variable names to retain in the data frame. Defaults to `NULL` to
  include all variables.

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

[`simulate()`](https://kcevers.github.io/sdbuildR/reference/simulate.stockflow.md),
[`stockflow()`](https://kcevers.github.io/sdbuildR/reference/stockflow.md)

## Examples

``` r
sfm <- stockflow("sir")
sim <- simulate(sfm)
df <- as.data.frame(sim)
head(df)
#>   time variable    value
#> 1 0.00 infected 1.000000
#> 2 0.01 infected 1.019000
#> 3 0.02 infected 1.038361
#> 4 0.03 infected 1.058089
#> 5 0.04 infected 1.078193
#> 6 0.05 infected 1.098678

# Get results in wide format
df_wide <- as.data.frame(sim, direction = "wide")
head(df_wide)
#>   time infected   recovered susceptible
#> 1 0.00 1.000000 0.000000000    99999.00
#> 2 0.01 1.019000 0.001000000    99998.98
#> 3 0.02 1.038361 0.002019000    99998.96
#> 4 0.03 1.058089 0.003057360    99998.94
#> 5 0.04 1.078193 0.004115450    99998.92
#> 6 0.05 1.098678 0.005193642    99998.90
```
