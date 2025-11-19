# Create data frame of simulation results

Convert simulation results to a data.frame.

## Usage

``` r
# S3 method for class 'sdbuildR_sim'
as.data.frame(x, row.names = NULL, optional = FALSE, direction = "long", ...)
```

## Arguments

- x:

  Output of simulate().

- row.names:

  NULL or a character vector giving the row names for the data frame.
  Missing values are not allowed.

- optional:

  Ignored parameter.

- direction:

  Format of data frame, either "long" (default) or "wide".

- ...:

  Optional parameters

## Value

A data.frame with simulation results. For `direction = "long"`
(default), the data frame has three columns: `time`, `variable`, and
`value`. For `direction = "wide"`, the data frame has columns `time`
followed by one column per variable.

## See also

[`simulate()`](https://kcevers.github.io/sdbuildR/reference/simulate.md),
[`xmile()`](https://kcevers.github.io/sdbuildR/reference/xmile.md)

## Examples

``` r
sfm <- xmile("SIR")
sim <- simulate(sfm)
df <- as.data.frame(sim)
head(df)
#>   time variable    value
#> 1 0.00 Infected 1.000000
#> 2 0.01 Infected 1.015000
#> 3 0.02 Infected 1.030225
#> 4 0.03 Infected 1.045678
#> 5 0.04 Infected 1.061363
#> 6 0.05 Infected 1.077283

# Get results in wide format
df_wide <- as.data.frame(sim, direction = "wide")
head(df_wide)
#>   time Infected  Recovered Susceptible
#> 1 0.00 1.000000 0.00000000    99999.00
#> 2 0.01 1.015000 0.00500000    99998.98
#> 3 0.02 1.030225 0.01007500    99998.96
#> 4 0.03 1.045678 0.01522612    99998.94
#> 5 0.04 1.061363 0.02045451    99998.92
#> 6 0.05 1.077283 0.02576132    99998.90
```
