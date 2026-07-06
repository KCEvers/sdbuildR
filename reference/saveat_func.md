# Internal function to save data frame at specific times

Internal function used to save the data frame at specific times in case
the save interval (`save_by`) is not equal to dt in the simulation
specifications.

## Usage

``` r
saveat_func(df, time_col, new_times)
```

## Arguments

- df:

  data.frame in wide format

- time_col:

  Name of the time column

- new_times:

  Vector of new times to save the data frame at

## Value

Interpolated data.frame. The data frame has columns `time` followed by
one column per variable.

## Examples

``` r
# Recommended: Use save_by in sim_settings() to downsample simulations
sfm <- stockflow("sir") |> sim_settings(dt = 0.01, save_by = 1)
sim <- simulate(sfm)
df <- as.data.frame(sim)
nrow(df) # Returns only times at intervals of 1
#> [1] 63
head(df)
#>   time variable        value
#> 1    0 infected     1.000000
#> 2    1 infected     6.567386
#> 3    2 infected    43.116029
#> 4    3 infected   282.439436
#> 5    4 infected  1823.799110
#> 6    5 infected 10783.794761

# The saveat_func() is the underlying function used by simulate()
# Direct use is not recommended, but shown here for completeness:
sfm <- sfm |> sim_settings(save_by = 0.01)
sim <- simulate(sfm)
df <- as.data.frame(sim)
nrow(df) # Many more rows
#> [1] 6003

# Manual downsampling (not recommended - use save_by instead)
new_times <- seq(min(df$time), max(df$time), by = 1)
df_wide <- as.data.frame(sim, direction = "wide")
df_manual <- saveat_func(df_wide, "time", new_times)
nrow(df_manual)
#> [1] 21
```
