# Internal function to save data frame at specific times

Internal function used to save the data frame at specific times in case
save_at is not equal to dt in the simulation specifications.

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
# Recommended: Use save_at in sim_specs() to downsample simulations
sfm <- xmile("SIR") |> sim_specs(dt = 0.01, save_at = 1)
sim <- simulate(sfm)
df <- as.data.frame(sim)
nrow(df) # Returns only times at intervals of 1
#> [1] 63
head(df)
#>   time variable       value
#> 1    0 Infected    1.000000
#> 2    1 Infected    4.431808
#> 3    2 Infected   19.636874
#> 4    3 Infected   86.929468
#> 5    4 Infected  383.273923
#> 6    5 Infected 1660.347298

# The saveat_func() is the underlying function used by simulate()
# Direct use is not recommended, but shown here for completeness:
sfm <- sfm |> sim_specs(save_at = 0.01)
sim <- simulate(sfm)
df <- as.data.frame(sim)
nrow(df) # Many more rows
#> [1] 6003

# Manual downsampling (not recommended - use save_at instead)
new_times <- seq(min(df$time), max(df$time), by = 1)
df_wide <- as.data.frame(sim, direction = "wide")
df_manual <- saveat_func(df_wide, "time", new_times)
nrow(df_manual)
#> [1] 21
```
