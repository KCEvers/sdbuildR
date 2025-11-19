# Set up threaded ensemble simulations

Specify the number of threads for ensemble simulations in Julia. This
will not overwrite your current global setting for JULIA_NUM_THREADS.
Note that this does not affect regular simulations with
[`simulate()`](https://kcevers.github.io/sdbuildR/reference/simulate.md).

## Usage

``` r
use_threads(n = parallel::detectCores() - 1, stop = FALSE)
```

## Arguments

- n:

  Number of Julia threads to use. Defaults to
  parallel::detectCores() - 1. If set to a value higher than the number
  of available cores minus 1, it will be set to the number of available
  cores minus 1.

- stop:

  Stop using threaded ensemble simulations. Defaults to FALSE.

## Value

No return value, called for side effects

## See also

[`ensemble()`](https://kcevers.github.io/sdbuildR/reference/ensemble.md),
[`use_julia()`](https://kcevers.github.io/sdbuildR/reference/use_julia.md)

## Examples

``` r
# Use Julia with 4 threads
use_julia()
#> Starting Julia ...
#> Connecting to Julia TCP server at localhost:11980 ...
#> Setting up Julia environment for sdbuildR...
use_threads(n = 4)
#> Warning: n is set to 4, which is higher than the number of available cores minus 1. Setting it to 3.

# Stop using threads
use_threads(stop = TRUE)

# Stop using Julia
use_julia(stop = TRUE)
#> Julia session closed.
```
