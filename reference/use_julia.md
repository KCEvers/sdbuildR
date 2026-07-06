# Start Julia and activate environment

Start Julia session and activate Julia environment to simulate
stock-and-flow models. To do so, Julia needs to be installed (see
<https://julialang.org/install/>) and findable from within R. See [this
vignette](https://kcevers.github.io/sdbuildR/articles/julia-setup.html)
for guidance. In addition, the Julia environment specifically for
sdbuildR needs to have been instantiated. This can be set up with
[`install_julia_env()`](https://kcevers.github.io/sdbuildR/reference/install_julia_env.md).

## Usage

``` r
use_julia(stop = FALSE, restart = FALSE, nthreads = NULL, quiet = FALSE)
```

## Arguments

- stop:

  If `TRUE`, stop active Julia session. Defaults to `FALSE`.

- restart:

  If `TRUE`, force Julia session to restart.

- nthreads:

  If not `NULL`, set the number of threads for Julia to use. This will
  temporarily set the environment variable `JULIA_NUM_THREADS` and
  restart Julia if it is already running to apply the new thread
  setting. See [this
  page](https://docs.julialang.org/en/v1/manual/parallel-computing/#man-parallel-computing)
  for more details on threading in Julia.

- quiet:

  If `TRUE`, suppress informational messages such as progress and status
  updates. Warnings and errors are always shown. Defaults to `FALSE`.

## Value

Returns `TRUE` invisibly after setting up the Julia environment, and
`NULL` invisibly if Julia was already set up or when stopping Julia with
`stop = TRUE`. Used for side effects.

## Details

In every R session, `use_julia()` needs to be run once (which is done
automatically in
[`simulate()`](https://kcevers.github.io/sdbuildR/reference/simulate.stockflow.md)),
which can take around 30-60 seconds.

## See also

[`install_julia_env()`](https://kcevers.github.io/sdbuildR/reference/install_julia_env.md)

## Examples

``` r
# Start a Julia session and activate the Julia environment for sdbuildR
use_julia()
#> ℹ Activating Julia environment for sdbuildR at
#>   /home/runner/.local/share/R/sdbuildR/julia...
#> ✔ Julia environment ready.

# Start Julia with 2 threads (only works if threading is supported)
use_julia(nthreads = 2)
#> ℹ Activating Julia environment for sdbuildR at
#>   /home/runner/.local/share/R/sdbuildR/julia...
#> ✔ Julia environment ready with 2 threads.

# Restart Julia session (in case of issues)
use_julia(restart = TRUE)
#> ✔ Closed Julia session.
#> ℹ Activating Julia environment for sdbuildR at
#>   /home/runner/.local/share/R/sdbuildR/julia...
#> ✔ Julia environment ready.

# Stop Julia session
use_julia(stop = TRUE)
#> ✔ Closed Julia session.
```
