# Check status of Julia installation and environment

Check if Julia can be found and if the Julia environment for sdbuildR
has been instantiated. Note that this does not mean a Julia session has
been started, merely whether it *could* be. For more guidance, please
see [this
vignette](https://kcevers.github.io/sdbuildR/articles/julia-setup.html).

## Usage

``` r
julia_status(verbose = TRUE)
```

## Arguments

- verbose:

  If TRUE, print detailed status information. Defaults to TRUE.

## Value

A list with components:

- julia_found:

  Logical. TRUE if Julia installation found.

- julia_version:

  Character. Julia version string, or "" if not found.

- env_exists:

  Logical. TRUE if Project.toml exists in sdbuildR package, which
  specifies the Julia packages and versions needed to instantiate the
  Julia environment for sdbuildR.

- env_instantiated:

  Logical. TRUE if Manifest.toml exists (i.e., Julia environment was
  instantiated).

- status:

  Character. Overall status: "julia_not_installed",
  "julia_needs_update", "sdbuildR_needs_reinstall", "install_julia_env",
  "ready", or "unknown".

## What to Do Next

Based on the 'status' value:

- "julia_not_installed":

  Install Julia from <https://julialang.org/install/>

- "julia_needs_update":

  Update Julia to \>= version 1.10

- "install_julia_env":

  Run
  [`install_julia_env()`](https://kcevers.github.io/sdbuildR/reference/install_julia_env.md)

- "ready":

  Run
  [`use_julia()`](https://kcevers.github.io/sdbuildR/reference/use_julia.md)
  to start a session

## Examples

``` r
status <- julia_status()
#> Julia environment is ready to be activated.
print(status)
#> $julia_found
#> [1] TRUE
#> 
#> $julia_version
#> [1] "1.12.1"
#> 
#> $env_exists
#> [1] TRUE
#> 
#> $env_instantiated
#> [1] TRUE
#> 
#> $status
#> [1] "ready"
#> 
```
