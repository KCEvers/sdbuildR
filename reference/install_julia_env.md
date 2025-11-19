# Install, update, or remove Julia environment

Instantiate the Julia environment for sdbuildR to run stock-and-flow
models using Julia. For more guidance, please see [this
vignette](https://kcevers.github.io/sdbuildR/articles/julia-setup.html).

## Usage

``` r
install_julia_env(remove = FALSE)
```

## Arguments

- remove:

  If TRUE, remove Julia environment for sdbuildR. This will delete the
  Manifest.toml file, as well as the SystemDynamicsBuildR.jl package.
  All other Julia packages remain untouched.

## Value

Invisibly returns NULL after instantiating the Julia environment.

## Details

`install_julia_env()` will:

- Start a Julia session

- Activate a Julia environment using sdbuildR's Project.toml

- Install SystemDynamicsBuildR.jl from GitHub
  (https://github.com/KCEvers/SystemDynamicsBuildR.jl)

- Install all other required Julia packages

- Create Manifest.toml

- Precompile packages for faster subsequent loading

- Stop the Julia session

Note that this may take 10-25 minutes the first time as Julia downloads
and compiles packages.

## See also

[`use_julia()`](https://kcevers.github.io/sdbuildR/reference/use_julia.md),
[`julia_status()`](https://kcevers.github.io/sdbuildR/reference/julia_status.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  install_julia_env()

  # Remove Julia environment
  install_julia_env(remove = TRUE)
} # }
```
