# Install, update, or remove Julia environment

Instantiate the Julia environment for sdbuildR to run stock-and-flow
models using Julia. For more guidance, see [this
vignette](https://kcevers.github.io/sdbuildR/articles/julia-setup.html).

## Usage

``` r
install_julia_env(remove = FALSE, quiet = FALSE)
```

## Arguments

- remove:

  If `TRUE`, remove Julia environment for sdbuildR. This will remove the
  SystemDynamicsBuildR.jl package and delete the environment directory
  (containing Project.toml and Manifest.toml). All other Julia packages
  remain untouched.

- quiet:

  If `TRUE`, suppress informational messages such as progress and status
  updates. Warnings and errors are always shown. Defaults to `FALSE`.

## Value

Invisibly returns `NULL` after instantiating the Julia environment.

## Details

`install_julia_env()` will:

- Start a Julia session

- Activate a Julia environment using sdbuildR's Project.toml

- Install SystemDynamicsBuildR.jl from GitHub
  (https://github.com/kcevers/SystemDynamicsBuildR.jl)

- Install all other required Julia packages

- Create Manifest.toml

- Precompile packages for faster subsequent loading

- Stop the Julia session

Note that this may take 10-25 minutes the first time as Julia downloads
and compiles packages.

## See also

[`use_julia()`](https://kcevers.github.io/sdbuildR/reference/use_julia.md)

## Examples

``` r
if (FALSE) { # \dontrun{
install_julia_env()

# Remove Julia environment
install_julia_env(remove = TRUE)
} # }
```
