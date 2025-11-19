# Start Julia and activate environment

Start Julia session and activate Julia environment to simulate
stock-and-flow models. To do so, Julia needs to be installed and
findable from within R. See [this
vignette](https://kcevers.github.io/sdbuildR/articles/julia-setup.html)
for guidance. In addition, the Julia environment specifically for
sdbuildR needs to have been instantiated. This can be set up with
[`install_julia_env()`](https://kcevers.github.io/sdbuildR/reference/install_julia_env.md).

## Usage

``` r
use_julia(stop = FALSE, force = FALSE)
```

## Arguments

- stop:

  If TRUE, stop active Julia session. Defaults to FALSE.

- force:

  If TRUE, force Julia setup to execute again.

## Value

Returns `NULL` invisibly, used for side effects

## Details

Julia supports running stock-and-flow models with units as well as
ensemble simulations (see
[`ensemble()`](https://kcevers.github.io/sdbuildR/reference/ensemble.md)).

In every R session, `use_julia()` needs to be run once (which is done
automatically in
[`simulate()`](https://kcevers.github.io/sdbuildR/reference/simulate.md)),
which can take around 30-60 seconds.

## See also

[`julia_status()`](https://kcevers.github.io/sdbuildR/reference/julia_status.md),
[`install_julia_env()`](https://kcevers.github.io/sdbuildR/reference/install_julia_env.md)

## Examples

``` r
# Start a Julia session and activate the Julia environment for sdbuildR
use_julia()
#> Starting Julia ...
#> Connecting to Julia TCP server at localhost:11980 ...
#> Setting up Julia environment for sdbuildR...

# Stop Julia session
use_julia(stop = TRUE)
#> Julia session closed.
```
