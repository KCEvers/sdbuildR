# Create, modify or remove a global variable or function

Macros are global variables or functions that can be used throughout
your stock-and-flow model. `macro()` adds, changes, or erases a macro.

## Usage

``` r
macro(sfm, name, eqn = "0.0", doc = "", change_name = NULL, erase = FALSE)
```

## Arguments

- sfm:

  Stock-and-flow model, object of class
  [`sdbuildR_xmile`](https://kcevers.github.io/sdbuildR/reference/xmile.md).

- name:

  Name of the macro. The equation will be assigned to this name.

- eqn:

  Equation of the macro. A character vector. Defaults to "0.0".

- doc:

  Documentation of the macro. Defaults to "".

- change_name:

  New name for macro (optional). Defaults to NULL to indicate no change.

- erase:

  If TRUE, remove macro from the model. Defaults to FALSE.

## Value

A stock-and-flow model object of class
[`sdbuildR_xmile`](https://kcevers.github.io/sdbuildR/reference/xmile.md)

## Examples

``` r
# Simple function
sfm <- xmile() |>
  macro("double", eqn = "function(x) x * 2") |>
  build("a", "constant", eqn = "double(2)")

# Function with defaults
sfm <- xmile() |>
  macro("scale", eqn = "function(x, factor = 10) x * factor") |>
  build("b", "constant", eqn = "scale(2)")

# If the logistic() function did not exist, you could create it yourself:
sfm <- macro(xmile(), "func", eqn = "function(x, slope = 1, midpoint = .5){
   1 / (1 + exp(-slope*(x-midpoint)))
 }") |>
  build("c", "constant", eqn = "func(2, slope = 50)")
```
