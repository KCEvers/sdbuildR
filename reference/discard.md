# Remove variable(s)

Remove variable(s) from a stock-and-flow model. All references in flow
connections and graphical function sources are also removed. A warning
will be thrown if any lingering references to the removed name remain in
the model.

## Usage

``` r
discard(
  object,
  name,
  remove_references = c("to", "from", "source", "unit_test")
)
```

## Arguments

- object:

  Stock-and-flow model, object of class
  [`stockflow`](https://kcevers.github.io/sdbuildR/reference/stockflow.md).

- name:

  Name(s) to remove. Accepts bare symbols (e.g., `x`), strings, or
  vectors via [`c()`](https://rdrr.io/r/base/c.html). Must be variable
  names.

- remove_references:

  Where to remove references to the discarded variables. By default,
  references to discarded variables in `"to"`, `"from"`, `"source"`, and
  `"unit_test"` are removed. Set to `NULL` to keep all references (not
  recommended). Note that any lingering references in equations will
  cause errors in simulation and should be removed or updated with
  [`update()`](https://rdrr.io/r/stats/update.html) after discarding the
  variable.

## Value

A stock-and-flow model object of class
[`stockflow`](https://kcevers.github.io/sdbuildR/reference/stockflow.md)

## See also

[`update()`](https://rdrr.io/r/stats/update.html),
[`change_name()`](https://kcevers.github.io/sdbuildR/reference/change_name.md)

## Examples

``` r
# Add stock
sfm <- stockflow() |> stock(x)
print(sfm)
#> 
#> ── Stock-and-Flow Model ────────────────────────────────────────────────────────
#> 1 stock
#> 
#> ── Stock-Flow Structure ──
#> x: (no flows)
#> 
#> ── Simulation Settings ──
#> Time: 0 to 100 seconds (dt = 0.01) • euler • R
#> Simulation output: stocks only

# Remove stock
sfm <- discard(sfm, x)
print(sfm)
#> 
#> ── Stock-and-Flow Model ────────────────────────────────────────────────────────
#> ℹ Empty model without any variables.
#> 
#> ── Simulation Settings ──
#> Time: 0 to 100 seconds (dt = 0.01) • euler • R
#> Simulation output: stocks only
```
