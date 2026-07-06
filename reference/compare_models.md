# Compare two stock-and-flow models

Compares the structure, equations, and simulation settings of two
`stockflow` models.

## Usage

``` r
compare_models(sfm1, sfm2)
```

## Arguments

- sfm1:

  A stock-and-flow model of class
  [`stockflow`](https://kcevers.github.io/sdbuildR/reference/stockflow.md).

- sfm2:

  A stock-and-flow model of class
  [`stockflow`](https://kcevers.github.io/sdbuildR/reference/stockflow.md).

## Value

An object of class `compare_stockflow` (a list) containing:

- `labels`:

  Names of the two model objects (captured expressions).

- `added`:

  Variables present in `sfm2` but not `sfm1`.

- `removed`:

  Variables present in `sfm1` but not `sfm2`.

- `type_changed`:

  Variables with different types.

- `eqn_changed`:

  Variables with different equations.

- `sim_settings_diff`:

  Simulation settings that differ.

- `properties`:

  Per-model counts of variables by type.

## See also

[`simulate()`](https://kcevers.github.io/sdbuildR/reference/simulate.stockflow.md),
[`summary()`](https://kcevers.github.io/sdbuildR/reference/summary.stockflow.md)

## Examples

``` r
sfm1 <- stockflow("sir")
sfm2 <- stock(sfm1, "susceptible", eqn = 0.5)
compare_models(sfm1, sfm2)
#> 
#> ── Stock-and-Flow Comparison: sfm1 vs sfm2 ─────────────────────────────────────
#> 
#> ── Structural Differences ──
#> ! Equation changed: `susceptible`: `99999` → `0.5`
#> 
#> ── Simulation Settings ──
#> ✔ Identical
#> 
#> ── Model Properties ──
#> Stocks 3 3
#> Flows 2 2
#> Auxiliaries 0 0
#> Constants 4 4
#> Lookups 0 0
```
