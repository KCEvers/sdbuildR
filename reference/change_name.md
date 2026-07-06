# Change name of variable

Change the name of a variable throughout the model. This updates the
data frame and all references in equations, flow connections, and
labels.

## Usage

``` r
change_name(object, name, new_name)
```

## Arguments

- object:

  Stock-and-flow model, object of class
  [`stockflow`](https://kcevers.github.io/sdbuildR/reference/stockflow.md).

- name:

  Variable name. Accepts a bare symbol (e.g., `population`), a string
  (`"population"`), or a vector via
  [`c()`](https://rdrr.io/r/base/c.html) (e.g., `c(a, b)` or
  `c("a", "b")`). Use `!!` to inject from a variable.

- new_name:

  New name. Character vector of the same length as `name`. Must be
  unique across all existing variables.

## Value

A stock-and-flow model object of class
[`stockflow`](https://kcevers.github.io/sdbuildR/reference/stockflow.md)
with the name changed throughout the model.

## See also

[`update()`](https://rdrr.io/r/stats/update.html),
[`discard()`](https://kcevers.github.io/sdbuildR/reference/discard.md)

## Examples

``` r
sfm <- stockflow("sir")
sfm <- change_name(sfm, c(susceptible, infected, recovered),
  new_name = c(S, I, R)
)
print(sfm)
#> 
#> ── Stock-and-Flow Model: Susceptible-Infected-Recovered (SIR) ──────────────────
#> 3 stocks • 2 flows • 4 constants
#> 
#> ── Stock-Flow Structure ──
#> I: + new_infections - new_recoveries
#> R: + new_recoveries
#> S: - new_infections
#> 
#> ── Other Variables ──
#> Constants: `contact_rate`, `infection_rate`, `recovery_rate`, and
#> `total_population`
#> 
#> ── Simulation Settings ──
#> Time: 0.0 to 20.0 weeks (dt = 0.01) • euler • R
#> Simulation output: stocks only

# References to old names are updated
as.data.frame(sfm, type = "flow", properties = c("name", "eqn", "to", "from"))
#>   type           name                    eqn to from
#> 1 flow new_infections infection_rate * S * I  I    S
#> 2 flow new_recoveries      recovery_rate * I  R    I
```
