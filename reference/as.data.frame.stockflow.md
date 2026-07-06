# Convert stock-and-flow model to data frame

Create a data frame with properties of all model variables and
functions. Specify the variable types, variable names, and/or properties
to get a subset of the data frame.

## Usage

``` r
# S3 method for class 'stockflow'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  vars = NULL,
  type = NULL,
  properties = NULL,
  ...
)
```

## Arguments

- x:

  A stock-and-flow model object of class
  [`stockflow`](https://kcevers.github.io/sdbuildR/reference/stockflow.md).

- row.names:

  `NULL` or a character vector giving the row names for the data frame.
  Missing values are not allowed.

- optional:

  Ignored parameter.

- vars:

  Variable names to retain in the data frame. Defaults to `NULL` to
  include all variables.

- type:

  Variable types to retain in the data frame. Must be one or more of
  'stock', 'flow', 'constant', 'aux', 'lookup', or 'func'. Defaults to
  `NULL` to include all types.

- properties:

  Variable properties to retain in the data frame. Defaults to `NULL` to
  include all properties.

- ...:

  Optional arguments

## Value

A data.frame with one row per model component. Common columns include
`type` (component type), `name` (variable name), `eqn` (equation), and
`label` (descriptive label). Additional columns may include `to`,
`from`, `non_negative`, and others depending on variable types. The
exact columns returned depend on the `type` and `properties` arguments.
Returns an empty data.frame if no components match the filters.

## Examples

``` r
as.data.frame(stockflow("sir"))
#>       type             name                                     eqn
#> 1    stock         infected                                       1
#> 2    stock        recovered                                       0
#> 3    stock      susceptible                                   99999
#> 4     flow   new_infections infection_rate * susceptible * infected
#> 5     flow   new_recoveries                recovery_rate * infected
#> 6 constant     contact_rate                                       2
#> 7 constant   infection_rate         contact_rate / total_population
#> 8 constant    recovery_rate                                     0.1
#> 9 constant total_population      susceptible + infected + recovered
#>              label        to        from non_negative xpts ypts
#> 1         Infected      <NA>        <NA>        FALSE NULL NULL
#> 2        Recovered      <NA>        <NA>        FALSE NULL NULL
#> 3      Susceptible      <NA>        <NA>        FALSE NULL NULL
#> 4   New infections  infected susceptible        FALSE NULL NULL
#> 5   New recoveries recovered    infected        FALSE NULL NULL
#> 6     Contact rate      <NA>        <NA>        FALSE NULL NULL
#> 7   Infection rate      <NA>        <NA>        FALSE NULL NULL
#> 8    Recovery rate      <NA>        <NA>        FALSE NULL NULL
#> 9 Total population      <NA>        <NA>        FALSE NULL NULL

# Only show stocks
as.data.frame(stockflow("sir"), type = "stock")
#>    type        name   eqn       label non_negative xpts ypts
#> 1 stock    infected     1    Infected        FALSE NULL NULL
#> 2 stock   recovered     0   Recovered        FALSE NULL NULL
#> 3 stock susceptible 99999 Susceptible        FALSE NULL NULL

# Only show specific variables
as.data.frame(stockflow("sir"), vars = c("susceptible", "infected"))
#>    type        name   eqn       label non_negative xpts ypts
#> 1 stock    infected     1    Infected        FALSE NULL NULL
#> 2 stock susceptible 99999 Susceptible        FALSE NULL NULL

# Only show equation and label
as.data.frame(stockflow("sir"), properties = c("eqn", "label"))
#>       type             name                                     eqn
#> 1    stock         infected                                       1
#> 2    stock        recovered                                       0
#> 3    stock      susceptible                                   99999
#> 4     flow   new_infections infection_rate * susceptible * infected
#> 5     flow   new_recoveries                recovery_rate * infected
#> 6 constant     contact_rate                                       2
#> 7 constant   infection_rate         contact_rate / total_population
#> 8 constant    recovery_rate                                     0.1
#> 9 constant total_population      susceptible + infected + recovered
#>              label
#> 1         Infected
#> 2        Recovered
#> 3      Susceptible
#> 4   New infections
#> 5   New recoveries
#> 6     Contact rate
#> 7   Infection rate
#> 8    Recovery rate
#> 9 Total population
```
