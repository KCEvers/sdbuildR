# Convert stock-and-flow model to data frame

Create a data frame with properties of all model variables, model units,
and macros. Specify the variable types, variable names, and/or
properties to get a subset of the data frame.

## Usage

``` r
# S3 method for class 'sdbuildR_xmile'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  type = NULL,
  name = NULL,
  properties = NULL,
  ...
)
```

## Arguments

- x:

  A stock-and-flow model object of class
  [`sdbuildR_xmile`](https://kcevers.github.io/sdbuildR/reference/xmile.md).

- row.names:

  NULL or a character vector giving the row names for the data frame.
  Missing values are not allowed.

- optional:

  Ignored parameter.

- type:

  Variable types to retain in the data frame. Must be one or more of
  'stock', 'flow', 'constant', 'aux', 'gf', 'macro', or 'model_units'.
  Defaults to NULL to include all types.

- name:

  Variable names to retain in the data frame. Defaults to NULL to
  include all variables.

- properties:

  Variable properties to retain in the data frame. Defaults to NULL to
  include all properties.

- ...:

  Optional arguments

## Value

A data.frame with one row per model component (variable, unit
definition, or macro). Common columns include `type` (component type),
`name` (variable name), `eqn` (equation), `units` (units of
measurement), and `label` (descriptive label). Additional columns may
include `to`, `from`, `non_negative`, and others depending on variable
types. The exact columns returned depend on the `type` and `properties`
arguments. Returns an empty data.frame if no components match the
filters.

## Examples

``` r
as.data.frame(xmile("SIR"))
#>        type                   name                                       eqn
#> 1     stock            Susceptible                                     99999
#> 2     stock               Infected                                         1
#> 3     stock              Recovered                                       0.0
#> 4  constant                   Beta Effective_Contact_Rate / Total_Population
#> 5  constant       Total_Population                                    100000
#> 6  constant Effective_Contact_Rate                                         2
#> 7  constant                  Delay                                         2
#> 8       aux                 Lambda                           Beta * Infected
#> 9      flow         Infection_Rate                      Susceptible * Lambda
#> 10     flow          Recovery_Rate                          Infected / Delay
#>    units                  label        to        from non_negative
#> 1      1            Susceptible      <NA>        <NA>        FALSE
#> 2      1               Infected      <NA>        <NA>        FALSE
#> 3      1              Recovered      <NA>        <NA>        FALSE
#> 4      1                   Beta      <NA>        <NA>        FALSE
#> 5      1       Total_Population      <NA>        <NA>        FALSE
#> 6      1 Effective_Contact_Rate      <NA>        <NA>        FALSE
#> 7      1                  Delay      <NA>        <NA>        FALSE
#> 8      1                 Lambda      <NA>        <NA>        FALSE
#> 9      1         Infection_Rate  Infected Susceptible        FALSE
#> 10     1          Recovery_Rate Recovered    Infected        FALSE
#>                                     eqn_julia
#> 1                                     99999.0
#> 2                                         1.0
#> 3                                         0.0
#> 4  Effective_Contact_Rate ./ Total_Population
#> 5                                    100000.0
#> 6                                         2.0
#> 7                                         2.0
#> 8                            Beta .* Infected
#> 9                       Susceptible .* Lambda
#> 10                          Infected ./ Delay

# Only show stocks
as.data.frame(xmile("SIR"), type = "stock")
#>    type        name   eqn units       label non_negative eqn_julia
#> 1 stock Susceptible 99999     1 Susceptible        FALSE   99999.0
#> 2 stock    Infected     1     1    Infected        FALSE       1.0
#> 3 stock   Recovered   0.0     1   Recovered        FALSE       0.0

# Only show equation and label
as.data.frame(xmile("SIR"), properties = c("eqn", "label"))
#>        type                   name                                       eqn
#> 1     stock            Susceptible                                     99999
#> 2     stock               Infected                                         1
#> 3     stock              Recovered                                       0.0
#> 4  constant                   Beta Effective_Contact_Rate / Total_Population
#> 5  constant       Total_Population                                    100000
#> 6  constant Effective_Contact_Rate                                         2
#> 7  constant                  Delay                                         2
#> 8       aux                 Lambda                           Beta * Infected
#> 9      flow         Infection_Rate                      Susceptible * Lambda
#> 10     flow          Recovery_Rate                          Infected / Delay
#>                     label
#> 1             Susceptible
#> 2                Infected
#> 3               Recovered
#> 4                    Beta
#> 5        Total_Population
#> 6  Effective_Contact_Rate
#> 7                   Delay
#> 8                  Lambda
#> 9          Infection_Rate
#> 10          Recovery_Rate
```
