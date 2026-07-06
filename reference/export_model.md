# Export a stock-and-flow model

Export a model of class
[`stockflow`](https://kcevers.github.io/sdbuildR/reference/stockflow.md)
to another format.

## Usage

``` r
export_model(
  object,
  format = c("sdbuildR", "deSolve", "psychomodels"),
  file = NULL,
  title = object[["meta"]][["name"]],
  description = object[["meta"]][["caption"]],
  explanation = description,
  publication_doi = "",
  publication_citation = "",
  framework = "Ordinary Differential Equations",
  programming_language = "R",
  psychology_discipline = "",
  software_package = "",
  model_variable = "",
  code_repository_url = "",
  data_url = "",
  submission_remarks = "",
  created_by = "",
  updated_by = "",
  published_by = "",
  published_at = Sys.time(),
  published_pending_moderation_at = Sys.time(),
  publication_citation_fetched_at = Sys.time(),
  publication_csl_fetched_at = Sys.time(),
  publication_csl_json = "",
  id = NA,
  slug = NULL,
  include_latex = TRUE,
  pretty = TRUE
)
```

## Arguments

- object:

  Stock-and-flow model, object of class
  [`stockflow`](https://kcevers.github.io/sdbuildR/reference/stockflow.md).

- format:

  Export format. One of `"sdbuildR"`, `"deSolve"`, or `"psychomodels"`.

- file:

  Output file path, or `NULL` to return the result directly.

- title:

  \[psychomodels\] Model title. Defaults to
  `object[["meta"]][["name"]]`.

- description:

  \[psychomodels\] Model description. Defaults to
  `object[["meta"]][["caption"]]`.

- explanation:

  \[psychomodels\] Free-text explanation. Defaults to `description`.

- publication_doi:

  \[psychomodels\] DOI for the associated publication.

- publication_citation:

  \[psychomodels\] Citation text.

- framework:

  \[psychomodels\] Modeling framework. Defaults to
  `"Ordinary Differential Equations"`.

- programming_language:

  \[psychomodels\] Programming language.

- psychology_discipline:

  \[psychomodels\] Discipline id(s), comma-separated.

- software_package:

  \[psychomodels\] Package id(s), comma-separated.

- model_variable:

  \[psychomodels\] Variable id(s), comma-separated.

- code_repository_url:

  \[psychomodels\] URL to code repository.

- data_url:

  \[psychomodels\] URL to model data.

- submission_remarks:

  \[psychomodels\] Optional remarks.

- created_by:

  \[psychomodels\] Identifier of creating user.

- updated_by:

  \[psychomodels\] Identifier of last updating user.

- published_by:

  \[psychomodels\] Identifier of publishing user.

- published_at:

  \[psychomodels\] Publication timestamp. Defaults to current time.

- published_pending_moderation_at:

  \[psychomodels\] Moderation timestamp.

- publication_citation_fetched_at:

  \[psychomodels\] Citation fetch timestamp.

- publication_csl_fetched_at:

  \[psychomodels\] CSL fetch timestamp.

- publication_csl_json:

  \[psychomodels\] CSL JSON text.

- id:

  \[psychomodels\] Optional record id.

- slug:

  \[psychomodels\] Optional slug. Generated from `title` if `NULL`.

- include_latex:

  \[psychomodels\] If `TRUE`, append LaTeX equations to explanation.

- pretty:

  \[psychomodels\] If `TRUE`, pretty-print output JSON.

## Value

For `file = NULL`: a character string containing the exported content.
For `file` specified: invisibly returns the file path.

## Details

### sdbuildR format (`format = "sdbuildR"`)

Returns R code that reconstructs the model using sdbuildR functions.
When `file = NULL`, returns a character string. When `file` is provided,
writes an `.R` file and returns the path invisibly. If `file` has no
`.R` extension, one is appended.

### deSolve format (`format = "deSolve"`)

Returns a standalone R script using
[`deSolve::ode()`](https://rdrr.io/pkg/deSolve/man/ode.html) directly —
no sdbuildR dependency required to run the output. When `file = NULL`,
returns a character string. When `file` is provided, writes an `.R` file
and returns the path invisibly. If `file` has no `.R` extension, one is
appended. Requires `sim_settings(language = "R")` (the default).

### Psychomodels format (`format = "psychomodels"`)

Generates a JSON record for upload to
[Psychomodels](https://www.psychomodels.org/models/). When
`file = NULL`, returns a JSON character string. When `file` is provided,
writes a `.json` file and returns the path invisibly. If `file` has no
`.json` extension, one is appended.

## See also

[`import_insightmaker()`](https://kcevers.github.io/sdbuildR/reference/import_insightmaker.md),
[`import_desolve()`](https://kcevers.github.io/sdbuildR/reference/import_desolve.md)

## Examples

``` r
sfm <- stockflow("sir")

# Get sdbuildR reconstruction code
cat(export_model(sfm, format = "sdbuildR"))
#> sfm <-    stockflow() |>
#>  sim_settings(start = "0.0", stop = "20.0", dt = "0.01", time_units = "weeks") |>
#>  meta(name = "Susceptible-Infected-Recovered (SIR)", created = "2026-07-06 08:12:02.781323", version = "1.0.0") |>
#>  stock(infected, eqn = 1, label = "Infected") |>
#>  stock(recovered, eqn = 0, label = "Recovered") |>
#>  stock(susceptible, eqn = 99999, label = "Susceptible") |>
#>  flow(new_infections, eqn = infection_rate * susceptible * infected, to = infected, from = susceptible, label = "New infections") |>
#>  flow(new_recoveries, eqn = recovery_rate * infected, to = recovered, from = infected, label = "New recoveries") |>
#>  constant(contact_rate, eqn = 2, label = "Contact rate") |>
#>  constant(infection_rate, eqn = contact_rate / total_population, label = "Infection rate") |>
#>  constant(recovery_rate, eqn = 0.1, label = "Recovery rate") |>
#>  constant(total_population, eqn = susceptible + infected + recovered, label = "Total population")

# Get standalone deSolve script
cat(export_model(sfm, format = "deSolve"))
#> library(deSolve)
#> 
#> 
#> # Define time sequence
#> dt = 0.01
#> times <- seq(from=0.0, to=20.0, by=dt)
#> t = times[1]
#> 
#> 
#> 
#> 
#> 
#> 
#> # Define ODE
#> ode_func = function(t, current_state, constants){
#> 
#>     current_state = as.list(current_state)
#> 
#>     # Compute change in stocks at current time t
#>     with(c(current_state, constants), {
#> 
#>         # Update auxiliaries and flows
#>         new_infections <- infection_rate * susceptible * infected # Flow from susceptible to infected
#>      new_recoveries <- recovery_rate * infected # Flow from infected to recovered
#> 
#>         # Collect inflows and outflows for each stock
#>         dinfected <- new_infections - new_recoveries
#>      drecovered <- new_recoveries
#>      dsusceptible <-  - new_infections
#> 
#>         # Combine change in stocks
#>         dSdt = c(dinfected, drecovered, dsusceptible)
#> 
#>         return(list(dSdt))
#>       })
#>       }
#> 
#> 
#> # Define parameters, initial conditions, and functions in correct order
#> 
#> contact_rate = 2
#> recovery_rate = 0.1
#> susceptible = 99999
#> infected = 1
#> recovered = 0
#> total_population = susceptible + infected + recovered
#> infection_rate = contact_rate / total_population
#> 
#> # Define parameters in named list
#> constants = list(contact_rate = contact_rate, infection_rate = infection_rate, recovery_rate = recovery_rate, total_population = total_population)
#> 
#> 
#> # Define initial condition
#> init = c(infected = infected, recovered = recovered, susceptible = susceptible)
#> 
#> 
#> # Run ODE
#> df = as.data.frame(deSolve::ode(
#>   func=ode_func,
#>   y=init,
#>   times=times,
#>   parms=constants,
#>   method = 'euler'
#> )) 

# Export to Psychomodels JSON
if (FALSE) { # \dontrun{
json <- export_model(sfm,
  format = "psychomodels",
  publication_doi = "10.0000/example"
)
} # }
```
