# Importing and Exporting Models

``` r

library(sdbuildR)
```

sdbuildR can exchange models with several external tools. This vignette
covers three workflows:

1.  **Importing** from [Insight Maker](https://insightmaker.com/)
2.  **Importing** from
    [deSolve](https://cran.r-project.org/package=deSolve)
3.  **Exporting** to [Psychomodels](https://www.psychomodels.org/)

------------------------------------------------------------------------

## Importing from Insight Maker

[Insight Maker](https://insightmaker.com/) is a free, browser-based
tool. It contains a large library of public stock-and-flow models which
can be imported to sdbuildR with
[`import_insightmaker()`](https://kcevers.github.io/sdbuildR/reference/import_insightmaker.md).

### From a URL

Pass the URL of any public Insight Maker model:

``` r

sfm <- import_insightmaker(
  url = "https://insightmaker.com/insight/43tz1nvUgbIiIOGSGtzIzj/Romeo-Juliet"
)
```

### From a downloaded file

Models can be downloaded as `.InsightMaker` files (go to **Share →
Export → Download Insight Maker file**) or as ModelJSON (`.json`) files:

``` r

sfm <- import_insightmaker(file = "my_model.InsightMaker")
sfm <- import_insightmaker(file = "my_model.json")
```

After importing, use the model as usual:

``` r

summary(sfm)
sfm |>
  simulate() |>
  plot()
```

### Limitations

Note that some features of Insight Maker models are not supported by
sdbuildR, and will be ignored during import. These include:

- Vectorized operations, destructuring assignment, or minimum and
  maximum constraints for variables.

- The Insight Maker functions Stop(), Prompt(), Confirm(), Pause(),
  Fix(), Map(), Filter(), and Repeat(), as well as the delay and past
  functions. A message is issued if any of these are detected.

- Units (e.g., kilograms) are also not supported.

- Agent-based models or Causal Loop Diagrams.

- sdbuildR provides only minimal support for non-negative stocks and
  flows. Specifically, setting stocks to non-negative will constrain the
  stocks to remain non-negative, but will not adjust the corresponding
  flows. In any case, enforcing either stocks or flows to be
  non-negative is not recommended, as it may mask model
  misspecification. Stocks and flows that logically cannot be negative
  (e.g., animals or deaths) should ideally remain non-negative as a
  result of the model’s equations and parameters, rather than by forcing
  them to be non-negative.

------------------------------------------------------------------------

## Importing from deSolve

[deSolve](https://cran.r-project.org/package=deSolve) is the standard R
package for solving ODEs. If you have an existing **deSolve** model you
can import it into sdbuildR to access features like ensemble
simulations, stock-and-flow diagrams, and export to Psychomodels.

### Supported pattern

[`import_desolve()`](https://kcevers.github.io/sdbuildR/reference/import_desolve.md)
parses the canonical **deSolve** model convention:

``` r
model <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    # intermediate calculations become auxiliary variables
    aux_var <- <expression>
    # d<VarName> assignments become net flows for stock VarName
    dVarName <- <expression>
    list(c(dVarName, ...))
  })
}
```

**Requirements:**

- The function must have exactly the arguments `(t, state, parameters)`.
- Variables must be accessed by name inside a
  `with(as.list(c(state, parameters)), {...})` block.
- Derivatives must be written as `d<VarName> <- <expression>` where
  `VarName` matches a name in `init`.
- `times` must be evenly spaced — use
  [`seq()`](https://rdrr.io/r/base/seq.html).

### Example: SIR epidemic model

``` r

sir_model <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    SI <- beta * S * I / N # intermediate calculation → aux variable
    IR <- gamma * I
    dS <- -SI
    dI <- SI - IR
    dR <- IR
    list(c(dS, dI, dR))
  })
}

sfm <- import_desolve(
  model  = sir_model,
  params = c(beta = 0.3, gamma = 0.1, N = 1000),
  init   = c(S = 990, I = 10, R = 0),
  times  = seq(0, 100, by = 0.1),
  name   = "SIR epidemic"
)

print(sfm)
#> 
#> ── Stock-and-Flow Model: SIR epidemic ──────────────────────────────────────────
#> 3 stocks • 3 flows • 3 constants • 2 auxiliaries
#> 
#> ── Stock-Flow Structure ──
#> I: + net_I
#> R: + net_R
#> S: + net_S
#> 
#> ── Other Variables ──
#> Constants: `beta`, `gamma`, and `N`
#> Auxiliaries: `IR` and `SI`
#> 
#> ── Simulation Settings ──
#> Time: 0.0 to 100.0 seconds (dt = 0.1) • lsoda • R
#> Simulation output: stocks only
```

``` r

sfm <- sim_settings(sfm, save_by = 5)
sfm |>
  simulate() |>
  plot()
```

The imported model is a full `stockflow` object. You can modify it with
[`update()`](https://rdrr.io/r/stats/update.html), add unit tests with
[`unit_test()`](https://kcevers.github.io/sdbuildR/reference/unit_test.md),
run ensembles, and export it to any supported format.

------------------------------------------------------------------------

## Exporting models

[`export_model()`](https://kcevers.github.io/sdbuildR/reference/export_model.md)
converts a `stockflow` object to another format. The currently supported
export formats are `sdbuildR` to generate the build code of the model,
`deSolve` to return a complete script for simulating a model with
`deSolve`, and `psychomodels`.

Use `file = "path"` to write directly to disk; omit `file` to return the
result in memory.

------------------------------------------------------------------------

### Deconstructing a model

To understand how a model was built, use
[`export_model()`](https://kcevers.github.io/sdbuildR/reference/export_model.md).
This will return the R code used to build the model, omitting default
specifications.

``` r

export_model(sfm, format = "sdbuildR")
#> [1] "sfm <-\tstockflow() |>\n\tsim_settings(method = \"lsoda\", start = \"0.0\", stop = \"100.0\", dt = \"0.1\", save_by = 5) |>\n\tmeta(name = \"SIR epidemic\", created = \"2026-07-07 05:00:02.096219\") |>\n\tstock(I, eqn = 10) |>\n\tstock(R, eqn = 0) |>\n\tstock(S, eqn = 990) |>\n\tflow(net_I, eqn = SI - IR, to = I) |>\n\tflow(net_R, eqn = IR, to = R) |>\n\tflow(net_S, eqn = -SI, to = S) |>\n\tconstant(beta, eqn = 0.3) |>\n\tconstant(gamma, eqn = 0.1) |>\n\tconstant(N, eqn = 1000) |>\n\taux(IR, eqn = gamma * I) |>\n\taux(SI, eqn = beta * S * I/N)\n"
```

### deSolve script

To generate a complete script for simulating a model with **deSolve**,
use `format = "deSolve"`. This will return a character string containing
the R code for the model function, parameter and initial value
definitions, and simulation code.

``` r

script <- export_model(sfm, format = "deSolve")
```

### Export to Psychomodels

[Psychomodels](https://www.psychomodels.org/) is a repository of
computational models in psychology. `format = "psychomodels"` generates
the JSON record expected by the platform. When `file = NULL`, returns a
JSON character string; when `file` is provided, writes a `.json` file
(extension appended if absent).

``` r

json <- export_model(
  sfm,
  format          = "psychomodels",
  publication_doi = "10.0000/my-paper",
  include_latex   = TRUE # appends LaTeX equations to the explanation field
)
```

Write directly to a `.json` file for upload:

``` r

export_model(sfm,
  format = "psychomodels",
  publication_doi = "10.0000/my-paper",
  file = "sir_psychomodels.json"
)
```

Key arguments (all optional):

| Argument                | Description                                |
|-------------------------|--------------------------------------------|
| `title`                 | Model title (defaults to `sfm$meta$name`)  |
| `description`           | Short description                          |
| `explanation`           | Long-form explanation                      |
| `publication_doi`       | DOI of the linked publication              |
| `include_latex`         | Append LaTeX equations (`TRUE`/`FALSE`)    |
| `programming_language`  | Programming language string                |
| `psychology_discipline` | Discipline id(s) as comma-separated string |
| `file`                  | File path; if `NULL`, returns JSON string  |
