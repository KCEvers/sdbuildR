# Changelog

## sdbuildR 2.2.0

- Renamed several
  [`plot.stockflow()`](https://kcevers.github.io/sdbuildR/reference/plot.stockflow.md)
  arguments for clarity: `minlen` is now `flow_length`, `nodesep` is now
  `spacing`, `pad` is now `margin`, `dependency_col` is now
  `color_dependency`, and `label_col` is now `font_color`. The old names
  are no longer recognised.

- Fixed time animations for variables that start with non-finite values,
  such as `NaN` from a `0/0` ratio at initialization. These plots now
  build without plotly trace-length warnings.

- Time animations can now be tuned with `control_options` in
  [`plot.simulate_stockflow()`](https://kcevers.github.io/sdbuildR/reference/plot.simulate_stockflow.md),
  [`plot.ensemble_stockflow()`](https://kcevers.github.io/sdbuildR/reference/plot.ensemble_stockflow.md),
  and
  [`plot.verify_stockflow()`](https://kcevers.github.io/sdbuildR/reference/plot.verify_stockflow.md).
  Use `duration`, `frame_ms`, `transition_ms`, or `max_frames` to
  control speed and smoothness.

- `plot.stockflow(show_eqn = TRUE)` now labels equations by variable
  type: `Initial value =` for stocks, `Rate =` for flows, `Value =` for
  constants, and `Equation =` for auxiliaries. Hover tooltips use the
  same wording.

- [`install_julia_env()`](https://kcevers.github.io/sdbuildR/reference/install_julia_env.md)
  now reinstalls the Julia environment from a clean sdbuildR user data
  directory.

- Informational messages are now controlled consistently with `quiet` in
  [`ensemble()`](https://kcevers.github.io/sdbuildR/reference/ensemble.md),
  [`simulate()`](https://rdrr.io/r/stats/simulate.html),
  [`use_julia()`](https://kcevers.github.io/sdbuildR/reference/use_julia.md),
  and
  [`install_julia_env()`](https://kcevers.github.io/sdbuildR/reference/install_julia_env.md).
  `ensemble(verbose = )` is deprecated.

- *Breaking:*
  [`plot.stockflow()`](https://kcevers.github.io/sdbuildR/reference/plot.stockflow.md)
  now uses `colors` instead of `stock_col` and `flow_col`. `colors`
  accepts a single colour, a list keyed by variable type, or a named
  vector keyed by variable name. Constants and auxiliaries can now be
  coloured as well.

- Dependency arrows in
  [`plot.stockflow()`](https://kcevers.github.io/sdbuildR/reference/plot.stockflow.md)
  now default to open arrow heads. Use the new `arrowhead_dependency`
  argument to choose a different shape.

- Plots and diagrams can now use Fontsource webfonts by passing a
  Fontsource id such as `"eb-garamond"` or `"source-serif-4"` to
  `font_family`. System fonts, such as `"Times New Roman"`, still work
  as before. *Breaking:* the default plot font changed from
  `"Times New Roman"` to `"stix-two-text"`; set
  `options(sdbuildR.font_family = "Times New Roman")` to restore the old
  default.

- *Breaking:*
  [`sim_settings()`](https://kcevers.github.io/sdbuildR/reference/sim_settings.md)
  now uses `save_by`, `save_times`, and `save_length` for output times.
  These replace the removed `save_at` and `save_n` arguments.

## sdbuildR 2.1.0

- The plotting `line_width` and `alpha` arguments now accept a richer
  grammar. In
  [`plot.ensemble_stockflow()`](https://kcevers.github.io/sdbuildR/reference/plot.ensemble_stockflow.md)
  they style three layers independently: the central-tendency line
  (`central`), the uncertainty band (`spread`), and the individual
  trajectories (`sims`). Pass a single value (applied everywhere), a
  named per-variable vector (names are variable labels, like `colors`),
  or a list keyed by layer
  (e.g. `line_width = list(central = 3, spread = 0, sims = 1)`),
  optionally with per-variable vectors inside each layer. `colors`
  likewise accepts a *partial* named vector now: name only the variables
  you want to recolour and the palette fills the rest. *Breaking:* the
  `central_line_width` argument is removed; use
  `line_width = list(central = ...)` instead. The ensemble defaults
  changed to `line_width = list(central = 3, spread = 0, sims = 1)` and
  `alpha = list(central = 1, spread = 0.3, sims = 0.3)` (the trajectory
  width is thinner and the band is drawn without a border by default).

- Improved the placement of condition sliders/dropdowns
  (`condition_display = "slider"`/`"dropdown"`) in
  [`plot.ensemble_stockflow()`](https://kcevers.github.io/sdbuildR/reference/plot.ensemble_stockflow.md)
  and
  [`plot.verify_stockflow()`](https://kcevers.github.io/sdbuildR/reference/plot.verify_stockflow.md).
  Plots with condition controls now have a fixed height, sized to the
  number of stacked controls. The gap between stacked controls can be
  tuned via `control_options = list(spacing = ...)` (now in pixels;
  `NULL` keeps the automatic default).

- Fixed `condition_display = "dropdown"`: selecting a condition from the
  dropdown(s) did not update the plot when several condition parameters
  were varied, because the handler waited for a relayout event that
  plotly.js never emits for dropdown buttons. The handler now reacts to
  the button-click event.

- [`ensemble()`](https://kcevers.github.io/sdbuildR/reference/ensemble.md)
  chooses which summary statistics to compute via `central` and
  `spread`, mirroring the vocabulary of
  [`plot.ensemble_stockflow()`](https://kcevers.github.io/sdbuildR/reference/plot.ensemble_stockflow.md).
  `central` is one or more of `"mean"`, `"median"`, or `"none"`;
  `spread` is one or more of `"quantile"` (quantile columns at the
  probabilities given by `quantiles`), `"sd"`, `"range"` (returned as
  `min`/`max` columns), or `"none"`. Unlike in
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html), here they
  are the *set* of statistics to compute (each becomes a column), not a
  preference order. A `missing_count` column is now always returned.
  `central`, `spread`, and `quantiles` can also be set on the model via
  [`sim_settings()`](https://kcevers.github.io/sdbuildR/reference/sim_settings.md);
  passing them to
  [`ensemble()`](https://kcevers.github.io/sdbuildR/reference/ensemble.md)
  overrides the model’s settings for that call. *Breaking:* this
  replaces the short-lived `summary_stats` argument; quantile columns
  are named `quant1`, `quant2`, … (in the order of `quantiles`; the
  probabilities are stored in the object’s `quantiles` field). Both
  accept lenient spellings (e.g. `"Medians"`, `"SDs"`, `"min-max"`).

- [`plot.ensemble_stockflow()`](https://kcevers.github.io/sdbuildR/reference/plot.ensemble_stockflow.md)
  gains `central` and `spread` arguments, each a preference vector.
  `central` (`"mean"`, `"median"`, `"none"`) picks the central line and
  `spread` (`"quantile"`, `"sd"`, `"range"`, `"none"`) picks the
  uncertainty band; the first option whose statistics are present in the
  summary is used. `central` replaces the previous `central_tendency`
  argument. Both accept lenient spellings (e.g. `"Medians"`, `"SDs"`).

- Fixed a bug where
  [`simulate()`](https://rdrr.io/r/stats/simulate.html) with
  `language = "julia"` ignored the `seed` set via
  [`sim_settings()`](https://kcevers.github.io/sdbuildR/reference/sim_settings.md),
  so models with random elements were not reproducible. Seeded Julia
  simulations are now reproducible, matching the R backend and the
  existing behaviour of
  [`ensemble()`](https://kcevers.github.io/sdbuildR/reference/ensemble.md).

- The [`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods
  for `simulate_stockflow`, `ensemble_stockflow`, and `verify_stockflow`
  results gain a `line_width` argument controlling the width of the
  plotted trajectories. It accepts either a single value applied to all
  variables, or a vector with one value per variable (mirroring
  `colors`). Defaults to `2`.

- In
  [`plot.ensemble_stockflow()`](https://kcevers.github.io/sdbuildR/reference/plot.ensemble_stockflow.md),
  the `central_tendency_width` argument has been renamed to
  `central_line_width` and now also accepts a vector with one value per
  variable (like `line_width`), in addition to a single value.

- The [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)
  methods for `simulate_stockflow`, `ensemble_stockflow`, and
  `verify_stockflow` results can now subset their output by variable
  with `vars` and by variable type with `type`, matching
  [`as.data.frame.stockflow()`](https://kcevers.github.io/sdbuildR/reference/as.data.frame.stockflow.md)
  and [`plot()`](https://rdrr.io/r/graphics/plot.default.html). For
  consistency, the selection argument is now called `vars` everywhere
  (in both
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) and
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html)); `name`
  remains reserved for the model-editing functions
  ([`update()`](https://rdrr.io/r/stats/update.html),
  [`stock()`](https://kcevers.github.io/sdbuildR/reference/stock.md),
  [`flow()`](https://kcevers.github.io/sdbuildR/reference/flow.md), …).
  The `name` argument of
  [`as.data.frame.stockflow()`](https://kcevers.github.io/sdbuildR/reference/as.data.frame.stockflow.md)
  has been renamed to `vars`.

- [`plot.stockflow()`](https://kcevers.github.io/sdbuildR/reference/plot.stockflow.md)
  gains three layout-control arguments. `direction` sets the overall
  flow direction (`"LR"`, `"TB"`, `"RL"`, or `"BT"`; default `"LR"`).
  `align` lines variables up across the flow direction (one or more
  groups, placed on the same Graphviz rank). `order` sequences variables
  along the flow direction as a soft hint via invisible edges, so it
  nudges the layout without overriding the real flows. All three accept
  any variable (not only stocks).

- The sdbuildR Julia environment is now stored in a persistent user
  directory (via
  [`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html)) instead
  of inside the installed package. It now survives reinstalling or
  updating sdbuildR, so you no longer have to rebuild it after every
  package update, and installation works on read-only or system-wide
  library locations. You are prompted to rebuild it with
  [`install_julia_env()`](https://kcevers.github.io/sdbuildR/reference/install_julia_env.md)
  only when its dependencies actually change.

- [`use_julia()`](https://kcevers.github.io/sdbuildR/reference/use_julia.md)
  and [`simulate()`](https://rdrr.io/r/stats/simulate.html) now detect
  when the sdbuildR Julia environment was built with a different version
  of Julia than the one currently running (for example after
  reinstalling or updating Julia) and prompt you to rebuild it with
  [`install_julia_env()`](https://kcevers.github.io/sdbuildR/reference/install_julia_env.md),
  instead of failing.

- [`install_julia_env()`](https://kcevers.github.io/sdbuildR/reference/install_julia_env.md)
  now reports clearly when setup is interrupted (for example by
  cancelling the 10-25 minute install), prompting you to run it again.

- [`plot.stockflow()`](https://kcevers.github.io/sdbuildR/reference/plot.stockflow.md)
  now uses default `minlen = 1` instead of `minlen = 2` to create
  shorter flow edges.

- [`plot.stockflow()`](https://kcevers.github.io/sdbuildR/reference/plot.stockflow.md)
  gains a `show_eqn` argument (default `TRUE`). Each variable’s equation
  is shown on a new line beneath its label, in a smaller font and the
  same colour as the label, wrapped to `wrap_width`. Set
  `show_eqn = FALSE` to hide the equations.

- [`plot.stockflow()`](https://kcevers.github.io/sdbuildR/reference/plot.stockflow.md)
  gains a `show_tooltip` argument (default `TRUE`) to control whether
  equations are shown as tooltips on hover.

- [`plot.stockflow()`](https://kcevers.github.io/sdbuildR/reference/plot.stockflow.md)
  gains a `label_col` argument to set the colour of variable labels (and
  of the equation text when `show_eqn = TRUE`).

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  simulation, ensemble, and verify results gains an `animation`
  argument. Use `animation = "time"` to cumulatively reveal trajectories
  over time with a play button and time slider. For ensemble and verify
  plots, time animation is supported for a single condition (one panel).

- [`plot.ensemble_stockflow()`](https://kcevers.github.io/sdbuildR/reference/plot.ensemble_stockflow.md)
  and
  [`plot.verify_stockflow()`](https://kcevers.github.io/sdbuildR/reference/plot.verify_stockflow.md)
  gain a `condition_display` argument. In addition to the default
  `"subplots"`, use `"slider"` or `"dropdown"` to show one
  condition/test at a time and select it interactively. These controls
  now (a) draw only one condition’s traces and swap the data
  client-side, so they stay fast even for ensembles with many
  conditions; (b) label each condition with its parameter values; and

  3.  for a crossed ensemble (`cross = TRUE`) with two or more
      parameters, show one control per parameter instead of a single
      condition selector. The interactive controls are self-contained
      and require no running R session, so they work in static HTML
      (e.g. pkgdown articles and Quarto slides).

### Bug fixes

- Fixed `citation("sdbuildR")` to report the package’s release year and
  version number without the developmental suffix.

## sdbuildR 2.0.0

CRAN release: 2026-06-16

### Breaking changes

- [`stockflow()`](https://kcevers.github.io/sdbuildR/reference/stockflow.md)
  is now the constructor for stock-and-flow models. Code that used
  `xmile()` or the development-only `sdbuildR()` constructor should call
  [`stockflow()`](https://kcevers.github.io/sdbuildR/reference/stockflow.md)
  instead.

- [`update()`](https://rdrr.io/r/stats/update.html) now replaces the old
  `build()` workflow for general model edits. For clearer model-building
  code, use
  [`stock()`](https://kcevers.github.io/sdbuildR/reference/stock.md),
  [`flow()`](https://kcevers.github.io/sdbuildR/reference/flow.md),
  [`constant()`](https://kcevers.github.io/sdbuildR/reference/constant.md),
  [`aux()`](https://kcevers.github.io/sdbuildR/reference/auxiliary.md)/[`auxiliary()`](https://kcevers.github.io/sdbuildR/reference/auxiliary.md),
  [`lookup()`](https://kcevers.github.io/sdbuildR/reference/lookup.md),
  and
  [`custom_func()`](https://kcevers.github.io/sdbuildR/reference/custom_func.md).

- `stockflow` is now the primary model class. Code that checked for
  `sdbuildR_xmile` or `sdbuildR` should check for `stockflow` instead.

- [`simulate()`](https://rdrr.io/r/stats/simulate.html) now returns
  `simulate_stockflow` objects,
  [`ensemble()`](https://kcevers.github.io/sdbuildR/reference/ensemble.md)
  returns `ensemble_stockflow` objects,
  [`verify()`](https://kcevers.github.io/sdbuildR/reference/verify.md)
  returns `verify_stockflow` objects,
  [`summary()`](https://rdrr.io/r/base/summary.html) returns
  `summary_stockflow` objects, and
  [`compare_models()`](https://kcevers.github.io/sdbuildR/reference/compare_models.md)
  returns `compare_stockflow` objects.

- [`sim_settings()`](https://kcevers.github.io/sdbuildR/reference/sim_settings.md)
  now replaces `sim_specs()`, and
  [`meta()`](https://kcevers.github.io/sdbuildR/reference/meta.md) now
  replaces `header()`.

- [`summary()`](https://rdrr.io/r/base/summary.html) now runs model
  diagnostics. Code that used
  [`debugger()`](https://rdrr.io/r/utils/debugger.html) should call
  [`summary()`](https://rdrr.io/r/base/summary.html) instead.

- [`dependencies()`](https://kcevers.github.io/sdbuildR/reference/dependencies.md)
  now replaces `find_dependencies()`.

- `export_model(format = "sdbuildR")` now replaces `get_build_code()`.

- [`sim_methods()`](https://kcevers.github.io/sdbuildR/reference/sim_methods.md)
  now replaces `solvers()`.

- [`custom_func()`](https://kcevers.github.io/sdbuildR/reference/custom_func.md)
  now replaces `macro()` for user-defined model functions.

- [`import_insightmaker()`](https://kcevers.github.io/sdbuildR/reference/import_insightmaker.md)
  now replaces `insightmaker_to_sfm()`, and
  [`url_to_insightmaker()`](https://kcevers.github.io/sdbuildR/reference/url_to_insightmaker.md)
  now replaces `url_to_IM()`.

- [`ensemble()`](https://kcevers.github.io/sdbuildR/reference/ensemble.md)
  now uses `conditions` instead of `range`. To retain individual
  simulations, use `save_sims = TRUE` instead of `return_sims = TRUE`.

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods now
  use `show_constants` instead of `add_constants`. For ensemble plots,
  use `which`, `sim`, `condition`, and `label_subplots` instead of
  `type`, `i`, `j`, and `j_labels`.

- `use_julia(nthreads = )` now controls Julia threading. Code that used
  `use_threads()` should set the thread count with
  [`use_julia()`](https://kcevers.github.io/sdbuildR/reference/use_julia.md)
  instead.

- Units are no longer supported. This means that unit-specific helpers
  from 1.x, including `u()`, `convert_u()`, `drop_u()`, `model_units()`,
  `unit_prefixes()`, `get_units()`, `get_regex_units()`, and
  `get_regex_time_units()`, are no longer available.

### New features

- [`stockflow()`](https://kcevers.github.io/sdbuildR/reference/stockflow.md)
  creates an empty model or loads a built-in template using a
  case-insensitive `template` name.

- [`stock()`](https://kcevers.github.io/sdbuildR/reference/stock.md),
  [`flow()`](https://kcevers.github.io/sdbuildR/reference/flow.md),
  [`constant()`](https://kcevers.github.io/sdbuildR/reference/constant.md),
  [`aux()`](https://kcevers.github.io/sdbuildR/reference/auxiliary.md)/[`auxiliary()`](https://kcevers.github.io/sdbuildR/reference/auxiliary.md),
  [`lookup()`](https://kcevers.github.io/sdbuildR/reference/lookup.md),
  and
  [`custom_func()`](https://kcevers.github.io/sdbuildR/reference/custom_func.md)
  provide typed model-building helpers around
  [`update()`](https://rdrr.io/r/stats/update.html).

- [`update()`](https://rdrr.io/r/stats/update.html) now supports bare
  variable names and R expressions in `name`, `type`, `eqn`, `to`,
  `from`, and `source`. Strings and programmatic injection with `!!`
  remain supported.

- [`change_name()`](https://kcevers.github.io/sdbuildR/reference/change_name.md),
  [`change_type()`](https://kcevers.github.io/sdbuildR/reference/change_type.md),
  and
  [`discard()`](https://kcevers.github.io/sdbuildR/reference/discard.md)
  provide focused helpers for renaming, changing, and removing model
  variables.

- [`sim_settings()`](https://kcevers.github.io/sdbuildR/reference/sim_settings.md)
  gains flexible output controls with `save_at`, `save_n`, `vars`, and
  `save_sims`.

- [`simulate()`](https://rdrr.io/r/stats/simulate.html) accepts
  temporary simulation-setting overrides through `...`, using the same
  arguments as
  [`sim_settings()`](https://kcevers.github.io/sdbuildR/reference/sim_settings.md).

- [`ensemble()`](https://kcevers.github.io/sdbuildR/reference/ensemble.md)
  can run ensembles in R and Julia, vary stocks and constants with
  `conditions`, and retain individual simulations with
  `save_sims = TRUE`.

- [`unit_test()`](https://kcevers.github.io/sdbuildR/reference/unit_test.md),
  [`unit_tests()`](https://kcevers.github.io/sdbuildR/reference/unit_tests.md),
  and
  [`verify()`](https://kcevers.github.io/sdbuildR/reference/verify.md)
  add model-level unit tests for expected simulation behavior, including
  tests under alternative `conditions`.

- [`compare_models()`](https://kcevers.github.io/sdbuildR/reference/compare_models.md)
  compares model structure, equations, and simulation settings across
  two `stockflow` models.

- [`import_desolve()`](https://kcevers.github.io/sdbuildR/reference/import_desolve.md)
  converts deSolve-style ODE models into `stockflow` models.

- [`import_insightmaker()`](https://kcevers.github.io/sdbuildR/reference/import_insightmaker.md)
  imports Insight Maker models from public URLs, `.InsightMaker` files,
  and ModelJSON `.json` files.

- [`export_model()`](https://kcevers.github.io/sdbuildR/reference/export_model.md)
  exports models as sdbuildR reconstruction code, standalone deSolve
  scripts, or Psychomodels JSON records.

### Improvements and fixes

- [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) methods
  now support the `stockflow`, `simulate_stockflow`,
  `ensemble_stockflow`, and `verify_stockflow` classes.

- [`dependencies()`](https://kcevers.github.io/sdbuildR/reference/dependencies.md)
  can now filter dependencies by variable name and type.

- [`export_plot()`](https://kcevers.github.io/sdbuildR/reference/export_plot.md)
  now supports the current plot classes and additional graph export
  options.

- [`simulate()`](https://rdrr.io/r/stats/simulate.html) and Julia code
  generation now use a faster AST-based R-to-Julia equation translator.
  Namespaced calls such as
  [`base::sum()`](https://rdrr.io/r/base/sum.html) translate correctly,
  and unsupported constructs fall back to the previous translator.

- [`simulate()`](https://rdrr.io/r/stats/simulate.html), compilation,
  and [`summary()`](https://rdrr.io/r/base/summary.html) now use a
  content-hashed assembly cache. Repeated calls on unchanged models can
  skip reassembly, and editing one equation only retranslates that
  equation.

- [`simulate()`](https://rdrr.io/r/stats/simulate.html) now keeps Julia
  stock derivative positions aligned with the state vector when stocks
  are added, removed, renamed, or created by changing a variable’s type.

- [`summary()`](https://rdrr.io/r/base/summary.html) and code generation
  now use an internal structural validator to catch inconsistent model
  layouts before simulation.

- [`simulate()`](https://rdrr.io/r/stats/simulate.html) now removes
  temporary CSV files created by single Julia simulations.

- `textutils` is now a required dependency.

## sdbuildR 1.0.8

CRAN release: 2025-11-19

### Minor improvements and bug fixes

- Fixed bug in replacing scientifically formatted numbers
- Fixed bug in comparing Julia versions
- Added alias “sigmoid” for logistic function
- Created constructor and validator for class sdbuildR_sim to improve
  consistency in output

## sdbuildR 1.0.7

CRAN release: 2025-11-03

## sdbuildR 1.0.6

- Fixed error in finding Julia installation

- Simulations in Julia are now ensured to stop at exact simulation
  times, which removes unexpected results created by numerical errors in
  the solver (using tstops argument in solve())

## sdbuildR 1.0.5

CRAN release: 2025-10-29

## sdbuildR 1.0.4

## sdbuildR 1.0.3

- Removed automatic installation of Julia packages and instead wrote a
  separate function for this: install_julia_env(). install_julia_env()
  is called in .onLoad() ONLY if the custom environmental variable
  AUTO_INSTALL_JULIA_ENV is “true” and NOT_CRAN is “true”. This is to
  ensure GitHub workflows have the Julia environment instantiated, but
  this will not affect users, as they do not have
  AUTO_INSTALL_JULIA_ENV.

- Added vignette for formalizing Job Demands-Resources (JDR) Theory

- Improved documentation

## sdbuildR 1.0.2

## sdbuildR 1.0.1

## sdbuildR 1.0.0

- Added extensive solver options
- Created Julia package (SystemDynamicsBuildR.jl) to contain sdbuildR
  Julia code
- Added vignette for support in installing and setting up Julia
- Added obligatory argument `times` to step, pulse, ramp, and seasonal
