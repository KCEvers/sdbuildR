# sdbuildR 2.2.1

* Updated the vignette "Formalizing Job-Demands Resources Theory" and added teaching materials for summer school Theory Building in Psychology in the Github Pages website.

* `plot.simulate_stockflow()` gained several new arguments for laying out and
  styling trajectories:
    * `vars_display` chooses how variables are arranged across panels.
      `"vstack"` (the default) stacks stocks in a panel above the other
      variables; `"hstack"` places the stock panel beside the others; and
      `"joint"` draws every variable together in a single panel. For example,
      `plot(sfm, vars_display = "joint")`.
    * `line_type` sets the line dash style. Pass one style for everything
      (`line_type = "dash"`), one style per variable
      (`line_type = c(S = "solid", I = "dot")`), or one style per variable type
      (`line_type = list(stock = "solid", flow = "dash")`). By default, stocks,
      flows, and auxiliaries are solid while constants and lookups are dashed.
    * `order` sets the order in which variables appear in the legend and are
      drawn. For example, `order = c("I", "S")` puts `I` first; any variables you
      leave out follow in their usual order. `order` defaults to `vars`, so the
      order you list variables in `vars` is followed unless you set `order`
      explicitly.
    * `fill_flows` shades the area under flow lines. Use `fill_flows = FALSE` to
      draw flows as plain lines.
    * `show_legend` shows or hides the legend, e.g. `show_legend = FALSE`.
      `line_type`, `order`, and `show_legend` also work in
      `plot.ensemble_stockflow()` and `plot.verify_stockflow()`.

* `export_model(sfm, format = "sdbuildR")` now quotes non-standard-evaluation arguments. For example, instead of `stock(a, eqn = runif(1))`, it returns `stock("a", eqn = "runif(1)")`.


# sdbuildR 2.2.0

* Renamed several `plot.stockflow()` arguments for clarity: `minlen` is now
  `flow_length`, `nodesep` is now `spacing`, `pad` is now `margin`,
  `dependency_col` is now `color_dependency`, and `label_col` is now
  `font_color`. The old names are no longer recognized.

* Fixed time animations for variables that start with non-finite values, such
  as `NaN` from a `0/0` ratio at initialization. These plots now build without
  plotly trace-length warnings.

* Time animations can now be tuned with `control_options` in
  `plot.simulate_stockflow()`, `plot.ensemble_stockflow()`, and
  `plot.verify_stockflow()`. Use `duration`, `frame_ms`, `transition_ms`, or
  `max_frames` to control speed and smoothness.

* `plot.stockflow(show_eqn = TRUE)` now labels equations by variable type:
  `Initial value =` for stocks, `Rate =` for flows, `Value =` for constants,
  and `Equation =` for auxiliaries. Hover tooltips use the same wording.

* `install_julia_env()` now reinstalls the Julia environment from a clean
  sdbuildR user data directory.

* Informational messages are now controlled consistently with `quiet` in
  `ensemble()`, `simulate()`, `use_julia()`, and `install_julia_env()`.
  `ensemble(verbose = )` is deprecated.

* *Breaking:* `plot.stockflow()` now uses `colors` instead of `stock_col` and
  `flow_col`. `colors` accepts a single colour, a list keyed by variable type,
  or a named vector keyed by variable name. Constants and auxiliaries can now
  be coloured as well.

* Dependency arrows in `plot.stockflow()` now default to open arrow heads. Use
  the new `arrowhead_dependency` argument to choose a different shape.

* Plots and diagrams can now use Fontsource webfonts by passing a Fontsource id
  such as `"eb-garamond"` or `"source-serif-4"` to `font_family`. System fonts,
  such as `"Times New Roman"`, still work as before. *Breaking:* the default
  plot font changed from `"Times New Roman"` to `"stix-two-text"`; set
  `options(sdbuildR.font_family = "Times New Roman")` to restore the old
  default.

* *Breaking:* `sim_settings()` now uses `save_by`, `save_times`, and
  `save_length` for output times. These replace the removed `save_at` and
  `save_n` arguments.

# sdbuildR 2.1.0

* The plotting `line_width` and `alpha` arguments now accept a richer grammar.
  In `plot.ensemble_stockflow()` they style three layers independently: the
  central-tendency line (`central`), the uncertainty band (`spread`), and the
  individual trajectories (`sims`). Pass a single value (applied everywhere), a
  named per-variable vector (names are variable labels, like `colors`), or a list
  keyed by layer (e.g. `line_width = list(central = 3, spread = 0, sims = 1)`),
  optionally with per-variable vectors inside each layer. `colors` likewise
  accepts a *partial* named vector now: name only the variables you want to
  recolour and the palette fills the rest. *Breaking:* the `central_line_width`
  argument is removed; use `line_width = list(central = ...)` instead. The
  ensemble defaults changed to `line_width = list(central = 3, spread = 0,
  sims = 1)` and `alpha = list(central = 1, spread = 0.3, sims = 0.3)` (the
  trajectory width is thinner and the band is drawn without a border by default).

* Improved the placement of condition sliders/dropdowns
  (`condition_display = "slider"`/`"dropdown"`) in `plot.ensemble_stockflow()`
  and `plot.verify_stockflow()`.
  Plots with condition controls now have a fixed height, sized to the number of
  stacked controls. The gap
  between stacked controls can be tuned via
  `control_options = list(spacing = ...)` (now in pixels; `NULL` keeps the
  automatic default).

* Fixed `condition_display = "dropdown"`: selecting a condition from the
  dropdown(s) did not update the plot when several condition parameters were
  varied, because the handler waited for a relayout event that plotly.js never
  emits for dropdown buttons. The handler now reacts to the button-click event.


* `ensemble()` chooses which summary statistics to compute via `central` and
  `spread`, mirroring the vocabulary of `plot.ensemble_stockflow()`. `central` is
  one or more of `"mean"`, `"median"`, or `"none"`; `spread` is one or more of
  `"quantile"` (quantile columns at the probabilities given by `quantiles`),
  `"sd"`, `"range"` (returned as `min`/`max` columns), or `"none"`. Unlike in
  `plot()`, here they are the *set* of statistics to compute (each becomes a
  column), not a preference order. A `missing_count` column is now always
  returned. `central`, `spread`, and `quantiles` can also be set on the model via
  `sim_settings()`; passing them to `ensemble()` overrides the model's settings
  for that call. *Breaking:* this replaces the short-lived `summary_stats`
  argument; quantile columns are named `quant1`, `quant2`, ... (in the order of
  `quantiles`; the probabilities are stored in the object's `quantiles` field).
  Both accept lenient spellings (e.g. `"Medians"`, `"SDs"`, `"min-max"`).

* `plot.ensemble_stockflow()` gains `central` and `spread` arguments, each a
  preference vector. `central` (`"mean"`, `"median"`, `"none"`) picks the central
  line and `spread` (`"quantile"`, `"sd"`, `"range"`, `"none"`) picks the
  uncertainty band; the first option whose statistics are present in the summary
  is used. `central` replaces the previous
  `central_tendency` argument. Both accept lenient spellings (e.g. `"Medians"`,
  `"SDs"`).

* Fixed a bug where `simulate()` with `language = "julia"` ignored the `seed` set
  via `sim_settings()`, so models with random elements were not reproducible.
  Seeded Julia simulations are now reproducible, matching the R backend and the
  existing behaviour of `ensemble()`.

* The `plot()` methods for `simulate_stockflow`, `ensemble_stockflow`, and
  `verify_stockflow` results gain a `line_width` argument controlling the width
  of the plotted trajectories. It accepts either a single value applied to all
  variables, or a vector with one value per variable (mirroring `colors`).
  Defaults to `2`.

* In `plot.ensemble_stockflow()`, the `central_tendency_width` argument has been
  renamed to `central_line_width` and now also accepts a vector with one
  value per variable (like `line_width`), in addition to a single value.

* The `as.data.frame()` methods for `simulate_stockflow`, `ensemble_stockflow`,
  and `verify_stockflow` results can now subset their output by variable with
  `vars` and by variable type with `type`, matching `as.data.frame.stockflow()`
  and `plot()`. For consistency, the selection argument is now called `vars`
  everywhere (in both `as.data.frame()` and `plot()`); `name` remains reserved
  for the model-editing functions (`update()`, `stock()`, `flow()`, ...). The
  `name` argument of `as.data.frame.stockflow()` has been renamed to `vars`.

* `plot.stockflow()` gains three layout-control arguments. `direction` sets the
  overall flow direction (`"LR"`, `"TB"`, `"RL"`, or `"BT"`; default `"LR"`).
  `align` lines variables up across the flow direction (one or more groups,
  placed on the same Graphviz rank). `order` sequences variables along the flow
  direction as a soft hint via invisible edges, so it nudges the layout without
  overriding the real flows. All three accept any variable (not only stocks).

* The sdbuildR Julia environment is now stored in a persistent user directory
  (via `tools::R_user_dir()`) instead of inside the installed package. It now
  survives reinstalling or updating sdbuildR, so you no longer have to rebuild
  it after every package update, and installation works on read-only or
  system-wide library locations. You are prompted to rebuild it with
  `install_julia_env()` only when its dependencies actually change.

* `use_julia()` and `simulate()` now detect when the sdbuildR Julia environment
  was built with a different version of Julia than the one currently running
  (for example after reinstalling or updating Julia) and prompt you to rebuild
  it with `install_julia_env()`, instead of failing.

* `install_julia_env()` now reports clearly when setup is interrupted (for
  example by cancelling the 10-25 minute install), prompting you to run it
  again.

* `plot.stockflow()` now uses default `minlen = 1` instead of `minlen = 2` to create shorter flow edges.

* `plot.stockflow()` gains a `show_eqn` argument (default `TRUE`). Each
  variable's equation is shown on a new line beneath its label, in a smaller
  font and the same colour as the label, wrapped to `wrap_width`. Set
  `show_eqn = FALSE` to hide the equations.

* `plot.stockflow()` gains a `show_tooltip` argument (default `TRUE`) to control
  whether equations are shown as tooltips on hover.

* `plot.stockflow()` gains a `label_col` argument to set the colour of variable
  labels (and of the equation text when `show_eqn = TRUE`).

* `plot()` for simulation, ensemble, and verify results gains an `animation`
  argument. Use `animation = "time"` to cumulatively reveal trajectories over
  time with a play button and time slider. For ensemble and verify plots, time
  animation is supported for a single condition (one panel).

* `plot.ensemble_stockflow()` and `plot.verify_stockflow()` gain a
  `condition_display` argument. In addition to the default `"subplots"`, use
  `"slider"` or `"dropdown"` to show one condition/test at a time and select it
  interactively. These controls now (a) draw only one condition's traces and
  swap the data client-side, so they stay fast even for ensembles
  with many conditions; (b) label each condition with its parameter values; and
  (c) for a crossed ensemble (`cross = TRUE`) with two or more parameters, show
  one control per parameter instead of a single condition selector. The
  interactive controls are self-contained and require no running R session, so
  they work in static HTML (e.g. pkgdown articles and Quarto slides).

## Bug fixes

* Fixed `citation("sdbuildR")` to report the package's release year and version number without the developmental suffix.

# sdbuildR 2.0.0

## Breaking changes

* `stockflow()` is now the constructor for stock-and-flow models. Code that
  used `xmile()` or the development-only `sdbuildR()` constructor should call
  `stockflow()` instead.

* `update()` now replaces the old `build()` workflow for general model edits.
  For clearer model-building code, use `stock()`, `flow()`, `constant()`,
  `aux()`/`auxiliary()`, `lookup()`, and `custom_func()`.

* `stockflow` is now the primary model class. Code that checked for
  `sdbuildR_xmile` or `sdbuildR` should check for `stockflow` instead.

* `simulate()` now returns `simulate_stockflow` objects, `ensemble()` returns
  `ensemble_stockflow` objects, `verify()` returns `verify_stockflow` objects,
  `summary()` returns `summary_stockflow` objects, and `compare_models()`
  returns `compare_stockflow` objects.

* `sim_settings()` now replaces `sim_specs()`, and `meta()` now replaces
  `header()`.

* `summary()` now runs model diagnostics. Code that used `debugger()` should
  call `summary()` instead.

* `dependencies()` now replaces `find_dependencies()`.

* `export_model(format = "sdbuildR")` now replaces `get_build_code()`.

* `sim_methods()` now replaces `solvers()`.

* `custom_func()` now replaces `macro()` for user-defined model functions.

* `import_insightmaker()` now replaces `insightmaker_to_sfm()`, and
  `url_to_insightmaker()` now replaces `url_to_IM()`.

* `ensemble()` now uses `conditions` instead of `range`. To retain individual
  simulations, use `save_sims = TRUE` instead of `return_sims = TRUE`.

* `plot()` methods now use `show_constants` instead of `add_constants`. For
  ensemble plots, use `which`, `sim`, `condition`, and `label_subplots` instead
  of `type`, `i`, `j`, and `j_labels`.

* `use_julia(nthreads = )` now controls Julia threading. Code that used
  `use_threads()` should set the thread count with `use_julia()` instead.

* Units are no longer supported. This means that unit-specific helpers from 1.x, 
  including `u()`, `convert_u()`, `drop_u()`,
  `model_units()`, `unit_prefixes()`, `get_units()`, `get_regex_units()`, and
  `get_regex_time_units()`, are no longer available.

## New features

* `stockflow()` creates an empty model or loads a built-in template using a
  case-insensitive `template` name.

* `stock()`, `flow()`, `constant()`, `aux()`/`auxiliary()`, `lookup()`, and
  `custom_func()` provide typed model-building helpers around `update()`.

* `update()` now supports bare variable names and R expressions in `name`,
  `type`, `eqn`, `to`, `from`, and `source`. Strings and programmatic injection
  with `!!` remain supported.

* `change_name()`, `change_type()`, and `discard()` provide focused helpers for
  renaming, changing, and removing model variables.

* `sim_settings()` gains flexible output controls with `save_at`, `save_n`,
  `vars`, and `save_sims`.

* `simulate()` accepts temporary simulation-setting overrides through `...`,
  using the same arguments as `sim_settings()`.

* `ensemble()` can run ensembles in R and Julia, vary stocks and constants with
  `conditions`, and retain individual simulations with `save_sims = TRUE`.

* `unit_test()`, `unit_tests()`, and `verify()` add model-level unit tests for
  expected simulation behavior, including tests under alternative `conditions`.

* `compare_models()` compares model structure, equations, and simulation
  settings across two `stockflow` models.

* `import_desolve()` converts deSolve-style ODE models into `stockflow` models.

* `import_insightmaker()` imports Insight Maker models from public URLs,
  `.InsightMaker` files, and ModelJSON `.json` files.

* `export_model()` exports models as sdbuildR reconstruction code, standalone
  deSolve scripts, or Psychomodels JSON records.

## Improvements and fixes

* `as.data.frame()` methods now support the `stockflow`, `simulate_stockflow`,
  `ensemble_stockflow`, and `verify_stockflow` classes.

* `dependencies()` can now filter dependencies by variable name and type.

* `export_plot()` now supports the current plot classes and additional graph
  export options.

* `simulate()` and Julia code generation now use a faster AST-based R-to-Julia
  equation translator. Namespaced calls such as `base::sum()` translate
  correctly, and unsupported constructs fall back to the previous translator.

* `simulate()`, compilation, and `summary()` now use a content-hashed assembly
  cache. Repeated calls on unchanged models can skip reassembly, and editing one
  equation only retranslates that equation.

* `simulate()` now keeps Julia stock derivative positions aligned with the state
  vector when stocks are added, removed, renamed, or created by changing a
  variable's type.

* `summary()` and code generation now use an internal structural validator to
  catch inconsistent model layouts before simulation.

* `simulate()` now removes temporary CSV files created by single Julia
  simulations.

* `textutils` is now a required dependency.

# sdbuildR 1.0.8

## Minor improvements and bug fixes

* Fixed bug in replacing scientifically formatted numbers
* Fixed bug in comparing Julia versions
* Added alias "sigmoid" for logistic function
* Created constructor and validator for class sdbuildR_sim to improve consistency in output

# sdbuildR 1.0.7

# sdbuildR 1.0.6

* Fixed error in finding Julia installation 

* Simulations in Julia are now ensured to stop at exact simulation times, which removes unexpected results created by numerical errors in the solver (using tstops argument in solve()) 

# sdbuildR 1.0.5

# sdbuildR 1.0.4

# sdbuildR 1.0.3

* Removed automatic installation of Julia packages and instead wrote a separate function for this: install_julia_env(). install_julia_env() is called in .onLoad() ONLY if the custom environmental variable AUTO_INSTALL_JULIA_ENV is "true" and NOT_CRAN is "true". This is to ensure GitHub workflows have the Julia environment instantiated, but this will not affect users, as they do not have AUTO_INSTALL_JULIA_ENV.

* Added vignette for formalizing Job Demands-Resources (JDR) Theory 

* Improved documentation

# sdbuildR 1.0.2

# sdbuildR 1.0.1

# sdbuildR 1.0.0

* Added extensive solver options
* Created Julia package (SystemDynamicsBuildR.jl) to contain sdbuildR Julia code
* Added vignette for support in installing and setting up Julia
* Added obligatory argument `times` to step, pulse, ramp, and seasonal
