# Plot timeseries of ensemble simulation

Visualize ensemble simulation results of a stock-and-flow model. Either
summary statistics or individual trajectories can be plotted. When
multiple conditions j are specified, a grid of subplots is plotted. See
[`ensemble()`](https://kcevers.github.io/sdbuildR/reference/ensemble.md)
for examples.

## Usage

``` r
# S3 method for class 'ensemble_stockflow'
plot(
  x,
  which = c("summary", "sims")[1],
  sim = seq(1, min(c(x[["n"]], 100))),
  condition = seq(1, min(c(x[["n_conditions"]], 9))),
  vars = NULL,
  show_constants = FALSE,
  nrows = ceiling(sqrt(max(condition))),
  margin = 0.05,
  shareX = TRUE,
  shareY = TRUE,
  palette = "Dark 2",
  alpha = list(central = 1, spread = 0.3, sims = 0.3),
  colors = NULL,
  line_width = list(central = 3, spread = 0, sims = 1),
  font_family = default_font_family(),
  font_size = 16,
  wrap_width = 25,
  showlegend = TRUE,
  label_subplots = TRUE,
  central = c("mean", "median", "none"),
  spread = c("quantile", "sd", "range"),
  format_label = TRUE,
  condition_display = c("subplots", "slider", "dropdown"),
  control_options = list(),
  animation = c("none", "time"),
  webgl = getOption("sdbuildR.webgl", default = TRUE),
  ...
)
```

## Arguments

- x:

  Output of
  [`ensemble()`](https://kcevers.github.io/sdbuildR/reference/ensemble.md).

- which:

  Type of plot. Either `"summary"` for a summary plot with mean or
  median lines and confidence intervals, or `"sims"` for individual
  simulation trajectories with mean or median lines. Defaults to
  `"summary"`.

- sim:

  Indices of the individual trajectories to plot if which = `"sims"`.
  Defaults to the first `min(n, 100)` trajectories. Including a high
  number of trajectories will slow down plotting considerably.

- condition:

  Indices of the condition(s) to plot. Defaults to 1:9.

- vars:

  Variables to plot. Defaults to `NULL` to plot all variables.

- show_constants:

  If `TRUE`, include constants in plot. Defaults to `FALSE`.

- nrows:

  Number of rows in the plot grid. Defaults to
  `ceiling(sqrt(max(condition)))`.

- margin:

  Margin between subplots. Either a single numeric or a vector of length
  four(left, right, top, bottom). See `?plotly::subplot()` for more
  details. Defaults to 0.05.

- shareX:

  If `TRUE`, share the x-axis across subplots. Defaults to `TRUE`.

- shareY:

  If `TRUE`, share the y-axis across subplots. Defaults to `TRUE`.

- palette:

  Colour palette. Must be one of hcl.pals().

- alpha:

  Opacity, with the same grammar as `line_width`: a single value, a
  named per-variable vector, or a list keyed by layer
  (`central`/`spread`/ `sims`). Defaults to
  `list(central = 1, spread = 0.3, sims = 0.3)`.

- colors:

  Colours for the plotted variables. A named vector (names are variable
  names) sets the colours of those variables and the palette fills the
  rest, so you can recolour only a few variables. An unnamed vector
  assigns colours in plot order. `NULL` uses `palette`. Defaults to
  `NULL`.

- line_width:

  Line width(s). The plot draws three layers: the central tendency line
  (`central`), the uncertainty band's border (`spread`), and the
  individual trajectories (`sims`). Supply a single value (used for
  every layer and variable), a named per-variable vector (names are
  variable names; unnamed values fill in plot order), or a list keyed by
  layer (`list(central = , spread = , sims = )`) whose elements are
  themselves a single value or per-variable vector. Unspecified
  layers/variables fall back to the defaults. Defaults to
  `list(central = 3, spread = 0, sims = 1)` (a `spread` width of `0`
  draws no band border).

- font_family:

  Font family. Kebab-case names (e.g. `"eb-garamond"`) are Fontsource
  ids (browse them at <https://fontsource.org/>), loaded as webfonts: no
  installation is needed, but internet access is required to display
  them. Other fonts must be installed on the system viewing the plot.
  Defaults to the `sdbuildR.font_family` option, or the
  `"stix-two-text"` webfont when the option is unset; use
  `options(sdbuildR.font_family = )` to change the default for all
  plots, e.g. in your .Rprofile.

- font_size:

  Font size. Defaults to 16.

- wrap_width:

  Width of text wrapping for labels. Must be an integer. Defaults to 25.

- showlegend:

  Whether to show legend. Must be TRUE or FALSE. Defaults to TRUE.

- label_subplots:

  Whether to plot labels indicating the condition of the subplot.

- central:

  Which central-tendency line to draw, given as preferences in order:
  the first one that
  [`ensemble()`](https://kcevers.github.io/sdbuildR/reference/ensemble.md)
  computed is used. For example, `c("mean", "median")` draws the mean if
  it is available, otherwise the median, and `"none"` draws no line.
  Defaults to `c("mean", "median", "none")`.

- spread:

  Which uncertainty band to draw, again as ordered preferences:
  `"quantile"` (between the lowest and highest quantile), `"sd"` (the
  central line plus/minus one standard deviation), `"range"` (between
  `min` and `max`), or `"none"`. The first band the computed statistics
  can support is used; `"sd"` also needs a central line. Defaults to
  `c("quantile", "sd", "range")`.

- format_label:

  If `TRUE`, apply default formatting (replacing periods and underscores
  with spaces) to variable labels that are the same as the variable
  name. Applies to the legend and any condition controls. Defaults to
  `TRUE`.

- condition_display:

  How to display multiple conditions. Use `"subplots"` to show
  conditions as panels, `"slider"` to select one condition with a
  slider, or `"dropdown"` to select one condition with a dropdown.
  Defaults to `"subplots"`. If only a single condition is available
  (e.g. no conditions were varied), `"slider"` and `"dropdown"` fall
  back to `"subplots"` with a message. To guarantee the control
  geometry, plots with a slider/dropdown have a fixed height (sized to
  the number of controls) instead of a responsive one.

- control_options:

  Named list fine-tuning the `"slider"`/`"dropdown"` condition control
  and the `animation = "time"` animation. For the condition control it
  supports `max_labels`: the maximum number of slider tick labels to
  keep visible when many conditions are varied (the slider always keeps
  one step per condition; intermediate labels are thinned above this
  count); and `spacing`: the vertical gap (in pixels) between the tops
  of stacked controls when several condition parameters are varied. By
  default the spacing and the reserved bottom margin are sized
  automatically so the controls never overlap each other or the x-axis
  title; pass a number to widen or tighten the gap. For the animation it
  supports `frame_ms`: the duration of each frame in milliseconds
  (default `100`); `duration`: the total animation length in seconds, as
  an alternative to `frame_ms` (supplying both is an error);
  `transition_ms`: the transition time between frames in milliseconds
  (default `0`); and `max_frames`: the maximum number of animation
  frames (default `50`). Defaults to
  [`list()`](https://rdrr.io/r/base/list.html), i.e. all defaults.

- animation:

  Animation mode. Use `"none"` for a static plot or `"time"` to
  cumulatively reveal trajectories over time. Defaults to `"none"`. Time
  animation requires `which = "sims"` (confidence ribbons cannot be
  animated) and a single condition (one panel); combining it with
  `condition_display` controls or multiple conditions is not supported.

- webgl:

  If `TRUE`, render trajectories with WebGL (plotly `scattergl`) for
  performance with many lines; if `FALSE`, use SVG (`scatter`). Defaults
  to `getOption("sdbuildR.webgl", default = TRUE)`. Set
  `options(sdbuildR.webgl = FALSE)` (e.g. in vignettes or dashboards, or
  when a plot renders blank) to disable WebGL globally.

- ...:

  Optional parameters

## Value

Plotly object

## Styling variables and layers

Names in `colors`, `alpha`, and `line_width` refer to the model variable
names, not the labels shown in the legend. For example, use `infected`,
not `Infected`, even when `format_label = TRUE` changes the legend text.

A single value applies everywhere:
`plot(sims, line_width = 3, alpha = 0.7)`.

A named vector styles selected variables and leaves the rest at their
defaults:
`plot(sims, colors = c(infected = "firebrick"), line_width = c(infected = 4))`.

Ensemble plots also have layers. Use a list to style the central
tendency line, the uncertainty band border, and individual trajectories
separately:
`plot(sims, which = "sims", line_width = list(central = 3, sims = 1, spread = 0))`.

List elements can be named vectors too, which is useful when only one
layer needs variable-specific styling:
`plot(sims, alpha = list(central = 1, sims = c(infected = 0.15), spread = 0.25))`.

The `spread` line width controls the border of the uncertainty band. The
default is `0`, so the band is filled but not outlined.

## See also

[`ensemble()`](https://kcevers.github.io/sdbuildR/reference/ensemble.md)
