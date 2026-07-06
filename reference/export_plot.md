# Save plot to a file

Save a plot of a stock-and-flow diagram or a simulation to a specified
file path. Note that saving plots requires additional packages to be
installed (see below).

## Usage

``` r
export_plot(
  pl,
  file,
  width = 3,
  height = 4,
  units = "cm",
  dpi = 300,
  font_family = NULL,
  close_browser = TRUE
)
```

## Arguments

- pl:

  Plot object. Can be a `grViz` object from the DiagrammeR package (for
  stock-and-flow diagrams) or a `plotly` object from the plotly package
  (for (ensemble) simulation results).

- file:

  File path to save plot to, including a file extension. For plotting a
  stock-and-flow model, the file extension can be one of png, pdf, svg,
  ps, eps, webp. For plotting a simulation, the file extension can be
  one of png, pdf, jpg, jpeg, webp. If no file extension is specified,
  it will default to png.

- width:

  Width of image in units.

- height:

  Height of image in units.

- units:

  Units in which width and height are specified. Either "cm", "in", or
  "px".

- dpi:

  Resolution of image. Only used if units is not "px".

- font_family:

  Font family to render the plot in, overriding the font the plot was
  created with. Kebab-case names (e.g. `"eb-garamond"`) are loaded as
  webfonts (see Details); any other name must be installed on your
  system. Defaults to `NULL`, which keeps the plot's own font.

- close_browser:

  If `TRUE` (default), browser-based exports run in a separate
  short-lived R process, so the headless browser is closed when the
  export finishes and leaves no state behind in your R session. Set to
  `FALSE` to render in a persistent browser session instead, which is
  faster when exporting many plots in a row; the browser then stays open
  in the background until your R session ends. Ignored for exports that
  do not use a browser.

## Value

Invisibly returns the file path of the exported plot, called for side
effects.

## Details

Exports that render in a headless browser (plotly plots, and diagrams
using a webfont) require the suggested packages webshot2 and callr, plus
a Chrome-based browser on your system. See `close_browser` for how the
browser's lifetime is managed.

## Fonts

When `font_family` (either passed here or to the
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) call that
created `pl`) is an all-lowercase kebab-case name, it is treated as a
font identifier from Fontsource (<https://fontsource.org/>) – browse the
catalogue there and use the id from the font page's URL, e.g.
`"eb-garamond"` or `"source-serif-4"`. The font is then loaded as a
*webfont*: a CSS rule pointing to the font's files on the jsDelivr
content delivery network is attached to the plot, and the headless
browser that renders the export fetches the font over the internet at
render time, just like a webpage. No fonts are downloaded by R or
installed on your system. Internet access is required during the export;
without it, the export still succeeds in a fallback font. Note that
webfonts are not supported for the ps/eps formats, which fall back to
system fonts.

## Examples

``` r

# Only if dependencies are installed
if (requireNamespace("DiagrammeRsvg", quietly = TRUE) &&
  requireNamespace("rsvg", quietly = TRUE)) {
  sfm <- stockflow("sir")
  file <- tempfile(fileext = ".png")

  # With a system font the diagram is rendered by rsvg; the default
  # webfont would instead require webshot2 and a headless browser
  export_plot(plot(sfm, font_family = "serif"), file)

  # Remove plot
  file.remove(file)
}
#> [1] TRUE

if (FALSE) { # \dontrun{
# requires internet
# Only if suggested dependencies are installed
if (requireNamespace("htmlwidgets", quietly = TRUE) &&
  requireNamespace("webshot2", quietly = TRUE)) {
  # Requires Chrome to save plotly plot:
  sim <- simulate(sfm)
  export_plot(plot(sim), file)

  # Remove plot
  file.remove(file)
}
} # }
```
