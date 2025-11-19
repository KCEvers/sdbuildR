# Save plot to a file

Save a plot of a stock-and-flow diagram or a simulation to a specified
file path. Note that saving plots requires additional packages to be
installed (see below).

## Usage

``` r
export_plot(pl, file, width = 3, height = 4, units = "cm", dpi = 300)
```

## Arguments

- pl:

  Plot object.

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

## Value

Returns `NULL` invisibly, called for side effects.

## Examples

``` r
# Only if dependencies are installed
if (require("DiagrammeRsvg", quietly = TRUE) &
  require("rsvg", quietly = TRUE)) {
  sfm <- xmile("SIR")
  file <- tempfile(fileext = ".png")
  export_plot(plot(sfm), file)

  # Remove plot
  file.remove(file)
}
#> Linking to librsvg 2.58.0
#> [1] TRUE

if (FALSE) { # interactive()
if (FALSE) { # \dontrun{
# requires internet
# Only if dependencies are installed
if (require("htmlwidgets", quietly = TRUE) &
  require("webshot2", quietly = TRUE)) {
  # Requires Chrome to save plotly plot:
  sim <- simulate(sfm)
  export_plot(plot(sim), file)

  # Remove plot
  file.remove(file)
}
} # }
}
```
