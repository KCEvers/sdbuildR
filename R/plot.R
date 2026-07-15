#' Save plot to a file
#'
#' Save a plot of a stock-and-flow diagram or a simulation to a specified file path. Note that saving plots requires additional packages to be installed (see below).
#'
#' @param pl Plot object. Can be a `grViz` object from the DiagrammeR package (for stock-and-flow diagrams) or a `plotly` object from the plotly package (for (ensemble) simulation results).
#' @param file File path to save plot to, including a file extension. For plotting a stock-and-flow model, the file extension can be one of png, pdf, svg, ps, eps, webp. For plotting a simulation, the file extension can be one of png, pdf, jpg, jpeg, webp. If no file extension is specified, it will default to png.
#' @param width Width of image in units.
#' @param height Height of image in units.
#' @param units Units in which width and height are specified. Either "cm", "in", or "px".
#' @param dpi Resolution of image. Only used if units is not "px".
#' @param font_family Font family to render the plot in, overriding the font
#'   the plot was created with. Kebab-case names (e.g. `"eb-garamond"`) are
#'   loaded as webfonts (see Details); any other name must be installed on
#'   your system. Defaults to `NULL`, which keeps the plot's own font.
#' @param close_browser If `TRUE` (default), browser-based exports run in a
#'   separate short-lived R process, so the headless browser is closed when
#'   the export finishes and leaves no state behind in your R session. Set to
#'   `FALSE` to render in a persistent browser session instead, which is
#'   faster when exporting many plots in a row; the browser then stays open
#'   in the background until your R session ends. Ignored for exports that do
#'   not use a browser.
#'
#' @details
#' Exports that render in a headless browser (plotly plots, and diagrams
#' using a webfont) require the suggested packages webshot2 and callr, plus a
#' Chrome-based browser on your system. See `close_browser` for how the
#' browser's lifetime is managed.
#'
#' # Fonts
#'
#' When `font_family` (either passed here or to the `plot()` call that
#' created `pl`) is an all-lowercase kebab-case name, it is treated as a font
#' identifier from Fontsource (<https://fontsource.org/>) -- browse the
#' catalogue there and use the id from the font page's URL, e.g.
#' `"eb-garamond"` or `"source-serif-4"`. The font is then loaded as a
#' *webfont*: a CSS rule pointing to the font's files on the jsDelivr content
#' delivery network is attached to the plot, and the headless browser that
#' renders the export fetches the font over the internet at render time, just
#' like a webpage. No fonts are downloaded by R or installed on your system.
#' Internet access is required during the export; without it, the export
#' still succeeds in a fallback font. Note that webfonts are not supported
#' for the ps/eps formats, which fall back to system fonts.
#'
#' @returns Invisibly returns the file path of the exported plot, called for side effects.
#' @export
#' @concept convenience
#'
#' @examples
#'
#' # Only if dependencies are installed
#' if (requireNamespace("DiagrammeRsvg", quietly = TRUE) &&
#'   requireNamespace("rsvg", quietly = TRUE)) {
#'   sfm <- stockflow("sir")
#'   file <- tempfile(fileext = ".png")
#'
#'   # With a system font the diagram is rendered by rsvg; the default
#'   # webfont would instead require webshot2 and a headless browser
#'   export_plot(plot(sfm, font_family = "serif"), file)
#'
#'   # Remove plot
#'   file.remove(file)
#' }
#'
#' @examplesIf has_internet()
#' \dontrun{
#' # requires internet
#' # Only if suggested dependencies are installed
#' if (requireNamespace("htmlwidgets", quietly = TRUE) &&
#'   requireNamespace("webshot2", quietly = TRUE)) {
#'   # Requires Chrome to save plotly plot:
#'   sim <- simulate(sfm)
#'   export_plot(plot(sim), file)
#'
#'   # Remove plot
#'   file.remove(file)
#' }
#' }
export_plot <- function(pl, file, width = 3, height = 4, units = "cm", dpi = 300, font_family = NULL, close_browser = TRUE) {
  if (!is.logical(close_browser) || length(close_browser) != 1 || is.na(close_browser)) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg close_browser} argument.",
      ">" = "The {.arg close_browser} argument must be a single {.code TRUE} or {.code FALSE}."
    ))
  }

  if (!is.null(font_family)) {
    if (!is.character(font_family) || length(font_family) != 1 || !nzchar(font_family)) {
      cli::cli_abort(c(
        "x" = "Invalid {.arg font_family} argument.",
        ">" = "The {.arg font_family} argument must be a single font family name."
      ))
    }
    pl <- set_plot_font(pl, font_family)
  }

  # Auto-detect format
  format <- tolower(tools::file_ext(file))

  if (!nzchar(format)) {
    # Default to png
    format <- "png"
    file <- paste0(file, ".", format)
  }

  width_px <- width
  height_px <- height

  # Convert dimensions to pixels
  if (units == "in") {
    width_px <- width * dpi
    height_px <- height * dpi
  } else if (units == "cm") {
    width_px <- width * dpi / 2.54
    height_px <- height * dpi / 2.54
  }

  if ("grViz" %in% class(pl)) {
    export_diagram(pl, file, format,
      width = width_px, height = height_px,
      close_browser = close_browser
    )
  } else if ("plotly" %in% class(pl)) {
    export_widget(pl, file,
      format = format,
      width = width_px, height = height_px,
      close_browser = close_browser
    )
  } else {
    cli::cli_abort(c(
      "x" = "Unsupported plot object class.",
      "i" = "The {.fn export_plot} function does not support plot objects of class {.cls {class(pl)}}.",
      ">" = "Use a {.cls grViz} or {.cls plotly} object."
    ))
  }

  invisible(file)
}


#' Restyle the font of an existing plot object
#'
#' For plotly, the plot is built and every font specification in the spec
#' (layout font, axis titles, tick labels, legend, annotations, sliders,
#' updatemenus, trace text, animation frames, ...) is overwritten. For grViz,
#' the fontname attributes baked into the DOT source are rewritten. In both
#' cases a webfont is attached when the family is a webfont id.
#'
#' @inheritParams export_plot
#' @returns The restyled plot object.
#' @noRd
set_plot_font <- function(pl, font_family) {
  if (inherits(pl, "grViz")) {
    pl$x$diagram <- gsub(
      "fontname=['\"][^'\"]*['\"]", sprintf("fontname=\"%s\"", font_family),
      pl$x$diagram
    )
  } else if (inherits(pl, "plotly")) {
    # Building resolves all font specifications into the spec (it is
    # idempotent and happens at render time anyway), so overwriting every
    # family field covers fonts set per-element at plot creation.
    pl <- plotly::plotly_build(pl)
    pl$x$layout <- replace_font_family(pl$x$layout, font_family)
    pl$x$data <- replace_font_family(pl$x$data, font_family)
    pl$x$frames <- replace_font_family(pl$x$frames, font_family)

    # Global font so unset families inherit the new one too
    pl$x$layout$font$family <- font_family
  }

  # Drop any webfont attached at plot creation; apply_webfont() re-adds it
  # when the new family is a webfont id, so a system-font override does not
  # force the browser-based export path
  attr(pl, "sdbuildR_webfont") <- NULL

  apply_webfont(pl, font_family)
}


#' Recursively overwrite every font family in a plotly spec fragment
#'
#' Font objects appear at many nesting levels of a built spec. In the plotly
#' schema the `family` field only occurs inside font objects, so replacing
#' every character `family` covers them all exactly.
#'
#' @param x A fragment of a built plotly spec (or NULL).
#' @param font_family Font family name.
#' @returns The fragment with all font families replaced.
#' @noRd
replace_font_family <- function(x, font_family) {
  if (!is.list(x)) {
    return(x)
  }

  if (is.character(x[["family"]])) {
    x[["family"]] <- font_family
  }

  for (i in seq_along(x)) {
    if (is.list(x[[i]])) {
      x[[i]] <- replace_font_family(x[[i]], font_family)
    }
  }

  x
}


#' Export diagram
#'
#' @inheritParams export_plot
#' @param format Output format.
#'
#' @returns Returns `NULL` invisibly.
#' @noRd
#'
export_diagram <- function(pl, file, format, width, height, close_browser = TRUE) {
  # Diagrams carrying a webfont cannot go through rsvg (librsvg resolves
  # system fonts only, not @font-face rules), so they are rendered in a
  # headless browser like plotly exports. SVG keeps the vector pipeline, with
  # the @font-face CSS injected for browsers viewing the file.
  webfont <- attr(pl, "sdbuildR_webfont")
  if (!is.null(webfont)) {
    if (format %in% c("png", "pdf", "jpg", "jpeg", "webp")) {
      # The SVG is placed on a bare HTML page (no widget JS, which interferes
      # with scaling) and stretched to the browser viewport; its viewBox
      # preserves the aspect ratio, matching how the rsvg path fits the
      # diagram to the requested size.
      rlang::check_installed("DiagrammeRsvg", reason = "to export stock-and-flow diagrams to image files.")
      svg <- inject_svg_webfont(DiagrammeRsvg::export_svg(pl), webfont)

      temp_html <- tempfile(fileext = ".html")
      on.exit(remove_files(temp_html), add = TRUE)
      writeLines(paste0(
        "<!DOCTYPE html><html><head><meta charset=\"utf-8\">",
        "<style>body { margin: 0; padding: 0; } svg { width: 100vw; height: 100vh; }</style>",
        "</head><body>", svg, "</body></html>"
      ), temp_html)

      return(webshot_html(temp_html, file, format, width, height,
        close_browser = close_browser
      ))
    } else if (format == "svg") {
      rlang::check_installed("DiagrammeRsvg", reason = "to export stock-and-flow diagrams to image files.")
      svg <- DiagrammeRsvg::export_svg(pl)
      writeLines(inject_svg_webfont(svg, webfont), file)
      return(invisible(file))
    } else {
      cli::cli_warn(c(
        "!" = "Webfonts are not supported for {.val {format}} exports.",
        "i" = "The {.val {webfont}} font will only render if it is installed on your system."
      ))
    }
  }

  rlang::check_installed("rsvg", reason = "to export stock-and-flow diagrams to image files.")

  rlang::check_installed("DiagrammeRsvg", reason = "to export stock-and-flow diagrams to image files.")

  temp <- charToRaw(DiagrammeRsvg::export_svg(pl))

  if (format == "webp") {
    rsvg::rsvg_webp(temp, file, width = width, height = height)
  } else if (format == "png") {
    rsvg::rsvg_png(temp, file, width = width, height = height)
  } else if (format == "pdf") {
    rsvg::rsvg_pdf(temp, file, width = width, height = height)
  } else if (format == "svg") {
    rsvg::rsvg_svg(temp, file, width = width, height = height)
  } else if (format == "ps") {
    rsvg::rsvg_ps(temp, file, width = width, height = height)
  } else if (format == "eps") {
    rsvg::rsvg_eps(temp, file, width = width, height = height)
  } else {
    cli::cli_abort(c(
      "x" = "Unsupported {.arg format} value.",
      "i" = "The format {.val {format}} is not supported.",
      ">" = "Use one of: {.code c('webp', 'png', 'pdf', 'svg', 'ps', 'eps')}."
    ))
  }

  invisible(file)
}


#' Export an htmlwidget (plotly or grViz) via a headless browser
#'
#' The widget is saved to a temporary HTML file and rendered in a headless
#' browser, so any webfont CSS attached to it is honoured.
#'
#' @inheritParams export_plot
#' @param format Output format.
#'
#' @returns Returns `NULL` invisibly.
#' @noRd
#'
export_widget <- function(pl, file, format, width, height, close_browser = TRUE) {
  rlang::check_installed("htmlwidgets",
    reason = "to export plots to image files."
  )

  # Create temporary HTML file
  temp_html <- tempfile(fileext = ".html")
  on.exit(remove_files(temp_html), add = TRUE)
  htmlwidgets::saveWidget(pl, temp_html, selfcontained = TRUE)

  webshot_html(temp_html, file, format, width, height,
    close_browser = close_browser
  )
}


#' Screenshot an HTML file to an image file via a headless browser
#'
#' @param url Path to the HTML file to render.
#' @inheritParams export_plot
#' @param format Output format.
#'
#' @returns Returns `NULL` invisibly.
#' @noRd
#'
webshot_html <- function(url, file, format, width, height, close_browser = TRUE) {
  needed <- if (close_browser) c("webshot2", "callr") else "webshot2"
  rlang::check_installed(needed,
    reason = "to export plots to image files."
  )

  # Set webshot2 parameters based on format
  webshot_params <- list(
    url = url,
    file = file,
    vwidth = width,
    vheight = height,
    delay = 1
  )

  # Format-specific settings
  if (!format %in% c("jpg", "jpeg", "webp", "png", "pdf")) {
    cli::cli_warn(c(
      "!" = "The format {.val {format}} may not be supported by {.pkg webshot2}.",
      "i" = "Attempting export anyway..."
    ))
  }

  # With close_browser = TRUE the screenshot is taken in a short-lived R
  # subprocess, so the headless browser and its supervisor (with their open
  # connections) live and die with the subprocess instead of lingering in the
  # user's session. With FALSE the browser is reused in the current session,
  # which is faster across many exports but leaves it running until R exits.
  take_shot <- function() {
    if (close_browser) {
      callr::r(
        function(params) {
          options(webshot.quiet = TRUE)
          do.call(webshot2::webshot, params)
        },
        args = list(params = webshot_params)
      )
    } else {
      old_option <- getOption("webshot.quiet")
      options(webshot.quiet = TRUE)
      on.exit(options(webshot.quiet = old_option), add = TRUE)
      do.call(webshot2::webshot, webshot_params)
    }
  }

  tryCatch(
    take_shot(),
    error = function(e) {
      cli::cli_abort(c(
        "x" = "Failed to export plot.",
        ">" = "This typically means Chrome/Chromium could not be launched. Try again.",
        "i" = conditionMessage(e)
      ), class = "stockflow_export_error")
    }
  )

  invisible(file)
}


#' Plot stock-and-flow diagram
#'
#' Visualize a stock-and-flow diagram using the R package DiagrammeR. Stocks are represented as boxes. Flows are represented as arrows between stocks and/or double circles, where the latter represent what it outside of the model boundary. Thin grey edges indicate dependencies between variables. By default, constants (indicated by italic labels) are not shown. Hover over the variables to see their equations.
#'
#' @param x A stock-and-flow model object of class [`stockflow`][stockflow].
#' @param vars Variables to plot. Defaults to NULL to plot all variables.
#' @param format_label If TRUE, apply default formatting (removing periods and underscores) to labels if labels are the same as variable names.
#' @param wrap_width Width of text wrapping for labels. Must be an integer. Defaults to 20.
#' @param font_size Font size. Defaults to 18.
#' @param font_family Font name. Kebab-case names (e.g. `"eb-garamond"`) are
#'   Fontsource ids (browse them at <https://fontsource.org/>), loaded as
#'   webfonts: no installation is needed, but internet access is required to
#'   display them. Other fonts must be installed on the system viewing the
#'   plot. Defaults to the `sdbuildR.font_family` option, or the
#'   `"stix-two-text"` webfont when the option is unset; use
#'   `options(sdbuildR.font_family = )` to change the default for all plots,
#'   e.g. in your .Rprofile.
#' @param colors Colours of the diagram variables, by variable type or
#'   variable name. A single colour applies to every variable. A named list
#'   keyed by type (`list(stock = , flow = , constant = , aux = )`), each entry
#'   a single colour, overrides the colour of those types and keeps the
#'   defaults for the rest (as with [utils::modifyList()]); the defaults are
#'   `list(stock = "#83d3d4", flow = "#f48153", constant = "grey90",
#'   aux = "grey90")`. A named vector (names are variable names, as in the
#'   other plot methods) recolours only those variables and leaves the rest at
#'   their type default. Stocks, constants, and auxiliaries are coloured by
#'   their node fill; flows by the coloured band of their flow arrows.
#'   Defaults to `NULL` (the default type colours).
#' @param color_dependency Colour of dependency arrows. Defaults to "#999999".
#' @param font_color Colour of variable labels (and of the equation text when `show_eqn = TRUE`). Defaults to "black".
#' @param show_eqn If `TRUE`, show each variable's equation on a new line beneath its label, in a smaller font and the same colour as the label (`font_color`). The equation is prefixed by what it defines for that variable type: `Initial value =` for stocks, `Rate =` for flows, `Value =` for constants, and `Equation =` for auxiliaries. Defaults to `TRUE`.
#' @param show_tooltip If `TRUE`, show each variable's equation as a tooltip when hovering over it. Defaults to `TRUE`.
#' @param show_dependencies If TRUE, show dependencies between variables. Defaults to TRUE.
#' @param show_constants If TRUE, show constants. Defaults to FALSE.
#' @param show_aux If TRUE, show auxiliary variables. Defaults to TRUE.
#' @param flow_length Minimum length of flow arrows; controls how far apart
#'   connected variables sit along the flow direction. Must be an integer.
#'   Defaults to 1.
#' @param margin Padding around the diagram. Defaults to 0.1.
#' @param spacing Minimum space between adjacent variables placed side by side
#'   (across the flow direction). Defaults to 0.5.
#' @param direction Overall flow direction of the layout, passed to Graphviz's
#'   `rankdir`. One of `"LR"` (left-to-right, the default), `"TB"` (top-to-bottom),
#'   `"RL"` (right-to-left), or `"BT"` (bottom-to-top).
#' @param align Optional alignment of variables *across* the flow direction. A
#'   character vector of variable names, or a list of such vectors. Each group is
#'   placed on the same Graphviz rank (`{rank=same; ...}`), so its members line up
#'   (vertically when `direction = "LR"`, horizontally when `direction = "TB"`).
#'   Works for any variable (stocks, flows, auxiliaries, constants), not only
#'   stocks. Names that are not currently drawn (hidden by `vars`,
#'   `show_constants`, or `show_aux`) are ignored with a warning; unknown names
#'   raise an error. Defaults to `NULL`.
#' @param order Optional ordering of variables *along* the flow direction. A
#'   character vector of variable names, or a list of such vectors, giving the
#'   desired sequence. Implemented as invisible edges between consecutive names,
#'   so it acts as a soft hint that Graphviz balances against the real flows
#'   rather than a hard constraint. On its own it sequences variables into
#'   successive ranks (e.g. separate columns when `direction = "LR"`); to instead
#'   line variables up in a single rank and control their order *within* it,
#'   combine `order` with `align` (the `align` group sets the rank, `order` sets
#'   the position within it). Same validation as `align`. Defaults to `NULL`.
#' @param arrowhead_dependency Shape of the arrowhead on dependency arrows,
#'   passed to Graphviz's `arrowhead`. One of `"open"` (the default, an open
#'   V-shape), `"normal"`, `"vee"`, `"diamond"`, `"dot"`, `"box"`, `"crow"`,
#'   `"curve"`, `"inv"`, `"tee"`, or `"none"`. See
#'   <https://graphviz.org/docs/attr-types/arrowType/> for illustrations.
#' @param ... Optional arguments
#'
#' @returns Stock-and-flow diagram
#' @export
#' @concept build
#' @method plot stockflow
#' @seealso [import_insightmaker()], [stockflow()], [plot.simulate_stockflow()]
#'
#' @examples
#' sfm <- stockflow("sir")
#' plot(sfm)
#'
#' # Don't show constants or auxiliaries
#' plot(sfm, show_constants = FALSE, show_aux = FALSE)
#'
#' # Only show specific variables
#' plot(sfm, vars = "susceptible")
#'
#' # Hide the equations shown beneath each label
#' plot(sfm, show_eqn = FALSE)
#'
#' # Hide the equation tooltips shown on hover
#' plot(sfm, show_tooltip = FALSE)
#'
#' # Custom label colour
#' plot(sfm, font_color = "#333333")
#'
#' # Colour every variable the same
#' plot(sfm, colors = "lightblue")
#'
#' # Change only the stock colour; other types keep their defaults
#' plot(sfm, colors = list(stock = "gold"))
#'
#' # Recolour a single variable
#' plot(sfm, colors = c(susceptible = "gold"))
#'
#' # Lay the model out top-to-bottom instead of left-to-right
#' plot(sfm, direction = "TB")
#'
#' # Align variables across the flow direction (same Graphviz rank)
#' plot(sfm, align = c("susceptible", "recovered"))
#'
#' # Order variables along the flow direction (soft hint via invisible edges)
#' plot(sfm, order = c("susceptible", "infected", "recovered"))
#'
plot.stockflow <- function(x,
                           vars = NULL,
                           format_label = TRUE,
                           wrap_width = 20,
                           font_size = 18,
                           font_family = getOption("sdbuildR.font_family", default = "stix-two-text"),
                           colors = NULL,
                           color_dependency = "#999999",
                           font_color = "black",
                           show_eqn = TRUE,
                           show_tooltip = TRUE,
                           show_dependencies = TRUE,
                           show_constants = FALSE,
                           show_aux = TRUE,
                           flow_length = 1,
                           margin = 0.1,
                           spacing = 0.5,
                           direction = "LR",
                           align = NULL,
                           order = NULL,
                           arrowhead_dependency = "open",
                           ...) {
  check_stockflow(x)

  if (!is.logical(show_eqn) || length(show_eqn) != 1 || is.na(show_eqn)) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg show_eqn} argument.",
      ">" = "The {.arg show_eqn} argument must be either TRUE or FALSE."
    ))
  }

  if (!is.logical(show_tooltip) || length(show_tooltip) != 1 || is.na(show_tooltip)) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg show_tooltip} argument.",
      ">" = "The {.arg show_tooltip} argument must be either TRUE or FALSE."
    ))
  }

  valid_direction <- c("LR", "TB", "RL", "BT")
  if (!is.character(direction) || length(direction) != 1L || !direction %in% valid_direction) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg direction} argument.",
      ">" = "The {.arg direction} argument must be one of {.val {valid_direction}}."
    ))
  }

  valid_arrowhead <- c(
    "open", "normal", "vee", "diamond", "dot",
    "box", "crow", "curve", "inv", "tee", "none"
  )
  if (!is.character(arrowhead_dependency) || length(arrowhead_dependency) != 1L ||
    !arrowhead_dependency %in% valid_arrowhead) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg arrowhead_dependency} argument.",
      ">" = "The {.arg arrowhead_dependency} argument must be one of {.val {valid_arrowhead}}."
    ))
  }

  # Anchor flow edges to the node sides that face the flow direction, so the
  # straight flow segments stay aligned with the layout: an edge leaves the
  # tail node on the downstream side and enters the head node on the upstream
  # side. (Dependency edges use the centre port '_', which needs no adjustment.)
  flow_ports <- switch(direction,
    LR = c(tail = "e", head = "w"),
    RL = c(tail = "w", head = "e"),
    TB = c(tail = "s", head = "n"),
    BT = c(tail = "n", head = "s")
  )

  # Get property dataframe
  df <- as.data.frame(x, properties = c("type", "name", "label", "eqn"))

  # All variable names in the model, before any vars filtering (for align/order
  # typo detection: a name absent from this is a typo, a name present but not
  # drawn is ignored with a warning).
  model_var_names <- df[["name"]]

  # Check whether there are any variables
  if (nrow(df) == 0) {
    cli::cli_warn(c(
      "i" = "Model contains no variables.",
      ">" = "Add variables using {.fn stock}, {.fn flow}, {.fn constant}, and {.fn aux}."
    ))
    return(invisible(NULL))
  }

  # Get dependencies
  dep <- dependencies(x)
  flow_df <- get_flow_df(x)

  if (!is.null(vars)) {
    vars <- clean_vars(vars)

    # Check whether specified variables are in the model
    validate_vars_in_model(vars, df, context = "model")

    # Only keep these variables in flow_df, dep, and df
    df <- df[df[["name"]] %in% vars, , drop = FALSE]
    dep <- dep[names(dep) %in% vars]
    flow_df <- flow_df[flow_df[["name"]] %in% vars, , drop = FALSE]

    # Set stocks not in vars to ''
    flow_df[["to"]][!flow_df[["to"]] %in% vars] <- ""
    flow_df[["from"]][!flow_df[["from"]] %in% vars] <- ""

    # Set show_aux to TRUE if any variables are aux
    if (any(vars %in% df[df[["type"]] == "aux", "name"])) {
      show_aux <- TRUE
    }

    # Set show_constants to TRUE if any variables are constants
    if (any(vars %in% df[df[["type"]] == "constant", "name"])) {
      show_constants <- TRUE
    }
  }

  # Per-variable node/edge colours, resolved by variable type and/or name.
  # Names are validated against the full model so a name hidden by `vars` is
  # ignored silently rather than warned about as unknown.
  var_colors <- resolve_diagram_colors(colors, df, valid_names = model_var_names)

  if (format_label) {
    df[["label"]] <- format_label_if_default(df[["name"]], df[["label"]])
  }

  # Unwrapped, human-readable labels for tooltips (before wrapping/escaping).
  # Single quotes are stripped so the labels are safe inside quoted DOT strings.
  dict_label <- stats::setNames(gsub("'", "", df[["label"]], fixed = TRUE), df[["name"]])

  # Prepare and format labels using centralized helper
  df <- prepare_labels(df, wrap_width = wrap_width, format_label = FALSE, deduplicate = FALSE)
  dict <- stats::setNames(df[["label"]], df[["name"]])

  # Get equations and remove quotation marks from unit strings
  eqn_label <- gsub("'", "", df[["eqn"]], fixed = TRUE)
  eqn_label <- gsub("\"", "", eqn_label, fixed = TRUE)
  dict_eqn <- stats::setNames(eqn_label, df[["name"]])

  # Categorize variables by type
  stock_names <- df[df[["type"]] == "stock", "name"]
  flow_names <- df[df[["type"]] == "flow", "name"]
  aux_names <- df[df[["type"]] == "aux", "name"]
  const_names <- df[df[["type"]] == "constant", "name"]

  # Filter based on show parameters
  if (!show_constants) const_names <- character(0)
  if (!show_aux) aux_names <- character(0)

  plot_var <- c(stock_names, flow_names)
  if (show_constants) {
    plot_var <- c(plot_var, const_names)
  }
  if (show_aux) {
    plot_var <- c(plot_var, aux_names)
  }

  # Shared node style
  style_node <- sprintf(
    "node [fontsize=%s,fontname='%s',fontcolor='%s']",
    font_size, font_family, font_color
  )

  # Font size for the equation line (shown beneath the label when show_eqn = TRUE)
  eqn_font_size <- max(round(font_size * 0.7), 6)

  # Build informative tooltips per variable: type, label, name (when different),
  # equation/value, and structure (inflows/outflows for stocks, from/to for flows).
  dict_type <- stats::setNames(df[["type"]], df[["name"]])
  outside_label <- "outside model boundary"

  tooltip_dict <- vapply(plot_var, function(nm) {
    type <- dict_type[[nm]]
    inflows <- outflows <- character(0)
    from_label <- to_label <- NA_character_

    if (type == "stock") {
      inflow_names <- flow_df[["name"]][flow_df[["to"]] == nm]
      outflow_names <- flow_df[["name"]][flow_df[["from"]] == nm]
      inflows <- unname(dict_label[intersect(inflow_names, names(dict_label))])
      outflows <- unname(dict_label[intersect(outflow_names, names(dict_label))])
    } else if (type == "flow") {
      i <- which(flow_df[["name"]] == nm)
      if (length(i) == 1L) {
        from_var <- flow_df[["from"]][i]
        to_var <- flow_df[["to"]][i]
        from_label <- if (nzchar(from_var) && from_var %in% names(dict_label)) {
          unname(dict_label[[from_var]])
        } else {
          outside_label
        }
        to_label <- if (nzchar(to_var) && to_var %in% names(dict_label)) {
          unname(dict_label[[to_var]])
        } else {
          outside_label
        }
      }
    }

    node_tooltip(
      type, dict_label[[nm]], nm, dict_eqn[[nm]],
      inflows = inflows, outflows = outflows,
      from_label = from_label, to_label = to_label
    )
  }, character(1))

  # Build the tooltip fragment for a set of nodes (empty when show_tooltip = FALSE)
  node_tooltip_attr <- function(names) {
    if (!show_tooltip) {
      return(rep("", length(names)))
    }
    sprintf(", tooltip = '%s'", tooltip_dict[names])
  }

  # Prepare stock nodes
  if (length(stock_names) > 0) {
    style_stock <- "node [shape=box,style=filled]"

    if (show_eqn) {
      stock_label <- sprintf(
        "label=<%s>",
        make_eqn_label("stock", dict[stock_names], dict_eqn[stock_names], eqn_font_size, font_color, wrap_width)
      )
    } else {
      stock_label <- sprintf("label='%s'", dict[stock_names])
    }

    stock_nodes <- sprintf(
      "%s [id=%s,fillcolor='%s',%s%s]",
      paste0("'", stock_names, "'"),
      paste0("'", stock_names, "'"),
      var_colors[stock_names],
      stock_label,
      node_tooltip_attr(stock_names)
    )
  } else {
    style_stock <- stock_nodes <- ""
  }

  # Prepare auxiliary nodes
  if (length(aux_names) > 0) {
    style_aux <- sprintf(
      "node [shape=circle,fontsize=%s, width=0.15, height=0.15, fixedsize=true, style=filled]",
      font_size - 2
    )

    if (show_eqn) {
      aux_xlabel <- sprintf(
        "xlabel=<%s>",
        make_eqn_label("aux", dict[aux_names], dict_eqn[aux_names], eqn_font_size, font_color, wrap_width)
      )
    } else {
      aux_xlabel <- sprintf("xlabel='%s'", dict[aux_names])
    }

    aux_nodes <- sprintf(
      "%s [id=%s,fillcolor='%s',%s,label=''%s]",
      paste0("'", aux_names, "'"),
      paste0("'", aux_names, "'"),
      var_colors[aux_names],
      aux_xlabel,
      node_tooltip_attr(aux_names)
    )
  } else {
    style_aux <- aux_nodes <- ""
  }

  # Prepare constant nodes (italic font)
  if (length(const_names) > 0) {
    if (show_eqn) {
      formatted_labels <- make_eqn_label(
        "constant", dict[const_names], dict_eqn[const_names], eqn_font_size, font_color, wrap_width,
        italic = TRUE
      )
    } else {
      # Format labels: convert \n to <BR/> and add italics
      formatted_labels <- vapply(dict[const_names], function(label) {
        label_with_html_breaks <- gsub("\n", "<BR/>", label, fixed = TRUE)
        paste0("<I>", label_with_html_breaks, "</I>")
      }, character(1), USE.NAMES = FALSE)
    }

    style_const <- sprintf(
      "node [shape=diamond,fontsize=%s,width=0.15, height=0.15, fixedsize=true, style=filled]",
      font_size - 2
    )

    const_nodes <- sprintf(
      "%s [id=%s,fillcolor='%s',xlabel=<%s>,label=''%s]",
      paste0("'", const_names, "'"),
      paste0("'", const_names, "'"),
      var_colors[const_names],
      formatted_labels,
      node_tooltip_attr(const_names)
    )
  } else {
    style_const <- const_nodes <- ""
  }


  # Create rank groupings based on dependencies
  rank_groups <- list()

  # Reverse dependencies, because we want to couple constants/aux with the first variable that depends on them
  dep_rev <- reverse_dep(dep)

  # Helper function
  get_first_dep <- function(var_names, dep_rev, plot_var) {
    if (length(var_names) == 0) {
      return(list())
    }

    dep_var <- dep_rev[var_names]

    # Only keep those in plot_var
    dep_var <- lapply(dep_var, function(y) {
      intersect(y, plot_var)
    })

    dep_var <- dep_var[lengths(dep_var) != 0]

    if (length(dep_var) > 0) {
      lapply(dep_var, `[[`, 1)
    } else {
      list()
    }
  }

  if (length(const_names) > 0) {
    dep_var <- get_first_dep(const_names, dep_rev, plot_var)
    rank_groups <- append(rank_groups, dep_var)
  }

  if (length(aux_names) > 0) {
    dep_var <- get_first_dep(aux_names, dep_rev, plot_var)
    rank_groups <- append(rank_groups, dep_var)
  }


  # Create rank statements
  rank_statements <- ""
  if (length(rank_groups) > 0) {
    rank_statements <- vapply(names(rank_groups), function(rank_node) {
      vars_in_rank <- rank_groups[[rank_node]]
      sprintf(
        "      {rank=same; %s }",
        paste0("'", c(rank_node, vars_in_rank), "'", collapse = "; ")
      )
    }, character(1), USE.NAMES = FALSE)
    rank_statements <- paste(rank_statements, collapse = "\n")
  }

  # User-specified alignment: each group becomes a {rank=same; ...}, appended to
  # the automatic (aux/constant) rank groupings. Graphviz merges shared nodes
  # across rank statements, so this composes with the groups built above.
  align_groups <- prepare_layout_groups(align, plot_var, model_var_names, "align")
  if (length(align_groups) > 0) {
    align_statements <- vapply(align_groups, function(g) {
      sprintf("      {rank=same; %s }", paste0("'", g, "'", collapse = "; "))
    }, character(1), USE.NAMES = FALSE)
    rank_statements <- paste(c(rank_statements, align_statements), collapse = "\n")
  }

  # User-specified ordering along the flow direction: invisible edges between
  # consecutive names. A soft hint (constraint = true, no flow_length bump) so Graphviz
  # balances it against the real flows rather than forcing the order.
  order_groups <- prepare_layout_groups(order, plot_var, model_var_names, "order")
  order_statements <- ""
  if (length(order_groups) > 0) {
    # One invisible edge per consecutive pair (a -> b, b -> c), each on its own
    # line. Equivalent to a -> b -> c in Graphviz, but keeps the DOT readable.
    edges <- unlist(lapply(order_groups, function(g) {
      n <- length(g)
      sprintf("'%s' -> '%s'", g[-n], g[-1])
    }), use.names = FALSE)
    order_statements <- paste0(
      "# Ordering hints (invisible edges)\n      edge [style=invis, constraint=true]\n      ",
      paste(edges, collapse = "\n      ")
    )
  }

  cloud_nodes <- flow_edges_from_source <- flow_edges_to_destination <- flow_nodes <- dependency_edges <- ""
  style_cloud <- style_flow_node <- style_flow_edges_from_source <- style_flow_edges_to_destination <- ""

  if (length(flow_names) > 0) {
    # If the flow is to a stock that doesn't exist, remove
    flow_df[["from"]] <- ifelse(flow_df[["from"]] %in% stock_names,
      flow_df[["from"]], ""
    )
    flow_df[["to"]] <- ifelse(flow_df[["to"]] %in% stock_names,
      flow_df[["to"]], ""
    )
    flow_df <- as.matrix(flow_df)

    # Fill in NA in flow_df with numbered clouds
    idxs <- which(flow_df == "")
    cloud_names <- character(0)
    if (length(idxs) > 0) {
      cloud_names <- paste0("Cloud", seq_along(idxs))

      # Identify the flow and role for each cloud before overwriting the matrix.
      # Columns are name (1), to (2), from (3): an empty 'to' is a sink (the flow
      # leaves the model), an empty 'from' is a source (the flow enters it).
      n_flow <- nrow(flow_df)
      cloud_rows <- ((idxs - 1L) %% n_flow) + 1L
      cloud_cols <- ((idxs - 1L) %/% n_flow) + 1L
      cloud_flow_names <- flow_df[cbind(cloud_rows, rep(1L, length(idxs)))]
      cloud_flow_labels <- unname(dict_label[cloud_flow_names])
      cloud_flow_labels[is.na(cloud_flow_labels)] <- cloud_flow_names[is.na(cloud_flow_labels)]

      flow_df[idxs] <- cloud_names

      cloud_tooltip_text <- ifelse(
        cloud_cols == 2L,
        paste0("Outside model boundary\\nSink of: ", cloud_flow_labels),
        paste0("Outside model boundary\\nSource of: ", cloud_flow_labels)
      )

      # Style for cloud nodes
      style_cloud <- sprintf("node [shape=doublecircle, fixedsize=true, width = .25, height = .25, orientation=15]")

      # External environment is represented as a cloud
      cloud_tooltip <- if (show_tooltip) {
        sprintf(", tooltip = '%s'", cloud_tooltip_text)
      } else {
        rep("", length(cloud_names))
      }
      cloud_nodes <- sprintf(
        "%s [label=''%s]",
        paste0("'", cloud_names, "'"),
        cloud_tooltip
      )
    }

    # Flow node style
    style_flow_node <- sprintf(
      "node [style = '',shape=plaintext, fontsize=%s, width=0.6, height=0.3]",
      font_size - 2
    )

    # Create intermediate flow nodes (small nodes that flows pass through)
    if (show_eqn) {
      flow_label <- sprintf(
        "label=<%s>",
        make_eqn_label("flow", dict[flow_names], dict_eqn[flow_names], eqn_font_size, font_color, wrap_width)
      )
    } else {
      flow_label <- sprintf("label='%s'", dict[flow_names])
    }

    flow_nodes <- sprintf(
      "%s [id=%s,%s%s]",
      paste0("'", flow_names, "'"),
      paste0("'", flow_names, "'"),
      flow_label,
      node_tooltip_attr(flow_names)
    )

    # Create edges: from -> flow_node -> to
    flow_edges_from_source <- c()
    flow_edges_to_destination <- c()

    style_flow_edges_from_source <- sprintf(
      "edge [style = '', arrowhead='none', penwidth=1.1, minlen=%s, tailport='%s', headport='%s']",
      flow_length,
      flow_ports[["tail"]],
      flow_ports[["head"]]
    )
    style_flow_edges_to_destination <- sprintf(
      "edge [style = '', arrowhead='normal', arrowsize=1.5, penwidth=1.1, minlen=%s, tailport='%s', headport='%s']",
      flow_length,
      flow_ports[["tail"]],
      flow_ports[["head"]]
    )

    for (i in seq_len(nrow(flow_df))) {
      flow_name <- flow_df[i, "name"]
      flow_node <- flow_name
      from_node <- flow_df[i, "from"]
      to_node <- flow_df[i, "to"]

      # Flows can be coloured per variable, so each edge carries its own colour
      # (a black-bordered band in the flow's colour) rather than the group style.
      edge_color <- sprintf("black:%s:black", var_colors[[flow_name]])

      # Edge from source to flow node
      flow_edges_from_source <- c(flow_edges_from_source, sprintf(
        "%s -> %s [color='%s']",
        paste0("'", from_node, "'"),
        paste0("'", flow_node, "'"),
        edge_color
      ))

      # Edge from flow node to destination
      flow_edges_to_destination <- c(flow_edges_to_destination, sprintf(
        "%s -> %s [color='%s']",
        paste0("'", flow_node, "'"),
        paste0("'", to_node, "'"),
        edge_color
      ))
    }
  }

  # Add dependency arrows if requested
  style_dependency <- dependency_edges <- ""
  if (show_dependencies) {
    style_dependency <- sprintf(
      # "edge [style = '', color='%s', arrowsize=0.8, penwidth=1, splines=true, constraint=false, tailport = '_', headport='_']",
      "edge [style = '', color='%s', arrowhead='%s', arrowsize=0.8, penwidth=1, constraint=false, tailport = '_', headport='_']",
      color_dependency,
      arrowhead_dependency
    )

    # Only keep dependencies in plot_var
    dep <- lapply(dep, function(y) {
      intersect(y, plot_var)
    })

    # Only keep entries in plot_var
    dep <- dep[names(dep) %in% plot_var]

    if (length(dep) > 0) {
      dependency_edges <- unlist(lapply(names(dep), function(y) {
        if (length(dep[[y]]) > 0) {
          vapply(dep[[y]], function(z) {
            sprintf(
              "%s -> %s",
              paste0("'", z, "'"),
              paste0("'", y, "'")
            )
          }, character(1), USE.NAMES = FALSE)
        }
      }))

      # Avoid overlap between flows and dependency edges
      if (!is.null(dependency_edges) && length(flow_names) > 0) {
        # Find dependency edges that link the same variables as flows do
        dep_split <- strsplit(dependency_edges, " -> ", fixed = TRUE)

        # Omit NA
        flow_df_ <- flow_df[!is.na(flow_df[, "from"]) & !is.na(flow_df[, "to"]), , drop = FALSE]

        flow_start <- c(flow_df_[, "from"], flow_df_[, "name"])
        flow_end <- c(flow_df_[, "name"], flow_df_[, "to"])

        idx <- lapply(dep_split, function(x) {
          x <- gsub("'", "", x)
          any(x[1] %in% flow_start & x[2] %in% flow_end)
        }) |> unlist()

        if (any(idx)) {
          suff1 <- " [headport = 'n', tailport = 'n']"
          suff2 <- " [headport = 's', tailport = 's']"

          # Alternative
          suff <- rep(c(suff1, suff2), length.out = sum(idx))

          # suff <- " [headport = 's']"

          # Add head/tail ports to dependency edges that overlap with flows
          dependency_edges[idx] <- paste0(dependency_edges[idx], suff)
        }
      }
    }
  }

  # Compile string for diagram
  viz_str <- sprintf(
    "
    digraph sfm {

      graph [layout = dot, rankdir = %s, center=true, outputorder='edgesfirst', pad=%s, nodesep=%s, splines = true, concentrate = false]

      # Shared across all nodes (persists until overridden)
      %s

      # Define stock nodes
      %s
      %s

      # Define flow nodes (intermediate nodes for flows)
      %s
      %s

      # Define external cloud nodes
      %s
      %s

      # Define auxiliary nodes
      %s
      %s

      # Define constant nodes
      %s
      %s

      # Define flow edges (stock -> flow_node)
      %s
      %s

      # Define flow edges (flow_node -> stock)
      %s
      %s

      # Define dependency edges
      %s
      %s

      %s


      # Rank groupings
      %s

    }
          ",
    direction,
    pad = as.character(margin),
    nodesep = as.character(spacing),
    style_node,
    style_stock,
    stock_nodes |> paste0(collapse = "\n\t"),
    # stock_nodes |> rev() |> paste0(collapse = "\n\t"),
    style_flow_node,
    flow_nodes |> paste0(collapse = "\n\t"),
    style_cloud,
    cloud_nodes |> paste0(collapse = "\n\t"),
    style_aux,
    aux_nodes |> paste0(collapse = "\n\t"),
    style_const,
    const_nodes |> paste0(collapse = "\n\t"),
    style_flow_edges_from_source,
    flow_edges_from_source |> paste0(collapse = "\n\t"),
    style_flow_edges_to_destination,
    flow_edges_to_destination |> paste0(collapse = "\n\t"),
    style_dependency,
    dependency_edges |> paste0(collapse = "\n\t"),
    order_statements,
    rank_statements
  )

  pl <- DiagrammeR::grViz(viz_str)

  apply_webfont(pl, font_family)
}


#' Prepare for plotting simulation
#'
#' @param type_sim Either "sim", "ensemble", or "verify"
#' @param df data.frame to plot
#' @param constants Constants to plot
#' @inheritParams plot.simulate_stockflow
#' @inheritParams update.stockflow
#'
#' @returns List
#' @noRd
#'
prep_plot <- function(
  object, type_sim, df, constants,
  show_constants, vars, palette, colors, wrap_width,
  format_label = TRUE, order = NULL
) {
  # Get names of stocks and non-stock variables
  names_df <- get_names(object)
  style_names <- names_df[["name"]]

  # Validate variable parameters
  validate_plot_params(vars = vars)

  if (!is.null(vars)) {
    vars <- clean_vars(vars)
  }

  # If vars is specified and contains constants, enable show_constants
  if (!is.null(vars)) {
    constant_names <- names_df[
      names_df[["type"]] %in% c("constant", "lookup"),
      "name"
    ]
    vars_constants <- intersect(constant_names, vars)
    constants_not_in_vars <- setdiff(constant_names, vars_constants)

    show_constants <- length(vars_constants) > 0

    if (show_constants) {
      # Remove non-selected constants from names_df
      names_df <- names_df[!(names_df[["name"]] %in% constants_not_in_vars), ,
        drop = FALSE
      ]

      # Filter constants to only those in vars
      if (type_sim == "sim") {
        constants <- constants[vars_constants]
      } else if (type_sim == "ensemble") {
        constants <- constants[!(constants[["variable"]] %in% constants_not_in_vars), ,
          drop = FALSE
        ]
      } else if (type_sim == "verify") {
        constants <- constants[!(constants[["variable"]] %in% constants_not_in_vars), ,
          drop = FALSE
        ]
      }
    }
  }

  # Add constants to dataframe if requested
  if (show_constants && length(constants) > 0) {
    result <- prep_constants(df, constants, names_df, type_sim = type_sim)
    df <- result$df
    names_df <- result$names_df
  }

  # Filter to specified variables if provided
  if (!is.null(vars)) {
    result <- filter_variables(vars, names_df, df)
    names_df <- result$names_df
    df <- result$df
    highlight_these_names <- names_df[["name"]]
  } else {
    # Default: highlight stocks
    highlight_these_names <- determine_highlight_vars(names_df, highlight_strategy = "auto")
  }

  # Ensure only variables which are in the dataframe are included
  names_df <- names_df[names_df[["name"]] %in% unique(df[["variable"]]), ,
    drop = FALSE
  ]
  highlight_these_names <- highlight_these_names[highlight_these_names %in% names_df[["name"]]]

  names_df <- apply_trace_order(names_df, order = order, model_var = style_names)
  highlight_these_names <- highlight_these_names[highlight_these_names %in% names_df[["name"]]]
  highlight_these_names <- names_df[["name"]][names_df[["name"]] %in% highlight_these_names]

  # Prepare and standardize labels (handle duplicates, wrapping, special characters)
  names_df <- prepare_labels(names_df, wrap_width = wrap_width, format_label = format_label)

  # Create dictionaries: name -> label (for visible and hidden variables)
  highlight_names <- names_df[match(highlight_these_names, names_df[["name"]]), , drop = FALSE]
  highlight_names <- stats::setNames(highlight_names[["name"]], highlight_names[["label"]])
  nonhighlight_names <- names_df[!names_df[["name"]] %in% highlight_these_names, , drop = FALSE]
  nonhighlight_names <- stats::setNames(nonhighlight_names[["name"]], nonhighlight_names[["label"]])

  # Split dataframe and recode labels
  df_highlight <- df[df[["variable"]] %in% unname(highlight_names), , drop = FALSE]
  df_nonhighlight <- df[df[["variable"]] %in% unname(nonhighlight_names), , drop = FALSE]

  df_highlight[["variable"]] <- factor(
    df_highlight[["variable"]],
    levels = unname(highlight_names),
    labels = names(highlight_names)
  )
  df_nonhighlight[["variable"]] <- factor(
    df_nonhighlight[["variable"]],
    levels = unname(nonhighlight_names),
    labels = names(nonhighlight_names)
  )

  # Generate colors keyed by the display labels used in Plotly traces, while
  # matching any user-supplied names against model variable names. Use the
  # prepared names table order so an explicit `order` controls trace, legend,
  # and positional aesthetic order instead of the highlight/nonhighlight split.
  plot_var_names <- names_df[["name"]]
  plot_labels <- names_df[["label"]]
  colors <- resolve_colors(colors, palette, plot_var_names,
    display_names = plot_labels, valid_names = style_names
  )

  list(
    highlight_names = highlight_names,
    nonhighlight_names = nonhighlight_names,
    df_highlight = df_highlight,
    df_nonhighlight = df_nonhighlight,
    colors = colors,
    labels = plot_labels,
    var_names = plot_var_names,
    var_types = stats::setNames(names_df[["type"]][match(plot_var_names, names_df[["name"]])], plot_labels),
    style_names = style_names
  )
}


#' Combine prepared simulation data frames in plot order
#'
#' @param out Result from prep_plot().
#' @returns Data frame with display-label factors ordered like out$labels.
#' @noRd
combine_prepped_sim_data <- function(out) {
  df <- rbind(out[["df_highlight"]], out[["df_nonhighlight"]])
  if (nrow(df) == 0L) {
    return(df)
  }
  df[["variable"]] <- factor(as.character(df[["variable"]]), levels = out[["labels"]])
  df
}


#' Build one simulation time-series panel
#'
#' @param df Prepared long data for one panel.
#' @param colors,line_width,line_type Aesthetics keyed by display label.
#' @param fill,fillcolor Optional fill attributes keyed by display label.
#' @inheritParams add_trace_pair
#' @returns Plotly object.
#' @noRd
build_sim_trace_panel <- function(df, colors, line_width, line_type,
                                  fill = NULL, fillcolor = NULL,
                                  show_legend = TRUE, type = "scatter",
                                  frame = NULL) {
  pl <- plotly::plot_ly()
  add_trace_pair(pl,
    df_highlight = df,
    df_nonhighlight = NULL,
    colors = colors,
    line_width = line_width,
    line_type = line_type,
    fill = fill,
    fillcolor = fillcolor,
    x_col = "time",
    y_col = "value",
    show_legend = show_legend,
    mode = "lines",
    type = type,
    frame = frame
  )
}


#' Build flow fill vectors for role-panel simulation plots
#'
#' @param labels Display labels in plot order.
#' @param var_types Variable types named by display label.
#' @param colors Colours named by display label.
#' @param fill_flows Whether to fill flow traces.
#' @returns List with fill and fillcolor vectors.
#' @noRd
flow_fill_aes <- function(labels, var_types, colors, fill_flows) {
  fill <- stats::setNames(rep(NA_character_, length(labels)), labels)
  fillcolor <- stats::setNames(rep(NA_character_, length(labels)), labels)
  flow_labels <- labels[unname(var_types[labels]) == "flow"]
  if (isTRUE(fill_flows) && length(flow_labels) > 0L) {
    fill[flow_labels] <- "tozeroy"
    fillcolor[flow_labels] <- vapply(colors[flow_labels], plotly_translucent_color, character(1))
  }
  list(fill = fill, fillcolor = fillcolor)
}


#' Resolve variable display layout for simulation plots
#'
#' @param vars_display Character layout choice.
#' @returns One of "vstack", "hstack", or "joint".
#' @noRd
clean_vars_display <- function(vars_display) {
  choices <- c("vstack", "hstack", "joint")
  if (length(vars_display) > 1L) vars_display <- vars_display[[1L]]
  if (!is.character(vars_display) || length(vars_display) != 1L ||
    !vars_display %in% choices) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg vars_display} value.",
      "i" = "Use one of {.val {choices}}."
    ))
  }
  vars_display
}


#' Plot timeseries of simulation
#'
#' Visualize simulation results of a stock-and-flow model. Plot the evolution of stocks over time, with the option of also showing other model variables.
#'
#' @param x Output of [`simulate()`][simulate.stockflow()].
#' @param show_constants If `TRUE`, include constants in plot. Defaults to `FALSE`.
#' @param vars Variables to plot. Defaults to `NULL` to plot all variables.
#' @param order Optional character vector of model variable names giving the
#'   trace and legend order. Listed variables appear first; any plotted
#'   variables not listed keep their default relative order. Defaults to `NULL`.
#' @param palette Colour palette. Must be one of hcl.pals().
#' @param colors Colours for the plotted variables. A named vector (names are
#'   variable names) sets the colours of those variables and the palette fills
#'   the rest, so you can recolour only a few variables. An unnamed vector
#'   assigns colours in plot order. `NULL` uses `palette`. Defaults to `NULL`.
#' @param line_width Line width of the plotted trajectories. Either a single
#'   value applied to all variables, a named per-variable vector (names are
#'   variable names), or an unnamed vector with one value per variable in plot
#'   order. Defaults to `2`.
#' @param line_type Plotly line dash style for trajectories. Either a single
#'   dash string applied to all variables, a named per-variable vector (names are
#'   variable names), an unnamed vector in plot order, or a named list keyed by
#'   variable type (`stock`, `flow`, `aux`, `constant`, `lookup`). Defaults to
#'   solid stocks, flows, and auxiliaries, with dashed constants and lookups.
#' @param vars_display How to arrange saved variables. Use `"vstack"` to stack
#'   stocks above flows, auxiliaries, constants, and lookups; `"hstack"` to put
#'   stocks beside non-stock variables; or `"joint"` to draw all variables in
#'   one panel. If only one variable group is available, a single panel is
#'   drawn. Defaults to `"vstack"`.
#' @param fill_flows If `TRUE`, flow traces are filled
#'   down to zero. Auxiliaries, constants, and lookups are never filled.
#'   Defaults to `TRUE`.
#' @param font_family Font family. Kebab-case names (e.g. `"eb-garamond"`)
#'   are Fontsource ids (browse them at <https://fontsource.org/>), loaded as
#'   webfonts: no installation is needed, but internet access is required to
#'   display them. Other fonts must be installed on the system viewing the
#'   plot. Defaults to the `sdbuildR.font_family` option, or
#'   the `"stix-two-text"` webfont when the option is unset; use
#'   `options(sdbuildR.font_family = )` to change the default for all plots,
#'   e.g. in your .Rprofile.
#' @param font_size Font size. Defaults to 16.
#' @param wrap_width Width of text wrapping for labels. Must be an integer. Defaults to 25.
#' @param show_legend Whether to show legend. Must be `TRUE` or `FALSE`. Defaults to `TRUE`.
#' @param format_label If `TRUE`, apply default formatting (replacing periods and
#'   underscores with spaces) to variable labels that are the same as the
#'   variable name. Applies to the legend and any condition controls. Defaults to
#'   `TRUE`.
#' @param animation Animation mode. Use `"none"` for a static plot or `"time"`
#'   to cumulatively reveal trajectories over time. Defaults to `"none"`.
#' @param control_options Named list fine-tuning the speed and smoothness of
#'   the `animation = "time"` animation. Supports `frame_ms`: the duration of
#'   each frame in milliseconds (default `100`); `duration`: the total
#'   animation length in seconds, as an alternative to `frame_ms` (supplying
#'   both is an error); `transition_ms`: the transition time between frames in
#'   milliseconds (default `0`); and `max_frames`: the maximum number of
#'   animation frames (default `50`; lower it for a chunkier reveal that also
#'   shortens the animation at a fixed `frame_ms`). Defaults to `list()`, i.e.
#'   all defaults.
#' @param webgl If `TRUE`, render trajectories with WebGL (plotly `scattergl`) for
#'   performance with many lines; if `FALSE`, use SVG (`scatter`). Defaults to
#'   `getOption("sdbuildR.webgl", default = TRUE)`. Set
#'   `options(sdbuildR.webgl = FALSE)` (e.g. in vignettes or dashboards, or when
#'   a plot renders blank) to disable WebGL globally.
#' @param ... Optional parameters
#'
#' @section Styling variables:
#' Names in `colors`, `line_width`, and `line_type` refer to the model variable
#' names, not the labels shown in the legend. This is usually the safest way to
#' style a plot, because labels may be wrapped, prettified, or customized for
#' display.
#'
#' Use one value to style every trajectory:
#' `plot(sim, line_width = 3)`.
#'
#' Use a named vector to style selected variables and leave the rest at their
#' defaults or palette colours:
#' `plot(sim, colors = c(susceptible = "#377EB8"), line_width = c(infected = 4))`.
#'
#' Unnamed vectors are still accepted and are applied in plot order, but named
#' vectors are easier to read and less sensitive to filtering with `vars`.
#'
#' @returns Plotly object
#' @export
#' @concept simulate
#' @seealso [`simulate()`][simulate.stockflow()], [as.data.frame.simulate_stockflow()], [plot.simulate_stockflow()]
#' @method plot simulate_stockflow
#'
#' @examples
#' sfm <- stockflow("sir")
#' sim <- simulate(sfm)
#' plot(sim)
#'
#' # The default plot title and axis labels can be changed like so:
#' plot(sim, main = "Simulated trajectory", xlab = "Time", ylab = "Value")
#'
#' # Add constants to the plot
#' plot(sim, show_constants = TRUE)
#'
#' # Save and plot non-stock variables in a compact role-panel view
#' sim_all <- simulate(sfm, only_stocks = FALSE)
#' plot(sim_all, vars_display = "vstack", fill_flows = TRUE)
#'
#' # Cumulatively reveal the trajectories over time
#' plot(sim, animation = "time")
#'
#' # Slow the animation down to ~10 seconds in total, or speed up the
#' # individual frames
#' plot(sim, animation = "time", control_options = list(duration = 10))
#' plot(sim, animation = "time", control_options = list(frame_ms = 40))
#'
plot.simulate_stockflow <- function(x,
                                    show_constants = FALSE,
                                    vars = NULL,
                                    order = NULL,
                                    palette = "Dark 2",
                                    colors = NULL,
                                    line_width = 2,
                                    line_type = list(stock = "solid", flow = "solid", aux = "solid", constant = "dash", lookup = "dash"),
                                    vars_display = c("vstack", "hstack", "joint"),
                                    fill_flows = TRUE,
                                    font_family = getOption("sdbuildR.font_family", default = "stix-two-text"),
                                    font_size = 16,
                                    wrap_width = 25,
                                    show_legend = TRUE,
                                    format_label = TRUE,
                                    animation = c("none", "time"),
                                    control_options = list(),
                                    webgl = getOption("sdbuildR.webgl", default = TRUE),
                                    ...) {
  if (missing(x)) {
    cli::cli_abort(c(
      "x" = "No simulation data available.",
      ">" = "Run a simulation first with {.fn simulate}."
    ))
  }

  check_simulate_stockflow(x)

  if (!x[["success"]]) {
    cli::cli_abort(c(
      "x" = "Simulation failed.",
      ">" = "Check your model specification and try again."
    ))
  }

  if (nrow(x[["df"]]) == 0) {
    cli::cli_abort(c("x" = "Simulation data frame has no rows"))
  }

  animation <- .clean_animation(animation)
  # No condition slider/dropdown here, so only the animation options apply.
  control_options <- resolve_control_options(control_options,
    allowed = c("frame_ms", "transition_ms", "max_frames", "duration")
  )

  # Validate common plot parameters
  validate_plot_params(
    show_legend = show_legend,
    vars = vars,
    palette = palette,
    colors = colors,
    font_family = font_family,
    font_size = font_size,
    wrap_width = wrap_width,
    format_label = format_label,
    webgl = webgl
  )
  vars_display <- clean_vars_display(vars_display)
  if (!is.logical(fill_flows) || length(fill_flows) != 1L || is.na(fill_flows)) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg fill_flows} argument.",
      "i" = "The {.arg fill_flows} argument must be {.cls logical}."
    ))
  }

  dots <- list(...)

  # Extract optional parameters with defaults
  time_unit <- x[["object"]][["sim_settings"]][["time_units"]]
  params <- extract_plot_params(dots, defaults = list(
    main = x[["object"]][["meta"]][["name"]],
    sub = "",
    xlab = paste0("Time", ifelse(nzchar(time_unit), paste0(" (", time_unit, ")"), "")),
    ylab = ""
  ))
  main <- params$main
  sub <- params$sub
  xlab <- params$xlab
  ylab <- params$ylab

  # Append subtitle to main title
  main <- paste0(main, "<span style='font-size:", font_size, "px;'>\n", sub, "</span>")
  
  out <- prep_plot(
    x[["object"]], "sim", x[["df"]], x[["constants"]], show_constants,
    vars, palette, colors, wrap_width, format_label,
    order = order
  )
  highlight_names <- out[["highlight_names"]]
  nonhighlight_names <- out[["nonhighlight_names"]]
  df_highlight <- out[["df_highlight"]]
  df_nonhighlight <- out[["df_nonhighlight"]]
  colors <- out[["colors"]]
  var_types <- out[["var_types"]]

  # Resolve per-variable line widths keyed by variable name (a single value, a named
  # per-variable vector, or a positional vector). Trajectories are the only
  # layer a simple simulation draws.
  line_width <- expand_aes(line_width, out[["var_names"]],
    default = 2, arg = "line_width", validate = "positive",
    display_names = out[["labels"]], valid_names = out[["style_names"]]
  )
  line_type <- resolve_line_type(line_type, out[["var_names"]], unname(var_types[out[["labels"]]]),
    display_names = out[["labels"]], valid_names = out[["style_names"]]
  )

  labels <- out[["labels"]]
  stock_labels <- labels[unname(var_types[labels]) == "stock"]
  nonstock_labels <- labels[unname(var_types[labels]) %in% c("flow", "aux", "constant", "lookup")]
  role_panels <- vars_display != "joint" && length(nonstock_labels) > 0L
  fill_aes <- flow_fill_aes(labels, var_types, colors, fill_flows = fill_flows)
  fills_flows <- any(!is.na(fill_aes[["fill"]]) & nzchar(fill_aes[["fill"]]))

  # For time animation, cumulatively reveal each trajectory frame by frame.
  if (animation == "time" && !role_panels) {
    df_highlight <- accumulate_by_time(df_highlight,
      max_frames = control_options[["max_frames"]]
    )
    df_nonhighlight <- accumulate_by_time(df_nonhighlight,
      max_frames = control_options[["max_frames"]]
    )
    frame <- ~.frame
  } else {
    frame <- NULL
  }

  # WebGL is fast for ordinary lines, but Plotly fill areas and animation frames
  # need SVG scatter for reliable rendering.
  if (fills_flows && isTRUE(webgl) && animation != "time") {
    cli::cli_warn(c(
      "!" = "Filled flow traces require Plotly SVG scatter traces.",
      ">" = "Using {.code webgl = FALSE} for this plot."
    ))
  }
  trace_type <- if (webgl && animation != "time" && !fills_flows) "scattergl" else "scatter"

  if (role_panels) {
    df_all <- combine_prepped_sim_data(out)
    panels <- list()
    frame_panels <- list()
    if (length(stock_labels) > 0L) {
      df_stock <- df_all[as.character(df_all[["variable"]]) %in% stock_labels, , drop = FALSE]
      panels[["stock"]] <- build_sim_trace_panel(df_stock,
        colors = colors, line_width = line_width, line_type = line_type,
        show_legend = show_legend, type = trace_type,
        frame = NULL
      )
      if (animation == "time") {
        df_stock_frame <- accumulate_by_time(df_stock, max_frames = control_options[["max_frames"]])
        frame_panels[["stock"]] <- build_sim_trace_panel(df_stock_frame,
          colors = colors, line_width = line_width, line_type = line_type,
          show_legend = show_legend, type = trace_type,
          frame = ~.frame
        )
      }
    }
    if (length(nonstock_labels) > 0L) {
      df_nonstock <- df_all[as.character(df_all[["variable"]]) %in% nonstock_labels, , drop = FALSE]
      panels[["nonstock"]] <- build_sim_trace_panel(df_nonstock,
        colors = colors, line_width = line_width, line_type = line_type,
        fill = fill_aes[["fill"]], fillcolor = fill_aes[["fillcolor"]],
        show_legend = show_legend, type = trace_type,
        frame = NULL
      )
      if (animation == "time") {
        df_nonstock_frame <- accumulate_by_time(df_nonstock, max_frames = control_options[["max_frames"]])
        frame_panels[["nonstock"]] <- build_sim_trace_panel(df_nonstock_frame,
          colors = colors, line_width = line_width, line_type = line_type,
          fill = fill_aes[["fill"]], fillcolor = fill_aes[["fillcolor"]],
          show_legend = show_legend, type = trace_type,
          frame = ~.frame
        )
      }
    }

    if (length(panels) > 1L) {
      subplot_args <- if (vars_display == "hstack") {
        list(nrows = 1L, heights = NULL, widths = c(0.5, 0.5), shareX = FALSE)
      } else {
        list(nrows = length(panels), heights = c(0.5, 0.5), widths = NULL, shareX = TRUE)
      }
      if (animation == "time") {
        pl <- subplot_linked_animation(unname(panels),
          frame_plots = unname(frame_panels),
          nrows = subplot_args[["nrows"]],
          heights = subplot_args[["heights"]],
          widths = subplot_args[["widths"]],
          margin = 0.06, shareX = subplot_args[["shareX"]], titleY = TRUE
        )
      } else {
        pl <- do.call(plotly::subplot, c(unname(panels), list(
          nrows = subplot_args[["nrows"]],
          heights = subplot_args[["heights"]],
          widths = subplot_args[["widths"]],
          margin = 0.06, shareX = subplot_args[["shareX"]], titleY = TRUE
        )))
      }
    } else {
      pl <- if (animation == "time") frame_panels[[1L]] else panels[[1L]]
    }

    n_frames <- if (animation == "time") {
      length(unique(unlist(lapply(frame_panels, function(panel) {
        frames <- plotly::plotly_build(panel)[["x"]][["frames"]]
        vapply(frames, function(frame) frame[["name"]], character(1))
      }))))
    } else {
      0L
    }
  } else {
    # Initialize plotly object
    pl <- plotly::plot_ly()

    # Add traces for highlight and nonhighlight variables. WebGL (scattergl) is
    # used for performance, but plotly animation frames are unreliable under gl, so
    # fall back to SVG scatter when animating.
    pl <- add_trace_pair(pl,
      df_highlight = df_highlight,
      df_nonhighlight = df_nonhighlight,
      colors = colors,
      line_width = line_width,
      line_type = line_type,
      fill = fill_aes[["fill"]],
      fillcolor = fill_aes[["fillcolor"]],
      x_col = "time",
      y_col = "value",
      show_legend = show_legend,
      mode = "lines",
      type = trace_type,
      frame = frame,
      trace_order = if (!is.null(order)) labels else NULL
    )
    n_frames <- if (animation == "time") {
      length(unique(c(df_highlight[[".frame"]], df_nonhighlight[[".frame"]])))
    } else {
      0L
    }
  }

  # Customize layout using theme
  theme <- plotly_theme(
    font_family = font_family,
    font_size = font_size
  )

  pl <- plotly::layout(pl,
    legend = theme$legend,
    title = main,
    xaxis = list(title = if (role_panels && vars_display == "hstack") "" else xlab, font = list(size = font_size)),
    xaxis2 = if (role_panels && vars_display == "hstack" && length(stock_labels) > 0L && length(nonstock_labels) > 0L) list(title = "", font = list(size = font_size)) else NULL,
    yaxis = list(title = ylab, font = list(size = font_size)),
    yaxis2 = if (role_panels && length(stock_labels) > 0L && length(nonstock_labels) > 0L) list(title = ylab, font = list(size = font_size)) else NULL,
    font = theme$font,
    margin = theme$margin
  )

  if (role_panels && vars_display == "hstack" && nzchar(xlab)) {
    pl <- plotly::layout(pl,
      margin = list(b = max(theme$margin$b, 75)),
      annotations = list(list(
        text = xlab,
        x = 0.5,
        y = -0.16,
        xref = "paper",
        yref = "paper",
        xanchor = "center",
        yanchor = "top",
        showarrow = FALSE,
        font = list(size = font_size, family = font_family)
      ))
    )
  }

  # If there is only one trace, legend doesn't show
  if (show_legend && (length(highlight_names) + length(nonhighlight_names)) == 1) {
    pl <- plotly::layout(pl, showlegend = TRUE)
  }

  # Set x-axis limits if specified
  if ("xlim" %in% names(dots)) {
    pl <- plotly::layout(pl,
      xaxis = list(range = dots[["xlim"]]),
      xaxis2 = if (role_panels && vars_display == "hstack" && length(stock_labels) > 0L && length(nonstock_labels) > 0L) list(range = dots[["xlim"]]) else NULL
    )
  }

  if ("ylim" %in% names(dots)) {
    pl <- plotly::layout(pl,
      yaxis = list(range = dots[["ylim"]]),
      yaxis2 = if (role_panels && length(stock_labels) > 0L && length(nonstock_labels) > 0L) list(range = dots[["ylim"]]) else NULL
    )
  }

  # Add play button and time slider for the cumulative reveal animation.
  if (animation == "time") {
    pl <- add_time_animation_controls(pl,
      time_unit = if (is.null(time_unit)) "" else time_unit,
      font_family = font_family,
      font_size = font_size,
      frame_ms = resolve_frame_ms(control_options, n_frames),
      transition_ms = control_options[["transition_ms"]]
    )
  }

  apply_webfont(set_plotly_export_format(pl), font_family)
}


#' Plot timeseries of ensemble simulation
#'
#' Visualize ensemble simulation results of a stock-and-flow model. Either summary statistics or individual trajectories can be plotted. When multiple conditions j are specified, a grid of subplots is plotted. See [ensemble()] for examples.
#'
#' @param x Output of [ensemble()].
#' @param which Type of plot. Either `"summary"` for a summary plot with mean or median lines and confidence intervals, or `"sims"` for individual simulation trajectories with mean or median lines. Defaults to `"summary"`.
#' @param sim Indices of the individual trajectories to plot if which = `"sims"`. Defaults to the first `min(n, 100)` trajectories. Including a high number of trajectories will slow down plotting considerably.
#' @param condition Indices of the condition(s) to plot. Defaults to 1:9.
#' @param nrows Number of rows in the plot grid. Defaults to `ceiling(sqrt(max(condition)))`.
#' @param margin Margin between subplots. Either a single numeric or a vector of length four(left, right, top, bottom). See `?plotly::subplot()` for more details. Defaults to 0.05.
#' @param shareX If `TRUE`, share the x-axis across subplots. Defaults to `TRUE`.
#' @param shareY If `TRUE`, share the y-axis across subplots. Defaults to `TRUE`.
#' @param palette Colour palette. Must be one of hcl.pals().
#' @param colors Colours for the plotted variables. A named vector (names are
#'   variable names) sets the colours of those variables and the palette fills
#'   the rest, so you can recolour only a few variables. An unnamed vector
#'   assigns colours in plot order. `NULL` uses `palette`. Defaults to `NULL`.
#' @param line_width Line width(s). The plot draws three layers: the central
#'   tendency line (`central`), the uncertainty band's border (`spread`), and the
#'   individual trajectories (`sims`). Supply a single value (used for every
#'   layer and variable), a named per-variable vector (names are variable names;
#'   unnamed values fill in plot order), or a list keyed by layer
#'   (`list(central = , spread = , sims = )`) whose elements are themselves a
#'   single value or per-variable vector. Unspecified layers/variables fall back
#'   to the defaults. Defaults to `list(central = 3, spread = 0, sims = 1)`
#'   (a `spread` width of `0` draws no band border).
#' @param alpha Opacity, with the same grammar as `line_width`: a single value, a
#'   named per-variable vector, or a list keyed by layer (`central`/`spread`/
#'   `sims`). Defaults to `list(central = 1, spread = 0.3, sims = 0.3)`.
#' @param font_family Font family. Kebab-case names (e.g. `"eb-garamond"`)
#'   are Fontsource ids (browse them at <https://fontsource.org/>), loaded as
#'   webfonts: no installation is needed, but internet access is required to
#'   display them. Other fonts must be installed on the system viewing the
#'   plot. Defaults to the `sdbuildR.font_family` option, or
#'   the `"stix-two-text"` webfont when the option is unset; use
#'   `options(sdbuildR.font_family = )` to change the default for all plots,
#'   e.g. in your .Rprofile.
#' @param font_size Font size. Defaults to 16.
#' @param wrap_width Width of text wrapping for labels. Must be an integer. Defaults to 25.
#' @param show_legend Whether to show legend. Must be TRUE or FALSE. Defaults to TRUE.
#' @param label_subplots Whether to plot labels indicating the condition of the subplot.
#' @param central Which central-tendency line to draw, given as preferences in
#'   order: the first one that [ensemble()] computed is used. For example,
#'   `c("mean", "median")` draws the mean if it is available, otherwise the
#'   median, and `"none"` draws no line. Defaults to `c("mean", "median",
#'   "none")`.
#' @param spread Which uncertainty band to draw, again as ordered preferences:
#'   `"quantile"` (between the lowest and highest quantile), `"sd"` (the central
#'   line plus/minus one standard deviation), `"range"` (between `min` and
#'   `max`), or `"none"`. The first band the computed statistics can support is
#'   used; `"sd"` also needs a central line. Defaults to `c("quantile", "sd",
#'   "range")`.
#' @param condition_display How to display multiple conditions. Use `"subplots"`
#'   to show conditions as panels, `"slider"` to select one condition with a
#'   slider, or `"dropdown"` to select one condition with a dropdown. Defaults
#'   to `"subplots"`. If only a single condition is available (e.g. no
#'   conditions were varied), `"slider"` and `"dropdown"` fall back to
#'   `"subplots"` with a message. To guarantee the control geometry, plots with
#'   a slider/dropdown have a fixed height (sized to the number of controls)
#'   instead of a responsive one.
#' @param control_options Named list fine-tuning the `"slider"`/`"dropdown"`
#'   condition control and the `animation = "time"` animation. For the
#'   condition control it supports `max_labels`: the maximum number of
#'   slider tick labels to keep visible when many conditions are varied (the
#'   slider always keeps one step per condition; intermediate labels are thinned
#'   above this count); and `spacing`: the vertical gap (in pixels) between the
#'   tops of stacked controls when several condition parameters are varied. By
#'   default the spacing and the reserved bottom margin are sized automatically
#'   so the controls never overlap each other or the x-axis title; pass a number
#'   to widen or tighten the gap. For the animation it supports `frame_ms`:
#'   the duration of each frame in milliseconds (default `100`); `duration`:
#'   the total animation length in seconds, as an alternative to `frame_ms`
#'   (supplying both is an error); `transition_ms`: the transition time between
#'   frames in milliseconds (default `0`); and `max_frames`: the maximum number
#'   of animation frames (default `50`). Defaults to `list()`, i.e. all
#'   defaults.
#' @param animation Animation mode. Use `"none"` for a static plot or `"time"`
#'   to cumulatively reveal trajectories over time. Defaults to `"none"`.
#'   Time animation requires `which = "sims"` (confidence ribbons cannot be
#'   animated) and a single condition (one panel); combining it with
#'   `condition_display` controls or multiple conditions is not supported.
#' @param ... Optional parameters
#' @inheritParams plot.simulate_stockflow
#'
#' @section Styling variables and layers:
#' Names in `colors`, `alpha`, and `line_width` refer to the model variable
#' names, not the labels shown in the legend. For example, use `infected`, not
#' `Infected`, even when `format_label = TRUE` changes the legend text.
#'
#' A single value applies everywhere:
#' `plot(sims, line_width = 3, alpha = 0.7)`.
#'
#' A named vector styles selected variables and leaves the rest at their
#' defaults:
#' `plot(sims, colors = c(infected = "firebrick"), line_width = c(infected = 4))`.
#'
#' Ensemble plots also have layers. Use a list to style the central tendency
#' line, the uncertainty band border, and individual trajectories separately:
#' `plot(sims, which = "sims", line_width = list(central = 3, sims = 1, spread = 0))`.
#'
#' List elements can be named vectors too, which is useful when only one layer
#' needs variable-specific styling:
#' `plot(sims, alpha = list(central = 1, sims = c(infected = 0.15), spread = 0.25))`.
#'
#' The `spread` line width controls the border of the uncertainty band. The
#' default is `0`, so the band is filled but not outlined.
#'
#' @returns Plotly object
#' @export
#' @concept ensemble
#' @seealso [ensemble()]
#' @method plot ensemble_stockflow
#'
plot.ensemble_stockflow <- function(x,
                                    which = c("summary", "sims")[1],
                                    sim = seq(1, min(c(x[["n"]], 100))),
                                    condition = seq(1, min(c(x[["n_conditions"]], 9))),
                                    vars = NULL,
                                    order = NULL,
                                    show_constants = FALSE,
                                    nrows = ceiling(sqrt(max(condition))),
                                    margin = .05,
                                    shareX = TRUE,
                                    shareY = TRUE,
                                    palette = "Dark 2",
                                    alpha = list(central = 1, spread = 0.3, sims = 0.3),
                                    colors = NULL,
                                    line_width = list(central = 3, spread = 0, sims = 1),
                                    font_family = getOption("sdbuildR.font_family", default = "stix-two-text"),
                                    font_size = 16,
                                    wrap_width = 25,
                                    show_legend = TRUE,
                                    label_subplots = TRUE,
                                    central = c("mean", "median", "none"),
                                    spread = c("quantile", "sd", "range"),
                                    format_label = TRUE,
                                    condition_display = c("subplots", "slider", "dropdown"),
                                    control_options = list(),
                                    animation = c("none", "time"),
                                    webgl = getOption("sdbuildR.webgl", default = TRUE),
                                    ...) {
  check_ensemble_stockflow(x)

  user_colors <- colors

  condition_display <- .clean_condition_display(condition_display)
  control_options <- resolve_control_options(control_options)
  animation <- .clean_animation(animation)

  if (animation == "time" && condition_display != "subplots") {
    cli::cli_abort(c(
      "x" = "Combining {.arg animation = \"time\"} with {.arg condition_display} controls is not supported yet.",
      ">" = "Use {.code condition_display = \"subplots\"} or {.code animation = \"none\"}."
    ))
  }

  # Validate common plot parameters
  validate_plot_params(
    show_legend = show_legend,
    vars = vars,
    palette = palette,
    colors = colors,
    font_family = font_family,
    font_size = font_size,
    wrap_width = wrap_width,
    label_subplots = label_subplots,
    format_label = format_label,
    webgl = webgl
  )

  which <- .clean_which(which)

  # Confidence ribbons (which = "summary") cannot be animated cleanly in plotly,
  # so time animation is only supported for individual trajectories.
  if (animation == "time" && which == "summary") {
    cli::cli_abort(c(
      "x" = "Animating summary confidence ribbons is not supported.",
      ">" = "Use {.code which = \"sims\"} to animate individual trajectories, or {.code animation = \"none\"}."
    ))
  }

  # Get passed arguments
  passed_arg <- names(as.list(match.call())[-1])

  dots <- list(...)

  # Resolve the central tendency and spread choices against the statistics that
  # are actually present in the ensemble summary. Both arguments are preference
  # vectors: the first available option wins, otherwise we fall back to "none".
  summary_cols <- names(x[["summary"]])

  if (isFALSE(central)) central <- "none"
  # Lenient matching: accept e.g. "medians" or "Mean" for "median"/"mean".
  central <- normalize_synonyms(central, central_synonyms())
  invalid_central <- setdiff(central, c("mean", "median", "none"))
  if (length(invalid_central) > 0) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg central} value{?s}: {.val {invalid_central}}.",
      "i" = "{.arg central} must be one or more of {.code 'mean'}, {.code 'median'}, or {.code 'none'}."
    ))
  }
  # Lenient matching: accept e.g. "quantiles" or "SDs" for "quantile"/"sd".
  spread <- normalize_synonyms(spread, spread_synonyms())
  invalid_spread <- setdiff(spread, c("quantile", "sd", "range", "none"))
  if (length(invalid_spread) > 0) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg spread} value{?s}: {.val {invalid_spread}}.",
      "i" = "{.arg spread} must be one or more of {.code 'quantile'}, {.code 'sd'}, {.code 'range'}, or {.code 'none'}."
    ))
  }

  central_avail <- intersect(c("mean", "median"), summary_cols)
  central_tendency <- resolve_summary_choice(central, central_avail)
  if (central_tendency == "none" && !("none" %in% central)) {
    cli::cli_warn(c(
      "!" = "None of the requested {.arg central} statistics are saved in the ensemble summary.",
      "i" = "Saved central statistics: {.val {central_avail}}.",
      ">" = "Re-run {.fn ensemble} with {.code central =
      {.val {central}}}."
    ))
  }

  quant_cols <- grep("^quant[0-9]+$", summary_cols, value = TRUE)
  spread_avail <- character(0)
  if (length(quant_cols) > 0) spread_avail <- c(spread_avail, "quantile")
  if ("sd" %in% summary_cols && central_tendency != "none") {
    spread_avail <- c(spread_avail, "sd")
  }
  if (all(c("min", "max") %in% summary_cols)) {
    spread_avail <- c(spread_avail, "range")
  }
  spread_resolved <- resolve_summary_choice(spread, spread_avail)
  if (spread_resolved == "none" && !("none" %in% spread)) {
    cli::cli_warn(c(
      "!" = "None of the requested {.arg spread} options are saved in the ensemble summary.",
      "i" = "Saved spread options: {.val {spread_avail}}.",
      # "i" = "{.code 'quantile'} needs quantile columns; {.code 'sd'} needs the {.field sd} statistic and a central line; {.code 'range'} needs the {.field min} and {.field max} statistics.",
      ">" = "Re-run {.fn ensemble} with {.code spread =
      {.val {spread}}}."
    ))
  }

  # Build default subtitle based on plot type, central tendency and spread
  band_txt <- if (spread_resolved == "quantile") {
    paste0("[", min(x[["quantiles"]]), ", ", max(x[["quantiles"]]), "] confidence interval")
  } else if (spread_resolved == "sd") {
    "+/- 1 SD band"
  } else if (spread_resolved == "range") {
    "min-max range"
  } else {
    ""
  }
  default_sub <- if (which == "summary") {
    lead <- paste(c(
      if (central_tendency != "none") title_case_ascii(central_tendency) else NULL,
      if (nzchar(band_txt)) band_txt else NULL
    ), collapse = " with ")
    paste0(
      if (nzchar(lead)) paste0(lead, " of ") else "",
      x[["n"]], " simulation", ifelse(x[["n"]] == 1, "", "s")
    )
  } else if (which == "sims") {
    paste0(
      ifelse(central_tendency == "none", "",
        paste0(title_case_ascii(central_tendency), " with ")
      ),
      length(sim), "/", x[["n"]], " simulation",
      ifelse(x[["n"]] == 1, "", "s")
    )
  }

  # Extract optional parameters with defaults
  time_unit <- x[["object"]][["sim_settings"]][["time_units"]]
  params <- extract_plot_params(dots, defaults = list(
    main = paste0("Ensemble of ", x[["object"]][["meta"]][["name"]]),
    xlab = paste0("Time", ifelse(nzchar(time_unit), paste0(" (", time_unit, ")"), "")),
    ylab = "",
    sub = default_sub,
    alpha = alpha
  ))
  main <- params$main
  xlab <- params$xlab
  ylab <- params$ylab
  sub <- params$sub
  alpha <- params$alpha

  # Append subtitle to main title
  main <- paste0(main, "<span style='font-size:", font_size, "px;'>\n", sub, "</span>")

  if (!is.null(x[["summary"]])) {
    summary_df <- x[["summary"]]
  } else {
    cli::cli_abort(c(
      "x" = "The ensemble object does not contain summary statistics."
    ))
  }

  # Validate condition index
  if ("condition" %in% passed_arg) {
    .check_condition_index(condition, x[["n_conditions"]])
  }

  # For a single-figure condition control, expose every condition. The default
  # `condition` is capped at 9 for readable subplot grids, which is irrelevant
  # when conditions are selected one at a time (and would break the per-
  # parameter cross-product for larger crossed designs).
  if (condition_display %in% c("slider", "dropdown") &&
    !("condition" %in% passed_arg)) {
    condition <- seq_len(x[["n_conditions"]])
  }

  # A condition control needs at least two conditions to step through; with a
  # single one (e.g. no conditions varied) fall back to the plain display.
  condition_display <- .revert_condition_display(
    condition_display, length(condition)
  )

  # Filter to the selected condition(s). This is a no-op for the default (all
  # conditions); without it, selecting a single condition would still draw every
  # condition's summary overlaid in one plot.
  summary_df <- summary_df[summary_df[["condition"]] %in% condition, , drop = FALSE]

  # Ensure there aren't more rows than condition
  nrows <- min(nrows, length(condition))

  # Whether to create subplots or not
  create_subplots <- length(condition) > 1

  # Plotly animations do not compose with subplot grids, so time animation is
  # only supported for a single condition (one panel).
  if (animation == "time" && create_subplots) {
    cli::cli_abort(c(
      "x" = "Animating multiple conditions at once is not supported.",
      ">" = "Select a single condition (e.g. {.code condition = 1}) to use {.code animation = \"time\"}."
    ))
  }

  # To plot individual simulation trajectories, extract df
  if (which == "sims") {
    if (!is.null(x[["df"]])) {
      df <- x[["df"]]

      # Validate and apply sim filter
      if ("sim" %in% passed_arg) {
        .check_sim_index(sim, x[["n"]])
      }

      # Filter by simulation index and selected condition(s)
      df <- df[df[["sim"]] %in% sim & df[["condition"]] %in% condition, , drop = FALSE]
    } else {
      cli::cli_abort(c(
        "x" = "No simulation data available.",
        "i" = "Individual simulation data is required for {.code which = 'sims'}.",
        ">" = "Run {.fn ensemble} with {.code save_sims = TRUE}."
      ))
    }
  } else if (which == "summary") {
    if ("sim" %in% passed_arg) {
      cli::cli_inform(c(
        "i" = "The {.arg sim} argument is ignored when {.code which = 'summary'}.",
        ">" = "Set {.code which = 'sims'} to plot individual trajectories."
      ))
    }

    df <- NULL
  }

  # Prepare for plotting
  out <- prep_plot(x[["object"]], "ensemble", summary_df,
    constants = x[["constants"]][["summary"]], show_constants = show_constants,
    vars = vars, palette = palette, colors = colors,
    wrap_width = wrap_width, format_label = format_label, order = order
  )
  summary_df_highlight <- out[["df_highlight"]]
  summary_df_nonhighlight <- out[["df_nonhighlight"]]
  colors <- out[["colors"]]
  labels <- out[["labels"]]
  var_names <- out[["var_names"]]

  # Resolve the role-keyed line widths and opacities to per-variable vectors for
  # each layer (central line, spread band, individual trajectories). A scalar or
  # named vector applies to every role; a list keyed by role styles each layer.
  aes_roles <- c("central", "spread", "sims")
  lw <- resolve_aes(line_width, aes_roles,
    defaults = list(central = 3, spread = 0, sims = 1),
    var_names = var_names, arg = "line_width",
    validate_by_role = list(central = "positive", spread = "nonneg", sims = "positive"),
    display_names = labels,
    valid_names = out[["style_names"]]
  )
  al <- resolve_aes(alpha, aes_roles,
    defaults = list(central = 1, spread = 0.3, sims = 0.3),
    var_names = var_names, arg = "alpha",
    validate_by_role = list(central = "unit", spread = "unit", sims = "unit"),
    display_names = labels,
    valid_names = out[["style_names"]]
  )

  if (which == "sims") {
    out <- prep_plot(x[["object"]], "ensemble", df,
      constants = x[["constants"]][["df"]], show_constants = show_constants,
      vars = vars, palette = palette, colors = user_colors,
      wrap_width = wrap_width, format_label = format_label, order = order
    )
    df_highlight <- out[["df_highlight"]]
    df_nonhighlight <- out[["df_nonhighlight"]]
  } else {
    df_highlight <- df_nonhighlight <- NULL
  }

  # For time animation, cumulatively reveal trajectories. Accumulate before
  # splitting by condition so each row keeps its condition; per-frame line
  # breaking happens later in plot_ensemble_helper().
  if (animation == "time") {
    mf <- control_options[["max_frames"]]
    summary_df_highlight <- accumulate_by_time(summary_df_highlight, max_frames = mf)
    summary_df_nonhighlight <- accumulate_by_time(summary_df_nonhighlight, max_frames = mf)
    df_highlight <- accumulate_by_time(df_highlight, max_frames = mf)
    df_nonhighlight <- accumulate_by_time(df_nonhighlight, max_frames = mf)
    frame <- ~.frame
  } else {
    frame <- NULL
  }

  # Whether to replace the subplot grid with a single condition selector.
  condition_control <- condition_display %in% c("slider", "dropdown")

  # Resolve the band bounds based on the spread choice. `q_low`/`q_high` are the
  # column names the helper draws the band from; NULL means no band.
  if (spread_resolved == "quantile") {
    # quant{i} corresponds to x[["quantiles"]][i]; pick lowest & highest prob.
    probs <- x[["quantiles"]]
    qi <- as.integer(sub("^quant", "", quant_cols))
    ord <- order(probs[qi])
    q_low <- quant_cols[ord[1]]
    q_high <- quant_cols[ord[length(ord)]]
  } else if (spread_resolved == "sd") {
    # Build a symmetric central +/- sd band as explicit columns.
    add_sd_band <- function(d) {
      if (is.null(d) || nrow(d) == 0) {
        return(d)
      }
      d[[".band_low"]] <- d[[central_tendency]] - d[["sd"]]
      d[[".band_high"]] <- d[[central_tendency]] + d[["sd"]]
      d
    }
    summary_df_highlight <- add_sd_band(summary_df_highlight)
    summary_df_nonhighlight <- add_sd_band(summary_df_nonhighlight)
    q_low <- ".band_low"
    q_high <- ".band_high"
  } else if (spread_resolved == "range") {
    # Band spans the min and max statistics directly.
    q_low <- "min"
    q_high <- "max"
  } else {
    q_low <- NULL
    q_high <- NULL
  }

  # Check whether there are multiple time points
  mode <- ifelse(length(unique(summary_df[["time"]])) == 1, "markers", "lines")

  # Per-subplot theme is invariant across conditions; compute once.
  subplot_theme <- plotly_theme(font_family = font_family, font_size = font_size)

  # Plot
  if (condition_control) {
    # Build one (non-subplot) plot per condition, then merge into a single
    # figure controlled by a slider or dropdown.
    hl_by <- split_by_cond(df_highlight)
    nhl_by <- split_by_cond(df_nonhighlight)
    shl_by <- split_by_cond(summary_df_highlight)
    snhl_by <- split_by_cond(summary_df_nonhighlight)

    pl_list <- lapply(seq_along(condition), function(j_idx) {
      j_name <- condition[j_idx]
      plot_ensemble_helper(
        subplot_label = "",
        which = which,
        create_subplots = FALSE,
        summary_df_highlight = get_cond(shl_by, j_name, summary_df_highlight),
        summary_df_nonhighlight = get_cond(snhl_by, j_name, summary_df_nonhighlight),
        df_highlight = get_cond(hl_by, j_name, df_highlight),
        df_nonhighlight = get_cond(nhl_by, j_name, df_nonhighlight),
        central_tendency = central_tendency,
        lw = lw,
        al = al,
        q_low = q_low,
        q_high = q_high,
        mode = mode,
        colors = colors,
        show_legend = show_legend,
        dots = dots,
        main = main,
        xlab = xlab, ylab = ylab,
        font_family = font_family,
        font_size = font_size,
        theme = subplot_theme,
        frame = frame,
        webgl = webgl
      )
    })

    control_theme <- plotly_theme(
      font_family = font_family, font_size = font_size, margin_t = 100
    )

    if (is.null(x[["conditions"]])) {
      cond_tbl <- data.frame()
    } else {
      cond_tbl <- as.data.frame(x[["conditions"]])
      cond_tbl <- cond_tbl[, setdiff(names(cond_tbl), "condition"), drop = FALSE]
    }

    pl <- assemble_condition_control_plot(
      pl_list,
      condition_ids = condition,
      type = condition_display,
      labels = ensemble_condition_labels(
        cond_tbl, x[["object"]], condition, format_label
      ),
      theme = control_theme,
      main = main, xlab = xlab, ylab = ylab,
      font_family = font_family, font_size = font_size,
      condition_table = cond_tbl,
      cross = isTRUE(x[["cross"]]),
      object = x[["object"]],
      max_labels = control_options[["max_labels"]],
      spacing = control_options[["spacing"]],
      format_label = format_label
    )
  } else if (!create_subplots) {
    j_idx <- 1
    j_name <- condition[j_idx]
    pl <- plot_ensemble_helper(
      subplot_label = ifelse(label_subplots, paste0("Condition ", j_name), ""),
      which = which,
      create_subplots = create_subplots,
      summary_df_highlight = summary_df_highlight,
      summary_df_nonhighlight = summary_df_nonhighlight,
      df_highlight = df_highlight,
      df_nonhighlight = df_nonhighlight,
      central_tendency = central_tendency,
      lw = lw,
      al = al,
      q_low = q_low,
      q_high = q_high,
      mode = mode,
      colors = colors,
      show_legend = show_legend,
      dots = dots,
      main = main,
      xlab = xlab, ylab = ylab,
      font_family = font_family,
      font_size = font_size,
      theme = subplot_theme,
      frame = frame,
      webgl = webgl
    )
  } else {
    # Pre-split every condition-filtered frame once instead of re-scanning the
    # full frame with a `condition == j_name` filter on every iteration.
    hl_by <- split_by_cond(df_highlight)
    nhl_by <- split_by_cond(df_nonhighlight)
    shl_by <- split_by_cond(summary_df_highlight)
    snhl_by <- split_by_cond(summary_df_nonhighlight)

    # Create a list of plotly objects for each condition
    pl_list <- list()
    for (j_idx in seq_along(condition)) {
      j_name <- condition[j_idx]

      pl_list[[j_idx]] <- plot_ensemble_helper(
        subplot_label = ifelse(label_subplots, paste0("Condition ", j_name), ""),
        which = which,
        create_subplots = create_subplots,
        summary_df_highlight = get_cond(shl_by, j_name, summary_df_highlight),
        summary_df_nonhighlight = get_cond(snhl_by, j_name, summary_df_nonhighlight),
        df_highlight = get_cond(hl_by, j_name, df_highlight),
        df_nonhighlight = get_cond(nhl_by, j_name, df_nonhighlight),
        central_tendency = central_tendency,
        lw = lw,
        al = al,
        q_low = q_low,
        q_high = q_high,
        mode = mode,
        colors = colors,
        # Only show legend if it's the last subplot
        show_legend = ifelse(j_idx != length(condition), FALSE, show_legend),
        dots = dots,
        main = main,
        xlab = xlab, ylab = ylab,
        font_family = font_family,
        font_size = font_size,
        theme = subplot_theme,
        frame = frame,
        webgl = webgl
      )
    }

    theme <- plotly_theme(
      font_family = font_family,
      font_size = font_size,
      margin_t = 100 # Extra space for ensemble title
    )

    pl <- plotly::subplot(pl_list,
      nrows = nrows,
      margin = margin,
      shareX = shareX,
      shareY = shareY,
      titleY = FALSE,
      titleX = FALSE
    ) |>
      plotly::layout(
        title = list(text = main),
        yaxis = list(title = list(text = ylab)),
        font = theme$font,
        margin = theme$margin,
        legend = list(
          orientation = "h",
          x = 0.5,
          y = -0.2,
          xanchor = "center",
          yanchor = "top"
        )
      )

    if (nzchar(xlab)) {
      pl <- pl |>
        plotly::layout(
          annotations = list(
            list(
              text = xlab,
              font = list(
                family = font_family
                # size = font_size
              ),
              xref = "paper",
              yref = "paper",
              xanchor = "center", yanchor = "top",
              x = 0.5, y = -.1,
              showarrow = FALSE
            )
          )
        )
    }
  }

  # Add play button and time slider for the cumulative reveal animation.
  if (animation == "time") {
    n_frames <- length(unique(c(
      summary_df_highlight[[".frame"]], summary_df_nonhighlight[[".frame"]],
      df_highlight[[".frame"]], df_nonhighlight[[".frame"]]
    )))
    pl <- add_time_animation_controls(pl,
      time_unit = if (is.null(time_unit)) "" else time_unit,
      font_family = font_family,
      font_size = font_size,
      frame_ms = resolve_frame_ms(control_options, n_frames),
      transition_ms = control_options[["transition_ms"]]
    )
  }

  apply_webfont(set_plotly_export_format(pl), font_family)
}


#' Order rows by sim then time and insert a spacer row between sims
#'
#' This lets a single scatter trace render each simulation as a separate line:
#' the spacer row carries NA in the x and y columns (so the line breaks between
#' simulations instead of connecting them) while keeping every other column,
#' notably the grouping/colour column, intact. This is applied per variable so
#' that `color = ~variable` still splits the data into one trace per variable
#' and the spacers stay within each variable's group.
#'
#' @param d data.frame for a single variable.
#' @param group Column(s) identifying a single trajectory. A trajectory is one
#'   `(condition, sim)` pair, so multiple columns may be supplied. Defaults to
#'   "sim".
#' @param x Column mapped to the x aesthetic. Defaults to "time".
#' @param y Column mapped to the y aesthetic. Defaults to "value".
#'
#' @returns data.frame with NA spacer rows inserted between trajectories.
#' @noRd
break_by_sim <- function(d, group = "sim", x = "time", y = "value") {
  if (nrow(d) == 0) {
    return(d)
  }
  # Composite key over the grouping columns so e.g. the same sim index in
  # different conditions counts as two distinct trajectories.
  key <- interaction(d[, group, drop = FALSE], drop = TRUE, lex.order = TRUE)
  ord <- order(key, d[[x]])
  d <- d[ord, , drop = FALSE]
  parts <- split(d, key[ord], drop = TRUE)
  if (length(parts) <= 1L) {
    return(d)
  } # nothing to separate
  spacer <- d[1, , drop = FALSE]
  spacer[[x]] <- NA # NA in x and y breaks the line; keep grouping columns
  spacer[[y]] <- NA
  out <- vector("list", 2L * length(parts) - 1L)
  out[seq(1L, length(out), by = 2L)] <- parts
  out[seq(2L, length(out), by = 2L)] <- rep(list(spacer), length(parts) - 1L)
  do.call(rbind, out)
}


#' Insert NA spacers between simulations within each variable
#'
#' Applies break_by_sim() to each variable level present so that a single
#' `color = ~variable` scatter trace renders each simulation as a separate
#' line. Returns the recombined data.frame (variable order preserved).
#'
#' @param d data.frame with a factor "variable" column.
#' @returns data.frame with NA spacer rows inserted between simulations.
#' @noRd
break_sims_by_variable <- function(d) {
  if (nrow(d) == 0) {
    return(d)
  }
  # A trajectory is one (condition, sim) pair; fall back to "sim" alone if no
  # condition column is present.
  group <- intersect(c("condition", "sim"), colnames(d))
  if (length(group) == 0L) group <- "sim"
  parts <- lapply(
    levels(droplevels(d[["variable"]])),
    function(v) break_by_sim(d[d[["variable"]] == v, , drop = FALSE], group = group)
  )
  do.call(rbind, parts)
}


#' Split a data.frame by its "condition" column
#'
#' Pre-splitting once before the subplot loop avoids re-scanning the full frame
#' with a `condition == j_name` filter on every iteration. List names are
#' `as.character()` of the condition values.
#'
#' @param d data.frame with a "condition" column, or NULL.
#' @returns Named list of per-condition data.frames, or NULL if `d` is NULL.
#' @noRd
split_by_cond <- function(d) {
  if (is.null(d)) {
    return(NULL)
  }
  split(d, d[["condition"]])
}


#' Retrieve one condition's rows from a pre-split list
#'
#' Returns an empty-but-correctly-typed frame (same columns and factor levels as
#' `template`) for conditions with no rows, so behaviour matches the old
#' `d[d[["condition"]] == key, ]` filter exactly.
#'
#' @param lst Named list produced by split_by_cond(), or NULL.
#' @param key Condition value to look up.
#' @param template Full data.frame to use as a 0-row template when absent, or
#'   NULL (in which case NULL is returned, matching the old `NULL[...]` filter).
#' @returns data.frame of rows for `key`, or NULL.
#' @noRd
get_cond <- function(lst, key, template) {
  if (is.null(template)) {
    return(NULL)
  }
  d <- lst[[as.character(key)]]
  if (is.null(d)) template[0, , drop = FALSE] else d
}


#' Helper function to plot ensemble simulation results
#'
#' @param subplot_label Label for the subplot. Only used if create_subplots = TRUE.
#' @param which Type of plot. Must be one of "summary" or "sims". Defaults to "summary". If "summary", the plot will show the mean and confidence intervals of the simulation results. If "sims", the plot will show all individual simulation runs in sim.
#' @param create_subplots If TRUE, create subplots for each condition. If FALSE, plot all conditions in one plot.
#' @param summary_df_highlight data.frame with summary statistics of the ensemble simulation results (stocks). Must contain columns "condition", "variable", "mean", and confidence interval columns (e.g., "q0.025", "q0.975").
#' @param summary_df_nonhighlight data.frame with summary statistics of the ensemble simulation results (non-stocks). Must contain columns "condition", "variable", "mean", and confidence interval columns (e.g., "q0.025", "q0.975").
#' @param df_highlight data.frame with individual simulation results (stocks). Must contain columns "sim", "condition", "variable", and "value". Only used if which = "sims".
#' @param df_nonhighlight data.frame with individual simulation results (non-stocks). Must contain columns "sim", "condition", "variable", and "value". Only used if which = "sims".
#' @param q_low Column name for the lower bound of the confidence interval (e.g., "q0.025").
#' @param q_high Column name for the upper bound of the confidence interval (e.g., "q0.975").
#' @param mode Plotting mode. Either "lines" if there are multiple time points or "markers" for a single time point.
#' @param colors Vector of colours. If NULL, the color palette will be used. If specified, will override palette. The number of colours must be equal to the number of variables in the simulation data frame. Defaults to NULL.
#' @param lw Resolved line widths: a list with `central`/`spread`/`sims`
#'   elements, each a per-variable numeric vector keyed by plotted label.
#' @param al Resolved opacities: a list with `central`/`spread`/`sims` elements,
#'   each a per-variable numeric vector keyed by plotted label.
#' @param dots List of additional parameters passed to the plotly functions.
#' @param main Main title of the plot. Defaults to the name of the stock-and-flow model and the number of simulations.
#' @param xlab Label on x-axis.
#' @param ylab Label on y-axis.
#' @param theme List of styling parameters from plotly_theme(). Invariant
#'   across conditions, so computed once by the caller and passed in.
#' @inheritParams plot.ensemble_stockflow
#'
#' @returns Plotly object
#' @noRd
plot_ensemble_helper <- function(subplot_label,
                                 which, create_subplots,
                                 summary_df_highlight,
                                 summary_df_nonhighlight,
                                 df_highlight,
                                 df_nonhighlight,
                                 central_tendency,
                                 lw, al,
                                 q_low, q_high,
                                 mode,
                                 colors, show_legend, dots,
                                 main, xlab, ylab,
                                 font_family, font_size, theme,
                                 frame = NULL, webgl = TRUE) {
  # Local wrapper that injects the animation frame mapping into every trace pair
  # only when animating, leaving non-animated behaviour untouched.
  avp <- function(pl, add_fn, ...) {
    args <- list(pl, add_fn, ...)
    if (!is.null(frame)) args[["frame"]] <- frame
    do.call(add_visibility_pair, args)
  }

  if (which == "sims") {
    plot_highlight <- nrow(df_highlight) > 0
    plot_nonhighlight <- nrow(df_nonhighlight) > 0
  } else if (which == "summary") {
    plot_highlight <- nrow(summary_df_highlight) > 0
    plot_nonhighlight <- nrow(summary_df_nonhighlight) > 0
  }
  plot_summary <- (nrow(summary_df_highlight) > 0) || (nrow(summary_df_nonhighlight) > 0)

  nr_var <- length(colors)

  # Per-variable styling for each layer, keyed by plotted labels (= names(colors)).
  # User-supplied names were matched against model variable names before this point.
  lw_sims <- lw[["sims"]]
  lw_ct <- lw[["central"]]
  lw_spread <- lw[["spread"]]
  al_sims <- al[["sims"]]
  al_ct <- al[["central"]]
  al_spread <- al[["spread"]]

  # A layer can use a single colour-mapped trace (the fast path) only when every
  # plotted variable shares the same width and opacity; otherwise each variable
  # needs its own trace to carry its own width/opacity.
  ct_uniform <- length(unique(unname(lw_ct))) == 1L &&
    length(unique(unname(al_ct))) == 1L
  spread_uniform <- length(unique(unname(lw_spread))) == 1L &&
    length(unique(unname(al_spread))) == 1L

  # The uniform fast path maps colours via plotly's `color = ~variable` aesthetic
  # and `colors =` palette. That palette is silently dropped (plotly falls back to
  # its default colourway) when the figure already contains traces with an
  # explicit `line$color` and no colour aesthetic. Two layers add such explicit
  # traces *before* the central tendency: the `which = "sims"` trajectories (drawn
  # with explicit rgba), and a non-uniform spread band (per-variable width/opacity
  # is drawn one explicit-colour ribbon per variable). Whenever either precedes
  # the central line, we must colour the central tendency explicitly too, so it
  # matches the band/trajectories instead of plotly's default colourway.
  nonuniform_band <- which == "summary" && mode == "lines" &&
    !is.null(q_low) && !spread_uniform
  explicit_color_traces <- which == "sims" || nonuniform_band

  # Initialize plotly object
  pl <- plotly::plot_ly()

  if (which == "summary") {
    # q_low/q_high are NULL when no spread band was requested/available. The band
    # width is its border line width (`spread`; 0 = fill only) and its opacity is
    # the `spread` alpha. No legend entry: the band shares legendgroup = ~variable
    # with the central tendency trace, which carries the legend and toggles both.
    if (mode == "lines" && !is.null(q_low)) {
      if (spread_uniform) {
        pl <- avp(pl, plotly::add_ribbons,
          data_nonhighlight = summary_df_nonhighlight,
          data_highlight = summary_df_highlight,
          plot_nonhighlight = plot_nonhighlight,
          plot_highlight = plot_highlight,
          x = ~time,
          ymin = ~ get(q_low),
          ymax = ~ get(q_high),
          color = ~variable,
          legendgroup = ~variable,
          fillcolor = ~variable,
          opacity = unname(al_spread)[1],
          line = list(width = unname(lw_spread)[1]),
          type = "scatter",
          mode = mode,
          colors = colors,
          showlegend = FALSE
        )
      } else {
        # Per-variable bands: each variable carries its own border width and
        # opacity (alpha baked into the fill colour so it varies per variable).
        add_band_one <- function(pl, data, visible_val) {
          if (is.null(data) || nrow(data) == 0) {
            return(pl)
          }
          for (v in levels(droplevels(data[["variable"]]))) {
            dv <- data[data[["variable"]] == v, , drop = FALSE]
            if (nrow(dv) == 0) next
            args <- list(pl,
              data = dv, x = ~time,
              ymin = ~ get(q_low), ymax = ~ get(q_high),
              name = v, legendgroup = v,
              fillcolor = grDevices::adjustcolor(colors[[v]], alpha.f = al_spread[[v]]),
              line = list(width = unname(lw_spread[[v]]), color = unname(colors[[v]])),
              type = "scatter", mode = mode, showlegend = FALSE
            )
            if (!is.null(frame)) args[["frame"]] <- frame
            args[["visible"]] <- visible_val
            pl <- do.call(plotly::add_ribbons, args)
          }
          pl
        }
        if (plot_nonhighlight) pl <- add_band_one(pl, summary_df_nonhighlight, "legendonly")
        if (plot_highlight) pl <- add_band_one(pl, summary_df_highlight, TRUE)
      }
    }
  } else if (which == "sims") {
    # Alpha is baked directly into each variable's line colour (rgba). Plotly's
    # color = ~variable / colors = mapping strips alpha during plotly_build, so we
    # set line = list(color = ...) explicitly instead. This makes overlapping
    # trajectories accumulate density both in SVG (webgl = FALSE: one separately
    # composited scatter trace per sim) and in WebGL (webgl = TRUE: a single
    # scattergl trace per variable, blended per-pixel on the GPU).
    trace_type <- if (webgl) "scattergl" else "scatter"

    # One legend entry per variable, but only when there is no summary trace to
    # carry the legend (otherwise it would be duplicated).
    sim_showlegend <- if (plot_summary) FALSE else show_legend

    # break_for_traces() inserts NA spacer rows between sims (and, when animating,
    # within each frame) so a single scattergl trace renders each sim as its own
    # line; connectgaps = FALSE keeps the NA gaps from being bridged.
    break_for_traces <- function(d) {
      if (is.null(frame) || !".frame" %in% colnames(d) || nrow(d) == 0L) {
        return(break_sims_by_variable(d))
      }
      parts <- lapply(split(d, d[[".frame"]]), break_sims_by_variable)
      do.call(rbind, parts)
    }

    # Add the per-variable trajectory traces for one data frame (highlight or
    # nonhighlight) at a given visibility.
    add_sim_variable_traces <- function(pl, d, visible) {
      if (is.null(d) || nrow(d) == 0) {
        return(pl)
      }
      for (v in levels(droplevels(d[["variable"]]))) {
        dv <- d[d[["variable"]] == v, , drop = FALSE]
        if (nrow(dv) == 0) next
        args <- list(
          pl,
          data = if (webgl) break_for_traces(dv) else dv,
          x = ~time,
          y = ~value,
          name = v,
          legendgroup = v,
          type = trace_type,
          mode = mode,
          line = list(
            color = grDevices::adjustcolor(colors[[v]], alpha.f = al_sims[[v]]),
            width = unname(lw_sims[[v]])
          ),
          showlegend = sim_showlegend,
          visible = visible
        )
        if (webgl) {
          args[["connectgaps"]] <- FALSE # required: NA must break the line, not bridge it
        } else {
          args[["split"]] <- ~sim # one separately-composited line per sim
        }
        if (!is.null(frame)) args[["frame"]] <- frame
        pl <- do.call(plotly::add_trace, args)
      }
      pl
    }

    # nonhighlight (visible = "legendonly") then highlight (visible = TRUE)
    if (plot_nonhighlight) {
      pl <- add_sim_variable_traces(pl, df_nonhighlight, "legendonly")
    }
    if (plot_highlight) {
      pl <- add_sim_variable_traces(pl, df_highlight, TRUE)
    }
  }

  # When central_tendency = "none", we still want a legend. Draw an invisible
  # central trace from a synthetic column (self-contained, so it works even when
  # no mean/median statistic is present in the summary).
  if (central_tendency == "none" && plot_summary) {
    # Prefer an existing mean/median column for the invisible legend trace, so
    # behaviour (and snapshots) match the historical default; only synthesise a
    # column when the summary has no central statistic at all.
    avail_ct <- intersect(c("mean", "median"), colnames(summary_df_highlight))
    if (length(avail_ct) == 0) {
      avail_ct <- intersect(c("mean", "median"), colnames(summary_df_nonhighlight))
    }
    central_tendency <- if (length(avail_ct) > 0) avail_ct[1] else ".ct_legend"

    if (mode == "markers") {
      # An all-Infinity value plots no marker, but the legend still shows up.
      summary_df_highlight[[central_tendency]] <- rep(Inf, nrow(summary_df_highlight))
      summary_df_nonhighlight[[central_tendency]] <- rep(Inf, nrow(summary_df_nonhighlight))
    } else if (mode == "lines") {
      # A single time point plots no line, but the legend still shows up.
      summary_df_highlight <- summary_df_highlight[summary_df_highlight[["time"]] == summary_df_highlight[["time"]][1], , drop = FALSE]
      summary_df_nonhighlight <- summary_df_nonhighlight[summary_df_nonhighlight[["time"]] == summary_df_nonhighlight[["time"]][1], , drop = FALSE]
      if (central_tendency == ".ct_legend") {
        summary_df_highlight[[central_tendency]] <- rep(Inf, nrow(summary_df_highlight))
        summary_df_nonhighlight[[central_tendency]] <- rep(Inf, nrow(summary_df_nonhighlight))
      }
    }
  }

  # Add the central-tendency trace(s) for one summary data frame at a given
  # visibility. When all widths are equal, a single colour-mapped trace per
  # visibility suffices (identical to before); when they differ -- or when
  # explicit-colour trajectories are present and would break plotly's palette
  # resolution -- one trace per variable is added so each carries its own
  # explicit colour and line width / marker size.
  add_ct_one <- function(pl, data, visible_val, extra_uniform, extra_by_var) {
    if (is.null(data) || nrow(data) == 0) {
      return(pl)
    }
    if (ct_uniform && !explicit_color_traces) {
      args <- c(
        list(pl,
          data = data, x = ~time, y = ~ get(central_tendency),
          color = ~variable, legendgroup = ~variable, type = "scatter",
          mode = mode, colors = colors, showlegend = show_legend
        ),
        extra_uniform
      )
      # Fade the central line/markers when its opacity is below 1 (omitted at
      # full opacity so the common case stays byte-identical to before).
      if (unname(al_ct)[1] < 1) args[["opacity"]] <- unname(al_ct)[1]
      # Match the historical key order (extras and frame before visible) so
      # uniform-width plots produce byte-identical output to the previous code.
      if (!is.null(frame)) args[["frame"]] <- frame
      args[["visible"]] <- visible_val
      return(do.call(plotly::add_trace, args))
    }
    for (v in levels(droplevels(data[["variable"]]))) {
      dv <- data[data[["variable"]] == v, , drop = FALSE]
      if (nrow(dv) == 0) next
      args <- c(
        list(pl,
          data = dv, x = ~time, y = ~ get(central_tendency),
          name = v, legendgroup = v, type = "scatter", mode = mode,
          showlegend = show_legend
        ),
        extra_by_var(v, dv)
      )
      if (!is.null(frame)) args[["frame"]] <- frame
      args[["visible"]] <- visible_val
      pl <- do.call(plotly::add_trace, args)
    }
    pl
  }

  # nonhighlight (visible = "legendonly") then highlight (visible = TRUE)
  add_ct_pair <- function(pl, extra_uniform, extra_by_var) {
    if (plot_nonhighlight) {
      pl <- add_ct_one(pl, summary_df_nonhighlight, "legendonly", extra_uniform, extra_by_var)
    }
    if (plot_highlight) {
      pl <- add_ct_one(pl, summary_df_highlight, TRUE, extra_uniform, extra_by_var)
    }
    pl
  }

  # Plot mean/median points/lines
  if (plot_summary) {
    if (mode == "lines") {
      pl <- add_ct_pair(pl,
        extra_uniform = list(line = list(width = unname(lw_ct)[1])), # thicker line for mean
        extra_by_var = function(v, dv) {
          list(line = list(
            width = unname(lw_ct[v]),
            color = grDevices::adjustcolor(colors[[v]], alpha.f = al_ct[[v]])
          ))
        }
      )
    } else if (mode == "markers" && which == "summary" && !is.null(q_low)) {
      pl <- add_ct_pair(pl,
        extra_uniform = list(
          error_y = ~ list(
            symmetric = FALSE,
            arrayminus = get(central_tendency) - get(q_low),
            array = get(q_high) - get(central_tendency),
            color = colors
          ),
          marker = list(size = unname(lw_ct)[1] * 3) # bigger marker for mean
        ),
        # Build error bars from the per-variable subset directly (a formula would
        # capture `v` by reference and resolve it after the loop has ended).
        extra_by_var = function(v, dv) {
          col_v <- grDevices::adjustcolor(colors[[v]], alpha.f = al_ct[[v]])
          list(
            error_y = list(
              symmetric = FALSE,
              arrayminus = dv[[central_tendency]] - dv[[q_low]],
              array = dv[[q_high]] - dv[[central_tendency]],
              color = col_v
            ),
            marker = list(size = unname(lw_ct[v]) * 3, color = col_v)
          )
        }
      )
    } else if (mode == "markers") {
      # Single time point, no error bars: plain markers for sims, or for summary
      # plots without a spread band.
      pl <- add_ct_pair(pl,
        extra_uniform = list(marker = list(size = unname(lw_ct)[1] * 3)), # bigger marker for mean
        extra_by_var = function(v, dv) {
          list(marker = list(
            size = unname(lw_ct[v]) * 3,
            color = grDevices::adjustcolor(colors[[v]], alpha.f = al_ct[[v]])
          ))
        }
      )
    }
  }

  # Customize layout (theme is invariant across conditions, computed by caller)
  pl <- plotly::layout(pl,
    margin = theme$margin,
    legend = theme$legend
  )

  # If there is only one trace, legend doesn't show
  if (show_legend && (nr_var == 1)) {
    pl <- plotly::layout(pl, showlegend = TRUE)
  }


  if (!create_subplots) {
    pl <- plotly::layout(pl,
      title = main,
      xaxis = list(title = xlab),
      yaxis = list(title = ylab),
      font = list(family = font_family, size = font_size)
    )
  }


  # Add subplot title
  if (create_subplots && nzchar(subplot_label)) {
    pl <- plotly::layout(pl,
      annotations = list(
        list(
          text = subplot_label,
          font = list(
            family = font_family,
            size = ceiling(font_size * .75)
          ),
          bgcolor = "white",
          xref = "paper",
          yref = "paper",
          xanchor = "center", yanchor = "top",
          x = 0.5, y = 1,
          showarrow = FALSE
        )
      )
    )
  }

  # Set x-axis limits if specified
  if ("xlim" %in% names(dots)) {
    pl <- plotly::layout(pl, xaxis = list(range = dots[["xlim"]]))
  }

  # Set y-axis limits if specified
  if ("ylim" %in% names(dots)) {
    pl <- plotly::layout(pl, yaxis = list(range = dots[["ylim"]]))
  }

  pl
}


#' Plot verify results
#'
#' Visualize the simulation(s) used during [verify()]. Each *condition* is one
#' simulation run -- the model with one set of parameter overrides (from
#' `unit_test(conditions = )`, possibly none) -- shared by every unit test
#' declaring those overrides. Each condition is displayed as a subplot (or a
#' slider/dropdown step, see `condition_display`), labelled by its overrides
#' and the test(s) evaluated on it: e.g. `"rate = 0 (test 2)"`, with the
#' unmodified run labelled `"Baseline (tests 1, 3)"` (or just `"Tests 1, 3"`
#' when no test varies anything). Simulations are always available since
#' [verify()] unconditionally retains them.
#'
#' @param x Output of [verify()].
#' @param test Integer vector of test numbers to plot.
#' Combines with `label` and `status` as AND intersection.
#' @param label Character vector of regex patterns for partial, case-insensitive
#'   label matching. A test is included if its label matches any pattern.
#' @param ignore_case Logical; whether `label` matching is case-insensitive.
#'   Default `TRUE`.
#' @param condition Integer vector of condition numbers to plot. Defaults to `1:9`. If only one condition is specified, the plot will not be a grid of subplots.
#' @param nrows Number of subplot rows. Defaults to `ceiling(sqrt(max(condition)))`.
#' @param shareX Share the x-axis across subplots. Defaults to `TRUE`.
#' @param shareY Share the y-axis across subplots. Defaults to `TRUE`.
#' @param palette Colour palette (see `hcl.pals()`). Defaults to `"Dark 2"`.
#' @param colors Colours for the plotted variables. A named vector (names are
#'   variable names) recolours only those variables, the palette fills the rest;
#'   an unnamed vector assigns colours in plot order. Defaults to `NULL`.
#' @param line_width Line width of the trajectories. Either a single value, a
#'   named per-variable vector (names are variable names), or an unnamed vector
#'   with one value per variable in plot order. Defaults to `2`.
#' @param alpha Trajectory opacity, between 0 and 1. A single value or a named
#'   per-variable vector. Defaults to `1`.
#' @param font_family Font family. Kebab-case names (e.g. `"eb-garamond"`)
#'   are Fontsource ids (browse them at <https://fontsource.org/>), loaded as
#'   webfonts: no installation is needed, but internet access is required to
#'   display them. Other fonts must be installed on the system viewing the
#'   plot. Defaults to the `sdbuildR.font_family` option, or
#'   the `"stix-two-text"` webfont when the option is unset; use
#'   `options(sdbuildR.font_family = )` to change the default for all plots,
#'   e.g. in your .Rprofile.
#' @param font_size Font size. Defaults to `16`.
#' @param wrap_width Label wrap width. Defaults to `25`.
#' @param show_legend Whether to show the legend. Defaults to `TRUE`.
#' @param label_subplots Whether to title each subplot with its condition's
#'   parameter overrides (or `"Baseline"`) and test number(s), e.g.
#'   `"rate = 0 (test 2)"`. Defaults to `TRUE`.
#' @param control_options Named list fine-tuning the `"slider"`/`"dropdown"`
#'   condition control and the `animation = "time"` animation; see
#'   [plot.ensemble_stockflow()] for the supported options (`max_labels`,
#'   `spacing`, `frame_ms`, `duration`, `transition_ms`, `max_frames`).
#'   Defaults to `list()`, i.e. all defaults.
#' @param ... Additional arguments passed to [plot.simulate_stockflow()].
#' @inheritParams as.data.frame.verify_stockflow
#' @inheritParams plot.simulate_stockflow
#' @inheritParams plot.ensemble_stockflow
#'
#' @section Styling variables:
#' Names in `colors`, `line_width`, and `alpha` refer to the model variable
#' names, not the labels shown in the legend. This keeps the styling stable if
#' labels are formatted or wrapped for display.
#'
#' Use one value to style every trajectory:
#' `plot(res, line_width = 3, alpha = 0.6)`.
#'
#' Use named vectors to style selected variables:
#' `plot(res, colors = c(susceptible = "#377EB8"), alpha = c(infected = 0.4))`.
#'
#' Unnamed vectors are applied in plot order. Named vectors are usually clearer,
#' especially when `vars`, `test`, `label`, or `status` filters change what is
#' drawn.
#'
#' @returns A plotly object.
#' @export
#' @concept unitTest
#' @method plot verify_stockflow
#' @seealso [verify()], [plot.simulate_stockflow()], [plot.ensemble_stockflow()]
#'
#' @examples
#' sfm <- stockflow("sir") |>
#'   unit_test(expr = all(susceptible >= 0))
#' res <- verify(sfm)
#' plot(res)
#'
#' # Select one condition at a time with a slider or dropdown
#' plot(res, condition_display = "slider")
#' plot(res, condition_display = "dropdown")
#'
#' # Cumulatively reveal the trajectories over time
#' plot(res, animation = "time")
plot.verify_stockflow <- function(x,
                                  test = NULL,
                                  vars = NULL,
                                  order = NULL,
                                  show_constants = FALSE,
                                  label = NULL,
                                  ignore_case = TRUE,
                                  status = c("pass", "fail", "error", "skip"),
                                  condition = seq(1, min(c(x[["n_conditions"]], 9))),
                                  nrows = ceiling(sqrt(max(condition))),
                                  shareX = TRUE,
                                  shareY = TRUE,
                                  palette = "Dark 2",
                                  colors = NULL,
                                  line_width = 2,
                                  font_family = getOption("sdbuildR.font_family", default = "stix-two-text"),
                                  font_size = 16,
                                  wrap_width = 25,
                                  show_legend = TRUE,
                                  label_subplots = TRUE,
                                  alpha = 1,
                                  margin = .05,
                                  format_label = TRUE,
                                  condition_display = c("subplots", "slider", "dropdown"),
                                  control_options = list(),
                                  animation = c("none", "time"),
                                  webgl = getOption("sdbuildR.webgl", default = TRUE),
                                  ...) {
  # Check whether it is a verify_stockflow object
  check_verify_stockflow(x)

  condition_display <- .clean_condition_display(condition_display)
  control_options <- resolve_control_options(control_options)
  animation <- .clean_animation(animation)

  if (animation == "time" && condition_display != "subplots") {
    cli::cli_abort(c(
      "x" = "Combining {.arg animation = \"time\"} with {.arg condition_display} controls is not supported yet.",
      ">" = "Use {.code condition_display = \"subplots\"} or {.code animation = \"none\"}."
    ))
  }

  # Validate common plot parameters
  validate_plot_params(
    show_legend = show_legend,
    vars = vars,
    palette = palette,
    colors = colors,
    font_family = font_family,
    font_size = font_size,
    wrap_width = wrap_width,
    label_subplots = label_subplots,
    format_label = format_label,
    webgl = webgl
  )

  # Get passed arguments
  passed_arg <- names(as.list(match.call())[-1])
  dots <- list(...)

  # Filter simulations based on test, label, status, and condition
  df <- as.data.frame(x,
    which = "sims",
    # Pass on filtering arguments to as.data.frame() to filter the simulations before plotting
    test = test, label = label,
    ignore_case = ignore_case, status = status, condition = condition
  )

  # Add sim column for plot_ensemble_helper() if not already present (e.g., when which = "summary")
  if (!"sim" %in% colnames(df)) {
    df[["sim"]] <- 1
  }

  # Find how many tests and conditions are available after filtering
  test_column <- paste0(unique(df[["test"]]), collapse = ", ")

  # Test numbers are stored as 1, 2, 3
  test_nrs <- strsplit(test_column, ", ") |>
    unlist() |>
    trimws() |>
    as.integer()
  n_tests <- length(test_nrs)
  condition_nrs <- unique(df[["condition"]])
  n_conditions <- length(condition_nrs)

  # A condition control needs at least two conditions to step through; with a
  # single one (e.g. after filtering) fall back to the plain display.
  condition_display <- .revert_condition_display(condition_display, n_conditions)

  # Ensure there aren't more rows than conditions
  nrows <- min(nrows, n_conditions)

  # Whether to create subplots or not
  create_subplots <- n_conditions > 1

  # Plotly animations do not compose with subplot grids, so time animation is
  # only supported for a single condition (one panel).
  if (animation == "time" && create_subplots) {
    cli::cli_abort(c(
      "x" = "Animating multiple conditions at once is not supported.",
      ">" = "Select a single condition (e.g. {.code condition = 1}) to use {.code animation = \"time\"}."
    ))
  }

  # Extract constants from verify object to pass to prep_plot()
  constants <- lapply(condition_nrs, function(y) {
    const <- x[["sims"]][[y]][["constants"]]
    data.frame(
      # For verify(), sim = 1
      sim = 1,
      condition = y, variable = names(const), value = unlist(unname(const))
    )
  })
  constants <- as.data.frame(do.call(rbind, constants))

  # Build default subtitle
  default_sub <- paste0(
    n_tests, " unit test", ifelse(n_tests > 1, "s", ""), " across ",
    n_conditions, " condition", ifelse(n_conditions > 1, "s", "")
  )

  # Extract optional parameters with defaults
  sfm <- x[["object"]]
  time_unit <- sfm[["sim_settings"]][["time_units"]]
  params <- extract_plot_params(dots, defaults = list(
    main = paste0("Unit Tests of ", sfm[["meta"]][["name"]]),
    xlab = paste0("Time", ifelse(nzchar(time_unit), paste0(" (", time_unit, ")"), "")),
    ylab = "",
    sub = default_sub,
    alpha = alpha
  ))
  main <- params$main
  xlab <- params$xlab
  ylab <- params$ylab
  sub <- params$sub
  alpha <- params$alpha

  # Append subtitle to main title
  main <- paste0(main, "<span style='font-size:", font_size, "px;'>\n", sub, "</span>")

  # Prepare for plotting
  out <- prep_plot(sfm, "verify", df,
    constants = constants, show_constants = show_constants,
    vars = vars, palette = palette, colors = colors,
    wrap_width = wrap_width, format_label = format_label, order = order
  )
  df_highlight <- out[["df_highlight"]]
  df_nonhighlight <- out[["df_nonhighlight"]]
  colors <- out[["colors"]]
  labels <- out[["labels"]]
  var_names <- out[["var_names"]]

  # verify() only draws individual trajectories (no central line or band), so
  # `line_width`/`alpha` style the `sims` layer; the other roles are placeholders
  # that the helper does not draw here.
  line_width_sims <- expand_aes(line_width, var_names,
    default = 2, arg = "line_width", validate = "positive",
    display_names = labels, valid_names = out[["style_names"]]
  )
  alpha_sims <- expand_aes(alpha, var_names,
    default = 1, arg = "alpha", validate = "unit",
    display_names = labels, valid_names = out[["style_names"]]
  )
  lw <- list(
    central = aes_constant(3, labels),
    spread = aes_constant(0, labels),
    sims = line_width_sims
  )
  al <- list(
    central = aes_constant(1, labels),
    spread = aes_constant(0.3, labels),
    sims = alpha_sims
  )

  # For time animation, cumulatively reveal trajectories (see ensemble plot)
  if (animation == "time") {
    df_highlight <- accumulate_by_time(df_highlight,
      max_frames = control_options[["max_frames"]]
    )
    df_nonhighlight <- accumulate_by_time(df_nonhighlight,
      max_frames = control_options[["max_frames"]]
    )
    frame <- ~.frame
  } else {
    frame <- NULL
  }

  # Whether to replace the subplot grid with a single condition selector
  condition_control <- condition_display %in% c("slider", "dropdown")

  # Check whether there are multiple time points
  mode <- ifelse(length(unique(df[["time"]])) == 1, "markers", "lines")

  # Plot
  which <- "sims"
  summary_df_highlight <- summary_df_nonhighlight <- data.frame()
  q_low <- q_high <- NULL
  central_tendency <- "none"

  # Per-subplot theme is invariant across conditions; compute once.
  subplot_theme <- plotly_theme(font_family = font_family, font_size = font_size)

  # One label per condition, shared by the subplot titles and the condition
  # controls so every display names a run the same way: by its parameter
  # overrides (or "Baseline") and the test(s) evaluated on it, never by the
  # bare condition index.
  cond_labels <- stats::setNames(
    make_verify_condition_labels(df, condition_nrs),
    as.character(condition_nrs)
  )
  cond_label <- function(j) {
    lab <- cond_labels[[as.character(j)]]
    if (is.null(lab)) paste0("Condition ", j) else lab
  }

  if (condition_control) {
    # Build one (non-subplot) plot per present condition, then merge into a
    # single figure controlled by a slider or dropdown.
    hl_by <- split_by_cond(df_highlight)
    nhl_by <- split_by_cond(df_nonhighlight)

    pl_list <- lapply(seq_along(condition_nrs), function(j_idx) {
      j_name <- condition_nrs[j_idx]
      plot_ensemble_helper(
        subplot_label = "",
        which = which,
        create_subplots = FALSE,
        summary_df_highlight = summary_df_highlight,
        summary_df_nonhighlight = summary_df_nonhighlight,
        df_highlight = get_cond(hl_by, j_name, df_highlight),
        df_nonhighlight = get_cond(nhl_by, j_name, df_nonhighlight),
        central_tendency = central_tendency,
        lw = lw,
        al = al,
        q_low = q_low,
        q_high = q_high,
        mode = mode,
        colors = colors,
        show_legend = show_legend,
        dots = dots,
        main = main,
        xlab = xlab, ylab = ylab,
        font_family = font_family,
        font_size = font_size,
        theme = subplot_theme,
        frame = frame,
        webgl = webgl
      )
    })

    control_theme <- plotly_theme(
      font_family = font_family, font_size = font_size, margin_t = 100
    )

    pl <- assemble_condition_control_plot(
      pl_list,
      condition_ids = condition_nrs,
      type = condition_display,
      labels = unname(cond_labels),
      theme = control_theme,
      main = main, xlab = xlab, ylab = ylab,
      font_family = font_family, font_size = font_size,
      max_labels = control_options[["max_labels"]],
      spacing = control_options[["spacing"]],
      format_label = format_label,
      # Verify condition labels are long descriptions (e.g. "rate = 0
      # (test 2)"); keep them off the slider rail ticks but show the
      # selected one in the slider title.
      tick_labels = FALSE
    )
  } else if (!create_subplots) {
    j_idx <- 1
    j_name <- condition_nrs[j_idx]
    pl <- plot_ensemble_helper(
      subplot_label = if (label_subplots) cond_label(j_name) else "",
      which = which,
      create_subplots = create_subplots,
      summary_df_highlight = summary_df_highlight,
      summary_df_nonhighlight = summary_df_nonhighlight,
      df_highlight = df_highlight,
      df_nonhighlight = df_nonhighlight,
      central_tendency = central_tendency,
      lw = lw,
      al = al,
      q_low = q_low,
      q_high = q_high,
      mode = mode,
      colors = colors,
      show_legend = show_legend,
      dots = dots,
      main = main,
      xlab = xlab, ylab = ylab,
      font_family = font_family,
      font_size = font_size,
      theme = subplot_theme,
      frame = frame,
      webgl = webgl
    )
  } else {
    # Pre-split the condition-filtered frames once (summary frames are empty for
    # verify, so they are passed through directly).
    hl_by <- split_by_cond(df_highlight)
    nhl_by <- split_by_cond(df_nonhighlight)

    # Create a list of plotly objects for each condition
    pl_list <- list()
    for (j_idx in seq_along(condition)) {
      j_name <- condition[j_idx]

      pl_list[[j_idx]] <- plot_ensemble_helper(
        subplot_label = if (label_subplots) cond_label(j_name) else "",
        which = which,
        create_subplots = create_subplots,
        summary_df_highlight = summary_df_highlight,
        summary_df_nonhighlight = summary_df_nonhighlight,
        df_highlight = get_cond(hl_by, j_name, df_highlight),
        df_nonhighlight = get_cond(nhl_by, j_name, df_nonhighlight),
        central_tendency = central_tendency,
        lw = lw,
        al = al,
        q_low = q_low,
        q_high = q_high,
        mode = mode,
        colors = colors,
        # Only show legend if it's the last subplot
        show_legend = ifelse(j_idx != length(condition), FALSE, show_legend),
        dots = dots,
        main = main,
        xlab = xlab, ylab = ylab,
        font_family = font_family,
        font_size = font_size,
        theme = subplot_theme,
        frame = frame,
        webgl = webgl
      )
    }

    theme <- plotly_theme(
      font_family = font_family,
      font_size = font_size,
      margin_t = 100 # Extra space for title
    )

    pl <- plotly::subplot(pl_list,
      nrows = nrows,
      margin = margin,
      shareX = shareX,
      shareY = shareY,
      titleY = FALSE,
      titleX = FALSE
    ) |>
      plotly::layout(
        title = list(text = main),
        yaxis = list(title = list(text = ylab)),
        font = theme$font,
        margin = theme$margin,
        legend = list(
          orientation = "h",
          x = 0.5,
          y = -0.2,
          xanchor = "center",
          yanchor = "top"
        )
      )

    if (nzchar(xlab)) {
      pl <- pl |>
        plotly::layout(
          annotations = list(
            list(
              text = xlab,
              font = list(
                family = font_family
                # size = font_size
              ),
              xref = "paper",
              yref = "paper",
              xanchor = "center", yanchor = "top",
              x = 0.5, y = -.1,
              showarrow = FALSE
            )
          )
        )
    }
  }

  # Add play button and time slider for the cumulative reveal animation.
  if (animation == "time") {
    n_frames <- length(unique(c(
      df_highlight[[".frame"]], df_nonhighlight[[".frame"]]
    )))
    pl <- add_time_animation_controls(pl,
      time_unit = if (is.null(time_unit)) "" else time_unit,
      font_family = font_family,
      font_size = font_size,
      frame_ms = resolve_frame_ms(control_options, n_frames),
      transition_ms = control_options[["transition_ms"]]
    )
  }

  apply_webfont(set_plotly_export_format(pl), font_family)
}
