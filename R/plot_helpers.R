#' Abort if a plot argument is non-NULL but of the wrong type
#'
#' Helper for validate_plot_params() that collapses the repeated
#' "check type, otherwise cli_abort" blocks into a single call.
#'
#' @param value The argument value (skipped when `NULL`).
#' @param arg Argument name, used in the error message.
#' @param type One of "logical", "character", "numeric"; the required type.
#' @param hint Optional final bullet with a usage hint.
#' @noRd
.assert_plot_type <- function(value, arg, type, hint = NULL) {
  predicate <- switch(type,
    logical = is.logical,
    character = is.character,
    numeric = is.numeric
  )
  if (!is.null(value) && !predicate(value)) {
    bullets <- c(
      "x" = "Invalid {.arg {arg}} argument.",
      "i" = "The {.arg {arg}} argument must be {.cls {type}}."
    )
    if (!is.null(hint)) bullets <- c(bullets, ">" = hint)
    cli::cli_abort(bullets)
  }
}


#' Resolve a preference vector against the available options
#'
#' Returns the first element of `pref` that is in `available` (or the literal
#' `"none"`), falling back to `"none"` when nothing matches. Used by
#' [plot.ensemble_stockflow()] to pick the central tendency and spread band from
#' user preference vectors, given which statistics are present in the summary.
#'
#' @param pref Character preference vector, in priority order.
#' @param available Character vector of options that can actually be used.
#' @returns A single character value: the first usable option, or `"none"`.
#' @noRd
resolve_summary_choice <- function(pref, available) {
  usable <- pref[pref %in% c(available, "none")]
  if (length(usable) == 0) {
    return("none")
  }
  usable[1]
}


#' Validate plot parameters
#'
#' Validate common parameters used across all plotting functions (plot.simulate_stockflow,
#' plot.stockflow, plot.ensemble_stockflow).
#'
#' @param show_legend Logical, whether to show legend.
#' @param vars Character vector of variable names to plot, or NULL.
#' @param palette Character, color palette name.
#' @param colors Character vector of colors, or NULL.
#' @param font_family Character, font family name.
#' @param font_size Numeric, font size in points.
#' @param wrap_width Integer, text wrap width for labels.
#' @param label_subplots Logical, whether to label subplots with condition names.
#' @param format_label Logical, whether to prettify name-defaulted labels.
#'
#' @returns Invisibly returns a list of validation results. Throws cli errors if validation fails.
#' @noRd
#'
validate_plot_params <- function(show_legend = NULL,
                                 vars = NULL,
                                 palette = NULL,
                                 colors = NULL,
                                 font_family = NULL,
                                 font_size = NULL,
                                 wrap_width = NULL,
                                 label_subplots = NULL,
                                 format_label = NULL,
                                 webgl = NULL) {
  .assert_plot_type(show_legend, "show_legend", "logical", "Use {.code TRUE} or {.code FALSE}.")

  .assert_plot_type(webgl, "webgl", "logical", "Use {.code TRUE} or {.code FALSE}.")

  .assert_plot_type(format_label, "format_label", "logical", "Use {.code TRUE} or {.code FALSE}.")

  if (!is.null(vars)) {
    if (!is.character(vars)) {
      cli::cli_abort(c(
        "x" = "Invalid {.arg vars} argument.",
        "i" = "Received: {.cls {typeof(vars)}}.",
        ">" = "Provide a character vector of variable names."
      ))
    }

    if (length(vars) == 0) {
      cli::cli_abort(c(
        "x" = "Empty {.arg vars} vector.",
        ">" = "Provide at least one variable name."
      ))
    }
  }

  .assert_plot_type(palette, "palette", "character", "Use {.code hcl.pals()} to see available palettes.")

  .assert_plot_type(colors, "colors", "character", "Provide a character vector of valid color names or hex codes.")

  .assert_plot_type(font_family, "font_family", "character")

  if (!is.null(font_size) && (!is.numeric(font_size) || font_size <= 0)) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg font_size} argument.",
      ">" = "The {.arg font_size} argument must be a positive number."
    ))
  }

  if (!is.null(wrap_width) && (!is.numeric(wrap_width) || wrap_width <= 0)) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg wrap_width} argument.",
      ">" = "The {.arg wrap_width} argument must be a positive integer."
    ))
  }

  .assert_plot_type(label_subplots, "label_subplots", "logical")

  invisible(TRUE)
}


#' Prepare and format variable labels
#'
#' Consolidate label formatting logic: handle duplicates, wrapping, special characters,
#' and create name-to-label dictionaries for variables.
#'
#' @param names_df Data frame with columns "name", "label", and "type".
#' @param wrap_width Integer, text wrap width.
#' @param format_label Logical, whether to apply default formatting (remove underscores/periods).
#'   Only applied if name equals label.
#'
#' @returns Data frame with prepared labels (original names_df plus modified "label" column).
#' @noRd
#'
prepare_labels <- function(names_df, wrap_width, format_label = FALSE, deduplicate = TRUE) {
  # Apply default formatting if requested (remove underscores and periods)
  if (format_label) {
    names_df[["label"]] <- format_label_if_default(
      names_df[["name"]], names_df[["label"]]
    )
  }

  # Escape single quotes for Graphviz/DiagrammeR compatibility
  names_df[["label"]] <- gsub("'", "\\\\'", names_df[["label"]])

  # Text wrap to prevent long labels from squishing plots
  names_df[["label"]] <- str_wrap_(names_df[["label"]], width = wrap_width)

  # Detect and handle duplicate labels by appending variable name in parentheses
  if (deduplicate && nrow(names_df) > 1) {
    labels <- names_df[["label"]]
    dup_indices <- which(labels %in% labels[duplicated(labels) |
      duplicated(labels, fromLast = TRUE)])

    if (length(dup_indices) > 0) {
      names_df[dup_indices, "label"] <- paste0(
        names_df[dup_indices, "label"], " (",
        names_df[dup_indices, "name"], ")"
      )
    }
  }

  return(names_df)
}


#' Escape characters that are special in Graphviz HTML-like labels
#'
#' @param x Character vector.
#' @returns Character vector with &, <, and > escaped.
#' @noRd
#'
escape_html_ <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}


#' Descriptive field name for a variable's equation, by variable type
#'
#' Shared vocabulary for the diagram equation line (make_eqn_label) and the
#' hover tooltip (node_tooltip), so the two can never drift apart.
#'
#' @param type Variable type ("stock", "flow", "aux", "constant", ...).
#' @returns Character scalar: "Initial value", "Rate", "Value", or "Equation".
#' @noRd
#'
eqn_field_ <- function(type) {
  switch(type,
    stock = "Initial value",
    flow = "Rate",
    constant = "Value",
    "Equation"
  )
}


#' Build an HTML-like node label that shows the label and its equation
#'
#' Produce a Graphviz HTML-like label body that places the variable label on one
#' line and its equation underneath, in a smaller font and a given colour. The
#' equation line is prefixed with a type-specific field name (see eqn_field_),
#' e.g. "Initial value = 100" for stocks or "Rate = a * b" for flows. The
#' returned value is the inner HTML, WITHOUT the surrounding angle brackets, so
#' callers assign it via `label=<...>` or `xlabel=<...>`.
#'
#' @param type Variable type, used to pick the equation field name.
#' @param label Character vector of variable labels (may contain "\\n" from wrapping).
#' @param eqn Character vector of equations (same length as label).
#' @param eqn_font_size Numeric, font size for the equation line.
#' @param eqn_col Character, colour of the equation text.
#' @param wrap_width Integer, text wrap width applied to the equation.
#' @param italic Logical, if TRUE wrap the label portion in <I></I> (constants).
#' @returns Character vector of HTML-like label bodies (no surrounding angle brackets).
#' @noRd
#'
make_eqn_label <- function(type, label, eqn, eqn_font_size, eqn_col, wrap_width, italic = FALSE) {
  # Labels arrive from prepare_labels() with single quotes escaped for quoted
  # Graphviz strings; undo that escaping for HTML-like labels.
  label <- gsub("\\'", "'", label, fixed = TRUE)
  label_html <- gsub("\n", "<BR/>", escape_html_(label), fixed = TRUE)
  if (italic) {
    label_html <- paste0("<I>", label_html, "</I>")
  }

  # Wrap the equation to the same width as the labels, then break and escape.
  eqn <- str_wrap_(eqn, width = wrap_width)
  eqn_html <- gsub("\n", "<BR/>", escape_html_(eqn), fixed = TRUE)

  sprintf(
    "%s<BR/><FONT POINT-SIZE=\"%s\" COLOR=\"%s\">%s = %s</FONT>",
    label_html, eqn_font_size, eqn_col, eqn_field_(type), eqn_html
  )
}


#' Compose an informative tooltip for a stock-and-flow diagram node
#'
#' Build a multi-line tooltip string describing a single variable: its type and
#' label, its name (only when it differs from the label), its equation/value,
#' and its structural role (inflows/outflows for stocks, from/to for flows).
#' Lines are joined with the literal escape "\\n", which Graphviz renders as a
#' line break in the SVG tooltip.
#'
#' @param type Variable type ("stock", "flow", "aux", or "constant").
#' @param label Human-readable (unwrapped) label.
#' @param name Variable name.
#' @param eqn Equation string (may be NA or "").
#' @param inflows,outflows Character vectors of flow labels (stocks only).
#' @param from_label,to_label Source/destination labels (flows only); NA to omit.
#' @returns A single tooltip string with "\\n"-separated lines.
#' @noRd
#'
node_tooltip <- function(type, label, name, eqn,
                         inflows = character(0), outflows = character(0),
                         from_label = NA_character_, to_label = NA_character_) {
  type_title <- switch(type,
    stock = "Stock",
    flow = "Flow",
    aux = "Auxiliary",
    constant = "Constant",
    type
  )

  lines <- sprintf("%s: %s", type_title, label)

  if (!identical(name, label)) {
    lines <- c(lines, sprintf("Name: %s", name))
  }

  if (!is.na(eqn) && nzchar(eqn)) {
    lines <- c(lines, sprintf("%s: %s", eqn_field_(type), eqn))
  }

  none <- "\u2014" # em dash
  if (type == "stock") {
    lines <- c(
      lines,
      sprintf("Inflows: %s", if (length(inflows)) paste(inflows, collapse = ", ") else none),
      sprintf("Outflows: %s", if (length(outflows)) paste(outflows, collapse = ", ") else none)
    )
  }

  if (type == "flow") {
    if (!is.na(from_label)) lines <- c(lines, sprintf("From: %s", from_label))
    if (!is.na(to_label)) lines <- c(lines, sprintf("To: %s", to_label))
  }

  paste(lines, collapse = "\\n")
}


#' Create plotly theme with consistent styling
#'
#' Generate a reusable theme configuration for plotly plots, including fonts,
#' margins, legend, and axis styling.
#'
#' @param font_family Character, font family name.
#' @param font_size Numeric, base font size in points.
#' @param margin_t Numeric, top margin in pixels.
#' @param margin_b Numeric, bottom margin in pixels.
#' @param margin_l Numeric, left margin in pixels.
#' @param margin_r Numeric, right margin in pixels.
#' @param legend_font_scale Numeric, scale factor for legend font size relative to base font_size.
#'   Defaults to 0.85.
#'
#' @returns List with plotly layout specifications.
#' @noRd
#'
plotly_theme <- function(font_family = getOption("sdbuildR.font_family", default = "stix-two-text"),
                         font_size = 16,
                         margin_t = 50,
                         margin_b = 50,
                         margin_l = 50,
                         margin_r = 50,
                         legend_font_scale = 0.85) {
  list(
    font = list(family = font_family, size = font_size),
    margin = list(t = margin_t, b = margin_b, l = margin_l, r = margin_r),
    legend = list(
      traceorder = "normal",
      font = list(size = ceiling(font_size * legend_font_scale))
    ),
    xaxis = list(font = list(size = font_size)),
    yaxis = list(font = list(size = font_size))
  )
}


#' Apply optional parameters from ... to layout
#'
#' Centralize the handling of ... arguments for optional parameters like
#' main, xlab, ylab, xlim, ylim, alpha, and sub across plot functions.
#'
#' @param dots List from \code{list(...)} in calling function.
#' @param defaults Named list of default values for optional parameters.
#'   Keys should match parameter names (e.g., "main", "xlab", "ylab").
#'
#' @returns List with finalized parameter values, using provided values from dots
#'   where available, otherwise defaults.
#' @noRd
#'
extract_plot_params <- function(dots, defaults) {
  result <- defaults

  for (name in names(defaults)) {
    if (name %in% names(dots)) {
      result[[name]] <- dots[[name]]
    }
  }

  result
}


#' Validate variable names against a model
#'
#' Check whether specified variables exist in the model and are present in
#' the data frame.
#'
#' @param vars Character vector of variable names to check, or NULL.
#' @param names_df Data frame with column "name" containing valid variable names.
#' @param df Data frame with column "variable" containing variables in data.
#' @param context Character, brief context for error message (e.g., "simulation", "diagram").
#'
#' @returns Invisibly returns TRUE if validation passes. Throws cli errors otherwise.
#' @noRd
#'
validate_vars_in_model <- function(vars, names_df, df = NULL, context = "model") {
  if (is.null(vars)) {
    return(invisible(TRUE))
  }

  # Check whether specified variables exist in names_df
  idx <- !(vars %in% names_df[["name"]])
  if (any(idx)) {
    cli::cli_abort(
      c(
        "!" = paste0(
          paste0(vars[idx], collapse = ", "),
          ifelse(sum(idx) == 1, " is not a variable", " are not variables"),
          " in the ", context, "."
        ),
        "i" = paste0(
          "Model variables: ",
          paste0(sort(names_df[["name"]]), collapse = ", ")
        )
      )
    )
  }

  # Check whether variables are in the data frame (if provided)
  if (!is.null(df) && "variable" %in% colnames(df)) {
    not_saved <- vars[!(vars %in% df[["variable"]])]
    if (length(not_saved) > 0) {
      vars_not_saved(not_saved, names_df, arg = "vars", action = "abort")
    }
  }

  invisible(TRUE)
}


#' Normalize and validate a layout-grouping argument for plot.stockflow()
#'
#' Used for the `align` and `order` arguments. Accepts either a single character
#' vector (one group) or a list of character vectors (several groups), normalizes
#' both to a list of character vectors, and validates the variable names:
#' \itemize{
#'   \item names absent from the model are treated as typos and abort;
#'   \item names that exist but are not currently drawn (hidden by `vars`,
#'     `show_constants`, or `show_aux`) are dropped with a warning;
#'   \item within-group whitespace is trimmed and duplicates removed (order kept);
#'   \item groups with fewer than `min_len` drawn members are dropped (a group of
#'     one has nothing to align or order).
#' }
#'
#' @param x The `align`/`order` argument: `NULL`, a character vector, or a list
#'   of character vectors.
#' @param plot_var Character vector of variable names actually drawn in the diagram.
#' @param model_var Character vector of all variable names in the model (typo check).
#' @param arg Argument name for messages ("align" or "order").
#' @param min_len Minimum number of drawn members for a group to be kept. Defaults to 2.
#'
#' @returns A list of character vectors, each with at least `min_len` drawn names,
#'   in the user-specified order. Returns an empty list when `x` is `NULL` or
#'   nothing survives filtering.
#' @noRd
prepare_layout_groups <- function(x, plot_var, model_var, arg, min_len = 2L) {
  if (is.null(x)) {
    return(list())
  }

  # A single character vector is one group
  if (is.character(x)) {
    x <- list(x)
  }

  if (!is.list(x) || length(x) == 0L || !all(vapply(x, is.character, logical(1)))) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg {arg}} argument.",
      ">" = "{.arg {arg}} must be a character vector or a list of character vectors."
    ))
  }

  # Trim, drop blanks, de-duplicate within each group (preserving order)
  x <- lapply(x, function(g) {
    g <- trimws(g)
    unique(g[nzchar(g)])
  })

  flat <- unique(unlist(x, use.names = FALSE))

  # Typo protection: names absent from the model abort
  unknown <- setdiff(flat, model_var)
  if (length(unknown) > 0) {
    cli::cli_abort(c(
      "!" = paste0(
        "{.arg {arg}}: ",
        paste0(unknown, collapse = ", "),
        ifelse(length(unknown) == 1, " is not a variable", " are not variables"),
        " in the model."
      ),
      "i" = paste0("Model variables: ", paste0(sort(model_var), collapse = ", "))
    ))
  }

  # Known but not currently drawn: drop with a warning
  not_drawn <- setdiff(flat, plot_var)
  if (length(not_drawn) > 0) {
    cli::cli_warn(c(
      "!" = paste0(
        "{.arg {arg}}: ",
        paste0(not_drawn, collapse = ", "),
        ifelse(length(not_drawn) == 1, " is", " are"),
        " not shown in the diagram and will be ignored."
      ),
      "i" = "Hidden by {.arg vars}, {.arg show_constants}, or {.arg show_aux}."
    ))
    x <- lapply(x, function(g) g[g %in% plot_var])
  }

  # Keep only groups with enough drawn members to matter
  x[vapply(x, function(g) length(g) >= min_len, logical(1))]
}


#' Reorder plotted variables for time-series plots
#'
#' @param names_df Data frame with a `name` column for plotted variables.
#' @param order Character vector of model variable names, or NULL.
#' @param model_var Character vector of all model variable names for typo checks.
#' @param reported Character vector of names whose absence from the plot has
#'   already been reported elsewhere (typically `vars`, since `order` defaults to
#'   `vars`). These are skipped when warning about ordered-but-not-shown
#'   variables so the same drop is not reported twice.
#' @returns `names_df`, reordered so requested variables appear first.
#' @noRd
apply_trace_order <- function(names_df, order = NULL, model_var = names_df[["name"]],
                              reported = NULL) {
  if (is.null(order)) {
    return(names_df)
  }

  if (!is.character(order)) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg order} argument.",
      ">" = "The {.arg order} argument must be a character vector of model variable names."
    ))
  }

  order <- unique(trimws(order))
  order <- order[nzchar(order)]
  if (length(order) == 0L) {
    cli::cli_abort(c(
      "x" = "Empty {.arg order} vector.",
      ">" = "Provide at least one variable name."
    ))
  }

  unknown <- setdiff(order, model_var)
  if (length(unknown) > 0L) {
    cli::cli_abort(c(
      "!" = paste0(
        "{.arg order}: ",
        paste0(unknown, collapse = ", "),
        ifelse(length(unknown) == 1L, " is not a variable", " are not variables"),
        " in the model."
      ),
      "i" = paste0("Model variables: ", paste0(sort(model_var), collapse = ", "))
    ))
  }

  plot_var <- names_df[["name"]]
  # Names already reported as dropped (e.g. by `vars` filtering) are excluded so
  # that defaulting `order = vars` does not warn twice about the same variable.
  not_plotted <- setdiff(setdiff(order, plot_var), reported)
  if (length(not_plotted) > 0L) {
    cli::cli_warn(c(
      "!" = paste0(
        "{.arg order}: ",
        paste0(not_plotted, collapse = ", "),
        ifelse(length(not_plotted) == 1L, " is", " are"),
        " not shown in the plot and will be ignored."
      ),
      "i" = "Hidden by {.arg vars}, {.arg show_constants}, or simulation output settings."
    ))
  }

  requested <- order[order %in% plot_var]
  remaining <- plot_var[!plot_var %in% requested]
  names_df[match(c(requested, remaining), plot_var), , drop = FALSE]
}


#' Generate or validate colors for variables
#'
#' Centralize color generation from palettes or validation of custom color vectors.
#'
#' @param n_vars Integer, number of variables needing colors.
#' @param colors Character vector of custom colors, or NULL.
#' @param palette Character, palette name (from hcl.pals()).
#'
#' @returns Character vector of colors (length = n_vars).
#' @noRd
#'
generate_colors <- function(n_vars, colors = NULL, palette = "Dark 2") {
  if (!is.null(colors)) {
    if (length(colors) < n_vars) {
      cli::cli_abort(c(
        "x" = "Insufficient colors provided.",
        "i" = "The {.arg colors} vector has length {.val {length(colors)}}, but {.val {n_vars}} variables need colors.",
        ">" = "Provide at least {.val {n_vars}} colors or use {.arg palette} instead."
      ))
    }
    # Normalize provided colors to canonical hex (#RRGGBB)
    norm <- vapply(colors[seq_len(n_vars)], function(col) {
      # Try col2rgb for names/hex/rgb; fall back to original string
      rgb_val <- tryCatch(grDevices::col2rgb(col), error = function(e) NULL)
      if (!is.null(rgb_val)) {
        grDevices::rgb(rgb_val[1, 1], rgb_val[2, 1], rgb_val[3, 1], maxColorValue = 255)
      } else {
        toupper(as.character(col))
      }
    }, character(1))
    return(norm)
  }

  # Ensure minimum of 3 colors for palette generation
  n_colors <- max(n_vars, 3)
  generated <- grDevices::hcl.colors(n = n_colors, palette = palette)

  # hcl.colors returns hex strings; normalize to uppercase #RRGGBB
  generated <- toupper(substr(generated, 1, 7))

  generated[seq_len(n_vars)]
}


#' Generate or validate line widths for variables
#'
#' Centralize line-width handling: recycle a single value across variables or
#' validate a custom vector, mirroring \code{\link{generate_colors}}.
#'
#' @param n_vars Integer, number of variables needing line widths.
#' @param line_width Numeric, either a single value applied to all variables or a
#'   vector with one value per variable, or NULL to use \code{default}.
#' @param default Numeric, the line width used when \code{line_width} is NULL.
#'   Defaults to 2 (plotly's default).
#' @param arg Character, the argument name used in error messages. Defaults to
#'   "line_width".
#'
#' @returns Numeric vector of line widths (length = n_vars).
#' @noRd
#'
generate_line_width <- function(n_vars, line_width = NULL, default = 2,
                                arg = "line_width") {
  if (is.null(line_width)) {
    return(rep(default, n_vars))
  }

  if (!is.numeric(line_width)) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg {arg}} argument.",
      "i" = "The {.arg {arg}} argument must be {.cls numeric}."
    ))
  }

  if (any(line_width <= 0)) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg {arg}} argument.",
      "i" = "All {.arg {arg}} values must be positive."
    ))
  }

  # A single value is recycled across all variables.
  if (length(line_width) == 1) {
    return(rep(unname(line_width), n_vars))
  }

  if (length(line_width) < n_vars) {
    cli::cli_abort(c(
      "x" = "Insufficient line widths provided.",
      "i" = "The {.arg {arg}} vector has length {.val {length(line_width)}}, but {.val {n_vars}} variables need line widths.",
      ">" = "Provide a single value, {.val {n_vars}} values, or omit {.arg {arg}}."
    ))
  }

  unname(line_width[seq_len(n_vars)])
}


#' Build a constant per-variable aesthetic vector
#'
#' @param value Single numeric value.
#' @param display_names Character vector of plotted variable labels.
#' @returns Named numeric vector (`value` repeated, named by `display_names`).
#' @noRd
aes_constant <- function(value, display_names) {
  stats::setNames(rep(value, length(display_names)), display_names)
}


#' Validate the numeric range of an aesthetic value
#'
#' @param x Numeric vector.
#' @param arg Argument name for messages.
#' @param validate One of "positive" (> 0), "nonneg" (>= 0), "unit" (0, 1).
#' @noRd
.validate_aes_range <- function(x, arg, validate) {
  if (validate == "positive" && any(x <= 0)) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg {arg}} argument.",
      ">" = "All {.arg {arg}} values must be positive."
    ))
  } else if (validate == "nonneg" && any(x < 0)) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg {arg}} argument.",
      ">" = "All {.arg {arg}} values must be non-negative."
    ))
  } else if (validate == "unit" && any(x < 0 | x > 1)) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg {arg}} argument.",
      "i" = "The {.arg {arg}} argument must be between 0 and 1."
    ))
  }
}


#' Split a role-structured aesthetic spec into a per-role list of raw specs
#'
#' Accepts the `line_width`/`alpha` grammar: a bare scalar or per-variable vector
#' (applies to every role), or a named list keyed by role (each element is itself
#' a scalar or per-variable vector). Returns a list over `roles`; a `NULL`
#' element means "use the default for that role".
#'
#' @param x The user argument (scalar, named vector, list, or NULL).
#' @param roles Character vector of valid role names.
#' @param arg Argument name for messages.
#' @returns Named list over `roles` of raw specs (or `NULL` per role).
#' @noRd
split_aes_roles <- function(x, roles, arg) {
  empty <- stats::setNames(vector("list", length(roles)), roles)
  if (is.null(x)) {
    return(empty)
  }
  if (is.list(x)) {
    if (is.null(names(x)) || any(!nzchar(names(x)))) {
      example <- sprintf("%s = list(%s = , %s = )", arg, roles[1], roles[2])
      cli::cli_abort(c(
        "x" = "Invalid {.arg {arg}} list.",
        ">" = "Name each element with a role ({.val {roles}}), e.g. {.code {example}}."
      ))
    }
    bad <- setdiff(names(x), roles)
    if (length(bad) > 0) {
      cli::cli_abort(c(
        "x" = "Unknown {.arg {arg}} role{?s}: {.val {bad}}.",
        "i" = "Valid roles are {.val {roles}}."
      ))
    }
    out <- empty
    for (r in names(x)) out[[r]] <- x[[r]]
    return(out)
  }
  # A bare scalar/vector applies to every role.
  stats::setNames(rep(list(x), length(roles)), roles)
}


#' Expand a raw aesthetic spec to a full per-variable named vector
#'
#' Handles the three leaf forms: a scalar (recycled across variables), a named
#' vector keyed by variable name (partial; unspecified variables fall back to
#' `default`, unknown names warn), or an unnamed positional vector (must have at
#' least one value per variable). Validates type and range first.
#'
#' @param raw Leaf spec (scalar, named vector, unnamed vector, or NULL).
#' @param var_names Character vector of variable names (defines length/order).
#' @param default Single numeric used for `NULL` and for unspecified labels.
#' @param arg Argument name for messages.
#' @param validate One of "positive", "nonneg", "unit".
#' @param display_names Character vector used to name the returned vector.
#' @param valid_names Character vector of valid model variable names for warning
#'   unknown names; defaults to `var_names`.
#' @returns Numeric vector named by `display_names`.
#' @noRd
expand_aes <- function(raw, var_names, default, arg, validate = "positive",
                       display_names = var_names, valid_names = var_names) {
  n <- length(var_names)
  if (is.null(raw)) raw <- default

  if (!is.numeric(raw)) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg {arg}} argument.",
      "i" = "The {.arg {arg}} argument must be {.cls numeric}."
    ))
  }
  .validate_aes_range(raw, arg, validate)

  # Scalar: recycle across all variables.
  if (length(raw) == 1 && is.null(names(raw))) {
    return(stats::setNames(rep(unname(raw), n), display_names))
  }

  # Named by variable name: partial allowed, unspecified -> default.
  if (!is.null(names(raw))) {
    out <- stats::setNames(rep(default, n), var_names)
    hit <- intersect(names(raw), var_names)
    out[hit] <- raw[hit]
    unknown <- setdiff(names(raw), valid_names)
    if (length(unknown) > 0) {
      cli::cli_warn(c(
        "!" = "Ignoring {.arg {arg}} name{?s} not matching a model variable: {.val {unknown}}.",
        "i" = "Plotted variables: {.val {var_names}}."
      ))
    }
    return(stats::setNames(unname(out), display_names))
  }

  # Unnamed positional vector: one value per variable, in plot order.
  if (length(raw) < n) {
    cli::cli_abort(c(
      "x" = "Insufficient {.arg {arg}} values provided.",
      "i" = "The {.arg {arg}} vector has length {.val {length(raw)}}, but {.val {n}} variables need values.",
      ">" = "Provide a single value, {.val {n}} values, a named vector, or omit {.arg {arg}}."
    ))
  }
  stats::setNames(unname(raw[seq_len(n)]), display_names)
}


#' Resolve a role-structured aesthetic to per-role, per-variable vectors
#'
#' Combines split_aes_roles() and expand_aes() so a `line_width`/`alpha`
#' argument becomes a named list over `roles`, each a numeric vector keyed by
#' plotted variable label.
#'
#' @param x The user argument (scalar, named vector, list, or NULL).
#' @param roles Character vector of role names.
#' @param defaults Named list of per-role default scalars.
#' @param var_names Character vector of variable names.
#' @param arg Argument name for messages.
#' @param validate_by_role Named list mapping each role to a `.validate_aes_range`
#'   mode ("positive"/"nonneg"/"unit").
#' @param display_names Character vector used to name the returned vectors.
#' @param valid_names Character vector of valid model variable names for warning
#'   unknown names; defaults to `var_names`.
#' @returns Named list over `roles` of per-label numeric vectors.
#' @noRd
resolve_aes <- function(x, roles, defaults, var_names, arg, validate_by_role,
                        display_names = var_names, valid_names = var_names) {
  raw <- split_aes_roles(x, roles, arg)
  stats::setNames(
    lapply(roles, function(r) {
      expand_aes(raw[[r]], var_names, defaults[[r]], arg,
        validate_by_role[[r]],
        display_names = display_names,
        valid_names = valid_names
      )
    }),
    roles
  )
}


#' Validate Plotly line dash strings
#'
#' @param x Character vector of dash strings.
#' @param arg Argument name for messages.
#' @noRd
validate_plotly_dash <- function(x, arg = "line_type") {
  fixed <- c("solid", "dot", "dash", "longdash", "dashdot", "longdashdot")
  dash_length <- "^\\s*\\d+(?:\\.\\d+)?px\\s*(?:,\\s*\\d+(?:\\.\\d+)?px\\s*)+$"
  ok <- x %in% fixed | grepl(dash_length, x, perl = TRUE)
  if (any(!ok)) {
    bad <- unique(x[!ok])
    cli::cli_abort(c(
      "x" = "Invalid {.arg {arg}} value{?s}: {.val {bad}}.",
      "i" = "Use a Plotly dash string such as {.val solid}, {.val dash}, {.val dot}, {.val dashdot}, or a pixel pattern like {.val 5px,10px,2px,2px}."
    ))
  }
  invisible(TRUE)
}


#' Expand a character aesthetic to a full per-variable named vector
#'
#' Mirrors expand_aes() for non-numeric aesthetics such as Plotly line dashes.
#'
#' @param raw Leaf spec (scalar, named vector, unnamed vector, or NULL).
#' @param var_names Character vector of variable names (defines length/order).
#' @param default Single character used for `NULL` and unspecified variables.
#' @param arg Argument name for messages.
#' @param display_names Character vector used to name the returned vector.
#' @param valid_names Character vector of valid model variable names for warning
#'   unknown names; defaults to `var_names`.
#' @param validator Optional function called on the resulting values.
#' @returns Character vector named by `display_names`.
#' @noRd
expand_character_aes <- function(raw, var_names, default, arg,
                                 display_names = var_names,
                                 valid_names = var_names,
                                 validator = NULL) {
  n <- length(var_names)
  if (is.null(raw)) raw <- default

  if (!is.character(raw) || length(raw) == 0L) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg {arg}} argument.",
      "i" = "The {.arg {arg}} argument must be {.cls character}."
    ))
  }

  if (length(raw) == 1L && is.null(names(raw))) {
    out <- stats::setNames(rep(unname(raw), n), display_names)
    if (!is.null(validator)) validator(out, arg = arg)
    return(out)
  }

  if (!is.null(names(raw))) {
    out <- stats::setNames(rep(default, n), var_names)
    hit <- intersect(names(raw), var_names)
    out[hit] <- raw[hit]
    unknown <- setdiff(names(raw), valid_names)
    if (length(unknown) > 0) {
      cli::cli_warn(c(
        "!" = "Ignoring {.arg {arg}} name{?s} not matching a model variable: {.val {unknown}}.",
        "i" = "Plotted variables: {.val {var_names}}."
      ))
    }
    out <- stats::setNames(unname(out), display_names)
    if (!is.null(validator)) validator(out, arg = arg)
    return(out)
  }

  if (length(raw) < n) {
    cli::cli_abort(c(
      "x" = "Insufficient {.arg {arg}} values provided.",
      "i" = "The {.arg {arg}} vector has length {.val {length(raw)}}, but {.val {n}} variables need values.",
      ">" = "Provide a single value, {.val {n}} values, a named vector, or omit {.arg {arg}}."
    ))
  }
  out <- stats::setNames(unname(raw[seq_len(n)]), display_names)
  if (!is.null(validator)) validator(out, arg = arg)
  out
}


#' Resolve line types for simulation traces
#'
#' @param line_type User line_type argument.
#' @param var_names Model variable names in plot order.
#' @param var_types Variable types matching `var_names`.
#' @param display_names Display labels matching `var_names`.
#' @param valid_names Valid model variable names for warnings.
#' @returns Character vector of Plotly dash strings named by display label.
#' @noRd
resolve_line_type <- function(line_type, var_names, var_types,
                              display_names = var_names,
                              valid_names = var_names) {
  roles <- c("stock", "flow", "aux", "constant", "lookup")
  defaults <- c(
    stock = "solid", flow = "solid", aux = "solid",
    constant = "dash", lookup = "dash"
  )

  if (is.list(line_type)) {
    raw <- split_aes_roles(line_type, roles, "line_type")
    by_role <- stats::setNames(vector("list", length(roles)), roles)
    for (role in roles) {
      by_role[[role]] <- expand_character_aes(raw[[role]], var_names,
        default = defaults[[role]], arg = "line_type",
        display_names = display_names, valid_names = valid_names,
        validator = validate_plotly_dash
      )
    }
    out <- vapply(seq_along(var_names), function(i) {
      role <- var_types[[i]]
      if (!role %in% roles) role <- "aux"
      unname(by_role[[role]][[display_names[[i]]]])
    }, character(1))
    out <- stats::setNames(out, display_names)
    validate_plotly_dash(out, arg = "line_type")
    return(out)
  }

  expand_character_aes(line_type, var_names,
    default = "solid", arg = "line_type",
    display_names = display_names, valid_names = valid_names,
    validator = validate_plotly_dash
  )
}


#' Resolve colours into a per-label vector, allowing partial named overrides
#'
#' Mirrors the `line_width`/`alpha` grammar for `colors`: a named vector sets the
#' colours of the named variables and the palette fills the rest (`colors` no
#' longer has to name every variable). An unnamed vector (or `NULL`) falls back to
#' the palette. In every case the result is named by the display label, so
#' add_trace_pair() maps colours to traces by label rather than by order.
#'
#' @param colors Character vector of colours (named, unnamed) or NULL.
#' @param palette Palette name passed to generate_colors().
#' @param var_names Character vector of variable names (defines length/order).
#' @param display_names Character vector of plotted variable labels.
#' @param valid_names Character vector of valid model variable names for warning
#'   unknown names; defaults to `var_names`.
#' @returns Character vector of colours named by `display_names`.
#' @noRd
resolve_colors <- function(colors, palette, var_names, display_names = var_names,
                           valid_names = var_names) {
  n <- length(var_names)

  if (!is.null(colors) && !is.null(names(colors))) {
    # Partial named vector: palette fills unspecified variables, named override.
    out <- stats::setNames(generate_colors(n, colors = NULL, palette = palette), var_names)
    hit <- intersect(names(colors), var_names)
    if (length(hit) > 0) {
      # generate_colors() normalises names/hex to canonical #RRGGBB.
      out[hit] <- generate_colors(length(hit), colors = unname(colors[hit]))
    }
    unknown <- setdiff(names(colors), valid_names)
    if (length(unknown) > 0) {
      cli::cli_warn(c(
        "!" = "Ignoring {.arg colors} name{?s} not matching a model variable: {.val {unknown}}.",
        "i" = "Plotted variables: {.val {var_names}}."
      ))
    }
    return(stats::setNames(unname(out), display_names))
  }

  stats::setNames(generate_colors(n, colors = colors, palette = palette), display_names)
}


#' Resolve diagram colours into a per-variable vector by type or name
#'
#' Mirrors the `colors` grammar of the timeseries plot methods for
#' plot.stockflow(), with variable types playing the role of layers: a single
#' colour applies to every variable; a named list keyed by type
#' (`stock`/`flow`/`constant`/`aux`), each entry a single colour, overrides the
#' defaults of those types and keeps the defaults for the rest (as with
#' utils::modifyList()); a named vector keyed by variable name recolours only
#' those variables and leaves the rest at their type default. Since every
#' variable has exactly one type, per-variable colours inside a type entry are
#' redundant and not supported. All colours are normalised to canonical hex.
#'
#' @param colors The user argument (single colour, named vector, named list, or NULL).
#' @param names_df Data frame with "name" and "type" columns of the plotted variables.
#' @param valid_names Character vector of all model variable names, used to
#'   warn about unknown names; defaults to `names_df[["name"]]`.
#' @param arg Argument name for messages.
#' @returns Character vector of colours named by variable name (in `names_df` order).
#' @noRd
resolve_diagram_colors <- function(colors, names_df,
                                   valid_names = names_df[["name"]],
                                   arg = "colors") {
  roles <- c("stock", "flow", "constant", "aux")
  type_colors <- c(
    stock = "#83d3d4", flow = "#f48153",
    constant = "grey90", aux = "grey90"
  )
  var_overrides <- NULL

  if (is.list(colors)) {
    # Role handling (name validation, unknown roles) as for line_width/alpha.
    raw <- split_aes_roles(colors, roles, arg)
    for (r in roles) {
      leaf <- raw[[r]]
      if (is.null(leaf)) next
      if (!is.character(leaf) || length(leaf) != 1 || !is.null(names(leaf))) {
        cli::cli_abort(c(
          "x" = "Invalid {.arg {arg}} entry for {.val {r}}.",
          "i" = "Each variable type takes a single colour.",
          ">" = "To recolour individual variables, use a named vector instead, e.g. {.code {arg} = c(susceptible = \"red\")}."
        ))
      }
      type_colors[[r]] <- leaf
    }
  } else if (!is.null(colors)) {
    if (!is.character(colors) || length(colors) == 0) {
      cli::cli_abort(c(
        "x" = "Invalid {.arg {arg}} argument.",
        ">" = "Colours must be given as character strings (colour names or hex codes)."
      ))
    }
    if (is.null(names(colors))) {
      # A diagram has no plot order, so unnamed vectors longer than one are ambiguous.
      if (length(colors) != 1) {
        cli::cli_abort(c(
          "x" = "Invalid {.arg {arg}} argument.",
          ">" = "Provide a single colour, a named vector (names are variable names), or a list keyed by variable type ({.val {roles}})."
        ))
      }
      type_colors[roles] <- colors
    } else {
      var_overrides <- colors
      unknown <- setdiff(names(colors), valid_names)
      if (length(unknown) > 0) {
        # A type name inside a named vector suggests the user meant the list form.
        hint <- if (any(unknown %in% roles)) {
          c(">" = "To colour variable types, use a list: {.code {arg} = list(stock = , flow = , constant = , aux = )}.")
        }
        cli::cli_warn(c(
          "!" = "Ignoring {.arg {arg}} name{?s} not matching a model variable: {.val {unknown}}.",
          "i" = "Model variables: {.val {valid_names}}.",
          hint
        ))
      }
    }
  }

  # generate_colors() normalises names/hex to canonical #RRGGBB.
  type_colors[] <- vapply(type_colors, function(col) {
    generate_colors(1, colors = col)
  }, character(1))

  out <- stats::setNames(unname(type_colors[names_df[["type"]]]), names_df[["name"]])
  hit <- intersect(names(var_overrides), names(out))
  if (length(hit) > 0) {
    out[hit] <- generate_colors(length(hit), colors = unname(var_overrides[hit]))
  }
  out
}


#' Filter variables in simulation data and metadata
#'
#' Keep only specified variables in both the names_df and simulation data frame.
#' Validates that all requested variables exist.
#'
#' @param vars Character vector of variable names to keep.
#' @param names_df Data frame with "name" and "type" columns.
#' @param df Data frame with "variable" column (simulation data).
#'
#' @returns List with elements $names_df and $df, filtered to only include vars.
#' @noRd
#'
filter_variables <- function(vars, names_df, df) {
  # Check whether specified variables are in the model
  validate_vars_in_model(vars, names_df, NULL, context = "model")

  vars_in_df <- vars[vars %in% df[["variable"]]]
  vars_missing_df <- setdiff(vars, vars_in_df)

  if (length(vars_in_df) == 0) {
    # None of the requested variables were saved: abort with guidance
    vars_not_saved(vars_missing_df, names_df, arg = "vars", action = "abort")
  }

  if (length(vars_missing_df) > 0) {
    # Some were saved: warn about the rest and continue with what is available
    vars_not_saved(vars_missing_df, names_df, arg = "vars", action = "warn")
  }

  # Filter both dataframes to include only specified variables
  names_df <- names_df[match(vars_in_df, names_df[["name"]]), , drop = FALSE]
  df <- df[df[["variable"]] %in% vars_in_df, , drop = FALSE]

  list(names_df = names_df, df = df)
}


#' Prepare constants for plotting
#'
#' Add constant values to the simulation/ensemble data frame, formatting them
#' as long-format rows for each time point.
#'
#' @param df Data frame with "variable" and "time" columns (simulation data).
#' @param constants Named list (for sim) or data frame (for ensemble) of constants.
#' @param names_df Data frame with variable metadata.
#' @param type_sim Character, either "sim", "ensemble", or "verify".
#'
#' @returns List with elements $df (updated data frame) and $names_df (updated metadata,
#'   with non-function constants removed).
#' @noRd
#'
prep_constants <- function(df, constants, names_df, type_sim = "sim") {
  # Find time vector from first variable
  times <- df[df[["variable"]] == df[["variable"]][1], "time"]

  if (type_sim == "sim") {
    # Ensure functions are not added
    idx_func <- vapply(constants, is.function, logical(1), USE.NAMES = FALSE)
    constants <- constants[!idx_func]

    # Remove functions from names_df
    names_df <- names_df[!names_df[["name"]] %in% names(idx_func[idx_func]), ,
      drop = FALSE
    ]

    # Duplicate long format for each constant
    if (length(constants) > 0) {
      temp <- lapply(names(constants), function(y) {
        data.frame(
          time = times,
          variable = y,
          value = constants[[y]]
        )
      }) |>
        do.call(rbind, args = _) |>
        as.data.frame()
      df <- bind_rows_(df, temp)
      rm(temp)
    }
  } else if (type_sim %in% c("ensemble", "verify")) {
    # Constants is a data frame
    if (nrow(constants) > 0) {
      # df <- bind_rows_(df, constants)
      constant_names <- constants[["variable"]]
      n_times <- length(times)

      constants_repeated <- constants[rep(seq_len(nrow(constants)), times = n_times), ]
      row.names(constants_repeated) <- NULL
      rep_times <- rep(times, each = length(constant_names))

      df <- bind_rows_(df, cbind(data.frame(time = rep_times), constants_repeated))
    }
  }

  list(df = df, names_df = names_df)
}


#' Determine which variables should be highlighted in plots
#'
#' Identify "highlight" variables (default: stocks) vs "nonhighlight" variables
#' (flows, auxiliaries). Used to determine initial visibility in interactive plots.
#'
#' @param names_df Data frame with "name" and "type" columns.
#' @param highlight_strategy Character. One of:
#'   - "auto": Highlight stocks (default behavior)
#'   - "all": Highlight all variables
#'   - "none": Highlight no variables
#'   - A character vector of variable names to highlight explicitly
#'
#' @returns Character vector of names to highlight.
#' @noRd
#'
determine_highlight_vars <- function(names_df, highlight_strategy = "auto") {
  if (is.character(highlight_strategy) && length(highlight_strategy) > 1) {
    # Custom variable list provided
    return(highlight_strategy)
  }

  highlight_strategy <- tolower(highlight_strategy)
  if (highlight_strategy == "auto") {
    return(names_df[names_df[["type"]] == "stock", "name"])
  } else if (highlight_strategy == "all") {
    return(names_df[["name"]])
  } else if (highlight_strategy == "none") {
    return(character(0))
  } else {
    cli::cli_abort(c(
      "Invalid {.arg highlight_strategy}.",
      "x" = "Must be 'auto', 'all', 'none', or a character vector of variable names."
    ))
  }
}


#' Add a nonhighlight/highlight trace pair differing only in data and visibility
#'
#' The ensemble plot draws each layer (confidence ribbons, central-tendency lines,
#' error-bar markers, raw simulations) twice: once for nonhighlight variables
#' (\code{visible = "legendonly"}) and once for highlight variables
#' (\code{visible = TRUE}). The two calls are otherwise identical. This helper runs
#' both guarded calls in that order, forwarding the shared trace arguments via
#' \code{...}. Formulas in \code{...} (e.g. \code{x = ~time}) are evaluated by
#' plotly against \code{data} in the caller's environment, as before.
#'
#' @param pl Plotly object.
#' @param add_fn Trace-adding function, e.g. \code{plotly::add_trace} or
#'   \code{plotly::add_ribbons}.
#' @param data_nonhighlight,data_highlight Data frames for each trace.
#' @param plot_nonhighlight,plot_highlight Logical guards; the trace is skipped when
#'   \code{FALSE}.
#' @param ... Shared arguments forwarded to \code{add_fn}.
#'
#' @returns Updated plotly object.
#' @noRd
#'
add_visibility_pair <- function(pl, add_fn,
                                data_nonhighlight, data_highlight,
                                plot_nonhighlight, plot_highlight, ...) {
  if (plot_nonhighlight) {
    pl <- add_fn(pl, data = data_nonhighlight, ..., visible = "legendonly")
  }
  if (plot_highlight) {
    pl <- add_fn(pl, data = data_highlight, ..., visible = TRUE)
  }
  pl
}


#' Add trace pair to plotly object
#'
#' Consolidate the pattern of adding highlight and nonhighlight traces to a plotly
#' plot. Both traces use the same aesthetic mappings but with different visibility.
#'
#' @param pl Plotly object to add traces to.
#' @param df_highlight Data frame with highlight variables.
#' @param df_nonhighlight Data frame with nonhighlight variables.
#' @param colors Character vector of colors for variables.
#' @param x_col Character, name of x-axis column (e.g., "time").
#' @param show_legend Logical, whether to show legend.
#' @param mode Character, "lines", "markers", or "lines+markers".
#' @param type Character, trace type (default "scatter").
#' @param opacity Numeric, opacity/transparency (0-1).
#' @param line_width Numeric line width for "lines" mode. Either a single value
#'   applied to all variables, or a named vector (keyed by plotted label) giving
#'   one width per variable. \code{NULL} uses plotly's default.
#' @param marker_size Numeric, marker size for "markers" mode.
#' @param split Optional formula for splitting traces (e.g., \code{~interaction(variable, i)}).
#' @param frame Optional formula (e.g. \code{~.frame}) mapping traces to animation
#'   frames. When \code{NULL} (default), no animation frame is added.
#'
#' @returns Updated plotly object.
#' @noRd
#'
add_trace_pair <- function(pl,
                           df_highlight = NULL,
                           df_nonhighlight = NULL,
                           colors = NULL,
                           x_col = "time",
                           y_col = "value",
                           show_legend = TRUE,
                           mode = "lines",
                           type = "scatter",
                           opacity = 1,
                           line_width = NULL,
                           line_type = NULL,
                           fill = NULL,
                           fillcolor = NULL,
                           marker_size = NULL,
                           split = NULL,
                           frame = NULL,
                           trace_order = NULL,
                           visible_highlight = TRUE,
                           visible_nonhighlight = "legendonly") {
  # Build an explicit variable->color mapping for variables actually present in
  # this trace pair. This avoids Plotly domain warnings when factor levels or
  # wrapped labels differ across traces.
  vars_nonhighlight <- if (!is.null(df_nonhighlight) && nrow(df_nonhighlight) > 0) {
    as.character(df_nonhighlight[["variable"]])
  } else {
    character(0)
  }
  vars_highlight <- if (!is.null(df_highlight) && nrow(df_highlight) > 0) {
    as.character(df_highlight[["variable"]])
  } else {
    character(0)
  }

  vars_present <- unique(c(vars_nonhighlight, vars_highlight))
  vars_present <- vars_present[!is.na(vars_present) & nzchar(vars_present)]

  if (!is.null(colors) && length(vars_present) > 0) {
    if (length(colors) < length(vars_present)) {
      cli::cli_abort(c(
        "x" = "Insufficient colors provided for traces.",
        "i" = "Need {.val {length(vars_present)}} colors for plotted variables but got {.val {length(colors)}}."
      ))
    }
    # resolve_colors() always names colours by display label and labels are
    # deduplicated, so map by name. Positional assignment is only a fallback for
    # the (now unreachable) case where the names don't cover the present
    # variables -- mapping positionally otherwise scrambles colours, because the
    # colour vector is built highlight-first while vars_present is nonhighlight-first.
    if (!is.null(names(colors)) && all(vars_present %in% names(colors))) {
      colors <- colors[vars_present]
    } else {
      colors <- unname(colors[seq_along(vars_present)])
      names(colors) <- vars_present
    }
  }

  # Resolve per-variable line widths for the plotted labels present in this
  # trace pair. User-supplied names are matched against model variable names
  # before add_trace_pair() is called.
  lw <- NULL
  if (!is.null(line_width) && length(vars_present) > 0) {
    if (length(line_width) == 1 && is.null(names(line_width))) {
      lw <- stats::setNames(rep(unname(line_width), length(vars_present)), vars_present)
    } else {
      lw <- line_width[vars_present]
    }
  }
  lt <- NULL
  if (!is.null(line_type) && length(vars_present) > 0) {
    if (length(line_type) == 1 && is.null(names(line_type))) {
      lt <- stats::setNames(rep(unname(line_type), length(vars_present)), vars_present)
    } else {
      lt <- line_type[vars_present]
    }
  }
  fl <- NULL
  if (!is.null(fill) && length(vars_present) > 0) {
    fl <- fill[vars_present]
  }
  fc <- NULL
  if (!is.null(fillcolor) && length(vars_present) > 0) {
    fc <- fillcolor[vars_present]
  }

  # When all per-line styles are identical and no per-variable fill is needed, a
  # single color-mapped trace suffices; otherwise each variable needs its own
  # trace so it can carry its own width, dash, and fill attributes.
  uniform_width <- is.null(lw) || length(unique(unname(lw))) == 1L
  uniform_dash <- is.null(lt) || length(unique(unname(lt))) == 1L
  needs_variable_fill <- !is.null(fl) && any(!is.na(fl) & nzchar(fl))
  uniform_style <- uniform_width && uniform_dash && !needs_variable_fill

  line_for <- function(variable = NULL) {
    out <- list()
    if (is.null(variable)) {
      if (!is.null(lw)) out[["width"]] <- unname(lw)[1]
      if (!is.null(lt)) out[["dash"]] <- unname(lt)[1]
    } else {
      if (!is.null(lw)) out[["width"]] <- unname(lw[variable])
      if (!is.null(colors)) out[["color"]] <- unname(colors[variable])
      if (!is.null(lt)) out[["dash"]] <- unname(lt[variable])
    }
    if (length(out) == 0L) NULL else out
  }

  # Build the trace(s) for one data frame (highlight or nonhighlight), injecting
  # split/frame only when supplied so behaviour is identical to before when
  # neither is requested.
  add_one <- function(pl, data, showlegend_val, visible_val) {
    if (uniform_style) {
      args <- list(
        pl,
        data = data,
        x = ~ get(x_col),
        y = ~ get(y_col),
        color = ~variable,
        legendgroup = ~variable,
        type = type,
        mode = mode,
        opacity = opacity,
        colors = colors,
        showlegend = showlegend_val,
        visible = visible_val
      )
      line <- line_for()
      if (!is.null(line)) args[["line"]] <- line
      if (!is.null(split)) args[["split"]] <- split
      if (!is.null(frame)) args[["frame"]] <- frame
      return(do.call(plotly::add_trace, args))
    }

    # Per-variable styles: add one trace per variable, setting line colour,
    # width, dash, and fill explicitly (plotly cannot map these across colour
    # levels in one trace).
    for (v in levels(droplevels(data[["variable"]]))) {
      dv <- data[data[["variable"]] == v, , drop = FALSE]
      if (nrow(dv) == 0) next
      args <- list(
        pl,
        data = dv,
        x = ~ get(x_col),
        y = ~ get(y_col),
        name = v,
        legendgroup = v,
        type = type,
        mode = mode,
        opacity = opacity,
        showlegend = showlegend_val,
        visible = visible_val
      )
      line <- line_for(v)
      if (!is.null(line)) args[["line"]] <- line
      if (!is.null(fl) && !is.na(fl[v]) && nzchar(fl[v])) args[["fill"]] <- unname(fl[v])
      if (!is.null(fc) && !is.na(fc[v]) && nzchar(fc[v])) args[["fillcolor"]] <- unname(fc[v])
      if (!is.null(split)) args[["split"]] <- split
      if (!is.null(frame)) args[["frame"]] <- frame
      pl <- do.call(plotly::add_trace, args)
    }
    pl
  }

  if (!is.null(trace_order)) {
    ordered_vars <- trace_order[trace_order %in% vars_present]
    add_ordered_one <- function(pl, variable, data, visible_val) {
      dv <- data[as.character(data[["variable"]]) == variable, , drop = FALSE]
      if (nrow(dv) == 0) {
        return(pl)
      }
      args <- list(
        pl,
        data = dv,
        x = ~ get(x_col),
        y = ~ get(y_col),
        name = variable,
        legendgroup = variable,
        type = type,
        mode = mode,
        opacity = opacity,
        showlegend = if (is.null(split)) show_legend else FALSE,
        visible = visible_val
      )
      line <- line_for(variable)
      if (!is.null(line)) args[["line"]] <- line
      if (!is.null(fl) && !is.na(fl[variable]) && nzchar(fl[variable])) args[["fill"]] <- unname(fl[variable])
      if (!is.null(fc) && !is.na(fc[variable]) && nzchar(fc[variable])) args[["fillcolor"]] <- unname(fc[variable])
      if (!is.null(split)) args[["split"]] <- split
      if (!is.null(frame)) args[["frame"]] <- frame
      do.call(plotly::add_trace, args)
    }

    for (v in ordered_vars) {
      if (v %in% vars_highlight) {
        pl <- add_ordered_one(pl, v, df_highlight, visible_highlight)
      } else if (v %in% vars_nonhighlight) {
        pl <- add_ordered_one(pl, v, df_nonhighlight, visible_nonhighlight)
      }
    }
    return(pl)
  }

  # Add nonhighlight traces first (will be hidden behind highlight traces)
  if (!is.null(df_nonhighlight) && nrow(df_nonhighlight) > 0) {
    # When splitting, individual trajectories must not each add a legend entry.
    pl <- add_one(pl, df_nonhighlight,
      showlegend_val = if (is.null(split)) show_legend else FALSE,
      visible_val = visible_nonhighlight
    )
  }

  # Add highlight traces (will be visible by default)
  if (!is.null(df_highlight) && nrow(df_highlight) > 0) {
    pl <- add_one(pl, df_highlight,
      showlegend_val = if (is.null(split)) show_legend else FALSE,
      visible_val = visible_highlight
    )
  }

  pl
}


#' Make a translucent fill color from a line color
#'
#' @param col Colour string.
#' @param alpha Alpha value for the returned colour.
#' @returns Colour string with alpha applied.
#' @noRd
plotly_translucent_color <- function(col, alpha = 0.25) {
  if (is.null(col) || is.na(col) || !nzchar(as.character(col)[1L])) {
    col <- "#1f77b4"
  }
  col <- as.character(col)[1L]
  if (grepl("^rgba?\\(", col)) {
    nums <- as.numeric(strsplit(gsub("rgba?\\(|\\)", "", col), ",")[[1]])
    if (length(nums) >= 3L) {
      if (max(nums[1:3], na.rm = TRUE) <= 1) nums[1:3] <- round(nums[1:3] * 255)
      return(sprintf("rgba(%d,%d,%d,%s)", nums[1], nums[2], nums[3], alpha))
    }
  }
  grDevices::adjustcolor(col, alpha.f = alpha)
}


#' Combine animated subplots with linked frames
#'
#' @param plots List of static plotly objects used to compose the subplot.
#' @param frame_plots Optional list of animated plotly objects used as the frame
#'   source. Defaults to `plots`.
#' @param nrows Number of subplot rows.
#' @param heights Optional relative row heights.
#' @param widths Optional relative column widths.
#' @param margin Subplot margin.
#' @param shareX Whether to share x axes.
#' @param titleY Whether subplot should preserve y-axis titles.
#' @returns Plotly object with frames reconstructed across subplots.
#' @noRd
subplot_linked_animation <- function(plots, frame_plots = plots,
                                     nrows = length(plots), heights = NULL,
                                     widths = NULL,
                                     margin = 0.06, shareX = TRUE,
                                     titleY = TRUE) {
  builds <- lapply(frame_plots, plotly::plotly_build)
  frame_names <- lapply(builds, function(b) {
    vapply(b$x$frames, function(f) as.character(f$name), character(1))
  })
  if (length(frame_names[[1]]) == 0L) {
    cli::cli_abort(c("x" = "Animated subplot has no animation frames."))
  }
  same <- vapply(frame_names[-1], identical, logical(1), frame_names[[1]])
  if (!all(same)) {
    cli::cli_abort(c("x" = "All animated panels must have identical animation frames."))
  }

  args <- c(plots, list(
    nrows = nrows, heights = heights, widths = widths,
    margin = margin, shareX = shareX, titleY = titleY
  ))
  sp <- do.call(plotly::subplot, args)
  sp <- plotly::plotly_build(sp)

  n_traces <- vapply(builds, function(b) length(b$x$data), integer(1))
  offsets <- cumsum(c(0L, utils::head(n_traces, -1L)))

  sp$x$frames <- lapply(seq_along(frame_names[[1]]), function(i) {
    frame <- list(name = frame_names[[1]][i], data = list(), traces = integer(0))
    for (k in seq_along(builds)) {
      built_frame <- builds[[k]]$x$frames[[i]]
      trace_idx <- unlist(built_frame$traces)
      for (j in seq_along(trace_idx)) {
        new_idx <- offsets[[k]] + trace_idx[[j]]
        trace <- built_frame$data[[j]]
        base <- sp$x$data[[new_idx + 1L]]
        trace$xaxis <- base$xaxis
        trace$yaxis <- base$yaxis
        frame$data[[length(frame$data) + 1L]] <- trace
        frame$traces <- c(frame$traces, new_idx)
      }
    }
    frame
  })

  sp
}


#' Set the export format for Plotly's "Download plot as a png/svg/jpeg/webp" button
#'
#' @param pl Plotly object to configure.
#' @param format Character, one of "png", "svg", "jpeg", or "webp". Defaults to "svg" for better quality and scalability.
#' @returns Updated Plotly object with configured export format.
#' @noRd
set_plotly_export_format <- function(pl, format = "svg") {
  # Check format
  format <- match.arg(format, choices = c("png", "svg", "jpeg", "webp"))

  plotly::config(pl,
    toImageButtonOptions = list(
      format = format
      # Use currently-rendered size by not specifying width/height
      # width = width,
      # height = height
    )
  )
}


#' Validate the animation argument
#'
#' @param animation Character, one of "none" or "time".
#' @returns The matched value.
#' @noRd
.clean_animation <- function(animation) {
  choices <- c("none", "time")
  if (length(animation) > 1) animation <- animation[1]
  if (!is.character(animation) || length(animation) != 1 || !animation %in% choices) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg animation} value.",
      "i" = "The {.arg animation} argument must be {.code 'none'} or {.code 'time'}."
    ))
  }
  animation
}


#' Validate the condition_display argument
#'
#' @param condition_display Character, one of "subplots", "slider", or "dropdown".
#' @returns The matched value.
#' @noRd
.clean_condition_display <- function(condition_display) {
  choices <- c("subplots", "slider", "dropdown")
  if (length(condition_display) > 1) condition_display <- condition_display[1]
  if (!is.character(condition_display) || length(condition_display) != 1 ||
    !condition_display %in% choices) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg condition_display} value.",
      "i" = "The {.arg condition_display} argument must be {.code 'subplots'}, {.code 'slider'}, or {.code 'dropdown'}."
    ))
  }
  condition_display
}


#' Fall back to subplots when a condition control has nothing to select
#'
#' A condition slider/dropdown steps through conditions, so it needs at least
#' two. With a single condition -- e.g. no conditions were varied in
#' `ensemble()`, or filtering left only one -- inform the user and revert to
#' the plain (single-panel) subplot display.
#'
#' @param condition_display Cleaned `condition_display` value.
#' @param n_conditions Number of conditions the control would step through.
#' @returns The (possibly reverted) `condition_display` value.
#' @noRd
.revert_condition_display <- function(condition_display, n_conditions) {
  if (condition_display %in% c("slider", "dropdown") && n_conditions <= 1) {
    cli::cli_inform(c(
      "i" = "{.code condition_display = \"{condition_display}\"} requires multiple conditions, but only one is available.",
      ">" = "Using {.code condition_display = \"subplots\"} instead."
    ))
    return("subplots")
  }
  condition_display
}


#' Accumulate rows cumulatively by time for a line-drawing animation
#'
#' Duplicates the data so that the frame for time `t` contains every row with
#' `time <= t`. This produces the "line drawing itself" effect when the frame
#' column is mapped to a Plotly animation frame. All other columns (e.g.
#' `variable`, `condition`, `sim`) are preserved, including factor levels.
#'
#' The first frame (at the earliest time point) holds a single point per series,
#' which draws nothing under `mode = "lines"`, so the animation starts from an
#' empty plot at the initial state. Non-finite starting values (e.g. a `0/0`
#' ratio at initialization) are backfilled in that first frame with the series'
#' first finite value -- see the inline note for why this is necessary.
#'
#' @param df Data frame with a time column, or NULL.
#' @param time_col Name of the time column. Defaults to "time".
#' @param frame_col Name of the frame column to create. Defaults to ".frame".
#' @param value_col Name of the value column, used to backfill non-finite
#'   starting values in the first frame. Defaults to "value"; backfilling is
#'   skipped if the column is absent.
#' @param max_frames Maximum number of animation frames. When the data has more
#'   unique time points than this, evenly spaced thresholds are used (always
#'   keeping the last time so the final frame is complete). This keeps the
#'   animation responsive: a naive one-frame-per-time-point expansion is
#'   quadratic in the number of time points and can freeze plotly for finely
#'   sampled simulations. Defaults to 50.
#' @returns Data frame with a `frame_col` column, or `df` unchanged if empty/NULL.
#' @noRd
accumulate_by_time <- function(df, time_col = "time", frame_col = ".frame",
                               value_col = "value", max_frames = 50) {
  if (is.null(df) || nrow(df) == 0L) {
    return(df)
  }

  times <- sort(unique(df[[time_col]]))

  # Cap the number of frames for performance. Use "nice" rounded breakpoints
  # (snapped to the nearest available time) so the slider tick labels read like
  # ordinary x-axis ticks (e.g. 0, 5, 10, ...) instead of raw sample times. The
  # line itself still draws at full resolution within each frame, since each
  # frame includes every row up to its threshold.
  if (length(times) > max_frames) {
    rng <- range(times)
    # Slider step labels are the frame times, so cap the *nice* breaks at ~20
    # regardless of max_frames: more tick labels than that overprint.
    nice <- pretty(rng, n = min(max_frames, 20))
    nice <- nice[nice > rng[1] & nice < rng[2]]
    snapped <- vapply(nice, function(v) times[which.min(abs(times - v))], numeric(1))
    times <- sort(unique(c(rng[1], snapped, rng[2])))

    # pretty() only approximates the requested count, so enforce the cap by
    # thinning evenly while keeping the first and last frame.
    if (length(times) > max_frames) {
      keep <- unique(round(seq(1L, length(times), length.out = max_frames)))
      times <- times[keep]
    }
  }

  out <- lapply(times, function(time_value) {
    d <- df[df[[time_col]] <= time_value, , drop = FALSE]
    d[[frame_col]] <- time_value
    d
  })

  out <- do.call(rbind, out)
  rownames(out) <- NULL

  # Keep every series' trace alive in the first frame. That frame holds a single
  # point per series (an invisible dot under mode = "lines"), so the animation
  # starts from an empty plot. But a series that is non-finite there -- e.g. an
  # auxiliary defined as a 0/0 ratio at t = 0 -- yields an all-NaN trace, which
  # plotly drops from the initial data while still padding it back into the
  # first frame. That trace-count mismatch both warns ("number of items to
  # replace is not a multiple of replacement length") and corrupts the initial
  # frame. Backfilling those values with each series' first finite value keeps
  # the trace present without changing what is drawn (a single point draws no
  # line). Series with no finite value anywhere are left as-is: they are dropped
  # from every frame consistently, so no mismatch arises.
  if (value_col %in% names(out)) {
    key_cols <- setdiff(names(df), c(time_col, value_col, frame_col))
    series_key <- function(d) {
      if (length(key_cols) == 0L) {
        return(rep("", nrow(d)))
      }
      do.call(paste, c(lapply(key_cols, function(k) as.character(d[[k]])), sep = "\r"))
    }
    bad <- which(out[[frame_col]] == times[1] & !is.finite(out[[value_col]]))
    if (length(bad) > 0L) {
      fin <- df[is.finite(df[[value_col]]), , drop = FALSE]
      fin <- fin[order(fin[[time_col]]), , drop = FALSE]
      k_fin <- series_key(fin)
      first_finite <- fin[[value_col]][!duplicated(k_fin)]
      names(first_finite) <- k_fin[!duplicated(k_fin)]
      repl <- first_finite[series_key(out[bad, , drop = FALSE])]
      # Series with no finite value anywhere have no replacement; leave them as
      # they are (they are dropped from every frame, so no mismatch arises).
      keep <- is.finite(repl)
      out[[value_col]][bad[keep]] <- repl[keep]
    }
  }

  out
}


#' Ensure Plotly animation controls have target scaffold objects
#'
#' Plotly creates `aniSlider`/`aniButton` layout objects automatically when it
#' builds traces mapped with `frame`. Manually reconstructed subplot frames do
#' not have that scaffold, so add a minimal compatible one before calling
#' `animation_slider()` and `animation_button()`.
#'
#' @param pl Plotly object with built animation frames.
#' @param frame_ms Frame duration in milliseconds.
#' @param transition_ms Transition duration in milliseconds.
#' @returns Plotly object with animation-control scaffold objects.
#' @noRd
ensure_animation_scaffold <- function(pl, frame_ms, transition_ms) {
  pl <- plotly::plotly_build(pl)
  frames <- pl$x$frames
  if (length(frames) == 0L) {
    return(pl)
  }

  frame_names <- vapply(frames, function(frame) as.character(frame$name), character(1))
  animate_opts <- list(
    transition = list(duration = transition_ms, easing = "linear"),
    frame = list(duration = frame_ms, redraw = FALSE),
    mode = "immediate"
  )

  sliders <- pl$x$layout$sliders
  has_slider <- length(sliders) > 0L && any(vapply(sliders, function(obj) {
    "aniSlider" %in% class(obj)
  }, logical(1)))
  if (!has_slider) {
    steps <- lapply(frame_names, function(name) {
      list(
        method = "animate",
        args = list(list(name), animate_opts),
        label = name,
        value = name
      )
    })
    slider <- structure(list(
      currentvalue = list(prefix = ".frame: ", xanchor = "right", font = list()),
      steps = steps,
      visible = TRUE,
      pad = list(t = 40)
    ), class = "aniSlider")
    pl$x$layout$sliders <- c(sliders, list(slider))
  }

  buttons <- pl$x$layout$updatemenus
  has_button <- length(buttons) > 0L && any(vapply(buttons, function(obj) {
    "aniButton" %in% class(obj)
  }, logical(1)))
  if (!has_button) {
    button <- structure(list(
      type = "buttons",
      direction = "right",
      showactive = FALSE,
      y = 0,
      x = 0,
      yanchor = "top",
      xanchor = "right",
      pad = list(t = 60, r = 5),
      buttons = list(list(
        label = "Play",
        method = "animate",
        args = list(NULL, c(list(fromcurrent = TRUE), animate_opts))
      ))
    ), class = "aniButton")
    pl$x$layout$updatemenus <- c(buttons, list(button))
  }

  pl
}


#' Add Plotly animation controls (play button and time slider)
#'
#' @param pl Plotly object with animation frames.
#' @param time_unit Time unit (e.g. "weeks"), used to build the slider's
#'   dynamic title, formatted like the x-axis title (e.g. "Time (20 weeks)").
#' @param font_family,font_size Font of the slider title, matched to the x-axis title.
#' @param frame_ms Frame duration in milliseconds. Defaults to 100.
#' @param transition_ms Transition duration in milliseconds. Defaults to 0.
#' @param show_slider Whether to add an animation slider. Defaults to TRUE.
#' @param show_button Whether to add a play button. Defaults to TRUE.
#' @returns Updated plotly object.
#' @noRd
add_time_animation_controls <- function(pl,
                                        time_unit = "",
                                        font_family = getOption("sdbuildR.font_family", default = "stix-two-text"),
                                        font_size = 16,
                                        frame_ms = 100,
                                        transition_ms = 0,
                                        show_slider = TRUE,
                                        show_button = TRUE) {
  pl <- plotly::animation_opts(
    pl,
    frame = frame_ms,
    transition = transition_ms,
    redraw = FALSE
  )
  pl <- ensure_animation_scaffold(pl,
    frame_ms = frame_ms,
    transition_ms = transition_ms
  )

  # The slider replaces the x-axis, so drop the axis title and ticks. The slider
  # step labels (the frame times) then act as the x-axis tick labels. Margin is
  # merged recursively, so existing top/left/right margins are preserved.
  pl <- plotly::layout(pl,
    xaxis = list(title = "", showticklabels = FALSE, ticks = ""),
    margin = list(b = 120)
  )

  # Dynamic slider title in the x-axis-title style, e.g. "Time (20 weeks)".
  if (nzchar(time_unit)) {
    cv_prefix <- "Time ("
    cv_suffix <- paste0(" ", time_unit, ")")
  } else {
    cv_prefix <- "Time "
    cv_suffix <- ""
  }

  # Place the controls below where the x-axis title used to sit (paper y < 0 is
  # the bottom margin). The slider leaves a right-side lane for the play button.
  control_y <- -0.18
  slider_len <- if (show_button) 0.88 else 1

  if (show_slider) {
    pl <- plotly::animation_slider(
      pl,
      currentvalue = list(
        prefix = cv_prefix,
        suffix = cv_suffix,
        xanchor = "center",
        # Plotly's default currentvalue colour is a light grey (#ccc); set the
        # normal text colour so the slider title is not greyed out.
        font = list(family = font_family, size = font_size, color = "#444444")
      ),
      x = 0,
      xanchor = "left",
      len = slider_len,
      y = control_y,
      yanchor = "top",
      pad = list(t = 0, b = 0),
      font = list(family = font_family, size = ceiling(font_size * 0.85))
    )
  }

  if (show_button) {
    pl <- plotly::animation_button(
      pl,
      x = 0.94,
      xanchor = "center",
      y = control_y + abs(control_y / 1.25),
      yanchor = "center"
    )
  }

  pl
}


#' Build a name -> human-readable label lookup from a stockflow model
#'
#' Mirrors the rest of the package's display convention (see `prepare_labels()`):
#' when `format` is `TRUE` and a variable has no custom label, its label defaults
#' to the raw name, so we prettify it by turning underscores/periods into spaces.
#'
#' @param object A stockflow model (or `NULL`).
#' @param format Whether to prettify labels that fall back to the variable name
#'   (matches the `format_label` plot argument). Defaults to `TRUE`.
#' @returns A function mapping a character vector of variable names to their
#'   model labels, falling back to the (formatted) name when no label is found.
#' @noRd
model_label_lookup <- function(object, format = TRUE) {
  fmt <- if (format) format_label_default else function(x) x
  ld <- NULL
  if (!is.null(object)) {
    ld <- tryCatch(
      as.data.frame(object, properties = c("name", "label")),
      error = function(e) NULL
    )
  }
  if (is.null(ld) || !all(c("name", "label") %in% names(ld))) {
    return(function(nm) fmt(nm))
  }
  function(nm) {
    out <- ld[["label"]][match(nm, ld[["name"]])]
    raw <- is.na(out) | out == nm
    out[raw] <- fmt(nm[raw])
    out
  }
}


#' Prettify a raw variable name for display (underscores/periods -> spaces)
#'
#' The single source of the default label formatting, shared by prepare_labels(),
#' [plot.stockflow()], and the ensemble condition controls so they all agree.
#'
#' @param x Character vector of names.
#' @returns Character vector with separators replaced by single spaces.
#' @noRd
format_label_default <- function(x) {
  out <- gsub("_", " ", x, fixed = TRUE)
  out <- gsub(".", " ", out, fixed = TRUE)
  gsub("  ", " ", out, fixed = TRUE)
}


#' Apply default label formatting only to labels still equal to their name
#'
#' A variable without a custom label has its label default to the raw name; in
#' that case (and only that case) we prettify it via format_label_default().
#' Custom labels are left untouched.
#'
#' @param name,label Character vectors of equal length (variable names and labels).
#' @returns The `label` vector with name-defaulted entries prettified.
#' @noRd
format_label_if_default <- function(name, label) {
  ifelse(name == label, format_label_default(label), label)
}


#' Rich per-condition labels for ensemble condition controls
#'
#' Produces labels that surface the parameter values of each condition, e.g.
#' `"Contact rate = 1"` (single parameter) or
#' `"Condition 3 (Contact rate = 1, Recovery rate = 0.05)"` (multiple).
#'
#' @param param_tbl Parameter table.
#' @param object Stockflow model, used to map parameter names to labels.
#' @param condition_ids Condition indices (rows of `param_tbl`) in display order.
#' @param format_label Whether to prettify parameter names lacking a custom label.
#' @returns Character vector of labels, one per `condition_ids`.
#' @noRd
ensemble_condition_labels <- function(param_tbl, object, condition_ids,
                                      format_label = TRUE) {
  if (is.null(param_tbl) || ncol(param_tbl) == 0L) {
    return(paste0("Condition ", condition_ids))
  }
  labof <- model_label_lookup(object, format_label)
  cvars <- names(param_tbl)
  plabs <- labof(cvars)
  vapply(condition_ids, function(i) {
    vals <- vapply(cvars, function(cn) {
      formatC(param_tbl[i, cn], format = "g")
    }, character(1))
    parts <- paste0(plabs, " = ", vals)
    if (length(cvars) == 1L) {
      paste(parts)
    } else {
      paste0("Condition ", i, " (", paste(parts, collapse = ", "), ")")
    }
  }, character(1))
}


#' Capture per-condition trace `y` arrays for client-side swapping
#'
#' Builds each condition's plotly object and extracts, per trace, the `y` array.
#' Because every condition shares the same variables and time grid, the trace
#' structure and `x` arrays are identical across conditions, so only `y` (which
#' also encodes ribbon polygons and NA-broken trajectories) needs swapping.
#'
#' @param pl_list List of single-condition plotly objects.
#' @returns A list over conditions, each a list of `y` arrays (one per trace).
#' @noRd
capture_swapdata <- function(pl_list) {
  built <- lapply(pl_list, function(p) plotly::plotly_build(p)[["x"]][["data"]])
  n_tr <- length(built[[1]])
  ylen1 <- vapply(built[[1]], function(tr) length(tr[["y"]]), integer(1))

  for (c in seq_along(built)) {
    if (length(built[[c]]) != n_tr) {
      cli::cli_abort(c(
        "x" = "Conditions produced different numbers of traces ({length(built[[c]])} vs {n_tr}).",
        "i" = "Interactive condition controls require the same variables in every condition."
      ))
    }
    ylc <- vapply(built[[c]], function(tr) length(tr[["y"]]), integer(1))
    if (!identical(ylc, ylen1)) {
      cli::cli_abort(c(
        "x" = "Conditions produced series of different lengths.",
        "i" = "Interactive condition controls assume a shared time grid across conditions."
      ))
    }
  }

  lapply(built, function(traces) lapply(traces, function(tr) tr[["y"]]))
}


#' Resolve and validate the `control_options` list for interactive controls
#'
#' Users pass a named list to fine-tune the condition slider/dropdown and the
#' time animation without bloating the `plot()` signature (cf. the `control`
#' lists of [stats::optim()] / [stats::loess()]). Unknown names are rejected so
#' typos surface early.
#'
#' Supported options:
#' * `max_labels` -- maximum number of slider tick labels to keep visible when
#'   many conditions are varied (a positive integer; the slider always keeps one
#'   step per condition). Defaults to 10.
#' * `spacing` -- vertical gap in pixels between the tops of stacked controls
#'   (see control_geometry()). `NULL` (the default) sizes it automatically.
#' * `frame_ms` -- duration of each `animation = "time"` frame in milliseconds.
#'   Defaults to 100.
#' * `transition_ms` -- transition (smoothing) time between animation frames in
#'   milliseconds. Defaults to 0.
#' * `max_frames` -- maximum number of animation frames (see
#'   accumulate_by_time()). Defaults to 50.
#' * `duration` -- total animation length in seconds; overrides `frame_ms`
#'   (supplying both is an error). `NULL` (the default) uses `frame_ms`.
#'
#' @param control_options A named list, or `NULL`/empty for defaults.
#' @param allowed Names the caller supports; anything else is rejected as
#'   unknown. Plot methods without condition controls pass only the animation
#'   options. Defaults to all supported options.
#' @returns A list with all supported options filled in.
#' @noRd
resolve_control_options <- function(control_options = list(),
                                    allowed = c(
                                      "max_labels", "spacing", "frame_ms",
                                      "transition_ms", "max_frames", "duration"
                                    )) {
  # `spacing = NULL` means "use the type-specific default" (see control_geometry);
  # `duration = NULL` means "use frame_ms".
  defaults <- list(
    max_labels = 10L, spacing = NULL,
    frame_ms = 100, transition_ms = 0, max_frames = 50L, duration = NULL
  )

  if (is.null(control_options)) {
    return(defaults)
  }
  if (!is.list(control_options)) {
    cli::cli_abort(c(
      "x" = "{.arg control_options} must be a named list.",
      "i" = "For example, {.code control_options = list(max_labels = 8)}."
    ))
  }

  unknown <- setdiff(names(control_options), allowed)
  if (length(control_options) > 0 &&
    (is.null(names(control_options)) || length(unknown) > 0)) {
    bad <- if (is.null(names(control_options))) "<unnamed>" else unknown
    cli::cli_abort(c(
      "x" = "Unknown {.arg control_options}: {.val {bad}}.",
      "i" = "Supported options: {.val {allowed}}."
    ))
  }

  # `duration` and `frame_ms` both set the animation pace; reject the ambiguity.
  if (all(c("duration", "frame_ms") %in% names(control_options))) {
    cli::cli_abort(c(
      "x" = "Supply either {.arg control_options$duration} or {.arg control_options$frame_ms}, not both.",
      "i" = "{.arg duration} sets the total animation length; {.arg frame_ms} sets the time per frame."
    ))
  }

  out <- utils::modifyList(defaults, control_options)

  ml <- out[["max_labels"]]
  if (!is.numeric(ml) || length(ml) != 1L || is.na(ml) || ml < 1) {
    cli::cli_abort(c(
      "x" = "{.arg control_options$max_labels} must be a single positive number.",
      "i" = "You supplied {.val {ml}}."
    ))
  }
  out[["max_labels"]] <- as.integer(ml)

  sp <- out[["spacing"]]
  if (!is.null(sp) &&
    (!is.numeric(sp) || length(sp) != 1L || is.na(sp) || sp <= 0)) {
    cli::cli_abort(c(
      "x" = "{.arg control_options$spacing} must be a single positive number (or NULL).",
      "i" = "You supplied {.val {sp}}."
    ))
  }

  fm <- out[["frame_ms"]]
  if (!is.numeric(fm) || length(fm) != 1L || is.na(fm) || fm <= 0) {
    cli::cli_abort(c(
      "x" = "{.arg control_options$frame_ms} must be a single positive number (milliseconds).",
      "i" = "You supplied {.val {fm}}."
    ))
  }

  tm <- out[["transition_ms"]]
  if (!is.numeric(tm) || length(tm) != 1L || is.na(tm) || tm < 0) {
    cli::cli_abort(c(
      "x" = "{.arg control_options$transition_ms} must be a single non-negative number (milliseconds).",
      "i" = "You supplied {.val {tm}}."
    ))
  }

  mf <- out[["max_frames"]]
  if (!is.numeric(mf) || length(mf) != 1L || is.na(mf) || mf < 2) {
    cli::cli_abort(c(
      "x" = "{.arg control_options$max_frames} must be a single number of at least 2.",
      "i" = "You supplied {.val {mf}}."
    ))
  }
  out[["max_frames"]] <- as.integer(mf)

  du <- out[["duration"]]
  if (!is.null(du) &&
    (!is.numeric(du) || length(du) != 1L || is.na(du) || du <= 0)) {
    cli::cli_abort(c(
      "x" = "{.arg control_options$duration} must be a single positive number (seconds, or NULL).",
      "i" = "You supplied {.val {du}}."
    ))
  }

  out
}


#' Effective per-frame duration for the time animation
#'
#' When `duration` (total animation length in seconds) is set, spread it evenly
#' over the frames; otherwise use `frame_ms` directly.
#'
#' @param opts Resolved `control_options` list (see resolve_control_options()).
#' @param n_frames Number of animation frames actually built.
#' @returns Frame duration in milliseconds.
#' @noRd
resolve_frame_ms <- function(opts, n_frames) {
  if (!is.null(opts[["duration"]]) && n_frames > 0) {
    return(opts[["duration"]] * 1000 / n_frames)
  }
  opts[["frame_ms"]]
}


#' Deterministic vertical geometry for stacked condition controls
#'
#' Sliders and dropdowns are anchored in the figure's bottom margin using paper
#' coordinates (a fraction of the plot-area height), while the margin that must
#' contain them is in pixels. With a responsive figure height the plot-area
#' height is unknown at build time, so *no* fixed paper offset can guarantee a
#' pixel gap between controls: as the bottom margin grows, the plot area
#' shrinks and the same paper offset collapses to fewer pixels, which is what
#' let stacked controls overlap. This helper therefore fixes the plot-area
#' height (`plot_area_px`), making every quantity exact in pixels: the
#' per-control offsets, the bottom margin that contains them, and (via
#' `plot_area_px`) the total figure height the caller must pin on the widget.
#' The x-axis title is pinned close to the axis (small `standoff`) so the first
#' control clears it.
#'
#' @param n_controls Number of stacked controls (>= 1).
#' @param type Either "slider" or "dropdown".
#' @param spacing Optional vertical gap in pixels between the tops of
#'   consecutive controls. `NULL` uses a type-specific default sized to a
#'   control's own height.
#' @param plot_area_px Fixed plot-area height in pixels.
#' @returns List with `y` (paper y-position per control, top-most first),
#'   `margin_b` (bottom margin in px), `standoff` (x-axis title standoff px),
#'   and `plot_area_px` (echoed so the caller can derive the figure height).
#' @noRd
control_geometry <- function(n_controls, type, spacing = NULL,
                             plot_area_px = 360) {
  n_controls <- max(1L, as.integer(n_controls))
  if (identical(type, "slider")) {
    first_px <- 75 # clears the x tick labels + axis title
    step_default_px <- 100 # currentvalue title + rail + tick labels is tall
    control_px <- 85 # height of one slider
  } else {
    first_px <- 65
    step_default_px <- 50 # dropdown buttons collapse to a single bar when closed
    control_px <- 35
  }
  step_px <- if (is.null(spacing)) step_default_px else spacing
  offsets_px <- first_px + step_px * (seq_len(n_controls) - 1L)
  list(
    y = -offsets_px / plot_area_px,
    margin_b = offsets_px[n_controls] + control_px + 15,
    standoff = 15,
    plot_area_px = plot_area_px
  )
}


#' Thin slider tick labels to at most `max_labels`, keeping the endpoints
#'
#' A slider keeps one step (jump) per parameter value, but printing every tick
#' label overprints when many values are varied. This blanks intermediate
#' labels so at most `max_labels` evenly spaced ticks (always including the
#' first and last) are labelled, while the number of steps is unchanged.
#'
#' @param labels Character vector of tick labels, one per step.
#' @param max_labels Maximum number of labels to keep visible.
#' @returns `labels` with intermediate entries blanked when there are more than
#'   `max_labels` of them.
#' @noRd
thin_slider_labels <- function(labels, max_labels = 10L) {
  n <- length(labels)
  if (n <= max_labels) {
    return(labels)
  }
  keep <- unique(round(seq(1, n, length.out = max_labels)))
  out <- rep("", n)
  out[keep] <- labels[keep]
  out
}


#' Build per-parameter controls (sliders or dropdowns) for a crossed ensemble
#'
#' One control per condition variable. The browser-side handler reads each
#' control's active value and finds the matching condition (cross-product).
#'
#' @param param_tbl Full parameter table.
#' @param condition_ids Condition indices built into `pl_list`/`ydata`, in order.
#' @param type Either "slider" or "dropdown".
#' @param object Stockflow model, for parameter labels.
#' @param max_labels Maximum number of slider tick labels to keep visible.
#' @param spacing Optional pixel gap between stacked controls (see
#'   control_geometry()). NULL uses the type-specific default.
#' @param format_label Whether to prettify parameter names lacking a custom label.
#' @returns List with `layout` (named list for [plotly::layout()]), `condVals`
#'   (per built condition, the parameter values), and `levels` (per parameter,
#'   its sorted unique values) for the JS handler.
#' @noRd
build_param_controls <- function(param_tbl, condition_ids, type, object,
                                 max_labels = 10L, spacing = NULL,
                                 format_label = TRUE) {
  labof <- model_label_lookup(object, format_label)
  cvars <- names(param_tbl)
  plabs <- labof(cvars)
  steps_per <- lapply(param_tbl, function(x) sort(unique(x)))

  geo <- control_geometry(length(cvars), type, spacing)

  controls <- lapply(seq_along(cvars), function(j) {
    vals <- steps_per[[cvars[j]]]
    nm <- as.character(j - 1L) # 0-based index tag read by the JS handler
    if (type == "slider") {
      tick_labels <- thin_slider_labels(formatC(vals, format = "g"), max_labels)
      list(
        active = 0, name = nm,
        # Full plot width: unlike the time-animation slider, there is no play
        # button to leave a lane for.
        x = 0, len = 1, y = geo[["y"]][j],
        pad = list(t = 10, b = 10),
        currentvalue = list(prefix = paste0(plabs[j], " = ")),
        steps = lapply(seq_along(vals), function(k) {
          list(label = tick_labels[k], method = "skip")
        })
      )
    } else {
      list(
        type = "dropdown", active = 0, showactive = TRUE, name = nm,
        direction = "up", x = 0, xanchor = "left",
        y = geo[["y"]][j], yanchor = "top",
        buttons = lapply(vals, function(v) {
          list(
            label = paste0(plabs[j], " = ", formatC(v, format = "g")),
            method = "skip"
          )
        })
      )
    }
  })

  layout <- if (type == "slider") {
    list(sliders = controls)
  } else {
    list(updatemenus = controls)
  }

  # Values aligned with the built conditions (= ydata order).
  condVals <- lapply(condition_ids, function(i) {
    as.list(as.numeric(param_tbl[i, cvars, drop = TRUE]))
  })
  levels <- lapply(cvars, function(cn) as.list(as.numeric(steps_per[[cn]])))

  list(layout = layout, condVals = condVals, levels = levels)
}


#' JavaScript handler that swaps trace `y` arrays on control change
#'
#' Used only for the per-parameter (crossed) layout, where independent controls
#' must be combined into a single condition via a cross-product lookup. Sliders
#' emit `plotly_sliderchange`; dropdown buttons emit `plotly_buttonclicked`.
#' (Both controls use `method = "skip"`, which performs no relayout/restyle of
#' its own, so `plotly_relayout` never fires -- these two events are the only
#' signals plotly.js gives us. Their payloads carry the control's `name`, which
#' we set to the 0-based parameter index, and its already-updated `active`.)
#'
#' @returns A length-one character vector with the handler source.
#' @noRd
swap_onrender_js <- function() {
  "
  function(el, x, data) {
    var gd = el;
    var K = data.levels.length;
    var state = new Array(K).fill(0);
    var idx = []; for (var t = 0; t < data.nTraces; t++) idx.push(t);

    function apply() {
      var chosen = state.map(function(a, j){ return data.levels[j][a]; });
      var ci = -1;
      for (var c = 0; c < data.condVals.length; c++) {
        var ok = true;
        for (var j = 0; j < K; j++) {
          if (Math.abs(data.condVals[c][j] - chosen[j]) > 1e-9) { ok = false; break; }
        }
        if (ok) { ci = c; break; }
      }
      if (ci < 0 || !data.ydata[ci]) return; // combination not simulated
      Plotly.restyle(gd, {y: data.ydata[ci]}, idx);
    }

    function readActives(arr) {
      if (!arr) return;
      for (var i = 0; i < arr.length; i++) {
        var nm = arr[i].name;
        state[nm != null ? +nm : i] = arr[i].active || 0;
      }
    }

    gd.on('plotly_sliderchange', function(e) {
      if (e && e.slider && e.slider.name != null && e.slider.active != null) {
        state[+e.slider.name] = e.slider.active;
      } else {
        readActives(gd._fullLayout && gd._fullLayout.sliders);
      }
      apply();
    });

    gd.on('plotly_buttonclicked', function(e) {
      if (e && e.menu && e.menu.name != null && e.active != null) {
        state[+e.menu.name] = e.active;
      } else {
        readActives(gd._fullLayout && gd._fullLayout.updatemenus);
      }
      apply();
    });
  }
  "
}


#' Assemble a single plot with condition selection controls
#'
#' Builds each condition's traces (via `pl_list`, one plotly object per
#' condition), keeps only the first condition's traces in the live figure, and
#' swaps their `y` arrays client-side as the user changes the control(s). For a
#' crossed ensemble (`cross = TRUE`, >= 2 parameters) one control is shown per
#' parameter; otherwise a single control steps through the conditions.
#'
#' @param pl_list List of single-condition plotly objects (one per condition).
#' @param condition_ids Vector of condition values, aligned with `pl_list`.
#' @param type Either "slider" or "dropdown".
#' @param labels Character vector of condition labels for the single-control
#'   layout.
#' @param theme Theme list from `plotly_theme()`.
#' @param main,xlab,ylab Title and axis labels.
#' @param font_family,font_size Font settings.
#' @param condition_table Optional parameter table enabling per-parameter controls.
#' @param cross Whether the ensemble conditions were crossed.
#' @param object Optional stockflow model, for parameter labels.
#' @param max_labels Maximum number of slider tick labels to keep visible.
#' @param spacing Optional pixel gap between stacked controls (see
#'   control_geometry()). NULL uses the type-specific default.
#' @param format_label Whether to prettify parameter names lacking a custom label.
#' @param tick_labels Whether to show the slider's rail tick labels. When FALSE
#'   the rail labels are hidden (useful when the condition labels are long) but
#'   the currently selected label still shows in the slider title.
#' @returns A combined plotly object with condition controls. The widget's
#'   height is pinned (see control_geometry()) so the control geometry is exact.
#' @noRd
assemble_condition_control_plot <- function(pl_list, condition_ids, type,
                                            labels, theme,
                                            main, xlab, ylab,
                                            font_family, font_size,
                                            condition_table = NULL,
                                            cross = FALSE,
                                            object = NULL,
                                            max_labels = 10L,
                                            spacing = NULL,
                                            format_label = TRUE,
                                            tick_labels = TRUE) {
  ydata <- capture_swapdata(pl_list)
  n_traces <- length(ydata[[1]])

  use_param <- isTRUE(cross) && !is.null(condition_table) &&
    ncol(condition_table) >= 2L
  n_controls <- if (use_param) ncol(condition_table) else 1L

  # Shared geometry: one source for the control offsets, the bottom margin that
  # must contain them, and the x-axis title standoff that keeps the title clear.
  geo <- control_geometry(n_controls, type, spacing)
  margin_b <- max(theme[["margin"]][["b"]], geo[["margin_b"]])

  # Live figure: the first condition's traces, with shared title/axes/theme.
  combined <- plotly::layout(pl_list[[1]],
    title = list(text = main),
    xaxis = list(title = list(text = xlab, standoff = geo[["standoff"]])),
    yaxis = list(title = ylab),
    font = list(family = font_family, size = font_size),
    margin = utils::modifyList(theme[["margin"]], list(b = margin_b)),
    legend = theme[["legend"]]
  )

  # Pin the figure height so the plot area is exactly `plot_area_px` tall. The
  # control offsets are paper coordinates (fractions of the plot-area height),
  # so with a responsive height the true pixel gaps are unknowable and stacked
  # controls can collapse into each other; a fixed height makes the geometry
  # exact. Set on the widget (container height) and in the plotly layout so
  # both agree; width stays responsive.
  fig_height <- geo[["plot_area_px"]] + theme[["margin"]][["t"]] + margin_b
  combined[["height"]] <- fig_height
  combined[["x"]][["layout"]][["height"]] <- fig_height

  if (use_param) {
    ctrl <- build_param_controls(
      condition_table, condition_ids, type, object, max_labels, spacing,
      format_label
    )
    combined <- do.call(plotly::layout, c(list(combined), ctrl[["layout"]]))
    combined <- htmlwidgets::onRender(combined, swap_onrender_js(),
      data = list(
        nTraces = n_traces, ydata = ydata,
        condVals = ctrl[["condVals"]], levels = ctrl[["levels"]]
      )
    )
    return(combined)
  }

  # Single control: each step/button natively restyles to its condition's y
  # arrays (no JS needed). For a slider over a single parameter, the parameter
  # label is in the slider title (currentvalue prefix) and the tick labels
  # carry only the values; otherwise the full condition labels are used.
  trace_idx <- seq_len(n_traces) - 1L

  slider_prefix <- ""
  step_labels <- as.character(unlist(labels))
  single_param <- !is.null(condition_table) && ncol(condition_table) == 1L
  if (type == "slider" && single_param) {
    cn <- names(condition_table)[1]
    slider_prefix <- paste0(model_label_lookup(object, format_label)(cn), " = ")
    step_labels <- formatC(condition_table[condition_ids, cn], format = "g")
  }
  # Thin the rail tick labels only when they are shown; when hidden, every step
  # keeps its full label so the slider title (which reads the active step's
  # label) is correct for every position.
  if (type == "slider" && tick_labels) {
    step_labels <- thin_slider_labels(step_labels, max_labels)
  }

  steps <- lapply(seq_along(condition_ids), function(k) {
    list(
      method = "restyle",
      args = list(list(y = ydata[[k]]), as.list(trace_idx)),
      label = step_labels[[k]]
    )
  })

  if (type == "slider") {
    slider <- list(
      # Full plot width: unlike the time-animation slider, there is no play
      # button to leave a lane for.
      active = 0, x = 0, len = 1, y = geo[["y"]][1],
      pad = list(t = 10, b = 10),
      currentvalue = list(prefix = slider_prefix),
      steps = steps
    )
    if (!tick_labels) {
      # Hide the (long) rail tick labels by making the step-label font
      # transparent, but keep the selected label in the slider title. The
      # currentvalue font inherits its colour from the slider font, so set an
      # explicit visible colour on it or the title would vanish too.
      slider[["font"]] <- list(color = "rgba(0,0,0,0)")
      slider[["currentvalue"]][["font"]] <-
        list(family = font_family, size = font_size, color = "#444444")
    }
    plotly::layout(combined, sliders = list(slider))
  } else {
    plotly::layout(combined,
      updatemenus = list(list(
        type = "dropdown", active = 0, showactive = TRUE,
        direction = "up", x = 0, xanchor = "left",
        y = geo[["y"]][1], yanchor = "top",
        buttons = steps
      ))
    )
  }
}


#' Build informative condition labels for verify plots
#'
#' A verify "condition" is one simulation run: the model with one set of
#' parameter overrides (possibly none), checked by one or more unit tests. The
#' bare condition index means nothing to a user, so each run is labelled by
#' what it *is*: its parameter overrides first, with the test number(s)
#' evaluated on it in parentheses, e.g. "rate = 0 (test 2)". The run without
#' overrides is called "Baseline (tests 1, 3)" -- but only when displayed
#' alongside varied runs; when nothing is varied there is no contrast to draw,
#' so the label is just the tests, e.g. "Tests 1, 3". The same labels are used
#' for subplot titles and slider/dropdown steps (see
#' [plot.verify_stockflow()]), so every display names a run identically.
#'
#' Uses the `test` and `conditions` columns from
#' `as.data.frame.verify_stockflow(which = "sims")`, falling back to
#' "Condition j" for runs absent from `df`.
#'
#' @param df Verify simulation data frame with `test`, `condition`, and
#'   optionally `conditions` columns.
#' @param condition_ids Vector of condition values to label.
#' @returns Character vector of labels, one per `condition_ids`.
#' @noRd
make_verify_condition_labels <- function(df, condition_ids) {
  cond_col <- if ("conditions" %in% colnames(df)) {
    as.character(df[["conditions"]])
  } else {
    character(0)
  }
  # Whether any displayed run has overrides: only then does the unmodified run
  # need a name ("Baseline") to contrast with them.
  any_varied <- any(!is.na(cond_col) & nzchar(cond_col))

  vapply(condition_ids, function(j) {
    rows <- df[df[["condition"]] == j, , drop = FALSE]
    if (nrow(rows) == 0L) {
      return(paste0("Condition ", j))
    }

    cond_str <- if ("conditions" %in% colnames(rows)) {
      as.character(rows[["conditions"]][1L])
    } else {
      ""
    }
    varied <- !is.na(cond_str) && nzchar(cond_str)

    # `test` is a display string, possibly listing several tests ("1, 3")
    # when they share this run.
    test_str <- as.character(rows[["test"]][1L])
    has_tests <- !is.na(test_str) && nzchar(test_str)
    plural <- has_tests &&
      length(strsplit(test_str, ",", fixed = TRUE)[[1L]]) > 1L
    tests <- paste0("test", if (plural) "s" else "", " ", test_str)

    if (varied) {
      if (has_tests) paste0(cond_str, " (", tests, ")") else cond_str
    } else if (any_varied) {
      if (has_tests) paste0("Baseline (", tests, ")") else "Baseline"
    } else if (has_tests) {
      # Nothing varied anywhere: no contrast to draw, name the tests directly.
      paste0(toupper(substring(tests, 1, 1)), substring(tests, 2))
    } else {
      paste0("Condition ", j)
    }
  }, character(1))
}
