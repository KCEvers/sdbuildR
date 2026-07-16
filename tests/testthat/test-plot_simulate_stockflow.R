test_that("plot() creates a basic plot for simulation", {
  sim <- sir_sim()
  result <- plot(sim)
  expect_plotly(result)
  sfm <- sim[["object"]]
  df <- as.data.frame(sfm, properties = c("label"))
  stock_labels <- df$label[df$type == "stock"]
  n_stocks <- length(stock_labels)

  # Default is show only stocks and show_legend = TRUE
  trace_info <- plotly_traces(result)
  expect_setequal(trace_info[["name"]], stock_labels)
  expect_true(all(trace_info[["show_legend"]]))
})

test_that("plot() method exists for simulate_stockflow objects", {
  expect_true("plot.simulate_stockflow" %in% methods("plot"))
})

# ============================================================================
# PARAMETER VALIDATION TESTS
# ============================================================================

test_that("plot() validates show_legend as logical", {
  sim <- sir_sim()

  expect_error(
    plot(sim, show_legend = "yes"),
    "show_legend"
  )
})

test_that("plot() validates vars as character vector", {
  sim <- sir_sim()

  expect_error(
    plot(sim, vars = 123),
    "vars"
  )
  expect_error(
    plot(sim, vars = character(0)),
    "Empty"
  )
})

test_that("plot() validates variable existence in simulation", {
  sim <- sir_sim()

  expect_error(
    plot(sim, vars = c("susceptible", "NonExistent")),
    "NonExistent.*not.*variable"
  )
})

test_that("plot() warns and continues when some vars are missing from data", {
  sim <- sir_sim(only_stocks = TRUE)

  expect_warning(
    pl <- plot(sim, vars = c("susceptible", "new_infections")),
    "not saved in the output"
  )
  expect_plotly(pl)
})


test_that("plot() errors when all requested vars are missing from data", {
  sim <- sir_sim(only_stocks = TRUE)

  expect_error(
    plot(sim, vars = c("new_recoveries", "new_infections")),
    "not saved in the output"
  )
})


test_that("plot() does not error when all requested vars are constants", {
  sim <- sir_sim(only_stocks = TRUE)

  expect_no_error(
    plot(sim, vars = c("infection_rate", "recovery_rate"))
  )
})

test_that("plot() does not error when some requested vars are constants", {
  sim <- sir_sim(only_stocks = TRUE)

  expect_no_error(
    plot(sim, vars = c("susceptible", "recovery_rate"))
  )
})

test_that("plot() validates font_family as character", {
  sim <- sir_sim()

  expect_error(
    plot(sim, font_family = 123),
    "font_family"
  )
})

test_that("plot() validates font_size as positive number", {
  sim <- sir_sim()

  expect_error(
    plot(sim, font_size = 0),
    "must be a positive number"
  )
})

test_that("plot() validates wrap_width as positive integer", {
  sim <- sir_sim()

  expect_error(
    plot(sim, wrap_width = -10),
    "must be a positive integer"
  )
})

test_that("plot() validates palette as character", {
  sim <- sir_sim()

  expect_error(
    plot(sim, palette = 123),
    "palette"
  )
})

test_that("plot() validates colors as character vector", {
  sim <- sir_sim()

  expect_error(
    plot(sim, colors = 123),
    "colors"
  )

  # SIR has 3+ variables, provide only 1 color
  expect_error(
    plot(sim, colors = "#FF0000"),
    "Insufficient colors provided"
  )
})

# ============================================================================
# LINE WIDTH TESTS
# ============================================================================

# Extract the line width of every built trace (NA when unset).
trace_line_widths <- function(pl) {
  traces <- plotly::plotly_build(pl)[["x"]][["data"]]
  vapply(traces, function(t) {
    w <- t[["line"]][["width"]]
    if (is.null(w)) NA_real_ else as.numeric(w)[1L]
  }, numeric(1))
}

trace_line_dashes <- function(pl) {
  traces <- plotly::plotly_build(pl)[["x"]][["data"]]
  vapply(traces, function(t) {
    dash <- t[["line"]][["dash"]]
    if (is.null(dash)) NA_character_ else as.character(dash)[1L]
  }, character(1))
}

trace_fills <- function(pl) {
  traces <- plotly::plotly_build(pl)[["x"]][["data"]]
  vapply(traces, function(t) {
    fill <- t[["fill"]]
    if (is.null(fill)) NA_character_ else as.character(fill)[1L]
  }, character(1))
}

test_that("plot() validates line_width as positive numeric", {
  sim <- sir_sim()

  expect_error(plot(sim, line_width = "thick"), "line_width")
  expect_error(plot(sim, line_width = -1), "positive")
  expect_error(plot(sim, line_width = 0), "positive")
})

test_that("plot() errors when line_width vector is too short", {
  sim <- sir_sim(only_stocks = FALSE)
  n <- length(unique(as.data.frame(sim)[["variable"]]))

  expect_error(
    plot(sim, line_width = rep(2, n - 1)),
    "Insufficient"
  )
})

test_that("plot() applies a scalar line_width to every trace", {
  sim <- sir_sim()
  pl <- plot(sim, line_width = 5)

  widths <- trace_line_widths(pl)
  expect_true(all(widths == 5, na.rm = TRUE))
  expect_false(anyNA(widths))
})

test_that("plot() applies a per-variable line_width vector", {
  sim <- sir_sim(only_stocks = FALSE)
  n <- length(unique(as.data.frame(sim)[["variable"]]))
  lw <- seq_len(n)

  pl <- plot(sim, line_width = lw, webgl = FALSE)
  widths <- trace_line_widths(pl)

  # Each requested width must appear exactly once across the traces.
  expect_setequal(widths, lw)
})

test_that("plot() defaults to a line width of 2", {
  sim <- sir_sim()
  widths <- trace_line_widths(plot(sim))
  expect_true(all(widths == 2, na.rm = TRUE))
})

test_that("plot() validates line_type dash strings", {
  sim <- sir_sim()
  valid <- c("solid", "dot", "dash", "longdash", "dashdot", "longdashdot", "5px,10px,2px,2px")

  for (dash in valid) {
    expect_no_error(plot(sim, line_type = dash, webgl = FALSE))
  }
  expect_error(plot(sim, line_type = "wiggle", webgl = FALSE), "line_type")
})

test_that("plot() applies named and type-keyed line_type styles", {
  sim <- sir_sim(only_stocks = FALSE)
  names_df <- as.data.frame(sim[["object"]], type = c("stock", "flow", "aux"), properties = c("label"))
  label_for <- stats::setNames(names_df[["label"]], names_df[["name"]])

  pl_named <- plot(sim,
    webgl = FALSE,
    line_type = c(susceptible = "dot", new_infections = "5px,10px,2px,2px")
  )
  dashes_named <- stats::setNames(trace_line_dashes(pl_named), plotly_traces(pl_named)[["name"]])
  expect_equal(dashes_named[[label_for[["susceptible"]]]], "dot")
  expect_equal(dashes_named[[label_for[["new_infections"]]]], "5px,10px,2px,2px")

  pl_type <- plot(sim, webgl = FALSE, line_type = list(stock = "dash", flow = "dot"))
  traces_type <- plotly_traces(pl_type)
  dashes_type <- stats::setNames(trace_line_dashes(pl_type), traces_type[["name"]])
  stock_labels <- names_df[["label"]][names_df[["type"]] == "stock"]
  flow_labels <- names_df[["label"]][names_df[["type"]] == "flow"]
  expect_true(all(dashes_type[stock_labels] == "dash"))
  expect_true(all(dashes_type[flow_labels] == "dot"))
})

# ============================================================================
# VISUAL REGRESSION TESTS (expect_snapshot_plot)
# ============================================================================

test_that("plot() creates standard line plot for SIR simulation", {
  sim <- sir_sim()
  pl <- plot(sim)
  expect_plotly(pl)

  sfm <- sim[["object"]]
  df <- as.data.frame(sfm, properties = c("label", "type"))
  stock_labels <- df$label[df$type == "stock"]
  trace_info <- plotly_traces(pl)
  expect_setequal(trace_info[["name"]], stock_labels)
  expect_true(all(trace_info$show_legend))

  expect_snapshot_plot("sim-sir-default", pl)
})

test_that("plot.simulate_stockflow() respects show_legend", {
  sim <- sir_sim()
  # Object-level expectations: legend toggles should reflect in built Plotly object
  pl_true <- plot(sim, show_legend = TRUE)
  pl_false <- plot(sim, show_legend = FALSE)
  expect_plotly(pl_true)
  expect_plotly(pl_false)
  traces_true <- plotly_traces(pl_true)
  traces_false <- plotly_traces(pl_false)
  expect_true(nrow(traces_true) > 0)
  expect_true(all(traces_true$show_legend))
  expect_true(nrow(traces_false) > 0)
  expect_true(all(!(traces_false$show_legend)))

  # Snapshots last
  expect_snapshot_plot(
    c("sim-show_legend-true", "sim-show_legend-false"),
    list(pl_true, pl_false)
  )
})

test_that("plot.simulate_stockflow() respects vars argument", {
  sim <- sir_sim()
  # Object-level expectations: vars filtering should limit plotted trace names
  sfm <- sim[["object"]]
  names_df <- as.data.frame(sfm, properties = c("label"))
  sus_label <- names_df$label[names_df$name == "susceptible"]
  pl_single <- plot(sim, vars = "susceptible")
  trace_names <- plotly_traces(pl_single)$name
  expect_setequal(trace_names, sus_label)

  sus_infected_labels <- names_df$label[names_df$name %in% c("susceptible", "infected")]
  pl_filtered <- plot(sim, vars = c("susceptible", "infected"))
  trace_names_filtered <- plotly_traces(pl_filtered)$name
  expect_setequal(trace_names_filtered, sus_infected_labels)

  # Snapshots last
  expect_snapshot_plot(
    c("sim-single-variable", "sim-filtered-vars"),
    list(pl_single, pl_filtered)
  )
})

test_that("plot.simulate_stockflow() with custom palette", {
  sim <- sir_sim()
  sfm <- sim[["object"]]
  stock_labels <- as.data.frame(sfm, properties = c("label", "type"))$label
  pl <- plot(sim, palette = "Blues")
  expect_plotly(pl)
  traces <- plotly_traces(pl)
  expect_true(nrow(traces) > 0)

  # Unique colors should be assigned across traces when a palette is used
  expect_true(length(unique(traces$color)) == nrow(traces))

  expect_snapshot_plot("sim-custom-palette", pl)
})

test_that("plot.simulate_stockflow() with custom colors vector", {
  sim <- sir_sim()
  # Object-level expectation: legend trace colors reflect custom palette when exposed
  df <- as.data.frame(sim, direction = "long")
  vars <- unique(df$variable)
  names_df <- as.data.frame(sim[["object"]])
  label_names <- names_df$label[match(vars, names_df$name)]
  custom_colors <- stats::setNames(rainbow(length(vars)), vars)
  expected_colors <- stats::setNames(unname(custom_colors), label_names)

  pl_colors <- plot(sim, colors = custom_colors, alpha = 1)
  expect_plotly(pl_colors)
  traces <- plotly_traces(pl_colors)
  expect_equal(length(traces[["name"]]), length(label_names))
  legend_check <- plotly_check_legend_colors(pl_colors, expected = expected_colors)
  expect_true(nrow(legend_check) > 0)
  expect_true(all(legend_check$ok))
  expect_true(all(legend_check$matches_expected))

  # Snapshot last
  expect_snapshot_plot("sim-custom-colors", pl_colors)
})

test_that("plot.simulate_stockflow() maps trace labels to source data and named colors", {
  sim <- sir_sim(only_stocks = FALSE)
  names_df <- as.data.frame(sim[["object"]], type = c("stock", "flow", "aux"))
  labels <- names_df[["name"]]
  colors <- stats::setNames(
    grDevices::rainbow(length(labels)),
    labels
  )
  colors <- rev(colors)

  pl <- plot(sim, colors = colors, webgl = FALSE)
  built_traces <- plotly::plotly_build(pl)[["x"]][["data"]]
  trace_info <- plotly_traces(pl)
  label_to_name <- stats::setNames(names_df[["name"]], names_df[["label"]])

  for (i in seq_along(built_traces)) {
    trace_label <- built_traces[[i]][["name"]]
    variable <- unname(label_to_name[[trace_label]])
    expected_y <- sim[["df"]][["value"]][sim[["df"]][["variable"]] == variable]

    expect_equal(as.numeric(built_traces[[i]][["y"]]), as.numeric(expected_y))
    expect_equal(
      trace_info[["color"]][i],
      normalize_color_string(colors[[variable]])
    )
  }
})

test_that("plot.simulate_stockflow() orders traces before applying aesthetics", {
  sim <- sir_sim(only_stocks = FALSE)
  names_df <- as.data.frame(sim[["object"]], type = c("stock", "flow", "aux"), properties = "label")
  requested_order <- c("new_recoveries", "susceptible", "new_infections", "infected", "recovered")
  expected_labels <- names_df[["label"]][match(requested_order, names_df[["name"]])]
  colors <- stats::setNames(
    c("red", "blue", "green", "purple", "orange"),
    rev(requested_order)
  )

  pl <- plot(sim,
    vars_display = "joint", order = requested_order,
    colors = colors, webgl = FALSE
  )
  traces <- plotly_traces(pl)

  expect_equal(traces[["name"]], expected_labels)
  for (i in seq_along(requested_order)) {
    expect_equal(
      traces[["color"]][i],
      normalize_color_string(colors[[requested_order[[i]]]])
    )
  }
})

test_that("plot.simulate_stockflow() validates and filters order", {
  sim <- sir_sim(only_stocks = TRUE)

  expect_error(plot(sim, order = "not_a_variable"), "order")
  expect_error(plot(sim, order = numeric(1)), "order")
  expect_error(plot(sim, order = ""), "order")

  expect_warning(
    pl <- plot(sim, order = c("new_infections", "infected")),
    "not shown"
  )
  expect_equal(plotly_traces(pl)[["name"]][1], "Infected")
})

test_that("plot.simulate_stockflow() follows vars order by default (order defaults to vars)", {
  # `order` defaults to `vars`, so the trace/legend order should follow the
  # order in which variables are listed in `vars`, without passing `order`.
  sim <- sir_sim(only_stocks = FALSE)
  names_df <- as.data.frame(sim[["object"]],
    type = c("stock", "flow", "aux"), properties = "label"
  )

  requested <- c("recovered", "susceptible", "infected")
  expected <- names_df[["label"]][match(requested, names_df[["name"]])]
  pl <- plot(sim, vars = requested, vars_display = "joint", webgl = FALSE)
  expect_equal(plotly_traces(pl)[["name"]], expected)

  # Mixed stocks and flows in a joint plot keep the vars order too.
  requested2 <- c("new_infections", "recovered", "susceptible")
  expected2 <- names_df[["label"]][match(requested2, names_df[["name"]])]
  pl2 <- plot(sim, vars = requested2, vars_display = "joint", webgl = FALSE)
  expect_equal(plotly_traces(pl2)[["name"]], expected2)

  # An explicit `order` still overrides the vars order.
  pl3 <- plot(sim,
    vars = requested2, order = rev(requested2),
    vars_display = "joint", webgl = FALSE
  )
  expect_equal(
    plotly_traces(pl3)[["name"]],
    names_df[["label"]][match(rev(requested2), names_df[["name"]])]
  )
})

test_that("plot.simulate_stockflow() does not warn twice when a vars entry is unsaved", {
  # `order` defaults to `vars`; a var that exists but was not saved must be
  # reported once (by the vars filter), not a second time by the ordering step.
  sim <- sir_sim(only_stocks = TRUE)

  warnings <- character(0)
  withCallingHandlers(
    plot(sim, vars = c("susceptible", "new_infections"), webgl = FALSE),
    warning = function(cnd) {
      warnings <<- c(warnings, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )

  expect_length(warnings, 1L)
  expect_match(warnings, "not saved in the output")
  expect_false(any(grepl("not shown", warnings)))
})

test_that("plot.simulate_stockflow() maps default palette colors to the correct labels", {
  # Regression test: with both stocks (highlight) and non-stocks (nonhighlight)
  # present and the default palette (colors = NULL), colours were assigned
  # positionally in highlight-first order while traces are emitted
  # nonhighlight-first, scrambling the label -> colour mapping. Each trace must
  # carry the colour resolve_colors() assigns to its label.
  sim <- sir_sim(only_stocks = FALSE)

  # Intended label -> colour mapping (the source of truth the plot must honour).
  prep <- prep_plot(
    sim[["object"]], "sim", sim[["df"]], sim[["constants"]],
    show_constants = FALSE, vars = NULL, palette = "Dark 2", colors = NULL,
    wrap_width = 25, format_label = TRUE
  )
  expected <- prep[["colors"]] # named by display label

  # Guard the regression: both highlight (stocks) and nonhighlight variables must
  # be present, otherwise the two orderings coincide and the bug cannot surface.
  expect_true(length(prep[["highlight_names"]]) > 0)
  expect_true(length(prep[["nonhighlight_names"]]) > 0)

  pl <- plot(sim, webgl = FALSE)
  traces <- plotly_traces(pl)

  # Every plotted label appears, and each label gets its intended colour.
  expect_setequal(traces[["name"]], names(expected))
  for (i in seq_len(nrow(traces))) {
    label <- traces[["name"]][i]
    expect_equal(
      traces[["color"]][i],
      normalize_color_string(expected[[label]]),
      info = paste0("colour mismatch for label '", label, "'")
    )
  }
})

test_that("plot.simulate_stockflow() with custom font family", {
  sim <- sir_sim()
  pl <- plot(sim, font_family = "Courier New")
  layout <- plotly_layout(pl)
  expect_equal(layout$font$family, "Courier New")

  expect_snapshot_plot("sim-custom-font-family", pl)
})

test_that("plot.simulate_stockflow() with custom font size", {
  sim <- sir_sim()
  pl <- plot(sim, font_size = 20)
  layout <- plotly_layout(pl)
  expect_equal(layout$font$size, 20)

  expect_snapshot_plot("sim-large-font-size", pl)
})

test_that("plot.simulate_stockflow() with custom wrap width", {
  sfm <- stockflow()
  sfm <- update(sfm,
    name = "a",
    label = "Very Long Stock Name That Should Wrap", type = "stock"
  )
  stock_name_clean <- sfm$variables$name[1]
  sfm <- update(sfm,
    name = "b",
    label = "Long Flow Name That Should Also Wrap", type = "flow",
    from = !!stock_name_clean
  )
  sim <- simulate(sfm, only_stocks = FALSE)
  pl <- plot(sim, wrap_width = 10, webgl = FALSE)
  expect_plotly(pl)
  traces <- plotly_traces(pl)
  expect_true(all(grepl("<br", traces[["name"]])))

  expect_snapshot_plot("sim-wrap-width-narrow", pl)
})

test_that("plot.simulate_stockflow() with custom title, axis labels, and limits", {
  sim <- sir_sim()
  pl <- plot(sim,
    main = "Custom Simulation Title",
    xlab = "Custom X", ylab = "Custom Y", xlim = c(0, 50), ylim = c(0, 800)
  )
  layout <- plotly_layout(pl)
  expect_true(grepl("Custom Simulation Title", layout$title))
  expect_equal(layout$xaxis$title, "Custom X")
  expect_equal(layout$yaxis$title, "Custom Y")
  expect_equal(layout$xaxis$range, c(0, 50))
  expect_equal(layout$yaxis$range, c(0, 800))

  expect_snapshot_plot("sim-custom-title-axes-limits", pl)
})

test_that("plot.simulate_stockflow() respects show_constants", {
  sfm <- stockflow()
  sfm <- update(sfm, "Stock1", type = "stock")
  sfm <- update(sfm, "const_val", type = "constant", eqn = "100")
  sim <- simulate(sfm)

  constants <- as.data.frame(sim[["object"]], type = "constants", properties = "label")
  const_label <- constants$label[constants$name == "const_val"]
  pl_with_constants <- plot(sim, show_constants = TRUE)
  expect_plotly(pl_with_constants)
  traces <- plotly_traces(pl_with_constants)
  # Default format_label = TRUE prettifies the name-defaulted label (const_val).
  expect_true(all(format_label_default(const_label) %in% traces[["name"]]))

  pl_without_constants <- plot(sim, show_constants = FALSE)
  expect_plotly(pl_without_constants)
  traces_no_const <- plotly_traces(pl_without_constants)
  expect_true(all(!(const_label %in% traces_no_const[["name"]])))

  expect_snapshot_plot(
    c("sim-with-constants", "sim-without-constants"),
    list(pl_with_constants, pl_without_constants)
  )
})

# ============================================================================
# EDGE CASES AND COMPLEX SCENARIOS
# ============================================================================

test_that("plot.simulate_stockflow() shows legend for single-variable plot", {
  sfm <- stockflow()
  sfm <- update(sfm, "Stock1", type = "stock")
  sim <- simulate(sfm)
  pl <- plot(sim, show_legend = TRUE, webgl = FALSE)
  expect_plotly(pl)
  trace_info <- plotly_traces(pl)
  expect_equal(nrow(trace_info), 1L)
  expect_equal(trace_info$name, "Stock1")
  expect_true(all(trace_info$show_legend))
  expect_snapshot_plot("sim-single-var-legend", pl)
})

test_that("plot.simulate_stockflow() works with both stocks and flow variables", {
  # SIR has susceptible (stock), infected (stock), recovered (stock)
  sim <- sir_sim(only_stocks = FALSE)
  df <- as.data.frame(sim, direction = "long")
  var_names <- unique(df$variable)
  pl <- plot(sim, show_legend = TRUE, webgl = FALSE)
  expect_plotly(pl)
  trace_info <- plotly_traces(pl)
  expect_equal(nrow(trace_info), length(var_names))
  expect_true(all(trace_info$show_legend))

  expect_snapshot_plot("sim-only-stocks-false", pl)
})

test_that("plot.simulate_stockflow() separates stocks from non-stock variables", {
  sim <- sir_sim(only_stocks = FALSE)
  names_df <- as.data.frame(sim[["object"]], type = c("stock", "flow", "aux"), properties = c("label"))
  stock_labels <- names_df[["label"]][names_df[["type"]] == "stock"]
  flow_labels <- names_df[["label"]][names_df[["type"]] == "flow"]

  pl <- plot(sim, webgl = FALSE)
  info <- plotly_subplot_grid(pl)
  expect_true(info$is_subplot)
  expect_equal(info$nrows, 2L)
  expect_equal(info$ncols, 1L)
  expect_equal(info$n_yaxes, 2L)
  expect_true(info$shareX)

  layout <- plotly_layout(pl)
  y_domains <- list(layout[["yaxis"]][["domain"]], layout[["yaxis2"]][["domain"]])
  y_heights <- vapply(y_domains, function(domain) diff(as.numeric(domain)), numeric(1))
  expect_equal(unname(y_heights[1]), unname(y_heights[2]), tolerance = 0.01)

  traces <- plotly_traces(pl)
  expect_setequal(traces[["name"]][traces[["yaxis"]] == "y"], stock_labels)
  expect_setequal(traces[["name"]][traces[["yaxis"]] == "y2"], flow_labels)
  expect_true(all(traces[["visible"]] == "TRUE"))

  fills <- stats::setNames(trace_fills(pl), traces[["name"]])
  expect_true(all(fills[flow_labels] == "tozeroy"))
  expect_true(all(is.na(fills[stock_labels])))
})

test_that("plot.simulate_stockflow() can put role panels side by side", {
  sim <- sir_sim(only_stocks = FALSE)
  names_df <- as.data.frame(sim[["object"]], type = c("stock", "flow", "aux"), properties = c("label"))
  stock_labels <- names_df[["label"]][names_df[["type"]] == "stock"]
  flow_labels <- names_df[["label"]][names_df[["type"]] == "flow"]

  pl <- plot(sim, webgl = FALSE, vars_display = "hstack", xlab = "Shared time")
  info <- plotly_subplot_grid(pl)
  expect_true(info$is_subplot)
  expect_equal(info$nrows, 1L)
  expect_equal(info$ncols, 2L)
  expect_equal(info$n_xaxes, 2L)
  expect_equal(info$n_yaxes, 2L)
  expect_false(info$shareY)

  layout <- plotly_layout(pl)
  x_domains <- list(layout[["xaxis"]][["domain"]], layout[["xaxis2"]][["domain"]])
  x_widths <- vapply(x_domains, function(domain) diff(as.numeric(domain)), numeric(1))
  expect_equal(unname(x_widths[1]), unname(x_widths[2]), tolerance = 0.01)

  traces <- plotly_traces(pl)
  expect_setequal(traces[["name"]][traces[["xaxis"]] == "x"], stock_labels)
  expect_setequal(traces[["name"]][traces[["xaxis"]] == "x2"], flow_labels)
  expect_setequal(traces[["name"]][traces[["yaxis"]] == "y"], stock_labels)
  expect_setequal(traces[["name"]][traces[["yaxis"]] == "y2"], flow_labels)

  fills <- stats::setNames(trace_fills(pl), traces[["name"]])
  expect_true(all(fills[flow_labels] == "tozeroy"))
  expect_true(all(is.na(fills[stock_labels])))

  expect_equal(layout[["xaxis"]][["title"]], "")
  expect_equal(layout[["xaxis2"]][["title"]], "")
  x_titles <- vapply(layout[["annotations"]], function(annotation) {
    annotation[["text"]] %||% NA_character_
  }, character(1))
  expect_equal(sum(x_titles == "Shared time", na.rm = TRUE), 1L)
  x_title <- layout[["annotations"]][[which(x_titles == "Shared time")]]
  expect_equal(x_title[["x"]], 0.5)
  expect_equal(x_title[["xref"]], "paper")
  expect_equal(x_title[["xanchor"]], "center")
})

test_that("plot.simulate_stockflow() can leave flow traces unfilled", {
  sim <- sir_sim(only_stocks = FALSE)
  names_df <- as.data.frame(sim[["object"]], type = c("stock", "flow", "aux"), properties = c("label"))
  flow_labels <- names_df[["label"]][names_df[["type"]] == "flow"]

  pl <- plot(sim, webgl = FALSE, fill_flows = FALSE)
  traces <- plotly_traces(pl)
  fills <- stats::setNames(trace_fills(pl), traces[["name"]])
  expect_true(all(is.na(fills[flow_labels])))
})

test_that("plot.simulate_stockflow() preserves combined view when requested", {
  sim <- sir_sim(only_stocks = FALSE)
  names_df <- as.data.frame(sim[["object"]], type = c("stock", "flow", "aux"), properties = c("label"))
  stock_labels <- names_df[["label"]][names_df[["type"]] == "stock"]
  flow_labels <- names_df[["label"]][names_df[["type"]] == "flow"]

  pl <- plot(sim, webgl = FALSE, vars_display = "joint")

  info <- plotly_subplot_grid(pl)
  expect_false(info$is_subplot)
  traces <- plotly_traces(pl)
  expect_true(all(traces[["yaxis"]] == "y"))

  fills <- stats::setNames(trace_fills(pl), traces[["name"]])
  expect_true(all(fills[flow_labels] == "tozeroy"))
  expect_true(all(is.na(fills[stock_labels])))
})

test_that("plot.simulate_stockflow() can leave joint flow traces unfilled", {
  sim <- sir_sim(only_stocks = FALSE)
  names_df <- as.data.frame(sim[["object"]], type = c("stock", "flow", "aux"), properties = c("label"))
  flow_labels <- names_df[["label"]][names_df[["type"]] == "flow"]

  pl <- plot(sim, webgl = FALSE, vars_display = "joint", fill_flows = FALSE)
  traces <- plotly_traces(pl)
  fills <- stats::setNames(trace_fills(pl), traces[["name"]])
  expect_true(all(is.na(fills[flow_labels])))
})

test_that("plot.simulate_stockflow() validates vars_display", {
  sim <- sir_sim()
  expect_error(plot(sim, vars_display = "rows"), "vars_display")
})


test_that("plot.simulate_stockflow() draws constants unfilled and dashed in non-stock panel", {
  sfm <- stockflow() |>
    update("Stock1", type = "stock") |>
    update("const_val", type = "constant", eqn = "75")
  sim <- simulate(sfm)

  pl <- plot(sim, vars = c("Stock1", "const_val"), webgl = FALSE)
  traces <- plotly_traces(pl)
  const_label <- format_label_default("const_val")
  const_row <- traces[traces[["name"]] == const_label, , drop = FALSE]
  expect_equal(const_row[["yaxis"]], "y2")

  fills <- stats::setNames(trace_fills(pl), traces[["name"]])
  dashes <- stats::setNames(trace_line_dashes(pl), traces[["name"]])
  expect_true(is.na(fills[[const_label]]))
  expect_equal(dashes[[const_label]], "dash")
})

test_that("plot.simulate_stockflow() handles variables with duplicate display labels", {
  sfm <- stockflow()
  sfm <- update(sfm, "var1", type = "stock", label = "Same")
  sfm <- update(sfm, "var2", type = "stock", label = "Same")
  sim <- simulate(sfm)
  pl <- plot(sim)
  traces <- plotly_traces(pl)
  expect_setequal(traces$name, c("Same (var1)", "Same (var2)"))
})

test_that("plot.simulate_stockflow() respects vars filtering for constants", {
  sfm <- stockflow()
  sfm <- update(sfm, "S", type = "stock")
  sfm <- update(sfm, "I", type = "stock")
  sfm <- update(sfm, "const1", type = "constant", eqn = "50")
  sfm <- update(sfm, "const2", type = "constant", eqn = "100")
  sim <- simulate(sfm)

  # Request only S and const1
  pl <- plot(sim, vars = c("S", "const1"), show_constants = TRUE)
  expect_plotly(pl)
  traces <- plotly_traces(pl)
  constants <- as.data.frame(sim[["object"]], type = "constants", properties = "label")
  const_label <- constants$label[constants$name == "const1"]
  expect_true(sum(const_label == traces[["name"]]) == 1)
  expect_snapshot_plot("sim-constants-filtered-vars", pl)
})

test_that("plot.simulate_stockflow() with vars = constant automatically enables show_constants", {
  sfm <- stockflow()
  sfm <- update(sfm, "Stock1", type = "stock")
  sfm <- update(sfm, "const_val", type = "constant", eqn = "75")
  sim <- simulate(sfm)

  # Even without show_constants = TRUE, specifying a constant in vars should include it
  pl <- plot(sim, vars = c("Stock1", "const_val"), show_constants = FALSE)
  expect_plotly(pl)
  traces <- plotly_traces(pl)
  expect_true(sum(format_label_default("const_val") == traces[["name"]]) == 1)
  expect_snapshot_plot("sim-vars-constant-show-constants", pl)
})

test_that("plot.simulate_stockflow() format_label toggles legend label prettifying", {
  sfm <- stockflow()
  sfm <- update(sfm, "Stock1", type = "stock")
  sfm <- update(sfm, "const_val", type = "constant", eqn = "75")
  sim <- simulate(sfm)

  # Default (TRUE): the name-defaulted label is prettified in the legend.
  names_on <- plotly_traces(plot(sim, show_constants = TRUE))[["name"]]
  expect_true("const val" %in% names_on)
  expect_false("const_val" %in% names_on)

  # FALSE: the raw variable name is kept.
  names_off <- plotly_traces(
    plot(sim, show_constants = TRUE, format_label = FALSE)
  )[["name"]]
  expect_true("const_val" %in% names_off)
  expect_false("const val" %in% names_off)
})

test_that("plot.simulate_stockflow() rejects a non-logical format_label", {
  sim <- simulate(stockflow("sir"))
  expect_error(plot(sim, format_label = "yes"), "format_label")
})

# ============================================================================
# DEFAULT BEHAVIOR TESTS
# ============================================================================

test_that("plot.simulate_stockflow() uses default titles", {
  sfm <- stockflow("sir") |> meta(name = "My Model")
  sim <- simulate(sfm)

  pl <- plot(sim)
  layout <- plotly_layout(pl)
  expect_true(grepl(sfm$meta$name, layout$title))
  expect_true(grepl("^Time", layout$xaxis$title))
  expect_equal(layout$yaxis$title, "")
  expect_plotly(pl)
})

# ============================================================================
# TIME ANIMATION
# ============================================================================

test_that("plot.simulate_stockflow() supports cumulative time animation", {
  sim <- sir_sim()
  pl <- plot(sim, animation = "time")
  expect_plotly(pl)

  layout <- plotly_layout(pl)
  expect_equal(layout$sliders[[1]]$x, 0)
  expect_equal(layout$sliders[[1]]$len, 0.88)
  expect_gt(layout$updatemenus[[1]]$x, layout$sliders[[1]]$x + layout$sliders[[1]]$len)

  frames <- plotly_frames(pl)
  expect_true(length(frames) > 0)

  # Frame count is capped for performance; frame times are a subset of the
  # simulation times, and the final frame reaches the last time point.
  all_times <- sort(unique(sim[["df"]][["time"]]))
  frame_names <- plotly_frame_names(pl)
  expect_true(length(frame_names) <= 50)
  expect_true(all(frame_names %in% as.character(all_times)))
  expect_equal(frame_names[length(frame_names)], as.character(max(all_times)))

  # Cumulative reveal: total plotted points never decrease across frames
  point_counts <- vapply(frames, function(frame) {
    sum(vapply(frame[["data"]], function(trace) length(trace[["x"]]), integer(1)))
  }, integer(1))
  expect_true(all(diff(point_counts) >= 0))
})

test_that("time animation builds cleanly when a variable is NaN at time zero", {
  # Regression: a variable that starts at NaN (e.g. a 0/0 ratio at
  # initialization) used to make plotly drop its all-NaN trace from the
  # initial data but not from the first frame, corrupting the first frame
  # with a "number of items to replace is not a multiple of replacement
  # length" warning at build time.
  sim <- sir_sim()
  first_time <- min(sim[["df"]][["time"]])
  first_var <- as.character(sim[["df"]][["variable"]][1])
  sim[["df"]][["value"]][
    sim[["df"]][["time"]] == first_time & sim[["df"]][["variable"]] == first_var
  ] <- NaN

  pl <- plot(sim, animation = "time")
  expect_no_warning(built <- plotly::plotly_build(pl))

  # The initial trace data and every frame carry the same traces
  n_traces <- length(built$x$data)
  expect_true(all(
    vapply(built$x$frames, function(frame) length(frame$data), integer(1)) == n_traces
  ))

  # The animation still starts from an empty plot at the first time point: the
  # first frame is at the earliest time and draws no line (a line needs two
  # consecutive finite points).
  all_times <- sort(unique(sim[["df"]][["time"]]))
  expect_equal(built$x$frames[[1]]$name, as.character(min(all_times)))
  max_consecutive_finite <- function(y) {
    fin <- is.finite(unlist(y))
    if (!any(fin)) {
      return(0L)
    }
    runs <- rle(fin)
    max(runs$lengths[runs$values])
  }
  draws_line <- vapply(
    built$x$frames[[1]]$data,
    function(trace) max_consecutive_finite(trace$y) >= 2, logical(1)
  )
  expect_false(any(draws_line))
})

test_that("plot.simulate_stockflow() is static by default (no frames)", {
  sim <- sir_sim()
  expect_equal(length(plotly_frames(plot(sim))), 0L)
  expect_equal(length(plotly_frames(plot(sim, animation = "none"))), 0L)
})

test_that("plot.simulate_stockflow() rejects invalid animation", {
  sim <- sir_sim()
  expect_error(plot(sim, animation = "fast"), "animation")
})

test_that("plot.simulate_stockflow() control_options tune the animation speed", {
  sim <- sir_sim()

  # Per-frame and transition durations reach the built animation options (the
  # play button's animate args carry them).
  pl <- plot(sim,
    animation = "time",
    control_options = list(frame_ms = 40, transition_ms = 20)
  )
  opts <- plotly_layout(pl)$updatemenus[[1]]$buttons[[1]]$args[[2]]
  expect_equal(opts$frame$duration, 40)
  expect_equal(opts$transition$duration, 20)

  # duration spreads the total animation length (seconds) evenly over frames
  pl <- plot(sim, animation = "time", control_options = list(duration = 10))
  n_frames <- length(plotly_frame_names(pl))
  opts <- plotly_layout(pl)$updatemenus[[1]]$buttons[[1]]$args[[2]]
  expect_equal(opts$frame$duration, 10000 / n_frames)

  # max_frames caps the number of frames (endpoints always kept)
  pl_few <- plot(sim, animation = "time", control_options = list(max_frames = 10))
  few_names <- plotly_frame_names(pl_few)
  all_times <- sort(unique(sim[["df"]][["time"]]))
  expect_lte(length(few_names), 10)
  expect_lt(length(few_names), length(plotly_frame_names(plot(sim, animation = "time"))))
  expect_equal(few_names[length(few_names)], as.character(max(all_times)))
})

test_that("plot.simulate_stockflow() rejects invalid control_options", {
  sim <- sir_sim()
  # Unknown keys, including condition-control keys that only apply to
  # ensemble/verify plots
  expect_error(plot(sim, control_options = list(speed = 2)), "control_options")
  expect_error(plot(sim, control_options = list(max_labels = 5)), "control_options")
  # duration and frame_ms both set the pace
  expect_error(
    plot(sim, animation = "time", control_options = list(duration = 5, frame_ms = 40)),
    "not both"
  )
  expect_error(plot(sim, control_options = list(frame_ms = 0)), "frame_ms")
  expect_error(plot(sim, control_options = list(transition_ms = -1)), "transition_ms")
  expect_error(plot(sim, control_options = list(max_frames = 1)), "max_frames")
  expect_error(plot(sim, control_options = list(duration = 0)), "duration")
})

test_that("plot.simulate_stockflow() preserves role panels in time animation", {
  sim <- sir_sim(only_stocks = FALSE)
  names_df <- as.data.frame(sim[["object"]], type = c("stock", "flow", "aux"), properties = c("label"))
  flow_labels <- names_df[["label"]][names_df[["type"]] == "flow"]

  pl <- plot(sim, animation = "time", webgl = FALSE)
  built <- plotly::plotly_build(pl)[["x"]]
  traces <- plotly_traces(pl)
  expect_setequal(unique(traces[["yaxis"]]), c("y", "y2"))
  expect_true(length(built[["frames"]]) > 0L)

  flow_trace_idx <- which(traces[["name"]] %in% flow_labels)
  expect_true(length(flow_trace_idx) > 0L)
  first_frame <- built[["frames"]][[1L]]
  expect_setequal(first_frame[["traces"]], seq_along(built[["data"]]) - 1L)

  frame_hits <- match(flow_trace_idx - 1L, first_frame[["traces"]])
  frame_fills <- vapply(first_frame[["data"]][frame_hits], function(trace) {
    fill <- trace[["fill"]]
    if (is.null(fill)) NA_character_ else as.character(fill)[1L]
  }, character(1))
  expect_true(all(frame_fills == "tozeroy"))
})

test_that("plot.simulate_stockflow() preserves hstack panels in time animation", {
  sim <- sir_sim(only_stocks = FALSE)
  pl <- plot(sim, animation = "time", vars_display = "hstack", webgl = FALSE)
  built <- plotly::plotly_build(pl)[["x"]]
  traces <- plotly_traces(pl)

  expect_setequal(unique(traces[["xaxis"]]), c("x", "x2"))
  expect_setequal(unique(traces[["yaxis"]]), c("y", "y2"))
  expect_true(length(built[["frames"]]) > 0L)
  expect_setequal(built[["frames"]][[1L]][["traces"]], seq_along(built[["data"]]) - 1L)
})

test_that("plot.simulate_stockflow() webgl toggles trace type", {
  sim <- sir_sim()

  types_gl <- vapply(
    plotly::plotly_build(plot(sim, webgl = TRUE))$x$data,
    function(d) d$type %||% "", character(1)
  )
  expect_true(any(types_gl == "scattergl"))

  types_svg <- vapply(
    plotly::plotly_build(plot(sim, webgl = FALSE))$x$data,
    function(d) d$type %||% "", character(1)
  )
  expect_false(any(types_svg == "scattergl"))

  expect_error(plot(sim, webgl = "no"), "webgl")
})


test_that("plot.simulate_stockflow() respects global webgl option", {
  sim <- sir_sim()

  withr::local_options(list(sdbuildR.webgl = TRUE))
  types_gl <- vapply(
    plotly::plotly_build(plot(sim))$x$data,
    function(d) d$type %||% "", character(1)
  )
  expect_true(any(types_gl == "scattergl"))

  withr::local_options(list(sdbuildR.webgl = FALSE))
  types_svg <- vapply(
    plotly::plotly_build(plot(sim))$x$data,
    function(d) d$type %||% "", character(1)
  )
  expect_false(any(types_svg == "scattergl"))

  # Setting webgl explicitly overrides the global option
  types_svg <- vapply(
    plotly::plotly_build(plot(sim, webgl = TRUE))$x$data,
    function(d) d$type %||% "", character(1)
  )
  expect_true(any(types_svg == "scattergl"))
})
