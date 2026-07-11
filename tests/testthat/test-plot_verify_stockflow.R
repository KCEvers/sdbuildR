# ============================================================================
# METHOD EXISTENCE + GUARD TESTS
# ============================================================================

test_that("plot.verify_stockflow method exists", {
  expect_true("plot.verify_stockflow" %in% methods("plot"))
})


# ============================================================================
# BASIC OUTPUT TYPE (non-snapshot)
# ============================================================================

test_that("plot.verify_stockflow for single condition, n=1", {
  res <- make_verify_model()
  pl <- plot(res, test = 1L)
  expect_plotly(pl)

  df <- as.data.frame(res, which = "sims", direction = "long")
  names_df <- as.data.frame(res[["object"]])
  var_names <- unique(df$variable)
  label_names <- names_df$label[match(var_names, names_df$name)]

  # Object-level expectations
  traces <- plotly_traces(pl)
  expect_setequal(traces[["name"]], label_names)
  expect_true(all(traces$show_legend))

  info <- plotly_subplot_grid(pl)
  expect_equal(info$n_panels, 1L)
  expect_equal(info$nrows, 1L)
  expect_equal(info$ncols, 1L)

  expect_snapshot_plot("verify-single-cond-n1", pl)
})

test_that("plot.verify_stockflow for two conditions", {
  skip_on_cran()

  res <- make_verify_model(n_tests = 2)
  pl <- plot(res, nrows = 2L, shareX = TRUE, shareY = TRUE)
  expect_plotly(pl)
  df <- as.data.frame(res, which = "sims", direction = "long")
  names_df <- as.data.frame(res[["object"]])
  var_names <- unique(df$variable)
  label_names <- names_df$label[match(var_names, names_df$name)]
  # Object-level expectations for subplot with multiple conditions
  traces <- plotly_traces(pl)
  expect_true(nrow(traces) > 0)
  expect_setequal(traces[["name"]], label_names)
  expect_true(all(plotly_dedupe_legend(traces)$show_legend))

  info <- plotly_subplot_grid(pl)
  expect_true(info$is_subplot)
  expect_equal(info$n_panels, 2L)
  expect_equal(info$nrows, 2L)
  expect_equal(info$ncols, 1L)
  expect_true(info$shareX)

  expect_snapshot_plot("verify-two-conditions", pl)
})


test_that("plot.verify_stockflow keeps trace colors aligned with the legend", {
  sfm <- make_verifiable_sfm() |>
    update(S, eqn = 10) |>
    unit_test(label = "S non-negative", expr = "all(S >= 0)") |>
    unit_test(label = "drain non-negative", expr = "all(drain >= 0)")

  res <- silence(verify(sfm))
  df <- as.data.frame(res, which = "sims", direction = "long")
  vars <- unique(df$variable)
  names_df <- as.data.frame(res[["object"]])
  label_names <- names_df$label[match(vars, names_df$name)]
  pl <- plot(res)
  expect_plotly(pl)
  # Object-level expectations for subplot with multiple conditions
  traces <- plotly_traces(pl)
  legend_check <- plotly_check_legend_colors(pl)
  expect_true(nrow(traces) > 0)
  expect_setequal(traces[["name"]], label_names)
  expect_true(nrow(legend_check) > 0)
  expect_true(all(legend_check$ok))
})


test_that("plot.verify_stockflow uses explicit custom colors on legend traces", {
  sfm <- make_verifiable_sfm() |>
    update(S, eqn = 10) |>
    unit_test(label = "S non-negative", expr = "all(S >= 0)") |>
    unit_test(label = "drain non-negative", expr = "all(drain >= 0)")

  res <- silence(verify(sfm))
  df <- as.data.frame(res, which = "sims", direction = "long")
  vars <- unique(df$variable)
  names_df <- as.data.frame(res[["object"]])
  label_names <- names_df$label[match(vars, names_df$name)]

  # Generate as many custom colors as there are variables
  custom_colors <- stats::setNames(rainbow(length(vars)), vars)
  expected_colors <- stats::setNames(unname(custom_colors), label_names)
  pl <- plot(res, colors = custom_colors, alpha = 1)
  expect_plotly(pl)
  traces <- plotly_traces(pl)
  expect_equal(nrow(traces), 2L)

  # Check that trace names match labels and colors match the custom colors provided
  legend_check <- plotly_check_legend_colors(pl, expected = expected_colors)
  expect_true(nrow(legend_check) > 0)
  expect_true(all(legend_check$ok))
  expect_true(all(legend_check$matches_expected))
})

test_that("plot.verify_stockflow respects trace order", {
  sfm <- stockflow() |>
    update("A", type = "stock", eqn = 1) |>
    update("B", type = "stock", eqn = 2) |>
    update("rate", type = "constant", eqn = 1) |>
    unit_test(label = "stocks non-negative", expr = "all(A >= 0 & B >= 0)")
  res <- silence(verify(sfm))
  names_df <- as.data.frame(res[["object"]])
  requested_order <- c("B", "A")
  expected_labels <- names_df[["label"]][match(requested_order, names_df[["name"]])]

  pl <- plot(res, order = requested_order, webgl = FALSE)
  legend_items <- plotly_dedupe_legend(plotly_traces(pl))

  expect_equal(legend_items[["name"]], expected_labels)
})

test_that("plot.verify_stockflow maps trace labels to source data and named colors", {
  res <- make_verify_model()
  names_df <- as.data.frame(res[["object"]])
  var_names <- names_df[["name"]]
  colors <- stats::setNames(grDevices::rainbow(length(var_names)), var_names)
  colors <- rev(colors)

  pl <- plot(res, colors = colors, webgl = FALSE)
  built_traces <- plotly::plotly_build(pl)[["x"]][["data"]]
  trace_info <- plotly_traces(pl)
  df <- as.data.frame(res, which = "sims", direction = "long")
  label_to_name <- stats::setNames(names_df[["name"]], names_df[["label"]])

  for (i in seq_along(built_traces)) {
    trace_label <- built_traces[[i]][["name"]]
    variable <- unname(label_to_name[[trace_label]])
    expected_y <- df[["value"]][df[["variable"]] == variable]

    expect_equal(as.numeric(built_traces[[i]][["y"]]), as.numeric(expected_y))
    expect_equal(
      trace_info[["color"]][i],
      normalize_color_string(colors[[variable]])
    )
  }
})


# ============================================================================
# VISUAL REGRESSION TESTS (expect_snapshot_plot)
# ============================================================================

test_that("plot() filtered j selects one condition from two", {
  res <- make_verify_model(n_tests = 2)
  pl <- plot(res, test = 2L)
  expect_plotly(pl)
  df <- as.data.frame(res, which = "sims", direction = "long")
  names_df <- as.data.frame(res[["object"]])
  var_names <- unique(df$variable)
  label_names <- names_df$label[match(var_names, names_df$name)]
  # Object-level expectations for subplot with multiple conditions
  traces <- plotly_traces(pl)
  expect_equal(nrow(traces), 1L)
  expect_setequal(traces[["name"]], label_names)
  expect_true(all(plotly_dedupe_legend(traces)$show_legend))
  expect_snapshot_plot("verify-filtered-j2", pl)
})


# ============================================================================
# VISUAL REGRESSION — LAYOUT CONTROL
# ============================================================================

test_that("plot() show_legend = FALSE hides legend", {
  res <- make_verify_model()
  # Object-level expectation: no legend items when disabled
  pl <- plot(res, show_legend = FALSE)
  expect_plotly(pl)
  traces <- plotly_traces(pl)
  expect_true(nrow(traces) > 0)
  expect_true(all(!(plotly_dedupe_legend(traces)$show_legend)))

  # Snapshot last
  expect_snapshot_plot("verify-show_legend-false", pl)
})


# ============================================================================
# VISUAL REGRESSION — FILTERING
# ============================================================================

test_that("plot() label filter selects matching condition from two", {
  res <- make_verify_model(n_tests = 2)
  pl <- plot(res, label = "non-neg")
  df <- as.data.frame(res, which = "sims", direction = "long", label = "non-neg")
  names_df <- as.data.frame(res[["object"]])
  var_names <- unique(df$variable)
  label_names <- names_df$label[match(var_names, names_df$name)]
  expect_plotly(pl)
  traces <- plotly_traces(pl)
  expect_true(nrow(traces) > 0)
  expect_snapshot_plot("verify-label-filter", pl)
})

test_that("plot() test and label filters intersect correctly", {
  res <- make_verify_model(n_tests = 2)
  pl <- plot(res, test = 1L, label = "non-neg")
  expect_plotly(pl)
  traces <- plotly_traces(pl)
  expect_equal(nrow(traces), 1L)
  expect_snapshot_plot("verify-nr-label-intersection", pl)
})

test_that("plot() status = 'pass' shows only passing tests", {
  res <- make_verify_model(with_fail = TRUE)
  pl <- plot(res, status = "pass")
  expect_plotly(pl)
  traces <- plotly_traces(pl)
  expect_true(nrow(traces) > 0)
  expect_snapshot_plot("verify-status-pass-only", pl)
})

test_that("plot() status = 'fail' shows only failing tests", {
  res <- make_verify_model(with_fail = TRUE)
  pl <- plot(res, status = "fail")
  expect_plotly(pl)
  traces <- plotly_traces(pl)
  expect_true(nrow(traces) > 0)
  expect_snapshot_plot("verify-status-fail-only", pl)
})


# ============================================================================
# VISUAL REGRESSION — APPEARANCE
# ============================================================================

test_that("plot() custom palette changes line colours", {
  res <- make_verify_model()
  pl <- plot(res, palette = "Pastel 1")
  expect_plotly(pl)
  traces <- plotly_traces(pl)
  expect_true(nrow(traces) > 0)
  # Unique colors should be assigned across traces when a palette is used
  expect_true(length(unique(traces$color)) == nrow(traces))
  expect_snapshot_plot("verify-custom-palette", pl)
})

test_that("plot() custom colors vector overrides palette", {
  res <- make_verify_model()
  names_df <- as.data.frame(res[["object"]])
  var_names <- names_df$name
  label_names <- names_df$label
  custom_colors <- stats::setNames(rep("steelblue", length(var_names)), var_names)
  expected_colors <- stats::setNames(unname(custom_colors), label_names)
  pl <- plot(res, colors = custom_colors)
  expect_plotly(pl)
  legend_check <- plotly_check_legend_colors(pl, expected = expected_colors)
  expect_true(nrow(legend_check) > 0)
  expect_true(all(legend_check$ok))
  expect_true(all(legend_check$matches_expected))
  expect_snapshot_plot("verify-custom-colors", pl)
})

test_that("plot() custom font_family changes annotation font", {
  res <- make_verify_model()
  pl <- plot(res, font_family = "Arial")
  layout <- plotly_layout(pl)
  expect_equal(layout$font$family, "Arial")

  # Snapshots last
  expect_snapshot_plot("verify-custom-font-family", pl)
})

test_that("plot() custom font_size changes annotation font", {
  res <- make_verify_model()
  pl <- plot(res, font_size = 20)
  layout <- plotly_layout(pl)
  expect_equal(layout$font$size, 20)

  # Snapshots last
  expect_snapshot_plot("verify-custom-font-size", pl)
})

test_that("plot() narrow wrap_width wraps long labels", {
  res <- make_verify_model()
  res[["object"]] <- update(res[["object"]], S, label = "This is a very long label that should be wrapped when wrap_width is narrow")
  pl <- plot(res, wrap_width = 10)
  expect_plotly(pl)
  traces <- plotly_traces(pl)
  expect_true(nrow(traces) > 0)
  expect_true(all(grepl("<br", traces[["name"]], fixed = TRUE)))

  expect_snapshot_plot("verify-wrap-width-narrow", pl)
})


# ============================================================================
# VISUAL REGRESSION — MULTI-RUN APPEARANCE
# ============================================================================

test_that("plot() custom alpha is accepted", {
  res <- make_verify_model()
  pl <- plot(res, alpha = 0.5)
  expect_plotly(pl)
  expect_true(nrow(plotly_traces(pl)) > 0)
  expect_snapshot_plot("verify-alpha-low", pl)
})

# ============================================================================
# CONDITION DISPLAY AND TIME ANIMATION
# ============================================================================

test_that("plot.verify_stockflow(condition_display = 'slider') builds a slider", {
  res <- make_verify_model(n_tests = 2)
  pl <- plot(res, condition_display = "slider")
  expect_plotly(pl)

  layout <- plotly_layout(pl)
  expect_true(length(layout$sliders) > 0)
  expect_equal(length(layout$sliders[[1]]$steps), res[["n_conditions"]])
})

test_that("plot.verify_stockflow(condition_display = 'dropdown') builds a dropdown", {
  res <- make_verify_model(n_tests = 2)
  pl <- plot(res, condition_display = "dropdown")
  expect_plotly(pl)

  layout <- plotly_layout(pl)
  expect_true(length(layout$updatemenus) > 0)
  expect_equal(length(layout$updatemenus[[1]]$buttons), res[["n_conditions"]])
})

test_that("plot.verify_stockflow() labels runs by overrides and tests everywhere", {
  res <- make_verify_model(n_tests = 2)

  # Subplot titles describe the run (overrides or "Baseline" + tests), not an
  # opaque condition index.
  pl <- plot(res)
  ann <- vapply(
    plotly::plotly_build(pl)$x$layout$annotations,
    function(a) a$text, character(1)
  )
  expect_true("Baseline (test 1)" %in% ann)
  expect_true("rate = 0 (test 2)" %in% ann)
  expect_false(any(grepl("^Condition", ann)))

  # Slider steps and dropdown buttons carry the same labels as the subplots.
  ps <- plot(res, condition_display = "slider")
  slider_labs <- vapply(
    plotly_layout(ps)$sliders[[1]]$steps,
    function(s) s$label, character(1)
  )
  expect_equal(slider_labs, c("Baseline (test 1)", "rate = 0 (test 2)"))

  pd <- plot(res, condition_display = "dropdown")
  button_labs <- vapply(
    plotly_layout(pd)$updatemenus[[1]]$buttons,
    function(b) b$label, character(1)
  )
  expect_equal(button_labs, slider_labs)

  # When nothing is varied there is no contrast to draw: no "Baseline", just
  # the test number(s). (A single panel carries no label annotation at all;
  # check the label builder directly.)
  res1 <- make_verify_model(n_tests = 1)
  df1 <- as.data.frame(res1, which = "sims")
  expect_equal(
    make_verify_condition_labels(df1, unique(df1[["condition"]])),
    "Test 1"
  )
})

test_that("plot.verify_stockflow() reverts condition controls for a single condition", {
  res <- make_verify_model(n_tests = 1) # a single condition
  expect_message(
    pl <- plot(res, condition_display = "slider"),
    "subplots"
  )
  expect_plotly(pl)
  expect_length(plotly_layout(pl)$sliders, 0)
})

test_that("plot.verify_stockflow(animation = 'time') builds frames for one condition", {
  res <- make_verify_model(n_tests = 1)
  pl <- plot(res, animation = "time")
  expect_plotly(pl)
  expect_true(length(plotly_frames(pl)) > 0)
})

test_that("plot.verify_stockflow() control_options tune the animation speed", {
  res <- make_verify_model(n_tests = 1)
  pl <- plot(res, animation = "time", control_options = list(duration = 2))
  expect_plotly(pl)
  n_frames <- length(plotly_frame_names(pl))
  opts <- plotly_layout(pl)$updatemenus[[1]]$buttons[[1]]$args[[2]]
  expect_equal(opts$frame$duration, 2000 / n_frames)
})

test_that("plot.verify_stockflow() rejects unsupported combinations", {
  res <- make_verify_model(n_tests = 2)
  expect_error(
    plot(res, animation = "time", condition_display = "slider"),
    "not supported"
  )
  # Multiple conditions (subplots) cannot be animated together
  expect_error(plot(res, animation = "time"), "not supported|multiple conditions")
})

test_that("plot.verify_stockflow() webgl toggles scatter type", {
  res <- make_verify_model()

  types_gl <- vapply(
    plotly::plotly_build(plot(res, webgl = TRUE))$x$data,
    function(d) d$type %||% "", character(1)
  )
  expect_true(any(types_gl == "scattergl"))

  types_svg <- vapply(
    plotly::plotly_build(plot(res, webgl = FALSE))$x$data,
    function(d) d$type %||% "", character(1)
  )
  expect_false(any(types_svg == "scattergl"))

  expect_error(plot(res, webgl = 1L), "webgl")
})


test_that("plot.verify_stockflow() obeys global webgl option", {
  res <- make_verify_model()

  withr::local_options(list(sdbuildR.webgl = TRUE))
  types_gl <- vapply(
    plotly::plotly_build(plot(res))$x$data,
    function(d) d$type %||% "", character(1)
  )
  expect_true(any(types_gl == "scattergl"))

  withr::local_options(list(sdbuildR.webgl = FALSE))
  types_svg <- vapply(
    plotly::plotly_build(plot(res))$x$data,
    function(d) d$type %||% "", character(1)
  )
  expect_false(any(types_svg == "scattergl"))

  # Setting webgl explicitly overrides the global option
  types_gl <- vapply(
    plotly::plotly_build(plot(res, webgl = TRUE))$x$data,
    function(d) d$type %||% "", character(1)
  )
  expect_true(any(types_gl == "scattergl"))
})
