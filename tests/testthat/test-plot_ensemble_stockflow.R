# ============================================================================
# METHOD EXISTENCE
# ============================================================================

test_that("plot.ensemble_stockflow method exists", {
  expect_true("plot.ensemble_stockflow" %in% methods("plot"))
})


# ============================================================================
# PARAMETER VALIDATION
# ============================================================================

test_that("plot.ensemble_stockflow() rejects invalid which", {
  sims <- make_r_ens(n = 2)
  expect_error(plot(sims, which = "NA"), "ust be.*summary.*sims")
})

test_that("plot.ensemble_stockflow() validates central", {
  withr::local_pdf(NULL)
  sims <- make_r_ens(n = 2)
  expect_no_error(plot(sims, central = "median"))
  expect_no_error(plot(sims, central = "none"))
  # Lenient matching: plurals and case variants are accepted.
  expect_no_error(plot(sims, central = "Medians"))
  expect_no_error(plot(sims, central = "MEAN"))
  expect_error(
    plot(sims, central = "medians2"),
    "must be.*mean.*median.*none"
  )
})

test_that("plot.ensemble_stockflow() validates spread", {
  withr::local_pdf(NULL)
  sims <- make_r_ens(n = 2, central = "mean", spread = c("quantile", "sd"))
  expect_no_error(plot(sims, spread = "sd"))
  expect_no_error(plot(sims, spread = "none"))
  # Lenient matching: plurals and case variants are accepted.
  expect_no_error(plot(sims, spread = "Quantiles"))
  expect_no_error(plot(sims, spread = "SDs"))
  expect_error(
    plot(sims, spread = "iqr"),
    "must be.*quantile.*sd.*none"
  )
})

test_that("plot.ensemble_stockflow() rejects invalid condition with single condition", {
  sims <- make_r_ens(n = 2)
  expect_error(plot(sims, condition = c(3, 6, 9)), "only one condition")
})

test_that("plot.ensemble_stockflow() rejects invalid condition with multiple conditions", {
  sims <- make_r_ens_2cond(n = 2)
  expect_error(plot(sims, condition = 10), "be integers between")
})

test_that("plot.ensemble_stockflow() informs when sim used with summary", {
  withr::local_pdf(NULL)
  sims <- make_r_ens(n = 2, save_sims = TRUE)
  expect_message(plot(sims, sim = 5), "sim.*argument is ignored")
})

test_that("plot.ensemble_stockflow() requires save_sims for which = 'sims'", {
  sims <- make_r_ens(n = 2, save_sims = FALSE)
  expect_error(plot(sims, which = "sims"), "Individual simulation data is required")
})

# ============================================================================
# ROLE-KEYED line_width / alpha (central / spread / sims)
# ============================================================================


test_that("plot.ensemble_stockflow() respects trace order", {

  skip_on_cran()

  sims <- make_aes_ens()
  vars <- ens_var_names(sims)
  requested_order <- rev(vars)
  expected_labels <- unname(vapply(requested_order, ens_label_for_name, character(1), sims = sims))

  pl <- plot(sims, which = "summary", order = requested_order)
  legend_items <- plotly_dedupe_legend(plotly_traces(pl))

  expect_equal(legend_items[["name"]], expected_labels)
})

test_that("line_width: a scalar styles every layer", {
  skip_on_cran()
  withr::local_pdf(NULL)
  sims <- make_aes_ens()
  a <- ens_trace_aes(plot(sims, which = "summary", line_width = 4))
  central <- a[is.na(a$fillcolor), ]
  spread <- a[!is.na(a$fillcolor), ]
  expect_true(nrow(central) > 0 && nrow(spread) > 0)
  expect_true(all(central$width == 4))
  expect_true(all(spread$width == 4))
})

test_that("line_width: a named vector targets specific variables", {
  skip_on_cran()
  withr::local_pdf(NULL)
  sims <- make_aes_ens()
  var <- ens_var_names(sims)[1]
  lbl <- ens_label_for_name(sims, var)
  a <- ens_trace_aes(plot(sims, which = "summary", line_width = stats::setNames(8, var)))
  central <- a[is.na(a$fillcolor), ]
  expect_equal(central$width[central$name == lbl], 8)
  # Unnamed variables keep the central default (3).
  expect_true(all(central$width[central$name != lbl] == 3))
})

test_that("line_width: a role list styles each layer independently", {
  skip_on_cran()
  withr::local_pdf(NULL)
  sims <- make_aes_ens()
  a <- ens_trace_aes(plot(sims,
    which = "summary",
    line_width = list(central = 5, spread = 0, sims = 1)
  ))
  expect_true(all(a[is.na(a$fillcolor), ]$width == 5))
  expect_true(all(a[!is.na(a$fillcolor), ]$width == 0))
})

test_that("line_width: a role list with a named per-variable vector", {
  skip_on_cran()
  withr::local_pdf(NULL)
  sims <- make_aes_ens()
  var <- ens_var_names(sims)[1]
  lbl <- ens_label_for_name(sims, var)
  a <- ens_trace_aes(plot(sims,
    which = "summary",
    line_width = list(central = stats::setNames(9, var), spread = 0)
  ))
  central <- a[is.na(a$fillcolor), ]
  expect_equal(central$width[central$name == lbl], 9)
  expect_true(all(central$width[central$name != lbl] == 3))
  expect_true(all(a[!is.na(a$fillcolor), ]$width == 0))
})

test_that("alpha: a scalar fades every layer", {
  skip_on_cran()
  withr::local_pdf(NULL)
  sims <- make_aes_ens()
  a <- ens_trace_aes(plot(sims, which = "summary", alpha = 0.5))
  expect_true(all(a[is.na(a$fillcolor), ]$opacity == 0.5))
  expect_true(all(a[!is.na(a$fillcolor), ]$opacity == 0.5))
})

test_that("alpha: a role list fades each layer independently", {
  skip_on_cran()
  withr::local_pdf(NULL)
  sims <- make_aes_ens()
  a <- ens_trace_aes(plot(sims,
    which = "summary",
    alpha = list(central = 0.5, spread = 0.1, sims = 0.4)
  ))
  expect_true(all(a[is.na(a$fillcolor), ]$opacity == 0.5))
  expect_true(all(a[!is.na(a$fillcolor), ]$opacity == 0.1))
})

test_that("alpha: a named vector targets specific variables", {
  skip_on_cran()
  withr::local_pdf(NULL)
  sims <- make_aes_ens()
  var <- ens_var_names(sims)[1]
  lbl <- ens_label_for_name(sims, var)
  # A per-variable alpha is baked into the central line colour (rgba), so the
  # opacity lives in the colour's alpha channel rather than the trace opacity.
  a <- ens_trace_aes(plot(sims, which = "summary", alpha = stats::setNames(0.2, var)))
  central <- a[is.na(a$fillcolor), ]
  av <- vapply(central$line_color, color_alpha, numeric(1))
  expect_equal(unname(av[central$name == lbl]), 0.2, tolerance = 0.01)
  # Unnamed variables keep the central default (opaque).
  expect_true(all(av[central$name != lbl] == 1))
})

test_that("alpha: a role list with a named per-variable vector", {
  skip_on_cran()
  withr::local_pdf(NULL)
  sims <- make_aes_ens()
  var <- ens_var_names(sims)[1]
  lbl <- ens_label_for_name(sims, var)
  a <- ens_trace_aes(plot(sims,
    which = "sims",
    alpha = list(sims = stats::setNames(0.1, var))
  ))
  # In which = "sims" the trajectory alpha is baked into the line colour. The
  # trajectories render as WebGL traces (scattergl), distinct from the central
  # tendency lines (scatter), so filter to those.
  traj <- a[a$type == "scattergl", ]
  av <- vapply(traj$line_color, color_alpha, numeric(1))
  expect_true(abs(av[traj$name == lbl][1] - 0.1) < 0.01)
  # Other variables keep the sims default opacity (0.3).
  expect_true(all(abs(av[traj$name != lbl] - 0.3) < 0.01))
})

test_that("line_width / alpha reject invalid input", {
  skip_on_cran()
  withr::local_pdf(NULL)
  sims <- make_aes_ens()
  expect_error(plot(sims, line_width = "thick"), "line_width")
  expect_error(plot(sims, line_width = -1), "positive")
  expect_error(plot(sims, line_width = list(foo = 1)), "role")
  expect_error(plot(sims, alpha = 2), "between 0 and 1")
  expect_error(plot(sims, alpha = list(spread = -0.1)), "between 0 and 1")
})


# ============================================================================
# ROLE-KEYED line_width / alpha (visual regression snapshots)
# ============================================================================

test_that("line_width snapshots: scalar / named / role / role+named", {
  snapshot_names <- c(
    "ens-lw-scalar",
    "ens-lw-named",
    "ens-lw-roles",
    "ens-lw-roles-named"
  )
  announce_plot_snapshot_files(snapshot_names)

  sims <- make_aes_ens()
  var <- ens_var_names(sims)[1]
  expect_snapshot_plot(
    snapshot_names,
    list(
      plot(sims, which = "summary", line_width = 4),
      plot(sims, which = "summary", line_width = stats::setNames(8, var)),
      plot(sims, which = "summary", line_width = list(central = 5, spread = 0, sims = 1)),
      plot(sims, which = "sims", line_width = list(central = stats::setNames(6, var), sims = 0.5))
    )
  )
})

test_that("alpha snapshots: scalar / named / role / role+named", {
  snapshot_names <- c(
    "ens-alpha-scalar",
    "ens-alpha-named",
    "ens-alpha-roles",
    "ens-alpha-roles-named"
  )
  announce_plot_snapshot_files(snapshot_names)

  sims <- make_aes_ens()
  var <- ens_var_names(sims)[1]
  expect_snapshot_plot(
    snapshot_names,
    list(
      plot(sims, which = "summary", alpha = 0.5),
      plot(sims, which = "summary", alpha = stats::setNames(0.2, var)),
      plot(sims, which = "summary", alpha = list(central = 0.5, spread = 0.1, sims = 0.4)),
      plot(sims, which = "sims", alpha = list(sims = stats::setNames(0.1, var)))
    )
  )
})


# ============================================================================
# BASIC OUTPUT TYPE (non-snapshot)
# ============================================================================

test_that("plot.ensemble_stockflow() creates a basic summary plot", {
  sims <- make_r_ens(n = 2)
  pl <- plot(sims)
  expect_plotly(pl)
  expect_true(nrow(plotly_traces(pl)) > 0)
})

test_that("plot.ensemble_stockflow() handles ensembles saved with selected vars", {
  
  skip_on_cran()

  withr::local_pdf(NULL)
  sims <- make_r_ens(
    n = 2,
    save_sims = TRUE,
    only_stocks = FALSE,
    vars = c("susceptible", "infected")
  )

  expect_setequal(unique(sims[["summary"]][["variable"]]), c("susceptible", "infected"))
  expect_setequal(unique(sims[["df"]][["variable"]]), c("susceptible", "infected"))

  pl <- expect_no_error(plot(sims, which = "summary"))
  expect_plotly(pl)
  legend <- plotly_dedupe_legend(plotly_traces(pl))
  expect_setequal(legend[["name"]], c("Susceptible", "Infected"))

  pl <- expect_no_error(plot(sims, which = "sims"))
  expect_plotly(pl)
  legend <- plotly_dedupe_legend(plotly_traces(pl))
  expect_setequal(legend[["name"]], c("Susceptible", "Infected"))
})


# ============================================================================
# VISUAL REGRESSION TESTS (expect_snapshot_plot)
# ============================================================================

test_that("plot() default summary plot", {
  snapshot_names <- "ens-summary-default"
  announce_plot_snapshot_files(snapshot_names)

  sims <- make_r_ens()
  pl <- plot(sims)
  expect_plotly(pl)
  traces <- plotly_traces(pl)
  expect_true(nrow(traces) > 0)
  expect_snapshot_plot(snapshot_names, pl)
})

test_that("plot() sims plot (individual trajectories)", {
  snapshot_names <- "ens-sims-default"
  announce_plot_snapshot_files(snapshot_names)

  sims <- make_r_ens(save_sims = TRUE)
  pl <- plot(sims, which = "sims")
  expect_plotly(pl)
  traces <- plotly_traces(pl)
  expect_true(nrow(traces) > 0)
  expect_snapshot_plot(snapshot_names, pl)
})

test_that("plot() sims: legend colours match trajectory colours", {
  # Regression test: the legend is carried by the central-tendency traces while
  # the individual trajectories set their colour explicitly. Plotly silently
  # drops the `colors =` palette of an aesthetic trace when explicit-colour
  # traces are already present, which made the legend swatches disagree with the
  # trajectories. Compare per legendgroup, ignoring alpha.
  skip_on_cran()
  
  withr::local_pdf(NULL)
  sims <- make_r_ens(n = 5, save_sims = TRUE)
  b <- plotly::plotly_build(plot(sims, which = "sims"))

  # Normalise any plotly colour spec (#RRGGBB, #RRGGBBAA, or rgba(...)) to "r,g,b".
  to_rgb <- function(col) {
    if (grepl("^rgba?\\(", col)) {
      nums <- as.numeric(strsplit(gsub("[rgba() ]", "", col), ",")[[1]])
      return(paste(nums[1:3], collapse = ","))
    }
    paste(grDevices::col2rgb(substr(col, 1, 7))[, 1], collapse = ",")
  }

  # Key on the trace `name` (the variable label), which both the legend-carrying
  # and trajectory traces reliably carry; `legendgroup` is unset on plotly's
  # palette-mapped traces, so it cannot be used to line them up.
  legend_col <- list() # variable -> colour of the legend-carrying trace
  traj_col <- list() # variable -> colour of the trajectory traces
  for (tr in b[["x"]][["data"]]) {
    col <- tr[["line"]][["color"]]
    if (is.null(col) || is.null(tr[["name"]])) next
    if (isTRUE(tr[["showlegend"]] %||% tr[["show_legend"]])) {
      legend_col[[tr[["name"]]]] <- to_rgb(col)
    } else {
      traj_col[[tr[["name"]]]] <- to_rgb(col)
    }
  }

  expect_true(length(legend_col) > 0)
  for (v in names(legend_col)) {
    expect_identical(legend_col[[v]], traj_col[[v]])
  }
})

test_that("plot() two conditions subplot grid", {
  snapshot_names <- "ens-two-conditions"
  announce_plot_snapshot_files(snapshot_names)

  sims <- make_r_ens_2cond()
  pl <- plot(sims, nrows = 2L, shareX = TRUE, shareY = TRUE)
  expect_plotly(pl)
  info <- plotly_subplot_grid(pl)
  expect_true(info$is_subplot)
  expect_equal(info$n_panels, 2L)
  expect_equal(info$nrows, 2L)
  expect_equal(info$ncols, 1L)
  expect_true(info$shareX)
  expect_true(is.na(info$shareY)) # shareY applies within a row
  expect_true(nrow(plotly_traces(pl)) > 0)

  expect_snapshot_plot(snapshot_names, pl)
})

test_that("plot.ensemble_stockflow() filtered condition shows single condition", {
  snapshot_names <- "ens-filtered-condition-2"
  announce_plot_snapshot_files(snapshot_names)

  sims <- make_r_ens_2cond()

  pl <- plot(sims, condition = 2L)
  expect_plotly(pl)
  info <- plotly_subplot_grid(pl)
  # expect_false(info$is_subplot) # Still a subplot grid, but with only one subplot
  expect_equal(info$n_panels, 1L)
  expect_equal(info$nrows, 1L)
  expect_equal(info$ncols, 1L)
  expect_true(nrow(plotly_traces(pl)) > 0)
  expect_snapshot_plot(snapshot_names, pl)
})

test_that("plot.ensemble_stockflow() with too many nrows", {
  snapshot_names <- "ens-too-many-nrows"
  announce_plot_snapshot_files(snapshot_names)

  sims <- make_r_ens_2cond()
  pl <- plot(sims, nrows = 3L) # More rows than conditions should be gracefully handled
  expect_plotly(pl)
  info <- plotly_subplot_grid(pl)
  # expect_false(info$is_subplot) # Still a subplot grid, but with only one subplot
  expect_equal(info$n_panels, 2L)
  expect_equal(info$nrows, 2L)
  expect_equal(info$ncols, 1L)
  expect_true(nrow(plotly_traces(pl)) > 0)
  expect_snapshot_plot(snapshot_names, pl)
})


test_that("plot.ensemble_stockflow() central = 'median'", {
  snapshot_names <- "ens-central-tendency-median"
  announce_plot_snapshot_files(snapshot_names)

  sims <- make_r_ens(central = "median")
  pl <- plot(sims, central = "median")
  expect_plotly(pl)
  expect_true(nrow(plotly_traces(pl)) > 0)
  expect_snapshot_plot(snapshot_names, pl)
})

test_that("plot.ensemble_stockflow() central = 'none' (no central line)", {
  snapshot_names <- "ens-central-tendency-false"
  announce_plot_snapshot_files(snapshot_names)

  sims <- make_r_ens()
  pl <- plot(sims, central = "none")
  expect_plotly(pl)
  expect_true(nrow(plotly_traces(pl)) > 0)
  expect_snapshot_plot(snapshot_names, pl)
})

test_that("plot.ensemble_stockflow() show_legend = FALSE", {
  snapshot_names <- "ens-show_legend-false"
  announce_plot_snapshot_files(snapshot_names)

  sims <- make_r_ens()
  # Object-level expectation: no legend items when disabled
  pl_noleg <- plot(sims, show_legend = FALSE)
  expect_plotly(pl_noleg)
  traces <- plotly_traces(pl_noleg)
  expect_true(nrow(traces) > 0)
  expect_true(all(!(traces$show_legend)))

  # Snapshot last
  expect_snapshot_plot(snapshot_names, pl_noleg)
})


test_that("plot.ensemble_stockflow() label_subplots = TRUE shows condition labels", {
  snapshot_names <- "ens-label-subplots-true"
  announce_plot_snapshot_files(snapshot_names)

  sims <- make_r_ens_2cond()
  pl <- plot(sims, label_subplots = TRUE)
  expect_plotly(pl)
  layout <- plotly_layout(pl)
  annot <- unlist(lapply(layout$annotations, `[[`, "text"))
  expect_true(length(annot) > 1)
  expect_true(sum(grepl("^Condition", annot)) == 2)

  # Snapshot last
  expect_snapshot_plot(snapshot_names, pl)
})


test_that("plot.ensemble_stockflow() label_subplots = FALSE hides condition labels", {
  snapshot_names <- "ens-label-subplots-false"
  announce_plot_snapshot_files(snapshot_names)

  sims <- make_r_ens_2cond()
  pl <- plot(sims, label_subplots = FALSE)
  expect_plotly(pl)
  layout <- plotly_layout(pl)
  annot <- unlist(lapply(layout$annotations, `[[`, "text"))
  expect_true(length(annot) == 1) # Only annotation for time label
  expect_false(any(grepl("^Condition", annot)))

  # Snapshot last
  expect_snapshot_plot(snapshot_names, pl)
})

test_that("plot.ensemble_stockflow() nrows works", {
  snapshot_names <- c("ens-nrows-1", "ens-nrows-2")
  announce_plot_snapshot_files(snapshot_names)

  sims <- make_r_ens_2cond()
  pl <- plot(sims, nrows = 1L)
  expect_plotly(pl)

  info <- plotly_subplot_grid(pl)
  expect_true(info$is_subplot)
  expect_equal(info$nrows, 1L)
  expect_equal(info$ncols, 2L)

  pl <- plot(sims, nrows = 2L)
  expect_plotly(pl)

  info <- plotly_subplot_grid(pl)
  expect_true(info$is_subplot)
  expect_equal(info$nrows, 2L)
  expect_equal(info$ncols, 1L)

  expect_snapshot_plot(
    snapshot_names,
    list(
      plot(sims, nrows = 1L),
      plot(sims, nrows = 2L)
    )
  )
})

test_that("plot.ensemble_stockflow() shareX and shareY works", {
  snapshot_names <- c(
    "ens-sharex-true-sharey-true",
    "ens-sharex-true-sharey-false",
    "ens-sharex-false-sharey-true",
    "ens-sharex-false-sharey-false"
  )
  announce_plot_snapshot_files(snapshot_names)

  # 4 conditions
  n <- 3
  nrows <- 2
  sims <- make_r_ens(n = n, conditions = list(
    "contact_rate" = c(1.5, 2.5),
    "recovery_rate" = c(0.1, 0.2)
  ), cross = TRUE)

  pl1 <- plot(sims, shareX = TRUE, shareY = TRUE, nrows = nrows)
  info <- plotly_subplot_grid(pl1)
  expect_true(info$shareX)
  expect_true(info$shareY)

  pl2 <- plot(sims, shareX = TRUE, shareY = FALSE, nrows = nrows)
  info <- plotly_subplot_grid(pl2)
  expect_true(info$shareX)
  expect_false(info$shareY)

  pl3 <- plot(sims, shareX = FALSE, shareY = TRUE, nrows = nrows)
  info <- plotly_subplot_grid(pl3)
  expect_false(info$shareX)
  expect_true(info$shareY)

  pl4 <- plot(sims, shareX = FALSE, shareY = FALSE, nrows = nrows)
  info <- plotly_subplot_grid(pl4)
  expect_false(info$shareX)
  expect_false(info$shareY)

  # Snapshot last
  expect_snapshot_plot(snapshot_names, list(pl1, pl2, pl3, pl4))
})


test_that("plot.ensemble_stockflow() custom palette", {
  snapshot_names <- "ens-custom-palette"
  announce_plot_snapshot_files(snapshot_names)

  sims <- make_r_ens()
  pl <- plot(sims, palette = "Greens")
  expect_plotly(pl)
  expect_true(nrow(plotly_traces(pl)) > 0)
  expect_snapshot_plot(snapshot_names, pl)
})

test_that("plot() custom colors vector", {
  snapshot_names <- "ens-custom-colors"
  announce_plot_snapshot_files(snapshot_names)

  sims <- make_r_ens()
  # Object-level expectation: legend trace colors reflect custom palette when exposed
  df <- as.data.frame(sims, direction = "long")
  vars <- unique(df$variable)
  names_df <- as.data.frame(sims[["object"]])
  label_names <- names_df$label[match(vars, names_df$name)]
  custom_colors <- stats::setNames(rainbow(length(vars)), vars)
  expected_colors <- stats::setNames(unname(custom_colors), label_names)

  pl_colors <- plot(sims, colors = custom_colors, alpha = 1)
  expect_plotly(pl_colors)
  traces <- plotly_traces(pl_colors)
  expect_equal(length(unique(traces[["name"]])), length(label_names))
  legend_check <- plotly_check_legend_colors(pl_colors, expected = expected_colors)
  expect_true(nrow(legend_check) > 0)
  expect_true(all(legend_check$ok))
  expect_true(all(legend_check$matches_expected))

  # Snapshot last
  expect_snapshot_plot(snapshot_names, pl_colors)
})

test_that("plot.ensemble_stockflow() maps central traces to source summaries and named colors", {

  skip_on_cran()

  sims <- make_r_ens(n = 3)
  names_df <- as.data.frame(sims[["object"]])
  names_all <- names_df[["name"]]
  labels <- names_df[["label"]][match(unique(sims[["summary"]][["variable"]]), names_df[["name"]])]
  colors <- stats::setNames(grDevices::rainbow(length(names_all)), names_all)
  colors <- rev(colors)

  pl <- plot(sims, colors = colors, webgl = FALSE)
  built_traces <- plotly::plotly_build(pl)[["x"]][["data"]]
  trace_info <- plotly_traces(pl)
  label_to_name <- stats::setNames(names_df[["name"]], names_df[["label"]])

  for (trace_label in labels) {
    variable <- unname(label_to_name[[trace_label]])
    expected_y <- sims[["summary"]][["mean"]][sims[["summary"]][["variable"]] == variable]
    matching <- which(vapply(built_traces, function(trace) {
      identical(trace[["name"]], trace_label) &&
        length(trace[["y"]]) == length(expected_y) &&
        isTRUE(all.equal(as.numeric(trace[["y"]]), as.numeric(expected_y)))
    }, logical(1)))

    expect_length(matching, 1L)
    expect_equal(
      trace_info[["color"]][matching],
      normalize_color_string(colors[[variable]])
    )
  }
})

test_that("plot() custom font family", {
  snapshot_names <- "ens-custom-font-family"
  announce_plot_snapshot_files(snapshot_names)

  sims <- make_r_ens()
  pl <- plot(sims, font_family = "Arial")
  expect_plotly(pl)
  layout <- plotly_layout(pl)
  expect_equal(layout$font$family, "Arial")

  # Snapshot last
  expect_snapshot_plot(snapshot_names, pl)
})

test_that("plot() custom font size", {
  snapshot_names <- "ens-large-font-size"
  announce_plot_snapshot_files(snapshot_names)

  sims <- make_r_ens()
  pl <- plot(sims, font_size = 20)
  expect_plotly(pl)
  layout <- plotly_layout(pl)
  expect_equal(layout$font$size, 20)

  # Snapshot last
  expect_snapshot_plot(snapshot_names, pl)
})

test_that("plot() narrow wrap_width wraps long labels", {
  snapshot_names <- "ens-wrap-width-narrow"
  announce_plot_snapshot_files(snapshot_names)

  sims <- make_r_ens()
  pl <- plot(sims, wrap_width = 1, show_constants = TRUE)
  expect_plotly(pl)
  traces <- plotly_traces(pl)
  expect_true(any(grepl("<br", traces$name, fixed = TRUE)))
  expect_snapshot_plot(snapshot_names, pl)
})


test_that("plot.ensemble_stockflow() with show_constants = TRUE", {
  snapshot_names <- "ens-show-constants"
  announce_plot_snapshot_files(snapshot_names)

  sims <- make_r_ens()
  constants <- as.data.frame(sims[["object"]], type = "constants", properties = "label")
  pl <- plot(sims, show_constants = TRUE)

  expect_plotly(pl)
  traces <- plotly_traces(pl)
  expect_true(all(constants[["label"]] %in% traces[["name"]]))

  # Snapshot last
  expect_snapshot_plot(snapshot_names, pl)
})

test_that("plot.ensemble_stockflow() with which = 'sims' and with show_constants = TRUE", {
  snapshot_names <- "ens-show-constants-sims"
  announce_plot_snapshot_files(snapshot_names)

  sims <- make_r_ens(save_sims = TRUE)
  constants <- as.data.frame(sims[["object"]], type = "constants", properties = "label")
  pl <- plot(sims, show_constants = TRUE, which = "sims")

  expect_plotly(pl)
  traces <- plotly_traces(pl)
  expect_true(all(constants[["label"]] %in% traces[["name"]]))

  # Snapshot last
  expect_snapshot_plot(snapshot_names, pl)
})

# ============================================================================
# CONDITION DISPLAY AND TIME ANIMATION
# ============================================================================

test_that("plot.ensemble_stockflow(condition_display = 'slider') builds a slider", {
  skip_on_cran()
  
  sims <- make_r_ens_2cond()
  pl <- plot(sims, condition_display = "slider")
  expect_plotly(pl)

  layout <- plotly_layout(pl)
  expect_true(length(layout$sliders) > 0)
  expect_equal(length(layout$sliders[[1]]$steps), sims[["n_conditions"]])

  # Single parameter -> one control; steps swap the data (method = "restyle").
  # The parameter label lives in the slider title (currentvalue prefix), and the
  # tick labels carry only the bare parameter values.
  steps <- layout$sliders[[1]]$steps
  expect_true(all(vapply(steps, function(s) s$method, character(1)) == "restyle"))
  expect_match(layout$sliders[[1]]$currentvalue$prefix, "=")
  expect_false(grepl("=", steps[[1]]$label))

  # Only one condition's traces are drawn; the slider swaps their y data
  # client-side rather than hiding per-condition traces.
  expect_equal(
    length(plotly::plotly_build(pl)$x$data),
    length(plotly::plotly_build(plot(sims, condition = 1))$x$data)
  )
})

test_that("plot.ensemble_stockflow() shows one control per parameter when cross = TRUE", {
  skip_on_cran()

  sims <- make_r_ens(conditions = list(
    contact_rate = c(1, 2), recovery_rate = c(0.05, 0.1)
  ))
  expect_true(isTRUE(sims[["cross"]]))

  pl <- plot(sims, condition_display = "slider")
  layout <- plotly_layout(pl)
  expect_equal(length(layout$sliders), 2L) # one slider per parameter

  # Slider titles use the parameter labels; steps are the parameter's values.
  prefixes <- vapply(layout$sliders, function(s) s$currentvalue$prefix, character(1))
  expect_true(any(grepl("Contact rate", prefixes)))
  expect_true(any(grepl("Recovery rate", prefixes)))
  expect_equal(length(layout$sliders[[1]]$steps), 2L)

  # Cross-product swap requires the client-side hook.
  expect_false(is.null(pl$jsHooks$render))

  # Still only one condition's worth of traces.
  expect_equal(
    length(plotly::plotly_build(pl)$x$data),
    length(plotly::plotly_build(plot(sims, condition = 1))$x$data)
  )

  pd <- plot(sims, condition_display = "dropdown")
  expect_equal(length(plotly_layout(pd)$updatemenus), 2L)
  expect_false(is.null(pd$jsHooks$render))
})

test_that("plot.ensemble_stockflow() slider keeps a step per value but thins labels according to max_labels", {

  skip_on_cran()

  n_values <- 12
  max_labels <- 5
  sims <- make_r_ens(conditions = list(contact_rate = seq(1, n_values)))
  pl <- plot(sims, condition_display = "slider", control_options = list(max_labels = max_labels))
  layout <- plotly_layout(pl)

  steps <- layout$sliders[[1]]$steps
  # One jump per varied parameter value ...
  expect_equal(length(steps), n_values)

  labs <- vapply(steps, function(s) s$label, character(1))
  nonempty <- labs[nzchar(labs)]
  # ... but tick labels are thinned to a reasonable default when > 10 values.
  expect_lte(length(nonempty), max_labels)
  # Endpoints stay labelled.
  expect_true(nzchar(labs[[1]]))
  expect_true(nzchar(labs[[length(labs)]]))
})


test_that("plot.ensemble_stockflow() rejects invalid control_options", {

  skip_on_cran()

  sims <- make_r_ens_2cond()
  expect_error(
    plot(sims, condition_display = "slider", control_options = list(foo = 1)),
    "control_options"
  )
  expect_error(
    plot(sims, condition_display = "slider", control_options = list(max_labels = 0)),
    "max_labels"
  )
  expect_error(
    plot(sims, condition_display = "slider", control_options = list(spacing = 0)),
    "spacing"
  )
  expect_error(
    plot(sims, condition_display = "slider", control_options = list(spacing = "a")),
    "spacing"
  )
})

test_that("plot.ensemble_stockflow() control_options$spacing widens the gap", {

  skip_on_cran()

  sims <- make_r_ens(n = 3, conditions = list(
    "contact_rate" = c(1.5, 2.5),
    "recovery_rate" = c(0.1, 0.2)
  ), cross = TRUE)

  auto <- plot(sims, condition_display = "slider")
  wide <- plot(sims,
    condition_display = "slider",
    control_options = list(spacing = 150)
  )

  auto_y <- vapply(plotly_layout(auto)$sliders, function(s) s$y, numeric(1))
  wide_y <- vapply(plotly_layout(wide)$sliders, function(s) s$y, numeric(1))

  # One control per varied parameter, stacked downward (decreasing y).
  expect_equal(length(auto_y), 2L)
  expect_true(all(diff(auto_y) < 0))
  # A larger spacing pushes the second control further from the first.
  expect_gt(abs(diff(wide_y)), abs(diff(auto_y)))
})

test_that("plot.ensemble_stockflow() controls reserve more bottom margin per control", {
  
  skip_on_cran()

  one <- make_r_ens_2cond()
  two <- make_r_ens(n = 3, conditions = list(
    "contact_rate" = c(1.5, 2.5),
    "recovery_rate" = c(0.1, 0.2)
  ), cross = TRUE)

  m1 <- plotly_layout(plot(one, condition_display = "slider"))$margin$b
  m2 <- plotly_layout(plot(two, condition_display = "slider"))$margin$b
  # Two stacked controls must reserve more space than one.
  expect_gt(m2, m1)
})

test_that("plot.ensemble_stockflow() condition controls pin the figure height", {

  skip_on_cran()

  one <- make_r_ens_2cond()
  two <- make_r_ens(n = 3, conditions = list(
    "contact_rate" = c(1.5, 2.5),
    "recovery_rate" = c(0.1, 0.2)
  ), cross = TRUE)

  p1 <- plot(one, condition_display = "slider")
  p2 <- plot(two, condition_display = "slider")

  # The control offsets are paper coordinates, so the pixel gaps between
  # stacked controls are only guaranteed when the plot-area height is fixed:
  # widget and layout carry an explicit height that grows per control.
  expect_false(is.null(p1$height))
  expect_equal(plotly_layout(p1)$height, p1$height)
  expect_gt(p2$height, p1$height)

  # Subplot displays stay responsive (no pinned height).
  expect_null(plot(one)$height)

  # The condition slider spans the full plot width (no play button lane).
  expect_equal(plotly_layout(p1)$sliders[[1]]$len, 1)
})

test_that("plot.ensemble_stockflow() reverts condition controls for a single condition", {

  skip_on_cran()

  sims <- make_r_ens() # no conditions varied
  expect_message(
    pl <- plot(sims, condition_display = "slider"),
    "subplots"
  )
  expect_plotly(pl)
  expect_length(plotly_layout(pl)$sliders, 0)

  expect_message(
    pd <- plot(sims, condition_display = "dropdown"),
    "subplots"
  )
  expect_length(plotly_layout(pd)$updatemenus, 0)

  # Also reverts when filtering leaves a single condition.
  two <- make_r_ens_2cond()
  expect_message(
    plot(two, condition = 1, condition_display = "slider"),
    "subplots"
  )
  # But not when several conditions remain.
  expect_no_message(plot(two, condition_display = "slider"))
})

test_that("plot.ensemble_stockflow() crossed dropdowns react via plotly_buttonclicked", {

  skip_on_cran()
  
  sims <- make_r_ens(conditions = list(
    contact_rate = c(1, 2), recovery_rate = c(0.05, 0.1)
  ))
  pd <- plot(sims, condition_display = "dropdown")

  # Dropdown buttons use method = "skip" (no native relayout/restyle), so the
  # swap handler must listen for plotly_buttonclicked to react at all.
  js <- paste(
    vapply(pd$jsHooks$render, function(h) h$code, character(1)),
    collapse = ""
  )
  expect_match(js, "plotly_buttonclicked", fixed = TRUE)
})

test_that("plot.ensemble_stockflow(condition_display = 'dropdown') builds a dropdown", {

  skip_on_cran()

  sims <- make_r_ens_2cond()
  pl <- plot(sims, condition_display = "dropdown")
  expect_plotly(pl)

  layout <- plotly_layout(pl)
  expect_true(length(layout$updatemenus) > 0)
  expect_equal(length(layout$updatemenus[[1]]$buttons), sims[["n_conditions"]])
})

test_that("plot.ensemble_stockflow() condition controls work with which = 'sims'", {

  skip_on_cran()

  sims <- make_r_ens_2cond(save_sims = TRUE)
  pl <- plot(sims, which = "sims", condition_display = "slider")
  expect_plotly(pl)
  expect_true(length(plotly_layout(pl)$sliders) > 0)
})

test_that("plot.ensemble_stockflow(animation = 'time') builds frames for one condition", {

  skip_on_cran()

  sims <- make_r_ens(save_sims = TRUE)
  pl <- plot(sims, which = "sims", animation = "time")
  expect_plotly(pl)
  expect_true(length(plotly_frames(pl)) > 0)
})

test_that("plot.ensemble_stockflow() animates a single selected condition", {

  skip_on_cran()

  sims <- make_r_ens_2cond(save_sims = TRUE)
  pl <- plot(sims, which = "sims", condition = 1, animation = "time")
  expect_plotly(pl)
  expect_true(length(plotly_frames(pl)) > 0)
})

test_that("plot.ensemble_stockflow() control_options tune the animation speed", {
  
  skip_on_cran()
  
  sims <- make_r_ens(save_sims = TRUE)
  pl <- plot(sims,
    which = "sims", animation = "time",
    control_options = list(frame_ms = 40, max_frames = 10)
  )
  expect_plotly(pl)
  opts <- plotly_layout(pl)$updatemenus[[1]]$buttons[[1]]$args[[2]]
  expect_equal(opts$frame$duration, 40)
  expect_lte(length(plotly_frame_names(pl)), 10)
})

test_that("plot.ensemble_stockflow() rejects invalid / unsupported combinations", {
  skip_on_cran()

  sims <- make_r_ens_2cond(save_sims = TRUE)
  expect_error(plot(sims, condition_display = "tabs"), "condition_display")
  expect_error(plot(sims, animation = "fast"), "animation")
  expect_error(
    plot(sims, which = "sims", animation = "time", condition_display = "slider"),
    "not supported"
  )
  # Multiple conditions (subplots) cannot be animated together
  expect_error(plot(sims, which = "sims", animation = "time"), "not supported|multiple conditions")
  # Summary confidence ribbons cannot be animated
  expect_error(plot(sims, which = "summary", condition = 1, animation = "time"), "not supported|ribbons")
})

test_that("plot.ensemble_stockflow() webgl toggles trace type for which = 'sims'", {

  skip_on_cran()

  sims <- make_r_ens(n = 5, save_sims = TRUE)

  pl_gl <- plot(sims, which = "sims", webgl = TRUE)
  types_gl <- vapply(plotly::plotly_build(pl_gl)$x$data, function(d) d$type %||% "", character(1))
  expect_true(any(types_gl == "scattergl"))

  pl_svg <- plot(sims, which = "sims", webgl = FALSE)
  data_svg <- plotly::plotly_build(pl_svg)$x$data
  types_svg <- vapply(data_svg, function(d) d$type %||% "", character(1))
  expect_false(any(types_svg == "scattergl"))
  # Per-sim traces produce more traces than the webgl single-trace-per-variable
  expect_gt(length(data_svg), length(plotly::plotly_build(pl_gl)$x$data))
})

test_that("plot.ensemble_stockflow() obeys global webgl option", {

  skip_on_cran()

  sims <- make_r_ens(n = 5, save_sims = TRUE)

  withr::local_options(list(sdbuildR.webgl = TRUE))
  pl_gl <- plot(sims, which = "sims")
  types_gl <- vapply(plotly::plotly_build(pl_gl)$x$data, function(d) d$type %||% "", character(1))
  expect_true(any(types_gl == "scattergl"))

  withr::local_options(list(sdbuildR.webgl = FALSE))
  pl_svg <- plot(sims, which = "sims")
  data_svg <- plotly::plotly_build(pl_svg)$x$data
  types_svg <- vapply(data_svg, function(d) d$type %||% "", character(1))
  expect_false(any(types_svg == "scattergl"))
  # Per-sim traces produce more traces than the webgl single-trace-per-variable
  expect_gt(length(data_svg), length(plotly::plotly_build(pl_gl)$x$data))

  # Setting webgl explicitly overrides the global option
  pl_gl2 <- plot(sims, which = "sims", webgl = TRUE)
  types_gl2 <- vapply(plotly::plotly_build(pl_gl2)$x$data, function(d) d$type %||% "", character(1))
  expect_true(any(types_gl2 == "scattergl"))
})


test_that("plot.ensemble_stockflow() rejects non-logical webgl", {

  skip_on_cran()

  sims <- make_r_ens(n = 3, save_sims = TRUE)
  expect_error(plot(sims, which = "sims", webgl = "yes"), "webgl")
})
