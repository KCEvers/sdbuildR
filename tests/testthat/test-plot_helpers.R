test_that("validate_plot_params passes on NULL inputs", {
  expect_invisible(validate_plot_params())
  expect_invisible(validate_plot_params(
    showlegend = NULL, vars = NULL, palette = NULL,
    colors = NULL, font_family = NULL, font_size = NULL, wrap_width = NULL
  ))
})

test_that("validate_plot_params passes on valid showlegend", {
  expect_invisible(validate_plot_params(showlegend = TRUE))
  expect_invisible(validate_plot_params(showlegend = FALSE))
})

test_that("validate_plot_params rejects non-logical showlegend", {
  expect_error(
    validate_plot_params(showlegend = "yes"),
    "showlegend.*logical"
  )
  expect_error(
    validate_plot_params(showlegend = 1),
    "showlegend.*logical"
  )
})

test_that("validate_plot_params passes on valid vars", {
  expect_invisible(validate_plot_params(vars = c("var1", "var2")))
  expect_invisible(validate_plot_params(vars = "single_var"))
})

test_that("validate_plot_params rejects non-character vars", {
  expect_error(
    validate_plot_params(vars = 123),
    "vars.*character"
  )
  expect_error(
    validate_plot_params(vars = c(TRUE, FALSE)),
    "vars.*character"
  )
})

test_that("validate_plot_params rejects empty vars vector", {
  expect_error(
    validate_plot_params(vars = character(0)),
    "Empty.*vars"
  )
})

test_that("validate_plot_params passes on valid palette", {
  expect_invisible(validate_plot_params(palette = "Dark 2"))
  expect_invisible(validate_plot_params(palette = "Set 1"))
})

test_that("validate_plot_params rejects non-character palette", {
  expect_error(
    validate_plot_params(palette = 123),
    "palette.*character"
  )
})

test_that("validate_plot_params passes on valid colors", {
  expect_invisible(validate_plot_params(colors = c("#FF0000", "#00FF00")))
  expect_invisible(validate_plot_params(colors = "red"))
})

test_that("validate_plot_params rejects non-character colors", {
  expect_error(
    validate_plot_params(colors = 123),
    "colors.*character"
  )
})

test_that("validate_plot_params passes on valid font parameters", {
  expect_invisible(validate_plot_params(
    font_family = "Arial",
    font_size = 12,
    wrap_width = 25
  ))
})

test_that("validate_plot_params rejects non-character font_family", {
  expect_error(
    validate_plot_params(font_family = 123),
    "font_family.*character"
  )
})

test_that("validate_plot_params rejects non-positive font_size", {
  expect_error(
    validate_plot_params(font_size = 0),
    "font_size.*positive"
  )
  expect_error(
    validate_plot_params(font_size = -5),
    "font_size.*positive"
  )
})

test_that("validate_plot_params rejects non-positive wrap_width", {
  expect_error(
    validate_plot_params(wrap_width = 0),
    "wrap_width.*positive"
  )
  expect_error(
    validate_plot_params(wrap_width = -10),
    "wrap_width.*positive"
  )
})

# ============================================================================
# prepare_labels TESTS
# ============================================================================

test_that("prepare_labels returns data frame with same structure", {
  names_df <- data.frame(
    name = c("var1", "var2"),
    label = c("Variable 1", "Variable 2"),
    type = c("stock", "aux")
  )

  result <- prepare_labels(names_df, wrap_width = 25)

  expect_equal(nrow(result), nrow(names_df))
  expect_equal(colnames(result), colnames(names_df))
})

test_that("prepare_labels removes underscores when format_label = TRUE", {
  names_df <- data.frame(
    name = "var_1",
    label = "var_1",
    type = "stock"
  )

  result <- prepare_labels(names_df, wrap_width = 25, format_label = TRUE)

  expect_equal(result$label, "var 1")
})

test_that("prepare_labels removes periods when format_label = TRUE", {
  names_df <- data.frame(
    name = "var.1",
    label = "var.1",
    type = "stock"
  )

  result <- prepare_labels(names_df, wrap_width = 25, format_label = TRUE)

  expect_equal(result$label, "var 1")
})

test_that("prepare_labels preserves custom labels when format_label = TRUE", {
  names_df <- data.frame(
    name = "var_1",
    label = "Custom Label",
    type = "stock"
  )

  result <- prepare_labels(names_df, wrap_width = 25, format_label = TRUE)

  # Should not modify custom labels, only name-matching labels
  expect_equal(result$label, "Custom Label")
})

test_that("model_label_lookup prettifies names when no custom label is set", {
  # A constant created without a label defaults its label to the raw name; the
  # slider/dropdown lookup should still show a prettified label (matching the
  # rest of a plot), not the underscored name.
  sfm <- stockflow() |>
    stock(energy, eqn = 1) |>
    flow(recovery, eqn = "recovery_rate * energy", to = energy) |>
    constant(recovery_rate, eqn = 0.75)

  f <- model_label_lookup(sfm)
  expect_equal(f("recovery_rate"), "recovery rate")
  # Vectorised lookup works and leaves single-word names untouched.
  expect_equal(f(c("recovery_rate", "energy")), c("recovery rate", "energy"))
})

test_that("model_label_lookup keeps custom labels and falls back without a model", {
  sfm <- stockflow("sir") # built-in model sets "Contact rate", etc.
  f <- model_label_lookup(sfm)
  expect_equal(f("contact_rate"), "Contact rate")

  # No model: prettify the bare name.
  expect_equal(model_label_lookup(NULL)("recovery_rate"), "recovery rate")
})

test_that("model_label_lookup format = FALSE keeps the raw name", {
  sfm <- stockflow() |>
    stock(energy, eqn = 1) |>
    flow(recovery, eqn = "recovery_rate * energy", to = energy) |>
    constant(recovery_rate, eqn = 0.75)

  f <- model_label_lookup(sfm, format = FALSE)
  expect_equal(f("recovery_rate"), "recovery_rate")
  # Falls back to the raw name with no model too.
  expect_equal(model_label_lookup(NULL, format = FALSE)("recovery_rate"), "recovery_rate")
})

test_that("prepare_labels detects and handles duplicate labels", {
  names_df <- data.frame(
    name = c("var1", "var2"),
    label = c("Same", "Same"),
    type = c("stock", "aux")
  )

  result <- prepare_labels(names_df, wrap_width = 25)

  # Duplicate labels should be disambiguated
  expect_equal(length(unique(result$label)), 2)
  expect_true(all(grepl("\\(var", result$label))) # Should have (var1), (var2)
})

test_that("prepare_labels wraps long labels", {
  names_df <- data.frame(
    name = "longvar",
    label = "This is a very long label that should be wrapped",
    type = "stock"
  )

  result <- prepare_labels(names_df, wrap_width = 10)

  # Wrapped label should contain newlines
  expect_true(grepl("\n", result$label))
})

test_that("prepare_labels does not wrap when width is large", {
  names_df <- data.frame(
    name = "var",
    label = "Short",
    type = "stock"
  )

  result <- prepare_labels(names_df, wrap_width = 100)

  # Should not wrap short label
  expect_false(grepl("\n", result$label))
})

test_that("prepare_labels escapes single quotes for Graphviz", {
  names_df <- data.frame(
    name = "var",
    label = "Label with 'quotes'",
    type = "stock"
  )

  result <- prepare_labels(names_df, wrap_width = 25)

  # Single quotes should be escaped
  expect_true(grepl("\\\\'", result$label))
})

# ============================================================================
# plotly_theme() tests
# ============================================================================

test_that("plotly_theme returns list with expected structure", {
  theme <- plotly_theme()

  expect_type(theme, "list")
  expect_true(all(c("font", "margin", "legend", "xaxis", "yaxis") %in% names(theme)))
})

test_that("plotly_theme uses custom font family", {
  theme <- plotly_theme(font_family = "Arial")

  expect_equal(theme$font$family, "Arial")
})

test_that("plotly_theme uses custom font size", {
  theme <- plotly_theme(font_size = 20)

  expect_equal(theme$font$size, 20)
})

test_that("plotly_theme sets margins correctly", {
  theme <- plotly_theme(margin_t = 100, margin_b = 50, margin_l = 75, margin_r = 25)

  expect_equal(theme$margin$t, 100)
  expect_equal(theme$margin$b, 50)
  expect_equal(theme$margin$l, 75)
  expect_equal(theme$margin$r, 25)
})

test_that("plotly_theme legend has reversed trace order", {
  theme <- plotly_theme()

  expect_equal(theme$legend$traceorder, "reversed")
})

test_that("plotly_theme legend font scales with base font size", {
  theme <- plotly_theme(font_size = 20, legend_font_scale = 0.85)

  expect_equal(theme$legend$font$size, ceiling(20 * 0.85))
})

# ============================================================================
# extract_plot_params TESTS
# ============================================================================

test_that("extract_plot_params returns defaults when dots is empty", {
  defaults <- list(main = "Default Title", xlab = "X", ylab = "Y")

  result <- extract_plot_params(list(), defaults)

  expect_equal(result$main, "Default Title")
  expect_equal(result$xlab, "X")
  expect_equal(result$ylab, "Y")
})

test_that("extract_plot_params overrides defaults with provided values", {
  defaults <- list(main = "Default Title", xlab = "X", ylab = "Y")
  dots <- list(main = "Custom Title", xlab = "Time")

  result <- extract_plot_params(dots, defaults)

  expect_equal(result$main, "Custom Title")
  expect_equal(result$xlab, "Time")
  expect_equal(result$ylab, "Y") # Not overridden
})

test_that("extract_plot_params ignores unknown parameters in defaults", {
  defaults <- list(main = "Title", xlab = "X")
  dots <- list(main = "New", unknown_param = "value")

  result <- extract_plot_params(dots, defaults)

  expect_equal(result$main, "New")
  expect_equal(length(names(result)), 2) # Only defaults
})

# ============================================================================
# generate_colors TESTS
# ============================================================================

test_that("generate_colors returns correct number of colors", {
  colors <- generate_colors(5, palette = "Dark 2")

  expect_equal(length(colors), 5)
})

test_that("generate_colors returns character vector", {
  colors <- generate_colors(3)

  expect_type(colors, "character")
})

test_that("generate_colors uses custom colors when provided", {
  custom <- c("#FF0000", "#00FF00", "#0000FF")

  result <- generate_colors(3, colors = custom)

  expect_equal(unname(result), custom)
})

test_that("generate_colors truncates excess custom colors", {
  custom <- c("#FF0000", "#00FF00", "#0000FF", "#FFFF00")

  result <- generate_colors(2, colors = custom)

  expect_equal(length(result), 2)
  expect_equal(unname(result), custom[1:2])
})

test_that("generate_colors errors on insufficient colors", {
  custom <- c("#FF0000", "#00FF00")

  expect_error(
    generate_colors(5, colors = custom),
    "Insufficient colors"
  )
})

test_that("generate_colors uses minimum of 3 for palette generation", {
  colors <- generate_colors(1, palette = "Dark 2")

  expect_equal(length(colors), 1)
})

test_that("generate_colors respects palette parameter", {
  colors1 <- generate_colors(5, palette = "Dark 2")
  colors2 <- generate_colors(5, palette = "Viridis")

  # Different palettes should produce different colors
  expect_false(identical(colors1, colors2))
})

# ============================================================================
# determine_highlight_vars TESTS
# ============================================================================

test_that("determine_highlight_vars returns stocks with 'auto' strategy", {
  names_df <- data.frame(
    name = c("S", "I", "R", "beta", "constant"),
    type = c("stock", "stock", "stock", "aux", "constant")
  )

  result <- determine_highlight_vars(names_df, "auto")

  expect_equal(result, c("S", "I", "R"))
})

test_that("determine_highlight_vars returns all with 'all' strategy", {
  names_df <- data.frame(
    name = c("S", "I", "aux"),
    type = c("stock", "stock", "aux")
  )

  result <- determine_highlight_vars(names_df, "all")

  expect_equal(length(result), 3)
})

test_that("determine_highlight_vars returns empty with 'none' strategy", {
  names_df <- data.frame(
    name = c("S", "I"),
    type = c("stock", "stock")
  )

  result <- determine_highlight_vars(names_df, "none")

  expect_equal(length(result), 0)
})

test_that("determine_highlight_vars accepts custom variable list", {
  names_df <- data.frame(
    name = c("S", "I", "R"),
    type = c("stock", "stock", "stock")
  )

  custom_vars <- c("S", "aux")
  result <- determine_highlight_vars(names_df, custom_vars)

  expect_equal(result, custom_vars)
})

test_that("determine_highlight_vars errors on invalid strategy", {
  names_df <- data.frame(
    name = c("S", "I"),
    type = c("stock", "stock")
  )

  expect_error(
    determine_highlight_vars(names_df, "invalid"),
    "Invalid.*highlight_strategy"
  )
})

# ============================================================================
# filter_variables TESTS
# ============================================================================

test_that("filter_variables keeps only specified variables", {
  names_df <- data.frame(
    name = c("S", "I", "R"),
    type = c("stock", "stock", "stock")
  )

  df <- data.frame(
    variable = c("S", "S", "I", "I", "R", "R"),
    value = 1:6
  )

  result <- filter_variables(c("S", "I"), names_df, df)

  expect_equal(nrow(result$names_df), 2)
  expect_equal(nrow(result$df), 4)
  expect_true(all(result$df$variable %in% c("S", "I")))
})

test_that("filter_variables errors on invalid variables", {
  names_df <- data.frame(
    name = c("S", "I"),
    type = c("stock", "stock")
  )

  df <- data.frame(
    variable = c("S", "I"),
    value = 1:2
  )

  expect_error(
    filter_variables(c("S", "NonExistent"), names_df, df),
    "not.*variable"
  )
})

# ============================================================================
# prep_constants TESTS
# ============================================================================

test_that("prep_constants adds constants to sim-type dataframe", {
  df <- data.frame(
    variable = c("S", "S"),
    time = c(0, 1),
    value = c(100, 95)
  )

  names_df <- data.frame(
    name = c("S", "const1"),
    type = c("stock", "constant")
  )

  constants <- list(const1 = 50)

  result <- prep_constants(df, constants, names_df, type_sim = "sim")

  expect_true(nrow(result$df) > nrow(df))
  expect_true("const1" %in% unique(result$df$variable))
})

test_that("prep_constants removes functions from sim-type constants", {
  df <- data.frame(
    variable = "S",
    time = 0,
    value = 100
  )

  names_df <- data.frame(
    name = c("S", "func_const"),
    type = c("stock", "constant")
  )

  constants <- list(const1 = 50, func_const = function() 100)

  result <- prep_constants(df, constants, names_df, type_sim = "sim")

  # Function constant should be removed
  expect_false("func_const" %in% unique(result$df$variable))
})

test_that("prep_constants handles empty constants", {
  df <- data.frame(variable = "S", time = 0, value = 100)
  names_df <- data.frame(name = "S", type = "stock")

  result <- prep_constants(df, list(), names_df, type_sim = "sim")

  expect_equal(nrow(result$df), nrow(df))
})

# ============================================================================
# validate_vars_in_model TESTS
# ============================================================================

test_that("validate_vars_in_model passes on NULL vars", {
  names_df <- data.frame(name = c("S", "I"), type = c("stock", "stock"))

  expect_invisible(validate_vars_in_model(NULL, names_df))
})

test_that("validate_vars_in_model validates variable existence", {
  names_df <- data.frame(name = c("S", "I"), type = c("stock", "stock"))

  expect_error(
    validate_vars_in_model(c("S", "NonExistent"), names_df),
    "not.*variable"
  )
})

test_that("validate_vars_in_model validates presence in dataframe", {
  names_df <- data.frame(name = c("S", "I"), type = c("stock", "stock"))
  df <- data.frame(variable = "S", value = 100)

  expect_error(
    validate_vars_in_model(c("S", "I"), names_df, df),
    "not saved in the output"
  )
})

test_that("validate_vars_in_model passes on valid variables", {
  names_df <- data.frame(name = c("S", "I"), type = c("stock", "stock"))
  df <- data.frame(variable = c("S", "I"), value = c(100, 50))

  expect_invisible(validate_vars_in_model(c("S", "I"), names_df, df))
})

# ============================================================================
# accumulate_by_time TESTS
# ============================================================================

test_that("accumulate_by_time reveals cumulatively and starts at the first time", {
  df <- data.frame(
    time = rep(0:10, times = 2),
    variable = rep(c("a", "b"), each = 11),
    value = 1
  )

  out <- accumulate_by_time(df)
  frames <- sort(unique(out[[".frame"]]))

  # The animation starts at the first time point (an empty plot: a single point
  # per series draws nothing under mode = "lines") and reveals cumulatively.
  expect_equal(frames, 0:10)
  expect_equal(sort(unique(out[out[[".frame"]] == 0, "time"])), 0)

  # Cumulative reveal: the last frame contains all rows
  expect_equal(nrow(out[out[[".frame"]] == 10, ]), nrow(df))
})

test_that("accumulate_by_time backfills non-finite starting values in the first frame", {
  # A series that is NaN at the first time point (e.g. a 0/0 ratio) would be an
  # all-NaN trace in the first frame, which plotly drops from the initial data
  # but pads back into the frame -- a trace-count mismatch that warns. It is
  # backfilled with the series' first finite value so the (invisible) trace
  # stays present.
  df <- data.frame(
    time = rep(0:3, times = 2),
    variable = rep(c("a", "ratio"), each = 4),
    value = c(1, 1, 1, 1, NaN, 0.9, 0.8, 0.7)
  )

  out <- accumulate_by_time(df)
  first <- out[out[[".frame"]] == 0, ]

  # Every series has a finite value in the first frame (ratio backfilled to 0.9)
  expect_true(all(is.finite(first[["value"]])))
  expect_equal(first[first[["variable"]] == "ratio", "value"], 0.9)

  # Later frames keep the real (non-finite) starting value
  ratio_f3 <- out[out[[".frame"]] == 3 & out[["variable"]] == "ratio", ]
  expect_true(is.nan(ratio_f3[ratio_f3[["time"]] == 0, "value"]))
})

test_that("accumulate_by_time leaves a never-finite series untouched", {
  df <- data.frame(
    time = rep(0:3, times = 2),
    variable = rep(c("a", "empty"), each = 4),
    value = c(1, 2, 3, 4, NaN, NaN, NaN, NaN)
  )

  out <- accumulate_by_time(df)
  empty_first <- out[out[[".frame"]] == 0 & out[["variable"]] == "empty", "value"]
  expect_true(all(is.nan(empty_first)))
})

test_that("accumulate_by_time caps frames while keeping first and last time", {
  df <- data.frame(time = seq(0, 10, by = 0.01), variable = "a", value = 1)

  out <- accumulate_by_time(df, max_frames = 50)
  frames <- sort(unique(out[[".frame"]]))

  expect_lte(length(frames), 50)
  expect_equal(frames[1], min(df$time))
  expect_equal(frames[length(frames)], max(df$time))
})
