# ============================================================================
# BASIC FUNCTIONALITY TESTS
# ============================================================================

test_that("plot() warns on empty model", {
  sfm <- stockflow()

  expect_warning(plot(sfm), "Model contains no variables")
})

test_that("plot() method exists for stockflow objects", {
  # Check that plot method exists
  expect_true("plot.stockflow" %in% methods("plot"))
})

test_that("plot() returns DiagrammeR grViz object", {
  sfm <- stockflow("sir")

  result <- plot(sfm)

  # Should return an htmlwidget (DiagrammeR graph)
  expect_true("grViz" %in% class(result))
  expect_true("htmlwidget" %in% class(result))
})

# ============================================================================
# PARAMETER VALIDATION TESTS
# ============================================================================

test_that("plot() checks vars argument", {
  skip_on_cran()

  sfm <- stockflow("sir")

  expect_error(
    plot(sfm, vars = 123),
    "vars"
  )
  expect_error(
    plot(sfm, vars = character(0)),
    "Invalid"
  )
  expect_error(
    plot(sfm, vars = c("susceptible", "NonExistentVar")),
    "NonExistentVar.*not.*variable"
  )
})

# ============================================================================
# VISUAL REGRESSION TESTS (expect_snapshot_plot)
# ============================================================================

test_that("plot() creates diagram for SIR template", {
  snapshot_names <- "stockflow-SIR-model-diagram"
  announce_plot_snapshot_files(snapshot_names, type = "grViz")

  sfm <- stockflow("sir")
  pl <- plot(sfm, show_aux = FALSE, show_constants = FALSE)
  nodes <- extract_diagram_nodes(pl)
  edges <- extract_diagram_edges(pl)
  df <- as.data.frame(sfm, properties = "eqn")
  var_names <- df$name[df$type %in% c("stock", "flow")]
  expect_setequal(nodes$name, var_names)
  expect_true(all(edges$from %in% var_names))
  expect_true(all(edges$to %in% var_names))
  expect_true(length(unique(nodes$id)) == nrow(nodes))

  expect_snapshot_plot(snapshot_names, pl)
})

test_that("plot() creates diagram for simple single-stock model", {
  snapshot_names <- "stockflow-simple-stock-flow"
  announce_plot_snapshot_files(snapshot_names, type = "grViz")

  sfm <- stockflow()
  sfm1 <- update(sfm, "Stock1", type = "stock", label = "Population")
  sfm2 <- update(sfm1, "Flow1", type = "flow", label = "Birth", from = "Stock1")
  pl <- plot(sfm2, show_aux = FALSE, show_constants = FALSE)
  nodes <- extract_diagram_nodes(pl)
  edges <- extract_diagram_edges(pl)
  var_names <- c("Stock1", "Flow1", "Cloud1")
  expect_setequal(nodes$name, var_names)
  expect_true(all(edges$from %in% var_names))
  expect_true(all(edges$to %in% var_names))
  expect_true(length(unique(nodes$id)) == nrow(nodes))

  expect_snapshot_plot(snapshot_names, pl)
})

test_that("plot() creates diagram with auxiliary variables and dependencies", {
  snapshot_names <- "stockflow-diagram-with-dependencies"
  announce_plot_snapshot_files(snapshot_names, type = "grViz")

  sfm <- stockflow()
  sfm1 <- update(sfm, "S", type = "stock")
  sfm2 <- update(sfm1, "I", type = "stock")
  sfm3 <- update(sfm2, "infection_rate", type = "aux", eqn = "S * I * 0.001")

  pl <- plot(sfm3, show_aux = TRUE)
  nodes <- extract_diagram_nodes(pl)
  edges <- extract_diagram_edges(pl)
  var_names <- c("S", "I", "infection_rate")
  expect_setequal(nodes$name, var_names)
  expect_true(all(edges$from %in% var_names))
  expect_true(all(edges$to %in% var_names))
  expect_true(length(unique(nodes$id)) == nrow(nodes))

  expect_snapshot_plot(snapshot_names, pl)
})

test_that("plot() with show_dependencies = FALSE hides dependency arrows", {
  snapshot_names <- "stockflow-no-dependencies"
  announce_plot_snapshot_files(snapshot_names, type = "grViz")

  sfm <- stockflow()
  sfm1 <- update(sfm, "S", type = "stock")
  sfm2 <- update(sfm1, "aux1", type = "aux", eqn = "S * 2")

  pl <- plot(sfm2, show_dependencies = FALSE)
  nodes <- extract_diagram_nodes(pl)
  edges <- extract_diagram_edges(pl)
  var_names <- c("S", "aux1")
  expect_setequal(nodes$name, var_names)
  expect_true(length(unique(nodes$id)) == nrow(nodes))
  expect_true(nrow(edges) == 0)

  expect_snapshot_plot(snapshot_names, pl)
})

test_that("plot() with show_constants = TRUE displays constants", {
  snapshot_names <- "stockflow-with-constants"
  announce_plot_snapshot_files(snapshot_names, type = "grViz")

  sfm <- stockflow()
  sfm1 <- update(sfm, "Stock1", type = "stock")
  sfm2 <- update(sfm1, "const1", type = "constant", eqn = "5")

  pl <- plot(sfm2, show_constants = TRUE)
  nodes <- extract_diagram_nodes(pl)
  edges <- extract_diagram_edges(pl)
  var_names <- c("Stock1", "const1")
  expect_setequal(nodes$name, var_names)
  expect_true(nrow(edges) == 0)

  expect_snapshot_plot(snapshot_names, pl)
})

test_that("plot() with show_constants = FALSE hides constants", {
  snapshot_names <- "stockflow-without-constants"
  announce_plot_snapshot_files(snapshot_names, type = "grViz")

  sfm <- stockflow()
  sfm1 <- update(sfm, "Stock1", type = "stock")
  sfm2 <- update(sfm1, "const1", type = "constant", eqn = "5")
  pl <- plot(sfm2, show_constants = FALSE)
  nodes <- extract_diagram_nodes(pl)
  edges <- extract_diagram_edges(pl)
  var_names <- c("Stock1")
  expect_setequal(nodes$name, var_names)
  expect_true(nrow(edges) == 0)
  expect_true(length(unique(nodes$id)) == nrow(nodes))

  expect_snapshot_plot(snapshot_names, pl)
})

test_that("plot() with show_aux = FALSE hides auxiliary variables", {
  snapshot_names <- "stockflow-no-auxiliaries"
  announce_plot_snapshot_files(snapshot_names, type = "grViz")

  sfm <- stockflow("sir")
  pl <- plot(sfm, show_aux = FALSE, show_constants = TRUE)
  nodes <- extract_diagram_nodes(pl)
  edges <- extract_diagram_edges(pl)
  df <- as.data.frame(sfm, properties = "eqn")
  var_names <- df$name[df$type %in% c("stock", "flow", "constant")]
  expect_setequal(nodes$name, var_names)
  expect_true(all(edges$from %in% var_names))
  expect_true(all(edges$to %in% var_names))
  expect_true(length(unique(nodes$id)) == nrow(nodes))

  expect_snapshot_plot(snapshot_names, pl)
})

test_that("plot() filters variables correctly", {
  snapshot_names <- c("stockflow-filtered-variables", "stockflow-single-variable-filter")
  announce_plot_snapshot_files(snapshot_names, type = "grViz")

  sfm <- stockflow("sir")
  var_names <- c("susceptible", "infected")
  pl <- plot(sfm, vars = var_names, show_aux = TRUE, show_constants = TRUE)
  nodes <- extract_diagram_nodes(pl)
  edges <- extract_diagram_edges(pl)
  expect_setequal(nodes$name, var_names)
  expect_true(all(edges$from %in% var_names))
  expect_true(all(edges$to %in% var_names))
  expect_true(length(unique(nodes$id)) == nrow(nodes))

  pl_filtered <- pl

  var_names <- c("susceptible")
  pl <- plot(sfm, vars = var_names, show_aux = TRUE, show_constants = TRUE)
  nodes <- extract_diagram_nodes(pl)
  edges <- extract_diagram_edges(pl)
  expect_setequal(nodes$name, var_names)
  expect_true(length(unique(nodes$id)) == nrow(nodes))
  expect_true(nrow(edges) == 0)

  expect_snapshot_plot(snapshot_names, list(pl_filtered, pl))
})

test_that("plot() applies custom stock color", {
  snapshot_names <- "stockflow-custom-stock-color"
  announce_plot_snapshot_files(snapshot_names, type = "grViz")

  sfm <- stockflow("sir")
  stock_color <- "#FF6B6B"
  df <- as.data.frame(sfm, properties = "type")
  stock_names <- df$name[df$type == "stock"]
  pl <- plot(sfm, colors = list(stock = stock_color))
  nodes <- extract_diagram_nodes(pl)
  stock_nodes <- nodes[nodes$name %in% stock_names, ]
  expect_equal(nrow(stock_nodes), length(stock_names))
  expect_true(all(stock_nodes$fillcolor == stock_color))

  expect_snapshot_plot(snapshot_names, pl)
})

test_that("plot() applies custom flow color", {
  snapshot_names <- "stockflow-custom-flow-color"
  announce_plot_snapshot_files(snapshot_names, type = "grViz")

  sfm <- stockflow("sir")
  flow_color <- "#4ECDC4"
  pl <- plot(sfm, colors = list(flow = flow_color))
  d <- pl[["x"]][["diagram"]]

  # Flows are drawn as black-bordered bands in the flow colour.
  expect_true(grepl(paste0("black:", flow_color, ":black"), d, fixed = TRUE))

  expect_snapshot_plot(snapshot_names, pl)
})

# ============================================================================
# colors ARGUMENT TESTS
# ============================================================================

# Model with all four variable types for colour tests
typed_model <- function() {
  sfm <- stockflow()
  sfm <- update(sfm, "S", type = "stock", eqn = "100")
  sfm <- update(sfm, "growth", type = "flow", to = "S", eqn = "r * a")
  sfm <- update(sfm, "r", type = "constant", eqn = "0.1")
  sfm <- update(sfm, "a", type = "aux", eqn = "S * 2")
  sfm
}

test_that("plot() colors: single colour applies to all variable types", {
  sfm <- typed_model()
  pl <- plot(sfm, colors = "red", show_constants = TRUE, show_aux = TRUE)
  nodes <- extract_diagram_nodes(pl)

  # Stocks, constants, and auxiliaries are filled with the (normalised) colour
  fill_nodes <- nodes[nodes$name %in% c("S", "r", "a"), ]
  expect_equal(nrow(fill_nodes), 3L)
  expect_true(all(fill_nodes$fillcolor == "#FF0000"))

  # Flows carry the colour in their edge bands
  expect_true(grepl("black:#FF0000:black", pl[["x"]][["diagram"]], fixed = TRUE))
})

test_that("plot() colors: list by type overrides only that type", {
  sfm <- typed_model()
  pl <- plot(sfm, colors = list(stock = "red"), show_constants = TRUE, show_aux = TRUE)
  nodes <- extract_diagram_nodes(pl)

  expect_equal(nodes$fillcolor[nodes$name == "S"], "#FF0000")
  # Other types keep their defaults (grey90 = #E5E5E5, flow = #F48153)
  expect_true(all(nodes$fillcolor[nodes$name %in% c("r", "a")] == "#E5E5E5"))
  expect_true(grepl("black:#F48153:black", pl[["x"]][["diagram"]], fixed = TRUE))
})

test_that("plot() colors: list can set every type at once", {
  sfm <- typed_model()
  pl <- plot(sfm,
    colors = list(
      stock = "#111111", flow = "#222222",
      constant = "#333333", aux = "#444444"
    ),
    show_constants = TRUE, show_aux = TRUE
  )
  nodes <- extract_diagram_nodes(pl)

  expect_equal(nodes$fillcolor[nodes$name == "S"], "#111111")
  expect_equal(nodes$fillcolor[nodes$name == "r"], "#333333")
  expect_equal(nodes$fillcolor[nodes$name == "a"], "#444444")
  expect_true(grepl("black:#222222:black", pl[["x"]][["diagram"]], fixed = TRUE))
})

test_that("plot() colors: named vector recolours single variables", {
  sfm <- stockflow("sir")
  pl <- plot(sfm, colors = c(susceptible = "gold"))
  nodes <- extract_diagram_nodes(pl)

  expect_equal(nodes$fillcolor[nodes$name == "susceptible"], "#FFD700")
  # Other stocks keep the default stock colour
  expect_true(all(nodes$fillcolor[nodes$name %in% c("infected", "recovered")] == "#83D3D4"))
})

test_that("plot() colors: named vector recolours single flows", {
  sfm <- typed_model()
  pl <- plot(sfm, colors = c(growth = "blue"))
  d <- pl[["x"]][["diagram"]]

  expect_true(grepl("black:#0000FF:black", d, fixed = TRUE))
  expect_false(grepl("black:#F48153:black", d, fixed = TRUE))
})

test_that("plot() colors: list entries must be a single colour", {
  sfm <- stockflow("sir")
  # A variable's type is unique, so per-variable colours inside a type entry
  # are redundant; the error points to the named-vector form instead.
  expect_error(
    plot(sfm, colors = list(stock = c(susceptible = "red"))),
    "named vector"
  )
  expect_error(plot(sfm, colors = list(stock = c("red", "blue"))), "single colour")
})

test_that("plot() colors: unknown variable names warn and are ignored", {
  sfm <- stockflow("sir")
  expect_warning(
    pl <- plot(sfm, colors = c(nonexistent = "red")),
    "nonexistent"
  )
  nodes <- extract_diagram_nodes(pl)
  expect_true(all(nodes$fillcolor[nodes$name == "susceptible"] == "#83D3D4"))
})

test_that("plot() colors: a type name in a named vector hints at the list form", {
  sfm <- stockflow("sir")
  expect_warning(
    plot(sfm, colors = c(stock = "red")),
    "list"
  )
})

test_that("plot() colors: invalid specifications raise errors", {
  sfm <- stockflow("sir")

  # Unknown type in the list
  expect_error(plot(sfm, colors = list(banana = "red")), "banana")
  # Non-character colours
  expect_error(plot(sfm, colors = 5), "character")
  # Unnamed vector of multiple colours is ambiguous in a diagram
  expect_error(plot(sfm, colors = c("red", "blue")), "single colour")
})

test_that("plot() applies custom dependency color", {
  snapshot_names <- "stockflow-custom-dependency-color"
  announce_plot_snapshot_files(snapshot_names, type = "grViz")

  sfm <- stockflow("sir")
  dependency_color <- "#FFE66D"
  df <- as.data.frame(sfm, properties = "type")
  pl <- plot(sfm, color_dependency = dependency_color, show_dependencies = TRUE)
  edges <- extract_diagram_edges(pl)
  # dependency_edges <- edges[edges$rel == "dependency", ]
  # expect_true(all(dependency_edges$color == dependency_color))

  expect_snapshot_plot(snapshot_names, pl)
})

test_that("plot() with custom font size", {
  snapshot_names <- "stockflow-large-font"
  announce_plot_snapshot_files(snapshot_names, type = "grViz")

  sfm <- stockflow()
  sfm1 <- update(sfm, "Stock1", type = "stock", label = "Population")
  font_size <- 12
  pl <- plot(sfm1, font_size = font_size)
  nodes <- extract_diagram_nodes(pl)
  expect_true(all(nodes$font.size == font_size))

  expect_snapshot_plot(snapshot_names, pl)
})

test_that("plot.stockflow() with custom wrap width", {
  snapshot_names <- "stockflow-wrap-width-small"
  announce_plot_snapshot_files(snapshot_names, type = "grViz")

  sfm <- stockflow()
  sfm1 <- update(sfm, "VeryLongStockNameThatShouldWrap",
    type = "stock", label = "Very Long Stock Name That Should Wrap"
  )

  pl <- plot(sfm1, wrap_width = 10, show_eqn = FALSE)
  nodes <- extract_diagram_nodes(pl)
  expect_true(grepl("\n", nodes$label, fixed = TRUE)) # check that label contains a newline (indicating wrapping)

  expect_snapshot_plot(snapshot_names, pl)
})

test_that("plot.stockflow() with format_label = FALSE preserves original labels", {
  snapshot_names <- "stockflow-format-label-false"
  announce_plot_snapshot_files(snapshot_names, type = "grViz")

  sfm <- stockflow()
  sfm1 <- update(sfm, "Stock_1", type = "stock", label = "Stock_1")

  pl <- plot(sfm1, format_label = FALSE, show_eqn = FALSE)
  nodes <- extract_diagram_nodes(pl)
  expect_true(nodes$label == "Stock_1")
  expect_snapshot_plot(snapshot_names, pl)
})

test_that("plot.stockflow() with format_label = TRUE removes underscores", {
  snapshot_names <- "stockflow-format-label-true"
  announce_plot_snapshot_files(snapshot_names, type = "grViz")

  sfm <- stockflow()
  sfm1 <- update(sfm, "Stock_1", type = "stock", label = "Stock_1")

  pl <- plot(sfm1, format_label = TRUE, show_eqn = FALSE)
  nodes <- extract_diagram_nodes(pl)
  expect_true(nodes$label == "Stock 1")
  expect_snapshot_plot(snapshot_names, pl)
})

# ============================================================================
# show_eqn AND font_color TESTS
# ============================================================================

test_that("plot() with show_eqn = TRUE (default) shows equations beneath labels", {
  snapshot_names <- "stockflow-show-eqn"
  announce_plot_snapshot_files(snapshot_names, type = "grViz")

  sfm <- stockflow("sir")
  pl <- plot(sfm, show_constants = TRUE)
  d <- pl[["x"]][["diagram"]]

  # Equations are rendered as a smaller FONT line, prefixed by what the
  # equation defines for that variable type.
  expect_true(grepl("FONT POINT-SIZE", d, fixed = TRUE))
  expect_true(grepl("Initial value = ", d, fixed = TRUE))
  expect_true(grepl("Rate = ", d, fixed = TRUE))
  expect_true(grepl("Value = ", d, fixed = TRUE))
  # HTML-like labels (label=< ... >) are used when show_eqn = TRUE.
  expect_true(grepl("label=<", d, fixed = TRUE))

  expect_snapshot_plot(snapshot_names, pl)
})

test_that("plot() show_eqn uses a type-specific prefix for every variable type", {
  sfm <- typed_model()
  pl <- plot(sfm, show_constants = TRUE, show_aux = TRUE)
  d <- pl[["x"]][["diagram"]]

  expect_true(grepl("Initial value = 100", d, fixed = TRUE))
  expect_true(grepl("Rate = r * a", d, fixed = TRUE))
  expect_true(grepl("Value = 0.1", d, fixed = TRUE))
  expect_true(grepl("Equation = S * 2", d, fixed = TRUE))
})

test_that("plot() with show_eqn = FALSE does not show equations in labels", {
  sfm <- stockflow("sir")
  pl <- plot(sfm, show_eqn = FALSE)
  d <- pl[["x"]][["diagram"]]

  expect_false(grepl("FONT POINT-SIZE", d, fixed = TRUE))
  expect_false(grepl("label=<", d, fixed = TRUE))
})

test_that("plot.stockflow() show_tooltip = TRUE (default) adds equation tooltips", {
  snapshot_names <- "stockflow-tooltip"
  announce_plot_snapshot_files(snapshot_names, type = "grViz")

  sfm <- stockflow("sir")
  pl <- plot(sfm, show_constants = TRUE, show_tooltip = TRUE)
  d <- pl[["x"]][["diagram"]]

  expect_true(grepl("tooltip", d, fixed = TRUE))
  expect_snapshot_plot(snapshot_names, pl)
})

test_that("plot.stockflow() with show_tooltip = FALSE omits tooltips", {
  snapshot_names <- "stockflow-no-tooltip"
  announce_plot_snapshot_files(snapshot_names, type = "grViz")

  sfm <- stockflow("sir")
  pl <- plot(sfm, show_tooltip = FALSE, show_constants = TRUE)
  d <- pl[["x"]][["diagram"]]

  expect_false(grepl("tooltip", d, fixed = TRUE))
  expect_snapshot_plot(snapshot_names, pl)
})

test_that("plot.stockflow() validates show_tooltip", {
  sfm <- stockflow("sir")
  expect_error(plot(sfm, show_tooltip = "yes"), "show_tooltip")
  expect_error(plot(sfm, show_tooltip = NA), "show_tooltip")
})

test_that("plot.stockflow() tooltips describe type, name, equation, and structure", {
  sfm <- stockflow("sir")
  pl <- plot(sfm, show_constants = TRUE, show_tooltip = TRUE)
  nodes <- extract_diagram_nodes(pl)

  stock <- nodes[nodes$name == "susceptible", ]
  expect_true(grepl("Stock: Susceptible", stock$tooltip, fixed = TRUE))
  expect_true(grepl("Initial value: 99999", stock$tooltip, fixed = TRUE))
  expect_true(grepl("Outflows: New infections", stock$tooltip, fixed = TRUE))

  flow <- nodes[nodes$name == "new_infections", ]
  expect_true(grepl("Flow: New infections", flow$tooltip, fixed = TRUE))
  expect_true(grepl("Rate: ", flow$tooltip, fixed = TRUE))
  expect_true(grepl("From: Susceptible", flow$tooltip, fixed = TRUE))
  expect_true(grepl("To: Infected", flow$tooltip, fixed = TRUE))

  const <- nodes[nodes$name == "recovery_rate", ]
  expect_true(grepl("Constant: Recovery rate", const$tooltip, fixed = TRUE))
  expect_true(grepl("Value: 0.1", const$tooltip, fixed = TRUE))
})

test_that("plot.stockflow() omits the Name line when name equals the label", {
  sfm <- stockflow()
  sfm <- update(sfm, "S", type = "stock")
  pl <- plot(sfm, show_tooltip = TRUE)
  nodes <- extract_diagram_nodes(pl)
  stock <- nodes[nodes$name == "S", ]

  expect_true(grepl("Stock: S", stock$tooltip, fixed = TRUE))
  expect_false(grepl("Name:", stock$tooltip, fixed = TRUE))
})

test_that("plot.stockflow() cloud tooltips state they are outside the model boundary", {
  sfm <- stockflow()
  sfm <- update(sfm, "Population", type = "stock")
  sfm <- update(sfm, "births", type = "flow", to = "Population")
  pl <- plot(sfm, show_tooltip = TRUE)
  nodes <- extract_diagram_nodes(pl)
  cloud <- nodes[grepl("Cloud", nodes$name), ]

  expect_true(nrow(cloud) >= 1)
  expect_true(any(grepl("Outside model boundary", cloud$tooltip, fixed = TRUE)))
  # A flow that enters the model from outside makes the cloud its source.
  expect_true(any(grepl("Source of: births", cloud$tooltip, fixed = TRUE)))
})

test_that("plot() show_eqn uses font_color for the equation text", {
  snapshot_names <- "stockflow-show-eqn-label-col"
  announce_plot_snapshot_files(snapshot_names, type = "grViz")

  sfm <- stockflow("sir")
  label_color <- "#123456"
  pl <- plot(sfm, show_eqn = TRUE, font_color = label_color)
  d <- pl[["x"]][["diagram"]]

  # Equation FONT colour and node fontcolor both use font_color.
  expect_true(grepl(paste0("COLOR=\"", label_color, "\""), d, fixed = TRUE))
  expect_true(grepl(label_color, d, fixed = TRUE))

  expect_snapshot_plot(snapshot_names, pl)
})

test_that("plot() show_eqn wraps long equations to wrap_width", {
  sfm <- stockflow("sir")
  pl <- plot(sfm, show_eqn = TRUE, wrap_width = 8)
  d <- pl[["x"]][["diagram"]]

  # A long equation wrapped to a narrow width contains a line break.
  expect_true(grepl("Rate =.*<BR/>", d))
})

test_that("plot() validates show_eqn", {
  sfm <- stockflow("sir")
  expect_error(plot(sfm, show_eqn = "yes"), "show_eqn")
  expect_error(plot(sfm, show_eqn = NA), "show_eqn")
})

test_that("plot() applies font_color to node fontcolor", {
  snapshot_names <- "stockflow-label-col"
  announce_plot_snapshot_files(snapshot_names, type = "grViz")

  sfm <- stockflow("sir")
  label_color <- "#654321"
  pl <- plot(sfm, font_color = label_color)
  d <- pl[["x"]][["diagram"]]

  expect_true(grepl(label_color, d, fixed = TRUE))

  expect_snapshot_plot(snapshot_names, pl)
})
