# ============================================================================
# LAYOUT CONTROLS for plot.stockflow(): direction / align / order
# ============================================================================

# ---- direction -----------------------------------------------------------


test_that("plot() honours direction for every valid value", {
  directions <- c("LR", "TB", "RL", "BT")
  snapshot_names <- paste0("plot_stockflow_direction_", directions)
  announce_plot_snapshot_files(snapshot_names, type = "grViz")

  sfm <- stockflow("predator_prey")
  for (dir in directions) {
    # print(plot(sfm, direction = dir))
    d <- plot(sfm, direction = dir)[["x"]][["diagram"]]
    expect_match(d, paste0("rankdir = ", dir))
  }

  # snapshot tests
  expect_snapshot_plot(
    snapshot_names,
    lapply(directions, function(dir) plot(sfm, direction = dir))
  )
})

test_that("plot() rejects an invalid direction", {
  sfm <- stockflow("predator_prey")
  expect_error(plot(sfm, direction = "sideways"), "direction")
  expect_error(plot(sfm, direction = c("LR", "TB")), "direction")
  expect_error(plot(sfm, direction = 1), "direction")
})

test_that("plot() anchors flow edges to the sides that face the flow direction", {
  sfm <- stockflow("predator_prey")
  # tail = downstream side, head = upstream side, per direction.
  expected <- list(
    LR = c(tail = "e", head = "w"),
    RL = c(tail = "w", head = "e"),
    TB = c(tail = "s", head = "n"),
    BT = c(tail = "n", head = "s")
  )
  for (dir in names(expected)) {
    edges <- extract_diagram_edges(plot(sfm, direction = dir, show_constants = FALSE))
    # Flow edges carry compass ports; dependency edges use the centre port "_".
    flow_edges <- edges[!is.na(edges$tailport) & edges$tailport != "_", , drop = FALSE]
    expect_true(nrow(flow_edges) > 0)
    expect_true(all(flow_edges$tailport == expected[[dir]][["tail"]]))
    expect_true(all(flow_edges$headport == expected[[dir]][["head"]]))
  }
})

# ---- align ---------------------------------------------------------------

test_that("plot() align accepts a single character vector", {
  sfm <- stockflow("predator_prey")
  pl <- plot(sfm, align = c("predator", "prey", "prey_births"), show_constants = FALSE)
  ranks <- extract_diagram_ranks(pl)
  expect_true(any(vapply(ranks, function(g) setequal(g, c("predator", "prey", "prey_births")), logical(1))))
})

test_that("plot() align accepts a list of groups, one rank=same each", {
  sfm <- stockflow("predator_prey")
  pl <- plot(sfm,
    align = list(c("predator", "prey"), c("prey_births", "predator_deaths")),
    show_constants = FALSE
  )
  ranks <- extract_diagram_ranks(pl)
  expect_true(any(vapply(ranks, function(g) setequal(g, c("predator", "prey")), logical(1))))
  expect_true(any(vapply(ranks, function(g) setequal(g, c("prey_births", "predator_deaths")), logical(1))))
})

test_that("plot() align is not restricted to stocks (flows align too)", {
  sfm <- stockflow("crielaard2022")
  pl <- plot(sfm, align = c("Feeling_hunger", "Satiety"), show_constants = FALSE)
  ranks <- extract_diagram_ranks(pl)
  expect_true(any(vapply(ranks, function(g) setequal(g, c("Feeling_hunger", "Satiety")), logical(1))))
})

test_that("plot() align composes with the automatic constant/aux rank groups", {
  sfm <- stockflow("predator_prey")
  # Constants are auto-ranked with their first dependent; the user group must
  # appear in addition, not replace them.
  with_const <- extract_diagram_ranks(plot(sfm, show_constants = TRUE))
  expect_gt(length(with_const), 0)

  pl <- plot(sfm, align = c("predator", "prey"), show_constants = TRUE)
  ranks <- extract_diagram_ranks(pl)
  expect_equal(length(ranks), length(with_const) + 1L)
  expect_true(any(vapply(ranks, function(g) setequal(g, c("predator", "prey")), logical(1))))
})

test_that("plot() align warns and drops variables that are not drawn", {
  sfm <- stockflow("predator_prey")
  expect_warning(
    pl <- plot(sfm, align = c("predator", "alpha"), show_constants = FALSE),
    "not shown"
  )
  # alpha hidden -> the group shrinks below 2 members -> no rank=same emitted
  ranks <- extract_diagram_ranks(pl)
  expect_false(any(vapply(ranks, function(g) "alpha" %in% g, logical(1))))
})

test_that("plot() align errors on an unknown variable", {
  sfm <- stockflow("predator_prey")
  expect_error(plot(sfm, align = c("predator", "nope")), "not a variable")
})

test_that("plot() align ignores a single-member group", {
  sfm <- stockflow("predator_prey")
  base <- extract_diagram_ranks(plot(sfm, show_constants = FALSE))
  pl <- plot(sfm, align = c("predator"), show_constants = FALSE)
  expect_equal(length(extract_diagram_ranks(pl)), length(base))
})

test_that("plot() align validates argument type", {
  sfm <- stockflow("predator_prey")
  expect_error(plot(sfm, align = 1:3), "align")
  expect_error(plot(sfm, align = list(c("predator", "prey"), 1)), "align")
})

# ---- order ---------------------------------------------------------------

test_that("plot() order adds an invisible edge chain", {
  sfm <- stockflow("predator_prey")
  pl <- plot(sfm, order = c("predator", "prey"), show_constants = FALSE)
  edges <- extract_diagram_edges(pl)
  invis <- edges[!is.na(edges$style) & edges$style == "invis", , drop = FALSE]
  expect_equal(nrow(invis), 1L)
  expect_equal(invis$from, "predator")
  expect_equal(invis$to, "prey")
})

test_that("plot() order chains three or more variables consecutively", {
  sfm <- stockflow("crielaard2022")
  pl <- plot(sfm, order = c("Hunger", "Food_intake", "Compensatory_behaviour"))
  edges <- extract_diagram_edges(pl)
  invis <- edges[!is.na(edges$style) & edges$style == "invis", , drop = FALSE]
  expect_equal(
    paste(invis$from, invis$to),
    c("Hunger Food_intake", "Food_intake Compensatory_behaviour")
  )
})

test_that("plot() order accepts a list of separate chains", {
  sfm <- stockflow("predator_prey")
  pl <- plot(sfm,
    order = list(c("predator", "prey"), c("predator_births", "predator_deaths")),
    show_constants = FALSE
  )
  edges <- extract_diagram_edges(pl)
  invis <- edges[!is.na(edges$style) & edges$style == "invis", , drop = FALSE]
  expect_equal(nrow(invis), 2L)
})

test_that("plot() order is a soft hint: real flow edges are untouched", {
  sfm <- stockflow("predator_prey")
  base_edges <- extract_diagram_edges(plot(sfm, show_constants = FALSE))
  base_real <- base_edges[is.na(base_edges$style) | base_edges$style != "invis", ]

  pl <- plot(sfm, order = c("prey", "predator"), show_constants = FALSE)
  edges <- extract_diagram_edges(pl)
  real <- edges[is.na(edges$style) | edges$style != "invis", ]

  # Same real edges as without order; only invisible edges are added.
  expect_setequal(paste(real$from, real$to), paste(base_real$from, base_real$to))
})

test_that("plot() order warns on undrawn variables and errors on unknown", {
  sfm <- stockflow("predator_prey")
  expect_warning(plot(sfm, order = c("predator", "alpha"), show_constants = FALSE), "not shown")
  expect_error(plot(sfm, order = c("predator", "nope")), "not a variable")
})

test_that("plot() order ignores a single-member chain", {
  sfm <- stockflow("predator_prey")
  pl <- plot(sfm, order = c("predator"), show_constants = FALSE)
  edges <- extract_diagram_edges(pl)
  invis <- edges[!is.na(edges$style) & edges$style == "invis", , drop = FALSE]
  expect_equal(nrow(invis), 0L)
})

# ---- rendered positions (align/order actually move nodes) ----------------

# Centroid (x, y) of a node's polygon in the exported SVG. Graphviz emits
# negative y, so a *smaller* y is higher up on the page.
svg_node_center <- function(pl, nm) {
  svg <- DiagrammeRsvg::export_svg(pl)
  i <- regexpr(paste0("<title>", nm, "</title>"), svg)
  if (i < 0) {
    return(c(x = NA_real_, y = NA_real_))
  }
  seg <- substr(svg, i, i + 400L)
  m <- regmatches(seg, regexec('points="([-0-9., ]+)"', seg))[[1]]
  if (length(m) < 2L) {
    return(c(x = NA_real_, y = NA_real_))
  }
  pts <- as.numeric(unlist(strsplit(gsub(",", " ", m[2]), "[ ]+")))
  xs <- pts[c(TRUE, FALSE)]
  ys <- pts[c(FALSE, TRUE)]
  c(x = mean(range(xs)), y = mean(range(ys)))
}

test_that("align places variables in the same rank (column) when direction = LR", {
  skip_on_cran()
  sfm <- stockflow("predator_prey")

  pl <- plot(sfm, align = c("predator", "prey"), show_constants = FALSE)
  p <- svg_node_center(pl, "predator")
  q <- svg_node_center(pl, "prey")
  # Same column => (nearly) equal x.
  expect_lt(abs(p[["x"]] - q[["x"]]), 5)
})

test_that("order sets the position within an aligned rank", {
  skip_on_cran()
  sfm <- stockflow("predator_prey")

  top_first <- plot(sfm,
    align = c("predator", "prey"), order = c("predator", "prey"),
    show_constants = FALSE
  )
  bottom_first <- plot(sfm,
    align = c("predator", "prey"), order = c("prey", "predator"),
    show_constants = FALSE
  )

  # order = (predator, prey): predator is higher (smaller y) than prey.
  expect_lt(svg_node_center(top_first, "predator")[["y"]], svg_node_center(top_first, "prey")[["y"]])
  # order = (prey, predator): the stacking flips.
  expect_lt(svg_node_center(bottom_first, "prey")[["y"]], svg_node_center(bottom_first, "predator")[["y"]])
})

test_that("order alone sequences variables into separate ranks (columns)", {
  skip_on_cran()
  sfm <- stockflow("predator_prey")

  pl <- plot(sfm, order = c("predator", "prey"), show_constants = FALSE)
  p <- svg_node_center(pl, "predator")
  q <- svg_node_center(pl, "prey")
  # Different columns => clearly different x; predator upstream (smaller x).
  expect_gt(abs(p[["x"]] - q[["x"]]), 5)
  expect_lt(p[["x"]], q[["x"]])
})

# ---- combinations --------------------------------------------------------

test_that("plot() applies direction, align and order together", {
  sfm <- stockflow("predator_prey")
  pl <- plot(sfm,
    direction = "TB",
    align = c("predator", "prey"),
    order = c("prey", "predator"),
    show_constants = FALSE
  )
  d <- pl[["x"]][["diagram"]]
  expect_match(d, "rankdir = TB")

  ranks <- extract_diagram_ranks(pl)
  expect_true(any(vapply(ranks, function(g) setequal(g, c("predator", "prey")), logical(1))))

  edges <- extract_diagram_edges(pl)
  invis <- edges[!is.na(edges$style) & edges$style == "invis", , drop = FALSE]
  expect_equal(invis$from, "prey")
  expect_equal(invis$to, "predator")
})

test_that("plot() with layout controls still returns a grViz object", {
  sfm <- stockflow("predator_prey")
  pl <- plot(sfm, direction = "TB", align = c("predator", "prey"))
  expect_s3_class(pl, "grViz")
})

# ---- prepare_layout_groups (unit) ----------------------------------------

test_that("prepare_layout_groups normalizes input forms", {
  pv <- c("a", "b", "c", "d")
  mv <- c("a", "b", "c", "d", "e")

  expect_equal(prepare_layout_groups(NULL, pv, mv, "align"), list())
  expect_equal(prepare_layout_groups(c("a", "b"), pv, mv, "align"), list(c("a", "b")))
  expect_equal(
    prepare_layout_groups(list(c("a", "b"), c("c", "d")), pv, mv, "align"),
    list(c("a", "b"), c("c", "d"))
  )
})

test_that("prepare_layout_groups trims, de-duplicates and drops blanks", {
  pv <- mv <- c("a", "b", "c")
  expect_equal(
    prepare_layout_groups(c(" a ", "b", "a", ""), pv, mv, "align"),
    list(c("a", "b"))
  )
})

test_that("prepare_layout_groups drops groups below min_len", {
  pv <- mv <- c("a", "b", "c")
  expect_equal(prepare_layout_groups(c("a"), pv, mv, "align"), list())
  expect_equal(
    prepare_layout_groups(list(c("a", "b"), c("c")), pv, mv, "align"),
    list(c("a", "b"))
  )
})

test_that("prepare_layout_groups errors on unknown names, warns on undrawn", {
  pv <- c("a", "b")
  mv <- c("a", "b", "c")
  expect_error(prepare_layout_groups(c("a", "z"), pv, mv, "align"), "not a variable")
  expect_warning(
    res <- prepare_layout_groups(c("a", "c"), pv, mv, "align"),
    "not shown"
  )
  # c is in the model but not drawn -> dropped -> group falls below min_len
  expect_equal(res, list())
})

test_that("prepare_layout_groups rejects non-character input", {
  pv <- mv <- c("a", "b")
  expect_error(prepare_layout_groups(1:3, pv, mv, "order"), "order")
  expect_error(prepare_layout_groups(list("a", 2), pv, mv, "order"), "order")
})
