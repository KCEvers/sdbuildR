# Comprehensive tests for update() and helpers in update.R

test_that("update() creates variables with defaults", {
  sfm <- stockflow()
  sfm <- update(sfm, "Population", type = "stock")

  vars <- as.data.frame(sfm)
  expect_equal(nrow(vars), 1)
  expect_equal(vars[["name"]], "Population")
  expect_equal(vars[["type"]], "stock")
  expect_equal(vars[["eqn"]], "0")
  expect_equal(vars[["label"]], "Population")
  expect_false(vars[["non_negative"]])
})


test_that("update() validates inputs and basic errors", {
  sfm <- stockflow()

  expect_error(update(sfm, "Var1", type = "invalid"), "must be one of")
  expect_error(update(sfm, "", type = "stock"), "cannot be empty")

  # invalid doc type
  expect_error(update(sfm, "D", type = "stock", doc = 1L), "must be")

  # invalid non_negative type
  expect_error(update(sfm, "E", type = "stock", non_negative = "no"), "must be")
})


test_that("update() rejects malformed equations at build time", {
  sfm <- stockflow()

  expect_error(
    update(sfm, "S", type = "stock", eqn = "1 // 2"),
    "Could not parse the equation for .*S",
    ignore.case = TRUE
  )

  expect_error(
    stock(sfm, S, eqn = "1 // 2"),
    "Could not parse the equation for .*S",
    ignore.case = TRUE
  )
})


test_that("update() handles long names with spaces without cli parse errors", {
  sfm <- stockflow()

  expect_warning(
    sfm2 <- update(sfm, "Very Long Stock Name That Should Wrap", type = "stock"),
    "name.*changed"
  )

  vars <- as.data.frame(sfm2)
  expect_true("Very_Long_Stock_Name_That_Should_Wrap" %in% vars[["name"]])
})


test_that("update() rejects disallowed names", {
  sfm <- stockflow()

  # NA name
  expect_error(update(sfm, NA_character_, type = "stock"), "Variable names cannot be NA")

  # whitespace-only name
  expect_error(update(sfm, "   ", type = "stock"), "cannot be empty")
})


test_that("update() enforces flow rules", {
  sfm <- stockflow()
  sfm <- update(sfm, "StockA", type = "stock")
  sfm <- update(sfm, "StockB", type = "stock")

  # Flow cannot target itself
  expect_error(update(sfm, "Flow1", type = "flow", to = "Flow1"), "flow cannot flow to itself")

  # Flow with same to/from is not allowed
  expect_error(update(sfm, "Flow2", type = "flow", to = "StockA", from = "StockA"), "same stock as both source and target")

  # Flow 'to' must be a stock (sanitize_stockflow will later clean; update should not accept non-stock in this context)
  expect_error(update(sfm, "Flow3", type = "flow", to = "Flow3"), "flow cannot flow to itself")
})


test_that("change_name() renames variables and updates references", {
  sfm <- stockflow()
  sfm <- update(sfm, "Prey", type = "stock", eqn = "10")
  sfm <- update(sfm, "Predator", type = "stock", eqn = "5")
  sfm <- update(sfm, "Hunt",
    type = "flow", eqn = "Prey * 0.1",
    from = "Predator", to = "Prey"
  )

  sfm_renamed <- change_name(sfm, "Prey", "Bunnies")
  vars <- sfm_renamed[["variables"]]

  hunt_row <- vars[vars[["name"]] == "Hunt", ]
  expect_equal(hunt_row[["eqn"]], "Bunnies * 0.1")
  expect_equal(hunt_row[["to"]], "Bunnies")

  renamed_stock <- vars[vars[["name"]] == "Bunnies", ]
  expect_equal(renamed_stock[["label"]], "Bunnies") # label follows name when not set explicitly
})


test_that("update() blocks type mismatches", {
  sfm <- stockflow()
  sfm <- update(sfm, "A", type = "stock")

  # specifying a conflicting type for existing var errors
  expect_error(update(sfm, "A", type = "flow"), "Wrong `type` passed")
})

test_that("discard() removes variables", {
  sfm <- stockflow() |> update("A", type = "stock")

  # discard fails on missing vars
  expect_error(discard(sfm, "Missing"), "not exist")

  # discard succeeds on existing
  sfm_erased <- discard(sfm, "A")
  expect_equal(nrow(as.data.frame(sfm_erased)), 0)
})

test_that("discard() removes discarded names from sim_settings vars", {
  sfm <- stockflow("sir") |>
    sim_settings(vars = c("susceptible", "new_infections"))

  sfm2 <- discard(sfm, "new_infections")
  expect_equal(sfm2[["sim_settings"]][["vars"]], "susceptible")
})

test_that("discard() clears sim_settings vars when all selected vars are removed", {
  sfm <- stockflow("sir") |>
    sim_settings(vars = c("new_recoveries"))

  sfm2 <- discard(sfm, "new_recoveries")
  expect_null(sfm2[["sim_settings"]][["vars"]])
})


test_that("change_type() changes types while preserving equations", {
  sfm <- stockflow()
  sfm <- update(sfm, "alpha", type = "aux", eqn = "2")

  sfm_changed <- change_type(sfm, "alpha", "constant")
  vars <- as.data.frame(sfm_changed)

  expect_equal(vars[["type"]], "constant")
  expect_equal(vars[["eqn"]], "2") # equation preserved when change_type is used without eqn
})


test_that("update() validates graphical functions", {
  sfm <- stockflow()

  expect_error(update(sfm, "curve1", type = "lookup", xpts = c(0, 1)), "required")
  expect_error(update(sfm, "curve2", type = "lookup", xpts = c(0, 1), ypts = c(2)), "Length mismatch")
  expect_error(update(sfm, "curve3", type = "lookup", xpts = c(0, 1), ypts = c(2, 3), interpolation = "bad"), "interpolation")
  expect_error(update(sfm, "curve4", type = "lookup", xpts = c(0, 1), ypts = c(2, 3), extrapolation = "bad"), "extrapolation")

  expect_error(update(sfm, "curve5", type = "lookup", xpts = c(0, 1), ypts = c(2, 3), source = c("a", "b")), "Invalid length")

  sfm_gf <- update(sfm, "curve_ok", type = "lookup", xpts = c(0, 1), ypts = c(2, 3), source = "X")
  gf_row <- sfm_gf[["variables"]][sfm_gf[["variables"]][["name"]] == "curve_ok", ]
  expect_equal(unlist(gf_row$xpts), c(0, 1))
  expect_equal(unlist(gf_row$ypts), c(2, 3))
})


test_that("update() supports bulk add via data frame and validates df", {
  sfm <- stockflow()
  df <- data.frame(
    type = c("stock", "flow"),
    name = c("S", "In"),
    eqn = c("5", "S * 0.1"),
    to = c("", "S"),
    from = c("", ""),
    stringsAsFactors = FALSE
  )

  sfm_new <- update(sfm, df = df)
  vars <- as.data.frame(sfm_new)
  expect_equal(sort(vars[["name"]]), c("In", "S"))
  expect_equal(vars[vars[["name"]] == "S", "type"], "stock")
  expect_equal(vars[vars[["name"]] == "In", "type"], "flow")

  # missing required columns
  bad_df <- data.frame(type = "stock")
  expect_error(update(sfm, df = bad_df), "required")

  # invalid column name
  bad_df2 <- data.frame(type = "stock", name = "A", badcol = 1)
  expect_error(update(sfm, df = bad_df2), "not valid propert")

  # TODO: inappropriate properties via df - verify warning is triggered
  # df_warn <- data.frame(type = "stock", name = "X", interpolation = "linear")
  # expect_warning(update(sfm, df = df_warn), "not appropriate")
})


test_that("update() warns when inappropriate properties are supplied", {
  sfm <- stockflow()
  expect_warning(
    update(sfm, "A", type = "stock", interpolation = "linear"),
    "Inappropriate propert"
  )
})


test_that("update() handles doc, non_negative lengths", {
  sfm <- stockflow()
  sfm <- update(sfm, c("A", "B"), type = c("stock", "stock"), doc = c("d1", "d2"), non_negative = c(TRUE, FALSE))
  vars <- as.data.frame(sfm)
  expect_true(vars[vars[["name"]] == "A", "non_negative"])
  expect_false(vars[vars[["name"]] == "B", "non_negative"])
})


# --- Vectorized lookup tests ------------------------------------------------

test_that("lookup() supports vectorized creation with lists", {
  sfm <- stockflow()
  sfm <- lookup(sfm,
    c("L1", "L2"),
    xpts = list(c(0, 1), c(0, 5, 10)),
    ypts = list(c(10, 20), c(0, 50, 100))
  )
  vars <- sfm[["variables"]]
  l1 <- vars[vars[["name"]] == "L1", ]
  l2 <- vars[vars[["name"]] == "L2", ]
  expect_equal(unlist(l1$xpts), c(0, 1))
  expect_equal(unlist(l1$ypts), c(10, 20))
  expect_equal(unlist(l2$xpts), c(0, 5, 10))
  expect_equal(unlist(l2$ypts), c(0, 50, 100))
  expect_equal(l1$type, "lookup")
  expect_equal(l2$type, "lookup")
})

test_that("lookup() single with list input unlists correctly", {
  sfm <- stockflow()
  sfm <- lookup(sfm, "L1",
    xpts = list(0, 1, 2),
    ypts = list(10, 20, 30)
  )
  l1 <- sfm[["variables"]][sfm[["variables"]][["name"]] == "L1", ]
  expect_equal(unlist(l1$xpts), c(0, 1, 2))
  expect_equal(unlist(l1$ypts), c(10, 20, 30))
})

test_that("lookup() single with vector input still works", {
  sfm <- stockflow()
  sfm <- lookup(sfm, "L1", xpts = c(0, 1), ypts = c(10, 20))
  l1 <- sfm[["variables"]][sfm[["variables"]][["name"]] == "L1", ]
  expect_equal(unlist(l1$xpts), c(0, 1))
  expect_equal(unlist(l1$ypts), c(10, 20))
})

test_that("lookup() vectorized errors on non-list xpts/ypts", {
  sfm <- stockflow()
  expect_error(
    lookup(sfm, c("L1", "L2"), xpts = c(0, 1), ypts = c(10, 20)),
    "must be a list"
  )
})

test_that("lookup() vectorized errors on wrong-length list", {
  sfm <- stockflow()
  expect_error(
    lookup(sfm, c("L1", "L2"),
      xpts = list(c(0, 1)),
      ypts = list(c(10, 20), c(0, 100))
    ),
    "Length mismatch"
  )
})

test_that("lookup() vectorized recycles scalar params", {
  sfm <- stockflow()
  sfm <- lookup(sfm,
    c("L1", "L2"),
    xpts = list(c(0, 1), c(0, 5)),
    ypts = list(c(10, 20), c(0, 100)),
    interpolation = "constant"
  )
  vars <- sfm[["variables"]]
  expect_equal(
    vars[vars[["name"]] == "L1", "interpolation"],
    "constant"
  )
  expect_equal(
    vars[vars[["name"]] == "L2", "interpolation"],
    "constant"
  )
})

test_that("lookup() vectorized accepts per-element params", {
  sfm <- stockflow() |>
    stock("A", eqn = 1) |>
    stock("B", eqn = 2)
  sfm <- lookup(sfm,
    c("L1", "L2"),
    xpts = list(c(0, 1), c(0, 5)),
    ypts = list(c(10, 20), c(0, 100)),
    source = c("A", "B")
  )
  vars <- sfm[["variables"]]
  expect_equal(vars[vars[["name"]] == "L1", "source"], "A")
  expect_equal(vars[vars[["name"]] == "L2", "source"], "B")
})

test_that("update() with type = 'gf' still works (backward compat)", {
  sfm <- stockflow()
  sfm <- update(sfm, "L1",
    type = "gf",
    xpts = c(0, 1), ypts = c(10, 20)
  )
  l1 <- sfm[["variables"]][sfm[["variables"]][["name"]] == "L1", ]
  expect_equal(l1$type, "lookup")
  expect_equal(unlist(l1$xpts), c(0, 1))
})


# --- Wrapper function tests --------------------------------------------------
# These verify that each thin wrapper correctly injects `type` and forwards
# its parameters to update().  Validation / error paths are already covered
# by the update() tests above and are NOT duplicated here.

test_that("stock() creates a stock and forwards all parameters", {
  sfm <- stockflow()
  sfm <- stock(sfm, "Pop",
    eqn = "100",
    label = "Population", doc = "total pop", non_negative = TRUE
  )

  v <- as.data.frame(sfm)
  expect_equal(nrow(v), 1)
  expect_equal(v[["name"]], "Pop")
  expect_equal(v[["type"]], "stock")
  expect_equal(v[["eqn"]], "100")
  expect_equal(v[["label"]], "Population")
  expect_equal(v[["doc"]], "total pop")
  expect_true(v[["non_negative"]])
})

test_that("flow() creates a flow and forwards to/from", {
  sfm <- stockflow() |> stock(c("S1", "S2"))
  sfm <- flow(sfm, "F1",
    eqn = "S1 * 0.5", from = "S1", to = "S2",
    non_negative = TRUE
  )

  v <- sfm[["variables"]]
  f <- v[v[["name"]] == "F1", ]
  expect_equal(f[["type"]], "flow")
  expect_equal(f[["eqn"]], "S1 * 0.5")
  expect_equal(f[["from"]], "S1")
  expect_equal(f[["to"]], "S2")
  expect_true(f[["non_negative"]])
})

test_that("auxiliary() creates an aux and forwards parameters", {
  sfm <- stockflow()
  sfm <- auxiliary(sfm, "rate",
    eqn = "0.05",
    label = "growth rate", doc = "per-capita"
  )

  v <- as.data.frame(sfm)
  expect_equal(nrow(v), 1)
  expect_equal(v[["name"]], "rate")
  expect_equal(v[["type"]], "aux")
  expect_equal(v[["eqn"]], "0.05")
  expect_equal(v[["label"]], "growth rate")
  expect_equal(v[["doc"]], "per-capita")
})

test_that("aux() behaves identically to auxiliary()", {
  sfm1 <- stockflow()
  sfm1 <- auxiliary(sfm1, "r", eqn = "0.1")

  sfm2 <- stockflow()
  sfm2 <- aux(sfm2, "r", eqn = "0.1")

  v1 <- as.data.frame(sfm1)
  v2 <- as.data.frame(sfm2)
  expect_equal(v1[["type"]], v2[["type"]])
  expect_equal(v1[["eqn"]], v2[["eqn"]])
})

test_that("custom_func() creates a func and forwards parameters", {
  sfm <- stockflow()
  sfm <- custom_func(sfm, "square", eqn = "x^2", doc = "square fn")

  v <- as.data.frame(sfm)
  expect_equal(nrow(v), 1)
  expect_equal(v[["name"]], "square")
  expect_equal(v[["type"]], "func")
  expect_equal(v[["eqn"]], "x^2")
  expect_equal(v[["doc"]], "square fn")
})

test_that("lookup() injects type = 'lookup'", {
  # Detailed lookup tests already exist above; this only checks type injection.
  sfm <- stockflow()
  sfm <- lookup(sfm, "LU",
    xpts = c(0, 10), ypts = c(1, 2),
    doc = "a lookup"
  )

  v <- as.data.frame(sfm)
  expect_equal(v[["type"]], "lookup")
  expect_equal(v[["doc"]], "a lookup")
})

test_that("wrappers work in a pipe chain", {
  sfm <- stockflow() |>
    stock("Prey", eqn = "100") |>
    stock("Predator", eqn = "10") |>
    flow("births", eqn = "Prey * r", to = "Prey") |>
    flow("hunt", eqn = "Prey * Predator * b", from = "Prey", to = "Predator") |>
    auxiliary("r", eqn = "0.5") |>
    custom_func("half", eqn = "x / 2") |>
    lookup("eff", xpts = c(0, 100), ypts = c(0, 1))

  v <- as.data.frame(sfm)
  expect_equal(nrow(v), 7)
  expect_equal(
    sort(v[["name"]]),
    sort(c("Prey", "Predator", "births", "hunt", "r", "half", "eff"))
  )
  expect_equal(v[v[["name"]] == "Prey", "type"], "stock")
  expect_equal(v[v[["name"]] == "births", "type"], "flow")
  expect_equal(v[v[["name"]] == "r", "type"], "aux")
  expect_equal(v[v[["name"]] == "half", "type"], "func")
  expect_equal(v[v[["name"]] == "eff", "type"], "lookup")
})


# ------------------------------------------------------------------------------
# Tests for detecting accidentally doubly-passed model object
# e.g., sfm |> constant(sfm, A) instead of sfm |> constant(A)
# ------------------------------------------------------------------------------

test_that("update() errors when model object passed as name", {
  sfm <- stockflow()
  expect_error(update(sfm, sfm, type = "constant"), "passed where a variable name")
})

test_that("wrapper functions error when model object passed as name", {
  sfm <- stockflow()
  expect_error(constant(sfm, sfm), "passed where a variable name")
  expect_error(stock(sfm, sfm), "passed where a variable name")
  expect_error(flow(sfm, sfm), "passed where a variable name")
  expect_error(auxiliary(sfm, sfm), "passed where a variable name")
})

test_that("discard() errors when model object passed as name", {
  sfm <- stockflow() |> update("A", type = "constant")
  expect_error(discard(sfm, sfm), "passed where a variable name")
})

test_that("change_type() errors when model object passed as name", {
  sfm <- stockflow() |> update("A", type = "constant")
  expect_error(change_type(sfm, sfm, new_type = "aux"), "passed where a variable name")
})


# ==============================================================================
# change_name() — unit test interaction
# ==============================================================================

test_that("change_name() updates unit test expr_str", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S non-neg", expr = all(S >= 0)) |>
    change_name("S", new_name = "Stock")

  expect_equal(sfm[["unit_tests"]][[1]][["expr_str"]], "all(Stock >= 0)")
})

test_that("change_name() updates unit test condition keys", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "at zero rate", expr = all(S >= 0), conditions = list(rate = 0)) |>
    change_name("rate", new_name = "beta")

  expect_equal(names(sfm[["unit_tests"]][[1]][["conditions"]]), "beta")
  expect_equal(sfm[["unit_tests"]][[1]][["conditions"]][["beta"]], 0)
})

test_that("change_name() updates auto-generated unit test label", {
  sfm <- make_verifiable_sfm() |>
    unit_test(expr = all(S >= 0)) |> # auto-generated label
    change_name("S", new_name = "Stock")

  # Label should reference the new name, not the old one
  expect_false(grepl("\\bS\\b", sfm[["unit_tests"]][[1]][["label"]]))
  expect_true(grepl("Stock", sfm[["unit_tests"]][[1]][["label"]]))
})

test_that("change_name() does not change manually set unit test label", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "my custom label", expr = all(S >= 0)) |>
    change_name("S", new_name = "Stock")

  # Custom label should be preserved
  expect_equal(sfm[["unit_tests"]][[1]][["label"]], "my custom label")
  # But expr should still be updated
  expect_equal(sfm[["unit_tests"]][[1]][["expr_str"]], "all(Stock >= 0)")
})

test_that("change_name() invalidates test deps cache", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S non-neg", expr = all(S >= 0)) |>
    change_name("S", new_name = "Stock")

  # Deps cache should be invalidated after rename
  expect_null(sfm[["assemble"]][["unit_tests"]][["deps"]])
})


# ==============================================================================
# change_type() — unit test condition cleanup
# ==============================================================================

test_that("change_type() strips conditions for variables that are no longer stock/constant", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "test", expr = all(S >= 0), conditions = list(rate = 0))

  expect_warning(
    sfm <-
      change_type(sfm, "rate", new_type = "aux"),
    regexp = "Removed.*rate.*conditions"
  )

  # rate is now an aux, so it should be stripped from conditions
  expect_equal(sfm[["unit_tests"]][[1]][["conditions"]], list())
})

test_that("change_type() warns when stripping conditions", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "test", expr = all(S >= 0), conditions = list(rate = 0))

  expect_warning(
    change_type(sfm, "rate", new_type = "aux"),
    regexp = "Removed.*rate.*conditions"
  )
})

test_that("change_type() does not strip conditions for stock-to-constant change", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "test", expr = all(S >= 0), conditions = list(S = 50))

  expect_warning(
    sfm2 <- change_type(sfm, "S", new_type = "constant"),
    "lingering reference"
  )

  # S is still a valid condition target (now constant instead of stock)
  expect_equal(names(sfm2[["unit_tests"]][[1]][["conditions"]]), "S")
})

test_that("change_type() invalidates test deps cache", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "test", expr = all(S >= 0), conditions = list(rate = 0))

  sfm <- expect_warning(
    change_type(sfm, "rate", new_type = "aux"),
    regexp = "Removed.*rate.*conditions"
  )

  expect_null(sfm[["assemble"]][["unit_tests"]][["deps"]])
})

# ==============================================================================
# change_type() — stock dSdt[] index alignment (Julia state-vector layout)
# ==============================================================================

test_that("change_type() to stock keeps dSdt[] indices aligned with stock order", {
  # Converting a non-stock to a stock adds a new state variable. The new stock
  # is appended during the change, then sanitize_stockflow() re-sorts variables
  # by type/name. The positional dSdt[] index must follow that final order, or
  # stock dynamics get silently swapped.
  sfm <- stockflow() |>
    update("a_stock", type = "stock", eqn = "1") |>
    update("z_stock", type = "stock", eqn = "2") |>
    # 'motiv' sorts alphabetically BETWEEN a_stock and z_stock, so making it a
    # stock shifts z_stock's index -- exactly the regressed scenario.
    update("motiv", type = "constant", eqn = "0.3") |>
    sim_settings(language = "Julia")

  expect_stock_indices_aligned(sfm)

  sfm <- change_type(sfm, "motiv", new_type = "stock")

  # motiv must land in its sorted position (2nd) and z_stock must shift to 3rd
  stocks <- sfm[["variables"]][sfm[["variables"]][["type"]] == "stock", ]
  expect_equal(stocks[["name"]], c("a_stock", "motiv", "z_stock"))
  expect_stock_indices_aligned(sfm)

  # A stock with no flows must get a zero derivative (not another stock's flows)
  motiv_row <- sfm[["variables"]][sfm[["variables"]][["name"]] == "motiv", ]
  expect_equal(motiv_row[["sum_name"]], "dSdt[2]")
  expect_equal(motiv_row[["sum_eqn"]], "0.0")
})


test_that("change_type() to stock keeps dSdt[] aligned in a realistic model", {
  # Regression for the JDR template: change_type(motivation_rate -> stock) used
  # to leave resources pointing at dSdt[4] while the state vector put it at 5.
  sfm <- stockflow("crielaard2022") |>
    sim_settings(language = "Julia") |>
    change_type("a2", new_type = "stock")

  expect_stock_indices_aligned(sfm)

  mr <- sfm[["variables"]][sfm[["variables"]][["name"]] == "a2", ]
  expect_equal(mr[["sum_eqn"]], "0.0") # no flows -> constant
})


test_that("discard() reindexes remaining stock dSdt[] contiguously", {
  sfm <- stockflow() |>
    update("a", type = "stock", eqn = "1") |>
    update("b", type = "stock", eqn = "2") |>
    update("c", type = "stock", eqn = "3") |>
    sim_settings(language = "Julia")

  expect_stock_indices_aligned(sfm)

  sfm <- suppressWarnings(discard(sfm, "a"))

  stocks <- sfm[["variables"]][sfm[["variables"]][["type"]] == "stock", ]
  expect_equal(stocks[["name"]], c("b", "c"))
  expect_stock_indices_aligned(sfm) # b -> dSdt[1], c -> dSdt[2]
})


test_that("change_type() away from stock reindexes remaining stocks", {
  sfm <- stockflow() |>
    update("a", type = "stock", eqn = "1") |>
    update("b", type = "stock", eqn = "2") |>
    update("c", type = "stock", eqn = "3") |>
    sim_settings(language = "Julia")

  sfm <- suppressWarnings(change_type(sfm, "a", new_type = "constant"))

  stocks <- sfm[["variables"]][sfm[["variables"]][["type"]] == "stock", ]
  expect_equal(stocks[["name"]], c("b", "c"))
  expect_stock_indices_aligned(sfm)
})


test_that("change_name() that reorders stocks keeps dSdt[] aligned", {
  # Renaming a stock can change its alphabetical sort position, which shifts
  # other stocks' state-vector indices.
  sfm <- stockflow() |>
    update("b", type = "stock", eqn = "1") |>
    update("c", type = "stock", eqn = "2") |>
    sim_settings(language = "Julia")

  # Rename c -> a, which now sorts first
  sfm <- change_name(sfm, "c", new_name = "a")

  stocks <- sfm[["variables"]][sfm[["variables"]][["type"]] == "stock", ]
  expect_equal(stocks[["name"]], c("a", "b"))
  expect_stock_indices_aligned(sfm)
})


test_that("change_type() to stock simulates correctly in Julia (no-flow stock is constant)", {
  # End-to-end behavioral regression: a stock with no flows must stay constant,
  # and must not absorb another stock's dynamics via a misaligned dSdt[] index.
  skip_if_julia_not_ready()

  sfm <- stockflow("crielaard2022") |>
    sim_settings(language = "Julia") |>
    change_type("a0", new_type = "stock")

  sim <- simulate(sfm)
  df <- as.data.frame(sim)
  mr <- df[df[["variable"]] == "a0", "value"]

  expect_true(length(mr) > 0)
  expect_equal(diff(range(mr)), 0) # no flows -> perfectly constant
})


test_that("change_name() updates multiple renames in unit tests simultaneously", {
  sfm <- make_verifiable_sfm() |>
    unit_test(
      label = "combined", expr = all(S >= 0) && rate > 0,
      conditions = list(rate = 0)
    ) |>
    change_name(c("S", "rate"), new_name = c("Stock", "beta"))

  ut <- sfm[["unit_tests"]][[1]]

  # expr should have both renames applied
  expect_true(grepl("Stock", ut[["expr_str"]]))
  expect_true(grepl("beta", ut[["expr_str"]]))
  expect_false(grepl("\\bS\\b", ut[["expr_str"]]))
  expect_false(grepl("\\brate\\b", ut[["expr_str"]]))

  # condition key should be renamed
  expect_equal(names(ut[["conditions"]]), "beta")
})
