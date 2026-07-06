# Tests for Non-Standard Evaluation (NSE) support in update() and related functions

# ==============================================================================
# .expr_to_char() helper tests
# ==============================================================================

test_that(".expr_to_char() handles NULL", {
  expect_null(.expr_to_char(NULL))
})

test_that(".expr_to_char() passes through character strings", {
  expect_equal(.expr_to_char("hello"), "hello")
  expect_equal(.expr_to_char(c("a", "b")), c("a", "b"))
})

test_that(".expr_to_char() converts numeric to character", {
  expect_equal(.expr_to_char(100), "100")
  expect_equal(.expr_to_char(0.5), "0.5")
})

test_that(".expr_to_char() converts logical to character", {
  expect_equal(.expr_to_char(TRUE), "TRUE")
})

test_that(".expr_to_char() converts symbols to names", {
  expect_equal(.expr_to_char(quote(population)), "population")
  expect_equal(.expr_to_char(quote(stock)), "stock")
})

test_that(".expr_to_char() handles c() calls", {
  expect_equal(.expr_to_char(quote(c(a, b, c))), c("a", "b", "c"))
  expect_equal(.expr_to_char(quote(c("x", "y"))), c("x", "y"))
  # Mixed: bare symbols and strings
  expect_equal(.expr_to_char(quote(c("x", y))), c("x", "y"))
})

test_that(".expr_to_char() deparses general expressions", {
  result <- .expr_to_char(quote(a * b + 1))
  expect_true(is.character(result))
  expect_equal(length(result), 1)
  # Deparsed form of a * b + 1
  expect_true(grepl("a", result))
  expect_true(grepl("b", result))
})

test_that(".expr_to_char() handles negative numbers", {
  result <- .expr_to_char(quote(-1))
  expect_true(is.character(result))
  expect_true(grepl("-1", result))
})


# ==============================================================================
# update() with NSE
# ==============================================================================

test_that("update() accepts bare symbol for name", {
  sfm <- stockflow()
  sfm <- update(sfm, population, type = "stock")
  vars <- as.data.frame(sfm)
  expect_equal(vars[["name"]], "population")
})

test_that("update() accepts string for type", {
  sfm <- stockflow()
  sfm <- update(sfm, x, type = "stock")
  vars <- as.data.frame(sfm)
  expect_equal(vars[["type"]], "stock")
})

test_that("update() accepts bare symbol for name and string for type", {
  sfm <- stockflow()
  sfm <- update(sfm, x, "stock")
  vars <- as.data.frame(sfm)
  expect_equal(vars[["name"]], "x")
  expect_equal(vars[["type"]], "stock")
})

test_that("update() accepts bare expression for eqn", {
  sfm <- stockflow()
  sfm <- update(sfm, x, "stock", eqn = 100)
  sfm <- update(sfm, y, "aux", eqn = x * 0.1 + 2)
  vars <- as.data.frame(sfm)
  expect_equal(vars[vars[["name"]] == "x", "eqn"], "100")
  expect_equal(vars[vars[["name"]] == "y", "eqn"], "x * 0.1 + 2")
})

test_that("update() accepts bare symbols for to/from", {
  sfm <- stockflow()
  sfm <- update(sfm, pop, "stock", eqn = 100)
  sfm <- update(sfm, births, "flow", eqn = pop * 0.1, to = pop)
  vars <- as.data.frame(sfm)
  expect_equal(vars[vars[["name"]] == "births", "to"], "pop")
})

test_that("update() accepts bare symbol for source", {
  sfm <- stockflow()
  sfm <- update(sfm, my_lookup, "lookup",
    xpts = c(0, 5, 10), ypts = c(0, 10, 15), source = t
  )
  vars <- as.data.frame(sfm)
  expect_equal(vars[vars[["name"]] == "my_lookup", "source"], "t")
})

test_that("update() vectorized with bare symbols in c()", {
  sfm <- stockflow()
  sfm <- update(sfm, c(a, b), "stock", eqn = c(1, 2))
  vars <- as.data.frame(sfm)
  expect_equal(sort(vars[["name"]]), c("a", "b"))
  expect_equal(vars[vars[["name"]] == "a", "eqn"], "1")
  expect_equal(vars[vars[["name"]] == "b", "eqn"], "2")
})

test_that("update(): strings still work", {
  sfm <- stockflow()
  sfm <- update(sfm, "population", "stock", eqn = "100")
  vars <- as.data.frame(sfm)
  expect_equal(vars[["name"]], "population")
  expect_equal(vars[["type"]], "stock")
  expect_equal(vars[["eqn"]], "100")
})

test_that("eqn with functions as strings", {
  sfm <- stockflow()
  sfm <- custom_func(sfm, a,
    eqn = "function(x, midpoint, slope) {x^slope / (midpoint^slope + x^slope)}"
  )

  vars <- as.data.frame(sfm)
  expect_equal(vars[["name"]], "a")
  expect_equal(vars[["type"]], "func")
  expect_equal(vars[["eqn"]], "function(x, midpoint, slope) {x^slope / (midpoint^slope + x^slope)}")
})


test_that("eqn with NSE functions", {
  sfm <- stockflow()
  sfm <- custom_func(sfm, a,
    eqn = function(x, midpoint, slope) {
      x^slope / (midpoint^slope + x^slope)
    }
  )

  vars <- as.data.frame(sfm)
  expect_equal(vars[["name"]], "a")
  expect_equal(vars[["type"]], "func")
  expect_equal(gsub("\\n| ", "", vars[["eqn"]]), "function(x,midpoint,slope){x^slope/(midpoint^slope+x^slope)}")
})


test_that("eqn with NSE functions specified with variable", {
  sfm <- stockflow()
  f <- function(x, midpoint, slope) {
    x^slope / (midpoint^slope + x^slope)
  }
  sfm <- custom_func(sfm, a,
    eqn = !!f
  )

  vars <- as.data.frame(sfm)
  expect_equal(vars[["name"]], "a")
  expect_equal(vars[["type"]], "func")
  expect_equal(gsub("\\n| ", "", vars[["eqn"]]), "function(x,midpoint,slope){x^slope/(midpoint^slope+x^slope)}")
})

test_that("update(): do.call still works", {
  sfm <- stockflow()
  sfm <- do.call(update.stockflow, list(object = sfm, name = "x", type = "stock", eqn = "10"))
  vars <- as.data.frame(sfm)
  expect_equal(vars[["name"]], "x")
  expect_equal(vars[["eqn"]], "10")
})

test_that("update() with !! injection for name", {
  sfm <- stockflow()
  my_name <- "population"
  sfm <- update(sfm, !!my_name, "stock", eqn = 100)
  vars <- as.data.frame(sfm)
  expect_equal(vars[["name"]], "population")
})

test_that("update() with !! injection for eqn", {
  sfm <- stockflow()
  my_eqn <- "a * b + 1"
  sfm <- update(sfm, x, "aux", eqn = !!my_eqn)
  vars <- as.data.frame(sfm)
  expect_equal(vars[["eqn"]], "a * b + 1")
})

test_that("update() modifying existing variable with NSE name", {
  sfm <- stockflow()
  sfm <- update(sfm, x, "stock", eqn = 10)
  sfm <- update(sfm, x, eqn = 20)
  vars <- as.data.frame(sfm)
  expect_equal(vars[["eqn"]], "20")
})

test_that("update() with df argument still works (no NSE interference)", {
  df <- data.frame(
    type = c("stock", "flow"),
    name = c("S", "In_flow"),
    eqn = c("5", "S * 0.1"),
    to = c(NA, "S"),
    stringsAsFactors = FALSE
  )
  sfm <- stockflow()
  sfm <- update(sfm, df = df)
  vars <- as.data.frame(sfm)
  expect_equal(nrow(vars), 2)
})


# ==============================================================================
# Wrapper functions with NSE
# ==============================================================================

test_that("stock() accepts bare symbols", {
  sfm <- stockflow()
  sfm <- stock(sfm, population, eqn = 100, label = "Population")
  vars <- as.data.frame(sfm)
  expect_equal(vars[["name"]], "population")
  expect_equal(vars[["eqn"]], "100")
  expect_equal(vars[["label"]], "Population")
})

test_that("flow() accepts bare symbols for name, to, from", {
  sfm <- stockflow()
  sfm <- stock(sfm, pop, eqn = 100)
  sfm <- flow(sfm, births, eqn = pop * 0.1, to = pop)
  vars <- as.data.frame(sfm)
  expect_equal(vars[vars[["name"]] == "births", "to"], "pop")
  # eqn deparsed from expression
  expect_equal(vars[vars[["name"]] == "births", "eqn"], "pop * 0.1")
})

test_that("constant() accepts bare symbols", {
  sfm <- stockflow()
  sfm <- constant(sfm, growth_rate, eqn = 0.1)
  vars <- as.data.frame(sfm)
  expect_equal(vars[["name"]], "growth_rate")
  expect_equal(vars[["eqn"]], "0.1")
})

test_that("auxiliary() / aux() accepts bare symbols", {
  sfm <- stockflow() |>
    stock("pop", eqn = "100") |>
    constant("K", eqn = "1000")
  sfm <- auxiliary(sfm, density, eqn = pop / K)
  vars <- as.data.frame(sfm)
  expect_equal(vars[vars[["name"]] == "density", "eqn"], "pop / K")
})

test_that("lookup() accepts bare symbols", {
  sfm <- stockflow()
  sfm <- lookup(sfm, my_gf,
    xpts = c(0, 5, 10), ypts = c(0, 10, 15),
    source = t
  )
  vars <- as.data.frame(sfm)
  expect_equal(vars[["name"]], "my_gf")
  expect_equal(vars[["source"]], "t")
})


# ==============================================================================
# discard() with NSE
# ==============================================================================

test_that("discard() accepts bare symbol for name", {
  sfm <- stockflow() |>
    update("x", "stock", eqn = "1")

  sfm <- discard(sfm, x)
  vars <- as.data.frame(sfm)
  expect_equal(nrow(vars), 0)
})

test_that("discard() with c() of bare symbols", {
  sfm <- stockflow() |>
    update("a", "stock") |>
    update("b", "stock")
  sfm <- discard(sfm, c(a, b))
  vars <- as.data.frame(sfm)
  expect_equal(nrow(vars), 0)
})


# ==============================================================================
# change_name() with NSE
# ==============================================================================

test_that("change_name() accepts bare symbols", {
  sfm <- stockflow() |>
    update("old_name", "stock")
  sfm <- change_name(sfm, old_name, new_name = new_name_var)
  vars <- as.data.frame(sfm)
  expect_equal(vars[["name"]], "new_name_var")
})

test_that("change_name() accepts unquoted new_name that requires cleaning", {
  sfm <- stockflow() |> update("alpha", "constant", eqn = 1)

  # Should not error when new_name needs cleaning (e.g., 't' -> cleaned form)
  expect_warning(expect_no_error(sfm <- change_name(sfm, alpha, new_name = t)), "name was changed")

  df <- as.data.frame(sfm)
  expect_equal(nrow(df), 1)
  expect_false("alpha" %in% df[["name"]])
  expect_true(any(grepl("^t", df[["name"]])))
})


# ==============================================================================
# change_type() with NSE
# ==============================================================================

test_that("change_type() accepts bare symbols", {
  sfm <- stockflow() |>
    update("delta", "constant", eqn = "0.025")
  sfm <- change_type(sfm, delta, new_type = aux)
  vars <- as.data.frame(sfm)
  expect_equal(vars[vars[["name"]] == "delta", "type"], "aux")
})


# ==============================================================================
# Pipe chains with full NSE
# ==============================================================================

test_that("full pipe chain with NSE works end-to-end", {
  sfm <- stockflow() |>
    stock(prey, eqn = 50) |>
    stock(predator, eqn = 10) |>
    flow(prey_births, eqn = alpha * prey, to = prey) |>
    flow(prey_deaths, eqn = beta * prey * predator, from = prey) |>
    constant(alpha, eqn = 0.5) |>
    constant(beta, eqn = 0.05)

  vars <- as.data.frame(sfm)
  expect_equal(nrow(vars), 6)
  expect_true("prey" %in% vars[["name"]])
  expect_true("predator" %in% vars[["name"]])
  expect_equal(vars[vars[["name"]] == "prey_births", "to"], "prey")
  expect_equal(vars[vars[["name"]] == "prey_births", "eqn"], "alpha * prey")
})


# ==============================================================================
# as.data.frame.stockflow() with NSE
# ==============================================================================

test_that("as.data.frame() accepts bare symbol for vars", {
  sfm <- stockflow("sir")
  df <- as.data.frame(sfm, vars = susceptible)
  expect_equal(nrow(df), 1L)
  expect_equal(df[["name"]], "susceptible")
})

test_that("as.data.frame() accepts c() of bare symbols for vars", {
  sfm <- stockflow("sir")
  df <- as.data.frame(sfm, vars = c(susceptible, infected))
  expect_equal(nrow(df), 2L)
  expect_true(all(c("susceptible", "infected") %in% df[["name"]]))
})


test_that("as.data.frame() accepts strings for vars", {
  sfm <- stockflow("sir")
  df_vars <- as.data.frame(sfm, vars = "susceptible")
  expect_equal(nrow(df_vars), 1L)
})
