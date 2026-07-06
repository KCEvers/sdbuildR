# Tests for import_desolve()


# ---- input validation ----

test_that("import_desolve() errors on non-function model", {
  expect_error(
    import_desolve("not_a_function", c(r = 0.5), c(X = 1), seq(0, 10)),
    class = "rlang_error"
  )
})


test_that("import_desolve() errors on wrong function arguments", {
  bad_model <- function(time, states, pars) {
    with(as.list(c(states, pars)), {
      dX <- r * X
      list(c(dX))
    })
  }
  expect_error(
    import_desolve(bad_model, c(r = 0.5), c(X = 1), seq(0, 10)),
    class = "rlang_error"
  )
})


test_that("import_desolve() errors on unnamed params", {
  expect_error(
    import_desolve(logistic_model_deSolve, c(0.5, 100), c(N = 10), seq(0, 10)),
    class = "rlang_error"
  )
})


test_that("import_desolve() errors on unnamed init", {
  expect_error(
    import_desolve(logistic_model_deSolve, c(r = 0.5, K = 100), c(10), seq(0, 10)),
    class = "rlang_error"
  )
})


test_that("import_desolve() errors on non-numeric times", {
  expect_error(
    import_desolve(
      logistic_model_deSolve, c(r = 0.5, K = 100), c(N = 10),
      c("0", "1", "2")
    ),
    class = "rlang_error"
  )
})


test_that("import_desolve() errors on unevenly spaced times", {
  expect_error(
    import_desolve(
      logistic_model_deSolve, c(r = 0.5, K = 100), c(N = 10),
      c(0, 1, 3, 6)
    ), # uneven gaps
    class = "rlang_error"
  )
})


# ---- non-canonical body ----

test_that("import_desolve() errors on body without with() block", {
  bad_model <- function(t, state, parameters) {
    dX <- parameters["r"] * state["X"]
    list(c(dX))
  }
  expect_error(
    import_desolve(bad_model, c(r = 0.5), c(X = 1), seq(0, 10)),
    class = "rlang_error"
  )
})


test_that("import_desolve() errors on missing derivative for a state variable", {
  incomplete_model <- function(t, state, parameters) {
    with(as.list(c(state, parameters)), {
      dX <- r * X
      list(c(dX)) # Y has no dY
    })
  }
  expect_error(
    import_desolve(incomplete_model, c(r = 0.5), c(X = 1, Y = 2), seq(0, 10)),
    class = "rlang_error"
  )
})


# ---- correct imports ----

test_that("import_desolve() logistic growth: returns stockflow object", {
  sfm <- import_desolve(
    model  = logistic_model_deSolve,
    params = c(r = 0.3, K = 100),
    init   = c(N = 10),
    times  = seq(0, 50, by = 0.1),
    name   = "Logistic growth"
  )
  expect_s3_class(sfm, "stockflow")
})


test_that("import_desolve() logistic growth: meta name is set", {
  sfm <- import_desolve(
    model  = logistic_model_deSolve,
    params = c(r = 0.3, K = 100),
    init   = c(N = 10),
    times  = seq(0, 50, by = 0.1),
    name   = "Logistic growth"
  )
  expect_equal(sfm[["meta"]][["name"]], "Logistic growth")
})


test_that("import_desolve() logistic growth: sim_settings are correct", {
  sfm <- import_desolve(
    model  = logistic_model_deSolve,
    params = c(r = 0.3, K = 100),
    init   = c(N = 10),
    times  = seq(0, 50, by = 0.1)
  )
  expect_equal(as.numeric(sfm[["sim_settings"]][["start"]]), 0)
  expect_equal(as.numeric(sfm[["sim_settings"]][["stop"]]), 50)
  expect_equal(as.numeric(sfm[["sim_settings"]][["dt"]]), 0.1)
})


test_that("import_desolve() logistic growth: method is stored", {
  sfm <- import_desolve(
    model  = logistic_model_deSolve,
    params = c(r = 0.3, K = 100),
    init   = c(N = 10),
    times  = seq(0, 50, by = 0.1),
    method = "rk4"
  )
  expect_equal(sfm[["sim_settings"]][["method"]], "rk4")
})


test_that("import_desolve() logistic growth: variables have correct types", {
  sfm <- import_desolve(
    model  = logistic_model_deSolve,
    params = c(r = 0.3, K = 100),
    init   = c(N = 10),
    times  = seq(0, 50, by = 0.1)
  )
  vars <- sfm[["variables"]]
  expect_true("N" %in% vars$name[vars$type == "stock"])
  expect_true("r" %in% vars$name[vars$type == "constant"])
  expect_true("K" %in% vars$name[vars$type == "constant"])
  net_N <- vars[vars$name == "net_N", ]
  expect_equal(nrow(net_N), 1L)
  expect_equal(net_N$to, "N")
})


test_that("import_desolve() SIR: aux variables created correctly", {
  sfm <- import_desolve(
    model  = sir_model_deSolve,
    params = c(beta = 0.3, gamma = 0.1, N = 1000),
    init   = c(S = 990, I = 10, R = 0),
    times  = seq(0, 100, by = 0.1)
  )
  vars <- sfm[["variables"]]
  expect_setequal(vars$name[vars$type == "stock"], c("S", "I", "R"))
  expect_setequal(vars$name[vars$type == "constant"], c("beta", "gamma", "N"))
  expect_setequal(vars$name[vars$type == "aux"], c("SI", "IR"))
  expect_setequal(vars$name[vars$type == "flow"], c("net_S", "net_I", "net_R"))
})


test_that("import_desolve() model using = assignment parses correctly", {
  eq_model <- function(t, state, parameters) {
    with(as.list(c(state, parameters)), {
      dX <- r * X
      list(c(dX))
    })
  }
  sfm <- expect_no_error(
    import_desolve(eq_model, c(r = 0.2), c(X = 1), seq(0, 10))
  )
  vars <- sfm[["variables"]]
  expect_true("X" %in% vars$name[vars$type == "stock"])
})


test_that("import_desolve() produces a simulatable model (logistic)", {
  sfm <- import_desolve(
    model  = logistic_model_deSolve,
    params = c(r = 0.3, K = 100),
    init   = c(N = 10),
    times  = seq(0, 10, by = 0.1)
  )
  sfm <- sim_settings(sfm, save_by = 1)
  sim <- expect_no_error(simulate(sfm))
  expect_s3_class(sim, "simulate_stockflow")
})


test_that("import_desolve() produces a simulatable model (SIR)", {
  sfm <- import_desolve(
    model  = sir_model_deSolve,
    params = c(beta = 0.3, gamma = 0.1, N = 1000),
    init   = c(S = 990, I = 10, R = 0),
    times  = seq(0, 50, by = 0.1)
  )
  sfm <- sim_settings(sfm, save_by = 5)
  sim <- expect_no_error(simulate(sfm))
  expect_s3_class(sim, "simulate_stockflow")
})
