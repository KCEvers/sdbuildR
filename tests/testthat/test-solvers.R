# Every solver sim_methods() advertises has to actually work, in both languages.
#
# sim_methods() is the public catalogue, so anything listed there is something a user can
# pass to sim_settings(). Two classes of bug live here and are invisible to any test that
# only exercises the default solver:
#
#   * a name the backend does not accept, so the generated script fails outright
#   * a name that runs but integrates something other than what it claims
#
# The fixture is the built-in logistic_model, dX/dt = r*X*(1 - X/K), which has a
# closed-form solution. That means each solver is checked against the true answer rather
# than against another solver, so a whole backend drifting in the same direction cannot
# hide. It is smooth and non-stiff, so no solver should struggle with it.

LOGISTIC_STOP <- 100
LOGISTIC_DT <- 0.05
LOGISTIC_SAVE_BY <- 5

# Parameters come from logistic_model in template_registry(): X0 = 0.01, r = 0.1, K = 1.
logistic_exact <- function(t, X0 = 0.01, r = 0.1, K = 1) {
  K / (1 + ((K - X0) / X0) * exp(-r * t))
}

# Solve the logistic model with one solver and return its trajectory, time-ordered.
solve_logistic <- function(language, method) {
  sfm <- stockflow(template = "logistic_model") |>
    sim_settings(
      stop = LOGISTIC_STOP,
      dt = LOGISTIC_DT,
      save_by = LOGISTIC_SAVE_BY,
      language = language,
      method = method
    )

  sim <- simulate(sfm, quiet = TRUE)
  traj <- sim[["df"]][sim[["df"]][["variable"]] == "X", c("time", "value")]
  traj[order(traj[["time"]]), ]
}

# Euler is first order, so it earns a looser bound than everything else. The tolerances
# are ~100x the errors actually observed, so this catches a broken solver without being
# brittle across deSolve / OrdinaryDiffEq versions.
solver_tolerance <- function(method) {
  if (method %in% c("euler", "Euler()", "ForwardEuler()")) 5e-3 else 1e-3
}

expect_solves_logistic <- function(traj, tol, label) {
  expect_equal(traj[["time"]], seq(0, LOGISTIC_STOP, by = LOGISTIC_SAVE_BY), info = label)
  expect_true(all(is.finite(traj[["value"]])), info = label)

  # Structural properties of logistic growth started below carrying capacity: strictly
  # increasing, and never above K. These hold for any correct solver at any accuracy.
  expect_true(all(diff(traj[["value"]]) > 0), info = label)
  expect_true(all(traj[["value"]] > 0 & traj[["value"]] <= 1 + tol), info = label)

  # And the numbers have to be right, not merely well-shaped.
  expect_lt(max(abs(traj[["value"]] - logistic_exact(traj[["time"]]))), tol)
}


# -- R ------------------------------------------------------------------------------

for (method in sim_methods(from = "R")) {
  test_that(paste0("R solver '", method, "' integrates the logistic model"), {
    traj <- solve_logistic("R", method)
    expect_solves_logistic(traj, solver_tolerance(method), method)
  })
}


# -- Julia --------------------------------------------------------------------------

for (method in sim_methods(from = "Julia")) {
  test_that(paste0("Julia solver '", method, "' integrates the logistic model"), {
    skip_if_julia_not_ready()
    traj <- solve_logistic("Julia", method)
    expect_solves_logistic(traj, solver_tolerance(method), method)
  })
}


# -- Translation --------------------------------------------------------------------

test_that("every R solver translates to a solver Julia supports", {
  julia_methods <- sim_methods(from = "Julia")

  for (method in sim_methods(from = "R")) {
    # Approximate translations warn; that is expected and tested separately.
    translated <- suppressWarnings(sim_methods(method, from = "R", to = "Julia"))
    expect_true(translated %in% julia_methods, info = method)
  }
})

test_that("every Julia solver translates to a solver R supports", {
  r_methods <- sim_methods(from = "R")

  for (method in sim_methods(from = "Julia")) {
    translated <- suppressWarnings(sim_methods(method, from = "Julia", to = "R"))
    expect_true(translated %in% r_methods, info = method)
  }
})

test_that("validation resolves aliases to a solver the backend defines", {
  # OrdinaryDiffEq has no ForwardEuler, so accepting the name unchanged would produce a
  # script that fails with UndefVarError.
  expect_equal(sim_methods("ForwardEuler()", from = "Julia"), "Euler()")

  # Case and parenthesis variants normalise too.
  expect_equal(sim_methods("euler", from = "Julia"), "Euler()")
  expect_equal(sim_methods("Tsit5", from = "Julia"), "Tsit5()")
})

test_that("unknown solvers are rejected in both languages", {
  expect_error(sim_methods("not_a_solver", from = "R"), "Unknown R solver")
  expect_error(sim_methods("NotASolver()", from = "Julia"), "Unknown Julia solver")

  # A real deSolve method that sdbuildR does not support gets its own message.
  expect_error(sim_methods("rk23", from = "R"), "not supported for translation")
})


# -- Cross-language agreement --------------------------------------------------------

test_that("solvers marked as exact equivalents agree across R and Julia", {
  skip_if_julia_not_ready()

  # Pairs sim_methods() reports as exact (approximate = FALSE). These are the same
  # Butcher tableau on both sides, so at a fixed step they should agree to within
  # floating-point noise - far tighter than the accuracy bounds above.
  pairs <- list(
    c("euler", "Euler()"),
    c("rk2", "Heun()"),
    c("rk4", "RK4()"),
    c("rk23bs", "BS3()"),
    c("ode23", "BS3()")
  )

  for (pair in pairs) {
    label <- paste(pair, collapse = " <-> ")
    r_traj <- solve_logistic("R", pair[1])
    julia_traj <- solve_logistic("Julia", pair[2])
    expect_equal(r_traj[["value"]], julia_traj[["value"]], tolerance = 1e-8, info = label)
  }
})

test_that("deSolve's rk2 is Heun's method, not the midpoint method", {
  # sim_methods() claims rk2 <-> Heun() is exact. Guard that against the tableau, so the
  # mapping cannot silently drift back to Midpoint().
  tableau <- deSolve::rkMethod("rk2")
  expect_equal(as.numeric(tableau[["c"]]), c(0, 1))
  expect_equal(as.numeric(tableau[["b1"]]), c(0.5, 0.5))

  expect_equal(sim_methods("rk2", from = "R", to = "Julia"), "Heun()")
  expect_equal(sim_methods("Heun()", from = "Julia", to = "R"), "rk2")
})

test_that("approximate translations warn and exact ones do not", {
  expect_no_warning(sim_methods("rk4", from = "R", to = "Julia"))
  expect_no_warning(sim_methods("euler", from = "R", to = "Julia"))

  expect_warning(sim_methods("lsoda", from = "R", to = "Julia"), "No exact Julia equivalent")
  expect_warning(sim_methods("Vern9()", from = "Julia", to = "R"), "No exact R equivalent")
})

test_that("deSolve solvers are emitted in the form deSolve::ode() accepts", {
  # Its built-in solvers go through as plain strings ...
  expect_equal(r_solver_arg("lsoda"), "'lsoda'")
  expect_equal(r_solver_arg("rk4"), "'rk4'")
  expect_equal(r_solver_arg("ode45"), "'ode45'")

  # ... but the rkMethod() tableaux have to be passed as objects. Quoting these makes
  # deSolve::ode() abort with "'arg' should be one of ...".
  expect_equal(r_solver_arg("rk45dp7"), "deSolve::rkMethod('rk45dp7')")
  expect_equal(r_solver_arg("irk3r"), "deSolve::rkMethod('irk3r')")
  expect_equal(r_solver_arg("rk2"), "deSolve::rkMethod('rk2')")
})
