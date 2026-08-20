#' Build the `method` argument for `deSolve::ode()`
#'
#' `deSolve::ode()` only accepts its own built-in solvers as a character string. The
#' Runge-Kutta tableaux listed by `deSolve::rkMethod()` have to be passed as an object
#' instead, so a name such as `"rk45dp7"` errors when it is quoted. Emit whichever form
#' the solver actually needs.
#'
#' @param method Solver name, already validated by [sim_methods()].
#'
#' @returns Character string of R code to splice into the generated script.
#' @noRd
r_solver_arg <- function(method) {
  # The set `deSolve::ode()` accepts by name; everything else supported by sdbuildR is
  # an rkMethod() tableau.
  builtin <- c(
    "lsoda", "lsode", "lsodes", "lsodar", "vode", "daspk",
    "euler", "rk4", "ode23", "ode45", "radau",
    "bdf", "bdf_d", "adams", "impAdams", "impAdams_d", "iteration"
  )

  if (method %in% builtin) {
    paste0("'", method, "'")
  } else {
    paste0("deSolve::rkMethod('", method, "')")
  }
}


#' Translate between deSolve and DifferentialEquations.jl solver names
#'
#' Translate between deSolve and DifferentialEquations.jl solver names, or validate that a given solver name is recognized in either language. This is used internally to allow users to specify familiar R solvers when using Julia for simulation, and to provide warnings when an exact equivalent is not available.
#'
#' @param method Solver name to validate or translate.
#' @param from Source solver family, either `"R"` or `"Julia"`.
#' @param to Target solver family when translating, either `"R"` or `"Julia"`.
#'
#' @return A character scalar (validated or translated solver name), a character vector of solver
#'   names when `method` is omitted, or a named list of solver names for both languages when called
#'   with no arguments.
#' @export
#' @concept simulate
#' @examples
#' # List supported solvers
#' sim_methods()
#'
#' # List supported R solvers
#' sim_methods(from = "R")
#'
#' # List supported Julia solvers
#' sim_methods(from = "Julia")
#'
#' # Validate or translate specific solvers
#' sim_methods("rk4", from = "R", to = "Julia")
sim_methods <- function(method, from = NULL, to = NULL) {
  solver_dict <- list(
    r_to_julia = list(
      euler = list(translation = "Euler()", alternatives = NULL, approximate = FALSE),
      # deSolve's rk2 tableau is c = (0, 1), b = (1/2, 1/2), i.e. Heun's method - not the
      # midpoint method. The two agree to floating-point precision.
      rk2 = list(translation = "Heun()", alternatives = c("Midpoint()"), approximate = FALSE),
      rk4 = list(translation = "RK4()", alternatives = NULL, approximate = FALSE),
      rk23bs = list(translation = "BS3()", alternatives = c("ode23"), approximate = FALSE),
      ode23 = list(translation = "BS3()", alternatives = c("rk23bs"), approximate = FALSE),
      rk45dp6 = list(translation = "Tsit5()", alternatives = NULL, approximate = TRUE),
      rk45dp7 = list(translation = "Tsit5()", alternatives = NULL, approximate = TRUE),
      rk45e = list(translation = "Tsit5()", alternatives = NULL, approximate = TRUE),
      rk45f = list(translation = "Tsit5()", alternatives = NULL, approximate = TRUE),
      rk45ck = list(translation = "Tsit5()", alternatives = NULL, approximate = TRUE),
      rk78dp = list(translation = "Vern8()", alternatives = c("Vern7()", "Vern9()"), approximate = TRUE),
      rk78f = list(translation = "Vern8()", alternatives = c("Vern7()", "Vern9()"), approximate = TRUE),
      ode45 = list(translation = "Tsit5()", alternatives = NULL, approximate = TRUE),
      irk3r = list(translation = "Rosenbrock23()", alternatives = NULL, approximate = TRUE),
      irk5r = list(translation = "Rosenbrock23()", alternatives = NULL, approximate = TRUE),
      irk4hh = list(translation = "Rosenbrock23()", alternatives = NULL, approximate = TRUE),
      irk4l = list(translation = "Rosenbrock23()", alternatives = NULL, approximate = TRUE),
      irk6kb = list(translation = "Rosenbrock23()", alternatives = NULL, approximate = TRUE),
      irk6l = list(translation = "Rosenbrock23()", alternatives = NULL, approximate = TRUE),
      lsoda = list(translation = "Tsit5()", alternatives = NULL, approximate = TRUE),
      lsodar = list(translation = "Tsit5()", alternatives = NULL, approximate = TRUE),
      lsode = list(translation = "Rosenbrock23()", alternatives = NULL, approximate = TRUE),
      lsodes = list(translation = "Rosenbrock23()", alternatives = NULL, approximate = TRUE),
      bdf = list(translation = "Rosenbrock23()", alternatives = NULL, approximate = TRUE),
      bdf_d = list(translation = "Rosenbrock23()", alternatives = NULL, approximate = TRUE),
      vode = list(translation = "Rosenbrock23()", alternatives = NULL, approximate = TRUE),
      daspk = list(translation = "Rosenbrock23()", alternatives = NULL, approximate = TRUE),
      adams = list(translation = "Vern6()", alternatives = c("Vern7()", "Vern8()"), approximate = TRUE),
      impAdams = list(translation = "Rosenbrock23()", alternatives = NULL, approximate = TRUE),
      impAdams_d = list(translation = "Rosenbrock23()", alternatives = NULL, approximate = TRUE),
      radau = list(translation = "Rosenbrock23()", alternatives = NULL, approximate = TRUE)
    ),
    julia_to_r = list(
      "Euler()" = list(translation = "euler", alternatives = NULL),
      # OrdinaryDiffEq has no ForwardEuler; accept the name but resolve it to Euler().
      "ForwardEuler()" = list(translation = "euler", alternatives = NULL, canonical = "Euler()"),
      # deSolve has no midpoint tableau among the supported solvers; rk2 is Heun's.
      "Midpoint()" = list(translation = NULL, alternatives = c("rk2")),
      "Heun()" = list(translation = "rk2", alternatives = NULL),
      "RK4()" = list(translation = "rk4", alternatives = NULL),
      "BS3()" = list(translation = "rk23bs", alternatives = c("ode23")),
      "Tsit5()" = list(translation = "rk45dp6", alternatives = c("rk45dp6", "rk45dp7", "rk45e", "rk45f", "rk45ck", "ode45")),
      "Vern6()" = list(translation = NULL, alternatives = c("adams")),
      "Vern7()" = list(translation = NULL, alternatives = c("rk78dp", "rk78f", "adams")),
      "Vern8()" = list(translation = NULL, alternatives = c("rk78dp", "rk78f")),
      "Vern9()" = list(translation = NULL, alternatives = c("rk78dp", "rk78f")),
      "Rosenbrock23()" = list(translation = NULL, alternatives = c("irk3r", "irk5r", "irk4hh", "irk4l", "irk6kb", "irk6l", "lsoda", "lsodar", "lsode", "lsodes", "bdf", "bdf_d", "vode", "daspk", "impAdams", "impAdams_d", "radau"))
    )
  )

  deSolve_methods <- c(
    names(solver_dict$r_to_julia),
    deSolve::rkMethod(),
    "lsoda", "lsode", "lsodes", "lsodar", "vode", "daspk",
    "euler", "rk4", "ode23", "ode45", "radau", "bdf", "bdf_d",
    "adams", "impAdams", "impAdams_d", "iteration"
  )

  normalize_julia_name <- function(x) {
    if (x %in% names(solver_dict$julia_to_r)) {
      return(x)
    }
    variants <- c(
      paste0(x, "()"),
      paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x))),
      paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)), "()"),
      tolower(x)
    )
    for (variant in variants) {
      if (variant %in% names(solver_dict$julia_to_r)) {
        return(variant)
      }
    }
    NULL
  }

  if (missing(method) && is.null(from)) {
    return(list(R = names(solver_dict$r_to_julia), Julia = names(solver_dict$julia_to_r)))
  }

  if (!is.null(from)) from <- clean_language(from)
  if (!is.null(to)) to <- clean_language(to)

  if (missing(method)) {
    if (from == "R") {
      return(names(solver_dict$r_to_julia))
    }
    return(names(solver_dict$julia_to_r))
  }

  if (is.null(from)) {
    cli::cli_abort(c(
      "x" = "{.arg from} is required when {.arg method} is provided.",
      "i" = 'Specify {.val "R"} or {.val "Julia"}.'
    ))
  }

  translate <- !is.null(to)

  if (from == "R") {
    if (!method %in% names(solver_dict$r_to_julia)) {
      if (method %in% deSolve_methods) {
        cli::cli_abort(c(
          "x" = "{.val {method}} is a valid deSolve method but is not supported for translation.",
          "i" = "Use {.code sim_methods(from = 'R')} to see supported R methods."
        ))
      }
      cli::cli_abort(c(
        "x" = "Unknown R solver method {.val {method}}.",
        "i" = "Use {.code sim_methods(from = 'R')} to see supported R methods."
      ))
    }
    solver_info <- solver_dict$r_to_julia[[method]]
    if (!translate) {
      return(method)
    }
    if (isTRUE(solver_info$approximate)) {
      cli::cli_warn(c(
        "!" = "No exact Julia equivalent for {.val {method}}.",
        "i" = "Using {.val {solver_info$translation}} as the closest supported Julia solver."
      ))
    }
    return(solver_info$translation)
  }

  method_clean <- normalize_julia_name(method)
  if (is.null(method_clean)) {
    cli::cli_abort(c(
      "x" = "Unknown Julia solver {.val {method}}.",
      "i" = "Use {.code sim_methods(from = 'Julia')} to see supported Julia methods."
    ))
  }
  solver_info <- solver_dict$julia_to_r[[method_clean]]
  if (!translate) {
    # Resolve aliases to the solver Julia actually defines.
    return(if (is.null(solver_info$canonical)) method_clean else solver_info$canonical)
  }
  if (!is.null(solver_info$translation)) {
    return(solver_info$translation)
  }
  cli::cli_warn(c(
    "!" = "No exact R equivalent for {.val {method_clean}}.",
    "i" = "Using {.val {solver_info$alternatives[[1]]}} as the closest supported R solver."
  ))
  return(solver_info$alternatives[[1]])
}
