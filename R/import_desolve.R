#' Import a deSolve model
#'
#' Convert a model written for [deSolve](https://cran.r-project.org/package=deSolve)
#' into a stock-and-flow model of class [`stockflow`][stockflow].
#'
#' The model function must follow the canonical deSolve convention:
#'
#' ```r
#' model <- function(t, state, parameters) {
#'   with(as.list(c(state, parameters)), {
#'     dX <- <rate expression>   # d<VarName> for each state in init
#'     list(c(dX))
#'   })
#' }
#' ```
#'
#' State variable names are taken from `names(init)`, parameter names from
#' `names(params)`. Each `d<VarName>` assignment inside the `with()` block is
#' parsed as the net rate of change for stock `VarName` and becomes a flow in
#' the sfm. Any other assignments in the `with()` block (intermediate
#' calculations) are imported as auxiliary variables in the order they appear.
#'
#' @param model A deSolve-style ODE function with arguments `(t, state, parameters)`.
#' @param params Named numeric vector of model parameters (constants).
#' @param init Named numeric vector of initial state values (stocks).
#' @param times Numeric vector of time points. Must be evenly spaced
#'   (e.g., from `seq(start, stop, by = dt)`).
#' @param method Integration method. Defaults to `"lsoda"`. See [sim_methods()].
#' @param name Optional model name. Character scalar.
#'
#' @returns A stock-and-flow model of class [`stockflow`][stockflow].
#' @export
#' @concept importExport
#' @seealso [import_insightmaker()], [export_model()], [update()]
#'
#' @examples
#' logistic_model <- function(t, state, parameters) {
#'   with(as.list(c(state, parameters)), {
#'     dN <- r * N * (1 - N / K)
#'     list(c(dN))
#'   })
#' }
#' sfm <- import_desolve(
#'   model  = logistic_model,
#'   params = c(r = 0.3, K = 100),
#'   init   = c(N = 10),
#'   times  = seq(0, 50, by = 0.1),
#'   method = "lsoda",
#'   name   = "Logistic growth"
#' )
#' \dontshow{
#' sfm <- sim_settings(sfm, save_by = 5)
#' }
#' sim <- simulate(sfm)
#' plot(sim)
import_desolve <- function(model, params, init, times,
                           method = "lsoda", name = NULL) {
  if (!is.function(model)) {
    cli::cli_abort(c(
      "{.arg model} must be a function.",
      "i" = "Provide a deSolve-style function with arguments {.code (t, state, parameters)}."
    ))
  }

  fargs <- names(formals(model))
  if (!identical(fargs, c("t", "state", "parameters"))) {
    cli::cli_abort(c(
      "{.arg model} must have exactly the arguments {.code (t, state, parameters)}.",
      "x" = "Found: {.code ({paste(fargs, collapse = ', ')})}."
    ))
  }

  if (!is.numeric(params) || is.null(names(params)) || any(!nzchar(names(params)))) {
    cli::cli_abort("{.arg params} must be a named numeric vector.")
  }

  if (!is.numeric(init) || is.null(names(init)) || any(!nzchar(names(init)))) {
    cli::cli_abort("{.arg init} must be a named numeric vector.")
  }

  if (!is.numeric(times) || length(times) < 2L) {
    cli::cli_abort("{.arg times} must be a numeric vector with at least two elements.")
  }

  dts <- diff(times)
  tol <- sqrt(.Machine$double.eps) * max(abs(times))
  if (any(abs(dts - dts[[1L]]) > tol)) {
    cli::cli_abort(c(
      "{.arg times} must be evenly spaced.",
      "i" = "Use {.code seq(start, stop, by = dt)} to generate a uniform time sequence."
    ))
  }

  parsed <- .parse_desolve_body(model, names(init), names(params))

  sfm <- stockflow()

  if (!is.null(name)) {
    sfm <- meta(sfm, name = name)
  }

  sfm <- do.call(sim_settings, list(
    object = sfm,
    start  = times[[1L]],
    stop   = times[[length(times)]],
    dt     = dts[[1L]],
    method = method
  ))

  for (pname in names(params)) {
    sfm <- do.call(update, list(
      object = sfm, name = pname, type = "constant",
      eqn = as.character(params[[pname]])
    ))
  }

  for (sname in names(init)) {
    sfm <- do.call(update, list(
      object = sfm, name = sname, type = "stock",
      eqn = as.character(init[[sname]])
    ))
  }

  for (aux_name in names(parsed[["aux_eqns"]])) {
    sfm <- do.call(update, list(
      object = sfm, name = aux_name, type = "aux",
      eqn = parsed[["aux_eqns"]][[aux_name]]
    ))
  }

  for (sname in names(parsed[["deriv_eqns"]])) {
    sfm <- do.call(update, list(
      object = sfm, name = paste0("net_", sname), type = "flow",
      to = sname, from = "",
      eqn = parsed[["deriv_eqns"]][[sname]]
    ))
  }

  sfm
}


.parse_desolve_body <- function(model, stock_names, param_names) {
  with_call <- .find_with_call(body(model))

  if (is.null(with_call)) {
    cli::cli_abort(c(
      "Cannot parse deSolve model body.",
      "x" = "Expected a {.code with(as.list(c(state, parameters)), {{...}})} block.",
      "i" = "Only the canonical deSolve pattern is supported by {.fn import_desolve}."
    ))
  }

  with_body <- with_call[[3L]]

  stmts <- if (is.call(with_body) && identical(with_body[[1L]], as.name("{"))) {
    lapply(seq_along(with_body)[-1L], function(i) with_body[[i]])
  } else {
    list(with_body)
  }

  deriv_eqns <- list()
  aux_eqns <- list()
  reserved <- c(stock_names, param_names)

  for (stmt in stmts) {
    if (!is.call(stmt)) next
    op <- as.character(stmt[[1L]])
    if (!(op %in% c("<-", "="))) next

    lhs <- as.character(stmt[[2L]])
    rhs_str <- paste(deparse(stmt[[3L]], width.cutoff = 500L), collapse = " ")

    if (startsWith(lhs, "d") && substring(lhs, 2L) %in% stock_names) {
      deriv_eqns[[substring(lhs, 2L)]] <- rhs_str
      next
    }

    if (lhs %in% reserved) next

    aux_eqns[[lhs]] <- rhs_str
  }

  missing_stocks <- setdiff(stock_names, names(deriv_eqns))
  if (length(missing_stocks) > 0L) {
    cli::cli_abort(c(
      "Missing derivative equation(s) in deSolve model.",
      "x" = "No {.code d<VarName>} assignment found for: {.val {missing_stocks}}.",
      "i" = "Each state variable in {.arg init} needs a {.code d<VarName> <- ...} assignment."
    ))
  }

  list(deriv_eqns = deriv_eqns, aux_eqns = aux_eqns)
}


.find_with_call <- function(expr) {
  if (!is.call(expr)) {
    return(NULL)
  }

  if (identical(expr[[1L]], as.name("with")) && length(expr) >= 3L) {
    return(expr)
  }

  for (i in seq_along(expr)) {
    result <- .find_with_call(expr[[i]])
    if (!is.null(result)) {
      return(result)
    }
  }

  NULL
}
