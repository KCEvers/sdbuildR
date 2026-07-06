#' Run model diagnostics
#'
#' Check for common formulation problems in a stock-and-flow model.
#'
#' The following problems are detected:
#' - An absence of stocks
#' - Flows without a source (`from`) or target (`to`)
#' - Flows connected to a stock that does not exist
#' - Undefined variable references in equations
#' - Circularity in equations
#'
#' The following potential problems are detected:
#' - Absence of flows
#' - Stocks without inflows or outflows
#' - Equations with a value of 0
#'
#' @inheritParams update.stockflow
#'
#' @returns Object of class `summary_stockflow`. A flat named list with one
#'   entry per check. Each entry contains a `problem` field (`"none"`,
#'   `"warning"`, or `"error"`) and type-specific data fields.
#' @concept build
#' @method summary stockflow
#' @export
#'
#' @examples
#' # No issues
#' sfm <- stockflow("sir")
#' summary(sfm)
#'
#' # Detect absence of stocks or flows
#' sfm <- stockflow()
#' summary(sfm)
#'
#' # Detect stocks without inflows or outflows
#' sfm <- stockflow() |> update("Prey", "stock")
#' summary(sfm)
#'
#' # Detect circularity in equation definitions
#' sfm <- stockflow() |>
#'   update("Prey", "stock", eqn = "Predator") |>
#'   update("Predator", "stock", eqn = "Prey")
#' summary(sfm)
#'
summary.stockflow <- function(object, ...) {
  check_stockflow(object)

  # Initialize all checks with "none" (all data fields empty)
  checks <- list(
    no_stocks            = list(problem = "none"),
    no_flows             = list(problem = "none"),
    disconnected_stocks  = list(problem = "none", stocks = character()),
    disconnected_flows   = list(problem = "none", flows = character()),
    bad_flow_connections = list(problem = "none", flows = character()),
    zero_equations       = list(problem = "none", variables = character()),
    undefined_vars       = list(problem = "none", refs = list()),
    unit_test_refs       = list(problem = "none", refs = list()),
    circular_static      = list(problem = "none", cycle_nodes = character(), edge_list = NULL),
    circular_dynamic     = list(problem = "none", cycle_nodes = character(), edge_list = NULL)
  )

  # Get stock and flow names
  stock_names <- object[["variables"]][object[["variables"]][["type"]] == "stock", "name"]
  flow_df <- get_flow_df(object)
  flow_names <- flow_df[["name"]]

  ### Check whether all stocks have inflows and/or outflows
  if (length(stock_names) > 0) {
    if (nrow(flow_df) > 0) {
      idx <- stock_names %in% flow_df[["to"]] | stock_names %in% flow_df[["from"]]
    } else {
      # If there are no flows, all stocks are disconnected
      idx <- rep(FALSE, length(stock_names))
    }

    if (any(!idx)) {
      checks[["disconnected_stocks"]] <- list(
        problem = "warning",
        stocks = stock_names[!idx]
      )
    }
  } else {
    checks[["no_stocks"]] <- list(problem = "error")
  }

  ### Check whether all flows either have a from or to property
  if (length(flow_names) > 0) {
    idx <- !nzchar(flow_df[["from"]]) & !nzchar(flow_df[["to"]])

    if (any(idx)) {
      checks[["disconnected_flows"]] <- list(
        problem = "error",
        flows = flow_names[idx]
      )
    }

    ### Find whether the from and to stocks exist
    idx_to <- (!flow_df[["to"]] %in% stock_names) & nzchar(flow_df[["to"]])
    idx_from <- (!flow_df[["from"]] %in% stock_names) & nzchar(flow_df[["from"]])

    if (any(idx_to) || any(idx_from)) {
      checks[["bad_flow_connections"]] <- list(
        problem = "error",
        flows = c(flow_names[idx_to], flow_names[idx_from])
      )
    }
  } else {
    checks[["no_flows"]] <- list(problem = "warning")
  }

  ### Check equations with zero
  zero_idx <- object[["variables"]][["eqn"]] %in% c("0", "0.0")
  if (any(zero_idx)) {
    checks[["zero_equations"]] <- list(
      problem = "warning",
      variables = object[["variables"]][zero_idx, "name"]
    )
  }

  ### Detect undefined variable references in equations
  out <- detect_undefined_var(object)
  if (out[["issue"]]) {
    checks[["undefined_vars"]] <- list(
      problem = "error",
      refs = out[["data"]][["refs"]]
    )
  }

  ### Detect undefined variable references in unit tests
  out <- .detect_undefined_unit_test_vars(object)
  if (out[["issue"]]) {
    checks[["unit_test_refs"]] <- list(
      problem = "warning",
      refs = out[["data"]]
    )
  }

  ### Detect circularity in equations
  out <- order_equations(object, print_msg = FALSE)
  if (out[["static"]][["issue"]]) {
    checks[["circular_static"]] <- list(
      problem = "error",
      cycle_nodes = out[["static"]][["cycle_nodes"]],
      edge_list = out[["static"]][["edge_list"]]
    )
  }
  if (out[["dynamic"]][["issue"]]) {
    checks[["circular_dynamic"]] <- list(
      problem = "error",
      cycle_nodes = out[["dynamic"]][["cycle_nodes"]],
      edge_list = out[["dynamic"]][["edge_list"]]
    )
  }

  result <- new_summary_stockflow(checks)
  result
}


#' Constructor for summary_stockflow class
#'
#' @param checks Flat named list of check results, one entry per check.
#'   Each entry has a `problem` field (`"none"`, `"warning"`, or `"error"`)
#'   and type-specific data fields.
#'
#' @returns Object of class `summary_stockflow`
#' @noRd
new_summary_stockflow <- function(checks) {
  structure(checks, class = "summary_stockflow")
}


#' Print method for summary_stockflow
#'
#' @param x Object of class `summary_stockflow`
#' @param ... Ignored
#'
#' @returns `x` invisibly
#' @export
#' @concept build
print.summary_stockflow <- function(x, ...) {
  cli::cli_h1("Stock-and-Flow Model Diagnostics")

  errors <- Filter(function(y) y$problem == "error", x)
  warnings <- Filter(function(y) y$problem == "warning", x)

  if (length(errors) == 0 && length(warnings) == 0) {
    cli::cli_inform(c("v" = "No problems detected!"))
    return(invisible(x))
  }

  n_err <- length(errors)
  n_warn <- length(warnings)

  if (n_err > 0) {
    cli_h2_tight("Problem{?s} ({n_err})")
    lapply(names(errors), function(nm) .print_one_check(nm, errors[[nm]]))
  }

  if (n_warn > 0) {
    cli_h2_tight("Potential problem{?s} ({n_warn})")
    lapply(names(warnings), function(nm) .print_one_check(nm, warnings[[nm]]))
  }

  invisible(x)
}


#' Format and print a single check entry
#'
#' @param nm Component name (key in the summary_stockflow list)
#' @param check Named list with `problem` field and type-specific data fields
#'
#' @noRd
.print_one_check <- function(nm, check) {
  switch(nm,
    no_stocks = cli::cli_inform(c(
      "!" = "Model has no stocks.",
      ">" = "Add at least one stock with {.fn stock} or {.fn update}."
    )),
    no_flows = cli::cli_inform(c(
      "*" = "Model has no flows.",
      ">" = "Add flows with {.fn flow} or {.fn update}."
    )),
    disconnected_stocks = cli::cli_inform(c(
      "*" = "{cli::qty(length(check$stocks))}Stock{?s} not connected to any flow: {.code {check$stocks}}."
    )),
    disconnected_flows = cli::cli_inform(c(
      "!" = "{cli::qty(length(check$flows))}Flow{?s} not connected to any stock: {.code {check$flows}}.",
      ">" = "Connect with {.arg to} and/or {.arg from} in {.fn flow} or {.fn update}."
    )),
    bad_flow_connections = cli::cli_inform(c(
      "!" = "{cli::qty(length(check$flows))}Flow{?s} connected to a non-existent stock: {.code {check$flows}}."
    )),
    zero_equations = cli::cli_inform(c(
      "*" = "{cli::qty(length(check$variables))}{.code {check$variables}} {?has/have} an equation of 0."
    )),
    undefined_vars = {
      n <- length(check$refs)
      cli::cli_inform(c("!" = "{cli::qty(n)}Reference{?s} to undefined variable{?s} found."))
      lapply(check$refs, function(r) {
        cli::cli_inform(c(
          " " = "{.code {r$var}}${r$prop}: {cli::qty(length(r$undefined))}{.code {r$undefined}} {?is/are} undefined."
        ))
      })
      cli::cli_inform(c(">" = "Define these variables or check for spelling mistakes."))
    },
    circular_static = {
      cli::cli_inform(c("!" = "Circular dependency in static equations."))
      if (length(check$cycle_nodes) > 0) {
        cli::cli_inform(c(" " = "Variables involved: {.code {check$cycle_nodes}}."))
      }
      if (!is.null(check$edge_list) && nrow(check$edge_list) > 0) {
        for (i in seq_len(nrow(check$edge_list))) {
          cli::cli_inform(c(
            " " = "{.code {check$edge_list[i, 1]}} depends on {.code {check$edge_list[i, 2]}}."
          ))
        }
      }
    },
    circular_dynamic = {
      cli::cli_inform(c("!" = "Circular dependency in dynamic equations."))
      if (length(check$cycle_nodes) > 0) {
        cli::cli_inform(c(" " = "Variables involved: {.code {check$cycle_nodes}}."))
      }
      if (!is.null(check$edge_list) && nrow(check$edge_list) > 0) {
        for (i in seq_len(nrow(check$edge_list))) {
          cli::cli_inform(c(
            " " = "{.code {check$edge_list[i, 1]}} depends on {.code {check$edge_list[i, 2]}}."
          ))
        }
      }
    },
    unit_test_refs = {
      n <- length(check$refs)
      cli::cli_inform(c("*" = "{cli::qty(n)}Unit test{?s} reference{?s} undefined variable{?s}."))
      lapply(check$refs, function(r) {
        cli::cli_inform(c(
          " " = "[{r$test}] {.val {r$label}} {r$prop}: {cli::qty(length(r$undefined))}{.code {r$undefined}} {?is/are} undefined."
        ))
      })
      cli::cli_inform(c(">" = "Update the affected tests or add the missing variables."))
    },
    cli::cli_inform(c("!" = "Unknown check: {nm}."))
  )
}


#' Short label for a check entry (used in simulate() abort message)
#'
#' @param nm Component name string
#'
#' @returns Single character string
#' @noRd
.problem_label <- function(nm) {
  switch(nm,
    no_stocks            = "No stocks.",
    disconnected_flows   = "Flow(s) not connected to a stock.",
    bad_flow_connections = "Flow(s) connected to non-existent stock.",
    undefined_vars       = "Undefined variable reference(s).",
    circular_static      = "Circular dependency in static equations.",
    circular_dynamic     = "Circular dependency in dynamic equations.",
    unit_test_refs       = "Unit test(s) reference undefined variable(s).",
    nm
  )
}


#' Detect undefined variables in equations
#'
#' @inheritParams update.stockflow
#'
#' @returns List with `issue` (logical) and, if TRUE, `data` (structured problem list)
#' @noRd
#'
detect_undefined_var <- function(object) {
  vars_df <- object[["variables"]]
  n <- nrow(vars_df)

  # Get names
  var_names <- get_model_var(object)

  # Funcs and graphical functions can be functions
  gf_names <- vars_df[vars_df[["type"]] == "lookup", "name"]
  func_names <- get_funcs(object)[["name"]]
  possible_func <- c(
    func_names,
    gf_names,
    syntax_julia[["syntax_df"]][["R_first_iter"]],
    unlist(P),
    "pi", "letters", "LETTERS",
    "month.abb", "month.name"
  )

  # Pre-compute func argument names for func-type rows
  func_args <- vector("list", n)
  is_func <- vars_df[["type"]] == "func"
  if (any(is_func)) {
    func_args[which(is_func)] <- lapply(vars_df[["eqn"]][is_func], function(eqn) {
      parsed <- tryCatch(parse(text = eqn)[[1]], error = function(e) NULL)
      if (!is.null(parsed) && is.call(parsed) &&
        identical(parsed[[1]], as.name("function"))) {
        names(parsed[[2]])
      } else {
        character(0)
      }
    })
  }
  func_args[!is_func] <- list(character(0))

  # Pre-compute which fields are defined (vectorized column checks)
  fields <- c("eqn", "to", "from", "source")
  defined <- matrix(FALSE,
    nrow = n, ncol = length(fields),
    dimnames = list(NULL, fields)
  )
  for (f in fields) {
    col <- vars_df[[f]]
    defined[, f] <- !is.na(col) & nzchar(col)
  }

  # Batch .dependencies() calls — one per field instead of one per row per field
  all_deps <- stats::setNames(vector("list", length(fields)), fields)
  for (f in fields) {
    idx <- which(defined[, f])
    if (length(idx) == 0L) next
    vals <- stats::setNames(vars_df[[f]][idx], vars_df[["name"]][idx])
    all_deps[[f]] <- .dependencies(object, vals,
      only_var = TRUE,
      only_model_var = FALSE
    )
  }

  # Post-process: apply per-row exclusions and collect undefined references
  missing_ref <- list()
  for (i in seq_len(n)) {
    row_name <- vars_df[["name"]][i]
    allowed <- c(possible_func, setdiff(var_names, row_name), func_args[[i]])
    row_result <- list()

    for (f in fields) {
      if (!defined[i, f]) next
      deps <- all_deps[[f]][[row_name]]
      if (is.null(deps) || length(deps) == 0L) next
      undefined <- setdiff(deps, allowed)
      if (length(undefined) > 0L) row_result[[f]] <- undefined
    }

    if (length(row_result) > 0L) missing_ref[[row_name]] <- row_result
  }

  if (length(missing_ref) > 0) {
    refs <- lapply(seq_along(missing_ref), function(i) {
      x <- missing_ref[[i]]
      nm <- names(missing_ref)[i]
      lapply(seq_along(x), function(j) {
        list(var = nm, prop = names(x)[j], undefined = unname(x[[j]]))
      })
    }) |> unlist(recursive = FALSE)

    list(
      issue = TRUE,
      data = list(type = "undefined_vars", refs = refs)
    )
  } else {
    list(issue = FALSE)
  }
}


#' Detect undefined variable references in unit test expressions and conditions
#'
#' Reuses `get_model_var()` (same as `detect_undefined_var`) and `.ut_expr_vars()`
#' (shared helper in R/verify.R) to extract variable-like symbols from each unit
#' test's `expr_str` and `conditions`.
#'
#' @inheritParams update.stockflow
#' @return List with `issue` (logical) and `data` (list of refs)
#' @noRd
.detect_undefined_unit_test_vars <- function(object) {
  tests <- object[["unit_tests"]]
  if (length(tests) == 0L) {
    return(list(issue = FALSE, data = list()))
  }

  model_names <- get_model_var(object)

  refs <- list()
  for (idx in seq_along(tests)) {
    test <- tests[[idx]]
    # Check expr
    unknown_expr <- .ut_expr_vars(test[["expr_str"]], model_names)[["unknown"]]
    if (length(unknown_expr) > 0L) {
      refs <- c(refs, list(list(
        test = idx,
        label = test[["label"]],
        prop = "expr",
        undefined = unknown_expr
      )))
    }

    # Check conditions names
    bad_cond <- setdiff(names(test[["conditions"]]), model_names)
    if (length(bad_cond) > 0L) {
      refs <- c(refs, list(list(
        test = idx,
        label = test[["label"]],
        prop = "conditions",
        undefined = bad_cond
      )))
    }
  }

  list(issue = length(refs) > 0L, data = refs)
}
