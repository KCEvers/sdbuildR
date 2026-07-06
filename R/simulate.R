#' Simulate stock-and-flow model
#'
#' Simulate a stock-and-flow model with simulation specifications defined by [sim_settings()]. If `sim_settings(language = "julia")`, the Julia environment will first be set up with [use_julia()]. If any problems are detected by [summary()], the model cannot be simulated.
#'
#' @inheritParams update.stockflow
#' @inheritParams sim_settings
#' @param nsim Number of simulations to run (unused; see [ensemble()] for running multiple simulations).
#' @param quiet If `TRUE`, suppress informational messages such as progress and status updates. Warnings and errors are always shown. Defaults to `FALSE`.
#' @param ... Optional arguments passed to [sim_settings()]; these can be used to override the simulation specifications set in the model object.
#'
#' @returns Object of class [`simulate_stockflow`][simulate.stockflow()], a list containing:
#' \describe{
#'   \item{object}{Stock-and-flow model object of class [`stockflow`][stockflow]}
#'   \item{df}{Data frame: simulation results (time, variable, value)}
#'   \item{init}{Named vector: initial stock values}
#'   \item{constants}{Named vector: constant parameters}
#'   \item{script}{Character: generated simulation code (R or Julia)}
#'   \item{duration}{Numeric: simulation time in seconds}
#'   \item{success}{Logical: TRUE if completed without errors}
#'   \item{error_message}{NULL if completed without errors}
#' }
#'
#' Use [as.data.frame()][as.data.frame.simulate_stockflow()] to extract results, [plot()][plot.simulate_stockflow()] to visualize.
#'
#'
#' @export
#' @importFrom stats simulate
#' @method simulate stockflow
#' @concept simulate
#' @seealso [update()], [stockflow()], [summary()], [sim_settings()], [use_julia()]
#'
#' @examples
#' sfm <- stockflow("sir")
#' sim <- simulate(sfm)
#' plot(sim)
#'
#' # Obtain all model variables
#' sim <- simulate(sim_settings(sfm, only_stocks = FALSE))
#' plot(sim, show_constants = TRUE)
#'
simulate.stockflow <- function(
  object,
  nsim = 1, seed = NULL,
  quiet = FALSE,
  ...
) {
  check_stockflow(object)

  # Override sim_settings with any arguments passed via ...
  varargs <- list(...)
  # Add seed if seed was passed
  if (!missing(seed)) {
    varargs <- c(list(seed = seed), varargs)
  }
  if (length(varargs) > 0) {
    object <- do.call(sim_settings, c(list(object), varargs))
  }

  check_summary_diagnostics(object)

  only_stocks <- object[["sim_settings"]][["only_stocks"]]
  vars <- object[["sim_settings"]][["vars"]]

  output_args <- resolve_sim_output_args(object, only_stocks, vars)
  only_stocks <- output_args[["only_stocks"]]
  vars <- output_args[["vars"]]

  if (tolower(object[["sim_settings"]][["language"]]) == "julia") {
    return(simulate_julia(object,
      only_stocks = only_stocks,
      vars = vars,
      quiet = quiet
    ))
  } else if (tolower(object[["sim_settings"]][["language"]]) == "r") {
    return(simulate_r(object,
      only_stocks = only_stocks,
      vars = vars
    ))
  }
}


#' Create new object of class [`simulate_stockflow`][simulate.stockflow()]
#' @noRd
new_simulate_stockflow <- function(success = FALSE,
                                   error_message = NULL,
                                   object = NULL,
                                   df = NULL,
                                   init = NULL,
                                   constants = NULL,
                                   script = NULL,
                                   duration = NULL,
                                   ...) {
  obj <- list(
    success = success,
    error_message = error_message,
    object = object,
    df = df,
    init = init,
    constants = constants,
    script = script,
    duration = duration,
    ...
  )

  structure(obj, class = "simulate_stockflow")
}


#' Check class [`simulate_stockflow`][simulate.stockflow()]
#'
#' @param x A simulation of a stock-and-flow model of class [`simulate_stockflow`][simulate.stockflow()]
#'
#' @returns Invisible x if valid, otherwise an error is thrown
#' @noRd
#'
check_simulate_stockflow <- function(x) {
  if (!inherits(x, "simulate_stockflow")) {
    cli::cli_abort(c(
      "Invalid object type.",
      "x" = "Expected object of class {.cls simulate_stockflow}.",
      "i" = "Use {.fn simulate} to create a valid simulation object."
    ))
  }

  if (!is.logical(x$success) || length(x$success) != 1) {
    cli::cli_abort(c(
      "Invalid {.arg success} field.",
      "x" = "The {.arg success} field must be a single {.cls logical} value.",
      "i" = "Expected {.val {TRUE}} or {.val {FALSE}}."
    ))
  }

  if (x$success) {
    # Successful simulation must have these components
    if (is.null(x$df) || !is.data.frame(x$df)) {
      cli::cli_abort(c(
        "Missing or invalid simulation data.",
        "x" = "Successful simulation must have a {.cls data.frame} in {.arg df}.",
        "i" = "This field is populated by {.fn simulate} with results."
      ))
    }
    if (is.null(x$init)) {
      cli::cli_abort(c(
        "Missing initial stock values.",
        "x" = "Successful simulation must preserve initial values in {.arg init}.",
        "i" = "Initial values of all stocks should be recorded."
      ))
    }

    if (is.null(x$duration)) {
      cli::cli_abort(c(
        "Missing simulation duration.",
        "x" = "Successful simulation must have {.arg duration}."
      ))
    }
  } else {
    # Failed simulation should have error message
    if (is.null(x$error_message)) {
      cli::cli_warn(c(
        "Failed simulation missing error information.",
        "!" = "Field {.arg error_message} should be populated for failed simulations."
      ))
    }
  }

  invisible(x)
}


#' Print simulation of a stock-and-flow model
#'
#' Prints the first rows of the simulation results in wide format. For a
#' statistical summary per variable use [summary()][summary.simulate_stockflow()].
#'
#' @param x A simulation result of class [`simulate_stockflow`][simulate.stockflow()]
#' @param ... Additional arguments (unused)
#'
#' @returns Invisibly returns `x`
#' @export
#' @concept build
#' @method print simulate_stockflow
#' @seealso [simulate.stockflow()], [summary.simulate_stockflow()],
#'   [plot.simulate_stockflow()], [as.data.frame.simulate_stockflow()]
#'
#' @examples
#' sfm <- stockflow("sir")
#' sim <- simulate(sfm)
#' print(sim)
#'
print.simulate_stockflow <- function(x, ...) {
  check_simulate_stockflow(x)

  model_name <- x[["object"]][["meta"]][["name"]]
  default_name <- formals(meta)[["name"]]
  has_name <- !is.null(model_name) && nzchar(model_name) && model_name != default_name

  if (has_name) {
    cli::cli_h1("Stock-and-Flow Simulation: {model_name}")
  } else {
    cli::cli_h1("Stock-and-Flow Simulation")
  }

  if (!x[["success"]]) {
    cli::cli_inform(c("x" = "Simulation failed"))
    cli::cli_inform(c("i" = "Inspect the error message with: {.code x$error_message}"))
    return(invisible(x))
  }

  df <- x[["df"]]
  if (!is.null(df) && nrow(df) > 0) {
    cli_h2_tight("Data (first rows)")
    print(head(as.data.frame(x, direction = "wide"), 5))

    # Print blank line
    cli::cli_text("")
  }

  cli::cli_inform(c(
    "i" = "Access with {.fn as.data.frame} \u2022 Visualise with {.fn plot}"
  ))

  invisible(x)
}


# Internal helper: count model variables by type
model_properties <- function(object) {
  vars <- object[["variables"]]

  count_type <- function(t) sum(vars[["type"]] == t)

  list(
    n_stocks = count_type("stock"),
    n_flows = count_type("flow"),
    n_aux = count_type("aux"),
    n_constants = count_type("constant"),
    n_lookups = count_type("lookup")
  )
}


#' Compare two stock-and-flow models
#'
#' Compares the structure, equations, and simulation settings of two
#' `stockflow` models.
#'
#' @param sfm1 A stock-and-flow model of class [`stockflow`][stockflow()].
#' @param sfm2 A stock-and-flow model of class [`stockflow`][stockflow()].
#'
#' @returns An object of class `compare_stockflow` (a list) containing:
#'   \describe{
#'     \item{`labels`}{Names of the two model objects (captured expressions).}
#'     \item{`added`}{Variables present in `sfm2` but not `sfm1`.}
#'     \item{`removed`}{Variables present in `sfm1` but not `sfm2`.}
#'     \item{`type_changed`}{Variables with different types.}
#'     \item{`eqn_changed`}{Variables with different equations.}
#'     \item{`sim_settings_diff`}{Simulation settings that differ.}
#'     \item{`properties`}{Per-model counts of variables by type.}
#'   }
#' @seealso [`simulate()`][simulate.stockflow()], [`summary()`][summary.stockflow()]
#' @concept build
#' @export
#' @examples
#' sfm1 <- stockflow("sir")
#' sfm2 <- stock(sfm1, "susceptible", eqn = 0.5)
#' compare_models(sfm1, sfm2)
#'
compare_models <- function(sfm1, sfm2) {
  label1 <- rlang::expr_deparse(rlang::enexpr(sfm1))
  label2 <- rlang::expr_deparse(rlang::enexpr(sfm2))

  check_stockflow(sfm1)
  check_stockflow(sfm2)

  v1 <- sfm1[["variables"]]
  v2 <- sfm2[["variables"]]

  names1 <- v1[["name"]]
  names2 <- v2[["name"]]

  only_in_2 <- setdiff(names2, names1)
  only_in_1 <- setdiff(names1, names2)
  in_both <- intersect(names1, names2)

  # Variables added / removed
  make_var_df <- function(vars, nms) {
    rows <- vars[vars[["name"]] %in% nms, c("name", "type", "eqn"), drop = FALSE]
    rownames(rows) <- NULL
    rows
  }
  added <- make_var_df(v2, only_in_2)
  removed <- make_var_df(v1, only_in_1)

  # Changes among shared variables
  v1s <- v1[v1[["name"]] %in% in_both, , drop = FALSE]
  v2s <- v2[v2[["name"]] %in% in_both, , drop = FALSE]
  # Align row order
  v1s <- v1s[order(match(v1s[["name"]], in_both)), , drop = FALSE]
  v2s <- v2s[order(match(v2s[["name"]], in_both)), , drop = FALSE]

  type_changed <- data.frame(
    name = in_both[v1s[["type"]] != v2s[["type"]]],
    type_1 = v1s[["type"]][v1s[["type"]] != v2s[["type"]]],
    type_2 = v2s[["type"]][v1s[["type"]] != v2s[["type"]]],
    stringsAsFactors = FALSE
  )

  eqn1 <- ifelse(is.na(v1s[["eqn"]]), "", v1s[["eqn"]])
  eqn2 <- ifelse(is.na(v2s[["eqn"]]), "", v2s[["eqn"]])
  eqn_diff_idx <- eqn1 != eqn2
  eqn_changed <- data.frame(
    name = in_both[eqn_diff_idx],
    eqn_1 = eqn1[eqn_diff_idx],
    eqn_2 = eqn2[eqn_diff_idx],
    stringsAsFactors = FALSE
  )


  # Sim specs diff
  spec_fields <- c(
    "start", "stop", "dt", "save_by", "save_times", "save_length",
    "time_units", "method", "seed", "language", "only_stocks"
  )
  s1 <- sfm1[["sim_settings"]]
  s2 <- sfm2[["sim_settings"]]
  sim_settings_diff <- Filter(
    Negate(is.null),
    stats::setNames(
      lapply(spec_fields, function(f) {
        val1 <- s1[[f]]
        val2 <- s2[[f]]
        # Treat NULL and NA as equal to themselves
        both_null <- is.null(val1) && is.null(val2)
        if (both_null) {
          return(NULL)
        }
        vals_equal <- isTRUE(all.equal(val1, val2))
        if (vals_equal) {
          return(NULL)
        }
        list(sfm1 = val1, sfm2 = val2)
      }),
      spec_fields
    )
  )

  result <- list(
    labels = c(label1, label2),
    added = added,
    removed = removed,
    type_changed = type_changed,
    eqn_changed = eqn_changed,
    sim_settings_diff = sim_settings_diff,
    properties = list(
      sfm1 = model_properties(sfm1),
      sfm2 = model_properties(sfm2)
    )
  )

  class(result) <- "compare_stockflow"
  result
}


#' Print comparison of two stock-and-flow models
#'
#' @param x An object of class [`compare_stockflow`][compare_models()]
#' @param ... Additional arguments (unused)
#'
#' @returns Invisibly returns `x`.
#' @export
#' @concept build
#' @method print compare_stockflow
#'
print.compare_stockflow <- function(x, ...) {
  l1 <- x[["labels"]][1]
  l2 <- x[["labels"]][2]

  cli::cli_h1("Stock-and-Flow Comparison: {l1} vs {l2}")

  # ── Structural Differences ──────────────────────────────────────
  cli_h2_tight("Structural Differences")

  n_added <- nrow(x[["added"]])
  n_removed <- nrow(x[["removed"]])
  n_type <- nrow(x[["type_changed"]])
  n_eqn <- nrow(x[["eqn_changed"]])
  total_struct <- n_added + n_removed + n_type + n_eqn

  if (total_struct == 0L) {
    cli::cli_alert_success("No structural differences")
  } else {
    if (n_added > 0) {
      entries <- paste0(x[["added"]][["name"]], " [", x[["added"]][["type"]], "]")
      cli::cli_alert_success("Added ({n_added}): {.code {entries}}")
    }
    if (n_removed > 0) {
      entries <- paste0(x[["removed"]][["name"]], " [", x[["removed"]][["type"]], "]")
      cli::cli_alert_danger(
        "Removed ({n_removed}): {.code {entries}}"
      )
    }
    if (n_type > 0) {
      tc <- x[["type_changed"]]
      entries <- paste0(tc[["name"]], ": ", tc[["type_1"]], " \u2192 ", tc[["type_2"]])
      cli::cli_alert_warning("Type changed ({n_type}): {.code {entries}}")
    }
    if (n_eqn > 0) {
      ec <- x[["eqn_changed"]]
      for (i in seq_len(nrow(ec))) {
        cli::cli_alert_warning(
          "Equation changed: {.code {ec$name[i]}}: {.code {ec$eqn_1[i]}} \u2192 {.code {ec$eqn_2[i]}}"
        )
      }
    }
  }

  # ── Simulation Settings ──────────────────────────────────────
  cli_h2_tight("Simulation Settings")
  if (length(x[["sim_settings_diff"]]) == 0L) {
    cli::cli_alert_success("Identical")
  } else {
    for (f in names(x[["sim_settings_diff"]])) {
      v1 <- x[["sim_settings_diff"]][[f]][["sfm1"]]
      v2 <- x[["sim_settings_diff"]][[f]][["sfm2"]]
      cli::cli_alert_warning("{f}: {.val {v1}} \u2192 {.val {v2}}")
    }
  }

  # ── Model Properties ──────────────────────────────────────────
  cli_h2_tight("Model Properties")

  p1 <- x[["properties"]][["sfm1"]]
  p2 <- x[["properties"]][["sfm2"]]

  fmt <- function(n) formatC(n, width = 8)
  row <- function(label, val1, val2) {
    cli::cli_text(
      paste0("  ", formatC(label, width = 24, flag = "-"), fmt(val1), fmt(val2))
    )
  }

  row("Stocks", p1[["n_stocks"]], p2[["n_stocks"]])
  row("Flows", p1[["n_flows"]], p2[["n_flows"]])
  row("Auxiliaries", p1[["n_aux"]], p2[["n_aux"]])
  row("Constants", p1[["n_constants"]], p2[["n_constants"]])
  row("Lookups", p1[["n_lookups"]], p2[["n_lookups"]])

  invisible(x)
}


#' Compare two simulations
#'
#' @param sim1 Simulation 1
#' @param sim2 Simulation 2
#' @param tolerance Numeric; tolerance for comparing values. Defaults to 0.00001.
#'
#' @returns List with comparison results
#' @noRd
#'
compare_sim <- function(sim1, sim2, tolerance = .00001) {
  if (sim1[["success"]] && !sim2[["success"]]) {
    return(c(
      equal = FALSE,
      msg = "Simulation 1 was successful, but simulation 2 failed."
    ))
  }

  if (!sim1[["success"]] && sim2[["success"]]) {
    return(c(
      equal = FALSE,
      msg = "Simulation 2 was successful, but simulation 1 failed."
    ))
  }

  get_prop <- function(sim) {
    list(
      colnames = colnames(sim[[P[["sim_df_name"]]]]),
      var_names = unique(sim[[P[["sim_df_name"]]]][["variable"]]),
      nrow = nrow(sim[[P[["sim_df_name"]]]]),
      ncol = ncol(sim[[P[["sim_df_name"]]]]),
      n_pars = length(sim[[P[["parameter_name"]]]]),
      language = sim[["object"]][["sim_settings"]][["language"]],
      method = sim[["object"]][["sim_settings"]][["method"]]
    )
  }

  prop1 <- get_prop(sim1)
  prop2 <- get_prop(sim2)

  overlapping_var_names <- intersect(prop1[["var_names"]], prop2[["var_names"]])
  nonoverlapping_var_names <- setdiff(
    union(
      prop1[["var_names"]],
      prop2[["var_names"]]
    ),
    overlapping_var_names
  )

  check_diff <- function(col1, col2) {
    col1 <- as.numeric(col1)
    col2 <- as.numeric(col2)

    if (length(col1) != length(col2)) {
      return(c(
        equal = FALSE,
        msg = paste0(
          "Column lengths are not equal: ",
          length(col1), " (sim1) vs ", length(col2), " (sim2)"
        )
      ))
    }

    # Calculate Euclidean distance, ignoring NAs
    return(c(
      equal = all(abs(col1 - col2) < tolerance, na.rm = TRUE),
      first_diff = which(abs(col1 - col2) > tolerance)[1],
      nr_diff = sum(abs(col1 - col2) > tolerance, na.rm = TRUE),
      max_diff = max(abs(col1 - col2), na.rm = TRUE),
      sqrt_sum_diff = sqrt(sum((col1 - col2)^2, na.rm = TRUE))
    ))
  }

  df <- bind_rows_(
    lapply(
      overlapping_var_names,
      function(name) {
        c(
          name = name,
          check_diff(
            sim1[["df"]][sim1[["df"]][["variable"]] == name, "value"],
            sim2[["df"]][sim2[["df"]][["variable"]] == name, "value"]
          )
        )
      }
    )
  )

  list(
    equal = all(as.logical(as.numeric(df[["equal"]]))),
    overlapping_var_names = overlapping_var_names,
    nonoverlapping_var_names = nonoverlapping_var_names,
    msg = paste0(
      "The following columns are not equal:\n",
      paste0(df[["name"]], ": ", df[["first_diff"]], " (",
        df[["nr_diff"]], " differences, max diff: ", df[["max_diff"]],
        ")\n",
        collapse = ""
      ),
      "\n"
    ),
    prop1 = prop1,
    prop2 = prop2,
    df = df
  )
}


#' Create data frame of simulation results
#'
#' Convert simulation results to a data.frame.
#'
#' @inheritParams plot.simulate_stockflow
#' @param direction Format of data frame, either "long" (default) or "wide".
#' @param vars Variable names to retain in the data frame. Defaults to `NULL` to include all variables.
#' @param type Variable types to retain in the data frame. Must be one or more of 'stock', 'flow', 'constant', 'aux', 'lookup', or 'func'. Defaults to `NULL` to include all types.
#' @param row.names NULL or a character vector giving the row names for the data frame. Missing values are not allowed.
#' @param optional Ignored parameter.
#'
#' @returns A data.frame with simulation results. For \code{direction = "long"} (default),
#'   the data frame has three columns: \code{time}, \code{variable}, and \code{value}.
#'   For \code{direction = "wide"}, the data frame has columns \code{time} followed by
#'   one column per variable.
#' @export
#' @seealso [`simulate()`][simulate.stockflow()], [stockflow()]
#' @concept build
#' @method as.data.frame simulate_stockflow
#'
#' @examples
#' sfm <- stockflow("sir")
#' sim <- simulate(sfm)
#' df <- as.data.frame(sim)
#' head(df)
#'
#' # Get results in wide format
#' df_wide <- as.data.frame(sim, direction = "wide")
#' head(df_wide)
#'
as.data.frame.simulate_stockflow <- function(x,
                                             row.names = NULL, optional = FALSE,
                                             direction = "long",
                                             vars = NULL, type = NULL, ...) {
  vars <- .expr_to_char(rlang::enexpr(vars))
  check_simulate_stockflow(x)

  direction <- trimws(tolower(direction))
  if (!direction %in% c("long", "wide")) {
    cli::cli_abort(c(
      "Invalid {.arg direction} argument.",
      "x" = "Must be either {.code 'long'} or {.code 'wide'}."
    ))
  }

  # Filter long-format data by variable and/or type
  df <- .filter_long_by_vars_type(x[["df"]], x[["object"]], vars = vars, type = type)

  if (direction == "wide") {
    df <- stats::reshape(df,
      timevar = "variable",
      idvar = "time",
      direction = "wide"
    )

    # Remove value. prefix
    names(df) <- sub("^value\\.", "", names(df))

    # Remove row names
    rownames(df) <- NULL
  }

  # Handle row.names if provided
  if (!is.null(row.names)) {
    if (length(row.names) != nrow(df)) {
      cli::cli_abort(c(
        "Length mismatch in {.arg row.names}.",
        "x" = "Got {length(row.names)} name{?s} but {nrow(df)} row{?s}."
      ))
    }
    rownames(df) <- row.names
  }

  df
}


#' Print first rows of a simulation
#'
#' Print the first rows of a simulation data frame of a stock-and-flow model. This is a wrapper around [head()] that first converts the simulation results to a data frame using [as.data.frame()][as.data.frame.simulate_stockflow()].
#'
#' @inheritParams as.data.frame.simulate_stockflow
#' @param n Number of rows to print. Defaults to 6.
#' @param ... Other arguments passed to [as.data.frame.simulate_stockflow()].
#'
#' @returns A data.frame with the first rows of the simulation results.
#' @export
#' @concept simulate
#' @importFrom utils head
#' @examples
#' sfm <- stockflow("sir")
#' sim <- simulate(sfm)
#' head(sim)
head.simulate_stockflow <- function(x, n = 6L, ...) {
  check_simulate_stockflow(x)

  df <- as.data.frame(x, ...)
  head(df, n)
}


#' Print last rows of a simulation
#'
#' Print the last rows of a simulation data frame of a stock-and-flow model. This is a wrapper around [tail()] that first converts the simulation results to a data frame using [as.data.frame()][as.data.frame.simulate_stockflow()].
#'
#' @inheritParams as.data.frame.simulate_stockflow
#' @param n Number of rows to print. Defaults to 6.
#' @param ... Other arguments passed to [as.data.frame.simulate_stockflow()].
#' @return A data.frame with the last rows of the simulation results.
#' @export
#' @concept simulate
#' @importFrom utils tail
#' @examples
#' sfm <- stockflow("sir")
#' sim <- simulate(sfm)
#' tail(sim)
tail.simulate_stockflow <- function(x, n = 6L, ...) {
  check_simulate_stockflow(x)

  df <- as.data.frame(x, ...)
  tail(df, n)
}


#' Summarize simulation results
#'
#' Returns a data frame with per-variable summary statistics (min, mean, max,
#' and final value) over the simulated time range.
#'
#' @param object A simulation result of class [`simulate_stockflow`][simulate.stockflow()]
#' @param ... Additional arguments (unused)
#'
#' @returns A `data.frame` with columns `variable`, `min`, `mean`, `max`, `final`.
#' @export
#' @concept simulate
#' @method summary simulate_stockflow
#' @seealso [print.simulate_stockflow()], [simulate.stockflow()]
#'
#' @examples
#' sfm <- stockflow("sir")
#' sim <- simulate(sfm)
#' summary(sim)
#'
summary.simulate_stockflow <- function(object, ...) {
  check_simulate_stockflow(object)

  if (!object[["success"]]) {
    cli::cli_abort(c(
      "Cannot summarize a failed simulation.",
      "i" = "Inspect the error message with: {.code x$error_message}"
    ))
  }

  df <- object[["df"]]
  vars <- unique(df[["variable"]])

  result <- do.call(rbind, lapply(vars, function(v) {
    vals <- df[df[["variable"]] == v, "value"]
    data.frame(
      variable = v,
      min = min(vals),
      mean = mean(vals),
      max = max(vals),
      final = vals[length(vals)],
      stringsAsFactors = FALSE
    )
  }))

  rownames(result) <- NULL
  result
}
