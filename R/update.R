# ==============================================================================
# NSE (Non-Standard Evaluation) HELPERS
# ==============================================================================
# These helpers convert captured expressions (from enexpr()) to character strings.

#' Convert a captured expression to a character vector
#'
#' Converts the output of [rlang::enexpr()] to a character string or vector.
#' Handles NULL, character, numeric, logical, symbols, `c()` calls, and
#' arbitrary expressions. For symbols, returns the symbol name. For `c()` calls,
#' recursively processes each element. For other calls, deparses to string.
#'
#' Use `!!` (bang-bang injection) to pass the value of a variable instead of
#' its name: `update(sfm, !!my_var)`.
#'
#' @param expr Expression captured by [rlang::enexpr()].
#'   Can be NULL, a character string, a number, a symbol, or a call.
#' @return Character vector, or NULL if `expr` is NULL.
#' @noRd
.expr_to_char <- function(expr) {
  if (is.null(expr)) {
    return(NULL)
  }
  if (is.character(expr)) {
    return(expr)
  }
  if (is.numeric(expr)) {
    return(as.character(expr))
  }
  if (is.logical(expr)) {
    return(as.character(expr))
  }
  if (rlang::is_symbol(expr)) {
    return(rlang::as_name(expr))
  }

  # If a function object (closure) was injected via `!!`, deparse the
  # function object to a single string representation.
  if (is.function(expr) || typeof(expr) == "closure") {
    return(paste(deparse(expr), collapse = "\n"))
  }

  if (rlang::is_call(expr)) {
    fn <- rlang::call_name(expr)

    # c() calls: recursively process each element
    if (!is.null(fn) && fn == "c") {
      args <- rlang::call_args(expr)
      return(unlist(lapply(args, .expr_to_char), use.names = FALSE))
    }

    # General expression: deparse to string. Collapse multi-line deparse
    # results into a single element so callers that expect length 1 (e.g.
    # function definitions) do not trigger length-mismatch errors.
    return(paste(rlang::expr_deparse(expr, width = 500L), collapse = "\n"))
  }

  # Fallback
  as.character(expr)
}


# ==============================================================================
# GUARD: detect accidental double-passing of stockflow object as variable name
# ==============================================================================

#' Abort with a helpful message when a stockflow object lands in the name slot
#' @noRd
.abort_name_is_stockflow <- function() {
  cli::cli_abort(c(
    "An {.cls stockflow} model object was passed where a variable name was expected.",
    "i" = "Did you accidentally pass the model twice?",
    ">" = "Use {.code sfm |> constant(A)} not {.code sfm |> constant(sfm, A)}."
  ))
}

#' Check that the name argument is not accidentally a stockflow model object.
#'
#' For NSE functions, pass the unevaluated expression (from [rlang::enexpr()])
#' and the caller's environment so the symbol can be looked up without forcing
#' the promise. For non-NSE functions, pass the already-evaluated value.
#'
#' @param name_or_expr Either an rlang expression (symbol) from [rlang::enexpr()]
#'   or an already-evaluated value.
#' @param env The environment in which to look up bare symbols. Defaults to
#'   [rlang::caller_env()].
#' @noRd
.check_name_not_stockflow <- function(name_or_expr,
                                      env = rlang::caller_env()) {
  is_stockflow_val <- function(x) inherits(x, "stockflow")

  if (rlang::is_symbol(name_or_expr)) {
    # NSE path: expression not yet forced — look up the symbol in caller's env
    nm <- rlang::as_name(name_or_expr)
    candidate <- tryCatch(
      get(nm, envir = env, inherits = TRUE),
      error = function(e) NULL
    )
    if (is_stockflow_val(candidate)) {
      .abort_name_is_stockflow()
    }
  } else if (is_stockflow_val(name_or_expr)) {
    # Non-NSE path: value was already evaluated and is a stockflow object
    .abort_name_is_stockflow()
  }
}


# ==============================================================================
# ==============================================================================
# These validators check input validity and return structured error messages.

#' Validate name argument
#' @param name Character vector of variable names
#' @param arg_name Name of argument for error messages (default: "name")
#' @return Validated, trimmed name vector (invisibly valid, or error raised)
#' @noRd
.validate_name_arg <- function(name, arg_name = "name") {
  if (!all(is.character(name))) {
    cli::cli_abort(c(
      "Invalid {.arg {arg_name}} argument.",
      "x" = "The {.arg {arg_name}} argument must be {.cls character}.",
      "i" = "Received: {.cls {typeof(name)}}."
    ))
  }

  if (any(is.na(name))) {
    cli::cli_abort(c(
      "Invalid {.arg {arg_name}} argument.",
      "x" = "Variable names cannot be NA."
    ))
  }

  name <- trimws(name)
  if (!all(nzchar(name))) {
    cli::cli_abort(c(
      "Invalid {.arg {arg_name}} argument.",
      "x" = "Variable names cannot be empty strings."
    ))
  }

  name
}

#' Validate type argument
#' @param type Character vector of types
#' @param arg_name Name of argument for error messages (default: "type")
#' @return Validated, cleaned type vector (invisibly valid, or error raised)
#' @noRd
.validate_type_arg <- function(type, arg_name = "type") {
  if (is.null(type)) {
    return(type)
  }

  type <- clean_type(type)
  allowed_types <- .stockflow_types()

  if (!all(type %in% allowed_types)) {
    types_display <- paste0("'", allowed_types, "'", collapse = ", ")
    x_msg <- paste0("The {.arg {arg_name}} must be one of: ", types_display, ".")
    cli::cli_abort(c(
      "Invalid {.arg {arg_name}} argument.",
      "x" = x_msg
    ))
  }

  type
}


#' Validate func-type equation definitions
#'
#' For function definitions, default arguments must be contiguous at the end.
#' @param eqn Character vector of equations
#' @param name Character vector of corresponding variable names
#' @noRd
.validate_func_eqn <- function(eqn, name) {
  for (j in seq_along(eqn)) {
    if (grepl("^\\s*function\\s*\\(", eqn[j])) {
      func_match <- regmatches(eqn[j], regexpr("function\\s*\\(([^)]*)", eqn[j]))
      if (length(func_match) > 0) {
        arg_str <- sub("^function\\s*\\(", "", func_match)
        if (nzchar(arg_str)) {
          func_args <- parse_args(arg_str)
          has_default <- grepl("=", func_args)
          if (any(has_default)) {
            default_positions <- which(has_default)
            if (max(default_positions) != length(func_args) ||
              any(diff(default_positions) > 1)) {
              cli::cli_abort(
                c(
                  "x" = "All arguments with defaults have to be placed at the end of the function arguments.",
                  ">" = "Change the function definition of {.val {name[j]}}."
                ),
                call. = FALSE
              )
            }
          }
        }
      }
    }
  }
  invisible()
}


.validate_eqn_arg <- function(eqn) {
  if (is.null(eqn)) {
    cli::cli_warn(c(
      "Empty {.arg eqn} argument.",
      ">" = "Setting {.code eqn = '0'}."
    ))
    eqn <- "0"
  }
  if (any(is.na(eqn))) {
    cli::cli_warn(c(
      "NA values in {.arg eqn} argument.",
      ">" = "Setting {.code eqn = '0'}."
    ))
    eqn[is.na(eqn)] <- "0"
  }
  if (any(!nzchar(eqn))) {
    cli::cli_warn(c(
      "Empty {.arg eqn} argument.",
      ">" = "Setting {.code eqn = '0'}."
    ))
    eqn[!nzchar(eqn)] <- "0"
  }

  as.character(eqn)
}


#' Validate equation syntax
#' @param eqn Character vector of equations
#' @param name Character vector of corresponding variable names
#' @noRd
.validate_eqn_syntax <- function(eqn, name) {
  for (j in seq_along(eqn)) {
    if (is.na(eqn[j]) || !nzchar(eqn[j])) {
      next
    }

    tryCatch(
      {
        parse(text = eqn[j])
      },
      error = function(e) {
        cli::cli_abort(c(
          "Invalid {.arg eqn} syntax.",
          "x" = "Could not parse the equation for {.val {name[j]}}.",
          "i" = e$message
        ), call. = FALSE)
      }
    )
  }

  invisible()
}


#' Validate label argument
#' @param label Character vector of labels
#' @noRd
.validate_label_arg <- function(label) {
  label <- trimws(label)
  if (!all(nzchar(label))) {
    cli::cli_abort(c(
      "Invalid {.arg label} argument.",
      "x" = "Labels cannot be empty strings."
    ))
  }
  label
}


.validate_flow_connector <- function(arg, arg_name) {
  if (!is.null(arg)) {
    arg[is.na(arg)] <- ""
    if (!inherits(arg, "character")) {
      cli::cli_abort(c(
        "Invalid {.arg {arg_name}} argument.",
        "x" = "The {.arg {arg_name}} argument must be {.cls character}.",
        "i" = "Received: {.cls {typeof(arg)}}."
      ))
    }
  } else {
    arg <- ""
  }
  arg
}


#' Validate flow connections (to/from)
#' @param to Target stock name
#' @param from Source stock name
#' @param name Flow variable name
#' @noRd
.validate_flow_connections <- function(to, from, name) {
  if (!is.null(to)) {
    if (length(name) == 1 && length(to) > 1) {
      cli::cli_abort(c(
        "Too many {.arg to} targets.",
        "x" = "A single flow can only target one stock."
      ))
    }
    if (any(to == name)) {
      cli::cli_abort(c(
        "Invalid {.arg to} target.",
        "x" = "A flow cannot flow to itself."
      ))
    }
  }

  if (!is.null(from)) {
    if (length(name) == 1 && length(from) > 1) {
      cli::cli_abort(c(
        "Too many {.arg from} sources.",
        "x" = "A flow can only originate from one stock."
      ))
    }
    if (any(from == name)) {
      cli::cli_abort(c(
        "Invalid {.arg from} source.",
        "x" = "A flow cannot flow from itself."
      ))
    }
  }

  if (!is.null(to) && !is.null(from)) {
    if (any(to %in% from)) {
      cli::cli_abort(c(
        "Invalid flow connections.",
        "x" = "A flow cannot have the same stock as both source and target."
      ))
    }
  }

  invisible(list(to = to, from = from))
}


.validate_lookup_points <- function(pts, arg_name) {
  if (is.character(pts)) {
    pts <- trimws(pts)
    pts <- gsub("^c\\(", "", pts)
    pts <- gsub("\\)$", "", pts)
    pts <- as.numeric(trimws(strsplit(pts, ",")[[1]]))
  }

  if (!is.numeric(pts)) {
    cli::cli_abort(c(
      "Invalid {.arg {arg_name}} argument.",
      "x" = "The {.arg {arg_name}} must be a numeric vector or a character string in the format 'c(...)'."
    ))
  }

  pts
}


# Validate xpts/ypts argument, handling single vs vectorized lookups.
# Returns a list of numeric vectors (one per name element).
.validate_pts_arg <- function(pts, arg_name, name) {
  if (length(name) == 1) {
    # Single lookup: accept vector or list (unlist if list)
    p <- if (is.list(pts)) unlist(pts) else pts
    list(.validate_lookup_points(p, arg_name))
  } else {
    # Multiple lookups: require list of length(name)
    if (!is.list(pts)) {
      cli::cli_abort(c(
        "Invalid {.arg {arg_name}} for vectorized lookup.",
        "x" = "When {.arg name} has multiple elements, {.arg {arg_name}} must be a list.",
        "i" = "Each element should be a numeric vector for the corresponding lookup."
      ))
    }
    if (length(pts) != length(name)) {
      cli::cli_abort(c(
        "Length mismatch.",
        "x" = paste0(
          "{.arg {arg_name}} has {length(pts)} element{?s} ",
          "but {.arg name} has {length(name)}."
        )
      ))
    }
    lapply(pts, .validate_lookup_points, arg_name = arg_name)
  }
}


.validate_interpolation_arg <- function(interpolation) {
  interpolation <- tolower(interpolation)
  if (!all(interpolation %in% c("linear", "constant"))) {
    cli::cli_abort(c(
      "Invalid {.arg interpolation} value.",
      "x" = "The {.arg interpolation} must be {.code 'linear'} or {.code 'constant'}.",
      "i" = "Received: {.code {interpolation}}."
    ))
  }
  interpolation
}


.validate_extrapolation_arg <- function(extrapolation) {
  extrapolation <- tolower(extrapolation)
  if (!all(extrapolation %in% c("nearest", "na"))) {
    cli::cli_abort(c(
      "Invalid {.arg extrapolation} value.",
      "x" = "The {.arg extrapolation} must be {.code 'nearest'} or {.code 'NA'}.",
      "i" = "Received: {.code {extrapolation}}."
    ))
  }
  extrapolation
}


.validate_source_arg <- function(source) {
  if (!is.null(source)) {
    source[is.na(source)] <- ""
    if (!inherits(source, "character")) {
      cli::cli_abort(c(
        "Invalid {.arg source} argument.",
        "x" = "The {.arg source} argument must be {.cls character}.",
        "i" = "Received: {.cls {typeof(source)}}."
      ))
    }
  }
  source
}

#' Validate graphical function properties
#' @param type Variable type
#' @param xpts X points vector
#' @param ypts Y points vector
#' @param source Source variable name
#' @param interpolation Interpolation method
#' @param extrapolation Extrapolation method
#' @noRd
.validate_graphical_function <- function(type, xpts, ypts, source,
                                         interpolation, extrapolation) {
  if (type != "lookup") {
    return(invisible(list()))
  }

  # Validate xpts/ypts
  if (!is.null(xpts) && !is.null(ypts)) {
    if (any(lengths(xpts) != lengths(ypts))) {
      cli::cli_abort(c(
        "Length mismatch between {.arg xpts} and {.arg ypts}.",
        "x" = "Length of {.arg xpts} is {.val {lengths(xpts)}}, but length of {.arg ypts} is {.val {lengths(ypts)}}.",
        ">" = "Ensure both arguments have the same length."
      ))
    }
  }

  # Validate interpolation
  if (length(interpolation) > 1) {
    cli::cli_abort(c(
      "Invalid {.arg interpolation} argument.",
      "x" = "Must be a single value: {.code 'linear'} or {.code 'constant'}."
    ))
  }
  interpolation <- .validate_interpolation_arg(interpolation)

  # Validate extrapolation
  if (length(extrapolation) > 1) {
    cli::cli_abort(c(
      "Invalid {.arg extrapolation} argument.",
      "x" = "Must be a single value: {.code 'nearest'} or {.code 'NA'}."
    ))
  }
  extrapolation <- .validate_extrapolation_arg(extrapolation)

  # Validate source
  source <- .validate_source_arg(source)
  if (length(source) > 1) {
    cli::cli_abort(c(
      "Invalid {.arg source} argument.",
      "x" = "Only one source variable can be specified."
    ))
  }

  invisible(list(
    xpts = xpts, ypts = ypts, source = source,
    interpolation = interpolation,
    extrapolation = extrapolation
  ))
}


.validate_non_negative_arg <- function(non_negative) {
  if (!is.null(non_negative)) {
    if (!all(is.logical(non_negative))) {
      cli::cli_abort(c(
        "Invalid {.arg non_negative} argument.",
        "x" = "The {.arg non_negative} must be {.cls logical}."
      ))
    }
  }
  non_negative
}

.validate_doc_arg <- function(doc) {
  if (!is.null(doc)) {
    if (!inherits(doc, "character")) {
      cli::cli_abort(c(
        "Invalid {.arg doc} argument.",
        "x" = "The {.arg doc} must be {.cls character}."
      ))
    }
  }
  doc
}


# ==============================================================================
# OPERATION-SPECIFIC INTERNAL FUNCTIONS - Modular update operations
# ==============================================================================
# These functions encapsulate specific update operations for clarity and testing.

#' Rename a variable throughout the model
#' @param object Stock-and-flow model
#' @param old_name Current variable name
#' @param new_name New variable name
#' @noRd
.change_name <- function(object, old_name, new_name) {
  var_names <- object[["variables"]][["name"]]
  # Validate that all requested old names exist to avoid NA subscripts
  check_var_existence(old_name, var_names)

  # Update name in data frame
  idx_var <- match(old_name, object[["variables"]][["name"]])
  object[["variables"]][idx_var, "name"] <- new_name

  # Update label if it was same as old name
  idx_label <- object[["variables"]][idx_var, "label"] == old_name
  if (any(idx_label)) {
    object[["variables"]][idx_var[idx_label], "label"] <- new_name[idx_label]
  }

  # Replace references using word boundaries
  for (i in seq_along(old_name)) {
    object[["variables"]][["eqn"]] <- gsub(
      paste0("\\b", old_name[i], "\\b"), new_name[i],
      object[["variables"]][["eqn"]]
    )
  }

  # Update to/from/source references
  for (col in c("to", "from", "source")) {
    idx <- match(object[["variables"]][[col]], old_name)
    if (any(!is.na(idx))) {
      object[["variables"]][which(!is.na(idx)), col] <- new_name[stats::na.omit(idx)]
    }
  }

  # Update unit test references
  if (length(object[["unit_tests"]]) > 0L) {
    for (i in seq_along(object[["unit_tests"]])) {
      test <- object[["unit_tests"]][[i]]

      # Update expr_str: replace old variable references with new names
      new_expr_str <- test[["expr_str"]]
      for (j in seq_along(old_name)) {
        new_expr_str <- gsub(
          paste0("\\b", old_name[j], "\\b"), new_name[j], new_expr_str
        )
      }
      if (new_expr_str != test[["expr_str"]]) {
        object[["unit_tests"]][[i]][["expr_str"]] <- new_expr_str

        # Re-generate label if the current label was auto-generated from the expression
        new_label <- tryCatch(
          interpret(parse(text = new_expr_str, keep.source = FALSE)[[1]]),
          error = function(e) NULL
        )
        old_label <- tryCatch(
          interpret(parse(text = test[["expr_str"]], keep.source = FALSE)[[1]]),
          error = function(e) NULL
        )
        if (!is.null(new_label) && !is.null(old_label) && test[["label"]] == old_label) {
          object[["unit_tests"]][[i]][["label"]] <- new_label
        }
      }

      # Update condition keys: rename old_name -> new_name in conditions
      cond_names <- names(test[["conditions"]])
      if (length(cond_names) > 0L) {
        hit <- match(cond_names, old_name)
        changed <- !is.na(hit)
        if (any(changed)) {
          cond_names[changed] <- new_name[hit[changed]]
          names(object[["unit_tests"]][[i]][["conditions"]]) <- cond_names
        }
      }
    }

    # Invalidate cached test deps (references changed)
    object <- invalidate_assemble(object, "unit_tests")
  }

  object
}

#' Update variable properties in data frame
#'
#' @param i Index in name vector (for vectorized update)
#' @param passed_arg Character vector of properties that were passed
#' @return Updated object with properties updated for variable at index i
#'
#' @inheritParams update.stockflow
#' @noRd
update_variable_row <- function(object, type, name,
                                eqn, label, doc,
                                non_negative, to, from,
                                xpts, ypts, source,
                                interpolation, extrapolation) {
  # Identify which arguments were passed
  passed_arg <- names(match.call())[-1] # Exclude function call

  # Find variable index
  idx_var <- which(object[["variables"]][["name"]] == name)

  if (length(idx_var) == 0) {
    return(object) # Variable doesn't exist, skip update
  }

  # Update scalar properties
  if ("eqn" %in% passed_arg) {
    object[["variables"]][idx_var, "eqn"] <- eqn
  }
  if ("label" %in% passed_arg) object[["variables"]][idx_var, "label"] <- label
  if ("doc" %in% passed_arg) object[["variables"]][idx_var, "doc"] <- doc
  if ("non_negative" %in% passed_arg) object[["variables"]][idx_var, "non_negative"] <- non_negative

  # Update flow properties
  if ("to" %in% passed_arg && type == "flow") {
    object[["variables"]][idx_var, "to"] <- to
  }
  if ("from" %in% passed_arg && type == "flow") {
    object[["variables"]][idx_var, "from"] <- from
  }

  # Update graphical function properties
  if (type == "lookup") {
    if (!is.null(xpts)) object[["variables"]][idx_var, "xpts"] <- list(xpts)
    if (!is.null(ypts)) object[["variables"]][idx_var, "ypts"] <- list(ypts)
    if (!is.null(source)) object[["variables"]][idx_var, "source"] <- source
    if ("interpolation" %in% passed_arg) object[["variables"]][idx_var, "interpolation"] <- interpolation
    if ("extrapolation" %in% passed_arg) object[["variables"]][idx_var, "extrapolation"] <- extrapolation
  }

  object
}


#' Prepare model variables for assembly/simulation
#'
#' Updates prepared equation strings (eqn_str, sum_eqn, sum_name) based on
#' the current language in sim_settings. Conservatively invalidates the base
#' assembly cache after any model-variable edit.
#'
#' @param object Stock-and-flow model
#' @param modified_names Character vector of variable names that were modified.
#'   If NULL (default), all variables are processed (full regeneration). If provided,
#'   only the specified variables are updated for incremental performance.
#' @param sanitize Logical: whether to run sanitize_stockflow()
#' @noRd
.prepare_model_for_assembly <- function(object, modified_names = NULL, sanitize = TRUE) {
  # Prepare equations for current language (adapter handles R vs Julia)
  object <- prep_equations_variables(object, modified_names = modified_names)
  object <- prep_stock_change(object, modified_names = modified_names)

  object <- invalidate_assemble(object, "variables")

  if (sanitize) {
    object <- sanitize_stockflow(object)
  }

  object
}


#' Add or modify stocks
#'
#' Stocks accumulate material or information over time, defining the state of
#' the system. [stock()] adds or changes a stock variable. This is a
#' convenience wrapper around [update()] with `type = "stock"`. See the
#' **Stocks** section of [update()] for more details.
#'
#' @inheritParams update.stockflow
#' @returns A stock-and-flow model object of class [`stockflow`][stockflow]
#' @seealso [update()], [discard()], [change_name()]
#' @concept build
#' @export
#'
#' @examples
#'
#' # Create a stock with an initial value
#' sfm <- stockflow() |>
#'   stock(population, eqn = 100, label = "Population")
#'
#' # Multiple stocks
#' sfm <- stockflow() |>
#'   stock(susceptible, eqn = 999, label = "susceptible") |>
#'   stock(infected, eqn = 1, label = "infected") |>
#'   stock(recovered, eqn = 0, label = "recovered")
#'
stock <- function(object, name,
                  eqn = 0,
                  label = name,
                  doc = "",
                  non_negative = FALSE
                  # inflow, outflow
) {
  # Capture passed arguments
  cl <- match.call()

  # Change function call to update() with type = "stock"
  cl[[1]] <- quote(update)
  cl$type <- "stock"

  # Evaluate the modified call in the parent frame
  eval.parent(cl)
}


#' Add or modify flows
#'
#' Flows move material and information through the system, increasing or
#' decreasing stocks. [flow()] adds or changes a flow variable. This is a
#' convenience wrapper around [update()] with `type = "flow"`. See the
#' **Flows** section of [update()] for more details.
#'
#' @inheritParams update.stockflow
#' @returns A stock-and-flow model object of class [`stockflow`][stockflow]
#' @seealso [update()], [discard()], [change_name()]
#' @concept build
#' @export
#'
#' @examples
#'
#' # Create a flow into a stock
#' sfm <- stockflow() |>
#'   stock(population, eqn = 100) |>
#'   flow(births, eqn = population * 0.1, to = population) |>
#'   flow(deaths, eqn = population * 0.05, from = population)
#'
flow <- function(object, name,
                 eqn = 0,
                 to = NULL,
                 from = NULL,
                 label = name,
                 doc = "",
                 non_negative = FALSE) {
  # Capture passed arguments
  cl <- match.call()

  # Change function call to update() with type = "flow"
  cl[[1]] <- quote(update)
  cl$type <- "flow"

  # Evaluate the modified call in the parent frame
  eval.parent(cl)
}

#' Add or modify constants
#'
#' Constants are time-independent variables that do not change over the course
#' of a simulation. [constant()] adds or changes a constant variable. This is a
#' convenience wrapper around [update()] with `type = "constant"`. See the
#' **Constants** section of [update()] for more details.
#'
#' @inheritParams update.stockflow
#' @returns A stock-and-flow model object of class [`stockflow`][stockflow]
#' @seealso [update()], [discard()], [change_name()]
#' @concept build
#' @export
#'
#' @examples
#'
#' # Create constants for model parameters
#' sfm <- stockflow() |>
#'   constant(growth_rate, eqn = 0.1, label = "Growth Rate") |>
#'   constant(carrying_capacity, eqn = 1000, label = "Carrying Capacity")
#'
constant <- function(object, name,
                     eqn = 0,
                     label = name,
                     doc = "",
                     non_negative = FALSE) {
  # Capture passed arguments
  cl <- match.call()

  # Change function call to update() with type = "constant"
  cl[[1]] <- quote(update)
  cl$type <- "constant"

  # Evaluate the modified call in the parent frame
  eval.parent(cl)
}

#' Add or modify auxiliaries
#'
#' Auxiliaries are dynamic variables used for intermediate calculations in the
#' system. [auxiliary()] adds or changes an auxiliary variable. This is a convenience
#' wrapper around [update()] with `type = "aux"`. See the **Auxiliaries** section
#' of [`update()`][update.stockflow()] for more details.
#'
#' @inheritParams update.stockflow
#' @returns A stock-and-flow model object of class [`stockflow`][stockflow]
#' @seealso [`update()`][update.stockflow()], [discard()], [change_name()]
#' @concept build
#' @export
#'
#' @examples
#'
#' # Create an auxiliary for an intermediate calculation
#' sfm <- stockflow() |>
#'   stock(population, eqn = 100) |>
#'   constant(carrying_capacity, eqn = 1000) |>
#'   auxiliary(density, eqn = population / carrying_capacity, label = "Density")
#'
auxiliary <- function(object, name,
                      eqn = 0,
                      label = name,
                      doc = "",
                      non_negative = FALSE) {
  # Capture passed arguments
  cl <- match.call()

  # Change function call to update() with type = "aux"
  cl[[1]] <- quote(update)
  cl$type <- "aux"

  # Evaluate the modified call in the parent frame
  eval.parent(cl)
}

#' @rdname auxiliary
#' @concept build
#' @export
aux <- auxiliary


#' Create or modify custom variables or functions
#'
#' Custom functions are user-defined functions that can be used
#' throughout a stock-and-flow model. [custom_func()] adds or changes a function. This is a convenience wrapper around [update()] with
#' `type = "func"`.
#'
#' @inheritParams update.stockflow
#' @param name Name of the function variable. The equation will be assigned to this name.
#' @param eqn Equation of the function variable. A character vector. Defaults to `0`.
#' @param doc Documentation. Defaults to "".
#'
#' @returns A stock-and-flow model object of class [`stockflow`][stockflow]
#' @seealso [`update()`][update.stockflow()], [discard()], [change_name()]
#' @concept build
#' @export
#'
#' @examples
#'
#' # Simple function
#' sfm <- stockflow() |>
#'   custom_func(double, eqn = "function(x) x * 2") |>
#'   constant(a, eqn = double(2))
#'
#' # Function with defaults
#' sfm <- stockflow() |>
#'   custom_func(scale, eqn = "function(x, factor = 10) x * factor") |>
#'   constant(b, eqn = scale(2))
#'
#' # If the logistic() function did not exist, you could create it yourself:
#' sfm <- stockflow() |>
#'   custom_func(my_logistic, eqn = "function(x, slope = 1, midpoint = .5){
#'    1 / (1 + exp(-slope*(x-midpoint)))
#'  }") |>
#'   constant(c_, eqn = my_logistic(2, slope = 50))
#'
custom_func <- function(object, name, eqn = 0,
                        label = name, doc = "") {
  cl <- match.call()
  cl[[1]] <- quote(update)
  cl$type <- "func"
  eval.parent(cl)
}


#' Add or modify lookup variables (graphical functions)
#'
#' Lookup variables define piecewise relationships using specified (x, y) points. [lookup()] adds or changes a lookup variable. This is a convenience wrapper around [update()] with `type = "lookup"`. See the **Lookup Variables** section of [update()] for more details.
#'
#' @inheritParams update.stockflow
#' @return A stock-and-flow model object of class [`stockflow`][stockflow()]
#'
#' @seealso [`update()`][update.stockflow()], [discard()], [change_name()]
#' @concept build
#' @export
#' @examples
#' # Create a lookup variable for a non-linear relationship
#' sfm <- stockflow() |>
#'   lookup(output,
#'     source = t,
#'     xpts = c(0, 5, 10),
#'     ypts = c(0, 10, 15),
#'     interpolation = "linear"
#'   ) |>
#'   stock(x) |>
#'   flow(x_in, eqn = output(t), to = x)
#'
#' sim <- simulate(sfm)
#' plot(sim)
#'
lookup <- function(object, name,
                   xpts, ypts, source = NULL,
                   interpolation = "linear",
                   extrapolation = "nearest",
                   label = name,
                   doc = "",
                   non_negative = FALSE) {
  # Capture passed arguments
  cl <- match.call()

  # Change function call to update() with type = "lookup"
  cl[[1]] <- quote(update)
  cl$type <- "lookup"

  # Evaluate the modified call in the parent frame
  eval.parent(cl)
}


.validate_build_args <- function(name, type, label, eqn, to, from,
                                 xpts, ypts, source, interpolation, extrapolation,
                                 non_negative, doc) {
  # Only validate arguments that were passed
  passed_arg <- names(match.call())[-1] # Exclude function name
  out <- list()

  # Validate name
  if ("name" %in% passed_arg) {
    out$name <- .validate_name_arg(name)
  }

  # Validate type
  if ("type" %in% passed_arg) {
    out$type <- .validate_type_arg(type)
  }

  # Validate eqn
  if ("eqn" %in% passed_arg) {
    out$eqn <- .validate_eqn_arg(eqn)
    .validate_eqn_syntax(out$eqn, name)
  }

  # Validate label
  if ("label" %in% passed_arg) {
    out$label <- .validate_label_arg(label)
  }

  # Validate flow connections
  if ("to" %in% passed_arg) {
    out$to <- .validate_flow_connector(to, "to")
  }

  if ("from" %in% passed_arg) {
    out$from <- .validate_flow_connector(from, "from")
  }

  # Validate graphical function properties
  # Use length(name) to disambiguate single vs vectorized lookups
  if ("xpts" %in% passed_arg) {
    out$xpts <- .validate_pts_arg(xpts, "xpts", name)
  }
  if ("ypts" %in% passed_arg) {
    out$ypts <- .validate_pts_arg(ypts, "ypts", name)
  }
  if ("interpolation" %in% passed_arg) {
    out$interpolation <- .validate_interpolation_arg(interpolation)
  }
  if ("extrapolation" %in% passed_arg) {
    out$extrapolation <- .validate_extrapolation_arg(extrapolation)
  }
  if ("source" %in% passed_arg) {
    out$source <- .validate_source_arg(source)
  }

  # Validate non_negative
  if ("non_negative" %in% passed_arg) {
    out$non_negative <- .validate_non_negative_arg(non_negative)
  }

  # Validate doc
  if ("doc" %in% passed_arg) {
    out$doc <- .validate_doc_arg(doc)
  }

  out
}


.ensure_length_build_args <- function(args) {
  if (length(args)) {
    # For any arguments that are not xpts or ypts, ensure they are either length 1 or the same length as name
    nms <- setdiff(names(args), c("xpts", "ypts"))
    for (arg_name in nms) {
      args[[arg_name]] <- ensure_length(args[[arg_name]], args[["name"]],
        arg_name = arg_name, target_name = "name"
      )
    }
  }
  args
}


.check_appropriate_properties <- function(type, passed_arg) {
  types <- unique(type)
  nonflow <- setdiff(types, "flow")
  nongf <- setdiff(types, "lookup")
  nonfunc <- setdiff(types, "func")
  gfarg <- intersect(passed_arg, c("xpts", "ypts", "source", "interpolation", "extrapolation"))

  # Only 'from' and 'to' are appropriate for flows
  if (length(nonflow) > 0) {
    flowarg <- intersect(passed_arg, c("from", "to"))
    if (length(flowarg) > 0) {
      cli::cli_warn(c(
        "Inappropriate propert{cli::qty(length(flowarg))}{?y/ies} for {.arg type} = {.val {nonflow}}.",
        "i" = "{.arg {flowarg}} {?is/are} only valid for flows.",
        ">" = "Ignored."
      ))
    }
  }

  # Only 'xpts', 'ypts', 'source', 'interpolation', and 'extrapolation' are appropriate for gfs
  if (length(nongf) > 0 && length(gfarg) > 0) {
    cli::cli_warn(c(
      "Inappropriate propert{cli::qty(length(gfarg))}{?y/ies} for {.arg type} = {.val {nongf}}.",
      "i" = "{.arg {gfarg}} {?is/are} only valid for graphical functions.",
      ">" = "Ignored."
    ))
  }

  invisible()
}


#' Create or modify variables
#'
#' Add or change variables in a stock-and-flow model. Variables may be stocks, flows, constants, auxiliaries, or graphical functions. When creating new variables, only "name", "type", and "eqn" (initial value for stocks) are required. When modifying existing variables, only "name" is required to identify the variable to modify, and any other properties can be updated by including the corresponding arguments.
#'
#' @section Stocks: Stocks define the state of the system. They accumulate material or information over time, such as people, products, or beliefs, which creates memory and inertia in the system. As such, stocks need not be tangible. Stocks are variables that can increase and decrease, and can be measured at a single moment in time. The value of a stock is increased or decreased by flows. A stock may have multiple inflows and multiple outflows. The net change in a stock is the sum of its inflows minus the sum of its outflows.
#'
#' The obligatory properties of a stock are "name", "type", and "eqn". Optional additional properties are "label", "doc", "non_negative".
#'
#' @section Flows: Flows move material and information through the system. Stocks can only decrease or increase through flows. A flow must flow from and/or flow to a stock. If a flow is not flowing from a stock, the source of the flow is outside of the model boundary. Similarly, if a flow is not flowing to a stock, the destination of the flow is outside the model boundary. Flows are defined in units of material or information moved over time, such as birth rates, revenue, and sales.
#'
#' The obligatory properties of a flow are "name", "type", "eqn", and either "from", "to", or both. Optional additional properties are "label", "doc", "non_negative".
#'
#' @section Constants: Constants are variables that do not change over the course of the simulation - they are time-independent. These may be numbers, but also functions. They can depend only on other constants.
#'
#' The obligatory properties of a constant are "name", "type", and "eqn". Optional additional properties are "label", "doc", "non_negative".
#'
#' @section Auxiliaries: Auxiliaries are dynamic variables that change over time. They are used for intermediate calculations in the system, and can depend on other flows, auxiliaries, constants, and stocks.
#'
#' The obligatory properties of an auxiliary are "name", "type", and "eqn". Optional additional properties are "label", "doc", "non_negative".
#'
#' @section Graphical functions: Graphical functions, also known as table or lookup functions, are interpolation functions used to define the desired output (y) for a specified input (x). They are defined by a set of x- and y-domain points, which are used to create a piecewise linear function. The interpolation method defines the behavior of the graphical function between x-points ("constant" to return the value of the previous x-point, "linear" to linearly interpolate between defined x-points), and the extrapolation method defines the behavior outside of the x-points ("NA" to return NA values outside of defined x-points, "nearest" to return the value of the closest x-point).
#'
#' The obligatory properties of a graphical function are "name", "type", "xpts", and "ypts". "xpts" and "ypts" must be of the same length. Optional additional properties are "label", "doc", "source", "interpolation", "extrapolation".
#'
#' @section Non-standard evaluation (NSE): The `name`, `type`, `eqn`, `to`, `from`, and `source` arguments
#' support non-standard evaluation. This means you can pass bare symbols and
#' expressions instead of quoted strings:
#'
#' ```r
#' # These are equivalent:
#' stock(sfm, "population", eqn = "birth_rate * 0.1")
#' stock(sfm, population, eqn = birth_rate * 0.1)
#' ```
#'
#' To inject the value of a variable (rather than its name), use the
#' `!!` (bang-bang) operator from rlang:
#'
#' ```r
#' my_name <- "population"
#' stock(sfm, !!my_name, eqn = 100)
#' ```
#'
#' The `label`, `doc`,  `non_negative`, `xpts`, `ypts`,
#' `interpolation`, and `extrapolation` arguments are not affected by NSE
#' and are evaluated normally.
#'
#' @param object Stock-and-flow model, object of class [`stockflow`][stockflow].
#' @param name Variable name. Accepts a bare symbol (e.g., `population`), a string (`"population"`), or a vector via `c()` (e.g., `c(a, b)` or `c("a", "b")`). Use `!!` to inject from a variable.
#' @param type Type of building block(s); accepts a bare symbol or string. One of `stock`, `flow`, `constant`, `aux`, `lookup`, or `func`. Does not need to be specified to modify an existing variable.
#' @param label Name of variable used for plotting. Defaults to the same as name.
#' @param eqn Equation (or initial value in the case of stocks). Accepts a bare expression (e.g., `a * b + 1`), a string (`"a * b + 1"`), or a numeric value. Use `!!` to inject from a variable. Defaults to `0`.
#' @param to Target of flow. Accepts a bare symbol or string. Must be a stock in the model. Defaults to `NULL` to indicate no target.
#' @param from Source of flow. Accepts a bare symbol or string. Must be a stock in the model. Defaults to `NULL` to indicate no source.
#' @param non_negative If TRUE, variable is enforced to be non-negative (i.e., strictly 0 or positive). Defaults to `FALSE`.
#' @param xpts Only for graphical functions: vector of x-domain points. Must be of the same length as ypts.
#' @param ypts Only for graphical functions: vector of y-domain points. Must be of the same length as xpts.
#' @param source Only for graphical functions: name of the variable which will serve as the input to the graphical function. Accepts a bare symbol or string. Defaults to `NULL`.
#' @param interpolation Only for graphical functions: interpolation method. Must be either "constant" or "linear". Defaults to "linear".
#' @param extrapolation Only for graphical functions: extrapolation method. Must be either `"nearest"` or `"NA"`. Defaults to `"nearest"`.
#' @param doc Description of variable. Defaults to `""` (no description).
#' @param df A data.frame with variable properties to add and/or modify. Each row represents one variable to update. Required columns depend on the variable type being created:
#'
#' - All types require: 'type', 'name'
#' - Stocks require: 'eqn' (initial value)
#' - Flows require: 'eqn', and at least one of 'from' or 'to'
#' - Constants require: 'eqn'
#' - Auxiliaries require: 'eqn'
#' - Graphical functions require: 'xpts', 'ypts'
#'
#' Optional columns for all types: 'label', 'doc', 'non_negative'
#' Optional columns for graphical functions: 'source', 'interpolation', 'extrapolation'
#'
#' Columns not applicable to a variable type should be set to NA. See Examples for a complete demonstration.
#' @param ... Additional arguments (currently unused).
#'
#' @returns A stock-and-flow model object of class [`stockflow`][stockflow]
#' @seealso [stockflow()] to initialize a model, [`simulate()`][simulate.stockflow()] to simulate a model, and [summary()] to run model diagnostics. Variable-specific helper functions [stock()], [flow()], [constant()], [aux()], and [lookup()] are also available as wrappers around update() that set the "type" argument for convenience. Further helper functions for modifying models are [change_name()] to rename a variable, [change_type()] to change a variable's type, and [discard()] to remove a variable.
#' @concept build
#' @importFrom rlang enexpr is_symbol is_call as_name call_name call_args expr_deparse
#' @importFrom stats update
#' @method update stockflow
#' @export
#'
#' @examples
#'
#' # First initialize an empty model
#' sfm <- stockflow()
#' print(sfm)
#' \dontshow{
#' sfm <- sim_settings(sfm, save_by = .5)
#' }
#'
#' # Add two stocks. Specify their initial values in the "eqn" property
#' # and their plotting label.
#' sfm <- stock(sfm, predator, eqn = 10, label = "Predator") |>
#'   stock(prey, eqn = 50, label = "Prey")
#'
#'
#' # Add four flows: the births and deaths of both the predators and prey. The
#' # "eqn" property of flows represents the rate of the flow. In addition, we
#' # specify which stock the flow is coming from ("from") or flowing to ("to").
#' sfm <- flow(sfm, predator_births,
#'   eqn = delta * prey * predator,
#'   label = "Predator Births", to = predator
#' ) |>
#'   flow(predator_deaths,
#'     eqn = gamma * predator,
#'     label = "Predator Deaths", from = predator
#'   ) |>
#'   flow(prey_births,
#'     eqn = alpha * prey,
#'     label = "Prey Births", to = prey
#'   ) |>
#'   flow(prey_deaths,
#'     eqn = beta * prey * predator,
#'     label = "Prey Deaths", from = prey
#'   )
#' plot(sfm)
#'
#' # The flows make use of four other variables: "delta", "gamma", "alpha", and
#' # "beta". Define these as constants in a vectorized manner for efficiency.
#' sfm <- constant(sfm, c(delta, gamma, alpha, beta),
#'   eqn = c(.025, .5, .5, .05),
#'   label = c("Delta", "Gamma", "Alpha", "Beta"),
#'   doc = c(
#'     "Birth rate of predators", "Death rate of predators",
#'     "Birth rate of prey", "Death rate of prey by predators"
#'   )
#' )
#'
#' # We now have a complete predator-prey model which is ready to be simulated.
#' sim <- simulate(sfm)
#' plot(sim)
#'
#' # Modify a variable - note that we no longer need to specify type
#' sfm <- update(sfm, delta, eqn = .03, label = "DELTA")
#'
#' # To add and/or modify variables more quickly, pass a data.frame.
#' # The data.frame is processed per row.
#' # For instance, to create a logistic population growth model:
#' df <- data.frame(
#'   type = c("stock", "flow", "flow", "constant", "constant"),
#'   name = c("X", "inflow", "outflow", "r", "K"),
#'   eqn = c(.01, "r * X", "r * X^2 / K", 0.1, 1),
#'   label = c(
#'     "Population size", "Births", "Deaths", "Growth rate",
#'     "Carrying capacity"
#'   ),
#'   to = c(NA, "X", NA, NA, NA),
#'   from = c(NA, NA, "X", NA, NA)
#' )
#' sfm <- update(stockflow(), df = df)
#'
#' # Run model diagnostics
#' summary(sfm)
#'
#' # --- Programmatic usage ---
#'
#' # To inject the value of an R variable, use !! (bang-bang)
#' my_name <- "growth"
#' sfm <- constant(sfm, !!my_name, eqn = 0.1)
#'
#' # Strings also work
#' sfm <- constant(sfm, "growth", eqn = 0.2)
#'
update.stockflow <- function(object, name, type = NULL,
                             eqn = 0,
                             label = name,
                             doc = "",
                             to = NULL, from = NULL,
                             non_negative = FALSE,
                             xpts = NULL, ypts = NULL,
                             source = NULL,
                             interpolation = "linear",
                             extrapolation = "nearest",
                             df = NULL, ...) {
  # Basic checks
  if (missing(object)) {
    missing_arg("object")
  }
  check_stockflow(object)

  # Handle data frame input
  if (!is.null(df)) {
    object <- add_from_df(object, df)
    return(object)
  }

  # Validate inputs
  if (missing(name)) {
    missing_arg("name")
  }

  # --- NSE: capture and deparse expressions before evaluation ----------------
  # Allows bare symbols and expressions: update(object, pop, eqn = a * b)
  # Use !! for injection from variables: update(object, !!my_name, "stock")
  name_expr <- rlang::enexpr(name)
  .check_name_not_stockflow(name_expr, rlang::caller_env())
  name <- .expr_to_char(name_expr)
  if (!missing(eqn)) eqn <- .expr_to_char(rlang::enexpr(eqn))
  if (!missing(to)) to <- .expr_to_char(rlang::enexpr(to))
  if (!missing(from)) from <- .expr_to_char(rlang::enexpr(from))
  if (!missing(source)) source <- .expr_to_char(rlang::enexpr(source))

  passed_arg <- setdiff(names(match.call()[-1]), c("object", "df"))
  args <- mget(passed_arg)
  args <- do.call(.validate_build_args, args)

  # Ensure length of vector arguments matches length of name
  args <- .ensure_length_build_args(args)

  # Get current variable names
  var_names <- object[["variables"]][["name"]]

  # Find which variables already exist
  idx_exist <- args[["name"]] %in% var_names

  # Get rows of existing variables
  if (any(idx_exist)) {
    var_df <- object[["variables"]][match(args[["name"]][idx_exist], var_names), , drop = FALSE]
  } else {
    var_df <- data.frame()
  }

  # If type was not passed, variables must exist
  # Determine type
  if (!"type" %in% passed_arg) {
    # If type not specified, all names must exist
    if (any(!idx_exist)) {
      missing_vars <- args[["name"]][!idx_exist]
      cli::cli_abort(c(
        "x" = "{.val {missing_vars}} {?does/do} not exist.",
        ">" = "To create a new variable, specify its {.arg type}."
      ))
    }

    # If type not specified, get type from existing variables
    args[["type"]] <- var_df[["type"]]
  } else {
    # Check if existing variables match the specified type
    if (any(idx_exist)) {
      existing_types <- var_df[["type"]]
      nonmatching_type <- args[["type"]][idx_exist] != existing_types

      if (any(nonmatching_type)) {
        bad_names <- args[["name"]][idx_exist][nonmatching_type]
        bad_types <- existing_types[nonmatching_type]
        n <- length(bad_names)

        msg_types <- paste(sprintf("%s ({.val %s})", bad_names, bad_types), collapse = ", ")
        cli::cli_abort(c(
          "x" = "Wrong {.arg type} passed.",
          "i" = "{cli::qty(n)}Variable{?s} {.val {bad_names}} exist{?s/} but ha{?s/ve} different type{?s} ({.val {bad_types}}).",
          ">" = "To create a new variable, specify a unique name.",
          # ">" = "To modify a variable, omit the {.arg type}.",
          ">" = "To change the type of an existing variable, use {.fn change_type}."
        ))
      }
    }
  }

  # Clean names for new variables
  if (any(!idx_exist)) {
    new_names <- clean_name(args[["name"]][!idx_exist], var_names)
    report_name_change(args[["name"]][!idx_exist], new_names)
    args[["name"]][!idx_exist] <- new_names
  }

  # Appropriate passed properties
  .check_appropriate_properties(args[["type"]], passed_arg)

  # Validate flow properties
  flow_result <- .validate_flow_connections(args[["to"]], args[["from"]], args[["name"]])
  args[["to"]] <- flow_result$to
  args[["from"]] <- flow_result$from

  # Validate graphical function (lookup) properties
  if (any(args[["type"]] == "lookup")) {
    lookup_idx <- which(args[["type"]] == "lookup")

    for (j in lookup_idx) {
      i_name <- args[["name"]][j]
      i_exist <- idx_exist[j]

      if (!i_exist) {
        # New lookup: both xpts and ypts are required
        if (is.null(args[["xpts"]]) && is.null(args[["ypts"]])) {
          cli::cli_abort(c(
            "Missing lookup properties for {.val {i_name}}.",
            "x" = "Both {.arg xpts} and {.arg ypts} must be specified for new lookups."
          ))
        } else if (is.null(args[["xpts"]])) {
          cli::cli_abort(c(
            "Missing {.arg xpts} for {.val {i_name}}.",
            "x" = "{.arg xpts} is required for new lookups."
          ))
        } else if (is.null(args[["ypts"]])) {
          cli::cli_abort(c(
            "Missing {.arg ypts} for {.val {i_name}}.",
            "x" = "{.arg ypts} is required for new lookups."
          ))
        }
      } else {
        # Existing lookup: fill in missing xpts or ypts
        idx_var <- which(
          object[["variables"]][["name"]] == i_name &
            object[["variables"]][["type"]] == "lookup"
        )
        if (is.null(args[["xpts"]])) {
          args[["xpts"]][[j]] <- object[["variables"]][idx_var, "xpts"][[1]]
        }
        if (is.null(args[["ypts"]])) {
          args[["ypts"]][[j]] <- object[["variables"]][idx_var, "ypts"][[1]]
        }
      }
    }

    # Per-element validation
    for (j in lookup_idx) {
      gf_result <- .validate_graphical_function(
        args[["type"]][j],
        args[["xpts"]][j],
        args[["ypts"]][j],
        args[["source"]][j],
        args[["interpolation"]][j],
        args[["extrapolation"]][j]
      )
      args[["xpts"]][[j]] <- gf_result$xpts
      args[["ypts"]][[j]] <- gf_result$ypts
      args[["source"]][j] <- gf_result$source
      args[["interpolation"]][j] <- gf_result$interpolation
      args[["extrapolation"]][j] <- gf_result$extrapolation
    }
  }


  # Build/update variables in data frame
  for (i in seq_along(args[["name"]])) {
    # Get the ith element of each argument for this variable
    args_ <- lapply(args, `[[`, i)

    if (idx_exist[i]) {
      # Update existing variable
      object <- do.call(update_variable_row, c(list(object = object), args_))
    } else {
      # Add new variable
      object <- do.call(add_variable_row, c(list(object = object), args_))
    }
  }

  # --- Handle func type: skip variable prep, only invalidate funcs cache ----
  if (all(args[["type"]] == "func")) {
    # Validate function definitions: default arguments must be at the end
    if ("eqn" %in% passed_arg) {
      .validate_func_eqn(args[["eqn"]], args[["name"]])
    }
    object <- invalidate_assemble(object, "funcs")
    object <- sanitize_stockflow(object)
    validate_stockflow(object)
    return(object)
  }

  # Prepare model for assembly/simulation
  object <- .prepare_model_for_assembly(object,
    modified_names = name,
    sanitize = TRUE
  )

  # Pre-assemble components (deferrable; see maybe_pre_assemble)
  object <- maybe_pre_assemble(object)

  object
}


#' Add and/or modify model from data frame
#'
#' @inheritParams update.stockflow
#'
#' @returns A stock-and-flow model object of class [`stockflow`][stockflow]
#' @noRd
#'
add_from_df <- function(object, df) {
  if (!inherits(df, "data.frame")) {
    cli::cli_abort(c(
      "Invalid {.arg df} argument.",
      "x" = "The {.arg df} must be a {.cls data.frame}.",
      "i" = "Received: {.cls {typeof(df)}}."
    ))
  }

  # Get all properties
  prop <- get_building_block_prop()

  # Check whether dataframe has necessary columns
  nec_prop <- c("type", "name")

  if (!all(nec_prop %in% colnames(df))) {
    missing_cols <- nec_prop[!nec_prop %in% colnames(df)]
    cli::cli_abort(c(
      "{cli::qty(length(missing_cols))}Missing required column{?s} in {.arg df}.",
      "x" = "{.val {missing_cols}} {?is/are} required."
    ))
  }

  # Check whether dataframe has columns only in prop
  idx <- !colnames(df) %in% unique(unlist(prop))
  if (any(idx)) {
    invalid_cols <- colnames(df)[idx]
    cli::cli_abort(c(
      "{cli::qty(length(invalid_cols))}Invalid column name{?s} in {.arg df}.",
      "x" = "{.val {invalid_cols}} {?is/are} not valid propert{?y/ies}.",
      "i" = "Valid properties: {.val {unique(unlist(prop))}}."
    ))
  }

  # Add each row by calling update.stockflow
  for (i in seq_len(nrow(df))) {
    arg <- as.list(df[i, ])
    arg <- arg[!is.na(arg)]

    # Only keep appropriate properties for this type
    arg <- arg[names(arg) %in% prop[[arg[["type"]]]]]

    arg[["object"]] <- object
    object <- do.call(update.stockflow, arg)
  }

  object <- sanitize_stockflow(object)
  validate_stockflow(object)
  object
}


#' Change name of variable
#'
#' Change the name of a variable throughout the model. This updates the data frame and all references in equations, flow connections, and labels.
#'
#' @inheritParams update.stockflow
#' @param new_name New name. Character vector of the same length as `name`. Must be unique across all existing variables.
#'
#' @returns A stock-and-flow model object of class [`stockflow`][stockflow] with the name changed throughout the model.
#'
#' @seealso [update()], [discard()]
#' @concept build
#' @export
#' @examples
#' sfm <- stockflow("sir")
#' sfm <- change_name(sfm, c(susceptible, infected, recovered),
#'   new_name = c(S, I, R)
#' )
#' print(sfm)
#'
#' # References to old names are updated
#' as.data.frame(sfm, type = "flow", properties = c("name", "eqn", "to", "from"))
#'
change_name <- function(object, name, new_name) {
  # Basic checks
  if (missing(object)) {
    missing_arg("object")
  }
  check_stockflow(object)

  if (missing(name)) {
    missing_arg("name")
  }

  if (missing(new_name)) {
    missing_arg("new_name")
  }

  # NSE: allow bare symbols, e.g., change_name(object, old, new_name = new)
  name_expr <- rlang::enexpr(name)
  .check_name_not_stockflow(name_expr, rlang::caller_env())
  name <- .expr_to_char(name_expr)
  new_name <- .expr_to_char(rlang::enexpr(new_name))

  new_name <- .validate_name_arg(new_name, arg_name = "new_name")

  # Check new_name length
  if (length(name) != length(new_name)) {
    cli::cli_abort(c(
      "x" = "Length of {.arg new_name} must match length of {.arg name}."
    ))
  }

  # Ensure new_name is clean and unique across variables
  var_names <- object[["variables"]][["name"]]
  # Validate requested source names exist before attempting to match/index
  check_var_existence(name, var_names)
  chosen_new_name <- new_name
  new_name <- clean_name(new_name, var_names)
  report_name_change(chosen_new_name, new_name)

  # Check if any renamed variables are funcs (for cache invalidation)
  renamed_types <- object[["variables"]][match(name, var_names), "type"]

  # If the previous label was the same as the old name, update it to match the new name
  idx_var <- match(name, var_names)
  old_labels <- object[["variables"]][idx_var, "label"]
  update_label <- old_labels == name
  new_labels <- ifelse(update_label, new_name, old_labels)
  object[["variables"]][idx_var, "label"] <- new_labels

  # Update variable name in data frame and all references to it in the model
  object <- .change_name(object, name, new_name)

  # Re-prep equations since .change_name() updates eqn via gsub but not eqn_str
  object <- prep_equations_variables(object)
  object <- prep_stock_change(object)
  object <- invalidate_assemble(object, "variables")
  if (any(renamed_types == "func")) {
    object <- invalidate_assemble(object, "funcs")
  }

  object <- sanitize_stockflow(object)
  validate_stockflow(object)
  object
}


#' Change variable type
#'
#' Change the type of a variable in a stock-and-flow model.
#'
#' @inheritParams update.stockflow
#' @param new_type New variable type; one of `'stock'`, `'flow'`, `'constant'`, `'aux'`, `'lookup'`, or `'func'`. Character vector of the same length as name.
#'
#' @returns A stock-and-flow model object of class [`stockflow`][stockflow()] with the variable type changed throughout the model. Note that changing the type may result in changes to other properties (e.g., a flow must have "to" and/or "from" properties, so these will be added if not already present), and may require changes to the equations of connected variables.
#' @seealso [update()]
#' @concept build
#' @export
#' @examples
#' # Start with a simple predator-prey model
#' sfm <- stockflow("predator_prey")
#'
#' # Change the birth rate of predators from a constant to an auxiliary
#' sfm <- change_type(sfm, delta, new_type = "aux")
#'
#' # This allows us to introduce time-dependence in the birth rate
#' # (e.g., seasonality with a sine function)
#' sfm <- sfm |>
#'   update(delta, eqn = 0.025 + 0.01 * sin(2 * pi * t / 12))
#' sim <- simulate(sfm)
#' plot(sim)
#'
change_type <- function(object, name, new_type) {
  # Basic checks
  if (missing(object)) {
    missing_arg("object")
  }
  check_stockflow(object)

  if (missing(name)) {
    missing_arg("name")
  }

  if (missing(new_type)) {
    missing_arg("new_type")
  }

  # NSE: allow bare symbols, e.g., change_type(object, delta, new_type = aux)
  name_expr <- rlang::enexpr(name)
  .check_name_not_stockflow(name_expr, rlang::caller_env())
  name <- .expr_to_char(name_expr)
  new_type <- .expr_to_char(rlang::enexpr(new_type))

  new_type <- .validate_type_arg(new_type, arg_name = "new_type")

  # Get current variable names
  var_names <- object[["variables"]][["name"]]
  check_var_existence(name, var_names)

  # Check new_type
  if (length(new_type) != length(name)) {
    cli::cli_abort(
      "Length of {.arg new_type} must match length of {.arg name}."
    )
  }

  # Only change type if different from existing type; throw message if type is already the same
  idx_var <- match(name, var_names)
  existing_type <- object[["variables"]][idx_var, "type"]
  same_type_names <- name[existing_type == new_type]
  if (length(same_type_names)) {
    cli::cli_inform(c(
      "Variable{?s} already {?has/have} the specified type -- no change needed.",
      "i" = "{.code {same_type_names}} {?is/are} already {.code {new_type[existing_type == new_type][1]}}."
    ))
    keep <- existing_type != new_type
    name <- name[keep]
    new_type <- new_type[keep]
    existing_type <- existing_type[keep]

    # If all variables have the same existing and new type, return the original model
    if (!length(name)) {
      return(object)
    }
  }

  # Get allowed args for add_variable_row
  allowed_args <- names(formals(add_variable_row))

  for (i in seq_along(name)) {
    # Get current variable properties of old type
    var_properties <- object[["variables"]][object[["variables"]][["name"]] == name[i], ]
    var_properties[["type"]] <- new_type[i]
    var_properties <- var_properties[, colnames(var_properties) %in% allowed_args, drop = FALSE]
    var_list <- as.list(var_properties)

    # Erase variable from model (including all references to it, except for source references as changing the type can still allow it to be a source for a graphical function)
    object <- .discard(object, name[i], remove_references = c("to", "from"))

    # Add variable back with new type
    var_list[["object"]] <- object
    object <- do.call(add_variable_row, var_list)
  }

  # Re-prep equations and invalidate cache since types changed
  object <- prep_equations_variables(object, modified_names = name)
  object <- prep_stock_change(object, modified_names = name)
  object <- invalidate_assemble(object, "variables")

  # Variables that changed away from stock/constant can no longer be conditions
  # in unit tests. Strip them from any test conditions.
  was_cond_type <- existing_type %in% c("stock", "constant")
  now_cond_type <- new_type %in% c("stock", "constant")
  lost_cond <- name[was_cond_type & !now_cond_type]

  if (length(lost_cond) > 0L && length(object[["unit_tests"]]) > 0L) {
    for (i in seq_along(object[["unit_tests"]])) {
      hit <- intersect(lost_cond, names(object[["unit_tests"]][[i]][["conditions"]]))
      if (length(hit) > 0L) {
        object[["unit_tests"]][[i]][["conditions"]][hit] <- NULL
        if (length(object[["unit_tests"]][[i]][["conditions"]]) == 0L) {
          object[["unit_tests"]][[i]][["conditions"]] <- list()
        }
        cli::cli_warn(c(
          "Removed {.val {hit}} from conditions of unit test [{i}] {.val {object[['unit_tests']][[i]][['label']]}}.",
          "i" = "{.val {hit}} {?is/are} no longer {?a stock/stocks} or {?a constant/constants}."
        ))
      }
    }
    # Invalidate cached test deps (cond_refs changed)
    object <- invalidate_assemble(object, "unit_tests")
  }

  sanitize_stockflow(object)
}

#' Check that variable(s) exist in the model
#'
#' @param name Character vector of variable names to check for existence.
#' @param var_names Character vector of existing variable names in the model.
#' @returns Invisibly returns the input name if all variables exist; otherwise, throws an error with the missing variable names.
#' @noRd
check_var_existence <- function(name, var_names) {
  # Find which variables already exist
  idx_exist <- name %in% var_names

  if (any(!idx_exist)) {
    missing_vars <- name[!idx_exist]
    cli::cli_abort(c(
      "{cli::qty(length(missing_vars))}Variable{?s} not found in model.",
      "x" = "{.code {missing_vars}} {cli::qty(length(missing_vars))}{?does/do} not exist."
    ))
  }

  invisible(name)
}


#' Remove variable(s)
#'
#' Remove variable(s) from a stock-and-flow model. All references in flow connections and graphical function sources are also removed. A warning will be thrown if any lingering references to the removed name remain in the model.
#'
#' @inheritParams update.stockflow
#' @param name Name(s) to remove. Accepts bare symbols (e.g., `x`), strings, or vectors via `c()`. Must be variable names.
#' @param remove_references Where to remove references to the discarded variables. By default, references to discarded variables in `"to"`, `"from"`, `"source"`, and `"unit_test"` are removed. Set to `NULL` to keep all references (not recommended). Note that any lingering references in equations will cause errors in simulation and should be removed or updated with `update()` after discarding the variable.
#' @returns A stock-and-flow model object of class [`stockflow`][stockflow()]
#'
#' @seealso [update()], [change_name()]
#' @export
#' @concept build
#' @examples
#' # Add stock
#' sfm <- stockflow() |> stock(x)
#' print(sfm)
#'
#' # Remove stock
#' sfm <- discard(sfm, x)
#' print(sfm)
discard <- function(object, name, remove_references = c("to", "from", "source", "unit_test")) {
  # NSE: allow bare symbols, e.g., discard(object, population)
  name_expr <- rlang::enexpr(name)
  .check_name_not_stockflow(name_expr, rlang::caller_env())
  name <- .expr_to_char(name_expr)

  # Determine if names are variables
  check_var_existence(name, object[["variables"]][["name"]])

  remove_references <- match.arg(remove_references, several.ok = TRUE)


  # Erase variable(s)
  discard_types <- object[["variables"]][object[["variables"]][["name"]] %in% name, "type"]

  object <- .discard(object, name, remove_references = remove_references)

  # Keep sim_settings(vars=...) consistent after variable removal
  sim_vars <- object[["sim_settings"]][["vars"]]
  if (!is.null(sim_vars)) {
    sim_vars <- sim_vars[!(sim_vars %in% name)]
    if (length(sim_vars) == 0L) {
      sim_vars <- NULL
    }
    object[["sim_settings"]][["vars"]] <- sim_vars
  }

  # Invalidate appropriate cache components
  if (any(discard_types == "func")) {
    object <- invalidate_assemble(object, "funcs")
  }
  if (any(discard_types != "func")) {
    object <- invalidate_assemble(object, "variables")
  }

  .warn_lingering_ref(object, name)

  # Re-prep equations and invalidate cache
  object <- prep_equations_variables(object, modified_names = NULL)
  object <- prep_stock_change(object, modified_names = NULL)
  object <- invalidate_assemble(object, "variables")
  object <- sanitize_stockflow(object)
  validate_stockflow(object)
}


#' Remove variable from stock-and-flow model
#'
#' Internal function to remove a variable and all references to it in the model.
#'
#' @inheritParams update.stockflow
#'
#' @returns A stock-and-flow model object of class [`stockflow`][stockflow]
#' @noRd
#'
.discard <- function(object, name, remove_references) {
  # Remove variables from the data frame
  object[["variables"]] <- object[["variables"]][!object[["variables"]][["name"]] %in% name, ]

  # Remove references to these variables in 'to', 'from', 'source' columns
  if (nrow(object[["variables"]])) {
    if ("to" %in% remove_references) {
      # Warn if any
      idx <- object[["variables"]][["to"]] %in% name

      if (any(idx)) {
        dep <- object[["variables"]][idx, "name"]
        cli::cli_warn(c(
          "{cli::qty(length(dep))}Found {?a /}lingering reference{?s} to removed variable{?s} {.code {name}}.",
          ">" = "Removing {.code {name}} from {.code to} of variable{?s} {.val {dep}}."
        ))
      }

      object[["variables"]][idx, "to"] <- ""
    }

    if ("from" %in% remove_references) {
      # Warn if any
      idx <- object[["variables"]][["from"]] %in% name

      if (any(idx)) {
        dep <- object[["variables"]][idx, "name"]
        cli::cli_warn(c(
          "{cli::qty(length(dep))}Found {?a /}lingering reference{?s} from removed variable{?s} {.code {name}}.",
          ">" = "Removing {.code {name}} from {.code from} of variable{?s} {.val {dep}}."
        ))
      }

      object[["variables"]][idx, "from"] <- ""
    }

    if ("source" %in% remove_references) {
      # Warn if any
      idx <- object[["variables"]][["source"]] %in% name

      if (any(idx)) {
        dep <- object[["variables"]][idx, "name"]
        cli::cli_warn(c(
          "{cli::qty(length(dep))}Found {?a /}lingering reference{?s} to removed variable{?s} {.code {name}}.",
          ">" = "Removing {.code {name}} from {.code source} of variable{?s} {.val {dep}}."
        ))
      }

      object[["variables"]][idx, "source"] <- ""
    }

    if ("unit_test" %in% remove_references && length(object[["unit_tests"]]) > 0) {
      # Get cached (or freshly computed) unit test dependencies
      td <- get_test_deps(object)
      object <- td[["object"]]
      test_deps <- td[["deps"]]

      # Remove tests whose expr ONLY references the discarded variable(s)
      keep <- vapply(seq_along(object[["unit_tests"]]), function(i) {
        er <- test_deps[[i]][["expr_refs"]]
        length(er) == 0L || !all(er %in% name)
      }, logical(1))

      removed_tests <- object[["unit_tests"]][!keep]
      if (length(removed_tests) > 0L) {
        removed_nrs <- which(!keep)
        removed_labels <- vapply(removed_tests, function(t) t[["label"]], character(1))
        removed_info <- paste0("[", removed_nrs, "] ", removed_labels)
        cli::cli_warn(c(
          "Removed {length(removed_labels)} unit test{?s} whose expression only referenced {.code {name}}.",
          ">" = "Removed: {.val {removed_info}}."
        ))
      }
      object[["unit_tests"]] <- object[["unit_tests"]][keep]
      test_deps <- test_deps[keep]


      # In remaining tests: strip discarded variable from conditions
      for (i in seq_along(object[["unit_tests"]])) {
        hit <- intersect(name, test_deps[[i]][["cond_refs"]])
        if (length(hit) > 0L) {
          object[["unit_tests"]][[i]][["conditions"]][hit] <- NULL

          # If conditions list is now empty, set to empty list (instead of NULL) for easier handling downstream
          if (length(object[["unit_tests"]][[i]][["conditions"]]) == 0L) {
            object[["unit_tests"]][[i]][["conditions"]] <- list()
          }

          cli::cli_warn(c(
            "Removed {.val {hit}} from conditions of unit test [{i}] {.val {object[['unit_tests']][[i]][['label']]}}."
          ))
        }
      }

      # Invalidate cached test deps (tests may have been removed/modified)
      object <- invalidate_assemble(object, "unit_tests")
    }
  }

  object
}


.warn_lingering_ref <- function(object, name) {
  # Check for lingering references to removed variable in equations
  for (var in name) {
    idx <- grepl(object[["variables"]][["eqn"]], pattern = paste0("\\b", var, "\\b"))
    dep <- object[["variables"]][idx, "name"]
    if (any(idx)) {
      cli::cli_warn(c(
        "{cli::qty(length(dep))}Found {?a /}lingering reference{?s} to removed variable {.code {var}}.",
        ">" = "Check equation of variable{?s} {.val {dep}}."
      ))
    }
  }

  # Check for lingering references in unit test expressions
  if (length(object[["unit_tests"]]) > 0L) {
    full_model_names <- c(get_model_var(object), name)

    for (i in seq_along(object[["unit_tests"]])) {
      linger <- intersect(
        name,
        .ut_expr_vars(object[["unit_tests"]][[i]][["expr_str"]], full_model_names)[["model_refs"]]
      )
      if (length(linger) > 0L) {
        cli::cli_warn(c(
          "Unit test [{i}] {.val {object[['unit_tests']][[i]][['label']]}} still references removed variable{?s} {.code {linger}}.",
          ">" = "Update or remove this test with {.fn unit_test} or {.fn discard_unit_test}."
        ))
      }
    }
  }

  invisible()
}
