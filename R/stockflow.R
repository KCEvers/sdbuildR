#' Create a new stock-and-flow model
#'
#' Initialize a stock-and-flow model of class [`stockflow`][stockflow]. You can
#' either create an empty stock-and-flow model or load a template from the model
#' library.
#'
#' Do not edit the object manually; this will likely lead to errors downstream.
#' Rather, use [meta()], [sim_settings()], [update()], and [custom_func()] for safe manipulation.
#'
#' @eval template_param_doc()
#' @param version Optional semantic version of the template to load. Flexible in
#' what it accepts: a number (`1`), a string (`"1.2.1"`), an optional `"v"` prefix
#' (`"v1"`), a [package_version()] value (injected with `!!`), or a bare symbol
#' (`v1.0.0`). Matching is prefix-based: `version = 1` returns the highest `1.x.y`,
#' `version = "1.2"` the highest `1.2.y`, and `version = "1.2.1"` is exact.
#' Defaults to the latest available version.
#'
#' @returns A stock-and-flow model object of class [`stockflow`][stockflow]. Its structure is based
#'  on [XML Interchange Language for System Dynamics (XMILE)](https://docs.oasis-open.org/xmile/xmile/v1.0/os/xmile-v1.0-os.html). It is a nested list, containing:
#' \describe{
#'  \item{meta}{Meta-information about model. A list containing arguments listed in [meta()].}
#'  \item{sim_settings}{Simulation specifications. A list containing arguments listed in [sim_settings()].}
#'  \item{model}{Model variables, grouped under the variable types stock, flow, aux (auxiliaries), constant, gf (graphical functions), and func (custom functions). Each variable contains arguments as listed in [update()].}
#'  }
#'
#' Use [summary()] to run model diagnostics, [as.data.frame()] to convert to a data.frame, [plot()] to visualize.
#'
#' @export
#' @concept build
#' @seealso [update()], [meta()], [custom_func()], [sim_settings()]
#'
#' @examples sfm <- stockflow()
#' summary(sfm)
#'
#' \dontshow{
#' sfm <- sim_settings(sfm, save_by = 1)
#' }
#'
#' # Load a template
#' sfm <- stockflow("lorenz")
#' sim <- simulate(sfm)
#' plot(sim)
stockflow <- function(template = NULL, version = NULL) {
  # NSE: resolve `version` from a bare symbol (v1.0.0), number, string, or
  # `!!`-injected value, reusing the same helper as update().
  version <- .expr_to_char(rlang::enexpr(version))

  if (!is.null(template)) {
    return(templates(template, version = version))
  }

  sfm <- new_stockflow()
  sfm
}


#' Create empty assemble cache
#' @returns Empty assemble cache
#' @noRd
empty_assemble <- function() {
  list(
    language = NULL,
    input_hash = NULL, # hash of base codegen inputs; mismatch => cache is stale
    eqn_cache = NULL, # per-variable memoized R->Julia translations
    ordering = NULL, # Contains deps_by_name for incremental updates
    funcs = "",
    times = "",
    static = list(script = "", par_names = character(0)),
    nonneg_stocks = empty_nonneg_stocks(),
    ode = "",
    callback = "",
    run = "",
    post = "",
    intermediaries = list(),
    ensemble = list(),
    summary = NULL, # cached diagnostics result
    unit_tests = list(deps = NULL) # cached unit test dependencies (positionally indexed)
  )
}


#' Complete assemble cache structure
#' @returns Assemble cache with canonical empty_assemble() fields
#' @noRd
complete_assemble <- function(assemble) {
  no_assemble <- empty_assemble()

  if (!is.list(assemble)) {
    return(no_assemble)
  }

  keep_names <- intersect(names(no_assemble), names(assemble))
  for (name in keep_names) {
    no_assemble[name] <- assemble[name]
  }

  no_assemble
}


# User-authored input columns of the variables table. The model hash is taken
# over exactly these (a whitelist), so any column that assembly *derives* (e.g.
# eqn_str, sum_eqn, sum_name, inflow, outflow, preceding_eqn) cannot perturb the
# hash mid-assembly -- which would otherwise defeat the cache short-circuit.
input_var_cols <- function() {
  c(
    "name", "type", "eqn", "label", "doc", "non_negative",
    "to", "from", "source", "interpolation", "extrapolation", "xpts", "ypts"
  )
}


# Base simulation settings that affect pre-assembled code. Runtime/output-only
# settings (seed, vars, only_stocks, save_sims) are intentionally excluded.
codegen_sim_setting_names <- function() {
  c(
    "method", "start", "stop", "dt", "save_by", "save_times", "save_length",
    "time_units", "language", "keep_nonnegative_stock", "keep_nonnegative_flow"
  )
}


#' Hash the user-authored inputs that determine base generated code
#'
#' The base assembly cache is valid iff this hash is unchanged. Runtime/output
#' choices such as {.arg vars}, {.arg only_stocks}, {.arg seed}, and
#' {.arg save_sims} are excluded because they do not affect the pre-assembled
#' model structure.
#'
#' Custom-function bodies live in the `eqn` of `func`-type rows, so hashing the
#' input columns of `variables` covers them too. `meta`, `import_metadata`, and
#' unit tests do not affect base generated simulation code and are excluded.
#'
#' @inheritParams update.stockflow
#' @returns A scalar hash string.
#' @noRd
compute_model_hash <- function(object) {
  vars <- object[["variables"]]
  input_cols <- intersect(input_var_cols(), colnames(vars))
  sim_settings <- object[["sim_settings"]]
  setting_names <- intersect(codegen_sim_setting_names(), names(sim_settings))

  rlang::hash(list(
    variables = vars[, input_cols, drop = FALSE],
    sim_settings = sim_settings[setting_names]
  ))
}


empty_unit_tests <- function() {
  list()
}


empty_variables <- function() {
  variables_df <- data.frame(
    name = character(0),
    type = character(0),
    eqn = character(0),
    label = character(0),
    doc = character(0),
    non_negative = logical(0),
    # Flow-specific
    to = character(0),
    from = character(0),
    # Graphical function-specific (list-columns)
    source = character(0),
    interpolation = character(0),
    extrapolation = character(0),
    # Prepared equation strings (language-specific, updated in update())
    eqn_str = character(0),
    # Stock accumulation equations and names (language-specific)
    sum_eqn = character(0),
    sum_name = character(0),
    stringsAsFactors = FALSE
  )

  # Add list-columns for xpts and ypts (graphical functions)
  variables_df$xpts <- list()
  variables_df$ypts <- list()

  # Add list-columns for inflow and outflow (stocks)
  variables_df$inflow <- list()
  variables_df$outflow <- list()
  variables_df
}


get_variable_row <- function(name, type,
                             eqn = "0",
                             label = name,
                             doc = "",
                             non_negative = FALSE,
                             to = "",
                             from = "",
                             xpts = NULL,
                             ypts = NULL,
                             source = "",
                             interpolation = "linear",
                             extrapolation = "nearest") {
  row <- data.frame(
    name = name,
    type = type,
    eqn = eqn %||% "0",
    label = label %||% name,
    doc = doc %||% "",
    non_negative = non_negative %||% FALSE,
    to = if (type == "flow") (to %||% "") else "",
    from = if (type == "flow") (from %||% "") else "",
    source = if (type == "lookup") (source %||% "") else "",
    interpolation = if (type == "lookup") (interpolation %||% "linear") else "",
    extrapolation = if (type == "lookup") (extrapolation %||% "nearest") else "",
    eqn_str = "",
    sum_eqn = "",
    sum_name = "",
    stringsAsFactors = FALSE
  )

  if (type == "lookup") {
    row[["xpts"]][[1]] <- xpts %||% numeric(0)
    row[["ypts"]][[1]] <- ypts %||% numeric(0)
  } else {
    row[["xpts"]] <- list(NULL)
    row[["ypts"]] <- list(NULL)
  }

  # Add list-columns for inflow and outflow (stocks)
  row[["inflow"]] <- list(NULL)
  row[["outflow"]] <- list(NULL)

  row
}


#' Add a variable to sfm
#'
#' @param object Stock-and-flow model
#' @inheritParams update.stockflow
#'
#' @returns Updated object with variable added
#' @noRd
#'
add_variable_row <- function(object, name, type,
                             eqn,
                             label,
                             doc,
                             non_negative,
                             to,
                             from,
                             xpts,
                             ypts,
                             source,
                             interpolation,
                             extrapolation) {
  # Create new row
  arg <- compact_(as.list(environment()))
  if ("object" %in% names(arg)) {
    arg[["object"]] <- NULL
  }
  new_row <- do.call(get_variable_row, arg)

  # Add to variables data frame
  object[["variables"]] <- rbind(object[["variables"]], new_row)

  object
}


new_meta <- function() {
  meta_defaults <- as.list(formals(meta))
  meta_defaults <- meta_defaults[!names(meta_defaults) %in%
    c("object", "...")]
  meta_defaults[["created"]] <- Sys.time() # Manually overwrite time
  meta_defaults
}

new_sim_settings <- function() {
  spec_defaults <- as.list(formals(sim_settings))
  spec_defaults <- spec_defaults[!names(spec_defaults) %in% c("object", "...")]

  # Save fields save_by / save_times / save_length are NULL (meaning "save all dt
  # steps") by default; at most one is ever non-NULL. Set explicitly so the named
  # NULL entries always exist regardless of how formals() carries the defaults.
  spec_defaults["save_by"] <- list(NULL)
  spec_defaults["save_times"] <- list(NULL)
  spec_defaults["save_length"] <- list(NULL)

  # Ensemble summary defaults: formals() yields unevaluated c(...) calls for
  # these, so set the realised vectors explicitly.
  spec_defaults[["central"]] <- c("mean", "median")
  spec_defaults[["spread"]] <- "quantile"
  spec_defaults[["quantiles"]] <- c(0.025, 0.975)
  spec_defaults
}

empty_nonneg_stocks <- function() {
  list(func_def = "", root_arg = "", check_root = "")
}

#' Create new object of class [`stockflow`][stockflow]
#'
#' @returns A stock-and-flow model of class [`stockflow`][stockflow]
#' @noRd
#'
new_stockflow <- function() {
  meta_defaults <- new_meta()
  spec_defaults <- new_sim_settings()

  # Create data frame for variables (all types in one data frame, including funcs)
  variables_df <- empty_variables()

  # Create list with fixed structure
  obj <- list(
    meta = meta_defaults,
    sim_settings = spec_defaults,
    variables = variables_df,
    # Cache for pre-assembled simulation components
    assemble = empty_assemble(),
    # Import metadata (NULL for programmatic models, populated for imported models)
    import_metadata = NULL,
    # User-defined unit tests; named list keyed by label
    unit_tests = empty_unit_tests()
  )

  object <- structure(obj, class = "stockflow")
  object <- sanitize_stockflow(object)
  # Ensure deterministic ordering for presentation/storage: sort by type then name
  if (nrow(object[["variables"]]) > 0) {
    type_levels <- c("stock", "flow", "constant", "aux", "lookup", "func")
    type_rank <- match(object[["variables"]][["type"]], type_levels)
    type_rank[is.na(type_rank)] <- length(type_levels) + 1
    name_lower <- tolower(object[["variables"]][["name"]])
    ord <- order(type_rank, name_lower)
    object[["variables"]] <- object[["variables"]][ord, , drop = FALSE]
    rownames(object[["variables"]]) <- NULL
  }

  object

  return(object)
}


#' Get the sources and destinations of flows
#'
#' @inheritParams update.stockflow
#'
#' @returns data.frame with for each flow which stock and flow to and/or from
#' @noRd
get_flow_df <- function(object) {
  check_stockflow(object)

  flows <- object[["variables"]][object[["variables"]][["type"]] == "flow", ]

  if (nrow(flows) == 0) {
    return(data.frame(name = character(0), to = character(0), from = character(0)))
  }

  data.frame(
    name = flows[["name"]],
    to = ifelse(is.na(flows[["to"]]), "", flows[["to"]]),
    from = ifelse(is.na(flows[["from"]]), "", flows[["from"]]),
    stringsAsFactors = FALSE
  )
}


#' Check whether object is of class [`stockflow`][stockflow]
#'
#' @inheritParams update.stockflow
#'
#' @returns Invisibly returns TRUE, called for side effects.
#' @noRd
check_stockflow <- function(object) {
  # Check whether it is a stockflow object
  if (!inherits(object, "stockflow")) {
    cli::cli_abort(c(
      "Expected object of class {.cls stockflow}.",
      "i" = "Create a stock-and-flow model with {.fn stockflow} or {.fn import_insightmaker}."
    ))
  }
  invisible(TRUE)
}


#' Check summary diagnostics for errors
#'
#' Checks the summary diagnostics for any errors and aborts with a message if any are found. Called before simulation and unit testing to ensure the model is in a valid state.
#' @inheritParams update.stockflow
#' @returns Invisibly returns TRUE if no errors are found, otherwise aborts with a message.
#' @noRd
check_summary_diagnostics <- function(object) {
  if (is.null(object[["assemble"]][["summary"]])) {
    object[["assemble"]][["summary"]] <- summary(object)
  }
  debug_result <- object[["assemble"]][["summary"]]
  errors <- Filter(function(c) c$problem == "error", debug_result)
  if (length(errors) > 0) {
    n <- length(errors)
    labels <- vapply(names(errors), .problem_label, character(1))
    names(labels) <- rep("x", n)
    cli::cli_abort(c(
      "{cli::qty(n)}Model has {n} problem{?s}. Run {.fn summary} for details.",
      labels
    ))
  }

  invisible(TRUE)
}

#' Validate stockflow class
#'
#' Pure structural validator for stock-and-flow models. Checks that the object
#' has the required fields and correct types, but does NOT modify the object.
#' Use sanitize_stockflow() to apply defaults and fix invalid state.
#'
#' @inheritParams update.stockflow
#'
#' @returns The stock-and-flow model, unchanged (invisibly)
#' @noRd
#'
validate_stockflow <- function(object) {
  check_stockflow(object)

  # Check variables data frame has required columns
  if (nrow(object[["variables"]]) > 0) {
    required_var_cols <- colnames(empty_variables())
    missing_cols <- setdiff(required_var_cols, colnames(object[["variables"]]))
    if (length(missing_cols) > 0) {
      cli::cli_warn(c("!" = "Variables is missing columns: {.field {missing_cols}}"))
    }

    # Warn about flow connections to non-stocks (but don't fix)
    flows <- object[["variables"]][object[["variables"]][["type"]] == "flow", ]
    non_stock_names <- object[["variables"]][object[["variables"]][["type"]] != "stock", "name"]

    if (nrow(flows) > 0) {
      flows_to_invalid <- !is.na(flows[["to"]]) & flows[["to"]] != "" & flows[["to"]] %in% non_stock_names
      if (any(flows_to_invalid)) {
        for (i in which(flows_to_invalid)) {
          flow_name <- flows[i, "name"]
          to_name <- flows[i, "to"]
          cli::cli_warn(c(
            "{.code {flow_name}} flows to non-stock variable {.code {to_name}}.",
            "x" = "{.code {to_name}} is not a stock."
          ))
        }
      }

      flows_from_invalid <- !is.na(flows[["from"]]) & flows[["from"]] != "" & flows[["from"]] %in% non_stock_names
      if (any(flows_from_invalid)) {
        for (i in which(flows_from_invalid)) {
          flow_name <- flows[i, "name"]
          from_name <- flows[i, "from"]
          cli::cli_warn(c(
            "{.code {flow_name}} flows from non-stock variable {.code {from_name}}.",
            "x" = "{.code {from_name}} is not a stock."
          ))
        }
      }

      flows_same <- !is.na(flows[["to"]]) & !is.na(flows[["from"]]) &
        flows[["to"]] == flows[["from"]] & flows[["to"]] != ""
      if (any(flows_same)) {
        for (i in which(flows_same)) {
          flow_name <- flows[i, "name"]
          same_name <- flows[i, "to"]
          cli::cli_warn(c(
            "{.code {flow_name}} flows to and from the same variable.",
            "x" = "{.code {same_name}} is both source and target."
          ))
        }
      }
    }
  }

  invisible(object)
}


#' Validate the compiled model layout before code generation
#'
#' Structural invariant check run at codegen time (via pre_assemble_components()).
#' Catches internal inconsistencies that would otherwise silently produce a
#' wrong simulation rather than an error -- most importantly that each stock's
#' derivative slot (`sum_name`, e.g. `dSdt[i]` in Julia) matches the stock's
#' position in the state vector. A mismatch means stock dynamics are swapped.
#'
#' These are invariants the package must uphold; a violation is a bug in
#' sdbuildR, not user error, so it aborts with a report link.
#'
#' @inheritParams update.stockflow
#' @returns Invisibly returns the object, unchanged.
#' @noRd
validate_layout <- function(object) {
  vars <- object[["variables"]]
  if (is.null(vars) || nrow(vars) == 0) {
    return(invisible(object))
  }

  internal_error <- function(...) {
    cli::cli_abort(
      c(..., "i" = "This is an internal sdbuildR error; please report it at {.url https://github.com/kcevers/sdbuildR/issues}."),
      class = "stockflow_layout_error"
    )
  }

  # Variable names must be unique (state vector / unpacking rely on this).
  dups <- unique(vars[["name"]][duplicated(vars[["name"]])])
  if (length(dups) > 0) {
    internal_error("Duplicate variable name{?s}: {.val {dups}}.")
  }

  # Stock derivative slots must match the state-vector order. sum_name is only
  # populated after prep_stock_change(), so skip if absent (e.g. empty model).
  stock_idx <- which(vars[["type"]] == "stock")
  if (length(stock_idx) > 0 && "sum_name" %in% colnames(vars) &&
    is_defined(vars[stock_idx[1], "sum_name"])) {
    lang <- lang_adapter(object[["sim_settings"]][["language"]])
    stock_names <- vars[stock_idx, "name"]
    expected <- vapply(
      seq_along(stock_idx),
      function(pos) lang$format_sum_name(vars[stock_idx[pos], ], pos, stock_names),
      character(1)
    )
    actual <- vars[stock_idx, "sum_name"]
    if (!identical(actual, expected)) {
      internal_error(
        "Stock derivative slots are misaligned with the state vector.",
        "x" = "Stocks (in order): {.val {stock_names}}.",
        "x" = "Expected slots: {.val {expected}}.",
        "x" = "Found slots: {.val {actual}}."
      )
    }
  }

  invisible(object)
}


#' Sanitize stockflow object
#'
#' Applies defaults and fixes invalid state in the stock-and-flow model.
#' Called after modifier functions to ensure the object is in a consistent state.
#'
#' @inheritParams update.stockflow
#'
#' @returns A stock-and-flow model of class [`stockflow`][stockflow]
#' @noRd
#'
sanitize_stockflow <- function(object) {
  check_stockflow(object)

  # Sanitize variables data frame
  if (nrow(object[["variables"]])) {
    # Migrate old "gf" type to "lookup"
    idx_gf <- object[["variables"]][["type"]] == "gf"
    if (any(idx_gf)) {
      object[["variables"]][idx_gf, "type"] <- "lookup"
    }

    # Ensure label is set (defaults to name if missing)
    idx_missing_label <- !vapply(object[["variables"]][["label"]], is_defined, logical(1))
    if (any(idx_missing_label)) {
      object[["variables"]][idx_missing_label, "label"] <- object[["variables"]][idx_missing_label, "name"]
    }

    # Fix flows: ensure to and from only refer to stocks
    flows <- object[["variables"]][object[["variables"]][["type"]] == "flow", ]
    non_stock_names <- object[["variables"]][object[["variables"]][["type"]] != "stock", "name"]

    if (nrow(flows)) {
      # Fix invalid 'to'
      flows_to_invalid <- !is.na(flows[["to"]]) & flows[["to"]] != "" & flows[["to"]] %in% non_stock_names
      if (any(flows_to_invalid)) {
        for (i in which(flows_to_invalid)) {
          flow_name <- flows[i, "name"]
          to_name <- flows[i, "to"]
          cli::cli_warn(c(
            "{.code {flow_name}} flows to non-stock {.code {to_name}}.",
            ">" = "Removed {.code {to_name}} from {.arg to}."
          ))
          object[["variables"]][object[["variables"]][["name"]] == flow_name, "to"] <- ""
        }
      }

      # Fix invalid 'from'
      flows_from_invalid <- !is.na(flows[["from"]]) & flows[["from"]] != "" & flows[["from"]] %in% non_stock_names
      if (any(flows_from_invalid)) {
        for (i in which(flows_from_invalid)) {
          flow_name <- flows[i, "name"]
          from_name <- flows[i, "from"]
          cli::cli_warn(c(
            "{.code {flow_name}} flows from non-stock {.code {from_name}}.",
            ">" = "Removed {.code {from_name}} from {.arg from}."
          ))
          object[["variables"]][object[["variables"]][["name"]] == flow_name, "from"] <- ""
        }
      }

      # Fix to and from being the same
      flows_invalid <- !is.na(flows[["to"]]) & !is.na(flows[["from"]]) &
        flows[["to"]] == flows[["from"]] & flows[["to"]] != ""
      if (any(flows_invalid)) {
        for (i in which(flows_invalid)) {
          flow_name <- flows[i, "name"]
          same_name <- flows[i, "from"]
          cli::cli_warn(c(
            "{.code {flow_name}} flows to and from the same variable {.code {same_name}}.",
            ">" = "Removed {.code {same_name}} from {.arg from}."
          ))
          object[["variables"]][object[["variables"]][["name"]] == flow_name, "from"] <- ""
        }
      }
    }
  }

  # Ensure deterministic ordering for presentation/storage: sort by type then name
  if (nrow(object[["variables"]]) > 0) {
    type_levels <- c("stock", "flow", "constant", "aux", "lookup", "func")
    type_rank <- match(object[["variables"]][["type"]], type_levels)
    type_rank[is.na(type_rank)] <- length(type_levels) + 1
    name_lower <- tolower(object[["variables"]][["name"]])
    ord <- order(type_rank, name_lower)
    object[["variables"]] <- object[["variables"]][ord, , drop = FALSE]
    rownames(object[["variables"]]) <- NULL

    # Refresh stock sum_name against the canonical order just established. For
    # Julia, sum_name is a positional state-vector index (dSdt[i]) that must
    # match the stock order used to build init/unpack at compile time. Mutators
    # (e.g. change_type) compute sum_name in prep_stock_change *before* this
    # sort, so without this re-sync the indices can be permuted (#change_type
    # bug). No-op for R, where sum_name is name-based.
    stock_idx <- which(object[["variables"]][["type"]] == "stock")
    has_sum_name <- "sum_name" %in% colnames(object[["variables"]]) &&
      length(stock_idx) > 0 &&
      is_defined(object[["variables"]][stock_idx[1], "sum_name"])
    if (has_sum_name) {
      lang <- lang_adapter(object[["sim_settings"]][["language"]])
      stock_names <- object[["variables"]][stock_idx, "name"]
      for (pos in seq_along(stock_idx)) {
        i <- stock_idx[pos]
        object[["variables"]][i, "sum_name"] <- lang$format_sum_name(
          object[["variables"]][i, ], pos, stock_names
        )
      }
    }
  }

  object
}


#' Modify meta of stock-and-flow model
#'
#' The meta of a stock-and-flow model contains metadata about the model, such as the name, author, and version. Modify the meta of an existing model with standard or custom properties.
#'
#' @inheritParams update.stockflow
#' @param name Model name. Defaults to "My Model".
#' @param caption Model description. Defaults to "My Model Description".
#' @param created Date the model was created. Defaults to Sys.time().
#' @param author Creator of the model. Defaults to "Me".
#' @param version Model version. Defaults to "0.0.0.9000", a development version.
#' @param URL URL associated with model. Defaults to "".
#' @param doi DOI associated with the model. Defaults to "".
#' @param ... Optional other entries to add to the meta.
#'
#' @returns A stock-and-flow model object of class [`stockflow`][stockflow]
#' @concept build
#' @export
#'
#' @examples
#' sfm <- stockflow() |>
#'   meta(
#'     name = "My first model",
#'     caption = "This is my first model",
#'     author = "Kyra Evers",
#'     version = "1.1"
#'   )
meta <- function(object, name = "My Model", caption = "My Model Description",
                 created = Sys.time(), author = "Me", version = "0.0.0.9000", URL = "", doi = "", ...) {
  # Basic check
  if (missing(object)) {
    missing_arg("object")
  }

  check_stockflow(object)

  argg <- list(...)
  if (!missing(name)) argg$name <- name
  if (!missing(caption)) argg$caption <- caption
  if (!missing(created)) argg$created <- created
  if (!missing(author)) argg$author <- author
  if (!missing(version)) argg$version <- version
  if (!missing(URL)) argg$URL <- URL
  if (!missing(doi)) argg$doi <- doi

  object[["meta"]] <- utils::modifyList(object[["meta"]], argg)

  object <- sanitize_stockflow(object)

  object
}


#' Report whether any names were changed
#'
#' @param old_names Vector with old names
#' @param new_names Vector with new names
#'
#' @returns Returns `NULL`, called for side effects
#' @noRd
report_name_change <- function(old_names, new_names) {
  # Warning if specified name changed
  idx <- !is.na(old_names) & !is.na(new_names) & old_names != new_names
  if (any(idx)) {
    n <- sum(idx)
    cli::cli_warn(c(
      "{cli::qty(n)}{?A name was/Names were} changed for syntactic validity or uniqueness.",
      "i" = paste0(
        paste0("{.val ", old_names[idx], "} \u2192 {.code ", new_names[idx], "}"),
        collapse = ", "
      )
    ))
  }

  return(invisible())
}


#' Get possible variable properties per building block type
#'
#' @returns List with default properties per building block type
#' @noRd
#'
get_building_block_prop <- function() {
  list(
    "stock" = c(
      "name", "type", "eqn", "label", "doc",
      "non_negative"
    ),
    "flow" = c(
      "name", "type", "eqn", "to", "from", "label", "doc",
      "non_negative"
    ),
    "constant" = c(
      "name", "type", "eqn", "label", "doc",
      "non_negative"
    ),
    "aux" = c(
      "name", "type", "eqn", "label", "doc",
      "non_negative"
    ),
    "lookup" = c("name", "type", "label", "xpts", "ypts", "source", "interpolation", "extrapolation", "doc"),
    "func" = c("name", "type", "eqn", "label", "doc")
  )
}


#' Convert stock-and-flow model to data frame
#'
#' Create a data frame with properties of all model variables and functions. Specify the variable types, variable names, and/or properties to get a subset of the data frame.
#'
#' @inheritParams plot.stockflow
#' @param vars Variable names to retain in the data frame. Defaults to `NULL` to include all variables.
#' @param type Variable types to retain in the data frame. Must be one or more of 'stock', 'flow', 'constant', 'aux', 'lookup', or 'func'. Defaults to `NULL` to include all types.
#' @param properties Variable properties to retain in the data frame. Defaults to `NULL` to include all properties.
#' @param row.names `NULL` or a character vector giving the row names for the data frame. Missing values are not allowed.
#' @param optional Ignored parameter.
#'
#' @returns A data.frame with one row per model component.
#'   Common columns include \code{type} (component type), \code{name} (variable name),
#'   \code{eqn} (equation), and \code{label} (descriptive label). Additional columns may include \code{to}, \code{from},
#'   \code{non_negative}, and others depending on variable types. The exact columns returned
#'   depend on the \code{type} and \code{properties} arguments. Returns an empty data.frame
#'   if no components match the filters.
#' @export
#' @concept build
#' @method as.data.frame stockflow
#'
#' @examples as.data.frame(stockflow("sir"))
#'
#' # Only show stocks
#' as.data.frame(stockflow("sir"), type = "stock")
#'
#' # Only show specific variables
#' as.data.frame(stockflow("sir"), vars = c("susceptible", "infected"))
#'
#' # Only show equation and label
#' as.data.frame(stockflow("sir"), properties = c("eqn", "label"))
#'
as.data.frame.stockflow <- function(x,
                                    row.names = NULL, optional = FALSE,
                                    vars = NULL, type = NULL,
                                    properties = NULL, ...) {
  vars <- .expr_to_char(rlang::enexpr(vars))
  check_stockflow(x)
  sfm <- x
  rm(x)

  # Check for mutually exclusive parameters
  if (!is.null(vars) && !is.null(type)) {
    cli::cli_warn(c("!" = "Both {.arg vars} and {.arg type} specified; ignoring {.arg type} and using {.arg vars} only."))
    type <- NULL
  }

  # Validate and clean parameters
  if (!is.null(vars)) {
    .validate_name_arg(vars, arg_name = "vars")
  }

  # Only keep specified types
  if (!is.null(type)) {
    type <- .validate_type_arg(type, arg_name = "type")

    if (length(type) == 0) {
      cli::cli_abort(c("x" = "At least one {.arg type} must be specified"))
    }
  }

  df <- data.frame()

  # Add model variables to data frame - already in data frame format!
  core_types <- .stockflow_types()
  if ((is.null(type) || any(core_types %in% type)) && nrow(sfm[["variables"]]) > 0) {
    var_df <- sfm[["variables"]]

    # Filter by type if specified
    if (!is.null(type)) {
      var_types <- type[type %in% core_types]
      var_df <- var_df[var_df[["type"]] %in% var_types, , drop = FALSE]
    }

    # Convert list-columns to character strings for display
    if (nrow(var_df) > 0) {
      gf_idx <- var_df[["type"]] == "lookup"
      if (any(gf_idx)) {
        var_df[gf_idx, "xpts"] <- vapply(var_df[gf_idx, "xpts"], function(x) {
          paste0(x, collapse = ", ")
        }, character(1))
        var_df[gf_idx, "ypts"] <- vapply(var_df[gf_idx, "ypts"], function(x) {
          paste0(x, collapse = ", ")
        }, character(1))
      }
    }

    df <- bind_rows_(df, var_df)
  }

  if (nrow(df) == 0) {
    return(df)
  }

  # Only keep specified variables
  if (!is.null(vars)) {
    # Clean variable names
    vars <- Filter(nzchar, unique(vars))

    if (length(vars) == 0) {
      cli::cli_abort(c("x" = "At least one {.arg vars} must be specified"))
    }

    # Check if variables exist
    idx_exist <- vars %in% df[["name"]]
    if (!all(idx_exist)) {
      missing_names <- vars[!idx_exist]
      cli::cli_abort(c(
        "Variable{cli::qty(length(missing_names))}{?s} not found in model.",
        "x" = "{.code {missing_names}} {cli::qty(length(missing_names))}{?does/do} not exist."
      ))
    }
    df <- df[df[["name"]] %in% vars, , drop = FALSE]
    if (nrow(df) == 0) {
      return(df)
    }
  }

  # Only keep columns that correspond to update() parameters
  allowed_props <- names(formals(get_variable_row))
  df <- df[, intersect(allowed_props, names(df)), drop = FALSE]

  # Only keep specified properties
  if (!is.null(properties)) {
    # Check if properties exist
    properties <- Filter(nzchar, unique(tolower(properties)))
    if (length(properties) == 0) {
      cli::cli_abort(c("x" = "At least one property must be specified"))
    }

    # Internal properties that shouldn't be exposed to users
    internal_props <- c("prefix")
    existing_prop <- setdiff(Reduce(union, get_building_block_prop()), internal_props)
    idx_exist <- properties %in% existing_prop

    if (!all(idx_exist)) {
      invalid_props <- properties[!idx_exist]
      cli::cli_abort(c(
        "Invalid propert{cli::qty(length(invalid_props))}{?y/ies}.",
        "x" = "{.code {invalid_props}} {?is/are} not valid."
      ))
    }

    # Always show name and type
    properties <- unique(c("type", "name", properties))
    df <- df[, names(df) %in% properties, drop = FALSE]
    if (nrow(df) == 0) {
      return(df)
    }
  }

  # Reorder columns
  order_first <- c("type", "name", "eqn", "label", "to", "from", "non_negative")

  # Get columns to prioritize (in order_first order)
  cols_first <- intersect(order_first, names(df))
  # Get remaining columns (in original order)
  cols_rest <- setdiff(names(df), order_first)
  # Combine columns (handles character(0) safely)
  new_cols <- c(cols_first, cols_rest)
  # Reorder data frame
  df <- df[, new_cols, drop = FALSE]

  # Make sure that for all columns, at least one row is not NA or empty
  # This is especially necessary when only interested in one type, e.g., func

  # Convert empty strings to NA and keep columns with at least one non-NA
  df[] <- lapply(df, function(x) {
    x[x == ""] <- NA
    x
  })
  df <- df[, colSums(!is.na(df)) > 0, drop = FALSE]

  # Handle row.names if provided
  if (!is.null(row.names)) {
    if (length(row.names) != nrow(df)) {
      cli::cli_abort(c(
        "Length mismatch in {.arg row.names}.",
        "x" = "Got {length(row.names)} name{?s} but {nrow(df)} row{?s}."
      ))
    }
    rownames(df) <- row.names
  } else {
    rownames(df) <- NULL
  }

  df
}


#' Print overview of stock-and-flow model
#'
#' Prints a descriptive overview of the model structure, including stock-flow
#' topology, variable names, and simulation settings. For model diagnostics,
#' use [`summary()`][summary.stockflow()].
#'
#' @param x A stock-and-flow model object of class [`stockflow`][stockflow]
#' @param ... Additional arguments (unused)
#'
#' @returns Invisibly returns `x`
#' @export
#' @concept build
#' @seealso [summary.stockflow()], [dependencies()]
#'
#' @examples
#' sfm <- stockflow("sir")
#' print(sfm)
#'
print.stockflow <- function(x, ...) {
  # Header
  model_name <- x[["meta"]][["name"]]
  default_name <- formals(meta)[["name"]]
  has_name <- !is.null(model_name) && nzchar(model_name) && model_name != default_name

  if (has_name) {
    cli::cli_h1("Stock-and-Flow Model: {model_name}")
  } else {
    cli::cli_h1("Stock-and-Flow Model")
  }

  # Count line
  vars <- x[["variables"]]
  types <- vars[["type"]]
  n_stocks <- sum(types == "stock")
  n_flows <- sum(types == "flow")
  n_constants <- sum(types == "constant")
  n_aux <- sum(types == "aux")
  n_lookup <- sum(types == "lookup")
  total <- n_stocks + n_flows + n_constants + n_aux + n_lookup

  if (total == 0) {
    cli::cli_alert_info("Empty model without any variables.")
  } else {
    parts <- c()
    if (n_stocks > 0) parts <- c(parts, "{n_stocks} stock{?s}")
    if (n_flows > 0) parts <- c(parts, "{n_flows} flow{?s}")
    if (n_constants > 0) parts <- c(parts, "{n_constants} constant{?s}")
    if (n_aux > 0) parts <- c(parts, "{n_aux} {?auxiliary/auxiliaries}")
    if (n_lookup > 0) parts <- c(parts, "{n_lookup} lookup{?s}")
    cli::cli_text(paste(parts, collapse = " \u2022 "))
  }

  # Stock-flow structure
  if (n_stocks > 0 || n_flows > 0) {
    cli_h2_tight("Stock-Flow Structure")

    stock_rows <- vars[types == "stock", , drop = FALSE]
    all_flow_names <- vars[types == "flow", "name"]

    shown_flows <- character(0)

    for (i in seq_len(nrow(stock_rows))) {
      stock_name <- stock_rows[i, "name"]
      inflows <- unlist(stock_rows[i, "inflow"])
      outflows <- unlist(stock_rows[i, "outflow"])

      if (is.null(inflows)) inflows <- character(0)
      if (is.null(outflows)) outflows <- character(0)

      shown_flows <- c(shown_flows, inflows, outflows)

      parts <- c(
        if (length(inflows) > 0) paste0("+ ", inflows),
        if (length(outflows) > 0) paste0("- ", outflows)
      )

      if (length(parts) == 0) {
        cli::cli_text("  {stock_name}: (no flows)")
      } else {
        cli::cli_text("  {stock_name}: {paste(parts, collapse = ' ')}")
      }
    }

    # Show truly disconnected flows (not in any stock's inflow/outflow)
    disconnected <- setdiff(all_flow_names, shown_flows)
    if (length(disconnected) > 0) {
      cli::cli_text("  {.emph Unconnected flows}: {.code {disconnected}}")
    }
  }

  # Other variables
  has_others <- n_constants > 0 || n_aux > 0 || n_lookup > 0
  if (has_others) {
    cli_h2_tight("Other Variables")
    if (n_constants > 0) {
      const_names <- vars[types == "constant", "name"]
      cli::cli_text("  {.strong Constants}:   {.code {const_names}}")
    }
    if (n_aux > 0) {
      aux_names <- vars[types == "aux", "name"]
      cli::cli_text("  {.strong Auxiliaries}: {.code {aux_names}}")
    }
    if (n_lookup > 0) {
      lookup_names <- vars[types == "lookup", "name"]
      cli::cli_text("  {.strong Lookups}:     {.code {lookup_names}}")
    }
  }

  # Simulation settings
  cli_h2_tight("Simulation Settings")
  ss <- x[["sim_settings"]]
  time_unit <- ss[["time_units"]]

  if (!is.null(ss[["save_by"]])) {
    save_suffix <- paste0(", save_by = ", ss[["save_by"]])
  } else if (!is.null(ss[["save_times"]])) {
    save_suffix <- paste0(", save_times = ", length(ss[["save_times"]]), " time points")
  } else if (!is.null(ss[["save_length"]])) {
    save_suffix <- paste0(", save_length = ", ss[["save_length"]])
  } else {
    save_suffix <- ""
  }

  seed <- ss[["seed"]]
  if (!is.null(seed)) {
    seed_suffix <- paste0(" \u2022 seed = ", seed)
  } else {
    seed_suffix <- ""
  }

  cli::cli_text(
    "  Time: {ss$start} to {ss$stop} {time_unit} (dt = {ss$dt}{save_suffix}) \u2022 {ss$method} \u2022 {ss$language}{seed_suffix}"
  )

  output_vars <- ss[["vars"]]
  if (length(output_vars) > 0) {
    output_text <- paste(output_vars, collapse = ", ")
  } else if (isTRUE(ss[["only_stocks"]])) {
    output_text <- "stocks only"
  } else {
    output_text <- "all variables"
  }
  cli::cli_text("  Simulation output: {output_text}")

  # Unit tests (only shown when at least one is defined)
  n_ut <- length(x[["unit_tests"]])
  if (n_ut > 0L) {
    n_active <- sum(vapply(x[["unit_tests"]], function(t) isTRUE(t[["active"]]), logical(1)))
    cli::cli_text("  {n_ut} unit test{?s} defined ({n_active} active) \u2022 Run with {.fn verify}.")
  }

  invisible(x)
}
