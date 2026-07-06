# ==============================================================================
# Shared helper functions for R and Julia script assembly
# ==============================================================================
# These functions extract data from the unified data frame structure and
# are used by both R and Julia assembly paths via the language adapter pattern.


#' Invalidate assembly cache components
#'
#' The current cache policy is deliberately conservative: any invalidation clears
#' the whole base assembly cache. Component-level invalidation should only be
#' added back for profiled bottlenecks with component-specific hashes.
#'
#' @param object Stock-and-flow model
#' @param what Character vector of invalidation categories. Currently accepted
#'   for call-site readability only; all categories clear the whole cache.
#'
#' @returns A stock-and-flow model with an empty assembly cache
#' @noRd
invalidate_assemble <- function(object, what = "all") {
  object[["assemble"]] <- empty_assemble()
  object
}


#' Extract equations by variable type from data frame
#'
#' @param object Stock-and-flow model
#' @param type Variable type: "stock", "flow", "constant", "aux", "lookup"
#' @param column Column to extract: "eqn_str", "eqn", "sum_eqn", "sum_name"
#' @returns Named list where names are variable names and values are the extracted column
#' @noRd
get_equations_by_type <- function(object, type, column = "eqn_str") {
  df <- object[["variables"]][object[["variables"]][["type"]] == type, ]

  if (nrow(df) == 0) {
    return(stats::setNames(list(), character(0)))
  }

  stats::setNames(as.list(df[[column]]), df[["name"]])
}

#' Get all equation types for static equations
#'
#' @param object Stock-and-flow model
#' @returns List with gf_eqn, constant_eqn, stock_eqn
#' @noRd
get_static_equations <- function(object) {
  list(
    gf_eqn = get_equations_by_type(object, "lookup", "eqn_str"),
    constant_eqn = get_equations_by_type(object, "constant", "eqn_str"),
    stock_eqn = get_equations_by_type(object, "stock", "eqn_str")
  )
}

#' Get all equation types for dynamic equations
#'
#' @param object Stock-and-flow model
#' @returns List with aux_eqn, flow_eqn
#' @noRd
get_dynamic_equations <- function(object) {
  list(
    aux_eqn = get_equations_by_type(object, "aux", "eqn_str"),
    flow_eqn = get_equations_by_type(object, "flow", "eqn_str")
  )
}


#' Prepare equations and variables for simulation
#'
#' Unified function for both R and Julia. Uses lang_adapter() to dispatch
#' language-specific formatting.
#'
#' @inheritParams update.stockflow
#' @param modified_names Character vector of variable names that were modified.
#'   If NULL (default), all variables are processed. If provided, only the
#'   specified variables are updated for incremental performance.
#'
#' @returns A stock-and-flow model object of class [`stockflow`][stockflow]
#' @noRd
#'
prep_equations_variables <- function(object, modified_names = NULL) {
  language <- object[["sim_settings"]][["language"]]
  lang <- lang_adapter(language)
  keep_nonnegative_flow <- object[["sim_settings"]][["keep_nonnegative_flow"]]
  names_df <- get_names(object)

  # Determine which rows to process
  if (is.null(modified_names)) {
    process_indices <- seq_len(nrow(object[["variables"]]))
  } else {
    process_indices <- which(object[["variables"]][["name"]] %in% modified_names)
  }

  # Pre-convert equations for types that need it (Julia converts R->Julia syntax)
  eqn_converted <- character(nrow(object[["variables"]]))
  var_names <- get_model_var(object)

  # Translation cache: convert_eqn (R->Julia) is the dominant cost. Memoize it
  # per variable keyed on (eqn, type). The whole cache is reset when the set of
  # model variable names changes, because translation can depend on var_names
  # (which identifiers are variables vs. functions). Result: editing one
  # equation retranslates only that one; resimulating retranslates nothing.
  vn_hash <- rlang::hash(var_names)
  eqn_cache <- object[["assemble"]][["eqn_cache"]]
  if (is.null(eqn_cache) || !identical(eqn_cache[["vn_hash"]], vn_hash) ||
    !identical(eqn_cache[["language"]], language)) {
    eqn_cache <- list(vn_hash = vn_hash, language = language, by_name = list())
  }

  for (i in process_indices) {
    type_i <- object[["variables"]][i, "type"]
    if (type_i == "func") next
    if (type_i %in% c("stock", "flow", "constant", "aux")) {
      eqn <- object[["variables"]][i, "eqn"]
      nm <- object[["variables"]][i, "name"]

      if (any(grepl("^[ ]*function[ ]*\\(", eqn))) {
        cli::cli_abort(c(
          "x" = "Invalid {.eqn} argument.",
          "i" = "Model variables cannot be defined as functions.",
          ">" = "To add a custom function, use {.fn custom_func} instead."
        ))
      }

      cached <- eqn_cache[["by_name"]][[nm]]
      if (!is.null(cached) && identical(cached[["eqn"]], eqn) &&
        identical(cached[["type"]], type_i)) {
        eqn_converted[i] <- cached[["converted"]]
      } else {
        conv <- lang$convert_eqn(
          type = type_i, name = nm, eqn = eqn, var_names = var_names
        )
        eqn_converted[i] <- conv
        eqn_cache[["by_name"]][[nm]] <- list(
          eqn = eqn, type = type_i, converted = conv
        )
      }
    }
  }

  object[["assemble"]][["eqn_cache"]] <- eqn_cache

  # Replace bare gf name references with gf(source) in converted equations
  gf_dict <- build_gf_source_dict(object)
  if (!is.null(gf_dict)) {
    # Match whole-word gf_name NOT already followed by (
    regex_dict <- stats::setNames(
      unname(gf_dict),
      paste0("\\b", stringr::str_escape(names(gf_dict)), "\\b(?!\\s*\\()")
    )
    for (i in process_indices) {
      if (nzchar(eqn_converted[i])) {
        eqn_converted[i] <- stringr::str_replace_all(eqn_converted[i], regex_dict)
      }
    }
  }

  # Process graphical functions
  gf_idx <- object[["variables"]][["type"]] == "lookup"
  for (i in intersect(which(gf_idx), process_indices)) {
    row <- object[["variables"]][i, ]
    result <- lang$format_lookup(row, names_df)
    if (!is.null(result)) {
      object[["variables"]][i, "eqn_str"] <- result
    }
  }

  # Constants and stock initial values (same formatting pattern)
  for (type in c("constant", "stock")) {
    type_idx <- object[["variables"]][["type"]] == type
    for (i in intersect(which(type_idx), process_indices)) {
      row <- object[["variables"]][i, ]
      object[["variables"]][i, "eqn_str"] <- lang$format_static(
        name = row[["name"]],
        eqn_converted = eqn_converted[i],
        row = row
      )
    }
  }

  # Auxiliary equations
  aux_idx <- object[["variables"]][["type"]] == "aux"
  for (i in intersect(which(aux_idx), process_indices)) {
    row <- object[["variables"]][i, ]
    eqn_str <- lang$format_aux(
      name = row[["name"]],
      eqn_converted = eqn_converted[i],
      row = row
    )

    if (lang$eqn_str_as_list) {
      if (!is.null(row[["preceding_eqn"]])) {
        eqn_str <- c(row[["preceding_eqn"]], eqn_str)
      }
      object[["variables"]][i, "eqn_str"] <- list(eqn_str)
    } else {
      object[["variables"]][i, "eqn_str"] <- eqn_str
    }
  }

  # Flow equations
  flow_idx <- object[["variables"]][["type"]] == "flow"
  for (i in intersect(which(flow_idx), process_indices)) {
    row <- object[["variables"]][i, ]
    eqn_str <- lang$format_flow(
      name = row[["name"]],
      eqn_converted = eqn_converted[i],
      row = row,
      keep_nonnegative_flow = keep_nonnegative_flow
    )

    if (lang$eqn_str_as_list) {
      if (!is.null(row[["preceding_eqn"]])) {
        eqn_str <- c(row[["preceding_eqn"]], eqn_str)
      }
      object[["variables"]][i, "eqn_str"] <- list(eqn_str)
    } else {
      object[["variables"]][i, "eqn_str"] <- eqn_str
    }
  }

  object
}


#' Prepare for summing change in stocks
#'
#' Unified function for both R and Julia. Uses lang_adapter() to dispatch
#' language-specific formatting of stock change names and equations.
#'
#' @inheritParams update.stockflow
#' @param modified_names Character vector of variable names that were modified.
#'   If `NULL` (default), all stocks are processed. If provided, only the
#'   specified stocks are updated for incremental performance.
#'
#' @returns A stock-and-flow model object of class [`stockflow`][stockflow]
#' @noRd
#'
prep_stock_change <- function(object, modified_names = NULL) {
  language <- object[["sim_settings"]][["language"]]
  lang <- lang_adapter(language)

  stock_idx <- object[["variables"]][["type"]] == "stock"

  # Determine which stock rows to process
  if (is.null(modified_names)) {
    process_stock_indices <- which(stock_idx)
  } else {
    modified_stock_indices <- which(stock_idx & object[["variables"]][["name"]] %in% modified_names)

    # Find stocks affected by modified flows
    flow_idx <- object[["variables"]][["type"]] == "flow"
    modified_flows <- object[["variables"]][["name"]] %in% modified_names & flow_idx
    if (any(modified_flows)) {
      affected_to <- object[["variables"]][modified_flows, "to"]
      affected_from <- object[["variables"]][modified_flows, "from"]
      affected_stocks <- unique(c(
        affected_to[nzchar(affected_to)],
        affected_from[nzchar(affected_from)]
      ))
      affected_stock_indices <- which(stock_idx & object[["variables"]][["name"]] %in% affected_stocks)
      process_stock_indices <- unique(c(modified_stock_indices, affected_stock_indices))
    } else {
      process_stock_indices <- modified_stock_indices
    }
  }

  # Populate inflows and outflows for each stock
  if (length(process_stock_indices) > 0) {
    flow_df <- get_flow_df(object)
    for (i in process_stock_indices) {
      stock_name <- object[["variables"]][i, "name"]
      inflows <- flow_df[flow_df[["to"]] == stock_name, "name"]
      outflows <- flow_df[flow_df[["from"]] == stock_name, "name"]

      object[["variables"]]$inflow[[i]] <- inflows
      object[["variables"]]$outflow[[i]] <- outflows
    }
  }

  # Get stock names for position lookup (Julia uses positional indexing)
  stock_names <- object[["variables"]][stock_idx, "name"]

  for (i in process_stock_indices) {
    row <- object[["variables"]][i, ]
    stock_position <- which(stock_names == row[["name"]])

    object[["variables"]][i, "sum_name"] <- lang$format_sum_name(row, stock_position, stock_names)

    # Build sum equation from inflows and outflows
    inflow_def <- object[["variables"]][i, "inflow"]
    outflow_def <- object[["variables"]][i, "outflow"]

    # Extract from list columns
    if (is.list(inflow_def)) {
      inflow_def <- inflow_def[[1]]
      if (is.null(inflow_def)) inflow_def <- character(0)
    }
    if (is.list(outflow_def)) {
      outflow_def <- outflow_def[[1]]
      if (is.null(outflow_def)) outflow_def <- character(0)
    }

    if (!is_defined(inflow_def) && !is_defined(outflow_def)) {
      inflow_def <- lang$zero
    }
    # } else {
    inflow <- outflow <- ""
    if (is_defined(inflow_def)) {
      inflow <- paste0(inflow_def, collapse = " + ")
    }
    if (is_defined(outflow_def)) {
      outflow <- paste0(paste0(" - ", outflow_def), collapse = "")
    }
    object[["variables"]][i, "sum_eqn"] <- paste0(inflow, outflow)
  }

  object
}


# ==============================================================================
# Shared compile helpers: equation extraction and ordering
# ==============================================================================

#' Extract and order static equations into a single string
#'
#' Used by both R and Julia branches of compile_static().
#' Extracts gf, constant, and stock equations, then orders them
#' according to the dependency ordering.
#'
#' @param object Stock-and-flow model
#' @param ordering Ordering from order_equations()
#' @param separator String to join equations with
#'
#' @returns List with `str` (ordered equation string), `gf_eqn`, `constant_eqn`, `stock_eqn`
#' @noRd
gather_static_equations <- function(object, ordering, separator = "\n") {
  gf_eqn <- get_equations_by_type(object, "lookup", "eqn_str")
  constant_eqn <- get_equations_by_type(object, "constant", "eqn_str")
  stock_eqn <- get_equations_by_type(object, "stock", "eqn_str")

  if (!ordering[["static_and_dynamic"]][["issue"]]) {
    static_str <- c(gf_eqn, constant_eqn, stock_eqn)[ordering[["static"]][["order"]]] |>
      unlist() |>
      paste0(collapse = separator)
  } else {
    aux_eqn <- get_equations_by_type(object, "aux", "eqn_str")
    flow_eqn <- get_equations_by_type(object, "flow", "eqn_str")

    static_str <- c(
      gf_eqn, constant_eqn, stock_eqn,
      aux_eqn, flow_eqn
    )[ordering[["static_and_dynamic"]][["order"]]] |>
      unlist() |>
      paste0(collapse = separator)
  }

  list(
    str = static_str,
    constant_eqn = constant_eqn,
    gf_eqn = gf_eqn,
    stock_eqn = stock_eqn
  )
}


#' Extract and order dynamic equations into a string
#'
#' Used by both R and Julia branches of compile_ode().
#'
#' @param object Stock-and-flow model
#' @param ordering Ordering from order_equations()
#' @param separator String to join equations with
#'
#' @returns List with `str` (ordered equation string), `eqns` (named vector of ordered equations)
#' @noRd
gather_dynamic_equations <- function(object, ordering, separator = "\n\t\t") {
  aux_eqn <- get_equations_by_type(object, "aux", "eqn_str")
  flow_eqn <- get_equations_by_type(object, "flow", "eqn_str")

  eqns <- unlist(c(aux_eqn, flow_eqn)[ordering[["dynamic"]][["order"]]])
  str <- paste0(eqns, collapse = separator)

  list(str = str, eqns = eqns)
}


#' Join complete simulation script sections
#'
#' @param sections Character vector of script sections, possibly including NULL
#'   values for language-specific sections.
#'
#' @returns A single script string.
#' @noRd
join_script_sections <- function(sections) {
  sections <- unlist(sections, use.names = FALSE)
  paste0(sections, collapse = "\n")
}


#' Compile final non-ensemble script from prepared sections
#'
#' The base assembly cache owns language-neutral section order. Runtime-specific
#' callers provide the ODE, solve, and post-processing sections.
#'
#' @inheritParams update.stockflow
#' @param ode Compiled ODE section.
#' @param run_ode Compiled solve section.
#' @param post Compiled post-processing section.
#'
#' @returns A complete script string.
#' @noRd
compile_script_sections <- function(object, ode, run_ode, post) {
  language <- object[["sim_settings"]][["language"]]
  assemble <- object[["assemble"]]

  join_script_sections(c(
    if (language == "R") "# Load packages\nlibrary(sdbuildR)" else NULL,
    assemble[["times"]],
    assemble[["funcs"]],
    if (language == "R") assemble[["nonneg_stocks"]][["func_def"]] else NULL,
    ode,
    assemble[["static"]][["script"]],
    run_ode,
    post
  ))
}


#' Normalize save schedule settings
#'
#' @param ss Simulation settings list.
#'
#' @returns List with save schedule type and value.
#' @noRd
save_schedule <- function(ss) {
  # Exactly one of save_by / save_times / save_length is non-NULL, or none (save
  # every dt step). Derive the discriminating type from whichever field is set.
  if (!is.null(ss[["save_by"]])) {
    list(type = "by", value = ss[["save_by"]])
  } else if (!is.null(ss[["save_times"]])) {
    list(type = "times", value = ss[["save_times"]])
  } else if (!is.null(ss[["save_length"]])) {
    list(type = "length", value = ss[["save_length"]])
  } else {
    list(type = "all", value = NULL)
  }
}


#' Build Julia saveat expression from simulation settings
#'
#' @inheritParams save_schedule
#'
#' @returns Julia expression used to define saveat.
#' @noRd
julia_saveat_expr <- function(ss) {
  schedule <- save_schedule(ss)

  switch(schedule[["type"]],
    "all" = P[["tstops_name"]],
    "by" = sprintf(
      "%s[1]:%s:%s[2]",
      P[["times_name"]], schedule[["value"]], P[["times_name"]]
    ),
    "times" = paste0("[", paste(schedule[["value"]], collapse = ", "), "]"),
    "length" = {
      if (as.integer(schedule[["value"]]) == 1L) {
        sprintf("[%s[2]]", P[["times_name"]])
      } else {
        sprintf(
          "range(%s[1], %s[2], length=%s)",
          P[["times_name"]], P[["times_name"]], schedule[["value"]]
        )
      }
    }
  )
}


#' Build R saveat post-processing script from simulation settings
#'
#' @inheritParams save_schedule
#'
#' @returns R script that filters/interpolates solver output times.
#' @noRd
r_saveat_script <- function(ss) {
  schedule <- save_schedule(ss)

  switch(schedule[["type"]],
    "all" = "",
    "by" = fmt_script("saveat_interval", "R", ss, save_by_val = schedule[["value"]]),
    "times" = fmt_script("saveat_explicit", "R", ss,
      save_times_str = paste(schedule[["value"]], collapse = ", ")
    ),
    "length" = if (as.integer(schedule[["value"]]) == 1L) {
      fmt_script("saveat_length1", "R", ss)
    } else {
      fmt_script("saveat_length", "R", ss, save_length_val = schedule[["value"]])
    }
  )
}


#' Build Julia output-selection arguments for clean_df()
#'
#' @inheritParams update.stockflow
#' @param vars Character vector of requested output variables, or NULL.
#'
#' @returns List of Julia code snippets for selected stocks, intermediaries, and
#'   final output variable filtering.
#' @noRd
julia_output_selection_args <- function(object, vars = NULL) {
  if (is.null(vars)) {
    return(list(
      save_idx_arg = "nothing",
      intermediary_names_arg = paste0(P[["model_setup_name"]], ".", P[["intermediary_names"]]),
      selected_var_names_arg = "nothing"
    ))
  }

  vars <- validate_sim_vars(object, vars)
  stock_names <- get_variables_by_type(object, "stock")[["name"]]
  intermediaries <- object[["assemble"]][["intermediaries"]]

  selected_stock_names <- vars[vars %in% stock_names]
  save_idx <- which(stock_names %in% selected_stock_names)
  save_idx_arg <- if (length(save_idx) > 0) {
    paste0("[", paste0(save_idx, collapse = ", "), "]")
  } else {
    "Int[]"
  }

  selected_inter_names <- vars[vars %in% intermediaries[["names"]]]
  intermediary_names_arg <- if (length(selected_inter_names) > 0) {
    paste0("[:", paste0(selected_inter_names, collapse = ", :"), "]")
  } else {
    "Symbol[]"
  }

  list(
    save_idx_arg = save_idx_arg,
    intermediary_names_arg = intermediary_names_arg,
    selected_var_names_arg = paste0("[:", paste0(vars, collapse = ", :"), "]")
  )
}


#' Build gf name to gf(source) mapping
#'
#' For each graphical function that has a source, builds a named character
#' vector mapping the bare gf name to `gf_name(source)`. Handles recursive
#' resolution when one gf's source is another gf.
#'
#' @param object Stock-and-flow model
#' @returns Named character vector (names = gf name, values = gf(source)),
#'   or NULL if no gf with sources exist.
#' @noRd
build_gf_source_dict <- function(object) {
  gf_df <- object[["variables"]][object[["variables"]][["type"]] == "lookup", ]
  if (nrow(gf_df) == 0) {
    return(NULL)
  }

  gf_sources <- stats::setNames(gf_df[["source"]], gf_df[["name"]])
  gf_sources <- gf_sources[!is.na(gf_sources) & nzchar(gf_sources)]
  if (length(gf_sources) == 0) {
    return(NULL)
  }

  # Base mapping: gf_name -> gf_name(source)
  dict <- paste0(names(gf_sources), "(", unname(gf_sources), ")") |>
    stats::setNames(names(gf_sources))

  # Recursively resolve nested gf sources (gf1 source is gf2)
  dict2 <- paste0("(", names(gf_sources), "(", unname(gf_sources), "))") |>
    stats::setNames(paste0("\\(", stringr::str_escape(names(gf_sources)), "\\)"))
  dict <- stringr::str_replace_all(unname(dict), dict2) |>
    stats::setNames(names(dict))

  dict
}


#' Build graphical function return string for R ODE
#'
#' Creates the string that includes graphical function calls in the ODE
#' return statement, using the shared gf source dictionary.
#'
#' @param object Stock-and-flow model
#' @returns Character string to append to R ODE return (empty if no gf)
#' @noRd
build_gf_return_str <- function(object) {
  gf_dict <- build_gf_source_dict(object)
  if (is.null(gf_dict)) {
    return("")
  }

  paste0(
    paste0("'", unname(gf_dict), "' = "), unname(gf_dict),
    collapse = ", "
  )
}


#' Build stock change equations string
#'
#' Builds the "sum_name = sum_eqn" strings for each stock.
#' Used by both R and Julia branches of compile_ode().
#'
#' @param object Stock-and-flow model
#' @param assign_op Assignment operator ("=" or "<-")
#'
#' @returns Named character vector of stock change equations
#' @noRd
gather_stock_changes <- function(object, assign_op, language) {
  stock_vars <- get_variables_by_type(object, "stock")

  if (language == "R") {
    paste0(stock_vars[["sum_name"]], " ", assign_op, " ", stock_vars[["sum_eqn"]])
  } else if (language == "Julia") {
    stock_change <- lapply(
      stock_vars[["name"]],
      function(stock_name) {
        stock_row <- object[["variables"]][object[["variables"]][["name"]] == stock_name, ]
        x <- as.list(stock_row)
        sum_expr <- x[["sum_eqn"]]
        paste(
          x[["sum_name"]],
          "=",
          sum_expr
        )
      }
    ) |> compact_()

    # Set names on stock_change
    names(stock_change) <- stock_vars[["name"]]
    stock_change
  }
}


#' Pre-assemble script components for later use/modification
#'
#' Populates object$assemble with all script components so they can be
#' inspected and modified before simulation. This allows users to change
#' things like stop time, dt, etc. without recompiling from scratch.
#'
#' Called by compile() and compile_ensemble() to ensure the base cache is
#' populated before runtime-specific script generation begins.
#'
#' @param object A stock-and-flow model object of class [`stockflow`][stockflow]
#'
#' @returns A stock-and-flow model object with populated object$assemble cache
#' @noRd
#'
pre_assemble_components <- function(object) {
  # Skip if no variables defined yet
  if (!is_defined(object[["variables"]]) || nrow(object[["variables"]]) == 0) {
    return(object)
  }

  object[["assemble"]] <- complete_assemble(object[["assemble"]])

  language <- object[["sim_settings"]][["language"]]

  # Content-hash short-circuit. input_hash is set only at the END of a
  # successful assembly, so a match here means the base cache is already built
  # for these exact inputs -> return immediately. This makes a repeated
  # compile()/simulate() with no intervening edits essentially free.
  current_hash <- compute_model_hash(object)
  if (!is.null(object[["assemble"]][["input_hash"]]) &&
    identical(object[["assemble"]][["input_hash"]], current_hash) &&
    identical(object[["assemble"]][["language"]], language)) {
    return(object)
  }

  # --- Julia-specific validation ---------------------------------------------
  if (language == "Julia") {
    var_names <- get_model_var(object)
    check_no_keyword_arg(object, var_names)
  }

  # Reaching here means a full base-cache rebuild is required.
  object[["assemble"]][["language"]] <- language
  object[["assemble"]]["input_hash"] <- list(NULL) # cleared until assembly completes

  # --- Ordering --------------------------------------------------------------
  object[["assemble"]][["ordering"]] <- order_equations(object)

  # --- Compile times ---------------------------------------------------------
  object[["assemble"]][["times"]] <- compile_times(object, language = language)

  # --- Compile funcs ---------------------------------------------------------
  object[["assemble"]][["funcs"]] <- compile_funcs(object, language = language)

  # --- Prepare equations and stock changes -----------------------------------
  object <- prep_equations_variables(object)
  object <- prep_stock_change(object)

  # --- Julia: intermediaries -------------------------------------------------
  if (language == "Julia") {
    object[["assemble"]][["intermediaries"]] <- prep_intermediary_variables(object, language = language)
  }

  # --- Compile static equations ----------------------------------------------
  object[["assemble"]][["static"]] <- compile_static(object, language = language)


  # --- R: compile nonneg stocks ----------------------------------------------
  if (language == "R") {
    object[["assemble"]][["nonneg_stocks"]] <- compile_nonneg_stocks(object, language = language)
  }

  # --- Julia: compile ODE (Julia ODE does not depend on only_stocks) ---------
  if (language == "Julia") {
    object[["assemble"]][["ode"]] <- compile_ode(object,
      only_stocks = FALSE,
      language = language,
      is_ensemble = FALSE
    )
  }

  # --- Populate validation caches --------------------------------------------
  # Always recomputed on the rebuild path (inputs changed), so diagnostics never
  # go stale relative to the model.
  object[["assemble"]][["summary"]] <- summary(object)

  # --- Structural invariant check (cheap; guards codegen correctness) ---------
  validate_layout(object)

  # Mark the cache as fully assembled for these inputs (enables the early-return
  # short-circuit on the next call with no edits).
  object[["assemble"]][["input_hash"]] <- current_hash

  object
}


#' Eagerly pre-assemble unless codegen is deferred
#'
#' Mutators (`update()`, `sim_settings()`) call this so the assembly cache is
#' ready immediately. Because the cache is now hash-gated, the codegen can be
#' safely deferred to the next codegen consumer (`compile()`/`simulate()`):
#' set `SDBUILDR_DEFER_CODEGEN=true` to skip the eager pass and only assemble
#' on demand. The default preserves the original eager behaviour.
#'
#' @inheritParams update.stockflow
#' @returns The model, with components pre-assembled unless deferral is enabled.
#' @noRd
maybe_pre_assemble <- function(object) {
  if (identical(tolower(Sys.getenv("SDBUILDR_DEFER_CODEGEN", unset = "false")), "true")) {
    return(object)
  }
  pre_assemble_components(object)
}
