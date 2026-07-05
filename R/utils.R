#' Normalize a file path with forward slashes
#'
#' Cross-platform tidy path: makes the path absolute and uses forward slashes on
#' all platforms (Windows accepts these everywhere), avoiding mixed separators in
#' constructed paths and user-facing messages. Does not require the path to exist.
#'
#' @param path Character vector of file paths.
#' @returns Normalized path(s) with forward slashes.
#' @noRd
norm_path <- function(path) {
  normalizePath(path, winslash = "/", mustWork = FALSE)
}


#' Remove files if they exist
#'
#' Deletes each path that exists, ignoring those that do not. Used in `on.exit()`
#' handlers to clean up temporary scripts and output files. `paths` may be a nested
#' list (e.g. the Julia ensemble filepath structure); it is flattened first.
#'
#' @param paths Character vector or (nested) list of file paths.
#' @returns Invisibly `NULL`.
#' @noRd
#'
remove_files <- function(paths) {
  for (path in unlist(paths, use.names = FALSE)) {
    if (file.exists(path)) {
      file.remove(path)
    }
  }
  invisible()
}


#' Read a simulation-output CSV written by the Julia backend
#'
#' Thin wrapper around [data.table::fread()] used to read the data, init,
#' constants and summary files produced by Julia simulations and ensembles, with
#' consistent NA handling, returned as a plain data.frame.
#'
#' @param path File path to read.
#' @returns A data.frame.
#' @noRd
#'
read_sim_csv <- function(path) {
  as.data.frame(data.table::fread(path, na.strings = c("", "NA")))
}

#' Normalize user-supplied keywords against a synonym table
#'
#' Lowercases and trims each element, then maps recognised synonyms to their
#' canonical spelling so users can pass lenient variants like "medians" or
#' "Means" wherever a fixed set of keywords is expected. Unrecognised values are
#' returned lowercased/trimmed so the caller's own validation reports them.
#'
#' @param x Character vector of user input.
#' @param synonyms Named list mapping each canonical value to a character vector
#'   of accepted alternative spellings (the canonical value is always accepted).
#' @returns Character vector the same length as `x`, canonicalised where possible.
#' @noRd
normalize_synonyms <- function(x, synonyms) {
  x <- trimws(tolower(as.character(x)))
  # Flat lookup mapping every accepted spelling to its canonical value.
  lookup <- unlist(lapply(names(synonyms), function(canon) {
    spellings <- unique(c(canon, synonyms[[canon]]))
    stats::setNames(rep(canon, length(spellings)), spellings)
  }))
  hit <- x %in% names(lookup)
  x[hit] <- unname(lookup[x[hit]])
  x
}

#' Synonym tables for lenient keyword matching
#'
#' Each entry maps a canonical keyword to accepted alternative spellings
#' (typically plurals and common abbreviations). Used by normalize_synonyms().
#' @noRd
central_synonyms <- function() {
  list(
  mean = c("means", "average", "averages", "avg"),
  median = c("medians", "med", "meds"),
  none = c("no", "off", "false")
)
}

#' @noRd
spread_synonyms <- function() {
  list(
    quantile = c("quantiles", "quant", "quants", "ci"),
    sd = c("sds", "std", "stds", "stdev", "stdevs"),
    range = c("ranges", "minmax", "min_max", "min-max"),
    none = c("no", "off", "false")
  )
}

#' @noRd
.clean_which <- function(which) {
  which <- trimws(tolower(which))
  switch(which,
    sims = ,
    sim = "sims",
    summary = ,
    summ = ,
    sum = "summary",
    cli::cli_abort(c(
      "Invalid {.arg which} value.",
      "x" = "Must be {.code 'summary'} or {.code 'sims'}, not {.val {which}}."
    ))
  )
}

#' @noRd
.clean_which_verify <- function(which) {
  which <- trimws(tolower(which))
  switch(which,
    tests = ,
    test = "tests",
    sims = ,
    sim = "sims",
    cli::cli_abort(c(
      "Invalid {.arg which} value.",
      "x" = "Must be {.code 'tests'} or {.code 'sims'}, not {.val {which}}."
    ))
  )
}

#' Set column names
#' @noRd
set_colnames <- `colnames<-`

#' Set row names
#' @noRd
set_rownames <- `rownames<-`


missing_arg <- function(arg_name) {
  cli::cli_abort(
    "Missing required argument {.arg {arg_name}}."
  )
}


#' Check if user has internet
#'
#' Internal function
#'
#' @returns Logical value
#'
#' @export
#' @concept internal
#' @examples
#' has_internet()
#'
has_internet <- function() {
  suppressWarnings(
    tryCatch(
      {
        con <- url("https://www.r-project.org", open = "rb")
        on.exit(close(con), add = TRUE)
        readBin(con, what = "raw", n = 1L)
        TRUE
      },
      error = function(e) FALSE
    )
  )
}


#' Bind rows
#'
#' @param ... (List of) data frames
#' @param .id ID column
#'
#' @returns Data frame
#' @noRd
bind_rows_ <- function(..., .id = NULL) {
  dfs <- list(...)
  if (length(dfs) == 1 && is.list(dfs[[1]]) && !is.data.frame(dfs[[1]])) {
    dfs <- dfs[[1]]
  }
  # Convert matrices and named vectors to data.frames
  dfs <- lapply(dfs, function(x) {
    if (is.matrix(x)) {
      as.data.frame(x)
    } else if (is.atomic(x) && !is.null(names(x))) {
      # Convert named vector to one-row data frame
      as.data.frame(as.list(x), stringsAsFactors = FALSE)
    } else {
      x
    }
  })
  # Silence warning about empty columns
  result <- withCallingHandlers(
    data.table::rbindlist(dfs,
      fill = TRUE, use.names = TRUE,
      ignore.attr = TRUE, idcol = .id
    ),
    warning = function(w) {
      if (grepl("filled with NA", w$message, fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
  as.data.frame(result)
}


#' Return last value
#'
#' Replacement for dplyr::last; convenience function
#'
#' @param x Vector
#' @param n Return last n values
#' @param default Default value to return
#'
#' @returns Last n values
#' @noRd
last <- function(x, n = 1L, default = NULL) {
  len <- length(x)

  if (len == 0L) {
    return(default)
  }

  if (n == 1L) {
    return(x[len])
  }

  if (n > len) {
    return(x)
  }

  x[(len - n + 1L):len]
}


#' Evaluate if x and y are the same within tolerance
#'
#' Replacement of dplyr::near; convenience function
#'
#' @param x First value
#' @param y Second value
#' @param tol Tolerance
#'
#' @returns Logical value
#' @noRd
near <- function(x, y, tol = .Machine$double.eps^0.5) {
  abs(x - y) < tol
}


#' Print a cli h2 header without a blank line below it
#'
#' @param text Header text, may contain glue/cli interpolation
#' @param .envir Environment for interpolation
#' @noRd
cli_h2_tight <- function(text, .envir = parent.frame()) {
  cli::cli_div(theme = list(h2 = list("margin-bottom" = 0)))
  cli::cli_h2(text, .envir = .envir)
  cli::cli_end()
}


#' Switch names and values of list, handling different lengths in entries
#'
#' @param x List
#' @returns List
#' @noRd
switch_list <- function(x) {
  # Switch names and values
  new_list <- unlist(lapply(names(x), function(name) {
    stats::setNames(rep(name, length(x[[name]])), x[[name]])
  }), recursive = FALSE)

  return(as.list(new_list))
}


#' Near equivalent of purrr::flatten()
#'
#' @param x List
#'
#' @returns List with one level removed
#' @noRd
flatten <- function(x) {
  result <- list()
  for (i in seq_along(x)) {
    elem <- x[[i]]
    outer_name <- names(x)[i]

    # Convert to list
    if (is.list(elem)) {
      elem_list <- elem
    } else {
      elem_list <- list(elem)
      # If element wasn't already a list and has an outer name, use it
      if (!is.null(outer_name) && outer_name != "") {
        names(elem_list) <- outer_name
      }
    }

    result <- c(result, elem_list)
  }
  result
}

#' Near equivalent of purrr::transpose()
#'
#' @param x List
#'
#' @returns Transposed list
#' @noRd
transpose_ <- function(x) {
  # Check if all elements are atomic vectors of the same length
  all_atomic <- all(vapply(x, is.atomic, logical(1)))
  lengths_vec <- lengths(x)
  all_same_length <- length(unique(lengths_vec)) == 1 && lengths_vec[1] > 0

  # Check if elements have names (like test4)
  elements_have_names <- any(vapply(x, function(e) !is.null(names(e)), logical(1)))

  if (all_atomic && all_same_length && !elements_have_names) {
    # Case: list of equal-length unnamed vectors - transpose like a matrix
    n <- lengths_vec[1]
    outer_names <- names(x)

    result <- lapply(seq_len(n), function(i) {
      values <- lapply(x, function(vec) unname(vec[i]))
      if (!is.null(outer_names)) {
        names(values) <- outer_names
      }
      values
    })

    return(result)
  }

  # Otherwise: standard transpose (swap inner/outer structure)
  inner_names <- unique(unlist(lapply(x, names)))
  outer_names <- names(x)

  result <- lapply(inner_names, function(nm) {
    values <- lapply(seq_along(x), function(i) {
      elem <- x[[i]]
      if (is.list(elem)) {
        elem[[nm]]
      } else {
        if (nm %in% names(elem)) unname(elem[[nm]]) else NULL
      }
    })

    if (!is.null(outer_names)) {
      names(values) <- outer_names
    }

    values
  })

  names(result) <- inner_names
  result
}


#' Near equivalent of purrr::compact()
#'
#' @param x List
#'
#' @returns List with NULL values removed
#' @noRd
compact_ <- function(x) {
  result <- Filter(Negate(function(x) length(x) == 0), Filter(Negate(is.null), x))
  if (length(result) == 0 & inherits(x, "list")) {
    list()
  } else {
    result
  }
}


#' Safely check whether value is defined
#'
#' @param x Value
#'
#' @returns Boolean; whether the value is defined
#' @noRd
is_defined <- function(x) {
  # Safely check whether x is defined
  if (length(x) == 0) {
    return(FALSE)
  } else {
    # if (any(is.na(x))) {
    if (all(is.na(x))) {
      return(FALSE)
    } else {
      return(any(nzchar(x)))
    }
  }
}


#' Extract entries from a nested list
#'
#' @param nested_list List to extract from
#' @param entry Name of entry to extract
#' @param keep_entry_name If TRUE, keep upper level name.
#'
#' @returns List with extracted entries
#' @noRd
list_extract <- function(nested_list, entry, keep_entry_name = FALSE) {
  traverse <- function(x) {
    if (!is.list(x)) {
      return(list())
    }

    out <- list()
    for (nm in names(x)) {
      if (nm == entry) {
        val <- x[[nm]]
        if (keep_entry_name) {
          out <- c(out, stats::setNames(list(val), nm))
        } else {
          # If value is a list (and not a data.frame), append its elements;
          # otherwise append the value as a single list element.
          if (is.list(val) && !inherits(val, "data.frame")) {
            out <- c(out, val)
          } else {
            out <- c(out, list(val))
          }
        }
      } else {
        out <- c(out, traverse(x[[nm]]))
      }
    }
    out
  }

  traverse(nested_list)
}


#' Apply purrr::map() and unlist() whilst preserving NULL
#'
#' @param x List
#' @param element_name String, desired name of list
#' @param change_null_to String, what to change NULL to
#'
#' @returns Vector
#' @noRd
#'
get_map <- function(x, element_name, change_null_to = "") {
  if (length(x) == 0) {
    return(c())
  }

  x_list <- lapply(x, `[[`, element_name)
  # Unlist preserving NULL
  x_list[vapply(x_list, function(x) {
    is.null(x) | length(x) == 0
  }, logical(1))] <- change_null_to
  return(unlist(x_list))
}


str_wrap_ <- function(str, width) {
  str_w <- stringi::stri_wrap(str,
    width = width, indent = 0,
    exdent = 0, whitespace_only = TRUE, simplify = FALSE
  )

  out <- vapply(str_w, stringi::stri_c, collapse = "\n", character(1))
  out
}

title_case_ascii <- function(value) {
  value <- as.character(value)
  out <- value
  idx <- !is.na(value) & nzchar(value)
  out[idx] <- paste0(toupper(substr(value[idx], 1, 1)), substr(value[idx], 2, nchar(value[idx], type = "chars")))
  out
}

#' Ensure length of arg is same as target
#'
#' @param arg Vector
#' @param target Target object to match length of
#' @param arg_name Name of arg (for error messages)
#' @param target_name Name of target (for error messages)
#'
#' @returns arg with same length as target
#' @noRd
#'
ensure_length <- function(arg, target, arg_name = NULL, target_name = NULL) {
  if (length(arg) != 1 && length(arg) != length(target)) {
    if (is.null(arg_name)) {
      arg_name <- rlang::caller_arg(arg)
    }
    if (is.null(target_name)) {
      target_name <- rlang::caller_arg(target)
    }
    arg_val <- paste0(arg, collapse = ", ")
    target_val <- paste0(target, collapse = ", ")

    cli::cli_abort(c(
      "x" = "Invalid length of {.arg {arg_name}}.",
      "i" = "Received length {.val {length(arg)}} for {.arg {arg_name}} and length {.val {length(target)}} for {.arg {target_name}}.",
      ">" = "Length must be either 1 or equal to the length of {.arg {target_name}}."
    ))
  } else if (length(arg) < length(target)) {
    arg <- rep(arg, length.out = length(target)) # Repeat to match the target length
  }
  arg
}


#' Get exported function names from a package
#'
#' @param package package name
#'
#' @returns Vector with names of exported functions
#' @noRd
get_exported_functions <- function(package) {
  # Load the package namespace (does not attach to search path)
  ns <- getNamespace(package)

  # Get all exported objects
  exports <- getNamespaceExports(package)

  # Filter for functions
  functions <- exports[vapply(exports, function(x) {
    is.function(get(x, envir = ns))
  }, logical(1))]

  # Return sorted for consistency
  sort(functions)
}


#' Helper function to clean coding language
#'
#' @param language Language
#'
#' @returns Cleaned language
#' @noRd
#'
clean_language <- function(language) {
  language <- trimws(tolower(language))
  if (!language %in% c("r", "julia", "jl")) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg language} value.",
      "i" = "Received {.val {language}}.",
      ">" = "Use {.code 'Julia'} or {.code 'R'}."
    ))
  } else {
    language <- title_case_ascii(language)
    language <- ifelse(language == "Jl", "Julia", language)
  }

  language
}


#' Clean variable type
#'
#' @inheritParams update.stockflow
#'
#' @returns Cleaned string or vector
#' @noRd
clean_type <- function(type) {
  if (!(all(is.character(type)))) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg type} argument.",
      "i" = "Must be {.cls character}."
    ))
  }

  type <- Filter(nzchar, trimws(tolower(type)))

  # Allow for use of auxiliary instead of aux
  type[type == "auxiliary" | type == "auxiliaries"] <- "aux"

  # Remove trailing s if present
  type <- gsub("s$", "", type)

  # Allow "function" as alias for "func"
  type[type == "function"] <- "func"
  type[type == "custom_func"] <- "func"
  type[type == "custom func"] <- "func"
  type[type == "custom function"] <- "func"

  # Allow "gf" and "graphical function" as aliases for "lookup"
  type[type == "gf" | type == "graphical function"] <- "lookup"

  type
}


#' Clean and normalise a test status vector
#'
#' @param status Character vector supplied by the user.
#' @returns Character vector with canonical values: `"pass"`, `"fail"`, `"error"`, `"skip"`.
#' @noRd
clean_status <- function(status) {
  if (!is.character(status) || length(status) == 0L) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg status} argument.",
      "i" = "Must be a non-empty {.cls character} vector."
    ))
  }

  s <- trimws(tolower(status))

  s[s == "passed"] <- "pass"
  s[s == "failed"] <- "fail"
  s[s == "errors" | s == "errored"] <- "error"
  s[s == "skipped"] <- "skip"

  canonical <- c("pass", "fail", "error", "skip")
  bad <- s[!s %in% canonical]
  if (length(bad) > 0L) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg status} value{?s}: {.val {bad}}.",
      ">" = "Must be one or more of {.val {canonical}}."
    ))
  }
  s
}

#' Clean a vars argument
#'
#' Trims whitespace, removes blank strings, and deduplicates a character vector
#' of variable names. Does not validate names against any model.
#'
#' @param vars Character vector of variable names supplied by the user.
#' @returns Cleaned character vector.
#' @noRd
clean_vars <- function(vars) {
  if (!is.character(vars) || length(vars) == 0L) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg vars} argument.",
      "i" = "Must be a non-empty {.cls character} vector."
    ))
  }

  vars <- trimws(vars)
  vars <- vars[nzchar(vars)]
  vars <- unique(vars)

  if (length(vars) == 0L) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg vars} argument.",
      "i" = "Cannot be empty.",
      ">" = "Provide one or more variable names."
    ))
  }

  vars
}


#' Clean variable name(s)
#'
#' Clean variable name(s) to create syntactically valid, unique names for use in R and Julia.
#'
#' @param new Vector of names to transform to valid names
#' @param protected Optional vector of protected names, e.g., existing names in model
#'
#' @returns Vector of cleaned names
#' @export
#' @concept internal
#' @examples
#' sfm <- stockflow("predator_prey")
#' # As the variable name "predator" is already taken, clean_name() will create
#' # a unique name
#' clean_name("predator", as.data.frame(sfm)[["name"]])
#'
clean_name <- function(new, protected = NULL) {
  # Make syntactically valid and unique names out of character vectors; Insight Maker allows names to be double, so make unique
  protected_names_complete <- c(protected_names, as.character(stats::na.omit(protected)))
  new_names <- make.names(c(protected_names_complete, trimws(new)), unique = TRUE)
  # For Julia translation, remove names with a period
  new_names <- gsub(".", "_", new_names, fixed = TRUE)
  # This may cause overlap in names, so repeat
  new_names <- make.names(new_names, unique = TRUE)
  new_names <- gsub(".", "_", new_names, fixed = TRUE)
  new_names <- make.names(new_names, unique = TRUE)[-seq_along(protected_names_complete)] # Remove protected names

  new_names
}


#' Get allowed variable types for stockflow model
#'
#' @returns Character vector of allowed type names.
#' @noRd
.stockflow_types <- function() {
  c("stock", "flow", "constant", "aux", "lookup", "func")
}


#' Quickly get names of model variables
#'
#' @inheritParams update.stockflow
#'
#' @noRd
#' @returns Vector with names of model variables
get_model_var <- function(object) {
  object[["variables"]][["name"]]
}


#' Get func-type variables from model
#'
#' @inheritParams update.stockflow
#' @returns data.frame of func-type variables
#' @noRd
get_funcs <- function(object) {
  object[["variables"]][object[["variables"]][["type"]] == "func", ]
}


#' Create data frame with stock-and-flow model variables, types, and labels
#'
#' @inheritParams update.stockflow
#'
#' @returns data.frame
#' @noRd
#'
get_names <- function(object) {
  # Return variables data frame (already has type, name, label)
  if (nrow(object[["variables"]]) == 0) {
    names_df <- data.frame(
      type = character(0),
      name = character(0),
      label = character(0),
      stringsAsFactors = FALSE
    )
  } else {
    names_df <- object[["variables"]][, c("type", "name", "label")]
  }

  rownames(names_df) <- NULL
  names_df
}


#' Filter a long-format simulation data frame by variable and/or type
#'
#' Shared filtering logic for the `as.data.frame()` methods of simulation,
#' ensemble, and verification results. Operates on a long-format data frame with
#' a variable column (default `"variable"`), mirroring the `vars`/`type`
#' behaviour of [as.data.frame.stockflow()]: `vars` and `type` are mutually
#' exclusive (specifying both warns and ignores `type`), `type` is resolved to
#' variable names via the model, and unknown variables raise an error.
#'
#' @param df Long-format data frame to filter.
#' @param object Stock-and-flow model (the `object` element of the result) used
#'   to map variable types to names.
#' @param vars Character vector of variable names to retain, or `NULL`.
#' @param type Character vector of variable types to retain, or `NULL`.
#' @param var_col Name of the column holding variable names. Defaults to
#'   `"variable"`.
#'
#' @returns The filtered data frame.
#' @noRd
.filter_long_by_vars_type <- function(df, object, vars = NULL, type = NULL,
                                      var_col = "variable") {
  if (is.null(vars) && is.null(type)) {
    return(df)
  }

  # Check for mutually exclusive parameters
  if (!is.null(vars) && !is.null(type)) {
    cli::cli_warn(c("!" = "Both {.arg vars} and {.arg type} specified; ignoring {.arg type} and using {.arg vars} only."))
    type <- NULL
  }

  names_df <- get_names(object)

  # Filter by type: resolve requested types to variable names via the model
  if (!is.null(type)) {
    type <- .validate_type_arg(type, arg_name = "type")
    if (length(type) == 0) {
      cli::cli_abort(c("x" = "At least one {.arg type} must be specified"))
    }
    keep <- names_df[names_df[["type"]] %in% type, "name"]
    df <- df[df[[var_col]] %in% keep, , drop = FALSE]
  }

  # Filter by vars: validate, then distinguish typos (not in the model) from
  # variables that exist in the model but were not saved in the output.
  if (!is.null(vars)) {
    vars <- .validate_name_arg(vars, arg_name = "vars")
    vars <- Filter(nzchar, unique(vars))
    if (length(vars) == 0) {
      cli::cli_abort(c("x" = "At least one {.arg vars} must be specified"))
    }

    # Variables absent from the model are typos
    validate_vars_in_model(vars, names_df, NULL, context = "model")

    # Variables that exist in the model but are missing from the saved output
    not_saved <- setdiff(vars, df[[var_col]])
    if (length(not_saved) > 0) {
      vars_not_saved(not_saved, names_df, arg = "vars", action = "abort")
    }

    df <- df[df[[var_col]] %in% vars, , drop = FALSE]
  }

  rownames(df) <- NULL
  df
}


#' Inform the user that requested variables were not saved in the output
#'
#' Produces a single, consistent, actionable message used across the package
#' (e.g. by the [as.data.frame()] methods and the plotting functions) when a
#' user requests a variable that exists in the model but is absent from the
#' saved simulation output. Constants are called out separately, as they are
#' never part of the time-series output regardless of `only_stocks`.
#'
#' @param vars Character vector of model variables missing from the output.
#' @param names_df Optional data frame with `type`/`name` columns, used to tailor
#'   the guidance when all the missing variables are constants.
#' @param arg Offending argument name for the message (e.g. `"name"`, `"vars"`).
#' @param action Either `"abort"` (default) to raise an error or `"warn"` to
#'   emit a warning and let the caller continue with the available variables.
#' @param call Calling environment for error attribution.
#'
#' @returns Invoked for its side effect (error or warning); returns nothing.
#' @noRd
vars_not_saved <- function(vars, names_df = NULL, arg = "vars",
                           action = c("abort", "warn"),
                           call = rlang::caller_env()) {
  action <- match.arg(action)

  # Constants (and lookups) are never written to the time-series output, so
  # only_stocks = FALSE would not help; give them tailored guidance.
  all_constants <- FALSE
  if (!is.null(names_df) && "type" %in% names(names_df) && length(vars) > 0) {
    types <- names_df[match(vars, names_df[["name"]]), "type"]
    all_constants <- all(!is.na(types) & types %in% c("constant", "lookup"))
  }

  lead <- if (action == "warn") {
    "Ignoring {.arg {arg}} {cli::qty(length(vars))}{?value/values} {.val {vars}}: {cli::qty(length(vars))}{?it exists/they exist} in the model but {cli::qty(length(vars))}{?was/were} not saved in the output."
  } else {
    "{cli::qty(length(vars))}{?Variable/Variables} {.val {vars}} {cli::qty(length(vars))}{?exists/exist} in the model but {cli::qty(length(vars))}{?was/were} not saved in the output."
  }

  remedy <- if (all_constants) {
    "Constants are not part of the time-series output; inspect them on the model with {.code as.data.frame(model)}."
  } else {
    "Re-run with {.code only_stocks = FALSE} to save all variables, or set {.arg vars} in {.fn sim_settings} to save specific ones."
  }

  if (action == "warn") {
    cli::cli_warn(c("!" = lead, ">" = remedy))
  } else {
    cli::cli_abort(c(lead, ">" = remedy), call = call)
  }
}


#' Validate vars argument for simulation output selection
#'
#' @inheritParams update.stockflow
#' @param vars Character vector of variable names to save.
#'
#' @returns Cleaned character vector of unique variable names.
#' @noRd
validate_sim_vars <- function(object, vars) {
  if (is.null(vars)) {
    return(NULL)
  }

  if (is.character(vars)) {
    trimmed_vars <- trimws(vars)
    if (length(trimmed_vars) == 0L || all(!is.na(trimmed_vars) & !nzchar(trimmed_vars))) {
      return(NULL)
    }
  }

  vars <- clean_vars(vars)

  # Check that all vars are in model variables
  model_vars <- get_model_var(object)
  invalid_vars <- setdiff(vars, model_vars)
  if (length(invalid_vars) > 0) {
    cli::cli_abort(c(
      "x" = "Invalid variable name{?s} in {.arg vars}: {.val {invalid_vars}}.",
      "i" = "The following variable names are not in the model: {.val {invalid_vars}}.",
      ">" = "Available variable names are: {.val {model_vars}}."
    ))
  }

  vars
}


#' Resolve simulation output variable settings
#'
#' If `vars` is supplied, it overrides `only_stocks`: stock-only output remains
#' TRUE only when all requested variables are stocks.
#'
#' @inheritParams update.stockflow
#' @param only_stocks Logical stock-only output setting.
#' @param vars Character vector of variable names, or NULL.
#'
#' @returns List with validated `vars` and resolved `only_stocks`.
#' @noRd
resolve_sim_output_args <- function(object, only_stocks, vars) {
  if (is.null(vars)) {
    return(list(only_stocks = only_stocks, vars = NULL))
  }

  vars <- validate_sim_vars(object, vars)
  stock_names <- get_variables_by_type(object, "stock")[["name"]]

  list(
    only_stocks = all(vars %in% stock_names),
    vars = vars
  )
}


#' Filter long simulation data frame by selected variables
#'
#' @param df Data frame with a "variable" column.
#' @param vars Character vector of variable names, or NULL.
#'
#' @returns Filtered data frame.
#' @noRd
filter_sim_df_vars <- function(df, vars) {
  if (is.null(vars) || nrow(df) == 0 || !"variable" %in% names(df)) {
    return(df)
  }

  df[df[["variable"]] %in% vars, , drop = FALSE]
}


#' Convert if possible
#'
#' @param x Value
#'
#' @returns Converted value
#' @noRd
#'
safe_convert <- function(x, target_class) {
  result <- switch(target_class,
    "numeric" = suppressWarnings(as.numeric(x)),
    "integer" = suppressWarnings(as.integer(x)),
    "logical" = suppressWarnings(as.logical(x)),
    "character" = as.character(x),
    x # return original if class not recognized
  )

  # Keep original if conversion failed (became NA but wasn't originally NA)
  if (target_class != "character" && is.na(result) && !is.na(x)) {
    return(x)
  } else {
    return(result)
  }
}


#' Split arguments to function by comma
#'
#' @param bracket_arg String with arguments, excluding surrounding brackets
#'
#' @returns Vector with arguments
#' @noRd
#'
parse_args <- function(bracket_arg) {
  # Split arguments by comma; in order to not split arguments which contain a comma (e.g., c(1,2,3)), find all brackets and quotation marks, and don't include commas within these

  # Find indices of commas
  idxs_commas <- unname(stringr::str_locate_all(bracket_arg, ",")[[1]][, 1])

  # If there's no commas, there's only one argument
  if (length(idxs_commas) == 0) {
    args <- bracket_arg
  } else {
    # Create sequence of indices between brackets/quotation marks, and check whether comma is between them
    paired_idxs <- get_range_all_pairs(bracket_arg, var_names = NULL)
    paired_idxs_seq <- unlist(mapply(seq, paired_idxs[["start"]], paired_idxs[["end"]], SIMPLIFY = FALSE))

    idxs_commas <- idxs_commas[!idxs_commas %in% paired_idxs_seq]

    # Only keep commas which are not between brackets
    # Start and end positions based on indices
    starts <- c(1, idxs_commas + 1)
    ends <- c(idxs_commas - 1, stringr::str_length(bracket_arg))

    # Split bracket argument by indices
    args <- mapply(stringr::str_sub, bracket_arg, starts, ends) |>
      trimws() |>
      unname()
  }

  args
}


#' Sort arguments in function call according to default order
#'
#' @param arg Vector with arguments in strings
#' @param func_name String with name of R function
#' @param default_arg Either NULL or named list of default arguments
#' @inheritParams convert_builtin_functions_julia_old
#'
#' @noRd
#' @returns List with named and sorted arguments
#'
sort_args <- function(arg, func_name, default_arg = NULL, var_names = NULL,
                      fill_defaults = TRUE) {
  # Find names and values of arguments (e.g. "mean = 3" -> name "mean", value "3")
  contains_name <- stringr::str_detect(arg, "=")
  arg_split <- stringr::str_split_fixed(arg, "=", n = 2)
  names_arg <- trimws(ifelse(contains_name, arg_split[, 1], NA))
  values_arg <- trimws(ifelse(contains_name, arg_split[, 2], arg_split[, 1]))

  if (!fill_defaults) {
    # syntax1 path: pass only the user-provided arguments, in the given order,
    # with names dropped. We deliberately do NOT inject R defaults the Julia
    # target lacks (na.rm, deparse.level, locale, ...) or reorder. The Julia
    # function or r_* wrapper supplies its own defaults and adapts argument
    # names / order / keywords internally.

    # Best-effort validation: flag named arguments that are not valid R formals
    # (catches typos like sd(a, y = test)). Skipped silently when formals are
    # unavailable (primitives, namespaced names) or variadic, so we never error
    # on cases the no-fill path is specifically designed to tolerate.
    named <- names_arg[!is.na(names_arg)]
    if (length(named) > 0) {
      fmls <- tryCatch(
        names(as.list(do.call(formals, list(func_name)))),
        error = function(e) NULL
      )
      if (!is.null(fmls) && !("..." %in% fmls)) {
        bad <- named[!(named %in% fmls)]
        if (length(bad) > 0) {
          cli::cli_abort(c(
            "x" = "{cli::qty(length(bad))}Invalid argument{?s} for {.fn {func_name}}.",
            "i" = "{.code {bad}} {?is/are} not allowed.",
            ">" = "Allowed arguments: {.code {fmls}}."
          ))
        }
      }
    }

    arg_R <- as.list(values_arg[nzchar(values_arg)])
  } else {
    # Fill path (distributions, seq, sample, and faithful r_* wrappers): resolve
    # against R's formals, fill unprovided defaults, and reorder into formal
    # order. The Julia target mirrors R's positional signature, so filled
    # defaults land in the correct positions.

    # If default arguments are not provided, assume func_name is an R function
    if (is.null(default_arg)) {
      # Find default arguments of R function
      # Assume Julia and R arguments are the same, with the same order
      default_arg <- do.call(formals, list(func_name)) |> as.list()
      varargs <- any(names(default_arg) == "...")
      default_arg <- default_arg[names(default_arg) != "..."] # Remove ellipsis

      # formals(seq) is empty for some reason
      if (func_name == "seq") {
        default_arg <- list(
          "from" = "1.0", "to" = "1.0", "by" = NULL,
          "length.out" = NULL, "along.with" = NULL
        )
      } else if (func_name == "seq_along") {
        default_arg <- list("along.with" = "NULL")
      } else if (func_name == "seq_len") {
        default_arg <- list("length.out" = "1.0")
      } else if (func_name == "rep") {
        # rep() is variadic with opaque formals; hardcode the args r_rep mirrors
        # so named forms (each =, length.out =) map to the correct positions.
        # length.out = -1 is r_rep's "unset" sentinel.
        default_arg <- as.list(alist(x = , times = "1", length.out = "-1", each = "1"))
      } else if (func_name == "diff") {
        # formals(diff) is just (x, ...) — the real args live in diff.default.
        # Hardcode them so named forms (e.g. differences =) map to the correct
        # positions of r_diff(x, lag, differences) instead of silently landing
        # in `lag`.
        default_arg <- as.list(alist(x = , lag = "1", differences = "1"))
      }
    }

    # For some functions, there are no default arguments, so there is no need to sort them
    if (length(default_arg) == 0) {
      arg_R <- stats::setNames(values_arg, names_arg)
    } else {
      # Check whether all argument names are in the allowed argument names in case of no dots argument (...)
      idx <- !names_arg %in% names(default_arg) & !is.na(names_arg)
      if (!varargs && any(idx)) {
        bad_args <- names_arg[idx]
        allowed <- names(default_arg)
        cli::cli_abort(c(
          "x" = "{cli::qty(length(bad_args))}Invalid argument{?s} for {.fn {func_name}}.",
          "i" = "{.code {bad_args}} {?is/are} not allowed.",
          ">" = "Allowed arguments: {.code {allowed}}."
        ))
      }

      # Check if there are too many arguments
      if (!varargs && length(arg) > length(default_arg)) {
        allowed <- names(default_arg)
        cli::cli_abort(c(
          "x" = "Too many arguments for {.fn {func_name}}.",
          "i" = "Got {length(arg)} but maximum is {length(default_arg)}.",
          ">" = "Allowed arguments: {.code {allowed}}."
        ))
      }

      # Add names to unnamed arguments; note that R can mix named and default arguments, e.g., runif(max = 10, 20, min = 1). Julia cannot if they're not keyword arguments!
      idx <- which(!contains_name & nzchar(values_arg)) # Find unnamed arguments which have values
      standard_order <- names(default_arg)
      if (length(idx) > 0 && length(standard_order) > 0) {
        new_names <- setdiff(standard_order, stats::na.omit(names_arg)) # names which are missing from the passed argument names
        names_arg[idx] <- new_names[seq_along(idx)] # Assign new names to unnamed arguments; only select as many as there are unnamed arguments
      }

      # Check for missing obligatory arguments
      # obligatory arguments without a default (class == "name" or is.symbol, e.g., n in formals(rnorm) is a symbol)
      obligatory_args <- unlist(lapply(default_arg, is.symbol))
      idx <- !names(default_arg[obligatory_args]) %in% names_arg

      if (any(idx)) {
        missing_args <- names(default_arg[obligatory_args])[idx]
        cli::cli_abort(c(
          "x" = "{cli::qty(length(missing_args))}Missing required argument{?s} for {.fn {func_name}}.",
          ">" = "{.code {missing_args}} {?is/are} required."
        ))
      }

      # Overwrite default arguments with specified arguments & remove NULL arguments
      default_arg_list <- default_arg[!obligatory_args | unlist(lapply(default_arg, is.null))]
      arg_R <- utils::modifyList(default_arg_list, as.list(stats::setNames(values_arg, names_arg)))

      # Sort order of arguments according to default order
      order_arg <- c(names(default_arg), setdiff(names(arg_R), names(default_arg)))
      arg_R <- arg_R[order_arg]

      # Check if any of the arguments are calls - these will need to be evaluated
      if (any(vapply(arg_R, class, character(1)) == "call")) {
        arg_R_num <- lapply(arg_R, function(x) {
          if (!is.call(x)) {
            if (!grepl("'|\"", x) & !is.na(suppressWarnings(as.numeric(x)))) {
              x <- as.numeric(x)
            }
          }
          return(x)
        })

        # Parse in case of default arguments like scale = 1/rate
        for (name in names(arg_R)) {
          if (is.language(arg_R[[name]]) && !is.name(arg_R[[name]])) {
            # Evaluate the expression in the context of merged_args
            env <- list2env(arg_R_num, parent = baseenv())

            # Substitute values into the expression
            arg_R[[name]] <- deparse(eval(bquote(substitute(.(arg_R[[name]]), env))))
          }
        }
      }

      # Ensure digits become floats for Julia
      for (name in names(arg_R)) {
        if (!is.null(arg_R[[name]])) {
          if (is.language(arg_R[[name]])) {
            arg_R[[name]] <- paste(deparse(arg_R[[name]]), collapse = "")
          } else {
            arg_R[[name]] <- as.character(arg_R[[name]])
          }
          arg_R[[name]] <- replace_digits_with_floats(arg_R[[name]], var_names)
        }
      }
    }
  }

  arg_R <- lapply(arg_R, as.character)

  # Lowercase logical defaults injected from R formals (e.g. ignore.case = FALSE)
  # so they are valid Julia. User-supplied TRUE/FALSE in the equation are already
  # converted earlier by replace_op_julia(); only injected defaults remain.
  arg_R <- lapply(arg_R, function(v) {
    if (is.null(v)) {
      return(v)
    }
    v[v == "TRUE"] <- "true"
    v[v == "FALSE"] <- "false"
    v
  })

  return(arg_R)
}


#' Get start and end indices of each name
#'
#' @param var_names Vector with variable names
#' @param names_with_brackets Boolean; whether to add square bracket around the variable names
#' @inheritParams convert_equations_IM
#'
#' @returns data.frame with start and end indices of each name
#' @noRd
#'
get_range_names <- function(eqn, var_names, names_with_brackets = FALSE) {
  idxs_df <- data.frame()

  if (length(var_names) > 0) {
    # Save original names
    original_names <- var_names

    # If names are surrounded by square brackets, add these to the names
    if (names_with_brackets) {
      var_names <- paste0("[", var_names, "]")
    }

    # Add surrounding word boundaries and escape special characters
    # \\b doesn't match beginning or end of string; \W is non-word character; ?: is non-capture group
    R_names <- paste0("(?:^|(?<=\\W))", stringr::str_escape(var_names), "(?=(?:\\W|$))")
    idxs_names <- stringr::str_locate_all(eqn, R_names)

    if (length(unlist(idxs_names)) > 0) {
      # Create indices dataframe with detected variable names
      idxs_df <- as.data.frame(do.call(rbind, idxs_names))
      idxs_df[["name"]] <- rep(original_names, vapply(idxs_names, nrow, numeric(1)))

      # Remove matches in characters
      idxs_exclude <- get_seq_exclude(eqn,
        type = "quot",
        names_with_brackets = names_with_brackets
      )

      if (nrow(idxs_df) > 0) idxs_df <- idxs_df[!(idxs_df[["start"]] %in% idxs_exclude | idxs_df[["end"]] %in% idxs_exclude), ]
    }
  }

  return(idxs_df)
}


#' Replace substrings in reverse order of position
#'
#' Applies a set of substring replacements to `x`, working from the last match to
#' the first so that earlier (lower-index) replacements are not invalidated by the
#' index shifts of later ones. This idiom recurs throughout the equation-conversion
#' code, where matches are located up front and spliced back in afterwards.
#'
#' @param x String to modify.
#' @param df data.frame with integer `start` and `end` columns giving the spans to
#'   replace.
#' @param replacement Replacement values, recycled to `nrow(df)`. Defaults to the
#'   `replacement` column of `df`; pass a scalar to use the same value for every span.
#'
#' @returns Updated string `x`.
#' @noRd
#'
apply_replacements_reversed <- function(x, df, replacement = df[["replacement"]]) {
  if (nrow(df) == 0) {
    return(x)
  }
  replacement <- rep_len(replacement, nrow(df))
  starts <- df[["start"]]
  ends <- df[["end"]]
  for (i in rev(seq_len(nrow(df)))) {
    stringr::str_sub(x, starts[i], ends[i]) <- replacement[i]
  }
  x
}


#' Replace a dictionary of operators in an equation
#'
#' Shared engine behind replace_op_IM() and replace_op_julia_impl(). Locates
#' every operator in `op`, drops matches that fall inside quoted strings / variable
#' names (and any spans flagged by `extra_exclude`), removes no-op matches, and
#' splices in the replacements from last to first.
#'
#' @param eqn Equation string.
#' @param var_names Variable names whose spans must not be treated as operators.
#' @param op Named character vector: names are (regex) patterns to match, values
#'   are their replacements.
#' @param names_with_brackets Passed to get_seq_exclude(); whether bracketed
#'   names are excluded.
#' @param ignore_case Whether the operator patterns are matched case-insensitively.
#' @param extra_exclude Optional `function(eqn, var_names)` returning additional
#'   indices to exclude from replacement (e.g. `=` inside `function(...)` argument
#'   lists).
#'
#' @returns Updated equation string.
#' @noRd
#'
apply_operator_replacements <- function(eqn, var_names, op,
                                        names_with_brackets = FALSE,
                                        ignore_case = FALSE,
                                        extra_exclude = NULL) {
  pattern <- if (ignore_case) {
    stringr::regex(names(op), ignore_case = TRUE)
  } else {
    names(op)
  }

  idxs_op <- stringr::str_locate_all(eqn, pattern)
  if (length(unlist(idxs_op)) == 0) {
    return(eqn)
  }

  df <- as.data.frame(do.call(rbind, idxs_op))
  df[["match"]] <- stringr::str_sub(eqn, df[["start"]], df[["end"]])
  df[["replacement"]] <- rep(unname(op), vapply(idxs_op, nrow, numeric(1)))
  df <- df[order(df[["start"]]), ]

  # Drop matches inside quotation marks or names
  idxs_exclude <- get_seq_exclude(eqn, var_names, names_with_brackets = names_with_brackets)
  if (nrow(df) > 0) df <- df[!(df[["start"]] %in% idxs_exclude | df[["end"]] %in% idxs_exclude), ]

  # Drop matches that already equal their replacement
  if (nrow(df) > 0) df <- df[df[["replacement"]] != df[["match"]], ]

  # Language-specific extra exclusions (e.g. = in function() argument defaults)
  if (nrow(df) > 0 && !is.null(extra_exclude)) {
    extra <- extra_exclude(eqn, var_names)
    if (length(extra) > 0) df <- df[!(df[["start"]] %in% extra | df[["end"]] %in% extra), ]
  }

  if (nrow(df) > 0) {
    eqn <- apply_replacements_reversed(eqn, df)
    # Remove double spaces
    eqn <- stringr::str_replace_all(eqn, "[ ]+", " ")
  }

  eqn
}


#' Get sequence of indices of to exclude
#'
#' @inheritParams convert_equations_IM
#' @inheritParams get_range_all_pairs
#' @inheritParams get_range_names
#'
#' @returns Sequence of indices
#' @noRd
#'
get_seq_exclude <- function(eqn,
                            var_names = NULL,
                            type = c("quot", "names"),
                            names_with_brackets = FALSE) {
  # When var_names includes "", then everything is included in the sequence to exclude -> remove ""
  if (!is.null(var_names)) {
    var_names <- var_names[var_names != ""]
    if (length(var_names) == 0) var_names <- NULL
  }

  pair_quotation_marks <- data.frame()
  pair_names <- data.frame()

  if ("quot" %in% type) {
    # Get start and end indices of paired ''
    pair_quotation_marks <- get_range_quot(eqn)
    if (nrow(pair_quotation_marks) > 0) pair_quotation_marks[["type"]] <- "quot"
  }

  if ("names" %in% type) {
    # Get start and end indices of variable names
    pair_names <- get_range_names(eqn, var_names,
      names_with_brackets = names_with_brackets
    )
    if (nrow(pair_names) > 0) pair_names[["type"]] <- "names"
  }

  # comb <- dplyr::bind_rows(pair_quotation_marks, pair_names)
  comb <- bind_rows_(pair_quotation_marks, pair_names)

  # Create sequence
  if (nrow(comb) > 0) {
    paired_seq <- lapply(seq_len(nrow(comb)), function(i) {
      seq(comb[i, ][["start"]], comb[i, ][["end"]])
    }) |>
      unlist() |>
      unique() |>
      sort()
  } else {
    paired_seq <- c()
  }

  return(paired_seq)
}


#' Extract start and end indices of all words
#'
#' @inheritParams convert_equations_IM
#'
#' @returns data.frame with start and end indices of all words as well as extracted words
#' @noRd
#'
get_words <- function(eqn) {
  # An existing function stringr::word() extracts words but treats e.g., "return(a)" as one word
  idxs_word <- stringr::str_locate_all(eqn, "([a-zA-Z_\\.0-9]+)")[[1]] |> as.data.frame()

  if (nrow(idxs_word) > 0) idxs_word[["word"]] <- stringr::str_sub(eqn, idxs_word[["start"]], idxs_word[["end"]])

  return(idxs_word)
}

#' Extract variables of a specific type from the flat variables data frame
#'
#' Helper functions to convert from old nested structure access to new flat data frame access.
#'
#' @param object A stock-and-flow model
#' @param type Type of variable to extract (e.g., "stock", "flow", "aux", "constant", "lookup")
#'
#' @returns data.frame of variables of the specified type
#' @noRd
get_variables_by_type <- function(object, type) {
  object[["variables"]][object[["variables"]][["type"]] == type, ]
}

#' Extract a column from variables of a specific type as a named list
#'
#' @param object A stock-and-flow model
#' @param type Type of variable to extract
#' @param column Column name to extract (e.g., "eqn_str")
#'
#' @returns Named list where names are variable names and values are the column values
#' @noRd
get_vars_column_as_list <- function(object, type, column) {
  vars_df <- get_variables_by_type(object, type)
  if (nrow(vars_df) == 0) {
    return(list())
  }
  stats::setNames(as.list(vars_df[[column]]), vars_df[["name"]])
}

# ===== Equation Conversion Utilities =====
# These functions are shared between insightmaker_conv_eqn.R and julia_conv_eqn.R
# They help identify positions in equations while respecting quotation marks,
# comments, variable names, and bracket pairs.

#' Get indices of all comments in equation
#'
#' @param eqn Equation string
#' @returns data.frame with start and end indices of all comments in eqn
#' @noRd
get_range_comments <- function(eqn) {
  idxs_comments <- stringr::str_locate_all(eqn, "#")[[1]][, "start"]
  idxs_newline <- unname(stringr::str_locate_all(eqn, "\n")[[1]][, 1]) |>
    c(nchar(eqn) + 1)

  if (length(idxs_comments) > 0) {
    pair_comments <- lapply(idxs_comments, function(i) {
      c(i, min(
        idxs_comments[idxs_comments > i][1],
        idxs_newline[idxs_newline > i][1],
        na.rm = TRUE
      ) - 1)
    }) |>
      do.call(rbind, args = _) |>
      set_colnames(c("start", "end")) |>
      as.data.frame()
  } else {
    pair_comments <- data.frame()
  }
  return(pair_comments)
}

#' Get indices of all quotation marks
#'
#' @param eqn Equation string
#' @returns data.frame with indices of quotation marks in eqn
#' @noRd
get_range_quot <- function(eqn) {
  pair_quotation_marks <- data.frame()
  idx_quot_single <- gregexpr("'", eqn)[[1]]
  idx_quot_escape <- gregexpr("\"", eqn)[[1]]
  idx_quot <- c(idx_quot_single, idx_quot_escape)
  idx_quot <- idx_quot[idx_quot != -1]

  if (length(idx_quot) > 0) {
    comment_df <- get_range_comments(eqn)
    if (nrow(comment_df) > 0) {
      idxs_comments <- unlist(mapply(seq, comment_df[, "start"], comment_df[, "end"], SIMPLIFY = FALSE))
      idx_quot <- setdiff(idx_quot, idxs_comments)
    }
    if (length(idx_quot) > 0) {
      pair_quotation_marks <- data.frame(
        start = idx_quot[seq(1, length(idx_quot), by = 2)],
        end = idx_quot[seq(2, length(idx_quot), by = 2)]
      )
    }
  }
  return(pair_quotation_marks)
}


#' Select the innermost built-in function to convert next
#'
#' Shared step of convert_builtin_functions_IM() and
#' convert_builtin_functions_julia_old(). Given the detected function matches
#' (`idx_df`), it pairs each function with its opening round bracket, adds back the
#' functions that take no brackets, and returns the single most deeply nested match
#' (the one to convert first so that nested calls are resolved inside-out).
#'
#' @param eqn Equation string.
#' @param idx_df data.frame of detected function matches (with `start`, `end`,
#'   `syntax` columns).
#' @param var_names Variable names, forwarded to get_range_all_pairs().
#' @param bracketless_syntaxes Character vector of `syntax` values whose functions
#'   do not require brackets and must be added back to the candidate set.
#' @param pair_args Extra arguments passed to get_range_all_pairs() (the only
#'   language-specific difference, e.g. `add_custom = "paste0()"` for Julia or
#'   `names_with_brackets = TRUE` for Insight Maker).
#'
#' @returns Single-row data.frame for the function to convert next.
#' @noRd
#'
select_innermost_function <- function(eqn, idx_df, var_names,
                                      bracketless_syntaxes, pair_args = list()) {
  # To find the arguments within round brackets, find all indices of matching '', (), [], c()
  paired_idxs <- do.call(get_range_all_pairs, c(list(eqn, var_names), pair_args))

  if (nrow(paired_idxs) > 0) {
    # Match the opening bracket of each function to round brackets in paired_idxs
    idx_funcs <- merge(
      paired_idxs[paired_idxs[["type"]] == "round", ],
      idx_df,
      by.x = "start",
      by.y = "end"
    )
    idx_funcs[["start_bracket"]] <- idx_funcs[["start"]]
    idx_funcs[["start"]] <- idx_funcs[["start.y"]]

    # Add back functions that do not need brackets
    bracketless <- idx_df[idx_df[["syntax"]] %in% bracketless_syntaxes, ]
    bracketless[["start_bracket"]] <- bracketless[["start"]]
    idx_funcs <- bind_rows_(idx_funcs, bracketless)
    idx_funcs <- idx_funcs[order(idx_funcs[["end"]]), ]
  } else {
    # If there are no brackets in the eqn, add start_bracket column to prevent errors
    idx_funcs <- idx_df
    idx_funcs[["start_bracket"]] <- idx_funcs[["start"]]
  }

  # Start with most nested function
  idx_funcs_ordered <- idx_funcs
  idx_funcs_ordered[["is_nested_around"]] <- any(idx_funcs_ordered[["start"]] < idx_funcs[["start"]] & idx_funcs_ordered[["end"]] > idx_funcs[["end"]])
  idx_funcs_ordered <- idx_funcs_ordered[order(idx_funcs_ordered[["is_nested_around"]]), ]
  idx_funcs_ordered[1, ]
}


#' Get indices of paired brackets
#'
#' @param eqn Equation string
#' @param var_names Variable names data frame
#' @param opening Opening bracket character(s)
#' @param closing Closing bracket character
#' @param names_with_brackets Whether to exclude variable names with brackets
#' @returns data.frame with start and end indices of bracket pairs
#' @noRd
get_range_pairs <- function(eqn, var_names,
                            opening = "c(", closing = ")",
                            names_with_brackets = FALSE) {
  opening_bare <- substr(opening, nchar(opening), nchar(opening))
  closing_bare <- substr(closing, nchar(closing), nchar(closing))

  open_locs <- stringr::str_locate_all(eqn, stringr::fixed(opening_bare))[[1]][, 1]
  close_locs <- stringr::str_locate_all(eqn, stringr::fixed(closing_bare))[[1]][, 1]

  exclude_idxs <- get_seq_exclude(eqn, var_names, names_with_brackets = names_with_brackets)
  open_locs <- open_locs[!open_locs %in% exclude_idxs]
  close_locs <- close_locs[!close_locs %in% exclude_idxs]

  if (length(open_locs) != length(close_locs)) {
    n_open <- length(open_locs)
    n_close <- length(close_locs)
    cli::cli_abort(c(
      "x" = "Mismatched brackets in equation.",
      "!" = "Found {n_open} {.code {opening_bare}} but {n_close} {.code {closing_bare}}.",
      "i" = "Equation: {.code {eqn}}"
    ))
  }

  if (length(open_locs) == 0 || length(close_locs) == 0) {
    return(data.frame(
      pair = integer(), start = integer(), end = integer(),
      id = integer(), nested_around = character(), nested_within = character(), match = character()
    ))
  }

  stack <- integer()
  pairs <- matrix(NA, nrow = length(open_locs), ncol = 2)
  pair_id <- 0
  for (i in seq_along(sort(c(open_locs, close_locs)))) {
    idx <- sort(c(open_locs, close_locs))[i]
    if (idx %in% open_locs) {
      stack <- c(stack, idx)
    } else {
      pair_id <- pair_id + 1
      pairs[pair_id, ] <- c(stack[length(stack)], idx)
      stack <- stack[-length(stack)]
    }
  }

  pair_df <- data.frame(pair = seq_len(pair_id), start = pairs[, 1], end = pairs[, 2])

  if (opening != opening_bare) {
    opening_strip <- substr(opening, 1, nchar(opening) - 1)
    matches <- stringr::str_sub(eqn, pair_df[["start"]] - nchar(opening_strip), pair_df[["start"]] - 1) == opening_strip
    pair_df <- pair_df[matches, ]
    pair_df[["start"]] <- pair_df[["start"]] - nchar(opening_strip)
  }

  if (nrow(pair_df) == 0) {
    return(data.frame(
      pair = integer(), start = integer(), end = integer(),
      id = integer(), nested_around = character(), nested_within = character(), match = character()
    ))
  }

  pair_df[["id"]] <- seq_len(nrow(pair_df))
  pair_df[["match"]] <- stringr::str_sub(eqn, pair_df[["start"]], pair_df[["end"]])
  pair_df[["nested_around"]] <- vapply(seq_len(nrow(pair_df)), function(i) {
    paste(which(pair_df[["start"]] < pair_df[["start"]][i] & pair_df[["end"]] > pair_df[["end"]][i]), collapse = ",")
  }, character(1))
  pair_df[["nested_within"]] <- vapply(seq_len(nrow(pair_df)), function(i) {
    paste(which(pair_df[["start"]] > pair_df[["start"]][i] & pair_df[["end"]] < pair_df[["end"]][i]), collapse = ",")
  }, character(1))

  return(pair_df)
}

#' Get indices of all paired brackets and quotation marks
#'
#' @param eqn Equation string
#' @param var_names Variable names data frame
#' @param add_custom Custom bracket pattern to add (e.g., "paste0()")
#' @param type Types of pairs to find (square, curly, round, vector, quot)
#' @param names_with_brackets Logical, whether to surround variable names with square brackets.
#' @returns data.frame with all bracket pairs
#' @noRd
#'
get_range_all_pairs <- function(eqn, var_names,
                                add_custom = NULL,
                                type = c("square", "curly", "round", "vector", "quot"),
                                names_with_brackets = FALSE) {
  pair_square_brackets <- data.frame()
  pair_curly_brackets <- data.frame()
  pair_round_brackets <- data.frame()
  pair_vector_brackets <- data.frame()
  pair_quotation_marks <- data.frame()
  pair_custom <- data.frame()

  if ("square" %in% type) {
    pair_square_brackets <- get_range_pairs(eqn, var_names, opening = "[", closing = "]", names_with_brackets = names_with_brackets)
    if (nrow(pair_square_brackets) > 0) pair_square_brackets[["type"]] <- "square"
  }

  if ("curly" %in% type) {
    pair_curly_brackets <- get_range_pairs(eqn, var_names, opening = "{", closing = "}", names_with_brackets = names_with_brackets)
    if (nrow(pair_curly_brackets) > 0) pair_curly_brackets[["type"]] <- "curly"
  }

  if ("round" %in% type) {
    pair_round_brackets <- get_range_pairs(eqn, var_names, opening = "(", closing = ")", names_with_brackets = names_with_brackets)
    if (nrow(pair_round_brackets) > 0) pair_round_brackets[["type"]] <- "round"
  }

  if ("vector" %in% type) {
    pair_vector_brackets <- get_range_pairs(eqn, var_names, opening = "c(", closing = ")", names_with_brackets = names_with_brackets)
    if (nrow(pair_vector_brackets) > 0) pair_vector_brackets[["type"]] <- "vector"
  }

  if ("quot" %in% type) {
    pair_quotation_marks <- get_range_quot(eqn)
    if (nrow(pair_quotation_marks) > 0) pair_quotation_marks[["type"]] <- "quot"
  }

  if (!is.null(add_custom)) {
    l <- nchar(add_custom)
    name_custom <- substr(add_custom, 1, l - 2)
    opening <- substr(add_custom, 1, l - 1)
    closing <- substr(add_custom, l, l)
    type <- c(type, name_custom)
    pair_custom <- get_range_pairs(eqn, var_names, opening = opening, closing = closing)
    if (nrow(pair_custom) > 0) pair_custom[["type"]] <- name_custom
  }

  paired_idxs <- bind_rows_(
    pair_square_brackets,
    pair_curly_brackets,
    pair_round_brackets,
    pair_vector_brackets,
    pair_quotation_marks,
    pair_custom
  ) |> set_rownames(NULL)

  paired_idxs
}


# ===== Import Conversion Context =====
# These functions manage the conversion pipeline for importing models from
# external vendors (InsightMaker, Stella, Vensim, etc.). The context pattern
# separates intermediate state (variables being transformed) from the final
# model structure (object), allowing conversions to happen in the correct order.

#' Create a new import conversion context
#'
#' Creates a context object for managing the import conversion pipeline.
#' The context holds intermediate data while components are progressively
#' moved to the object as they become ready.
#'
#' @param vendor Character. The source vendor (e.g., "insightmaker", "stella", "vensim").
#'
#' @returns A list with class "stockflow_import_context"
#' @noRd
#'
create_import_context <- function(vendor) {
  ctx <- list(
    # The object being built - components added progressively as they become ready
    object = new_stockflow(),

    # Vendor identification
    vendor = vendor,

    # Source information (populated by caller)
    file_path = NULL,
    url = NULL,

    # Raw model before any transformations (for import_metadata)
    raw_model = NULL,

    # Vendor-specific meta info (for import_metadata)
    vendor_meta = list(),

    # Variables in intermediate state (not yet ready for object)
    # This is a list of variable specs, NOT yet a data frame
    # Each element has: name, type, eqn, etc.
    variables = list(),

    # Original variable info (for import_metadata, captured before transformations)
    original_variables = NULL,

    # Macros in intermediate state (raw text, not yet parsed)
    macros_raw = "",

    # Original macro info (for import_metadata)
    original_macros = NULL,

    # Settings from the source model
    settings = list(),

    # Header info from the source model
    meta = list()
  )

  class(ctx) <- c("stockflow_import_context", "list")
  ctx
}


#' Add simulation specs to context's object
#'
#' Once sim_settings are parsed and validated, add them to the sfm.
#'
#' @param ctx Import context
#' @param settings List with method, time_units, start, length, dt, etc.
#' @param settings_converter Function to convert vendor-specific settings to sim_settings args
#'
#' @returns Updated context
#' @noRd
#'
ctx_add_sim_settings <- function(ctx, settings, settings_converter = identity) {
  if (!is.null(settings) && length(settings) > 0) {
    # Convert vendor-specific settings if a converter is provided
    args <- settings_converter(settings)

    # Call sim_settings with the converted arguments
    ctx$object <- do.call(sim_settings, c(list(object = ctx$object), args))
  }
  ctx
}


#' Add meta to context's object
#'
#' Once meta info is parsed, add it to the sfm.
#'
#' @param ctx Import context
#' @param meta List with name, author, caption, etc.
#'
#' @returns Updated context
#' @noRd
#'
ctx_add_meta <- function(ctx, meta) {
  if (!is.null(meta) && length(meta) > 0) {
    ctx$object[["meta"]] <- utils::modifyList(ctx$object[["meta"]], meta)
  }
  ctx
}


#' Store original variable info for import_metadata
#'
#' Captures the original variable information before any transformations.
#' This should be called right after parsing, before any equation conversion.
#'
#' @param ctx Import context
#'
#' @returns Updated context with original_variables populated
#' @noRd
#'
ctx_capture_original_variables <- function(ctx) {
  if (length(ctx$variables) > 0) {
    ctx$original_variables <- data.frame(
      name = vapply(ctx$variables, function(x) x[["name"]] %||% NA_character_, character(1)),
      original_id = vapply(ctx$variables, function(x) x[["id_insightmaker"]] %||% NA_character_, character(1)),
      original_name = vapply(ctx$variables, function(x) x[["name_insightmaker"]] %||% x[["name"]] %||% NA_character_, character(1)),
      original_eqn = vapply(ctx$variables, function(x) x[["eqn_insightmaker"]] %||% x[["eqn"]] %||% NA_character_, character(1)),
      stringsAsFactors = FALSE
    )
  } else {
    ctx$original_variables <- data.frame(
      name = character(0),
      original_id = character(0),
      original_name = character(0),
      original_eqn = character(0),
      stringsAsFactors = FALSE
    )
  }
  ctx
}


#' Store original macro info for import_metadata
#'
#' Captures the original macro information before any transformations.
#'
#' @param ctx Import context
#'
#' @returns Updated context with original_macros populated
#' @noRd
#'
ctx_capture_original_macros <- function(ctx) {
  if (nzchar(ctx$macros_raw)) {
    ctx$original_macros <- data.frame(
      name = "macros", # placeholder, actual parsing happens during conversion
      original_name = "macros",
      original_eqn = ctx$macros_raw,
      stringsAsFactors = FALSE
    )
  } else {
    ctx$original_macros <- data.frame(
      name = character(0),
      original_name = character(0),
      original_eqn = character(0),
      stringsAsFactors = FALSE
    )
  }
  ctx
}


#' Move variables from context to object (raw, no Julia conversion)
#'
#' Adds variables to the object WITHOUT Julia conversion. This is used by import
#' pipelines where equations need further conversion before Julia translation.
#'
#' Temporary columns that exist in object$variables will be preserved by
#' add_variable_row(). These should be initialized with ctx_init_temp_columns()
#' and cleaned up with ctx_cleanup_temp_columns().
#'
#' Uses add_variable_row() internally.
#'
#' @param ctx Import context
#'
#' @returns Updated context with variables added to object
#' @noRd
#'
ctx_add_variables <- function(ctx) {
  if (length(ctx$variables) == 0) {
    return(ctx)
  }

  # Parameters that add_variable_row() accepts
  valid_params <- colnames(empty_variables())

  # Temporary columns that need to be added manually after add_variable_row()
  temp_cols <- c(
    "eqn_insightmaker", "name_insightmaker", "id_insightmaker"
    # "conveyor", "len"
  )

  # Add temporary columns to object$variables if they don't exist yet
  for (col in temp_cols) {
    if (!col %in% colnames(ctx$object[["variables"]])) {
      ctx$object[["variables"]][[col]] <- rep(NA_character_, nrow(ctx$object[["variables"]]))
    }
  }

  for (elem in ctx$variables) {
    # Copy eqn_insightmaker to eqn if it exists
    if (!is.null(elem[["eqn_insightmaker"]])) {
      elem[["eqn"]] <- elem[["eqn_insightmaker"]]
    }

    # Filter elem to only include valid parameters for add_variable_row()
    elem_for_add <- elem[names(elem) %in% valid_params]

    # Add variable to object using add_variable_row()
    # ctx$object <- do.call(add_variable_row, c(list(object = ctx$object), elem_for_add))
    row <- do.call(get_variable_row, elem_for_add)

    # Add temporary columns to the row
    for (col in temp_cols) {
      if (col %in% names(elem)) {
        row[[col]] <- elem[[col]]
      } else {
        row[[col]] <- NA
      }
    }

    ctx$object[["variables"]] <- bind_rows_(ctx$object[["variables"]], row)
  }

  ctx
}


#' Move variables from context to object using update()
#'
#' After all equation conversions are complete (including Julia conversion),
#' add variables to the object using update(). This does full validation and
#' Julia equation conversion.
#'
#' @param ctx Import context
#'
#' @returns Updated context with variables added to object
#' @noRd
#'
ctx_finalize_variables <- function(ctx) {
  if (length(ctx$variables) == 0) {
    return(ctx)
  }

  allowed_col <- colnames(empty_variables())

  for (elem in ctx$variables) {
    elem_for_build <- elem[names(elem) %in% allowed_col]

    # Add variable to object using update()
    ctx$object <- do.call(update.stockflow, c(list(object = ctx$object), elem_for_build))
  }

  ctx
}


#' Build import_metadata from context
#'
#' Creates the import_metadata structure from information gathered in the context.
#'
#' @param ctx Import context
#'
#' @returns import_metadata list structure
#' @noRd
#'
ctx_build_import_metadata <- function(ctx) {
  create_import_metadata(
    vendor = ctx$vendor,
    file_path = ctx$file_path,
    url = ctx$url,
    raw_model = ctx$raw_model,
    vendor_meta = ctx$vendor_meta,
    original_variables = ctx$original_variables,
    original_macros = ctx$original_macros
  )
}


#' Finalize context and return object
#'
#' Final step: build import_metadata and attach to sfm.
#'
#' @param ctx Import context
#'
#' @returns object with import_metadata attached
#' @noRd
#'
ctx_finalize <- function(ctx) {
  ctx$object[["import_metadata"]] <- ctx_build_import_metadata(ctx)
  ctx$object <- sanitize_stockflow(ctx$object)
  ctx$object
}


#' String formatting with named placeholders
#'
#' @param template Character string
#' @param replacements Named list or character vector of replacements
#' @param fixed Logical, whether to use fixed string matching. Passed to gsub().
#'
#' @returns Character string with placeholders replaced
#' @noRd
fmt <- function(template, replacements, fixed = TRUE) {
  Reduce(
    function(x, i) gsub(names(replacements)[i], replacements[i], x, fixed = fixed),
    seq_along(replacements),
    init = template
  )
}


# ==============================================================================
# INDEX VALIDATION HELPERS (shared by verify and ensemble methods)
# ==============================================================================

#' Validate condition indices
#' @noRd
.check_condition_index <- function(condition, n_conditions) {
  if (length(condition) == 0) {
    cli::cli_abort(c(
      "x" = "Empty {.arg condition} vector.",
      ">" = "Provide at least one condition index."
    ))
  }
  if (!is.numeric(condition)) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg condition} type.",
      "i" = "Got: {.cls {typeof(condition)}}.",
      ">" = "The {.arg condition} argument must be {.cls numeric}."
    ))
  }
  if (any(condition < 1 | condition > n_conditions)) {
    if (n_conditions == 1L) {
      cli::cli_abort(c(
        "x" = "Invalid {.arg condition} index.",
        "i" = "There is only one condition.",
        ">" = "Set {.code condition = 1}."
      ))
    } else {
      cli::cli_abort(c(
        "x" = "Invalid {.arg condition} indices.",
        ">" = "Must be integers between {.val {1}} and {.val {as.numeric(n_conditions)}}."
      ))
    }
  }
  invisible(TRUE)
}


#' Validate simulation indices
#' @noRd
.check_sim_index <- function(sim, n) {
  if (length(sim) == 0) {
    cli::cli_abort(c(
      "x" = "Empty {.arg sim} vector.",
      ">" = "Provide at least one simulation index."
    ))
  }
  if (!is.numeric(sim)) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg sim} type.",
      "i" = "Got: {.cls {typeof(sim)}}.",
      ">" = "The {.arg sim} argument must be {.cls numeric}."
    ))
  }
  if (any(sim < 1 | sim > n)) {
    if (n == 1L) {
      cli::cli_abort(c(
        "x" = "Invalid {.arg sim} index.",
        "i" = "There is only one simulation.",
        ">" = "Set {.code sim = 1}."
      ))
    } else {
      cli::cli_abort(c(
        "x" = "Invalid {.arg sim} indices.",
        ">" = "Must be integers between {.val {1}} and {.val {as.numeric(n)}}."
      ))
    }
  }
  invisible(TRUE)
}


#' @noRd
.check_test_index <- function(test, n_tests) {
  if (length(test) == 0) {
    cli::cli_abort(c(
      "x" = "Empty {.arg test} vector.",
      ">" = "Provide at least one test number."
    ))
  }
  if (!is.numeric(test)) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg test} type.",
      "i" = "Got: {.cls {typeof(test)}}.",
      ">" = "The {.arg test} argument must be {.cls numeric}."
    ))
  }
  if (any(test < 1 | test > n_tests)) {
    if (n_tests == 1L) {
      cli::cli_abort(c(
        "x" = "Invalid {.arg test} index.",
        "i" = "There is only one test.",
        ">" = "Set {.code test = 1}."
      ))
    } else {
      cli::cli_abort(c(
        "x" = "Invalid {.arg test} indices.",
        ">" = "Must be integers between {.val {1}} and {.val {as.numeric(n_tests)}}."
      ))
    }
  }
  invisible(TRUE)
}
