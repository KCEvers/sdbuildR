#' Check if user has internet
#'
#' Internal function
#'
#' @returns Logical value
#'
#' @export
#' @family internal
#' @examples
#' has_internet()
#'
has_internet <- function() {
  tryCatch(
    {
      con <- url("https://www.r-project.org")
      close(con)
      TRUE
    },
    error = function(e) FALSE
  )
}


#' Near equivalent of purrr::compact()
#'
#' @param x
#'
#' @returns List with NULL values removed
#' @noRd
compact_ <- function(x) {
  Filter(Negate(rlang::is_empty), Filter(Negate(is.null), x))
}


#' Safely check whether value is defined
#'
#' @param x Value
#'
#' @return Boolean; whether the value is defined
#' @noRd
is_defined <- function(x) {
  # Safely check whether x is defined
  if (length(x) == 0) {
    return(FALSE)
  } else {
    if (any(is.na(x))) {
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
  result <- list()

  # Helper function to traverse the list
  traverse <- function(x) {
    if (is.list(x)) {
      for (name in names(x)) {
        if (name == entry) {
          if (keep_entry_name) {
            result <<- c(result, stats::setNames(list(x[[name]]), name))
          } else {
            result <<- c(result, x[[name]])
          }
        } else {
          traverse(x[[name]])
        }
      }
    }
  }

  traverse(nested_list)
  return(result)
}


#' Apply purrr::map() and unlist() whilst preserving NULL
#'
#' @param x List
#' @param element_name String, desired name of list
#' @param change_null_to String, what to change NULL to
#'
#' @return Vector
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


#' Ensure length of arg is same as target
#'
#' @param arg Vector
#' @param target Target object to match length of
#'
#' @return arg with same length as target
#' @noRd
#'
ensure_length <- function(arg, target) {
  if (length(arg) != 1 && length(arg) != length(target)) {
    stop(sprintf(
      "The length of %s = %s must be either 1 or equal to the length of %s = %s.",
      deparse(substitute(arg)), paste0(arg, collapse = ", "),
      deparse(substitute(target)), paste0(target, collapse = ", ")
    ))
  } else if (length(arg) < length(target)) {
    arg <- rep(arg, length.out = length(target)) # Repeat to match the target length
  }
  return(arg)
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
#' @param langauge Language
#'
#' @returns Cleaned language
#' @noRd
#'
clean_language <- function(language) {
  language <- trimws(tolower(language))
  if (!language %in% c("r", "julia", "jl")) {
    stop(sprintf("The language %s is not one of the languages available in sdbuildR. The available languages are 'Julia' or 'R'.", language))
  } else {
    language <- stringr::str_to_title(language)
    language <- ifelse(language == "Jl", "Julia", language)
  }
  return(language)
}


#' Clean variable type
#'
#' @inheritParams build
#'
#' @returns Cleaned string or vector
#' @noRd
clean_type <- function(type) {
  if (!(all(is.character(type)))) {
    stop("type must be a character!")
  }

  type <- Filter(nzchar, trimws(tolower(type)))

  # Allow for use of auxiliary instead of aux
  type[type == "auxiliary" | type == "auxiliaries"] <- "aux"

  # Remove trailing s if present
  type <- gsub("s$", "", type)

  type[type == "model_unit"] <- "model_units"

  return(type)
}


#' Clean variable name(s)
#'
#' Clean variable name(s) to create syntactically valid, unique names for use in R and Julia.
#'
#' @param new Vector of names to transform to valid names
#' @param existing Vector of existing names in model
#' @param protected Optional vector of protected names
#'
#' @return Translated names
#' @export
#' @family internal
#' @examples
#' sfm <- xmile("predator_prey")
#' # As the variable name "predator" is already taken, clean_name() will create
#' # an unique name
#' clean_name("predator", as.data.frame(sfm)[["name"]]) # "predator_1"
#'
clean_name <- function(new, existing, protected = c()) {
  # Define protected names: these cannot be used as variable names
  protected_names <- c(
    # Reserved words in R
    "if",
    "else", "repeat", "function", "return", "while", "for", "in", "next", "break", "TRUE", "FALSE", # already protected
    "T", "F",
    # "NULL", "Inf", "NaN", "NA", "NA_integer_", "NA_real_", "NA_complex_", "NA_character_", # already protected
    "time", # used as first variable in simulation dataframe #"Time", "TIME",
    # "constraints",
    # Add Julia keywords
    "baremodule", "begin", "break", "catch", "const", "continue", "do",
    "else", "elseif", "end", "export", "false", "finally",
    "global", "error", "throw",
    "import", "let", "local", "macro", "module", "quote", "return", "struct", "true", "try", "catch", "using",
    "Missing", "missing", "Nothing", "nothing",

    # Add R custom functions
    get_exported_functions("sdbuildR"),

    # Add Julia custom function names
    names(julia_func()),

    # These are variables in the ode and cannot be model element names
    unname(unlist(.sdbuildR_env[["P"]][names(.sdbuildR_env[["P"]]) %in% c(
      "jl_pkg_name", "model_setup_name", "macro_name", "initial_value_name",
      "initial_value_names", "parameter_name", "parameter_names",
      "state_name", "time_name", "change_state_name", "times_name",
      "timestep_name", "saveat_name", "time_units_name", "ensemble_iter",
      "ode_func_name", "callback_func_name", "callback_name", "intermediaries",
      "rootfun_name", "eventfun_name", "convert_u_func", "sdbuildR_units",
      "MyCustomUnits", "init_sdbuildR"
    )])),
    protected,
    as.character(stats::na.omit(existing))
  ) |> unique()

  # stock_names <- names_df[names_df[["type"]] == "stock", "name"]
  # if (length(stock_names) > 0) {
  #   if (all(!is.null(stock_names) & !is.na(stock_names))) {
  #     protected_names <- c(
  #       protected_names,
  #       paste0(.sdbuildR_env[["P"]][["change_prefix"]], stock_names)
  #     )
  #   }
  # }

  # Make syntactically valid and unique names out of character vectors; Insight Maker allows names to be double, so make unique
  new_names <- make.names(c(protected_names, trimws(new)), unique = TRUE)
  # For Julia translation, remove names with a period
  new_names <- stringr::str_replace_all(new_names, "\\.", "_")
  # This may cause overlap in names, so repeat
  new_names <- make.names(new_names, unique = TRUE)
  new_names <- stringr::str_replace_all(new_names, "\\.", "_")
  new_names <- make.names(new_names, unique = TRUE)[-seq_along(protected_names)] # Remove protected names

  # If any names end in a suffix used by sdbuildR, add _
  pattern <- paste0(
    # e.g. names cannot end with _delay[0-9]+$ or _delay[0-9]+_acc[0-9]+$
    .sdbuildR_env[["P"]][["conveyor_suffix"]], "$|", .sdbuildR_env[["P"]][["delay_suffix"]],
    "[0-9]+$|", .sdbuildR_env[["P"]][["past_suffix"]], "[0-9]+$|",
    .sdbuildR_env[["P"]][["fix_suffix"]], "$|",
    .sdbuildR_env[["P"]][["fix_length_suffix"]], "$|",
    .sdbuildR_env[["P"]][["conveyor_suffix"]], "$|",
    .sdbuildR_env[["P"]][["delayN_suffix"]], "[0-9]+",
    .sdbuildR_env[["P"]][["acc_suffix"]], "[0-9]+$|",
    .sdbuildR_env[["P"]][["smoothN_suffix"]], "[0-9]+",
    .sdbuildR_env[["P"]][["acc_suffix"]], "[0-9]+$"
  )

  idx <- grepl(new_names, pattern = pattern)
  new_names[idx] <- paste0(new_names[idx], "_")

  return(new_names)
}


#' Quickly get names of model variables
#'
#' @inheritParams build
#'
#' @noRd
#' @returns Vector with names of model variables
get_model_var <- function(sfm) {
  c(unname(unlist(lapply(sfm[["model"]][["variables"]], names))), names(sfm[["macro"]]))
}


#' Create dataframe with stock-and-flow model variables, types, labels, and units
#'
#' @inheritParams build
#'
#' @return Dataframe
#' @noRd
#'
get_names <- function(sfm) {
  # Return empty dataframe if no variables
  nr_var <- sum(lengths(sfm[["model"]][["variables"]]))
  if (nr_var == 0) {
    names_df <- data.frame(
      type = character(0),
      name = character(0),
      label = character(0),
      units = character(0)
    )
    return(names_df)
  }

  # Building blocks to check
  blocks <- c("stock", "aux", "constant", "flow", "gf")
  entries <- list()

  # Collect variable information
  for (block in blocks) {
    if (!is.null(sfm[["model"]][["variables"]][[block]])) {
      for (var in sfm[["model"]][["variables"]][[block]]) {
        if (!is.null(var[["name"]])) {
          entries[[length(entries) + 1]] <- list(
            type = block,
            name = var[["name"]],
            label = var[["label"]],
            units = var[["units"]]
          )
        }
      }
    }
  }

  # Convert to dataframe
  if (length(entries) > 0) {
    names_df <- do.call(rbind, lapply(entries, as.data.frame, stringsAsFactors = FALSE))
  } else {
    column_names <- c("type", "name", "label", "units")
    names_df <- as.data.frame(matrix(NA, nrow = 1, ncol = length(column_names)))
    colnames(names_df) <- column_names
  }

  # Add macros if any
  if (!is.null(sfm[["macro"]]) && length(names(sfm[["macro"]])) > 0) {
    macro_df <- data.frame(
      type = "macro",
      name = names(sfm[["macro"]]),
      label = names(sfm[["macro"]]),
      units = "",
      stringsAsFactors = FALSE
    )
    names_df <- rbind(names_df, macro_df)
  }

  rownames(names_df) <- NULL
  return(names_df)
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
#' @return Vector with arguments
#' @noRd
#'
parse_args <- function(bracket_arg) {
  # Split arguments by comma; in order to not split arguments which contain a comma (e.g. c(1,2,3)), find all brackets and quotation marks, and don't include commas within these

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

  return(args)
}


#' Sort arguments in function call according to default order
#'
#' @param arg Vector with arguments in strings
#' @param func_name String with name of R function
#' @param default_arg Either NULL or named list of default arguments
#' @inheritParams convert_builtin_functions_julia
#'
#' @noRd
#' @returns List with named and sorted arguments
#'
sort_args <- function(arg, func_name, default_arg = NULL, var_names = NULL) {
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
    }

    # default_arg_Julia = JuliaCall::julia_eval(sprintf("using Distributions; params(%s())", Julia_func))
  }

  # Find names and values of arguments
  contains_name <- stringr::str_detect(arg, "=")
  arg_split <- stringr::str_split_fixed(arg, "=", n = 2)
  names_arg <- trimws(ifelse(contains_name, arg_split[, 1], NA))
  values_arg <- trimws(ifelse(contains_name, arg_split[, 2], arg_split[, 1]))

  # For some functions, there are no default arguments, so there is no need to sort them
  if (length(default_arg) == 0) {
    arg_R <- stats::setNames(values_arg, names_arg)
  } else {
    # Check whether all argument names are in the allowed argument names in case of no dots argument (...)
    idx <- !names_arg %in% names(default_arg) & !is.na(names_arg)
    if (!varargs & any(idx)) {
      stop(paste0(
        "Argument",
        ifelse(sum(idx) > 1, "s ", " "),
        paste0(names_arg[idx], collapse = ", "),
        ifelse(sum(idx) > 1, " are", " is"),
        " not allowed for function ", func_name, "(). Allowed arguments: ",
        paste0(names(default_arg), collapse = ", "), "."
      ))
    }

    # Check if there are too many arguments
    if (!varargs & length(arg) > length(default_arg)) {
      stop(paste0(
        "Too many arguments for function ", func_name, "(). Allowed arguments: ",
        paste0(names(default_arg), collapse = ", "), "."
      ))
    }

    # Add names to unnamed arguments; note that R can mix named and default arguments, e.g. runif(max = 10, 20, min = 1). Julia cannot if they're not keyword arguments!
    idx <- which(!contains_name & nzchar(values_arg)) # Find unnamed arguments which have values
    standard_order <- names(default_arg)
    if (length(idx) > 0 && length(standard_order) > 0) {
      new_names <- setdiff(standard_order, stats::na.omit(names_arg)) # names which are missing from the passed argument names
      names_arg[idx] <- new_names[seq_along(idx)] # Assign new names to unnamed arguments; only select as many as there are unnamed arguments
    }


    # Check for missing obligatory arguments
    # obligatory arguments without a default (class == "name" or is.symbol, e.g. n in formals(rnorm) is a symbol)
    obligatory_args <- unlist(lapply(default_arg, is.symbol))
    idx <- !names(default_arg[obligatory_args]) %in% names_arg

    if (any(idx)) {
      stop(paste0(
        "Obligatory argument",
        ifelse(sum(idx) > 1, "s ", " "),
        paste0(names(default_arg[obligatory_args])[idx], collapse = ", "),
        ifelse(sum(idx) > 1, " are", " is"),
        " missing for function ", func_name, "()."
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
          # arg_R[[name]] <- eval(arg_R_correct_class[[name]], envir = env)

          # Substitute values into the expression
          arg_R[[name]] <- deparse(eval(bquote(substitute(.(arg_R[[name]]), env))))
        }
      }
    }

    # Ensure digits become floats for Julia
    for (name in names(arg_R)) {
      if (!is.null(arg_R[[name]])) {
        arg_R[[name]] <- replace_digits_with_floats(arg_R[[name]], var_names)
      }
    }
  }

  arg_R <- lapply(arg_R, as.character)

  return(arg_R)
}


#' Get start and end indices of each name
#'
#' @param var_names Vector with variable names
#' @param names_with_brackets Boolean; whether to add square bracket around the variable names
#' @inheritParams convert_equations_IM
#'
#' @return Dataframe with start and end indices of each name
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
    # R_names <- paste0("\\b", stringr::str_escape(var_names), "\\b")
    # \\b doesn't match beginning or end of string; \W is non-wodr character; ?: is non-capture group
    R_names <- paste0("(?:^|(?<=\\W))", stringr::str_escape(var_names), "(?=(?:\\W|$))")
    idxs_names <- stringr::str_locate_all(eqn, R_names)

    if (length(unlist(idxs_names)) > 0) {
      # Create indices dataframe with detected variable names
      idxs_df <- as.data.frame(do.call(rbind, idxs_names))
      idxs_df[["name"]] <- rep(original_names, vapply(idxs_names, nrow, numeric(1)))

      # Remove matches in characters
      idxs_exclude <- get_seq_exclude(eqn, type = "quot", names_with_brackets = names_with_brackets)

      if (nrow(idxs_df) > 0) idxs_df <- idxs_df[!(idxs_df[["start"]] %in% idxs_exclude | idxs_df[["end"]] %in% idxs_exclude), ]
    }
  }

  return(idxs_df)
}


#' Get sequence of indices of to exclude
#'
#' @inheritParams convert_equations_IM
#' @inheritParams get_range_all_pairs
#' @inheritParams get_range_names
#'
#' @return Sequence of indices
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
    pair_names <- get_range_names(eqn, var_names, names_with_brackets = names_with_brackets)
    if (nrow(pair_names) > 0) pair_names[["type"]] <- "names"
  }

  comb <- dplyr::bind_rows(pair_quotation_marks, pair_names)

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
#' @return Dataframe with start and end indices of all words as well as extracted words
#' @noRd
#'
get_words <- function(eqn) {
  # An existing function stringr::word() extracts words but treats e.g. "return(a)" as one word
  idxs_word <- stringr::str_locate_all(eqn, "([a-zA-Z_\\.0-9]+)")[[1]] |> as.data.frame()

  if (nrow(idxs_word) > 0) idxs_word[["word"]] <- stringr::str_sub(eqn, idxs_word[["start"]], idxs_word[["end"]])

  return(idxs_word)
}
