#' Check if user has internet
#'
#' Internal function
#'
#' @returns Logical value
#'
#' @export
#' @family internal
#' @examplesIf has_internet()
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



#' Check if on CRAN
#'
#' @returns Logical value
#' @export
#' @family internal
#'
#' @examples
#' not_on_cran()
not_on_cran <- function() {
  env <- Sys.getenv("NOT_CRAN")
  if (identical(env, "")) {
    interactive()
  } else {
    isTRUE(as.logical(env))
  }
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
clean_language = function(language){
  language <- trimws(tolower(language))
  if (!language %in% c("r", "julia", "jl")) {
    stop(sprintf("The language %s is not one of the languages available in sdbuildR. The available languages are 'Julia' or 'R'.", language))
  } else {
    language <- stringr::str_to_title(language)
    language <- ifelse(language == "Jl", "Julia", language)
  }
  return(language)
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

