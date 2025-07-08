
#' Equivalent of purrr::compact()
#'
#' @param x
#'
#' @returns List with NULL values removed
#' @noRd
compact_ = function(x){
  Filter(Negate(rlang::is_empty), Filter(Negate(is.null), x))
}




#' Safely check whether value is defined
#'
#' @param x Value
#'
#' @return Boolean; whether the value is defined
#' @noRd
is_defined = function(x){

  # Safely check whether x is defined
  if (length(x) == 0){
    return(FALSE)
  } else {
    if (any(is.na(x))){
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
          if (keep_entry_name){
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
get_map = function(x, element_name, change_null_to = ""){

  if (length(x) == 0){
    return(c())
  }

  # x_list = x %>% purrr::map(element_name)
  x_list = lapply(x, `[[`, element_name)
  # Unlist preserving NULL
  x_list[sapply(x_list, function(x){is.null(x) | length(x) == 0 })] = change_null_to
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
    stop(sprintf("The length of %s = %s must be either 1 or equal to the length of %s = %s.",
                 deparse(substitute(arg)), paste0(arg, collapse = ", "),
                 deparse(substitute(target)), paste0(target, collapse = ", ")))
  } else if (length(arg) < length(target)) {
    arg <- rep(arg, length.out = length(target))  # Repeat to match the target length
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
  functions <- exports[sapply(exports, function(x) {
    is.function(get(x, envir = ns))
  })]

  # Return sorted for consistency
  sort(functions)
}
