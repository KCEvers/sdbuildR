#' Create a new stock-and-flow model
#'
#' Initialize an empty stock-and-flow model of class sdbuildR_xmile. You can
#' either create an empty stock-and-flow model or load a template from the model
#' library.
#'
#' Do not edit the object manually; this will likely lead to errors downstream.
#' Rather, use [header()], [sim_specs()], [build()], [macro()], and
#' [model_units()] for safe manipulation.
#'
#' @param name Name of the template to load. If NULL, an empty stock-and-flow
#' model will be created with default simulation parameters and a default header.
#' If specified, the name should be one of "logistic_model", "SIR",
#' "predator-prey", "Crielaard2022", "coffee_cup", "bank_account", "Lorenz",
#' "Rossler", "vanderPol", "Duffing", "Chua", "spruce_budworm".
#'
#' @return Stock-and-flow model of class sdbuildR_xmile. Its structure is based
#'  on [XML Interchange Language for System Dynamics (XMILE)](https://docs.oasis-open.org/xmile/xmile/v1.0/os/xmile-v1.0-os.html). It is a nested list, containing:
#' \describe{
#'  \item{header}{Meta-information about model. A list containing arguments listed in [header()].}
#'  \item{sim_specs}{Simulation specifications. A list containing arguments listed in [sim_specs()].}
#'  \item{model}{Model variables. A nested list containing arguments listed in [build()].}
#'  \item{macro}{Global variable or functions. A list containing arguments listed in [macro()].}
#'  \item{model_units}{Custom model units. A list containing arguments listed in [model_units()].}
#'  }
#'
#' @export
#' @family build
#' @seealso [build()]
#'
#' @examples sfm <- xmile()
#' summary(sfm)
#'
#' \dontshow{
#' sfm <- sim_specs(sfm, save_at = 1)
#' }
#'
#' # Load a template
#' sfm <- xmile("Lorenz")
#' sim <- simulate(sfm)
#' plot(sim)
xmile <- function(name = NULL) {
  if (!is.null(name)) {
    return(template(name))
  }

  sfm <- new_sdbuildR_xmile()
  return(sfm)
}


#' Create new object of class sdbuildR_xmile
#'
#' @return Stock-and-flow model of class [`xmile`][sdbuildR_xmile]
#' @noRd
#'
new_sdbuildR_xmile <- function() {
  header_defaults <- as.list(formals(header))
  header_defaults <- header_defaults[!names(header_defaults) %in%
    c("sfm", "...")]
  header_defaults[["created"]] <- Sys.time() # manually overwrite

  spec_defaults <- as.list(formals(sim_specs))
  spec_defaults <- spec_defaults[!names(spec_defaults) %in% c("sfm", "...")]

  # Manually overwrite these as the defaults of save_at and save_from are
  # defined in terms of other variables
  spec_defaults[["save_at"]] <- spec_defaults[["dt"]]
  spec_defaults[["save_from"]] <- spec_defaults[["start"]]

  # Create list
  obj <- list(
    header = header_defaults,
    sim_specs = spec_defaults,
    model = list(
      variables = list(
        stock = list(),
        constant = list(),
        aux = list(),
        flow = list(),
        gf = list()
      )
    ),
    macro = list(),
    model_units = list()
  )

  sfm <- structure(obj, class = "sdbuildR_xmile")
  sfm <- validate_xmile(sfm)
  return(sfm)
}


#' Get the sources and destinations of flows
#'
#' @inheritParams build
#'
#' @returns Dataframe with for each flow which stock and flow to and/or from
#' @noRd
get_flow_df <- function(sfm) {
  check_xmile(sfm)

  flow_to <- get_map(sfm[["model"]][["variables"]][["flow"]], "to")
  flow_from <- get_map(sfm[["model"]][["variables"]][["flow"]], "from")

  dplyr::bind_cols(
    name = names(flow_to),
    to = unname(flow_to),
    from = unname(flow_from)
  ) |> as.data.frame()
}






#' Create dataframe of simulation results
#'
#' Convert simulation results to a dataframe. The first column is time, followed by all stocks, and then all other auxiliary and flow variables.
#'
#' @inheritParams plot.sdbuildR_sim
#' @param direction Format of dataframe, either "long" (default) or "wide".
#' @param row.names NULL or a character vector giving the row names for the data frame. Missing values are not allowed.
#' @param optional Ignored parameter.
#'
#' @returns Dataframe with simulation results
#' @export
#' @seealso [simulate()], [xmile()]
#' @family build
#' @method as.data.frame sdbuildR_sim
#'
#' @examples
#' sfm <- xmile("SIR")
#' sim <- simulate(sfm)
#' head(as.data.frame(sim))
#'
as.data.frame.sdbuildR_sim <- function(x,
                                       row.names = NULL, optional = FALSE,
                                       direction = "long", ...) {
  # Check whether it is an xmile object
  if (!inherits(x, "sdbuildR_sim")) {
    stop("This is not an object of class sdbuildR_sim! Simulate a stock-and-flow model with simulate().")
  }

  direction <- trimws(tolower(direction))
  if (direction != "long" & direction != "wide") {
    stop("direction should either be \"long\" or \"wide\"!")
  }

  if (direction == "long") {
    df <- x[["df"]]
  } else if (direction == "wide") {
    df <- stats::reshape(x[["df"]],
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
      stop("Length of row.names (", length(row.names), ") does not match number of rows (", nrow(df), ")")
    }
    rownames(df) <- row.names
  }

  return(df)
}


#' Find longest regex match
#'
#' @param x Value
#' @param regex_units Regex units dictionary
#'
#' @returns Longest cleaned regex match
#' @noRd
find_matching_regex <- function(x, regex_units) {
  matches <- names(regex_units[regex_units == x])

  # Clean regex and select longest match
  matches <- sub("\\$$", "", sub("^\\^", "", matches))
  matches <- sub("\\[s\\]\\?", "s", matches)

  matches <- unique(tolower(stringr::str_replace_all(matches,
             "\\[([a-zA-Z])\\|([a-zA-Z])\\]", "\\1")))
  matches[which.max(nchar(matches))] # Return longest match
}


#' Get delayN and smoothN from stock-and-flow model
#'
#' @inheritParams build
#'
#' @returns List with delayN and smoothN functions
#' @noRd
get_delayN_smoothN <- function(sfm) {
  z <- unlist(unname(sfm[["model"]][["variables"]]), recursive = FALSE,
              use.names = TRUE)
  z <- lapply(z, function(x) {
    c(x[["func"]][["delayN"]], x[["func"]][["smoothN"]])
  })
  z <- z[lengths(z) > 0]
  return(z)
}


#' Get delay and past from stock-and-flow model
#'
#' @inheritParams build
#'
#' @returns List with delay and past functions
#' @noRd
get_delay_past <- function(sfm) {
  z <- unlist(unname(sfm[["model"]][["variables"]]), recursive = FALSE, use.names = TRUE)
  z <- lapply(z, function(x) {
    c(x[["func"]][["delay"]], x[["func"]][["past"]])
  })
  z <- z[lengths(z) > 0]
  return(z)
}




#' Check whether object is of class sdbuildR_xmile
#'
#' @inheritParams build
#'
#' @returns NULL
#' @noRd
check_xmile <- function(sfm) {
  # Check whether it is an xmile object
  if (!inherits(sfm, "sdbuildR_xmile")) {
    stop("This is not an object of class sdbuildR_xmile! Create a stock-and-flow model with xmile() or insightmaker_to_sfm().")
  }
}


#' Validate sdbuildR_xmile class
#'
#' Internally used function to ensure that the stock-and-flow model is valid and contains all necessary properties.
#'
#' @inheritParams build
#'
#' @return Validated stock-and-flow model of class sdbuildR_xmile
#' @noRd
#'
validate_xmile <- function(sfm) {
  check_xmile(sfm)

  # Ensure model units have default properties
  defaults <- as.list(formals(model_units))
  defaults <- defaults[!names(defaults) %in% c("sfm", "name", "erase",
                                               "change_name")]
  sfm[["model_units"]] <- lapply(sfm[["model_units"]], function(x) {
    x[["prefix"]] <- FALSE

    # Merge with defaults
    utils::modifyList(defaults, x)
  })

  # Ensure names are the same as names properties
  names(sfm[["model_units"]]) <- unname(unlist(lapply(sfm[["model_units"]],
                                                      `[[`, "name")))


  # No need to validate model variables if there are no variables
  nr_var <- sum(lengths(sfm[["model"]][["variables"]]))
  if (nr_var > 0) {
    # Make sure name property matches with the name of the list entry
    type_names <- names(sfm[["model"]][["variables"]])
    sfm[["model"]][["variables"]] <- lapply(
      seq_along(sfm[["model"]][["variables"]]),
      function(i) {
        x <- sfm[["model"]][["variables"]][[i]]

        if (length(x) == 0) {
          x <- list()
        } else {
          var_names <- unname(unlist(lapply(x, `[[`, "name")))
          x <- stats::setNames(x, var_names)

          # Make sure the type matches
          type <- type_names[i]
          x <- lapply(x, function(y) {
            y[["type"]] <- type
            return(y)
          })
        }
        return(x)
      }
    )
    names(sfm[["model"]][["variables"]]) <- type_names

    # Ensure each variable has the necessary properties for its building block;
    # otherwise, add defaults.
    keep_prop <- get_building_block_prop()
    defaults <- as.list(formals(build))
    defaults <- defaults[!names(defaults) %in% c(
      "sfm", "name", "type", "label", "erase",
      "change_name", "change_type", "..."
    )]

    # Process variables
    type_names <- names(sfm[["model"]][["variables"]])
    sfm[["model"]][["variables"]] <- lapply(names(sfm[["model"]][["variables"]]),
                                            function(type) {
      vars <- sfm[["model"]][["variables"]][[type]]
      # Pre-compute type-specific defaults
      type_defaults <- defaults[names(defaults) %in% keep_prop[[type]]]

      lapply(vars, function(y) {
        # Add label, eqn, eqn_julia if missing
        if (is.null(y[["label"]])) y[["label"]] <- y[["name"]]
        if (is.null(y[["eqn"]])) y[["eqn"]] <- "0.0"
        if (is.null(y[["eqn_julia"]])) y[["eqn_julia"]] <- "0.0"

        # Merge with type-specific defaults
        utils::modifyList(type_defaults, y)
      })
    })
    names(sfm[["model"]][["variables"]]) <- type_names # Preserve names

    # Ensure to and from in flows are only referring to stocks
    names_df <- get_names(sfm)
    stock_names <- names_df[names_df[["type"]] == "stock", "name"]
    nonstock_names <- names_df[names_df[["type"]] != "stock", "name"]

    sfm[["model"]][["variables"]][["flow"]] <- lapply(
      sfm[["model"]][["variables"]][["flow"]], function(x) {
      if (is_defined(x[["from"]])) {

        # If from is not in stocks but is another variable, remove
        non_stocks <- x[["from"]][!x[["from"]] %in% stock_names &
                                    x[["from"]] %in% nonstock_names]
        if (length(non_stocks) > 0) {
          warning(paste0(x[["name"]],
                         " is flowing from a variable which is not a stock (",
                         paste0(non_stocks, collapse = ", "), ")! Removing ",
                         paste0(non_stocks, collapse = ", "), " from `from`..."))
          x[["from"]] <- intersect(x[["from"]], stock_names)
          if (length(x[["from"]]) == 0) {
            x[["from"]] <- ""
          }
        }
      }

      if (is_defined(x[["to"]])) {

        # If to is not in stocks but is another variable, remove
        non_stocks <- x[["to"]][!x[["to"]] %in% stock_names &
                                  x[["to"]] %in% nonstock_names]
        if (length(non_stocks) > 0) {
          warning(paste0(x[["name"]],
                         " is flowing to a variable which is not a stock (",
                         paste0(non_stocks, collapse = ", "), ")! Removing ",
                         paste0(non_stocks, collapse = ", "), " from `to`..."))
          x[["to"]] <- intersect(x[["to"]], stock_names)
          if (length(x[["to"]]) == 0) {
            x[["to"]] <- ""
          }
        }
      }

      # Ensure that to and from are not the same
      if (is_defined(x[["to"]]) && is_defined(x[["from"]]) &&
          x[["to"]] == x[["from"]]) {
        message(paste0(x[["name"]],
                       " is flowing to and from the same variable (",
                       x[["to"]], ")! Removing `from`..."))
        x[["from"]] <- ""
      }

      return(x)
    })
  }

  # Ensure macros have default properties
  defaults <- as.list(formals(macro))
  defaults <- defaults[!names(defaults) %in% c("sfm", "name", "erase",
                                               "change_name")]
  sfm[["macro"]] <- lapply(sfm[["macro"]], function(x) {
    if (is.null(x[["eqn"]])) x[["eqn"]] <- "0.0"
    if (is.null(x[["eqn_julia"]])) x[["eqn_julia"]] <- "0.0"

    # Merge with defaults
    utils::modifyList(defaults, x)
  })

  # Ensure names are the same as names properties
  names(sfm[["macro"]]) <- unname(unlist(lapply(sfm[["macro"]], `[[`, "name")))

  # To prevent downstream errors, don't:
  # - add inflows and outflows to stocks

  # To prevent massively slowing down code, don't:
  # - translate all equations to Julia here
  # - detect undefined units

  return(sfm)
}



#' Switch names and values of list, handling different lengths in entries
#'
#' @param x List
#' @return List
#' @noRd
switch_list <- function(x) {
  # Switch names and values
  new_list <- unlist(lapply(names(x), function(name) {
    stats::setNames(rep(name, length(x[[name]])), x[[name]])
  }), recursive = FALSE)

  return(as.list(new_list))
}



#' Create, modify or remove custom units
#'
#' A large library of units already exists, but you may want to define your own custom units. Use `model_units()` to add, change, or erase custom units from a stock-and-flow model. Custom units may be new base units, or may be defined in terms of other (custom) units. See `?u()` for more information on the rules of specifying units. Note that units are only supported in Julia, not in R.
#'
#' @inheritParams build
#' @param name Name of unit. A character vector.
#' @param eqn Definition of unit. String or vector of unit definitions. Defaults to "1" to indicate a base unit not defined in terms of other units.
#' @param doc Documentation of unit.
#' @param erase If TRUE, remove model unit from the model. Defaults to FALSE.
#' @param change_name New name for model unit. Defaults to NULL to indicate no change.
#'
#' @return The modified stock-and-flow object with the specified unit(s) added or removed.
#'
#' @export
#' @family units
#' @seealso [unit_prefixes()]
#'
#' @examples
#' sfm <- xmile("Crielaard2022")
#' sfm <- model_units(sfm, "BMI", eqn = "kg/m^2", doc = "Body Mass Index")
#'
#' # You may also use words rather than symbols for the unit definition.
#' # The following modifies the unit BMI:
#' sfm <- model_units(sfm, "BMI", eqn = "kilogram/meters^2")
#'
#' # Remove unit:
#' sfm <- model_units(sfm, "BMI", erase = TRUE)
#'
#' # Unit names may be changed to be syntactically valid and avoid overlap:
#' sfm <- model_units(xmile(), "C0^2")
#'
model_units <- function(sfm, name, eqn = "1", doc = "",
                        erase = FALSE, change_name = NULL) {
  # Basic check
  if (missing(sfm)) {
    stop("No model specified!")
  }

  if (missing(name)) {
    stop("name must be specified!")
  }

  check_xmile(sfm)

  idx_nonexist <- which(!name %in% names(sfm[["model_units"]]))

  # Remove unit from model
  if (erase) {
    if (length(idx_nonexist) == 0) {
      sfm[["model_units"]][name] <- NULL
    } else {
      stop(paste0(
        paste0(name[idx_nonexist], collapse = ", "),
        ifelse(length(idx_nonexist) == 1,
          " does not exist as a custom unit!",
          " do not exist as custom units!"
        ),
        ifelse(length(sfm[["model_units"]]) > 0,
          paste0(
            "\nExisting model units: ",
            paste0(names(sfm[["model_units"]]), collapse = ", ")
          ),
          "Your model has no custom units."
        )
      ))
    }
  } else {
    # Change units to units valid for Julia's Unitful package
    regex_units <- get_regex_units()

    if (!is.null(change_name)) {
      if (length(name) > 1 | length(change_name) > 1) {
        stop("You can only change the name of one custom unit at a time.")
      }

      old_name <- name
      chosen_name <- change_name
    } else {
      chosen_name <- name
    }

    name <- vapply(chosen_name, function(x) {
      clean_unit(x, regex_units, unit_name = TRUE)
    }, character(1), USE.NAMES = FALSE)

    # Keep existing names the same
    name[!idx_nonexist] <- chosen_name[!idx_nonexist]

    idx_changed <- name != chosen_name

    # Check if unit already exists in unit package.
    # Default units cannot be overwritten
    name_in_units <- name %in% unname(regex_units)

    if (any(name_in_units)) {
      stop(sprintf(
        "The custom unit name%s %s match%s the standard unit%s %s, which cannot be overwritten.\nPlease choose %sunique name%s for: %s ",
        ifelse(sum(name_in_units) > 1, "s", ""),
        paste0(chosen_name[name_in_units], collapse = ", "),
        ifelse(sum(name_in_units) > 1, "", "es"),
        ifelse(sum(name_in_units) > 1, "s", ""),
        paste0(name[name_in_units], collapse = ", "),
        ifelse(sum(name_in_units) > 1, "", "a "),
        ifelse(sum(name_in_units) > 1, "s", ""),
        paste0(chosen_name[name_in_units], collapse = ", ")
      ))
    }

    # Check if all unit names contain at least one letter or digit
    idx_invalid <- !grepl("[a-zA-Z0-9]", name)

    if (any(idx_invalid)) {
      stop(sprintf(
        "Each custom unit name needs at least one letter or number.\nPlease choose %sdifferent name%s for: %s ",
        ifelse(sum(name_in_units) > 1, "", "a "),
        ifelse(sum(name_in_units) > 1, "s", ""),
        paste0(chosen_name[idx_invalid], collapse = ", ")
      ))
    }

    if (any(idx_changed)) {
      warning(sprintf(
        "The custom unit name%s %s %s modified to %s to comply with Julia's syntactic rules.\nUse sfm |> model_units('old_name', change_name = 'new_name') to update the name%s in your model.",
        ifelse(sum(idx_changed) > 1, "s", ""),
        paste0(chosen_name[idx_changed], collapse = ", "),
        ifelse(sum(idx_changed) > 1, "were", "was"),
        paste0(name[idx_changed], collapse = ", "),
        ifelse(sum(idx_changed) > 1, "s", "")
      ))
    }


    if (!is.null(change_name)) {
      # Check if name is already in use
      unit_exists <- name %in% setdiff(names(sfm[["model_units"]]), old_name)

      if (!unit_exists) {
        sfm[["model_units"]][name] <- sfm[["model_units"]][old_name]
        sfm[["model_units"]][old_name] <- NULL

        # Ensure the unit is translated in the entire model
        dict <- stats::setNames(name, paste0("^", old_name, "$"))

        sfm[["model_units"]] <- lapply(
          sfm[["model_units"]],
          function(x) {
            if (is_defined(x[["eqn"]])) {
              x[["eqn"]] <- clean_unit(x[["eqn"]], dict)
            }
            return(x)
          }
        )

        var_names <- get_model_var(sfm)
        sfm[["model"]][["variables"]] <- lapply(
          sfm[["model"]][["variables"]],
          function(y) {
            lapply(y, function(x) {
              if (is_defined(x[["units"]])) {
                x[["units"]] <- clean_unit(x[["units"]], dict)
              }

              if (is_defined(x[["eqn"]])) {
                old_eqn <- x[["eqn"]]
                x[["eqn"]] <- clean_unit_in_u(x[["eqn"]], dict)

                # If equation changed, redo Julia translation
                if (old_eqn != x[["eqn"]]) {
                  x[["eqn_julia"]] <- convert_equations_julia(sfm, x[["type"]],
                    x[["name"]], x[["eqn"]],
                    var_names,
                    regex_units = dict
                  )
                }
              }
              return(x)
            })
          }
        )
      } else {
        stop(sprintf(
          "%s already exists as a custom unit! Choose a different new name for %s",
          name, old_name
        ))
      }
    }

    # Get names of passed arguments
    passed_arg <- names(as.list(match.call())[-1]) |>
      # Remove some arguments
      setdiff(c("sfm", "erase", "change_name"))
    argg <- list()
    argg[["name"]] <- name

    if ("eqn" %in% passed_arg) {
      eqn <- vapply(eqn, clean_unit, character(1), regex_units, USE.NAMES = FALSE)
      eqn <- ensure_length(eqn, name)
      argg[["eqn"]] <- eqn
    }

    if ("doc" %in% passed_arg) {
      doc <- ensure_length(doc, name)
      argg[["doc"]] <- doc
    }

    new_units <- stats::setNames(purrr::transpose(argg), name)

    # Add units to model (in for-loop, as otherwise not all elements are added or overwritten)
    for (i in seq_along(name)) {
      sfm[["model_units"]] <- utils::modifyList(sfm[["model_units"]], new_units[i])
    }
  }

  sfm <- validate_xmile(sfm)

  return(sfm)
}




#' Create, modify or remove a global variable or function
#'
#' Macros are global variables or functions that can be used throughout your stock-and-flow model. `macro()` adds, changes, or erases a macro.
#'
#' @inheritParams build
#' @param name Name of the macro. The equation will be assigned to this name.
#' @param eqn Equation of the macro. A character vector. Defaults to "0.0".
#' @param doc Documentation of the macro. Defaults to "".
#' @param change_name New name for macro (optional). Defaults to NULL to indicate no change.
#' @param erase If TRUE, remove macro from the model. Defaults to FALSE.
#'
#' @return Updated stock-and-flow model
#' @family build
#' @export
#'
#' @examples
#' # If the logistic() function did not exist, you could create it yourself:
#' sfm <- macro(xmile(), "func", eqn = "function(x, slope = 1, midpoint = .5){
#'    1 / (1 + exp(-slope*(x-midpoint)))
#'  }")
#'
macro <- function(sfm, name, eqn = "0.0", doc = "", change_name = NULL, erase = FALSE) {
  # Basic check
  if (missing(sfm)) {
    stop("No model specified!")
  }

  check_xmile(sfm)

  if (missing(name)) {
    stop("name must be specified!")
  }

  # Check change name of variable
  if (!is.null(change_name)) {
    if (length(change_name) > 1 | length(name) > 1) {
      stop("You can only change the name of one variable at a time!")
    }
  }

  passed_arg <- names(as.list(match.call())[-1]) |>
    # Remove some arguments
    setdiff(c("sfm", "erase", "change_name"))
  argg <- list()

  names_df <- get_names(sfm)
  var_names <- get_model_var(sfm)

  idx_exist <- name %in% names(sfm[["macro"]])

  if (erase) {
    if (any(!idx_exist) == 0) {
      sfm[["macro"]][name] <- NULL
    } else {
      stop(sprintf(
        "%s do%s not exist as %scustom macro%s! %s",
        paste0(name[!idx_exist], collapse = ", "),
        ifelse(length(name[!idx_exist]) > 1, "", "es"),
        ifelse(length(name[!idx_exist]) > 1, "", "a "),
        ifelse(length(name[!idx_exist]) > 1, "s", ""),
        ifelse(length(sfm[["macro"]]) > 0, paste0(
          "Existing macros: ",
          paste0(names(sfm[["macro"]]), collapse = ", ")
        ),
        "Your model has no custom macros."
        )
      ))
    }
  } else {
    # If overwriting name with change_name
    if (!is.null(change_name)) {
      # Ensure new name is syntactically valid
      chosen_new_name <- change_name
      change_name <- create_R_names(change_name, names_df)
      report_name_change(chosen_new_name, change_name)

      # Overwrite name
      macro_names <- names(sfm[["macro"]])
      macro_names[macro_names == name] <- change_name
      names(sfm[["macro"]]) <- macro_names
      sfm[["macro"]][[change_name]][["name"]] <- change_name

      # Replace references to name with change_name everywhere
      sfm[["model"]][["variables"]] <- lapply(sfm[["model"]][["variables"]], function(y) {
        lapply(y, function(x) {
          if (is_defined(x[["eqn"]])) {
            idx_df <- get_range_names(x[["eqn"]], name, names_with_brackets = FALSE)
            if (nrow(idx_df) > 0) {
              # Reverse indices to replace correctly
              for (i in rev(seq_len(nrow(idx_df)))) {
                stringr::str_sub(x[["eqn"]], idx_df[i, "start"], idx_df[i, "end"]) <- change_name
              }

              # Update Julia translation
              idx_df <- get_range_names(x[["eqn_julia"]], name, names_with_brackets = FALSE)
              if (nrow(idx_df) > 0) {
                # Reverse indices to replace correctly
                for (i in rev(seq_len(nrow(idx_df)))) {
                  stringr::str_sub(x[["eqn_julia"]], idx_df[i, "start"], idx_df[i, "end"]) <- change_name
                }
              }
            }
          }
          return(x)
        })
      })

      name <- change_name

      # Redo equation (below)
      if (!"eqn" %in% passed_arg) {
        eqn <- sfm[["macro"]][[name]][["eqn"]]
        passed_arg <- c(passed_arg, "eqn")
      }

      # Update
      var_names <- get_model_var(sfm)
      idx_exist <- name %in% names(sfm[["macro"]])
    }

    # Ensure names are valid of new variables
    if (any(!idx_exist)) {
      # Create syntactically valid, unique names (this also avoids overlap with previous names, but we stopped the function already if this is the case)
      new_names <- create_R_names(name[!idx_exist], names_df)

      # Warning if specified name changed
      report_name_change(name[!idx_exist], new_names)

      # Change name
      name[!idx_exist] <- new_names
    }


    if ("eqn" %in% passed_arg) {
      regex_units <- get_regex_units()

      if (any(is.null(eqn))) {
        warning("Equation cannot be NULL! Setting empty equations to 0...")
        eqn[is.null(eqn)] <- "0.0"
      }

      if (any(!nzchar(eqn))) {
        warning("Equation cannot be empty! Setting empty equations to 0...")
        eqn[!nzchar(eqn)] <- "0.0"
      }

      # Change all equations to characters
      if (!is.null(eqn)) {
        eqn <- as.character(eqn)
      }

      # Ensure units are cleaned in u() in eqn
      eqn <- clean_unit_in_u(eqn, regex_units)
      eqn <- ensure_length(eqn, name)

      # Convert equation to Julia
      eqn_julia <- vapply(seq_along(name), function(i) {
        # Assign name already to convert functions correctly
        x <- paste0(name[i], " = ", eqn[i])

        convert_equations_julia(sfm,
          type = "macro", name = name[i], eqn = x, var_names = var_names,
          regex_units = regex_units
        )[["eqn_julia"]]
        # No need to save $func because delay family cannot be used for macros
      }, character(1), USE.NAMES = FALSE)

      argg[["eqn"]] <- eqn
      argg[["eqn_julia"]] <- eqn_julia
    }

    argg[["name"]] <- name

    if ("doc" %in% passed_arg) {
      doc <- ensure_length(doc, name)
      argg[["doc"]] <- doc
    }

    new_macros <- purrr::transpose(argg) |> stats::setNames(name)

    # Add elements to model (in for-loop, as otherwise not all elements are added or overwritten)
    for (i in seq_along(name)) {
      sfm[["macro"]] <- utils::modifyList(sfm[["macro"]], new_macros[i])
    }
  }

  sfm <- validate_xmile(sfm)

  return(sfm)
}


#' Modify header of stock-and-flow model
#'
#' The header of a stock-and-flow model contains metadata about the model, such as the name, author, and version. Modify the header of an existing model with standard or custom properties.
#'
#' @inheritParams build
#' @param name Model name. Defaults to "My Model".
#' @param caption Model description. Defaults to "My Model Description".
#' @param created Date the model was created. Defaults to Sys.time().
#' @param author Creator of the model. Defaults to "Me".
#' @param version Model version. Defaults to "1.0".
#' @param URL URL associated with model. Defaults to "".
#' @param doi DOI associated with the model. Defaults to "".
#' @param ... Optional other entries to add to the header.
#'
#' @return Updated stock-and-flow model.
#' @family build
#' @export
#'
#' @examples
#' sfm <- xmile() |>
#'   header(
#'     name = "My first model",
#'     caption = "This is my first model",
#'     author = "Kyra Evers",
#'     version = "1.1"
#'   )
header <- function(sfm, name = "My Model", caption = "My Model Description",
                   created = Sys.time(), author = "Me", version = "1.0", URL = "", doi = "", ...) {
  # Basic check
  if (missing(sfm)) {
    stop("No model specified!")
  }

  check_xmile(sfm)

  # Get names of passed arguments
  passed_arg <- names(as.list(match.call())[-1]) |>
    # Remove some arguments
    setdiff(c("sfm", "..."))

  # Collect all arguments
  argg <- c(
    as.list(environment()),
    list(...)
  )[unique(passed_arg)]

  sfm[["header"]] <- utils::modifyList(sfm[["header"]], argg)

  sfm <- validate_xmile(sfm)

  return(sfm)
}



#' Modify simulation specifications
#'
#' Simulation specifications are the settings that determine how the model is simulated, such as the integration method (i.e. solver), start and stop time, and timestep. Modify these specifications for an existing stock-and-flow model.
#'
#' @inheritParams build
#' @param method Integration method. Defaults to "euler".
#' @param start Start time of simulation. Defaults to 0.
#' @param stop End time of simulation. Defaults to 100.
#' @param dt Timestep of solver. Defaults to 0.01.
#' @param save_at Timestep at which to save computed values. Defaults to dt.
#' @param save_from Time to at which to start saving computed values. Defaults to start. Set to a larger time than start to store less data.
#' @param seed Seed number to ensure reproducibility across runs in case of random elements. Must be an integer. Defaults to NULL (no seed).
#' @param time_units Simulation time unit, e.g. 's' (second). Defaults to "s".
#' @param language Coding language in which to simulate model. Either "R" or "Julia". Julia is necessary for using units or delay functions. Defaults to "R".
#'
#' @return Updated stock-and-flow model with new simulation specifications
#' @family simulate
#' @seealso [solvers()]
#' @export
#'
#' @examples
#' sfm <- xmile("predator-prey") |>
#'   sim_specs(start = 0, stop = 50, dt = 0.1)
#' sim <- simulate(sfm)
#' plot(sim)
#'
#' # Change the simulation method to "rk4"
#' sfm <- sim_specs(sfm, method = "rk4")
#'
#' # Change the time units to "years", such that one time unit is one year
#' sfm <- sim_specs(sfm, time_units = "years")
#'
#' # To save storage but not affect accuracy, use save_at and save_from
#' sfm <- sim_specs(sfm, save_at = 1, save_from = 10)
#' sim <- simulate(sfm)
#' head(as.data.frame(sim))
#'
#' # Add stochastic initial condition but specify seed to obtain same result
#' sfm <- sim_specs(sfm, seed = 1) |>
#'   build(c("predator", "prey"), eqn = "runif(1, 20, 50)")
#'
#' # Change the simulation language to Julia to use units
#' sfm <- sim_specs(sfm, language = "Julia")
#'
sim_specs <- function(sfm,
                      method = "euler",
                      start = "0.0",
                      stop = "100.0",
                      dt = "0.01",
                      save_at = dt,
                      save_from = start,
                      # adaptive = FALSE,
                      seed = NULL,
                      time_units = "s",
                      language = "R") {
  # Basic check
  if (missing(sfm)) {
    stop("No model specified!")
  }

  check_xmile(sfm)

  # Get names of passed arguments
  passed_arg <- names(as.list(match.call())[-1]) |>
    # Remove some arguments
    setdiff(c("sfm"))

  if (!missing(start)) {
    start <- suppressWarnings(as.numeric(start))
    if (is.na(start)) {
      stop("Start time must be a number!")
    }
  }

  if (!missing(stop)) {
    stop <- suppressWarnings(as.numeric(stop))
    if (is.na(stop)) {
      stop("Stop time must be a number!")
    }
  }

  if (!missing(dt)) {
    dt <- suppressWarnings(as.numeric(dt))
    if (is.na(dt)) {
      stop("dt must be a number!")
    }

    if (dt != 1) {
      if (dt > .1) {
        warning(paste0("dt is larger than 0.1! This will likely lead to inaccuracies in the simulation. To reduce the size of the simulaton dataframe, use save_at = ", dt, ", and keep dt to a smaller value. To simulate in discrete time, set dt = 1."))
      }
    }
  }

  if (!missing(save_at)) {
    save_at <- suppressWarnings(as.numeric(save_at))
    if (is.na(save_at)) {
      stop("save_at must be a number!")
    }
  }

  if (!missing(save_from)) {
    save_from <- suppressWarnings(as.numeric(save_from))
    if (is.na(save_from)) {
      stop("save_from must be a vector of two numbers!")
    }
  }

  # Ensure time_units are formatted correctly
  if (!missing(time_units)) {
    if (length(time_units) != 1) {
      stop("time_units must be a single string!")
    }

    # Time units can only contain letters or spaces
    if (any(grepl("[^a-zA-Z _]", time_units))) {
      stop("time_units can only contain letters, spaces, or underscores!")
    }
    regex_time_units <- get_regex_time_units()
    time_units <- clean_unit(time_units, regex_time_units) # Units are not used in R, so translate to julia directly

    if (!any(time_units == unname(regex_time_units))) {
      stop(sprintf("The time unit %s is not one of the time units available in sdbuildR. The available time units are: %s", time_units, paste0(unique(unname(regex_time_units)), collapse = ", ")))
    }
  }


  if ("method" %in% passed_arg) {
    method <- trimws(method)
  }

  # Check coding language
  if ("language" %in% passed_arg) {
    language <- clean_language(language)

    # Translate method if method was not specified
    old_language <- sfm[["sim_specs"]][["language"]]
    if (!"method" %in% passed_arg & language != old_language) {
      method <- solvers(sfm[["sim_specs"]][["method"]],
        from = old_language, to = language,
        show_info = TRUE
      )

      if (is.null(method[["translation"]])) {
        method <- method[["alternatives"]][1]
      } else {
        method <- method[["translation"]]
      }
      passed_arg <- c(passed_arg, "method")
    } else if ("method" %in% passed_arg) {
      # If method was specified, check whether it is a valid method in the new coding language
      method <- solvers(method, from = language, show_info = TRUE)
      method <- method[["name"]]
    }
  } else if ("method" %in% passed_arg) {
    # If language was not specified but methods were, check method
    language <- sfm[["sim_specs"]][["language"]]
    method <- solvers(method, from = language, show_info = TRUE)
    method <- method[["name"]]
  }

  # Check whether start is smaller than stop
  if ("start" %in% passed_arg) {
    if (!"stop" %in% passed_arg) {
      stop <- as.numeric(sfm[["sim_specs"]][["stop"]])
    }
    if (start >= stop) {
      stop("Start time must be smaller than stop time!")
    }
  }

  if ("stop" %in% passed_arg) {
    if (!"start" %in% passed_arg) {
      start <- as.numeric(sfm[["sim_specs"]][["start"]])
    }
    if (start >= stop) {
      stop("Start time must be smaller than stop time!")
    }
  }

  # Check whether dt is smaller than stop; if not, stop
  if ("dt" %in% passed_arg) {
    if (!"stop" %in% passed_arg) {
      stop <- as.numeric(sfm[["sim_specs"]][["stop"]])
    }
    if (!"start" %in% passed_arg) {
      start <- as.numeric(sfm[["sim_specs"]][["start"]])
    }
    if (dt > (stop - start)) {
      stop("dt must be smaller than the difference between start and stop!")
    }
  }

  # Check whether save_at is smaller than stop; if not, stop
  if ("save_at" %in% passed_arg) {
    if (!"stop" %in% passed_arg) {
      stop <- as.numeric(sfm[["sim_specs"]][["stop"]])
    }
    if (!"start" %in% passed_arg) {
      start <- as.numeric(sfm[["sim_specs"]][["start"]])
    }
    if (!"save_from" %in% passed_arg) {
      save_from <- as.numeric(sfm[["sim_specs"]][["save_from"]])
    }
    if (save_at > (stop - start)) {
      stop("save_at must be smaller than the difference between start and stop!")
    }
    if (save_at > (stop - save_from)) {
      stop("save_at must be smaller than the difference between save_from and stop!")
    }
  }

  # Check whether dt is smaller than save_at; if not, set save_at to dt
  if ("dt" %in% passed_arg) {
    if ("save_at" %in% passed_arg) {
      if (dt > save_at) {
        warning("dt must be smaller or equal to save_at! Setting save_at equal to dt...")
        save_at <- dt
        passed_arg <- c(passed_arg, "save_at")
      }
    } else if (!"save_at" %in% passed_arg) {
      if (is_defined(sfm[["sim_specs"]][["save_at"]])) {
        if (dt > as.numeric(sfm[["sim_specs"]][["save_at"]])) {
          # warning("dt must be smaller or equal to save_at! Setting save_at equal to dt...")
          save_at <- dt
          passed_arg <- c(passed_arg, "save_at")
        }
      } else {
        save_at <- dt
        passed_arg <- c(passed_arg, "save_at")
      }
    }
  } else if ("save_at" %in% passed_arg) {
    # The above ifelse takes care of when save_at and dt are both not NULL; now only save_at can be not NULL
    if (is_defined(sfm[["sim_specs"]][["dt"]])) {
      if (save_at < as.numeric(sfm[["sim_specs"]][["dt"]])) {
        warning("dt must be smaller or equal to save_at! Setting save_at equal to dt...")
        save_at <- dt
        passed_arg <- c(passed_arg, "save_at")
      }
    }
  }

  # Check whether save_from is smaller than stop and larger than start; if not, stop
  if ("save_from" %in% passed_arg) {
    if (!"start" %in% passed_arg) {
      start <- as.numeric(sfm[["sim_specs"]][["start"]])
    }
    if (!"stop" %in% passed_arg) {
      stop <- as.numeric(sfm[["sim_specs"]][["stop"]])
    }

    if (save_from < start | save_from > stop) {
      stop(paste0("save_from must be within the start (", start, ") and stop (", stop, ") time of the simulation!"))
    }
  } else {
    # Ensure that save_from stays within start and stop, also when save_from is not specified
    # When save_from is not specified, it is automatically updated to start
    if ("start" %in% passed_arg) {
      save_from <- start
      passed_arg <- c(passed_arg, "save_from")
    }
  }

  # Seed must be NULL or an integer
  if ("seed" %in% passed_arg) {
    if (!is.null(seed)) {
      if (nzchar(seed)) {
        seed <- strtoi(seed)

        if (is.na(seed)) {
          stop("seed must be an integer!")
        }
        seed <- as.character(seed)
      } else {
        seed <- NULL
      }
    }
  }

  # Ensure no scientific notation is present
  if ("start" %in% passed_arg) {
    start <- replace_digits_with_floats(scientific_notation(start), NULL)
  }
  if ("stop" %in% passed_arg) {
    stop <- replace_digits_with_floats(scientific_notation(stop), NULL)
  }
  if ("dt" %in% passed_arg) {
    dt <- replace_digits_with_floats(scientific_notation(dt), NULL)
  }
  if ("save_at" %in% passed_arg) {
    save_at <- replace_digits_with_floats(scientific_notation(save_at), NULL)
  }
  if ("save_from" %in% passed_arg) {
    save_from <- replace_digits_with_floats(scientific_notation(save_from), NULL)
  }


  # Collect all arguments
  argg <- c(
    as.list(environment())
  )[unique(passed_arg)]

  # Overwrite simulation specifications
  sfm[["sim_specs"]] <- utils::modifyList(sfm[["sim_specs"]], argg)

  sfm <- validate_xmile(sfm)

  return(sfm)
}





#' Remove variable from stock-and-flow model
#'
#' @inheritParams build
#'
#' @return Updated stock-and-flow Model
#' @noRd
#'
erase_var <- function(sfm, name) {
  # Erase specified variables
  sfm[["model"]][["variables"]] <- lapply(
    sfm[["model"]][["variables"]],
    function(x) {
      # Remove variable from model
      x <- x[!names(x) %in% name]

      # Remove variable from to, from, source
      lapply(x, function(y) {
        if (is_defined(y[["to"]])) {
          if (y[["to"]] %in% name) y[["to"]] <- NULL
        }
        if (is_defined(y[["from"]])) {
          if (y[["from"]] %in% name) y[["from"]] <- NULL
        }
        if (is_defined(y[["source"]])) {
          if (y[["source"]] %in% name) y[["source"]] <- NULL
        }
        return(y)
      })
    }
  )

  sfm <- validate_xmile(sfm)

  return(sfm)
}



#' Report whether any names were changed
#'
#' @param old_names Vector with old names
#' @param new_names Vector with new names
#'
#' @returns NULL
#' @noRd
report_name_change <- function(old_names, new_names) {
  # Warning if specified name changed
  idx <- old_names != new_names
  if (any(idx)) {
    warning(paste0(
      ifelse(sum(idx) > 1, "Name was", "Names were"),
      " changed to be syntactically valid and/or avoid overlap: ",
      paste0(paste0(old_names[idx], " -> ", new_names[idx]), collapse = ", ")
    ))
  }

  return(invisible())
}







#' Create, modify or remove variables
#'
#' Add, change, or erase variables in a stock-and-flow model. Variables may be stocks, flows, constants, auxiliaries, or graphical functions.
#'
#' @section Stocks: Stocks define the state of the system. They accumulate material or information over time, such as people, products, or beliefs, which creates memory and inertia in the system. As such, stocks need not be tangible. Stocks are variables that can increase and decrease, and can be measured at a single moment in time. The value of a stock is increased or decreased by flows. A stock may have multiple inflows and multiple outflows. The net change in a stock is the sum of its inflows minus the sum of its outflows.
#'
#' The obligatory properties of a stock are "name", "type", and "eqn". Optional additional properties are "units", "label", "doc", "non_negative".
#'
#' @section Flows: Flows move material and information through the system. Stocks can only decrease or increase through flows. A flow must flow from and/or flow to a stock. If a flow is not flowing from a stock, the source of the flow is outside of the model boundary. Similarly, if a flow is not flowing to a stock, the destination of the flow is outside the model boundary. Flows are defined in units of material or information moved over time, such as birth rates, revenue, and sales.
#'
#' The obligatory properties of a flow are "name", "type", "eqn", and either "from", "to", or both. Optional additional properties are "units", "label", "doc", "non_negative".
#'
#' @section Constants: Constants are variables that do not change over the course of the simulation - they are time-independent. These may be numbers, but also functions. They can depend only on other constants.
#'
#' The obligatory properties of a constant are "name", "type", and "eqn". Optional additional properties are "units", "label", "doc", "non_negative".
#'
#' @section Auxiliaries: Auxiliaries are dynamic variables that change over time. They are used for intermediate calculations in the system, and can depend on other flows, auxiliaries, constants, and stocks.
#'
#' The obligatory properties of an auxiliary are "name", "type", and "eqn". Optional additional properties are "units", "label", "doc", "non_negative".
#'
#' @section Graphical functions: Graphical functions, also known as table or lookup functions, are interpolation functions used to define the desired output (y) for a specified input (x). They are defined by a set of x- and y-domain points, which are used to create a piecewise linear function. The interpolation method defines the behavior of the graphical function between x-points ("constant" to return the value of the previous x-point, "linear" to linearly interpolate between defined x-points), and the extrapolation method defines the behavior outside of the x-points ("NA" to return NA values outside of defined x-points, "nearest" to return the value of the closest x-point).
#'
#' The obligatory properties of an auxiliary are "name", "type", "xpts", and "ypts". "xpts" and "ypts" must be of the same length. Optional additional properties are "units", "label", "doc", "source", "interpolation", "extrapolation".
#'
#' @param sfm Stock-and-flow model, object of class sdbuildR_xmile.
#' @param name Variable name. Character vector.
#' @param type Type of building block(s); one of 'stock', 'flow', 'constant', 'aux', or 'gf'). Does not need to be specified to modify an existing variable.
#' @param change_name New name for variable (optional). Defaults to NULL to indicate no change.
#' @param change_type New type for variable (optional). Defaults to NULL to indicate no change.
#' @param erase If TRUE, remove variable from model. Defaults to FALSE.
#' @param label Name of variable used for plotting. Defaults to the same as name.
#' @param eqn Equation (or initial value in the case of stocks). Defaults to "0.0".
#' @param to Target of flow. Must be a stock in the model. Defaults to NULL to indicate no target.
#' @param from Source of flow. Must be a stock in the model. Defaults to NULL to indicate no source.
#' @param units Unit of variable, such as 'meter'. Defaults to "1" (no units).
#' @param non_negative If TRUE, variable is enforced to be non-negative (i.e. strictly 0 or positive). Defaults to FALSE.
#' @param xpts Only for graphical functions: vector of x-domain points. Must be of the same length as ypts.
#' @param ypts Only for graphical functions: vector of y-domain points. Must be of the same length as xpts.
#' @param source Only for graphical functions: name of the variable which will serve as the input to the graphical function. Necessary to specify if units are used. Defaults to NULL.
#' @param interpolation Only for graphical functions: interpolation method. Must be either "constant" or "linear". Defaults to "linear".
#' @param extrapolation Only for graphical functions: extrapolation method. Must be either "nearest" or "NA". Defaults to "nearest".
#' @param doc Description of variable. Defaults to "".
#' @param df Dataframe with variable properties to add and/or modify.
#'
#' @return Updated stock-and-flow model.
#' @seealso [xmile()]
#' @family build
#' @export
#'
#' @examples
#'
#' # First initialize an empty model:
#' sfm <- xmile()
#' summary(sfm)
#' \dontshow{
#' sfm <- sim_specs(sfm, save_at = .5)
#' }
#'
#' # Add two stocks. Specify their initial values in the "eqn" property, as well
#' # as their plotting label
#' sfm <- build(sfm, "predator", "stock", eqn = 10, label = "Predator") |>
#'   build("prey", "stock", eqn = 50, label = "Prey")
#'
#'
#' # Add four flows: the births and deaths of both the predators and prey. The
#' # "eqn" property of flows represents the rate of the flow. In addition, we
#' # specify which stock the flow is coming from ("from") or flowing to ("to").
#' sfm <- build(sfm, "predator_births", "flow",
#'   eqn = "delta*prey*predator",
#'   label = "Predator Births", to = "predator"
#' ) |>
#'   build("predator_deaths", "flow",
#'     eqn = "gamma*predator",
#'     label = "Predator Deaths", from = "predator"
#'   ) |>
#'   build("prey_births", "flow",
#'     eqn = "alpha*prey",
#'     label = "Prey Births", to = "prey"
#'   ) |>
#'   build("prey_deaths", "flow",
#'     eqn = "beta*prey*predator",
#'     label = "Prey Deaths", from = "prey"
#'   )
#' plot(sfm)
#'
#' # The flows make use of four other variables: "delta", "gamma", "alpha", and
#' # "beta". Define these as constants in a vectorized manner for efficiency:
#' sfm <- build(sfm, c("delta", "gamma", "alpha", "beta"), "constant",
#'   eqn = c(.025, .5, .5, .05),
#'   label = c("Delta", "Gamma", "Alpha", "Beta"),
#'   doc = c(
#'     "Birth rate of predators", "Death rate of predators",
#'     "Birth rate of prey", "Death rate of prey by predators"
#'   )
#' )
#'
#' # We now have a complete predator-prey model which is ready to be simulated
#' sim <- simulate(sfm)
#' plot(sim)
#'
#' # Modify variable
#' sfm <- build(sfm, "delta", eqn = .03, label = "DELTA")
#'
#' # Change variable name (throughout the model)
#' sfm <- build(sfm, "delta", change_name = "DELTA")
#'
#' # Change variable type
#' sfm <- build(sfm, "DELTA", change_type = "stock")
#'
#' # Remove variable
#' sfm <- build(sfm, "prey", erase = TRUE)
#'
#' # To add and/or modify variables more quickly, pass a dataframe.
#' # The dataframe is processed row-wise.
#' # To create a logistic population growth model:
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
#' sfm <- build(sfm, df = df)
#'
#' # Check for errors in the model
#' debugger(sfm)
#'
build <- function(sfm, name, type,
                  eqn = "0.0",
                  units = "1",
                  label = name,
                  doc = "",
                  change_name = NULL,
                  change_type = NULL,
                  erase = FALSE,
                  # Flow arguments
                  to = NULL, from = NULL,
                  # Rarely used arguments
                  non_negative = FALSE,
                  # Graphical function arguments
                  xpts = NULL, ypts = NULL,
                  source = NULL,
                  interpolation = "linear",
                  extrapolation = "nearest",
                  df = NULL) {
  # Basic check
  if (missing(sfm)) {
    stop("No model specified!")
  }
  check_xmile(sfm)

  if (!is.null(df)) {
    sfm <- add_from_df(sfm, df)
    return(sfm)
  }

  if (missing(name)) {
    stop("name must be specified!")
  }

  if (!(all(is.character(name)))) {
    stop("name must be a character!")
  }

  name <- trimws(name)
  if (!(all(nzchar(name)))) {
    stop("name cannot be empty!")
  }

  label <- trimws(label)
  if (!(all(nzchar(label)))) {
    stop("label cannot be empty!")
  }

  # Remove variable from model
  if (!is.null(erase)) {
    if (length(erase) != 1) {
      stop("erase must be a single logical value!")
    }

    if (!is.logical(erase)) {
      stop("erase must be TRUE or FALSE!")
    }
  }

  # Get names dataframe
  names_df <- get_names(sfm)
  var_names <- names_df[["name"]]

  # Get names of passed arguments
  passed_arg <- names(as.list(match.call())[-1]) |>
    # Remove some arguments
    setdiff(c("sfm", "erase", "change_name", "change_type"))

  # Find variables which already exist
  idx_exist <- name %in% var_names

  # Check if name does not exists and type is missing
  if (missing(type)) {
    # If type is not specified, all names have to exist
    if (any(!idx_exist)) {
      stop(sprintf(
        "The variable%s %s %s not exist in your model! If you're trying to add a new variable, specify type (one of 'stock', 'flow', 'constant', 'aux', 'gf').",
        ifelse(length(name[!idx_exist]) > 1, "s", ""),
        paste0(name[!idx_exist], collapse = ", "),
        ifelse(length(name[!idx_exist]) > 1, "do", "does")
      ))
    }

    # Find corresponding building block
    type <- names_df[match(name, names_df[["name"]]), "type"]
  } else if (!missing(type)) {
    type <- clean_type(type)

    if (!all(type %in% c("stock", "flow", "constant", "aux", "gf"))) {
      stop("type needs to be one of 'stock', 'flow', 'constant', 'aux', or 'gf'!")
    }
    type <- ensure_length(type, name)

    # If type is specified, and name already exists, but it doesn't match that type, stop
    match_type <- names_df[match(name, names_df[["name"]]), "type"]

    nonmatching_type <- idx_exist & type != match_type

    if (any(nonmatching_type)) {
      if (erase) {
        stop(paste0(
          "These variables exist in your model but not as the type specified:\n- ",
          paste0(paste0(name[nonmatching_type], " (type: ", match_type[nonmatching_type], ")"), collapse = ", ")
        ))
      } else {
        stop(paste0(
          "These variables already exist in your model, but not as the type specified. Either omit the type to modify the variable, or specify a unique variable name to add a new variable of that type:\n- ",
          paste0(paste0(name[nonmatching_type], " (type: ", match_type[nonmatching_type], ")"), collapse = ", ")
        ))
      }
    }

    # Ensure names are valid of new variables
    if (any(!idx_exist)) {
      # Create syntactically valid, unique names (this also avoids overlap with previous names, but we stopped the function already if this is the case)
      new_names <- create_R_names(name[!idx_exist], names_df)

      # Warning if specified name changed
      report_name_change(name[!idx_exist], new_names)

      # Change name
      name[!idx_exist] <- new_names
    }
  }

  # Remove variable
  if (erase) {
    # For erase, all names have to exist
    if (any(!idx_exist)) {
      stop(sprintf(
        "The variable%s %s %s not exist in your model!",
        ifelse(length(name[!idx_exist]) > 1, "s", ""),
        paste0(name[!idx_exist], collapse = ", "),
        ifelse(length(name[!idx_exist]) > 1, "do", "does")
      ))
    }

    sfm <- erase_var(sfm, name)
    return(sfm)
  }

  # Check change name of variable
  if (!is.null(change_name)) {
    if (length(change_name) > 1 | length(name) > 1) {
      stop("You can only change the name of one variable at a time!")
    }

    if (!nzchar(trimws(change_name))) {
      stop("change_name cannot be empty!")
    }
  }

  # Check change type of variable
  if (!is.null(change_type)) {
    if (length(change_type) > 1 | length(name) > 1) {
      stop("You can only change the type of one variable at a time!")
    }

    change_type <- clean_type(change_type)
    if (!change_type %in% c("stock", "flow", "constant", "aux", "gf")) {
      stop("change_type needs to be one of 'stock', 'flow', 'constant', 'aux', or 'gf'!")
    }
  }


  # Get properties per building block
  keep_prop <- get_building_block_prop()

  # Check whether appropriate properties were passed for this variable type; issue warning if not
  if (is.null(change_type)) {
    type_ <- type
  } else {
    type_ <- change_type
  }
  appr_prop <- Reduce(intersect, keep_prop[type_])
  idx_inappr <- !(passed_arg %in% appr_prop)
  if (any(idx_inappr)) {
    warning(sprintf(
      "These properties are not appropriate for %s specified type%s (%s):\n- %s\nThese will be ignored.",
      ifelse(length(unique(type_)) > 1, "all", "the"),
      ifelse(length(unique(type_)) > 1, "s", ""),
      paste0(unique(type_), collapse = ", "), paste0(passed_arg[idx_inappr], collapse = ", ")
    ))
  }

  # Flow properties
  if ("to" %in% passed_arg) {
    if (identical(to, NULL)) {
      to <- ""
    }

    to[is.na(to)] <- ""

    if (!inherits(to, "character")) {
      stop("to must be a character!")
    }

    if (length(name) == 1 & length(to) > 1) {
      stop("A flow may only have one target!")
    }

    to <- ensure_length(to, name)

    if (any(to == name)){
      stop("A flow cannot flow to itself!")
    }

  }

  if ("from" %in% passed_arg) {
    if (identical(from, NULL)) {
      from <- ""
    }

    from[is.na(from)] <- ""

    if (!inherits(from, "character")) {
      stop("from must be a character!")
    }

    if (length(name) == 1 & length(from) > 1) {
      stop("A flow may only have one source!")
    }

    from <- ensure_length(from, name)

    if (any(from == name)){
      stop("A flow cannot flow from itself!")
    }

  }

  # Ensure to and from are not the same
  if (!is.null(to) & !is.null(from)) {
    if (any(to == from)) {
      stop("A flow cannot flow to and from the same stock!")
    }
  }


  # Graphical functions
  if (any(type == "gf")) {
    if (length(name) != 1) {
      stop("Vectorized building is not supported for graphical functions. Please ony specify one graphical function at a time.")
    }

    if (is.null(xpts) & !is.null(ypts)) {
      stop("xpts must be specified for graphical functions!")
    }

    if (is.null(ypts) & !is.null(xpts)) {
      stop(" ypts must be specified for graphical functions!")
    }

    if (!is.null(xpts) & !is.null(ypts)) {
      # Split xpts and ypts temporarily to check length
      if (inherits(xpts, "character")) {
        xpts <- strsplit(xpts, ",")[[1]]
        xpts[1] <- gsub("^c\\(", "", xpts[1])
        xpts[length(xpts)] <- gsub("\\)$", "", xpts[length(xpts)])
      }

      if (inherits(ypts, "character")) {
        ypts <- strsplit(ypts, ",")[[1]]
        ypts[1] <- gsub("^c\\(", "", ypts[1])
        ypts[length(ypts)] <- gsub("\\)$", "", ypts[length(ypts)])
      }

      if (length(xpts) != length(ypts)) {
        # Ensure length of xpts and ypts for graphical functions is the same
        stop(paste0(
          "For graphical functions, the length of xpts must match that of ypts.\n",
          paste0("The length of xpts is ", length(xpts), "; the length of ypts is ", length(ypts), collapse = "\n")
        ))
      }


      if (!inherits(xpts, "character")) {
        xpts <- paste0("c(", paste0(xpts, collapse = ", "), ")")
      }

      if (!inherits(ypts, "character")) {
        ypts <- paste0("c(", paste0(ypts, collapse = ", "), ")")
      }
    }

    interpolation <- tolower(interpolation)

    if (length(interpolation) > 1) {
      stop("interpolation must be a single value!")
    }

    if (!interpolation %in% c("linear", "constant")) {
      stop(sprintf("interpolation must be 'linear' or 'constant'!"))
    }

    if (length(extrapolation) > 1) {
      stop("extrapolation must be a single value!")
    }

    if (!extrapolation %in% c("nearest", "NA")) {
      stop(sprintf("extrapolation must be either 'nearest' or 'NA'!"))
    }

    if (!is.null(source)) {
      if (!inherits(source, "character")) {
        stop("source must be a character!")
      }

      # Ensure source is a single value
      if (length(source) > 1) {
        stop("source must be a single value!")
      }
    }
  }

  # If overwriting name with change_name
  if (!is.null(change_name)) {
    # Ensure new name is syntactically valid
    chosen_new_name <- change_name
    change_name <- create_R_names(change_name, names_df)
    report_name_change(chosen_new_name, change_name)

    # Overwrite name
    variable_names <- names(sfm[["model"]][["variables"]][[type]])
    variable_names[variable_names == name] <- change_name
    names(sfm[["model"]][["variables"]][[type]]) <- variable_names
    sfm[["model"]][["variables"]][[type]][[change_name]][["name"]] <- change_name

    # Overwrite label in case it was the same as the old name
    if ("label" %in% passed_arg) {
      sfm[["model"]][["variables"]][[type]][[change_name]][["label"]] <- label
    } else {
      if (sfm[["model"]][["variables"]][[type]][[change_name]][["label"]] == name) {
        sfm[["model"]][["variables"]][[type]][[change_name]][["label"]] <- change_name
      }
    }

    # Replace references to name with change_name everywhere (eqn, from, to)
    sfm[["model"]][["variables"]] <- lapply(sfm[["model"]][["variables"]], function(y) {
      lapply(y, function(x) {
        if (is_defined(x[["eqn"]])) {
          idx_df <- get_range_names(x[["eqn"]], name,
            names_with_brackets = FALSE
          )
          if (nrow(idx_df) > 0) {
            # Reverse indices to replace correctly
            for (i in rev(seq_len(nrow(idx_df)))) {
              stringr::str_sub(x[["eqn"]], idx_df[i, "start"], idx_df[i, "end"]) <- change_name
            }

            # Update julia translation
            idx_df <- get_range_names(x[["eqn_julia"]], name,
              names_with_brackets = FALSE
            )
            if (nrow(idx_df) > 0) {
              # Reverse indices to replace correctly
              for (i in rev(seq_len(nrow(idx_df)))) {
                stringr::str_sub(x[["eqn_julia"]], idx_df[i, "start"], idx_df[i, "end"]) <- change_name
              }
            }
          }
        }
        if (is_defined(x[["from"]])) {
          if (x[["from"]] == name) x[["from"]] <- change_name
        }
        if (is_defined(x[["to"]])) {
          if (x[["to"]] == name) x[["to"]] <- change_name
        }
        return(x)
      })
    })

    name <- change_name

    # Update
    var_names <- get_model_var(sfm)
    idx_exist <- name %in% var_names

    # Redo equation (in case of delay variables, the names need to be updated to get the correct suffix, e.g. "a" -> "b" needs new delay names "b_delay1_acc1", etc.; in addition, some types can't have delays)

    if (!"eqn" %in% passed_arg) {
      eqn <- sfm[["model"]][["variables"]][[type]][[name]][["eqn"]]
      passed_arg <- c(passed_arg, "eqn")
    }
  }

  # Change type of building block
  if (!is.null(change_type)) {
    if (type != change_type) {
      old_prop <- sfm[["model"]][["variables"]][[type]][[name]]

      updated_defaults <- utils::modifyList(formals(build), old_prop)
      updated_defaults <- updated_defaults[names(updated_defaults) %in% keep_prop[[change_type]]]
      updated_defaults <- updated_defaults[!lengths(updated_defaults) == 0]

      # Remove old part
      sfm[["model"]][["variables"]][[type]][name] <- NULL

      # Add new part
      sfm[["model"]][["variables"]][[change_type]][[name]] <- updated_defaults

      type <- change_type

      # Redo equation (in case of delay variables, the names need to be updated to get the correct suffix, e.g. "a" -> "b" needs new delay names "b_delay1_acc1", etc.; in addition, some types can't have delays)
      if (!"eqn" %in% passed_arg) {
        eqn <- sfm[["model"]][["variables"]][[type]][[name]][["eqn"]]
        passed_arg <- c(passed_arg, "eqn")
      }
    }
  }



  # Only need regex_units if any of the following are passed
  if (any(c("eqn", "units") %in% passed_arg)) {
    regex_units <- get_regex_units()
  }


  if ("eqn" %in% passed_arg) {
    if (is.null(eqn)) {
      warning("eqn cannot be NULL! Setting empty equation to 0...")
      eqn <- "0.0"
    }

    if (any(is.na(eqn))) {
      warning("eqn cannot be NA! Setting equations to 0...")
      eqn[is.na(eqn)] <- "0.0"
    }

    if (any(!nzchar(eqn))) {
      warning("eqn cannot be empty! Setting empty equations to 0...")
      eqn[!nzchar(eqn)] <- "0.0"
    }

    # Change all equations to characters
    if (!is.null(eqn)) {
      eqn <- as.character(eqn)
    }

    if (any(grepl("^[ ]*function[ ]*\\(", eqn))) {
      stop("Model variables cannot be functions! To add a custom function, use macro().")
    }


    # Ensure units are cleaned in u() in eqn
    eqn <- clean_unit_in_u(eqn, regex_units)
    eqn <- ensure_length(eqn, name)

    # Convert to julia - note that with delay() and past(), an intermediary property is added; with delayN() and smoothN(), a func property (nested list) is added
    eqn_julia <- lapply(seq_along(name), function(i) {
      convert_equations_julia(sfm, type[i], name[i], eqn[i], var_names,
        regex_units = regex_units
      )
    }) |> unname()

    # Remove old func list
    for (i in length(name)) {
      sfm[["model"]][["variables"]][[type[i]]][[name[i]]][["func"]] <- NULL
    }
  }

  # Units
  if (!is.null(units)) {
    if (!inherits(units, "character")) {
      units <- as.character(units)
    }

    # Set empty unit to 1
    if (any(!nzchar(units))) {
      units[!nzchar(units)] <- "1"
    }

    # Units are not supported well in R, so translate to julia directly
    units <- vapply(units, function(x) {
      clean_unit(x, regex_units)
    }, character(1), USE.NAMES = FALSE)
    units <- ensure_length(units, name)
  }

  if ("non_negative" %in% passed_arg) {
    if (!all(is.logical(non_negative))) {
      stop("non_negative must be either TRUE or FALSE!")
    }
    non_negative <- ensure_length(non_negative, name)
  }

  if ("label" %in% passed_arg) {
    if (!inherits(label, "character")) {
      stop("label must be a character!")
    }
    label <- ensure_length(label, name)
  }

  if ("doc" %in% passed_arg) {
    if (!inherits(doc, "character")) {
      stop("doc must be a character!")
    }
    doc <- ensure_length(doc, name)
  }


  # Collect all arguments in environment but only keep those that were passed
  argg <- c(as.list(environment()))[unique(passed_arg)]
  argg[["type"]] <- type # Keep type for ease

  # Create nested 3-level list with all model entries
  new_element <- purrr::transpose(argg) |> lapply(list)

  new_element <- lapply(seq_along(new_element), function(y) {
    # Create three named levels: type, name, properties

    # Make sure each model element only has appropriate entries
    x <- new_element[[y]]

    keep_prop_y <- keep_prop[[type[y]]]
    keep_x <- x[[1]][names(x[[1]]) %in% keep_prop_y]

    # Add converted Julia equation
    if ("eqn" %in% passed_arg) {
      keep_x <- utils::modifyList(keep_x, eqn_julia[[y]])
    }

    stats::setNames(list(keep_x), name[y])
  }) |> stats::setNames(type)

  # Add elements to model (in for-loop, as otherwise not all elements are added)
  for (i in seq_along(name)) {
    sfm[["model"]][["variables"]] <- utils::modifyList(sfm[["model"]][["variables"]], new_element[i])
  }

  sfm <- validate_xmile(sfm)

  return(sfm)
}




#' Add and/or modify model from dataframe
#'
#' @inheritParams build
#'
#' @return Updated stock-and-flow model.
#' @noRd
#'
add_from_df <- function(sfm, df) {
  if (!inherits(df, "data.frame")) {
    stop("df must be a dataframe!")
  }

  # Get all properties
  prop <- get_building_block_prop()

  # Check whether dataframe has necessary columns
  nec_prop <- c("type", "name")

  if (!all(nec_prop %in% colnames(df))) {
    stop("Please specify ", paste0(nec_prop, collapse = ", "))
  }

  # Check whether dataframe has columns only in prop
  idx <- !colnames(df) %in% unique(unlist(prop))
  if (any(idx)) {
    stop("The following column names are not valid properties: ",
         paste0(colnames(df)[idx], collapse = ", "))
  }

  # Add each row
  for (i in seq_len(nrow(df))) {
    arg <- as.list(df[i, ])
    arg <- arg[!is.na(arg)]

    # Only keep appropriate properties for this type
    arg <- arg[names(arg) %in% prop[[arg[["type"]]]]]

    arg[["sfm"]] <- sfm
    sfm <- do.call(sdbuildR::build, arg)
  }

  sfm <- validate_xmile(sfm)

  return(sfm)
}




#' Get possible variable properties per building block type
#'
#' @return List with default properties per building block type
#' @noRd
#'
get_building_block_prop <- function() {
  return(list(
    "stock" = c(
      "name", "type", "eqn", "units", "label", "doc",
      "non_negative",
      "eqn_julia"
    ),
    "flow" = c(
      "name", "type", "eqn", "to", "from", "units", "label", "doc",
      "non_negative",
      "eqn_julia"
    ),
    "constant" = c(
      "name", "type", "eqn", "units", "label", "doc",
      "non_negative",
      "eqn_julia"
    ),
    "aux" = c(
      "name", "type", "eqn", "units", "label", "doc",
      "non_negative",
      "eqn_julia"
    ),
    "gf" = c("name", "type", "units", "label", "xpts", "ypts", "source", "interpolation", "extrapolation", "doc")
  ))
}




#' Create syntactically valid, unique names for use in R and Julia
#'
#' @param create_names Vector of strings with names to transform to valid names
#' @param names_df Dataframe with at least the column name
#' @param protected Optional vector of protected names
#'
#' @return Translated names
#' @noRd
#'
create_R_names <- function(create_names, names_df, protected = c()) {
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
    # "?", "]",

    # Add R custom functions
    get_exported_functions("sdbuildR"),

    # Add julia custom functions
    names(get_func_julia()),

    # These are variables in the ode and cannot be model element names
    unname(unlist(.sdbuildR_env[["P"]][names(.sdbuildR_env[["P"]]) %in% c(
      "jl_pkg_name", "model_setup_name", "macro_name", "initial_value_name", "initial_value_names", "parameter_name", "parameter_names",
      "state_name", "time_name", "change_state_name", "times_name", "timestep_name", "saveat_name", "time_units_name", "ensemble_iter", "ode_func_name", "callback_func_name", "callback_name", "intermediaries", "rootfun_name", "eventfun_name", "convert_u_func", "sdbuildR_units", "MyCustomUnits", "init_sdbuildR"
    )])),
    protected,
    as.character(stats::na.omit(names_df[["name"]]))
  ) |> unique()

  stock_names <- names_df[names_df[["type"]] == "stock", "name"]
  if (length(stock_names) > 0) {
    if (all(!is.null(stock_names) & !is.na(stock_names))) {
      protected_names <- c(
        protected_names,
        paste0(.sdbuildR_env[["P"]][["change_prefix"]], stock_names)
      )
    }
  }

  # Make syntactically valid and unique names out of character vectors; Insight Maker allows names to be double, so make unique
  new_names <- make.names(c(protected_names, trimws(create_names)), unique = TRUE)
  # For Julia translation, remove names with a period
  new_names <- stringr::str_replace_all(new_names, "\\.", "_")
  # This may cause overlap in names, so repeat
  new_names <- make.names(new_names, unique = TRUE)
  new_names <- stringr::str_replace_all(new_names, "\\.", "_")
  new_names <- make.names(new_names, unique = TRUE)[-seq_along(protected_names)] # Remove protected names



  # If any names end in a suffix used by sdbuildR, add _
  pattern <- paste0(
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
  # e.g. names cannot end with _delay[0-9]+$ or _delay[0-9]+_acc[0-9]+$

  return(new_names)
}





#' Generate code to build stock-and-flow model
#'
#' Create R code to rebuild an existing stock-and-flow model. This may help to understand how a model is built, or to modify an existing one.
#'
#' @inheritParams build
#'
#' @return String with code to build stock-and-flow model from scratch.
#' @family build
#' @export
#'
#' @examples
#' sfm <- xmile("SIR")
#' get_build_code(sfm)
get_build_code <- function(sfm) {
  check_xmile(sfm)

  # Simulation specifications - careful here. If a default is 100.0, this will be turned into 100. Need to have character defaults to preserve digits.
  sim_specs_list <- sfm[["sim_specs"]]
  sim_specs_list <- lapply(sim_specs_list, function(z) if (is.character(z)) paste0("\"", z, "\"") else z)
  sim_specs_str <- paste0(names(sim_specs_list), " = ", unname(sim_specs_list), collapse = ", ")
  sim_specs_str <- paste0(" |>\n\t\tsim_specs(", sim_specs_str, ")")

  # Model units
  if (length(sfm[["model_units"]]) > 0) {
    model_units_str <- lapply(sfm[["model_units"]], function(x) {
      x <- lapply(x, function(z) if (is.character(z)) paste0("\"", z, "\"") else z)


      sprintf("model_units(%s)", paste0(names(x), " = ", unname(x), collapse = ", "))
    }) |>
      unlist() |>
      paste0(collapse = "|>\n\t\t")
    model_units_str <- paste0(" |>\n\t\t", model_units_str)
  } else {
    model_units_str <- ""
  }

  # Macros
  if (length(sfm[["macro"]]) > 0) {
    macro_str <- lapply(sfm[["macro"]], function(x) {
      # Remove properties containing "_julia"
      x[grepl("_julia", names(x))] <- NULL

      x <- lapply(x, function(z) if (is.character(z)) paste0("\"", z, "\"") else z)
      sprintf("macro(%s)", paste0(names(x), " = ", unname(x), collapse = ", "))
    }) |>
      unlist() |>
      paste0(collapse = "|>\n\t\t")
    macro_str <- paste0(" |>\n\t\t", macro_str)
  } else {
    macro_str <- ""
  }

  # Header string
  h <- sfm[["header"]]
  defaults_header <- formals(header)
  defaults_header <- defaults_header[!names(defaults_header) %in% c("sfm", "created", "...")]

  # Find which elements in h are identical to those in defaults_header
  h <- h[vapply(names(h), function(name) {
    !name %in% names(defaults_header) || !identical(h[[name]], defaults_header[[name]])
  }, logical(1))]

  h <- lapply(h, function(z) if (is.character(z) | inherits(z, "POSIXt")) paste0("\"", z, "\"") else z)

  header_str <- paste0(" |>\n\t\theader(", paste0(names(h), " = ", unname(h), collapse = ", "), ")")

  # Variables
  if (length(unlist(sfm[["model"]][["variables"]])) > 0) {
    defaults <- formals(build)
    defaults <- defaults[!names(defaults) %in% c("sfm", "name", "type", "label", "...")]

    # Get properties per building block
    keep_prop <- get_building_block_prop()


    var_str <- lapply(sfm[["model"]][["variables"]], function(x) {
      lapply(x, function(y) {
        z <- y
        z[["func"]] <- NULL

        # Remove properties containing "_julia"
        z[grepl("_julia", names(z))] <- NULL

        # Find which elements in h are identical to those in defaults_header
        z <- z[vapply(names(z), function(name) {
          !name %in% names(defaults) || !identical(z[[name]], defaults[[name]])
        }, logical(1))]

        # Order z according to default
        order_names <- intersect(keep_prop[[z[["type"]]]], names(z))
        z <- z[order_names]

        z <- lapply(z, function(a) {
          ifelse(is.character(a), paste0("\"", a, "\""), a)
        })

        paste0(
          "build(",
          paste0(names(z), " = ", unname(z),
            collapse = ", "
          ), ")"
        )
        # sprintf("build('%s', '%s'%s)",
        #         y[["name"]], y[["type"]], ifelse(length(z) > 0,
        #                                          paste0(", ", paste0(names(z), " = ", unname(z),
        #                                                              collapse = ", ")), "") )
      })
    })
    var_str <- var_str[lengths(var_str) > 0]
    var_str <- paste0(" |>\n\t\t", paste0(unlist(var_str), collapse = " |>\n\t\t"))
  } else {
    var_str <- ""
  }

  script <- sprintf("sfm = xmile()%s%s%s%s%s", sim_specs_str, header_str, var_str, macro_str, model_units_str)

  # Format code
  if (requireNamespace("styler", quietly = TRUE)) {
    # Temporarily set option
    old_option <- getOption("styler.colored_print.vertical")
    options(styler.colored_print.vertical = FALSE)

    script <- tryCatch(
      {
        suppressWarnings(suppressMessages(
          script <- styler::style_text(script)
        ))
      },
      error = function(e) {
        return(script)
      }
    )

    on.exit({
      if (is.null(old_option)) {
        options(styler.colored_print.vertical = NULL)
      } else {
        options(styler.colored_print.vertical = old_option)
      }
    })
  } else {
    message("The code will not be formatted as styler is not installed. Install styler or wrap the script in cat().")
  }

  return(script)
}



#' Debug stock-and-flow model
#'
#' Check for common formulation problems in a stock-and-flow model.
#'
#' The following problems are detected:
#' - An absence of stocks
#' - Flows without a source (`from`) or target (`to`)
#' - Flows connected to a stock that does not exist
#' - Undefined variable references in equations
#' - Circularity in equations
#' - Connected stocks and flows without both having units or no units
#' - Missing unit definitions
#'
#' The following potential problems are detected:
#' - Absence of flows
#' - Stocks without inflows or outflows
#' - Equations with a value of 0
#'
#' @inheritParams build
#' @param quietly If TRUE, don't print problems. Defaults to FALSE.
#'
#' @returns Logical value indicating whether any problems were detected.
#' @family build
#' @export
#'
#' @examples
#' # No issues
#' sfm <- xmile("SIR")
#' debugger(sfm)
#'
#' # Detect absence of stocks or flows
#' sfm <- xmile()
#' debugger(sfm)
#'
#' # Detect stocks without inflows or outflows
#' sfm <- xmile() |> build("Prey", "stock")
#' debugger(sfm)
#'
#' # Detect circularity in equation definitions
#' sfm <- xmile() |>
#'   build("Prey", "stock", eqn = "Predator") |>
#'   build("Predator", "stock", eqn = "Prey")
#' debugger(sfm)
#'
debugger <- function(sfm, quietly = FALSE) {
  check_xmile(sfm)

  if (!is.logical(quietly)) {
    stop("quietly must be TRUE or FALSE!")
  }

  problems <- c()
  potential_problems <- c()

  constant_names <- names(sfm[["model"]][["variables"]][["constant"]])
  aux_names <- names(sfm[["model"]][["variables"]][["aux"]])
  stock_names <- names(sfm[["model"]][["variables"]][["stock"]])
  flow_df <- get_flow_df(sfm)
  flow_names <- flow_df[["name"]]
  names_df <- get_names(sfm)

  ### Check whether all Stocks have inflows and/or outflows
  if (length(stock_names) > 0 & nrow(flow_df) > 0) {
    idx <- stock_names %in% flow_df[["to"]] | stock_names %in% flow_df[["from"]]

    if (any(!idx)) {
      potential_problems <- c(potential_problems, paste0(
        "* These stocks are not connected to any flows:\n- ",
        paste0(stock_names[!idx], collapse = ", ")
      ))
    }
  } else if (length(stock_names) == 0) {
    problems <- c(problems, "* Your model has no stocks.")
  }


  ### Check whether all flows either have a from or to property
  if (length(flow_names) > 0) {
    idx <- !nzchar(flow_df[["from"]]) & !nzchar(flow_df[["to"]])

    if (any(idx)) {
      problems <- c(problems, paste0(
        "* These flows are not connected to any stock:\n- ",
        paste0(flow_names[idx], collapse = ", "), "\nConnect a flow to a stock using 'to' and/or 'from' in build()."
      ))
    }

    ### Find whether the from and to stocks exist
    idx_to <- (!flow_df[["to"]] %in% stock_names) & nzchar(flow_df[["to"]])
    idx_from <- (!flow_df[["from"]] %in% stock_names) & nzchar(flow_df[["from"]])

    if (any(idx_to) | any(idx_from)) {
      problems <- c(problems, paste0(
        "* These flows are connected to a stock that does not exist:\n - ",
        paste0(c(flow_names[idx_to], flow_names[idx_from]), collapse = ", ")
      ))
    }

    ### Find whether both flows and stocks have units
    flows_units <- names_df[match(flow_names, names_df[["name"]]), "units"]
    stock_units <- names_df[match(stock_names, names_df[["name"]]), "units"]
  } else {
    potential_problems <- c(potential_problems, "* Your model has no flows.")
  }


  ### Check equations with zero
  zero_eqn <- lapply(unname(sfm[["model"]][["variables"]]), function(y) {
    lapply(y, function(x) {
      if (is_defined(x[["eqn"]])) {
        if (x[["eqn"]] == "0" | x[["eqn"]] == "0.0") {
          return(x[["name"]])
        }
      }
      return(NULL)
    })
  }) |>
    unlist() |>
    compact_()

  if (length(zero_eqn) > 0) {
    potential_problems <- c(potential_problems, paste0("* These variables have an equation of 0:\n- ", paste0(unname(zero_eqn), collapse = ", ")))
  }

  ### Detect undefined variable references in equations
  out <- detect_undefined_var(sfm)
  if (out[["issue"]]) {
    problems <- c(problems, paste0("* ", out[["msg"]]))
  }

  # ### Detect whether static variables depend on dynamic ones
  # out = static_depend_on_dyn(sfm)
  # if (out[["issue"]]){
  #   potential_problems = c(potential_problems, paste0("* ",  out[["msg"]]))
  # }

  ### Detect circularity in equations
  out <- order_equations(sfm, print_msg = FALSE)
  if (out[["static"]][["issue"]]) {
    problems <- c(
      problems,
      paste0("* Ordering static equations failed. ", out[["static"]][["msg"]], collapse = "")
    )
  }
  if (out[["dynamic"]][["issue"]]) {
    problems <- c(
      problems,
      paste0("* Ordering dynamic equations failed. ", out[["dynamic"]][["msg"]], collapse = "")
    )
  }


  ### Find missing unit definitions
  regex_units <- get_regex_units()

  # Check whether all units are defined
  add_model_units <- detect_undefined_units(sfm,
    new_eqns = c(
      sfm[["model"]][["variables"]] |>
        lapply(function(x) {
          lapply(x, `[[`, "eqn_julia")
        }) |> unlist(),
      unlist(lapply(sfm[["macro"]], `[[`, "eqn_julia"))
    ),
    new_units = sfm[["model"]][["variables"]] |>
      lapply(function(x) {
        lapply(x, `[[`, "units")
      }) |> unlist(),
    regex_units = regex_units, R_or_Julia = "Julia"
  )
  if (length(add_model_units) > 0) {
    problems <- c(problems, paste0(
      "* These units are not defined:\n- ",
      paste0(names(add_model_units), collapse = ", ")
    ))
  }



  if (!quietly & length(problems) > 0) {
    message("Problems:")
    message(paste0(problems, collapse = "\n\n"))
  } else if (!quietly) {
    message("No problems detected!")
  }

  if (!quietly & length(potential_problems) > 0) {
    prefix <- ifelse(!quietly & length(problems) > 0, "\n", "")
    message(paste0(prefix, "Potentially problematic:"))
    message(paste0(potential_problems, collapse = "\n\n"))
  }


  if (quietly) {
    return(list(
      problems = paste0(problems, collapse = "\n\n"),
      potential_problems = paste0(potential_problems, collapse = "\n\n")
    ))
  } else {
    return(invisible())
  }
}



#' Check whether static variables (stock's initial values, constants) depend on dynamic variables
#'
#' @inheritParams build
#'
#' @noRd
#' @returns Logical value
static_depend_on_dyn <- function(sfm) {
  # Check whether a stock depends on a dynamic variable, give warning
  dependencies <- sfm[["model"]][["variables"]][c("stock", "constant")] |>
    unname() |>
    purrr::list_flatten() |>
    lapply(`[[`, "eqn") |>
    find_dependencies_(sfm, eqns = _, only_model_var = TRUE)

  names_df <- get_names(sfm)
  dynamic_var <- names_df[names_df[["type"]] %in% c("aux", "flow"), "name"]

  static_with_dyn_dep <- lapply(dependencies, function(x) {
    x[x %in% dynamic_var]
  }) |> compact_()

  if (length(static_with_dyn_dep) > 0) {
    # static_with_dyn_dep <- sapply(static_with_dyn_dep, paste0, collapse = ", ")
    static_with_dyn_dep <- vapply(static_with_dyn_dep, paste0, character(1), collapse = ", ")
    stock_or_constant <- names_df[match(names(static_with_dyn_dep), names_df[["name"]]), "type"]

    msg <- paste0(
      c(
        "Simulation impossible: static variables depend on dynamic variables!",
        paste0(
          paste0(
            "- ",
            ifelse(stock_or_constant == "stock", "The initial value of stock ", "The constant "),
            names(static_with_dyn_dep), " depends on ", static_with_dyn_dep
          ),
          collapse = "\n"
        )
      ),
      collapse = "\n"
    )

    return(list(issue = TRUE, msg = msg))
  } else {
    return(list(issue = FALSE))
  }
}





#' Convert stock-and-flow model to dataframe
#'
#' Create a dataframe with properties of all model variables, model units, and macros. Specify the variable types, variable names, and/or properties to get a subset of the dataframe.
#'
#' @inheritParams plot.sdbuildR_xmile
#' @param type Variable types to retain in the dataframe. Must be one or more of 'stock', 'flow', 'constant', 'aux', 'gf', 'model_units', or 'macro'. Defaults to NULL to include all variable types.
#' @param name Variable names to retain in the dataframe. Defaults to NULL to include all variables.
#' @param properties Variable properties to retain in the dataframe. Defaults to NULL to include all properties.
#' @param row.names NULL or a character vector giving the row names for the data frame. Missing values are not allowed.
#' @param optional Ignored parameter.
#'
#' @returns Dataframe with properties of all model variables, model units, and macros.
#' @export
#' @family build
#' @method as.data.frame sdbuildR_xmile
#'
#' @examples as.data.frame(xmile("SIR"))
#'
#' # Only show stocks
#' as.data.frame(xmile("SIR"), type = "stock")
#'
#' # Only show equation and label
#' as.data.frame(xmile("SIR"), properties = c("eqn", "label"))
#'
as.data.frame.sdbuildR_xmile <- function(x,
                                         row.names = NULL, optional = FALSE,
                                         type = NULL, name = NULL, properties = NULL, ...) {
  check_xmile(x)
  sfm <- x

  # Only keep specified types
  if (!is.null(type)) {
    type <- Filter(nzchar, unique(tolower(type)))

    if (length(type) == 0) {
      stop("At least one type must be specified!")
    }

    # Remove trailing "s" from plural types to show a bit more grace to the user
    type[type != "model_units"] <- gsub("s$", "", type[type != "model_units"])
    type[type == "model_unit"] <- "model_units"

    if (!all(type %in% c("stock", "flow", "constant", "aux", "gf", "model_units", "macro"))) {
      stop("type needs to be one or more of 'stock', 'flow', 'constant', 'aux', 'gf', 'model_units', or 'macro'!")
    }
  }

  df <- data.frame()

  # Add model variables
  nr_var <- sum(lengths(sfm[["model"]][["variables"]]))
  if ((is.null(type) | any(c("stock", "flow", "constant", "aux", "gf") %in% type)) & nr_var > 0) {
    if (!is.null(type)) {
      sfm[["model"]][["variables"]] <- sfm[["model"]][["variables"]][type[type %in% c("stock", "flow", "constant", "aux", "gf")]]
    }

    # Remove func
    sfm[["model"]][["variables"]] <- lapply(sfm[["model"]][["variables"]], function(y) {
      lapply(y, function(x) {
        x["translated_func"] <- NULL
        x["func"] <- NULL

        if (x[["type"]] == "gf") {
          x[["xpts"]] <- paste0(x[["xpts"]], collapse = ", ")
          x[["ypts"]] <- paste0(x[["ypts"]], collapse = ", ")
        }

        return(x)
      })
    })

    # Create dataframe with model variable properties
    model_df <- lapply(sfm[["model"]][["variables"]] |> compact_(), function(x) {
      as.data.frame(do.call(dplyr::bind_rows, x))
    }) |> do.call(dplyr::bind_rows, args = _)
    df <- dplyr::bind_rows(df, model_df)
  }

  # Add model units
  if ((is.null(type) | "model_units" %in% type) & length(sfm[["model_units"]]) > 0) {
    units_df <- as.data.frame(do.call(dplyr::bind_rows, sfm[["model_units"]]))
    units_df[["prefix"]] <- NULL
    units_df[["type"]] <- "model_units"
    df <- dplyr::bind_rows(df, units_df)
  }

  # Add macros
  if ((is.null(type) | "macro" %in% type) & length(sfm[["macro"]]) > 0) {
    # Remove func
    sfm[["macro"]] <- lapply(sfm[["macro"]], function(x) {
      # x["translated_func"] = NULL
      x["func"] <- NULL
      return(x)
    })

    macro_df <- as.data.frame(do.call(dplyr::bind_rows, sfm[["macro"]]))
    macro_df[["type"]] <- "macro"
    df <- dplyr::bind_rows(df, macro_df)
  }

  if (nrow(df) == 0) {
    return(df)
  }

  # Only keep specified names
  if (!is.null(name)) {
    # Check if names exist
    name <- Filter(nzchar, unique(name))

    if (length(name) == 0) {
      stop("At least one name must be specified!")
    }

    idx_exist <- name %in% df[["name"]]
    if (!all(idx_exist)) {
      stop(sprintf(
        "The variable%s %s %s not exist in your model!",
        ifelse(length(name[!idx_exist]) > 1, "s", ""),
        paste0(name[!idx_exist], collapse = ", "),
        ifelse(length(name[!idx_exist]) > 1, "do", "does")
      ))
    }
    df <- df[df[["name"]] %in% name, , drop = FALSE]
    if (nrow(df) == 0) {
      return(df)
    }
  }

  # Only keep specified properties
  if (!is.null(properties)) {
    # Check if properties exist
    properties <- Filter(nzchar, unique(tolower(properties)))
    if (length(properties) == 0) {
      stop("At least one property must be specified!")
    }

    existing_prop <- Reduce(union, get_building_block_prop())
    idx_exist <- properties %in% existing_prop
    prop_in_df <- properties %in% names(df)

    if (!all(idx_exist)) {
      stop(sprintf(
        "%s %s!",
        paste0(properties[!idx_exist], collapse = ", "),
        ifelse(length(properties[!idx_exist]) > 1, "are not existing properties", "is not an existing property")
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
  order_first <- c("type", "name", "eqn", "units", "label", "to", "from", "non_negative")

  # Get columns to prioritize (in order_first order)
  cols_first <- intersect(order_first, names(df))
  # Get remaining columns (in original order)
  cols_rest <- setdiff(names(df), order_first)
  # Combine columns (handles character(0) safely)
  new_cols <- c(cols_first, cols_rest)
  # Reorder data frame
  df <- df[, new_cols, drop = FALSE]

  # Make sure that for all columns, at least one row is not NA or empty
  # This is especially necessary when only interested in one type, e.g. macro or model_units

  # Convert empty strings to NA and keep columns with at least one non-NA
  df[] <- lapply(df, function(x) {
    x[x == ""] <- NA
    x
  })
  df <- df[, colSums(!is.na(df)) > 0, drop = FALSE]

  # Handle row.names if provided
  if (!is.null(row.names)) {
    if (length(row.names) != nrow(df)) {
      stop("Length of row.names (", length(row.names), ") does not match number of rows (", nrow(df), ")")
    }
    rownames(df) <- row.names
  }

  return(df)
}



#' Print overview of stock-and-flow model
#'
#' Print summary of stock-and-flow model, including number of stocks, flows, constants, auxiliaries, graphical functions, macros, custom model units, and use of delay functions. Also prints simulation specifications.
#'
#' @param object Stock-and-flow model of class sdbuildR_xmile
#' @inheritParams plot.sdbuildR_xmile
#'
#' @return NULL
#' @family build
#' @export
#' @seealso [build()]
#'
#' @examples
#' sfm <- xmile("SIR")
#' summary(sfm)
#'
summary.sdbuildR_xmile <- function(object, ...) {
  # Extract model components
  stocks <- names(object[["model"]][["variables"]][["stock"]])
  flows <- names(object[["model"]][["variables"]][["flow"]])
  constants <- names(object[["model"]][["variables"]][["constant"]])
  auxs <- names(object[["model"]][["variables"]][["aux"]])
  gfs <- names(object[["model"]][["variables"]][["gf"]])
  model_units_str <- names(object[["model_units"]])
  macro_str <- lapply(object[["macro"]], `[[`, "property") |>
    unlist() |>
    Filter(nzchar, x = _)

  # Check for delay functions
  delay_past <- get_delay_past(object)
  delay_func <- get_delayN_smoothN(object)
  matched_time_unit <- find_matching_regex(object[["sim_specs"]][["time_units"]], get_regex_time_units())

  # Create structured summary object
  summary_obj <- list(
    model_components = list(
      stocks = stocks,
      flows = flows,
      constants = constants,
      auxiliaries = auxs,
      graphical_functions = gfs,
      custom_units = model_units_str,
      macros = macro_str
    ),
    delay_functions = list(
      delay_past = if (length(delay_past) > 0) unique(names(delay_past)) else character(0),
      delay_func = if (length(delay_func) > 0) unique(names(delay_func)) else character(0)
    ),
    simulation = list(
      start = object[["sim_specs"]][["start"]],
      stop = object[["sim_specs"]][["stop"]],
      dt = object[["sim_specs"]][["dt"]],
      save_at = object[["sim_specs"]][["save_at"]],
      time_units = matched_time_unit,
      method = object[["sim_specs"]][["method"]],
      seed = object[["sim_specs"]][["seed"]],
      language = object[["sim_specs"]][["language"]]
    )
  )

  class(summary_obj) <- "summary.sdbuildR_xmile"
  return(summary_obj)
}


#' Print method for summary.sdbuildR_xmile
#'
#' @param x A summary object of class "summary.sdbuildR_xmile"
#' @param ... Additional arguments (unused)
#'
#' @return Invisibly returns the input object
#' @export
#' @family build
print.summary.sdbuildR_xmile <- function(x, ...) {
  cat("Your model contains:\n")

  # Print model components
  with(x$model_components, {
    cat(sprintf(
      "* %d Stocks%s%s\n",
      length(stocks),
      ifelse(length(stocks) > 0, ": ", ""),
      paste0(stocks, collapse = ", ")
    ))
    cat(sprintf(
      "* %d Flows%s%s\n",
      length(flows),
      ifelse(length(flows) > 0, ": ", ""),
      paste0(flows, collapse = ", ")
    ))
    cat(sprintf(
      "* %d Constants%s%s\n",
      length(constants),
      ifelse(length(constants) > 0, ": ", ""),
      paste0(constants, collapse = ", ")
    ))
    cat(sprintf(
      "* %d Auxiliaries%s%s\n",
      length(auxiliaries),
      ifelse(length(auxiliaries) > 0, ": ", ""),
      paste0(auxiliaries, collapse = ", ")
    ))
    cat(sprintf(
      "* %d Graphical Functions%s%s\n",
      length(graphical_functions),
      ifelse(length(graphical_functions) > 0, ": ", ""),
      paste0(graphical_functions, collapse = ", ")
    ))
    cat(sprintf(
      "* %d Custom model units%s%s\n",
      length(custom_units),
      ifelse(length(custom_units) > 0, ": ", ""),
      paste0(custom_units, collapse = ", ")
    ))
    cat(sprintf(
      "* %d Macro%s\n",
      length(macros),
      ifelse(length(macros) == 1, "", "s")
    ))
  })

  # Print delay functions if present
  if (length(x$delay_functions$delay_past) > 0 || length(x$delay_functions$delay_func) > 0) {
    cat("\nDelay family functions:\n")

    if (length(x$delay_functions$delay_past) > 0) {
      cat(sprintf(
        "* %d variable%s uses past() or delay(): %s\n",
        length(x$delay_functions$delay_past),
        ifelse(length(x$delay_functions$delay_past) == 1, "", "s"),
        paste0(x$delay_functions$delay_past, collapse = ", ")
      ))
    }

    if (length(x$delay_functions$delay_func) > 0) {
      cat(sprintf(
        "* %d variable%s uses delayN() or smoothN(): %s\n",
        length(x$delay_functions$delay_func),
        ifelse(length(x$delay_functions$delay_func) == 1, "", "s"),
        paste0(x$delay_functions$delay_func, collapse = ", ")
      ))
    }
  }

  # Print simulation specifications
  cat(sprintf(
    "\nSimulation time: %s to %s %s (dt = %s%s)\n",
    x$simulation$start, x$simulation$stop, x$simulation$time_units,
    x$simulation$dt,
    ifelse(x$simulation$save_at == x$simulation$dt, "",
      paste0(", save_at = ", x$simulation$save_at)
    )
  ))

  cat(sprintf(
    "Simulation settings: solver %s%s in %s\n",
    x$simulation$method,
    ifelse(is_defined(x$simulation$seed),
      paste0(" and seed ", x$simulation$seed), ""
    ),
    x$simulation$language
  ))

  invisible(x)
}
