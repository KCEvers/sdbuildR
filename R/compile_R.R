#' Simulate stock-and-flow model in R
#'
#' @inheritParams simulate
#'
#' @return List with variables created in the simulation script
#' @noRd
#'
simulate_R <- function(sfm,
                       keep_nonnegative_flow,
                       keep_nonnegative_stock,
                       verbose,
                       only_stocks) {
  # Collect arguments
  argg <- c(
    as.list(environment())
  )
  # Remove NULL arguments
  argg <- argg[!lengths(argg) == 0]

  # Compile script without plot
  script <- compile_R(sfm,
    keep_nonnegative_flow = keep_nonnegative_flow,
    keep_nonnegative_stock = keep_nonnegative_stock,
    only_stocks = only_stocks
  )

  # Evaluate script
  sim <- tryCatch(
    {
      # Create a new environment to collect variables
      envir <- new.env()

      start_t <- Sys.time()

      # Evaluate script
      eval(parse(text = script), envir = envir)

      end_t <- Sys.time()

      if (verbose) {
        message(paste0("Simulation took ", round(end_t - start_t, 4), " seconds"))
      }

      out <- list()
      out[[.sdbuildR_env[["P"]][["sim_df_name"]]]] <- envir[[.sdbuildR_env[["P"]][["sim_df_name"]]]]
      out[["init"]] <- unlist(envir[[.sdbuildR_env[["P"]][["initial_value_name"]]]])
      out[["constants"]] <- unlist(envir[[.sdbuildR_env[["P"]][["parameter_name"]]]])
      out[["keep_unit"]] <- FALSE
      out[["script"]] <- script
      out[["success"]] <- TRUE
      out[["duration"]] <- end_t - start_t

      out |>
        utils::modifyList(argg) |>
        structure(class = "sdbuildR_sim")
    },
    error = function(e) {
      warning("\nAn error occurred while running the R script.")
      list(
        success = FALSE,
        error_message = e[["message"]], script = script
      ) |>
        structure(class = "sdbuildR_sim")
    }
  )


  return(sim)
}


#' Compile R script to simulate stock-and-flow model
#'
#' @inheritParams simulate
#'
#' @return R script
#' @noRd
#'
compile_R <- function(sfm,
                      keep_nonnegative_flow,
                      keep_nonnegative_stock,
                      only_stocks,
                      verbose) {
  # Get flows and connections
  flow_df <- get_flow_df(sfm)

  sfm[["model"]][["variables"]][["stock"]] <- lapply(
    sfm[["model"]][["variables"]][["stock"]],
    function(x) {
      x[["inflow"]] <- flow_df[flow_df[["to"]] == x[["name"]], "name"]
      x[["outflow"]] <- flow_df[flow_df[["from"]] == x[["name"]], "name"]

      if (length(x[["inflow"]]) == 0) {
        x[["inflow"]] <- ""
      }
      if (length(x[["outflow"]]) == 0) {
        x[["outflow"]] <- ""
      }

      return(x)
    }
  )

  # Check model for unit strings
  eqn_units <- find_unit_strings(sfm)

  # Stop if equations contain unit strings
  if (length(eqn_units) > 0) {
    stop(paste0("The model contains unit strings u(''), which are not supported for simulations in R.\nSet sim_specs(sfm, language = 'Julia') or modify the equations of these variables:\n\n", paste0(names(eqn_units), collapse = ", ")))
  }

  # Check model for delayN() and smoothN() functions
  delayN_smoothN <- get_delayN_smoothN(sfm)

  # Check model for delay() and past() functions
  delay_past <- get_delay_past(sfm)

  if (length(delayN_smoothN) > 0) {
    stop(paste0("The model contains either delayN() or smoothN(), which are not supported for simulations in R.\nSet sfm |> sim_specs(language = 'Julia') or modify the equations of these variables: ", paste0(names(delayN_smoothN), collapse = ", ")))
  }

  if (length(delay_past) > 0) {
    stop(paste0("The model contains either delay() or past(), which are not supported for simulations in R.\nSet sfm |> sim_specs(language = 'Julia') or modify the equations of these variables: ", paste0(names(delay_past), collapse = ", ")))
  }

  # # Convert conveyors
  # sfm = convert_conveyor(sfm)

  # Compile all parts of the R script
  times <- compile_times(sfm)
  # constraints = compile_constraints(sfm)
  ordering <- order_equations(sfm)

  # Only need to save stocks if there are no dynamic variables
  only_stocks <- ifelse(is.null(ordering[["dynamic"]][["order"]]), TRUE, only_stocks)

  # Order Stocks alphabetically to match ordering in init
  sfm[["model"]][["variables"]][["stock"]] <- sfm[["model"]][["variables"]][["stock"]][sort(names(sfm[["model"]][["variables"]][["stock"]]))]

  # Macros
  macros <- compile_macros(sfm)

  # # Add prefixes (constants$ and init$) to static equations
  # sfm = substitute_var(sfm)

  # Prepare equations
  sfm <- prep_equations_variables(sfm, keep_nonnegative_flow)

  # Static equations
  static_eqn <- compile_static_eqn(sfm, ordering)

  # Stocks
  sfm <- prep_stock_change(sfm)
  nonneg_stocks <- compile_nonneg_stocks(sfm, keep_nonnegative_stock)

  zeallot_def <- compile_destructuring_assign(sfm, static_eqn)

  seed_str <- ifelse(!is_defined(sfm[["sim_specs"]][["seed"]]), "",
    sprintf("# Ensure reproducibility across runs in case of random elements
set.seed(%s)", as.character(sfm[["sim_specs"]][["seed"]]))
  )


  prep_script <- sprintf("# Script generated on %s by sdbuildR.

# Load packages
%s
library(sdbuildR)

%s
%s
%s
%s
", Sys.time(), zeallot_def[["script"]], seed_str, times[["script"]], macros[["script"]], nonneg_stocks[["func_def"]])


  ode <- compile_ode(
    sfm, ordering, prep_script, static_eqn,
    keep_nonnegative_flow, keep_nonnegative_stock,
    only_stocks
  )
  run_ode <- compile_run_ode(sfm, nonneg_stocks)

  script <- paste0(
    prep_script, "\n",
    ode[["script"]],
    static_eqn[["script"]],
    run_ode[["script"]]
  )


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
  }

  return(script)
}


#' Find unit strings
#'
#' @inheritParams build
#'
#' @returns List with unit strings
#' @noRd
find_unit_strings <- function(sfm) {
  # Extract all unit strings from equations
  var_units <- lapply(sfm[["model"]][["variables"]], function(y) {
    lapply(y, function(x) {
      if (is_defined(x[["eqn"]])) {
        return(stringr::str_extract_all(x[["eqn"]], "\\bu\\([\"|'](.*?)[\"|']\\)"))
      }
    })
  }) |>
    unname() |>
    unlist()

  # Extract all unit strings from macros
  macro_units <- lapply(sfm[["macro"]], function(x) {
    if (is_defined(x[["eqn"]])) {
      return(stringr::str_extract_all(x[["eqn"]], "\\bu\\([\"|'](.*?)[\"|']\\)"))
    }
    return(x)
  }) |> unlist()

  eqn_units <- c(var_units, macro_units)

  return(eqn_units)
}


#' Compile script for enabling destructuring assignment in R
#'
#' @inheritParams build
#' @inheritParams compile_ode
#'
#' @return List with necessary scripts
#' @noRd
#'
compile_destructuring_assign <- function(sfm, static_eqn) {
  # Add package for destructuring assignment in case it was used
  eqns <- c(static_eqn[["script"]], unlist(
    # purrr::map_depth(sfm[["model"]][["variables"]], 2, "eqn")
    lapply(
      sfm[["model"]][["variables"]],
      function(x) {
        lapply(x, `[[`, "eqn")
      }
    )
  ))

  if (any(stats::na.omit(stringr::str_detect(eqns, stringr::fixed("%<-%"))))) {
    script <- "\n# Add package for destructuring assignment\nif (!require('zeallot')) install.packages('zeallot'); library(zeallot)\n"
  } else {
    script <- ""
  }

  return(list(script = script))
}


#' Add prefixes to static equations
#'
#' @inheritParams build
#' @inheritParams compile_R
#' @inheritParams order_equations
#'
#' @return Updated stock-and-flow model
#' @noRd
#'
substitute_var_old <- function(sfm) {
  # Replace variable references in static equations
  static_var <- c(names(sfm[["model"]][["variables"]][["stock"]]), names(sfm[["model"]][["variables"]][["constant"]]))
  dynamic_var <- c(
    names(sfm[["model"]][["variables"]][["flow"]]),
    setdiff(names(sfm[["model"]][["variables"]][["aux"]]), names(sfm[["model"]][["variables"]][["constant"]]))
  )

  static_replacements <-
    c(
      paste0(.sdbuildR_env[["P"]][["initial_value_name"]], "$", names(sfm[["model"]][["variables"]][["stock"]])),
      paste0(.sdbuildR_env[["P"]][["parameter_name"]], "$", names(sfm[["model"]][["variables"]][["constant"]]))
    ) |>
    as.list() |>
    stats::setNames(static_var) |>
    # Convert to expressions for substitutions
    lapply(function(x) {
      parse(text = x)[[1]]
    })

  # Implement replacements
  sfm[["model"]][["variables"]] <- lapply(sfm[["model"]][["variables"]], function(y) {
    lapply(y, function(x) {
      if (x[["name"]] %in% static_var) {
        expr1 <- parse(text = x[["eqn"]])
        replace_expr <- do.call("substitute", list(expr1[[1]], static_replacements))
        x[["eqn"]] <- deparse1(replace_expr)

        # x[["eqn"]] = stringr::str_replace_all(x[["eqn"]], static_replacements)
      }

      return(x)
    })
  })

  return(sfm)
}





#' Compile script for global variables
#'
#' @inheritParams build
#'
#' @return List with macro script
#' @noRd
compile_macros <- function(sfm) {
  script <- ""

  # If there are macros
  if (any(nzchar(unlist(lapply(sfm[["macro"]], `[[`, "eqn"))))) {
    script <- paste0(
      script, "\n",
      lapply(sfm[["macro"]], function(x) {
        # If a name is defined, assign macro to that name
        if (nzchar(x[["name"]])) {
          return(paste0(x[["name"]], " = ", x[["eqn"]]))
        } else {
          return(x[["eqn"]])
        }
      }) |> unlist() |> paste0(collapse = "\n")
    )
  }

  if (nzchar(script)) {
    script <- paste0("\n\n# User-specified macros\n", script, "\n")
  }

  return(list(script = script))
}


#' Compile script for creating time vector
#'
#' @return List
#' @importFrom rlang .data
#' @inheritParams compile_R
#' @noRd
#'
compile_times <- function(sfm) {
  script <- sprintf(
    "
# Define time sequence
%s = %s
%s <- seq(from=%s, to=%s, by=%s) # needs to be global to be used in step, pulse, ramp, seasonal functions
%s = %s[1]

# Simulation time unit (smallest time scale in your model)
%s = '%s'
",
    .sdbuildR_env[["P"]][["timestep_name"]],
    as.character(sfm[["sim_specs"]][["dt"]]),
    .sdbuildR_env[["P"]][["times_name"]],
    as.character(sfm[["sim_specs"]][["start"]]),
    as.character(sfm[["sim_specs"]][["stop"]]),
    .sdbuildR_env[["P"]][["timestep_name"]],
    .sdbuildR_env[["P"]][["time_name"]], .sdbuildR_env[["P"]][["times_name"]],
    .sdbuildR_env[["P"]][["time_units_name"]],
    sfm[["sim_specs"]][["time_units"]]
  )

  .sdbuildR_env[["times"]] <- seq(
    from = as.numeric(sfm[["sim_specs"]][["start"]]),
    to = as.numeric(sfm[["sim_specs"]][["stop"]]),
    by = as.numeric(sfm[["sim_specs"]][["dt"]])
  )

  return(list(script = script))
}



#' Compile script for setting minimum and maximum constraints
#'
#'
#' @return List
#' @importFrom rlang .data
#' @inheritParams build
#' @noRd
#'
compile_constraints_old <- function(sfm) {
  # Compile string of minimum and maximum constraints
  constraint_def <- lapply(sfm[["model"]][["variables"]], function(x) {
    lapply(x, function(y) {
      if ("min" %in% names(y)) {
        min_str <- ifelse(is_defined(y[["min"]]), paste0("min = ", y[["min"]]), "")
        max_str <- ifelse(is_defined(y[["max"]]), paste0("max = ", y[["max"]]), "")

        if (nzchar(min_str) | nzchar(max_str)) {
          return(sprintf("%s = c(%s%s%s)", x[["name"]], min_str, ifelse(nzchar(min_str) & nzchar(max_str), ", ", ""), max_str))
        }
      }
    })
  }) |>
    unlist() |>
    paste0(collapse = ",\n\t\t\t\t\t\t\t\t\t\t")

  script <- ifelse(nzchar(constraint_def), sprintf("\n\n# Constraints of minimum and maximum value\n%s = list(%s) |> \n\tget_logical_constraints()\n", .sdbuildR_env[["P"]][["constraint_def"]], constraint_def), "")

  update_ode <- ifelse(nzchar(constraint_def), sprintf("\n\n\t\t# Check constraints\n\t\tcheck_constraints(%s, environment(), %s)\n", .sdbuildR_env[["P"]][["constraint_def"]], .sdbuildR_env[["P"]][["time_name"]]), "")

  return(list(
    script = script,
    update_ode = update_ode
  ))
}


#' Compile script for static variables, i.e. initial conditions, functions, and parameters
#'
#' @inheritParams compile_R
#' @inheritParams order_equations
#' @param ordering List with order of static and dynamic variables, output of order_equations()
#'
#' @noRd
#'
#' @return List with necessary scripts
#'
compile_static_eqn <- function(sfm, ordering) {
  # Macros
  macros_script <- ifelse(is_defined(sfm[["macro"]][["eqn"]]),
    sprintf(
      "\n\n# User-defined macros and globals\n%s\n",
      paste0(sfm[["macro"]][["eqn"]], collapse = "\n")
    ), ""
  )

  # Graphical functions
  gf_eqn <- lapply(sfm[["model"]][["variables"]][["gf"]], `[[`, "eqn_str")

  # Constant equations
  constant_eqn <- lapply(sfm[["model"]][["variables"]][["constant"]], `[[`, "eqn_str")

  # Initial states of Stocks
  stock_eqn <- lapply(sfm[["model"]][["variables"]][["stock"]], `[[`, "eqn_str")

  if (ordering[["static_and_dynamic"]][["issue"]]) {
    # Compile and order static equations
    static_eqn_str <- c(gf_eqn, constant_eqn, stock_eqn)[ordering[["static"]][["order"]]] |>
      unlist() |>
      paste0(collapse = "\n")
    static_eqn_str
  } else {
    # Auxiliary equations (dynamic auxiliaries)
    aux_eqn <- lapply(sfm[["model"]][["variables"]][["aux"]], `[[`, "eqn_str")

    # Flow equations
    flow_eqn <- lapply(sfm[["model"]][["variables"]][["flow"]], `[[`, "eqn_str")

    # Compile and order static and dynamic equations
    static_eqn_str <- c(
      gf_eqn, constant_eqn, stock_eqn,
      aux_eqn, flow_eqn
    )[ordering[["static_and_dynamic"]][["order"]]] |>
      unlist() |>
      paste0(collapse = "\n")
    static_eqn_str
  }

  # Put parameters together
  if (length(sfm[["model"]][["variables"]][["constant"]]) > 0) {
    constants_def <- paste0("\n\n# Define parameters in named list\n", .sdbuildR_env[["P"]][["parameter_name"]], " = list(", paste0(paste0(names(constant_eqn), " = ", names(constant_eqn)), collapse = ", "), ")\n")
  } else {
    constants_def <- paste0("\n\n# Define empty parameters\n", .sdbuildR_env[["P"]][["parameter_name"]], " = list()\n")
  }


  # Define init
  init_def <- paste0(
    "\n\n# Define initial condition\n", .sdbuildR_env[["P"]][["initial_value_name"]], " = c(",
    paste0(paste0(names(stock_eqn), " = ", names(stock_eqn)), collapse = ", "), ")"
  )


  return(list(script = paste0(
    macros_script,
    #                               sprintf("\n\n# Set-up parameters and initial condition
    # %s = list()
    # %s = list()", .sdbuildR_env[["P"]][["parameter_name"]], .sdbuildR_env[["P"]][["initial_value_name"]]),
    "\n\n# Define parameters, initial conditions, and functions in correct order\n",
    static_eqn_str,
    # sprintf("\n\n# Turn initial condition into alphabetically ordered named vector\n%s = unlist(%s)[order(names(%s))]", .sdbuildR_env[["P"]][["initial_value_name"]], .sdbuildR_env[["P"]][["initial_value_name"]], .sdbuildR_env[["P"]][["initial_value_name"]])
    constants_def, init_def
  )))
}



#' Prepare equations and variables in stock-and-flow model
#'
#' @inheritParams compile
#'
#' @returns Updated stock-and-flow model
#' @noRd
prep_equations_variables <- function(sfm, keep_nonnegative_flow) {
  # Graphical functions
  sfm[["model"]][["variables"]][["gf"]] <- lapply(sfm[["model"]][["variables"]][["gf"]], function(x) {
    if (is_defined(x[["xpts"]])) {
      if (inherits(x[["xpts"]], "numeric")) {
        xpts_str <- paste0("c(", paste0(as.character(x[["xpts"]]), collapse = ", "), ")")
      } else {
        xpts_str <- x[["xpts"]]
      }

      # ypts is not obligatory
      if (!is_defined(x[["ypts"]])) {
        ypts_str <- ""
      } else {
        if (inherits(x[["ypts"]], "numeric")) {
          x[["ypts"]] <- paste0("c(", paste0(as.character(x[["ypts"]]), collapse = ", "), ")")
        }
        ypts_str <- sprintf("\n\t\ty = %s,", x[["ypts"]])
      }

      x[["eqn_str"]] <- sprintf(
        "%s = stats::approxfun(x = %s,%s\n\t\tmethod = '%s', rule = %s)",
        x[["name"]], xpts_str,
        ypts_str,
        x[["interpolation"]], ifelse(x[["extrapolation"]] == "nearest", 2,
          ifelse(x[["extrapolation"]] == "NA", 1, x[["extrapolation"]])
        )
      )
    }

    return(x)
  })

  # Constant equations
  sfm[["model"]][["variables"]][["constant"]] <- lapply(sfm[["model"]][["variables"]][["constant"]], function(x) {
    x[["eqn_str"]] <- paste0(x[["name"]], " = ", x[["eqn"]])
    return(x)
  })

  # Initial states of Stocks
  sfm[["model"]][["variables"]][["stock"]] <- lapply(sfm[["model"]][["variables"]][["stock"]], function(x) {
    x[["eqn_str"]] <- paste0(x[["name"]], " = ", x[["eqn"]])

    return(x)
  })

  # Auxiliary equations (dynamic auxiliaries)
  sfm[["model"]][["variables"]][["aux"]] <- lapply(sfm[["model"]][["variables"]][["aux"]], function(x) {
    x[["eqn_str"]] <- sprintf("%s <- %s", x[["name"]], x[["eqn"]])

    if (!is.null(x[["preceding_eqn"]])) {
      x[["eqn_str"]] <- c(x[["preceding_eqn"]], x[["eqn_str"]])
    }
    return(x)
  })

  # Flow equations
  sfm[["model"]][["variables"]][["flow"]] <- lapply(sfm[["model"]][["variables"]][["flow"]], function(x) {
    x[["eqn_str"]] <- sprintf(
      "%s <- %s%s%s # Flow%s%s",
      x[["name"]],
      ifelse(x[["non_negative"]], "nonnegative(", ""),
      x[["eqn"]],
      ifelse(x[["non_negative"]], "\n\t\t)", ""),
      # Add comment
      ifelse(is_defined(x[["from"]]), paste0(" from ", x[["from"]]), ""),
      ifelse(is_defined(x[["to"]]), paste0(" to ", x[["to"]]), "")
    )

    if (!is.null(x[["preceding_eqn"]])) {
      x[["eqn_str"]] <- c(x[["preceding_eqn"]], x[["eqn_str"]])
    }
    return(x)
  })


  return(sfm)
}




#' Prepare for summing change in stocks in stock-and-flow model
#'
#' @inheritParams build
#' @inheritParams compile_R
#'
#' @noRd
#' @return Updated stock-and-flow model
#'
prep_stock_change <- function(sfm) {
  # Add temporary property to sum change in Stocks
  sfm[["model"]][["variables"]][["stock"]] <- lapply(sfm[["model"]][["variables"]][["stock"]], function(x) {
    if (!is.null(x[["delayN"]])) {
      x[["sum_name"]] <- paste0(x[["inflow"]], "$update")
      x[["sum_eqn"]] <- ""
      x[["sum_units"]] <- ""
    } else {
      inflow <- outflow <- ""
      x[["sum_name"]] <- paste0(.sdbuildR_env[["P"]][["change_prefix"]], x[["name"]])

      y_str <- paste0(.sdbuildR_env[["P"]][["change_prefix"]], x[["name"]])

      # In case no inflow and no outflow is defined, update with 0
      if (!is_defined(x[["inflow"]]) & !is_defined(x[["outflow"]])) {
        x[["sum_eqn"]] <- "0"
      } else {
        if (is_defined(x[["inflow"]])) {
          inflow <- paste0(x[["inflow"]], collapse = " + ")
        }
        if (is_defined(x[["outflow"]])) {
          outflow <- paste0(paste0(" - ", x[["outflow"]]), collapse = "")
        }
        x[["sum_eqn"]] <- sprintf("%s%s", inflow, outflow)
      }
      x[["sum_units"]] <- ""
    }
    return(x)
  }) |> compact_()

  sfm <- validate_xmile(sfm)

  return(sfm)
}



#' Compile script for non-negative Stocks
#'
#' @inheritParams build
#' @inheritParams compile_R
#'
#' @noRd
#' @return List with necessary scripts for ensuring non-negative Stocks
#'
compile_nonneg_stocks <- function(sfm, keep_nonnegative_stock) {
  # Non-negative Stocks
  nonneg_stock <- which(unlist(lapply(sfm[["model"]][["variables"]][["stock"]], `[[`, "non_negative")))

  if (keep_nonnegative_stock & length(nonneg_stock) > 0) {
    func_def <- sprintf(
      "
# Ensure non-negativity of (selected) Stocks
%s = which(names(%s) %%in%% c(%s))

# Define root function to be triggered when non-negative Stocks go below 0
%s <- function (%s, %s, %s) {
  return(ifelse(any(%s[%s] < 0), 0, 1))
}

# Set non-negative Stocks to zero when root function is triggered
%s <- function(%s, %s, %s) {
  %s[%s] = 0
  return(%s)
}
",
      .sdbuildR_env[["P"]][["nonneg_stock_name"]], .sdbuildR_env[["P"]][["initial_value_name"]],
      paste0("'", names(nonneg_stock), "'", collapse = ", "),
      .sdbuildR_env[["P"]][["rootfun_name"]], .sdbuildR_env[["P"]][["time_name"]], .sdbuildR_env[["P"]][["state_name"]], .sdbuildR_env[["P"]][["parameter_name"]],
      .sdbuildR_env[["P"]][["state_name"]], .sdbuildR_env[["P"]][["nonneg_stock_name"]],
      .sdbuildR_env[["P"]][["eventfun_name"]], .sdbuildR_env[["P"]][["time_name"]], .sdbuildR_env[["P"]][["state_name"]], .sdbuildR_env[["P"]][["parameter_name"]],
      .sdbuildR_env[["P"]][["state_name"]], .sdbuildR_env[["P"]][["nonneg_stock_name"]], .sdbuildR_env[["P"]][["state_name"]]
    )

    root_arg <- sprintf(
      ",\n\t\t\t\tevents = list(func = %s, root = TRUE), rootfun = %s",
      .sdbuildR_env[["P"]][["eventfun_name"]], .sdbuildR_env[["P"]][["rootfun_name"]]
    )

    check_root <- sprintf("
# Times at which non-negative Stocks fell below 0
attributes(%s)$troot

# Values of non-negative Stocks when root function was triggered
attributes(%s)$valroot
", .sdbuildR_env[["P"]][["out_name"]], .sdbuildR_env[["P"]][["out_name"]])

    return(list(
      func_def = func_def,
      root_arg = root_arg,
      check_root = check_root
    ))
  } else {
    return(list(
      func_def = "",
      root_arg = "",
      check_root = ""
    ))
  }
}


#' Compile script for ODE function passed to deSolve::ode
#'
#' @inheritParams build
#' @inheritParams compile
#' @inheritParams order_equations
#' @inheritParams compile_static_eqn
#' @param prep_script Intermediate output of compile_R()
#' @param static_eqn Output of compile_static_eqn()
#'
#' @return List
#' @importFrom rlang .data
#' @noRd
#'
compile_ode <- function(sfm, ordering, prep_script, static_eqn,
                        # constraints,
                        keep_nonnegative_flow, keep_nonnegative_stock,
                        only_stocks) {
  # @param constraints Output of compile_constraints()

  # Auxiliary equations (dynamic auxiliaries)
  aux_eqn <- lapply(sfm[["model"]][["variables"]][["aux"]], `[[`, "eqn_str")

  # Flow equations
  flow_eqn <- lapply(sfm[["model"]][["variables"]][["flow"]], `[[`, "eqn_str")

  # Compile and order all dynamic equations
  dynamic_eqn <- unlist(c(aux_eqn, flow_eqn)[ordering[["dynamic"]][["order"]]])
  dynamic_eqn

  # Compile and order all dynamic equations
  dynamic_eqn_str <- paste0(dynamic_eqn, collapse = "\n\t\t")

  # Sum change in stock equations
  stock_change <- lapply(sfm[["model"]][["variables"]][["stock"]], function(x) {
    if (!is.null(x[["delayN"]])) {
      return(NULL)
    } else {
      paste0(x[["sum_name"]], " <- ", x[["sum_eqn"]])
    }
  })
  stock_change <- stock_change[lengths(stock_change) > 0]

  # Compile stock changes in one string
  stock_change_str <- paste0(stock_change, collapse = "\n\t\t")

  # Get names of summed change in stocks
  stock_changes_names <- unlist(lapply(sfm[["model"]][["variables"]][["stock"]], `[[`, "sum_name"))

  state_change_str <- paste0(
    .sdbuildR_env[["P"]][["change_state_name"]], " = c(",
    paste0(unname(stock_changes_names), collapse = ", "), ")"
  )

  # Graphical functions (gf)
  if (length(sfm[["model"]][["variables"]][["gf"]]) > 0) {
    # Some gf have other gf as source; recursively replace
    gf_sources <- unlist(lapply(sfm[["model"]][["variables"]][["gf"]], `[[`, "source"))

    if (length(gf_sources) > 0) {
      dict <- paste0(names(gf_sources), "(", unname(gf_sources), ")") |> stats::setNames(names(gf_sources))

      dict2 <- paste0("(", names(gf_sources), "(", unname(gf_sources), "))") |> stats::setNames(paste0("\\(", stringr::str_escape(names(gf_sources)), "\\)"))

      gf_str <- stringr::str_replace_all(unname(dict), dict2)

      gf_str <- paste0(", ", paste0(paste0("'", gf_str, "' = "), gf_str, collapse = ", "))
    } else {
      gf_str <- ""
    }
  } else {
    gf_str <- ""
  }

  # Save all variables in return statement
  if (!only_stocks) {
    # Filter out functions in case they are in auxiliaries
    save_var_str <- paste0(
      ", Filter(Negate(is.function), c(",
      paste0(paste0(names(dynamic_eqn), " = ", names(dynamic_eqn)), collapse = ", "),
      gf_str, "))"
    )
  } else {
    save_var_str <- ""
  }

  S_str <- sprintf("%s = as.list(%s)", .sdbuildR_env[["P"]][["state_name"]], .sdbuildR_env[["P"]][["state_name"]])

  # Compile
  script <- sprintf(
    "\n\n# Define ODE
%s = function(%s, %s, %s){

  %s

  \n# Compute change in stocks at current time %s
  with(c(%s, %s), {

    # Update auxiliaries and flows
    %s

    # Collect inflows and outflows for each stock
    %s

    # Combine change in stocks
    %s

    return(list(%s%s))
  })
}", .sdbuildR_env[["P"]][["ode_func_name"]], .sdbuildR_env[["P"]][["time_name"]], .sdbuildR_env[["P"]][["state_name"]], .sdbuildR_env[["P"]][["parameter_name"]],
    S_str,
    .sdbuildR_env[["P"]][["time_name"]], .sdbuildR_env[["P"]][["state_name"]], .sdbuildR_env[["P"]][["parameter_name"]],
    dynamic_eqn_str,
    stock_change_str,
    state_change_str,
    .sdbuildR_env[["P"]][["change_state_name"]], save_var_str
  )

  return(list(script = script))
}


#' Compile script for running ODE
#'
#' @param nonneg_stocks Output of compile_nonneg_stocks()
#' @inheritParams compile_ode
#'
#' @return List
#' @inheritParams compile_R
#' @noRd
#'
compile_run_ode <- function(sfm, nonneg_stocks) {
  script <- sprintf(
    "\n\n# Run ODE
%s = as.data.frame(deSolve::%s(
  func=%s,
  y=%s,
  times=%s,
  parms=%s,
  method = '%s'%s
)) %s
", .sdbuildR_env[["P"]][["sim_df_name"]], "ode",
    .sdbuildR_env[["P"]][["ode_func_name"]],
    .sdbuildR_env[["P"]][["initial_value_name"]],
    .sdbuildR_env[["P"]][["times_name"]], .sdbuildR_env[["P"]][["parameter_name"]],
    sfm[["sim_specs"]][["method"]],
    nonneg_stocks[["root_arg"]],
    nonneg_stocks[["check_root"]]
  )

  # If different times need to be saved, linearly interpolate
  if (sfm[["sim_specs"]][["dt"]] != sfm[["sim_specs"]][["save_at"]] |
    sfm[["sim_specs"]][["start"]] != sfm[["sim_specs"]][["save_from"]]) {
    script <- paste0(
      script, "\n# Linearly interpolate to reduce stored values to save_at\n",
      "new_times = seq(", sfm[["sim_specs"]][["save_from"]], ", ",
      sfm[["sim_specs"]][["stop"]], ", by = ",
      sfm[["sim_specs"]][["save_at"]], ")\n",
      # Create new time vector\n",

      .sdbuildR_env[["P"]][["sim_df_name"]], " = ", .sdbuildR_env[["P"]][["saveat_func"]], "(", .sdbuildR_env[["P"]][["sim_df_name"]], ", 'time', new_times)\n"
    )
  }

  # Convert to long format
  script <- paste0(
    script,
    sprintf(
      "# Wide to long
    %s <- stats::reshape(
       data = as.data.frame(%s),
       direction = \"long\",
       idvar = \"time\",
       varying = colnames(%s)[colnames(%s) != \"time\"],
       v.names = \"value\",
       timevar = \"variable\",
       # Ensure variable names are used
       times = colnames(%s)[colnames(%s) != \"time\"]
     ) |> magrittr::set_rownames(NULL)", .sdbuildR_env[["P"]][["sim_df_name"]],
      .sdbuildR_env[["P"]][["sim_df_name"]], .sdbuildR_env[["P"]][["sim_df_name"]],
      .sdbuildR_env[["P"]][["sim_df_name"]], .sdbuildR_env[["P"]][["sim_df_name"]],
      .sdbuildR_env[["P"]][["sim_df_name"]]
    )
  )


  return(list(script = script))
}



#' Compile script for plotting ODE
#'
#' @inheritParams compile_ode
#'
#' @return List
#' @inheritParams compile_R
#' @noRd
#'
compile_plot_ode <- function(sfm) {
  # Add sources of graphical functions if necessary
  var_names <- get_model_var(sfm)
  plot_var <- var_names[var_names %in% sfm[["display_var"]]]

  script <- sprintf("
# Plot ODE
plot_sim(sfm, %s)", .sdbuildR_env[["P"]][["sim_df_name"]])

  return(list(script = script))
}
