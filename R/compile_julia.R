#' Simulate stock-and-flow model in Julia
#'
#' @inheritParams simulate
#'
#' @return List with variables created in the simulation script
#' @noRd
#'
simulate_julia <- function(sfm,
                           keep_nonnegative_flow,
                           keep_nonnegative_stock,
                           keep_unit,
                           only_stocks,
                           verbose) {
  # Collect arguments
  argg <- c(as.list(environment()))
  # Remove NULL arguments
  argg <- argg[!lengths(argg) == 0]

  # Get output filepaths
  filepath_sim <- get_tempfile(fileext = ".csv")
  filepath <- get_tempfile(fileext = ".jl")

  # Compile script
  script <- compile_julia(sfm,
    filepath_sim = filepath_sim,
    ensemble_pars = NULL,
    keep_nonnegative_flow = keep_nonnegative_flow,
    keep_nonnegative_stock = keep_nonnegative_stock,
    only_stocks = only_stocks,
    keep_unit = keep_unit
  )
  write_script(script, filepath)
  script <- paste0(readLines(filepath), collapse = "\n")

  # Evaluate script
  sim <- tryCatch(
    {
      use_julia()

      # Evaluate script
      start_t <- Sys.time()

      # Wrap in invisible and capture.output to not show message of units module being overwritten
      out = invisible({
        utils::capture.output({
          JuliaConnectoR::juliaEval(paste0('include("', filepath, '")'))
          })
        })

      end_t <- Sys.time()

      if (verbose) {
        message(paste0("Simulation took ", round(end_t - start_t, 4), " seconds"))
      }

      # Read the constants
      constants <- as.numeric(JuliaConnectoR::juliaEval(.sdbuildR_env[["P"]][["parameter_name"]]))
      names(constants) <- JuliaConnectoR::juliaEval(.sdbuildR_env[["P"]][["parameter_names"]])

      # Read the initial values of stocks
      init <- as.numeric(JuliaConnectoR::juliaEval(.sdbuildR_env[["P"]][["initial_value_name"]]))
      names(init) <- JuliaConnectoR::juliaEval(.sdbuildR_env[["P"]][["initial_value_names"]])

      df <- as.data.frame(data.table::fread(filepath_sim, na.strings = c("", "NA")))

      # Delete files
      file.remove(filepath)
      file.remove(filepath_sim)

      list(
        success = TRUE,
        df = df,
        init = init,
        constants = constants,
        script = script,
        duration = end_t - start_t
      ) |>
        utils::modifyList(argg) |>
        structure(class = "sdbuildR_sim")
    },
    error = function(e) {
      warning("\nAn error occurred while running the Julia script.")
      list(
        success = FALSE, error_message = e[["message"]],
        script = script
      ) |>
        utils::modifyList(argg) |>
        structure(class = "sdbuildR_sim")
    }
  )


  return(sim)
}


#' Compile Julia script to simulate stock-and-flow model
#'
#' @inheritParams simulate
#' @param ensemble_pars List; parameters for the ensemble simulation. Defaults to NULL to not run an ensemble and simply a regular trajectory.
#'
#' @return Julia script
#' @noRd
#'
compile_julia <- function(sfm, filepath_sim,
                          ensemble_pars,
                          keep_nonnegative_flow,
                          keep_nonnegative_stock,
                          keep_unit, only_stocks) {

  # Add "inflow" and "outflow" entries to stocks to match flow "to" and "from" entries
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

  # Adjust keep_unit to FALSE if there are no units defined
  names_df <- get_names(sfm)
  var_names <- get_model_var(sfm)

  # ** check
  names_df_no_flow <- names_df
  # Don't check whether flows have units because these are automatically added
  names_df_no_flow <- names_df_no_flow[names_df_no_flow[["type"]] != "flow", ]
  # keep_unit = ifelse(!any(nzchar(names_df_no_flow$units) & names_df_no_flow$units != "1"), FALSE, keep_unit)

  all_eqns <- c(
    lapply(
      sfm[["model"]][["variables"]],
      function(x) {
        lapply(x, `[[`, "eqn")
      }
    ) |> unlist(),
    unlist(lapply(sfm[["macro"]], `[[`, "eqn"))
  )
  units_used <- unlist(stringr::str_extract_all(all_eqns, "\\bu\\([\"|'](.*?)[\"|']\\)"))

  keep_unit <- ifelse(!any(names_df_no_flow[["units"]] != "1" & nzchar(names_df_no_flow[["units"]])) & length(units_used) == 0, FALSE, keep_unit)

  # if (keep_unit){
  #   # Ensure all units are defined
  #   add_model_units = detect_undefined_units(sfm,
  #                                      new_eqns = c(sfm[["model"]][["variables"]] |> lapply(function(x){lapply(x, `[[`, "eqn_julia")}) |> unlist(),
  #                                                   unlist(lapply(sfm[["macro"]], `[[`, "eqn_julia"))),
  #                                      new_units = sfm[["model"]][["variables"]] |> lapply(function(x){lapply(x, `[[`, "units")}) |> unlist())
  #   sfm[["model_units"]] = add_model_units |> utils::modifyList(sfm[["model_units"]])
  # }


  # **# Convert conveyors
  # sfm = convert_conveyor(sfm)
  #


  # Order stocks alphabetically for order in init_names and init
  sfm[["model"]][["variables"]][["stock"]] <- sfm[["model"]][["variables"]][["stock"]][sort(names(sfm[["model"]][["variables"]][["stock"]]))]


  # Check keyword arguments are not used for custom functions in Julia
  check_no_keyword_arg(sfm, var_names)

  # Prepare model for ensemble range if specified
  out <- prep_ensemble_range(sfm, ensemble_pars)
  sfm <- out[["sfm"]]
  ensemble_pars <- out[["ensemble_pars"]]
  rm(out)

  # Prepare model for delayN() and smoothN() functions
  delayN_smoothN <- get_delayN_smoothN(sfm)

  sfm <- prep_delayN_smoothN(sfm, delayN_smoothN)


  # Order equations including delayN() and smoothN() functions, if any
  ordering <- order_equations(sfm)

  # If there are no dynamic variables or delayed variables, set only_stocks to TRUE
  delay_past <- get_delay_past(sfm)

  if (!only_stocks & is.null(ordering[["dynamic"]][["order"]])) {
    only_stocks <- TRUE
  }

  if (length(delay_past) > 0) {
    only_stocks <- FALSE
  }

  # Compile all parts of the R script
  times <- compile_times_julia(sfm, keep_unit)

  # Macros
  macros <- compile_macros_julia(sfm)

  # Prepare equations
  sfm <- prep_equations_variables_julia(sfm, keep_unit, keep_nonnegative_flow)

  # Stocks
  sfm <- prep_stock_change_julia(sfm, keep_unit)

  # Prepare intermediary variables
  intermediaries <- prep_intermediary_variables_julia(sfm, ordering = ordering)

  # Static equations
  static_eqn <- compile_static_eqn_julia(sfm,
    ensemble_pars = ensemble_pars,
    ordering = ordering,
    intermediaries = intermediaries,
    keep_unit = keep_unit
  )

  # Compile unit definitions
  units_def <- compile_units_julia(sfm, keep_unit)

  # Seed string
  seed_str <- ifelse(!is_defined(sfm[["sim_specs"]][["seed"]]), "",
    sprintf("# Ensure reproducibility across runs in case of random elements\nRandom.seed!(%s)\n", as.character(sfm[["sim_specs"]][["seed"]]))
  )

  prep_script <- sprintf(
    "# Script generated on %s by sdbuildR.\n\n%s%s%s%s",
    Sys.time(), seed_str,
    units_def[["script"]],
    times[["script"]],
    macros[["script"]]
  )

  # Compile ODE script
  ode <- compile_ode_julia(sfm,
    ensemble_pars = ensemble_pars,
    ordering = ordering,
    intermediaries = intermediaries,
    prep_script = prep_script, static_eqn = static_eqn,
    keep_nonnegative_stock = keep_nonnegative_stock,
    keep_unit = keep_unit,
    only_stocks = only_stocks
  )

  run_ode <- compile_run_ode_julia(sfm,
    ensemble_pars = ensemble_pars,
    static_eqn_script = static_eqn[["script"]],
    filepath_sim = filepath_sim,
    only_stocks = only_stocks,
    stock_names = static_eqn[["stock_names"]],
    keep_unit = keep_unit
  )

  script <- paste0(
    prep_script, "\n",
    ode[["script_ode"]],
    ode[["script_callback"]],
    static_eqn[["ensemble_def"]],
    static_eqn[["script"]],
    run_ode[["script"]]
  )


  return(script)
}



#' Check for keyword arguments in Julia translated equation
#'
#' Check whether any variable names are used as functions with keyword arguments in the Julia translated equation
#'
#' @inheritParams build
#' @param var_names Character vector; variable names in the model.
#'
#' @returns NULL
#' @noRd
check_no_keyword_arg <- function(sfm, var_names) {
  # Check for all variable names if they are used as functions. If so, throw error if they are used with keyword arguments in the Julia translated equation.

  eqns <- lapply(
    sfm[["model"]][["variables"]],
    function(x) {
      lapply(x, `[[`, "eqn_julia")
    }
  ) |>
    compact_() |>
    unname() |>
    unlist()

  # Find if any variables were used as functions
  idx <- stringr::str_detect(eqns, paste0(paste0(var_names, "\\("), collapse = "|"))

  if (any(idx)) {
    # Get the equations that use variables as functions
    eqns <- eqns[idx]

    named_idxs <- vapply(eqns, function(eqn) {
      # Find all round brackets
      paired_idxs <- get_range_all_pairs(eqn, var_names = var_names, type = "round")
      if (nrow(paired_idxs) == 0) {
        return(FALSE)
      }

      # Get start and end indices of variable names
      pair_names <- get_range_names(eqn, var_names = var_names, names_with_brackets = FALSE)
      if (nrow(pair_names) == 0) {
        return(FALSE)
      }

      # Match brackets to variable name
      pair_idxs <- match(pair_names[["end"]] + 1, paired_idxs[["start"]])
      if (all(is.na(pair_idxs))) {
        return(FALSE)
      }
      paired <- cbind(pair_names[pair_idxs, ], paired_idxs[pair_idxs, ])

      # Remove NA
      paired <- paired[!is.na(paired[["match"]]), ]

      named_idxs <- vapply(paired[["match"]], function(x) {
        y <- parse_args(gsub("\\)$", "", gsub("^\\(", "", x)))

        # Check for named arguments
        any(stringr::str_detect(y, "="))
      }, logical(1))
      any(unlist(unname(named_idxs)))
    }, logical(1))

    if (any(named_idxs)) {
      stop(
        "The following variables were used as functions with named arguments in the Julia translated equation: ",
        paste0(names(named_idxs)[unname(named_idxs)], collapse = ", "), ".\n",
        "This is not allowed in Julia. Please use arguments without naming them."
      )
    }
  }

  return(invisible())
}


#' Prepare stock-and-flow model for ensemble range
#'
#' @inheritParams build
#' @inheritParams compile_julia
#'
#' @returns List with updated stock-and-flow model and updated ensemble parameters
#' @noRd
prep_ensemble_range <- function(sfm, ensemble_pars) {
  if (!is.null(ensemble_pars[["range"]])) {
    # Prepare the ranges for Julia
    ensemble_pars[["range"]] <- lapply(
      ensemble_pars[["range"]],
      function(vec) {
        replace_digits_with_floats(
          paste0("[", paste0(vec, collapse = ", "), "]"),
          # No variable names to account for
          NULL
        )
      }
    )

    # Set the equations of the variables in the model to 0 because these will be replaced by the ensemble parameters
    names_df <- get_names(sfm)
    stocks <- names_df[match(names(ensemble_pars[["range"]]), names_df[["name"]]), "type"]

    for (i in seq_along(ensemble_pars[["range"]])) {
      name <- names(ensemble_pars[["range"]])[i]
      # Replace the equations of the chosen variables with ensemble_pars.name[i]
      sfm[["model"]][["variables"]][[stocks[i]]][[name]][["eqn_julia"]] <- "0.0"
    }
  }

  return(list(sfm = sfm, ensemble_pars = ensemble_pars))
}


#' Prepare model for delayN and smoothN
#'
#' @inheritParams build
#' @param delayN_smoothN List with delayN and smoothN functions
#'
#' @returns Updated stock-and-flow model
#' @noRd
prep_delayN_smoothN <- function(sfm, delayN_smoothN) {
  # If delayN() and smoothN() were used, add these to the model
  if (length(delayN_smoothN) > 0) {
    # Order alphabetically
    delayN_smoothN <- delayN_smoothN[sort(names(delayN_smoothN))]

    names_df <- get_names(sfm)
    allowed_delay_var <- names_df[names_df[["type"]] %in% c("stock", "flow", "aux", "gf"), "name"]
    delayN_smoothN <- unlist(unname(delayN_smoothN), recursive = FALSE)

    sfm[["model"]][["variables"]][["stock"]] <- append(
      sfm[["model"]][["variables"]][["stock"]],
      lapply(seq_along(delayN_smoothN), function(i) {
        x <- delayN_smoothN[[i]]
        y <- list()

        # In rare cases, the delayed variable is a graphical function, and in that case the unit of that variable cannot be found
        bare_var <- sub("\\(.*", "", x[["var"]])

        # Check whether the variable is in the model
        if (!bare_var %in% names_df[["name"]]) {
          stop(paste0("The variable '", bare_var, "' used in delayN() or smoothN() is not defined in the model."))
        }

        # # Check whether variable is either a stock, flow, or aux
        # if (!bare_var %in% allowed_delay_var){
        #   stop(paste0("The variable '", bare_var, "' used in delayN() or smoothN() is not a stock, flow, auxiliary, or graphical function variable."))
        # }
        # Check whether variable is either a stock, flow, aux, or gf
        if (!bare_var %in% allowed_delay_var) {
          stop(paste0(x[["name"]], " attempts to delay a constant ('", bare_var, "') in a delayN() or smoothN() function. Please only use dynamic variables (stock, flow, aux, or gf) in delayN() or smoothN()."))
        }

        # Unit is the same as the delayed variable
        y[["units"]] <- names_df[names_df[["name"]] == bare_var, ][["units"]]
        y[["name"]] <- y[["label"]] <- names(delayN_smoothN)[i]
        y[["type"]] <- "delayN_smoothN"

        # To get the dependencies right, we need the initial value, length and order in eqn
        y[["eqn"]] <- paste0(x[["initial"]], " / ", x[["length"]], " * ", x[["order"]])
        y[["eqn_julia"]] <- x[["setup"]]
        y[["inflow"]] <- x[["update"]]
        return(y)
      }) |> stats::setNames(names(delayN_smoothN))
    )

    sfm[["model"]][["variables"]][["aux"]] <- append(
      sfm[["model"]][["variables"]][["aux"]],
      lapply(seq_along(delayN_smoothN), function(i) {
        x <- delayN_smoothN[[i]]
        y <- list()

        # In rare cases, the delayed variable is a graphical function, and in that case the unit of that variable cannot be found
        bare_var <- sub("\\(.*", "", x[["var"]])

        # Unit is the same as the delayed variable
        y[["units"]] <- names_df[names_df[["name"]] == bare_var, ][["units"]]
        y[["name"]] <- y[["label"]] <- names(delayN_smoothN)[i]
        y[["type"]] <- "delayN_smoothN"

        # To get the dependencies right, we need the delayed variable, length and order in eqn
        y[["eqn"]] <- paste0(bare_var, " / ", x[["length"]], " * ", x[["order"]])
        y[["eqn_julia"]] <- x[["compute"]]
        return(y)
      }) |> stats::setNames(names(delayN_smoothN))
    )
  }

  return(sfm)
}





#' Compile script for defining a units module in Julia
#'
#' @inheritParams compile_julia
#'
#' @return List with script
#'
#' @noRd
compile_units_julia <- function(sfm, keep_unit) {

#   script <- sprintf("\n# Clear any existing definitions to avoid conflicts
# if @isdefined(%s)
#     # Force garbage collection to clean up old module
#     %s = nothing
#     GC.gc()
# end\n", .sdbuildR_env[["P"]][["MyCustomUnits"]], .sdbuildR_env[["P"]][["MyCustomUnits"]])
  script = ""

  if (length(sfm[["model_units"]]) > 0) {

    if (length(sfm[["model_units"]]) > 1) {

      # Topological sort of units
      eq_names <- names(sfm[["model_units"]])
      dependencies <- lapply(
        lapply(sfm[["model_units"]], `[[`, "eqn"),
        function(x) {
          unlist(stringr::str_extract_all(x, eq_names))
        }
      )
      out <- topological_sort(dependencies)

      if (out[["issue"]]) {
        message(paste0("Ordering custom units failed. ", paste0(out[["msg"]])))
      }

      sfm[["model_units"]] <- sfm[["model_units"]][out[["order"]]]
    }


    unit_str <- lapply(sfm[["model_units"]], function(x) {
      if (is_defined(x[["eqn"]])) {
        unit_def <- x[["eqn"]]
      } else {
        unit_def <- "1"
      }

      paste0(
        "@unit ", x[["name"]], " \"", x[["name"]], "\" ",
        x[["name"]], " u\"", unit_def, "\" ", ifelse(x[["prefix"]], "true", "false")
      )
    }) |> paste0(collapse = sprintf("\n\tUnitful.register(%s)\n\t", .sdbuildR_env[["P"]][["MyCustomUnits"]]))

    script <- paste0(script,
      "\n# Define custom units; register after each unit as some units may be defined by other units\nmodule ", .sdbuildR_env[["P"]][["MyCustomUnits"]], "\n\tusing Unitful\n\tusing ", .sdbuildR_env[["P"]][["jl_pkg_name"]], ".", .sdbuildR_env[["P"]][["sdbuildR_units"]], "\n\t",
      unit_str,
      "\n\tUnitful.register(", .sdbuildR_env[["P"]][["MyCustomUnits"]], ")\nend\n\n",
      "Unitful.register(", .sdbuildR_env[["P"]][["MyCustomUnits"]], ")\n"
    )
  }

  return(list(script = script))
}



#' Compile Julia script for global variables
#'
#' @inheritParams compile_R
#'
#' @return List with macro script
#' @noRd
compile_macros_julia <- function(sfm) {
  script <- ""

  # If there are macros
  if (any(nzchar(unlist(lapply(sfm[["macro"]], `[[`, "eqn_julia"))))) {
    # names_df = get_names(sfm)

    script <- paste0(
      script, "\n",
      lapply(sfm[["macro"]], `[[`, "eqn_julia") |> unlist() |>
        paste0(collapse = "\n")
    )
  }

  if (nzchar(script)) {
    script <- paste0("\n\n# User-specified macros\n", script, "\n\n")
  }

  return(list(script = script))
}






#' Compile Julia script for creating time vector
#'
#' @return List
#' @importFrom rlang .data
#'
#' @inheritParams compile_julia
#'
#' @noRd
compile_times_julia <- function(sfm, keep_unit) {
  script <- sprintf(
    "\n\n# Simulation time unit (smallest time scale in your model)
%s = u\"%s\"\n# Define time sequence\n%s = (%s, %s)%s\n# Initialize time (only necessary if constants use t)\n%s = %s[1]\n# Time step\n%s = %s%s\n# Save at value\n%s = %s%s\n# Define saving time sequence\n%s = %s%s; %s = %s:%s:%s[2]\n",
    .sdbuildR_env[["P"]][["time_units_name"]], sfm[["sim_specs"]][["time_units"]],
    .sdbuildR_env[["P"]][["times_name"]], sfm[["sim_specs"]][["start"]], sfm[["sim_specs"]][["stop"]],
    ifelse(keep_unit, paste0(" .* ", .sdbuildR_env[["P"]][["time_units_name"]]), ""),
    .sdbuildR_env[["P"]][["time_name"]], .sdbuildR_env[["P"]][["times_name"]],
    .sdbuildR_env[["P"]][["timestep_name"]], sfm[["sim_specs"]][["dt"]],
    ifelse(keep_unit, paste0(" * ", .sdbuildR_env[["P"]][["time_units_name"]]), ""),
    .sdbuildR_env[["P"]][["saveat_name"]], sfm[["sim_specs"]][["save_at"]],
    ifelse(keep_unit, paste0(" * ", .sdbuildR_env[["P"]][["time_units_name"]]), ""),
    .sdbuildR_env[["P"]][["savefrom_name"]], sfm[["sim_specs"]][["save_from"]],
    ifelse(keep_unit, paste0(" .* ", .sdbuildR_env[["P"]][["time_units_name"]]), ""),
    .sdbuildR_env[["P"]][["savefrom_name"]], .sdbuildR_env[["P"]][["savefrom_name"]],
    .sdbuildR_env[["P"]][["saveat_name"]], .sdbuildR_env[["P"]][["times_name"]]
  )

  return(list(script = script))
}




#' Prepare equations of all model variables
#'
#' @inheritParams build
#' @inheritParams compile
#'
#' @returns Updated stock-and-flow model with equations as strings
#' @noRd
prep_equations_variables_julia <- function(sfm, keep_unit, keep_nonnegative_flow) {
  names_df <- get_names(sfm)

  # Graphical functions
  sfm[["model"]][["variables"]][["gf"]] <- lapply(
    sfm[["model"]][["variables"]][["gf"]],
    function(x) {
      if (is_defined(x[["xpts"]]) & is_defined(x[["ypts"]])) {
        # Check whether xpts is defined as numeric or string
        if (inherits(x[["xpts"]], "numeric")) {
          xpts_str <- paste0("[", paste0(as.character(x[["xpts"]]), collapse = ", "), "]")
        } else {
          xpts_str <- stringr::str_replace_all(
            x[["xpts"]],
            "^c\\(", "["
          ) |>
            stringr::str_replace_all("\\)$", "]")
        }

        # Add units of source if defined
        if (keep_unit) {
          if (is_defined(x[["source"]])) {
            if (x[["source"]] == "t") {
              xpts_str <- paste0(xpts_str, " .* ", .sdbuildR_env[["P"]][["time_units_name"]])
            } else {
              unit_source <- names_df[names_df[["name"]] == x[["source"]], "units"]
              if (unit_source != "1") {
                xpts_str <- paste0(xpts_str, " .* u\"", unit_source, "\"")
              }
            }
          }
        }

        # Check whether ypts is defined as numeric or string
        if (inherits(x[["ypts"]], "numeric")) {
          ypts_str <- paste0("[", paste0(as.character(x[["ypts"]]), collapse = ", "), "]")
        } else {
          ypts_str <- stringr::str_replace_all(x[["ypts"]], "^c\\(", "[") |>
            stringr::str_replace_all("\\)$", "]")
        }

        if (keep_unit & is_defined(x[["units"]]) & x[["units"]] != "1") {
          ypts_str <- paste0(ypts_str, " .* u\"", x[["units"]], "\"")
        }

        x[["eqn_str"]] <- sprintf(
          "%s = itp(%s,\n\t%s, method = \"%s\", extrapolation = \"%s\")",
          x[["name"]], xpts_str, ypts_str,
          x[["interpolation"]], x[["extrapolation"]]
        )
      }
      return(x)
    }
  )

  # Constant equations
  sfm[["model"]][["variables"]][["constant"]] <- lapply(
    sfm[["model"]][["variables"]][["constant"]],
    function(x) {
      if (keep_unit & is_defined(x[["units"]]) & x[["units"]] != "1") {
        x[["eqn_str"]] <- paste0(
          x[["name"]], " = ",
          .sdbuildR_env[["P"]][["convert_u_func"]], "(",
          x[["eqn_julia"]], ", u\"", x[["units"]], "\")"
        )
      } else {
        x[["eqn_str"]] <- paste0(x[["name"]], " = ", x[["eqn_julia"]])
      }
      return(x)
    }
  )

  # Initial states of stocks
  sfm[["model"]][["variables"]][["stock"]] <- lapply(
    sfm[["model"]][["variables"]][["stock"]],
    function(x) {
      if (keep_unit & is_defined(x[["units"]]) & x[["units"]] != "1") {
        x[["eqn_str"]] <- paste0(
          x[["name"]], " = ",
          .sdbuildR_env[["P"]][["convert_u_func"]], "(",
          x[["eqn_julia"]], ", u\"",
          x[["units"]], "\")"
        )
      } else {
        x[["eqn_str"]] <- paste0(x[["name"]], " = ", x[["eqn_julia"]])
      }
      return(x)
    }
  )


  # Auxiliary equations (dynamic auxiliaries)
  sfm[["model"]][["variables"]][["aux"]] <- lapply(
    sfm[["model"]][["variables"]][["aux"]],
    function(x) {
      if (keep_unit & is_defined(x[["units"]]) & x[["units"]] != "1") {
        x[["eqn_str"]] <- paste0(
          x[["name"]], " = ", .sdbuildR_env[["P"]][["convert_u_func"]],
          "(", x[["eqn_julia"]], ", u\"",
          x[["units"]], "\")"
        )
      } else {
        x[["eqn_str"]] <- paste0(x[["name"]], " = ", x[["eqn_julia"]])
      }

      if (!is.null(x[["preceding_eqn"]])) {
        x[["eqn_str"]] <- c(x[["preceding_eqn"]], x[["eqn_str"]])
      }
      return(x)
    }
  )

  # Flow equations
  flow_df <- get_flow_df(sfm)
  sfm[["model"]][["variables"]][["flow"]] <- lapply(
    sfm[["model"]][["variables"]][["flow"]],
    function(x) {
      x[["eqn_str"]] <- sprintf(
        "\n\t# Flow%s%s\n\t%s = %s%s%s%s%s",
        # Add comment
        ifelse(is_defined(x[["from"]]), paste0(" from ", x[["from"]]), ""),
        ifelse(is_defined(x[["to"]]), paste0(" to ", x[["to"]]), ""),
        x[["name"]],
        ifelse(keep_unit & x[["units"]] != "1", paste0(.sdbuildR_env[["P"]][["convert_u_func"]], "("), ""),
        ifelse(x[["non_negative"]] & keep_nonnegative_flow, "nonnegative(", ""),
        x[["eqn_julia"]],
        ifelse(x[["non_negative"]] & keep_nonnegative_flow, ")", ""),
        ifelse(keep_unit & x[["units"]] != "1", paste0(", u\"", x[["units"]], "\")"), "")
      )

      if (!is.null(x[["preceding_eqn"]])) {
        x[["eqn_str"]] <- c(x[["preceding_eqn"]], x[["eqn_str"]])
      }
      return(x)
    }
  )


  return(sfm)
}


#' Prepare intermediary variables
#'
#' @inheritParams build
#' @inheritParams compile_static_eqn_julia
#'
#' @returns List of intermediary variables and values
#' @noRd
prep_intermediary_variables_julia <- function(sfm, ordering) {
  # Create separate vector for names of intermediate variables and values, because graphical functions need to be in the intermediate funciton as gf(t), but their name should be gf
  intermediary_var <- intermediary_var_values <- ordering[["dynamic"]][["order"]]

  # Graphical functions (gf)
  gf_str <- ""
  if (length(sfm[["model"]][["variables"]][["gf"]]) > 0) {
    # Some gf have other gf as source; recursively replace
    gf_sources <- lapply(sfm[["model"]][["variables"]][["gf"]], `[[`, "source") |>
      compact_() |>
      unlist()

    if (length(gf_sources) > 0) {
      # Graphical functions with source
      gf <- paste0(names(gf_sources), "(", unname(gf_sources), ")") |> stats::setNames(names(gf_sources))

      # Create dictionary to add source to nested graphical functions
      dict2 <- paste0("(", names(gf_sources), "(", unname(gf_sources), "))") |>
        stats::setNames(paste0("\\(", stringr::str_escape(names(gf_sources)), "\\)"))

      gf_str <- stringr::str_replace_all(unname(gf), dict2)

      # Add names of graphical functions to intermediary_var
      intermediary_var <- c(intermediary_var, names(gf))
      intermediary_var_values <- c(intermediary_var_values, gf_str)
    }
  }

  # Add fixed delayed and past variables to intermediary_var
  delay_past <- get_delay_past(sfm)
  extra_intermediary_var <- list_extract(delay_past, "var")


  if (length(extra_intermediary_var) > 0) {
    # Check whether the intermediary variables are in the model
    names_df <- get_names(sfm)
    allowed_intermediary_var <- names_df[names_df[["type"]] %in% c("stock", "flow", "aux"), "name"]

    idx <- !(extra_intermediary_var %in% names_df[["name"]])
    if (any(idx)) {
      stop(paste0("The following variables used in delay() or past() are not defined in the model: ", paste0(extra_intermediary_var[idx], collapse = ", ")))
    }

    idx <- !(extra_intermediary_var %in% allowed_intermediary_var)
    if (any(idx)) {
      stop(paste0("The following variables used in delay() or past() are not stocks, flows, or auxiliaries: ", paste0(extra_intermediary_var[idx], collapse = ", ")))
    }

    # Get unique intermediary variables to add
    new_intermediary_var <- setdiff(unique(unlist(extra_intermediary_var)), intermediary_var)

    if (length(new_intermediary_var) > 0) {
      intermediary_var <- c(intermediary_var, new_intermediary_var)
      intermediary_var_values <- c(intermediary_var_values, new_intermediary_var)
    }
  }

  # If delayN() and smoothN() were used, state has to be unpacked differently
  delayN_smoothN <- get_delayN_smoothN(sfm)

  if (length(delayN_smoothN) > 0) {
    delay_names <- names(unlist(unname(delayN_smoothN), recursive = FALSE))

    intermediary_var <- setdiff(intermediary_var, delay_names)
    intermediary_var_values <- setdiff(intermediary_var_values, delay_names)
  }

  # Order intermediary variables and values alphabetically
  if (length(extra_intermediary_var) > 0) {
    idx <- order(intermediary_var)
    intermediary_var <- intermediary_var[idx]
    intermediary_var_values <- intermediary_var_values[idx]
  }

  return(list(
    names = intermediary_var,
    values = intermediary_var_values
  ))
}


#' Compile Julia script for static variables, i.e. initial conditions, functions, and parameters
#'
#' @inheritParams compile_julia
#' @inheritParams order_equations
#' @param ordering List with order of static and dynamic variables, output of order_equations()
#' @param intermediaries List with intermediary variables and values
#'
#' @return List with necessary scripts
#'
#' @noRd
compile_static_eqn_julia <- function(sfm, ensemble_pars, ordering, intermediaries, keep_unit) {
  names_df <- get_names(sfm)

  # Graphical functions
  gf_eqn <- lapply(sfm[["model"]][["variables"]][["gf"]], `[[`, "eqn_str")

  # Constant equations
  constant_eqn <- lapply(sfm[["model"]][["variables"]][["constant"]], `[[`, "eqn_str")

  # Initial states of stocks
  stock_eqn <- lapply(sfm[["model"]][["variables"]][["stock"]], `[[`, "eqn_str")


  # Prepare ensemble range if specified
  if (length(ensemble_pars[["range"]]) > 0) {
    ensemble_def <- paste0(
      "\n\n# Generate ensemble design\n",
      .sdbuildR_env[["P"]][["ensemble_n"]], " = ", ensemble_pars[["n"]], "\n",
      .sdbuildR_env[["P"]][["ensemble_range"]], " = (\n",
      paste0(paste0(
        names(ensemble_pars[["range"]]), " = ",
        unname(ensemble_pars[["range"]])
      ), collapse = ",\n"),
      ",\n)\n",
      .sdbuildR_env[["P"]][["ensemble_pars"]], ", ", .sdbuildR_env[["P"]][["ensemble_total_n"]], " = generate_param_combinations(\n",
      .sdbuildR_env[["P"]][["ensemble_range"]], "; crossed=", ifelse(ensemble_pars[["cross"]], "true", "false"), ", n_replicates = ",
      .sdbuildR_env[["P"]][["ensemble_n"]], ")\n",
      # Initialize ensemble range iterator if specified
      .sdbuildR_env[["P"]][["ensemble_iter"]], " = 1\n"
    )

    ensemble_iter <- paste0(
      "\n\t# Assign ensemble parameters\n\t",
      paste0(names(ensemble_pars[["range"]]), collapse = ", "), ", = ",
      .sdbuildR_env[["P"]][["ensemble_pars"]], "[div(", .sdbuildR_env[["P"]][["ensemble_iter"]], "-1, ", .sdbuildR_env[["P"]][["ensemble_n"]], ") + 1]\n\n"
    )

    # Remove ensemble variables from equations
    ordering[["static"]][["order"]] <- ordering[["static"]][["order"]][!ordering[["static"]][["order"]] %in% names(ensemble_pars[["range"]])]
    ordering[["static_and_dynamic"]][["order"]] <- ordering[["static_and_dynamic"]][["order"]][!ordering[["static_and_dynamic"]][["order"]] %in% names(ensemble_pars[["range"]])]
  } else if (!is.null(ensemble_pars)) {
    ensemble_def <- paste0(
      .sdbuildR_env[["P"]][["ensemble_n"]], " = ", ensemble_pars[["n"]], "\n",
      .sdbuildR_env[["P"]][["ensemble_total_n"]], " = ", ensemble_pars[["n"]], "\n",
      # Initialize ensemble range iterator if specified
      .sdbuildR_env[["P"]][["ensemble_iter"]], " = 1\n"
    )
    ensemble_iter <- ""
  } else {
    ensemble_def <- ensemble_iter <- ""
  }




  # If there was an issue with the ordering of static and dynamic equations, only compile static equations
  if (ordering[["static_and_dynamic"]][["issue"]]) {
    # Compile and order static equations
    static_eqn_str <- unlist(c(gf_eqn, constant_eqn, stock_eqn)[ordering[["static"]][["order"]]]) |>
      paste0(collapse = "\n")
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
  }

  # Replace any reference to model_setup.intermediary_names with intermediary_names
  static_eqn_str <- stringr::str_replace_all(
    static_eqn_str,
    paste0(
      .sdbuildR_env[["P"]][["model_setup_name"]], "\\.",
      .sdbuildR_env[["P"]][["intermediary_names"]]
    ),
    .sdbuildR_env[["P"]][["intermediary_names"]]
  )


  # Put parameters together in named tuple; include graphical functions as otherwise these are not defined outside of the let block
  if (length(sfm[["model"]][["variables"]][["constant"]]) > 0 | length(sfm[["model"]][["variables"]][["gf"]]) > 0) {
    pars_def <- paste0(
      "\n\n# Define parameters in named tuple\n",
      .sdbuildR_env[["P"]][["parameter_name"]], " = (",
      paste0(c(names(constant_eqn), names(gf_eqn)), " = ",
        c(names(constant_eqn), names(gf_eqn)),
        collapse = ", "
      ), ",)\n"
    )
  } else {
    pars_def <- paste0("\n\n# Define empty parameters\n", .sdbuildR_env[["P"]][["parameter_name"]], " = ()\n")
  }

  # Check for delayN() and smoothN() functions
  delayN_smoothN <- get_delayN_smoothN(sfm)

  if (length(delayN_smoothN) > 0) {
    delay_names <- names(unlist(unname(delayN_smoothN), recursive = FALSE))

    # Preserve order of stocks but wrap delayN and smoothN stocks in values() and keys()
    x <- y <- names(stock_eqn)
    idx <- names(stock_eqn) %in% delay_names
    x[idx] <- paste0("values(", x[idx], ")")
    y[!idx] <- paste0(":", y[!idx])
    y[idx] <- paste0("keys(", y[idx], ")...")

    init_def_stocks <- paste0(x, collapse = ", ")
    init_names <- paste0(y, collapse = ", ")

    # Find indices of names in vector
    init_idx <- paste0(
      "\n", .sdbuildR_env[["P"]][["delay_idx_name"]], " = (",
      paste0(paste0(
        delay_names, " = ",
        "findall(n -> occursin(r\"", delay_names, .sdbuildR_env[["P"]][["acc_suffix"]], "[0-9]+$\", string(n)), ",
        .sdbuildR_env[["P"]][["initial_value_names"]], ")"
      ), collapse = ",\n\t"),
      ",)\n"
    )


    # Make sure that any .outflow references are replaced with first(values(variable))
    dict <- stringr::fixed(stats::setNames(
      paste0("first(values(", delay_names, "))"),
      paste0(delay_names, .sdbuildR_env[["P"]][["outflow_suffix"]])
    ))
    static_eqn_str <- stringr::str_replace_all(static_eqn_str, dict)
  } else {
    init_def_stocks <- paste0(names(stock_eqn), collapse = ", ")
    # Symbols are faster than characters
    init_names <- paste0(paste0(":", names(stock_eqn)), collapse = ", ")
    init_idx <- ""
  }

  # Put initial states together in (unnamed) vector
  init_def <- paste0(
    "\n# Define initial condition in vector\n",
    .sdbuildR_env[["P"]][["initial_value_name"]],
    " = [Base.Iterators.flatten([",
    init_def_stocks,
    # Add extra comma in case there is only one Stock
    ",])...]\n"
  )

  init_names <- paste0(
    .sdbuildR_env[["P"]][["initial_value_names"]], " = [",
    init_names,
    "]\n"
  )


  if (length(intermediaries[["names"]]) > 0) {
    intermediary_names <- paste0(.sdbuildR_env[["P"]][["intermediary_names"]], " = [", paste0(
      paste0(
        ":",
        intermediaries[["names"]]
      ),
      collapse = ", "
    ), "]\n")

    # Remove names of intermediaries that are functions; these won't be saved
    intermediary_names_correct <- paste0(
      "\n# Remove names of intermediaries that are functions\n",
      "is_not_function = collect(.!is_function_or_interp.((",
      paste0(intermediaries[["values"]], collapse = ", "),
      ifelse(length(intermediaries[["values"]]) == 1, ",", ""),
      ")))\n",
      .sdbuildR_env[["P"]][["intermediary_names"]], " = ", .sdbuildR_env[["P"]][["intermediary_names"]], "[is_not_function]\n"
    )
  } else {
    intermediary_names <- paste0(.sdbuildR_env[["P"]][["intermediary_names"]], " = Nothing\n")
    intermediary_names_correct <- ""
  }




  return(list(
    stock_names = names(stock_eqn),
    par_names = c(names(constant_eqn), names(gf_eqn)),
    ensemble_def = ensemble_def,
    script = paste0(
      "\n\n# Define parameters, initial conditions, and functions in correct order\n",
      .sdbuildR_env[["P"]][["model_setup_name"]],
      " = let\n",
      # Assign ensemble parameters for this iteration
      ensemble_iter,
      intermediary_names,
      static_eqn_str,
      pars_def,
      init_def,
      init_names,
      init_idx,
      intermediary_names_correct,
      "\n\t(", .sdbuildR_env[["P"]][["parameter_name"]], " = ", .sdbuildR_env[["P"]][["parameter_name"]], ", ",
      .sdbuildR_env[["P"]][["initial_value_name"]], " = ", .sdbuildR_env[["P"]][["initial_value_name"]], ", ",
      .sdbuildR_env[["P"]][["initial_value_names"]], " = ", .sdbuildR_env[["P"]][["initial_value_names"]], ", ",
      .sdbuildR_env[["P"]][["intermediary_names"]], " = ", .sdbuildR_env[["P"]][["intermediary_names"]],
      ifelse(nzchar(init_idx), paste0(", ", .sdbuildR_env[["P"]][["delay_idx_name"]], " = ", .sdbuildR_env[["P"]][["delay_idx_name"]]), ""),
      ")\n",
      "end\n"
    )
  ))
}



#' Prepare for summing change in stocks in stock-and-flow model in Julia script
#'
#' @inheritParams compile_julia
#'
#' @return Updated stock-and-flow model
#' @noRd
#'
prep_stock_change_julia <- function(sfm, keep_unit) {

 # Add temporary property to sum change in Stocks
  stock_names <- names(sfm[["model"]][["variables"]][["stock"]])
  sfm[["model"]][["variables"]][["stock"]] <- lapply(
    sfm[["model"]][["variables"]][["stock"]],
    function(x) {
      inflow <- outflow <- ""

      if (x[["type"]] == "delayN_smoothN") {
        x[["sum_name"]] <- paste0(.sdbuildR_env[["P"]][["change_state_name"]], "[", .sdbuildR_env[["P"]][["model_setup_name"]], ".", .sdbuildR_env[["P"]][["delay_idx_name"]], ".", x[["name"]], "]")
        x[["unpack_state"]] <- paste0(.sdbuildR_env[["P"]][["state_name"]], "[", .sdbuildR_env[["P"]][["model_setup_name"]], ".", .sdbuildR_env[["P"]][["delay_idx_name"]], ".", x[["name"]], "]")
      } else {
        x[["sum_name"]] <- paste0(.sdbuildR_env[["P"]][["change_state_name"]], "[", match(x[["name"]], stock_names), "]")
      }


      # In case no inflow and no outflow is defined, update with 0
      if (!is_defined(x[["inflow"]]) & !is_defined(x[["outflow"]])) {
        # # If keep_unit = TRUE, flows always need to have units as the times variable has units
        # if (keep_unit) {
        #   # Safer: in case x evaluates to a unit but no units were set
        #   x[["sum_eqn"]] <- paste0(.sdbuildR_env[["P"]][["convert_u_func"]], "(0.0, Unitful.unit.(", x[["name"]], ")/", .sdbuildR_env[["P"]][["time_units_name"]], ")")
        # } else {
          x[["sum_eqn"]] <- "0.0"
        # }
      } else {
        if (is_defined(x[["inflow"]])) {
          inflow <- paste0(x[["inflow"]], collapse = " + ")
        }
        if (is_defined(x[["outflow"]])) {
          outflow <- paste0(paste0(" - ", x[["outflow"]]), collapse = "")
        }
        x[["sum_eqn"]] <- sprintf("%s%s", inflow, outflow)
      }

      # Add units if defined
      if (keep_unit & is_defined(x[["units"]])) {
        if (x[["type"]] == "delayN_smoothN") {
          x[["sum_eqn"]] <- paste0(
            x[["sum_eqn"]], " ./ ",
            .sdbuildR_env[["P"]][["time_units_name"]]
          )
        } else {
          x[["sum_eqn"]] <- paste0(
            .sdbuildR_env[["P"]][["convert_u_func"]],
            "(", x[["sum_eqn"]],
            ", Unitful.unit.(",
            x[["name"]], ")/",
            .sdbuildR_env[["P"]][["time_units_name"]], ")"
          )
        }
      }

      return(x)
    }
  )
  sfm[["model"]][["variables"]][["stock"]] <- sfm[["model"]][["variables"]][["stock"]][lengths(sfm[["model"]][["variables"]][["stock"]]) > 0]

  return(sfm)
}





#' Compile Julia script for ODE function
#'
#' @inheritParams build
#' @inheritParams compile
#' @inheritParams compile_julia
#' @inheritParams order_equations
#' @inheritParams compile_static_eqn
#' @inheritParams compile_static_eqn_julia
#' @param prep_script Intermediate output of compile_julia()
#' @param static_eqn Output of compile_static_eqn()
#'
#' @return List
#' @importFrom rlang .data
#' @noRd
#'
compile_ode_julia <- function(sfm, ensemble_pars, ordering, intermediaries,
                              prep_script, static_eqn,
                              keep_nonnegative_stock, keep_unit,
                              only_stocks) {
  # Auxiliary equations (dynamic auxiliaries)
  aux_eqn <- lapply(sfm[["model"]][["variables"]][["aux"]], `[[`, "eqn_str")

  # Flow equations
  flow_eqn <- lapply(sfm[["model"]][["variables"]][["flow"]], `[[`, "eqn_str")

  # Compile and order all dynamic equations
  dynamic_eqn <- unlist(c(aux_eqn, flow_eqn)[ordering[["dynamic"]][["order"]]])

  # Compile and order all dynamic equations
  dynamic_eqn_str <- paste0(dynamic_eqn, collapse = "\n\t")

  # Sum change in stock equations
  stock_change <- lapply(
    sfm[["model"]][["variables"]][["stock"]],
    function(x) {
      sprintf(
        "%s %s %s", x[["sum_name"]],
        # Broadcast assignment for delayed variables
        ifelse(x[["type"]] == "delayN_smoothN", ".=", "="),
        x[["sum_eqn"]]
      )
    }
  ) |> compact_()

  stock_change_str <- paste0(stock_change, collapse = "\n\t")

  # Non-negative Stocks
  nonneg_stocks <- lapply(sfm[["model"]][["variables"]][["stock"]], `[[`, "non_negative") |> unlist()
  add_nonneg <- any(nonneg_stocks) & keep_nonnegative_stock

  if (add_nonneg) {
    # Create if-statement to keep selected Stocks non-negative
    nonneg_str <- lapply(
      sfm[["model"]][["variables"]][["stock"]],
      function(x) {
        if (x[["non_negative"]]) {
          sprintf(
            "if (%s * %s + %s < 0) %s = %s/%s end",
            x[["sum_name"]], .sdbuildR_env[["P"]][["timestep_name"]], x[["name"]], x[["sum_name"]], x[["name"]], .sdbuildR_env[["P"]][["timestep_name"]]
          )
        } else {
          return(NULL)
        }
      }
    ) |> compact_()

    # Format complete string with explanation
    stock_change_str <- paste0(
      stock_change_str,
      "\n\n\t# Prevent ", paste0(names(nonneg_str), collapse = ", "), " from turning negative\n\t",
      paste0(nonneg_str, collapse = "\n\t")
    )
  }

  # Names of changing Stocks, e.g. dR for stock R
  stock_changes_names <- unname(unlist(lapply(sfm[["model"]][["variables"]][["stock"]], `[[`, "sum_name")))

  # If delayN() and smoothN() were used, state has to be unpacked differently
  delayN_smoothN <- get_delayN_smoothN(sfm)

  if (length(delayN_smoothN) > 0) {
    delay_names <- names(unlist(unname(delayN_smoothN), recursive = FALSE))

    # Unpack non delayN stocks
    unpack_nondelayN <- paste0(
      paste0(setdiff(names(stock_change), delay_names), collapse = ", "), ", = ", .sdbuildR_env[["P"]][["state_name"]], "[findall(n -> !occursin(r\"", .sdbuildR_env[["P"]][["delayN_suffix"]], "[0-9]+", .sdbuildR_env[["P"]][["acc_suffix"]], "[0-9]+$|",
      .sdbuildR_env[["P"]][["smoothN_suffix"]], "[0-9]+", .sdbuildR_env[["P"]][["acc_suffix"]], "[0-9]+$\", string(n)), ", .sdbuildR_env[["P"]][["model_setup_name"]], ".", .sdbuildR_env[["P"]][["initial_value_names"]], ")]"
    )

    # Unpack each delayN or smoothN stock separately
    unpack_delayN <- lapply(
      sfm[["model"]][["variables"]][["stock"]],
      function(x) {
        if (is_defined(x[["unpack_state"]])) {
          return(paste0(x[["name"]], " = ", x[["unpack_state"]]))
        }
      }
    ) |> compact_()

    unpack_state_str <- paste0(unpack_nondelayN, "\n\t", paste0(unpack_delayN, collapse = "\n\t"))
  } else {
    unpack_state_str <- paste0(paste0(names(stock_change), collapse = ", "), ", = ", .sdbuildR_env[["P"]][["state_name"]])
  }

  # Compile
  script_ode <- paste0(
    sprintf(
      "\n\n# Define ODE
function %s!(%s, %s%s, %s)",
      .sdbuildR_env[["P"]][["ode_func_name"]],
      .sdbuildR_env[["P"]][["change_state_name"]], .sdbuildR_env[["P"]][["state_name"]],
      paste0(", ", .sdbuildR_env[["P"]][["parameter_name"]]), .sdbuildR_env[["P"]][["time_name"]]
    ),
    "\n\n\t# Round t to deal with inaccuracies in floating point arithmetic\n\t",
    .sdbuildR_env[["P"]][["time_name"]], " = round_(", .sdbuildR_env[["P"]][["time_name"]], ", digits = 12)",
    "\n\n\t# Unpack state variables\n\t",
    unpack_state_str,
    ifelse(length(sfm[["model"]][["variables"]][["constant"]]) > 0,
      paste0(
        "\n\n\t# Unpack parameters\n\t",
        paste0(
          paste0(static_eqn[["par_names"]], collapse = ", "),
          ", = ", .sdbuildR_env[["P"]][["parameter_name"]]
        )
      ), ""
    ),
    "\n\n\t# Update auxiliaries\n\t",
    dynamic_eqn_str,
    "\n\n\t# Collect inflows and outflows for each stock\n\t",
    stock_change_str,
    "\n\tnothing\nend\n"
  )

  # Compile callback function
  if (only_stocks) {
    script_callback <- paste0(
      "\n\n# Define empty callback function\n",
      .sdbuildR_env[["P"]][["intermediaries"]], " = nothing\n",
      .sdbuildR_env[["P"]][["callback_name"]], " = nothing\n\n"
    )
  } else {
    script_callback <- paste0(
      sprintf(
        "\n\n# Define callback function
function %s(%s, %s, integrator)",
        .sdbuildR_env[["P"]][["callback_func_name"]],
        .sdbuildR_env[["P"]][["state_name"]], .sdbuildR_env[["P"]][["time_name"]]
      ),
      "\n\n\t# Round t to deal with inaccuracies in floating point arithmetic\n\t",
      .sdbuildR_env[["P"]][["time_name"]], " = round_(", .sdbuildR_env[["P"]][["time_name"]], ", digits = 12)",
      "\n\n\t# Unpack state variables\n\t",
      unpack_state_str,
      ifelse(length(sfm[["model"]][["variables"]][["constant"]]) > 0,
        paste0(
          "\n\n\t# Get parameters from integrator\n\t",
          paste0(.sdbuildR_env[["P"]][["parameter_name"]], " = integrator.p"),
          # Check whether you're overwriting it
          "\n\n\t# Unpack parameters\n\t",
          paste0(
            paste0(static_eqn[["par_names"]], collapse = ", "),
            ", = ", .sdbuildR_env[["P"]][["parameter_name"]]
          )
        ), ""
      ),
      "\n\n\t# Update auxiliaries\n\t",
      dynamic_eqn_str,
      "\n\n\t# Return intermediary values and remove funcions\n\t",
      "return filter(x -> !is_function_or_interp(x), (",
      paste0(intermediaries[["values"]], collapse = ", "),
      ifelse(length(intermediaries[["values"]]) == 1, ",", ""),
      "))\n",
      "\n\nend\n\n# Callback setup\n",

      # Only define intermediaries if ensemble_pars is NULL
      ifelse(!is.null(ensemble_pars), "", paste0(
        .sdbuildR_env[["P"]][["intermediaries"]], " = SavedValues(",
        # Make time a Unitful.Quantity if keeping units, otherwise a float
        "eltype(", .sdbuildR_env[["P"]][["time_name"]], "), Any)\n",
        .sdbuildR_env[["P"]][["callback_name"]], " = SavingCallback(",
        .sdbuildR_env[["P"]][["callback_func_name"]], ", ", .sdbuildR_env[["P"]][["intermediaries"]],
        ", saveat = ", .sdbuildR_env[["P"]][["savefrom_name"]],
        ")\n"
      ))
    )
  }

  return(list(
    script_ode = script_ode,
    script_callback = script_callback
  ))
}





#' Compile Julia script for running ODE
#'
#' @param filepath_sim Path to output file
#' @param nonneg_stocks Output of compile_nonneg_stocks()
#' @param stock_names Names of stocks
#' @param static_eqn_script Output of compile_static_eqn_julia(); only used in this function for ensemble trajectories.
#' @inheritParams compile_ode_julia
#' @inheritParams compile_julia
#' @inheritParams compile_R
#' @inheritParams order_equations
#'
#' @return List
#' @noRd
#'
compile_run_ode_julia <- function(sfm,
                                  ensemble_pars,
                                  static_eqn_script,
                                  filepath_sim,
                                  nonneg_stocks,
                                  stock_names,
                                  only_stocks, keep_unit) {
  if (is.null(ensemble_pars)) {
    script <- paste0(
      "\n\n# Run ODE\n",
      .sdbuildR_env[["P"]][["prob_name"]], " = ODEProblem(", .sdbuildR_env[["P"]][["ode_func_name"]], "!, ",
      .sdbuildR_env[["P"]][["model_setup_name"]], ".", .sdbuildR_env[["P"]][["initial_value_name"]],
      ", ", .sdbuildR_env[["P"]][["times_name"]], ", ",
      .sdbuildR_env[["P"]][["model_setup_name"]], ".", .sdbuildR_env[["P"]][["parameter_name"]],
      ")\n", .sdbuildR_env[["P"]][["solution_name"]], " = solve(", .sdbuildR_env[["P"]][["prob_name"]], ", ",
      sfm[["sim_specs"]][["method"]],
      paste0(
        ", dt = ", .sdbuildR_env[["P"]][["timestep_name"]],
        ", saveat = ", .sdbuildR_env[["P"]][["savefrom_name"]],
        ", adaptive = false"
      ),
      ifelse(!only_stocks,
        paste0(", ", .sdbuildR_env[["P"]][["callback_name"]], " = ",
               .sdbuildR_env[["P"]][["callback_name"]]), ""
      ),
      ")\n",
      .sdbuildR_env[["P"]][["sim_df_name"]],
      ", ", .sdbuildR_env[["P"]][["parameter_name"]],
      ", ", .sdbuildR_env[["P"]][["parameter_names"]],
      ", ", .sdbuildR_env[["P"]][["initial_value_name"]],
      ", ", .sdbuildR_env[["P"]][["initial_value_names"]],
      " = clean_df(",
      .sdbuildR_env[["P"]][["prob_name"]], ", ",
      .sdbuildR_env[["P"]][["solution_name"]], ", ",
      .sdbuildR_env[["P"]][["model_setup_name"]], ".", .sdbuildR_env[["P"]][["initial_value_names"]], ", ",
      .sdbuildR_env[["P"]][["intermediaries"]], ", ",
      .sdbuildR_env[["P"]][["model_setup_name"]], ".", .sdbuildR_env[["P"]][["intermediary_names"]], ")\n"
    )

    # Save to CSV
    script <- paste0(
      script, '\nCSV.write("', filepath_sim, '", ', .sdbuildR_env[["P"]][["sim_df_name"]],
      ")\n\n# Delete variables\n",
      .sdbuildR_env[["P"]][["solution_name"]], " = Nothing\n",
      .sdbuildR_env[["P"]][["sim_df_name"]], " = Nothing\n",
      "Nothing"
    )
  } else if (!is.null(ensemble_pars)) {
    script <- paste0(
      "\n\n# Create ODE problem\n",
      .sdbuildR_env[["P"]][["prob_name"]], " = ODEProblem(", .sdbuildR_env[["P"]][["ode_func_name"]], "!, ",
      .sdbuildR_env[["P"]][["model_setup_name"]], ".", .sdbuildR_env[["P"]][["initial_value_name"]],
      ", ", .sdbuildR_env[["P"]][["times_name"]], ", ",
      .sdbuildR_env[["P"]][["model_setup_name"]], ".", .sdbuildR_env[["P"]][["parameter_name"]],
      ")\n\n",


      # Callback in ensemble
      # https://discourse.julialang.org/t/savingcallback-when-using-ensemble-simulations/88483/5
      ifelse(!only_stocks, paste0(
        "\n\n# Set up intermediaries for saving in callback\n",
        .sdbuildR_env[["P"]][["intermediaries"]], " = Vector{SavedValues{eltype(", .sdbuildR_env[["P"]][["time_name"]], "), Any}}(undef, ",
        .sdbuildR_env[["P"]][["ensemble_total_n"]], ")\n\n# Populate the vector above with something to avoid undef\n",
        "for ", .sdbuildR_env[["P"]][["ensemble_iter"]], " in eachindex(", .sdbuildR_env[["P"]][["intermediaries"]], ")\n\t",
        .sdbuildR_env[["P"]][["intermediaries"]], "[", .sdbuildR_env[["P"]][["ensemble_iter"]], "] = SavedValues(eltype(", .sdbuildR_env[["P"]][["time_name"]], "), Any)\nend\n\n"
      ), ""),



      # Ensemble problem function
      "# Define ensemble problem\nfunction ", .sdbuildR_env[["P"]][["ensemble_func_name"]], "(prob, ", .sdbuildR_env[["P"]][["ensemble_iter"]], ", repeat)\n",
      static_eqn_script,
      ifelse(!only_stocks, paste0(
        "\n\t", .sdbuildR_env[["P"]][["callback_name"]],
        " = SavingCallback(", .sdbuildR_env[["P"]][["callback_func_name"]], ", ", .sdbuildR_env[["P"]][["intermediaries"]], "[", .sdbuildR_env[["P"]][["ensemble_iter"]], "], saveat = ",
        .sdbuildR_env[["P"]][["savefrom_name"]],
        ")\n"
      ), ""),
      "\n\tremake(prob, u0 = ",
      .sdbuildR_env[["P"]][["model_setup_name"]], ".", .sdbuildR_env[["P"]][["initial_value_name"]],
      ", p = ", .sdbuildR_env[["P"]][["model_setup_name"]], ".", .sdbuildR_env[["P"]][["parameter_name"]],
      ifelse(!only_stocks, paste0(
        ", ", .sdbuildR_env[["P"]][["callback_name"]],
        " = ", .sdbuildR_env[["P"]][["callback_name"]]
      ), ""),
      ")\nend\n\n",

      # Output function
      "function ", .sdbuildR_env[["P"]][["ensemble_output_func"]], "(sol, i)\n",
      "\t# Save both solution and parameters\n",
      "\treturn (t = sol.t, u = sol.u, p = sol.prob.p, u0 = sol.prob.u0), false\n",
      "end\n\n",

      # Ensemble problem definition
      .sdbuildR_env[["P"]][["ensemble_prob_name"]], " = EnsembleProblem(",
      .sdbuildR_env[["P"]][["prob_name"]], ", prob_func = ", .sdbuildR_env[["P"]][["ensemble_func_name"]],
      ", output_func = ", .sdbuildR_env[["P"]][["ensemble_output_func"]], ")\n",
      # Solve ensemble problem
      .sdbuildR_env[["P"]][["solution_name"]], " = solve(",
      .sdbuildR_env[["P"]][["ensemble_prob_name"]], ", ", sfm[["sim_specs"]][["method"]],
      ifelse(ensemble_pars[["threaded"]], ", EnsembleThreads()", ""),
      ", dt = ", .sdbuildR_env[["P"]][["timestep_name"]],
      ", saveat = ", .sdbuildR_env[["P"]][["savefrom_name"]],
      ", adaptive = false, trajectories = ",
      .sdbuildR_env[["P"]][["ensemble_total_n"]], ");\n"
    )

    # Save timeseries dataframe
    script <- paste0(
      script, "\n# Save timeseries dataframe\n",
      .sdbuildR_env[["P"]][["sim_df_name"]], ", ", .sdbuildR_env[["P"]][["parameter_name"]], ", ", .sdbuildR_env[["P"]][["parameter_names"]], ", ", .sdbuildR_env[["P"]][["initial_value_name"]], ", ", .sdbuildR_env[["P"]][["initial_value_names"]],
      ifelse(ensemble_pars[["threaded"]], " = ensemble_to_df_threaded(", " = ensemble_to_df("),
      .sdbuildR_env[["P"]][["solution_name"]], ", ",
      .sdbuildR_env[["P"]][["model_setup_name"]], ".", .sdbuildR_env[["P"]][["initial_value_names"]],
      ", ",
      .sdbuildR_env[["P"]][["intermediaries"]],
      ", ",
      .sdbuildR_env[["P"]][["model_setup_name"]], ".", .sdbuildR_env[["P"]][["intermediary_names"]],
      ", ", .sdbuildR_env[["P"]][["ensemble_n"]],
      ")\n"
    )

    if (ensemble_pars[["return_sims"]]) {
      script <- paste0(script, 'CSV.write("', ensemble_pars[["filepath_df"]], '", ', .sdbuildR_env[["P"]][["sim_df_name"]], ")\n")
    }


    # Compute summary statisics
    script <- paste0(
      script, "\n# Compute summary statisics\n",

      # .sdbuildR_env[["P"]][["sim_df_name"]], " = SciMLBase.EnsembleSummary(", .sdbuildR_env[["P"]][["solution_name"]], ".", .sdbuildR_env[["P"]][["solution_name"]], ", quantiles = [", quantiles[1], ", ", quantiles[2], "])\n"

      .sdbuildR_env[["P"]][["summary_df_name"]],
      ifelse(ensemble_pars[["threaded"]], " = ensemble_summ_threaded(", " = ensemble_summ("),
      .sdbuildR_env[["P"]][["sim_df_name"]], ", ",
      "[", paste0(ensemble_pars[["quantiles"]], collapse = ", "),
      "])\n\n# Save to CSV\n",
      "CSV.write(\"", ensemble_pars[["filepath_summ"]], "\", ", .sdbuildR_env[["P"]][["summary_df_name"]], ")\n\n# Delete variables\n",
      .sdbuildR_env[["P"]][["sim_df_name"]], " = Nothing\n",
      .sdbuildR_env[["P"]][["summary_df_name"]], " = Nothing\n",
      .sdbuildR_env[["P"]][["solution_name"]], " = Nothing\n",
      .sdbuildR_env[["P"]][["intermediaries"]], " = Nothing\n"
    )
  }

  return(list(script = script))
}






#' Write a string to a temporary file
#'
#' @param script String containing the code to write
#' @param fileext String with file extension, either ".R" or ".jl"
#' @return The path to the created file
#'
#' @noRd
write_script <- function(script,
                         filepath) {
  filepath <- normalizePath(filepath, winslash = "/", mustWork = FALSE)

  # Decode unicode characters when writing to Julia
  if (tools::file_ext(filepath) == "jl") {
    if (grepl("(\\\\u|\\\\\\\\u)[0-9a-fA-F]{4}", script)) {
      script <- decode_unicode(script)
    }
  }

  # Write the script to the file
  writeLines(script, filepath, useBytes = FALSE)

  invisible()
}


#' Get a temporary file path with a specific extension
#'
#' @param fileext String with file extension, either ".R" or ".jl"
#'
#' @returns Filepath to temporary file
#' @noRd
get_tempfile <- function(fileext) {
  filepath <- normalizePath(tempfile(fileext = fileext), winslash = "/", mustWork = FALSE)
  return(filepath)
}


#' Decode unicode characters in a string
#'
#' @param text String containing unicode escape sequences (e.g., "\\uXXXX")
#'
#' @returns String with unicode characters decoded
#' @noRd
#'
decode_unicode <- function(text) {
  stringr::str_replace_all(
    text,
    "(\\\\u|\\\\\\\\u)[0-9a-fA-F]{4}",
    function(matched) {
      # Extract the Unicode escape sequence
      jsonlite::fromJSON(sprintf('"%s"', matched))
    }
  )
}
