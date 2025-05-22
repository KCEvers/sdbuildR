

#' Simulate stock-and-flow model in julia
#'
#' @inheritParams simulate
#'
#' @return List with variables created in the simulation script
#' \describe{
#'   \item{dt}{Numeric, the timestep}
#'   \item{times}{Numeric, sequence of time values}
#'   \item{ode_func}{Function, the ODE function}
#'   \item{pars}{List, constant parameters (i.e. static Auxiliaries)}
#'   \item{xstart}{Numeric, initial value of Stocks}
#'   \item{df}{Dataframe, timeseries of computed variables in the ODE}
#'   \item{...}{Other variables created in the simulation script.}
#' }
#' @noRd
#'
simulate_julia = function(sfm,
                          format_code=TRUE,
                          keep_nonnegative_flow = TRUE,
                          keep_nonnegative_stock = FALSE,
                          keep_unit = TRUE,
                          only_stocks = FALSE,
                          verbose = FALSE,
                          debug = FALSE){


  # Collect arguments
  argg <- c(
    as.list(environment()))
  # Remove NULL arguments
  argg = argg[!lengths(argg) == 0]
  # Remove some elements
  # argg[c("sfm")] = NULL

  # Get output filepaths
  filepath_df = get_tempfile(fileext = ".csv")
  filepath = get_tempfile(fileext = ".jl")

  # Compile script
  script = compile_julia(sfm, filepath_df = filepath_df,
                         format_code = format_code,
                         keep_nonnegative_flow = keep_nonnegative_flow,
                         keep_nonnegative_stock = keep_nonnegative_stock,
                         only_stocks = only_stocks,
                         keep_unit = keep_unit, debug = debug)
  write_script(script, filepath)
  script = paste0(readLines(filepath), collapse = "\n")

  # Evaluate script
  sim = tryCatch({

    # Evaluate script
    # JuliaCall::julia_setup()

    use_julia()

    start_t = Sys.time()

    # Wrap in invisible and capture.output to not show message of units module being overwritten
    invisible(utils::capture.output(JuliaConnectoR::juliaEval(paste0('include("', filepath, '")'))))

    end_t = Sys.time()

    if (verbose){
      message(paste0("Simulation took ", round(end_t - start_t, 4), " seconds"))
    }

    # df = JuliaConnectoR::juliaGet(JuliaConnectoR::juliaEval(P[["sim_df_name"]]))
    pars_julia = JuliaConnectoR::juliaGet(JuliaConnectoR::juliaEval(P[["parameter_name"]]))

    # pars_julia = JuliaConnectoR::juliaGet(JuliaConnectoR::juliaEval(paste0("values(", P[["parameter_name"]], ")")))

    xstart_julia = JuliaConnectoR::juliaEval(P[["initial_value_name"]])

    # df <- utils::read.csv(filepath_df) # reads numbers as strings
    df = as.data.frame(data.table::fread(filepath_df, na.strings = c("", "NA")))

    # Delete files
    file.remove(filepath)
    file.remove(filepath_df)

    # units_julia = JuliaCall::julia_eval(P$units_dict)

    list(success = TRUE,
         df = df, pars = pars_julia,
         xstart = xstart_julia,
         # units = units_julia,
         script = script,
         # filepath = filepath,
         duration = end_t - start_t) %>% utils::modifyList(argg) %>%
      structure(., class = "sdbuildR_sim")

  },
  error = function(e) {
    warning("\nAn error occurred while running the Julia script.")
    # print(e$message)
    list(success = FALSE, error_message = e$message,
         script = script,
         filepath = filepath) %>% utils::modifyList(argg) %>%
      structure(., class = "sdbuildR_sim")
  })


  return(sim)

}


#' Compile Julia script to simulate stock-and-flow model
#'
#' @inheritParams simulate
#'
#' @return Julia script
#' @noRd
#'
compile_julia = function(sfm, filepath_df,
                         format_code=TRUE,
                         keep_nonnegative_flow = TRUE,
                         keep_nonnegative_stock = FALSE,
                         keep_unit = TRUE, only_stocks = FALSE,
                         verbose = FALSE, debug = FALSE){

  # Add "inflow" and "outflow" entries to stocks to match flow "to" and "from" entries
  flow_df = get_flow_df(sfm)

  sfm$model$variables$stock = lapply(sfm$model$variables$stock,
                                     function(x){

                                       x$inflow = flow_df[flow_df$to == x$name, "name"]
                                       x$outflow = flow_df[flow_df$from == x$name, "name"]

                                       if (length(x$inflow) == 0){
                                         x$inflow = ""
                                       }
                                       if (length(x$outflow) == 0){
                                         x$outflow = ""
                                       }

                                       return(x)
                                     })


  # if (length(sfm$model$variables$flow) == 0){
  #   stop("Your model contains no Flows!")
  # }

  # Adjust keep_unit to FALSE if there are no units defined
  names_df = get_names(sfm)
  var_names = get_model_var(sfm)

  # ** check
  names_df_no_flow = names_df
  # Don't check whether flows have units because these are automatically added
  names_df_no_flow = names_df_no_flow[names_df_no_flow$type != "flow", ]
  # keep_unit = ifelse(!any(nzchar(names_df_no_flow$units) & names_df_no_flow$units != "1"), FALSE, keep_unit)

  all_eqns = c(sfm$model$variables %>% lapply(function(x){lapply(x, `[[`, "eqn")}) %>% unlist(),
               # sfm$global$eqn_julia,
               unlist(lapply(sfm$macro, `[[`, "eqn")))
  units_used = unlist(stringr::str_extract_all(all_eqns, "\\bu\\([\"|'](.*?)[\"|']\\)"))

  keep_unit = ifelse(!any(names_df_no_flow$units != "1" & nzchar(names_df_no_flow$units)) & length(units_used) == 0, FALSE, keep_unit)

  # if (keep_unit){
  #   # Ensure all units are defined
  #   add_model_units = detect_undefined_units(sfm,
  #                                      new_eqns = c(sfm$model$variables %>% lapply(function(x){lapply(x, `[[`, "eqn_julia")}) %>% unlist(),
  #                                                   sfm$global$eqn_julia,
  #                                                   unlist(lapply(sfm$macro, `[[`, "eqn_julia"))),
  #                                      new_units = sfm$model$variables %>% lapply(function(x){lapply(x, `[[`, "units")}) %>% unlist())
  #   sfm$model_units = add_model_units %>% utils::modifyList(sfm$model_units)
  # }


  # **# Convert conveyors
  # sfm = convert_conveyor(sfm)
  #

  # Order Stocks alphabetically for order in xstart_names and xstart
  sfm$model$variables$stock = sfm$model$variables$stock[sort(names(sfm$model$variables$stock))]

  # # Add keyword arguments to all functions - do this at compilation in case a function is redefined
  # sfm = add_keyword_arg_wrapper(sfm, var_names)

  # ** to do: remove all argument names from functions and order arguments correctly
  # sfm = order_arg_in_func_wrapper(sfm)


  # Compile all parts of the R script
  times = compile_times_julia(sfm, keep_unit)
  # constraints = compile_constraints_julia(sfm)
  # constants = split_aux(sfm)
  # ordering = order_equations(sfm, constants)

  # Prepare model for delayN() and smoothN() functions
  delayN_smoothN = get_delayN_smoothN(sfm)

  sfm = prep_delayN_smoothN(sfm, delayN_smoothN)

  # Order equations including delayN() and smoothN() functions, if any
  ordering = order_equations(sfm)

  # If there are no dynamic variables or delayed variables, set only_stocks to TRUE
  delay_past = get_delay_past(sfm)

  only_stocks = ifelse(is.null(ordering$dynamic$order) & length(delay_past) == 0, TRUE, only_stocks)

  # Don't allow for mixing static and dynamic equations if any delay family functions are specified
  if (length(delay_past) > 0 | length(delayN_smoothN) > 0){
    ordering$static_and_dynamic$issue = TRUE
  }

  # # Translate Julia equations
  # sfm = convert_equations_julia_wrapper(sfm, debug = debug)

  # Macros
  macros = compile_macros_julia(sfm, debug = debug)

  # # Add prefixes (p.) and units to static equations
  # sfm = substitute_var_julia(sfm, constants, keep_unit)

  # # Function definitions (macros and helper functions)
  # func_def = compile_func_julia(sfm, keep_nonnegative_flow, keep_unit)

  # Prepare equations
  sfm = prep_equations_variables_julia(sfm, keep_unit, keep_nonnegative_flow)

  # Static equations
  static_eqn = compile_static_eqn_julia(sfm, ordering, keep_unit)

  # Stocks
  sfm = prep_stock_change_julia(sfm, keep_unit)

  # Compile unit definitions
  units_def = compile_units_julia(sfm, keep_unit)

  # Seed string
  seed_str = ifelse(!is_defined(sfm$sim_specs$seed), "",
                    sprintf("# Ensure reproducibility across runs in case of random elements\nRandom.seed!(%s)\n", as.character(sfm$sim_specs$seed)))

  prep_script = sprintf("# Script generated on %s by sdbuildR.\n\n%s%s%s%s", Sys.time(), seed_str,
                        units_def$script,
                        times$script,
                        macros$script
  )

  # Compile ODE script
  ode = compile_ode_julia(sfm, ordering,
                          prep_script, static_eqn,
                          # constraints,
                          keep_nonnegative_stock,
                          keep_unit,
                          only_stocks = only_stocks)

  run_ode = compile_run_ode_julia(sfm, filepath_df = filepath_df,
                                  only_stocks = only_stocks,
                                  stock_names = static_eqn$stock_names,
                                  intermediary_var = ode$intermediary_var,
                                  keep_unit = keep_unit)

  script = sprintf("%s\n%s%s%s%s",
                   prep_script,
                   ode$script_ode,
                   ode$script_callback,
                   static_eqn$script,
                   run_ode$script
  )


  return(script)
}



#' Prepare model for delayN and smoothN
#'
#' @inheritParams build
#' @param delayN_smoothN List with delayN and smoothN functions
#'
#' @returns Updated stock-and-flow model
#' @noRd
prep_delayN_smoothN = function(sfm, delayN_smoothN){

  # If delayN() and smoothN() were used, add these to the model
  if (length(delayN_smoothN) > 0){

    names_df = get_names(sfm)
    delayN_smoothN = unlist(unname(delayN_smoothN), recursive = FALSE)

    sfm$model$variables$stock = append(sfm$model$variables$stock,
                                       lapply(seq_along(delayN_smoothN), function(i){
                                         x = delayN_smoothN[[i]]
                                         y = list()

                                         # In rare cases, the delayed variable is a graphical function, and in that case the unit of that variable cannot be found
                                         bare_var = sub("\\(.*", "", x[["var"]])

                                         # Unit is the same as the delayed variable
                                         y$units = names_df[names_df$name == bare_var, ]$units
                                         y$name = names(delayN_smoothN)[i]
                                         y$label = names(delayN_smoothN)[i]
                                         # y$initial = x[["initial"]]
                                         y$type = "delayN"

                                         y$eqn_julia = x[["setup"]]
                                         y$inflow = y[["update"]]
                                         return(y)
                                       }) %>% stats::setNames(., names(delayN_smoothN))
    )

    sfm$model$variables$aux = append(sfm$model$variables$aux,
                                     lapply(seq_along(delayN_smoothN), function(i){
                                       x = delayN_smoothN[[i]]
                                       y = list()

                                       # In rare cases, the delayed variable is a graphical function, and in that case the unit of that variable cannot be found
                                       bare_var = sub("\\(.*", "", x[["var"]])

                                       # Unit is the same as the delayed variable
                                       y$units = names_df[names_df$name == bare_var, ]$units
                                       y$name = names(delayN_smoothN)[i]
                                       y$label = names(delayN_smoothN)[i]
                                       y$type = "delayN"

                                       y$eqn_julia = x[["compute"]]
                                       return(y)
                                     }) %>% stats::setNames(., names(delayN_smoothN))
    )
  }

  return(sfm)

}





#' Compile script for setting minimum and maximum constraints in Julia
#'
#'
#' @return List
#' @importFrom rlang .data
#' @inheritParams build
#'
#' @noRd
compile_constraints_julia_old = function(sfm){

  # # **to do
  # # eval(Meta.parse()) has issues with environment in ODE, doesn't recognize variables...
  #
  # # Compile string of minimum and maximum constraints
  # constraint_def = sfm$model$variables %>% purrr::map(function(x){
  #   purrr::imap(x, function(y, name){
  #     if (any(c("min_julia", "max_julia") %in% names(y))){
  #       min_str = ifelse(is_defined(y$min_julia), paste0("min = ", y$min_julia), "")
  #       max_str = ifelse(is_defined(y$max_julia), paste0("max = ", y$max_julia), "")
  #
  #       # "Urge" => (min=0.0, max=100.0)
  #
  #       if (nzchar(min_str) | nzchar(max_str)){
  #         sprintf("%s => (%s%s%s,)", name, min_str, ifelse(nzchar(min_str) & nzchar(max_str), ", ", ""), max_str) %>% return()
  #       }
  #     }
  #
  #   })
  # }) %>% unlist() %>% paste0(., collapse = ",\n\t\t\t\t\t\t\t\t\t\t")
  #
  # script = ifelse(nzchar(constraint_def), sprintf("\n\n# Constraints of minimum and maximum value\n%s = get_logical_constraints(Dict(%s))\n", P$constraint_def, constraint_def ), "")
  #
  # update_ode = ifelse(nzchar(constraint_def), sprintf("\n\n\t\t# Check constraints\n\t\tcheck_constraints(%s, environment(), %s)\n", P$constraint_def, P$time_name), "")


  # constraint_pairs = @constraints(Healthy < 0, Healthy > 10, Flow < 0u"1/common_yr")
  # constraints_str = [pair[1] for pair in constraint_pairs]
  # constraints_violated = [pair[2] for pair in constraint_pairs]
  #
  # msg = join(["$c: $v" for (c, v) in zip(constraints_str, constraints_violated)], "\n")
  #
  # if any(constraints_violated)
  # throw(msg)
  # end


  # Compile string of minimum and maximum constraints
  constraint_def = lapply(sfm$model$variables, function(x){
    lapply(x, function(y){
      if (any(c("min_julia", "max_julia") %in% names(y))){
        unit_str = ifelse(is_defined(y$units) & y$units != "1", paste0("u\"", y$units, "\""), "")
        min_str = ifelse(is_defined(y$min_julia), paste0(y$name, " < ", y$min_julia,
                                                         unit_str), "")
        max_str = ifelse(is_defined(y$max_julia), paste0(y$name, " > ", y$max_julia,
                                                         unit_str), "")
        return(list(min_str, max_str))
      }

    })
  }) %>% unlist() %>% unname() %>% Filter(nzchar, .)

  # script = ifelse(length(constraint_def) > 0, get_func_julia()[["@constraints"]], "")
  script = ifelse(length(constraint_def) > 0,
                  sprintf("\n\n# Check constraints of minimum and maximum value\nconstraint_pairs = @constraints(", paste0(constraint_def, collapse = ", "), ")\nconstraints_str = [pair[1] for pair in constraint_pairs]\nconstraints_violated = [pair[2] for pair in constraint_pairs]\nmsg = join([\"$c: $v\" for (c, v) in zip(constraints_str, constraints_violated)], \"\n\")\n\nif any(constraints_violated)\n\tthrow(msg)\nend"),
                  "")

  return(list(script = script))
}



#' Compile script for defining a units module in Julia
#'
#' @inheritParams compile_julia
#'
#' @return List with script
#'
#' @noRd
compile_units_julia = function(sfm, keep_unit){

  script = ""

  # if ((keep_unit & length(sfm$model_units) > 0) | sfm$sim_specs$time_units %in% c("common_yr", "common_quarter", "common_month")){
  if (length(sfm$model_units) > 0){

    # Topological sort of units
    if (length(sfm$model_units) > 1){
      eq_names = names(sfm$model_units)
      dependencies = lapply(sfm$model_units %>% get_map("eqn"),
                            function(x){stringr::str_extract_all(x, eq_names) %>% unlist() })
      out = topological_sort(dependencies)

      if (out$issue){
        message(paste0("Ordering custom units failed. ", paste0(out$msg)))
      }

      sfm$model_units = sfm$model_units[out$order]
    }

    # # Add standard custom units
    # custom_units = custom_units() %>%
    #   utils::modifyList(sfm$model_units)

    unit_str = lapply(sfm$model_units, function(x){

      if (is_defined(x$eqn)){
        unit_def = x$eqn
      } else {
        unit_def = "1"
      }

      paste0("@unit ", x$name, " \"", x$name, "\" ", x$name, " u\"", unit_def, "\" ", ifelse(x$prefix, "true", "false"))

    }) %>% paste0(collapse = sprintf("\n\tUnitful.register(%s)\n\t", P$MyCustomUnits))

    script = paste0("\n# Define custom units; register after each unit as some units may be defined by other units\nmodule ", P$MyCustomUnits, "\n\tusing Unitful\n\tusing Main.", P$sdbuildR_units, "\n\t",
                    unit_str,
                    "\n\tUnitful.register(", P$MyCustomUnits, ")\nend\n\n",
                    P$unit_context, " = [Unitful.Unitful, ",
                    P$sdbuildR_units, ", ", P$MyCustomUnits, "];\n\n")


  }

  return(list(script = script))
}



#' Compile julia script for global variables
#'
#' @inheritParams compile_R
#'
#' @return List with macro script
#' @noRd
compile_macros_julia = function(sfm, debug){

  script = ""


  # # If there are globals
  # if (is_defined(sfm$global$eqn_julia)){
  #   # names_df = get_names(sfm)
  #
  #   script = paste0(script, sfm$global$eqn_julia)
  #
  # }

  # If there are macros
  if (any(nzchar(purrr::map_vec(sfm$macro, "eqn_julia")))){
    # names_df = get_names(sfm)

    script = paste0(script, "\n",
                    lapply(sfm$macro, function(x){

                      # # If a name is defined, assign macro to that name
                      # if (nzchar(x$name)){
                      #   paste0(x$name, " = ", x$eqn_julia) %>% return()
                      # } else {
                      x$eqn_julia %>% return()
                      # }

                    }) %>% unlist() %>% paste0(collapse = "\n"))

  }

  if (nzchar(script)){
    script = paste0("\n\n# User-specified macros\n", script, "\n\n")
  }

  return(list(script = script))
}






#' Compile julia script for creating time vector
#'
#' @return List
#' @importFrom rlang .data
#'
#' @inheritParams compile_julia
#'
#' @noRd
compile_times_julia = function(sfm, keep_unit){

  script = sprintf("\n\n# Simulation time unit (smallest time scale in your model)
%s = u\"%s\"\n# Define time sequence\n%s = (%s, %s)%s\n# Initialize time (only necessary if constants use t)\n%s = %s[1];\n# Time step\n%s = %s%s\n",
                   P$time_units_name, sfm$sim_specs$time_units,
                   P$times_name, sfm$sim_specs$start, sfm$sim_specs$stop,
                   ifelse(keep_unit, paste0(" .* ", P$time_units_name), ""),
                   # paste0(" .* ", P$time_units_name),
                   P$time_name, P$times_name,
                   P$timestep_name, sfm$sim_specs$dt,
                   ifelse(keep_unit, paste0(" * ", P$time_units_name), "")
                   # paste0(" * ", P$time_units_name)

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
prep_equations_variables_julia = function(sfm, keep_unit, keep_nonnegative_flow){

  names_df = get_names(sfm)

  # Graphical functions
  sfm$model$variables$gf = lapply(sfm$model$variables$gf,
                  function(x){

                    if (is_defined(x$xpts) & is_defined(x$ypts)){

                      # Check whether xpts is defined as numeric or string
                      if (inherits(x$xpts, "numeric")){
                        xpts_str = paste0(as.character(x$xpts), collapse= ", ") %>% paste0("[", ., "]")
                      } else {
                        xpts_str = x$xpts %>% stringr::str_replace_all("^c\\(", "[")%>% stringr::str_replace_all("\\)$", "]")
                      }

                      # Add units of source if defined
                      if (keep_unit){

                        if (is_defined(x$source)){
                          if (x$source == "t"){
                            xpts_str = paste0(xpts_str, " .* ", P$time_units_name)
                          } else {
                            unit_source = names_df[names_df$name == x$source, "units"]
                            if (unit_source != "1"){
                              xpts_str = paste0(xpts_str, " .* u\"", unit_source, "\"")
                            }
                          }
                        }

                      }

                      # Check whether ypts is defined as numeric or string
                      if (inherits(x$ypts, "numeric")){
                        ypts_str = paste0(as.character(x$ypts), collapse= ", ") %>% paste0("[", ., "]")
                      } else {
                        ypts_str = x$ypts %>% stringr::str_replace_all("^c\\(", "[")%>% stringr::str_replace_all("\\)$", "]")
                      }

                      if (keep_unit & is_defined(x$units) & x$units != "1"){
                        ypts_str = paste0(ypts_str, " .* u\"", x$units, "\"")
                      }

                      x$eqn_str = sprintf("%s = itp(%s, %s, method = \"%s\", extrapolation = \"%s\")",
                              x$name, xpts_str, ypts_str, x$interpolation, x$extrapolation)

                    }
                    return(x)
  })

  # Constant equations
  sfm$model$variables$constant = lapply(sfm$model$variables$constant,
                        function(x){

                          if (keep_unit & is_defined(x$units) & x$units != "1"){
                            x$eqn_str = paste0(x$name, " = ", P$convert_u, "(", x$eqn_julia, ", u\"", x$units, "\")")
                          } else {
                            x$eqn_str = paste0(x$name, " = ", x$eqn_julia)
                          }
                          return(x)
                        })

  # Initial states of Stocks
  sfm$model$variables$stock = lapply(sfm$model$variables$stock,
                     function(x){

                       if (keep_unit & is_defined(x$units) & x$units != "1"){
                         x$eqn_str = paste0(x$name, " = ", P$convert_u, "(", x$eqn_julia, ", u\"", x$units, "\")") %>% return()
                       } else {
                         x$eqn_str = paste0(x$name, " = ", x$eqn_julia) %>% return()
                       }
                       return(x)

                     })


  # Auxiliary equations (dynamic auxiliaries)
  sfm$model$variables$aux = lapply(sfm$model$variables$aux,
                   function(x){

                     if (keep_unit & is_defined(x$units) & x$units != "1"){
                       x$eqn_str = paste0(x$name, " = ", P$convert_u, "(", x$eqn_julia, ", u\"", x$units, "\")") %>% return()
                     } else {
                       x$eqn_str = paste0(x$name, " = ", x$eqn_julia)
                     }
                     # }

                     if (!is.null(x$preceding_eqn)){
                       x$eqn_str = c(x$preceding_eqn, x$eqn_str)
                     }
                     return(x)
                   })

  # Flow equations
  flow_df = get_flow_df(sfm)
  sfm$model$variables$flow = lapply(sfm$model$variables$flow,
                    function(x){

                      x$eqn_str = sprintf("\n\t# Flow%s%s\n\t%s = %s%s%s%s%s",
                                    # Add comment
                                    ifelse(is_defined(x$from), paste0(" from ", x$from), ""),
                                    ifelse(is_defined(x$to), paste0(" to ", x$to), ""),
                                    x$name,
                                    ifelse(keep_unit & x$units != "1", paste0(P$convert_u, "("), ""),
                                    ifelse(x$non_negative & keep_nonnegative_flow, "nonnegative(", ""),
                                    x$eqn_julia,
                                    ifelse(x$non_negative & keep_nonnegative_flow, ")", ""),
                                    ifelse(keep_unit & x$units != "1", paste0(", u\"", x$units, "\")"), "")

                      )

                      if (!is.null(x$preceding_eqn)){
                        x$eqn_str = c(x$preceding_eqn, x$eqn_str)
                      }
                      return(x)
                    })


  return(sfm)
}


#' Compile Julia script for static variables, i.e. initial conditions, functions, and parameters
#'
#' @inheritParams compile_julia
#' @inheritParams order_equations
#' @param ordering List with order of static and dynamic variables, output of order_equations()
#'
#' @return List with necessary scripts
#'
#' @noRd
compile_static_eqn_julia = function(sfm, ordering, keep_unit){

  names_df = get_names(sfm)

  # Graphical functions
  gf_eqn = lapply(sfm$model$variables$gf, `[[`, "eqn_str")

  # Constant equations
  constant_eqn = lapply(sfm$model$variables$constant,`[[`, "eqn_str")

  # Initial states of Stocks
  stock_eqn = lapply(sfm$model$variables$stock,`[[`, "eqn_str")

  if (ordering$static_and_dynamic$issue){

    # Compile and order static equations
    static_eqn_str = c(gf_eqn, constant_eqn, stock_eqn)[ordering$static$order] %>%
      unlist() %>%
      paste0(collapse = "\n")
    static_eqn_str

  } else {

    # Auxiliary equations (dynamic auxiliaries)
    aux_eqn = lapply(sfm$model$variables$aux,`[[`, "eqn_str")

    # Flow equations
    flow_eqn = lapply(sfm$model$variables$flow,`[[`, "eqn_str")

    # Compile and order static and dynamic equations
    static_eqn_str = c(gf_eqn, constant_eqn, stock_eqn,
                       aux_eqn, flow_eqn)[ordering$static_and_dynamic$order] %>%
      unlist() %>%
      paste0(collapse = "\n")
    static_eqn_str
  }

  # Put parameters together in named tuple
  if (length(sfm$model$variables$constant) > 0){
    pars_def = paste0("\n\n# Define parameters in named tuple\n", P$parameter_name, " = (", paste0(names(constant_eqn), " = ", names(constant_eqn), collapse = ", "), ",)\n")
  } else {
    pars_def = paste0("\n\n# Define empty parameters\n", P$parameter_name, " = ()\n")
  }

  # Check for delayN() and smoothN() functions
  delayN_smoothN = get_delayN_smoothN(sfm)

  if (length(delayN_smoothN) > 0){
    delay_names = names(unlist(unname(delayN_smoothN), recursive = FALSE))

    xstart_def_stocks = paste0(
      paste0(setdiff(names(stock_eqn), delay_names), collapse = ", "), ", ",
      paste0(paste0("values(", delay_names, ")"), collapse = ", "))
    xstart_names = paste0(
      # paste0(paste0("\"", setdiff(names(stock_eqn), delay_names), "\""), collapse = ", "), ", ",
      paste0(paste0(":", setdiff(names(stock_eqn), delay_names)), collapse = ", "), ", ",
      paste0(paste0("keys(", delay_names, ")..."), collapse = ", ")
    )

  } else {
    xstart_def_stocks = paste0(names(stock_eqn), collapse = ", ")
    # xstart_names = paste0(paste0("\"", names(stock_eqn), "\""), collapse = ", ")
    # Symbols are faster than characters
    xstart_names = paste0(paste0(":", names(stock_eqn)), collapse = ", ")
  }

  # Put initial states together in (unnamed) vector
  xstart_def = paste0("\n# Define initial condition in vector\n", P$initial_value_name,
                      # " = reduce(vcat, (",
                      " = [Base.Iterators.flatten([",
                      xstart_def_stocks,

                      # Add extra comma in case there is only one Stock
                      # ",))\n"
                      ",])...]\n"
  )

  xstart_names = paste0(P$initial_value_names, " = [",
                        xstart_names,
                        "]\n")

  return(list(
    stock_names = names(stock_eqn),
    par_names = names(constant_eqn),
              script = paste0(
                "\n\n# Define parameters, initial conditions, and functions in correct order\n",
                static_eqn_str,
                pars_def, xstart_def, xstart_names
              )))
}



#' Prepare for summing change in stocks in stock-and-flow model in julia script
#'
#' @inheritParams compile_julia
#'
#' @return Updated stock-and-flow model
#' @noRd
#'
prep_stock_change_julia = function(sfm, keep_unit){

  # Add temporary property to sum change in Stocks
  stock_names = names(sfm$model$variables$stock)
  sfm$model$variables$stock = lapply(sfm$model$variables$stock,
                                     function(x){

                                       inflow = outflow = ""

                                       if (x$type == "delayN"){
                                         regex_find_idx = paste0("findall(n -> occursin(r\"", x$name, P$delayN_acc_suffix, "[0-9]+$\", string(n)), ", P$initial_value_names, ")")
                                         x$sum_name = paste0(P$change_state_name, "[", regex_find_idx, "]")
                                         x$unpack_state = paste0(P$state_name, "[", regex_find_idx, "]")
                                       } else {
                                         x$sum_name = paste0(P$change_state_name, "[", match(x$name, stock_names), "]")
                                       }


                                       # In case no inflow and no outflow is defined, update with 0
                                       if (!is_defined(x$inflow) & !is_defined(x$outflow)){
                                         # sprintf("%s%s = 0 # Collect Flows of %s", P$change_prefix, y, y) %>% return()


                                         # If keep_unit = TRUE, flows always need to have units as the times variable has units
                                         if (keep_unit){
                                           # x$sum_eqn = paste0(P$convert_u, "(0.0, u\"", x$units, "/", sfm$sim_specs$time_units, "\")")

                                           # Safer: in case x evaluates to a unit but no units were set
                                           x$sum_eqn = paste0(P$convert_u, "(0.0, Unitful.unit.(", x$name, ")/", P$time_units_name, ")")

                                         } else {
                                           x$sum_eqn = "0.0"
                                         }


                                       } else {
                                         if (is_defined(x$inflow)){
                                           inflow = x$inflow %>% paste0(collapse = " + ")
                                         }
                                         if (is_defined(x$outflow)){
                                           outflow = paste0(" - ", x$outflow) %>% paste0(collapse = "")
                                         }
                                         x$sum_eqn = sprintf("%s%s", inflow, outflow)
                                       }

                                       # Add units if defined
                                       if (keep_unit & is_defined(x$units)){
                                         # x$sum_eqn = paste0(P$convert_u, "(", x$sum_eqn, ", u\"", x$units, "/", sfm$sim_specs$time_units, "\")")
                                         x$sum_eqn = paste0(P$convert_u, "(", x$sum_eqn, ", Unitful.unit.(", x$name, ")/", P$time_units_name, ")")

                                       }
                                       # }
                                       return(x)

                                     })
  sfm$model$variables$stock = sfm$model$variables$stock[lengths(sfm$model$variables$stock) > 0]

  return(sfm)
}





#' Compile Julia script for ODE function
#'
#' @inheritParams build
#' @inheritParams compile
#' @inheritParams order_equations
#' @inheritParams compile_static_eqn
#' @param prep_script Intermediate output of compile_julia()
#' @param static_eqn Output of compile_static_eqn()
#'
#' @return List
#' @importFrom rlang .data
#' @noRd
#'
compile_ode_julia = function(sfm, ordering, prep_script, static_eqn,
                             # constraints,
                             keep_nonnegative_stock, keep_unit,
                             only_stocks){

  # @param constraints Intermediate output of compile_constraints_julia()

  # Auxiliary equations (dynamic auxiliaries)
  aux_eqn = lapply(sfm$model$variables$aux,`[[`, "eqn_str")

  # Flow equations
  flow_eqn = lapply(sfm$model$variables$flow,`[[`, "eqn_str")

  # Compile and order all dynamic equations
  dynamic_eqn = unlist(c(aux_eqn, flow_eqn)[ordering$dynamic$order])
  dynamic_eqn

  # Compile and order all dynamic equations
  dynamic_eqn_str = paste0(dynamic_eqn, collapse = "\n\t")

  # Create separate vector for names of intermediate variables and values, because graphical functions need to be in the intermediate funciton as gf(t), but their name should be gf
  intermediary_var = intermediary_var_values = names(dynamic_eqn)

  # Sum change in Stock equations
  stock_change = lapply(sfm$model$variables$stock,
                        function(x){

                          sprintf("%s %s %s", x$sum_name,
                                  # Broadcast assignment for delayed variables
                                  ifelse(x$type == "delayN", ".=", "="),
                                  x$sum_eqn)

                        }) %>% purrr::compact()

  stock_change_str = paste0(stock_change, collapse = "\n\t")

  # Non-negative Stocks
  nonneg_stocks = sfm$model$variables$stock %>% purrr::map("non_negative") %>% unlist()
  add_nonneg = any(nonneg_stocks) & keep_nonnegative_stock

  if (add_nonneg){

    # Create if-statement to keep selected Stocks non-negative
    nonneg_str = lapply(sfm$model$variables$stock,
                        function(x){
                          if (x$non_negative){
                            # sprintf("if (%s%s * %s + %s < 0) %s%s = %s/%s end",
                            #         P$change_prefix, y, P$timestep_name, y, P$change_prefix, y, y, P$timestep_name
                            # )
                            sprintf("if (%s * %s + %s < 0) %s = %s/%s end",
                                    x$sum_name, P$timestep_name, x$name, x$sum_name, x$name, P$timestep_name
                            )
                          } else {
                            return(NULL)
                          }
                        }) %>% purrr::compact()

    # Format complete string with explanation
    stock_change_str = paste0(stock_change_str,
                              "\n\n\t# Prevent ", paste0(names(nonneg_str), collapse = ", "), " from turning negative\n\t",
                              paste0(nonneg_str, collapse = "\n\t")
    )
  }

  # Names of changing Stocks, e.g. dR for stock R
  stock_changes_names = unname(unlist(lapply(sfm$model$variables$stock, `[[`, "sum_name")))

  # Final update to derivative
  # state_change_str = sprintf("d%sdt[[%s]] .= %s",
  #                            P$state_name,
  #                            # ifelse(length(stock_changes_names) > 1,
  #                                   # paste0("1:", length(stock_changes_names)), "1"),
  #                            paste0(as.character(seq_along(stock_changes_names)), collapse = ", "),
  #                            paste0(stock_changes_names, collapse = ", ")
  #                            )

  # state_change_str = sprintf("d%sdt[collect(%s)] .= %s",
  #                            P$state_name,
  #                            ifelse(length(stock_changes_names) > 1,
  #                                   paste0("1:", length(stock_changes_names)), "1"),
  #                            paste0(stock_changes_names, collapse = ", ")
  # )

  # state_change_str = sprintf("d%sdt[%s] .= collect(Base.Iterators.Flatten([%s,]))",
  #                            P$state_name,
  #                            ifelse(length(stock_changes_names) > 1,
  #                                   paste0("1:", length(stock_changes_names)), "1"),
  #                            # paste0(as.character(seq_along(stock_changes_names)), collapse = ", "),
  #                            paste0(stock_changes_names, collapse = ", ")
  # )

  # Graphical functions (gf)
  gf_str = ""
  if (length(sfm$model$variables$gf) > 0){

    # Some gf have other gf as source; recursively replace
    gf_sources = lapply(sfm$model$variables$gf, `[[`, "source") %>% purrr::compact() %>% unlist()

    if (length(gf_sources) > 0){

      # Graphical functions with source
      gf = paste0(names(gf_sources), "(", unname(gf_sources), ")") %>% stats::setNames(names(gf_sources))

      # Create dictionary to add source to nested graphical functions
      dict2 = paste0("(", names(gf_sources), "(", unname(gf_sources), "))") %>%
        stats::setNames(paste0("\\(", stringr::str_escape(names(gf_sources)), "\\)"))

      gf_str = stringr::str_replace_all(unname(gf), dict2) #%>%
      # paste0(collapse = ", ")

      # Add names of graphical functions to intermediary_var
      intermediary_var = c(intermediary_var, names(gf))
      intermediary_var_values = c(intermediary_var_values, gf_str)

    }
  }


  # Add fixed delayed and past variables to intermediary_var
  # extra_intermediary_var = unname(sfm$model$variables) %>% purrr::list_flatten() %>%
  #   purrr::map("intermediary") %>% purrr::compact()
  # extra_intermediary_var = lapply(sfm$model$variables, function(x){
  #   lapply(x, `[[`, "intermediary")
  # }) %>% unlist() %>% unname()
  delay_past = get_delay_past(sfm)
  extra_intermediary_var = list_extract(delay_past, "var")

  if (length(extra_intermediary_var) > 0){

    # Get unique intermediary variables to add
    new_intermediary_var = setdiff(unique(unlist(extra_intermediary_var)), intermediary_var)

    if (length(new_intermediary_var) > 0){
      intermediary_var = c(intermediary_var, new_intermediary_var)
      intermediary_var_values = c(intermediary_var_values, new_intermediary_var)
    }

  }

  # If delayN() and smoothN() were used, state has to be unpacked differently
  delayN_smoothN = get_delayN_smoothN(sfm)

  if (length(delayN_smoothN) > 0){

    # delay_names = names(delayN_smoothN)
    delay_names = names(unlist(unname(delayN_smoothN), recursive = FALSE))

    intermediary_var = setdiff(intermediary_var, delay_names)
    intermediary_var_values = setdiff(intermediary_var_values, delay_names)

    # Unpack non delayN stocks
    unpack_nondelayN = paste0(paste0(setdiff(names(stock_change), delay_names), collapse = ", "), ", = ", P$state_name, "[findall(n -> !occursin(r\"", P$delayN_suffix, "[0-9]+", P$delayN_acc_suffix, "[0-9]+$\", string(n)), ", P$initial_value_names, ")]")

    # Unpack each delayN or smoothN stock separately
    unpack_delayN = lapply(sfm$model$variables$stock,
                           function(x){
                             if (is_defined(x$unpack_state)){
                               paste0(x$name, " = ", x$unpack_state) %>% return()
                             }
                           }) %>% purrr::compact()

    unpack_state_str = paste0(unpack_nondelayN, "\n\t", paste0(unpack_delayN, collapse = "\n\t"))

  } else {
    unpack_state_str = paste0(paste0(names(stock_change), collapse = ", "), ", = ", P$state_name)
  }


  # Sometimes, step/pulse/ramp functions are in auxiliaries. Exclude these from intermediary_var
  idx = grepl("_step$|_pulse$|_ramp$", intermediary_var)
  intermediary_var = intermediary_var[!idx]
  intermediary_var_values = intermediary_var_values[!idx]

  # **to do: what if delayed variable is gf

  # Compile
  script_ode = paste0(
    sprintf("\n\n# Define ODE
function %s!(%s, %s%s, %s)",
            P$ode_func_name,
            P$change_state_name, P$state_name,
            paste0(", ", P$parameter_name), P$time_name),
    "\n\n\t# Unpack state variables\n\t",
    unpack_state_str,
    ifelse(length(sfm$model$variables$constant) > 0,
           paste0("\n\n\t# Unpack parameters\n\t",
                  paste0(paste0(static_eqn$par_names, collapse = ", "), ", = ", P$parameter_name)), ""),
    "\n\n\t# Update auxiliaries\n\t",
    dynamic_eqn_str,
    # constraints$script,
    "\n\n\t# Collect inflows and outflows for each Stock\n\t",
    stock_change_str,

    # "\n\n\t# State update\n\t",
    # state_change_str,
    "\n\tnothing\nend\n")

  # Compile callback function
  if (!only_stocks){
    script_callback = paste0(
      sprintf("\n\n# Define callback function
function %s(%s, %s, integrator)",
              P$callback_func_name,
              P$state_name, P$time_name),
      "\n\n\t# Unpack state variables\n\t",
      unpack_state_str,
      ifelse(length(sfm$model$variables$constant) > 0, paste0("\n\n\t# Get parameters from integrator\n\t",
                                                              paste0(P$parameter_name, " = integrator.p"), # Check whether you're overwriting it

                                                              "\n\n\t# Unpack parameters\n\t",
                                                              paste0(paste0(static_eqn$par_names, collapse = ", "), ", = ", P$parameter_name)), ""),

      "\n\n\t# Update auxiliaries\n\t",
      dynamic_eqn_str,

      "\n\n\t# Return intermediary values\n\t",
      "return(", paste0(intermediary_var_values, collapse = ", "), ")\n",

      "\n\nend\n\n# Callback setup\n",
      # P$intermediary_names, " = [", paste0("\"", intermediary_var, "\"", collapse = ", "), "]\n",
      # Symbols are faster than characters
      P$intermediary_names, " = [", paste0(paste0(":", intermediary_var), collapse = ", "), "]\n",
      P$intermediaries, " = SavedValues(",
      # Make time a Unitful.Quantity if keeping units, otherwise a float
      # ifelse(keep_unit, "Unitful.Quantity", "Float64"),
      "eltype(", P$time_name, "), Any)\n",

      P$callback_name, " = SavingCallback(", P$callback_func_name, ", ", P$intermediaries,
      # ", saveat = ", sfm$sim_specs$saveat,
      # ifelse(keep_unit, paste0(" * ", P$time_units_name), ""),
      ")\n")
  } else {
    script_callback = ""
  }
  return(list(script_ode = script_ode,
              script_callback = script_callback,
              intermediary_var = intermediary_var))
}





#' Compile Julia script for running ODE
#'
#' @param filepath_df Path to output file
#' @param nonneg_stocks Output of compile_nonneg_stocks()
#' @param stock_names Names of stocks
#' @param intermediary_var Names of intermediary variables
#' @inheritParams compile_ode_julia
#' @inheritParams compile_julia
#' @inheritParams order_equations
#'
#' @return List
#' @inheritParams compile_R
#' @noRd
#'
compile_run_ode_julia = function(sfm, filepath_df,
                                 nonneg_stocks,
                                 stock_names,
                                 intermediary_var,
                                 only_stocks, keep_unit){


  script = paste0("\n\n# Run ODE\n",
                  P$prob_name, " = ODEProblem(", P$ode_func_name, "!, ", P$initial_value_name, ", ", P$times_name, ", ", P$parameter_name, ")\n", P$solution_name, " = solve(", P$prob_name, ", ",
                  ifelse(
                    sfm$sim_specs$method == "euler", "Euler()",
                    ifelse(
                      sfm$sim_specs$method == "rk4", "RK4()", NA_character_
                    )
                  ),
                  ifelse(sfm$sim_specs$method %in% c("euler", "rk4"),
                         paste0(", dt = ", P$timestep_name, ", adaptive=", ifelse(sfm$sim_specs$adaptive, "true", "false")), ""),
                  ifelse(!only_stocks, paste0(", ", P$callback_name, " = ", P$callback_name
                                              # ", saveat = ", sfm$sim_specs$saveat,
                                              # ifelse(keep_unit, paste0(" * ", P$time_units_name), "")

                  ), ""), ")\n",
                  P$sim_df_name, " = ", ifelse(keep_unit, "Unitful.ustrip.(", ""), "DataFrame(", P$solution_name, ", [:time; ", P$initial_value_names, "]", ifelse(keep_unit, ")", ""), ")\n")



  if (!only_stocks){
    script = paste0(script,
                    "\n\n# Create DataFrame for intermediary variables\n", P$intermediary_df, " = ", ifelse(keep_unit, "Unitful.ustrip.(", ""), "DataFrame(",

                    ifelse(length(intermediary_var) > 1, paste0(P$intermediaries, ".saveval, ", P$intermediary_names),
                           # Error is thrown for dataframe creation of there is only one variable
                           # Necessary to add first.() because otherwise the column is a list in R, causing issues in plot(sim)
                           paste0(intermediary_var, " = first.(", P$intermediaries, ".saveval)")),

                    ")", ifelse(keep_unit, ")", ""), "\n",

                    # Don't select overlapping variables
                    ifelse(any(stock_names %in% intermediary_var), paste0("select!(", P$intermediary_df, ", setdiff(", P$intermediary_names, ", ", P$initial_value_names, "))\n" ), ""),


                    # "n_vars = length(", P$intermediary_names, ")\n\tvals = n_vars == 1 ? hcat([val[1] for val in ", P$intermediaries, ".saveval]) : hcat(", P$intermediaries, ".saveval...)\n\t", P$intermediary_df, " = DataFrame(vals, Symbol.(", P$intermediary_names, "))\n\t", P$intermediary_df, "[!, :time] = ", P$intermediaries, ".t",
#                     "\n\n\n# Select intermediary columns not in df (except time)
# cols_to_keep = [Symbol(name) for name in ", P$intermediary_names, " if !(name in ", P$initial_value_names, ")]\npush!(cols_to_keep, :time)\n",
#                     P$intermediary_df, " = select(", P$intermediary_df, ", cols_to_keep)",

"\n# Combine DataFrames\n", P$sim_df_name, " = hcat(", P$sim_df_name, ", ", P$intermediary_df, ")\n")
  }

  # script = paste0(script, sprintf("\n\n# Overwrite initial values to named dictionary and strip units\n%s = Dict(%s .=> Unitful.ustrip.(%s))\n", P$initial_value_name, P$initial_value_names, P$initial_value_name),
  #                 sprintf("\n# Strip units from parameters\n%s = (; (name => isa(val, Unitful.Quantity) ? ustrip(val) : val for (name, val) in pairs(%s))...)", P$parameter_name, P$parameter_name), "\n")



  # If different times need to be saved, linearly interpolate
  if (sfm$sim_specs$dt != sfm$sim_specs$saveat){
    script = paste0(script, "\n# Linearly interpolate to reduce stored values to saveat\n",
                    ifelse(keep_unit, paste0(P$times_name, " = Unitful.ustrip.(", P$times_name, ")\n"),""),
                    "new_times = collect(", P$times_name, "[1]:",
                    # ifelse(keep_unit, paste0("u\"", sfm$sim_specs$saveat, sfm$sim_specs$time_units, "\""),
                    #        sfm$sim_specs$saveat),
                    sfm$sim_specs$saveat,
                    ":", P$times_name, "[2]) # Create new time vector\n",

                    P$sim_df_name, " = DataFrame(Dict(\n",
                    ":time => new_times,
    [Symbol(col) => ", P$saveat_func, "(", P$sim_df_name, ".time, ", P$sim_df_name, "[!, col], new_times) for col in names(", P$sim_df_name, ") if col != \"time\"]...
))\n")

  } else {
    # In some cases, not setting saveat results in a dataframe that is too long.
    #  For some models, setting saveat throws an error when xstart has units. This happens when the solver t is not exactly t %% dt == 0.
    # e.g. "https://insightmaker.com/insight/77C9XTvvb66nQnGepYAn5m/Wind-Resistance-Model"
    #  We thus cannot use saveat but need to ensure the solution is saved at correct times.


    script = paste0(script, "\nif nrow(", P$sim_df_name, ") != length(", P$times_name, "[1]:", P$timestep_name, ":", P$times_name, "[2])", "\n# Linearly interpolate to reduce stored values to saveat\n",
                    ifelse(keep_unit, paste0(P$times_name, " = Unitful.ustrip.(", P$times_name, ")\n"),""),
                    "new_times = collect(", P$times_name, "[1]:",
                    # ifelse(keep_unit, paste0("u\"", sfm$sim_specs$saveat, sfm$sim_specs$time_units, "\""),
                    #        sfm$sim_specs$saveat),
                    sfm$sim_specs$saveat,
                    ":", P$times_name, "[2]) # Create new time vector\n",

                    P$sim_df_name, " = DataFrame(Dict(\n",
                    ":time => new_times,
    [Symbol(col) => ", P$saveat_func, "(", P$sim_df_name, ".time, ", P$sim_df_name, "[!, col], new_times) for col in names(", P$sim_df_name, ") if col != \"time\"]...
))\nend\n")
  }


  if (keep_unit){
    script = paste0(script,
                    # "\n\n# Extract and store units\n",
                    # P$units_dict, " = Dict(name => string(unit(val)) for (name, val) in pairs(first(", P$sim_df_name,
                    # ")) if isa(val, Unitful.Quantity))\n",
                    # "\n\n# Strip units from DataFrame\n", P$sim_df_name, " = Unitful.ustrip.(", P$sim_df_name, ")\n",
                    "\n\n# Strip units from initial conditions and parameters\n",
                    P$initial_value_name, " = Dict(xstart_names .=> Unitful.ustrip.(xstart))\n",
                    P$parameter_name, " = (; (name => isa(val, Unitful.Quantity) ? Unitful.ustrip(val) : val for (name, val) in pairs(pars))...)")

  }


  # Remove pars that are not floats or vectors
  script = paste0(script,


  "\n\n# Find keys where values are Float64 or Vector
  valid_keys = [k for k in keys(", P$parameter_name, ") if isa(", P$parameter_name, "[k], Float64) || isa(", P$parameter_name, "[k], Vector)]

  # Convert valid_keys to a tuple for NamedTuple construction
  valid_keys_tuple = Tuple(valid_keys)

  # Reconstruct filtered named tuple\n", P$parameter_name, " = NamedTuple{valid_keys_tuple}(",
  P$parameter_name, "[k] for k in valid_keys)\n")


  # Save to CSV
  script = paste0(script, '\nCSV.write("', filepath_df, '", ', P[["sim_df_name"]], ')\nNothing')


  # else {
  #   script = paste0(script, "\n# Assign empty dictionary to units\n", P$units_dict, " = Dict()\n")
  # }

  # ** remove delays ?

  return(list(script=script))
}






#' Write a string to a temporary file
#'
#' @param script String containing the code to write
#' @param fileext String with file extension, either ".R" or ".jl"
#' @return The path to the created file
#'
#' @noRd
#' @examples
#' julia_code <- "println(\"Hello from julia!\")"
#' filepath = get_tempfile(".jl")
#' write_script(julia_code, filepath)
write_script <- function(script,
                         # base_name = "model",
                         # dir = file.path(tempdir(), "julia_output"),
                         filepath) {

  # # Ensure directory exists
  # if (!dir.exists(dir)) {
  #   dir.create(dir, recursive = TRUE)
  # }
#
#   # Initial filename
#   filepath <- file.path(dir, paste0(base_name, ext))
#   filepath <- normalizePath(filepath, winslash = "/", mustWork = FALSE)
#
#   # Check if file exists and generate a unique name
#   counter <- 1
#   while (file.exists(filepath) & !overwrite) {
#     filepath <- file.path(dir, paste0(base_name, "_", counter, ext))
#     counter <- counter + 1
#   }
#
  # filepath = get_tempfile(fileext = fileext)
  filepath <- normalizePath(filepath, winslash = "/", mustWork = FALSE)


  # Decode unicode characters when writing to Julia
  if (tools::file_ext(filepath) == "jl"){
    if (grepl("(\\\\u|\\\\\\\\u)[0-9a-fA-F]{4}", script)){
      script = decode_unicode(script)
    }
  }

  # Write the script to the file
  writeLines(script, filepath)

  invisible()
}


#' Get a temporary file path with a specific extension
#'
#' @param fileext String with file extension, either ".R" or ".jl"
#'
#' @returns Filepath to temporary file
#' @noRd
get_tempfile = function(fileext){
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

  stringr::str_replace_all(text,
                           # "\\[\\]?u[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]",
                           "(\\\\u|\\\\\\\\u)[0-9a-fA-F]{4}",
                           function(matched) {
                             # Extract the Unicode escape sequence
                             jsonlite::fromJSON(sprintf('"%s"', matched))
                             # hex_code <- stringr::str_sub(matched, nchar(matched)-3, nchar(matched))  # Extract the "XXXX" part
                             # print(hex_code)
                             # # Convert the hexadecimal code to an integer
                             # code_int <- strtoi(hex_code, base = 16)
                             # # Convert the integer to the corresponding Unicode character
                             #
                             #
                             # print(intToUtf8(code_int))
                             # intToUtf8(code_int)

                           })

}



