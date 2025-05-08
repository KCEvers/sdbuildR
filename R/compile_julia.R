

#' Simulate stock-and-flow model in Julia
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
#'
simulate_Julia = function(sfm,
                          format_code=TRUE,
                          keep_nonnegative_flow = TRUE,
                          keep_nonnegative_stock = FALSE,
                          keep_unit = TRUE, verbose = FALSE, debug = FALSE, only_stocks = FALSE){


  # Collect arguments
  argg <- c(
    as.list(environment()))
  # Remove NULL arguments
  argg = argg[!lengths(argg) == 0]
  # Remove some elements
  # argg[c("sfm")] = NULL

  # Compile script without plot
  script = compile_Julia(sfm,
                         format_code=format_code,
                         keep_nonnegative_flow = keep_nonnegative_flow,
                         keep_nonnegative_stock = keep_nonnegative_stock,
                         keep_unit = keep_unit, debug = debug, only_stocks = only_stocks)
  filepath = write_script(script, ext = ".jl")
  script = readLines(filepath) %>% paste0(collapse = "\n")

  # Evaluate script
  sim = tryCatch({

    # # Create a new environment to collect variables
    # envir <- new.env()

    # from diffeqr
    # julia_locate <- do.call(":::", list("JuliaCall", quote(julia_locate)))
    # julia_locate()


    # Evaluate script
    # JuliaCall::julia_setup()

    # ** to do: check whether sdbuildR_setup() was run
    # if (!.julia$initialized){
    if (is.null(options()[["initialization_sdbuildR"]])){
      sdbuildR_setup()
    }

    # # Run initialization
    # run_init()

    start_t = Sys.time()
    JuliaCall::julia_source(filepath)
    end_t = Sys.time()
    if (verbose){
      message(paste0("Simulation took ", round(end_t - start_t, 4), " seconds"))
    }

    # Delete simulation file
    file.remove(filepath)

    df = JuliaCall::julia_eval(P$sim_df_name)
    pars_Julia = JuliaCall::julia_eval(P$parameter_name)
    xstart_Julia = JuliaCall::julia_eval(P$initial_value_name)
    units_Julia = JuliaCall::julia_eval(P$units_dict)

    # if (include_plot){
    #   # Plot stocks
    #   pl = plot_sim(sfm, df)
    #   argg$pl = pl
    # }

    list(success = TRUE,
               df = df, pars = pars_Julia, xstart = xstart_Julia,
         units = units_Julia, script = script, filepath = filepath,
         duration = end_t - start_t) %>% utils::modifyList(argg) %>%
      structure(., class = "sdbuildR_sim")

  },
  error = function(e) {
    warning("\nAn error occurred while writing or running the Julia script.")
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
#'
compile_Julia = function(sfm,
                             format_code=TRUE,
                             keep_nonnegative_flow = TRUE,
                             keep_nonnegative_stock = FALSE,
                             keep_unit = TRUE, verbose = FALSE, debug = FALSE, only_stocks = FALSE){

  # print("Compiling R script...")




  # Add "inflow" and "outflow" entries to stocks to match flow "to" and "from" entries
  # flow_to = sfm$model$variables$flow %>% purrr::map("to") %>% purrr::compact()
  # flow_from = sfm$model$variables$flow %>% purrr::map("from") %>% purrr::compact()

  flow_df = get_flow_df(sfm)

  sfm$model$variables$stock = lapply(sfm$model$variables$stock,
                                         function(x){

                                           x$inflow = flow_df[flow_df$to == x$name, "name"]
                                           x$outflow = flow_df[flow_df$from == x$name, "name"]
                                           # x$inflow = names(flow_to)[unname(unlist(flow_to)) == x$name]
                                           # x$outflow = names(flow_from)[unname(unlist(flow_from)) == x$name]

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
                                    sfm$global$eqn_Julia,
                                    unlist(lapply(sfm$macro, `[[`, "eqn")))
  units_used = unlist(stringr::str_extract_all(all_eqns, "\\bu\\([\"|'](.*?)[\"|']\\)"))

  keep_unit = ifelse(!any(names_df_no_flow$units != "1") & length(units_used) == 0, FALSE, keep_unit)

  # if (keep_unit){
  #   # Ensure all units are defined
  #   add_model_units = detect_undefined_units(sfm,
  #                                      new_eqns = c(sfm$model$variables %>% lapply(function(x){lapply(x, `[[`, "eqn_Julia")}) %>% unlist(),
  #                                                   sfm$global$eqn_Julia,
  #                                                   unlist(lapply(sfm$macro, `[[`, "eqn_Julia"))),
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
  times = compile_times_Julia(sfm, keep_unit)
  constraints = compile_constraints_Julia(sfm)
  # constants = split_aux(sfm)
  # ordering = order_equations(sfm, constants)

  # Prepare model for delayN() and smoothN() functions
  sfm = prep_delayN_smoothN(sfm)

  # Order equations including delayN() and smoothN() functions, if any
  ordering = order_equations(sfm)

  # If there are no dynamic variables or delayed variables, set only_stocks to TRUE
  # only_stocks = ifelse(is.null(ordering$dynamic$order) & length(delays$extra_intermediary_var) == 0, TRUE, only_stocks)
  # intermediary = unname(sfm$model$variables) %>% purrr::list_flatten() %>%
  #   purrr::map("intermediary") %>% purrr::compact()

  intermediary = lapply(sfm$model$variables, function(x){
    lapply(x, `[[`, "intermediary")
  }) %>% unlist() %>% unname()

  only_stocks = ifelse(is.null(ordering$dynamic$order) & length(intermediary) == 0, TRUE, only_stocks)


  # # Translate Julia equations
  # sfm = convert_equations_Julia_wrapper(sfm, debug = debug)

  # Macros
  macros = compile_macros_Julia(sfm, debug = debug)

  # # Add prefixes (p.) and units to static equations
  # sfm = substitute_var_Julia(sfm, constants, keep_unit)

  # # Function definitions (macros and helper functions)
  # func_def = compile_func_Julia(sfm, keep_nonnegative_flow, keep_unit)

  # Static equations
  static_eqn = compile_static_eqn_Julia(sfm, ordering, keep_unit)

  # Stocks
  sfm = prep_stock_change_Julia(sfm, keep_unit)

  # Compile unit definitions
  units_def = compile_units_Julia(sfm, keep_unit)

  # Seed string
  seed_str = ifelse(!is_defined(sfm$sim_specs$seed), "",
                    sprintf("# Ensure reproducibility across runs in case of random elements\nRandom.seed!(%s)\n", as.character(sfm$sim_specs$seed)))

  prep_script = sprintf("# Script generated on %s by sdbuildR.\n\n%s%s%s%s", Sys.time(), seed_str,
                        units_def$script,
                        times$script,
                        macros$script
                       )

  # Compile ODE script
  ode = compile_ode_Julia(sfm, ordering,
                          prep_script, static_eqn,
                          constraints,
                          keep_nonnegative_flow, keep_nonnegative_stock,
                          keep_unit,
                          only_stocks = only_stocks)

  run_ode = compile_run_ode_Julia(sfm, only_stocks = only_stocks,
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



#' Get delayN and smoothN from stock-and-flow model
#'
#' @inheritParams build
#'
#' @returns List with delayN and smoothN functions
get_delayN_smoothN = function(sfm){
  sfm$model$variables %>%
    purrr::map_depth(2, "func") %>% unname() %>%
    purrr::list_flatten() %>%
    unname() %>%
    purrr::list_flatten() %>%
    purrr::compact()
}



#' Prepare model for delayN and smoothN
#'
#' @inheritParams build
#'
#' @returns Updated stock-and-flow model
prep_delayN_smoothN = function(sfm){

  # If delayN() and smoothN() were used, add these to the model
  delay_func = get_delayN_smoothN(sfm)

  if (length(delay_func) > 0){

    names_df = get_names(sfm)

    sfm$model$variables$stock = append(sfm$model$variables$stock,
                                       lapply(seq_along(delay_func), function(i){
                                         x = delay_func[[i]]
                                         y = list()

                                         # Unit is the same as the delayed variable
                                         y$units = names_df[names_df$name == x[["var"]], ]$units
                                         y$name = names(delay_func)[i]
                                         y$label = names(delay_func)[i]
                                         y$type = "delayN"

                                         y$eqn_Julia = x[["setup"]]
                                         y$inflow = y[["update"]]
                                         return(y)
                                       }) %>% stats::setNames(., names(delay_func))
    )

    sfm$model$variables$aux = append(sfm$model$variables$aux,
                                     lapply(seq_along(delay_func), function(i){
                                       x = delay_func[[i]]
                                       y = list()

                                       # Unit is the same as the delayed variable
                                       y$units = names_df[names_df$name == x[["var"]], ]$units
                                       y$name = names(delay_func)[i]
                                       y$label = names(delay_func)[i]
                                       y$type = "delayN"

                                       y$eqn_Julia = x[["compute"]]
                                       return(y)
                                     }) %>% stats::setNames(., names(delay_func))
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
compile_constraints_Julia = function(sfm){

  # # **to do
  # # eval(Meta.parse()) has issues with environment in ODE, doesn't recognize variables...
  #
  # # Compile string of minimum and maximum constraints
  # constraint_def = sfm$model$variables %>% purrr::map(function(x){
  #   purrr::imap(x, function(y, name){
  #     if (any(c("min_Julia", "max_Julia") %in% names(y))){
  #       min_str = ifelse(is_defined(y$min_Julia), paste0("min = ", y$min_Julia), "")
  #       max_str = ifelse(is_defined(y$max_Julia), paste0("max = ", y$max_Julia), "")
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
      if (any(c("min_Julia", "max_Julia") %in% names(y))){
        unit_str = ifelse(is_defined(y$units) & y$units != "1", paste0("u\"", y$units, "\""), "")
        min_str = ifelse(is_defined(y$min_Julia), paste0(y$name, " < ", y$min_Julia,
                                                         unit_str), "")
        max_str = ifelse(is_defined(y$max_Julia), paste0(y$name, " > ", y$max_Julia,
                                                         unit_str), "")
        return(list(min_str, max_str))
      }

    })
  }) %>% unlist() %>% unname() %>% Filter(nzchar, .)

  # script = ifelse(length(constraint_def) > 0, get_func_Julia()[["@constraints"]], "")
  script = ifelse(length(constraint_def) > 0,
                      sprintf("\n\n# Check constraints of minimum and maximum value\nconstraint_pairs = @constraints(", paste0(constraint_def, collapse = ", "), ")\nconstraints_str = [pair[1] for pair in constraint_pairs]\nconstraints_violated = [pair[2] for pair in constraint_pairs]\nmsg = join([\"$c: $v\" for (c, v) in zip(constraints_str, constraints_violated)], \"\n\")\n\nif any(constraints_violated)\n\tthrow(msg)\nend"),
                      "")

  return(list(script = script))
}



#' Compile script for defining a units module in Julia
#'
#' @inheritParams compile_Julia
#'
#' @return List with script
#'
compile_units_Julia = function(sfm, keep_unit){

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



#' Compile Julia script for global variables
#'
#' @inheritParams compile_R
#'
#' @return List with macro script
compile_macros_Julia = function(sfm, debug){

  script = ""


  # If there are globals
  if (is_defined(sfm$global$eqn_Julia)){
    # names_df = get_names(sfm)

    script = paste0(script, sfm$global$eqn_Julia)

  }

  # If there are macros
  if (any(nzchar(purrr::map_vec(sfm$macro, "eqn_Julia")))){
      # names_df = get_names(sfm)

      script = paste0(script, "\n",
                      lapply(sfm$macro, function(x){

                        # # If a name is defined, assign macro to that name
                        # if (nzchar(x$name)){
                        #   paste0(x$name, " = ", x$eqn_Julia) %>% return()
                        # } else {
                          x$eqn_Julia %>% return()
                        # }

                      }) %>% unlist() %>% paste0(collapse = "\n"))

  }

  if (nzchar(script)){
    script = paste0("\n\n# User-specified macros\n", script, "\n\n")
  }

  return(list(script = script))
}






#' Compile Julia script for creating time vector
#'
#' @return List
#' @importFrom rlang .data
#'
#' @inheritParams compile_Julia
#'
compile_times_Julia = function(sfm, keep_unit){

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



#' Compile Julia script for static variables, i.e. initial conditions, functions, and parameters
#'
#' @inheritParams compile_Julia
#' @inheritParams order_equations
#' @param ordering List with order of static and dynamic variables, output of order_equations()
#'
#' @return List with necessary scripts
#'
compile_static_eqn_Julia = function(sfm, ordering, keep_unit){

  names_df = get_names(sfm)

  # Graphical functions
  gf_eqn = lapply(sfm$model$variables$gf,
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

        sprintf("%s = itp(%s, %s, method = \"%s\", extrapolation = \"%s\")",
                x$name, xpts_str, ypts_str, x$interpolation, x$extrapolation) %>% return()

        # extrapolation = 1: return NA when outside of bounds
        # extrapolation = 2: return nearest value when outside of bounds
        # } #else if (x$interpolation == "cubic_spline"){
        # #   # **to do: doesn't work yet
        # #   sprintf("%s = cubic_spline_interpolation(x_vals, y_vals,extrapolation_bc=%s)",
        # #           y, xpts_str, ypts_str, rule_str) %>% return()
        # # }
      }
    })

  # Constant equations
  constant_eqn = lapply(sfm$model$variables$constant,
    function(x){

      if (keep_unit & is_defined(x$units) & x$units != "1"){
        paste0(x$name, " = ", P$setunit, "(", x$eqn_Julia, ", u\"", x$units, "\")") %>% return()
      } else {
        paste0(x$name, " = ", x$eqn_Julia) %>% return()
      }
    })

  # Initial states of Stocks
  stock_eqn = lapply(sfm$model$variables$stock,
                     function(x){
      # if (!is.null(x$delayN)){
      #   sprintf("# Initialize delay accumulator for %s\n%s = c(%s, %s)",
      #           y,
      #           P$initial_value_name, P$initial_value_name, x$eqn_Julia) %>% return()
      # } else {

      if (keep_unit & is_defined(x$units) & x$units != "1"){
        paste0(x$name, " = ", P$setunit, "(", x$eqn_Julia, ", u\"", x$units, "\")") %>% return()
      } else {
        paste0(x$name, " = ", x$eqn_Julia) %>% return()
      }
      # }
    })

  # dependencies_dict = sfm$variables[c("gf", "constant", "stock")] %>% unname() %>%
  #   purrr::map_depth(2, "deep_dependencies") %>% purrr::list_flatten()
  # order_idxs = topological_sort(dependencies_dict)
  # order_idxs



  # Compile and order static equations
  static_eqn_str = c(gf_eqn, constant_eqn, stock_eqn)[ordering$static$order] %>%
    unlist() %>%
    paste0(collapse = "\n")
  static_eqn_str

  # Put parameters together in named tuple
  if (length(sfm$model$variables$constant) > 0){
    pars_def = paste0("\n\n# Define parameters in named tuple\n", P$parameter_name, " = (", paste0(names(constant_eqn), " = ", names(constant_eqn), collapse = ", "), ",)\n")
  } else {
    pars_def = paste0("\n\n# Define empty parameters\n", P$parameter_name, " = ()\n")
  }

  # Check for delayN() and smoothN() functions
  delay_func = get_delayN_smoothN(sfm)

  if (length(delay_func) > 0){
    # delay_names = gsub(paste0(P$delayN_suffix, "[0-9]+$"), "", names(delay_func))
    delay_names = names(delay_func)

    xstart_def_stocks = paste0(
      paste0(setdiff(names(stock_eqn), delay_names), collapse = ", "), ", ",
      paste0(paste0("values(", delay_names, ")"), collapse = ", "))
    xstart_names = paste0(
      paste0(paste0("\"", setdiff(names(stock_eqn), delay_names), "\""), collapse = ", "), ", ",
      paste0(paste0("keys(", delay_names, ")..."), collapse = ", ")
    )

  } else {
    xstart_def_stocks = paste0(names(stock_eqn), collapse = ", ")
    xstart_names = paste0(paste0("\"", names(stock_eqn), "\""), collapse = ", ")
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

  return(list(par_names = names(constant_eqn),
              script = paste0(
    "\n\n# Define parameters, initial conditions, and functions in correct order\n",
                              static_eqn_str,
    pars_def, xstart_def, xstart_names
  )))
}



#' Prepare for summing change in stocks in stock-and-flow model in Julia script
#'
#' @inheritParams compile_Julia
#'
#' @return Updated stock-and-flow model
#'
prep_stock_change_Julia = function(sfm, keep_unit){

  # Add temporary property to sum change in Stocks
  stock_names = names(sfm$model$variables$stock)
  sfm$model$variables$stock = lapply(sfm$model$variables$stock,
                                     function(x){

      # if (!is.null(x$delayN) & is_defined(x$inflow)){
      #   # return(NULL)
      #
      #   x$sum_name = paste0(x$inflow, "$update")
      #   x$sum_eqn = ""
      # } else {

        inflow = outflow = ""

        if (x$type == "delayN"){
          regex_find_idx = paste0("findall(n -> occursin(r\"", x$name, P$delay_acc_suffix, "[0-9]+$\", n), ", P$initial_value_names, ")")
          x$sum_name = paste0("d", P$state_name, "dt[", regex_find_idx, "]")
          x$unpack_state = paste0(P$state_name, "[", regex_find_idx, "]")
        } else {
          x$sum_name = paste0("d", P$state_name, "dt[", match(x$name, stock_names), "]")
        }


        # In case no inflow and no outflow is defined, update with 0
        if (!is_defined(x$inflow) & !is_defined(x$outflow)){
          # sprintf("%s%s = 0 # Collect Flows of %s", P$change_prefix, y, y) %>% return()


          # If keep_unit = TRUE, flows always need to have units as the times variable has units
          if (keep_unit){
            # x$sum_eqn = paste0(P$setunit, "(0.0, u\"", x$units, "/", sfm$sim_specs$time_units, "\")")

            # Safer: in case x evaluates to a unit but no units were set
            x$sum_eqn = paste0(P$setunit, "(0.0, Unitful.unit.(", x$name, ")/", P$time_units_name, ")")

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
          # x$sum_eqn = paste0(P$setunit, "(", x$sum_eqn, ", u\"", x$units, "/", sfm$sim_specs$time_units, "\")")
          x$sum_eqn = paste0(P$setunit, "(", x$sum_eqn, ", Unitful.unit.(", x$name, ")/", P$time_units_name, ")")

        }
      # }
      return(x)

    }) %>% purrr::compact()

  # sfm = validate_xmile(sfm)

  return(sfm)
}





#' Compile Julia script for ODE function
#'
#' @inheritParams build
#' @inheritParams compile
#' @inheritParams order_equations
#' @inheritParams compile_static_eqn
#' @param prep_script Intermediate output of compile_Julia()
#' @param constraints Intermediate output of compile_constraints_Julia()
#' @param static_eqn Output of compile_static_eqn()
#'
#' @return List
#' @importFrom rlang .data
#'
compile_ode_Julia = function(sfm, ordering, prep_script, static_eqn,
                             constraints,
                             keep_nonnegative_flow, keep_nonnegative_stock, keep_unit,
                             only_stocks){

  # Auxiliary equations (dynamic auxiliaries)
  aux_eqn = lapply(sfm$model$variables$aux,
                   function(x){

      if (keep_unit & is_defined(x$units) & x$units != "1"){
        out = paste0(x$name, " = ", P$setunit, "(", x$eqn_Julia, ", u\"", x$units, "\")") %>% return()
      } else {
        out = paste0(x$name, " = ", x$eqn_Julia)
      }
      # }

      if (!is.null(x$preceding_eqn)){
        out = c(x$preceding_eqn, out)
      }
      return(out)
    })

  # Flow equations
  # names_df = get_names(sfm)
  flow_df = get_flow_df(sfm)
  flow_eqn = lapply(sfm$model$variables$flow,
                    function(x){

      # ** Don't add this here! If setting units on flows, other variables may do something with the flows. Only set units on sum of flows in compile_stocks()
      # # If keep_unit = TRUE, flows always need to have units as the times variable has units
      # if (keep_unit & x$units == "1"){
      #   connected_stocks = flow_df[flow_df$name == x$name, ]
      #   connected_stocks = Filter(nzchar, c(connected_stocks$from, connected_stocks$to))
      #   stock_units = names_df[match(connected_stocks, names_df$name), "units"]
      #   stock_unit = unique(stock_units)[1]
      #   if (length(unique(stock_units)) > 1){
      #     message(paste0("Flow ", x$name, " is connected to stocks with different units: ",
      #                    paste0(paste0(connected_stocks, " (unit: ", stock_units, ")"), collapse = ", "),
      #                    "\nSetting unit of ", x$name, " to '", stock_unit, "'..."))
      #   }
      #   x$units = paste0(stock_unit, "/", sfm$sim_specs$time_units)
      # }

      out = sprintf("\n\t# Flow%s%s\n\t%s = %s%s%s%s%s",
                    # Add comment
                    ifelse(is_defined(x$from), paste0(" from ", x$from), ""),
                    ifelse(is_defined(x$to), paste0(" to ", x$to), ""),
                    x$name,
                    ifelse(keep_unit & x$units != "1", paste0(P$setunit, "("), ""),
                    ifelse(x$non_negative & keep_nonnegative_flow, "nonnegative(", ""),
                    x$eqn_Julia,
                    ifelse(x$non_negative & keep_nonnegative_flow, ")", ""),
                    ifelse(keep_unit & x$units != "1", paste0(", u\"", x$units, "\")"), "")

      )

      if (!is.null(x$preceding_eqn)){
        out = c(x$preceding_eqn, out)
      }
      return(out)
    })

  # Compile and order all dynamic equations
  dynamic_eqn = c(aux_eqn, flow_eqn)[ordering$dynamic$order] %>% unlist()
  dynamic_eqn

  # Compile and order all dynamic equations
  dynamic_eqn_str = dynamic_eqn %>% paste0(collapse = "\n\t")

  # Create separate vector for names of intermediate variables and values, because graphical functions need to be in the intermediate funciton as gf(t), but their name should be gf
  intermediary_var = intermediary_var_values = names(dynamic_eqn)

  # Sum change in Stock equations
  stock_change = lapply(sfm$model$variables$stock,
                        function(x){
      # if (!is.null(x$delayN)){
      #   return(NULL)
      # } else {
      sprintf("%s %s %s", x$sum_name,
              # Broadcast assignment for delayed variables
              ifelse(x$type == "delayN", ".=", "="),
              x$sum_eqn)


      # }
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


  # **what if delayed variable is gf
  # Add fixed delayed and past variables to intermediary_var
  # extra_intermediary_var = unname(sfm$model$variables) %>% purrr::list_flatten() %>%
  #   purrr::map("intermediary") %>% purrr::compact()
  extra_intermediary_var = lapply(sfm$model$variables, function(x){
    lapply(x, `[[`, "intermediary")
  }) %>% unlist() %>% unname()

  if (length(extra_intermediary_var) > 0){

    # Get unique intermediary variables to add
    new_intermediary_var = setdiff(unique(extra_intermediary_var), intermediary_var)

    if (length(new_intermediary_var) > 0){
      intermediary_var = c(intermediary_var, new_intermediary_var)
      intermediary_var_values = c(intermediary_var_values, new_intermediary_var)
    }

  }

  # If delayN() and smoothN() were used, state has to be unpacked differently
  delay_func = get_delayN_smoothN(sfm)

  if (length(delay_func) > 0){

    # delay_names = gsub(paste0(P$delayN_suffix, "[0-9]+$"), "", names(delay_func))
    delay_names = names(delay_func)

    intermediary_var = setdiff(intermediary_var, delay_names)
    intermediary_var_values = setdiff(intermediary_var_values, delay_names)

    # Unpack non delayN stocks
    unpack_nondelayN = paste0(paste0(setdiff(names(stock_change), delay_names), collapse = ", "), ", = ", P$state_name, "[findall(n -> !occursin(r\"", P$delayN_suffix, "[0-9]+", P$delay_acc_suffix, "[0-9]+$\", n), ", P$initial_value_names, ")]")

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



  # if (keep_unit){
  #   names_df = get_names(sfm)
  #
  #   # If any Stock has a unit
  #   stock_units = names_df[names_df$type == "stock", "units"]
  #   if (any(nzchar(stock_units) & stock_units != "1")){
  #     S_str = sprintf("# Set units on Stocks\n\t%s = Map(set_units, as.list(%s), %s)", P$state_name, P$state_name, P$stock_units_name )
  #   } else {
  #     S_str = sprintf("%s = as.list(%s)", P$state_name, P$state_name)
  #
  #   }
  #
  #   get_var_str = ""
  #
  #   new_var_str = P$ODE_var_name
  # } else {
  #   S_str = sprintf("%s = as.list(%s)", P$state_name, P$state_name)
  #
  #   get_var_str = sprintf("# Get names of variables in environment
  #   %s <- names(environment())", P$env_var_name)
  #
  #   new_var_str = sprintf("setdiff(names(environment()), c('%s', %s))", P$env_var_name, P$env_var_name)
  # }

  # Compile
  script_ode = paste0(
    sprintf("\n\n# Define ODE
function %s!(d%sdt, %s%s, %s)",
            P$ode_func_name,
            P$state_name, P$state_name,
            paste0(", ", P$parameter_name), P$time_name),
    "\n\n\t# Unpack state variables\n\t",
    unpack_state_str,
    ifelse(length(sfm$model$variables$constant) > 0,
           paste0("\n\n\t# Unpack parameters\n\t",
                  paste0(paste0(static_eqn$par_names, collapse = ", "), ", = ", P$parameter_name)), ""),
    "\n\n\t# Update auxiliaries\n\t",
    dynamic_eqn_str,
    constraints$script,
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
      P$intermediary_names, " = [", paste0("\"", intermediary_var, "\"", collapse = ", "), "]\n",
      P$intermediaries, " = SavedValues(",
      # Make time a Unitful.Quantity if keeping units, otherwise a float
      ifelse(keep_unit, "Unitful.Quantity", "Float64"), ", Any)\n", P$callback_name, " = SavingCallback(", P$callback_func_name, ", ", P$intermediaries,
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
#' @param nonneg_stocks Output of compile_nonneg_stocks()
#' @inheritParams compile_ode_Julia
#' @inheritParams compile_Julia
#' @inheritParams order_equations
#'
#' @return List
#' @inheritParams compile_R
#'
compile_run_ode_Julia = function(sfm,
                                     nonneg_stocks,
                                     only_stocks, keep_unit){

  state_var = names(sfm$model$variables$stock)

  script = paste0("\n\n# Run ODE\n",
                  P$prob_name, " = ODEProblem(", P$ode_func_name, "!, ", P$initial_value_name, ", ", P$times_name, ", ", P$parameter_name, ")\n", P$sim_df_name, " = solve(", P$prob_name, ", ",
                  dplyr::case_when(sfm$sim_specs$method == "euler" ~ "Euler()",
                                   sfm$sim_specs$method == "rk4" ~ "RK4()"),
                  ifelse(sfm$sim_specs$method %in% c("euler", "rk4"),
                         paste0(", dt = ", P$timestep_name, ", adaptive=", ifelse(sfm$sim_specs$adaptive, "true", "false")), ""),
                  ifelse(!only_stocks, paste0(", ", P$callback_name, " = ", P$callback_name
                                          # ", saveat = ", sfm$sim_specs$saveat,
                                          # ifelse(keep_unit, paste0(" * ", P$time_units_name), "")

                  ), ""), ")\n",
                  P$sim_df_name, " = DataFrame([
	:time => ", P$sim_df_name, ".t,\n\t[Symbol(name) => ", P$sim_df_name, "[i, :] for (i, name) in enumerate(", P$initial_value_names, ")]...\n])\n"


  )

  if (!only_stocks){
    script = paste0(script,
                    "\n\n# Create DataFrame for intermediary variables
if isempty(", P$intermediaries, ".saveval)
	", P$intermediary_df, " = DataFrame(time = ", P$intermediaries, ".t)\nelse\n\t# Handle single or multiple variables\n\t",

                    "if length(", P$intermediary_names, ") == 1\n\t\t# Single variable case\n\t\t", P$intermediary_df,
                    " = DataFrame(hcat([val[1] for val in ", P$intermediaries, ".saveval]), Symbol.(", P$intermediary_names, "))\n\telse\n\t\t",
                    P$intermediary_df, " = DataFrame(", P$intermediaries, ".saveval, Symbol.(", P$intermediary_names, "))\n\tend\n\t",
                    P$intermediary_df, "[!, :time] = ", P$intermediaries, ".t",

                    # "n_vars = length(", P$intermediary_names, ")\n\tvals = n_vars == 1 ? hcat([val[1] for val in ", P$intermediaries, ".saveval]) : hcat(", P$intermediaries, ".saveval...)\n\t", P$intermediary_df, " = DataFrame(vals, Symbol.(", P$intermediary_names, "))\n\t", P$intermediary_df, "[!, :time] = ", P$intermediaries, ".t",
                    "\nend\n\n\n# Select intermediary columns not in df (except time)
cols_to_keep = [Symbol(name) for name in ", P$intermediary_names, " if !(name in ", P$initial_value_names, ")]\npush!(cols_to_keep, :time)\n",
                    P$intermediary_df, " = select(", P$intermediary_df, ", cols_to_keep)

# Combine DataFrames\n", P$sim_df_name, " = innerjoin(", P$sim_df_name, ", ", P$intermediary_df, ", on = :time)\n")
  }

  # script = paste0(script, sprintf("\n\n# Overwrite initial values to named dictionary and strip units\n%s = Dict(%s .=> Unitful.ustrip.(%s))\n", P$initial_value_name, P$initial_value_names, P$initial_value_name),
  #                 sprintf("\n# Strip units from parameters\n%s = (; (name => isa(val, Unitful.Quantity) ? ustrip(val) : val for (name, val) in pairs(%s))...)", P$parameter_name, P$parameter_name), "\n")


  # If different times need to be saved, linearly interpolate
  if (sfm$sim_specs$dt != sfm$sim_specs$saveat){
    script = paste0(script, "\n# Linearly interpolate to reduce stored values to saveat
new_times = collect(", P$times_name, "[1]:",
                    ifelse(keep_unit, paste0("u\"", sfm$sim_specs$saveat, sfm$sim_specs$time_units, "\""),
                           sfm$sim_specs$saveat), ":", P$times_name, "[2]) # Create new time vector\n",

                    P$sim_df_name, " = DataFrame(Dict(\n",
                    ":time => new_times,
    [Symbol(col) => ", P$saveat_func, "(", P$sim_df_name, ".time, ", P$sim_df_name, "[!, col], new_times) for col in names(", P$sim_df_name, ") if col != \"time\"]...
))\n")

  }


  if (keep_unit){
    script = paste0(script,
                    "\n\n# Extract and store units\n",
                    P$units_dict, " = Dict(name => string(unit(val)) for (name, val) in pairs(first(", P$sim_df_name,
                    ")) if isa(val, Unitful.Quantity))\n",
                    "\n\n# Strip units from DataFrame\n", P$sim_df_name, " = Unitful.ustrip.(", P$sim_df_name, ")\n",
                    "\n\n# Strip units from initial conditions and parameters\n",
                    P$initial_value_name, " = Dict(xstart_names .=> Unitful.ustrip.(xstart))\n",
                    P$parameter_name, " = (; (name => isa(val, Unitful.Quantity) ? Unitful.ustrip(val) : val for (name, val) in pairs(pars))...)")

  } else {
    script = paste0(script, "\n# Assign empty dictionary to units\n", P$units_dict, " = Dict()\n")
  }


  return(list(script=script))
}






#' Write a string to a R or Julia script file with a unique filename
#'
#' @param script String containing the code to write
#' @param base_name Base name for the file (without extension, default: "script")
#' @param dir Directory to save the file (default: current working directory)
#' @param ext String with file extension, either ".R" or ".jl"
#' @param overwrite Boolean; whether to overwrite the file if it does exist
#' @return The path to the created file
#'
#' @export
#' @examples
#' julia_code <- "println(\"Hello from Julia!\")"
#' filepath <- write_script(julia_code, "my_script", ext = ".jl")
#' print(filepath)
write_script <- function(script,
                         base_name = "model",
                         dir = file.path(tempdir(), "Julia_output"),
                         ext = ".jl",
                         overwrite = FALSE) {

  # Ensure directory exists
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }

  # Initial filename
  filepath <- file.path(dir, paste0(base_name, ext))

  # Check if file exists and generate a unique name
  counter <- 1
  while (file.exists(filepath) & !overwrite) {
    filepath <- file.path(dir, paste0(base_name, "_", counter, ext))
    counter <- counter + 1
  }

  # Decode unicode characters when writing to Julia
  if (ext == ".jl"){
    if (grepl("(\\\\u|\\\\\\\\u)[0-9a-fA-F]{4}", script)){
      script = decode_unicode(script)
    }
  }

  # Write the script to the file
  writeLines(script, filepath)

  # Return the file path
  return(filepath)
}


#' Decode unicode characters in a string
#'
#' @param text String containing unicode escape sequences (e.g., "\\uXXXX")
#'
#' @returns String with unicode characters decoded
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


#' Customary functions written in Julia
#'
#' @return String with Julia code
#' @export
#'
get_func_Julia = function(){
  func_def = c(
#   "itp"= "# Extrapolation function\nfunction itp(x, y; method = \"linear\", extrapolation = 2)
#
#     # Ensure y is sorted along x
#     idx = sortperm(x)
#     x = x[idx]
#     y = y[idx]
#
#     # Extrapolation extrapolation: What happens outside of defined values?
#     # Rule 1: return NaN; Rule 2: return closest value
#     rule_method = ifelse(extrapolation == 1, NaN, ifelse(extrapolation == 2, Flat(), \"?\"))
#
#     if method == \"constant\"
#         func = extrapolate(interpolate((x,), y, Gridded(Constant{Previous}())), rule_method)
#     elseif method == \"linear\"
#         func = linear_interpolation(x, y, extrapolation_bc=rule_method)
#     end
#
#     return(func)
# end",
    # extrapolation = 1: return NA when outside of bounds
    # extrapolation = 2: return nearest value when outside of bounds
      "itp"= "# Extrapolation function\nfunction itp(x, y; method = \"linear\", extrapolation = \"nearest\")

  # Ensure y is sorted along x
  idx = sortperm(x)
  x = x[idx]
  y = y[idx]

  # Extrapolation rule: What happens outside of defined values?
  # Rule \"NA\": return NaN; Rule \"nearest\": return closest value
  rule_method = ifelse(extrapolation == \"NA\", DataInterpolations.ExtrapolationType.None, ifelse(extrapolation == \"nearest\", DataInterpolations.ExtrapolationType.Constant, extrapolation))

  if method == \"constant\"
      func = DataInterpolations.ConstantInterpolation(y, x; extrapolation = rule_method) # notice order of x and y
  elseif method == \"linear\"
      func = DataInterpolations.LinearInterpolation(y, x; extrapolation = rule_method)
  end

  return(func)
end",
      # **to do: ExtrapolationType.None throws error instead of returning NaN

#                "ramp" = "# Make ramp signal\nfunction ramp(times; start_t_ramp, end_t_ramp, start_h_ramp = 0.0, end_h_ramp = 1.0)
#
#     x = [start_t_ramp, end_t_ramp]
#     y = [start_h_ramp, end_h_ramp]
#
#     # If the ramp is after the start time, add a zero at the start
#     if start_t_ramp > first(times)
#         x = [first(times); x]
#         y = [0; y]
#     end
#
#     func = itp(x, y, method = \"linear\", extrapolation = 2)
#
#     return(func)
# end",
"ramp" = "function ramp(; start_t_ramp, end_t_ramp, start_h_ramp = 0.0, end_h_ramp = 1.0)
    # If times has units, but the ramp times don't, convert them to the same units
    if eltype(times) <: Unitful.Quantity
        if !(eltype(start_t_ramp) <: Unitful.Quantity)
            start_t_ramp = setunit(start_t_ramp, time_units)
        end
        if !(eltype(end_t_ramp) <: Unitful.Quantity)
            end_t_ramp = setunit(end_t_ramp, time_units)
        end
    else
        # If times does not have units, but start_t_ramp does, convert the ramp times to the same units as time_units
        if eltype(start_t_ramp) <: Unitful.Quantity
            start_t_ramp = Unitful.ustrip(setunit(start_t_ramp, time_units))
        end
        if eltype(end_t_ramp) <: Unitful.Quantity
            end_t_ramp = Unitful.ustrip(setunit(end_t_ramp, time_units))
        end
    end


    # Ensure start_h_ramp and end_h_ramp are both of the same type
    if eltype(start_h_ramp) <: Unitful.Quantity && !(eltype(end_h_ramp) <: Unitful.Quantity)
        end_h_ramp = setunit(end_h_ramp, Unitful.unit(start_h_ramp))
        add_y = setunit(0.0, Unitful.unit(start_h_ramp))
    elseif !(eltype(start_h_ramp) <: Unitful.Quantity) && eltype(end_h_ramp) <: Unitful.Quantity
        start_h_ramp = setunit(start_h_ramp, Unitful.unit(end_h_ramp))
        add_y = setunit(0.0, Unitful.unit(end_h_ramp))
    else
        add_y = 0.0
    end

    x = [start_t_ramp, end_t_ramp]
    y = [start_h_ramp, end_h_ramp]

    # If the ramp is after the start time, add a zero at the start
    if start_t_ramp > first(times)
        x = [first(times); x]
        y = [add_y; y]
    end

    func = itp(x, y, method = \"linear\", extrapolation = \"nearest\")

    return(func)
end ",
#                "step" = "# Make step signal\nfunction step(times; start_t_step, h_step = 1.0)
#
#     x = [start_t_step, times[2]]
#     y = [h_step, h_step]
#
#     # If the step is after the start time, add a zero at the start
#     if start_t_step > first(times)
#         x = [first(times); x]
#         y = [0; y]
#     end
#
#     func = itp(x, y, method = \"constant\", extrapolation = \"nearest\")
#
#     return(func)
# end",
"step" = "# Make step signal
function step(; start_t_step, h_step = 1.0)

    # If times has units, but the ramp times don't, convert them to the same units
    if eltype(times) <: Unitful.Quantity
        if !(eltype(start_t_step) <: Unitful.Quantity)
            start_t_step = setunit(start_t_step, time_units)
        end
    else
        # If times does not have units, but start_t_step does, convert the ramp times to the same units as time_units
        if eltype(start_t_step) <: Unitful.Quantity
            start_t_step = Unitful.ustrip(setunit(start_t_step, time_units))
        end
    end

    if eltype(h_step) <: Unitful.Quantity
      add_y = setunit(0.0, Unitful.unit(h_step))
    else
      add_y = 0.0
    end

    x = [start_t_step, times[2]]
    y = [h_step, h_step]

    # If the step is after the start time, add a zero at the start
    if start_t_step > first(times)
        x = [first(times); x]
        y = [add_y; y]
    end

    func = itp(x, y, method = \"constant\", extrapolation = \"nearest\")

    return(func)
end",
"@constraints" = "# Constraints macro
macro constraints(exprs...)
    pairs = map(exprs) do ex
        quote
            ($(string(ex)), $(esc(ex)))
        end
    end
    quote
        [$(pairs...)]
    end
end",
#                "pulse" = "# Make pulse signal\nfunction pulse(times; start_t_pulse, h_pulse = 1.0, w_pulse = 1.0 * time_units, repeat_interval = nothing)
#
#     # Define start and end times of pulses
#     last_time = last(times)
#     # If no repeats, set end of pulse to after end time
#     step_size = isnothing(repeat_interval) ? last_time * 2 : repeat_interval
#     start_ts = collect(start_t_pulse:step_size:last_time)
#     end_ts = start_ts .+ w_pulse
#
#     # Build signal as vectors of times and y-values
#     signal_times = [start_ts; end_ts]
#     signal_y = [fill(h_pulse, length(start_ts)); fill(0, length(end_ts))]
#
#     # If the first pulse is after the start time, add a zero at the start
#     if minimum(start_ts) > first(times)
#         signal_times = [first(times); signal_times]
#         signal_y = [0; signal_y]
#     end
#
#     # If the last pulse doesn't cover the end, add a zero at the end
#     # (I don't fully understand why this is necessary, but otherwise it gives incorrect results with repeat_interval <= 0)
#     if maximum(end_ts) < last_time
#         signal_times = [signal_times; last_time]
#         signal_y = [signal_y; 0]
#     end
#
#     # Sort by time
#     perm = sortperm(signal_times)
#     x = signal_times[perm]
#     y = signal_y[perm]
#     func = itp(x, y, method = \"constant\", extrapolation = \"nearest\")
#
#     return(func)
# end",
"pulse" = "# Make pulse signal
function pulse(; start_t_pulse, h_pulse = 1.0, w_pulse = 1.0 * time_units, repeat_interval = nothing)
    # If times has units, but the pulse times don't, convert them to the same units
    if eltype(times) <: Unitful.Quantity
        if !(eltype(start_t_pulse) <: Unitful.Quantity)
            start_t_pulse = setunit(start_t_pulse, time_units)
        end
        if !(eltype(w_pulse) <: Unitful.Quantity)
            w_pulse = setunit(w_pulse, time_units)
        end
        if (!isnothing(repeat_interval) && !(eltype(repeat_interval) <: Unitful.Quantity))
            repeat_interval = setunit(repeat_interval, time_units)
        end
    else
        # If times does not have units, but start_t_pulse does, convert the pulse times to the same units as time_units
        if eltype(start_t_pulse) <: Unitful.Quantity
            start_t_pulse = Unitful.ustrip(setunit(start_t_pulse, time_units))
        end
        if eltype(w_pulse) <: Unitful.Quantity
            w_pulse = Unitful.ustrip(setunit(w_pulse, time_units))
        end
        if (!isnothing(repeat_interval) && eltype(repeat_interval) <: Unitful.Quantity)
            repeat_interval = Unitful.ustrip(setunit(repeat_interval, time_units))
        end
    end

    # Define start and end times of pulses
    last_time = last(times)
    # If no repeats, set end of pulse to after end time
    step_size = isnothing(repeat_interval) ? last_time * 2 : repeat_interval
    start_ts = collect(start_t_pulse:step_size:last_time)
    end_ts = start_ts .+ w_pulse

    # Build signal as vectors of times and y-values
    signal_times = [start_ts; end_ts]
    signal_y = [fill(h_pulse, length(start_ts)); fill(0, length(end_ts))]

    if eltype(h_pulse) <: Unitful.Quantity
      add_y = setunit(0.0, Unitful.unit(h_pulse))
    else
      add_y = 0.0
    end

    # If the first pulse is after the start time, add a zero at the start
    if minimum(start_ts) > first(times)
        signal_times = [first(times); signal_times]
        signal_y = [add_y; signal_y]
    end

    # If the last pulse doesn't cover the end, add a zero at the end
    # (I don't fully understand why this is necessary, but otherwise it gives incorrect results with repeat_interval <= 0)
    if maximum(end_ts) < last_time
        signal_times = [signal_times; last_time]
        signal_y = [signal_y; add_y]
    end

    # Sort by time
    perm = sortperm(signal_times)
    x = signal_times[perm]
    y = signal_y[perm]
    func = itp(x, y, method = \"constant\", extrapolation = \"nearest\")

    return(func)
end",
               # ** constant interpolation is not supported with units! linear is

               # ** other custom_func
"IM_round" = "# Convert InsightMaker's Round() function to R\n# Difference: in Insight Maker, Round(.5) = 1; in R, round(.5) = 0; in Julia, round(.5) = 0.0\nfunction IM_round(x::Real, digits::Int=0)
    # Compute the fractional part after scaling by 10^digits
    scaled_x = x * 10.0^digits
    frac = scaled_x % 1
    # Check if fractional part is exactly 0.5 or -0.5
    if abs(frac) == 0.5
        return ceil(scaled_x) / 10.0^digits
    else
        return round(scaled_x, digits=0) / 10.0^digits
    end
end",
               "logit" = "# Logit function\nfunction logit(p)
    return log(p / (1 - p))
end",
               "expit" = "# Expit function\nfunction expit(x)
    return 1 / (1+exp(-x))
end",
"sigmoid" = "function sigmoid(x; slope=1, midpoint=0)
    @assert isfinite(slope) && isfinite(midpoint) \"slope and midpoint must be numeric\"
    1 / (1 + exp(-slope * (x - midpoint)))
end

# Also define without keyword arguments
function sigmoid(x, slope, midpoint)
    @assert isfinite(slope) && isfinite(midpoint) \"slope and midpoint must be numeric\"
    1 / (1 + exp(-slope * (x - midpoint)))
end
",
               "nonnegative" = "# Prevent non-negativity\nfunction nonnegative(x)
    if eltype(x) <: Unitful.Quantity
        max.(0.0, Unitful.ustrip(x)) .* Unitful.unit.(x)
    else
        max.(0.0, x)
    end
end",
"rbool" = "# Generate random boolean value, equivalent of RandBoolean() in InsightMaker\nfunction rbool(p)
    return rand() < p
end",
#                "rdist" = "function rdist(a,b)
#     StatsBase.wsample(a, b, 1)
# end",
"rdist" = "function rdist(a::Vector{T}, b::Vector{<:Real}) where T
    # Check lengths match
    if length(a) != length(b)
        throw(ArgumentError(\"Length of a and b must match\"))
    end
    # Normalize probabilities
    b_sum = sum(b)
    if b_sum <= 0
        throw(ArgumentError(\"Sum of probabilities must be positive\"))
    end
    b_normalized = b / b_sum
    # Sample using Categorical
    return a[rand(Distributions.Categorical(b_normalized))]
end",
"indexof" = "function indexof(haystack, needle)
    if isa(haystack, AbstractString) && isa(needle, AbstractString)
        pos = findfirst(needle, haystack)
        return isnothing(pos) ? 0 : first(pos)
    else
        pos = findfirst(==(needle), haystack)
        return isnothing(pos) ? 0 : pos
    end
end",
# "IM_length"= "function IM_length(x)
#     if isa(x, AbstractString)
#         return length(x)
#     else
#         return length(x)
#     end
# end",
"IM_contains" = "function IM_contains(haystack, needle)
    if isa(haystack, AbstractString) && isa(needle, AbstractString)
        return occursin(needle, haystack)
    else
        return needle in haystack
    end
end",
"substr_i" = "function substr_i(string::AbstractString, idxs::Union{Int, Vector{Int}})
    chars = collect(string)
    return join(chars[idxs])
end",
"IM_filter" = "function IM_filter(y::Vector{T}, condition_func::Function) where T
    names_y = string.(1:length(y))
    result = Dict{String,T}()
    for (key, val) in zip(names_y, y)
        if condition_func(val, key)
            result[key] = val
        end
    end
    return collect(values(result)), collect(keys(result))
end",

#                "weeks" = "function weeks(t, time_units)
#     Unitful.uconvert(u\"wk\", t * Unitful.uparse(time_units))  # Parse the string dynamically
# end",
#                "setunit" = "# Set or convert unit\nfunction setunit(x, unit_def)
#
#     # In case unit_def is not identified as a unit, extract unit
#     if eltype(unit_def) <: Unitful.Quantity
#         unit_def = Unitful.unit(unit_def)  # Extract the unit from a Quantity
#     end
#
#     # If x already has a unit, convert
#     if eltype(x) <: Unitful.Quantity
#         try
#             return Unitful.uconvert.(unit_def, x)
#         catch e
#             if isa(e, Unitful.DimensionError)
#                 error(\"Cannot convert $(unit(x)) to $unit_def: incompatible dimensions\")
#             else
#                 rethrow(e)
#             end
#         end
#     else
#         return x .* unit_def
#     end
# end
# ",
"setunit" = sprintf("# Set or convert unit\nfunction setunit(x, unit_def)
    # Parse unit_def into a Unitful.Unit
    target_unit = if unit_def isa Unitful.Quantity
        Unitful.unit(unit_def)  # Extract unit from Quantity (e.g., 1u\"wk\" -> u\"wk\")
    elseif unit_def isa Unitful.Units
        unit_def  # Already a unit (e.g., u\"wk\")
    elseif unit_def isa AbstractString
        try
            Unitful.uparse(unit_def, unit_context = %s)  # Parse string to unit (e.g., \"wk\" -> u\"wk\")
        catch e
            error(\"Invalid unit string: $unit_def\")
        end
    else
        error(\"unit_def must be a Quantity, Unit, or String, got $(typeof(unit_def))\")
    end

    # Handle x based on whether it has a unit
    #if x isa Unitful.Quantity
    if eltype(x) <: Unitful.Quantity
        # x has a unit, convert it
        try
            return Unitful.uconvert.(target_unit, x)
        catch e
            if isa(e, Unitful.DimensionError)
                error(\"Cannot convert $(Unitful.unit.(x)) to $target_unit: incompatible dimensions\")
            else
                rethrow(e)
            end
        end
    else
        # x has no unit, set it via multiplication
        return x .* target_unit
    end
end", P$unit_context),
# "seasonal" = "# Create seasonal wave\nfunction seasonal(;wave_unit=u\"yr\", wave_peak=0u\"yr\")
#     (t, u=wave_unit, p=wave_peak) -> cos.(Unitful.ustrip.(Unitful.uconvert.(u, t - p)))
# end"
"seasonal" = "# Create seasonal wave\nfunction seasonal(t; period = u\"1yr\", shift = u\"0yr\")
    phase = 2 * pi * (t - shift) / period  # π radians
    return(cos(phase))
end",
"\\u2295" = "# Define the operator \\u2295 for the modulus
function \\u2295(x, y)
    return mod(x, y)
end",
# "retrieve_past"= "function retrieve_past(var_value, delay_time, default_value, t, var_name, single_or_interval, intermediaries, intermediary_names)
#
# 	# Ensure t and delay_time are of the same type
# 	if !(eltype(delay_time) <: Unitful.Quantity) & (eltype(t) <: Unitful.Quantity)
# 		delay_time = setunit(delay_time, t)
# 	end
#
#     # Extract single value from the past
# 	if single_or_interval == \"single\"
# 		extract_t = t - delay_time
#
# 		# If trying to retrieve a value in the \"past\" (i.e. before times[1]), use default value if specified, otherwise use value at first time point
# 		if extract_t < 0
# 			if isnothing(default_value)
#                 if isempty(intermediaries.saveval)
#                     return var_value
#                 else
#                     return intermediaries.saveval[1][findfirst(isequal(var_name), intermediary_names)]
#                 end
# 			else
# 				return default_value
# 			end
# 		elseif extract_t == 0 || extract_t == t
#       return var_value
# 		end
#
# 	# Or: extract interval of past values
# 	elseif single_or_interval == \"interval\"
#
#         # Check if intermediaries.saveval is empty
#         if isempty(intermediaries.saveval)
#             return var_value
#         end
#
#         # Create vector of times
#         if !(t in intermediaries.t)
#             extract_t = [intermediaries.t; t]
#         else
#             extract_t = intermediaries.t
#         end
#
# 		# If no past interval is specified, access entire history
# 		if !isnothing(delay_time)
# 			first_time = t - delay_time
#
# 			if first_time < 0
# 				first_time = intermediaries.t[1]  # Set to start time if negative
# 			end
#
# 			# Extract from first_time up until t
# 			_, first_time_idx = findmin(abs.(extract_t .- first_time))
# 			extract_t = extract_t[first_time_idx:end]
#
#             if length(extract_t) == 1
#                 return var_value
#             end
# 		end
# 	end
#
#     # Check if intermediaries.saveval is empty
#     if isempty(intermediaries.saveval)
#         # If empty, return default value if specified, otherwise return NaN
#         if isnothing(default_value)
#             return var_value
#         else
#             return default_value
#         end
#     end
#
# 	## Create a DataFrame from the saved values
# 	i#ntermediary_df = DataFrame(intermediaries.saveval, Symbol.(intermediary_names))
#
#     # Add t if not in intermediaries.t yet
#     if !(t in intermediaries.t)
#         ts = [intermediaries.t; t]
#     else
#         ts = intermediaries.t
#     end
#
#     #ys = [intermediary_df[!, var_name]; var_value]
#    # ys = ys[1:length(ts)]  # Ensure ys is the same length as ts
#
#     var_index = findfirst(==(var_name), intermediary_names)
#     ys = [[val[var_index] for val in intermediaries.saveval]; var_value][1:length(ts)]
#
# 	# Define interpolation function
#     itp(ts, ys, method = \"linear\", extrapolation = \"nearest\")(extract_t)
#
# end
# ",
"retrieve_past"= "# Function to retrieve past values

function retrieve_past(var_value, delay_time, default_value, t, var_name, single_or_interval, intermediaries, intermediary_names)
    # Handle empty intermediaries
    if isempty(intermediaries.saveval)
        return isnothing(default_value) ? var_value : default_value
    end

    # Ensure t and delay_time have compatible units
    if !(eltype(delay_time) <: Unitful.Quantity) && (eltype(t) <: Unitful.Quantity)
        # delay_time = delay_time * unit(t)
        delay_time = setunit(delay_time, t)
    end

    # Extract variable index
    var_index = findfirst(==(var_name), intermediary_names)
    # if isnothing(var_index)
    #     error(\"Variable '$var_name' not found in intermediary_names: $intermediary_names\")
    # end

    # Extract times and values
    ts = intermediaries.t
    ys = [val[var_index] for val in intermediaries.saveval]

    # Handle current time t
    if !(t in ts)
        ts = [ts; t]
    end

    ys = [ys; var_value][1:length(ts)]  # Ensure ys is the same length as ts

    # Single value extraction
    if single_or_interval == \"single\"
        extract_t = t - delay_time
        if extract_t < ts[1]
            return isnothing(default_value) ? ys[1] : default_value
        elseif extract_t == t
            return var_value
        end

        # Interpolate
        # interp = LinearInterpolation(ys, ts, extrapolation_bc = Flat())
        # Define interpolation function
        return itp(ts, ys, method = \"linear\")(extract_t)

    end

    # Interval extraction
    if single_or_interval == \"interval\"
        if isnothing(delay_time)
            return ys  # Return entire history
        end

        first_time = t - delay_time
        if first_time < ts[1]
            first_time = ts[1]
        end

        # Find indices for interval
        idx = findfirst(t -> t >= first_time, ts)
        if isnothing(idx) || idx == length(ts)
            return [var_value]
        end

        return ys[idx:end]
    end

end
",
"saveat_func" = "# Function to save dataframe at specific times
function saveat_func(t, y, new_times)
    # Interpolate y at new_times
    itp(t, y, method = \"linear\", extrapolation = \"nearest\")(new_times)
end",
# "setunit_flow" = "# Define function to set unit of flow; Alleviate users from dividing the flow in the equation by the time unit if they have specified the desired units in the units property of the flow
# function setunit_flow(x, unit_def)
#     # If trying to set the unit throws an error
#     try
#         return setunit(x, unit_def)
#     catch e
#         if isa(e, ErrorException) | isa(e, Unitful.DimensionError)
#             try
#                 # In cases where e.g. x = 1u\"m\", unit_def = u\"m/d\"
#                 # I want to find the necessary divisor.
#                 div_required = x / unit_def
#                 # if Unitful.unit(x / div_required) == unit_def   # this is always true
#                 # If the unit of the divisor is a time unit, return the flow divided by the divisor
#                 if typeof(Unitful.unit(div_required)) <: Unitful.TimeUnits
#                     return x / div_required
#                 else
#                     error(\"Cannot set unit of flow: $x to $unit_def\")
#                 end
#
#             catch
#                 error(\"Cannot set unit of flow: $x to $unit_def\")
#             end
#         else
#             error(\"Cannot set unit of flow: $x to $unit_def\")
#         end
#     end
# end"

"compute_delayN" = "function compute_delayN(inflow, accumulator, length_delay, order_delay)
    order_delay = round(Int, order_delay)
    d_accumulator = zeros(eltype(accumulator), order_delay)
    exit_rate_stage = accumulator / (length_delay / order_delay)
    d_accumulator[1] = inflow - exit_rate_stage[1]
    if order_delay > 1
        @inbounds for ord in 2:order_delay
            d_accumulator[ord] = exit_rate_stage[ord-1] - exit_rate_stage[ord]
        end
    end
    outflow = exit_rate_stage[order_delay]
    return (outflow=outflow, update=d_accumulator)
end",
"setup_delayN" = "function setup_delayN(initial_value, length_delay, order_delay, name::String)
    # Compute the initial value for each accumulator
    # from https://www.simulistics.com/help/equations/functions/delay.html
    order_delay = round(Int, order_delay) # Turn order into integer
    value = initial_value * length_delay / order_delay

    # Create a dictionary with names like \"name_acc1\", \"name_acc2\", ...
    return Dict(string(name, \"_acc\", i) => value for i in 1:order_delay)
end"

)

  return(func_def)
}




#' Internal function to create initialization file for Julia
#'
#' @return NULL
#'
create_init = function(){


  # Note for extending comparison operators:
  # eltype(5u"m") <: Unitful.Quantity # true
  # eltype(5u"m") <: Number # true
  # eltype(5u"m") <: Real # false
  # eltype(5.0) <: Real # true
  # eltype(5) <: Unitful.Quantity # false
  # so we cannot use x::Number, but have to use x::Real

  # Add standard custom units
  unit_str = lapply(custom_units(),
                    function(x){

      if (is_defined(x$eqn)){
        unit_def = x$eqn
      } else {
        unit_def = "1"
      }

      paste0("@unit ", x$name, " \"", x$name, "\" ", x$name, " u\"", unit_def, "\" ", ifelse(x$prefix, "true", "false"))

    }) %>% paste0(collapse = sprintf("\n\tUnitful.register(%s)\n\t", P$sdbuildR_units))

  unit_str = paste0("\n\n# Define custom units; register after each unit as some units may be defined by other units\nmodule ", P$sdbuildR_units, "\n\tusing Unitful\n\t",
                  unit_str,
                  "\n\tUnitful.register(", P$sdbuildR_units, ")\nend\n\n", P$unit_context, " = [Unitful.Unitful, ", P$sdbuildR_units, "];\n\n")

  script = paste0("# Load packages
using DifferentialEquations#: ODEProblem, solve, Euler, RK4, Tsit5
using DiffEqCallbacks#: SavingCallback, SavedValues
using DataFrames#: DataFrame, select, innerjoin, rename!
using Distributions
using Statistics
using Unitful
using DataInterpolations
using Random

# Julia initialization for sdbuildR package
# Required when extending a module’s function
#import Base: <, >, <=, >=, ==, != #, +, - #, *, /, ^

# Extend base methods (multiple dispatch) to allow for comparison between a unit and a non-unit; if one of the arguments is a Unitful.Quantity, convert the other to the same unit.
Base.:<(x::Unitful.AbstractQuantity, y::Real) = <(x, y * Unitful.unit(x))
Base.:<(x::Real, y::Unitful.AbstractQuantity) = <(x * Unitful.unit(y), y)

Base.:>(x::Unitful.AbstractQuantity, y::Real) = >(x, y * Unitful.unit(x))
Base.:>(x::Real, y::Unitful.AbstractQuantity) = >(x * Unitful.unit(y), y)

Base.:(<=)(x::Unitful.AbstractQuantity, y::Real) = <=(x, y * Unitful.unit(x))
Base.:(<=)(x::Real, y::Unitful.AbstractQuantity) = <=(x * Unitful.unit(y), y)

Base.:(>=)(x::Unitful.AbstractQuantity, y::Real) = >=(x, y * Unitful.unit(x))
Base.:(>=)(x::Real, y::Unitful.AbstractQuantity) = >=(x * Unitful.unit(y), y)

Base.:(==)(x::Unitful.AbstractQuantity, y::Real) = ==(x, y * Unitful.unit(x))
Base.:(==)(x::Real, y::Unitful.AbstractQuantity) = ==(x * Unitful.unit(y), y)

Base.:(!=)(x::Unitful.AbstractQuantity, y::Real) = !=(x, y * Unitful.unit(x))
Base.:(!=)(x::Real, y::Unitful.AbstractQuantity) = !=(x * Unitful.unit(y), y)

Base.:%(x::Unitful.AbstractQuantity, y::Real) = %(x, y * Unitful.unit(x))
Base.:%(x::Real, y::Unitful.AbstractQuantity) = %(x * Unitful.unit(y), y)

Base.mod(x::Unitful.AbstractQuantity, y::Real) = mod(x, y * Unitful.unit(x))
Base.mod(x::Real, y::Unitful.AbstractQuantity) = mod(x * Unitful.unit(y), y)

Base.rem(x::Unitful.AbstractQuantity, y::Real) = rem(x, y * Unitful.unit(x))
Base.rem(x::Real, y::Unitful.AbstractQuantity) = rem(x * Unitful.unit(y), y)

Base.min(x::Unitful.AbstractQuantity, y::Real) = min(x, y * Unitful.unit(x))
Base.min(x::Real, y::Unitful.AbstractQuantity) = min(x * Unitful.unit(y), y)

Base.max(x::Unitful.AbstractQuantity, y::Real) = max(x, y * Unitful.unit(x))
Base.max(x::Real, y::Unitful.AbstractQuantity) = max(x * Unitful.unit(y), y)

# Extend min/max: when applied to a single vector, use minimum, like in R
Base.min(v::AbstractVector) = minimum(v)
Base.max(v::AbstractVector) = maximum(v)

Base.round(x::Unitful.AbstractQuantity) = round(Unitful.ustrip.(x)) * Unitful.unit(x)
Base.floor(x::Unitful.AbstractQuantity) = floor(Unitful.ustrip.(x)) * Unitful.unit(x)
Base.ceil(x::Unitful.AbstractQuantity) = ceil(Unitful.ustrip.(x)) * Unitful.unit(x)
Base.trunc(x::Unitful.AbstractQuantity) = trunc(Unitful.ustrip.(x)) * Unitful.unit(x)
Base.round(x::Unitful.AbstractQuantity, digits::Int) = round(Unitful.ustrip.(x), digits=digits) * Unitful.unit(x)

# # Macro to extend functions to preserve units
# macro unitful_preserve_units(funcs...)
# expr = Expr(:block)
# for f in funcs
# push!(expr.args, quote
#      # Extend the function for Unitful.Quantity
#       function Base.$f(x::Unitful.Quantity)
#       # Strip the unit, apply the function, and reattach the unit
#       val = $f(Unitful.ustrip(x))
#       return val * Unitful.unit(x)
#       end
#
#       # # Handle rounding with a type argument (e.g., round(Int, x))
#       # function Base.$f(::Type{T}, x::Unitful.Quantity) where {T<:Number}
#       #     val = $f(T, ustrip(x))
#       #     return val * unit(x)
#       # end
#       end)
# end
# return esc(expr)
# end
#
# # Apply the Macro to Desired Function
# # Extend to preserve units
# @unitful_preserve_units round floor ceil trunc
",
  paste0(get_func_Julia() %>% unname(), collapse = "\n\n"),

  # Add custom functions
  unit_str,
  # Add initialization of sdbuildR
  paste0("\n\n", P$init_sdbuildR, " = true"),
  collapse = "\n\n")

  # Write script
  env_path <- system.file(package = "sdbuildR")
  filepath = write_script(script, base_name = "init", dir = env_path, ext = ".jl", overwrite = TRUE)
  invisible()
}



#' Set up Julia environment for sdbuildR
#'
#' Create Julia environment to simulate stock-and-flow models. `sdbuildR_setup()` looks for a Julia installation within the sdbuildR package directory, and will install Julia as well as some required packages if not yet found. Keep in mind that the first time running `sdbuildR_setup()` can take around 5-15 minutes, and all subsequent calls can take around 1 minute.
#'
#' @param version Julia version. Default is "latest", which will install the most recent stable release.
#' @param force If TRUE, force Julia installation even if existing version is found. Default is FALSE.
#' @param remove If TRUE, remove Julia installation(s). You will be asked which versions to remove. Default is FALSE.
#' @param ... Optional arguments passed to JuliaCall::julia_setup()
#'
#' @return NULL
#' @export
#'
#' @examples
#' sdbuildR_setup()
sdbuildR_setup <- function(
    version = "latest",
    force = FALSE,
    # update_pkg = FALSE,
    remove = FALSE, ...){

  # Check whether installation of Julia exists in sdbuildR directory
  env_path <- system.file(package = "sdbuildR")
  dirs = file.path(env_path, "julia")

  if (!file.exists(dirs)){
    dir.create(dirs, recursive = TRUE)
    install = TRUE
  } else {
    # Check latest version installed
    installed = list.files(dirs)

    # If no directories were found, install Julia
    if (length(installed) == 0){
      install = TRUE
    } else {

      # Choose latest version
      installed = as.character(max(package_version(installed)))
      julia_dir = file.path(dirs, installed)
      install = FALSE

      # Remove Julia if requested
      if (remove){

        # Ask to verify removal or which versions to remove
        if (length(installed) == 1){
          var1 = readline(prompt = paste0("Found Julia version ", installed , " in sdbuildR directory. Do you want to remove it? (y/n) "))
          if (trimws(tolower(var1)) %in% c("y", "yes")){
            message("Removing Julia version ", installed, " from sdbuildR directory...")
            unlink(julia_dir, recursive = TRUE)
            stop()
          } else {
            message("Keeping Julia version ", installed, " in sdbuildR directory.")
            stop()
          }
        } else {
          message("Found multiple Julia versions in sdbuildR directory. Which do you want to remove?")
          for (i in seq_along(installed)){
            message(i, ": ", installed[i])
          }
          message(i + 1, ": All")
          var1 = readline(prompt = "Enter one or more numbers: ")
          var1 = as.numeric(unlist(strsplit(var1, ",| ")))
          var1 = var1[!is.na(var1)]
          remove_all = any(var1 == i + 1)
          var1 = var1[var1 > 0 & var1 <= length(installed)]

          if (remove_all){
            message("Removing all Julia versions from sdbuildR directory...")
            unlink(dirs, recursive = TRUE)
          } else if (any(var1 %in% seq_along(installed))){
            message("Removing Julia version(s) ", paste0(installed[var1], collapse = ", "), " from sdbuildR directory...")
            for (i in var1){
              unlink(file.path(dirs, installed[i]), recursive = TRUE)
            }
          } else {
            message("Keeping Julia version(s) ", paste0(installed[var1], collapse = ", "), " in sdbuildR directory.")
          }

        }


      }
    }
  }

  if (remove & install){
    stop("No Julia installation found to remove in sdbuildR directory.")
  }

  install = ifelse(force, TRUE, install)

  if (install){
    message("No Julia installation found in sdbuildR directory. Installing Julia...")
    julia_dir = install_julia_sdbuildR(prefix = dirs, version = version)
  }

  JULIA_HOME = dirname(list.files(
    path = julia_dir,
    pattern = "julia.exe$",
    full.names = TRUE,
    recursive = TRUE
  ))
  julia <- JuliaCall::julia_setup(JULIA_HOME = JULIA_HOME, installJulia=FALSE, ...)

  # Required Julia packages
  pkgs = c("DifferentialEquations", "DiffEqCallbacks",
           "DataFrames", "Unitful", "Statistics", "Random",
           "Distributions", "DataInterpolations")
  if (install){
    message("Installing required Julia packages. This can take 5-15 minutes...")
    out = lapply(pkgs, JuliaCall::julia_install_package)
  }
  # if (update_pkg){
  #   message("Updating Julia packages...")
  #   check = lapply(pkgs, JuliaCall::julia_update_package)
  # }
  # lapply(pkgs, JuliaCall::julia_library)

  # Run initialization
  run_init()

  # Set global option of initialization
  # options()[[P$init_sdbuildR]] = TRUE
  options("initialization_sdbuildR" = TRUE)

  invisible()
}




#' Check whether init.jl already ran
#'
#' @return NULL
run_init = function(){

  # already_init = JuliaCall::julia_eval(paste0("isdefined(@__MODULE__, :", P$init_sdbuildR, ")"))
  #
  # if (!already_init){
    message("Setting up Julia environment for sdbuildR...")
    # Initialize set-up for sdbuildR in Julia
    env_path <- system.file(package = "sdbuildR")
    JuliaCall::julia_source(file.path(env_path, "init.jl"))
  # }

  return(NULL)
}

