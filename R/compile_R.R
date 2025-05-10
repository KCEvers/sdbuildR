

#' Simulate stock-and-flow model in R
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
simulate_R = function(sfm,
                      format_code=TRUE,
                      keep_nonnegative_flow = TRUE,
                      keep_nonnegative_stock = FALSE,
                      verbose = FALSE,
                      debug = FALSE){

  # Collect arguments
  argg <- c(
    as.list(environment()))
  # Remove NULL arguments
  argg = argg[!lengths(argg) == 0]

  # Compile script without plot
  script = compile_R(sfm,
                     format_code=format_code,
                     keep_nonnegative_flow = keep_nonnegative_flow,
                     keep_nonnegative_stock = keep_nonnegative_stock,
                     debug = debug)
  filepath = write_script(script, ext = ".R")

  # Evaluate script
  sim = tryCatch({

    # Create a new environment to collect variables
    envir <- new.env()

    start_t = Sys.time()

    # Evaluate script
    eval(parse(text = script), envir = envir)

    end_t = Sys.time()
    if (verbose){
      message(paste0("Simulation took ", round(end_t - start_t, 4), " seconds"))
    }

    # ls(envir)
    # names(envir)

    # if (include_plot){
    #   # Plot stocks
    #   pl = plot_sim(sfm, envir[[P$sim_df_name]])
    #   envir$pl = pl
    # }

    # message("The R script ran successfully without errors.")
    # list(
    #      success = TRUE,
    #      df = envir[[P$sim_df_name]],
    #      xstart = envir[[P$initial_value_name]],
    #      pars = envir[[P$parameter_name]],
    #      ode_func = envir[[P$ode_func_name]]
    #      )
    envir$script = script
    envir$filepath = filepath
    envir$success = TRUE
    envir$duration = end_t - start_t
    as.list(envir) %>% utils::modifyList(argg) %>%
      structure(., class = "sdbuildR_sim")
  },
  error = function(e) {
    warning("\nAn error occurred while running the R script.")
    # print(e$message)
    list(success = FALSE, error_message = e$message, script = script, filepath = filepath) %>%
      structure(., class = "sdbuildR_sim")
  })


  return(sim)

}


#' Compile R script to simulate stock-and-flow model
#'
#' @inheritParams simulate
#'
#' @return R script
#' @noRd
#'
compile_R = function(sfm,
                     format_code=TRUE,
                     keep_nonnegative_flow = TRUE,
                     keep_nonnegative_stock = FALSE,
                     verbose = FALSE,
                     debug = FALSE){

  # Get flows and connections
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


  # Function to remove unit

  # # **don't use Unitful with R
  # replace_unit_old = function(x){
  #   # print(x)
  #   new_x = JuliaCall::julia_eval(paste0("Unitful.ustrip(",
  #                                        stringr::str_replace_all(x, "u\\(['|\"]", "u\"") %>%
  #                                          stringr::str_replace_all("['|\"]\\)$", "\""),
  #                                        ")"))
  #   # print(new_x)
  #   return(new_x)
  # }
  #
  # # ** this doesn;t work; see
  # # URL = "https://insightmaker.com/insight/6u2G5l9tGzD73cWnZAoabS/Influence-of-Surface-Temperature-on-Albedo-and-Greenhouse-Effect"
  # replace_unit = function(x){
  #   print(x)
  #   x_split = split_units(stringr::str_replace_all(x, "u\\(['|\"]", "") %>%
  #                           stringr::str_replace_all("['|\"]\\)$", "")) %>% unlist()
  #   print(x_split)
  #   new_x = stringr::str_replace_all(x_split, "([a-zA-z][a-zA-Z0-9_]*)", "") %>% trimws() %>% paste0(collapse = "")
  #   print(new_x)
  #   # new_x = JuliaCall::julia_eval(paste0("Unitful.ustrip(",
  #   #                                      stringr::str_replace_all(x, "u\\(['|\"]", "u\"") %>%
  #   #                                        stringr::str_replace_all("['|\"]\\)$", "\""),
  #   #                                      ")"))
  #   return(new_x)
  # }

  # # Remove all unit strings from equations
  # sfm$model$variables = sfm$model$variables %>%
  #   purrr::map_depth(2, function(x){
  #     if (is_defined(x$eqn)){
  #       x$eqn = stringr::str_replace_all(x$eqn, "\\bu\\([\"|'](.*?)[\"|']\\)", "")
  #       # x$eqn = stringr::str_replace_all(x$eqn, "\\bu\\([\"|'](.*?)[\"|']\\)", replace_unit)
  #     }
  #     return(x)
  #   })
  #
  # # Remove all unit strings from macros
  # sfm$macro = sfm$macro %>%
  #   purrr::map(function(x){
  #     if (is_defined(x$eqn)){
  #       x$eqn = stringr::str_replace_all(x$eqn, "\\bu\\([\"|'](.*?)[\"|']\\)", "")
  #     }
  #     return(x)
  #   })

  # Extract all unit strings from equations
  var_units = sfm$model$variables %>%
    purrr::map_depth(2, function(x){
      if (is_defined(x$eqn)){
        return(stringr::str_extract_all(x$eqn, "\\bu\\([\"|'](.*?)[\"|']\\)"))
      }
    }) %>% unname() %>% unlist()

  # Extract all unit strings from macros
  macro_units = sfm$macro %>%
    purrr::map(function(x){
      if (is_defined(x$eqn)){
        return(stringr::str_extract_all(x$eqn, "\\bu\\([\"|'](.*?)[\"|']\\)"))
      }
      return(x)
    }) %>% unlist()

  eqn_units = c(var_units, macro_units)

  # Stop if equations contain unit strings
  if (length(eqn_units) > 0){

    eqn_units_format = eqn_units %>% purrr::imap(function(x, name){
      paste0(name, "$eqn contains ", unname(x))
    }) %>% unlist() %>% unname()

    stop(paste0("Unit strings u('') detected in model! Units are not supported for simulations in R.\nSet sfm %>% sim_specs(language = '') or modify the following unit strings:\n\n", paste0(eqn_units_format, collapse = "\n")))
  }

  # # Convert conveyors
  # sfm = convert_conveyor(sfm)

  # Compile all parts of the R script
  times = compile_times(sfm)
  constraints = compile_constraints(sfm)
  ordering = order_equations(sfm)

  # **to do:add deep dependencies for pluck_from_ode

  # Order Stocks alphabetically to match ordering in xstart
  sfm$model$variables$stock = sfm$model$variables$stock[sort(names(sfm$model$variables$stock))]

  # Macros
  macros = compile_macros(sfm)

  # Add prefixes (pars$ and xstart$) to static equations
  sfm = substitute_var(sfm)

  # Static equations
  static_eqn = compile_static_eqn(sfm, ordering)

  # Stocks
  sfm = prep_stock_change(sfm)
  nonneg_stocks = compile_nonneg_stocks(sfm, keep_nonnegative_stock)

  zeallot_def = compile_destructuring_assign(sfm, static_eqn)

  seed_str = ifelse(!is_defined(sfm$sim_specs$seed), "",
                    sprintf("# Ensure reproducibility across runs in case of random elements
set.seed(%s)", as.character(sfm$sim_specs$seed)))


  prep_script = sprintf("# Script generated on %s by sdbuildR.

# Load packages
library(dplyr)%s
library(sdbuildR)

%s
%s
%s
%s
", Sys.time(), zeallot_def$script, seed_str, times$script, macros$script, nonneg_stocks$func_def)


  ode = compile_ode(sfm, ordering, prep_script, static_eqn, constraints, keep_nonnegative_flow, keep_nonnegative_stock)
  run_ode = compile_run_ode(sfm, nonneg_stocks)

  script = sprintf("%s
%s%s%s%s", prep_script,
                   ode$script,
                   # globalpast$script,
                   constraints$script,
                   static_eqn$script,
                   run_ode$script
  )


  # Format code
  if (format_code & requireNamespace("styler", quietly = TRUE)){
    script = tryCatch({
      # script <- suppressWarnings(suppressMessages(styler::style_text(script)))
      suppressWarnings(suppressMessages(
        script <- styler::style_text(script)
      ))
      script
    }, error = function(e){
      message("Error in formatting script!")
      script
    }
    )
  }

  return(script)
  # return(cat(script))
}



#' Compile script for enabling destructuring assignment in R
#'
#' @inheritParams build
#' @inheritParams compile_ode
#'
#' @return List with necessary scripts
#' @noRd
#'
compile_destructuring_assign = function(sfm, static_eqn){

  # names_df = get_names(sfm)

  # Add package for destructuring assignment in case it was used
  eqns = c(static_eqn$script, unlist(sfm$model$variables %>% purrr::map_depth(2, "eqn")))

  # if ( any(stringr::str_detect(c(static_eqn$script, ode$script), stringr::fixed("%<-%"))) ){

  if ( any(stats::na.omit(stringr::str_detect(eqns, stringr::fixed("%<-%")))) ){
    script = "\n# Add package for destructuring assignment\nif (!require('zeallot')) install.packages('zeallot'); library(zeallot)\n"
  } else {
    script = ""
  }

  #   # Add package for units in case it was used
  #   units_used = any(stats::na.omit(stringr::str_detect(eqns, stringr::fixed("set_units"))))
  #   units_functions_used = any(stats::na.omit(stringr::str_detect(eqns, "seconds\\(|minutes\\(|hours\\(|days\\(|weeks\\(|months\\(|quarters\\(|years\\(")))
  #
  #   if ( units_used | units_functions_used ){
  #     add_units = "\n# Add package for specifying units of each model element\nif (!require('units')) install.packages('units')\n library(units)\nunits::units_options(allow_mixed = T, simplify = T, set_units_mode = 'standard')"
  #   } else {
  #     add_units = ""
  #   }
  #
  #   # Prep ODE variables if they have units
  #   if (keep_unit & any(nzchar(names_df$units))){
  #
  #     ODE_var = c(ordering$dynamic, purrr::map_vec(sfm$model$variables$stock, "sum_name") )
  #     ODE_unit = c(names_df[match(ordering$dynamic, names_df$name), "units"], purrr::map_vec(sfm$model$variables$stock, "sum_units"))
  #     ODE_unit[ODE_unit == "1"] = ""
  #     ODE_unit = ifelse(stringr::str_starts(ODE_unit, "paste0\\("), ODE_unit, paste0("'", ODE_unit, "'"))
  #
  #     ODE_var_str = sprintf("
  # # Create global ODE variables with units
  # %s = c(%s)
  # %s = c(%s)
  # invisible(mapply(function(name, unit){
  #   if (nzchar(unit)){
  #     x = set_units(NA, unit)
  #   } else {
  #     x = NA
  #   }
  #   assign(name, x, envir = .GlobalEnv)
  #   }, name = %s, unit = %s))
  # ", P$ODE_var_name, paste0("'", ODE_var, "'", collapse = ", "),
  #                           P$ODE_unit_name, paste0(ODE_unit, collapse = ", "),
  #                           P$ODE_var_name, P$ODE_unit_name)
  #
  #     #  x = dplyr::if_else(nzchar(unit), set_units(NA, unit), NA)
  #
  #     # invisible(mapply(function(name, unit){
  #     #   # if (nzchar(unit)){
  #     #   #   assign(name, set_units(NA, unit), envir = .GlobalEnv)
  #     #   # } else {
  #     #   #   assign(name, NA, envir = .GlobalEnv)
  #     #   # }
  #     #   # x = ifelse(nzchar(unit), set_units(NA, unit), NA)
  #     #   x = dplyr::if_else(nzchar(unit), set_units(NA, unit), NA)
  #     #   assign(name, x, envir = .GlobalEnv)
  #     #   },
  #     #        name = ODE_var, unit = ODE_units))
  #
  #
  #   } else {
  # ODE_var_str = ""
  # }

  return(list(script = script
              # add_units = add_units,
              # ODE_var_str = ODE_var_str
              ))
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
substitute_var = function(sfm){

  # # Get variable names
  # names_df = get_names(sfm)

  # Replace variable references in static equations
  static_var = c(names(sfm$model$variables$stock), names(sfm$model$variables$constant))
  dynamic_var = c(names(sfm$model$variables$flow),
                  setdiff(names(sfm$model$variables$aux), names(sfm$model$variables$constant)))

  static_replacements =
    c(paste0(P$initial_value_name, "$", names(sfm$model$variables$stock)),
      paste0(P$parameter_name, "$", names(sfm$model$variables$constant))
      # paste0("pluck_from_ode('", dynamic_var, "', environment())"),
      # References to time for static variables (e.g. in convert_time_units) should be times[1]
      ) %>% as.list() %>%
    stats::setNames(static_var) %>%
    # Convert to expressions for substitutions
    lapply(function(x){parse(text = x)[[1]]})

  # names(static_replacements) <- paste0("(?<![\\w._])", names(static_replacements), "(?![\\w._])")


  # gf_str = sfm$model$variables$gf %>%
  #   purrr::map(\(x) paste0(x$name, "(", x$source, ")")) %>%
  #   unlist() %>% paste0(collapse = ", ")


  # if (keep_unit){
  #   dynamic_replacements = names_df %>% dplyr::filter(.data$type == "stock" & is_defined(.data$units)) %>%
  #     dplyr::mutate(replacement =  paste0("set_units(", .data$name, ", '", .data$units, "')")) %>%
  #     dplyr::pull(.data$replacement, .data$name) %>% as.list() %>%
  #     # Convert to expressions for substitutions
  #     lapply(function(x){parse(text = x)[[1]]})
  #
  # } else {
  #   dynamic_replacements = c()
  # }


  # Implement replacements
  sfm$model$variables = lapply(sfm$model$variables, function(y){
    lapply(y, function(x){

      if (x$name %in% static_var){

        expr1 = parse(text = x$eqn)
        replace_expr = do.call("substitute", list(expr1[[1]], static_replacements))
        x$eqn = deparse1(replace_expr)

        # x$eqn = stringr::str_replace_all(x$eqn, static_replacements)

      }

      return(x)
    })
  })

  return(sfm)
}



#' Modify conveyors in stock-and-flow model
#'
#' @inheritParams build
#'
#' @return Updated stock-and-flow model
#' @noRd
#'
convert_conveyor = function(sfm){

  # Conveyor stocks are essentially a combination of a Stock and an Auxiliary, where the Auxiliary accesses a delayed value of the Stock.

  # Get names of conveyor Stocks
  conveyor_stocks = names(which(get_map(sfm$model$variables$stock, "conveyor") == TRUE))

  new_element = sfm$model$variables$stock %>%
    purrr::map(function(x){

      if (!is.null(x$conveyor)){
        if (x$conveyor == TRUE){

          # Add Stock for conveyor belt
          conveyor_stock_name = paste0(x$name, P$conveyor_suffix)

          conveyor_stock = x
          conveyor_stock$conveyor = NULL
          conveyor_stock$len = NULL

          if (is.null(x$len)){
            stop(sprintf("The conveyor Stock %s does not have a delay length specified! Add the delay length with sfm = build(sfm, '%s', len = ...)", x$name, x$name))
          }

          # Add static Auxiliary for delay length
          if (!contains_only_names(x$len)){
            delay_length = paste0(x$name, P$delay_suffix, P$delay_length_suffix)
            delay_length_list = list(aux = list(list(name = delay_length, eqn = x$len)) %>% stats::setNames(delay_length))
          } else {
            delay_length = as.character(x$len)
            delay_length_list = list()
          }

          # The increments until the delay time is reached are the initial value of the Stock divided by the delay time, times the timestep
          delay0 = sprintf(
            "%s[['%s']]/%s*%s",
            P$initial_value_name,
            conveyor_stock_name,
            delay_length,
            P$time_name
          )

          # The Stock name now will become an auxiliary
          out = list(
            # Access delayed version of Stock
            aux = list(list(name = x$name,
                            eqn = sprintf("add_delay(%s, %s, %s)", conveyor_stock_name,
                                          as.character(x$len), delay0))) %>%
              stats::setNames(x$name),
            # Conveyor belt
            stock = list(conveyor_stock) %>% stats::setNames(conveyor_stock_name))

          if (length(delay_length_list) > 0){
            out = out %>% utils::modifyList(delay_length_list)
          }

          return(out)
        }
      }
    }) %>% purrr::flatten()

  new_element

  # Remove old conveyor stocks
  sfm$model$variables$stock[conveyor_stocks] = NULL

  # Add elements to model (in for-loop, as otherwise not all elements are added)
  for (i in seq_along(new_element)){
    sfm$model$variables = sfm$model$variables %>%
      utils::modifyList(new_element[i])
  }

  sfm = validate_xmile(sfm)

  return(sfm)

}




#' Compile script for global variables
#'
#' @inheritParams build
#'
#' @return List with macro script
#' @noRd
compile_macros = function(sfm){

  script = ""

  # If there are globals
  if (is_defined(sfm$global$eqn)){
    # names_df = get_names(sfm)

    script = paste0(script,
                    sfm$global$eqn)

  }

  # If there are macros
  if (any(nzchar(purrr::map_vec(sfm$macro, "eqn")))){
    # names_df = get_names(sfm)

    script = paste0(script, "\n",
                    sfm$macro %>% purrr::map(function(x){

                      # If a name is defined, assign macro to that name
                      if (nzchar(x$name)){
                        paste0(x$name, " = ", x$eqn) %>% return()
                      } else {
                        x$eqn %>% return()
                      }

                    }) %>% unlist() %>% paste0(collapse = "\n"))

  }

  if (nzchar(script)){
    script = paste0("\n\n# User-specified macros\n", script, "\n")
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
compile_times = function(sfm){

  # if (length(sfm$model_units) > 0 & keep_unit){
  #
  #   # Topological sort
  #   eq_names = names(sfm$model_units)
  #   dependencies = lapply(sfm$model_units %>% get_map("def"),
  #                         function(x){stringr::str_extract_all(x, eq_names) %>% unlist() })
  #   ordering = topological_sort(dependencies)
  #   sfm$model_units = sfm$model_units[ordering]
  #
  #   # Create string with units to install
  #   add_install_unit_script = purrr::imap(sfm$model_units,
  #                                         function(x, y){
  #                                           # Create unit definition for install_custom_unit()
  #                                           def_str = name_str = ""
  #                                           if ("def" %in% names(x)){
  #                                             def_str = paste0(", def = '", x$def, "'")
  #                                           }
  #                                           if ("name" %in% names(x)){
  #                                             name_str = paste0(", name = '", x$name, "'")
  #                                           }
  #
  #                                           return(sprintf("c(symbol = '%s'%s%s)", y, def_str, name_str))
  #
  #                                         }) %>% unlist() %>% paste0(collapse = ", ") %>%
  #     paste0("\n\n# Install custom units\ninvisible(list(", ., ") %>%\n\t\tlapply(., install_custom_unit))")
  #
  # } else {
  add_install_unit_script = ""
  # }


  # Only track variables in "past"
  obligatory_var = sfm$model$variables %>% purrr::map_depth(2, "archive_var") %>% unname() %>%
    purrr::list_flatten() %>% unlist() %>% unname() %>% unique()

  if (length(obligatory_var) > 0){
    add_extra_dt = sprintf("+ %s", P$timestep_name)
  } else {
    add_extra_dt = ""
  }

  script = sprintf("
# Define time sequence
%s = %s
%s = seq(from=%s, to=%s%s, by=%s)
%s = %s[1]

# Simulation time unit (smallest time scale in your model)
%s = '%s' %s
                   ",
                   P$timestep_name, as.character(sfm$sim_specs$dt),
                   P$times_name,
                   as.character(sfm$sim_specs$start),
                   as.character(sfm$sim_specs$stop),
                   add_extra_dt, # P$timestep_name,
                   P$timestep_name,
                   P$time_name, P$times_name,
                   P$time_units_name,
                   sfm$sim_specs$time_units, add_install_unit_script)

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
compile_constraints = function(sfm){

  # Compile string of minimum and maximum constraints
  constraint_def = sfm$model$variables %>% purrr::map(function(x){
    purrr::imap(x, function(y, name){
      if ("min" %in% names(y)){
        min_str = ifelse(is_defined(y$min), paste0("min = ", y$min), "")
        max_str = ifelse(is_defined(y$max), paste0("max = ", y$max), "")

        if (nzchar(min_str) | nzchar(max_str)){
          sprintf("%s = c(%s%s%s)", name, min_str, ifelse(nzchar(min_str) & nzchar(max_str), ", ", ""), max_str) %>% return()
        }
      }

    })
  }) %>% unlist() %>% paste0(., collapse = ",\n\t\t\t\t\t\t\t\t\t\t")

  script = ifelse(nzchar(constraint_def), sprintf("\n\n# Constraints of minimum and maximum value\n%s = list(%s) %%>%% \n\tget_logical_constraints()\n", P$constraint_def, constraint_def ), "")

  update_ode = ifelse(nzchar(constraint_def), sprintf("\n\n\t\t# Check constraints\n\t\tcheck_constraints(%s, environment(), %s)\n", P$constraint_def, P$time_name), "")

  return(list(script = script,
              update_ode = update_ode))
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
compile_static_eqn = function(sfm, ordering){

  # Macros
  macros_script = ifelse(is_defined(sfm$macro$eqn),
                         sprintf("\n\n# User-defined macros and globals\n%s\n",
                                 paste0(sfm$macro$eqn, collapse = "\n")), "")


  # Graphical functions
  gf_eqn = sfm$model$variables$gf %>%
    purrr::imap(function(x, y){

      if (is_defined(x$xpts)){

        if (inherits(x$xpts, "numeric")){
          xpts_str = paste0(as.character(x$xpts), collapse= ", ") %>% paste0("c(", ., ")")
        } else {
          xpts_str = x$xpts
        }

        # ypts is not obligatory
        if (!is_defined(x$ypts)){
          ypts_str = ""
        } else {
          if (inherits(x$ypts, "numeric")){
            x$ypts = paste0(as.character(x$ypts), collapse= ", ") %>% paste0("c(", ., ")")
          }
          ypts_str = sprintf("\n\t\ty = %s,", x$ypts)
        }

        # if (keep_unit & is_defined(x$units)){
        #   unit_str = sprintf(" %%>%%\n\t\tgf_with_units(., %s)", paste0("'", x$units, "'"))
        # } else {
        # unit_str = ""
        # }

        sprintf("%s = stats::approxfun(x = %s,%s\n\t\tmethod = '%s', rule = %s)",
                y, xpts_str,
                ypts_str,
                x$interpolation, ifelse(x$extrapolation == "nearest", 2,
                                        ifelse(x$extrapolation == "NA", 1, x$extrapolation))) %>% return()
      }
    })

  # Constant equations
  constant_eqn = sfm$model$variables$constant %>%
    purrr::imap(function(x, y){
      paste0(P$parameter_name, "$", y, " = ", x$eqn)
    })

  # Initial states of Stocks
  stock_eqn = sfm$model$variables$stock %>%
    purrr::imap(function(x, y){
      if (!is.null(x$delayN)){
        sprintf("# Initialize delay accumulator for %s\n%s = c(%s, %s)",
                y,
                P$initial_value_name, P$initial_value_name, x$eqn) %>% return()
      } else {
        paste0(P$initial_value_name, "$", y, " = ", x$eqn) %>% return()
      }
    })

  # dependencies_dict = sfm$variables[c("gf", "constant", "stock")] %>% unname() %>%
  #   purrr::map_depth(2, "deep_dependencies") %>% purrr::list_flatten()
  # order_idxs = topological_sort(dependencies_dict)
  # order_idxs

  # Compile and order static equations
  static_eqn_str = c(gf_eqn, constant_eqn, stock_eqn)[ordering$static$order] %>% unlist() %>% paste0(collapse = "\n")
  static_eqn_str

  names_df = get_names(sfm)
  stock_units = names_df[names_df$type == "stock", "units"]

  return(list(script = paste0(macros_script,
                              sprintf("\n\n# Set-up parameters and initial condition
%s = list()
%s = list()", P$parameter_name, P$initial_value_name), "\n\n# Define parameters, initial conditions, and functions in correct order\n",
                              static_eqn_str,
                              # ifelse(keep_unit & (any(nzchar(stock_units) & stock_units != "1")), sprintf("\n\n%s = lapply(%s[order(names(%s))], deparse_if_unit)", P$stock_units_name, P$initial_value_name, P$initial_value_name), ""),
                              "",
                              sprintf("\n\n# Turn initial condition into alphabetically ordered named vector\n%s = unlist(%s)[order(names(%s))]", P$initial_value_name, P$initial_value_name, P$initial_value_name)
  )))
}





#' Prepare for summing change in stocks in stock-and-flow model
#'
#' @inheritParams build
#' @inheritParams compile_R
#'
#' @noRd
#' @return Updated stock-and-flow model
#'
prep_stock_change = function(sfm){

  # # Order Stocks alphabetically to match ordering in xstart
  # sfm$model$variables$stock = sfm$model$variables$stock[sort(names(sfm$model$variables$stock))]

  # Add temporary property to sum change in Stocks
  sfm$model$variables$stock = sfm$model$variables$stock %>%
    purrr::imap(function(x, y){

      if (!is.null(x$delayN)){
        # return(NULL)

        x$sum_name = paste0(x$inflow, "$update")
        x$sum_eqn = ""
        x$sum_units = ""
      } else {

        inflow = outflow = ""
        x$sum_name = paste0(P$change_prefix, y)

        # if (keep_unit){
        #   y_str = paste0(P$change_prefix, y, "[]")
        # } else {
        y_str = paste0(P$change_prefix, y)
        # }

        # In case no inflow and no outflow is defined, update with 0
        if (!is_defined(x$inflow) & !is_defined(x$outflow)){
          # sprintf("%s%s = 0 # Collect Flows of %s", P$change_prefix, y, y) %>% return()
          x$sum_eqn = "0"
        } else {
          if (is_defined(x$inflow)){
            inflow = x$inflow %>% paste0(collapse = " + ")
          }
          if (is_defined(x$outflow)){
            outflow = paste0(" - ", x$outflow) %>% paste0(collapse = "")
          }
          x$sum_eqn = sprintf("%s%s", inflow, outflow)        }

        # # Add units if defined
        # if (keep_unit & is_defined(x$units)){
        #   x$sum_units = paste0("paste0('", x$units, "/', ", P$time_units_name, ")")
        # } else {
        x$sum_units = ""
        # }
      }
      return(x)

    }) %>% purrr::compact()

  sfm = validate_xmile(sfm)

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
compile_nonneg_stocks = function(sfm, keep_nonnegative_stock){

  # Non-negative Stocks
  nonneg_stock = sfm$model$variables$stock %>% purrr::map("non_negative") %>% unlist() %>% which()

  if (keep_nonnegative_stock & length(nonneg_stock) > 0){

    func_def = sprintf("
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
P$nonneg_stock_name, P$initial_value_name,
paste0("'", names(nonneg_stock), "'", collapse = ", "),
P$rootfun_name, P$time_name, P$state_name, P$parameter_name,
P$state_name, P$nonneg_stock_name,
P$eventfun_name, P$time_name, P$state_name, P$parameter_name,
P$state_name, P$nonneg_stock_name, P$state_name
    )

    root_arg = sprintf(",\n\t\t\t\tevents = list(func = %s, root = TRUE), rootfun = %s",
                       P$eventfun_name, P$rootfun_name)

    check_root = sprintf("
# Times at which non-negative Stocks fell below 0
attributes(%s)$troot

# Values of non-negative Stocks when root function was triggered
attributes(%s)$valroot
", P$out_name, P$out_name)

    return(list(func_def = func_def,
                root_arg = root_arg,
                check_root = check_root))
    # nonneg_stock_str = lapply(sort(names(nonneg_stock)), # Order alphabetically for aesthetic reasons
    #                           function(x){
    #                             sprintf("if (%s + d%sdt[names(%s) == '%s']*%s < 0){\n\t\t\t\td%sdt[names(%s) == '%s'] = - %s # Subtract what's left of %s before it turns negative\n\t\t}", x, P$state_name, P$initial_value_name, x, P$timestep_name, P$state_name, P$initial_value_name, x, x, x)}) %>%
    #   paste0(collapse = "\n\t\t")
    # nonneg_stock_str = sprintf("\n\t\t# Ensure non-negativity of (selected) Stocks\n\t\t%s\n", nonneg_stock_str)


  } else {
    return(list(func_def = "",
                root_arg = "",
                check_root = ""))
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
#' @param constraints Output of compile_constraints()
#'
#' @return List
#' @importFrom rlang .data
#' @noRd
#'
compile_ode = function(sfm, ordering, prep_script, static_eqn, constraints, keep_nonnegative_flow, keep_nonnegative_stock){

  # # Delay
  # delay_var = sfm$model$variables %>% purrr::map_depth(2, function(x){
  #  if (!is.null(x$eqn)){
  #    if (stringr::str_detect(x$eqn, "delayN\\(")){
  #     return(x$name)
  #   }
  # }
  #   }) %>% unlist() %>% unname()

  # Auxiliary equations (dynamic auxiliaries)
  aux_eqn = sfm$model$variables$aux %>%
    purrr::imap(function(x, y){

      # if (keep_unit){
      #   out = sprintf("%s[] <- %s", y, x$eqn)
      # } else {
      out = sprintf("%s <- %s", y, x$eqn)
      # }

      if (!is.null(x$preceding_eqn)){
        out = c(x$preceding_eqn, out)
      }
      return(out)
    })

  # Flow equations
  flow_eqn = sfm$model$variables$flow %>%
    purrr::imap(function(x, y){

      # if (keep_unit){
      #   y_str = paste0(y, "[]")
      # } else {
      y_str = y
      # }

      out = sprintf("%s <- %s%s%s # Flow%s%s",
                    y_str,
                    ifelse(x$non_negative, "nonnegative(", ""),
                    x$eqn,
                    ifelse(x$non_negative, "\n\t\t)", ""),
                    # Add comment
                    ifelse(is_defined(x$from), paste0(" from ", x$from), ""),
                    ifelse(is_defined(x$to), paste0(" to ", x$to), ""))

      if (!is.null(x$preceding_eqn)){
        out = c(x$preceding_eqn, out)
      }
      return(out)
    })

  # Compile and order all dynamic equations
  dynamic_eqn = c(aux_eqn, flow_eqn)[ordering$dynamic$order] %>% unlist()
  dynamic_eqn

  # # To correct a units error, e.g. 1 + set_units(2, 'second' ), we need to evaluate each line of code
  # dynamic_eqn = tryCatch({
  #
  #   # Create a new environment to collect variables
  #   envir <- new.env()
  #
  #   # To see whether any unitless needs to be added, we need to run prep_script, the globalpast script, and the static equations script
  #   eval(parse(text = prep_script), envir = envir)
  #
  #   # if (nzchar(globalpast$script)){
  #   #   eval(parse(text = globalpast$script), envir = envir)
  #   # }
  #
  #   eval(parse(text = static_eqn$script), envir = envir)
  #
  #   # # xstart is turned into a vector at the end of static_eqn$script, but to use list2env(), it needs to be a list
  #   # eval(parse(text = sprintf("%s = as.list(%s)", P$initial_value_name, P$initial_value_name)), envir = envir)
  #
  #   # Unwrap the xstart elements into the current environment
  #   list2env(as.list(envir[[P$initial_value_name]]), envir = envir)
  #
  #   # Change xstart back into a vector and create a state variable S as well as time t
  #   # eval(parse(text = sprintf("%s = unlist(%s); %s = %s", P$initial_value_name, P$initial_value_name, P$state_name, P$initial_value_name)), envir = envir)
  #   eval(parse(text = sprintf("%s = %s", P$state_name, P$initial_value_name)), envir = envir)
  #   eval(parse(text = sprintf("%s = %s[1]", P$time_name, P$times_name)), envir = envir)
  #
  #   # Unwrap the parameters elements into the current environment
  #   list2env(envir[[P$parameter_name]], envir = envir)
  #
  #   # With this environment containing constant parameters and initial conditions, evaluate each line of code in dynamic eqn
  #
  #   envir2 <- envir # Create a new environment to collect variables
  #
  #   for (i in seq_along(dynamic_eqn)){
  #     # print(i)
  #     code_string = dynamic_eqn[i]
  #     # print(code_string)
  #
  #     out = incompatible_unit_operation(code_string, envir2)
  #
  #     envir2 = out$envir
  #     dynamic_eqn[i] = out$code_string
  #     # print(out$code_string)
  #   }
  #   dynamic_eqn
  #
  # }, error = function(e) {
  #   print(e)
  #   return(dynamic_eqn)
  # })



  # Compile and order all dynamic equations
  dynamic_eqn_str = dynamic_eqn %>% paste0(collapse = "\n\t\t")

  # Sum change in Stock equations
  stock_change = sfm$model$variables$stock %>%
    purrr::map(function(x){
      if (!is.null(x$delayN)){
        return(NULL)
      } else {
        sprintf("%s%s <- %s", x$sum_name,
                # ifelse(keep_unit, "[]", ""),
                "",
                x$sum_eqn)
      }
    }) %>% purrr::compact()

  stock_change_str = stock_change %>%
    paste0(collapse = "\n\t\t")

  # stock_change_str = stock_change[sort(names(stock_change))] %>%
  #   paste0(collapse = "\n\t\t")

  # Order Stocks alphabetically to match ordering in xstart
  stock_changes_names = unlist(lapply(sfm$model$variables$stock, `[[`, "sum_name"))

  # state_change_names = unname(unlist(stock_changes_names[sort(names(sfm$model$variables$stock))]))

  # state_change_str = sprintf("d%sdt = c(%s)",
  #                            P$state_name, state_change_names %>%
  #                              paste0(ifelse(keep_unit, "drop_if_units(", ""), ., ifelse(keep_unit, ")", "")) %>%
  #                              paste0(collapse = ", ") )
  # state_change_str = sprintf("d%sdt = %sc(%s)%s",
  #                            P$state_name, ifelse(keep_unit, "drop_if_units(", ""),
  #                              paste0(unname(stock_changes_names), collapse = ", "), ifelse(keep_unit, ")", "") )
  state_change_str = sprintf("d%sdt = c(%s)",
                             P$state_name,
                             paste0(unname(stock_changes_names), collapse = ", ") )
  # Graphical functions (gf)
  if (length(sfm$model$variables$gf) > 0){

    # Some gf have other gf as source; recursively replace
    gf_sources = sfm$model$variables$gf %>% purrr::map("source") %>% purrr::compact() %>% unlist()

    if (length(gf_sources) > 0){
      dict = paste0(names(gf_sources), "(", unname(gf_sources), ")") %>% stats::setNames(names(gf_sources))

      dict2 = paste0("(", names(gf_sources), "(", unname(gf_sources), "))") %>% stats::setNames(paste0("\\(", stringr::str_escape(names(gf_sources)), "\\)"))

      gf_str = stringr::str_replace_all(unname(dict), dict2)

      # gf_str = sfm$model$variables$gf %>%
      #   purrr::map(\(x) paste0(x$name, "(", x$source, ")")) %>%
      #   unlist() %>% paste0(collapse = ", ")
      gf_str = paste0(", ", paste0(paste0("'", gf_str, "' = "), gf_str, collapse = ", "))
    } else {
      gf_str = ""
    }
  } else {
    gf_str = ""
  }

  # # Save all variables in return statement
  # gf_names = names_df %>% dplyr::filter(.data$type == "gf") %>% dplyr::pull(.data$R_name_final)

  # save_var = c(
  #   names_df[match(ordering$dynamic, names_df$name), "R_name_final"] %>%
  #   # names(c(aux_eqn, flow_eqn)[ordering$dynamic])
  #                # Remove delay variables
  #                setdiff(delay_var),
  #              paste0(P$change_prefix, names(stock_change[sort(names(stock_change))])),
  #              gf_names) #%>% setdiff(globalpast$archive_var)
  # save_var_str = paste0(paste0("'", save_var, "'", " = ", ifelse(keep_unit, "drop_if_units(", ""), save_var,
  #                              ifelse(keep_unit, ")", "")), collapse = ",\n\t\t\t\t\t\t\t\t\t\t\t\t")


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
  S_str = sprintf("%s = as.list(%s)", P$state_name, P$state_name)

  # Compile
  script = sprintf("\n\n# Define ODE
%s = function(%s, %s, %s){

  %s

  \n# Compute change in Stocks at current time %s
  with(c(%s, %s), {

    # Update Auxiliaries and Flows
    %s

    # Collect inflows and outflows for each Stock
    %s

    # Combine change in Stocks
    %s
    %s
    return(list(d%sdt%s))
  })
}", P$ode_func_name,P$time_name, P$state_name, P$parameter_name,
                   S_str,
                   P$time_name, P$state_name, P$parameter_name,
                   dynamic_eqn_str,
                   stock_change_str,
                   state_change_str,
                   constraints$update_ode,
                   P$state_name, gf_str)

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
compile_run_ode = function(sfm, nonneg_stocks){

  #   script = sprintf("\n\n# Run ODE
  # %s = deSolve::%s(
  #   func=%s,
  #   y=%s,
  #   times=%s,
  #   parms=%s,
  #   method = '%s'
  # ) %%>%% as.data.frame()
  #
  # # Create complete dataframe
  # sim = %s %%>%%
  #   # Add parameters (of length 1)
  #   dplyr::bind_cols(%s %%>%% purrr::keep(\\(x) length(x) == 1 & !inherits(x, 'function'))) %s
  # ", P$sim_df_name, "ode", P$ode_func_name, P$initial_value_name, P$times_name, P$parameter_name, sfm$sim_specs$method,
  #                    P$sim_df_name,   P$parameter_name, globalpast$add_archive_var)

  script = sprintf("\n\n# Run ODE
%s = as.data.frame(deSolve::%s(
  func=%s,
  y=%s,
  times=%s,
  parms=%s,
  method = '%s'%s
)) %s
", P$sim_df_name, "ode", P$ode_func_name, P$initial_value_name, P$times_name, P$parameter_name, sfm$sim_specs$method,
  nonneg_stocks$root_arg,
  nonneg_stocks$check_root)

  # if (sfm$m)

return(list(script=script))
}



#' Compile script for plotting ODE
#'
#' @inheritParams compile_ode
#'
#' @return List
#' @inheritParams compile_R
#' @noRd
#'
compile_plot_ode = function(sfm){

  # Add sources of graphical functions if necessary
  var_names = get_model_var(sfm)
  plot_var = var_names[var_names %in% sfm$display_var]

  # # Variables to plot: if none were specified, plot Stocks
  # plot_var_str = sprintf("plot_var = c(%s)",
  #                        plot_var %>% paste0("'", ., "'", collapse = ", "))

  # ** add delays deselect?

  script = sprintf("
# Plot ODE
plot_sim(sfm, %s)",
                   # plot_var_str,
                   P$sim_df_name)

  return(list(script=script))
}


