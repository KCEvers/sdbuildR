#' Write a string to a temporary file
#'
#' @param script String containing the code to write
#' @param fileext String with file extension, either ".R" or ".jl"
#' @returns The path to the created file
#'
#' @noRd
write_script <- function(script,
                         filepath) {
  filepath <- norm_path(filepath)

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
  filepath <- norm_path(tempfile(fileext = fileext))
  filepath
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


#' Compile a complete simulation script
#'
#' Unified entry point for both R and Julia non-ensemble script compilation.
#' Calls pre_assemble_components() to ensure the model structure cache is
#' populated, then generates the runtime-specific run_ode and post sections.
#'
#' @inheritParams simulate.stockflow
#' @param filepath_sim Path to output CSV file (Julia only).
#' @param only_stocks Logical; if TRUE, only stock values are saved.
#' @param vars Character vector of variable names to save, or NULL.
#'
#' @returns List with `script` (character) and `object` (updated model with cache).
#' @noRd
compile <- function(object, only_stocks = FALSE,
                    filepath_sim = NULL,
                    vars = NULL) {
  language <- object[["sim_settings"]][["language"]]

  output_args <- resolve_sim_output_args(object, only_stocks, vars)
  only_stocks <- output_args[["only_stocks"]]
  vars <- output_args[["vars"]]

  # Populate the model structure cache (ordering, times, funcs, static, ode, etc.)
  object <- pre_assemble_components(object)

  ordering <- object[["assemble"]][["ordering"]]

  # Adjust only_stocks based on ordering (no dynamic vars → stocks only)
  if (is.null(ordering[["dynamic"]][["order"]])) {
    only_stocks <- TRUE
  }

  # --- R: compile ODE (depends on only_stocks, not pre-assembled) ------------
  if (language == "R") {
    object[["assemble"]][["ode"]] <- compile_ode(object,
      only_stocks = only_stocks,
      language = language,
      is_ensemble = FALSE,
      vars = vars
    )
  }

  ode <- object[["assemble"]][["ode"]]

  # --- Compile run ODE and post ----------------------------------------------
  run_ode <- compile_run_ode(object,
    filepath_sim = filepath_sim,
    only_stocks = only_stocks,
    language = language
  )

  post <- compile_post(object,
    filepath_sim = filepath_sim,
    language = language,
    vars = vars
  )

  # --- Assemble final script -------------------------------------------------
  script <- compile_script_sections(object, ode = ode, run_ode = run_ode, post = post)

  list(script = script, object = object)
}


#' Compile script for creating time vector
#'
#' @returns List
#' @importFrom rlang .data
#' @inheritParams compile
#' @noRd
#'
compile_times <- function(object, language) {
  ss <- object[["sim_settings"]]

  if (language == "R") {
    script <- fmt_script("times", "R", ss)
  } else if (language == "Julia") {
    script <- fmt_script("times", "julia", ss,
      saveat_expr = julia_saveat_expr(ss)
    )
  }

  script
}


#' Compile script for global variables
#'
#' @inheritParams update.stockflow
#'
#' @returns Func script as character string
#' @noRd
compile_funcs <- function(object, language) {
  script <- ""
  func_df <- get_funcs(object)

  if (nrow(func_df) == 0 || !any(nzchar(func_df[["eqn"]]))) {
    return(script)
  }

  lang <- lang_adapter(language)

  # Julia needs var_namesfor equation conversion; R ignores them
  var_names <- if (language == "Julia") get_model_var(object) else NULL

  eqns <- character(nrow(func_df))
  for (i in seq_len(nrow(func_df))) {
    if (nzchar(func_df[i, "eqn"])) {
      eqns[i] <- lang$convert_func_eqn(
        name = func_df[i, "name"],
        eqn = func_df[i, "eqn"],
        var_names = var_names
      )
    }
  }

  eqns[is.na(eqns)] <- ""
  has_eqn <- nzchar(eqns)

  if (any(has_eqn)) {
    func_body <- paste0("\n", paste0(eqns[has_eqn], collapse = "\n"))
    script <- fmt_script("funcs", language, func_body = func_body)
  }

  script
}


#' Compile script for static variables, i.e., initial conditions, functions, and parameters
#'
#' @inheritParams compile
#' @inheritParams order_equations
#'
#' @noRd
#'
#' @returns List with necessary scripts
#'
compile_static <- function(object, language,
                           ordering_override = NULL,
                           ensemble_iter_code = "") {
  intermediaries <- object[["assemble"]][["intermediaries"]]
  ordering <- if (!is.null(ordering_override)) ordering_override else object[["assemble"]][["ordering"]]

  # If ordering for static_and_dynamic is missing, treat as issue and fall back to static only
  if (is.null(ordering[["static_and_dynamic"]][["order"]])) {
    ordering[["static_and_dynamic"]][["issue"]] <- TRUE
  }

  # Extract and order equations (same helper as R branch)
  gathered <- gather_static_equations(object, ordering)
  constant_eqn <- gathered$constant_eqn
  stock_eqn <- gathered$stock_eqn
  gf_eqn <- gathered$gf_eqn
  static_str <- gathered$str

  if (language == "R") {
    # Put parameters together
    if (length(constant_eqn) > 0) {
      const_list_items <- paste0(names(constant_eqn), " = ", names(constant_eqn))
      constants_def <- paste0(
        "\n\n# Define parameters in named list\n", P[["parameter_name"]],
        " = list(", paste0(const_list_items, collapse = ", "), ")\n"
      )
    } else {
      constants_def <- paste0("\n\n# Define empty parameters\n", P[["parameter_name"]], " = list()\n")
    }

    # Define init
    init_list_items <- paste0(names(stock_eqn), " = ", names(stock_eqn))
    init_def <- paste0(
      "\n\n# Define initial condition\n", P[["initial_value_name"]], " = c(",
      paste0(init_list_items, collapse = ", "), ")"
    )

    script <- fmt_script("static", "R",
      static_str = static_str,
      constants_def = constants_def,
      init_def = init_def
    )
  } else if (language == "Julia") {
    ensemble_iter <- ensemble_iter_code

    #** removed:
    #   # Re-gather with possibly modified ordering (after ensemble var removal)
    #   gathered <- gather_static_equations(object, ordering)
    #   static_str <- gathered$str

    # Replace any reference to model_setup.intermediary_names with intermediary_names
    static_str <- gsub(
      paste0(P[["model_setup_name"]], "\\.", P[["intermediary_names"]]),
      P[["intermediary_names"]], static_str
    )


    # Put parameters together in named tuple; include graphical functions as otherwise these are not defined outside of the let block
    #   if (nrow(get_variables_by_type(object, "constant")) > 0 || nrow(get_variables_by_type(object, "gf")) > 0) {
    if (length(constant_eqn) > 0 || length(gf_eqn) > 0) {
      pars_def <- paste0(
        "\n\n# Define parameters in named tuple\n",
        P[["parameter_name"]], " = (",
        paste0(c(names(constant_eqn), names(gf_eqn)), " = ",
          c(names(constant_eqn), names(gf_eqn)),
          collapse = ", "
        ), ",)\n"
      )
    } else {
      pars_def <- paste0("\n\n# Define empty parameters\n", P[["parameter_name"]], " = ()\n")
    }

    init_def_stocks <- paste0(names(stock_eqn), collapse = ", ")
    # Symbols are faster than characters
    init_names <- paste0(paste0(":", names(stock_eqn)), collapse = ", ")
    init_idx <- ""

    # Put initial states together in (unnamed) vector
    init_def <- paste0(
      "\n# Define initial condition in vector\n",
      P[["initial_value_name"]],
      " = [Base.Iterators.flatten(",
      "[",
      init_def_stocks,
      # Add extra comma in case there is only one stock
      ",]",
      ")...]\n"
    )

    init_names <- paste0(
      P[["initial_value_names"]], " = [",
      init_names,
      "]\n"
    )


    if (length(intermediaries[["names"]]) > 0) {
      intermediary_names <- paste0(P[["intermediary_names"]], " = [", paste0(
        paste0(
          ":",
          intermediaries[["names"]]
        ),
        collapse = ", "
      ), "]\n")

      # Keep all intermediary names; skip function filtering to avoid undefined references
      intermediary_names_correct <- paste0(
        "\n# Keep all intermediary names (skip function filtering)\n",
        "is_not_function = trues(length(", P[["intermediary_names"]], "))\n",
        P[["intermediary_names"]], " = ", P[["intermediary_names"]], "[is_not_function]\n"
      )
    } else {
      intermediary_names <- paste0(P[["intermediary_names"]], " = Nothing\n")
      intermediary_names_correct <- ""
    }

    script <- fmt_script("static", "Julia",
      ensemble_iter_code = ensemble_iter,
      intermediary_names_str = intermediary_names,
      static_str = static_str,
      pars_def = pars_def,
      init_def = init_def,
      init_names_str = init_names,
      init_idx = init_idx,
      intermediary_names_correct = intermediary_names_correct
    )
  }


  list(
    script = script,
    par_names = c(names(constant_eqn), names(gf_eqn))
  )
}


#' Compile script for non-negative stocks
#'
#' @inheritParams update.stockflow
#' @inheritParams compile
#'
#' @noRd
#' @returns List with necessary scripts for ensuring non-negative stocks
#'
compile_nonneg_stocks <- function(object, language) {
  keep_nonnegative_stock <- object[["sim_settings"]][["keep_nonnegative_stock"]]
  nonneg_stocks <- empty_nonneg_stocks()
  scripts <- prep_script_template()


  if (language == "R") {
    # Non-negative stocks
    stock_df <- object[["variables"]][object[["variables"]][["type"]] == "stock", ]
    nonneg_idx <- which(stock_df[["non_negative"]])

    if (keep_nonnegative_stock && length(nonneg_idx) > 0) {
      nonneg_stock_names <- stock_df[nonneg_idx, "name"]
      quoted_names <- paste0("'", nonneg_stock_names, "'", collapse = ", ")

      func_def <- fmt_script("nonneg_stocks", "R",
        nonneg_stock_names_quoted = quoted_names
      )

      root_arg <- scripts[["nonneg_root_arg_r"]][["template"]]

      check_root <- scripts[["nonneg_check_root_r"]][["template"]]

      nonneg_stocks[["func_def"]] <- func_def
      nonneg_stocks[["root_arg"]] <- root_arg
      nonneg_stocks[["check_root"]] <- check_root
    }
  }

  #    else if (language == "Julia"){
  #   **check: not supported in julia or inline?
  # }

  nonneg_stocks
}


#' Compile script for running ODE
#'
#' @param nonneg_stocks Output of compile_nonneg_stocks()
#' @inheritParams compile_ode
#'
#' @returns List
#' @inheritParams compile
#' @noRd
#'
compile_run_ode <- function(
  object,
  filepath_sim = NULL,
  only_stocks = NULL,
  language
) {
  nonneg_stocks <- object[["assemble"]][["nonneg_stocks"]]
  intermediaries <- object[["assemble"]][["intermediaries"]]
  save_intermediaries <- length(intermediaries[["names"]]) > 0

  if (language == "R") {
    script <- fmt_script("run_ode", language,
      method = r_solver_arg(object[["sim_settings"]][["method"]]),
      root_arg = nonneg_stocks[["root_arg"]],
      check_root = nonneg_stocks[["check_root"]]
    )
  } else if (language == "Julia") {
    callback_arg <- ifelse(save_intermediaries,
      paste0(", ", P[["callback_name"]], " = ", P[["callback_name"]]),
      ""
    )

    script <- fmt_script("run_ode", language,
      method = object[["sim_settings"]][["method"]],
      callback_arg = callback_arg
    )
  }

  script
}


compile_post <- function(object, filepath_sim = NULL, language, vars = NULL) {
  intermediaries <- object[["assemble"]][["intermediaries"]]
  save_intermediaries <- length(intermediaries[["names"]]) > 0

  if (language == "R") {
    # Process ODE output
    script <- fmt_script("post_ode", language,
      saveat_script = r_saveat_script(object[["sim_settings"]])
    )
  } else if (language == "Julia") {
    intermediaries_or_nothing <- ifelse(save_intermediaries, P[["intermediaries"]], "nothing")
    selection <- julia_output_selection_args(object, vars)

    script <- fmt_script("post_ode", language,
      intermediaries_or_nothing = intermediaries_or_nothing,
      filepath_sim = filepath_sim,
      intermediary_names_arg = selection[["intermediary_names_arg"]],
      save_idx_arg = selection[["save_idx_arg"]],
      selected_var_names_arg = selection[["selected_var_names_arg"]]
    )
  }

  script
}


#' Compile a complete ensemble simulation script
#'
#' Calls pre_assemble_components() to reuse the model structure cache, then
#' builds the ensemble-specific portions (ensemble_def, ensemble_iter,
#' run_ode, post) on top.
#'
#' @inheritParams simulate.stockflow
#' @inheritParams ensemble
#' @param ensemble_pars List of ensemble parameters constructed by ensemble().
#'
#' @returns List with `script` (character) and `object` (updated model).
#' @noRd
compile_ensemble <- function(object, ensemble_pars, only_stocks = TRUE) {
  language <- object[["sim_settings"]][["language"]]

  # Ensure base cache is populated — reuses update() cache if already done
  object <- pre_assemble_components(object)

  # Reformat conditions values as Julia float literals (does not modify object)
  out <- prep_ensemble_conditions(object, ensemble_pars)
  object <- out[["object"]]
  ensemble_pars <- out[["ensemble_pars"]]
  rm(out)

  ordering <- object[["assemble"]][["ordering"]]

  # Adjust only_stocks based on ordering (no dynamic vars → stocks only)
  if (is.null(ordering[["dynamic"]][["order"]])) {
    only_stocks <- TRUE
  }

  # Ensemble-specific static: conditions vars excluded, ensemble_def/iter prepended
  static_ens <- compile_static_ensemble(object, ensemble_pars)

  # ODE without callback_setup (recreated per member inside run_ode_ensemble)
  ode <- compile_ode(object, only_stocks, language, is_ensemble = TRUE)

  # Ensemble run_ode and post-processing
  run_ode <- compile_run_ode_ensemble(object, ensemble_pars, static_ens, only_stocks)
  post <- compile_post_ensemble(object, ensemble_pars)

  script <- paste0(c(
    object[["assemble"]][["times"]],
    object[["assemble"]][["funcs"]],
    ode,
    static_ens[["script"]],
    run_ode,
    post
  ), collapse = "\n")

  list(script = script, object = object)
}


#' Compile script for ODE function passed to deSolve::ode
#'
#' @inheritParams update.stockflow
#' @inheritParams compile
#' @inheritParams order_equations
#' @inheritParams compile_static
#'
#' @returns List
#' @importFrom rlang .data
#' @noRd
#'
compile_ode <- function(object,
                        only_stocks,
                        language,
                        is_ensemble = FALSE,
                        vars = NULL) {
  keep_nonnegative_stock <- object[["sim_settings"]][["keep_nonnegative_stock"]]

  ordering <- object[["assemble"]][["ordering"]]
  static <- object[["assemble"]][["static"]]
  intermediaries <- object[["assemble"]][["intermediaries"]]
  save_intermediaries <- length(intermediaries[["names"]]) > 0

  lang <- lang_adapter(language)

  # Get and order dynamic equations
  dynamic <- gather_dynamic_equations(object, ordering, separator = "\n\t\t")
  dynamic_eqn <- dynamic$eqns
  dynamic_eqn_str <- dynamic$str

  if (language == "R") {
    # Sum change in stock equations
    stock_change <- gather_stock_changes(object, assign_op = lang$assign_op, language = language)

    # Compile stock changes in one string
    stock_change_str <- paste0(stock_change, collapse = "\n\t\t")

    # Get names of summed change in stocks from data frame
    stock_df_all <- object[["variables"]][object[["variables"]][["type"]] == "stock", ]
    stock_changes_names <- stock_df_all[["sum_name"]]

    state_change_str <- paste0(
      P[["change_state_name"]], " = c(",
      paste0(unname(stock_changes_names), collapse = ", "), ")"
    )

    # Graphical functions (gf)
    gf_str <- build_gf_return_str(object)

    # Save all variables in return statement
    if (!only_stocks) {
      # Filter out functions in case they are in auxiliaries
      if (length(names(dynamic_eqn)) > 0 || nzchar(gf_str)) {
        selected_dynamic_names <- names(dynamic_eqn)
        if (!is.null(vars)) {
          selected_dynamic_names <- vars[vars %in% selected_dynamic_names]
        }

        # Build variable name assignments
        var_assignments <- ""
        if (length(selected_dynamic_names) > 0) {
          var_assignments <- paste0(paste0(selected_dynamic_names, " = ", selected_dynamic_names), collapse = ", ")
        }

        include_gf <- nzchar(gf_str) && is.null(vars)
        save_var_str <- paste0(
          ", Filter(Negate(is.function), c(",
          var_assignments, ifelse(include_gf && nzchar(var_assignments), ", ", ""),
          ifelse(include_gf, gf_str, ""), "))"
        )

        if (!nzchar(var_assignments) && !include_gf) {
          save_var_str <- ""
        }
      } else {
        # No variables to save (isolated stocks only)
        save_var_str <- ""
      }
    } else {
      save_var_str <- ""
    }

    S_str <- sprintf("%s = as.list(%s)", P[["state_name"]], P[["state_name"]])

    # Compile
    script <- fmt_script("ode", language,
      S_str = S_str,
      dynamic_eqn_str = dynamic_eqn_str,
      stock_change_str = stock_change_str,
      state_change_str = state_change_str,
      save_var_str = save_var_str
    )
  } else if (language == "Julia") {
    # Sum change in stock equations
    stock_change <- gather_stock_changes(object, assign_op = lang$assign_op, language = language)
    stock_change_str <- paste0(stock_change, collapse = "\n\t")

    # Non-negative stocks
    stock_vars_df <- get_variables_by_type(object, "stock")
    nonneg_stocks <- stock_vars_df[["non_negative"]] |> unlist()
    add_nonneg <- any(nonneg_stocks) & keep_nonnegative_stock

    if (add_nonneg) {
      # Create if-statement to keep selected stocks non-negative
      nonneg_str <- lapply(
        seq_len(nrow(stock_vars_df)),
        function(i) {
          x <- as.list(stock_vars_df[i, ])
          if (x[["non_negative"]]) {
            sprintf(
              "if (%s * %s + %s < 0) %s = %s/%s end",
              x[["sum_name"]], P[["timestep_name"]], x[["name"]],
              x[["sum_name"]], x[["name"]], P[["timestep_name"]]
            )
          } else {
            return(NULL)
          }
        }
      ) |> compact_()

      # Format complete string with explanation
      stock_change_str <- paste0(
        stock_change_str,
        "\n\n\t# Prevent ", paste0(names(nonneg_str), collapse = ", "),
        " from turning negative\n\t",
        paste0(nonneg_str, collapse = "\n\t")
      )
    }

    unpack_state_str <- paste0(
      paste0(names(stock_change), collapse = ", "),
      ", = ", P[["state_name"]]
    )

    # Unpack parameters string
    if (length(static[["par_names"]])) {
      unpack_pars_str <- paste0(
        "\n\n\t# Unpack parameters\n\t",
        paste0(static[["par_names"]], collapse = ", "),
        ", = ", P[["parameter_name"]]
      )
    } else {
      unpack_pars_str <- ""
    }


    # Compile ODE
    script_ode <- fmt_script("ode", "Julia",
      unpack_state_str = unpack_state_str,
      unpack_pars_str = unpack_pars_str,
      dynamic_eqn_str = dynamic_eqn_str,
      stock_change_str = stock_change_str
      # nonneg_stocks
    )

    # Compile callback function
    if (!save_intermediaries) {
      script_callback <- fmt_script("callback_empty", "Julia")
    } else {
      # Unpack parameters from integrator
      if (length(static[["par_names"]])) {
        unpack_pars_integrator_str <- paste0(
          "\n\n\t# Get parameters from integrator\n\t",
          P[["parameter_name"]], " = integrator.p",
          "\n\n\t# Unpack parameters\n\t",
          paste0(static[["par_names"]], collapse = ", "),
          ", = ", P[["parameter_name"]]
        )
      } else {
        unpack_pars_integrator_str <- ""
      }


      intermediary_values <- paste0(
        paste0(intermediaries[["values"]], collapse = ", "),
        ifelse(length(intermediaries[["values"]]) == 1, ",", "")
      )

      # Callback setup (only define if not ensemble)
      callback_setup <- ifelse(is_ensemble, "", paste0(
        P[["intermediaries"]], " = SavedValues(",
        "typeof(float(", P[["time_name"]], ")), Any)\n",
        P[["callback_name"]], " = SavingCallback(",
        P[["callback_func_name"]], ", ", P[["intermediaries"]],
        ", saveat = ", P[["saveat_name"]],
        ")\n"
      ))

      script_callback <- fmt_script("callback", "Julia",
        unpack_state_str = unpack_state_str,
        unpack_pars_integrator_str = unpack_pars_integrator_str,
        dynamic_eqn_str = dynamic_eqn_str,
        intermediary_values = intermediary_values,
        callback_setup = callback_setup
      )
    }

    script <- paste0(script_ode, "\n\n", script_callback)
  }

  script
}
