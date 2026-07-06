#' Simulate stock-and-flow model in Julia
#'
#' @inheritParams simulate.stockflow
#'
#' @returns List with variables created in the simulation script
#' @noRd
#'
simulate_julia <- function(object,
                           only_stocks,
                           vars = NULL,
                           quiet = FALSE) {
  # Get output filepaths
  filepath_sim <- get_tempfile(fileext = ".csv")
  filepath <- get_tempfile(fileext = ".jl")

  # Compile script - returns list with script and modified object
  result <- compile(object,
    filepath_sim = filepath_sim,
    only_stocks = only_stocks,
    vars = vars
  )

  script <- result$script
  object <- result$object # Get updated object with cache populated
  seed_nr <- object[["sim_settings"]][["seed"]]

  on.exit(
    # Ensure files are deleted even if an error occurs (both the .jl script and
    # the .csv Julia writes the results to)
    remove_files(c(filepath, filepath_sim)),
    add = TRUE
  )

  write_script(script, filepath)
  script <- paste0(readLines(filepath), collapse = "\n")

  use_julia(quiet = quiet)

  # Evaluate script
  sim <- tryCatch(
    {
      # Evaluate script
      start_t <- Sys.time()

      out <- julia_eval(jl_include_command(filepath, seed_nr))

      end_t <- Sys.time()

      # if (verbose) {
      #   elapsed <- round(as.numeric(end_t) - as.numeric(start_t), 4)
      #   cli::cli_inform(c(
      #     "v" = "Simulation completed in {.val {elapsed}} seconds."
      #   ))
      # }

      # Read the constants
      constants <- as.numeric(julia_eval(P[["parameter_name"]]))
      names(constants) <- julia_eval(P[["parameter_names"]])

      # Read the initial values of stocks
      init <- as.numeric(julia_eval(P[["initial_value_name"]]))
      names(init) <- julia_eval(P[["initial_value_names"]])

      # Read the simulation results dataframe
      df <- read_sim_csv(filepath_sim)
      df <- filter_sim_df_vars(df, vars)

      new_simulate_stockflow(
        success = TRUE,
        object = object, # Return object with cache
        df = df,
        init = init,
        constants = constants,
        script = script,
        duration = end_t - start_t
      )
    },
    error = function(e) {
      warning("\nAn error occurred while running the Julia script.")
      new_simulate_stockflow(
        success = FALSE,
        error_message = e[["message"]],
        script = script,
        object = object
      )
    }
  )

  sim
}


#' Check for keyword arguments in Julia translated equation
#'
#' Check whether any variable names are used as functions with keyword arguments in the Julia translated equation
#'
#' @inheritParams update.stockflow
#' @param var_names Character vector; variable names in the model.
#'
#' @returns Returns `NULL`, called for side effects.
#' @noRd
check_no_keyword_arg <- function(object, var_names) {
  # Check for all variable names if they are used as functions. If so, throw error if they are used with keyword arguments in the Julia translated equation.

  # Batch convert equations to Julia format for checking
  eqns <- character(nrow(object[["variables"]]))
  for (i in seq_len(nrow(object[["variables"]]))) {
    if (object[["variables"]][i, "type"] %in% c("stock", "flow", "constant", "aux")) {
      result <- convert_equations_julia(
        type = object[["variables"]][i, "type"],
        name = object[["variables"]][i, "name"],
        eqn = object[["variables"]][i, "eqn"],
        var_names = var_names
      )
      eqns[i] <- result[["eqn"]]
    }
  }

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
      cli::cli_abort(
        c(
          "!" = paste0(
            "The following variables were used as functions with named arguments in the Julia translated equation: ",
            paste0(names(named_idxs)[unname(named_idxs)], collapse = ", "), ".\n",
            "This is not allowed in Julia when using positional arguments."
          ),
          ">" = "Modify the equation(s) to not use named arguments, ensuring arguments are ordered correctly."
        ),
        call. = FALSE
      )
    }
  }

  invisible(NULL)
}


#' Prepare stock-and-flow model for ensemble conditions
#'
#' @inheritParams update.stockflow
#' @inheritParams compile
#'
#' @returns List with updated stock-and-flow model and updated ensemble parameters
#' @noRd
prep_ensemble_conditions <- function(object, ensemble_pars) {
  if (!is.null(ensemble_pars[["conditions"]])) {
    # Prepare the conditions for Julia
    ensemble_pars[["conditions"]] <- lapply(
      ensemble_pars[["conditions"]],
      function(vec) {
        replace_digits_with_floats(
          paste0("[", paste0(vec, collapse = ", "), "]"),
          # No variable names to account for
          NULL
        )
      }
    )

    # Ensemble variables will be removed from ordering in compile_static
    # No need to modify their equations here
  }

  return(list(object = object, ensemble_pars = ensemble_pars))
}


#' Prepare intermediary variables
#'
#' @inheritParams update.stockflow
#' @inheritParams compile_static
#'
#' @returns List of intermediary variables and values
#' @noRd
prep_intermediary_variables <- function(object, language) {
  ordering <- object[["assemble"]][["ordering"]]

  if (language == "R") {
    cli::cli_abort(c("x" = "prep_intermediary_variables() is not implemented for R."), call. = FALSE)
  } else if (language == "Julia") {
    # Create separate vector for names of intermediate variables and values, because graphical functions need to be in the intermediate function as gf(t), but their name should be gf
    intermediary_var <- intermediary_var_values <- ordering[["dynamic"]][["order"]]

    # Graphical functions (gf) — add gf(source) to intermediary variables
    gf_dict <- build_gf_source_dict(object)
    if (!is.null(gf_dict)) {
      intermediary_var <- c(intermediary_var, names(gf_dict))
      intermediary_var_values <- c(intermediary_var_values, unname(gf_dict))
    }

    # Order intermediary variables and values alphabetically
    if (length(intermediary_var) > 0) {
      idx <- order(intermediary_var)
      intermediary_var <- intermediary_var[idx]
      intermediary_var_values <- intermediary_var_values[idx]
    }

    list(
      names = intermediary_var,
      values = intermediary_var_values
    )
  }
}


# ==============================================================================
# Ensemble-specific script generators
# ==============================================================================

#' Compile static script with ensemble modifications
#'
#' Wraps compile_static() with ensemble-specific ordering (conditions vars
#' excluded) and prepends the ensemble_def block.
#'
#' @param object Stock-and-flow model
#' @param ensemble_pars List of ensemble parameters
#'
#' @returns List with `script`, `script_prob_func`, and `par_names`
#' @noRd
compile_static_ensemble <- function(object, ensemble_pars) {
  ordering <- object[["assemble"]][["ordering"]]

  # Remove ensemble conditions variables from the ordering so they are not
  # included in the static equations (they are defined by the ensemble loop)
  if (length(ensemble_pars[["conditions"]]) > 0) {
    conditions_names <- names(ensemble_pars[["conditions"]])
    ordering[["static"]][["order"]] <- setdiff(ordering[["static"]][["order"]], conditions_names)
    ordering[["static_and_dynamic"]][["order"]] <- setdiff(ordering[["static_and_dynamic"]][["order"]], conditions_names)

    conditions_items <- paste0(
      names(ensemble_pars[["conditions"]]), " = ",
      unname(ensemble_pars[["conditions"]]),
      collapse = ",\n"
    )
    ensemble_def <- fmt_script("ensemble_def_conditions", "Julia",
      n_value = format(ensemble_pars[["n"]], scientific = FALSE),
      conditions_items = conditions_items,
      crossed = ifelse(ensemble_pars[["cross"]], "true", "false")
    )
    ensemble_iter <- fmt_script("ensemble_iter", "Julia",
      conditions_names = paste0(names(ensemble_pars[["conditions"]]), collapse = ", ")
    )
  } else {
    ensemble_def <- fmt_script("ensemble_def_noconditions", "Julia",
      n_value = format(ensemble_pars[["n"]], scientific = FALSE)
    )
    ensemble_iter <- ""
  }

  result <- compile_static(object, "Julia",
    ordering_override = ordering,
    ensemble_iter_code = ensemble_iter
  )
  static_script <- result[["script"]]
  result[["script_prob_func"]] <- static_script
  result[["script"]] <- paste0(ensemble_def, "\n\n", static_script)
  result
}


#' Compile run_ode script for ensemble simulation
#'
#' Generates the ensemble problem, output function, and solve call for Julia.
#'
#' @param object Stock-and-flow model
#' @param ensemble_pars List of ensemble parameters
#' @param static Result of compile_static_ensemble()
#' @param only_stocks Logical; if TRUE, only stock values are saved
#'
#' @returns Character string with the run_ode script
#' @noRd
compile_run_ode_ensemble <- function(object, ensemble_pars, static, only_stocks) {
  intermediaries <- object[["assemble"]][["intermediaries"]]
  save_intermediaries <- length(intermediaries[["names"]]) > 0

  intermediaries_setup <- ""
  intermediaries_callback <- ""
  intermediaries_remake <- ""

  if (save_intermediaries) {
    intermediaries_setup <- paste0(
      "\n\n# Set up intermediaries for saving in callback\n",
      P[["intermediaries"]],
      " = Vector{SavedValues{typeof(float(",
      P[["time_name"]], ")), Any}}(undef, ",
      P[["ensemble_total_n"]],
      ")\n\n# Populate the vector above with something to avoid undef\n",
      "for ", P[["ensemble_iter"]], " in eachindex(",
      P[["intermediaries"]], ")\n\t",
      P[["intermediaries"]], "[",
      P[["ensemble_iter"]], "] = SavedValues(typeof(float(",
      P[["time_name"]], ")), Any)\nend\n\n"
    )
    intermediaries_callback <- paste0(
      "\n\t", P[["callback_name"]],
      " = SavingCallback(",
      P[["callback_func_name"]], ", ",
      P[["intermediaries"]], "[",
      P[["ensemble_iter"]], "], saveat = ",
      P[["saveat_name"]], ")\n"
    )
    intermediaries_remake <- paste0(
      ", ", P[["callback_name"]], " = ", P[["callback_name"]]
    )
  }

  # Seeding is applied by wrapping the whole script in with_rng() at include
  # time (see jl_include_command()), so the per-iteration template hooks are
  # left empty here.
  use_with_rng_open <- use_with_rng_close <- ""

  # When using EnsembleThreads(), add a warm-up solve to force JIT compilation
  # of all solver code paths before threads are spawned. This prevents a known
  # Julia GC crash (EXCEPTION_ACCESS_VIOLATION in jl_gc_small_alloc) caused by
  # concurrent JIT compilation during threaded ensemble solving.
  if (ensemble_pars[["threaded"]]) {
    warmup_str <- paste0(
      "# Warm-up: pre-compile solver to avoid GC crash during threaded JIT\n",
      "solve(", P[["prob_name"]], ", ",
      object[["sim_settings"]][["method"]],
      ", dt = ", P[["timestep_name"]],
      ", saveat = [", P[["times_name"]], "[2]]",
      ", tstops = ", P[["tstops_name"]],
      ", adaptive = false)\n",
      "GC.gc()\n\n"
    )
  } else {
    warmup_str <- ""
  }

  fmt_script("ensemble_prob", "Julia",
    intermediaries_setup = intermediaries_setup,
    static_str = static[["script_prob_func"]],
    intermediaries_callback = intermediaries_callback,
    intermediaries_remake = intermediaries_remake,
    method = object[["sim_settings"]][["method"]],
    threaded_str = ifelse(ensemble_pars[["threaded"]], ", EnsembleThreads()", ""),
    warmup_str = warmup_str,
    use_with_rng_open = use_with_rng_open,
    use_with_rng_close = use_with_rng_close
  )
}


#' Compile post-processing script for ensemble simulation
#'
#' Generates the ensemble save, CSV output, and summary statistics scripts.
#'
#' @param object Stock-and-flow model
#' @param ensemble_pars List of ensemble parameters
#'
#' @returns Character string with the post script
#' @noRd
compile_post_ensemble <- function(object, ensemble_pars) {
  script <- ""

  ensemble_to_df_func <- ifelse(ensemble_pars[["threaded"]],
    " = ensemble_to_df_threaded(", " = ensemble_to_df("
  )
  script <- paste0(script, fmt_script("ensemble_save", "Julia",
    ensemble_to_df_func = ensemble_to_df_func
  ))

  if (ensemble_pars[["save_sims"]]) {
    script <- paste0(script, fmt_script("ensemble_csv", "Julia",
      filepath_df = ensemble_pars[["filepath_df"]][["df"]],
      filepath_constants = ensemble_pars[["filepath_df"]][["constants"]],
      filepath_init = ensemble_pars[["filepath_df"]][["init"]]
    ))
  }

  ensemble_summ_func <- ifelse(ensemble_pars[["threaded"]],
    " = ensemble_summ_threaded(", " = ensemble_summ("
  )
  # Render the requested stats as a Julia string array, e.g. "mean", "median"
  summary_stats_jl <- paste0(
    "\"", ensemble_pars[["summary_stats"]], "\"",
    collapse = ", "
  )
  script <- paste0(script, fmt_script("ensemble_summary", "Julia",
    ensemble_summ_func = ensemble_summ_func,
    quantiles = paste0(ensemble_pars[["quantiles"]], collapse = ", "),
    summary_stats = summary_stats_jl,
    filepath_summary_df = ensemble_pars[["filepath_summary"]][["df"]],
    filepath_summary_constants = ensemble_pars[["filepath_summary"]][["constants"]],
    filepath_summary_init = ensemble_pars[["filepath_summary"]][["init"]]
  ))

  script
}
