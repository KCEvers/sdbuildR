#' Script templates for code generation
#'
#' Templates use `%(key)s` placeholders that get substituted via fmt().
#'
#' @noRd
script_template <- function() {
  list(
    # -- compile_times ---------------------------------------------------------
    times_r = "
# Define time sequence
%(timestep_name)s = %(dt)s
%(times_name)s <- seq(from=%(start)s, to=%(stop)s, by=%(timestep_name)s)
%(time_name)s = %(times_name)s[1]

",
    times_julia = "\n
# Define time sequence
%(times_name)s = (%(start)s, %(stop)s)
# Initialize time (only necessary if constants use t)
%(time_name)s = %(times_name)s[1]
# Time step
%(timestep_name)s = %(dt)s
# Define saving time sequence
%(tstops_name)s = %(times_name)s[1]:%(timestep_name)s:%(times_name)s[2]
%(saveat_name)s = %(saveat_expr)s\n",

    # -- compile_nonneg_stocks -------------------------------------------------

    nonneg_stocks_r = "
# Ensure non-negativity of (selected) stocks
%(nonneg_stock_name)s = which(names(%(initial_value_name)s) %in% c(%(nonneg_stock_names_quoted)s))

# Define root function to be triggered when non-negative stocks go below 0
%(rootfun_name)s <- function (%(time_name)s, %(state_name)s, %(parameter_name)s) {
  return(ifelse(any(%(state_name)s[%(nonneg_stock_name)s] < 0), 0, 1))
}

# Set non-negative stocks to zero when root function is triggered
%(eventfun_name)s <- function(%(time_name)s, %(state_name)s, %(parameter_name)s) {
  %(state_name)s[%(state_name)s[%(nonneg_stock_name)s] < 0] = 0
  return(%(state_name)s)
}
",
    nonneg_root_arg_r = ",\n\t\t\t\tevents = list(func = %(eventfun_name)s, root = TRUE), rootfun = %(rootfun_name)s",
    nonneg_check_root_r = "
# Times at which non-negative stocks fell below 0
attributes(%(sim_df_name)s)$troot

# Values of non-negative stocks when root function was triggered
attributes(%(sim_df_name)s)$valroot
",

    # -- compile_funcs ----------------------------------------------------------

    funcs_r = "\n\n# User-specified funcs\n%(func_body)s\n",
    funcs_julia = "\n\n# User-specified funcs\n%(func_body)s\n\n",

    # -- compile_static ----------------------------------------------------

    static_r = "\n\n# Define parameters, initial conditions, and functions in correct order

%(static_str)s%(constants_def)s%(init_def)s",
    static_julia = "\n\n# Define parameters, initial conditions, and functions in correct order

%(model_setup_name)s = let
%(ensemble_iter_code)s%(intermediary_names_str)s%(static_str)s%(pars_def)s%(init_def)s%(init_names_str)s%(init_idx)s%(intermediary_names_correct)s
  (%(parameter_name)s = %(parameter_name)s, %(initial_value_name)s = %(initial_value_name)s, %(initial_value_names)s = %(initial_value_names)s, %(intermediary_names)s = %(intermediary_names)s)

end
",

    # -- compile_ode -----------------------------------------------------------

    ode_r = "\n\n# Define ODE\n%(ode_func_name)s = function(%(time_name)s, %(state_name)s, %(parameter_name)s){

    %(S_str)s

    # Compute change in stocks at current time %(time_name)s
    with(c(%(state_name)s, %(parameter_name)s), {

        # Update auxiliaries and flows
        %(dynamic_eqn_str)s

        # Collect inflows and outflows for each stock
        %(stock_change_str)s

        # Combine change in stocks
        %(state_change_str)s

        return(list(%(change_state_name)s%(save_var_str)s))
      })
      }",
    ode_julia = "\n\n# Define ODE
  function %(ode_func_name)s!(%(change_state_name)s, %(state_name)s, %(parameter_name)s, %(time_name)s)

    # Unpack state variables
    %(unpack_state_str)s%(unpack_pars_str)s

    # Update auxiliaries
    %(dynamic_eqn_str)s

    # Collect inflows and outflows for each stock
    %(stock_change_str)s

    nothing
    end
    ",
    callback_julia = "\n\n# Define callback function\nfunction %(callback_func_name)s(%(state_name)s, %(time_name)s, integrator)\n\n\t# Unpack state variables\n\t%(unpack_state_str)s%(unpack_pars_integrator_str)s\n\n\t# Update auxiliaries\n\t%(dynamic_eqn_str)s\n\n\t# Return intermediary values and remove functions\n\treturn filter(x -> !is_function_or_interp(x), (%(intermediary_values)s))\n\n\nend\n\n# Callback setup\n%(callback_setup)s",
    callback_empty_julia = "\n\n# Define empty callback function\n%(intermediaries)s = nothing\n%(callback_name)s = nothing\n\n",

    # -- compile_run_ode -------------------------------------------------------

    run_ode_r = "\n\n# Run ODE\n%(sim_df_name)s = as.data.frame(deSolve::ode(\n  func=%(ode_func_name)s,\n  y=%(initial_value_name)s,\n  times=%(times_name)s,\n  parms=%(parameter_name)s,\n  method = '%(method)s'%(root_arg)s\n)) %(check_root)s\n",
    post_ode_r = "%(saveat_script)s# Wide to long (fast manual melt; equivalent to stats::reshape but avoids its per-call overhead)\n    %(sim_df_name)s <- as.data.frame(%(sim_df_name)s)\n    .sdb_melt_vars <- colnames(%(sim_df_name)s)[colnames(%(sim_df_name)s) != \"time\"]\n    .sdb_melt_n <- nrow(%(sim_df_name)s)\n    %(sim_df_name)s <- data.frame(\n       time = rep(%(sim_df_name)s[[\"time\"]], times = length(.sdb_melt_vars)),\n       variable = rep(.sdb_melt_vars, each = .sdb_melt_n),\n       value = unlist(%(sim_df_name)s[.sdb_melt_vars], use.names = FALSE),\n       stringsAsFactors = FALSE\n     )\nrownames(%(sim_df_name)s) <- NULL",
    run_ode_julia = "
# Run ODE
%(prob_name)s = ODEProblem(%(ode_func_name)s!, %(model_setup_name)s.%(initial_value_name)s, %(times_name)s, %(model_setup_name)s.%(parameter_name)s)

%(solution_name)s = Base.invokelatest(solve, %(prob_name)s, %(method)s, dt = %(timestep_name)s, saveat = %(saveat_name)s, tstops = %(tstops_name)s, adaptive = false%(callback_arg)s)
",
    post_ode_julia = "
global %(sim_df_name)s, %(parameter_name)s, %(parameter_names)s, %(initial_value_name)s, %(initial_value_names)s
global clean_df_ok = false
try
  global %(sim_df_name)s, %(parameter_name)s, %(parameter_names)s, %(initial_value_name)s, %(initial_value_names)s = clean_df(%(prob_name)s, %(solution_name)s, %(model_setup_name)s.%(initial_value_names)s, %(intermediaries_or_nothing)s, %(intermediary_names_arg)s, %(save_idx_arg)s)
  global clean_df_ok = true
catch
  try
      global %(sim_df_name)s, %(parameter_name)s, %(parameter_names)s, %(initial_value_name)s, %(initial_value_names)s = clean_df(%(prob_name)s, %(solution_name)s, %(model_setup_name)s.%(initial_value_names)s, %(intermediaries_or_nothing)s, %(model_setup_name)s.%(intermediary_names)s)

      global clean_df_ok = true

    catch

      global clean_df_ok = false
      end
  end

if !clean_df_ok
  error(\"Failed to convert Julia solution to dataframe with clean_df\")
end

if %(selected_var_names_arg)s !== nothing
  selected_var_names = Set(String.(%(selected_var_names_arg)s))

  filter!(row -> row.variable in selected_var_names, %(sim_df_name)s)
end

CSV.write(\"%(filepath_sim)s\", %(sim_df_name)s)

# Delete variables
%(solution_name)s = Nothing
%(sim_df_name)s = Nothing
Nothing",

    # -- compile_run_ode: R saveat interpolation --------------------------------

    saveat_interval_r = "\n# Save at interval\nnew_times = seq(%(start)s, %(stop)s, by = %(save_by_val)s)\n%(sim_df_name)s = %(saveat_func)s(%(sim_df_name)s, 'time', new_times)\n",
    saveat_length_r = "\n# Save a fixed number of evenly-spaced points\nnew_times = seq(%(start)s, %(stop)s, length.out = %(save_length_val)s)\n%(sim_df_name)s = %(saveat_func)s(%(sim_df_name)s, 'time', new_times)\n",
    saveat_length1_r = "\n# Save only stop\nnew_times = c(%(stop)s)\n%(sim_df_name)s = %(saveat_func)s(%(sim_df_name)s, 'time', new_times)\n",
    saveat_explicit_r = "\n# Explicit save times\nnew_times = c(%(save_times_str)s)\n%(sim_df_name)s = %(saveat_func)s(%(sim_df_name)s, 'time', new_times)\n",

    # -- compile_static: Julia ensemble definition --------------------------

    ensemble_def_conditions_julia = "

# Generate ensemble design
%(ensemble_n)s = %(n_value)s
%(ensemble_conditions)s = (
%(conditions_items)s,
)
%(ensemble_pars)s, %(ensemble_total_n)s = generate_param_combinations(
%(ensemble_conditions)s; crossed=%(crossed)s, n_replicates = %(ensemble_n)s)
%(ensemble_iter)s = 1
",
    ensemble_def_noconditions_julia = "%(ensemble_n)s = %(n_value)s
%(ensemble_total_n)s = %(n_value)s
%(ensemble_iter)s = 1
",
    ensemble_iter_julia = "
  # Assign ensemble parameters
  %(conditions_names)s, = %(ensemble_pars)s[div(%(ensemble_iter)s-1, %(ensemble_n)s) + 1]

",

    # -- compile_run_ode: Julia ensemble problem --------------------------------

    ensemble_prob_julia = "

# Create ODE problem
%(prob_name)s = ODEProblem(%(ode_func_name)s!, %(model_setup_name)s.%(initial_value_name)s, %(times_name)s, %(model_setup_name)s.%(parameter_name)s)

%(intermediaries_setup)s

# Define ensemble problem
function %(ensemble_func_name)s(prob, %(ensemble_ctx)s)
  %(use_with_rng_open)s
  %(ensemble_iter)s = %(ensemble_ctx)s.sim_id
  %(static_str)s%(intermediaries_callback)s
  remake(prob, u0 = %(model_setup_name)s.%(initial_value_name)s, p = %(model_setup_name)s.%(parameter_name)s%(intermediaries_remake)s)
  %(use_with_rng_close)s
end

# Define ensemble output function to save parameters, initial conditions, and intermediaries along with solution
function %(ensemble_output_func)s(sol, %(ensemble_ctx)s)
  # Save both solution and parameters
  return (t = sol.t, u = sol.u, p = sol.prob.p, u0 = sol.prob.u0), false
end

%(ensemble_prob_name)s = EnsembleProblem(%(prob_name)s, prob_func = %(ensemble_func_name)s, output_func = %(ensemble_output_func)s)
%(warmup_str)s%(solution_name)s = Base.invokelatest(solve, %(ensemble_prob_name)s, %(method)s%(threaded_str)s, dt = %(timestep_name)s, saveat = %(saveat_name)s, tstops = %(tstops_name)s, adaptive = false, trajectories = %(ensemble_total_n)s);
    ",

    # -- compile_run_ode: Julia ensemble save dataframe -------------------------

    ensemble_save_julia = "\n# Save timeseries dataframe
%(sim_df_name)s, %(parameter_name)s, %(initial_value_name)s%(ensemble_to_df_func)s%(solution_name)s, %(model_setup_name)s.%(initial_value_names)s, %(intermediaries)s, %(model_setup_name)s.%(intermediary_names)s, %(ensemble_n)s)
",

    # -- compile_run_ode: Julia ensemble save CSVs ------------------------------

    ensemble_csv_julia = "
CSV.write(\"%(filepath_df)s\", %(sim_df_name)s)
CSV.write(\"%(filepath_constants)s\", %(parameter_name)s)
CSV.write(\"%(filepath_init)s\", %(initial_value_name)s)
",

    # -- compile_run_ode: Julia ensemble summary stats --------------------------

    ensemble_summary_julia = "
# Compute summary statistics
%(summary_df_name)s%(ensemble_summ_func)s%(sim_df_name)s, [%(quantiles)s], [%(summary_stats)s])

%(parameter_name)s[!, :time] .= 0.0
%(summary_df_constants_name)s%(ensemble_summ_func)s%(parameter_name)s, [%(quantiles)s], [%(summary_stats)s])
select!(%(summary_df_constants_name)s, Not(:time))\n\n%(initial_value_name)s[!, :time] .= 0.0
%(summary_df_init_name)s%(ensemble_summ_func)s%(initial_value_name)s, [%(quantiles)s], [%(summary_stats)s])
select!(%(summary_df_init_name)s, Not(:time))

# Save to CSV
CSV.write(\"%(filepath_summary_df)s\", %(summary_df_name)s)
CSV.write(\"%(filepath_summary_constants)s\", %(summary_df_constants_name)s)
CSV.write(\"%(filepath_summary_init)s\", %(summary_df_init_name)s)

# Delete variables
%(sim_df_name)s = Nothing
%(parameter_name)s = Nothing
%(initial_value_name)s = Nothing
%(summary_df_name)s = Nothing
%(summary_df_constants_name)s = Nothing
%(summary_df_init_name)s = Nothing
%(solution_name)s = Nothing
%(intermediaries)s = Nothing
"
  )
}

extract_placeholders <- function(x) {
  unique(sub(
    "%\\(([^)]+)\\)s", "\\1",
    regmatches(x, gregexpr("%\\(([^)]+)\\)s", x))[[1]]
  ))
}

prep_script_template <- function() {
  # Get all templates
  templates <- script_template()
  nms <- names(templates)

  # Extract placeholders for each template
  placeholders <- lapply(templates, extract_placeholders)

  # Already replace placeholders contained in P
  templates <- lapply(nms, function(name) {
    tpl <- templates[[name]]
    replacements <- P[names(P) %in% placeholders[[name]]]
    if (length(replacements) == 0) {
      return(tpl)
    }
    names(replacements) <- paste0("%(", names(replacements), ")s")
    fmt(tpl, replacements)
  })
  names(templates) <- nms

  # Extract placeholders again for each template
  placeholders <- lapply(templates, extract_placeholders)

  # Merge into one list with templates and placeholders
  scripts <- list()
  for (name in names(templates)) {
    scripts[[name]] <- list(
      template = templates[[name]],
      placeholders = placeholders[[name]]
    )
  }
  names(scripts) <- nms

  scripts
}


# ---------------------------------------------------------------------------
# Public interface
# ---------------------------------------------------------------------------

#' Get a filled-in script template
#'
#' @param type Template type, e.g., `"times"`, `"prep"`, `"nonneg_stocks"`.
#' @param language `"R"` or `"Julia"`.
#' @param ... Named values to substitute into the template
#'
#' @returns Character string with placeholders replaced
#' @noRd
fmt_script <- function(type, language, ...) {
  # Get template
  key <- paste0(type, "_", tolower(language))
  scripts <- prep_script_template()
  tpl <- scripts[[key]]
  if (is.null(tpl)) {
    stop(sprintf("No template found for type='%s', language='%s' (key='%s')", type, language, key))
  }

  replacements <- do.call(c, list(...))
  if (length(replacements) == 0) {
    return(tpl[["template"]])
  }

  # Warn if any necessary placeholders are missing
  missing_placeholders <- setdiff(tpl[["placeholders"]], names(replacements))
  if (length(missing_placeholders) > 0) {
    stop(sprintf(
      "Missing placeholders for template type='%s', language='%s': %s",
      type, language,
      paste(missing_placeholders, collapse = ", ")
    ))
  }
  replacements <- replacements[tpl[["placeholders"]]] # only keep needed placeholders

  # Set placeholders with NULL or of length 0 to empty string
  idx <- vapply(replacements, function(x) is.null(x) || length(x) == 0, logical(1))
  replacements[idx] <- ""

  names(replacements) <- paste0("%(", names(replacements), ")s")

  fmt(tpl[["template"]], replacements)
}
