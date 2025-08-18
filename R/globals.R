# Global variables for the sdbuildR package

# Check first if .sdbuildR_env was already initialized. This is for the rare case where someone has run use_julia() already, and reloads sdbuildR, which will overwrite the initialization of use_julia()
if (!exists(".sdbuildR_env")) {
  .sdbuildR_env <- new.env(parent = emptyenv())

  # Names of variables and functions
  .sdbuildR_env[["P"]] <- list(
    debug = FALSE,
    insightmaker_version = 38,
    jl_required_version = "1.11",
    jl_pkg_name = "sdbuildRUtils",
    model_setup_name = "model_setup",
    macro_name = "macro",
    initial_value_name = "init",
    initial_value_names = "init_names",
    parameter_name = "constants",
    parameter_names = "constant_names",
    state_name = "current_state",
    change_prefix = "d",
    time_name = "t",
    change_state_name = "dSdt",
    times_name = "times",
    timestep_name = "dt",
    saveat_name = "saveat",
    savefrom_name = "savefrom",
    time_units_name = "time_units",
    conveyor_suffix = "_conv",
    delayN_suffix = "_delayN",
    smoothN_suffix = "_smoothN",
    delay_suffix = "_delay",
    outflow_suffix = ".outflow",
    acc_suffix = "_acc",
    delay_idx_name = "idx",
    past_suffix = "_past",
    # past_length_suffix = "_length",
    fix_suffix = "_fix",
    fix_length_suffix = "_fixlength",
    ensemble_prob_name = "ensemble_prob",
    ensemble_output_func = "output_func",
    ensemble_range = "ensemble_range",
    ensemble_pars = "ensemble_pars",
    ensemble_iter = "i",
    ensemble_n = "ensemble_n",
    ensemble_total_n = "ensemble_total_n",
    ensemble_func_name = "prob_func",
    summary_df_name = "summary_df",
    summary_df_constants_name = "summary_df_constants",
    summary_df_init_name = "summary_df_init",
    sim_df_name = "df",
    prob_name = "prob",
    solution_name = "solve_out",
    ode_func_name = "ode_func",
    callback_func_name = "save_intermediaries",
    callback_name = "callback",
    intermediaries = "intermediaries",
    intermediary_df = "intermediary_df",
    intermediary_names = "intermediary_names",
    rootfun_name = "rootfun",
    eventfun_name = "eventfun",
    nonneg_stock_name = "nonneg_stock",
    convert_u_func = "convert_u",
    sdbuildR_units = "sdbuildR_units",
    MyCustomUnits = "MyCustomUnits",
    saveat_func = "saveat_func",
    init_sdbuildR = "init_sdbuildR"
  )

  .sdbuildR_env[["jl_pkg_pkg"]] <- c(
    "DataFrames" = "1.7",
    "Distributions" = "0.25",
    "Statistics" = "1.11",
    "Unitful" = "1.23",
    "DataInterpolations" = "8.1",
    "Random" = "1.11"
  )

  .sdbuildR_env[["jl_env_pkg"]] <- c(
    "OrdinaryDiffEq" = "6.98",
    "DiffEqCallbacks" = "4.8",
    "DataFrames" = "1.7",
    "Distributions" = "0.25",
    "Statistics" = "1.11",
    "StatsBase" = "0.34",
    "Unitful" = "1.23",
    "DataInterpolations" = "8.1",
    "Random" = "1.11",
    "CSV" = "0.10",
    "SciMLBase" = "2.102"
  )


}
