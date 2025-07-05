# Global variables for the sdbuildR package

#' @param insightmaker_version Integer with Insight Maker version the package sdbuildR was built with
#' @param jl_pkg_name String; name of the Julia package
#' @param model_setup_name String; name of the model setup variable
#' @param initial_value_name String; name of the initial value variable
#' @param initial_value_names String; name of the variable with stock names
#' @param parameter_name String; name of the parameters variable
#' @param state_name String; name of the initial value variable
#' @param change_prefix String; prefix of the state change variable
#' @param time_name String; name of the current value of time in the ODE
#' @param change_state_name String; name of the state change variable
#' @param times_name String; name of the time sequence variable
#' @param timestep_name String; name of the timestep variable
#' @param time_units_name String; name of the simulation time unit variable
#' @param conveyor_suffix String; suffix of conveyor variables
#' @param delayN_suffix String; suffix of delayN variables
#' @param smoothN_suffix String; suffix of smoothN variables
#' @param delay_suffix String; suffix of delay variables
#' @param outflow_suffix String; suffix of outflow entry in delay variables
#' @param delayN_acc_suffix String; suffix of delayN accumulator variables
#' @param smoothN_acc_suffix String; suffix of smoothN accumulator variables
#' @param past_suffix String; suffix of past variables
#' @param fix_suffix String; suffix of fix variables
#' @param fix_length_suffix String; suffix of fix length variables
#' @param ensemble_prob_name String; name of the ensemble problem
#' @param ensemble_suffix String; suffix of specified ensemble conditions
#' @param ensemble_iter String; name of the ensemble iteration variable
#' @param ensemble_func_name String; name of the ensemble function
#' @param sim_df_name String; name of the simulation data frame
#' @param prob_name String; name of the problem variable
#' @param solution_name String; name of the solution variable
#' @param ode_func_name String; name of the ODE function
#' @param callback_func_name String; name of the callback function
#' @param callback_name String; name of the callback variable
#' @param intermediaries String; name of the intermediaries variable
#' @param intermediary_df String; name of the intermediary data frame
#' @param intermediary_names String; name of the intermediary names variable
#' @param rootfun_name String; name of the root function
#' @param eventfun_name String; name of the event function
#' @param nonneg_stock_name String; name of the non-negativity stock variable
#' @param convert_u_func String; name of the function to convert units
#' @param units_dict String; name of the units dictionary variable
#' @param sdbuildR_units String; name of the sdbuildR units module
#' @param MyCustomUnits String; name of the MyCustomUnits module
#' @param constraint_def String; name of the constraint definition variable
#' @param saveat_func String; name of the saveat function
#' @param init_sdbuildR String; name of the initialization function for sdbuildR
P = list(
  insightmaker_version = 38,
  jl_pkg_name = "sdbuildRUtils",
  model_setup_name = "model_setup",
  macro_name = "macro",
  initial_value_name = "init",
         initial_value_names = "init_names",
         parameter_name = "constants",
         state_name = "current_state",
         change_prefix = "d",
         time_name = "t",
         change_state_name = "dSdt",
         times_name = "times",
         timestep_name = "dt",
         saveat_name = "saveat",
         time_units_name = "time_units",
         # time_unit_pars_name = time_unit_pars_name,
         # archive_name = "archive",
         # archive_var_name = "archive_var",
         conveyor_suffix = "_conv",
         delayN_suffix = "_delayN",
         smoothN_suffix = "_smoothN",
         delay_suffix = "_delay",
         outflow_suffix = ".outflow",
         # delay_order_suffix = "_order",
         # delay_length_suffix = "_length",
         delayN_acc_suffix = "_acc",
         smoothN_acc_suffix = "_acc",
         past_suffix = "_past",
         # past_length_suffix = "_length",
         fix_suffix = "_fix",
         fix_length_suffix = "_fixlength",
         ensemble_prob_name = "ensemble_prob",
         ensemble_pars = "ensemble_pars",
         ensemble_iter = "i",
         ensemble_func_name = "prob_func",
         summary_df_name = "summary_df",
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
         # units_dict = "units_dict",
         # unit_context = "unit_context",
         sdbuildR_units ="sdbuildR_units",
         MyCustomUnits ="MyCustomUnits",
         constraint_def = "constraint_def",
         saveat_func = "saveat_func",
         init_sdbuildR = "initialization_sdbuildR"
)

# Suppress warnings for these global variables
utils::globalVariables(c(".", "P", P$times_name))
# times needs to be global as it is used in step, pulse, ramp, and seasonal

# Set the styler.colored_print.vertical option to FALSE
options(styler.colored_print.vertical = FALSE)
