
# Create a new environment for the package
# pkg.env <- new.env(parent = emptyenv())


# Store variable names
# pkg.env$


# #' @param P List of names for variables in the output R script, e.g. P = list(time_name = "t", parameter_mame = "pars")



#' @param initial_value_name String; name of the initial value variable; defaults to "xstart"
#' @param parameter_name String; name of the parameters variable; defaults to "pars"
#' @param state_name String; name of the initial value variable; defaults to "S"
#' @param change_prefix String; prefix of the state change variable; defaults to "d"
#' @param time_name String; name of the current value of time in the ODE; defaults to "t"
#' @param times_name String; name of the time sequence variable; defaults to "times"
#' @param timestep_name String; name of the timestep variable; defaults to "dt"
#' @param sim_time_unit_name String; name of the simulation time unit variable; defaults to "time_units"
#' @param time_unit_pars_name String; name of the time unit parameters variable; defaults to "TU"
#' @param past_name String; name of the past variable; defaults to "past"
#' @param archive_var_name String; name of the variable which lists which variables in the model to store the history of in past; defaults to "archive_var"
#' @param conveyor_suffix String; suffix of conveyor variables; defaults to "_conveyor"
#' @param delay_acc_suffix String; suffix of delay variables; defaults to "_acc"
#' @param ode_func_name String; name of ODE function; defaults to "ode_func"
#' @param open_script Boolean; whether to open the created R script
#' @param check_script Boolean; whether to run the created R script
#' @param format_code Boolean; whether to format the created R script with the styler package
#' @param overwrite Boolean; whether to overwrite filepath_script if it already exists
#' @param filepath_script File path to output R script, optional
#'
P = list(initial_value_name = "xstart",
         initial_value_names = "xstart_names",
                          parameter_name = "pars",
                          state_name = "S",
                          change_prefix = "d",
                          time_name = "t",
                          times_name = "times",
                          timestep_name = "dt",
                          time_units_name = "time_units",
                          # time_unit_pars_name = time_unit_pars_name,
                          # archive_name = "archive",
                          # archive_var_name = "archive_var",
                          conveyor_suffix = "_conv",
         delayN_suffix = "_delayN",
         # delay_suffix = "_delay",
         # delay_order_suffix = "_order",
         # delay_length_suffix = "_length",
         delay_acc_suffix = "_acc",
         # past_suffix = "_past",
         # past_length_suffix = "_length",
         fix_suffix = "_fix",
         fix_length_suffix = "_length",
         sim_df_name = "df",
         prob_name = "prob",
         out_name = "out",
         ode_func_name = "ode_func",
         callback_func_name = "save_intermediaries",
         callback_name = "callback",
         intermediaries = "intermediaries",
         intermediary_df = "intermediary_df",
         intermediary_names = "intermediary_names",
         env_var_name = "env_var",
         env_update_name = "env_update",
         # stock_units_name = "stock_units",
         rootfun_name = "rootfun",
         eventfun_name = "eventfun",
         nonneg_stock_name = "nonneg_stock",
         ODE_var_name = "ODE_var",
         # ODE_unit_name= "ODE_unit",
         # macro_script_name = "MACRO",
         setunit_func = "setunit",
         # setunit_flow = "setunit_flow",
         units_dict = "units_dict",
         unit_context = "unit_context",
         sdbuildR_units ="sdbuildR_units",
         MyCustomUnits ="MyCustomUnits",
         constraint_def = "constraint_def",
         saveat_func = "saveat_func",
         init_sdbuildR = "initialization_sdbuildR"
)

# Suppress warnings for these global variables
# utils::globalVariables(c(".", "func_nr", "P", "regex_units", "regex_units_Julia"))
# utils::globalVariables(c(".", "func_nr", "P", "times", "time_units"))
utils::globalVariables(c(".", "P"))

# Set the styler.colored_print.vertical option to FALSE
options(styler.colored_print.vertical = FALSE)
