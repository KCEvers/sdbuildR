# sdbuildRUtils main module
module sdbuildRUtils
__precompile__()

include("unit_func.jl")
include("custom_func.jl")
include("delay.jl")
include("clean.jl")
include("ensemble.jl")
include("sdbuildR_units.jl")

using .unit_func: convert_u
using .custom_func: is_function_or_interp, itp, ramp, make_step, pulse, seasonal, round_IM, logit, expit, logistic, nonnegative, rbool, rdist, indexof, contains_IM, substr_i, filter_IM, round_, ⊕
using .delay: retrieve_delay, retrieve_past, compute_delayN, compute_smoothN, setup_delayN, setup_smoothN
using .clean: saveat_func, clean_df, clean_constants, clean_init
using .ensemble: transform_intermediaries, ensemble_to_df, generate_param_combinations, ensemble_to_df_threaded, ensemble_summ, ensemble_summ_threaded

export is_function_or_interp, itp, ramp, make_step, pulse, seasonal, round_IM, logit, expit, logistic, nonnegative, rbool, rdist, indexof, contains_IM, substr_i, filter_IM, round_, ⊕, convert_u, retrieve_delay, retrieve_past, compute_delayN, compute_smoothN, setup_delayN, setup_smoothN, saveat_func, clean_df, clean_constants, clean_init, transform_intermediaries, ensemble_to_df, generate_param_combinations, ensemble_to_df_threaded, ensemble_summ, ensemble_summ_threaded

end
