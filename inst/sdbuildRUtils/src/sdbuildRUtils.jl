# sdbuildRUtils main module
module sdbuildRUtils
__precompile__()

include("unit_func.jl")
include("custom_func.jl")
include("past.jl")
include("clean.jl")
include("ensemble.jl")
include("sdbuildR_units.jl")

using .unit_func: convert_u
using .custom_func: itp, ramp, make_step, pulse, seasonal, IM_round, logit, expit, logistic, nonnegative, rbool, rdist, indexof, IM_contains, substr_i, IM_filter, round_, ⊕
using .past: retrieve_delay, retrieve_past, compute_delayN, compute_smoothN, setup_delayN, setup_smoothN
using .clean: saveat_func, clean_df, clean_constants, clean_init
using .ensemble: transform_intermediaries, ensemble_to_df, generate_param_combinations, ensemble_to_df_threaded, ensemble_summ, ensemble_summ_threaded

export itp, ramp, make_step, pulse, seasonal, IM_round, logit, expit, logistic, nonnegative, rbool, rdist, indexof, IM_contains, substr_i, IM_filter, round_, ⊕, convert_u, retrieve_delay, retrieve_past, compute_delayN, compute_smoothN, setup_delayN, setup_smoothN, saveat_func, clean_df, clean_constants, clean_init, transform_intermediaries, ensemble_to_df, generate_param_combinations, ensemble_to_df_threaded, ensemble_summ, ensemble_summ_threaded

end
