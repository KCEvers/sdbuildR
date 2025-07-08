# clean
module clean
using Unitful
using DataFrames
using ..custom_func: itp

# Function to save dataframe at specific times
function saveat_func(t, y, new_times)
    # Interpolate y at new_times
    itp(t, y, method = "linear", extrapolation = "nearest")(new_times)
end

function clean_df(solve_out, init_names, intermediaries=nothing, intermediary_names=nothing)
    """
Convert a single (non-ensemble) solution to a DataFrame, including intermediaries.

Args:
  solve_out: Single solution object from DifferentialEquations.jl
init_names: Names of the initial conditions/state variables
intermediaries: Optional intermediary values from saving callback
intermediary_names: Optional names for intermediary variables

Returns:
  timeseries_df: DataFrame with columns [time, variable, value]
"""

    # Get time values
    t_vals = isa(solve_out.t[1], Quantity) ? ustrip.(solve_out.t) : solve_out.t

    # Determine number of variables and their names
    if isa(solve_out.u[1], AbstractVector)
        n_vars = length(solve_out.u[1])
        var_names = [string(name) for name in init_names]
    else
        n_vars = 1
        var_names = [string(init_names[1])]
    end

    # Estimate total rows needed
    total_rows = length(t_vals) * n_vars
    if !isnothing(intermediaries) && !isnothing(intermediary_names)
        if !isempty(intermediaries.t)
            if isa(intermediaries.saveval[1], AbstractVector)
                total_rows += length(intermediaries.t) * length(intermediaries.saveval[1])
            else
                total_rows += length(intermediaries.t)
            end
        end
    end

    # Pre-allocate vectors
    time_vec = Vector{Float64}()
    variable_vec = Vector{String}()
    value_vec = Vector{Float64}()

    # Process main solution
    for (t_idx, t_val) in enumerate(t_vals)
        u_val = solve_out.u[t_idx]

        if isa(u_val, Union{AbstractVector, Tuple})
            for (var_idx, var_val) in enumerate(u_val)
                if !isa(var_val, Function)
                    val_stripped = isa(var_val, Quantity) ? ustrip(var_val) : Float64(var_val)
                    var_name = var_idx <= length(var_names) ? var_names[var_idx] : "var_$var_idx"

                    push!(time_vec, t_val)
                    push!(variable_vec, var_name)
                    push!(value_vec, val_stripped)
                end
            end
        else
            if !isa(u_val, Function)
                val_stripped = isa(u_val, Quantity) ? ustrip(u_val) : Float64(u_val)

                push!(time_vec, t_val)
                push!(variable_vec, var_names[1])
                push!(value_vec, val_stripped)
            end
        end
    end

    # Process intermediaries if provided
    if !isnothing(intermediaries) && !isnothing(intermediary_names) && !isempty(intermediaries.t)
        int_t_vals = isa(intermediaries.t[1], Quantity) ? ustrip.(intermediaries.t) : intermediaries.t
        int_var_names = [string(name) for name in intermediary_names]

        for (t_idx, t_val) in enumerate(int_t_vals)
            saved_val = intermediaries.saveval[t_idx]

            if isa(saved_val, Union{AbstractVector, Tuple})
                for (var_idx, var_val) in enumerate(saved_val)
                    if !isa(var_val, Function)
                        val_stripped = isa(var_val, Quantity) ? ustrip(var_val) : Float64(var_val)
                        var_name = var_idx <= length(int_var_names) ? int_var_names[var_idx] : "int_var_$var_idx"

                        push!(time_vec, t_val)
                        push!(variable_vec, var_name)
                        push!(value_vec, val_stripped)
                    end
                end
            else
                if !isa(saved_val, Function)
                    val_stripped = isa(saved_val, Quantity) ? ustrip(saved_val) : Float64(saved_val)

                    push!(time_vec, t_val)
                    push!(variable_vec, int_var_names[1])
                    push!(value_vec, val_stripped)
                end
            end
        end
    end

    # Create DataFrame
    timeseries_df = DataFrame(
        time = time_vec,
        variable = variable_vec,
        value = value_vec
    )

    return timeseries_df
end

function clean_constants(constants)
    constants = (; (name => isa(val, Unitful.Quantity) ? Unitful.ustrip(val) : val for (name, val) in pairs(constants))...)

    # Find keys where values are Float64 or Vector
    valid_keys = [k for k in keys(constants) if isa(constants[k], Float64) || isa(constants[k], Vector)]

    # Convert valid_keys to a tuple for NamedTuple construction
    valid_keys_tuple = Tuple(valid_keys)

    # Reconstruct filtered named tuple
    constants = NamedTuple{valid_keys_tuple}(constants[k] for k in valid_keys)

end


function clean_init(init, init_names)
    Dict(init_names .=> Unitful.ustrip.(init))
end
end
