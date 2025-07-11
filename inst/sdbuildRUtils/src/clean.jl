# clean
module clean
using Unitful
using DataFrames
using ..custom_func: itp, is_function_or_interp

# Function to save dataframe at specific times
function saveat_func(t, y, new_times)
    # Interpolate y at new_times
    itp(t, y, method = "linear", extrapolation = "nearest")(new_times)
end

function clean_df(prob, solve_out, init_names, intermediaries=nothing, intermediary_names=nothing)
    """
Convert a single (non-ensemble) solution to a DataFrame, including intermediaries, and extract parameter/initial values.

Args:
  prob: Single problem function object from DifferentialEquations.jl
  solve_out: Single solution object from DifferentialEquations.jl
  init_names: Names of the initial conditions/state variables
  intermediaries: Optional intermediary values from saving callback
  intermediary_names: Optional names for intermediary variables

Returns:
  timeseries_df: DataFrame with columns [time, variable, value]
  param_values: Vector of parameter values
  param_names: Vector of parameter names
  init_values: Vector of initial values
  init_names: Vector of initial value names (same as input for completeness)
"""

    # Extract parameter names and values
    param_names = String[]
    param_values = Float64[]
    params = prob.p

    if isa(params, NamedTuple)
        for (key, val) in pairs(params)
            if !is_function_or_interp(val)
                push!(param_names, string(key))
                val_stripped = isa(val, Quantity) ? ustrip(val) : Float64(val)
                push!(param_values, val_stripped)
            end
        end
    elseif isa(params, AbstractVector)
        for i in eachindex(params)
            if !is_function_or_interp(params[i])
                push!(param_names, "p$i")
                val_stripped = isa(params[i], Quantity) ? ustrip(params[i]) : Float64(params[i])
                push!(param_values, val_stripped)
            end
        end
    elseif isa(params, Number)
        push!(param_names, "p1")
        val_stripped = isa(params, Quantity) ? ustrip(params) : Float64(params)
        push!(param_values, val_stripped)
    end

    # Extract initial values
    init_values = Float64[]
    init_vals = prob.u0
    init_val_names = [string(name) for name in init_names]

    if isa(init_vals, NamedTuple)
        for init_name in init_val_names
            init_val = getproperty(init_vals, Symbol(init_name))
            init_val_stripped = isa(init_val, Quantity) ? ustrip(init_val) : Float64(init_val)
            push!(init_values, init_val_stripped)
        end
    elseif isa(init_vals, AbstractVector)
        for init_val in init_vals
            init_val_stripped = isa(init_val, Quantity) ? ustrip(init_val) : Float64(init_val)
            push!(init_values, init_val_stripped)
        end
    else
        # Single initial value
        init_val_stripped = isa(init_vals, Quantity) ? ustrip(init_vals) : Float64(init_vals)
        push!(init_values, init_val_stripped)
    end

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

    return timeseries_df, param_values, param_names, init_values, init_val_names
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
