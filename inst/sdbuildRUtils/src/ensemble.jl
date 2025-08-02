# ensemble
module ensemble
using Unitful
using Statistics
using DataFrames
using ..custom_func: is_function_or_interp

function transform_intermediaries(intermediaries, intermediary_names=nothing)
    """
Transform intermediaries to the same format as solve_out for unified processing.
This creates a pseudo-solution object that can be processed with the same logic.
"""
    transformed = []

    for (traj_idx, intermediate_vals) in enumerate(intermediaries)
        if !isnothing(intermediate_vals) && !isempty(intermediate_vals.t)
            # Create a pseudo-solution object with the same structure as solve_out
            pseudo_solution = (
                t = intermediate_vals.t,
                u = intermediate_vals.saveval,
                p = nothing  # intermediaries don't have parameters
            )
            push!(transformed, pseudo_solution)
        else
            # Create empty pseudo-solution for consistency
            push!(transformed, (t=Float64[], u=Float64[], p=nothing))
        end
    end

    return transformed
end

function ensemble_to_df(solve_out, init_names,
    intermediaries, intermediary_names, ensemble_n)
    """
Unified processing where intermediaries are transformed to solve_out format first.
"""
    n_trajectories = length(solve_out)

    # Get dimensions from first trajectory
    first_result = solve_out[1]
    t_vals = isa(first_result.t[1], Quantity) ? ustrip.(first_result.t) : first_result.t

    # Determine number of variables and their names
    if isa(first_result.u[1], AbstractVector)
        n_vars = length(first_result.u[1])
        var_names = [string(name) for name in init_names]
    else
        n_vars = 1
        var_names = [string(init_names[1])]
    end

    # Transform intermediaries to solve_out format
    transformed_intermediaries = nothing
    if !isnothing(intermediaries)
        transformed_intermediaries = transform_intermediaries(intermediaries, intermediary_names)
    end

    # Process both solution and intermediaries with the same logic
    function process_solution_like(solutions, var_names_to_use, variable_prefix="")
        if isnothing(solutions)
            return Int[], Float64[], String[], Float64[]
        end

        total_rows = 0
        for sol in solutions
            if !isempty(sol.t)
                if isa(sol.u[1], Union{AbstractVector, Tuple})
                    total_rows += length(sol.t) * length(sol.u[1])
                else
                    total_rows += length(sol.t)
                end
            end
        end

        if total_rows == 0
            return Int[], Float64[], String[], Float64[]
        end

        trajectory_vec = Vector{Int}(undef, total_rows)
        time_vec = Vector{Float64}(undef, total_rows)
        variable_vec = Vector{String}(undef, total_rows)
        value_vec = Vector{Float64}(undef, total_rows)

        row_idx = 1

        for (traj_idx, result) in enumerate(solutions)
            if !isempty(result.t)
                t_stripped = isa(result.t[1], Quantity) ? ustrip.(result.t) : result.t

                for (t_idx, t_val) in enumerate(t_stripped)
                    u_val = result.u[t_idx]

                    if isa(u_val, Union{AbstractVector, Tuple})
                        for (var_idx, var_val) in enumerate(u_val)
                            if !isa(var_val, Function)
                                val_stripped = isa(var_val, Quantity) ? ustrip(var_val) : Float64(var_val)
                                var_name = if isempty(variable_prefix)
                                    var_idx <= length(var_names_to_use) ? var_names_to_use[var_idx] : "var_$var_idx"
                                else
                                    var_idx <= length(var_names_to_use) ? "$(variable_prefix)$(var_names_to_use[var_idx])" : "$(variable_prefix)_$var_idx"
                                end

                                trajectory_vec[row_idx] = traj_idx
                                time_vec[row_idx] = t_val
                                variable_vec[row_idx] = var_name
                                value_vec[row_idx] = val_stripped
                                row_idx += 1
                            end
                        end
                    else
                        if !isa(u_val, Function)
                            val_stripped = isa(u_val, Quantity) ? ustrip(u_val) : Float64(u_val)
                            var_name = if isempty(variable_prefix)
                                var_names_to_use[1]
                            else
                                string(intermediary_names[1])
                            end

                            trajectory_vec[row_idx] = traj_idx
                            time_vec[row_idx] = t_val
                            variable_vec[row_idx] = var_name
                            value_vec[row_idx] = val_stripped
                            row_idx += 1
                        end
                    end
                end
            end
        end

        # Trim to actual size
        resize!(trajectory_vec, row_idx - 1)
        resize!(time_vec, row_idx - 1)
        resize!(variable_vec, row_idx - 1)
        resize!(value_vec, row_idx - 1)

        return trajectory_vec, time_vec, variable_vec, value_vec
    end

    # Process main solution
    main_traj, main_time, main_var, main_val = process_solution_like(solve_out, var_names)

    # Process intermediaries
    if !isnothing(transformed_intermediaries)
        int_var_names = [string(name) for name in intermediary_names]
        int_traj, int_time, int_var, int_val = process_solution_like(transformed_intermediaries, int_var_names)

        # Combine all data
        append!(main_traj, int_traj)
        append!(main_time, int_time)
        append!(main_var, int_var)
        append!(main_val, int_val)
    end

    # Create DataFrame
    timeseries_df = DataFrame(
        # count = main_traj,
        # Parameter ensemble index
        j = div.(main_traj .- 1, ensemble_n) .+ 1,
        # Trajectory index within the ensemble
        i = rem.(main_traj .- 1, ensemble_n) .+ 1,
        time = main_time,
        variable = main_var,
        value = main_val
    )

    # Extract parameter matrix (same as before)
    param_names = String[]
    first_params = solve_out[1].p
    if isa(first_params, NamedTuple)
        for (key, val) in pairs(first_params)
            if !is_function_or_interp(val)
                push!(param_names, string(key))
            end
        end
    elseif isa(first_params, AbstractVector)
        for i in eachindex(first_params)
            if !is_function_or_interp(first_params[i])
                push!(param_names, "p$i")
            end
        end
    end

    # Parameter matrix: (trajectories, parameters)
    param_matrix = Array{Float64, 2}(undef, n_trajectories, length(param_names))

    for (traj_idx, result) in enumerate(solve_out)
        params = result.p
        for (param_idx, param_name) in enumerate(param_names)
            if isa(params, NamedTuple)
                param_val = getproperty(params, Symbol(param_name))
            else
                p_idx = parse(Int, param_name[2:end])
                param_val = params[p_idx]
            end

            param_val_stripped = isa(param_val, Quantity) ? ustrip(param_val) : param_val
            param_matrix[traj_idx, param_idx] = param_val_stripped
        end
    end

    # Add parameter index
    b = 1:size(param_matrix, 1)
    param_matrix = hcat(
        # Parameter ensemble index
        div.(b .- 1, ensemble_n) .+ 1,
        # Trajectory index within the ensemble
        rem.(b .- 1, ensemble_n) .+ 1,
        param_matrix)

    # Extract initial values matrix
    init_val_names = [string(name) for name in init_names]

    # Initial values matrix: (trajectories, initial values)
    init_val_matrix = Array{Float64, 2}(undef, n_trajectories, length(init_val_names))

    for (traj_idx, result) in enumerate(solve_out)
        init_vals = result.u0

        if isa(init_vals, NamedTuple)
            for (init_idx, init_name) in enumerate(init_val_names)
                init_val = getproperty(init_vals, Symbol(init_name))
                init_val_stripped = isa(init_val, Quantity) ? ustrip(init_val) : init_val
                init_val_matrix[traj_idx, init_idx] = init_val_stripped
            end
        elseif isa(init_vals, AbstractVector)
            for (init_idx, init_name) in enumerate(init_val_names)
                init_val = init_vals[init_idx]
                init_val_stripped = isa(init_val, Quantity) ? ustrip(init_val) : init_val
                init_val_matrix[traj_idx, init_idx] = init_val_stripped
            end
        else
            # Single initial value
            init_val_stripped = isa(init_vals, Quantity) ? ustrip(init_vals) : init_vals
            init_val_matrix[traj_idx, 1] = init_val_stripped
        end
    end

    # Add initial values index
    init_val_matrix = hcat(
        # Parameter ensemble index
        div.(b .- 1, ensemble_n) .+ 1,
        # Trajectory index within the ensemble
        rem.(b .- 1, ensemble_n) .+ 1,
        init_val_matrix)

    return timeseries_df, param_matrix, param_names, init_val_matrix, init_val_names
end



"""
generate_param_combinations(param_ranges; crossed=true, n_replicates=100)

Generate parameter combinations for ensemble simulations.

# Arguments
- `param_ranges`: Dict or NamedTuple of parameter names to ranges/vectors
- `crossed`: Boolean, whether to cross all parameter combinations (default: true)
- `n_replicates`: Number of replicates per condition (default: 100)

# Returns
- `param_combinations`: Vector of parameter combinations
- `total_sims`: Total number of simulations (combinations x replicates)

# Examples
```julia
# Using named parameters (recommended)
param_ranges = Dict(
  :alpha => [0.1, 0.5, 1.0],
  :beta => [2.0, 5.0],
  :gamma => [0.01, 0.05, 0.1]
)

# Crossed design (all combinations)
param_combinations, total_sims = generate_param_combinations(
  param_ranges; crossed=true, n_replicates=50
)

# Non-crossed design (paired parameters)
param_combinations, total_sims = generate_param_combinations(
  param_ranges; crossed=false, n_replicates=100
)

# Using positional parameters
param_ranges = [[0.1, 0.5], [2.0, 5.0]]
param_combinations, total_sims = generate_param_combinations(
  param_ranges; param_names=[:alpha, :beta], crossed=true
)
```
"""
function generate_param_combinations(param_ranges;
                                   crossed=true, n_replicates=100)

       # Sort keys for consistent ordering
    names_list = sort(collect(keys(param_ranges)))
    values_list = [param_ranges[name] for name in names_list]

    # Generate parameter combinations
    if crossed
        # All combinations (Cartesian product)
        param_combinations = collect(Iterators.product(values_list...))
        param_combinations = [collect(combo) for combo in vec(param_combinations)]
    else
        # Paired combinations (requires all ranges to have same length)
        lengths = [length(range) for range in values_list]
        if !all(l == lengths[1] for l in lengths)
            throw(ArgumentError("For non-crossed design, all parameter ranges must have the same length"))
        end
        param_combinations = [[values_list[i][j] for i in 1:length(values_list)] for j in 1:lengths[1]]
    end

    # Calculate total simulations
    total_sims = length(param_combinations) * n_replicates

    return param_combinations, total_sims
end


function ensemble_to_df_threaded(solve_out, init_names, intermediaries, intermediary_names, ensemble_n)
    """
Unified processing where intermediaries are transformed to solve_out format first.
"""
    n_trajectories = length(solve_out)

    # Get dimensions from first trajectory
    first_result = solve_out[1]
    t_vals = isa(first_result.t[1], Quantity) ? ustrip.(first_result.t) : first_result.t

    # Determine number of variables and their names
    if isa(first_result.u[1], AbstractVector)
        n_vars = length(first_result.u[1])
        var_names = [string(name) for name in init_names]
    else
        n_vars = 1
        var_names = [string(init_names[1])]
    end

    # Transform intermediaries to solve_out format
    transformed_intermediaries = nothing
    if !isnothing(intermediaries)
        transformed_intermediaries = transform_intermediaries(intermediaries, intermediary_names)
    end

    # Process both solution and intermediaries with the same logic
    function process_solution_like(solutions, var_names_to_use, variable_prefix="")
        if isnothing(solutions)
            return Int[], Float64[], String[], Float64[]
        end

        # First pass: calculate row counts for each trajectory
        row_counts = Vector{Int}(undef, length(solutions))

        Base.Threads.@threads for i in 1:length(solutions)
            sol = solutions[i]
            count = 0
            if !isempty(sol.t)
                if isa(sol.u[1], Union{AbstractVector, Tuple})
                    count = length(sol.t) * length(sol.u[1])
                else
                    count = length(sol.t)
                end
            end
            row_counts[i] = count
        end

        total_rows = sum(row_counts)

        if total_rows == 0
            return Int[], Float64[], String[], Float64[]
        end

        # Pre-allocate output arrays
        trajectory_vec = Vector{Int}(undef, total_rows)
        time_vec = Vector{Float64}(undef, total_rows)
        variable_vec = Vector{String}(undef, total_rows)
        value_vec = Vector{Float64}(undef, total_rows)

        # Calculate start indices for each trajectory
        start_indices = Vector{Int}(undef, length(solutions))
        start_indices[1] = 1
        for i in 2:length(solutions)
            start_indices[i] = start_indices[i-1] + row_counts[i-1]
        end

        # Second pass: fill arrays in parallel
        Base.Threads.@threads for traj_idx in 1:length(solutions)
            result = solutions[traj_idx]
            if !isempty(result.t)
                t_stripped = isa(result.t[1], Quantity) ? ustrip.(result.t) : result.t

                row_idx = start_indices[traj_idx]

                for (t_idx, t_val) in enumerate(t_stripped)
                    u_val = result.u[t_idx]

                    if isa(u_val, Union{AbstractVector, Tuple})
                        for (var_idx, var_val) in enumerate(u_val)
                            if !isa(var_val, Function)
                                val_stripped = isa(var_val, Quantity) ? ustrip(var_val) : Float64(var_val)
                                var_name = if isempty(variable_prefix)
                                    var_idx <= length(var_names_to_use) ? var_names_to_use[var_idx] : "var_$var_idx"
                                else
                                    var_idx <= length(var_names_to_use) ? "$(variable_prefix)$(var_names_to_use[var_idx])" : "$(variable_prefix)_$var_idx"
                                end

                                trajectory_vec[row_idx] = traj_idx
                                time_vec[row_idx] = t_val
                                variable_vec[row_idx] = var_name
                                value_vec[row_idx] = val_stripped
                                row_idx += 1
                            end
                        end
                    else
                        if !isa(u_val, Function)
                            val_stripped = isa(u_val, Quantity) ? ustrip(u_val) : Float64(u_val)
                            var_name = if isempty(variable_prefix)
                                var_names_to_use[1]
                            else
                                string(intermediary_names[1])
                            end

                            trajectory_vec[row_idx] = traj_idx
                            time_vec[row_idx] = t_val
                            variable_vec[row_idx] = var_name
                            value_vec[row_idx] = val_stripped
                            row_idx += 1
                        end
                    end
                end
            end
        end

        return trajectory_vec, time_vec, variable_vec, value_vec
    end

    # Process main solution
    main_traj, main_time, main_var, main_val = process_solution_like(solve_out, var_names)

    # Process intermediaries
    if !isnothing(transformed_intermediaries)
        int_var_names = [string(name) for name in intermediary_names]
        int_traj, int_time, int_var, int_val = process_solution_like(transformed_intermediaries, int_var_names)

        # Combine all data
        append!(main_traj, int_traj)
        append!(main_time, int_time)
        append!(main_var, int_var)
        append!(main_val, int_val)
    end

    # Create DataFrame
    timeseries_df = DataFrame(
        # count = main_traj,
        # Parameter ensemble index
        j = div.(main_traj .- 1, ensemble_n) .+ 1,
        # Trajectory index within the ensemble
        i = rem.(main_traj .- 1, ensemble_n) .+ 1,
        time = main_time,
        variable = main_var,
        value = main_val
    )

    # Extract parameter matrix (same as before)
    param_names = String[]
    first_params = solve_out[1].p
    if isa(first_params, NamedTuple)
        for (key, val) in pairs(first_params)
            if !is_function_or_interp(val)
                push!(param_names, string(key))
            end
        end
    elseif isa(first_params, AbstractVector)
        for i in eachindex(first_params)
            if !is_function_or_interp(first_params[i])
                push!(param_names, "p$i")
            end
        end
    end

    # Parameter matrix: (trajectories, parameters)
    param_matrix = Array{Float64, 2}(undef, n_trajectories, length(param_names))

    Base.Threads.@threads for traj_idx in 1:length(solve_out)
        result = solve_out[traj_idx]
        params = result.p
        for (param_idx, param_name) in enumerate(param_names)
            if isa(params, NamedTuple)
                param_val = getproperty(params, Symbol(param_name))
            else
                p_idx = parse(Int, param_name[2:end])
                param_val = params[p_idx]
            end

            param_val_stripped = isa(param_val, Quantity) ? ustrip(param_val) : param_val
            param_matrix[traj_idx, param_idx] = param_val_stripped
        end
    end

    # Add parameter index
    b = 1:size(param_matrix, 1)
    param_matrix = hcat(
        # Parameter ensemble index
        div.(b .- 1, ensemble_n) .+ 1,
        # Trajectory index within the ensemble
        rem.(b .- 1, ensemble_n) .+ 1,
        param_matrix)

    # Extract initial values matrix
    init_val_names = [string(name) for name in init_names]

    # Initial values matrix: (trajectories, initial values)
    init_val_matrix = Array{Float64, 2}(undef, n_trajectories, length(init_val_names))

    Base.Threads.@threads for traj_idx in 1:length(solve_out)
        result = solve_out[traj_idx]
        init_vals = result.u0

        if isa(init_vals, NamedTuple)
            for (init_idx, init_name) in enumerate(init_val_names)
                init_val = getproperty(init_vals, Symbol(init_name))
                init_val_stripped = isa(init_val, Quantity) ? ustrip(init_val) : init_val
                init_val_matrix[traj_idx, init_idx] = init_val_stripped
            end
        elseif isa(init_vals, AbstractVector)
            for (init_idx, init_name) in enumerate(init_val_names)
                init_val = init_vals[init_idx]
                init_val_stripped = isa(init_val, Quantity) ? ustrip(init_val) : init_val
                init_val_matrix[traj_idx, init_idx] = init_val_stripped
            end
        else
            # Single initial value
            init_val_stripped = isa(init_vals, Quantity) ? ustrip(init_vals) : init_vals
            init_val_matrix[traj_idx, 1] = init_val_stripped
        end
    end

    # Add initial values index
    init_val_matrix = hcat(
        # Parameter ensemble index
        div.(b .- 1, ensemble_n) .+ 1,
        # Trajectory index within the ensemble
        rem.(b .- 1, ensemble_n) .+ 1,
        init_val_matrix)

    return timeseries_df, param_matrix, param_names, init_val_matrix, init_val_names
end


function ensemble_summ(timeseries_df, quantiles=[0.025, 0.0975])
    # Group by time and variable, then compute statistics
    stats_df = combine(groupby(timeseries_df, [:j, :time, :variable])) do group
        values = group.value

        # Filter out missing and NaN
        is_valid = .!(ismissing.(values) .| isnan.(values))
        clean_values = values[is_valid]
        num_missing = count(!, is_valid)

        if isempty(clean_values)
            # Return NaNs if no valid values
            result = (
                mean = NaN,
                variance = NaN,
                median = NaN,
                missing_count = num_missing
            )

            for q in quantiles
                q_str = replace(string(q), r"^0\." => "")
                result = merge(result, (Symbol("q$q_str") => NaN,))
            end
        else
            # Compute statistics
            result = (
                mean = mean(clean_values),
                variance = var(clean_values),
                median = Statistics.median(clean_values),
                missing_count = num_missing
            )

            for q in quantiles
                q_str = replace(string(q), r"^0\." => "")
                result = merge(result, (Symbol("q$q_str") => Statistics.quantile(clean_values, q),))
            end
        end

        return result
    end

    return stats_df
end

function ensemble_summ_threaded(timeseries_df, quantiles=[0.025, 0.975])
   # Group the data
    grouped_df = groupby(timeseries_df, [:j, :time, :variable])

    # Get the keys and create arrays to store results
    group_keys = keys(grouped_df)
    n_groups = length(group_keys)

    # Pre-allocate result arrays
    j_vals = Vector{Int}(undef, n_groups)
    time_vals = Vector{Float64}(undef, n_groups)
    variable_vals = Vector{String}(undef, n_groups)
    mean_vals = Vector{Float64}(undef, n_groups)
    variance_vals = Vector{Float64}(undef, n_groups)
    median_vals = Vector{Float64}(undef, n_groups)
    missing_counts = Vector{Int}(undef, n_groups)

    # Pre-allocate quantile arrays
    quantile_arrays = Dict{String, Vector{Float64}}()
    for q in quantiles
        q_str = replace(string(q), r"^0\." => "")
        quantile_arrays["q$q_str"] = Vector{Float64}(undef, n_groups)
    end

    # Process groups in parallel
    Base.Threads.@threads for i in 1:n_groups
        group = grouped_df[i]
        key = group_keys[i]
        values = group.value

        # Count and filter NaN/missing
        is_valid = .!(ismissing.(values) .| isnan.(values))
        clean_values = values[is_valid]
        num_missing = count(!, is_valid)

        # Extract group keys
        j_vals[i] = key.j
        time_vals[i] = key.time
        variable_vals[i] = key.variable

        # Handle empty groups after filtering
        if isempty(clean_values)
            mean_vals[i] = NaN
            variance_vals[i] = NaN
            median_vals[i] = NaN
            for q in quantiles
                q_str = replace(string(q), r"^0\." => "")
                quantile_arrays["q$q_str"][i] = NaN
            end
        else
            # Compute stats
            mean_vals[i] = mean(clean_values)
            variance_vals[i] = var(clean_values)
            median_vals[i] = Statistics.median(clean_values)
            for q in quantiles
                q_str = replace(string(q), r"^0\." => "")
                quantile_arrays["q$q_str"][i] = Statistics.quantile(clean_values, q)
            end
        end

        # Store missing count
        missing_counts[i] = num_missing
    end

    # Create result DataFrame with desired column order
    # Start with the main columns in order
    stats_df = DataFrame(
        j = j_vals,
        time = time_vals,
        variable = variable_vals,
        mean = mean_vals,
        median = median_vals,
        variance = variance_vals,
        missing_count = missing_counts
    )

    # Add quantile columns in order
    for q in quantiles
        q_str = replace(string(q), r"^0\." => "")
        stats_df[!, Symbol("q$q_str")] = quantile_arrays["q$q_str"]
    end

    return stats_df
end

end
