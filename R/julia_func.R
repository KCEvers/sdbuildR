#' Customary functions written in Julia
#'
#' @returns List with Julia code
#' @noRd
julia_func <- function() {
  func_def <- list(
    # extrapolation = 1: return NA when outside of bounds
    # extrapolation = 2: return nearest value when outside of bounds
    "custom_func" = list(
      "is_function_or_interp" = "is_function_or_interp(x) = isa(x, Function) || isa(x, DataInterpolations.AbstractInterpolation)",
      "itp" = "# Extrapolation function\nfunction itp(x, y; method = \"linear\", extrapolation = \"nearest\")

  # Ensure y is sorted along x
  idx = sortperm(x)
  x = x[idx]
  y = y[idx]

  # Extrapolation rule: What happens outside of defined values?
  # Rule \"NA\": return NaN; Rule \"nearest\": return closest value
  rule_method = ifelse(extrapolation == \"NA\", DataInterpolations.ExtrapolationType.None, ifelse(extrapolation == \"nearest\", DataInterpolations.ExtrapolationType.Constant, extrapolation))

  if method == \"constant\"
      func = DataInterpolations.ConstantInterpolation(y, x; extrapolation = rule_method) # notice order of x and y
  elseif method == \"linear\"
      func = DataInterpolations.LinearInterpolation(y, x; extrapolation = rule_method)
  end

  return(func)
end",
      "ramp" = "function ramp(times, time_units, start, finish, height = 1.0)

    @assert start < finish \"The finish time of the ramp cannot be before the start time. To specify a decreasing ramp, set the height to a negative value.\"

    # If times has units, but the ramp times don't, convert them to the same units
    if eltype(times) <: Unitful.Quantity
        if !(eltype(start) <: Unitful.Quantity)
            start = convert_u(start, time_units)
        end
        if !(eltype(finish) <: Unitful.Quantity)
            finish = convert_u(finish, time_units)
        end
    else
        # If times does not have units, but start does, convert the ramp times to the same units as time_units
        if eltype(start) <: Unitful.Quantity
            start = Unitful.ustrip(convert_u(start, time_units))
        end
        if eltype(finish) <: Unitful.Quantity
            finish = Unitful.ustrip(convert_u(finish, time_units))
        end
    end


    # Ensure start_h_ramp and height are both of the same type
    start_h_ramp = 0.0
    if !(eltype(height) <: Unitful.Quantity)
        height = convert_u(height, Unitful.unit(start_h_ramp))
        add_y = convert_u(0.0, Unitful.unit(start_h_ramp))
    elseif eltype(height) <: Unitful.Quantity
        start_h_ramp = convert_u(start_h_ramp, Unitful.unit(height))
        add_y = convert_u(0.0, Unitful.unit(height))
    else
        add_y = 0.0
    end

    x = [start, finish]
    y = [start_h_ramp, height]

    # If the ramp is after the start time, add a zero at the start
    if start > first(times)
        x = [first(times); x]
        y = [add_y; y]
    end

    func = itp(x, y, method = \"linear\", extrapolation = \"nearest\")

    return(func)
end ",
      "make_step" = "# Make step signal
function make_step(times, time_units, start, height = 1.0)

    # If times has units, but the ramp times don't, convert them to the same units
    if eltype(times) <: Unitful.Quantity
        if !(eltype(start) <: Unitful.Quantity)
            start = convert_u(start, time_units)
        end
    else
        # If times does not have units, but start does, convert the ramp times to the same units as time_units
        if eltype(start) <: Unitful.Quantity
            start = Unitful.ustrip(convert_u(start, time_units))
        end
    end

    if eltype(height) <: Unitful.Quantity
      add_y = convert_u(0.0, Unitful.unit(height))
    else
      add_y = 0.0
    end

    x = [start, times[2]]
    y = [height, height]

    # If the step is after the start time, add a zero at the start
    if start > first(times)
        x = [first(times); x]
        y = [add_y; y]
    end

    func = itp(x, y, method = \"constant\", extrapolation = \"nearest\")

    return(func)
end

    ",
      "pulse" = "# Make pulse signal
function pulse(times, time_units, start, height = 1.0, width = 1.0 * time_units, repeat_interval = nothing)

    # Width of pulse cannot be zero
    if eltype(width) <: Unitful.Quantity
        if Unitful.ustrip(convert_u(width, time_units)) <= 0.0
            throw(ArgumentError(\"The width of the pulse cannot be equal to or less than 0; to indicate an 'instantaneous' pulse, specify the simulation step size (dt).\"))
        end
    else
        if width <= 0.0
            throw(ArgumentError(\"The width of the pulse cannot be equal to or less than 0; to indicate an 'instantaneous' pulse, specify the simulation step size (dt).\"))
        end
    end

    # If times has units, but the pulse times don't, convert them to the same units
    if eltype(times) <: Unitful.Quantity
        if !(eltype(start) <: Unitful.Quantity)
            start = convert_u(start, time_units)
        end
        if !(eltype(width) <: Unitful.Quantity)
            width = convert_u(width, time_units)
        end
        if (!isnothing(repeat_interval) && !(eltype(repeat_interval) <: Unitful.Quantity))
            repeat_interval = convert_u(repeat_interval, time_units)
        end
    else
        # If times does not have units, but start does, convert the pulse times to the same units as time_units
        if eltype(start) <: Unitful.Quantity
            start = Unitful.ustrip(convert_u(start, time_units))
        end
        if eltype(width) <: Unitful.Quantity
            width = Unitful.ustrip(convert_u(width, time_units))
        end
        if (!isnothing(repeat_interval) && eltype(repeat_interval) <: Unitful.Quantity)
            repeat_interval = Unitful.ustrip(convert_u(repeat_interval, time_units))
        end
    end

    # Define start and end times of pulses
    last_time = last(times)
    # If no repeats, set end of pulse to after end time
    step_size = isnothing(repeat_interval) ? last_time * 2 : repeat_interval
    start_ts = collect(start:step_size:last_time)
    end_ts = start_ts .+ width

    # Build signal as vectors of times and y-values
    signal_times = [start_ts; end_ts]
    signal_y = [fill(height, length(start_ts)); fill(0, length(end_ts))]

    if eltype(height) <: Unitful.Quantity
      add_y = convert_u(0.0, Unitful.unit(height))
    else
      add_y = 0.0
    end

    # If the first pulse is after the start time, add a zero at the start
    if minimum(start_ts) > first(times)
        signal_times = [first(times); signal_times]
        signal_y = [add_y; signal_y]
    end

    # If the last pulse doesn't cover the end, add a zero at the end
    # (I don't fully understand why this is necessary, but otherwise it gives incorrect results with repeat_interval <= 0)
    if maximum(end_ts) < last_time
        signal_times = [signal_times; last_time]
        signal_y = [signal_y; add_y]
    end

    # Sort by time
    perm = sortperm(signal_times)
    x = signal_times[perm]
    y = signal_y[perm]
    func = itp(x, y, method = \"constant\", extrapolation = \"nearest\")

    return(func)
end",
      "seasonal" = "# Create seasonal wave \nfunction seasonal(times, dt, period = u\"1yr\", shift = u\"0yr\")

    @assert Unitful.ustrip(period) > 0 \"The period of the seasonal wave must be greater than 0.\"

    time_vec = times[1]:dt:times[2]
    phase = 2 * pi .* (time_vec .- shift) ./ period  # π radians
    y = cos.(phase)
    func = itp(time_vec, y, method = \"linear\", extrapolation = \"nearest\")

    return(func)
end",
      "round_IM" = "# Convert Insight Maker's Round() function to R\n# Difference: in Insight Maker, Round(.5) = 1; in R, round(.5) = 0; in julia, round(.5) = 0.0\nfunction round_IM(x::Real, digits::Int=0)
    # Compute the fractional part after scaling by 10^digits
    scaled_x = x * 10.0^digits
    frac = scaled_x % 1
    # Check if fractional part is exactly 0.5 or -0.5
    if abs(frac) == 0.5
        return ceil(scaled_x) / 10.0^digits
    else
        return round_(scaled_x, digits=0) / 10.0^digits
    end
end",
      "logit" = "# Logit function\nfunction logit(p)
    return log(p / (1 - p))
end",
      "expit" = "# Expit function\nfunction expit(x)
    return 1 / (1+exp(-x))
end",
      "logistic" = "function logistic(x, slope=1.0, midpoint=0.0, upper = 1.0)
    @assert isfinite(Unitful.ustrip(slope)) && isfinite(Unitful.ustrip(midpoint)) && isfinite(Unitful.ustrip(upper)) \"slope, midpoint, and upper must be numeric\"
    upper / (1 + exp(-slope * (x - midpoint)))
end
",
      "nonnegative" = "# Prevent non-negativity (below zero)
# Scalar case: non-unitful types
nonnegative(x::Real) = max(0.0, x)

# Scalar case: Unitful.Quantity
nonnegative(x::Unitful.Quantity) = max(0.0, Unitful.ustrip(x)) * Unitful.unit(x)

# Array case: non-unitful elements
nonnegative(x::AbstractArray{<:Real}) = max.(0.0, x)

# Array case: Unitful.Quantity elements
nonnegative(x::AbstractArray{<:Unitful.Quantity}) = max.(0.0, Unitful.ustrip.(x)) .* Unitful.unit.(x)",
      "rbool" = "# Generate random boolean value, equivalent of RandBoolean() in Insight Maker\nfunction rbool(p)
    return rand() < p
end",
      "rdist" = "function rdist(a::Vector{T}, b::Vector{<:Real}) where T
    # Check lengths match
    if length(a) != length(b)
        throw(ArgumentError(\"Length of a and b must match\"))
    end
    # Normalize probabilities
    b_sum = sum(b)
    if b_sum <= 0
        throw(ArgumentError(\"Sum of probabilities must be positive\"))
    end
    b_normalized = b / b_sum
    # Sample using Categorical
    return a[rand(Distributions.Categorical(b_normalized))]
end",
      "indexof" = "function indexof(haystack, needle)
    if isa(haystack, AbstractString) && isa(needle, AbstractString)
        pos = findfirst(needle, haystack)
        return isnothing(pos) ? 0 : first(pos)
    else
        pos = findfirst(==(needle), haystack)
        return isnothing(pos) ? 0 : pos
    end
end",
      "contains_IM" = "function contains_IM(haystack, needle)
    if isa(haystack, AbstractString) && isa(needle, AbstractString)
        return occursin(needle, haystack)
    else
        return needle in haystack
    end
end",
      "substr_i" = "function substr_i(string::AbstractString, idxs::Union{Int, Vector{Int}})
    chars = collect(string)
    return join(chars[idxs])
end",
      "filter_IM" = "function filter_IM(y::Vector{T}, condition_func::Function) where T
    names_y = string.(1:length(y))
    result = Dict{String,T}()
    for (key, val) in zip(names_y, y)
        if condition_func(val, key)
            result[key] = val
        end
    end
    return collect(values(result)), collect(keys(result))
end",
      "round_" = "
#round_(x::Unitful.Quantity) = round(Unitful.ustrip.(x)) * Unitful.unit(x)
#round_(x::Unitful.Quantity, digits::Int) = round(Unitful.ustrip.(x), digits=digits) * Unitful.unit(x)
#round_(x::Unitful.Quantity; digits::Int) = round(Unitful.ustrip.(x), digits=digits) * Unitful.unit(x)
#round_(x::Unitful.Quantity, digits::Float64) = round(Unitful.ustrip.(x), digits=round(digits)) * Unitful.unit(x)
#round_(x::Unitful.Quantity; digits::Float64) = round(Unitful.ustrip.(x), digits=round(digits)) * Unitful.unit(x)

#round_(x) = round(x)

round_(x, digits::Real) = round(x, digits=round(Int, digits))

round_(x; digits::Real=0) = round(x, digits=round(Int, digits))

#round_(x::Unitful.Quantity) = round(Unitful.ustrip(x)) * Unitful.unit(x)

round_(x::Unitful.Quantity, digits::Real) = round(Unitful.ustrip(x), digits=round(Int, digits)) * Unitful.unit(x)

round_(x::Unitful.Quantity; digits::Real=0) = round(Unitful.ustrip(x), digits=round(Int, digits)) * Unitful.unit(x)",
      "\\u2295" = "# Define the operator \\u2295 for the modulus
function \\u2295(x, y)
    return mod(x, y)
end"
    ),
    "unit_func" = list(
      "convert_u" = sprintf("# Set or convert unit wrappers per type
function convert_u(x::Unitful.Quantity, unit_def::Unitful.Quantity)
    if Unitful.unit(x) == Unitful.unit(unit_def)
        return x  # No conversion needed
    else
        Unitful.uconvert.(Unitful.unit(unit_def), x)
    end
end

function convert_u(x::Unitful.Quantity, unit_def::Unitful.Units)
    if Unitful.unit(x) == unit_def
        return x  # No conversion needed
    else
        Unitful.uconvert.(unit_def, x)
    end
end

# If x is not a Unitful.Quantity but Float64:
function convert_u(x::Float64, unit_def::Unitful.Quantity)
    x * Unitful.unit(unit_def)
end

function convert_u(x::Float64, unit_def::Unitful.Units)
    x * unit_def
end
")
    ),
    "delay" = list(
      "retrieve_delay" = "function retrieve_delay(var_value, delay_time, default_value, t, var_name, intermediaries, intermediary_names)
    # Handle empty intermediaries
    if isempty(intermediaries.saveval)
        return isnothing(default_value) ? var_value : default_value
    end

    # Ensure t and delay_time have compatible units
    if !(eltype(delay_time) <: Unitful.Quantity) && (eltype(t) <: Unitful.Quantity)
        delay_time = convert_u(delay_time, t)
    end

    # Extract variable index
    var_index = findfirst(==(var_name), intermediary_names)

    # Extract times and values
    ts = intermediaries.t
    ys = [val[var_index] for val in intermediaries.saveval]

    # Append current time if not present
    #if !isapprox(t, ts[end]; atol=1e-10)
    if abs(Unitful.ustrip(t - last(ts))) > 1e-10
        ts = [ts; t]
    end

    ys = [ys; var_value][1:length(ts)]  # Ensure ys is the same length as ts

    # Single value extraction
    extract_t = t - delay_time
    if extract_t < ts[1]
        return isnothing(default_value) ? ys[1] : default_value
    elseif extract_t == t
        return var_value
    else
        return itp(ts, ys, method = \"linear\")(extract_t)
    end

end",
      "retrieve_past" = "function retrieve_past(var_value, delay_time, default_value, t, var_name, intermediaries, intermediary_names)
    # Handle empty intermediaries
    if isempty(intermediaries.saveval)
        return isnothing(default_value) ? var_value : default_value
    end

    # Ensure t and delay_time have compatible units
    if !(eltype(delay_time) <: Unitful.Quantity) && (eltype(t) <: Unitful.Quantity)
        delay_time = convert_u(delay_time, t)
    end

    # Extract variable index
    var_index = findfirst(==(var_name), intermediary_names)

    # Extract times and values
    ts = intermediaries.t
    ys = [val[var_index] for val in intermediaries.saveval]

    if isnothing(delay_time)
        return ys  # Return entire history
    end

    # # Handle current time t
    # if !(t in ts)
    #     ts = [ts; t]
    # end

    # Append current time if not present
    #if !isapprox(t, ts[end]; atol=1e-10)
    if abs(Unitful.ustrip(t - last(ts))) > 1e-10
        ts = [ts; t]
    end

    ys = [ys; var_value][1:length(ts)]  # Ensure ys is the same length as ts

    # Interval extraction
    first_time = t - delay_time

    # Ensure first_time is not before the first recorded time
    if first_time < ts[1]
        first_time = ts[1]
    end

    # Find indices for interval
    idx = findfirst(t -> t >= first_time, ts)

    # If no index found or if the index is the last element, return the current value
    if isnothing(idx) || idx == length(ts)
        return [var_value]
    else
        return ys[idx:end]
    end

end",
      "compute_delayN" = "function compute_delayN(inflow, accumulator::AbstractVector{Float64}, length_delay, order_delay::Float64)
    order_delay = round(Int, order_delay)
    d_accumulator = zeros(eltype(accumulator), order_delay)
    exit_rate_stage = accumulator / (length_delay / order_delay)
    d_accumulator[1] = inflow - exit_rate_stage[1]
    if order_delay > 1
        @inbounds for ord in 2:order_delay
            d_accumulator[ord] = exit_rate_stage[ord-1] - exit_rate_stage[ord]
        end
    end
    outflow = exit_rate_stage[order_delay] # in delayN, the outflow is the last stage
    return (outflow=outflow, update=d_accumulator)
end",
      "compute_smoothN" = "function compute_smoothN(input, state::AbstractVector{Float64}, length_delay, order::Float64)
    order = round(Int, order)
    d_state = zeros(eltype(state), order)
    adjustment_rate = (input - state[1]) / (length_delay / order)
    d_state[1] = adjustment_rate
    if order > 1
        @inbounds for ord in 2:order
            d_state[ord] = (state[ord - 1] - state[ord]) / (length_delay / order)
        end
    end
    outflow = state[end] # in smoothN, the outflow is the last state
    return (outflow=outflow, update=d_state)
end",
      "setup_delayN" = sprintf("function setup_delayN(initial_value, length_delay, order_delay::Float64, name::Symbol)
    # Compute the initial value for each accumulator
    # from https://www.simulistics.com/help/equations/functions/delay.htm
    order_delay = round(Int, order_delay) # Turn order into integer
    value = initial_value * length_delay / order_delay

    # Create a dictionary with names like \"name_acc1\", \"name_acc2\", ...
    #return Dict(string(name, \"_acc\", i) => value for i in 1:order_delay)
    return Dict(Symbol(name, \"%s\", i) => value for i in 1:order_delay)
end", .sdbuildR_env[["P"]][["acc_suffix"]]),
      "setup_smoothN" = sprintf("function setup_smoothN(initial_value, length_delay, order_delay::Float64, name::Symbol)
    # Compute the initial value for each accumulator
    # from https://www.simulistics.com/help/equations/functions/delay.htm
    order_delay = round(Int, order_delay) # Turn order into integer
    value = initial_value #* length_delay / order_delay

    # Create a dictionary with names like \"name_acc1\", \"name_acc2\", ...
    #return Dict(string(name, \"_acc\", i) => value for i in 1:order_delay)
    return Dict(Symbol(name, \"%s\", i) => value for i in 1:order_delay)
end", .sdbuildR_env[["P"]][["acc_suffix"]])
    ),
    "clean" = list(
      "saveat_func" = "# Function to save dataframe at specific times
function saveat_func(t, y, new_times)
    # Interpolate y at new_times
    itp(t, y, method = \"linear\", extrapolation = \"nearest\")(new_times)
end",
      "clean_df" = "function clean_df(prob, solve_out, init_names, intermediaries=nothing, intermediary_names=nothing)
    \"\"\"
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
\"\"\"

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
                push!(param_names, \"p$i\")
                val_stripped = isa(params[i], Quantity) ? ustrip(params[i]) : Float64(params[i])
                push!(param_values, val_stripped)
            end
        end
    elseif isa(params, Number)
        push!(param_names, \"p1\")
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
                    var_name = var_idx <= length(var_names) ? var_names[var_idx] : \"var_$var_idx\"

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
                        var_name = var_idx <= length(int_var_names) ? int_var_names[var_idx] : \"int_var_$var_idx\"

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
end",
      "clean_constants" = sprintf("function clean_constants(%s)
    %s = (; (name => isa(val, Unitful.Quantity) ? Unitful.ustrip(val) : val for (name, val) in pairs(%s))...)

    # Find keys where values are Float64 or Vector
    valid_keys = [k for k in keys(%s) if isa(constants[k], Float64) || isa(constants[k], Vector)]

    # Convert valid_keys to a tuple for NamedTuple construction
    valid_keys_tuple = Tuple(valid_keys)

    # Reconstruct filtered named tuple
    %s = NamedTuple{valid_keys_tuple}(%s[k] for k in valid_keys)

end
", .sdbuildR_env[["P"]][["parameter_name"]], .sdbuildR_env[["P"]][["parameter_name"]], .sdbuildR_env[["P"]][["parameter_name"]], .sdbuildR_env[["P"]][["parameter_name"]], .sdbuildR_env[["P"]][["parameter_name"]], .sdbuildR_env[["P"]][["parameter_name"]]),
      "clean_init" = sprintf("function clean_init(%s, %s)
    Dict(%s .=> Unitful.ustrip.(%s))
end", .sdbuildR_env[["P"]][["initial_value_name"]], .sdbuildR_env[["P"]][["initial_value_names"]], .sdbuildR_env[["P"]][["initial_value_names"]], .sdbuildR_env[["P"]][["initial_value_name"]])
    ),
    "ensemble" = list(
      "transform_intermediaries" = "function transform_intermediaries(intermediaries, intermediary_names=nothing)
    \"\"\"
Transform intermediaries to the same format as solve_out for unified processing.
This creates a pseudo-solution object that can be processed with the same logic.
\"\"\"
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
end",
      "generate_param_combinations" = "
\"\"\"
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
\"\"\"
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
            throw(ArgumentError(\"For non-crossed design, all parameter ranges must have the same length\"))
        end
        param_combinations = [[values_list[i][j] for i in 1:length(values_list)] for j in 1:lengths[1]]
    end

    # Calculate total simulations
    total_sims = length(param_combinations) * n_replicates

    return param_combinations, total_sims
end
",
      #       "ensemble_to_df" = "function ensemble_to_df(solve_out, init_names,
      #     intermediaries, intermediary_names, ensemble_n)
      #     \"\"\"
      # Unified processing where intermediaries are transformed to solve_out format first.
      # \"\"\"
      #     n_trajectories = length(solve_out)
      #
      #     # Get dimensions from first trajectory
      #     first_result = solve_out[1]
      #     t_vals = isa(first_result.t[1], Quantity) ? ustrip.(first_result.t) : first_result.t
      #
      #     # Determine number of variables and their names
      #     if isa(first_result.u[1], AbstractVector)
      #         n_vars = length(first_result.u[1])
      #         var_names = [string(name) for name in init_names]
      #     else
      #         n_vars = 1
      #         var_names = [string(init_names[1])]
      #     end
      #
      #     # Transform intermediaries to solve_out format
      #     transformed_intermediaries = nothing
      #     if !isnothing(intermediaries)
      #         transformed_intermediaries = transform_intermediaries(intermediaries, intermediary_names)
      #     end
      #
      #     # Process both solution and intermediaries with the same logic
      #     function process_solution_like(solutions, var_names_to_use, variable_prefix=\"\")
      #         if isnothing(solutions)
      #             return Int[], Float64[], String[], Float64[]
      #         end
      #
      #         total_rows = 0
      #         for sol in solutions
      #             if !isempty(sol.t)
      #                 if isa(sol.u[1], Union{AbstractVector, Tuple})
      #                     total_rows += length(sol.t) * length(sol.u[1])
      #                 else
      #                     total_rows += length(sol.t)
      #                 end
      #             end
      #         end
      #
      #         if total_rows == 0
      #             return Int[], Float64[], String[], Float64[]
      #         end
      #
      #         trajectory_vec = Vector{Int}(undef, total_rows)
      #         time_vec = Vector{Float64}(undef, total_rows)
      #         variable_vec = Vector{String}(undef, total_rows)
      #         value_vec = Vector{Float64}(undef, total_rows)
      #
      #         row_idx = 1
      #
      #         for (traj_idx, result) in enumerate(solutions)
      #             if !isempty(result.t)
      #                 t_stripped = isa(result.t[1], Quantity) ? ustrip.(result.t) : result.t
      #
      #                 for (t_idx, t_val) in enumerate(t_stripped)
      #                     u_val = result.u[t_idx]
      #
      #                     if isa(u_val, Union{AbstractVector, Tuple})
      #                         for (var_idx, var_val) in enumerate(u_val)
      #                             if !isa(var_val, Function)
      #                                 val_stripped = isa(var_val, Quantity) ? ustrip(var_val) : Float64(var_val)
      #                                 var_name = if isempty(variable_prefix)
      #                                     var_idx <= length(var_names_to_use) ? var_names_to_use[var_idx] : \"var_$var_idx\"
      #                                 else
      #                                     var_idx <= length(var_names_to_use) ? \"$(variable_prefix)$(var_names_to_use[var_idx])\" : \"$(variable_prefix)_$var_idx\"
      #                                 end
      #
      #                                 trajectory_vec[row_idx] = traj_idx
      #                                 time_vec[row_idx] = t_val
      #                                 variable_vec[row_idx] = var_name
      #                                 value_vec[row_idx] = val_stripped
      #                                 row_idx += 1
      #                             end
      #                         end
      #                     else
      #                         if !isa(u_val, Function)
      #                             val_stripped = isa(u_val, Quantity) ? ustrip(u_val) : Float64(u_val)
      #                             var_name = if isempty(variable_prefix)
      #                                 var_names_to_use[1]
      #                             else
      #                                 string(intermediary_names[1])
      #                             end
      #
      #                             trajectory_vec[row_idx] = traj_idx
      #                             time_vec[row_idx] = t_val
      #                             variable_vec[row_idx] = var_name
      #                             value_vec[row_idx] = val_stripped
      #                             row_idx += 1
      #                         end
      #                     end
      #                 end
      #             end
      #         end
      #
      #         # Trim to actual size
      #         resize!(trajectory_vec, row_idx - 1)
      #         resize!(time_vec, row_idx - 1)
      #         resize!(variable_vec, row_idx - 1)
      #         resize!(value_vec, row_idx - 1)
      #
      #         return trajectory_vec, time_vec, variable_vec, value_vec
      #     end
      #
      #     # Process main solution
      #     main_traj, main_time, main_var, main_val = process_solution_like(solve_out, var_names)
      #
      #     # Process intermediaries
      #     if !isnothing(transformed_intermediaries)
      #         int_var_names = [string(name) for name in intermediary_names]
      #         int_traj, int_time, int_var, int_val = process_solution_like(transformed_intermediaries, int_var_names)
      #
      #         # Combine all data
      #         append!(main_traj, int_traj)
      #         append!(main_time, int_time)
      #         append!(main_var, int_var)
      #         append!(main_val, int_val)
      #     end
      #
      #     # Create DataFrame
      #     timeseries_df = DataFrame(
      #         # count = main_traj,
      #         # Parameter ensemble index
      #         j = div.(main_traj .- 1, ensemble_n) .+ 1,
      #         # Trajectory index within the ensemble
      #         i = rem.(main_traj .- 1, ensemble_n) .+ 1,
      #         time = main_time,
      #         variable = main_var,
      #         value = main_val
      #     )
      #
      #     # Extract parameter matrix (same as before)
      #     param_names = String[]
      #     first_params = solve_out[1].p
      #     if isa(first_params, NamedTuple)
      #         for (key, val) in pairs(first_params)
      #             if !is_function_or_interp(val)
      #                 push!(param_names, string(key))
      #             end
      #         end
      #     elseif isa(first_params, AbstractVector)
      #         for i in eachindex(first_params)
      #             if !is_function_or_interp(first_params[i])
      #                 push!(param_names, \"p$i\")
      #             end
      #         end
      #     end
      #
      #     # Parameter matrix: (trajectories, parameters)
      #     param_matrix = Array{Float64, 2}(undef, n_trajectories, length(param_names))
      #
      #     for (traj_idx, result) in enumerate(solve_out)
      #         params = result.p
      #         for (param_idx, param_name) in enumerate(param_names)
      #             if isa(params, NamedTuple)
      #                 param_val = getproperty(params, Symbol(param_name))
      #             else
      #                 p_idx = parse(Int, param_name[2:end])
      #                 param_val = params[p_idx]
      #             end
      #
      #             param_val_stripped = isa(param_val, Quantity) ? ustrip(param_val) : param_val
      #             param_matrix[traj_idx, param_idx] = param_val_stripped
      #         end
      #     end
      #
      #     # Add parameter index
      #     b = 1:size(param_matrix, 1)
      #     param_matrix = hcat(
      #         # Parameter ensemble index
      #         div.(b .- 1, ensemble_n) .+ 1,
      #         # Trajectory index within the ensemble
      #         rem.(b .- 1, ensemble_n) .+ 1,
      #         param_matrix)
      #
      #     # Extract initial values matrix
      #     init_val_names = [string(name) for name in init_names]
      #
      #     # Initial values matrix: (trajectories, initial values)
      #     init_val_matrix = Array{Float64, 2}(undef, n_trajectories, length(init_val_names))
      #
      #     for (traj_idx, result) in enumerate(solve_out)
      #         init_vals = result.u0
      #
      #         if isa(init_vals, NamedTuple)
      #             for (init_idx, init_name) in enumerate(init_val_names)
      #                 init_val = getproperty(init_vals, Symbol(init_name))
      #                 init_val_stripped = isa(init_val, Quantity) ? ustrip(init_val) : init_val
      #                 init_val_matrix[traj_idx, init_idx] = init_val_stripped
      #             end
      #         elseif isa(init_vals, AbstractVector)
      #             for (init_idx, init_name) in enumerate(init_val_names)
      #                 init_val = init_vals[init_idx]
      #                 init_val_stripped = isa(init_val, Quantity) ? ustrip(init_val) : init_val
      #                 init_val_matrix[traj_idx, init_idx] = init_val_stripped
      #             end
      #         else
      #             # Single initial value
      #             init_val_stripped = isa(init_vals, Quantity) ? ustrip(init_vals) : init_vals
      #             init_val_matrix[traj_idx, 1] = init_val_stripped
      #         end
      #     end
      #
      #     # Add initial values index
      #     init_val_matrix = hcat(
      #         # Parameter ensemble index
      #         div.(b .- 1, ensemble_n) .+ 1,
      #         # Trajectory index within the ensemble
      #         rem.(b .- 1, ensemble_n) .+ 1,
      #         init_val_matrix)
      #
      #     return timeseries_df, param_matrix, param_names, init_val_matrix, init_val_names
      # end
      # ",


      "ensemble_to_df" = "function ensemble_to_df(solve_out, init_names,
    intermediaries, intermediary_names, ensemble_n)
    \"\"\"
Unified processing where intermediaries are transformed to solve_out format first.
Parameters are also returned in long format.
\"\"\"
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
    function process_solution_like(solutions, var_names_to_use, variable_prefix=\"\")
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
                                    var_idx <= length(var_names_to_use) ? var_names_to_use[var_idx] : \"var_$var_idx\"
                                else
                                    var_idx <= length(var_names_to_use) ? \"$(variable_prefix)$(var_names_to_use[var_idx])\" : \"$(variable_prefix)_$var_idx\"
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

    # Extract parameter names
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
                push!(param_names, \"p$i\")
            end
        end
    end

    # Create parameters DataFrame in long format
    param_df = DataFrame()
    if !isempty(param_names)
        param_traj_vec = Int[]
        param_j_vec = Int[]
        param_i_vec = Int[]
        param_name_vec = String[]
        param_value_vec = Float64[]

        for (traj_idx, result) in enumerate(solve_out)
            params = result.p
            for param_name in param_names
                if isa(params, NamedTuple)
                    param_val = getproperty(params, Symbol(param_name))
                else
                    p_idx = parse(Int, param_name[2:end])
                    param_val = params[p_idx]
                end

                param_val_stripped = isa(param_val, Quantity) ? ustrip(param_val) : param_val

                push!(param_traj_vec, traj_idx)
                push!(param_j_vec, div(traj_idx - 1, ensemble_n) + 1)  # Parameter ensemble index
                push!(param_i_vec, rem(traj_idx - 1, ensemble_n) + 1)  # Trajectory index within ensemble
                push!(param_name_vec, param_name)
                push!(param_value_vec, param_val_stripped)
            end
        end

        param_df = DataFrame(
            j = param_j_vec,
            i = param_i_vec,
            variable = param_name_vec,
            value = param_value_vec
        )
    end

    # Extract initial values in long format
    init_val_names = [string(name) for name in init_names]

    init_df = DataFrame()
    if !isempty(init_val_names)
        init_traj_vec = Int[]
        init_j_vec = Int[]
        init_i_vec = Int[]
        init_name_vec = String[]
        init_value_vec = Float64[]

        for (traj_idx, result) in enumerate(solve_out)
            init_vals = result.u0

            if isa(init_vals, NamedTuple)
                for init_name in init_val_names
                    init_val = getproperty(init_vals, Symbol(init_name))
                    init_val_stripped = isa(init_val, Quantity) ? ustrip(init_val) : init_val

                    push!(init_traj_vec, traj_idx)
                    push!(init_j_vec, div(traj_idx - 1, ensemble_n) + 1)
                    push!(init_i_vec, rem(traj_idx - 1, ensemble_n) + 1)
                    push!(init_name_vec, init_name)
                    push!(init_value_vec, init_val_stripped)
                end
            elseif isa(init_vals, AbstractVector)
                for (init_idx, init_name) in enumerate(init_val_names)
                    init_val = init_vals[init_idx]
                    init_val_stripped = isa(init_val, Quantity) ? ustrip(init_val) : init_val

                    push!(init_traj_vec, traj_idx)
                    push!(init_j_vec, div(traj_idx - 1, ensemble_n) + 1)
                    push!(init_i_vec, rem(traj_idx - 1, ensemble_n) + 1)
                    push!(init_name_vec, init_name)
                    push!(init_value_vec, init_val_stripped)
                end
            else
                # Single initial value
                init_val_stripped = isa(init_vals, Quantity) ? ustrip(init_vals) : init_vals

                push!(init_traj_vec, traj_idx)
                push!(init_j_vec, div(traj_idx - 1, ensemble_n) + 1)
                push!(init_i_vec, rem(traj_idx - 1, ensemble_n) + 1)
                push!(init_name_vec, init_val_names[1])
                push!(init_value_vec, init_val_stripped)
            end
        end

        init_df = DataFrame(
            j = init_j_vec,
            i = init_i_vec,
            variable = init_name_vec,
            value = init_value_vec
        )
    end

    return timeseries_df, param_df, init_df
end",

      #       "ensemble_to_df_threaded" = "function ensemble_to_df_threaded(solve_out, init_names, intermediaries, intermediary_names, ensemble_n)
      #     \"\"\"
      # Unified processing where intermediaries are transformed to solve_out format first.
      # \"\"\"
      #     n_trajectories = length(solve_out)
      #
      #     # Get dimensions from first trajectory
      #     first_result = solve_out[1]
      #     t_vals = isa(first_result.t[1], Quantity) ? ustrip.(first_result.t) : first_result.t
      #
      #     # Determine number of variables and their names
      #     if isa(first_result.u[1], AbstractVector)
      #         n_vars = length(first_result.u[1])
      #         var_names = [string(name) for name in init_names]
      #     else
      #         n_vars = 1
      #         var_names = [string(init_names[1])]
      #     end
      #
      #     # Transform intermediaries to solve_out format
      #     transformed_intermediaries = nothing
      #     if !isnothing(intermediaries)
      #         transformed_intermediaries = transform_intermediaries(intermediaries, intermediary_names)
      #     end
      #
      #     # Process both solution and intermediaries with the same logic
      #     function process_solution_like(solutions, var_names_to_use, variable_prefix=\"\")
      #         if isnothing(solutions)
      #             return Int[], Float64[], String[], Float64[]
      #         end
      #
      #         # First pass: calculate row counts for each trajectory
      #         row_counts = Vector{Int}(undef, length(solutions))
      #
      #         Base.Threads.@threads for i in 1:length(solutions)
      #             sol = solutions[i]
      #             count = 0
      #             if !isempty(sol.t)
      #                 if isa(sol.u[1], Union{AbstractVector, Tuple})
      #                     count = length(sol.t) * length(sol.u[1])
      #                 else
      #                     count = length(sol.t)
      #                 end
      #             end
      #             row_counts[i] = count
      #         end
      #
      #         total_rows = sum(row_counts)
      #
      #         if total_rows == 0
      #             return Int[], Float64[], String[], Float64[]
      #         end
      #
      #         # Pre-allocate output arrays
      #         trajectory_vec = Vector{Int}(undef, total_rows)
      #         time_vec = Vector{Float64}(undef, total_rows)
      #         variable_vec = Vector{String}(undef, total_rows)
      #         value_vec = Vector{Float64}(undef, total_rows)
      #
      #         # Calculate start indices for each trajectory
      #         start_indices = Vector{Int}(undef, length(solutions))
      #         start_indices[1] = 1
      #         for i in 2:length(solutions)
      #             start_indices[i] = start_indices[i-1] + row_counts[i-1]
      #         end
      #
      #         # Second pass: fill arrays in parallel
      #         Base.Threads.@threads for traj_idx in 1:length(solutions)
      #             result = solutions[traj_idx]
      #             if !isempty(result.t)
      #                 t_stripped = isa(result.t[1], Quantity) ? ustrip.(result.t) : result.t
      #
      #                 row_idx = start_indices[traj_idx]
      #
      #                 for (t_idx, t_val) in enumerate(t_stripped)
      #                     u_val = result.u[t_idx]
      #
      #                     if isa(u_val, Union{AbstractVector, Tuple})
      #                         for (var_idx, var_val) in enumerate(u_val)
      #                             if !isa(var_val, Function)
      #                                 val_stripped = isa(var_val, Quantity) ? ustrip(var_val) : Float64(var_val)
      #                                 var_name = if isempty(variable_prefix)
      #                                     var_idx <= length(var_names_to_use) ? var_names_to_use[var_idx] : \"var_$var_idx\"
      #                                 else
      #                                     var_idx <= length(var_names_to_use) ? \"$(variable_prefix)$(var_names_to_use[var_idx])\" : \"$(variable_prefix)_$var_idx\"
      #                                 end
      #
      #                                 trajectory_vec[row_idx] = traj_idx
      #                                 time_vec[row_idx] = t_val
      #                                 variable_vec[row_idx] = var_name
      #                                 value_vec[row_idx] = val_stripped
      #                                 row_idx += 1
      #                             end
      #                         end
      #                     else
      #                         if !isa(u_val, Function)
      #                             val_stripped = isa(u_val, Quantity) ? ustrip(u_val) : Float64(u_val)
      #                             var_name = if isempty(variable_prefix)
      #                                 var_names_to_use[1]
      #                             else
      #                                 string(intermediary_names[1])
      #                             end
      #
      #                             trajectory_vec[row_idx] = traj_idx
      #                             time_vec[row_idx] = t_val
      #                             variable_vec[row_idx] = var_name
      #                             value_vec[row_idx] = val_stripped
      #                             row_idx += 1
      #                         end
      #                     end
      #                 end
      #             end
      #         end
      #
      #         return trajectory_vec, time_vec, variable_vec, value_vec
      #     end
      #
      #     # Process main solution
      #     main_traj, main_time, main_var, main_val = process_solution_like(solve_out, var_names)
      #
      #     # Process intermediaries
      #     if !isnothing(transformed_intermediaries)
      #         int_var_names = [string(name) for name in intermediary_names]
      #         int_traj, int_time, int_var, int_val = process_solution_like(transformed_intermediaries, int_var_names)
      #
      #         # Combine all data
      #         append!(main_traj, int_traj)
      #         append!(main_time, int_time)
      #         append!(main_var, int_var)
      #         append!(main_val, int_val)
      #     end
      #
      #     # Create DataFrame
      #     timeseries_df = DataFrame(
      #         # count = main_traj,
      #         # Parameter ensemble index
      #         j = div.(main_traj .- 1, ensemble_n) .+ 1,
      #         # Trajectory index within the ensemble
      #         i = rem.(main_traj .- 1, ensemble_n) .+ 1,
      #         time = main_time,
      #         variable = main_var,
      #         value = main_val
      #     )
      #
      #     # Extract parameter matrix (same as before)
      #     param_names = String[]
      #     first_params = solve_out[1].p
      #     if isa(first_params, NamedTuple)
      #         for (key, val) in pairs(first_params)
      #             if !is_function_or_interp(val)
      #                 push!(param_names, string(key))
      #             end
      #         end
      #     elseif isa(first_params, AbstractVector)
      #         for i in eachindex(first_params)
      #             if !is_function_or_interp(first_params[i])
      #                 push!(param_names, \"p$i\")
      #             end
      #         end
      #     end
      #
      #     # Parameter matrix: (trajectories, parameters)
      #     param_matrix = Array{Float64, 2}(undef, n_trajectories, length(param_names))
      #
      #     Base.Threads.@threads for traj_idx in 1:length(solve_out)
      #         result = solve_out[traj_idx]
      #         params = result.p
      #         for (param_idx, param_name) in enumerate(param_names)
      #             if isa(params, NamedTuple)
      #                 param_val = getproperty(params, Symbol(param_name))
      #             else
      #                 p_idx = parse(Int, param_name[2:end])
      #                 param_val = params[p_idx]
      #             end
      #
      #             param_val_stripped = isa(param_val, Quantity) ? ustrip(param_val) : param_val
      #             param_matrix[traj_idx, param_idx] = param_val_stripped
      #         end
      #     end
      #
      #     # Add parameter index
      #     b = 1:size(param_matrix, 1)
      #     param_matrix = hcat(
      #         # Parameter ensemble index
      #         div.(b .- 1, ensemble_n) .+ 1,
      #         # Trajectory index within the ensemble
      #         rem.(b .- 1, ensemble_n) .+ 1,
      #         param_matrix)
      #
      #     # Extract initial values matrix
      #     init_val_names = [string(name) for name in init_names]
      #
      #     # Initial values matrix: (trajectories, initial values)
      #     init_val_matrix = Array{Float64, 2}(undef, n_trajectories, length(init_val_names))
      #
      #     Base.Threads.@threads for traj_idx in 1:length(solve_out)
      #         result = solve_out[traj_idx]
      #         init_vals = result.u0
      #
      #         if isa(init_vals, NamedTuple)
      #             for (init_idx, init_name) in enumerate(init_val_names)
      #                 init_val = getproperty(init_vals, Symbol(init_name))
      #                 init_val_stripped = isa(init_val, Quantity) ? ustrip(init_val) : init_val
      #                 init_val_matrix[traj_idx, init_idx] = init_val_stripped
      #             end
      #         elseif isa(init_vals, AbstractVector)
      #             for (init_idx, init_name) in enumerate(init_val_names)
      #                 init_val = init_vals[init_idx]
      #                 init_val_stripped = isa(init_val, Quantity) ? ustrip(init_val) : init_val
      #                 init_val_matrix[traj_idx, init_idx] = init_val_stripped
      #             end
      #         else
      #             # Single initial value
      #             init_val_stripped = isa(init_vals, Quantity) ? ustrip(init_vals) : init_vals
      #             init_val_matrix[traj_idx, 1] = init_val_stripped
      #         end
      #     end
      #
      #     # Add initial values index
      #     init_val_matrix = hcat(
      #         # Parameter ensemble index
      #         div.(b .- 1, ensemble_n) .+ 1,
      #         # Trajectory index within the ensemble
      #         rem.(b .- 1, ensemble_n) .+ 1,
      #         init_val_matrix)
      #
      #     return timeseries_df, param_matrix, param_names, init_val_matrix, init_val_names
      # end
      # ",

      "ensemble_to_df_threaded" = "function ensemble_to_df_threaded(solve_out, init_names, intermediaries, intermediary_names, ensemble_n)
    \"\"\"
Threaded version with unified processing where intermediaries are transformed to solve_out format first.
Parameters are also returned in long format.
\"\"\"
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
    function process_solution_like(solutions, var_names_to_use, variable_prefix=\"\")
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
                                    var_idx <= length(var_names_to_use) ? var_names_to_use[var_idx] : \"var_$var_idx\"
                                else
                                    var_idx <= length(var_names_to_use) ? \"$(variable_prefix)$(var_names_to_use[var_idx])\" : \"$(variable_prefix)_$var_idx\"
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

    # Extract parameter names
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
                push!(param_names, \"p$i\")
            end
        end
    end

    # Create parameters DataFrame in long format (with threading)
    param_df = DataFrame()
    if !isempty(param_names)
        # Pre-allocate arrays
        total_param_rows = n_trajectories * length(param_names)
        param_j_vec = Vector{Int}(undef, total_param_rows)
        param_i_vec = Vector{Int}(undef, total_param_rows)
        param_name_vec = Vector{String}(undef, total_param_rows)
        param_value_vec = Vector{Float64}(undef, total_param_rows)

        Base.Threads.@threads for traj_idx in 1:n_trajectories
            result = solve_out[traj_idx]
            params = result.p

            for (param_idx, param_name) in enumerate(param_names)
                row_idx = (traj_idx - 1) * length(param_names) + param_idx

                if isa(params, NamedTuple)
                    param_val = getproperty(params, Symbol(param_name))
                else
                    p_idx = parse(Int, param_name[2:end])
                    param_val = params[p_idx]
                end

                param_val_stripped = isa(param_val, Quantity) ? ustrip(param_val) : param_val

                param_j_vec[row_idx] = div(traj_idx - 1, ensemble_n) + 1
                param_i_vec[row_idx] = rem(traj_idx - 1, ensemble_n) + 1
                param_name_vec[row_idx] = param_name
                param_value_vec[row_idx] = param_val_stripped
            end
        end

        param_df = DataFrame(
            j = param_j_vec,
            i = param_i_vec,
            variable = param_name_vec,
            value = param_value_vec
        )
    end

    # Extract initial values in long format (with threading)
    init_val_names = [string(name) for name in init_names]

    init_df = DataFrame()
    if !isempty(init_val_names)
        # Pre-allocate arrays
        total_init_rows = n_trajectories * length(init_val_names)
        init_j_vec = Vector{Int}(undef, total_init_rows)
        init_i_vec = Vector{Int}(undef, total_init_rows)
        init_name_vec = Vector{String}(undef, total_init_rows)
        init_value_vec = Vector{Float64}(undef, total_init_rows)

        Base.Threads.@threads for traj_idx in 1:n_trajectories
            result = solve_out[traj_idx]
            init_vals = result.u0

            if isa(init_vals, NamedTuple)
                for (init_idx, init_name) in enumerate(init_val_names)
                    row_idx = (traj_idx - 1) * length(init_val_names) + init_idx
                    init_val = getproperty(init_vals, Symbol(init_name))
                    init_val_stripped = isa(init_val, Quantity) ? ustrip(init_val) : init_val

                    init_j_vec[row_idx] = div(traj_idx - 1, ensemble_n) + 1
                    init_i_vec[row_idx] = rem(traj_idx - 1, ensemble_n) + 1
                    init_name_vec[row_idx] = init_name
                    init_value_vec[row_idx] = init_val_stripped
                end
            elseif isa(init_vals, AbstractVector)
                for (init_idx, init_name) in enumerate(init_val_names)
                    row_idx = (traj_idx - 1) * length(init_val_names) + init_idx
                    init_val = init_vals[init_idx]
                    init_val_stripped = isa(init_val, Quantity) ? ustrip(init_val) : init_val

                    init_j_vec[row_idx] = div(traj_idx - 1, ensemble_n) + 1
                    init_i_vec[row_idx] = rem(traj_idx - 1, ensemble_n) + 1
                    init_name_vec[row_idx] = init_name
                    init_value_vec[row_idx] = init_val_stripped
                end
            else
                # Single initial value
                row_idx = (traj_idx - 1) * length(init_val_names) + 1
                init_val_stripped = isa(init_vals, Quantity) ? ustrip(init_vals) : init_vals

                init_j_vec[row_idx] = div(traj_idx - 1, ensemble_n) + 1
                init_i_vec[row_idx] = rem(traj_idx - 1, ensemble_n) + 1
                init_name_vec[row_idx] = init_val_names[1]
                init_value_vec[row_idx] = init_val_stripped
            end
        end

        init_df = DataFrame(
            j = init_j_vec,
            i = init_i_vec,
            variable = init_name_vec,
            value = init_value_vec
        )
    end

    return timeseries_df, param_df, init_df
end",
      "ensemble_summ" = "function ensemble_summ(timeseries_df, quantiles=[0.025, 0.0975])
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
                q_str = replace(string(q), r\"^0\\.\" => \"\")
                result = merge(result, (Symbol(\"q$q_str\") => NaN,))
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
                q_str = replace(string(q), r\"^0\\.\" => \"\")
                result = merge(result, (Symbol(\"q$q_str\") => Statistics.quantile(clean_values, q),))
            end
        end

        return result
    end

    return stats_df
end",
      "ensemble_summ_threaded" = "function ensemble_summ_threaded(timeseries_df, quantiles=[0.025, 0.975])
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
        q_str = replace(string(q), r\"^0\\.\" => \"\")
        quantile_arrays[\"q$q_str\"] = Vector{Float64}(undef, n_groups)
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
                q_str = replace(string(q), r\"^0\\.\" => \"\")
                quantile_arrays[\"q$q_str\"][i] = NaN
            end
        else
            # Compute stats
            mean_vals[i] = mean(clean_values)
            variance_vals[i] = var(clean_values)
            median_vals[i] = Statistics.median(clean_values)
            for q in quantiles
                q_str = replace(string(q), r\"^0\\.\" => \"\")
                quantile_arrays[\"q$q_str\"][i] = Statistics.quantile(clean_values, q)
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
        q_str = replace(string(q), r\"^0\\.\" => \"\")
        stats_df[!, Symbol(\"q$q_str\")] = quantile_arrays[\"q$q_str\"]
    end

    return stats_df
end
"
    )
  )

  return(func_def)
}
