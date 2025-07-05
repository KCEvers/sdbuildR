# past
module past
using Unitful
using ..custom_func: itp
using ..unit_func: convert_u

function retrieve_delay(var_value, delay_time, default_value, t, var_name, intermediaries, intermediary_names)
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

    if !isapprox(t, ts[end]; atol=1e-10)
        ts = [ts; t]  # Append current time if not present
    end

    ys = [ys; var_value][1:length(ts)]  # Ensure ys is the same length as ts

    # Single value extraction
    extract_t = t - delay_time
    if extract_t < ts[1]
        return isnothing(default_value) ? ys[1] : default_value
    elseif extract_t == t
        return var_value
    else
        return itp(ts, ys, method = "linear")(extract_t)
    end

end

function retrieve_past(var_value, delay_time, default_value, t, var_name, intermediaries, intermediary_names)
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

    if !isapprox(t, ts[end]; atol=1e-10)
        ts = [ts; t]  # Append current time if not present
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

end

function compute_delayN(inflow, accumulator::AbstractVector{Float64}, length_delay, order_delay::Float64)
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
end

function compute_smoothN(input, state::AbstractVector{Float64}, length_delay, order::Float64)
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
end

function setup_delayN(initial_value, length_delay, order_delay::Float64, name::Symbol)
    # Compute the initial value for each accumulator
    # from https://www.simulistics.com/help/equations/functions/delay.htm
    order_delay = round(Int, order_delay) # Turn order into integer
    value = initial_value * length_delay / order_delay

    # Create a dictionary with names like "name_acc1", "name_acc2", ...
    #return Dict(string(name, "_acc", i) => value for i in 1:order_delay)
    return Dict(Symbol(name, "_acc", i) => value for i in 1:order_delay)
end

function setup_smoothN(initial_value, length_delay, order_delay::Float64, name::Symbol)
    # Compute the initial value for each accumulator
    # from https://www.simulistics.com/help/equations/functions/delay.htm
    order_delay = round(Int, order_delay) # Turn order into integer
    value = initial_value * length_delay / order_delay # same initialization for smoothN() as delayN() as verified by Insight Maker

    # Create a dictionary with names like "name_acc1", "name_acc2", ...
    #return Dict(string(name, "_acc", i) => value for i in 1:order_delay)
    return Dict(Symbol(name, "_acc", i) => value for i in 1:order_delay)
end
end
