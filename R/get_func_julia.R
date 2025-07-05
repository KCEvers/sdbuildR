
#' Customary functions written in Julia
#'
#' @return List with Julia code
#' @noRd
get_func_julia = function(){

  func_def = list(
    #   "itp"= "# Extrapolation function\nfunction itp(x, y; method = \"linear\", extrapolation = 2)
    #
    #     # Ensure y is sorted along x
    #     idx = sortperm(x)
    #     x = x[idx]
    #     y = y[idx]
    #
    #     # Extrapolation extrapolation: What happens outside of defined values?
    #     # Rule 1: return NaN; Rule 2: return closest value
    #     rule_method = ifelse(extrapolation == 1, NaN, ifelse(extrapolation == 2, Flat(), \"?\"))
    #
    #     if method == \"constant\"
    #         func = extrapolate(interpolate((x,), y, Gridded(Constant{Previous}())), rule_method)
    #     elseif method == \"linear\"
    #         func = linear_interpolation(x, y, extrapolation_bc=rule_method)
    #     end
    #
    #     return(func)
    # end",
    # extrapolation = 1: return NA when outside of bounds
    # extrapolation = 2: return nearest value when outside of bounds
    "custom_func" = list(
    "itp"= "# Extrapolation function\nfunction itp(x, y; method = \"linear\", extrapolation = \"nearest\")

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
    # **to do: ExtrapolationType.None throws error instead of returning NaN

    #                "ramp" = "# Make ramp signal\nfunction ramp(times; start, end, start_h_ramp = 0.0, height = 1.0)
    #
    #     x = [start, end]
    #     y = [start_h_ramp, height]
    #
    #     # If the ramp is after the start time, add a zero at the start
    #     if start > first(times)
    #         x = [first(times); x]
    #         y = [0; y]
    #     end
    #
    #     func = itp(x, y, method = \"linear\", extrapolation = 2)
    #
    #     return(func)
    # end",
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
    #                "step" = "# Make step signal\nfunction step(times; start, height = 1.0)
    #
    #     x = [start, times[2]]
    #     y = [height, height]
    #
    #     # If the step is after the start time, add a zero at the start
    #     if start > first(times)
    #         x = [first(times); x]
    #         y = [0; y]
    #     end
    #
    #     func = itp(x, y, method = \"constant\", extrapolation = \"nearest\")
    #
    #     return(func)
    # end",
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
#     "@constraints" = "# Constraints macro
# macro constraints(exprs...)
#     pairs = map(exprs) do ex
#         quote
#             ($(string(ex)), $(esc(ex)))
#         end
#     end
#     quote
#         [$(pairs...)]
#     end
# end",
    #                "pulse" = "# Make pulse signal\nfunction pulse(times; start, height = 1.0, width = 1.0 * time_units, repeat_interval = nothing)
    #
    #     # Define start and end times of pulses
    #     last_time = last(times)
    #     # If no repeats, set end of pulse to after end time
    #     step_size = isnothing(repeat_interval) ? last_time * 2 : repeat_interval
    #     start_ts = collect(start:step_size:last_time)
    #     end_ts = start_ts .+ width
    #
    #     # Build signal as vectors of times and y-values
    #     signal_times = [start_ts; end_ts]
    #     signal_y = [fill(height, length(start_ts)); fill(0, length(end_ts))]
    #
    #     # If the first pulse is after the start time, add a zero at the start
    #     if minimum(start_ts) > first(times)
    #         signal_times = [first(times); signal_times]
    #         signal_y = [0; signal_y]
    #     end
    #
    #     # If the last pulse doesn't cover the end, add a zero at the end
    #     # (I don't fully understand why this is necessary, but otherwise it gives incorrect results with repeat_interval <= 0)
    #     if maximum(end_ts) < last_time
    #         signal_times = [signal_times; last_time]
    #         signal_y = [signal_y; 0]
    #     end
    #
    #     # Sort by time
    #     perm = sortperm(signal_times)
    #     x = signal_times[perm]
    #     y = signal_y[perm]
    #     func = itp(x, y, method = \"constant\", extrapolation = \"nearest\")
    #
    #     return(func)
    # end",
    "pulse" = "# Make pulse signal
function pulse(times, time_units, start, height = 1.0, width = 1.0 * time_units, repeat_interval = nothing)
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

    @assert period > 0 \"The period of the seasonal wave must be greater than 0.\"

    time_vec = times[1]:dt:times[2]
    phase = 2 * pi .* (time_vec .- shift) ./ period  # π radians
    y = cos.(phase)
    func = itp(time_vec, y, method = \"linear\", extrapolation = \"nearest\")

    return(func)
end",
    # ** constant interpolation is not supported with units! linear is

    # ** other custom_func
    "IM_round" = "# Convert Insight Maker's Round() function to R\n# Difference: in Insight Maker, Round(.5) = 1; in R, round(.5) = 0; in julia, round(.5) = 0.0\nfunction IM_round(x::Real, digits::Int=0)
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
    @assert isfinite(slope) && isfinite(midpoint) && isfinite(upper) \"slope, midpoint, and upper must be numeric\"
    upper / (1 + exp(-slope * (x - midpoint)))
end
",
    #     "nonnegative" = "# Prevent non-negativity\nfunction nonnegative(x)
    #     if eltype(x) <: Unitful.Quantity
    #         max.(0.0, Unitful.ustrip(x)) .* Unitful.unit.(x)
    #     else
    #         max.(0.0, x)
    #     end
    # end",
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
    #                "rdist" = "function rdist(a,b)
    #     StatsBase.wsample(a, b, 1)
    # end",
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
    # "IM_length"= "function IM_length(x)
    #     if isa(x, AbstractString)
    #         return length(x)
    #     else
    #         return length(x)
    #     end
    # end",
    "IM_contains" = "function IM_contains(haystack, needle)
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
    "IM_filter" = "function IM_filter(y::Vector{T}, condition_func::Function) where T
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
# "seasonal" = "# Create seasonal wave\nfunction seasonal(;wave_unit=u\"yr\", wave_peak=0u\"yr\")
#     (t, u=wave_unit, p=wave_peak) -> cos.(Unitful.ustrip.(Unitful.uconvert.(u, t - p)))
# end"
#     "seasonal" = "# Create seasonal wave \nfunction seasonal(t, period = u\"1yr\", shift = u\"0yr\")
#     phase = 2 * pi * (t - shift) / period  # π radians
#     return(cos(phase))
# end",

"\\u2295" = "# Define the operator \\u2295 for the modulus
function \\u2295(x, y)
    return mod(x, y)
end"),

"unit_func" = list(

"convert_u"= sprintf("# Set or convert unit wrappers per type
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


# Previously: convert_u supported passing a string as unit_def, but this requires a global variable unit_context or to pass unit_context to many functions.
#
# function convert_u(x::Unitful.Quantity, unit_def::String)
# try
# unit_def = Unitful.uparse(unit_def, unit_context = unit_context)  # Parse string to unit (e.g., \"wk\" -> u\"wk\")
#
# if Unitful.unit(x) == unit_def
# return x  # No conversion needed
# else
#   Unitful.uconvert.(unit_def, x)
# end
# catch e
# error(\"Invalid unit string: $unit_def\")
#     end
# end
#
# function convert_u(x::Float64, unit_def::String)
# try
# unit_def = Unitful.uparse(unit_def, unit_context = unit_context)  # Parse string to unit (e.g., \"wk\" -> u\"wk\")
# x * unit_def
# catch e
# error(\"Invalid unit string: $unit_def\")
#     end
# end

    # "retrieve_past"= "function retrieve_past(var_value, delay_time, default_value, t, var_name, single_or_interval, intermediaries, intermediary_names)
    #
    # 	# Ensure t and delay_time are of the same type
    # 	if !(eltype(delay_time) <: Unitful.Quantity) & (eltype(t) <: Unitful.Quantity)
    # 		delay_time = convert_u(delay_time, t)
    # 	end
    #
    #     # Extract single value from the past
    # 	if single_or_interval == \"single\"
    # 		extract_t = t - delay_time
    #
    # 		# If trying to retrieve a value in the \"past\" (i.e. before times[1]), use default value if specified, otherwise use value at first time point
    # 		if extract_t < 0
    # 			if isnothing(default_value)
    #                 if isempty(intermediaries.saveval)
    #                     return var_value
    #                 else
    #                     return intermediaries.saveval[1][findfirst(isequal(var_name), intermediary_names)]
    #                 end
    # 			else
    # 				return default_value
    # 			end
    # 		elseif extract_t == 0 || extract_t == t
    #       return var_value
    # 		end
    #
    # 	# Or: extract interval of past values
    # 	elseif single_or_interval == \"interval\"
    #
    #         # Check if intermediaries.saveval is empty
    #         if isempty(intermediaries.saveval)
    #             return var_value
    #         end
    #
    #         # Create vector of times
    #         if !(t in intermediaries.t)
    #             extract_t = [intermediaries.t; t]
    #         else
    #             extract_t = intermediaries.t
    #         end
    #
    # 		# If no past interval is specified, access entire history
    # 		if !isnothing(delay_time)
    # 			first_time = t - delay_time
    #
    # 			if first_time < 0
    # 				first_time = intermediaries.t[1]  # Set to start time if negative
    # 			end
    #
    # 			# Extract from first_time up until t
    # 			_, first_time_idx = findmin(abs.(extract_t .- first_time))
    # 			extract_t = extract_t[first_time_idx:end]
    #
    #             if length(extract_t) == 1
    #                 return var_value
    #             end
    # 		end
    # 	end
    #
    #     # Check if intermediaries.saveval is empty
    #     if isempty(intermediaries.saveval)
    #         # If empty, return default value if specified, otherwise return NaN
    #         if isnothing(default_value)
    #             return var_value
    #         else
    #             return default_value
    #         end
    #     end
    #
    # 	## Create a DataFrame from the saved values
    # 	i#ntermediary_df = DataFrame(intermediaries.saveval, Symbol.(intermediary_names))
    #
    #     # Add t if not in intermediaries.t yet
    #     if !(t in intermediaries.t)
    #         ts = [intermediaries.t; t]
    #     else
    #         ts = intermediaries.t
    #     end
    #
    #     #ys = [intermediary_df[!, var_name]; var_value]
    #    # ys = ys[1:length(ts)]  # Ensure ys is the same length as ts
    #
    #     var_index = findfirst(==(var_name), intermediary_names)
    #     ys = [[val[var_index] for val in intermediaries.saveval]; var_value][1:length(ts)]
    #
    # 	# Define interpolation function
    #     itp(ts, ys, method = \"linear\", extrapolation = \"nearest\")(extract_t)
    #
    # end
    # ",
#     "retrieve_past"= "# Function to retrieve past values
#
# function retrieve_past(var_value, delay_time, default_value, t, var_name, single_or_interval, intermediaries, intermediary_names)
#     # Handle empty intermediaries
#     if isempty(intermediaries.saveval)
#         return isnothing(default_value) ? var_value : default_value
#     end
#
#     # Ensure t and delay_time have compatible units
#     if !(eltype(delay_time) <: Unitful.Quantity) && (eltype(t) <: Unitful.Quantity)
#         # delay_time = delay_time * unit(t)
#         delay_time = convert_u(delay_time, t)
#     end
#
#     # Extract variable index
#     var_index = findfirst(==(var_name), intermediary_names)
#     # if isnothing(var_index)
#     #     error(\"Variable '$var_name' not found in intermediary_names: $intermediary_names\")
#     # end
#
#     # Extract times and values
#     ts = intermediaries.t
#     ys = [val[var_index] for val in intermediaries.saveval]
#
#     # Handle current time t
#     if !(t in ts)
#         ts = [ts; t]
#     end
#
#     ys = [ys; var_value][1:length(ts)]  # Ensure ys is the same length as ts
#
#     # Single value extraction
#     if single_or_interval == \"single\"
#         extract_t = t - delay_time
#         if extract_t < ts[1]
#             return isnothing(default_value) ? ys[1] : default_value
#         elseif extract_t == t
#             return var_value
#         end
#
#         # Interpolate
#         # interp = LinearInterpolation(ys, ts, extrapolation_bc = Flat())
#         # Define interpolation function
#         return itp(ts, ys, method = \"linear\")(extract_t)
#
#     end
#
#     # Interval extraction
#     if single_or_interval == \"interval\"
#         if isnothing(delay_time)
#             return ys  # Return entire history
#         end
#
#         first_time = t - delay_time
#         if first_time < ts[1]
#             first_time = ts[1]
#         end
#
#         # Find indices for interval
#         idx = findfirst(t -> t >= first_time, ts)
#         if isnothing(idx) || idx == length(ts)
#             return [var_value]
#         end
#
#         return ys[idx:end]
#     end
#
# end
# ",

"past" = list(
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

end",

    # "setunit_flow" = "# Define function to set unit of flow; Alleviate users from dividing the flow in the equation by the time unit if they have specified the desired units in the units property of the flow
    # function setunit_flow(x, unit_def)
    #     # If trying to set the unit throws an error
    #     try
    #         return convert_u(x, unit_def)
    #     catch e
    #         if isa(e, ErrorException) | isa(e, Unitful.DimensionError)
    #             try
    #                 # In cases where e.g. x = 1u\"m\", unit_def = u\"m/d\"
    #                 # I want to find the necessary divisor.
    #                 div_required = x / unit_def
    #                 # if Unitful.unit(x / div_required) == unit_def   # this is always true
    #                 # If the unit of the divisor is a time unit, return the flow divided by the divisor
    #                 if typeof(Unitful.unit(div_required)) <: Unitful.TimeUnits
    #                     return x / div_required
    #                 else
    #                     error(\"Cannot set unit of flow: $x to $unit_def\")
    #                 end
    #
    #             catch
    #                 error(\"Cannot set unit of flow: $x to $unit_def\")
    #             end
    #         else
    #             error(\"Cannot set unit of flow: $x to $unit_def\")
    #         end
    #     end
    # end"

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
end",P$delayN_acc_suffix),

"setup_smoothN" = sprintf("function setup_smoothN(initial_value, length_delay, order_delay::Float64, name::Symbol)
    # Compute the initial value for each accumulator
    # from https://www.simulistics.com/help/equations/functions/delay.htm
    order_delay = round(Int, order_delay) # Turn order into integer
    value = initial_value * length_delay / order_delay # same initialization for smoothN() as delayN() as verified by Insight Maker

    # Create a dictionary with names like \"name_acc1\", \"name_acc2\", ...
    #return Dict(string(name, \"_acc\", i) => value for i in 1:order_delay)
    return Dict(Symbol(name, \"%s\", i) => value for i in 1:order_delay)
end",P$smoothN_acc_suffix)),

"clean" = list(
  "saveat_func" = "# Function to save dataframe at specific times
function saveat_func(t, y, new_times)
    # Interpolate y at new_times
    itp(t, y, method = \"linear\", extrapolation = \"nearest\")(new_times)
end",
"clean_df" = sprintf("function clean_df(%s, %s, %s, %s, %s;
                  %s=nothing, %s=nothing)
    # Always create df from solve_out and init_names
    %s = Unitful.ustrip.(DataFrame(%s, [:time; %s]))

    # If intermediary data was passed, process and append
    if %s !== nothing && %s !== nothing
      # Error is thrown for dataframe creation of there is only one variable
      # Necessary to add first.() because otherwise the column is a list in R, causing issues in plot(sim)
      if length(%s) == 1
        intermediary_df = Unitful.ustrip.(DataFrame(:temp => first.(%s.saveval)))
        rename!(intermediary_df, :temp => %s...)
      else
        intermediary_df = Unitful.ustrip.(DataFrame(%s.saveval, %s))
      end

      # Remove variables in intermediary_var that are in initial_value_names
      if any(name -> name in %s, %s)
        select!(intermediary_df, setdiff(%s, %s))
      end

      # Merge df with intermediary values
      %s = hcat(%s, intermediary_df)
    end

    # Ensure correct number of rows, resample if needed
    if %s != %s
        # Linearly interpolate to reduce stored values to saveat
    new_times = collect(%s[1]:%s:%s[2]) # Create new time vector
    %s = DataFrame(Dict(
    :time => new_times,
        [Symbol(col) => saveat_func(%s.time, %s[!, col], new_times) for col in names(%s) if col != \"time\"]...
    ))
    elseif nrow(%s) != length(%s[1]:%s:%s[2])
        %s = Unitful.ustrip.(%s)
        new_times = collect(%s[1]:%s:%s[2])  # use passed dt
        %s = DataFrame(Dict(
            :time => new_times,
            [Symbol(col) => saveat_func(%s.time, %s[!, col], new_times)
             for col in names(%s) if col != \"time\"]...
        ))
    end

    # Convert to long
    long = stack(%s, Not(:time), variable_name=:variable, value_name=:value)

    return long
end",
                     P$solution_name, P$initial_value_names,
                     P$times_name, P$timestep_name, P$saveat_name,
                     P$intermediaries, P$intermediary_names,
                     P$sim_df_name, P$solution_name, P$initial_value_names,
                     P$intermediaries, P$intermediary_names,
                     P$intermediary_names,
                     P$intermediaries,
                     P$intermediary_names,
                     P$intermediaries, P$intermediary_names,

                     P$intermediary_names, P$initial_value_names,
                     P$intermediary_names, P$initial_value_names,

                     P$sim_df_name, P$sim_df_name,
                     P$timestep_name, P$saveat_name, P$times_name, P$saveat_name, P$times_name, P$sim_df_name, P$sim_df_name,P$sim_df_name, P$sim_df_name,

                     P$sim_df_name, P$times_name, P$timestep_name, P$times_name,
                     P$times_name, P$times_name, P$times_name, P$timestep_name, P$times_name,
                     P$sim_df_name, P$sim_df_name, P$sim_df_name, P$sim_df_name, P$sim_df_name ),

"clean_constants" = sprintf("function clean_constants(%s)
    %s = (; (name => isa(val, Unitful.Quantity) ? Unitful.ustrip(val) : val for (name, val) in pairs(%s))...)

    # Find keys where values are Float64 or Vector
    valid_keys = [k for k in keys(%s) if isa(constants[k], Float64) || isa(constants[k], Vector)]

    # Convert valid_keys to a tuple for NamedTuple construction
    valid_keys_tuple = Tuple(valid_keys)

    # Reconstruct filtered named tuple
    %s = NamedTuple{valid_keys_tuple}(%s[k] for k in valid_keys)

end
", P[["parameter_name"]], P[["parameter_name"]], P[["parameter_name"]], P[["parameter_name"]], P[["parameter_name"]], P[["parameter_name"]]),
"clean_init" = sprintf("function clean_init(%s, %s)
    Dict(%s .=> Unitful.ustrip.(%s))
end", P[["initial_value_name"]], P[["initial_value_names"]], P[["initial_value_names"]], P[["initial_value_name"]])),

"ensemble" = list(


  "all_timestep_stats" = "function all_timestep_stats(intermediaries, key = :saveval, qs = [0.05, 0.95])
    n_steps = length(getfield(first(intermediaries), key))  # number of time steps
    n_vars = length(first(getfield(first(intermediaries), key)))  # number of variables

    # Preallocate storage
    means   = zeros(n_vars, n_steps)
    vars_   = zeros(n_vars, n_steps)
    medians = zeros(n_vars, n_steps)
    qlows   = zeros(n_vars, n_steps)
    qhighs  = zeros(n_vars, n_steps)

    for i in 1:n_steps
        vals = [collect(getfield(sv, key)[i]) for sv in intermediaries]
        mat = reduce(hcat, vals)  # rows: variables, cols: trajectories

        means[:, i]   .= mapslices(mean, mat; dims=2)[:]
        vars_[:, i]   .= mapslices(var, mat; dims=2)[:]
        medians[:, i] .= mapslices(Statistics.median, mat; dims=2)[:]
        qlows[:, i]   .= mapslices(x -> Statistics.quantile(x, qs[1]), mat; dims=2)[:]
        qhighs[:, i]  .= mapslices(x -> Statistics.quantile(x, qs[2]), mat; dims=2)[:]
    end

    return (
        mean = means,
        var = vars_,
        median = medians,
        qlow = qlows,
        qhigh = qhighs
    )
end",
#'
#'
#' "all_timestep_stats" = "function all_timestep_stats(intermediaries, key = :saveval, qs = (0.05, 0.95))    n_steps = length(getfield(first(intermediaries), key))          # time steps
#'     n_vars  = length(getfield(first(intermediaries), key)[1])       # variables
#'     n_ens   = length(intermediaries)                                # ensemble size
#'
#'     # Preallocate storage
#'     means   = zeros(n_vars, n_steps)
#'     vars_   = zeros(n_vars, n_steps)
#'     medians = zeros(n_vars, n_steps)
#'     qlows   = zeros(n_vars, n_steps)
#'     qhighs  = zeros(n_vars, n_steps)
#'
#'     # Reuse buffer to avoid repeated allocation
#'     buffer = Vector{Float64}(undef, n_ens)
#'
#'     for t in 1:n_steps
#'         for v in 1:n_vars
#'             # Extract the v-th variable at time step t across all trajectories
#'             @inbounds for i in 1:n_ens
#'                 buffer[i] = getfield(intermediaries[i], key)[t][v]
#'             end
#'
#'             # Summary statistics
#'             @inbounds means[v, t]   = mean(buffer)
#'             @inbounds vars_[v, t]   = var(buffer)
#'             @inbounds medians[v, t] = median(buffer)
#'             @inbounds qlows[v, t]   = quantile(buffer, qs[1])
#'             @inbounds qhighs[v, t]  = quantile(buffer, qs[2])
#'         end
#'     end
#'
#'     return (
#'         mean = means,
#'         var = vars_,
#'         median = medians,
#'         qlow = qlows,
#'         qhigh = qhighs
#'     )
#' end",
#'
#'

#   "summary_to_long" = "function summary_to_long(stats, times, var_names)
#     dfs = DataFrame[]
#     # for (statname, matrix) in stats
#     for statname in keys(stats)
#
#         df = permutedims(DataFrame(getfield(stats, statname), :auto))
#         rename!(df, var_names)
#         df.time = times
#         long = stack(df, var_names; variable_name=:variable, value_name=:value)
#         long.statistic .= statname
#         push!(dfs, long)
#     end
#     vcat(dfs...)
# end",

"summary_to_long" = "function summary_to_long(stats, times, var_names)
    dfs = DataFrame[]
    # for (statname, matrix) in stats
    for statname in keys(stats)

        df = permutedims(DataFrame(getfield(stats, statname), :auto))
        rename!(df, var_names)

        if (statname == first(keys(stats)))
            df.time = times
        end

        long = stack(df, var_names; variable_name=:variable, value_name=statname)

        # Remove the variable column
        if (statname != first(keys(stats)))
            select!(long, Not(:variable))
        end

        # long.statistic .= statname
        push!(dfs, long)
    end

    hcat(dfs...)
end",

  "create_ensemble_summ" = "function create_ensemble_summ(solve_out, init_names, intermediaries, intermediary_names, qs = [0.05, 0.95])

    stats = all_timestep_stats(solve_out, :u, qs);
    summ = summary_to_long(stats, solve_out[1].t, init_names)

    if !isnothing(intermediaries)
        stats = all_timestep_stats(intermediaries, :saveval, qs);
        summ = vcat(summ, summary_to_long(stats, intermediaries[1].t, intermediary_names))
    end

    return summ
end
",

# "ensemble_to_df" = "
# function ensemble_to_df(solve_out, init_names, times, dt, saveat;
#                   intermediaries=nothing, intermediary_names=nothing)
#     dfs = DataFrame[]
#
#     for i in eachindex(solve_out)
#
#         df = clean_df(solve_out[i], init_names, times, dt, saveat;
#           intermediaries=intermediaries[i], intermediary_names=intermediary_names)
#
#         insertcols!(df, 1, :simulation .=> i)
#         push!(dfs, df)
#     end
#
#     vcat(dfs...)
# end"

"ensemble_to_df" = "function ensemble_to_df(solve_out, init_names, times, dt, saveat;
                   intermediaries=nothing, intermediary_names=nothing)
    n = length(solve_out)
    dfs = Vector{DataFrame}(undef, n)

    for i in 1:n
        df = clean_df(
            solve_out[i], init_names, times, dt, saveat;
            intermediaries = isnothing(intermediaries) ? nothing : intermediaries[i],
            intermediary_names = intermediary_names
        )
        df.simulation = fill(i, nrow(df))
        dfs[i] = df
    end

    return reduce(vcat, dfs)
end"

# "solve_out_to_df" = "function solve_out_to_df(solve_out, var_names)
#     dfs = DataFrame[]
#
#     for (i, sol) in enumerate(solve_out)
#         df = DataFrame(sol; copycols=true)
#         rename!(df, [:time; var_names])
#         insertcols!(df, 1, :simulation .=> i)
#         long = stack(df, var_names; variable_name = :variable, value_name = :value)
#         push!(dfs, long)
#     end
#
#     vcat(dfs...)
# end",
#
# "intermediaries_to_df" = "function intermediaries_to_df(intermediaries, var_names)
#     dfs = DataFrame[]
#
#     for (i, sv) in enumerate(intermediaries)
#         df = DataFrame(sv.saveval, var_names)
#         rename!(df, var_names)
#         # df.time = sv.t
#         # Put simulation number as first column
#         # insertcols!(df, 1, :simulation .=> i)
#         long = stack(df, var_names; variable_name = :variable, value_name = :value)
#         push!(dfs, long)
#     end
#
#     vcat(dfs...)
# end"

))

  return(func_def)
}

