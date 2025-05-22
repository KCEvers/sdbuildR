
#' Customary functions written in julia
#'
#' @return String with julia code
#' @noRd
#'
get_func_julia = function(){
  func_def = c(
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

    #                "ramp" = "# Make ramp signal\nfunction ramp(times; start_t_ramp, end_t_ramp, start_h_ramp = 0.0, end_h_ramp = 1.0)
    #
    #     x = [start_t_ramp, end_t_ramp]
    #     y = [start_h_ramp, end_h_ramp]
    #
    #     # If the ramp is after the start time, add a zero at the start
    #     if start_t_ramp > first(times)
    #         x = [first(times); x]
    #         y = [0; y]
    #     end
    #
    #     func = itp(x, y, method = \"linear\", extrapolation = 2)
    #
    #     return(func)
    # end",
    "ramp" = "function ramp(; start_t_ramp, end_t_ramp, start_h_ramp = 0.0, end_h_ramp = 1.0)
    # If times has units, but the ramp times don't, convert them to the same units
    if eltype(times) <: Unitful.Quantity
        if !(eltype(start_t_ramp) <: Unitful.Quantity)
            start_t_ramp = convert_u(start_t_ramp, time_units)
        end
        if !(eltype(end_t_ramp) <: Unitful.Quantity)
            end_t_ramp = convert_u(end_t_ramp, time_units)
        end
    else
        # If times does not have units, but start_t_ramp does, convert the ramp times to the same units as time_units
        if eltype(start_t_ramp) <: Unitful.Quantity
            start_t_ramp = Unitful.ustrip(convert_u(start_t_ramp, time_units))
        end
        if eltype(end_t_ramp) <: Unitful.Quantity
            end_t_ramp = Unitful.ustrip(convert_u(end_t_ramp, time_units))
        end
    end


    # Ensure start_h_ramp and end_h_ramp are both of the same type
    if eltype(start_h_ramp) <: Unitful.Quantity && !(eltype(end_h_ramp) <: Unitful.Quantity)
        end_h_ramp = convert_u(end_h_ramp, Unitful.unit(start_h_ramp))
        add_y = convert_u(0.0, Unitful.unit(start_h_ramp))
    elseif !(eltype(start_h_ramp) <: Unitful.Quantity) && eltype(end_h_ramp) <: Unitful.Quantity
        start_h_ramp = convert_u(start_h_ramp, Unitful.unit(end_h_ramp))
        add_y = convert_u(0.0, Unitful.unit(end_h_ramp))
    else
        add_y = 0.0
    end

    x = [start_t_ramp, end_t_ramp]
    y = [start_h_ramp, end_h_ramp]

    # If the ramp is after the start time, add a zero at the start
    if start_t_ramp > first(times)
        x = [first(times); x]
        y = [add_y; y]
    end

    func = itp(x, y, method = \"linear\", extrapolation = \"nearest\")

    return(func)
end ",
    #                "step" = "# Make step signal\nfunction step(times; start_t_step, h_step = 1.0)
    #
    #     x = [start_t_step, times[2]]
    #     y = [h_step, h_step]
    #
    #     # If the step is after the start time, add a zero at the start
    #     if start_t_step > first(times)
    #         x = [first(times); x]
    #         y = [0; y]
    #     end
    #
    #     func = itp(x, y, method = \"constant\", extrapolation = \"nearest\")
    #
    #     return(func)
    # end",
    "step" = "# Make step signal
function step(; start_t_step, h_step = 1.0)

    # If times has units, but the ramp times don't, convert them to the same units
    if eltype(times) <: Unitful.Quantity
        if !(eltype(start_t_step) <: Unitful.Quantity)
            start_t_step = convert_u(start_t_step, time_units)
        end
    else
        # If times does not have units, but start_t_step does, convert the ramp times to the same units as time_units
        if eltype(start_t_step) <: Unitful.Quantity
            start_t_step = Unitful.ustrip(convert_u(start_t_step, time_units))
        end
    end

    if eltype(h_step) <: Unitful.Quantity
      add_y = convert_u(0.0, Unitful.unit(h_step))
    else
      add_y = 0.0
    end

    x = [start_t_step, times[2]]
    y = [h_step, h_step]

    # If the step is after the start time, add a zero at the start
    if start_t_step > first(times)
        x = [first(times); x]
        y = [add_y; y]
    end

    func = itp(x, y, method = \"constant\", extrapolation = \"nearest\")

    return(func)
end",
    "@constraints" = "# Constraints macro
macro constraints(exprs...)
    pairs = map(exprs) do ex
        quote
            ($(string(ex)), $(esc(ex)))
        end
    end
    quote
        [$(pairs...)]
    end
end",
    #                "pulse" = "# Make pulse signal\nfunction pulse(times; start_t_pulse, h_pulse = 1.0, w_pulse = 1.0 * time_units, repeat_interval = nothing)
    #
    #     # Define start and end times of pulses
    #     last_time = last(times)
    #     # If no repeats, set end of pulse to after end time
    #     step_size = isnothing(repeat_interval) ? last_time * 2 : repeat_interval
    #     start_ts = collect(start_t_pulse:step_size:last_time)
    #     end_ts = start_ts .+ w_pulse
    #
    #     # Build signal as vectors of times and y-values
    #     signal_times = [start_ts; end_ts]
    #     signal_y = [fill(h_pulse, length(start_ts)); fill(0, length(end_ts))]
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
function pulse(; start_t_pulse, h_pulse = 1.0, w_pulse = 1.0 * time_units, repeat_interval = nothing)
    # If times has units, but the pulse times don't, convert them to the same units
    if eltype(times) <: Unitful.Quantity
        if !(eltype(start_t_pulse) <: Unitful.Quantity)
            start_t_pulse = convert_u(start_t_pulse, time_units)
        end
        if !(eltype(w_pulse) <: Unitful.Quantity)
            w_pulse = convert_u(w_pulse, time_units)
        end
        if (!isnothing(repeat_interval) && !(eltype(repeat_interval) <: Unitful.Quantity))
            repeat_interval = convert_u(repeat_interval, time_units)
        end
    else
        # If times does not have units, but start_t_pulse does, convert the pulse times to the same units as time_units
        if eltype(start_t_pulse) <: Unitful.Quantity
            start_t_pulse = Unitful.ustrip(convert_u(start_t_pulse, time_units))
        end
        if eltype(w_pulse) <: Unitful.Quantity
            w_pulse = Unitful.ustrip(convert_u(w_pulse, time_units))
        end
        if (!isnothing(repeat_interval) && eltype(repeat_interval) <: Unitful.Quantity)
            repeat_interval = Unitful.ustrip(convert_u(repeat_interval, time_units))
        end
    end

    # Define start and end times of pulses
    last_time = last(times)
    # If no repeats, set end of pulse to after end time
    step_size = isnothing(repeat_interval) ? last_time * 2 : repeat_interval
    start_ts = collect(start_t_pulse:step_size:last_time)
    end_ts = start_ts .+ w_pulse

    # Build signal as vectors of times and y-values
    signal_times = [start_ts; end_ts]
    signal_y = [fill(h_pulse, length(start_ts)); fill(0, length(end_ts))]

    if eltype(h_pulse) <: Unitful.Quantity
      add_y = convert_u(0.0, Unitful.unit(h_pulse))
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
        return round(scaled_x, digits=0) / 10.0^digits
    end
end",
    "logit" = "# Logit function\nfunction logit(p)
    return log(p / (1 - p))
end",
    "expit" = "# Expit function\nfunction expit(x)
    return 1 / (1+exp(-x))
end",
    "sigmoid" = "function sigmoid(x; slope=1, midpoint=0)
    @assert isfinite(slope) && isfinite(midpoint) \"slope and midpoint must be numeric\"
    1 / (1 + exp(-slope * (x - midpoint)))
end

# Also define without keyword arguments
function sigmoid(x, slope, midpoint)
    @assert isfinite(slope) && isfinite(midpoint) \"slope and midpoint must be numeric\"
    1 / (1 + exp(-slope * (x - midpoint)))
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

    #                "weeks" = "function weeks(t, time_units)
    #     Unitful.uconvert(u\"wk\", t * Unitful.uparse(time_units))  # Parse the string dynamically
    # end",
    #                "convert_u" = "# Set or convert unit\nfunction convert_u(x, unit_def)
    #
    #     # In case unit_def is not identified as a unit, extract unit
    #     if eltype(unit_def) <: Unitful.Quantity
    #         unit_def = Unitful.unit(unit_def)  # Extract the unit from a Quantity
    #     end
    #
    #     # If x already has a unit, convert
    #     if eltype(x) <: Unitful.Quantity
    #         try
    #             return Unitful.uconvert.(unit_def, x)
    #         catch e
    #             if isa(e, Unitful.DimensionError)
    #                 error(\"Cannot convert $(unit(x)) to $unit_def: incompatible dimensions\")
    #             else
    #                 rethrow(e)
    #             end
    #         end
    #     else
    #         return x .* unit_def
    #     end
    # end
    # ",
#     "convert_u" = sprintf("# Set or convert unit\nfunction convert_u(x, unit_def)
#     # Parse unit_def into a Unitful.Unit
#     target_unit = if unit_def isa Unitful.Quantity
#         Unitful.unit(unit_def)  # Extract unit from Quantity (e.g., 1u\"wk\" -> u\"wk\")
#     elseif unit_def isa Unitful.Units
#         unit_def  # Already a unit (e.g., u\"wk\")
#     elseif unit_def isa AbstractString
#         try
#             Unitful.uparse(unit_def, unit_context = %s)  # Parse string to unit (e.g., \"wk\" -> u\"wk\")
#         catch e
#             error(\"Invalid unit string: $unit_def\")
#         end
#     else
#         error(\"unit_def must be a Quantity, Unit, or String, got $(typeof(unit_def))\")
#     end
#
#     # Handle x based on whether it has a unit
#     #if x isa Unitful.Quantity
#     if eltype(x) <: Unitful.Quantity
#         # x has a unit, convert it
#         try
#             return Unitful.uconvert.(target_unit, x)
#         catch e
#             if isa(e, Unitful.DimensionError)
#                 error(\"Cannot convert $(Unitful.unit.(x)) to $target_unit: incompatible dimensions\")
#             else
#                 rethrow(e)
#             end
#         end
#     else
#         # x has no unit, set it via multiplication
#         return x .* target_unit
#     end
# end", P$unit_context),

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


function convert_u(x::Unitful.Quantity, unit_def::String)
    try
        unit_def = Unitful.uparse(unit_def, unit_context = %s)  # Parse string to unit (e.g., \"wk\" -> u\"wk\")

        if Unitful.unit(x) == unit_def
            return x  # No conversion needed
        else
            Unitful.uconvert.(unit_def, x)
        end
    catch e
        error(\"Invalid unit string: $unit_def\")
    end
end

# If x is not a Unitful.Quantity but Float64:
function convert_u(x::Float64, unit_def::Unitful.Quantity)
    x * Unitful.unit(unit_def)
end

function convert_u(x::Float64, unit_def::Unitful.Units)
    x * unit_def
end

function convert_u(x::Float64, unit_def::String)
    try
        unit_def = Unitful.uparse(unit_def, unit_context = unit_context)  # Parse string to unit (e.g., \"wk\" -> u\"wk\")
        x * unit_def
    catch e
        error(\"Invalid unit string: $unit_def\")
    end
end", P$unit_context),


    # "seasonal" = "# Create seasonal wave\nfunction seasonal(;wave_unit=u\"yr\", wave_peak=0u\"yr\")
    #     (t, u=wave_unit, p=wave_peak) -> cos.(Unitful.ustrip.(Unitful.uconvert.(u, t - p)))
    # end"
    "seasonal" = "# Create seasonal wave\nfunction seasonal(t, period = u\"1yr\", shift = u\"0yr\")
    phase = 2 * pi * (t - shift) / period  # π radians
    return(cos(phase))
end",
    "\\u2295" = "# Define the operator \\u2295 for the modulus
function \\u2295(x, y)
    return mod(x, y)
end",
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
    "retrieve_past"= "# Function to retrieve past values

function retrieve_past(var_value, delay_time, default_value, t, var_name, single_or_interval, intermediaries, intermediary_names)
    # Handle empty intermediaries
    if isempty(intermediaries.saveval)
        return isnothing(default_value) ? var_value : default_value
    end

    # Ensure t and delay_time have compatible units
    if !(eltype(delay_time) <: Unitful.Quantity) && (eltype(t) <: Unitful.Quantity)
        # delay_time = delay_time * unit(t)
        delay_time = convert_u(delay_time, t)
    end

    # Extract variable index
    var_index = findfirst(==(var_name), intermediary_names)
    # if isnothing(var_index)
    #     error(\"Variable '$var_name' not found in intermediary_names: $intermediary_names\")
    # end

    # Extract times and values
    ts = intermediaries.t
    ys = [val[var_index] for val in intermediaries.saveval]

    # Handle current time t
    if !(t in ts)
        ts = [ts; t]
    end

    ys = [ys; var_value][1:length(ts)]  # Ensure ys is the same length as ts

    # Single value extraction
    if single_or_interval == \"single\"
        extract_t = t - delay_time
        if extract_t < ts[1]
            return isnothing(default_value) ? ys[1] : default_value
        elseif extract_t == t
            return var_value
        end

        # Interpolate
        # interp = LinearInterpolation(ys, ts, extrapolation_bc = Flat())
        # Define interpolation function
        return itp(ts, ys, method = \"linear\")(extract_t)

    end

    # Interval extraction
    if single_or_interval == \"interval\"
        if isnothing(delay_time)
            return ys  # Return entire history
        end

        first_time = t - delay_time
        if first_time < ts[1]
            first_time = ts[1]
        end

        # Find indices for interval
        idx = findfirst(t -> t >= first_time, ts)
        if isnothing(idx) || idx == length(ts)
            return [var_value]
        end

        return ys[idx:end]
    end

end
",
    "saveat_func" = "# Function to save dataframe at specific times
function saveat_func(t, y, new_times)
    # Interpolate y at new_times
    itp(t, y, method = \"linear\", extrapolation = \"nearest\")(new_times)
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
    outflow = exit_rate_stage[order_delay]
    return (outflow=outflow, update=d_accumulator)
end",
    "setup_delayN" = sprintf("function setup_delayN(initial_value, length_delay, order_delay::Float64, name::Symbol)
    # Compute the initial value for each accumulator
    # from https://www.simulistics.com/help/equations/functions/delay.html
    order_delay = round(Int, order_delay) # Turn order into integer
    value = initial_value * length_delay / order_delay

    # Create a dictionary with names like \"name_acc1\", \"name_acc2\", ...
    #return Dict(string(name, \"_acc\", i) => value for i in 1:order_delay)
    return Dict(Symbol(name, \"%s\", i) => value for i in 1:order_delay)
end",P$delayN_acc_suffix)

  )

  return(func_def)
}

