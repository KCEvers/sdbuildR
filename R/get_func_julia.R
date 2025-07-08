
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
end"
#       "sample_weighted" = "
# \"\"\"
#     sample_weighted(x, weights)
#
# Sample a single element from x using weights.
# \"\"\"
# function sample_weighted(x::AbstractVector, weights::AbstractVector)
#     r = rand()
#     cumsum_weights = cumsum(weights)
#     idx = findfirst(w -> r <= w, cumsum_weights)
#     return x[idx]
# end
# ",
#
#       "sample_weighted_without_replacement" = "
# \"\"\"
#     sample_weighted_without_replacement(x, weights, size)
#
# Sample without replacement using weights.
# \"\"\"
# function sample_weighted_without_replacement(x::AbstractVector, weights::AbstractVector, size::Integer)
#     n = length(x)
#     result = eltype(x)[]
#     available_indices = collect(1:n)
#     current_weights = copy(weights)
#
#     for _ in 1:size
#         # Normalize current weights
#         if sum(current_weights) ≈ 0
#             break
#         end
#         norm_weights = current_weights ./ sum(current_weights)
#
#         # Sample from available indices
#         r = rand()
#         cumsum_weights = cumsum(norm_weights)
#         local_idx = findfirst(w -> r <= w, cumsum_weights)
#
#         # Get the actual index and add to result
#         actual_idx = available_indices[local_idx]
#         push!(result, x[actual_idx])
#
#         # Remove selected index and corresponding weight
#         deleteat!(available_indices, local_idx)
#         deleteat!(current_weights, local_idx)
#     end
#
#     return result
# end
# ",
#       "sample" = "
# \"\"\"
#       sample(x, size=length(x); replace=false, prob=nothing)
#
#       Sample from vector x with or without replacement.
#       Equivalent to R's sample(x, size, replace, prob).
# \"\"\"
# function sample(x::AbstractVector, size::Integer=length(x); replace::Bool=false, prob::Union{Nothing,AbstractVector}=nothing)
#     if size < 0
#         throw(ArgumentError(\"'size' must be non-negative\"))
#     end
#
#     if size == 0
#         return eltype(x)[]
#     end
#
#     n = length(x)
#
#     if !replace && size > n
#         throw(ArgumentError(\"cannot take a sample larger than the population when 'replace=false'\"))
#     end
#
#     if prob === nothing
#         if replace
#             return [x[rand(1:n)] for _ in 1:size]
#         else
#             if size == n
#                 return x[randperm(n)]
#             else
#                 indices = randperm(n)[1:size]
#                 return x[indices]
#             end
#         end
#     else
#         # Weighted sampling
#         if length(prob) != n
#             throw(ArgumentError(\"'prob' should be of length $(n)\"))
#         end
#
#         if any(p -> p < 0, prob)
#             throw(ArgumentError(\"'prob' should contain non-negative values\"))
#         end
#
#         if sum(prob) ≈ 0
#             throw(ArgumentError(\"'prob' should contain at least some positive values\"))
#         end
#
#         # Normalize probabilities
#         weights = prob ./ sum(prob)
#
#         if replace
#             return [sample_weighted(x, weights) for _ in 1:size]
#         else
#             return sample_weighted_without_replacement(x, weights, size)
#         end
#     end
# end
#
# \"\"\"
#     sample(n::Integer, size=n; replace=false, prob=nothing)
#
# Sample from 1:n with or without replacement.
# Equivalent to R's sample(n, size, replace, prob).
#       \"\"\"
# function sample(n::Integer, size::Integer=n; replace::Bool=false, prob::Union{Nothing,AbstractVector}=nothing)
#     if n < 0
#         throw(ArgumentError(\"'n' must be non-negative\"))
#     end
#     return sample(1:n, size; replace=replace, prob=prob)
# end
#
# \"\"\"
#       sample(x; replace=false, prob=nothing)
#
#       Sample all elements from x (permutation when replace=false).
#       Equivalent to R's sample(x).
# \"\"\"
# function sample(x::AbstractVector; replace::Bool=false, prob::Union{Nothing,AbstractVector}=nothing)
#     return sample(x, length(x); replace=replace, prob=prob)
# end
# ",
#       "sample_int" = "\"\"\"
#       sample_int(n, size=n; replace=false, prob=nothing)
#
#       Sample integers from 1:n with or without replacement.
#       Equivalent to R's sample.int(n, size, replace, prob).
# More efficient than sample(1:n, ...) for large n.
# \"\"\"
# function sample_int(n::Integer, size::Integer=n; replace::Bool=false, prob::Union{Nothing,AbstractVector}=nothing)
#     if n < 0
#         throw(ArgumentError(\"'n' must be non-negative\"))
#     end
#
#     if size < 0
#         throw(ArgumentError(\"'size' must be non-negative\"))
#     end
#
#     if size == 0
#         return Int[]
#     end
#
#     if !replace && size > n
#         throw(ArgumentError(\"cannot take a sample larger than the population when 'replace=false'\"))
#     end
#
#     if prob === nothing
#         if replace
#             return [rand(1:n) for _ in 1:size]
#         else
#             if size == n
#                 return randperm(n)
#             else
#                 return randperm(n)[1:size]
#             end
#         end
#     else
#         # Use the existing weighted sampling on 1:n
#         return sample(1:n, size; replace=replace, prob=prob)
#     end
# end",
#       "seq"= "
#       \"\"\"
#     seq(from, to, by=1)
#
# Generate sequence from 'from' to 'to' by step 'by'.
# Equivalent to R's seq(from, to, by).
# \"\"\"
#       function seq(from::Real, to::Real, by::Real=1)
#       if by == 0
#       throw(ArgumentError(\"'by' must be non-zero\"))
#       end
#
#       if (to - from) * by < 0
#       return typeof(from)[]  # Empty sequence
#       end
#
#       return collect(from:by:to)
#       end
#
#       \"\"\"
#     seq(from, to; length_out)
#
# Generate sequence from 'from' to 'to' with specified length.
# Equivalent to R's seq(from, to, length.out=length_out).
# \"\"\"
#       function seq(from::Real, to::Real; length_out::Integer)
#       if length_out < 0
#       throw(ArgumentError(\"'length_out' must be non-negative\"))
#       end
#
#       if length_out == 0
#       return Float64[]
#       elseif length_out == 1
#       return [from]
#       else
#         return collect(range(from, to, length=length_out))
#       end
#       end
#
#       \"\"\"
#     seq(from, to; along_with)
#
# Generate sequence with same length as along_with.
# Equivalent to R's seq(from, to, along.with=along_with).
# \"\"\"
#       function seq(from::Real, to::Real; along_with::AbstractVector)
#       return seq(from, to; length_out=length(along_with))
#       end
#
#       \"\"\"
#     seq(; along_with)
#
# Generate sequence 1:length(along_with).
# Equivalent to R's seq(along.with=along_with) or seq_along(along_with).
# \"\"\"
#       function seq(; along_with::AbstractVector)
#       return collect(1:length(along_with))
#       end",
#
#       "seq_along" = "
#       \"\"\"
#     seq_along(x)
#
# Generate sequence 1:length(x).
# Equivalent to R's seq_along(x).
# \"\"\"
#       function seq_along(x::AbstractVector)
#       return collect(1:length(x))
#       end
#       ",
#
#       "seq_len" = "
#       \"\"\"
#     seq_len(n)
#
# Generate sequence 1:n.
# Equivalent to R's seq_len(n).
# \"\"\"
#       function seq_len(n::Integer)
#       if n < 0
#       throw(ArgumentError(\"'n' must be non-negative\"))
#       end
#       return n == 0 ? Int[] : collect(1:n)
#       end
#       "
      ),

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
      # "clean_df" = sprintf("function clean_df(%s, %s, %s, %s, %s;
      #                   %s=nothing, %s=nothing)
      #     # Always create df from solve_out and init_names
      #     %s = Unitful.ustrip.(DataFrame(%s, [:time; %s]))
      #
      #     # If intermediary data was passed, process and append
      #     if %s !== nothing && %s !== nothing
      #       # Error is thrown for dataframe creation of there is only one variable
      #       # Necessary to add first.() because otherwise the column is a list in R, causing issues in plot(sim)
      #       if length(%s) == 1
      #         intermediary_df = Unitful.ustrip.(DataFrame(:temp => first.(%s.saveval)))
      #         rename!(intermediary_df, :temp => %s...)
      #       else
      #         intermediary_df = Unitful.ustrip.(DataFrame(%s.saveval, %s))
      #       end
      #
      #       # Remove variables in intermediary_var that are in initial_value_names
      #       if any(name -> name in %s, %s)
      #         select!(intermediary_df, setdiff(%s, %s))
      #       end
      #
      #       # Merge df with intermediary values
      #       %s = hcat(%s, intermediary_df)
      #     end
      #
      #     # Ensure correct number of rows, resample if needed
      #     if %s != %s
      #         # Linearly interpolate to reduce stored values to saveat
      #     new_times = collect(%s[1]:%s:%s[2]) # Create new time vector
      #     %s = DataFrame(Dict(
      #     :time => new_times,
      #         [Symbol(col) => saveat_func(%s.time, %s[!, col], new_times) for col in names(%s) if col != \"time\"]...
      #     ))
      #     elseif nrow(%s) != length(%s[1]:%s:%s[2])
      #         %s = Unitful.ustrip.(%s)
      #         new_times = collect(%s[1]:%s:%s[2])  # use passed dt
      #         %s = DataFrame(Dict(
      #             :time => new_times,
      #             [Symbol(col) => saveat_func(%s.time, %s[!, col], new_times)
      #              for col in names(%s) if col != \"time\"]...
      #         ))
      #     end
      #
      #     # Convert to long
      #     long = stack(%s, Not(:time), variable_name=:variable, value_name=:value)
      #
      #     return long
      # end",
      #                      P$solution_name, P$initial_value_names,
      #                      P$times_name, P$timestep_name, P$saveat_name,
      #                      P$intermediaries, P$intermediary_names,
      #                      P$sim_df_name, P$solution_name, P$initial_value_names,
      #                      P$intermediaries, P$intermediary_names,
      #                      P$intermediary_names,
      #                      P$intermediaries,
      #                      P$intermediary_names,
      #                      P$intermediaries, P$intermediary_names,
      #
      #                      P$intermediary_names, P$initial_value_names,
      #                      P$intermediary_names, P$initial_value_names,
      #
      #                      P$sim_df_name, P$sim_df_name,
      #                      P$timestep_name, P$saveat_name, P$times_name, P$saveat_name, P$times_name, P$sim_df_name, P$sim_df_name,P$sim_df_name, P$sim_df_name,
      #
      #                      P$sim_df_name, P$times_name, P$timestep_name, P$times_name,
      #                      P$times_name, P$times_name, P$times_name, P$timestep_name, P$times_name,
      #                      P$sim_df_name, P$sim_df_name, P$sim_df_name, P$sim_df_name, P$sim_df_name ),

      "clean_df" = "function clean_df(solve_out, init_names, intermediaries=nothing, intermediary_names=nothing)
    \"\"\"
Convert a single (non-ensemble) solution to a DataFrame, including intermediaries.

Args:
  solve_out: Single solution object from DifferentialEquations.jl
init_names: Names of the initial conditions/state variables
intermediaries: Optional intermediary values from saving callback
intermediary_names: Optional names for intermediary variables

Returns:
  timeseries_df: DataFrame with columns [time, variable, value]
\"\"\"

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

    return timeseries_df
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
", P[["parameter_name"]], P[["parameter_name"]], P[["parameter_name"]], P[["parameter_name"]], P[["parameter_name"]], P[["parameter_name"]]),
      "clean_init" = sprintf("function clean_init(%s, %s)
    Dict(%s .=> Unitful.ustrip.(%s))
end", P[["initial_value_name"]], P[["initial_value_names"]], P[["initial_value_names"]], P[["initial_value_name"]])),

    "ensemble" = list(


      #   "all_timestep_stats" = "function all_timestep_stats(intermediaries, key = :saveval, qs = [0.05, 0.95])
      #     n_steps = length(getfield(first(intermediaries), key))  # number of time steps
      #     n_vars = length(first(getfield(first(intermediaries), key)))  # number of variables
      #
      #     # Preallocate storage
      #     means   = zeros(n_vars, n_steps)
      #     vars_   = zeros(n_vars, n_steps)
      #     medians = zeros(n_vars, n_steps)
      #     qlows   = zeros(n_vars, n_steps)
      #     qhighs  = zeros(n_vars, n_steps)
      #
      #     for i in 1:n_steps
      #         vals = [collect(getfield(sv, key)[i]) for sv in intermediaries]
      #         mat = reduce(hcat, vals)  # rows: variables, cols: trajectories
      #
      #         means[:, i]   .= mapslices(mean, mat; dims=2)[:]
      #         vars_[:, i]   .= mapslices(var, mat; dims=2)[:]
      #         medians[:, i] .= mapslices(Statistics.median, mat; dims=2)[:]
      #         qlows[:, i]   .= mapslices(x -> Statistics.quantile(x, qs[1]), mat; dims=2)[:]
      #         qhighs[:, i]  .= mapslices(x -> Statistics.quantile(x, qs[2]), mat; dims=2)[:]
      #     end
      #
      #     return (
      #         mean = means,
      #         var = vars_,
      #         median = medians,
      #         qlow = qlows,
      #         qhigh = qhighs
      #     )
      # end",
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
      #
      # "summary_to_long" = "function summary_to_long(stats, times, var_names)
      #     dfs = DataFrame[]
      #     # for (statname, matrix) in stats
      #     for statname in keys(stats)
      #
      #         df = permutedims(DataFrame(getfield(stats, statname), :auto))
      #         rename!(df, var_names)
      #
      #         if (statname == first(keys(stats)))
      #             df.time = times
      #         end
      #
      #         long = stack(df, var_names; variable_name=:variable, value_name=statname)
      #
      #         # Remove the variable column
      #         if (statname != first(keys(stats)))
      #             select!(long, Not(:variable))
      #         end
      #
      #         # long.statistic .= statname
      #         push!(dfs, long)
      #     end
      #
      #     hcat(dfs...)
      # end",
      #
      #   "create_ensemble_summ" = "function create_ensemble_summ(solve_out, init_names, intermediaries, intermediary_names, qs = [0.05, 0.95])
      #
      #     stats = all_timestep_stats(solve_out, :u, qs);
      #     summ = summary_to_long(stats, solve_out[1].t, init_names)
      #
      #     if !isnothing(intermediaries)
      #         stats = all_timestep_stats(intermediaries, :saveval, qs);
      #         summ = vcat(summ, summary_to_long(stats, intermediaries[1].t, intermediary_names))
      #     end
      #
      #     return summ
      # end
      # ",

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

      # "ensemble_to_df" = "function ensemble_to_df(solve_out, init_names, times, dt, saveat;
      #                    intermediaries=nothing, intermediary_names=nothing)
      #     n = length(solve_out)
      #     dfs = Vector{DataFrame}(undef, n)
      #
      #     for i in 1:n
      #         df = clean_df(
      #             solve_out[i], init_names, times, dt, saveat;
      #             intermediaries = isnothing(intermediaries) ? nothing : intermediaries[i],
      #             intermediary_names = intermediary_names
      #         )
      #         df.simulation = fill(i, nrow(df))
      #         dfs[i] = df
      #     end
      #
      #     return reduce(vcat, dfs)
      # end",

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


      "ensemble_to_df" = "function ensemble_to_df(solve_out, init_names,
    intermediaries, intermediary_names, ensemble_n)
    \"\"\"
Unified processing where intermediaries are transformed to solve_out format first.
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

    # Extract parameter matrix (same as before)
    param_names = String[]
    first_params = solve_out[1].p
    if isa(first_params, NamedTuple)
        for (key, val) in pairs(first_params)
            if !isa(val, Function)
                push!(param_names, string(key))
            end
        end
    elseif isa(first_params, AbstractVector)
        for i in eachindex(first_params)
            if !isa(first_params[i], Function)
                push!(param_names, \"p$i\")
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

    return timeseries_df, param_matrix, param_names
end
",

      "generate_param_combinations" = "
\"\"\"
generate_param_combinations(param_ranges; crossed=true, n_replicates=100,
                            param_names=nothing, random_seed=nothing)

Generate parameter combinations for ensemble simulations.

# Arguments
- `param_ranges`: Dict or NamedTuple of parameter names to ranges/vectors
- `crossed`: Boolean, whether to cross all parameter combinations (default: true)
- `n_replicates`: Number of replicates per condition (default: 100)
- `param_names`: Optional vector of parameter names if using positional parameters
- `random_seed`: Optional seed for reproducibility

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
                                   crossed=true, n_replicates=100,
                                   param_names=nothing, random_seed=nothing)

    # Set random seed if provided
    if random_seed !== nothing
        Random.seed!(random_seed)
    end

    # Handle different input formats
    if isa(param_ranges, Dict) || isa(param_ranges, NamedTuple)
        param_dict = Dict(param_ranges)
        names_list = collect(keys(param_dict))
        values_list = collect(values(param_dict))
    else
        # Assume it's a vector of ranges
        if param_names === nothing
            param_names = [Symbol(\"param$i\") for i in 1:length(param_ranges)]
        end
        param_dict = Dict(zip(param_names, param_ranges))
        names_list = param_names
        values_list = param_ranges
    end

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

"ensemble_to_df_threaded" = "function ensemble_to_df_threaded(solve_out, init_names, intermediaries, intermediary_names, ensemble_n)
    \"\"\"
Unified processing where intermediaries are transformed to solve_out format first.
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

    # Extract parameter matrix (same as before)
    param_names = String[]
    first_params = solve_out[1].p
    if isa(first_params, NamedTuple)
        for (key, val) in pairs(first_params)
            if !isa(val, Function)
                push!(param_names, string(key))
            end
        end
    elseif isa(first_params, AbstractVector)
        for i in eachindex(first_params)
            if !isa(first_params[i], Function)
                push!(param_names, \"p$i\")
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

    return timeseries_df, param_matrix, param_names
end
",

"ensemble_summ" = "function ensemble_summ(timeseries_df, quantiles=[0.025, 0.0975])
    # Group by time and variable, then compute statistics
    stats_df = combine(groupby(timeseries_df, [:j, :time, :variable])) do group
        values = group.value

        # Base statistics
        result = (
            mean = mean(values),
            variance = var(values),
            median = Statistics.median(values)
        )

        # Add quantiles
        for q in quantiles
            q_str = replace(string(q), r\"^0\\.\" => \"\")
            result = merge(result, (Symbol(\"q$q_str\") => Statistics.quantile(values, q),))
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

        # Extract group keys
        j_vals[i] = key.j
        time_vals[i] = key.time
        variable_vals[i] = key.variable

        # Calculate statistics
        mean_vals[i] = mean(values)
        variance_vals[i] = var(values)
        median_vals[i] = Statistics.median(values)

        # Calculate quantiles
        for q in quantiles
            q_str = replace(string(q), r\"^0\\.\" => \"\")
            quantile_arrays[\"q$q_str\"][i] = Statistics.quantile(values, q)
        end
    end

    # Create result DataFrame with desired column order
    # Start with the main columns in order
    stats_df = DataFrame(
        j = j_vals,
        time = time_vals,
        variable = variable_vals,
        mean = mean_vals,
        median = median_vals,
        variance = variance_vals
    )

    # Add quantile columns in order
    for q in quantiles
        q_str = replace(string(q), r\"^0\\.\" => \"\")
        stats_df[!, Symbol(\"q$q_str\")] = quantile_arrays[\"q$q_str\"]
    end

    return stats_df
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

