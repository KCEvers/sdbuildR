# Load packages
#using DifferentialEquations#: ODEProblem, solve, Euler, RK4, Tsit5
using OrdinaryDiffEq
using DiffEqCallbacks#: SavingCallback, SavedValues
using DataFrames#: DataFrame, select, innerjoin, rename!
using Distributions
using Statistics
using Unitful
using DataInterpolations
using Random
using CSV

# julia initialization for sdbuildR package
# Required when extending a module’s function
#import Base: <, >, <=, >=, ==, != #, +, - #, *, /, ^

# Extend base methods (multiple dispatch) to allow for comparison between a unit and a non-unit; if one of the arguments is a Unitful.Quantity, convert the other to the same unit.
Base.:<(x::Unitful.Quantity, y::Float64) = <(x, y * Unitful.unit(x))
Base.:<(x::Float64, y::Unitful.Quantity) = <(x * Unitful.unit(y), y)

Base.:>(x::Unitful.Quantity, y::Float64) = >(x, y * Unitful.unit(x))
Base.:>(x::Float64, y::Unitful.Quantity) = >(x * Unitful.unit(y), y)

Base.:(<=)(x::Unitful.Quantity, y::Float64) = <=(x, y * Unitful.unit(x))
Base.:(<=)(x::Float64, y::Unitful.Quantity) = <=(x * Unitful.unit(y), y)

Base.:(>=)(x::Unitful.Quantity, y::Float64) = >=(x, y * Unitful.unit(x))
Base.:(>=)(x::Float64, y::Unitful.Quantity) = >=(x * Unitful.unit(y), y)

Base.:(==)(x::Unitful.Quantity, y::Float64) = ==(x, y * Unitful.unit(x))
Base.:(==)(x::Float64, y::Unitful.Quantity) = ==(x * Unitful.unit(y), y)

Base.:(!=)(x::Unitful.Quantity, y::Float64) = !=(x, y * Unitful.unit(x))
Base.:(!=)(x::Float64, y::Unitful.Quantity) = !=(x * Unitful.unit(y), y)

Base.:%(x::Unitful.Quantity, y::Float64) = %(x, y * Unitful.unit(x))
Base.:%(x::Float64, y::Unitful.Quantity) = %(x * Unitful.unit(y), y)

Base.mod(x::Unitful.Quantity, y::Float64) = mod(x, y * Unitful.unit(x))
Base.mod(x::Float64, y::Unitful.Quantity) = mod(x * Unitful.unit(y), y)

Base.rem(x::Unitful.Quantity, y::Float64) = rem(x, y * Unitful.unit(x))
Base.rem(x::Float64, y::Unitful.Quantity) = rem(x * Unitful.unit(y), y)

Base.min(x::Unitful.Quantity, y::Float64) = min(x, y * Unitful.unit(x))
Base.min(x::Float64, y::Unitful.Quantity) = min(x * Unitful.unit(y), y)

Base.max(x::Unitful.Quantity, y::Float64) = max(x, y * Unitful.unit(x))
Base.max(x::Float64, y::Unitful.Quantity) = max(x * Unitful.unit(y), y)

# Extend min/max: when applied to a single vector, use minimum, like in R
Base.min(v::AbstractVector) = minimum(v)
Base.max(v::AbstractVector) = maximum(v)

Base.round(x::Unitful.Quantity) = round(Unitful.ustrip.(x)) * Unitful.unit(x)
Base.floor(x::Unitful.Quantity) = floor(Unitful.ustrip.(x)) * Unitful.unit(x)
Base.ceil(x::Unitful.Quantity) = ceil(Unitful.ustrip.(x)) * Unitful.unit(x)
Base.trunc(x::Unitful.Quantity) = trunc(Unitful.ustrip.(x)) * Unitful.unit(x)
Base.round(x::Unitful.Quantity, digits::Int) = round(Unitful.ustrip.(x), digits=digits) * Unitful.unit(x)
Base.round(x::Unitful.Quantity, digits::Float64) = round(Unitful.ustrip.(x), digits=round(digits)) * Unitful.unit(x)

# Extrapolation function
function itp(x, y; method = "linear", extrapolation = "nearest")

  # Ensure y is sorted along x
  idx = sortperm(x)
  x = x[idx]
  y = y[idx]

  # Extrapolation rule: What happens outside of defined values?
  # Rule "NA": return NaN; Rule "nearest": return closest value
  rule_method = ifelse(extrapolation == "NA", DataInterpolations.ExtrapolationType.None, ifelse(extrapolation == "nearest", DataInterpolations.ExtrapolationType.Constant, extrapolation))

  if method == "constant"
      func = DataInterpolations.ConstantInterpolation(y, x; extrapolation = rule_method) # notice order of x and y
  elseif method == "linear"
      func = DataInterpolations.LinearInterpolation(y, x; extrapolation = rule_method)
  end

  return(func)
end

function ramp(; start_t_ramp, end_t_ramp, start_h_ramp = 0.0, end_h_ramp = 1.0)
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

    func = itp(x, y, method = "linear", extrapolation = "nearest")

    return(func)
end 

# Make step signal
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

    func = itp(x, y, method = "constant", extrapolation = "nearest")

    return(func)
end

# Constraints macro
macro constraints(exprs...)
    pairs = map(exprs) do ex
        quote
            ($(string(ex)), $(esc(ex)))
        end
    end
    quote
        [$(pairs...)]
    end
end

# Make pulse signal
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
    func = itp(x, y, method = "constant", extrapolation = "nearest")

    return(func)
end

# Convert Insight Maker's Round() function to R
# Difference: in Insight Maker, Round(.5) = 1; in R, round(.5) = 0; in julia, round(.5) = 0.0
function IM_round(x::Real, digits::Int=0)
    # Compute the fractional part after scaling by 10^digits
    scaled_x = x * 10.0^digits
    frac = scaled_x % 1
    # Check if fractional part is exactly 0.5 or -0.5
    if abs(frac) == 0.5
        return ceil(scaled_x) / 10.0^digits
    else
        return round(scaled_x, digits=0) / 10.0^digits
    end
end

# Logit function
function logit(p)
    return log(p / (1 - p))
end

# Expit function
function expit(x)
    return 1 / (1+exp(-x))
end

function sigmoid(x; slope=1, midpoint=0)
    @assert isfinite(slope) && isfinite(midpoint) "slope and midpoint must be numeric"
    1 / (1 + exp(-slope * (x - midpoint)))
end

# Also define without keyword arguments
function sigmoid(x, slope, midpoint)
    @assert isfinite(slope) && isfinite(midpoint) "slope and midpoint must be numeric"
    1 / (1 + exp(-slope * (x - midpoint)))
end


# Prevent non-negativity (below zero)
# Scalar case: non-unitful types
nonnegative(x::Real) = max(0.0, x)

# Scalar case: Unitful.Quantity
nonnegative(x::Unitful.Quantity) = max(0.0, Unitful.ustrip(x)) * Unitful.unit(x)

# Array case: non-unitful elements
nonnegative(x::AbstractArray{<:Real}) = max.(0.0, x)

# Array case: Unitful.Quantity elements
nonnegative(x::AbstractArray{<:Unitful.Quantity}) = max.(0.0, Unitful.ustrip.(x)) .* Unitful.unit.(x)

# Generate random boolean value, equivalent of RandBoolean() in Insight Maker
function rbool(p)
    return rand() < p
end

function rdist(a::Vector{T}, b::Vector{<:Real}) where T
    # Check lengths match
    if length(a) != length(b)
        throw(ArgumentError("Length of a and b must match"))
    end
    # Normalize probabilities
    b_sum = sum(b)
    if b_sum <= 0
        throw(ArgumentError("Sum of probabilities must be positive"))
    end
    b_normalized = b / b_sum
    # Sample using Categorical
    return a[rand(Distributions.Categorical(b_normalized))]
end

function indexof(haystack, needle)
    if isa(haystack, AbstractString) && isa(needle, AbstractString)
        pos = findfirst(needle, haystack)
        return isnothing(pos) ? 0 : first(pos)
    else
        pos = findfirst(==(needle), haystack)
        return isnothing(pos) ? 0 : pos
    end
end

function IM_contains(haystack, needle)
    if isa(haystack, AbstractString) && isa(needle, AbstractString)
        return occursin(needle, haystack)
    else
        return needle in haystack
    end
end

function substr_i(string::AbstractString, idxs::Union{Int, Vector{Int}})
    chars = collect(string)
    return join(chars[idxs])
end

function IM_filter(y::Vector{T}, condition_func::Function) where T
    names_y = string.(1:length(y))
    result = Dict{String,T}()
    for (key, val) in zip(names_y, y)
        if condition_func(val, key)
            result[key] = val
        end
    end
    return collect(values(result)), collect(keys(result))
end

# Set or convert unit wrappers per type
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
        unit_def = Unitful.uparse(unit_def, unit_context = unit_context)  # Parse string to unit (e.g., "wk" -> u"wk")

        if Unitful.unit(x) == unit_def
            return x  # No conversion needed
        else
            Unitful.uconvert.(unit_def, x)
        end
    catch e
        error("Invalid unit string: $unit_def")
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
        unit_def = Unitful.uparse(unit_def, unit_context = unit_context)  # Parse string to unit (e.g., "wk" -> u"wk")
        x * unit_def
    catch e
        error("Invalid unit string: $unit_def")
    end
end

# Create seasonal wave
function seasonal(t; period = u"1yr", shift = u"0yr")
    phase = 2 * pi * (t - shift) / period  # π radians
    return(cos(phase))
end

# Define the operator ⊕ for the modulus
function ⊕(x, y)
    return mod(x, y)
end

# Function to retrieve past values

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
    #     error("Variable '$var_name' not found in intermediary_names: $intermediary_names")
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
    if single_or_interval == "single"
        extract_t = t - delay_time
        if extract_t < ts[1]
            return isnothing(default_value) ? ys[1] : default_value
        elseif extract_t == t
            return var_value
        end

        # Interpolate
        # interp = LinearInterpolation(ys, ts, extrapolation_bc = Flat())
        # Define interpolation function
        return itp(ts, ys, method = "linear")(extract_t)

    end

    # Interval extraction
    if single_or_interval == "interval"
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


# Function to save dataframe at specific times
function saveat_func(t, y, new_times)
    # Interpolate y at new_times
    itp(t, y, method = "linear", extrapolation = "nearest")(new_times)
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
    outflow = exit_rate_stage[order_delay]
    return (outflow=outflow, update=d_accumulator)
end

function setup_delayN(initial_value, length_delay, order_delay::Float64, name::Symbol)
    # Compute the initial value for each accumulator
    # from https://www.simulistics.com/help/equations/functions/delay.html
    order_delay = round(Int, order_delay) # Turn order into integer
    value = initial_value * length_delay / order_delay

    # Create a dictionary with names like "name_acc1", "name_acc2", ...
    #return Dict(string(name, "_acc", i) => value for i in 1:order_delay)
    return Dict(Symbol(name, "_acc", i) => value for i in 1:order_delay)
end

# Define custom units; register after each unit as some units may be defined by other units
module sdbuildR_units
	using Unitful
	@unit common_yr "common_yr" common_yr u"365d" false
	Unitful.register(sdbuildR_units)
	@unit common_quarter "common_quarter" common_quarter u"365/4*d" false
	Unitful.register(sdbuildR_units)
	@unit common_month "common_month" common_month u"365/12*d" false
	Unitful.register(sdbuildR_units)
	@unit quarter "quarter" quarter u"1/4*yr" false
	Unitful.register(sdbuildR_units)
	@unit month "month" month u"1/12*yr" false
	Unitful.register(sdbuildR_units)
	@unit quart "quart" quart u"946.35cm^3" false
	Unitful.register(sdbuildR_units)
	@unit tonne "tonne" tonne u"1000kg" false
	Unitful.register(sdbuildR_units)
	@unit ton "ton" ton u"907.18474kg" false
	Unitful.register(sdbuildR_units)
	@unit atom "atom" atom u"1/6.02214076e23mol" false
	Unitful.register(sdbuildR_units)
	@unit molecule "molecule" molecule u"1/6.02214076e23mol" false
	Unitful.register(sdbuildR_units)
	@unit US_gal "US_gal" US_gal u"0.003785411784m^3" false
	Unitful.register(sdbuildR_units)
	@unit fluidOunce "fluidOunce" fluidOunce u"29.5735295625mL" false
	Unitful.register(sdbuildR_units)
	@unit EUR "EUR" EUR u"1" false
	Unitful.register(sdbuildR_units)
	@unit USD "USD" USD u"1" false
	Unitful.register(sdbuildR_units)
	@unit GBP "GBP" GBP u"1" false
	Unitful.register(sdbuildR_units)
	@unit deg "deg" deg u"pi/180" false
	Unitful.register(sdbuildR_units)
	@unit Ohm "Ohm" Ohm u"1V/A" false
	Unitful.register(sdbuildR_units)
	@unit reduced_Planck_constant "reduced_Planck_constant" reduced_Planck_constant u"h/2pi" false
	Unitful.register(sdbuildR_units)
	@unit superconducting_magnetic_flux_quantum "superconducting_magnetic_flux_quantum" superconducting_magnetic_flux_quantum u"h/(2q)" false
	Unitful.register(sdbuildR_units)
	@unit degF "degF" degF u"(45967//100)Ra" false
	Unitful.register(sdbuildR_units)
	@unit degC "degC" degC u"(27315//100)K" false
	Unitful.register(sdbuildR_units)
	@unit Stefan_Boltzmann_constant "Stefan_Boltzmann_constant" Stefan_Boltzmann_constant u"pi^2*k^4/(60*reduced_Planck_constant^3*c^2)" false
	Unitful.register(sdbuildR_units)
	@unit AngHertz "AngHertz" AngHertz u"2pi/s" false
	Unitful.register(sdbuildR_units)
	@unit magnetic_constant "magnetic_constant" magnetic_constant u"4pi*(1//10)^7*H/m" false
	Unitful.register(sdbuildR_units)
	@unit electric_constant "electric_constant" electric_constant u"1/(μ0*c^2)" false
	Unitful.register(sdbuildR_units)
	@unit Bohr_magneton "Bohr_magneton" Bohr_magneton u"q*reduced_Planck_constant/(2*me)" false
	Unitful.register(sdbuildR_units)
	@unit Rydberg_constant "Rydberg_constant" Rydberg_constant u"10_973_731.568_160/m" false
	Unitful.register(sdbuildR_units)
end

unit_context = [Unitful.Unitful, sdbuildR_units];



initialization_sdbuildR = true
