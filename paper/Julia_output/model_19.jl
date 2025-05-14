# Script generated on 2025-04-12 09:40:17.255051 by sdbuildR. Please cite ***

# Load packages
using DifferentialEquations
using DiffEqCallbacks
using DataFrames
using Distributions
using Statistics
using Unitful
using DataInterpolations
using Random
using StatsBase


# Define custom units; register after each unit as some units may be defined by other units
module MyUnits
	using Unitful
	@unit common_yr "common_yr" common_yr u"365d" false
	Unitful.register(MyUnits)
	@unit common_quarter "common_quarter" common_quarter u"365/4d" false
	Unitful.register(MyUnits)
	@unit common_month "common_month" common_month u"365/12d" false
	Unitful.register(MyUnits)
	@unit quarts "quarts" quarts u"0.000946m^3" false
	Unitful.register(MyUnits)
	@unit tonne "tonne" tonne u"1000000g" false
	Unitful.register(MyUnits)
	@unit ton "ton" ton u"907184.74g" false
	Unitful.register(MyUnits)
	@unit atom "atom" atom u"1/6.02214076e23mol" false
	Unitful.register(MyUnits)
	@unit molecule "molecule" molecule u"1/6.02214076e23mol" false
	Unitful.register(MyUnits)
	@unit fl_oz "fl_oz" fluidOunce u"0.00003m^3" false
	Unitful.register(MyUnits)
end

unit_context = [Unitful.Unitful, MyUnits];


# Macro to extend functions to preserve units
macro unitful_preserve_units(funcs...)
    expr = Expr(:block)
    for f in funcs
        push!(expr.args, quote
            # Extend the function for Unitful.Quantity
            function Base.$f(x::Unitful.Quantity)
                # Strip the unit, apply the function, and reattach the unit
                val = $f(Unitful.ustrip(x))
                return val * Unitful.unit(x)
            end

            # # Handle rounding with a type argument (e.g., round(Int, x))
            # function Base.$f(::Type{T}, x::Unitful.Quantity) where {T<:Number}
            #     val = $f(T, ustrip(x))
            #     return val * unit(x)
            # end
        end)
    end
    return esc(expr)
end

# Apply the Macro to Desired Function
# Extend round, floor, and ceil to preserve units
@unitful_preserve_units round floor ceil

# Simulation time unit (smallest time scale in your model)
time_units = u"d"
# Define time sequence
times = (0.0, 100.0)
# Initialize time
t = times[1];
# Time step
dt = 0.01




# Define ODE
function ode_func!(dSdt, S, pars, t)

	# Unpack state variables
	negative_thoughts, = S

	# Update Auxiliaries and Flows
	
	# Flow to negative_thoughts
	rumination = .1 .* negative_thoughts

	# Collect inflows and outflows for each Stock
	dnegative_thoughts = rumination

	# State update
	dSdt[collect(1)] .= dnegative_thoughts

end


# Define callback function
function save_intermediaries(S, t, integrator)

	# Unpack state variables
	negative_thoughts, = S

	# Update Auxiliaries and Flows
	
	# Flow to negative_thoughts
	rumination = .1 .* negative_thoughts

	# Return intermediary values
	return(rumination)


end

# Callback setup
intermediaries = SavedValues(Float64, Any)
callback = SavingCallback(save_intermediaries, intermediaries, saveat = 0.01)


# Define parameters, initial conditions, and functions in correct order
negative_thoughts = .5

# Define empty parameters
pars = ()

# Define initial condition in vector
xstart = [Base.Iterators.flatten([negative_thoughts,])...]
xstart_names = [:negative_thoughts]


# Run ODE
prob = ODEProblem(ode_func!, xstart, times, pars)
out = solve(prob, RK4(), dt = dt, adaptive=false, callback = callback, saveat = 0.01)
df = DataFrame(out)
rename!(df, [:timestamp] .=> [:time])  # Efficient renaming with .=>
rename!(df, [:value1] .=> xstart_names)  # Efficient renaming with .=>

# Create dataframe of intermediary variables
intermediary_names = ["rumination"]
intermediary_data = [getindex.(intermediaries.saveval, i) for i in 1:length(intermediary_names)]  # Extract each tuple element
intermediary_df = DataFrame(intermediary_data, Symbol.(intermediary_names))
intermediary_df[!, :time] = intermediaries.t  # Add time column

# Combine state and intermediary dataframes (assuming times align)
df = innerjoin(df, intermediary_df, on = :time)

# Overwrite initial values to named dictionary
xstart = Dict(xstart_names .=> xstart)

# Assign empty dictionary to units
units_dict = Dict()

