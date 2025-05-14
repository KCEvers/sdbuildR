# Script generated on 2025-04-10 13:44:07.999729 by sdbuildR. Please cite ***

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


# Simulation time unit (smallest time scale in your model)
time_units = u"hr"
# Define time sequence
times = (0.0, 100.0)
# Time step
dt = 0.01




# Define ODE
function ode_func!(dSdt, S, pars, t)

	# Unpack state variables
	leisure_time, relationship_quality, = S

	# Update Auxiliaries and Flows
	
	# Flow to leisure_time
	time_progressing = 3.0
	
	# Flow from leisure_time to relationship_quality
	time_with_friends = .5 .* leisure_time

	# Collect inflows and outflows for each Stock
	dleisure_time = time_progressing - time_with_friends
	drelationship_quality = time_with_friends

	# State update
	dSdt[[1, 2]] .= dleisure_time, drelationship_quality

end


# Define callback function
function save_intermediaries(S, t, integrator)

	# Unpack state variables
	leisure_time, relationship_quality, = S

	# Update Auxiliaries and Flows
	
	# Flow to leisure_time
	time_progressing = 3.0
	
	# Flow from leisure_time to relationship_quality
	time_with_friends = .5 .* leisure_time

	# Return intermediary values
	return(time_progressing, time_with_friends)


end

# Callback setup
intermediaries = SavedValues(Float64, Any)
callback = SavingCallback(save_intermediaries, intermediaries, saveat = 0.1)


# Define parameters, initial conditions, and functions in correct order
leisure_time = 0.0
relationship_quality = 3.0

# Define empty parameters
pars = ()

# Define initial condition in vector
xstart = [Base.Iterators.flatten([leisure_time, relationship_quality,])...]
xstart_names = [:leisure_time, :relationship_quality]


# Run ODE
prob = ODEProblem(ode_func!, xstart, times, pars)
out = solve(prob, RK4(), dt = dt, adaptive=false, callback = callback, saveat = 0.1)
df = DataFrame(out)
rename!(df, [:timestamp] .=> [:time])  # Efficient renaming with .=>
rename!(df, [:value1, :value2] .=> xstart_names)  # Efficient renaming with .=>

# Create dataframe of intermediary variables
intermediary_names = ["time_progressing", "time_with_friends"]
intermediary_data = [getindex.(intermediaries.saveval, i) for i in 1:length(intermediary_names)]  # Extract each tuple element
intermediary_df = DataFrame(intermediary_data, Symbol.(intermediary_names))
intermediary_df[!, :time] = intermediaries.t  # Add time column

# Combine state and intermediary dataframes (assuming times align)
df = innerjoin(df, intermediary_df, on = :time)

# Overwrite initial values to named dictionary
xstart = Dict(xstart_names .=> xstart)

# Assign empty dictionary to units
units_dict = Dict()

