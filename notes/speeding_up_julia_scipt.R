

#without parameters
script = "function lorenz!(du, u, p, t)
    du[1] = 10.0 * (u[2] - u[1])
    du[2] = u[1] * (28.0 - u[3]) - u[2]
    du[3] = u[1] * u[2] - (8 / 3) * u[3]
end


u0 = [1.0; 0.0; 0.0]
tspan = (0.0, 1000.0)
prob = ODEProblem(lorenz!, u0, tspan)
sol = solve(prob)"

filepath = write_script(script, ext = ".jl")

JuliaCall::julia_source(filepath)

microbenchmark::microbenchmark(JuliaCall::julia_source(filepath))

# with parameters
script = "
function parameterized_lorenz!(du, u, p, t)
    x, y, z = u
    σ, ρ, β = p
    du[1] = dx = σ * (y - x)
    du[2] = dy = x * (ρ - z) - y
    du[3] = dz = x * y - β * z
end

u0 = [1.0, 0.0, 0.0]
tspan = (0.0, 1000.0)
p = [10.0, 28.0, 8 / 3]
prob = ODEProblem(parameterized_lorenz!, u0, tspan, p)
df = solve(prob)

"

filepath = write_script(script, ext = ".jl")

JuliaCall::julia_source(filepath)

microbenchmark::microbenchmark(JuliaCall::julia_source(filepath))

# with euler
script = "
function parameterized_lorenz!(du, u, p, t)
    x, y, z = u
    σ, ρ, β = p
    du[1] = dx = σ * (y - x)
    du[2] = dy = x * (ρ - z) - y
    du[3] = dz = x * y - β * z
end

u0 = [1.0, 0.0, 0.0]
dt = 0.01
tspan = (0.0, 1000.0)
p = [10.0, 28.0, 8 / 3]
prob = ODEProblem(parameterized_lorenz!, u0, tspan, p)
df = solve(prob, Euler(), dt = dt, adaptive=false)
"


filepath = write_script(script, ext = ".jl")

JuliaCall::julia_source(filepath)

microbenchmark::microbenchmark(JuliaCall::julia_source(filepath))




# with rk4
script = "
function parameterized_lorenz!(du, u, p, t)
    x, y, z = u
    σ, ρ, β = p
    du[1] = dx = σ * (y - x)
    du[2] = dy = x * (ρ - z) - y
    du[3] = dz = x * y - β * z
end

u0 = [1.0, 0.0, 0.0]
dt = 0.01
tspan = (0.0, 1000.0)
p = [10.0, 28.0, 8 / 3]
prob = ODEProblem(parameterized_lorenz!, u0, tspan, p)
df = solve(prob, RK4(), dt = dt, adaptive=false)
"


filepath = write_script(script, ext = ".jl")

JuliaCall::julia_source(filepath)

microbenchmark::microbenchmark(JuliaCall::julia_source(filepath))



# with callback
script = "
dt = 0.01
tspan = (0.0, 1000.0)

function parameterized_lorenz!(du, u, p, t)
    x, y, z = u
    σ, ρ, β = p
    du[1] = dx = σ * (y - x)
    du[2] = dy = x * (ρ - z) - y
    du[3] = dz = x * y - β * z
end


function save_intermediaries(u, t, integrator)
    x, y, z = u
    σ, ρ, β = integrator.p
    du1 = dx = σ * (y - x)
    du2 = dy = x * (ρ - z) - y
    du3 = dz = x * y - β * z

    return(du1, du2, du3)
end

# intermediaries = SavedValues(Float64, Any)
intermediaries = SavedValues(Float64, Tuple{Float64, Float64, Float64})
callback = SavingCallback(save_intermediaries, intermediaries)# saveat is worse,saveat =dt)


u0 = [1.0, 0.0, 0.0]
p = [10.0, 28.0, 8 / 3]
prob = ODEProblem(parameterized_lorenz!, u0, tspan, p)
df = solve(prob, RK4(), dt = dt, adaptive=false, callback = callback)
"


filepath = write_script(script, ext = ".jl")

JuliaCall::julia_source(filepath)

microbenchmark::microbenchmark(JuliaCall::julia_source(filepath))



# save dataframe with callback
script = "
dt = 0.01
tspan = (0.0, 1000.0)

function parameterized_lorenz!(du, u, p, t)
    x, y, z = u
    σ, ρ, β = p
    du[1] = dx = σ * (y - x)
    du[2] = dy = x * (ρ - z) - y
    du[3] = dz = x * y - β * z
end


function save_intermediaries(u, t, integrator)
    x, y, z = u
    σ, ρ, β = integrator.p
    du1 = dx = σ * (y - x)
    du2 = dy = x * (ρ - z) - y
    du3 = dz = x * y - β * z

    return(du1, du2, du3)
end

# intermediaries = SavedValues(Float64, Any)
intermediaries_names = ['du1', 'du2', 'du3']
intermediaries = SavedValues(Float64, Tuple{Float64, Float64, Float64})
callback = SavingCallback(save_intermediaries, intermediaries)# saveat is worse,saveat =dt)


u0 = [1.0, 0.0, 0.0]
p = [10.0, 28.0, 8 / 3]
prob = ODEProblem(parameterized_lorenz!, u0, tspan, p)
sol = solve(prob, RK4(), dt = dt, adaptive=false, callback = callback)

# Convert to matrix (rows: time points, columns: variables); # Include time as first column

#matrix = hcat(sol.t, hcat(sol.u...)') # slower, surprisingly

xstart_names = ['x', 'y', 'z']
df = DataFrame([
	:time => sol.t,
	[Symbol(name) => sol[i, :] for (i, name) in enumerate(xstart_names)]...
])

df2 = DataFrame([
	:time => intermediaries.t,
	[Symbol(name) => intermediaries.saveval[i, :] for (i, name) in enumerate(intermediaries_names)]...
])
"


filepath = write_script(script, ext = ".jl")

JuliaCall::julia_source(filepath)

microbenchmark::microbenchmark(JuliaCall::julia_source(filepath))


# save dataframe but just dataframe() with callback
script = "
dt = 0.01
tspan = (0.0, 1000.0)

function parameterized_lorenz!(du, u, p, t)
    x, y, z = u
    σ, ρ, β = p
    du[1] = dx = σ * (y - x)
    du[2] = dy = x * (ρ - z) - y
    du[3] = dz = x * y - β * z
end


function save_intermediaries(u, t, integrator)
    x, y, z = u
    σ, ρ, β = integrator.p
    du1 = dx = σ * (y - x)
    du2 = dy = x * (ρ - z) - y
    du3 = dz = x * y - β * z

    return(du1, du2, du3)
end

# intermediaries = SavedValues(Float64, Any)
intermediaries_names = [:du1, :du2, :du3]
intermediaries = SavedValues(Float64, Tuple{Float64, Float64, Float64})
callback = SavingCallback(save_intermediaries, intermediaries)# saveat is worse,saveat =dt)


u0 = [1.0, 0.0, 0.0]
p = [10.0, 28.0, 8 / 3]
prob = ODEProblem(parameterized_lorenz!, u0, tspan, p)
sol = solve(prob, RK4(), dt = dt, adaptive=false, callback = callback)

# Convert to matrix (rows: time points, columns: variables); # Include time as first column

#matrix = hcat(sol.t, hcat(sol.u...)') # slower, surprisingly

xstart_names = [:x, :y, :z]
df = Unitful.ustrip.(DataFrame(sol, [:time; xstart_names])) # stripping units adds ~  20 milliseconds

df2 = Unitful.ustrip.(DataFrame(intermediaries.saveval, intermediaries_names))
df = hcat(df, df2) # Does not add much time


"


filepath = write_script(script, ext = ".jl")

JuliaCall::julia_source(filepath)

microbenchmark::microbenchmark(JuliaCall::julia_source(filepath))

