# setup.jl - One-time setup script for sdbuildR Julia environment

println("Setting up Julia environment for sdbuildR...")

using Pkg

# Get the current script directory (where setup.jl is located)
# This should be the sdbuildR package installation directory
env_path = @__DIR__

println("Activating environment at: ", env_path)
Pkg.activate(env_path)

# Install SystemDynamicsBuildR from GitHub
println("\nInstalling SystemDynamicsBuildR.jl from GitHub...")
Pkg.add(url="https://github.com/KCEvers/SystemDynamicsBuildR.jl")

# Install all other dependencies from Project.toml
println("\nInstalling dependencies from Project.toml...")
Pkg.instantiate()

# Resolve dependencies without installing
Pkg.resolve()

# Precompile packages for faster loading
println("\nPrecompiling packages...")
Pkg.precompile()

println("\nSetup complete!")
