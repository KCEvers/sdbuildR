# setup.jl - One-time setup script for sdbuildR Julia environment

println("Setting up Julia environment for sdbuildR...\n\n")

using Pkg

# Use the environment path provided by R (install_julia_env), falling back to
# the directory of this script if setup.jl is run directly.
env_path = isdefined(Main, :sdbuildR_env_path) ? sdbuildR_env_path : @__DIR__

# println("Activating environment at: ", env_path, "\n")
Pkg.activate(env_path)

# Install SystemDynamicsBuildR from GitHub
println("\nInstalling SystemDynamicsBuildR.jl from GitHub...")
Pkg.add(url="https://github.com/kcevers/SystemDynamicsBuildR.jl", rev = "v0.4.0")

# Install all other dependencies from Project.toml
println("\nInstalling dependencies from Project.toml...")
Pkg.instantiate()

## Resolve dependencies without installing
# Pkg.resolve()

# Precompile packages for faster loading
println("\nPrecompiling packages...")
Pkg.precompile()

println("\nSetup complete!")

