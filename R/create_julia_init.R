
#' Internal function to create initialization file for julia
#'
#' @return NULL
#' @noRd
#'
create_julia_init = function(){

  # Note for extending comparison operators:
  # eltype(5u"m") <: Unitful.Quantity # true
  # eltype(5u"m") <: Number # true
  # eltype(5u"m") <: Float64 # false
  # eltype(5.0) <: Real # true
  # eltype(5) <: Unitful.Quantity # false
  # so we cannot use x::Number, but have to use x::Real

  # Add standard custom units
  unit_str = lapply(custom_units(),
                    function(x){

                      if (is_defined(x$eqn)){
                        unit_def = x$eqn
                      } else {
                        unit_def = "1"
                      }

                      paste0("@unit ", x$name, " \"", x$name, "\" ", x$name, " u\"", unit_def, "\" ", ifelse(x$prefix, "true", "false"))

                    }) %>% paste0(collapse = sprintf("\n\tUnitful.register(%s)\n\t", P$sdbuildR_units))

  unit_str = paste0("\n\n# Define custom units; register after each unit as some units may be defined by other units\nmodule ", P$sdbuildR_units, "\n\tusing Unitful\n\t",
                    unit_str,
                    "\n\tUnitful.register(", P$sdbuildR_units, ")\nend\n\n", P$unit_context, " = [Unitful.Unitful, ", P$sdbuildR_units, "];\n\n")

  script = paste0("# Load packages
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

Base.floor(x::Unitful.Quantity) = floor(Unitful.ustrip.(x)) * Unitful.unit(x)
Base.ceil(x::Unitful.Quantity) = ceil(Unitful.ustrip.(x)) * Unitful.unit(x)
Base.trunc(x::Unitful.Quantity) = trunc(Unitful.ustrip.(x)) * Unitful.unit(x)

#round_(x::Unitful.Quantity) = round(Unitful.ustrip.(x)) * Unitful.unit(x)
#round_(x::Unitful.Quantity, digits::Int) = round(Unitful.ustrip.(x), digits=digits) * Unitful.unit(x)
#round_(x::Unitful.Quantity; digits::Int) = round(Unitful.ustrip.(x), digits=digits) * Unitful.unit(x)
#round_(x::Unitful.Quantity, digits::Float64) = round(Unitful.ustrip.(x), digits=round(digits)) * Unitful.unit(x)
#round_(x::Unitful.Quantity; digits::Float64) = round(Unitful.ustrip.(x), digits=round(digits)) * Unitful.unit(x)

round_(x) = round(x)

round_(x, digits::Real) = round(x, digits=round(Int, digits))

round_(x; digits::Real=0) = round(x, digits=round(Int, digits))

round_(x::Unitful.Quantity) = round(Unitful.ustrip(x)) * Unitful.unit(x)

round_(x::Unitful.Quantity, digits::Real) = round(Unitful.ustrip(x), digits=round(Int, digits)) * Unitful.unit(x)

round_(x::Unitful.Quantity; digits::Real=0) = round(Unitful.ustrip(x), digits=round(Int, digits)) * Unitful.unit(x)

",
                  paste0(get_func_julia() %>% unname(), collapse = "\n\n"),

                  # Add custom functions
                  unit_str,
                  # Add initialization of sdbuildR
                  paste0("\n\n", P$init_sdbuildR, " = true"),
                  collapse = "\n\n")

  # Write script
  env_path <- system.file(package = "sdbuildR")
  filepath = file.path(env_path, "init.jl")
  write_script(script, filepath)
  invisible()
}
