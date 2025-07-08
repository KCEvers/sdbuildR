

#' Set up Julia environment for sdbuildR
#'
#' Create Julia environment to simulate stock-and-flow models. `use_julia()` looks for a Julia installation, and will install Julia as well as some required packages if not found. Keep in mind that the installation can take around 5-15 minutes. In every R session, `use_julia()` needs to be run once, which can take around 20 seconds.
#'
#' @param stop If TRUE, stop active Julia session. Defaults to FALSE.
#' @param version Julia version. Default is "latest", which will install the most recent stable release.
#' @param JULIA_HOME Path to Julia installation. Defaults to NULL to locate Julia automatically.
#' @param JULIA_NUM_THREADS Number of Julia threads to use. Defaults to 4. If set to a value higher than the number of available cores minus 2, it will be set to the number of available cores minus 2.
#' @param dir Directory to install Julia. Defaults to NULL to use default location.
#' @param force If TRUE, force Julia setup to execute again.
#' @param force_install If TRUE, force Julia installation even if existing version is found. Defaults to FALSE.
#' @param ... Optional arguments passed to JuliaCall::julia_setup()
#'
#' @return NULL
#' @export
#'
#' @examples
#' # Start Julia session
#' use_julia()
#' # Stop Julia session
#' use_julia(stop = TRUE)
use_julia <- function(
    stop = FALSE,
    version = "latest",
    JULIA_HOME = NULL,
    JULIA_NUM_THREADS = 4,
    dir = NULL,
    force = FALSE,
    force_install = FALSE, ...){

  # # Ensure Julia session is stopped on exit
  # # Necessary for correct clean up in devtools::check() of examples
  # on.exit({
  #   if (!stop){
  #     options("initialization_sdbuildR" = NULL)
  #     JuliaConnectoR::stopJulia()
  #   }
  # }, add = TRUE)

  if (stop){
    options("initialization_sdbuildR" = NULL)
    JuliaConnectoR::stopJulia()
    return(invisible())
  }

  # Check whether use_julia() was run
  if (!force & !force_install & !is.null(options()[["initialization_sdbuildR"]])){
    return(invisible())
  }

  # Required Julia version for sdbuildR
  required_version = "1.11"

  # If Julia location was specified, check presence of Julia
  if (!is.null(JULIA_HOME)){
    if (!file.exists(JULIA_HOME)){
      stop("Location ", JULIA_HOME, " does not exist.")
    }

    # Find Julia installation in the specified directory - account for users not exactly specifying \bin location
    JULIA_HOME0 = JULIA_HOME
    JULIA_HOME = dir(dirname(JULIA_HOME), pattern = "^bin$", full.names = TRUE)
    # if (.Platform$OS.type == "unix") {
    #   JULIA_HOME = dirname(list.files(
    #     path = dirname(JULIA_HOME),
    #     pattern = "julia$",
    #     full.names = TRUE,
    #     recursive = TRUE
    #   ))
    # } else if (.Platform$OS.type == "windows") {
    #   JULIA_HOME = dirname(list.files(
    #     path = dirname(JULIA_HOME),
    #     pattern = "julia.exe$",
    #     full.names = TRUE,
    #     include.dirs = TRUE,
    #     recursive = TRUE
    #   ))
    # } else {
    #   stop("Unknown or unsupported OS")
    # }

    if (length(JULIA_HOME) == 0){
      stop("No Julia installation found in ", JULIA_HOME0, ". Try use_julia() again without specifying JULIA_HOME to attempt to find it automatically.")
    } else if (length(JULIA_HOME) > 1){
      stop("Multiple Julia installations found in ", JULIA_HOME0, ". Please specify a more specific location.")
    }

  } else {
    # Try to find Julia installation if JULIA_HOME is not specified
    JULIA_HOME <- julia_locate(JULIA_HOME)

    if (is.null(JULIA_HOME)){
      message("No Julia installation found.")
      installJulia = TRUE
    }
  }

  # Check if Julia version is sufficient
  if (!is.null(JULIA_HOME)){
    # Find version
    installed_version = tryCatch({
      julia_version(JULIA_HOME)},
      error = function(e) {
        stop("Julia version of ", JULIA_HOME, " cannot be determined.")
      }
    )

    # Check if version is sufficient
    if (package_version(installed_version) < package_version(required_version)){
      warning("Detected Julia version ", installed_version, ", which is older than needed for sdbuildR (version >= ", required_version, ").")
      installJulia = TRUE
    } else {
      installJulia = FALSE
    }
  }

  installJulia = ifelse(force_install, TRUE, installJulia)

  if (installJulia){

    if (version == "latest"){
      version = julia_latest_version()
    } else {
      if (package_version(version) < package_version(required_version)){
        stop("Julia version ", version, " is older than needed for sdbuildR (version >= ", required_version, ").")
      }
    }

    ans = readline(paste0("Do you want to install Julia version ", version, "? (y/n) "))
    if (trimws(tolower(ans)) %in% c("y", "yes")){
      JULIA_HOME = install_julia_sdbuildR(version = version, dir = dir)

    } else {
      stop("sdbuildR set-up could not be completed.")
    }
  }

  # # Find set-up location for sdbuildR in Julia
  # # sysimage_path = file.path(env_path, "Sysimage.so")
  # julia <- JuliaCall::julia_setup(JULIA_HOME = JULIA_HOME, installJulia=FALSE,
  #                                 install = FALSE, # Don't run - this installs dependencies into the global environment
  #                                 force = force,
  #                                 # sysimage_path = sysimage_path,
  #                                 ...)

  # Set path to Julia executable
  Sys.setenv(JULIA_BINDIR = JULIA_HOME)

  JuliaConnectoR::startJuliaServer()
  JuliaConnectoR::juliaSetupOk()

  # Find set-up location for sdbuildR in Julia
  env_path <- system.file(package = "sdbuildR")
  julia_pkg_path <- file.path(env_path, P[["jl_pkg_name"]])

  # Check if the package is already developed to avoid redundant operations
  pkg_already_developed <- JuliaConnectoR::juliaEval(
    paste0('using Pkg; "', P[["jl_pkg_name"]], '" in [pkg.name for pkg in values(Pkg.dependencies())]')
  )

  # Make sdbuildRUtils available to sdbuildR package environment
  if (!pkg_already_developed){

    # Navigate to Julia package directory
    JuliaConnectoR::juliaEval(paste0('cd("', file.path(env_path, P[["jl_pkg_name"]]), '")'))

    # Activate the sdbuildR package environment
    JuliaConnectoR::juliaEval(paste0('Pkg.activate("', env_path, '")'))

    # Develop the package in the current environment
    JuliaConnectoR::juliaEval(paste0('Pkg.develop(path = "', file.path(env_path, P[["jl_pkg_name"]]), '")'))
  }


  # Set number of Julia threads to use
  if (JULIA_NUM_THREADS > (parallel::detectCores() - 2)){
    warning("JULIA_NUM_THREADS is set to ", JULIA_NUM_THREADS, ", which is higher than the number of available cores minus 2. Setting it to ", parallel::detectCores() - 2, ".")
    JULIA_NUM_THREADS = parallel::detectCores() - 2
  }
  Sys.setenv("JULIA_NUM_THREADS" = JULIA_NUM_THREADS)
  # Sys.getenv("JULIA_NUM_THREADS")

  # Run initialization
  run_init()

  # Set global option of initialization
  # options()[[P$init_sdbuildR]] = TRUE
  options("initialization_sdbuildR" = TRUE)

  invisible()
}



#' Set-up Julia environment for sdbuildR with init.jl
#'
#' @return NULL
#' @noRd
run_init = function(){

  message("Setting up Julia environment for sdbuildR...\n")

  # Find set-up location for sdbuildR in Julia
  env_path <- system.file(package = "sdbuildR")

  # Construct the Julia command to activate the environment and instantiate
  julia_cmd <- sprintf("using Pkg; Pkg.activate(\"%s\"); Pkg.instantiate()", env_path)

  # Execute the command in Julia
  # JuliaCall::julia_command(julia_cmd)
  # JuliaCall::julia_source(file.path(env_path, "init.jl"))

  JuliaConnectoR::juliaEval(julia_cmd)
  JuliaConnectoR::juliaEval(paste0('include("', normalizePath(file.path(env_path, "init.jl"),
                                                              winslash = "/", mustWork = FALSE), '")')) # Source the script

  return(NULL)
}




#' Internal function to create initialization file for julia
#'
#' @return NULL
#' @noRd
#'
create_julia_env = function(){

  # Initialize Julia
  use_julia()

  # Activate Julia environment
  env_path <- system.file(package = "sdbuildR")
  JuliaConnectoR::juliaEval(paste0('using Pkg; Pkg.activate("', env_path, '")'))
  JuliaConnectoR::juliaEval(paste0("Pkg.instantiate()"))

  # Note for extending comparison operators:
  # eltype(5u"m") <: Unitful.Quantity # true
  # eltype(5u"m") <: Number # true
  # eltype(5u"m") <: Float64 # false
  # eltype(5.0) <: Real # true
  # eltype(5) <: Unitful.Quantity # false
  # so we cannot use x::Number, but have to use x::Real

  # # Add standard custom units
  # unit_str = lapply(custom_units(),
  #                   function(x){
  #
  #                     if (is_defined(x[["eqn"]])){
  #                       unit_def = x[["eqn"]]
  #                     } else {
  #                       unit_def = "1"
  #                     }
  #
  #                     paste0("@unit ", x[["name"]], " \"", x[["name"]], "\" ", x[["name"]], " u\"", unit_def, "\" ", ifelse(x[["prefix"]], "true", "false"))
  #
  #                   }) %>% paste0(collapse = sprintf("\n\tUnitful.register(%s)\n\t", P[["sdbuildR_units"]]))
  #
  # unit_str = paste0("\n\n# Define custom units; register after each unit as some units may be defined by other units\nmodule ", P[["sdbuildR_units"]], "\n\tusing Unitful\n\t",
  #                   unit_str,
  #                   "\n\tUnitful.register(", P[["sdbuildR_units"]], ")\nend\n\n", P[["unit_context"]], " = [Unitful.Unitful, ", P[["sdbuildR_units"]], "];\n\n")

  script = paste0(
      "# Load packages\n",
# "using DifferentialEquations#: ODEProblem, solve, Euler, RK4, Tsit5\n",
"using SciMLBase.EnsembleAnalysis\n",
"using OrdinaryDiffEq\n",
"using DiffEqCallbacks#: SavingCallback, SavedValues\n",
"using DataFrames#: DataFrame, select, innerjoin, rename!\n",
"using Distributions\n",
"using Statistics\n",
"using Unitful\n",
"using DataInterpolations\n",
"using Random\n",
"using CSV\n",
"using ", P[["jl_pkg_name"]], "\n",
"using ", P[["jl_pkg_name"]], ".", P[["sdbuildR_units"]], "\n",
"Unitful.register(", P[["jl_pkg_name"]], ".", P[["sdbuildR_units"]], ")\n",

    # P[["unit_context"]], " = [Unitful.Unitful, ", P[["jl_pkg_name"]], ".", P[["sdbuildR_units"]], "];\n\n",

    # # Required when extending a module’s function
    # #import Base: <, >, <=, >=, ==, != #, +, - #, *, /, ^
    #
    # Extend base methods (multiple dispatch) to allow for comparison between a unit and a non-unit; if one of the arguments is a Unitful.Quantity, convert the other to the same unit.
    "Base.:<(x::Unitful.Quantity, y::Float64) = <(x, y * Unitful.unit(x))
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
Base.trunc(x::Unitful.Quantity) = trunc(Unitful.ustrip.(x)) * Unitful.unit(x)\n",

    # Add initialization of sdbuildR
    paste0("\n", P[["init_sdbuildR"]], " = true"),
    collapse = "\n")

  # Write script
  env_path <- system.file(package = "sdbuildR")
  filepath = file.path(env_path, "init.jl")
  write_script(script, filepath)

  # Add dependencies
  pkgs = c("OrdinaryDiffEq" = "6", "DiffEqCallbacks" = "4.8", "DataFrames" = "1.7", "Distributions" = "0.25",
           "Statistics" = "1.11", "Unitful" = "1.23", "DataInterpolations" = "8.1", "Random" = "1.11", "CSV" = "0.10", "SciMLBase" = "2.102")

  for (pkg in names(pkgs)){
    JuliaConnectoR::juliaEval(paste0("Pkg.add(\"", pkg, "\")"))
    JuliaConnectoR::juliaEval(paste0("Pkg.status(\"", pkg, "\")"))
    # JuliaConnectoR::juliaEval(paste0("Pkg.compat(\"", pkg, "\", \"", pkgs[[pkg]], "\")"))
  }

  JuliaConnectoR::juliaEval(paste0("Pkg.update()"))

  JuliaConnectoR::juliaEval("Pkg.compat(\"julia\", \"1.11\")")
  JuliaConnectoR::juliaEval("Pkg.resolve()")
  JuliaConnectoR::juliaEval(paste0("Pkg.precompile()"))

  # Stop Julia
  use_julia(stop = TRUE)

  return(invisible())
}



create_julia_pkg = function(){

  # Initialize Julia
  use_julia()

  # Get Julia functions
  func_def = get_func_julia()

  # Create package directory
  env_path <- system.file(package = "sdbuildR")
  JuliaConnectoR::juliaEval(paste0('cd("', env_path, '")'))
  JuliaConnectoR::juliaEval("using Pkg")
  # JuliaConnectoR::juliaEval(paste0("Pkg.generate(\"", P[["jl_pkg_name"]], "\")"))
  JuliaConnectoR::juliaEval("Pkg.instantiate()")

  # Specify the prefixes for each submodule
  prefix = list(
    "unit_func" = "using Unitful\n",
    "custom_func" = "using Unitful\nusing DataInterpolations\nusing Distributions\nusing ..unit_func: convert_u\n",
    "past" = "using Unitful\nusing ..custom_func: itp\nusing ..unit_func: convert_u\n",
    "clean" = "using Unitful\nusing DataFrames\nusing ..custom_func: itp\n",
    "ensemble" = "using Unitful\nusing Statistics\nusing DataFrames\n")


  # Write all submodules
  for (name in names(prefix)){

    script = paste0("# ", name, "\n",
                    "module ", name, "\n",
                    prefix[[name]], "\n",
                    paste0(func_def[[name]], collapse = "\n\n"),
                    "\nend")

    # Write script
    filepath = file.path(env_path, P[["jl_pkg_name"]], "src", paste0(name, ".jl"))
    write_script(script, filepath)
  }

  # Create module for unit definitions

  # Add standard custom units
  unit_str = lapply(custom_units(),
                    function(x){

                      if (is_defined(x[["eqn"]])){
                        unit_def = x[["eqn"]]
                      } else {
                        unit_def = "1"
                      }

                      paste0("@unit ", x[["name"]], " \"",
                             x[["name"]], "\" ", x[["name"]],
                             " u\"", unit_def, "\" ",
                             ifelse(x[["prefix"]], "true", "false"))

                    }) %>% paste0(collapse = sprintf("\n\tUnitful.register(%s)\n\t", P[["sdbuildR_units"]]))

  script = paste0("# Define custom units; register after each unit as some units may be defined by other units\nmodule ", P[["sdbuildR_units"]], "\n\tusing Unitful\n\t",
                  unit_str,
                  "\n\tUnitful.register(",
                  P[["sdbuildR_units"]], ")\nend\n\n"
                  # P[["unit_context"]], " = [Unitful.Unitful, ", P[["sdbuildR_units"]], "];\n\n"
  )

  # Write script
  filepath = file.path(env_path, P[["jl_pkg_name"]], "src", paste0(P[["sdbuildR_units"]], ".jl"))
  write_script(script, filepath)

#   # Base extensions
#   script = "# julia initialization for sdbuildR package
# # Required when extending a module’s function
# #import Base: <, >, <=, >=, ==, != #, +, - #, *, /, ^
# module BaseExtensions
#
# using Unitful
# # Extend base methods (multiple dispatch) to allow for comparison between a unit and a non-unit; if one of the arguments is a Unitful.Quantity, convert the other to the same unit.
# Base.:<(x::Unitful.Quantity, y::Float64) = <(x, y * Unitful.unit(x))
# Base.:<(x::Float64, y::Unitful.Quantity) = <(x * Unitful.unit(y), y)
#
# Base.:>(x::Unitful.Quantity, y::Float64) = >(x, y * Unitful.unit(x))
# Base.:>(x::Float64, y::Unitful.Quantity) = >(x * Unitful.unit(y), y)
#
# Base.:(<=)(x::Unitful.Quantity, y::Float64) = <=(x, y * Unitful.unit(x))
# Base.:(<=)(x::Float64, y::Unitful.Quantity) = <=(x * Unitful.unit(y), y)
#
# Base.:(>=)(x::Unitful.Quantity, y::Float64) = >=(x, y * Unitful.unit(x))
# Base.:(>=)(x::Float64, y::Unitful.Quantity) = >=(x * Unitful.unit(y), y)
#
# Base.:(==)(x::Unitful.Quantity, y::Float64) = ==(x, y * Unitful.unit(x))
# Base.:(==)(x::Float64, y::Unitful.Quantity) = ==(x * Unitful.unit(y), y)
#
# Base.:(!=)(x::Unitful.Quantity, y::Float64) = !=(x, y * Unitful.unit(x))
# Base.:(!=)(x::Float64, y::Unitful.Quantity) = !=(x * Unitful.unit(y), y)
#
# Base.:%(x::Unitful.Quantity, y::Float64) = %(x, y * Unitful.unit(x))
# Base.:%(x::Float64, y::Unitful.Quantity) = %(x * Unitful.unit(y), y)
#
# Base.mod(x::Unitful.Quantity, y::Float64) = mod(x, y * Unitful.unit(x))
# Base.mod(x::Float64, y::Unitful.Quantity) = mod(x * Unitful.unit(y), y)
#
# Base.rem(x::Unitful.Quantity, y::Float64) = rem(x, y * Unitful.unit(x))
# Base.rem(x::Float64, y::Unitful.Quantity) = rem(x * Unitful.unit(y), y)
#
# Base.min(x::Unitful.Quantity, y::Float64) = min(x, y * Unitful.unit(x))
# Base.min(x::Float64, y::Unitful.Quantity) = min(x * Unitful.unit(y), y)
#
# Base.max(x::Unitful.Quantity, y::Float64) = max(x, y * Unitful.unit(x))
# Base.max(x::Float64, y::Unitful.Quantity) = max(x * Unitful.unit(y), y)
#
# # Extend min/max: when applied to a single vector, use minimum, like in R
# Base.min(v::AbstractVector) = minimum(v)
# Base.max(v::AbstractVector) = maximum(v)
#
# Base.floor(x::Unitful.Quantity) = floor(Unitful.ustrip.(x)) * Unitful.unit(x)
# Base.ceil(x::Unitful.Quantity) = ceil(Unitful.ustrip.(x)) * Unitful.unit(x)
# Base.trunc(x::Unitful.Quantity) = trunc(Unitful.ustrip.(x)) * Unitful.unit(x)
#
# #round_(x::Unitful.Quantity) = round(Unitful.ustrip.(x)) * Unitful.unit(x)
# #round_(x::Unitful.Quantity, digits::Int) = round(Unitful.ustrip.(x), digits=digits) * Unitful.unit(x)
# #round_(x::Unitful.Quantity; digits::Int) = round(Unitful.ustrip.(x), digits=digits) * Unitful.unit(x)
# #round_(x::Unitful.Quantity, digits::Float64) = round(Unitful.ustrip.(x), digits=round(digits)) * Unitful.unit(x)
# #round_(x::Unitful.Quantity; digits::Float64) = round(Unitful.ustrip.(x), digits=round(digits)) * Unitful.unit(x)
#
# round_(x) = round(x)
#
# round_(x, digits::Real) = round(x, digits=round(Int, digits))
#
# round_(x; digits::Real=0) = round(x, digits=round(Int, digits))
#
# round_(x::Unitful.Quantity) = round(Unitful.ustrip(x)) * Unitful.unit(x)
#
# round_(x::Unitful.Quantity, digits::Real) = round(Unitful.ustrip(x), digits=round(Int, digits)) * Unitful.unit(x)
#
# round_(x::Unitful.Quantity; digits::Real=0) = round(Unitful.ustrip(x), digits=round(Int, digits)) * Unitful.unit(x)
#
# end
# "
#   # Write base extensions
#   filepath = file.path(env_path, P[["jl_pkg_name"]], "src", "BaseExtensions.jl")
#   write_script(script, filepath)



  # Write main module
  func_names = names(unlist(unname(func_def), recursive = FALSE, use.names = TRUE))
  script = paste0("# sdbuildRUtils main module\n",
                  "module ", P[["jl_pkg_name"]], "\n",

                  "__precompile__()\n\n",

                  # The order of the files is apparently important; modules calling other modules should be ordered correctly
                  paste0(paste0("include(\"", c(names(prefix),
                                                P[["sdbuildR_units"]]
                                                # "BaseExtensions"
                                                ), ".jl\")"), collapse = "\n"),
                  "\n\n",

                  lapply(names(prefix), function(name){
                    paste0("using .", name, ": ", paste0(unique(names(func_def[[name]])), collapse = ", "))
                  }) %>% paste0(collapse = "\n"), "\n\n",

                  "export ", paste0(func_names, collapse = ", "), "\n\n",

                  # "using ..", P[["sdbuildR_units"]], "\n\n",
                  "end")

  # Write script
  filepath = file.path(env_path, P[["jl_pkg_name"]], "src", paste0(P[["jl_pkg_name"]], ".jl"))
  write_script(script, filepath)

  # Add dependencies
  JuliaConnectoR::juliaEval(paste0('Pkg.activate("', env_path, "\\\\", P[["jl_pkg_name"]],'")'))

  # Add dependencies
  pkgs = c("DataFrames" = "1.7", "Distributions" = "0.25",
           "Statistics" = "1.11", "Unitful" = "1.23",
           "DataInterpolations" = "8.1", "Random" = "1.11")

  for (pkg in names(pkgs)){
    JuliaConnectoR::juliaEval(paste0("Pkg.add(\"", pkg, "\")"))
    JuliaConnectoR::juliaEval(paste0("Pkg.status(\"", pkg, "\")"))
    # JuliaConnectoR::juliaEval(paste0("Pkg.compat(\"", pkg, "\", \"", pkgs[[pkg]], "\")"))
  }

  # JuliaConnectoR::juliaEval("Pkg.gc()")
  JuliaConnectoR::juliaEval("Pkg.update()")

  # Add Julia compatibility
  JuliaConnectoR::juliaEval("Pkg.compat(\"julia\", \"1.11\")")

  # Resolve and precompile packages
  JuliaConnectoR::juliaEval("Pkg.resolve()")
  JuliaConnectoR::juliaEval("Pkg.precompile()")

  # Make sdbuildRUtils available to sdbuildR package environment

  # Navigate to Julia package directory
  JuliaConnectoR::juliaEval(paste0('cd("', file.path(env_path, P[["jl_pkg_name"]]), '")'))

  # Activate the sdbuildR package environment
  JuliaConnectoR::juliaEval(paste0('Pkg.activate("', env_path, '")'))

  # Develop the package in the current environment
  JuliaConnectoR::juliaEval(paste0('Pkg.develop(path = "', file.path(env_path, P[["jl_pkg_name"]]), '")'))

  use_julia(stop=TRUE)

  return(invisible())

}


