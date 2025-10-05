#' Check Julia environment set up
#'
#' @return Logical value
#' @export
#' @family simulate
#' @examples
#' julia_setup_ok()
julia_setup_ok <- function() {
  isTRUE(.sdbuildR_env[[.sdbuildR_env[["P"]][["init_sdbuildR"]]]]) && !is.null(.sdbuildR_env[["JULIA_BINDIR"]])
}


#' Set up Julia environment
#'
#' Create Julia environment to simulate stock-and-flow models. `use_julia()` looks for a Julia installation, and will install Julia as well as some required packages if not found. A Julia environment is created in the inst directory of the sdbuildR package.
#'
#' Keep in mind that the installation can take around 5-15 minutes. In every R session, `use_julia()` needs to be run once (which is done automatically in simulate()), which can take around 30 seconds.
#'
#' @param stop If TRUE, stop active Julia session. Defaults to FALSE.
#' @param JULIA_HOME Path to Julia installation. Defaults to NULL to locate Julia automatically.
#' @param force If TRUE, force Julia setup to execute again.
#'
#' @return NULL
#' @export
#' @family simulate
#'
#' @examples
#' if (FALSE) {
#'   # Start Julia session
#'   # or install Julia and set up environment (first time use)
#'   use_julia()
#'
#'   # Stop Julia session
#'   use_julia(stop = TRUE)
#' }
use_julia <- function(
    stop = FALSE,
    JULIA_HOME = NULL,
    force = FALSE) {

  if (stop) {
    .sdbuildR_env[[.sdbuildR_env[["P"]][["init_sdbuildR"]]]] <- NULL
    JuliaConnectoR::stopJulia()
    return(invisible())
  }

  # If use_julia() was already run, stop
  if (!force & julia_setup_ok()) {
    return(invisible())
  }

  # Required Julia version for sdbuildR
  required_version <- .sdbuildR_env[["P"]][["jl_required_version"]]

  # If Julia location was specified, check presence of Julia
  if (!is.null(JULIA_HOME)) {
    if (!file.exists(JULIA_HOME)) {
      stop("Location ", JULIA_HOME, " does not exist.")
    }

    # Find Julia installation in the specified directory - account for users not exactly specifying \bin location
    JULIA_HOME0 <- JULIA_HOME
    JULIA_HOME <- dir(dirname(JULIA_HOME), pattern = "^bin$", full.names = TRUE)

    if (length(JULIA_HOME) == 0) {
      stop("No Julia installation found in ", JULIA_HOME0, ". Try use_julia() again without specifying JULIA_HOME to attempt to find it automatically, or go to https://julialang.org/install/ to install Julia.")
    } else if (length(JULIA_HOME) > 1) {
      stop("Multiple Julia installations found in ", JULIA_HOME0, ". Please provide a more specific location.")
    }
  } else {
    if (!is.null(.sdbuildR_env[["JULIA_BINDIR"]])) {
      JULIA_HOME <- .sdbuildR_env[["JULIA_BINDIR"]]
    } else {
      # Try to find Julia installation if JULIA_HOME is not specified
      # JULIA_HOME <- julia_locate(JULIA_HOME)
      JULIA_HOME <- Sys.getenv("JULIA_BINDIR")

      if (JULIA_HOME == ""){
        JULIA_HOME <- Sys.which("julia")

        # Get bin directory
        if (JULIA_HOME != ""){
          if (grepl("julia(\\.exe)?$", basename(JULIA_HOME))) {
            JULIA_HOME <- dirname(JULIA_HOME)
          }
        }

      }
    }

    if (is.null(JULIA_HOME) || JULIA_HOME == "") {
      stop("No Julia installation found. Please go to https://julialang.org/install/ to install Julia.")
    }
  }

  # Check if Julia version is sufficient
  if (!is.null(JULIA_HOME)) {
    # Find version
    installed_version <- tryCatch(
      {
        julia_version(JULIA_HOME)
      },
      error = function(e) {
        stop("Julia version of ", JULIA_HOME, " cannot be determined.")
      }
    )

    # Check if version is sufficient
    if (package_version(installed_version) <= package_version(required_version)) {
      stop("Detected Julia version ", installed_version, ", which is older than needed for sdbuildR (version >= ", required_version, "). Please go to https://julialang.org/install/ to update Julia, or point to a more recently installed Julia version with the argument JULIA_HOME.")
    }
  }


  # Set JULIA_BINDIR to ensure JuliaConnectoR uses the right Julia version for sdbuildR
  old_option <- Sys.getenv("JULIA_BINDIR", unset = NA)
  Sys.setenv("JULIA_BINDIR" = JULIA_HOME)
  .sdbuildR_env[["JULIA_BINDIR"]] <- JULIA_HOME

  on.exit({
    if (is.na(old_option)) {
      Sys.unsetenv("JULIA_BINDIR")
    } else {
      Sys.setenv("JULIA_BINDIR" = old_option)
    }
  })

  tryCatch(
    {
      JuliaConnectoR::startJuliaServer()
    },
    warning = function(w) {
      if (grepl(
        "There is already a connection to Julia established",
        conditionMessage(w)
      )) {
        use_julia(stop = TRUE)
        use_julia()
      }
    }
  )

  if (!JuliaConnectoR::juliaSetupOk()) {
    stop("Set-up of Julia environment FAILED!")
  }

  # Run initialization
  run_init()

  # Set global option of initialization
  if (isFALSE(JuliaConnectoR::juliaEval(.sdbuildR_env[["P"]][["init_sdbuildR"]]))) {
    stop("Set-up of Julia environment FAILED!")
  } else {
    .sdbuildR_env[[.sdbuildR_env[["P"]][["init_sdbuildR"]]]] <- TRUE
    return(invisible())
  }
}



#' Get Julia version
#'
#' @param JULIA_HOME The path to the Julia installation directory.
#'
#' @returns String with version number
#' @noRd
#'
julia_version <- function(JULIA_HOME) {
  command <- c("--startup-file=no", "-e", "print(VERSION)")
  system2(file.path(JULIA_HOME, "julia"), shQuote(command), stdout = TRUE)
}



#' Set up Julia environment for sdbuildR with init.jl
#'
#' @return NULL
#' @noRd
run_init <- function() {

  message("Setting up Julia environment for sdbuildR...\n")

  # Find set-up location for sdbuildR in Julia
  env_path <- system.file(package = "sdbuildR")

  # Activate the Julia environment for sdbuildR
  julia_cmd <- sprintf("using Pkg; Pkg.activate(\"%s\")", env_path)
  JuliaConnectoR::juliaEval(julia_cmd)

  # Install all dependencies from Project.toml
  JuliaConnectoR::juliaEval('Pkg.instantiate()')
  # JuliaConnectoR::juliaEval('Pkg.resolve()')

  # JuliaConnectoR::juliaEval(paste0('using Pkg; Pkg.activate("',
  #                                  normalizePath(env_path, winslash = "/"), '")'))

  # SystemDynamicsBuildR is not in the Project.toml because it is not registered
  # Install from GitHub
  install_sdbuildR_jl_pkg()

  # Source the init.jl script
  julia_cmd <- sprintf("include(\"%s\")", file.path(env_path, "init.jl"))
  JuliaConnectoR::juliaEval(julia_cmd)

  # JuliaConnectoR::juliaEval(paste0(
  #   'include("',
  #   normalizePath(file.path(env_path, "init.jl"),
  #     winslash = "/", mustWork = FALSE
  #   ), '")'
  # ))


  return(NULL)
}



install_sdbuildR_jl_pkg <- function(){

  # Check if SystemDynamicsBuildR is in environment packages
  pkg_in_project <- JuliaConnectoR::juliaEval(
    paste0('haskey(Pkg.project().dependencies, "',
           .sdbuildR_env[["P"]][["jl_pkg_name"]],
           '")')
  )

  installed_version <- tryCatch({
    JuliaConnectoR::juliaEval(paste0(
      'string(Pkg.dependencies()[findfirst(p -> p.name == "',
      .sdbuildR_env[["P"]][["jl_pkg_name"]],
      '", Pkg.dependencies())].version)'
    ))
  }, error = function(e) {
    "0.0.0" # If can't determine version, assume it needs update
  })


  # Add SystemDynamicsBuildR from GitHub if not already added.
  # This will add SystemDynamicsBuildR to the Poject.toml of the Julia
  # environment in the inst directory.
  if (!pkg_in_project) {
    github_url <- "https://github.com/KCEvers/SystemDynamicsBuildR.jl"
    message("Installing SystemDynamicsBuildR from GitHub: ", github_url)
    JuliaConnectoR::juliaEval(paste0(
      'Pkg.add(url="', github_url, '")'
    ))


    # Precompile
    JuliaConnectoR::juliaEval('Pkg.precompile()')

  }


  return(invisible())
}






#' Internal function to create initialization file for Julia
#'
#' @return NULL
#' @noRd
#'
create_julia_init_env <- function() {

  # # Initialize Julia
  # use_julia()
  #
  # # Activate Julia environment
  # env_path <- system.file(package = "sdbuildR")
  # JuliaConnectoR::juliaEval(paste0('using Pkg; Pkg.activate("', env_path, '")'))
  # JuliaConnectoR::juliaEval(paste0("Pkg.instantiate()"))

  # Note for extending comparison operators:
  # eltype(5u"m") <: Unitful.Quantity # true
  # eltype(5u"m") <: Number # true
  # eltype(5u"m") <: Float64 # false
  # eltype(5.0) <: Real # true
  # eltype(5) <: Unitful.Quantity # false
  # so we cannot use x::Number, but have to use x::Real

  script <- paste0(
    "# Load packages\n",
    # "using DifferentialEquations#: ODEProblem, solve, Euler, RK4, Tsit5\n",
    # "using SciMLBase.EnsembleAnalysis\n",
    "using OrdinaryDiffEq\n",
    "using DiffEqCallbacks\n", #: SavingCallback, SavedValues\n",
    "using DataFrames\n", #: DataFrame, select, innerjoin, rename!\n",
    "using Distributions\n",
    "using Statistics\n",
    "using StatsBase\n",
    "using Unitful\n",
    "using DataInterpolations\n",
    "using Random\n",
    "using CSV\n",
    "using ", .sdbuildR_env[["P"]][["jl_pkg_name"]], "\n",
    "using ", .sdbuildR_env[["P"]][["jl_pkg_name"]], ".", .sdbuildR_env[["P"]][["sdbuildR_units"]], "\n",
    # "Unitful.register(", .sdbuildR_env[["P"]][["jl_pkg_name"]], ".", .sdbuildR_env[["P"]][["sdbuildR_units"]], ")\n",

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
    paste0("\n", .sdbuildR_env[["P"]][["init_sdbuildR"]], " = true"),
    collapse = "\n"
  )

  # Write script
  env_path <- system.file(package = "sdbuildR")
  filepath <- file.path(env_path, "init.jl")
  write_script(script, filepath)

  # # Add dependencies
  # pkgs <- .sdbuildR_env[["jl_env_pkg"]]
  #
  # for (pkg in names(pkgs)) {
  #   JuliaConnectoR::juliaEval(paste0("Pkg.add(\"", pkg, "\")"))
  #   JuliaConnectoR::juliaEval(paste0("Pkg.status(\"", pkg, "\")"))
  #   JuliaConnectoR::juliaEval(paste0(
  #     "Pkg.compat(\"", pkg, "\", \"",
  #     pkgs[[pkg]], "\")"
  #   ))
  # }
  #
  # # JuliaConnectoR::juliaEval(paste0("Pkg.update()"))
  # # JuliaConnectoR::juliaEval("Pkg.compat(\"julia\", \"1.11\")")
  # # JuliaConnectoR::juliaEval("Pkg.resolve()")
  # # JuliaConnectoR::juliaEval(paste0("Pkg.precompile()"))
  #
  # # Stop Julia
  # use_julia(stop = TRUE)

  return(invisible())
}

