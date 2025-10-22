#' Check Julia setup
#'
#' @return Logical value
#' @noRd
julia_setup_ok <- function() {
  JuliaConnectoR::juliaSetupOk() &&
    isTRUE(.sdbuildR_env[["jl"]][["init"]]) &&
    !is.null(.sdbuildR_env[["JULIA_BINDIR"]])
}


#' Check Julia environment was initialized
#'
#' This should only be run if a Julia session was already initialized with JuliaConnectoR.
#'
#' @return Logical value
#' @noRd
julia_init_ok <- function() {
  # Check if Unitful was loaded (as an example package that needs to be loaded)
  julia_cmd <- "isdefined(Main, :Unitful)"
  unitful_loaded <- JuliaConnectoR::juliaEval(julia_cmd)

  julia_cmd <- "using Pkg; Pkg.Types.read_project(Pkg.project().path).name"
  project_name <- JuliaConnectoR::juliaEval(julia_cmd)

  # project_name2 <- JuliaConnectoR::juliaEval('
  #   using Pkg, TOML
  #   project_dict = TOML.parsefile(Pkg.project().path)
  #   get(project_dict, "name", "unknown")
  # ')
  # print("project_name2")
  # print(project_name2)

  # # Check name of active project
  # julia_cmd <- "basename(dirname(dirname(Base.active_project())))"
  # active_project <- JuliaConnectoR::juliaEval(julia_cmd)

  # Check init variable exists
  julia_cmd <- paste0("isdefined(Main, :",
                      .sdbuildR_env[["P"]][["init_sdbuildR"]], ")")
  init_sdbuildR <- JuliaConnectoR::juliaEval(julia_cmd)

  return(unitful_loaded && project_name == "sdbuildR" && isTRUE(init_sdbuildR))
}




#' Check status of Julia installation and environment
#'
#' Check if Julia can be found and if the Julia environment for sdbuildR has been instantiated. Note that this does not mean a Julia session has been started, merely whether it *could* be. For more guidance, please see [this vignette](https://kcevers.github.io/sdbuildR/articles/julia-setup.html).
#'
#' @param verbose If TRUE, print detailed status information. Defaults to TRUE.
#'
#' @return A list with components:
#'   \item{julia_found}{Logical. TRUE if Julia installation found.}
#'   \item{julia_path}{Character. Path to Julia bin.}
#'   \item{julia_version}{Character. Julia version string, or NULL if not found.}
#'   \item{env_exists}{Logical. TRUE if inst/Project.toml exists in sdbuildR package.}
#'   \item{env_instantiated}{Logical. TRUE if inst/Manifest.toml exists (i.e., Julia environment was instantiated).}
#'   \item{status}{Character. Overall status: "julia_not_installed", "julia_needs_update", "sdbuildR_needs_reinstall", "install_julia_env", "ready", or "unknown".}
#'
#' @export
#' @family julia
#' @examples
#' status <- julia_status()
#' print(status)
#'
julia_status <- function(verbose = TRUE) {

  result <- list(
    julia_found = FALSE,
    julia_path = NULL,
    julia_version = NULL,
    env_exists = FALSE,
    env_instantiated = FALSE,
    status = "unknown"
  )

  # Find Julia installation
  julia_loc <- find_julia()
  result[["julia_found"]] <- JuliaConnectoR::juliaSetupOk() && !is.null(julia_loc[["path"]]) && nzchar(julia_loc[["path"]]) && file.exists(julia_loc[["path"]])
  result[["julia_path"]] <- julia_loc[["path"]]

  if (!result[["julia_found"]]){
    result$status <- "julia_not_installed"
    if (verbose) {
      message("Julia not found. Install Julia from https://julialang.org/install/")
    }
    return(result)
  }

  result[["julia_version"]] <- julia_loc[["version"]]

  # Required Julia version for sdbuildR
  required_jl_version <- .sdbuildR_env[["jl"]][["required_version"]]

  # Check if version is sufficient
  if (package_version(result[["julia_version"]]) <= package_version(required_jl_version)) {
    result$status <- "julia_needs_update"
    if (verbose) {
      message("Detected Julia version ", result[["julia_version"]],
         ", which is older than needed for sdbuildR (version >= ",
         required_jl_version,
         "). Please go to https://julialang.org/install/ to update Julia.")
    }
    return(result)
  }

  # Check sdbuildR environment files
  env_path <- system.file(package = "sdbuildR")
  project_file <- file.path(env_path, "Project.toml")
  manifest_file <- file.path(env_path, "Manifest.toml")

  result$env_exists <- file.exists(project_file)
  result$env_instantiated <- file.exists(manifest_file)

  if (!result$env_exists) {
    result$status <- "sdbuildR_needs_reinstall"
    if (verbose) {
      message("sdbuildR inst/Project.toml not found. This shouldn't happen - try reinstalling sdbuildR.")
    }
    return(result)
  }

  if (!result$env_instantiated) {
    result$status <- "install_julia_env"
    if (verbose) {
      message("Julia environment not instantiated. Run: install_julia_env()")
    }
    return(result)
  }

  # Everything looks good
  result$status <- "ready"

  if (verbose) {
    message("Julia environment is ready to be activated.")
  }

  return(result)
}


#' Check Manifest.toml for a Package
#'
#' Parses Manifest.toml to check if a package is installed and get its version.
#'
#' @param manifest_file Path to Manifest.toml file
#' @inheritParams find_jl_pkg_version
#'
#' @return List with components:
#'   \item{installed}{Logical. TRUE if package found in Manifest.}
#'   \item{version}{Character. Package version, or NULL if not found.}
#'
#' @noRd
check_manifest_for_pkg <- function(manifest_file, pkg_name) {

  result <- list(
    installed = FALSE,
    version = "0.0.0"
  )

  if (!file.exists(manifest_file)) {
    return(result)
  }

  tryCatch({
    # Read Manifest.toml
    manifest_content <- readLines(manifest_file, warn = FALSE)

    # Find the package section
    # Manifest.toml format (v2.0):
    # [[deps.PackageName]]
    # uuid = "..."
    # version = "1.2.3"

    # Look for the package
    pkg_pattern <- sprintf("\\[\\[deps\\.%s\\]\\]", pkg_name)
    pkg_idx <- grep(pkg_pattern, manifest_content)

    if (length(pkg_idx) == 0) {
      # Try alternative format (older manifests)
      pkg_pattern <- sprintf("\\[\"%s\"\\]", pkg_name)
      pkg_idx <- grep(pkg_pattern, manifest_content)
    }

    if (length(pkg_idx) > 0) {
      result$installed <- TRUE

      # Look for version in the next few lines
      # Read up to 20 lines after the package declaration
      search_lines <- manifest_content[pkg_idx[1]:(min(pkg_idx[1] + 20, length(manifest_content)))]

      version_line <- grep("^version\\s*=", search_lines, value = TRUE)

      if (length(version_line) > 0) {
        # Extract version string
        version_match <- regmatches(
          version_line[1],
          regexec("version\\s*=\\s*\"([^\"]+)\"", version_line[1])
        )

        if (length(version_match[[1]]) > 1) {
          result$version <- version_match[[1]][2]
        }
      }
    }

  }, error = function(e) {
    warning("Error reading Manifest.toml: ", conditionMessage(e))
  })

  return(result)
}


#' Check Julia environment for a package
#'
#' Starts a Julia session, activates the Julia environment for sdbuildR, and checks if a package is installed and get its version.
#'
#' @inheritParams find_jl_pkg_version
#'
#' @return List with components:
#'   \item{installed}{Logical. TRUE if package found in Manifest.}
#'   \item{version}{Character. Package version, or NULL if not found.}
#'
#' @noRd
check_julia_env_for_pkg <- function(pkg_name){

  result <- list(
    installed = FALSE,
    version = "0.0.0"
  )

  result <- tryCatch({

    # Check if package is installed
    is_installed <- tryCatch(
      {
        JuliaConnectoR::juliaEval(paste0(
          '!isnothing(findfirst(p -> p.name == "', pkg_name,
          '", Pkg.dependencies()))'
        ))
      },
      error = function(e) {
        FALSE
      }
    )

    # Check whether version is up to date
    if (is_installed){
      installed_version <- tryCatch(
      {
        JuliaConnectoR::juliaEval(paste0(
          'string(Pkg.dependencies()[findfirst(p -> p.name == "',
          pkg_name,
          '", Pkg.dependencies())].version)'
        ))
      },
      error = function(e) {
        "0.0.0" # If can't determine version, assume it needs update
      }
    )
    } else {
      installed_version <- FALSE
    }

    list(installed = is_installed,
         version = installed_version)

  }, error = function(e){
    return(result)
  })

  return(result)
}



#' Install or update Julia environment
#'
#' Instantiate the Julia environment for sdbuildR to run stock-and-flow models using Julia. For more guidance, please see [this vignette](https://kcevers.github.io/sdbuildR/articles/julia-setup.html).
#'
#' `install_julia_env()` will:
#' * Start a Julia session
#' * Activate a Julia environment using sdbuildR's inst/Project.toml
#' * Install SystemDynamicsBuildR.jl from GitHub (https://github.com/KCEvers/SystemDynamicsBuildR.jl)
#' * Install all other required Julia packages
#' * Create inst/Manifest.toml
#' * Precompile packages for faster subsequent loading
#' * Stop the Julia session
#'
#' Note that this may take 10-25 minutes the first time as Julia downloads and compiles packages.
#'
#' @param remove If TRUE, remove Julia environment for sdbuildR. This will delete the inst/Manifest.toml file, as well as the SystemDynamicsBuildR.jl package. All other Julia packages remain untouched.
#'
#' @returns Invisibly returns NULL after instantiating the Julia environment.
#' @export
#' @family julia
#'
#' @examples
#' \dontrun{
#' if (julia_status()$julia_found) {
#'  install_julia_env()
#' }
#' }
install_julia_env <- function(remove = FALSE){

  status <- julia_status(verbose = FALSE)

  if (!status[["status"]] %in% c("install_julia_env", "ready")){
    stop()
  }

  # Start Julia
  suppressWarnings({ # There is already a connection to Julia established
    JuliaConnectoR::startJuliaServer()
  })

  if (remove){

    # Find set-up location for sdbuildR in Julia
    env_path <- system.file(package = "sdbuildR")

    # Activate the Julia environment for sdbuildR
    julia_cmd <- sprintf("using Pkg; Pkg.activate(\"%s\")", env_path)
    JuliaConnectoR::juliaEval(julia_cmd)

    # Delete SystemDynamicsBuildR.jl
    JuliaConnectoR::juliaEval(sprintf('using Pkg; Pkg.rm("%s")',
                                      .sdbuildR_env[["jl"]][["pkg_name"]]))

    manifest_file <- system.file("Manifest.toml", package = "sdbuildR")
    if (file.exists(manifest_file)){
      file.remove(manifest_file)
    }
    JuliaConnectoR::juliaEval("Pkg.gc()")
    status <- julia_status(verbose = FALSE)

    if (status[["status"]] != "install_julia_env"){
      warning("Removing Julia environment FAILED!")
    } else {
      message("Julia environment successfully removed.")
    }

  } else {
    # Run the setup script
    setup_script <- system.file("setup.jl", package = "sdbuildR")
    JuliaConnectoR::juliaEval(sprintf('include("%s")', setup_script))
    julia_status()

  }

  # Stop Julia
  JuliaConnectoR::stopJulia()

  return(invisible())
}


#' Set up Julia environment
#'
#' Start Julia session and activate Julia environment to simulate stock-and-flow models. `use_julia()` looks for a Julia installation and an instantiated Julia environment specifically for sdbuildR. This can be set up with `install_julia_env()`. For more guidance, please see [this vignette](https://kcevers.github.io/sdbuildR/articles/julia-setup.html).
#'
#' In every R session, `use_julia()` needs to be run once (which is done automatically in `simulate()`), which can take around 30-60 seconds.
#'
#' @param stop If TRUE, stop active Julia session. Defaults to FALSE.
#' @param force If TRUE, force Julia setup to execute again.
#'
#' @return Returns `NULL` invisibly.
#' @export
#' @family julia
#'
#' @examples
#' \dontrun{ # requires Julia setup
#' if (julia_status()$status == "ready") {
#'   # Start a Julia session and activate the Julia environment for sdbuildR
#'   use_julia()
#'
#'   # Stop Julia session
#'   use_julia(stop = TRUE)
#' }
#' }
use_julia <- function(
    stop = FALSE,
    force = FALSE) {

  if (stop) {
    # Check whether a session is active
    if (!julia_setup_ok()) {
      message("No active Julia session found.")
    } else {
      .sdbuildR_env[["jl"]][["init"]] <- FALSE
      JuliaConnectoR::stopJulia()

      message("Julia session closed.")
    }
    return(invisible())
  }

  # If use_julia() was already run, no need to do anything
  if (!force && julia_setup_ok() && julia_init_ok()) {
    return(invisible())
  }

  status <- julia_status()

  if (status[["status"]] != "ready"){
    stop()
  }

  # Set JULIA_BINDIR to ensure JuliaConnectoR uses the right Julia version for sdbuildR
  JULIA_HOME <- status[["julia_path"]]
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

  if (!JuliaConnectoR::juliaSetupOk()) {
    stop("JuliaConnectoR setup of Julia FAILED!")
  }

  tryCatch(
    {
      JuliaConnectoR::startJuliaServer()

      # Run initialization
      run_init()
    },
    warning = function(w) {
      if (grepl(
        "There is already a connection to Julia established",
        conditionMessage(w)
      )) {
        # Ensure Julia environment was correctly initialized for sdbuildR
        if (!julia_init_ok()) {
          use_julia(stop = TRUE)
          JuliaConnectoR::startJuliaServer()

          # Run initialization
          run_init()
        }
      }
    }
  )

  # Try one more time
  if (!julia_init_ok()) {
    use_julia(stop = TRUE)
    JuliaConnectoR::startJuliaServer()

    # Run initialization
    run_init()
  }

  if (!julia_init_ok()) {
    stop("Setup of Julia environment FAILED!")
  }

  # Check whether SystemDynamicsBuildR.jl is up to date
  required_pkg_version <- .sdbuildR_env[["jl"]][["pkg_version"]]
  installed_pkg_version <- find_jl_pkg_version(pkg_name = .sdbuildR_env[["jl"]][["pkg_name"]])

  if (package_version(installed_pkg_version) < package_version(required_pkg_version)){
    JuliaConnectoR::stopJulia()
    stop("Julia packages need to be updated!\nPlease run install_julia_env().")
  }

  # Set global option of initialization
  .sdbuildR_env[["jl"]][["init"]] <- TRUE

  return(invisible())
}


#' Find installed version of Julia package
#'
#' Find the current version of a package by inspecting the inst/Manifest.toml. If this does not work, verify its version in the Julia session. This function should only be run if Julia session was started and Julia environment was activated.
#'
#' @param pkg_name Name of the package to look for
#'
#' @returns Logical value indicating whether the package needs to be updated
#' @noRd
#'
find_jl_pkg_version <- function(pkg_name){

  # Check if SystemDynamicsBuildR.jl is installed by examining Manifest.toml
  manifest_file <- system.file("Manifest.toml", package = "sdbuildR")
  pkg_check <- check_manifest_for_pkg(
    manifest_file,
    pkg_name
  )

  if (pkg_check$installed & pkg_check$version == "0.0.0"){
    # Try alternative way of finding package version if this did not work
    pkg_check <- check_julia_env_for_pkg(pkg_name)
  }

  return(pkg_check$version)
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
  JuliaConnectoR::juliaEval("Pkg.instantiate()")
  JuliaConnectoR::juliaEval('Pkg.resolve()')

  # Source the init.jl script
  init_file <- system.file("init.jl", package = "sdbuildR")
  julia_cmd <- sprintf("include(\"%s\")", init_file)
  JuliaConnectoR::juliaEval(julia_cmd)

  return(NULL)
}




#' Find Julia installation and version
#'
#' @returns List with path to Julia installation (bin directory, not executable) and Julia version
#' @noRd
find_julia <- function(){

  julia_paths <- getJuliaExecutablePath()
  julia_version <- getJuliaVersionViaCmd(julia_paths$juliaCmd)

  return(list(path = julia_paths$juliaBindir,
              version = julia_version))
}



getJuliaExecutablePath <- function() {

  juliaBindir <- Sys.getenv("JULIA_BINDIR")
  if (juliaBindir == "") {
    if (Sys.which("julia") == "") {
      juliaBindir <- juliaCmd <- fallbackOnDefaultJuliaupPath()
    } else { # Julia is on the PATH, simply use the command "julia"
      juliaBindir <- Sys.which("julia")
      juliaCmd <- "julia"
    }
  } else { # use the JULIA_BINDIR variable, as it is specified
    juliaExe <- list.files(path = juliaBindir, pattern = "^julia.*")
    if (length(juliaExe) == 0) {
      stop(paste0("No Julia executable file found in supposed bin directory \"" ,
                  juliaBindir, "\""))
    }
    juliaCmd <- file.path(juliaBindir, "julia")
  }

  # Ensure juliaBindir ends in Julia
  # juliaBindir <- dir(dirname(juliaBindir), pattern = "^bin$", full.names = TRUE)


  # If JULIA_HOME already points to bin directory, keep it
  if (basename(juliaBindir) == "bin") {
    # Already pointing to bin, nothing to do
  } else {
    # Look for bin subdirectory
    juliaBindir0 <- juliaBindir
    dirs <- list.dirs(dirname(juliaBindir), full.names = TRUE)
    bin_dirs <- dirs[grepl("bin$", dirs)]

    if (length(bin_dirs) == 0) {
      stop("No bin directory found in ", juliaBindir0)
    } else if (length(bin_dirs) > 1) {
      stop("Multiple bin directories found in ", juliaBindir0)
    } else {
      juliaBindir <- bin_dirs[1]
    }
  }

  return(list(juliaBindir = juliaBindir,
              juliaCmd = juliaCmd))
}


fallbackOnDefaultJuliaupPath <- function() {
  # If Julia is not found on the PATH, check the default Juliaup installation location
  # on Linux and Mac and use the Julia command there if it exists.
  # (On Mac, Julia might not be on the PATH in the R session even though
  # Julia has been installed in the default way via Juliaup.)
  juliaCmd <- file.path(Sys.getenv("HOME"), ".juliaup", "bin", "julia")
  if (!file.exists(juliaCmd) || Sys.info()['sysname'] == "Windows") {
    stop('Julia could not be found.
Julia needs to be installed and findable for the "JuliaConnectoR" package to work.
After installing Julia, the best way make Julia findable is to put the folder containing the Julia executable into the PATH environment variable.
For more information, see the help topic ?`Julia-Setup`.
')
  } else {
    return(juliaCmd)
  }
}


getJuliaVersionViaCmd <- function(juliaCmd = getJuliaExecutablePath()) {
  juliaVersion <- NULL
  try({
    juliaVersion <- system2(juliaCmd, "--version", stdout = TRUE
                            # env = getJuliaEnv()
                            )
    juliaVersion <- regmatches(juliaVersion,
                               regexpr("[0-9]+\\.[0-9]+\\.[0-9]+",
                                       juliaVersion))
  })
  juliaVersion
}


#' Internal function to create initialization file for Julia
#'
#' @return NULL
#' @noRd
#'
create_julia_init_env <- function() {
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
    "using CSV\n",
    "using DataFrames\n", # : DataFrame, select, innerjoin, rename!\n",
    "using DataInterpolations\n",
    "using DiffEqCallbacks\n", # : SavingCallback, SavedValues\n",
    "using Distributions\n",
    "using OrdinaryDiffEq\n",
    "using Random\n",
    "using Statistics\n",
    "using StatsBase\n",
    "using Unitful\n",
    "using ", .sdbuildR_env[["jl"]][["pkg_name"]], "\n",
    "using ", .sdbuildR_env[["jl"]][["pkg_name"]], ".",
    .sdbuildR_env[["P"]][["sdbuildR_units"]], "\n",
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

  return(invisible())
}
