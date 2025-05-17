

#' Set up Julia environment for sdbuildR
#'
#' Create Julia environment to simulate stock-and-flow models. `use_julia()` looks for a Julia installation, and will install Julia as well as some required packages if not found. Keep in mind that the first time running `use_julia()` can take around 5-15 minutes, and all subsequent calls can take around 1 minute.
#'
#' @param version Julia version. Default is "latest", which will install the most recent stable release.
#' @param JULIA_HOME Path to Julia installation. Defaults to NULL to locate Julia automatically.
#' @param dir Directory to install Julia. Defaults to NULL to use default location.
#' @param force_install If TRUE, force Julia installation even if existing version is found. Defaults to FALSE.
#' @param ... Optional arguments passed to JuliaCall::julia_setup()
#'
#' @return NULL
#' @export
#'
#' @examples
#' use_julia()
use_julia <- function(
    version = "latest",
    JULIA_HOME = NULL,
    dir = NULL,
    force_install = FALSE, ...){

  # Check whether use_julia() was run
  if (!is.null(options()[["initialization_sdbuildR"]])){
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

  julia <- JuliaCall::julia_setup(JULIA_HOME = JULIA_HOME, installJulia=FALSE,
                                  install = TRUE, # Install dependencies
                                  ...)

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

  message("Setting up Julia environment for sdbuildR...")

  # Find set-up location for sdbuildR in Julia
  env_path <- system.file(package = "sdbuildR")

  # Construct the Julia command to activate the environment and instantiate
  julia_cmd <- sprintf("using Pkg; Pkg.activate(\"%s\"); Pkg.instantiate()", env_path)

  # Execute the command in Julia
  JuliaCall::julia_command(julia_cmd)
  JuliaCall::julia_source(file.path(env_path, "init.jl"))

  return(NULL)
}

