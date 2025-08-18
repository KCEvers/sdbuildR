# Copied from JuliaCall package to fix minor bugs (as of 29.04.2025) in Julia installation, all credit goes to the authors.
# https://github.com/JuliaInterop/JuliaCall/blob/master/R/installJulia.R
# Issue: https://github.com/JuliaInterop/JuliaCall/issues/253
#
# MIT License
# Copyright 2019 Changcheng Li
#
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
#
#   The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


#' Find the default installation directory for Julia
#'
#' @returns Directory
#' @noRd
julia_default_install_dir <- function() {
  if (exists("R_user_dir", asNamespace("tools"))) {
    R_user_dir <- get("R_user_dir", envir = asNamespace("tools"))
    return(file.path(R_user_dir("JuliaCall"), "julia"))
  } else {
    stop("Please update the tools package.")
  }
  # Most users will now have R_user_dir() in the tools package; no need for rappdirs dependency
  # dir <- if (requireNamespace("rappdirs", quietly = TRUE)) {
  #   file.path(rappdirs::user_data_dir("JuliaCall"), "julia")
  # } else {
  #   NULL
  # }
  return(dir)
}


#' Find latest version of Julia
#'
#' \code{ @evalRd julia_latest_version() }
#'
#' Updated function in JuliaCall, as JuliaCall::julia_version() does not get the latest version as of 29.04.2025
#'
#' @noRd
#' @returns String with version number
julia_latest_version <- function() {
  url <- "https://julialang-s3.julialang.org/bin/versions.json"
  file <- tempfile()
  utils::download.file(url, file, quiet = TRUE)
  readfile <- jsonlite::fromJSON(file)
  versions <- names(Filter(function(v) v$stable, readfile))
  as.character(max(package_version(versions))) # KCE: Edit to get correct latest version
}


#' Get the URL for a specific version of Julia
#'
#' @inheritParams use_julia
#'
#' @noRd
#' @returns URL
julia_url <- function(version) {
  sysmachine <- Sys.info()["machine"]
  arch <- if (sysmachine == "arm64") {
    "aarch64"
  } else if (.Machine$sizeof.pointer == 8) {
    "x64"
  } else {
    "x86"
  }
  # short_version <- substr(version, 1, 3) # KCE: Edit for correct version
  short_version <- paste0(strsplit(version, "\\.")[[1]][1:2], collapse = ".")
  sysname <- Sys.info()["sysname"]
  if (sysname == "Linux") {
    os <- "linux"
    slug <- "linux-x86_64"
    ext <- "tar.gz"
  } else if (sysname == "Darwin") {
    os <- "mac"
    slug <- ifelse(sysmachine == "arm64", "macaarch64", "mac64")
    ext <- "tar.gz"
  } else if (sysname == "Windows") {
    os <- "winnt"
    slug <- "win64"
    ext <- "zip"
  } else {
    stop("Unknown or unsupported OS")
  }

  sprintf(
    "https://julialang-s3.julialang.org/bin/%s/%s/%s/julia-%s-%s.%s",
    os, arch, short_version, version, slug, ext
  )
}


#' Get the default depot directory for Julia
#'
#' @noRd
#' @returns Directory
julia_default_depot <- function() {
  key <- if (Sys.info()["sysname"] == "Windows") {
    "USERPROFILE"
  } else {
    "HOME"
  }
  return(file.path(Sys.getenv(key), ".julia"))
}


#' Save Julia installation directory
#'
#' @param dir Directory to save Julia installation
#'
#' @noRd
#' @returns NULL
julia_save_install_dir <- function(dir) {
  depot <- Sys.getenv("JULIA_DEPOT_PATH", unset = julia_default_depot())
  prefs <- file.path(depot, "prefs")
  dir.create(prefs, recursive = TRUE, showWarnings = FALSE)
  cat(file.path(dir, "bin"), file = file.path(prefs, "JuliaCall"))
}


#' Install Julia (adapted for sdbuildR)
#'
#' @param version The version of Julia to install (e.g. \code{"1.6.3"}).
#' @param dir the directory where Julia will be installed.
#' @noRd
#'
install_julia_sdbuildR <- function(version, dir) {
  if (is.null(dir)) {
    dir <- julia_default_install_dir()
  } else if (!file.exists(dir)) {
    stop("The specified directory does not exist.")
  }

  if (is.null(dir) || length(julia_default_install_dir) == 0) {
    stop("Please specify a valid directory for the Julia installation in dir.")
  }

  url <- julia_url(version)

  file <- tempfile()
  tryCatch(
    {
      old_timeout <- getOption("timeout")
      options(timeout = 300)
      utils::download.file(url, file)
      options(timeout = old_timeout)
    },
    error = function(err) {
      stop(paste("There was an error downloading Julia. This could be due ",
        "to network issues, and might be resolved by re-running ",
        "`install_julia`.",
        sep = ""
      ))
    }
  )

  dest <- file.path(dir, version)
  if (dir.exists(dest)) {
    unlink(dest, recursive = TRUE)
  }

  sysname <- Sys.info()["sysname"]
  if (sysname == "Linux") {
    utils::untar(file, exdir = dest)
    subfolder <- paste("julia-", version, sep = "")
  } else if (sysname == "Darwin") {
    utils::untar(file, exdir = dest)
    subfolder <- paste("julia-", version, sep = "")
  } else if (sysname == "Windows") {
    utils::unzip(file, exdir = dest)
    subfolder <- paste("julia-", version, sep = "")
  }
  dest <- file.path(dest, subfolder)

  julia_save_install_dir(dest)

  message(sprintf("Installed Julia to %s", dest))

  return(dest)
}


# https://github.com/JuliaInterop/JuliaCall/blob/7ca03b3a7e2c6657c72108c562ab7d49fdef9207/R/aaa.R
#' Locate Julia installation
#'
#' @param JULIA_HOME
#'
#' @returns Filepath to Julia installation or NULL if not found
#' @noRd
#'
julia_locate <- function(JULIA_HOME = NULL) {
  if (is.null(JULIA_HOME)) {
    JULIA_HOME <- getOption("JULIA_HOME")
  }
  if (is.null(JULIA_HOME)) {
    JULIA_HOME <- if (Sys.getenv("JULIA_HOME") == "") {
      NULL
    } else {
      Sys.getenv("JULIA_HOME")
    }
  }
  if (is.null(JULIA_HOME)) {
    depot <- Sys.getenv("JULIA_DEPOT_PATH", unset = julia_default_depot())
    prefs_file <- file.path(depot, "prefs", "JuliaCall")
    if (file.exists(prefs_file)) {
      JULIA_HOME <- readChar(prefs_file, 256)
    }
  }

  if (is.null(JULIA_HOME)) {
    ## In macOS, the environment variables, e.g., PATH of a GUI is set by launchctl not the SHELL.
    ## You may need to do bash -l -c "which julia" to determine the path to julia.
    ## This fixes the issue that in macOS, R.app GUI cannot find julia.
    ## Thank @randy3k
    julia_bin <- Sys.which("julia")
    if (julia_bin == "") {
      if (.Platform$OS.type == "unix") {
        julia_bin <- system2("bash", "-l -c 'which julia'", stdout = TRUE)[1]
      } else if (.Platform$OS.type == "windows") {
        # look for julia in the most common installation path
        appdata_local_path <- Sys.getenv("LOCALAPPDATA")


        # if the path is not defined, then try to construct it manually
        if (appdata_local_path == "") {
          windows_login_id <- Sys.info()[["login"]]
          # if(windows_login_id == "unknown") {
          #     stop("The Windows login is 'unknown'. Can not find julia executable")
          # }
          appdata_local_path <- file.path("C:/Users/", windows_login_id, "AppData/Local")
        }


        # get a list of folder names
        ld <- list.dirs(appdata_local_path, recursive = FALSE, full.names = FALSE)

        # which of these folers start with "Julia"
        x <- ld[sort(which(substr(ld, 1, 5) == "Julia"))]

        # if(length(x) == 0) {
        #     stop(sprintf("Can not find the Julia installation in the default installation path '%s'", appdata_local_path))
        # }
        if (length(x) > 0) {
          # TODO if interactive() let the user choose a version of Julia
          # keep the lastest version of Julia as that is likeley to be the default
          x <- x[length(x)]

          # filter the ld for folders that starts with Julia
          julia_bin <- file.path(appdata_local_path, x, "bin/julia.exe")
        }
      } else {
        julia_bin <- "julia"
      }
    }
    tryCatch(
      {
        r <- system2(julia_bin, "--startup-file=no -E \"1;\"", stdout = TRUE)
        r <- system2(julia_bin, "--startup-file=no -E \"try println(JULIA_HOME) catch e println(Sys.BINDIR) end;\"", stdout = TRUE)
        r[length(r) - 1]
      },
      warning = function(war) {},
      error = function(err) NULL
    )
  } else {
    tryCatch(
      {
        r <- system2(file.path(JULIA_HOME, "julia"),
          "--startup-file=no -E \"1;\"",
          stdout = TRUE
        )
        r <- system2(file.path(JULIA_HOME, "julia"),
          "--startup-file=no -E \"try println(JULIA_HOME) catch e println(Sys.BINDIR) end;\"",
          stdout = TRUE
        )
        r[length(r) - 1]
      },
      warning = function(war) {},
      error = function(err) NULL
    )
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
