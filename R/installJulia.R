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


#' Find latest version of Julia
#'
#' \code{ @evalRd julia_latest_version() }
#'
#' Updated function in JuliaCall, as JuliaCall::julia_version() does not get the latest version as of 29.04.2025
#'
#' @returns String with version number
julia_latest_version <- function(){
  url <- "https://julialang-s3.julialang.org/bin/versions.json"
  file <- tempfile()
  utils::download.file(url, file, quiet = TRUE)
  readfile <- jsonlite::fromJSON(file)
  versions = names(Filter(function(v) v$stable, readfile))
  as.character(max(package_version(versions))) # KCE: Edit to get correct latest version
}


#' Get the URL for a specific version of Julia
#'
#' @inheritParams sdbuildR_setup
#'
#' @returns URL
julia_url <- function(version){
  sysmachine <- Sys.info()["machine"]
  arch <- if (sysmachine == "arm64") {
    "aarch64"
  } else if (.Machine$sizeof.pointer == 8) {
    "x64"
  } else {
    "x86"
  }
  # short_version <- substr(version, 1, 3) # KCE: Edit for correct version
  short_version = paste0(strsplit(version, "\\.")[[1]][1:2], collapse = ".")
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
#' @returns Directory
julia_default_depot <- function(){
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
#' @returns NULL
julia_save_install_dir <- function(dir){
  depot <- Sys.getenv("JULIA_DEPOT_PATH", unset = julia_default_depot())
  prefs <- file.path(depot, "prefs")
  dir.create(prefs, recursive = TRUE, showWarnings = FALSE)
  cat(file.path(dir, "bin"), file = file.path(prefs, "JuliaCall"))
}


#' Install Julia.
#'
#' @param version The version of Julia to install (e.g. \code{"1.6.3"}).
#' @param prefix the directory where Julia will be installed.
#'
install_julia_sdbuildR <- function(version, prefix){

  if (version == "latest"){
    version = julia_latest_version()
  } else if (package_version(version) < "1.11"){
    stop("sdbuildR requires Julia version 1.11 or higher.")
  }
  url <- julia_url(version)

  file <- tempfile()
  tryCatch({
    old_timeout = getOption("timeout")
    options(timeout = 300)
    utils::download.file(url, file)
    options(timeout = old_timeout)
  }, error = function(err) {
    stop(paste("There was an error downloading Julia. This could be due ",
               "to network issues, and might be resolved by re-running ",
               "`install_julia`.",
               sep = ""))
  })

  dest <- file.path(prefix, version)
  if (dir.exists(dest)) {
    unlink(dest, recursive = TRUE)
  }

  sysname <- Sys.info()["sysname"]
  if (sysname == "Linux") {
    utils::untar(file, exdir=dest)
    subfolder <- paste("julia-", version, sep="")
  } else if (sysname == "Darwin") {
    utils::untar(file, exdir=dest)
    subfolder <- paste("julia-", version, sep="")
  } else if (sysname == "Windows") {
    utils::unzip(file, exdir = dest)
    subfolder <- paste("julia-", version, sep="")
  }
  dest <- file.path(dest, subfolder)

  julia_save_install_dir(dest)

  message(sprintf("Installed Julia to %s", dest))

  # invisible(TRUE)
  return(dest)
}




#' Get Julia version
#'
#' @param JULIA_HOME The path to the Julia installation directory.
#'
#' @returns String with version number
#'
julia_version = function(JULIA_HOME){
  command = c("--startup-file=no", "-e", "print(VERSION)")
  system2(file.path(JULIA_HOME, "julia"), shQuote(command), stdout = TRUE)
}
