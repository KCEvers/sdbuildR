# Global variables for the sdbuildR package

# Check first if .sdbuildR_env was already initialized. This is for the rare case where someone has run use_julia() already, and reloads sdbuildR, which will overwrite the initialization of use_julia()
if (!exists(".sdbuildR_env")) {
  .sdbuildR_env <- new.env(parent = emptyenv())

  .sdbuildR_env[["jl"]] <- list(
    use_threads = FALSE,
    env_checked = FALSE,
    # TRUE once use_julia() has a Julia session with init.jl loaded. Used only to decide
    # whether to announce the start-up wait: use_julia() runs on every simulate() and is
    # a fast no-op once Julia is up, so the notice must not repeat.
    initialized = FALSE
  )
}
