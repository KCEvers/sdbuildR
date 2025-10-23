# R/zzz.R

.onLoad <- function(libname, pkgname) {
  # Auto-setup Julia environment ONLY during automated testing
  # Multiple conditions ensure this never runs for regular users

  should_auto_setup <- (
    # Condition 1: NOT_CRAN is "true" (testing environment)
    identical(Sys.getenv("NOT_CRAN"), "true") &&

      # Condition 2: Custom environmental variable is "true"
      identical(Sys.getenv("AUTO_INSTALL_JULIA_ENV"), "true") #&&

      # # Condition 3: Additional safety - check for testing indicators
      # (
      #   # testthat is running
      #   requireNamespace("testthat", quietly = TRUE) ||
      #     # devtools is loaded (devtools::check)
      #     "devtools" %in% loadedNamespaces() ||
      #     # CI environment indicators
      #     nzchar(Sys.getenv("CI")) ||
      #     nzchar(Sys.getenv("GITHUB_ACTIONS"))
      # )
  )

  if (should_auto_setup) {
    tryCatch(
      {
        status <- julia_status(verbose = FALSE)

        if (status$status == "install_julia_env") {
          install_julia_env()
        }
      }, error = function(e){
        invisible()
      }
    )
  }
}
