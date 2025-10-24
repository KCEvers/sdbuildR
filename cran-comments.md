## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Resubmission
This is a resubmission. In this version I have:

* Removed the LICENSE file.

* Changed @returns in use_threads() from "NULL" to "No return value, called for side effects", which adds a \value tag in the documentation.

* Removed automatic installation of Julia packages and instead wrote a separate function for this: install_julia_env(). In the description of the function, it is made clear that this will install Julia packages. The example is surrounded by \dontrun{}. install_julia_env() is called in .onLoad() ONLY if the custom environmental variable AUTO_INSTALL_JULIA_ENV is "true" and NOT_CRAN is "true". This is to ensure GitHub workflows have the Julia environment instantiated, but this will not affect users, as they do not have AUTO_INSTALL_JULIA_ENV.

* Generally improved function documentation. 
