# sdbuildR (development version)

# sdbuildR 1.0.7

# sdbuildR 1.0.6

* Fixed error in finding Julia installation 

* Simulations in Julia are now ensured to stop at exact simulation times, which removes unexpected results created by numerical errors in the solver (using tstops argument in solve()) 

# sdbuildR 1.0.5

# sdbuildR 1.0.4

# sdbuildR 1.0.3

* Removed automatic installation of Julia packages and instead wrote a separate function for this: install_julia_env(). install_julia_env() is called in .onLoad() ONLY if the custom environmental variable AUTO_INSTALL_JULIA_ENV is "true" and NOT_CRAN is "true". This is to ensure GitHub workflows have the Julia environment instantiated, but this will not affect users, as they do not have AUTO_INSTALL_JULIA_ENV.

* Added vignette for formalizing Job Demands-Resources (JDR) Theory 

* Improved documentation

# sdbuildR 1.0.2

# sdbuildR 1.0.1

# sdbuildR 1.0.0

* Added extensive solver options
* Created Julia package (SystemDynamicsBuildR.jl) to contain sdbuildR Julia code
* Added vignette for support in installing and setting up Julia
* Added obligatory argument `times` to step, pulse, ramp, and seasonal
