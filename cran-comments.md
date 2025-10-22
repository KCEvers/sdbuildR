## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Resubmission
This is a resubmission. In this version I have:

* Removed the LICENSE file.

* Changed @returns in use_threads() from "NULL" to "No return value, called for side effects", which adds a \value tag in the documentation.

* Removed automatic installation of Julia packages and instead wrote a separate function for this: install_julia_env(). In the description of the function, it is made clear that this will install Julia packages.

