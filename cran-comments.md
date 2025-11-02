## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Reason for update
Fixed critical error in finding Julia installation. The previous version 
  assumed Julia executables were always in a 'bin' directory, but on Windows 
  systems with Julia installed via the Microsoft Store, the executable is 
  located directly in WindowsApps without a 'bin' subdirectory. This caused 
  the package to fail to detect valid Julia installations on these systems. This 
  was only discovered shortly after the initial CRAN release.
