## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Resubmission
This is a resubmission. In this version I have:

* Changed 'buildeR' to 'builder' in the DESCRIPTION.

* Removed the Maintainer field in the DESCRIPTION, keeping only Authors@R.

* Changed the URL from 'https://vensim.com/' to 'https://en.wikipedia.org/wiki/Vensim' in the README.

* Surrounded examples which require a custom Julia environment to be set up in \dontrun{}. The installation of Julia and required packages can take up to 20 minutes, and the initialization of the Julia environment can take up to a minute, considerably slowing down these examples.

