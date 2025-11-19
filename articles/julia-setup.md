# Setting up Julia

``` r
library(sdbuildR)
library(JuliaConnectoR)
```

sdbuildR uses Julia for ensemble simulations and working with units in
models. This is supported by the R package
[JuliaConnectoR](https://github.com/stefan-m-lenz/JuliaConnectoR/). This
guide will help you install Julia and configure it to work with
sdbuildR.

## Step 1: Install Julia

Download and install Julia from <https://julialang.org/install/>. If you
already have Julia installed, go to the next step.

We recommend using juliaup, Julia’s official version manager, which
makes it easy to install and switch between Julia versions. Some useful
commands are listed below.

``` bash
# See installed versions and current default
juliaup status

# Install latest stable version
juliaup add release

# Install specific version
juliaup add 1.11.3

# Set default version
juliaup default 1.11.3

# Update juliaup and all Julia versions
juliaup update

# Remove a version
juliaup remove 1.10.0

# List all available versions
juliaup list
```

Alternatively, you can download a standalone Julia installer from
<https://julialang.org/downloads/>.

## Step 2: Check if Julia is accessible

After installing Julia, check if JuliaConnectoR can find it:

``` r
juliaSetupOk()
#> [1] TRUE
```

If the Julia installation can be found, try to start a Julia session:

``` r
startJuliaServer()
#> Starting Julia ...
#> Connecting to Julia TCP server at localhost:11980 ...
stopJulia()
```

If this runs successfully, you can skip to step 6. If this throws an
error, go to the next step.

## Step 3: Find your Julia installation path

You need to locate the **bin directory** of your Julia installation (not
the Julia executable itself).

### From Julia

If you can start Julia from your terminal/command prompt, run this
inside Julia:

``` julia
Base.julia_cmd()[1]
```

This returns something like:

- Windows:
  `"C:\\Users\\YourName\\.julia\\juliaup\\julia-1.11.3+0.x64.w64.mingw32\\bin\\julia.exe"`
- macOS:
  `"/Applications/Julia-1.11.app/Contents/Resources/julia/bin/julia"`
- Linux: `"/usr/bin/julia"`

**Important**: You need the bin directory containing the executable
(remove `julia.exe` or `julia` from the end).

### From your terminal/command prompt

**Windows:**

``` bash
where julia
```

**macOS/Linux:**

``` bash
which julia
# or
whereis julia
```

Take note of the path, removing the filename to get just the `bin`
directory.

## Step 4: Add Julia to your PATH permanently

To make Julia accessible to R across all sessions, add it to your
`.Renviron` file.

### Open .Renviron

Run this in R to open your `.Renviron` file:

``` r
# Install usethis if needed
if (!require("usethis")) install.packages("usethis")

# Open .Renviron for editing
usethis::edit_r_environ()
```

This will open `.Renviron` in your text editor. If the file doesn’t
exist, it will be created.

### Add Julia to PATH

Add one of the following lines to `.Renviron`, replacing the path with
your actual Julia bin directory:

**Windows (use forward slashes for the path, semicolon as separator):**

    PATH="C:/Users/YourName/.julia/juliaup/julia-1.11.3+0.x64.w64.mingw32/bin;${PATH}"

**macOS (use `:` as separator):**

    PATH="/Applications/Julia-1.11.app/Contents/Resources/julia/bin:${PATH}"

**Linux (use `:` as separator):**

    PATH="/usr/bin:${PATH}"

Replace the path before the separator with your actual Julia bin
directory. Be sure to keep `${PATH}` at the end, such that the Julia
path is appended to the PATH.

If using juliaup (recommended), you can point to the juliaup directory
instead of a specific version:

**Windows:**

    PATH="C:/Users/YourName/.julia/juliaup;${PATH}"

**macOS/Linux:**

    PATH="~/.julia/juliaup/bin:${PATH}"

This allows juliaup to manage which Julia version is used.

### Save and restart R.

1.  Save the `.Renviron` file
2.  Restart R (Session → Restart R in RStudio, or close and reopen R)

## Step 5: Verify the setup

After restarting R, verify that Julia is accessible:

``` r
juliaSetupOk()
#> [1] TRUE
```

Check whether a Julia session can be started from R:

``` r
startJuliaServer()
#> Starting Julia ...
#> Connecting to Julia TCP server at localhost:11980 ...
stopJulia()
```

## Step 6: Using Julia with sdbuildR

After installing Julia, you need to set up the Julia environment for
sdbuildR:

``` r
install_julia_env()
```

Note that this may take 10-25 minutes the first time as Julia downloads
and compiles packages.

## Step 7: Verify Julia environment setup

Check whether the Julia environment setup was successful:

``` r
julia_status()
#> Julia environment is ready to be activated.
#> $julia_found
#> [1] TRUE
#> 
#> $julia_version
#> [1] "1.12.1"
#> 
#> $env_exists
#> [1] TRUE
#> 
#> $env_instantiated
#> [1] TRUE
#> 
#> $status
#> [1] "ready"
```

To start a Julia session and activate the Julia environment for
sdbuildR:

``` r
use_julia()
#> Starting Julia ...
#> Connecting to Julia TCP server at localhost:11980 ...
#> Setting up Julia environment for sdbuildR...
```

This needs to be done in *each* new R session.

To close the Julia session:

``` r
use_julia(stop = TRUE)
#> Julia session closed.
```

## Troubleshooting

### Julia not found

JuliaConnectoR cannot find Julia, i.e. this evaluates to `FALSE`:

``` r
juliaSetupOk()
#> [1] TRUE
```

JuliaConnectoR tries to find Julia in the following order:

``` r
Sys.getenv("JULIA_BINDIR") # "" if not found

Sys.which("julia") # "" if not found

# On Mac/Linux:
julia_path <- file.path(Sys.getenv("HOME"), ".juliaup", "bin", "julia")
file.exists(julia_path) # FALSE if not found
```

To add Julia to your PATH, follow step 4. If this still doesn’t work:

1.  Double-check the path you added to `.Renviron`
2.  Make sure you’re pointing to the `bin` directory, not the Julia
    executable
3.  Make sure you’re using the file path separator appropriate for your
    operating system, which you can find with `.Platform$file.sep`.
4.  Verify you saved `.Renviron` and restarted R
5.  View your current PATH

Check whether your Julia bin directory is in your PATH:

``` r
# View PATH split by separator for readability
paths <- strsplit(Sys.getenv("PATH"), .Platform$path.sep)[[1]]
paths[grepl("julia", paths)]
```

### Removing the Julia environment for sdbuildR

``` r
install_julia_env(remove = TRUE)
```

### Advanced: Using multiple Julia versions

If you have multiple Julia versions installed, the first one found in
your PATH will be used by default.

#### Switch versions with juliaup

If using juliaup, you can change the default version without modifying
`.Renviron`:

``` bash
# From terminal/command prompt
juliaup default 1.11.3
```

#### Switch versions temporarily

sdbuildR uses your default Julia version. To use a different version:

``` r
Sys.setenv(JULIA_BINDIR = "C:/Users/YourName/.julia/juliaup/julia-1.11.3+0.x64.w64.mingw32/bin")
```

Note: This only affects the current R session and is not permanent.
