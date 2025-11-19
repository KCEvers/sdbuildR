# Check or translate between deSolve and Julia DifferentialEquations solvers

This function either checks whether a solver method exists or provides
bidirectional translation between R's deSolve package solvers and
Julia's DifferentialEquations.jl solvers.

## Usage

``` r
solvers(method, from = c("R", "Julia"), to = NULL, show_info = FALSE)
```

## Arguments

- method:

  Character string of solver name

- from:

  Character string indicating source language: "R" or "Julia"

- to:

  Character string indicating target language: "R" or "Julia"

- show_info:

  Logical, whether to display additional solver information

## Value

Character vector of equivalent solver(s) or list with details

## Examples

``` r
# Translate from R to Julia
solvers("euler", from = "R", to = "Julia")
#> [1] "Euler()"
solvers("rk45dp6", from = "R", to = "Julia")
#> [1] "DP5()"

# Translate from Julia to R
solvers("Tsit5", from = "Julia", to = "R")
#> [1] "rk45dp6"
solvers("DP5", from = "Julia", to = "R", show_info = TRUE)
#> $translation
#> [1] "rk45dp6"
#> 
#> $alternatives
#> [1] "rk45dp7" "rk45e"   "ode45"  
#> 

# List all available solvers
solvers(from = "R")
#>  [1] "euler"      "rk2"        "rk4"        "rk23"       "rk23bs"    
#>  [6] "rk34f"      "rk45f"      "rk45ck"     "rk45e"      "rk45dp6"   
#> [11] "rk45dp7"    "rk78dp"     "rk78f"      "irk3r"      "irk5r"     
#> [16] "irk4hh"     "irk6kb"     "irk4l"      "irk6l"      "ode23"     
#> [21] "ode45"      "lsoda"      "lsode"      "lsodes"     "lsodar"    
#> [26] "vode"       "daspk"      "bdf"        "bdf_d"      "adams"     
#> [31] "impAdams"   "impAdams_d" "radau"     
solvers(from = "Julia")
#>  [1] "Euler()"              "ForwardEuler()"       "Midpoint()"          
#>  [4] "Heun()"               "RK4()"                "BS3()"               
#>  [7] "DP5()"                "RKF45()"              "DP8()"               
#> [10] "RadauIIA3()"          "RadauIIA5()"          "LobattoIIIA4()"      
#> [13] "LobattoIIIA6()"       "Tsit5()"              "Vern6()"             
#> [16] "Vern7()"              "Vern8()"              "Vern9()"             
#> [19] "TanYam7()"            "TsitPap8()"           "Feagin10()"          
#> [22] "Feagin12()"           "Feagin14()"           "Rosenbrock23()"      
#> [25] "Rosenbrock32()"       "TRBDF2()"             "KenCarp4()"          
#> [28] "KenCarp5()"           "AutoTsit5()"          "AutoVern7()"         
#> [31] "CompositeAlgorithm()" "QNDF()"               "FBDF()"              
#> [34] "DFBDF()"              "Rodas4()"             "Rodas5()"            
#> [37] "VCAB3()"              "VCAB4()"              "VCAB5()"             
#> [40] "VCABM3()"             "VCABM4()"             "VCABM5()"            
#> [43] "IDA()"                "SimpleATsit5()"      
```
