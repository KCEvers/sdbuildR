#' Check or translate between deSolve and Julia DifferentialEquations solvers
#'
#' This function either checks whether a solver method exists or provides
#' bidirectional translation between R's deSolve package solvers and Julia's
#' DifferentialEquations.jl solvers.
#'
#' @param method Character string of solver name
#' @param from Character string indicating source language: "R" or "Julia"
#' @param to Character string indicating target language: "R" or "Julia"
#' @param show_info Logical, whether to display additional solver information
#'
#' @return Character vector of equivalent solver(s) or list with details
#' @family simulate
#' @export
#'
#' @examples
#' # Translate from R to Julia
#' solvers("euler", from = "R", to = "Julia")
#' solvers("rk45dp6", from = "R", to = "Julia")
#'
#' # Translate from Julia to R
#' solvers("Tsit5", from = "Julia", to = "R")
#' solvers("DP5", from = "Julia", to = "R", show_info = TRUE)
#'
#' # List all available solvers
#' solvers(from = "R")
#' solvers(from = "Julia")
solvers <- function(method,
                    from = c("R", "Julia"),
                    to = NULL,
                    show_info = FALSE) {
  if (missing(method) & missing(from)) {
    stop("Either method or from must be specified!")
  }
  from <- clean_language(from)

  # If 'to' is missing, check whether method is valid for this language
  if (is.null(to)) {
    translate <- FALSE
  } else {
    translate <- TRUE
    to <- clean_language(to)
    if (to == from) {
      translate <- FALSE
      to <- NULL
    }
  }

  # Solver translation dictionary
  solver_dict <- list(
    # R to Julia mappings
    r_to_julia = list(
      # Explicit Runge-Kutta methods
      "euler" = list(
        translation = "Euler()",
        alternatives = c("ForwardEuler()"),
        description = "Forward Euler method - 1st order",
        category = "explicit",
        order = 1,
        adaptive = FALSE,
        stiff = FALSE,
        r_only = FALSE
      ),
      "rk2" = list(
        translation = "Midpoint()",
        alternatives = c("Heun()", "RK2()"),
        description = "2nd-order Runge-Kutta",
        category = "explicit",
        order = 2,
        adaptive = FALSE,
        stiff = FALSE,
        r_only = FALSE
      ),
      "rk4" = list(
        translation = "RK4()",
        alternatives = c("ClassicalRungeKutta()"),
        description = "Classical 4th-order Runge-Kutta",
        category = "explicit",
        order = 4,
        adaptive = FALSE,
        stiff = FALSE,
        r_only = FALSE
      ),
      "rk23" = list(
        translation = "BS3()",
        alternatives = c("BogackiShampine3()"),
        description = "Bogacki-Shampine 2(3) adaptive",
        category = "explicit",
        order = c(2, 3),
        adaptive = TRUE,
        stiff = FALSE,
        r_only = FALSE
      ),
      "rk23bs" = list(
        translation = "BS3()",
        alternatives = c("BogackiShampine3()"),
        description = "Bogacki-Shampine 2(3) adaptive",
        category = "explicit",
        order = c(2, 3),
        adaptive = TRUE,
        stiff = FALSE,
        r_only = FALSE
      ),
      "rk34f" = list(
        translation = "BS3()",
        alternatives = c("OwrenZen3()", "RKF45()"),
        description = "3rd-order with 4th-order error estimation",
        category = "explicit",
        order = c(3, 4),
        adaptive = TRUE,
        stiff = FALSE,
        r_only = FALSE
      ),
      "rk45f" = list(
        translation = "DP5()",
        alternatives = c("RKF45()", "Tsit5()"),
        description = "4th-order with 5th-order error estimation",
        category = "explicit",
        order = c(4, 5),
        adaptive = TRUE,
        stiff = FALSE,
        r_only = FALSE
      ),
      "rk45ck" = list(
        translation = "RKF45()",
        alternatives = c("DP5()", "Tsit5()"),
        description = "Runge-Kutta-Fehlberg 4(5)",
        category = "explicit",
        order = c(4, 5),
        adaptive = TRUE,
        stiff = FALSE,
        r_only = FALSE
      ),
      "rk45e" = list(
        translation = "DP5()",
        alternatives = c("Tsit5()", "RKF45()"),
        description = "Dormand-Prince 4(5) variant",
        category = "explicit",
        order = c(4, 5),
        adaptive = TRUE,
        stiff = FALSE,
        r_only = FALSE
      ),
      "rk45dp6" = list(
        translation = "DP5()",
        alternatives = c("Tsit5()", "DormandPrince()"),
        description = "Dormand-Prince 4(5) - most common adaptive method",
        category = "explicit",
        order = c(4, 5),
        adaptive = TRUE,
        stiff = FALSE,
        r_only = FALSE
      ),
      "rk45dp7" = list(
        translation = "DP5()",
        alternatives = c("Tsit5()", "DormandPrince()"),
        description = "Dormand-Prince 4(5) implementation variant",
        category = "explicit",
        order = c(4, 5),
        adaptive = TRUE,
        stiff = FALSE,
        r_only = FALSE
      ),
      "rk78dp" = list(
        translation = "DP8()",
        alternatives = c("Vern7()", "Vern8()", "TanYam7()"),
        description = "Dormand-Prince 7(8) - high-order adaptive",
        category = "explicit",
        order = c(7, 8),
        adaptive = TRUE,
        stiff = FALSE,
        r_only = FALSE
      ),
      "rk78f" = list(
        translation = "DP8()",
        alternatives = c("Feagin14()", "Vern8()"),
        description = "7th-order with 8th-order error estimation",
        category = "explicit",
        order = c(7, 8),
        adaptive = TRUE,
        stiff = FALSE,
        r_only = FALSE
      ),

      # Implicit Runge-Kutta methods
      "irk3r" = list(
        translation = "RadauIIA3()",
        alternatives = c("Rosenbrock23()", "ImplicitEuler()"),
        description = "3rd-order Radau IIA - A-stable implicit",
        category = "implicit",
        order = 3,
        adaptive = TRUE,
        stiff = TRUE,
        r_only = FALSE
      ),
      "irk5r" = list(
        translation = "RadauIIA5()",
        alternatives = c("KenCarp4()", "TRBDF2()"),
        description = "5th-order Radau IIA - high-order implicit",
        category = "implicit",
        order = 5,
        adaptive = TRUE,
        stiff = TRUE,
        r_only = FALSE
      ),
      "irk4hh" = list(
        translation = "LobattoIIIA4()",
        alternatives = c("RadauIIA5()", "Rosenbrock32()"),
        description = "4th-order Hammer-Hollingsworth (Lobatto family)",
        category = "implicit",
        order = 4,
        adaptive = TRUE,
        stiff = TRUE,
        r_only = FALSE
      ),
      "irk6kb" = list(
        translation = "LobattoIIIA6()",
        alternatives = c("RadauIIA5()", "KenCarp5()"),
        description = "6th-order implicit method",
        category = "implicit",
        order = 6,
        adaptive = TRUE,
        stiff = TRUE,
        r_only = FALSE
      ),
      "irk4l" = list(
        translation = "LobattoIIIA4()",
        alternatives = c("RadauIIA3()", "Rosenbrock32()"),
        description = "4th-order Lobatto IIIA - symmetric implicit",
        category = "implicit",
        order = 4,
        adaptive = TRUE,
        stiff = TRUE,
        r_only = FALSE
      ),
      "irk6l" = list(
        translation = "LobattoIIIA6()",
        alternatives = c("RadauIIA5()", "KenCarp5()"),
        description = "6th-order Lobatto IIIA - symmetric implicit",
        category = "implicit",
        order = 6,
        adaptive = TRUE,
        stiff = TRUE,
        r_only = FALSE
      ),

      # MATLAB-compatible methods
      "ode23" = list(
        translation = "BS3()",
        alternatives = c("BogackiShampine3()"),
        description = "MATLAB ode23 equivalent - Bogacki-Shampine 2(3)",
        category = "explicit",
        order = c(2, 3),
        adaptive = TRUE,
        stiff = FALSE,
        r_only = FALSE
      ),
      "ode45" = list(
        translation = "DP5()",
        alternatives = c("Tsit5()", "DormandPrince()"),
        description = "MATLAB ode45 equivalent - Dormand-Prince 4(5)",
        category = "explicit",
        order = c(4, 5),
        adaptive = TRUE,
        stiff = FALSE,
        r_only = FALSE
      ),
      # LSODA family - automatic stiffness detection
      "lsoda" = list(
        translation = "AutoTsit5(Rosenbrock23())",
        alternatives = c("AutoVern7(Rodas4())", "CompositeAlgorithm(Tsit5(), Rodas4())", "QNDF()"),
        description = "Automatic stiffness detection - switches between non-stiff and stiff solvers",
        category = "automatic",
        order = "variable",
        adaptive = TRUE,
        stiff = "auto",
        r_only = FALSE
      ),
      "lsode" = list(
        translation = "QNDF()",
        alternatives = c("Rodas4()", "KenCarp4()", "TRBDF2()"),
        description = "Backward differentiation formulas for stiff systems",
        category = "implicit",
        order = c(1, 5),
        adaptive = TRUE,
        stiff = TRUE,
        r_only = FALSE
      ),
      "lsodes" = list(
        translation = "QNDF()",
        alternatives = c("Rodas4()", "KenCarp4()", "FBDF()"),
        description = "BDF method for stiff systems with sparse Jacobians",
        category = "implicit",
        order = c(1, 5),
        adaptive = TRUE,
        stiff = TRUE,
        r_only = FALSE
      ),
      "lsodar" = list(
        translation = "AutoTsit5(Rosenbrock23())",
        alternatives = c("AutoVern7(Rodas4())", "CompositeAlgorithm(Tsit5(), Rodas4())"),
        description = "LSODA with root finding capabilities",
        category = "automatic",
        order = "variable",
        adaptive = TRUE,
        stiff = "auto",
        r_only = FALSE
      ),
      "vode" = list(
        translation = "Rodas4()",
        alternatives = c("QNDF()", "KenCarp4()", "TRBDF2()"),
        description = "Variable-order, variable-step BDF method",
        category = "implicit",
        order = c(1, 5),
        adaptive = TRUE,
        stiff = TRUE,
        r_only = FALSE
      ),
      "daspk" = list(
        translation = "IDA()",
        alternatives = c("DFBDF()", "Rodas4()", "RadauIIA5()"),
        description = "Differential-algebraic equation solver",
        category = "dae",
        order = c(1, 5),
        adaptive = TRUE,
        stiff = TRUE,
        r_only = FALSE
      ),

      # BDF methods
      "bdf" = list(
        translation = "QNDF()",
        alternatives = c("FBDF()", "Rodas4()"),
        description = "Backward Differentiation Formulas - variable order",
        category = "implicit",
        order = c(1, 5),
        adaptive = TRUE,
        stiff = TRUE,
        r_only = FALSE
      ),
      "bdf_d" = list(
        translation = "QNDF()",
        alternatives = c("FBDF()", "Rodas4()"),
        description = "BDF with dense output",
        category = "implicit",
        order = c(1, 5),
        adaptive = TRUE,
        stiff = TRUE,
        r_only = FALSE
      ),

      # Adams methods
      "adams" = list(
        translation = "VCAB3()",
        alternatives = c("VCAB4()", "VCAB5()", "Vern6()"),
        description = "Adams-Bashforth-Moulton predictor-corrector",
        category = "explicit",
        order = c(1, 12),
        adaptive = TRUE,
        stiff = FALSE,
        r_only = FALSE
      ),
      "impAdams" = list(
        translation = "VCABM3()",
        alternatives = c("VCABM4()", "VCABM5()", "RadauIIA3()"),
        description = "Implicit Adams-Moulton method",
        category = "implicit",
        order = c(1, 12),
        adaptive = TRUE,
        stiff = TRUE,
        r_only = FALSE
      ),
      "impAdams_d" = list(
        translation = "VCABM3()",
        alternatives = c("VCABM4()", "VCABM5()"),
        description = "Implicit Adams-Moulton with dense output",
        category = "implicit",
        order = c(1, 12),
        adaptive = TRUE,
        stiff = TRUE,
        r_only = FALSE
      ),

      # Radau methods
      "radau" = list(
        translation = "RadauIIA5()",
        alternatives = c("RadauIIA3()", "Rodas4()", "KenCarp4()"),
        description = "Radau IIA implicit Runge-Kutta method",
        category = "implicit",
        order = 5,
        adaptive = TRUE,
        stiff = TRUE,
        r_only = FALSE
      )
    ),

    # Julia to R mappings (reverse lookup)
    julia_to_r = list(
      # Direct equivalents
      "Euler()" = list(translation = "euler", alternatives = NULL),
      "ForwardEuler()" = list(translation = "euler", alternatives = NULL),
      "Midpoint()" = list(translation = "rk2", alternatives = NULL),
      "Heun()" = list(translation = "rk2", alternatives = NULL),
      "RK4()" = list(translation = "rk4", alternatives = NULL),
      "BS3()" = list(
        translation = "rk23",
        alternatives = c("rk23bs", "ode23")
      ),
      "DP5()" = list(
        translation = "rk45dp6",
        alternatives = c("rk45dp7", "rk45e", "ode45")
      ),
      "RKF45()" = list(translation = "rk45ck", alternatives = c("rk45f")),
      "DP8()" = list(translation = "rk78dp", alternatives = c("rk78f")),
      "RadauIIA3()" = list(translation = "irk3r", alternatives = NULL),
      "RadauIIA5()" = list(translation = "irk5r", alternatives = NULL),
      "LobattoIIIA4()" = list(translation = "irk4l", alternatives = c("irk4hh")),
      "LobattoIIIA6()" = list(translation = "irk6l", alternatives = c("irk6kb")),

      # Julia-only methods (no direct R equivalent)
      "Tsit5()" = list(
        translation = NULL,
        alternatives = c("rk45dp6", "ode45", "rk45e"),
        description = "Tsitouras 5/4 - often fastest general-purpose solver",
        category = "explicit",
        order = c(5, 4),
        adaptive = TRUE,
        stiff = FALSE,
        julia_only = TRUE
      ),
      "Vern6()" = list(
        translation = NULL,
        alternatives = c("rk78dp", "rk78f"),
        description = "Verner 6th order - high precision",
        category = "explicit",
        order = 6,
        adaptive = TRUE,
        stiff = FALSE,
        julia_only = TRUE
      ),
      "Vern7()" = list(
        translation = NULL,
        alternatives = c("rk78dp", "rk78f"),
        description = "Verner 7th order - high precision",
        category = "explicit",
        order = 7,
        adaptive = TRUE,
        stiff = FALSE,
        julia_only = TRUE
      ),
      "Vern8()" = list(
        translation = NULL,
        alternatives = c("rk78dp", "rk78f"),
        description = "Verner 8th order - very high precision",
        category = "explicit",
        order = 8,
        adaptive = TRUE,
        stiff = FALSE,
        julia_only = TRUE
      ),
      "Vern9()" = list(
        translation = NULL,
        alternatives = c("rk78dp", "rk78f"),
        description = "Verner 9th order - extremely high precision",
        category = "explicit",
        order = 9,
        adaptive = TRUE,
        stiff = FALSE,
        julia_only = TRUE
      ),
      "TanYam7()" = list(
        translation = NULL,
        alternatives = c("rk78dp", "rk78f"),
        description = "Tanaka-Yamashita 7th order",
        category = "explicit",
        order = 7,
        adaptive = TRUE,
        stiff = FALSE,
        julia_only = TRUE
      ),
      "TsitPap8()" = list(
        translation = NULL,
        alternatives = c("rk78dp", "rk78f"),
        description = "Tsitouras-Papakostas 8th order",
        category = "explicit",
        order = 8,
        adaptive = TRUE,
        stiff = FALSE,
        julia_only = TRUE
      ),
      "Feagin10()" = list(
        translation = NULL,
        alternatives = c("rk78dp", "rk78f"),
        description = "Feagin 10th order - ultra high precision",
        category = "explicit",
        order = 10,
        adaptive = TRUE,
        stiff = FALSE,
        julia_only = TRUE
      ),
      "Feagin12()" = list(
        translation = NULL,
        alternatives = c("rk78dp", "rk78f"),
        description = "Feagin 12th order - ultra high precision",
        category = "explicit",
        order = 12,
        adaptive = TRUE,
        stiff = FALSE,
        julia_only = TRUE
      ),
      "Feagin14()" = list(
        translation = NULL,
        alternatives = c("rk78dp", "rk78f"),
        description = "Feagin 14th order - maximum precision explicit",
        category = "explicit",
        order = 14,
        adaptive = TRUE,
        stiff = FALSE,
        julia_only = TRUE
      ),
      "Rosenbrock23()" = list(
        translation = NULL,
        alternatives = c("irk3r", "irk5r"),
        description = "Rosenbrock 2/3 - for mildly stiff problems",
        category = "implicit",
        order = c(2, 3),
        adaptive = TRUE,
        stiff = TRUE,
        julia_only = TRUE
      ),
      "Rosenbrock32()" = list(
        translation = NULL,
        alternatives = c("irk5r", "irk4l"),
        description = "Rosenbrock 3/2 - for mildly stiff problems",
        category = "implicit",
        order = c(3, 2),
        adaptive = TRUE,
        stiff = TRUE,
        julia_only = TRUE
      ),
      "TRBDF2()" = list(
        translation = NULL,
        alternatives = c("irk5r", "irk6kb"),
        description = "Trapezoidal rule + BDF2 - for stiff problems",
        category = "implicit",
        order = 2,
        adaptive = TRUE,
        stiff = TRUE,
        julia_only = TRUE
      ),
      "KenCarp4()" = list(
        translation = NULL,
        alternatives = c("irk5r", "irk4l"),
        description = "Kennedy-Carpenter IMEX 4th order",
        category = "implicit",
        order = 4,
        adaptive = TRUE,
        stiff = TRUE,
        julia_only = TRUE
      ),
      "KenCarp5()" = list(
        translation = NULL,
        alternatives = c("irk6kb", "irk6l"),
        description = "Kennedy-Carpenter IMEX 5th order",
        category = "implicit",
        order = 5,
        adaptive = TRUE,
        stiff = TRUE,
        julia_only = TRUE
      ),
      # Automatic switching algorithms
      "AutoTsit5()" = list(
        translation = "lsoda",
        alternatives = c("lsodar"),
        description = "Automatic switching between Tsit5 and stiff solver",
        category = "automatic",
        order = "variable",
        adaptive = TRUE,
        stiff = "auto",
        julia_only = FALSE
      ),
      "AutoVern7()" = list(
        translation = "lsoda",
        alternatives = c("lsodar"),
        description = "Automatic switching with Verner 7th order base",
        category = "automatic",
        order = "variable",
        adaptive = TRUE,
        stiff = "auto",
        julia_only = TRUE
      ),
      "CompositeAlgorithm()" = list(
        translation = "lsoda",
        alternatives = c("lsodar"),
        description = "Composite algorithm for automatic stiffness switching",
        category = "automatic",
        order = "variable",
        adaptive = TRUE,
        stiff = "auto",
        julia_only = TRUE
      ),

      # BDF methods
      "QNDF()" = list(
        translation = "lsode",
        alternatives = c("lsodes", "bdf", "bdf_d", "vode"),
        description = "Quasi-constant step Nordsieck BDF",
        category = "implicit",
        order = c(1, 5),
        adaptive = TRUE,
        stiff = TRUE,
        julia_only = FALSE
      ),
      "FBDF()" = list(
        translation = "bdf",
        alternatives = c("bdf_d", "lsodes"),
        description = "Fixed-leading coefficient BDF",
        category = "implicit",
        order = c(1, 5),
        adaptive = TRUE,
        stiff = TRUE,
        julia_only = TRUE
      ),
      "DFBDF()" = list(
        translation = NULL,
        alternatives = c("lsode", "bdf", "vode"),
        description = "Dense Fixed BDF - BDF with dense output",
        category = "implicit",
        order = c(1, 5),
        adaptive = TRUE,
        stiff = TRUE,
        julia_only = TRUE
      ),

      # Rosenbrock methods (additional)
      "Rodas4()" = list(
        translation = NULL,
        alternatives = c("vode", "lsode", "radau"),
        description = "Rosenbrock method of order 4 - excellent for stiff problems",
        category = "implicit",
        order = 4,
        adaptive = TRUE,
        stiff = TRUE,
        julia_only = TRUE
      ),
      "Rodas5()" = list(
        translation = NULL,
        alternatives = c("vode", "radau", "lsode"),
        description = "Rosenbrock method of order 5",
        category = "implicit",
        order = 5,
        adaptive = TRUE,
        stiff = TRUE,
        julia_only = TRUE
      ),

      # Adams methods
      "VCAB3()" = list(
        translation = "adams",
        alternatives = NULL,
        description = "Variable coefficient Adams-Bashforth 3rd order",
        category = "explicit",
        order = 3,
        adaptive = TRUE,
        stiff = FALSE,
        julia_only = TRUE
      ),
      "VCAB4()" = list(
        translation = "adams",
        alternatives = NULL,
        description = "Variable coefficient Adams-Bashforth 4th order",
        category = "explicit",
        order = 4,
        adaptive = TRUE,
        stiff = FALSE,
        julia_only = TRUE
      ),
      "VCAB5()" = list(
        translation = "adams",
        alternatives = NULL,
        description = "Variable coefficient Adams-Bashforth 5th order",
        category = "explicit",
        order = 5,
        adaptive = TRUE,
        stiff = FALSE,
        julia_only = TRUE
      ),
      "VCABM3()" = list(
        translation = "impAdams",
        alternatives = c("impAdams_d"),
        description = "Variable coefficient Adams-Bashforth-Moulton 3rd order",
        category = "implicit",
        order = 3,
        adaptive = TRUE,
        stiff = TRUE,
        julia_only = TRUE
      ),
      "VCABM4()" = list(
        translation = "impAdams",
        alternatives = c("impAdams_d"),
        description = "Variable coefficient Adams-Bashforth-Moulton 4th order",
        category = "implicit",
        order = 4,
        adaptive = TRUE,
        stiff = TRUE,
        julia_only = TRUE
      ),
      "VCABM5()" = list(
        translation = "impAdams",
        alternatives = c("impAdams_d"),
        description = "Variable coefficient Adams-Bashforth-Moulton 5th order",
        category = "implicit",
        order = 5,
        adaptive = TRUE,
        stiff = TRUE,
        julia_only = TRUE
      ),

      # DAE solvers
      "IDA()" = list(
        translation = "daspk",
        alternatives = NULL,
        description = "Implicit Differential-Algebraic equation solver",
        category = "dae",
        order = c(1, 5),
        adaptive = TRUE,
        stiff = TRUE,
        julia_only = FALSE
      ),

      # Simple methods
      "SimpleATsit5()" = list(
        translation = NULL,
        alternatives = c("euler", "rk4"),
        description = "Simple adaptive Tsitouras method",
        category = "explicit",
        order = 5,
        adaptive = TRUE,
        stiff = FALSE,
        julia_only = TRUE
      )
    )
  )

  # Handle special cases
  if (missing(method)) {
    if (from == "R") {
      return(names(solver_dict[["r_to_julia"]]))
    } else if (from == "Julia") {
      return(names(solver_dict[["julia_to_r"]]))
    }
  }

  # Check whether specified method is a valid option
  if (!translate) {
    if (from == "R") {
      # Check whether user asked for method in deSolve that is not available in the package
      if (!method %in% names(solver_dict[["r_to_julia"]])) {
        # From https://github.com/cran/deSolve/blob/master/R/ode.R
        additional_methods <- c(
          "lsoda", "lsode", "lsodes", "lsodar", "vode", "daspk",
          "euler", "rk4", "ode23", "ode45", "radau",
          "bdf", "bdf_d", "adams", "impAdams", "impAdams_d",
          "iteration"
        )

        if (method %in% c(deSolve::rkMethod(), additional_methods)) {
          stop(
            "Method ",
            method,
            " is in deSolve, but not yet supported by sdbuildR. Please post an issue on Github."
          )
        } else {
          stop(
            "Method ",
            method,
            " is not found in deSolve methods. Choose from: ",
            paste0(names(solver_dict[["r_to_julia"]]), collapse = ", ")
          )
        }
      } else {
        solver_info <- solver_dict[["r_to_julia"]][[method]]
      }
    } else if (from == "Julia") {
      if (!method %in% names(solver_dict[["julia_to_r"]])) {
        # Handle case variations
        method_clean <- method

        # Try common variations
        variations <- c(
          paste0(method, "()"),
          paste0(toupper(substr(
            method, 1, 1
          )), substr(method, 2, nchar(method))),
          paste0(toupper(substr(method, 1, 1)), substr(method, 2, nchar(method)), "()"),
          tolower(method)
        )
        found <- FALSE
        for (var in variations) {
          if (var %in% names(solver_dict[["julia_to_r"]])) {
            method_clean <- var
            found <- TRUE
            break
          }
        }
        if (!found) {
          stop(
            "Method ",
            method,
            " not found in Julia DifferentialEquations methods, or not supported by sdbuildR. Choose from: ",
            paste0(names(solver_dict[["julia_to_r"]]), collapse = ", ")
          )
        } else {
          method <- method_clean
        }
      }
      solver_info <- solver_dict[["julia_to_r"]][[method]]
    }

    # Remove translation
    solver_info[["translation"]] <- NULL
    solver_info[["name"]] <- method
  } else if (translate) {
    # Perform translation
    if (from == "R" && to == "Julia") {
      if (!method %in% names(solver_dict[["r_to_julia"]])) {
        stop(
          paste(
            "Solver",
            method,
            "not found in deSolve methods. Run solvers(from='R') to see available solvers."
          )
        )
      }

      solver_info <- solver_dict[["r_to_julia"]][[method]]
    } else if (from == "Julia" && to == "R") {
      if (!method %in% names(solver_dict[["julia_to_r"]])) {
        # Handle case variations
        method_clean <- method
        # Try common variations
        variations <- c(
          paste0(method, "()"),
          paste0(toupper(substr(method, 1, 1)), substr(method, 2, nchar(method))),
          paste0(toupper(substr(method, 1, 1)), substr(method, 2, nchar(method)), "()"),
          tolower(method)
        )
        found <- FALSE
        for (var in variations) {
          if (var %in% names(solver_dict[["julia_to_r"]])) {
            method_clean <- var
            found <- TRUE
            break
          }
        }
        if (!found) {
          stop(
            paste(
              "Solver",
              method,
              "not found in Julia DifferentialEquations methods. Run solvers(from='Julia') to see available solvers."
            )
          )
        } else {
          method <- method_clean
        }
      }
      solver_info <- solver_dict[["julia_to_r"]][[method]]
    }
  }


  if (show_info) {
    return(solver_info)
  } else {
    if (translate) {
      if (!is.null(solver_info[["r_only"]])) {
        if (solver_info[["r_only"]]) {
          return(solver_info[["alternatives"]][1])
        }
      } else if (!is.null(solver_info[["julia_only"]])) {
        if (solver_info[["julia_only"]]) {
          return(solver_info[["alternatives"]][1])
        }
      }
      return(solver_info[["translation"]])
    } else {
      return(solver_info[["name"]])
    }
  }
}
