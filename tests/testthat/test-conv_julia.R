test_that("converting equations to Julia", {
  sfm <- stockflow("predator_prey")
  var_names <- get_model_var(sfm)
  name <- var_names[1]
  type <- "aux"

  result <- convert_equations_julia(
    type, name, "min(predator_births)", var_names
  )
  expected <- "r_min(predator_births)"
  expect_equal(result$eqn, expected)

  result <- convert_equations_julia(
    type, name, "max(predator_births)", var_names
  )
  expected <- "r_max(predator_births)"
  expect_equal(result$eqn, expected)

  result <- convert_equations_julia(
    type, name, "c(0, predator_births, 1)", var_names
  )
  expected <- "[0.0, predator_births, 1.0]"
  expect_equal(result$eqn, expected)

  result <- convert_equations_julia(
    type, name, "range(predator_births, predator_deaths) * 10", var_names
  )
  expected <- "r_range(predator_births, predator_deaths) .* 10.0"
  expect_equal(result$eqn, expected)

  result <- convert_equations_julia(
    type, name, "test = function(a, b){
                                       a + b
                                       }", var_names
  )
  expected <- "function test(a, b) a .+ b end"
  expect_equal(stringr::str_squish(result$eqn), expected)

  result <- convert_equations_julia(
    type, name, "c(9 + 8 - 0, c('1 + 2 + 3'))", var_names
  )
  expected <- "[9.0 .+ 8.0 .- 0.0, [\"1 + 2 + 3\"]]"
  expect_equal(result$eqn, expected)

  result <- convert_equations_julia(
    type, name, "1E08", var_names
  )
  expected <- "100000000.0"
  expect_equal(result$eqn, expected)

  result <- convert_equations_julia(
    type, name, "c(T, TRUE, 'F', F+T, NULL, NA)", var_names
  )
  expected <- "[true, true, \"F\", false .+ true, nothing, missing]"
  expect_equal(result$eqn, expected)

  result <- convert_equations_julia(
    type, name, "for (i in 1:9){\n\tprint(i)\n}", var_names
  )
  expected <- "for i in 1:9\nprintln(i)\nend"
  expect_equal(result$eqn, expected)

  result <- convert_equations_julia(
    type, name, "if(t<2020){\n\tgf<-0.07\n} else if (t<2025){\n\tgf<-0.03\n} else {\n\tgf<-0.02\n}", var_names
  )
  expected <- "if t .< 2020.0\ngf = 0.07\nelseif t .< 2025.0\ngf = 0.03\nelse\ngf = 0.02\nend"
  expect_equal(result$eqn, expected)

  # while
  result <- convert_equations_julia(
    type, name, "while(a < 0){\n\tif (prey >0){\n\t\ta <- 0\n\t} else {\n\ta = 1\n\t}\n}", var_names
  )
  expected <- "while a .< 0.0\nif prey .> 0.0\na = 0.0\nelse\na = 1.0\nend\nend"
  expect_equal(result$eqn, expected)

  # oneliner if ***
})


test_that("R integer literals convert to Julia-safe numeric or index forms", {
  var_names <- c("x", "n")

  conv <- function(expr) convert_equations_julia("aux", "z", expr, var_names)[["eqn"]]

  expect_equal(conv("1L"), "1.0")
  expect_equal(conv("c(1L, 2L)"), "[1.0, 2.0]")
  expect_equal(conv("x[1L]"), "x[1]")
  expect_equal(conv("1L:3L"), "1:3")
  expect_equal(conv("seq(1L, 3L)"), "range(1.0, 3.0, step=1.0)")
})


test_that("Julia conversion rejects or handles previously unchecked edge cases", {
  var_names <- c("x", "n", "a", "b")
  conv <- function(expr) convert_equations_julia("aux", "z", expr, var_names)[["eqn"]]

  expect_equal(conv("sample.int(n, 2)"), "StatsBase.sample(seq(1.0, n), round_(2.0), replace=false)")
  expect_equal(conv("break"), "break")
  expect_equal(conv("next"), "continue")
  expect_equal(
    conv("repeat { x <- x + 1; if (x > 10) break }"),
    "while true\nx = x .+ 1.0\nif x .> 10.0\nbreak\nend\nend"
  )
  expect_equal(conv("x[c(1, 2)]"), "x[[1, 2]]")
  expect_equal(conv("base::sum(x)"), "sum(x)")
  expect_equal(conv("stats::median(x)"), "Statistics.median(x)")

  expect_error(
    conv("pkg::unknown(x)"),
    "Unsupported namespaced function"
  )
  expect_error(
    conv("x[-1]"),
    "Negative R-style indices"
  )
  expect_error(
    conv("x[TRUE]"),
    "Logical R-style indices"
  )
  expect_error(
    conv("x[1.5]"),
    "Non-integer indices"
  )
  expect_error(
    conv("x$y"),
    "Field and slot access"
  )
  expect_error(
    conv("x@y"),
    "Field and slot access"
  )
})


test_that("converting functions to Julia with named arguments", {
  skip_on_cran()

  sfm <- stockflow("predator_prey")
  var_names <- get_model_var(sfm)
  name <- var_names[1]
  type <- "aux"

  # Check that functions without named arguments (e.g., min) have their names stripped
  result <- convert_equations_julia(
    type, name, "min(x = predator_births)", var_names
  )
  expected <- "r_min(predator_births)"
  expect_equal(result$eqn, expected)

  result <- convert_equations_julia(
    type, name, "min(x = max(y = predator_births))", var_names
  )
  expected <- "r_min(r_max(predator_births))"
  expect_equal(result$eqn, expected)

  result <- convert_equations_julia(
    type, name, "range(x = 10, y= 8)", var_names
  )
  expected <- "r_range(10.0, 8.0)"
  expect_equal(result$eqn, expected)

  result <- convert_equations_julia(
    type, name, "logistic(x, midpoint = 8)", var_names
  )
  expected <- "logistic.(x, 1.0, 8.0, 1.0)"
  expect_equal(result$eqn, expected)

  result <- convert_equations_julia(
    type, name, "logistic(x, upper = 8)", var_names
  )
  expected <- "logistic.(x, 1.0, 0.0, 8.0)"
  expect_equal(result$eqn, expected)


  # Error for na.rm
  expect_error(
    convert_equations_julia(type, name, "sd(x = test, na.rm = T)", var_names),
    "na\\.rm.*not supported"
  )

  expect_error(
    lang_adapter("Julia")$convert_eqn(type, name, "sd(x = test, na.rm = TRUE)", var_names),
    "na\\.rm.*not supported"
  )

  # Check that wrong arguments throw error
  expect_error(convert_equations_julia(
    type, name, "sd(a, y = test)",
    var_names
  ), "not allowed.*sd|Invalid argument.*sd", ignore.case = TRUE)

  expect_error(convert_equations_julia(
    type, name, "rnorm(x = predator_births, mean = 0)",
    var_names
  ), "not allowed.*rnorm|Invalid argument.*rnorm", ignore.case = TRUE)

  expect_error(convert_equations_julia(
    type, name, "rnorm(n = 1, x = predator_births, mean = 0)",
    var_names
  ), "not allowed.*rnorm|Invalid argument.*rnorm", ignore.case = TRUE)

  expect_error(convert_equations_julia(
    type, name, "rnorm(dt)",
    var_names
  ), "Invalid first argument of.*rnorm")


  # Check for missing obligatory arguments
  expect_error(convert_equations_julia(
    type, name, "rnorm()",
    var_names
  ), "Missing required argument.*rnorm", ignore.case = TRUE)

  expect_error(convert_equations_julia(
    type, name, "rnorm(sd = predator_births, mean = 0)",
    var_names
  ), "Missing required argument.*rnorm", ignore.case = TRUE)


  # Check error for too many arguments
  expect_error(convert_equations_julia(
    type, name, "dnorm(1, 2, 3, log=FALSE, predator_deaths)",
    var_names
  ), "Too many arguments.*dnorm", ignore.case = TRUE)


  # Error when not all default arguments are at the end
  expect_error(
    stockflow() |> custom_func("Function", "function(x, y = 1, z) x + y"),
    "Change the function definition of"
  )

  expect_error(stockflow() |> custom_func("Function", "function(x, y = 1, z){\nx + y\n}"), "Change the function definition of")

  expect_error(stockflow() |> custom_func("Function", "function(x, y = 1, z, a = 1) x + y"), "Change the function definition of")

  expect_error(stockflow() |> custom_func("Function", "function(x, y = 1, z, a = 1){\nx + y\n}"), "Change the function definition of")

  expect_no_error(stockflow() |> custom_func("Function", "function(x, y = 1, a = 1){\nx + y\n}"))
  expect_no_error(stockflow() |> custom_func("Function", "function(x){\nx + y\n}"))
  expect_no_error(stockflow() |> custom_func("Function", "function(y = 1){\nx + y\n}"))
})


test_that("custom function definitons work", {
  sfm <- stockflow() |>
    custom_func("myfunc", "function(x, y = 1, z = 2) x + y")

  # Test func definition conversion via convert_equations_julia
  var_names <- get_model_var(sfm)
  func_eqn <- as.data.frame(sfm, type = "func")[["eqn"]][[1]]
  result <- convert_equations_julia(
    "func", "myfunc",
    paste0("myfunc = ", func_eqn), var_names
  )
  expect_equal(result$eqn, "function myfunc(x, y = 1.0, z = 2.0)\nx .+ y\nend")

  # Is the function usable in R?
  sfm <- sfm |>
    sim_settings(language = "R", stop = 1, dt = .1) |>
    update("a", "stock", eqn = "myfunc(1, 2)")
  sim <- expect_no_error(simulate(sfm))
  expect_equal(sim$df[sim$df$variable == "a", "value"][1], 1 + 2)

  # Named argument in R
  sfm <- sfm |> update("a", eqn = "myfunc(1, y = 2)")
  sim <- expect_no_error(simulate(sfm))
  expect_equal(sim$df[1, "value"], 1 + 2)

  # Switch order of arguments in R
  sfm <- sfm |> update("a", eqn = "myfunc(1, z = 3, y = 2)")
  sim <- expect_no_error(simulate(sfm))
  expect_equal(sim$df[1, "value"], 1 + 2)

  # Test named argument conversion to Julia
  result <- convert_equations_julia("stock", "a", "myfunc(1, y = 2)", var_names)
  expect_equal(result$eqn, "myfunc(1.0, y = 2.0)")

  result <- convert_equations_julia("stock", "a", "myfunc(1, z = 3, y = 2)", var_names)
  expect_equal(result$eqn, "myfunc(1.0, z = 3.0, y = 2.0)")

  # Repeat in Julia
  skip_if_julia_not_ready()

  sfm <- stockflow() |>
    custom_func("myfunc", "function(x, y = 1, z = 2) x + y") |>
    sim_settings(language = "Julia", stop = 1, dt = .1) |>
    update("a", "stock", eqn = "myfunc(1, 2)")
  sim <- expect_no_error(simulate(sfm))
  expect_equal(sim$df[1, "value"], 1 + 2)
})


test_that("converting statements", {
  sfm <- stockflow("predator_prey")
  var_names <- get_model_var(sfm)

  eqn <- "if(a > b){\n\t a + b\n} # test () {}"
  result <- convert_all_statements_julia(eqn, var_names)
  expected <- "if a > b \n\t a + b\nend # test () {}"
  expect_equal(result, expected)

  eqn <- "if(a > b){\n\t print('brace { in quote')\n}"
  result <- convert_all_statements_julia(eqn, var_names)
  expected <- "if a > b \n\t print('brace { in quote')\nend"
  expect_equal(result, expected)

  eqn <- "if (a + min(c(1, 2)) < 0){\n\t print(a)\n} else {\n\t  print(b)\n}"
  result <- convert_all_statements_julia(eqn, var_names)
  expected <- "if  a + min(c(1, 2)) < 0 \n\t print(a)\n else \n\t  print(b)\nend"
  expect_equal(result, expected)


  eqn <- "if (a + min(c(1, 2)) < 0){\n\t print(a)\n} else if (b + a == 1)  {\n\t  print(b)\n} else if (b + a == 1)  {\n\t  print(c)\n} else {\n\t  print('no')\n}"
  result <- convert_all_statements_julia(eqn, var_names)
  expected <- "if  a + min(c(1, 2)) < 0 \n\t print(a)\nelseif b + a == 1   \n\t  print(b)\nelseif b + a == 1   \n\t  print(c)\n else \n\t  print('no')\nend"
  expect_equal(result, expected)


  eqn <- "# Description\na = function(c, b = 1) {\n\t return(c + b)\n}"
  result <- convert_all_statements_julia(eqn, var_names)
  expected <- "# Description\nfunction a(c, b = 1) \n\t return(c + b)\nend"
  expect_equal(result, expected)

  #   # More complicated nested statements
  #   eqn = "a = function(c, b = 1){
  #   if (c > 0){
  #     return(c + b)
  #   } else if (c < 0){
  #     return(c - b)
  #   } else {
  #     return(0)
  #   }
  # }"
  #   result = convert_all_statements_julia(eqn, var_names)
  #   expected = ""
  #   expect_equal(result, expected)


  # # One-liner functions with brackets
  # # ** to do: this doesn't work if there is no name assigned...
  # # eqn = "sum_two_nums <- function(x, y) x + y"
  # # eqn = "sum_two_nums <- function(x, y = c('a', 'b')) x + y"
  # eqn = "function(x) x + 1"
  # result = convert_all_statements_julia(eqn, var_names)
  # expected = ""
  # expect_equal(result, expected)

  # **to do: hysteresis model
  # "F <- function(x,a,d){\nif (x > a + d){\n  1\n  } else if (x > a){\n   1-2*(((a-x + d)/(2*d))^2)\n   } else {\n       ifelse(x>a-d, 2*(((x-a + d)/(2*d))^2), 0)\n}\n}"
})


test_that("convert_distribution() to Julia", {
  sfm <- stockflow("predator_prey")
  # names_df = get_names(sfm)
  var_names <- get_model_var(sfm)
  name <- var_names[1]
  type <- "stock"

  # When n = 1, don't add n, otherwise this create a vector
  result <- convert_equations_julia(type, name, "runif(1)", var_names)$eqn
  expected <- "rand(Distributions.Uniform(0.0, 1.0))"
  expect_equal(result, expected)

  result <- convert_equations_julia(type, name, "runif(1, min =1, max=3)", var_names)$eqn
  expected <- "rand(Distributions.Uniform(1.0, 3.0))"
  expect_equal(result, expected)

  result <- convert_equations_julia(type, name, "runif(10, min =-1, max=3)", var_names)$eqn
  expected <- "rand(Distributions.Uniform(-1.0, 3.0), 10)"
  expect_equal(result, expected)

  result <- convert_equations_julia(type, name, "rnorm(1)", var_names)$eqn
  expected <- "rand(Distributions.Normal(0.0, 1.0))"
  expect_equal(result, expected)

  # Different order of arguments
  result <- convert_equations_julia(type, name, "rnorm(10, sd =1, mean=3)", var_names)$eqn
  expected <- "rand(Distributions.Normal(3.0, 1.0), 10)"
  expect_equal(result, expected)

  result <- convert_equations_julia(type, name, "rnorm(10, 1, 3)", var_names)$eqn
  expected <- "rand(Distributions.Normal(1.0, 3.0), 10)"
  expect_equal(result, expected)

  # R parameterizes Exponential/Gamma by rate; Julia uses scale = 1 / rate
  result <- convert_equations_julia(type, name, "rexp(10, 3)", var_names)$eqn
  expected <- "rand(Distributions.Exponential(1 / (3.0)), 10)"
  expect_equal(result, expected)

  result <- convert_equations_julia(type, name, "rexp(1, rate=30)", var_names)$eqn
  expected <- "rand(Distributions.Exponential(1 / (30.0)))"
  expect_equal(result, expected)


  # cdf, pdf, quantile
  result <- convert_equations_julia(type, name, "pexp(1, rate=30)", var_names)$eqn
  expected <- "Distributions.cdf.(Distributions.Exponential(1 / (30.0)), 1)"
  expect_equal(result, expected)

  result <- convert_equations_julia(type, name, "qexp(1, rate=30)", var_names)$eqn
  expected <- "Distributions.quantile.(Distributions.Exponential(1 / (30.0)), 1)"
  expect_equal(result, expected)

  result <- convert_equations_julia(type, name, "dexp(1, rate=30)", var_names)$eqn
  expected <- "Distributions.pdf.(Distributions.Exponential(1 / (30.0)), 1)"
  expect_equal(result, expected)

  # Gamma: sort_args() resolves scale = 1/rate, so rate is dropped
  result <- convert_equations_julia(type, name, "pgamma(1, 2, rate=30)", var_names)$eqn
  expected <- "Distributions.cdf.(Distributions.Gamma(2.0, 1.0/30.0), 1)"
  expect_equal(result, expected)

  result <- convert_equations_julia(type, name, "qgamma(1, 2, rate=30)", var_names)$eqn
  expected <- "Distributions.quantile.(Distributions.Gamma(2.0, 1.0/30.0), 1)"
  expect_equal(result, expected)

  result <- convert_equations_julia(type, name, "dgamma(1, 2, rate=30)", var_names)$eqn
  expected <- "Distributions.pdf.(Distributions.Gamma(2.0, 1.0/30.0), 1)"
  expect_equal(result, expected)
})


test_that("corrected builtin conversions to Julia", {
  sfm <- stockflow("predator_prey")
  var_names <- c(get_model_var(sfm), "m", "x", "y", "tbl", "cond", "a", "b")
  name <- var_names[1]
  type <- "aux"
  conv <- function(s) convert_equations_julia(type, name, s, var_names)$eqn

  # Reductions: R's na.rm/use defaults are no longer injected (syntax1 no-fill)
  expect_equal(conv("mean(x)"), "Statistics.mean(x)")
  expect_equal(conv("median(x)"), "Statistics.median(x)")
  expect_equal(conv("sd(x)"), "Statistics.std(x)")
  expect_equal(conv("var(x)"), "Statistics.var(x)")
  expect_equal(conv("cor(x, y)"), "Statistics.cor(x, y)")

  # pmin/pmax keep all args (na.rm no longer captures a positional arg)
  expect_equal(conv("pmin(a, b)"), "min.(a, b)")
  expect_equal(conv("pmax(a, b)"), "max.(a, b)")

  # sort -> r_sort; decreasing passed positionally to the wrapper
  expect_equal(conv("sort(x)"), "r_sort(x)")
  expect_equal(conv("sort(x, decreasing=TRUE)"), "r_sort(x, true)")
  expect_equal(conv("unique(x)"), "unique(x)")

  # paste0 -> string (broadcast), not join
  expect_equal(conv("paste0(\"a\", x)"), "string.(\"a\", x)")

  # as.logical -> r_as_logical (broadcast), not Bool
  expect_equal(conv("as.logical(x)"), "r_as_logical.(x)")

  # nchar -> length (broadcast); no spurious type/allowNA/keepNA args
  expect_equal(conv("nchar(x)"), "length.(x)")

  # str_to_title -> titlecase (namespaced name no longer errors; no locale arg)
  expect_equal(conv("stringr::str_to_title(x)"), "titlecase.(x)")

  # match -> r_match (was a latent passthrough to Julia's regex match)
  expect_equal(conv("match(x, tbl)"), "r_match(x, tbl)")

  # na.omit -> r_na_omit (eager), range -> r_range (vector), ifelse broadcast
  expect_equal(conv("na.omit(x)"), "r_na_omit(x)")
  expect_equal(conv("range(x)"), "r_range(x)")
  expect_equal(conv("ifelse(cond, a, b)"), "ifelse.(cond, a, b)")

  # grep -> r_grep (faithful wrapper, fill path); value lands in correct slot
  expect_equal(
    conv("grep(\"a\", x)"),
    "r_grep(\"a\", x, false, false, false, false, false, false)"
  )
  expect_equal(
    conv("grep(\"a\", x, value=TRUE)"),
    "r_grep(\"a\", x, false, false, true, false, false, false)"
  )

  # rbind -> r_rbind, and arguments are no longer dropped (deparse.level fix)
  expect_equal(conv("rbind(x, x)"), "r_rbind(x, x)")
  expect_equal(conv("rbind(x, x, x)"), "r_rbind(x, x, x)")

  # cbind keeps all arguments now (previously dropped to hcat(x))
  expect_equal(conv("cbind(x, x)"), "hcat(x, x)")

  # upper.tri / lower.tri -> logical-mask helpers (diag from wrapper default)
  expect_equal(conv("upper.tri(m)"), "r_upper_tri(m)")
  expect_equal(conv("upper.tri(m, TRUE)"), "r_upper_tri(m, true)")
  expect_equal(conv("lower.tri(m)"), "r_lower_tri(m)")

  # getwd typo fixed (was getcd, never matched)
  expect_equal(conv("getwd()"), "pwd()")

  # diff -> r_diff. diff now fills defaults (fill_defaults = TRUE) so that NAMED
  # args map to the right slot of r_diff(x, lag, differences); the trade-off is
  # that the lag/differences defaults are emitted explicitly.
  expect_equal(conv("diff(x)"), "r_diff(x, 1.0, 1.0)")
  expect_equal(conv("diff(x, 2)"), "r_diff(x, 2.0, 1.0)") # lag = 2
  expect_equal(conv("diff(x, lag = 2)"), "r_diff(x, 2.0, 1.0)")
  # Regression: differences = 2 must set `differences`, not `lag`.
  expect_equal(conv("diff(x, differences = 2)"), "r_diff(x, 1.0, 2.0)")
  expect_equal(conv("diff(x, differences = 3, lag = 2)"), "r_diff(x, 2.0, 3.0)")
})


test_that("added builtin conversions to Julia (batch 3)", {
  sfm <- stockflow("predator_prey")
  var_names <- c(get_model_var(sfm), "m", "x", "y", "n", "k")
  name <- var_names[1]
  type <- "aux"
  conv <- function(s) convert_equations_julia(type, name, s, var_names)$eqn

  # rep -> r_rep; named each/length.out/times map to correct positions (fill path)
  expect_equal(conv("rep(x, 3)"), "r_rep(x, 3.0, -1.0, 1.0)")
  expect_equal(conv("rep(x, each=2)"), "r_rep(x, 1.0, -1.0, 2.0)")
  expect_equal(conv("rep(x, length.out=5)"), "r_rep(x, 1.0, 5.0, 1.0)")
  expect_equal(conv("rep(x, times=2, each=3)"), "r_rep(x, 2.0, -1.0, 3.0)")

  # Matrix margins -> vector-returning helpers
  expect_equal(conv("rowSums(m)"), "r_rowsums(m)")
  expect_equal(conv("colSums(m)"), "r_colsums(m)")
  expect_equal(conv("rowMeans(m)"), "r_rowmeans(m)")
  expect_equal(conv("colMeans(m)"), "r_colmeans(m)")

  # cumulative max/min (Base has cumsum/cumprod but not these)
  expect_equal(conv("cummax(x)"), "r_cummax(x)")
  expect_equal(conv("cummin(x)"), "r_cummin(x)")

  # Base Julia targets, broadcast
  expect_equal(conv("factorial(n)"), "factorial.(n)")
  expect_equal(conv("choose(n, k)"), "binomial.(n, k)")
  expect_equal(conv("trimws(x)"), "strip.(x)")
})


test_that("floor-division operator %/% maps to the Julia ⊘ operator", {
  sfm <- stockflow("predator_prey")
  var_names <- c(get_model_var(sfm), "a", "b")
  conv <- function(s) convert_equations_julia("aux", var_names[1], s, var_names)$eqn

  # Intermediate form carries the ⊘ escape (decoded to ⊘ when the .jl is written)
  expect_equal(conv("a %/% b"), "a \\u2298 b")
  expect_equal(decode_unicode(conv("a %/% b")), "a ⊘ b")

  # The slash inside %/% is not mistaken for the division operator
  expect_false(grepl("\\./", conv("a %/% b")))
})


test_that("sort_args lowercases injected logical defaults (TRUE/FALSE -> true/false)", {
  # Injected R defaults are capitalized ("FALSE"/"TRUE") because they come from
  # formals() *after* replace_op_julia() has already lowercased user-written
  # logicals. sort_args() must lowercase them so the emitted Julia is valid.

  # grep's ignore.case/perl/value/fixed/useBytes/invert all default to FALSE
  out <- sort_args(c("\"a\"", "x"), "grep",
    var_names = character(0), fill_defaults = TRUE
  )
  vals <- unname(unlist(out))
  expect_false(any(vals %in% c("TRUE", "FALSE"))) # no R-style capitals leak through
  expect_true("false" %in% vals)

  # pexp has lower.tail = TRUE (and log.p = FALSE) -> exercises both directions
  out2 <- sort_args("1.0", "pexp",
    var_names = character(0), fill_defaults = TRUE
  )
  vals2 <- unname(unlist(out2))
  expect_true("true" %in% vals2)
  expect_true("false" %in% vals2)
  expect_false(any(vals2 %in% c("TRUE", "FALSE")))

  # End-to-end: grep's filled defaults appear lowercased in the generated call
  sfm <- stockflow("predator_prey")
  var_names <- c(get_model_var(sfm), "x")
  out3 <- convert_equations_julia("aux", var_names[1], "grep(\"a\", x)", var_names)$eqn
  expect_false(grepl("FALSE|TRUE", out3))
  expect_match(out3, "false")
})


test_that("convert sequence works", {
  sfm <- stockflow("predator_prey")
  var_names <- get_model_var(sfm)
  name <- var_names[1]
  type <- "aux"

  result <- convert_equations_julia(type, name, "seq()", var_names)[["eqn"]]
  expected <- "range(1.0, 1.0, step=1.0)"
  expect_equal(result, expected)

  result <- convert_equations_julia(type, name, "seq(by = 1)", var_names)[["eqn"]]
  expected <- "range(1.0, 1.0, step=1.0)"
  expect_equal(result, expected)

  result <- convert_equations_julia(type, name, "seq(1, 10)", var_names)[["eqn"]]
  expected <- "range(1.0, 10.0, step=1.0)"
  expect_equal(result, expected)

  result <- convert_equations_julia(type, name, "seq(1, 10, 2)", var_names)[["eqn"]]
  expected <- "range(1.0, 10.0, step=2.0)"
  expect_equal(result, expected)

  result <- convert_equations_julia(type, name, "seq(1, 10, by=2)", var_names)[["eqn"]]
  expected <- "range(1.0, 10.0, step=2.0)"
  expect_equal(result, expected)

  result <- convert_equations_julia(type, name, "seq(1, 10, length.out=5)", var_names)[["eqn"]]
  expected <- "range(1.0, 10.0, round_(5.0))"
  expect_equal(result, expected)
})


test_that("convert sample works", {
  sfm <- stockflow("predator_prey")
  var_names <- get_model_var(sfm)
  name <- var_names[1]
  type <- "aux"

  result <- convert_equations_julia(type, name, "sample(1:10, 5)", var_names)[["eqn"]]
  expected <- "StatsBase.sample(1.0:10.0, round_(5.0), replace=false)"
  expect_equal(result, expected)

  result <- convert_equations_julia(type, name, "sample(1:10, 5, replace = TRUE)", var_names)[["eqn"]]
  expected <- "StatsBase.sample(1.0:10.0, round_(5.0), replace=true)"
  expect_equal(result, expected)

  result <- convert_equations_julia(type, name, "sample(1:10, 5, replace = FALSE)", var_names)[["eqn"]]
  expected <- "StatsBase.sample(1.0:10.0, round_(5.0), replace=false)"
  expect_equal(result, expected)

  result <- convert_equations_julia(
    type, name, "sample(1:10, 5, prob = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))",
    var_names
  )[["eqn"]]
  expected <- "StatsBase.sample(1.0:10.0, StatsBase.pweights([0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0]), round_(5.0), replace=false)"
  expect_equal(result, expected)
})


test_that("vector_to_square_brackets works", {
  var_names <- NULL
  result <- vector_to_square_brackets("c(1, 2, 3)", var_names)
  expected <- "[1, 2, 3]"
  expect_equal(result, expected)

  # Ensure that when c() is preceded by a letter, it is not converted
  result <- vector_to_square_brackets("ac(1, 2, 3)", var_names)
  expected <- "ac(1, 2, 3)"
  expect_equal(result, expected)

  result <- vector_to_square_brackets("c(1, 2, 3) + ac(4, 5, 6)", var_names)
  expected <- "[1, 2, 3] + ac(4, 5, 6)"
  expect_equal(result, expected)
})


test_that("replacing digits with floats works", {
  var_names <- NULL

  result <- replace_digits_with_floats("1", var_names)
  expected <- "1.0"
  expect_equal(result, expected)

  result <- replace_digits_with_floats("1000", var_names)
  expected <- "1000.0"
  expect_equal(result, expected)

  result <- replace_digits_with_floats("1:10", var_names)
  expected <- "1.0:10.0"
  expect_equal(result, expected)

  result <- replace_digits_with_floats("1.0:10.0", var_names)
  expected <- "1.0:10.0"
  expect_equal(result, expected)

  result <- replace_digits_with_floats("1/9 + (hello9 + hello10)", var_names)
  expected <- "1.0/9.0 + (hello9 + hello10)"
  expect_equal(result, expected)
})


test_that("removing scientific notation", {
  expect_equal(scientific_notation("1"), "1")
  expect_equal(scientific_notation(1), "1")
  expect_equal(scientific_notation("a + 1e+02"), "a + 100")
  expect_equal(scientific_notation(".1e+02"), "10")
  expect_equal(scientific_notation("e-2 + 1e-02"), "e-2 + 0.01")
  expect_equal(scientific_notation(" 1e-12"), " 0.000000000001")
  expect_equal(scientific_notation(c("1e+02", NA_character_)), c("100", NA_character_))
})


test_that("adding scientific notation", {
  expect_equal(scientific_notation("1", task = "add"), "1")
  expect_equal(scientific_notation("hiding 1e+23", task = "add"), "hiding 1e+23")
  expect_equal(scientific_notation("a + 1e+02"), "a + 100")
  expect_equal(scientific_notation("10000", task = "add", digits_max = 4), "1e+04")
  expect_equal(scientific_notation(" 1e-12"), " 0.000000000001")

  # Scientific notation already present will not be formatted correctly; and leading zeros will be preserved
  expect_equal(scientific_notation(".1e+02", task = "add"), ".1e+02")
})
