
test_that("converting equations to Julia", {

  sfm = xmile("predator-prey")
  var_names = get_model_var(sfm)
  regex_units = get_regex_units()
  name = var_names[1]
  type = "aux"

  result = convert_equations_julia(sfm, type, name, "min(predator_births)", var_names,
                                   regex_units)
  expected = "min(predator_births)"
  expect_equal(result$eqn_julia, expected)

  result = convert_equations_julia(sfm, type, name, "max(predator_births)", var_names,
                                   regex_units)
  expected = "max(predator_births)"
  expect_equal(result$eqn_julia, expected)

  result = convert_equations_julia(sfm, type, name, "c(0, predator_births, 1)", var_names,
                                   regex_units)
  expected = "[0.0, predator_births, 1.0]"
  expect_equal(result$eqn_julia, expected)

  result = convert_equations_julia(sfm, type, name, "range(predator_births, predator_deaths) * 10", var_names,
                                   regex_units)
  expected = "extrema(predator_births, predator_deaths) .* 10.0"
  expect_equal(result$eqn_julia, expected)

  result = convert_equations_julia(sfm, type, name, "test = function(a, b){
                                       a + b
                                       }", var_names,
                                   regex_units)
  expected = "function test(a, b) a .+ b end"
  expect_equal(stringr::str_squish(result$eqn_julia), expected)

  result = convert_equations_julia(sfm, type, name, "c(9 + 8 - 0, c('1 + 2 + 3'))", var_names,
                                   regex_units)
  expected = "[9.0 .+ 8.0 .- 0.0, [\"1 + 2 + 3\"]]"
  expect_equal(result$eqn_julia, expected)

  result = convert_equations_julia(sfm, type, name, "1E08", var_names,
                                   regex_units)
  expected = "100000000.0"
  expect_equal(result$eqn_julia, expected)

  result = convert_equations_julia(sfm, type, name, "c(T, TRUE, 'F', F+T, NULL, NA)", var_names,
                                   regex_units)
  expected = "[true, true, \"F\", false .+ true, nothing, missing]"
  expect_equal(result$eqn_julia, expected)

  result = convert_equations_julia(sfm, type, name, "for (i in 1:9){\n\tprint(i)\n}", var_names,
                                   regex_units)
  expected = "for  i in 1.0:9.0\n\tprintln(i)\nend"
  expect_equal(result$eqn_julia, expected)

  result = convert_equations_julia(sfm, type, name, "if(t<2020){\n\tgf<-0.07\n} else if (t<2025){\n\tgf<-0.03\n} else {\n\tgf<-0.02\n}", var_names,
                                   regex_units)
  expected = "if t .< 2020.0
	gf = 0.07
elseif t .< 2025.0
	gf = 0.03
 else
	gf = 0.02
end"
  expect_equal(result$eqn_julia, expected)

  # while
  result = convert_equations_julia(sfm, type, name, "while(a < 0){\n\tif (prey >0){\n\t\ta <- 0\n\t} else {\n\ta = 1\n\t}\n}", var_names,
                                   regex_units)
  expected = "while a .< 0.0\n\tif prey .> 0.0\n\t\ta = 0.0\n\t else\n\ta = 1.0\n\tend\nend"
  expect_equal(result$eqn_julia, expected)

  # oneliner if ***


})



test_that("converting functions to Julia with named arguments", {

  sfm = xmile("predator-prey")
  var_names = get_model_var(sfm)
  regex_units = get_regex_units()
  name = var_names[1]
  type = "aux"

  # Check that functions without named arguments (e.g. min) have their names stripped
  result = convert_equations_julia(sfm, type, name, "min(x = predator_births)", var_names,
                                   regex_units)
  expected = "min(predator_births)"
  expect_equal(result$eqn_julia, expected)

  result = convert_equations_julia(sfm, type, name, "min(x = max(y = predator_births))", var_names,
                                   regex_units)
  expected = "min(max(predator_births))"
  expect_equal(result$eqn_julia, expected)

  result = convert_equations_julia(sfm, type, name, "range(x = 10, y= 8)", var_names,
                                   regex_units)
  expected = "extrema(10.0, 8.0)"
  expect_equal(result$eqn_julia, expected)

  result = convert_equations_julia(sfm, type, name, "logistic(x, midpoint = 8)", var_names,
                                   regex_units)
  expected = "logistic.(x, 1.0, 8.0, 1.0)"
  expect_equal(result$eqn_julia, expected)

  result = convert_equations_julia(sfm, type, name, "logistic(x, upper = 8)", var_names,
                                   regex_units)
  expected = "logistic.(x, 1.0, 0.0, 8.0)"
  expect_equal(result$eqn_julia, expected)


  # Error for na.rm
  expect_error(xmile() |> build("a", "stock", eqn = "sd(x = test, na.rm = T)"), "na\\.rm is not supported as an argument in sdbuildR. Please use na\\.omit\\(x\\) instead")

  # Check that wrong arguments throw error
  expect_error(convert_equations_julia(sfm, type, name, "sd(a, y = test)",
                                       var_names,
                                       regex_units), "Argument y is not allowed for function sd\\. Allowed arguments: x, na\\.rm")

  expect_error(convert_equations_julia(sfm, type, name, "rnorm(x = predator_births, mean = 0)",
                                       var_names,
                                       regex_units), "Argument x is not allowed for function rnorm. Allowed arguments: n, mean, sd")

  expect_error(convert_equations_julia(sfm, type, name, "rnorm(n = 1, x = predator_births, mean = 0)",
                                       var_names,
                                       regex_units), "Argument x is not allowed for function rnorm. Allowed arguments: n, mean, sd")


  # Check for missing obligatory arguments
  expect_error(convert_equations_julia(sfm, type, name, "rnorm(sd = predator_births, mean = 0)",
                                       var_names,
                                       regex_units), "Obligatory argument n is missing for function rnorm")


  # Check error for too many arguments
  expect_error(convert_equations_julia(sfm, type, name, "dnorm(1, 2, 3, log=FALSE, predator_deaths)",
                                       var_names,
                                       regex_units), "Too many arguments for function dnorm. Allowed arguments: x, mean, sd, log")


  # Error when not all default arguments are at the end
  expect_error(xmile() |> macro("Function", "function(x, y = 1, z) x + y"), "Please change the function definition of Function. All arguments with defaults have to be placed at the end of the function arguments.")

  expect_error(xmile() |> macro("Function", "function(x, y = 1, z){\nx + y\n}"), "Please change the function definition of Function. All arguments with defaults have to be placed at the end of the function arguments.")

  expect_error(xmile() |> macro("Function", "function(x, y = 1, z, a = 1) x + y"), "Please change the function definition of Function. All arguments with defaults have to be placed at the end of the function arguments")

  expect_error(xmile() |> macro("Function", "function(x, y = 1, z, a = 1){\nx + y\n}"), "Please change the function definition of Function. All arguments with defaults have to be placed at the end of the function arguments")

  expect_no_error(xmile() |> macro("Function", "function(x, y = 1, a = 1){\nx + y\n}"))
  expect_no_error(xmile() |> macro("Function", "function(x){\nx + y\n}"))
  expect_no_error(xmile() |> macro("Function", "function(y = 1){\nx + y\n}"))
})


test_that("custom function definitons work", {

  sfm = xmile() |> macro("func", "function(x, y = 1, z = 2) x + y")
  expect_equal(sfm$macro$func$eqn_julia, "function func(x, y = 1.0, z = 2.0)\n x .+ y\nend")

  # Is the function now usable?
  sfm = sfm |> sim_specs(language="R", stop = 1, dt = .1) |>
    build("a", "stock", eqn = "func(1, 2)")
  sim = expect_no_error(simulate(sfm))
  expect_equal(sim$df[1, "value"], 1 + 2)

  # Name argument
  sfm = sfm |> build("a", eqn = "func(1, y = 2)")
  expect_equal(sfm$model$variables$stock$a$eqn_julia, "func(1.0, y = 2.0)")
  sim = expect_no_error(simulate(sfm))
  expect_equal(sim$df[1, "value"], 1 + 2)

  # Switch order of arguments
  sfm = sfm |> build("a", eqn = "func(1, z = 3, y = 2)")
  expect_equal(sfm$model$variables$stock$a$eqn_julia, "func(1.0, z = 3.0, y = 2.0)")
  sim = expect_no_error(simulate(sfm))
  expect_equal(sim$df[1, "value"], 1 + 2)

  # # Named argument throws error when translating to Julia
  # sfm = sfm |> build("a", eqn = "func(1, y = 2)")
  # expect_equal(sfm$model$variables$stock$a$eqn_julia, "func(1.0, y = 2.0)")
  # expect_error(simulate(sfm), "The following variables were used as functions with named arguments in the Julia translated equation")
  #
  # # Switch order of arguments
  # sfm = sfm |>
  #   build("a",eqn = "func(1, z = 3, y = 2)")
  # expect_equal(sfm$model$variables$stock$a$eqn_julia, "func(1.0, z = 3.0, y = 2.0)")
  # expect_error(simulate(sfm), "The following variables were used as functions with named arguments in the Julia translated equation")

  # Repeat in Julia
  testthat::skip_on_cran()
  sfm = xmile() |> macro("func", "function(x, y = 1, z = 2) x + y")
  expect_equal(sfm$macro$func$eqn_julia, "function func(x, y = 1.0, z = 2.0)\n x .+ y\nend")

  # Is the function now usable?
  sfm = sfm |> sim_specs(language="Julia", stop = 1, dt = .1) |>
    build("a", "stock", eqn = "func(1, 2)")
  sim = expect_no_error(simulate(sfm))
  expect_equal(sim$df[1, "value"], 1 + 2)

})



test_that("clean units for Julia", {

  regex_units = get_regex_units()

  x = "meter"
  result = clean_unit(x, regex_units)
  expected = "m"
  expect_equal(result, expected)

  x = "meter squared"
  result = clean_unit(x, regex_units)
  expected = "m^2"
  expect_equal(result, expected)

  x = "cubic meter"
  result = clean_unit(x, regex_units)
  expected = "m^3"
  expect_equal(result, expected)

  x = "cubic"
  result = clean_unit(x, regex_units)
  expected = "cubic"
  expect_equal(result, expected)

  x = "meter per 100 sec"
  result = clean_unit(x, regex_units)
  expected = "m/100s"
  expect_equal(result, expected)

  x = "HOUR"
  result = clean_unit(x, regex_units)
  expected = "HOUR"
  expect_equal(result, expected)

  x = "3 feet / minute"
  result = clean_unit(x, regex_units)
  expected = "3ft/minute"
  expect_equal(result, expected)

  # Scientific notation
  x = "3e+02 watts per hour"
  result = clean_unit(x, regex_units)
  expected = "300W/hr"
  expect_equal(result, expected)

  # Special characters except - removed
  x = "my-new-unit  /  my!other!unit"
  result = clean_unit(x, regex_units)
  expected = "my-new-unit/my_other_unit"
  expect_equal(result, expected)

  # Don't remove phrases
  x = "Kilograms Meters per Second"
  result = clean_unit(x, regex_units)
  expected = "KilogramsMeters/s"
  expect_equal(result, expected)

  # Check whether it works with numbers
  x = " 10 Kilograms Meters per Second "
  result = clean_unit(x, regex_units)
  expected = "10KilogramsMeters/s"
  expect_equal(result, expected)

  # Check whether it works with numbers
  x = ".1 meters"
  result = clean_unit(x, regex_units)
  expected = ".1m"
  expect_equal(result, expected)

  # Check whether it works with numbers
  x = "0.8 meters"
  result = clean_unit(x, regex_units)
  expected = "0.8m"
  expect_equal(result, expected)

  # Different plurals; leading zeros are preserved
  x = "08 inches"
  result = clean_unit(x, regex_units)
  expected = "08inch"
  expect_equal(result, expected)

  x = "180 foot"
  result = clean_unit(x, regex_units)
  expected = "180ft"
  expect_equal(result, expected)

  # Prefixes
  x = "0.8 Kilometers"
  result = clean_unit(x, regex_units)
  expected = "0.8km"
  expect_equal(result, expected)

  x = "10 millimeters per millisecond"
  result = clean_unit(x, regex_units)
  expected = "10mm/ms"
  expect_equal(result, expected)

  # Units that shouldn't have prefixes shouldn't be translated
  x = "0.8 kiloinch"
  result = clean_unit(x, regex_units)
  expected = "0.8kiloinch"
  expect_equal(result, expected)

  # Units with special characters should be replaced
  x = "CO^2"
  result = clean_unit(x, regex_units, unit_name = TRUE)
  expected = "CO_2"
  expect_equal(result, expected)

  x = "C02"
  result = clean_unit(x, regex_units, unit_name = TRUE)
  expected = "C02"
  expect_equal(result, expected)

  x = "CO2"
  result = clean_unit(x, regex_units, unit_name = TRUE)
  expected = "CO2"
  expect_equal(result, expected)

  x = "a+b"
  result = clean_unit(x, regex_units, unit_name = TRUE)
  expected = "a_b"
  expect_equal(result, expected)

  x = "my-unit"
  result = clean_unit(x, regex_units, unit_name = TRUE)
  expected = "my_unit"
  expect_equal(result, expected)

  x = "a/b"
  result = clean_unit(x, regex_units, unit_name = TRUE)
  expected = "a_b"
  expect_equal(result, expected)

  x = "S&P"
  result = clean_unit(x, regex_units)
  expected = "S_P"
  expect_equal(result, expected)

  x = "10 CO^2"
  result = clean_unit(x, regex_units)
  expected = "10CO^2"
  expect_equal(result, expected)

  x = "0.0000000567 Watts/(Meters^2 * Degrees Kelvin^4)"
  result = clean_unit(x, regex_units)
  expected = "0.0000000567W/(m^2*K^4)"
  expect_equal(result, expected)

  x = "0.0000000567 Watts / square meter / Degrees Celsius^4"
  result = clean_unit(x, regex_units)
  expected = "0.0000000567W/m^2/degC^4"
  expect_equal(result, expected)

  x = "273 Degrees Celsius"
  result = clean_unit(x, regex_units)
  expected = "273degC"
  expect_equal(result, expected)

  x = "386000000000000013920400480 Watts"
  result = clean_unit(x, regex_units)
  expected = "3.86e+26W"
  expect_equal(result, expected)

  x = "1004 Joules / kilograms / Degrees Celsius"
  result = clean_unit(x, regex_units)
  expected = "1004J/kg/degC"
  expect_equal(result, expected)

  x = "4180 Joules / kg / Degrees Celsius"
  result = clean_unit(x, regex_units)
  expected = "4180J/kg/degC"
  expect_equal(result, expected)

  x = "1000 kg / cubic meter"
  result = clean_unit(x, regex_units)
  expected = "1000kg/m^3"
  expect_equal(result, expected)

  # **test unicode symbols like ohm and degree

  # **test ignore case

})


test_that("clean_unit_in_u() works", {

  regex_units = get_regex_units()

  result = clean_unit_in_u("u('10 Meters') + u('Kilograms per sec') + u('10 pounds squared')", regex_units)
  expected = "u(\"10m\") + u(\"kg/s\") + u(\"10lb^2\")"
  expect_equal(result, expected)

  # Complex equation from Insight Maker
  x = "u(\"3.86e26 Watts\") * ([Radius_of_planet] / [Distance_from_sun])^2 / 4\n\n# The sun's total radiation is 3.86×10^26 Watts.  From https://en.wikipedia.org/wiki/Solar_constant#The_Sun.27s_total_radiation\n# Incoming solar radiation = total radiation * (Shadow area of planet) / (Surface area of sphere at planet distance)\n# At Earth's distance, the incoming radiation density should be {1367 Watts / square meter}."
  result = clean_unit_in_u(x, regex_units)
  expected = "u(\"3.86e+26W\") * ([Radius_of_planet] / [Distance_from_sun])^2 / 4\n\n# The sun's total radiation is 3.86×10^26 Watts.  From https://en.wikipedia.org/wiki/Solar_constant#The_Sun.27s_total_radiation\n# Incoming solar radiation = total radiation * (Shadow area of planet) / (Surface area of sphere at planet distance)\n# At Earth's distance, the incoming radiation density should be {1367 Watts / square meter}."
  expect_equal(result, expected)

  # Check whether repeating units are all replaced
  x = "u('10 Meters squared per second') - u('10lb^2') + u('10 Meters squared per second') * + u('10lb^2')"
  result = clean_unit_in_u(x, regex_units)
  expected = "u(\"10m^2/s\") - u(\"10lb^2\") + u(\"10m^2/s\") * + u(\"10lb^2\")"
  expect_equal(result, expected)

  # Nested unit string throws error
  x = "u('10 Meters squared per second + u('2 meters')')"
  expect_error(clean_unit_in_u(x, regex_units),
               "Nested units u\\(' u\\(''\\) '\\) are not allowed")

})


test_that("converting statements", {

  sfm = xmile("predator-prey")
  var_names = get_model_var(sfm)

  eqn = "if(a > b){\n\t a + b\n} # test () {}"
  result = convert_all_statements_julia(eqn, var_names)
  expected = "if a > b \n\t a + b\nend # test () {}"
  expect_equal(result, expected)

  eqn = "if (a + min(c(1, 2)) < 0){\n\t print(a)\n} else {\n\t  print(b)\n}"
  result = convert_all_statements_julia(eqn, var_names)
  expected = "if  a + min(c(1, 2)) < 0 \n\t print(a)\n else \n\t  print(b)\nend"
  expect_equal(result, expected)


  eqn = "if (a + min(c(1, 2)) < 0){\n\t print(a)\n} else if (b + a == 1)  {\n\t  print(b)\n} else if (b + a == 1)  {\n\t  print(c)\n} else {\n\t  print('no')\n}"
  result = convert_all_statements_julia(eqn, var_names)
  expected = "if  a + min(c(1, 2)) < 0 \n\t print(a)\nelseif b + a == 1   \n\t  print(b)\nelseif b + a == 1   \n\t  print(c)\n else \n\t  print('no')\nend"
  expect_equal(result, expected)


  eqn = "# Description\na = function(c, b = 1) {\n\t return(c + b)\n}"
  result = convert_all_statements_julia(eqn, var_names)
  expected = "# Description\nfunction a(c, b = 1) \n\t return(c + b)\nend"
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



test_that("replace_written_powers() works", {

  result = replace_written_powers("cubic meter")
  expected = "meter^3"
  expect_equal(result, expected)

  result = replace_written_powers("100 Meters squared")
  expected = "100 Meters^2"
  expect_equal(result, expected)

  result = replace_written_powers("squared cubic meter")
  expected = "meter^3^2"
  expect_equal(result, expected)

})


test_that("convert_distribution() to Julia", {
  sfm = xmile("predator-prey")
  # names_df = get_names(sfm)
  var_names = get_model_var(sfm)
  name = var_names[1]
  type = "stock"

  # When n = 1, don't add n, otherwise this create a vector
  result = convert_builtin_functions_julia(type, name, "runif(1)", var_names)$eqn
  expected = "rand(Distributions.Uniform(0.0, 1.0))"
  expect_equal(result, expected)

  result = convert_builtin_functions_julia(type, name, "runif(1, min =1, max=3)", var_names)$eqn
  expected = "rand(Distributions.Uniform(1.0, 3.0))"
  expect_equal(result, expected)

  result = convert_builtin_functions_julia(type, name, "runif(10, min =-1, max=3)", var_names)$eqn
  expected = "rand(Distributions.Uniform(-1.0, 3.0), 10)"
  expect_equal(result, expected)

  result = convert_builtin_functions_julia(type, name, "rnorm(1)", var_names)$eqn
  expected = "rand(Distributions.Normal(0.0, 1.0))"
  expect_equal(result, expected)

  # Different order of arguments
  result = convert_builtin_functions_julia(type, name, "rnorm(10, sd =1, mean=3)", var_names)$eqn
  expected = "rand(Distributions.Normal(3.0, 1.0), 10)"
  expect_equal(result, expected)

  result = convert_builtin_functions_julia(type, name, "rnorm(10, 1, 3)", var_names)$eqn
  expected = "rand(Distributions.Normal(1.0, 3.0), 10)"
  expect_equal(result, expected)

  result = convert_builtin_functions_julia(type, name, "rexp(10, 3)", var_names)$eqn
  expected = "rand(Distributions.Exponential(3.0), 10)"
  expect_equal(result, expected)

  result = convert_builtin_functions_julia(type, name, "rexp(1, rate=30)", var_names)$eqn
  expected = "rand(Distributions.Exponential(30.0))"
  expect_equal(result, expected)


  # cdf, pdf, quantile
  result = convert_builtin_functions_julia(type, name, "pexp(1, rate=30)", var_names)$eqn
  expected = "Distributions.cdf.(Distributions.Exponential(30.0), 1)"
  expect_equal(result, expected)

  result = convert_builtin_functions_julia(type, name, "qexp(1, rate=30)", var_names)$eqn
  expected = "Distributions.quantile.(Distributions.Exponential(30.0), 1)"
  expect_equal(result, expected)

  result = convert_builtin_functions_julia(type, name, "dexp(1, rate=30)", var_names)$eqn
  expected = "Distributions.pdf.(Distributions.Exponential(30.0), 1)"
  expect_equal(result, expected)

  result = convert_builtin_functions_julia(type, name, "pgamma(1, 2, rate=30)", var_names)$eqn
  expected = "Distributions.cdf.(Distributions.Gamma(2.0, 30.0, 1.0/30.0), 1)"
  expect_equal(result, expected)

  result = convert_builtin_functions_julia(type, name, "qgamma(1, 2, rate=30)", var_names)$eqn
  expected = "Distributions.quantile.(Distributions.Gamma(2.0, 30.0, 1.0/30.0), 1)"
  expect_equal(result, expected)

  result = convert_builtin_functions_julia(type, name, "dgamma(1, 2, rate=30)", var_names)$eqn
  expected = "Distributions.pdf.(Distributions.Gamma(2.0, 30.0, 1.0/30.0), 1)"
  expect_equal(result, expected)


})



test_that("convert sequence works", {

  sfm = xmile("predator-prey")
  var_names = get_model_var(sfm)
  name = var_names[1]
  type = "aux"

  result = convert_equations_julia(sfm, type, name, "seq()", var_names)[["eqn_julia"]]
  expected = "range(1.0, 1.0, step=1.0)"
  expect_equal(result, expected)

  result = convert_equations_julia(sfm, type, name, "seq(by = 1)", var_names)[["eqn_julia"]]
  expected = "range(1.0, 1.0, step=1.0)"
  expect_equal(result, expected)

  result = convert_equations_julia(sfm, type, name, "seq(1, 10)", var_names)[["eqn_julia"]]
  expected = "range(1.0, 10.0, step=1.0)"
  expect_equal(result, expected)

  result = convert_equations_julia(sfm, type, name, "seq(1, 10, 2)", var_names)[["eqn_julia"]]
  expected = "range(1.0, 10.0, step=2.0)"
  expect_equal(result, expected)

  result = convert_equations_julia(sfm, type, name, "seq(1, 10, by=2)", var_names)[["eqn_julia"]]
  expected = "range(1.0, 10.0, step=2.0)"
  expect_equal(result, expected)

  result = convert_equations_julia(sfm, type, name, "seq(1, 10, length.out=5)", var_names)[["eqn_julia"]]
  expected = "range(1.0, 10.0, round_(5.0))"
  expect_equal(result, expected)

})


test_that("convert sample works", {

  sfm = xmile("predator-prey")
  var_names = get_model_var(sfm)
  name = var_names[1]
  type = "aux"

  result = convert_equations_julia(sfm, type, name, "sample(1:10, 5)", var_names)[["eqn_julia"]]
  expected = "StatsBase.sample(1.0:10.0, round_(5.0), replace=false)"
  expect_equal(result, expected)

  result = convert_equations_julia(sfm, type, name, "sample(1:10, 5, replace = TRUE)", var_names)[["eqn_julia"]]
  expected = "StatsBase.sample(1.0:10.0, round_(5.0), replace=true)"
  expect_equal(result, expected)

  result = convert_equations_julia(sfm, type, name, "sample(1:10, 5, replace = FALSE)", var_names)[["eqn_julia"]]
  expected = "StatsBase.sample(1.0:10.0, round_(5.0), replace=false)"
  expect_equal(result, expected)

  result = convert_equations_julia(sfm, type, name, "sample(1:10, 5, prob = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))",
                                   var_names)[["eqn_julia"]]
  expected = "StatsBase.sample(1.0:10.0, StatsBase.pweights([0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0]), round_(5.0), replace=false)"
  expect_equal(result, expected)


})


test_that("vector_to_square_brackets works", {

  var_names = NULL
  result = vector_to_square_brackets("c(1, 2, 3)", var_names)
  expected = "[1, 2, 3]"
  expect_equal(result, expected)

  # Ensure that when c() is preceded by a letter, it is not converted
  result = vector_to_square_brackets("ac(1, 2, 3)", var_names)
  expected = "ac(1, 2, 3)"
  expect_equal(result, expected)

  result = vector_to_square_brackets("c(1, 2, 3) + ac(4, 5, 6)", var_names)
  expected = "[1, 2, 3] + ac(4, 5, 6)"
  expect_equal(result, expected)

})


test_that("replacing digits with floats works", {
  var_names = NULL

  result = replace_digits_with_floats("1", var_names)
  expected = "1.0"
  expect_equal(result, expected)

  result = replace_digits_with_floats("1000", var_names)
  expected = "1000.0"
  expect_equal(result, expected)

  result = replace_digits_with_floats("1:10", var_names)
  expected = "1.0:10.0"
  expect_equal(result, expected)

  result = replace_digits_with_floats("1.0:10.0", var_names)
  expected = "1.0:10.0"
  expect_equal(result, expected)

  result = replace_digits_with_floats("1/9 + (hello9 + hello10)", var_names)
  expected = "1.0/9.0 + (hello9 + hello10)"
  expect_equal(result, expected)

})


test_that("removing scientific notation", {
  expect_equal(scientific_notation("1"), "1")
  expect_equal(scientific_notation(1), "1")
  expect_equal(scientific_notation("a + 1e+02"), "a + 100")
  expect_equal(scientific_notation(".1e+02"), "10")
  expect_equal(scientific_notation("e-2 + 1e-02"), "e-2 + 0.01")
  expect_equal(scientific_notation(" 1e-12"), " 0.000000000001")
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


