
test_that("converting equations to Julia", {

  sfm = xmile("predator-prey")
  var_names = get_model_var(sfm)
  regex_units = get_regex_units()
  name = var_names[1]
  type = "aux"

  result = convert_equations_julia(sfm, type, name, "min(predator_births)", var_names,
                                   regex_units, debug = FALSE)
  expected = "min(predator_births)"
  expect_equal(result$eqn_julia, expected)

  result = convert_equations_julia(sfm, type, name, "max(predator_births)", var_names,
                                   regex_units, debug = FALSE)
  expected = "max(predator_births)"
  expect_equal(result$eqn_julia, expected)

  result = convert_equations_julia(sfm, type, name, "c(0, predator_births, 1)", var_names,
                                   regex_units, debug = FALSE)
  expected = "[0.0, predator_births, 1.0]"
  expect_equal(result$eqn_julia, expected)

  result = convert_equations_julia(sfm, type, name, "range(predator_births, predator_deaths) * 10", var_names,
                                   regex_units, debug = FALSE)
  expected = "extrema(predator_births, predator_deaths) .* 10.0"
  expect_equal(result$eqn_julia, expected)

  result = convert_equations_julia(sfm, type, name, "test = function(a, b){
                                       a + b
                                       }", var_names,
                                   regex_units, debug = FALSE)
  expected = "function test(a, b) a .+ b end"
  expect_equal(stringr::str_squish(result$eqn_julia), expected)

  result = convert_equations_julia(sfm, type, name, "c(9 + 8 - 0, c('1 + 2 + 3'))", var_names,
                                   regex_units, debug = FALSE)
  expected = "[9.0 .+ 8.0 .- 0.0, [\"1 + 2 + 3\"]]"
  expect_equal(result$eqn_julia, expected)

  result = convert_equations_julia(sfm, type, name, "1E08", var_names,
                                   regex_units, debug = FALSE)
  expected = "100000000.0"
  expect_equal(result$eqn_julia, expected)

  result = convert_equations_julia(sfm, type, name, "c(T, TRUE, 'F', F+T, NULL, NA)", var_names,
                                   regex_units, debug = FALSE)
  expected = "[true, true, \"F\", false .+ true, nothing, missing]"
  expect_equal(result$eqn_julia, expected)

  result = convert_equations_julia(sfm, type, name, "for (i in 1:9){\n\tprint(i)\n}", var_names,
                                   regex_units, debug = FALSE)
  expected = "for  i in 1:9\n\tprintln(i)\nend"
  expect_equal(result$eqn_julia, expected)

  result = convert_equations_julia(sfm, type, name, "if(t<2020){\n\tgf<-0.07\n} else if (t<2025){\n\tgf<-0.03\n} else {\n\tgf<-0.02\n}", var_names,
                                   regex_units, debug = FALSE)
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
                                   regex_units, debug = FALSE)
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
                                   regex_units, debug = FALSE)
  expected = "min(predator_births)"
  expect_equal(result$eqn_julia, expected)

  result = convert_equations_julia(sfm, type, name, "min(x = max(y = predator_births))", var_names,
                                   regex_units, debug = FALSE)
  expected = "min(max(predator_births))"
  expect_equal(result$eqn_julia, expected)

  result = convert_equations_julia(sfm, type, name, "range(x = 10, y= 8)", var_names,
                                   regex_units, debug = FALSE)
  expected = "extrema(10.0, 8.0)"
  expect_equal(result$eqn_julia, expected)

  result = convert_equations_julia(sfm, type, name, "sigmoid(x, midpoint = 8)", var_names,
                                   regex_units, debug = FALSE)
  expected = "sigmoid.(x, 1, 8.0)"
  expect_equal(result$eqn_julia, expected)

  # Error for na.rm
  expect_error(xmile() %>% build("a", "stock", eqn = "sd(x = test, na.rm = T)"), "na\\.rm is not supported as an argument in sdbuildR. Please use na\\.omit\\(x\\) instead")

  # Check that wrong arguments throw error
  expect_error(convert_equations_julia(sfm, type, name, "sd(a, y = test)",
                                       var_names,
                                       regex_units, debug = FALSE), "Argument y is not allowed for function sd\\. Allowed arguments: x, na\\.rm")

  expect_error(convert_equations_julia(sfm, type, name, "rnorm(x = predator_births, mean = 0)",
                                       var_names,
                                       regex_units, debug = FALSE), "Argument x is not allowed for function rnorm. Allowed arguments: n, mean, sd")

  expect_error(convert_equations_julia(sfm, type, name, "rnorm(n = 1, x = predator_births, mean = 0)",
                                       var_names,
                                       regex_units, debug = FALSE), "Argument x is not allowed for function rnorm. Allowed arguments: n, mean, sd")


  # Check for missing obligatory arguments
  expect_error(convert_equations_julia(sfm, type, name, "rnorm(sd = predator_births, mean = 0)",
                                       var_names,
                                       regex_units, debug = FALSE), "Obligatory argument n is missing for function rnorm")


  # Check error for too many arguments
  expect_error(convert_equations_julia(sfm, type, name, "dnorm(1, 2, 3, log=FALSE, predator_deaths)",
                                       var_names,
                                       regex_units, debug = FALSE), "Too many arguments for function dnorm. Allowed arguments: x, mean, sd, log")


  # Error when not all default arguments are at the end
  expect_error(xmile() %>% macro("Function", "function(x, y = 1, z) x + y"), "Please change the function definition of Function. All arguments with defaults have to be placed at the end of the function arguments.")

  expect_error(xmile() %>% macro("Function", "function(x, y = 1, z){\nx + y\n}"), "Please change the function definition of Function. All arguments with defaults have to be placed at the end of the function arguments.")

  expect_error(xmile() %>% macro("Function", "function(x, y = 1, z, a = 1) x + y"), "Please change the function definition of Function. All arguments with defaults have to be placed at the end of the function arguments")
  expect_error(xmile() %>% macro("Function", "function(x, y = 1, z, a = 1){\nx + y\n}"), "Please change the function definition of Function. All arguments with defaults have to be placed at the end of the function arguments")

  expect_no_error(xmile() %>% macro("Function", "function(x, y = 1, a = 1){\nx + y\n}"))
  expect_no_error(xmile() %>% macro("Function", "function(x){\nx + y\n}"))
  expect_no_error(xmile() %>% macro("Function", "function(y = 1){\nx + y\n}"))

  sfm = xmile() %>% macro("Function", "function(x, y = 1, z = 2) x + y")
  expect_equal(sfm$macro$Function$eqn_julia, "function Function(x, y = 1.0, z = 2.0)\n x .+ y\nend")

  # sfm = sfm %>% build("a", "aux", eqn = "Function(1)")

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

  # **test unicode symbols like ohm and degree

  # **test ignore case



})


test_that("clean_unit_in_u() works", {

  regex_units = get_regex_units()

  result = clean_unit_in_u("u('10 Meters') + u('Kilograms per sec') + u('10 pounds squared')", regex_units)
  expected = "u('10m') + u('kg/s') + u('10lb^2')"
  expect_equal(result, expected)

  x = "u(\"3.86e26 Watts\") * ([Radius_of_planet] / [Distance_from_sun])^2 / 4\n\n# The sun's total radiation is 3.86×10^26 Watts.  From https://en.wikipedia.org/wiki/Solar_constant#The_Sun.27s_total_radiation\n# Incoming solar radiation = total radiation * (Shadow area of planet) / (Surface area of sphere at planet distance)\n# At Earth's distance, the incoming radiation density should be {1367 Watts / square meter}."
  result = clean_unit_in_u(x, regex_units)
  expected = "u(\"3.86e+26W\") * ([Radius_of_planet] / [Distance_from_sun])^2 / 4\n\n# The sun's total radiation is 3.86×10^26 Watts.  From https://en.wikipedia.org/wiki/Solar_constant#The_Sun.27s_total_radiation\n# Incoming solar radiation = total radiation * (Shadow area of planet) / (Surface area of sphere at planet distance)\n# At Earth's distance, the incoming radiation density should be {1367 Watts / square meter}."
  expect_equal(result, expected)

})


test_that("converting statements", {

  sfm = xmile("predator-prey")
  # names_df = get_names(sfm)
  var_names = get_model_var(sfm)

  eqn = "if(a > b){\n\t a + b\n} # test () {}"
  result = convert_all_statements_julia(eqn, var_names, debug = FALSE)
  expected = "if a > b \n\t a + b\nend # test () {}"
  expect_equal(result, expected)


  eqn = "if (a + min(c(1, 2)) < 0){\n\t print(a)\n} else {\n\t  print(b)\n}"
  result = convert_all_statements_julia(eqn, var_names, debug = FALSE)
  expected = "if  a + min(c(1, 2)) < 0 \n\t print(a)\n else \n\t  print(b)\nend"
  expect_equal(result, expected)


  eqn = "if (a + min(c(1, 2)) < 0){\n\t print(a)\n} else if (b + a == 1)  {\n\t  print(b)\n} else if (b + a == 1)  {\n\t  print(c)\n} else {\n\t  print('no')\n}"
  result = convert_all_statements_julia(eqn, var_names, debug = FALSE)
  expected = "if  a + min(c(1, 2)) < 0 \n\t print(a)\nelseif b + a == 1   \n\t  print(b)\nelseif b + a == 1   \n\t  print(c)\n else \n\t  print('no')\nend"
  expect_equal(result, expected)


  eqn = "# Description\na = function(c, b = 1) {\n\t return(c + b)\n}"
  result = convert_all_statements_julia(eqn, var_names, debug = FALSE)
  expected = "# Description\nfunction a(c, b = 1) \n\t return(c + b)\nend"
  expect_equal(result, expected)


  # # One-liner functions with brackets
  # # ** to do: this doesn't work if there is no name assigned...
  # # eqn = "sum_two_nums <- function(x, y) x + y"
  # # eqn = "sum_two_nums <- function(x, y = c('a', 'b')) x + y"
  # eqn = "function(x) x + 1"
  # result = convert_all_statements_julia(eqn, var_names, debug = FALSE)
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
  expected = "meter^2^3"
  expect_equal(result, expected)

})


test_that("convert_distribution() to Julia", {
  sfm = xmile("predator-prey")
  # names_df = get_names(sfm)
  var_names = get_model_var(sfm)
  name = var_names[1]
  type = "stock"

  # When n = 1, don't add n, otherwise this create a vector
  result = convert_builtin_functions_julia(type, name, "runif(1)", var_names, debug = F)$eqn
  expected = "rand(Distributions.Uniform(0, 1))"
  expect_equal(result, expected)

  result = convert_builtin_functions_julia(type, name, "runif(1, min =1, max=3)", var_names, debug = F)$eqn
  expected = "rand(Distributions.Uniform(1, 3))"
  expect_equal(result, expected)

  result = convert_builtin_functions_julia(type, name, "runif(10, min =-1, max=3)", var_names, debug = F)$eqn
  expected = "rand(Distributions.Uniform(-1, 3), 10)"
  expect_equal(result, expected)

  result = convert_builtin_functions_julia(type, name, "rnorm(1)", var_names, debug = F)$eqn
  expected = "rand(Distributions.Normal(0, 1))"
  expect_equal(result, expected)

  # Different order of arguments
  result = convert_builtin_functions_julia(type, name, "rnorm(10, sd =1, mean=3)", var_names, debug = F)$eqn
  expected = "rand(Distributions.Normal(3, 1), 10)"
  expect_equal(result, expected)

  result = convert_builtin_functions_julia(type, name, "rnorm(10, 1, 3)", var_names, debug = F)$eqn
  expected = "rand(Distributions.Normal(1, 3), 10)"
  expect_equal(result, expected)

  result = convert_builtin_functions_julia(type, name, "rexp(10, 3)", var_names, debug = F)$eqn
  expected = "rand(Distributions.Exponential(3), 10)"
  expect_equal(result, expected)

  result = convert_builtin_functions_julia(type, name, "rexp(1, rate=30)", var_names, debug = F)$eqn
  expected = "rand(Distributions.Exponential(30))"
  expect_equal(result, expected)


  # cdf, pdf, quantile
  result = convert_builtin_functions_julia(type, name, "pexp(1, rate=30)", var_names, debug = F)$eqn
  expected = "Distributions.cdf.(Distributions.Exponential(30), 1)"
  expect_equal(result, expected)

  result = convert_builtin_functions_julia(type, name, "qexp(1, rate=30)", var_names, debug = F)$eqn
  expected = "Distributions.quantile.(Distributions.Exponential(30), 1)"
  expect_equal(result, expected)

  result = convert_builtin_functions_julia(type, name, "dexp(1, rate=30)", var_names, debug = F)$eqn
  expected = "Distributions.pdf.(Distributions.Exponential(30), 1)"
  expect_equal(result, expected)

  result = convert_builtin_functions_julia(type, name, "pgamma(1, 2, rate=30)", var_names, debug = F)$eqn
  expected = "Distributions.cdf.(Distributions.Gamma(2, 30, 1/30), 1)"
  expect_equal(result, expected)

  result = convert_builtin_functions_julia(type, name, "qgamma(1, 2, rate=30)", var_names, debug = F)$eqn
  expected = "Distributions.quantile.(Distributions.Gamma(2, 30, 1/30), 1)"
  expect_equal(result, expected)

  result = convert_builtin_functions_julia(type, name, "dgamma(1, 2, rate=30)", var_names, debug = F)$eqn
  expected = "Distributions.pdf.(Distributions.Gamma(2, 30, 1/30), 1)"
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


test_that("functions in Julia work", {

  # round() with units
  sfm = xmile() %>% build("a", "stock", eqn = "round(10.235)")
  expect_no_error(simulate(sfm))

  sfm = xmile() %>% build("a", "stock", eqn = "round(u('100.80 kilograms'))")
  expect_no_error(simulate(sfm))

  sfm = xmile() %>% build("a", "stock", eqn = "round(u('108.67 seconds'))")
  expect_no_error(simulate(sfm))

  # # with digits argument ** to do: digits is a keyword argument in Julia and cannot be a float!
  # sfm = xmile() %>% build("a", "stock", eqn = "round(10.235, digits = 1)")
  # expect_no_error(simulate(sfm))

  # Cosine function needs unitless argument or argument in radians
  sfm = xmile() %>% build("a", "stock", eqn = "cos(10)")
  expect_no_error(simulate(sfm))

  sfm = xmile() %>% build("a", "stock", eqn = "cos(u('10meters'))")
  expect_warning(simulate(sfm), "An error occurred while running the Julia script")

  sfm = xmile() %>% build("a", "stock", eqn = "cos(u('10radians'))")
  expect_no_error(simulate(sfm))

})

