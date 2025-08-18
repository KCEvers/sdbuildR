test_that("destructuring assignment", {
  result <- conv_destructuring_assignment("a, b <- {4, 5}")
  expected <- "c(a, b) %<-% {4, 5}"
  expect_equal(result, expected)
})



test_that("convert_builtin_functions_IM assignment", {
  sfm <- xmile("predator-prey")
  var_names <- get_model_var(sfm)
  regex_units <- get_regex_units()
  name <- "test"
  type <- "stock"

  eqn <- "0.1"
  result <- convert_builtin_functions_IM(type, name, eqn, var_names)$eqn
  expected <- "0.1"
  expect_equal(result, expected)

  # Insight Maker is case-insensitive
  eqn <- "MIN(3,4,5)"
  result <- convert_builtin_functions_IM(type, name, eqn, var_names)$eqn
  expected <- "min(c(3, 4, 5))"
  expect_equal(result, expected)


  eqn <- "min(3,4,5)"
  result <- convert_builtin_functions_IM(type, name, eqn, var_names)$eqn
  expected <- "min(c(3, 4, 5))"
  expect_equal(result, expected)

  # Nesting
  eqn <- "MIN(max(3,4,5), median(7,8,9))"
  result <- convert_builtin_functions_IM(type, name, eqn, var_names)$eqn
  expected <- "min(c(max(c(3, 4, 5)), median(c(7, 8, 9))))"
  expect_equal(result, expected)


  eqn <- "Rand + Rand"
  result <- convert_builtin_functions_IM(type, name, eqn, var_names)$eqn
  expected <- "runif(1) + runif(1)"
  expect_equal(result, expected)

  # Comments
  eqn <- "# A Test\nmin(3,4,5)\n# Another Test\nmax(3,4,5)"
  result <- remove_comments(eqn)
  expect_equal(result$eqn, "min(3,4,5)\n\nmax(3,4,5)")
  expect_equal(result$doc, "# A Test# Another Test")

  # result = convert_equations_IM(eqn,
  #                                 var_names,
  #                                 name, type, regex_units)
  # expect_equal(result[[type]][[name]]$eqn, "{\nmin(c(3, 4, 5))\nmax(c(3, 4, 5))\n}")
  # expect_equal(result[[type]][[name]]$doc, "# A Test# Another Test")


  # ** statements

  # eqn = "Max(.6, 8) + Min(PastValues([Population], 10), 100, c(70, 50)) + DelayN([Hares], 7, 5) + DelayN([Moose], [tau], 5) + 'Halo'.Length() + c(1, 2, 3, 4, 5).Sample(3, TRUE)"  #
  # convert_builtin_functions_IM("DelayN([Population], [tau], 10)", var_names, P, time_units, times, name)
  # convert_builtin_functions_IM("DelayN([Population] * 2, [tau], 10)", var_names, P, time_units, times, name)
})


# **
# eqn = "Repeat(x^2, 3)" #  {1, 4, 9}
# func = "repeat"
# arg = c("x^2", "3")
# eqn = "Repeat(x*10, 5)" # {10, 20, 30, 40, 50}
# func = "repeat"
# arg = c("x*10", "5")
# # Creates a vector {10, 20, 30, 40, 50} by multiplying index by 10
# eqn = "Repeat(2^x, 4)" # {2, 4, 8, 16}
# func = "repeat"
# arg = c("2^x", "4")
# # Generates a vector with powers of 2
# eqn = "Repeat('Group ' + key, {'a', 'b', 'c'})" # {a: 'Group a', b: 'Group b', c:'Group c'}
# func = "repeat"
# arg = c("'Group ' + key", "{'a', 'b', 'c'}")
