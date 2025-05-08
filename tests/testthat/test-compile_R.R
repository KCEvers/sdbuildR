
test_that("templates work", {

  sfm = xmile("SIR") %>% sim_specs(language = "R")
  expect_no_error(simulate(sfm))

  sfm = xmile("predator-prey") %>% sim_specs(language = "R")
  expect_no_error(simulate(sfm))

  sfm = xmile("logistic_model") %>% sim_specs(language = "R")
  expect_no_error(simulate(sfm))

  # Check whether the population converges to the carrying capacity
  sim = simulate(sfm)
  expect_equal(dplyr::last(sim$df$X), sim$pars$K, tolerance = .01)

  # No R simulations for models with units
  sfm = xmile("coffee_cup") %>% sim_specs(language = "R")
  expect_error(simulate(sfm), "Unit strings u\\(''\\) detected in model! Units are not supported for simulations in R")

  sfm = xmile("Crielaard2022") %>% sim_specs(language = "R")
  expect_no_error(simulate(sfm))

  # Duffing previously had an error with cos()
  sfm = xmile("Duffing") %>% sim_specs(language = "R")
  expect_no_error(simulate(sfm))

  sfm = xmile("Chua") %>% sim_specs(language = "R")
  expect_no_error(simulate(sfm))

})


