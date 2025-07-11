

test_that("compare output Julia and R for templates", {

  sfm = xmile("SIR")
  sim1 = simulate(sfm %>% sim_specs(language = "R"))
  sim2 = simulate(sfm %>% sim_specs(language = "Julia"))
  comp = compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)
  expect_equal(sim1$success, TRUE)
  expect_equal(nrow(sim1$df) > 0, TRUE)
  expect_equal(sim2$success, TRUE)
  expect_equal(nrow(sim2$df) > 0, TRUE)

  sim1 = simulate(sfm %>% sim_specs(language = "R"), only_stocks = TRUE)
  sim2 = simulate(sfm %>% sim_specs(language = "Julia"), only_stocks = TRUE)
  comp = compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)
  expect_equal(sim1$success, TRUE)
  expect_equal(nrow(sim1$df) > 0, TRUE)
  expect_equal(sim2$success, TRUE)
  expect_equal(nrow(sim2$df) > 0, TRUE)

  sfm = xmile("predator-prey")
  sim1 = simulate(sfm %>% sim_specs(language = "R"))
  sim2 = simulate(sfm %>% sim_specs(language = "Julia"))
  comp = compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)
  expect_equal(sim1$success, TRUE)
  expect_equal(nrow(sim1$df) > 0, TRUE)
  expect_equal(sim2$success, TRUE)
  expect_equal(nrow(sim2$df) > 0, TRUE)

  sim1 = simulate(sfm %>% sim_specs(language = "R"), only_stocks = TRUE)
  sim2 = simulate(sfm %>% sim_specs(language = "Julia"), only_stocks = TRUE)
  comp = compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)
  expect_equal(sim1$success, TRUE)
  expect_equal(nrow(sim1$df) > 0, TRUE)
  expect_equal(sim2$success, TRUE)
  expect_equal(nrow(sim2$df) > 0, TRUE)

  sfm = xmile("logistic_model")
  sim1 = simulate(sfm %>% sim_specs(language = "R"))
  sim2 = simulate(sfm %>% sim_specs(language = "Julia"))
  comp = compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)
  expect_equal(sim1$success, TRUE)
  expect_equal(nrow(sim1$df) > 0, TRUE)
  expect_equal(sim2$success, TRUE)
  expect_equal(nrow(sim2$df) > 0, TRUE)

  # Check whether the population converges to the carrying capacity
  expect_equal(dplyr::last(sim1$df[sim1$df$variable == "X", "value"]),
               sim1$constants[["K"]], tolerance = .01)
  expect_equal(dplyr::last(sim2$df[sim2$df$variable == "X", "value"]),
               sim2$constants[["K"]], tolerance = .01)

  sim1 = simulate(sfm %>% sim_specs(language = "R"), only_stocks = TRUE)
  sim2 = simulate(sfm %>% sim_specs(language = "Julia"), only_stocks = TRUE)
  comp = compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)
  expect_equal(sim1$success, TRUE)
  expect_equal(nrow(sim1$df) > 0, TRUE)
  expect_equal(sim2$success, TRUE)
  expect_equal(nrow(sim2$df) > 0, TRUE)

  sfm = xmile("Crielaard2022") %>%
    # Update initial condition to be non-stochastic
    build(c("Food_intake", "Hunger", "Compensatory_behaviour"), eqn = round(runif(3), 8))

  sim1 = simulate(sfm %>% sim_specs(language = "R"))
  sim2 = simulate(sfm %>% sim_specs(language = "Julia"))
  comp = compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)
  expect_equal(sim1$success, TRUE)
  expect_equal(nrow(sim1$df) > 0, TRUE)
  expect_equal(sim2$success, TRUE)
  expect_equal(nrow(sim2$df) > 0, TRUE)

  sim1 = simulate(sfm %>% sim_specs(language = "R"), only_stocks = TRUE)
  sim2 = simulate(sfm %>% sim_specs(language = "Julia"), only_stocks = TRUE)
  comp = compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)
  expect_equal(sim1$success, TRUE)
  expect_equal(nrow(sim1$df) > 0, TRUE)
  expect_equal(sim2$success, TRUE)
  expect_equal(nrow(sim2$df) > 0, TRUE)

  # Duffing previously had an error with cos()
  sfm = xmile("Duffing")
  sim1 = simulate(sfm %>% sim_specs(language = "R"))
  sim2 = simulate(sfm %>% sim_specs(language = "Julia"))
  comp = compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)
  expect_equal(sim1$success, TRUE)
  expect_equal(nrow(sim1$df) > 0, TRUE)
  expect_equal(sim2$success, TRUE)
  expect_equal(nrow(sim2$df) > 0, TRUE)

  sim1 = simulate(sfm %>% sim_specs(language = "R"), only_stocks = TRUE)
  sim2 = simulate(sfm %>% sim_specs(language = "Julia"), only_stocks = TRUE)
  comp = compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)
  expect_equal(sim1$success, TRUE)
  expect_equal(nrow(sim1$df) > 0, TRUE)
  expect_equal(sim2$success, TRUE)
  expect_equal(nrow(sim2$df) > 0, TRUE)

  sfm = xmile("Chua")
  sim1 = simulate(sfm %>% sim_specs(language = "R"))
  sim2 = simulate(sfm %>% sim_specs(language = "Julia"))
  comp = compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)
  expect_equal(sim1$success, TRUE)
  expect_equal(nrow(sim1$df) > 0, TRUE)
  expect_equal(sim2$success, TRUE)
  expect_equal(nrow(sim2$df) > 0, TRUE)

  sim1 = simulate(sfm %>% sim_specs(language = "R"), only_stocks = TRUE)
  sim2 = simulate(sfm %>% sim_specs(language = "Julia"), only_stocks = TRUE)
  comp = compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)
  expect_equal(sim1$success, TRUE)
  expect_equal(nrow(sim1$df) > 0, TRUE)
  expect_equal(sim2$success, TRUE)
  expect_equal(nrow(sim2$df) > 0, TRUE)

  # Check whether coffee cup reaches room temperature
  sfm = xmile("coffee_cup") %>% sim_specs(language = "Julia")
  sim1 = simulate(sfm)
  expect_equal(sim1$success, TRUE)
  expect_equal(nrow(sim1$df) > 0, TRUE)
  expect_equal(dplyr::last(sim1$df[sim1$df$variable == "coffee_temperature", "value"]), sim1$constants[["room_temperature"]], tolerance = .01)

  # Can't be simulated in R, already tested in compile_r

})



test_that("as.data.frame(sim) works", {

  sfm = xmile("SIR")
  sim = simulate(sfm %>% sim_specs(language = "Julia"))
  expect_equal(class(as.data.frame(sim)), "data.frame")
  expect_equal(nrow(as.data.frame(sim)) > 0, TRUE)

  sim = simulate(sfm %>% sim_specs(language = "R"))
  expect_equal(class(as.data.frame(sim)), "data.frame")
  expect_equal(nrow(as.data.frame(sim)) > 0, TRUE)

})
