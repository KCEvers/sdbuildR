test_that("compare output julia and r for templates", {

  sfm = xmile("SIR")
  sim1 = simulate(sfm %>% sim_specs(language = "R"))
  sim2 = simulate(sfm %>% sim_specs(language = "Julia"))
  comp = compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)

  sim1 = simulate(sfm %>% sim_specs(language = "R"), only_stocks = TRUE)
  sim2 = simulate(sfm %>% sim_specs(language = "Julia"), only_stocks = TRUE)
  comp = compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)

  sfm = xmile("predator-prey")
  sim1 = simulate(sfm %>% sim_specs(language = "R"))
  sim2 = simulate(sfm %>% sim_specs(language = "Julia"))
  comp = compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)

  sim1 = simulate(sfm %>% sim_specs(language = "R"), only_stocks = TRUE)
  sim2 = simulate(sfm %>% sim_specs(language = "Julia"), only_stocks = TRUE)
  comp = compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)

  sfm = xmile("logistic_model")
  sim1 = simulate(sfm %>% sim_specs(language = "R"))
  sim2 = simulate(sfm %>% sim_specs(language = "Julia"))
  comp = compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)

  sim1 = simulate(sfm %>% sim_specs(language = "R"), only_stocks = TRUE)
  sim2 = simulate(sfm %>% sim_specs(language = "Julia"), only_stocks = TRUE)
  comp = compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)

  sfm = xmile("Crielaard2022") %>%
    # Update initial condition to be non-stochastic
    build(c("Food_intake", "Hunger", "Compensatory_behaviour"), eqn = round(runif(3), 8))

  sim1 = simulate(sfm %>% sim_specs(language = "R"))
  sim2 = simulate(sfm %>% sim_specs(language = "Julia"))
  comp = compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)

  sim1 = simulate(sfm %>% sim_specs(language = "R"), only_stocks = TRUE)
  sim2 = simulate(sfm %>% sim_specs(language = "Julia"), only_stocks = TRUE)
  comp = compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)

  # Duffing previously had an error with cos()
  sfm = xmile("Duffing")
  sim1 = simulate(sfm %>% sim_specs(language = "R"))
  sim2 = simulate(sfm %>% sim_specs(language = "Julia"))
  comp = compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)

  sim1 = simulate(sfm %>% sim_specs(language = "R"), only_stocks = TRUE)
  sim2 = simulate(sfm %>% sim_specs(language = "Julia"), only_stocks = TRUE)
  comp = compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)

  sfm = xmile("Chua")
  sim1 = simulate(sfm %>% sim_specs(language = "R"))
  sim2 = simulate(sfm %>% sim_specs(language = "Julia"))
  comp = compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)

  sim1 = simulate(sfm %>% sim_specs(language = "R"), only_stocks = TRUE)
  sim2 = simulate(sfm %>% sim_specs(language = "Julia"), only_stocks = TRUE)
  comp = compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)
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
