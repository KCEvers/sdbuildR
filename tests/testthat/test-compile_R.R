
test_that("templates work", {

  sfm = xmile("SIR") %>% sim_specs(language = "R")
  expect_no_error(simulate(sfm))
  sim = simulate(sfm)
  expect_equal(sim$success, TRUE)
  expect_equal(nrow(sim$df) > 0, TRUE)

  sfm = xmile("predator-prey") %>% sim_specs(language = "R")
  expect_no_error(simulate(sfm))
  sim = simulate(sfm)
  expect_equal(sim$success, TRUE)
  expect_equal(nrow(sim$df) > 0, TRUE)

  sfm = xmile("logistic_model") %>% sim_specs(language = "R")
  expect_no_error(simulate(sfm))
  sim = simulate(sfm)
  expect_equal(sim$success, TRUE)
  expect_equal(nrow(sim$df) > 0, TRUE)

  # Check whether the population converges to the carrying capacity
  sim = simulate(sfm)
  expect_equal(dplyr::last(sim$df$X), sim$pars$K, tolerance = .01)

  sfm = xmile("Crielaard2022") %>% sim_specs(language = "R")
  expect_no_error(simulate(sfm))
  sim = simulate(sfm)
  expect_equal(sim$success, TRUE)
  expect_equal(nrow(sim$df) > 0, TRUE)

  # Duffing previously had an error with cos()
  sfm = xmile("Duffing") %>% sim_specs(language = "R")
  expect_no_error(simulate(sfm))
  sim = simulate(sfm)
  expect_equal(sim$success, TRUE)
  expect_equal(nrow(sim$df) > 0, TRUE)

  sfm = xmile("Chua") %>% sim_specs(language = "R")
  expect_no_error(simulate(sfm))
  sim = simulate(sfm)
  expect_equal(sim$success, TRUE)
  expect_equal(nrow(sim$df) > 0, TRUE)
})





test_that("simulate with different components works", {

  # Without stocks throws error
  sfm = xmile() %>% sim_specs(language = "R")
  expect_error(simulate(sfm), "Your model has no stocks.")

  sfm = xmile() %>% sim_specs(language = "R") %>%
    build("a", "stock") %>%
    build("b", "flow")
  expect_error(simulate(sfm), "These flows are not connected to any stock:\\n- b")

  # With one stock and no flows and no parameters
  sfm = xmile() %>% sim_specs(language = "R")  %>%
    sim_specs(start = 0, stop = 10, dt = 0.1) %>%
    build("A", "stock", eqn = "100")
  expect_no_error(simulate(sfm))
  sim = simulate(sfm)
  expect_equal(sort(names(sim$df)), c("A", "time"))

  # One stock with flows, other stock without flows
  sfm = xmile()%>% sim_specs(language = "R") %>%
    sim_specs(start = 0, stop = 10, dt = 0.1) %>%
    build(c("A", "B"), "stock", eqn = "100") %>%
    build("C", "flow", eqn = "1", to = "A")
  expect_no_error(simulate(sfm))
  sim = simulate(sfm)
  expect_equal(sort(names(sim$df)), c("A", "B", "C", "time"))

  # With one intermediary -> error in constructing Dataframe before in Julia
  sfm = xmile()%>% sim_specs(language = "R") %>%
    sim_specs(start = 0, stop = 10, dt = 0.1) %>%
    build("A", "stock", eqn = "100") %>%
    build("B", "flow", eqn = "1", to = "A") %>%
    build("C", "aux", eqn = "B + 1")
  expect_no_message(simulate(sfm))
  sim = simulate(sfm)
  expect_equal(sort(names(sim$df)), c("A", "B", "C", "time"))

  # Stocks without flows
  sfm = xmile()%>% sim_specs(language = "R") %>%
    sim_specs(start = 0, stop = 10, dt = 0.1) %>%
    build("A", "stock", eqn = "100") %>%
    build("B", "stock", eqn = "1") %>%
    build("C", "aux", eqn = "B + 1")
  expect_no_message(simulate(sfm))
  sim = simulate(sfm)
  expect_equal(sort(names(sim$df)), c("A", "B", "C", "time"))

  # # With macros
  # sfm = xmile(start = 0, stop = 10, dt = 0.1) %>%
  #   build("A", "stock", eqn = "100") %>%
  #   build("B", "flow", eqn = "1 + C(t)", to = "A") %>%
  #   macro("C", eqn = "function(x) x + 1")
  # expect_no_message(simulate(sfm))
  # **solve macros& functions and how to define, maybe don't use (;x) as all arguments have to be named then, but with (x) they have to be in the right order
  # **variables cannot be functions because of the name issue with translating functions to Julia
  # **sigmoid() errorsfm = xmile() %>% header(name = "Maya's Burnout") %>%
  # eqn = "sigmoid((workday - normal_workday), midpoint = health)"



  # Only keep stocks
  sfm = xmile("SIR") %>% sim_specs(language = "R")
  sim = simulate(sfm, only_stocks = TRUE)
  expect_equal(ncol(as.data.frame(sim)), 1 + length(names(sfm$model$variables$stock)))

  # All variables should be kept if only_stocks = FALSE
  sfm = xmile("SIR") %>% sim_specs(language = "R")
  sim = simulate(sfm, only_stocks = FALSE)
  df = as.data.frame(sfm)
  df = df[df$type != "constant", ]
  expect_equal(ncol(as.data.frame(sim)), 1 + length(df$name))


})



test_that("throw error in compile_R for unsupported functions", {

  # No R simulations for models with units
  sfm = xmile("coffee_cup") %>% sim_specs(language = "R")
  expect_error(simulate(sfm), "The model contains unit strings u\\(''\\), which are not supported for simulations in R")

  # No R simulations for models with delay
  sfm = xmile() %>% sim_specs(language = "R") %>%
    build("A", "stock", eqn = "100") %>%
    build("B", "flow", eqn = "delay(A, 5)", to = "A")
  expect_error(simulate(sfm), "The model contains either delay\\(\\) or past\\(\\), which are not supported for simulations in R")

  # No R simulations for models with past
  sfm = xmile() %>% sim_specs(language = "R") %>%
    build("A", "stock", eqn = "100") %>%
    build("B", "flow", eqn = "past(A, 5)", to = "A")
  expect_error(simulate(sfm), "The model contains either delay\\(\\) or past\\(\\), which are not supported for simulations in R")

  # No R simulations for models with delayN
  sfm = xmile() %>% sim_specs(language = "R") %>%
    build("A", "stock", eqn = "100") %>%
    build("B", "flow", eqn = "delayN(A, 5, 3)", to = "A")
  expect_error(simulate(sfm), "The model contains either delayN\\(\\) or smoothN\\(\\), which are not supported for simulations in R")

  # No R simulations for models with smoothN
  sfm = xmile() %>% sim_specs(language = "R") %>%
    build("A", "stock", eqn = "100") %>%
    build("B", "flow", eqn = "smoothN(A, 5, 3)", to = "A")
  expect_error(simulate(sfm), "The model contains either delayN\\(\\) or smoothN\\(\\), which are not supported for simulations in R")

})


test_that("seed works", {

  # Without a seed, simulations shouldn't be the same
  sfm = xmile("predator-prey") %>% sim_specs(language = "R") %>%
    sim_specs(seed = NULL) %>%
    build(c("predator", "prey"), eqn = "runif(1, 20, 50)")
  sim1 = simulate(sfm)
  sim2 = simulate(sfm)
  expect_equal(sim1$df$predator[1] == sim2$df$predator[1], FALSE)
  expect_equal(dplyr::last(sim1$df$predator) == dplyr::last(sim2$df$predator), FALSE)

  # With a seed, simulations should be the same
  sfm = sfm %>% sim_specs(language = "R") %>% sim_specs(seed = 1)
  sim1 = simulate(sfm)
  sim2 = simulate(sfm)
  expect_equal(dplyr::last(sim1$df$predator), dplyr::last(sim2$df$predator))

})

