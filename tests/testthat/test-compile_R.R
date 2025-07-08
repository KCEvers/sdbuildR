
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
  sim = expect_no_error(simulate(sfm))
  expect_equal(sort(names(sim$df)), c("time", "value", "variable"))
  expect_no_error(plot(sim))

  # One stock with flows, other stock without flows
  sfm = xmile()%>% sim_specs(language = "R") %>%
    sim_specs(start = 0, stop = 10, dt = 0.1) %>%
    build(c("A", "B"), "stock", eqn = "100") %>%
    build("C", "flow", eqn = "1", to = "A")
  sim = expect_no_error(simulate(sfm, only_stocks = FALSE))
  expect_equal(sort(names(sim$df)), c("time", "value", "variable"))
  expect_equal(unique(sim$df$variable), c("A", "B", "C"))

  # With one intermediary -> error in constructing Dataframe before in Julia
  sfm = xmile() %>% sim_specs(language = "R") %>%
    sim_specs(start = 0, stop = 10, dt = 0.1) %>%
    build("A", "stock", eqn = "100") %>%
    build("B", "flow", eqn = "1", to = "A") %>%
    build("C", "aux", eqn = "B + 1")
  sim = expect_no_message(simulate(sfm, only_stocks = FALSE))
  expect_equal(sort(names(sim$df)), c("time", "value", "variable"))
  expect_equal(unique(sim$df$variable), c("A", "B", "C"))

  # Stocks without flows
  sfm = xmile()%>% sim_specs(language = "R") %>%
    sim_specs(start = 0, stop = 10, dt = 0.1) %>%
    build("A", "stock", eqn = "100") %>%
    build("B", "stock", eqn = "1") %>%
    build("C", "aux", eqn = "B + 1")
  sim = expect_no_message(simulate(sfm, only_stocks = FALSE))
  expect_equal(sort(names(sim$df)), c("time", "value", "variable"))
  expect_equal(unique(sim$df$variable), c("A", "B", "C"))

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
  expect_equal(length(unique(as.data.frame(sim)$variable)), length(names(sfm$model$variables$stock)))

  # All variables should be kept if only_stocks = FALSE
  sfm = xmile("SIR") %>% sim_specs(language = "R")
  sim = simulate(sfm, only_stocks = FALSE)
  df = as.data.frame(sfm)
  df = df[df$type != "constant", ]
  expect_equal(length(unique(as.data.frame(sim)$variable)), length(df$name))

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


test_that("save_from works", {

  sfm = xmile("SIR") %>% sim_specs(start = 0, stop = 100,
                                   save_from = 10, language = "R")
  sim = expect_no_error(simulate(sfm))
  expect_equal(min(sim$df$time), 10)
  expect_equal(max(sim$df$time), 100)
  expect_no_error(plot(sim))
  expect_no_error(summary(sfm))

})


test_that("seed works", {

  # Without a seed, simulations shouldn't be the same
  sfm = xmile("predator-prey") %>%
    sim_specs(language = "R", start = 0, stop = 10, dt = 0.1) %>%
    sim_specs(seed = NULL) %>%
    build(c("predator", "prey"), eqn = "runif(1, 20, 50)")
  sim1 = simulate(sfm)
  sim2 = simulate(sfm)
  expect_equal(sim1$df$value[1] == sim2$df$value[1], FALSE)
  expect_equal(dplyr::last(sim1$df$value) == dplyr::last(sim2$df$value), FALSE)

  # With a seed, simulations should be the same
  sfm = sfm %>% sim_specs(seed = 1)
  sim1 = simulate(sfm)
  sim2 = simulate(sfm)
  expect_equal(dplyr::last(sim1$df$value), dplyr::last(sim2$df$value))

})

