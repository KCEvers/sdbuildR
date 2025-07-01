
test_that("templates work", {

  sfm = xmile("SIR") %>% sim_specs(language = "Julia")
  expect_no_error(plot(sfm))
  expect_no_error(as.data.frame(sfm))

  # Already simulate templates in julia_vs_r

  sfm = xmile("predator-prey") %>% sim_specs(language = "Julia")
  expect_no_error(plot(sfm))
  expect_no_error(as.data.frame(sfm))

  sfm = xmile("logistic_model") %>% sim_specs(language = "Julia")
  expect_no_error(plot(sfm))
  expect_no_error(as.data.frame(sfm))

  # Check whether coffee cup reaches room temperature
  sfm = xmile("coffee_cup") %>% sim_specs(language = "Julia")
  expect_no_error(plot(sfm))
  expect_no_error(as.data.frame(sfm))
  sim = expect_no_error(simulate(sfm))
  expect_equal(sim$success, TRUE)
  expect_equal(nrow(sim$df) > 0, TRUE)
  expect_equal(dplyr::last(sim$df$coffee_temperature), sim$constants$room_temperature, tolerance = .01)

  sfm = xmile("Crielaard2022") %>% sim_specs(language = "Julia")
  expect_no_error(plot(sfm))
  expect_no_error(as.data.frame(sfm))

  # Duffing previously had an error with cos()
  sfm = xmile("Duffing") %>% sim_specs(language = "Julia")
  expect_no_error(plot(sfm))
  expect_no_error(as.data.frame(sfm))

  sfm = xmile("Chua") %>% sim_specs(language = "Julia")
  expect_no_error(plot(sfm))
  expect_no_error(as.data.frame(sfm))

  # Non existing template
  expect_error(xmile("A"), "A is not an available template. The available templates are")
  expect_error(xmile(""), "is not an available template. The available templates are")
  expect_error(xmile(" "), "  is not an available template. The available templates are")

})



test_that("saveat works", {

  use_julia()

  # Cannot set saveat to lower than dt
  sfm = xmile("SIR")
  expect_warning(sfm %>% sim_specs(dt = .1, saveat = .01),
                 "dt must be smaller or equal to saveat! Setting saveat equal to dt")

  # Check whether dataframe is returned at saveat times
  sfm = sfm %>%
    sim_specs(saveat = 0.1, dt = 0.001, start = 100, stop = 200)
  sim = simulate(sfm %>% sim_specs(language = "Julia"))
  expect_equal(diff(sim$df$time)[1], as.numeric(sfm$sim_specs$saveat))

  sim = simulate(sfm %>% sim_specs(language = "R"))
  expect_equal(diff(sim$df$time)[1], as.numeric(sfm$sim_specs$saveat))

})




test_that("simulate with different components works", {

  # Without stocks throws error
  sfm = xmile()
  expect_error(simulate(sfm %>% sim_specs(language = "Julia")), "Your model has no stocks.")

  sfm = xmile() %>% build("a", "stock") %>%
    build("b", "flow")
  expect_error(simulate(sfm %>% sim_specs(language = "Julia")), "These flows are not connected to any stock:\\n- b")

  # With one stock and no flows and no parameters
  sfm = xmile() %>% sim_specs(start = 0, stop = 10, dt = 0.1) %>%
    build("A", "stock", eqn = "100")
  sim = expect_no_error(simulate(sfm %>% sim_specs(language = "Julia")))
  expect_equal(sort(names(sim$df)), c("A", "time"))

  # One stock with flows, other stock without flows
  sfm = xmile() %>% sim_specs(start = 0, stop = 10, dt = 0.1) %>%
    build(c("A", "B"), "stock", eqn = "100") %>%
    build("C", "flow", eqn = "1", to = "A")
  sim = expect_no_error(simulate(sfm %>% sim_specs(language = "Julia")))
  expect_equal(sort(names(sim$df)), c("A", "B", "C", "time"))

  # With one intermediary -> error in constructing Dataframe before
  sfm = xmile() %>% sim_specs(start = 0, stop = 10, dt = 0.1) %>%
    build("A", "stock", eqn = "100") %>%
    build("B", "flow", eqn = "1", to = "A") %>%
    build("C", "aux", eqn = "B + 1")
  sim = expect_no_message(simulate(sfm %>% sim_specs(language = "Julia")))
  expect_equal(sort(names(sim$df)), c("A", "B", "C", "time"))

  # One intermediary variable that is also a stock, so it is removed -> does merging of df and intermediary_df still work?
  sfm = xmile() %>% sim_specs(start = 0, stop = 10, dt = 0.1) %>%
    build("A", "stock", eqn = "100") %>%
    build("B", "flow", eqn = "delay(A, 5)", to = "A")
  sim = expect_no_message(simulate(sfm %>% sim_specs(language = "Julia")))
  expect_equal(sort(names(sim$df)), c("A", "B", "time"))

  # Stocks without flows
  sfm = xmile() %>% sim_specs(start = 0, stop = 10, dt = 0.1) %>%
    build("A", "stock", eqn = "100") %>%
    build("B", "stock", eqn = "1") %>%
    build("C", "aux", eqn = "B + 1")
  sim = expect_no_message(simulate(sfm %>% sim_specs(language = "Julia")))
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
  sfm = xmile("SIR")
  sim = simulate(sfm %>% sim_specs(language = "Julia"), only_stocks = TRUE)
  expect_equal(ncol(as.data.frame(sim)), 1 + length(names(sfm$model$variables$stock)))

  # All variables should be kept if only_stocks = FALSE
  sfm = xmile("SIR")
  sim = simulate(sfm %>% sim_specs(language = "Julia"), only_stocks = FALSE)
  df = as.data.frame(sfm)
  df = df[df$type != "constant", ]
  expect_equal(ncol(as.data.frame(sim)), 1 + length(df$name))


  # ** some have units

})



test_that("seed works", {

  # Without a seed, simulations shouldn't be the same
  sfm = xmile("predator-prey") %>%
    sim_specs(seed = NULL) %>%
    build(c("predator", "prey"), eqn = "runif(1, 20, 50)") %>% sim_specs(language = "Julia")
  sim1 = simulate(sfm)
  sim2 = simulate(sfm)
  expect_equal(sim1$df$predator[1] == sim2$df$predator[1], FALSE)
  expect_equal(dplyr::last(sim1$df$predator) == dplyr::last(sim2$df$predator), FALSE)

  # With a seed, simulations should be the same
  sfm = sfm %>% sim_specs(seed = 1)
  sim1 = simulate(sfm)
  sim2 = simulate(sfm)
  expect_equal(dplyr::last(sim1$df$predator), dplyr::last(sim2$df$predator))

})



test_that("units in stocks and flows", {

  # No unit specified in stock yet stock evaluates to unit
  sfm = xmile()  %>% sim_specs(language = "Julia") %>% build("a", "stock", eqn = "round(u('100.80 kilograms'))")
  expect_no_error(simulate(sfm))

  sfm = xmile()  %>% sim_specs(language = "Julia") %>% build("a", "stock", eqn = "round(u('108.67 seconds'))")
  expect_no_error(simulate(sfm))

})

