
test_that("templates work", {

  sfm = xmile("SIR") %>% sim_specs(language = "Julia")
  expect_no_error(plot(sfm))
  expect_no_error(as.data.frame(sfm))
  expect_no_error(simulate(sfm))
  sim = simulate(sfm)
  expect_equal(sim$success, TRUE)
  expect_equal(nrow(sim$df) > 0, TRUE)

  sfm = xmile("predator-prey") %>% sim_specs(language = "Julia")
  expect_no_error(plot(sfm))
  expect_no_error(as.data.frame(sfm))
  expect_no_error(simulate(sfm))
  sim = simulate(sfm)
  expect_equal(sim$success, TRUE)
  expect_equal(nrow(sim$df) > 0, TRUE)

  sfm = xmile("logistic_model") %>% sim_specs(language = "Julia")
  expect_no_error(plot(sfm))
  expect_no_error(as.data.frame(sfm))
  expect_no_error(simulate(sfm))
  sim = simulate(sfm)
  expect_equal(sim$success, TRUE)
  expect_equal(nrow(sim$df) > 0, TRUE)

  # Check whether the population converges to the carrying capacity
  sim = simulate(sfm)
  expect_equal(sim$success, TRUE)
  expect_equal(nrow(sim$df) > 0, TRUE)
  expect_equal(dplyr::last(sim$df$X), sim$pars$K, tolerance = .01)

  # Check whether coffee cup reaches room temperature
  sfm = xmile("coffee_cup") %>% sim_specs(language = "Julia")
  expect_no_error(plot(sfm))
  expect_no_error(as.data.frame(sfm))
  expect_no_error(simulate(sfm))
  sim = simulate(sfm)
  expect_equal(sim$success, TRUE)
  expect_equal(nrow(sim$df) > 0, TRUE)
  expect_equal(dplyr::last(sim$df$coffee_temperature), sim$pars$room_temperature, tolerance = .01)

  sfm = xmile("Crielaard2022") %>% sim_specs(language = "Julia")
  expect_no_error(plot(sfm))
  expect_no_error(as.data.frame(sfm))
  expect_no_error(simulate(sfm))
  sim = simulate(sfm)
  expect_equal(sim$success, TRUE)
  expect_equal(nrow(sim$df) > 0, TRUE)

  # Duffing previously had an error with cos()
  sfm = xmile("Duffing") %>% sim_specs(language = "Julia")
  expect_no_error(plot(sfm))
  expect_no_error(as.data.frame(sfm))
  expect_no_error(simulate(sfm))
  sim = simulate(sfm)
  expect_equal(sim$success, TRUE)
  expect_equal(nrow(sim$df) > 0, TRUE)

  sfm = xmile("Chua") %>% sim_specs(language = "Julia")
  expect_no_error(plot(sfm))
  expect_no_error(as.data.frame(sfm))
  expect_no_error(simulate(sfm))
  sim = simulate(sfm)
  expect_equal(sim$success, TRUE)
  expect_equal(nrow(sim$df) > 0, TRUE)

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
  expect_no_error(simulate(sfm %>% sim_specs(language = "Julia")))
  sim = simulate(sfm %>% sim_specs(language = "Julia"))
  expect_equal(sort(names(sim$df)), c("A", "time"))

  # One stock with flows, other stock without flows
  sfm = xmile() %>% sim_specs(start = 0, stop = 10, dt = 0.1) %>%
    build(c("A", "B"), "stock", eqn = "100") %>%
    build("C", "flow", eqn = "1", to = "A")
  expect_no_error(simulate(sfm %>% sim_specs(language = "Julia")))
  sim = simulate(sfm %>% sim_specs(language = "Julia"))
  expect_equal(sort(names(sim$df)), c("A", "B", "C", "time"))

  # With one intermediary -> error in constructing Dataframe before
  sfm = xmile() %>% sim_specs(start = 0, stop = 10, dt = 0.1) %>%
    build("A", "stock", eqn = "100") %>%
    build("B", "flow", eqn = "1", to = "A") %>%
    build("C", "aux", eqn = "B + 1")
  expect_no_message(simulate(sfm %>% sim_specs(language = "Julia")))
  sim = simulate(sfm %>% sim_specs(language = "Julia"))
  expect_equal(sort(names(sim$df)), c("A", "B", "C", "time"))

  # One intermediary variable that is also a stock, so it is removed -> does merging of df and intermediary_df still work?
  sfm = xmile() %>% sim_specs(start = 0, stop = 10, dt = 0.1) %>%
    build("A", "stock", eqn = "100") %>%
    build("B", "flow", eqn = "delay(A, 5)", to = "A")
  expect_no_message(simulate(sfm %>% sim_specs(language = "Julia")))
  sim = simulate(sfm %>% sim_specs(language = "Julia"))
  expect_equal(sort(names(sim$df)), c("A", "B", "time"))

  # Stocks without flows
  sfm = xmile() %>% sim_specs(start = 0, stop = 10, dt = 0.1) %>%
    build("A", "stock", eqn = "100") %>%
    build("B", "stock", eqn = "1") %>%
    build("C", "aux", eqn = "B + 1")
  expect_no_message(simulate(sfm %>% sim_specs(language = "Julia")))
  sim = simulate(sfm %>% sim_specs(language = "Julia"))
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



test_that("delay() works", {

  # Check delay() with wrong building block throws error
  expect_error({xmile() %>% build("a", "stock", eqn = "delay(a, 1)")}, "Adjust equation of a: delay\\(\\) cannot be used for a stock")

  # Check delay() with too few arguments throws error
  expect_error({xmile() %>% build("a", "aux", eqn = "delay()")}, "Obligatory arguments variable, length are missing for function delay")
  expect_error({xmile() %>% build("a", "aux", eqn = "delay(a)")}, "Obligatory argument length is missing for function delay")


  # Check that multiple delays are saved
  sfm = xmile() %>%
    build("Outflow", "flow", eqn = "delay(Inflow, AverageDelay, 100) + delay(Inflow, 100) + delay(Inflow, 50, 4)")
  expect_equal(length(sfm$model$variables$flow$Outflow$func$delay), 3)
  expect_equal(sort(names(sfm$model$variables$flow$Outflow$func$delay)),
               paste0("Outflow", P$delay_suffix, 1:3))
  expect_equal(sfm$model$variables$flow$Outflow$func$delay[[1]]$var, "Inflow")
  expect_equal(sfm$model$variables$flow$Outflow$func$delay[[2]]$var, "Inflow")
  expect_equal(sfm$model$variables$flow$Outflow$func$delay[[3]]$var, "Inflow")
  expect_equal(sfm$model$variables$flow$Outflow$func$delay[[1]]$length, "AverageDelay")
  expect_equal(sfm$model$variables$flow$Outflow$func$delay[[2]]$length, "100.0")
  expect_equal(sfm$model$variables$flow$Outflow$func$delay[[3]]$length, "50.0")
  expect_equal(sfm$model$variables$flow$Outflow$func$delay[[1]]$initial, "100.0")
  expect_equal(sfm$model$variables$flow$Outflow$func$delay[[2]]$initial, "nothing")
  expect_equal(sfm$model$variables$flow$Outflow$func$delay[[3]]$initial, "4.0")

  # Chapter 4 Duggan: Fixed Delay
  # https://github.com/JimDuggan/SDMR/blob/master/models/04%20Chapter/Extra%20Examples/FixedDelay.R

  sfm = xmile()  %>% sim_specs(language = "Julia") %>%
    sim_specs(start = 0, stop = 20, dt = 0.1) %>%
    build("MaterialinTransit", "stock", eqn = "400") %>%
    build("Inflow", "flow", eqn = "ifelse(t < 8, 100, 200)", to = "MaterialinTransit") %>%
    build("Outflow", "flow", eqn = "delay(Inflow, AverageDelay, 100)", from = "MaterialinTransit") %>%
    build("AverageDelay", "constant", eqn = 4)
  expect_no_error(plot(sfm))
  expect_no_error(simulate(sfm))
  sim = simulate(sfm)
  expect_equal(sim$df$Inflow[which(sim$df$time < 8)[1]], 100)
  expect_equal(sim$df$Inflow[which(sim$df$time > 8)[1]], 200)
  expect_equal(sim$df$Outflow[which(sim$df$time < 8 + 4)[1]], 100)
  expect_equal(sim$df$Outflow[which(sim$df$time >= 8 + 4)[1]], 200)

  # **Check whether first argument to delay() is in variables


  #

})



test_that("past() works", {

  # Check past() with wrong building block throws error
  expect_error({xmile() %>% build("a", "stock", eqn = "past(a, 1)")}, "Adjust equation of a: past\\(\\) cannot be used for a stock")

  # Check past() with too few arguments throws error
  expect_error({xmile() %>% build("a", "aux", eqn = "past()")}, "Obligatory argument variable is missing for function past")


  sfm = xmile()  %>% sim_specs(language = "Julia") %>%
    sim_specs(start = 0, stop = 20, dt = 0.1) %>%
    build("MaterialinTransit", "stock", eqn = "400") %>%
    build("Inflow", "flow", eqn = "ifelse(t < 8, 100, 200)", to = "MaterialinTransit") %>%
    build("Outflow", "flow", eqn = "max(past(Inflow, AverageDelay))", from = "MaterialinTransit") %>%
    build("AverageDelay", "constant", eqn = 4)
  expect_no_error(plot(sfm))
  expect_no_error(simulate(sfm))
  sim = simulate(sfm)
  expect_equal(sim$df$Inflow[which(sim$df$time < 8)[1]], 100)
  expect_equal(sim$df$Inflow[which(sim$df$time > 8)[1]], 200)
  expect_equal(sim$df$Outflow[which(sim$df$time < 8 + 4)[1]], 100)
  expect_equal(sim$df$Outflow[which(sim$df$time >= 8 + 4)[1]], 200)


})



test_that("delayN() works", {


  # Check delayN() with wrong building block throws error
  expect_error({xmile() %>% build("a", "stock", eqn = "delayN(a, 1, 2)")}, "Adjust equation of a: delayN\\(\\) cannot be used for a stock")

  # Check delayN() with too few arguments throws error
  expect_error({xmile() %>% build("a", "aux", eqn = "delayN()")}, "Obligatory arguments variable, length, order are missing for function delayN")
  expect_error({xmile() %>% build("a", "aux", eqn = "delayN(a)")}, "Obligatory arguments length, order are missing for function delayN")
  expect_error({xmile() %>% build("a", "aux", eqn = "delayN(a, 1)")}, "Obligatory argument order is missing for function delayN")


  # Test delayN() in simple model
  sfm = xmile() %>% sim_specs(language = "Julia") %>%
    sim_specs(start = 0, stop = 20, dt = 0.1) %>% # dt=.125->dt=.1
    build("MaterialinTransit", "stock", eqn = "400") %>%
    build("Inflow", "flow", eqn = "ifelse(t < 8, 100, 200)", to = "MaterialinTransit") %>%
    build("Outflow", "flow", eqn = "delayN(Inflow, AverageDelay, 5, 0)", from = "MaterialinTransit") %>%
    build("AverageDelay", "constant", eqn = 4)
  expect_no_error(plot(sfm))
  expect_no_error(simulate(sfm))
  sim = simulate(sfm)
  expect_equal(sim$df$Inflow[which(sim$df$time < 8)[1]], 100)
  expect_equal(sim$df$Inflow[which(sim$df$time > 8)[1]], 200)

  # **


})


test_that("smoothN() works", {

  # Check smoothN() with wrong building block throws error
  expect_error({xmile() %>% build("a", "stock", eqn = "smoothN(a, 1, 2)")}, "Adjust equation of a: smoothN\\(\\) cannot be used for a stock")

  # Check smoothN() with too few arguments throws error
  expect_error({xmile() %>% build("a", "aux", eqn = "smoothN()")}, "Obligatory arguments variable, length, order are missing for function smoothN")
  expect_error({xmile() %>% build("a", "aux", eqn = "smoothN(a)")}, "Obligatory arguments length, order are missing for function smoothN")
  expect_error({xmile() %>% build("a", "aux", eqn = "smoothN(a, 1)")}, "Obligatory argument order is missing for function smoothN")


})


test_that("mixing delay family functions works", {

  sfm = xmile() %>%
    build("Outflow", "flow", eqn = "delay(Inflow, 10*3, 1/4) + past(Inflow, 100) + delayN(Inflow, 50, 4) + smoothN(Inflow,150,3)")
  expect_equal(length(sfm$model$variables$flow$Outflow$func$delay), 1)
  expect_equal(length(sfm$model$variables$flow$Outflow$func$past), 1)
  expect_equal(length(sfm$model$variables$flow$Outflow$func$delayN), 1)
  expect_equal(length(sfm$model$variables$flow$Outflow$func$smoothN), 1)
  expect_equal(names(sfm$model$variables$flow$Outflow$func$delay),
               paste0("Outflow", P$delay_suffix, 1))
  expect_equal(names(sfm$model$variables$flow$Outflow$func$past),
               paste0("Outflow", P$past_suffix, 1))
  expect_equal(names(sfm$model$variables$flow$Outflow$func$delayN),
               paste0("Outflow", P$delayN_suffix, 1))
  expect_equal(names(sfm$model$variables$flow$Outflow$func$smoothN),
               paste0("Outflow", P$smoothN_suffix, 1))
  expect_equal(sfm$model$variables$flow$Outflow$func$delay[[1]]$var, "Inflow")
  expect_equal(sfm$model$variables$flow$Outflow$func$delay[[1]]$length, "10.0 .* 3.0")
  expect_equal(sfm$model$variables$flow$Outflow$func$delay[[1]]$initial, "1.0 ./ 4.0")
  expect_equal(sfm$model$variables$flow$Outflow$func$past[[1]]$var, "Inflow")
  expect_equal(sfm$model$variables$flow$Outflow$func$past[[1]]$length, "100.0")
  expect_equal(sfm$model$variables$flow$Outflow$func$delayN[[1]]$var, "Inflow")
  expect_equal(sfm$model$variables$flow$Outflow$func$delayN[[1]]$length, "50.0")
  expect_equal(sfm$model$variables$flow$Outflow$func$delayN[[1]]$order, "4.0")
  expect_equal(sfm$model$variables$flow$Outflow$func$delayN[[1]]$initial, "Inflow")
  expect_equal(sfm$model$variables$flow$Outflow$func$smoothN[[1]]$var, "Inflow")
  expect_equal(sfm$model$variables$flow$Outflow$func$smoothN[[1]]$length, "150.0")
  expect_equal(sfm$model$variables$flow$Outflow$func$smoothN[[1]]$order, "3.0")
  expect_equal(sfm$model$variables$flow$Outflow$func$smoothN[[1]]$initial, "Inflow")

})


test_that("units in stocks and flows", {

  # No unit specified in stock yet stock evaluates to unit
  sfm = xmile()  %>% sim_specs(language = "Julia") %>% build("a", "stock", eqn = "round(u('100.80 kilograms'))")
  expect_no_error(simulate(sfm))

  sfm = xmile()  %>% sim_specs(language = "Julia") %>% build("a", "stock", eqn = "round(u('108.67 seconds'))")
  expect_no_error(simulate(sfm))

})

