

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
  sim = expect_no_error(simulate(sfm))
  expect_equal(sim$df[sim$df$variable == "Inflow", "value"][which(sim$df$time < 8)[1]], 100)
  expect_equal(sim$df[sim$df$variable == "Inflow", "value"][which(sim$df$time > 8)[1]], 200)
  expect_equal(sim$df[sim$df$variable == "Outflow", "value"][which(sim$df$time < 8 + 4)[1]], 100)
  expect_equal(sim$df[sim$df$variable == "Outflow", "value"][which(sim$df$time >= 8 + 4)[1]], 200)

  # **Check whether first argument to delay() is in variables



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
  sim = expect_no_error(simulate(sfm))
  expect_equal(sim$df[sim$df$variable == "Inflow", "value"][which(sim$df$time < 8)[1]], 100)
  expect_equal(sim$df[sim$df$variable == "Inflow", "value"][which(sim$df$time > 8)[1]], 200)
  expect_equal(sim$df[sim$df$variable == "Outflow", "value"][which(sim$df$time < 8 + 4)[1]], 100)
  expect_equal(sim$df[sim$df$variable == "Outflow", "value"][which(sim$df$time >= 8 + 4)[1]], 200)


})



test_that("delayN() works", {

  # Check delayN() with wrong building block throws error
  expect_error({xmile() %>% build("a", "stock", eqn = "delayN(a, 1, 2)")}, "Adjust equation of a: delayN\\(\\) cannot be used for a stock")

  # Check delayN() with too few arguments throws error
  expect_error({xmile() %>% build("a", "aux", eqn = "delayN()")}, "Obligatory arguments variable, length, order are missing for function delayN")
  expect_error({xmile() %>% build("a", "aux", eqn = "delayN(a)")}, "Obligatory arguments length, order are missing for function delayN")
  expect_error({xmile() %>% build("a", "aux", eqn = "delayN(a, 1)")}, "Obligatory argument order is missing for function delayN")


  # Test delayN() in simple model
  sfm = expect_no_error(xmile() %>% sim_specs(language = "Julia") %>%
    sim_specs(start = 0, stop = 20, dt = 0.1) %>% # dt=.125->dt=.1
    build("MaterialinTransit", "stock", eqn = "400") %>%
    build("Inflow", "flow", eqn = "ifelse(t < 8, 100, 200)", to = "MaterialinTransit") %>%
    build("Outflow", "flow", eqn = "delayN(Inflow, AverageDelay, 5, 0)", from = "MaterialinTransit") %>%
    build("AverageDelay", "constant", eqn = 4))
  expect_no_error(plot(sfm))
  sim = expect_no_error(simulate(sfm))
  expect_equal(sim$df[sim$df$variable == "Inflow", "value"][which(sim$df$time < 8)[1]], 100)
  expect_equal(sim$df[sim$df$variable == "Inflow", "value"][which(sim$df$time > 8)[1]], 200)


  sfm = expect_no_error(xmile() %>%
    sim_specs(language = "Julia", time_units = "days") %>%
    build("effort", "stock", eqn = .5) %>%
    build("goal", "constant", eqn = 1) %>%
    build("delayed", "aux", "smoothN(effort, .1, 3)") %>%
    build("discrepancy", "aux", "goal - smoothN(effort, .1, 3)") %>%
    build("exertion", "flow", "0.1 * discrepancy", to = "effort"))

  sim = expect_no_error(simulate(sfm))
  expect_no_error(plot(sim, add_constants = TRUE))

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

  sfm = expect_no_error(xmile() %>%
    build("Outflow", "flow", eqn = "delay(Inflow, 10*3, 1/4) + past(Inflow, 100) + delayN(Inflow, 50, 4) + smoothN(Inflow,150,3)"))
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

