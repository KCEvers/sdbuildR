
test_that("logit works", {
  expect_equal(logit(.5), 0)
})

test_that("expit works", {
  expect_equal(expit(0), 0.5)
})

test_that("rbool works", {
  expect_equal(rbool(0), FALSE)
  expect_equal(rbool(1), TRUE)
})


test_that("nonnegative works", {
  expect_equal(nonnegative(-10), 0)
})

test_that("convert_u works", {
  expect_equal(convert_u(1, u('s')), 1)
})


test_that("logistic works", {

  expect_equal(logistic(0), 0.5)
  expect_equal(logistic(0.9, midpoint = .9), 0.5)
  expect_equal(logistic(-1.59, midpoint = -1.59), 0.5)
  expect_equal(logistic(1, slope = 50), 1)
  expect_equal(logistic(1, slope = -50), 0)
  expect_equal(logistic(1, slope = 50, upper = 10), 10)

  sfm0 = xmile() %>%
    build("a", "stock") %>%
    build("b", "flow",
          eqn = "logistic(t, slope = -9, midpoint = 0.5, upper = 10)", to = "a")
  sim = expect_no_error(simulate(sfm0))
  df = as.data.frame(sim)
  x = df$time
  expect_equal(df$b, logistic(x, slope = -9, midpoint = 0.5, upper = 10))

  sim = expect_no_error(simulate(sfm0 %>% sim_specs(language = "Julia")))
  df = as.data.frame(sim)
  x = df$time
  expect_equal(df$b, logistic(x, slope = -9, midpoint = 0.5, upper = 10))

  sfm0 = xmile() %>%
    build("a", "stock") %>%
    build("b", "flow", eqn = "logistic(t, 9, upper = 10, midpoint = 0.5)", to = "a")
  sim = expect_no_error(simulate(sfm0))
  df = as.data.frame(sim)
  x = df$time
  expect_equal(df$b, logistic(x, slope = 9, midpoint = 0.5, upper = 10))

  sim = expect_no_error(simulate(sfm0 %>% sim_specs(language = "Julia")))
  df = as.data.frame(sim)
  x = df$time
  expect_equal(df$b, logistic(x, slope = 9, midpoint = 0.5, upper = 10))

})


test_that("step works", {

  # Set-up basic sfm
  sfm0 = xmile() %>% sim_specs(stop = 10, dt = .1) %>%
    build("a", "stock") %>%
    build("b", "flow", eqn = "input(t)", to = "a")

  expect_no_error(sfm0 %>% build("input", "constant", eqn = "step(5)"))
  sfm = sfm0 %>% build("input", "constant", eqn = "step(5)")
  sim = expect_no_error(simulate(sfm))
  df = as.data.frame(sim)
  expect_equal(df$a[which(df$time < 5)[1]], 0)
  expect_equal(df$a[which(dplyr::near(df$time, 5))[1]], 0)
  expect_equal(df$a[which(df$time > 5)[1]], 0.1 * 1) # dt * height step

  sim = expect_no_error(simulate(sfm %>% sim_specs(language = "Julia")))
  df = as.data.frame(sim)
  expect_equal(df$a[which(df$time < 5)[1]], 0)
  expect_equal(df$a[which(dplyr::near(df$time, 5))[1]], 0)
  expect_equal(df$a[which(df$time > 5)[1]], 0.1 * 1) # dt * height step

  # Also works with keyword arguments
  expect_no_error(sfm0 %>% build("input", "constant", eqn = "step(start=5)"))
  sfm = sfm0 %>% build("input", "constant", eqn = "step(start=5)")
  expect_no_error(simulate(sfm))
  expect_no_error(simulate(sfm %>% sim_specs(language = "Julia")))

  expect_no_error(sfm0 %>% build("input", "constant", eqn = "step(start = 5, 8)"))
  sfm = sfm0 %>% build("input", "constant", eqn = "step(start = 5, 8)")
  expect_no_error(simulate(sfm))
  expect_no_error(simulate(sfm %>% sim_specs(language = "Julia")))

  expect_no_error(sfm0 %>% build("input", "constant", eqn = "step(5, height = 8)"))
  sfm = sfm0 %>% build("input", "constant", eqn = "step(5, height = 8)")
  expect_no_error(simulate(sfm))
  expect_no_error(simulate(sfm %>% sim_specs(language = "Julia")))

  expect_no_error(sfm0 %>% build("input", "constant", eqn = "step(height = 8, 5)"))
  sfm = sfm0 %>% build("input", "constant", eqn = "step(height = 8, 5)")
  expect_no_error(simulate(sfm))
  expect_no_error(simulate(sfm %>% sim_specs(language = "Julia")))

  expect_no_error(sfm0 %>% build("input", "constant", eqn = "step(start = 5, height = 8)"))
  sfm = sfm0 %>% build("input", "constant", eqn = "step(start = 5, height = 8)")
  expect_no_error(simulate(sfm))
  expect_no_error(simulate(sfm %>% sim_specs(language = "Julia")))

  # Also works with units
  expect_no_error(sfm0 %>% build("input", "constant", eqn = "step(start = u('5seconds'))"))
  sfm = sfm0 %>% build("input", "constant", eqn = "step(start = u('5seconds'))") %>% sim_specs(language = "Julia", time_units = "seconds")
  sim = expect_no_error(simulate(sfm))
  df = as.data.frame(sim)
  expect_equal(df$a[which(df$time < 5)[1]], 0)
  expect_equal(df$a[which(dplyr::near(df$time, 5))[1]], 0)
  expect_equal(df$a[which(df$time > 5)[1]], 0.1 * 1) # dt * height step

  # expect_no_error(sfm0 %>% build("input", "constant", eqn = "step(start = u('5seconds'))") %>%
  #                   build("a", units = "kcal"))
  # sfm = sfm0 %>% build("input", "constant", eqn = "step(start = u('5seconds'))")
  # sim = expect_no_error(simulate(sfm %>% sim_specs(language = "Julia")))

})


test_that("pulse works", {

  expect_error(pulse(5, 1, 0), "The width of the pulse cannot be equal to or less than 0")

  # Set-up basic sfm
  sfm0 = xmile() %>% sim_specs(stop = 20, dt = .1) %>%
    build("a", "stock") %>%
    build("b", "flow", eqn = "input(t)", to = "a")

  expect_no_error(sfm0 %>% build("input", "constant", eqn = "pulse(5, 2)"))
  sfm = sfm0 %>% build("input", "constant", eqn = "pulse(5, 2)")
  sim = expect_no_error(simulate(sfm))
  df = as.data.frame(sim)
  expect_equal(df$a[which(df$time < 5)[1]], 0)
  expect_equal(df$a[which(dplyr::near(df$time, 5))[1]], 0)
  expect_equal(df$a[which(df$time > 5)[1]], 0.1 * 2) # dt * height pulse

  sim = expect_no_error(simulate(sfm %>% sim_specs(language = "Julia")))
  df = as.data.frame(sim)
  expect_equal(df$a[which(df$time < 5)[1]], 0)
  expect_equal(df$a[which(dplyr::near(df$time, 5))[1]], 0)
  expect_equal(df$a[which(df$time > 5)[1]], 0.1 * 2) # dt * height pulse

  # Passing a NULL argument
  sfm = expect_no_error(sfm0 %>% build("input", "constant",
                                       eqn = "pulse(10, height = 1, width = 1, repeat_interval = NULL)"))
  sim = expect_no_error(simulate(sfm))
  df = as.data.frame(sim)
  expect_equal(df$a[which(df$time < 10)[1]], 0)
  expect_equal(df$a[which(dplyr::near(df$time, 5))[1]], 0)
  expect_equal(df$a[which(df$time > 10)[1]], 0.1 * 1) # dt * height pulse

  sim = expect_no_error(simulate(sfm %>% sim_specs(language = "Julia")))
  df = as.data.frame(sim)
  expect_equal(df$a[which(df$time < 10)[1]], 0)
  expect_equal(df$a[which(dplyr::near(df$time, 5))[1]], 0)
  expect_equal(df$a[which(df$time > 10)[1]], 0.1 * 1) # dt * height pulse

  # Test repeating pulses
  sfm = expect_no_error(sfm0 %>% build("input", "constant",
                                       eqn = "pulse(10, height = 1, width = 1, repeat_interval = 5)"))
  sim = expect_no_error(simulate(sfm))
  df = as.data.frame(sim)
  expect_equal(df$a[which(df$time < 10)[1]], 0)
  expect_equal(df$a[which(dplyr::near(df$time, 5))[1]], 0)
  expect_equal(df$a[which(df$time > 10)[1]], 0.1 * 1) # dt * height pulse
  expect_equal(df$a[which(df$time > 15)[1]], 1 + 0.1 * 1) # dt * height pulse

  sim = expect_no_error(simulate(sfm %>% sim_specs(language = "Julia")))
  df = as.data.frame(sim)
  expect_equal(df$a[which(df$time < 10)[1]], 0)
  expect_equal(df$a[which(dplyr::near(df$time, 5))[1]], 0)
  expect_equal(df$a[which(df$time > 10)[1]], 0.1 * 1) # dt * height pulse
  expect_equal(df$a[which(df$time > 15)[1]], 1 + 0.1 * 1) # dt * height pulse


})


test_that("ramp works", {

  expect_error(ramp(5, 2), "The finish time of the ramp cannot be before the start time\\. To specify a decreasing ramp, set the height to a negative value")

  # Set-up basic sfm
  sfm0 = xmile() %>% sim_specs(stop = 10, dt = .1) %>%
    build("a", "stock") %>%
    build("b", "flow", eqn = "input(t)", to = "a")

  expect_warning(simulate(sfm0 %>% build("input", "constant", eqn = "ramp(5, 2)")),
                 "An error occurred while running the R script")

  expect_no_error(sfm0 %>% build("input", "constant", eqn = "ramp(2, 5)"))
  sfm = sfm0 %>% build("input", "constant", eqn = "ramp(2, 5)")
  sim = expect_no_error(simulate(sfm))
  df = as.data.frame(sim)
  expect_equal(df$a[which(df$time < 2)[1]], 0)
  expect_equal(df$a[which(dplyr::near(df$time, 2))[1]], 0)
  expect_equal(df$a[which(df$time > 2)[1]], 0) # first value is still zero
  expect_equal(df$a[which(df$time > 2)[2]] > 0, TRUE)

  sim = expect_no_error(simulate(sfm %>% sim_specs(language = "Julia")))
  df = as.data.frame(sim)
  expect_equal(df$a[which(df$time < 2)[1]], 0)
  expect_equal(df$a[which(dplyr::near(df$time, 2))[1]], 0)
  expect_equal(df$a[which(df$time > 2)[1]], 0) # first value is still zero
  expect_equal(df$a[which(dplyr::near(df$time, 2.2))[1]] > 0, TRUE)

})

test_that("seasonal works", {

  expect_error(seasonal(-10), "The period of the seasonal wave must be greater than 0")

  # Set-up basic sfm
  sfm0 = xmile() %>% sim_specs(stop = 10, dt = .1) %>%
    build("a", "stock") %>%
    build("b", "flow", eqn = "input(t)", to = "a")

  expect_no_error(sfm0 %>% build("input", "constant", eqn = "seasonal()"))
  sfm = sfm0 %>% build("input", "constant", eqn = "seasonal()")
  sim = expect_no_error(simulate(sfm))
  df = as.data.frame(sim)
  expect_equal(df$a[1], 0)
  expect_equal(df$a[2] > 0, TRUE)

  sim = expect_no_error(simulate(sfm %>% sim_specs(language = "Julia")))
  df = as.data.frame(sim)
  expect_equal(df$a[1], 0)
  expect_equal(df$a[2] > 0, TRUE)

  # With shift
  expect_no_error(sfm0 %>% build("input", "constant", eqn = "seasonal(shift = 1)"))
  sfm = sfm0 %>% build("input", "constant", eqn = "seasonal(shift = 1)")
  sim = expect_no_error(simulate(sfm))
  df = as.data.frame(sim)
  expect_equal(df$a[1], 0)
  expect_equal(df$a[2] > 0, TRUE)

  sim = expect_no_error(simulate(sfm %>% sim_specs(language = "Julia")))
  df = as.data.frame(sim)
  expect_equal(df$a[1], 0)
  expect_equal(df$a[2] > 0, TRUE)

})

