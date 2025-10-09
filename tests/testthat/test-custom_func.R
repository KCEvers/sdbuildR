test_that("round_IM works", {
  expect_equal(round_IM(0.5), 1)
  expect_equal(round_IM(-0.5), 0)
  expect_equal(round_IM(-2.2), -2)
  expect_equal(round_IM(-2.2, 2), -2.20)
})

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

test_that("length_IM works", {
  expect_equal(length_IM(c("a", "b", "c")), 3)
  expect_equal(length_IM("abcdef"), 6)
  expect_equal(length_IM(c("abcdef")), 6)
})


test_that("contains_IM works", {
  expect_false(contains_IM(c("a", "b", "c"), "d"))
  expect_true(contains_IM(c("abcdef"), "bc"))
})

test_that("indexof works", {
  expect_equal(indexof(c("a", "b", "c"), "b"), 2)
  expect_equal(indexof("haystack", "hay"), 1)
  expect_equal(indexof("haystack", "haym"), 0)
})


test_that("nonnegative works", {
  expect_equal(nonnegative(-10), 0)
  expect_equal(nonnegative(10), 10)
  expect_equal(nonnegative(0), 0)
})


test_that("rem works", {
  a <- 7
  b <- 3
  expect_equal(rem(a, b), mod(a, b))
  a <- -7
  b <- 3
  expect_equal(rem(a, b), -1)
  expect_equal(mod(a, b), 2)
  a <- 7
  b <- -3
  expect_equal(rem(a, b), 1)
  expect_equal(mod(a, b), -2)
  a <- -7
  b <- -3
  expect_equal(rem(a, b), mod(a, b))
  expect_equal(a %REM% b, rem(a, b))
})


test_that("saveat_func works", {
  time_vec <- seq(0, 10, by = .1)
  df <- data.frame(time = time_vec, y = sin(time_vec))
  new_times <- 1:10
  result <- saveat_func(df, "time", new_times)
  expect_equal(result[["time"]], new_times)
  expect_equal(result[["y"]], df[match(new_times, df[["time"]]), "y"])
})


test_that("convert_u works", {
  expect_equal(convert_u(1, u("s")), 1)
})


test_that("logistic works", {
  expect_equal(logistic(0), 0.5)
  expect_equal(logistic(0.9, midpoint = .9), 0.5)
  expect_equal(logistic(-1.59, midpoint = -1.59), 0.5)
  expect_equal(logistic(1, slope = 50), 1)
  expect_equal(logistic(1, slope = -50), 0)
  expect_equal(logistic(1, slope = 50, upper = 10), 10)

  sfm0 <- xmile() |>
    sim_specs(start = 0, stop = 1, dt = .1) |>
    build("a", "stock") |>
    build("b", "flow",
      eqn = "logistic(t, slope = -9, midpoint = 0.5, upper = 10)", to = "a"
    )
  sim <- expect_no_error(simulate(sfm0))
  df <- as.data.frame(sim)
  x <- df[df$variable == "b", "time"]
  expect_equal(
    df[df$variable == "b", "value"],
    logistic(x, slope = -9, midpoint = 0.5, upper = 10)
  )

  sfm2 <- xmile() |>
    build("a", "stock") |>
    build("b", "flow", eqn = "logistic(t, 9, upper = 10, midpoint = 0.5)", to = "a")
  sim <- expect_no_error(simulate(sfm2))
  df <- as.data.frame(sim)
  x <- df[df$variable == "b", "time"]
  expect_equal(
    df[df$variable == "b", "value"],
    logistic(x, slope = 9, midpoint = 0.5, upper = 10)
  )

  testthat::skip_on_cran()

  sim <- expect_no_error(simulate(sfm0 |> sim_specs(language = "Julia")))
  df <- as.data.frame(sim)
  x <- df[df$variable == "b", "time"]
  expect_equal(
    df[df$variable == "b", "value"],
    logistic(x, slope = -9, midpoint = 0.5, upper = 10)
  )

  sim <- expect_no_error(simulate(sfm2 |> sim_specs(language = "Julia")))
  df <- as.data.frame(sim)
  x <- df[df$variable == "b", "time"]
  expect_equal(
    df[df$variable == "b", "value"],
    logistic(x, slope = 9, midpoint = 0.5, upper = 10)
  )
})


test_that("step works", {

  times <- seq(0, 10, by = .1)

  expect_error(step(), "argument \"start\" is missing, with no default")
  expect_error(step(times), "argument \"start\" is missing, with no default")
  expect_warning(step(times, -10), "Start of step before beginning of simulation time")
  expect_warning(step(times, 11), "Start of step after end of simulation time")

  # Set-up basic sfm
  sfm0 <- xmile() |>
    sim_specs(stop = 10, dt = .1) |>
    build("a", "stock") |>
    build("b", "flow", eqn = "input(t)", to = "a")

  expect_no_error(sfm0 |> build("input", "constant", eqn = "step(times, 5)"))
  sfm <- sfm0 |> build("input", "constant", eqn = "step(times, 5)")
  sim <- expect_no_error(simulate(sfm))
  df <- as.data.frame(sim)
  a <- df[df$variable == "a", "value"]
  expect_equal(a[which(df$time < 5)[1]], 0)
  expect_equal(a[which(dplyr::near(df$time, 5))[1]], 0)
  expect_equal(a[which(df$time > 5)[1]], 0.1 * 1) # dt * height step

  # Also works with keyword arguments
  expect_no_error(sfm0 |> build("input", "constant", eqn = "step(times, start=5)"))
  sfm <- sfm0 |> build("input", "constant", eqn = "step(times, start=5)")
  expect_no_error(simulate(sfm))

  expect_no_error(sfm0 |> build("input", "constant", eqn = "step(times, start = 5, 8)"))
  sfm <- sfm0 |> build("input", "constant", eqn = "step(times, start = 5, 8)")
  expect_no_error(simulate(sfm))

  expect_no_error(sfm0 |> build("input", "constant", eqn = "step(times, 5, height = 8)"))
  sfm <- sfm0 |> build("input", "constant", eqn = "step(times, 5, height = 8)")
  expect_no_error(simulate(sfm))

  expect_no_error(sfm0 |> build("input", "constant", eqn = "step(times, height = 8, 5)"))
  sfm <- sfm0 |> build("input", "constant", eqn = "step(times, height = 8, 5)")
  expect_no_error(simulate(sfm))

  expect_no_error(sfm0 |> build("input", "constant", eqn = "step(times, start = 5, height = 8)"))
  sfm <- sfm0 |> build("input", "constant", eqn = "step(times, start = 5, height = 8)")
  sim <- expect_no_error(simulate(sfm))

  # Ensure plotting works with add_constants as these are functions
  sim <- expect_no_error(simulate(sfm))
  expect_no_error(plot(sim, add_constants = TRUE))
})


test_that("step works (Julia)", {
  testthat::skip_on_cran()

  # Set-up basic sfm
  sfm0 <- xmile() |>
    sim_specs(stop = 10, dt = .1) |>
    build("a", "stock") |>
    build("b", "flow", eqn = "input(t)", to = "a")

  expect_no_error(sfm0 |> build("input", "constant", eqn = "step(times, 5)"))
  sfm <- sfm0 |> build("input", "constant", eqn = "step(times, 5)")
  sim <- expect_no_error(simulate(sfm |> sim_specs(language = "Julia")))
  df <- as.data.frame(sim)
  a <- df[df$variable == "a", "value"]
  expect_equal(a[which(df$time < 5)[1]], 0)
  expect_equal(a[which(dplyr::near(df$time, 5))[1]], 0)
  expect_equal(a[which(df$time > 5)[1]], 0.1 * 1) # dt * height step

  # Also works with keyword arguments
  expect_no_error(sfm0 |> build("input", "constant", eqn = "step(times, start=5)"))
  sfm <- sfm0 |> build("input", "constant", eqn = "step(times, start=5)")
  expect_no_error(simulate(sfm |> sim_specs(language = "Julia")))

  expect_no_error(sfm0 |> build("input", "constant", eqn = "step(times, start = 5, 8)"))
  sfm <- sfm0 |> build("input", "constant", eqn = "step(times, start = 5, 8)")
  expect_no_error(simulate(sfm |> sim_specs(language = "Julia")))

  expect_no_error(sfm0 |> build("input", "constant", eqn = "step(times, 5, height = 8)"))
  sfm <- sfm0 |> build("input", "constant", eqn = "step(times, 5, height = 8)")
  expect_no_error(simulate(sfm |> sim_specs(language = "Julia")))

  expect_no_error(sfm0 |> build("input", "constant", eqn = "step(times, height = 8, 5)"))
  sfm <- sfm0 |> build("input", "constant", eqn = "step(times, height = 8, 5)")
  expect_no_error(simulate(sfm |> sim_specs(language = "Julia")))

  expect_no_error(sfm0 |> build("input", "constant", eqn = "step(times, start = 5, height = 8)"))
  sfm <- sfm0 |> build("input", "constant", eqn = "step(times, start = 5, height = 8)")
  sim <- expect_no_error(simulate(sfm |> sim_specs(language = "Julia")))

  # Ensure plotting works with add_constants as these are functions
  sim <- expect_no_error(simulate(sfm |> sim_specs(language = "Julia")))
  expect_no_error(plot(sim, add_constants = TRUE))

  # Also works with units
  expect_no_error(sfm0 |> build("input", "constant", eqn = "step(times, start = u('5seconds'))"))
  sfm <- sfm0 |>
    build("input", "constant", eqn = "step(times, start = u('5seconds'))") |>
    sim_specs(language = "Julia", time_units = "seconds")
  sim <- expect_no_error(simulate(sfm))
  df <- as.data.frame(sim)
  a <- df[df$variable == "a", "value"]
  expect_equal(a[which(df$time < 5)[1]], 0)
  expect_equal(a[which(dplyr::near(df$time, 5))[1]], 0)
  expect_equal(a[which(df$time > 5)[1]], 0.1 * 1) # dt * height step
})


test_that("pulse works", {
  times <- seq(0, 10, by = .1)

  expect_error(pulse(), "argument \"start\" is missing, with no default")
  expect_error(pulse(times), "argument \"start\" is missing, with no default")
  expect_error(pulse(times, 5, 1, 0), "The width of the pulse cannot be equal to or less than 0")
  expect_warning(pulse(times, -10), "Start of pulse before beginning of simulation time")
  expect_warning(pulse(times, 11), "Start of pulse after end of simulation time")
  expect_warning(pulse(times, 5, 1, 10, 10),
                 "width \\(10\\) >= repeat_interval \\(10\\) creates a continuous pulse")

  # Set-up basic sfm
  sfm0 <- xmile() |>
    sim_specs(stop = 20, dt = .1) |>
    build("a", "stock") |>
    build("b", "flow", eqn = "input(t)", to = "a")

  expect_no_error(sfm0 |> build("input", "constant", eqn = "pulse(times, 5, 2)"))
  sfm <- sfm0 |> build("input", "constant", eqn = "pulse(times, 5, 2)")
  sim <- expect_no_error(simulate(sfm))
  df <- as.data.frame(sim)
  a <- df[df$variable == "a", "value"]
  expect_equal(a[which(df$time < 5)[1]], 0)
  expect_equal(a[which(dplyr::near(df$time, 5))[1]], 0)
  expect_equal(a[which(df$time > 5)[1]], 0.1 * 2) # dt * height pulse

  # Forgetting times
  sfm <- expect_error(sfm0 |> build("input", "constant",
                                       eqn = "pulse(10)"
  ), "Obligatory argument start is missing for function pulse")

  # Passing a NULL argument
  sfm <- expect_no_error(sfm0 |> build("input", "constant",
    eqn = "pulse(times, 10, height = 1, width = 1, repeat_interval = NULL)"
  ))
  sim <- expect_no_error(simulate(sfm))
  df <- as.data.frame(sim)
  a <- df[df$variable == "a", "value"]
  expect_equal(a[which(df$time < 10)[1]], 0)
  expect_equal(a[which(dplyr::near(df$time, 5))[1]], 0)
  expect_equal(a[which(df$time > 10)[1]], 0.1 * 1) # dt * height pulse

  # Test repeating pulses
  sfm <- expect_no_error(sfm0 |> build("input", "constant",
    eqn = "pulse(times, 10, height = 1, width = 1, repeat_interval = 5)"
  ))
  sim <- expect_no_error(simulate(sfm))
  df <- as.data.frame(sim)
  a <- df[df$variable == "a", "value"]
  expect_equal(a[which(df$time < 10)[1]], 0)
  expect_equal(a[which(dplyr::near(df$time, 5))[1]], 0)
  expect_equal(a[which(df$time > 10)[1]], 0.1 * 1) # dt * height pulse
  expect_equal(a[which(df$time > 15)[1]], 1 + 0.1 * 1) # dt * height pulse

  # Ensure plotting works with add_constants as these are functions
  sim <- expect_no_error(simulate(sfm))
  expect_no_error(plot(sim, add_constants = TRUE))
})


test_that("pulse works (Julia)", {
  testthat::skip_on_cran()

  # Set-up basic sfm
  sfm0 <- xmile() |>
    sim_specs(stop = 20, dt = .1) |>
    build("a", "stock") |>
    build("b", "flow", eqn = "input(t)", to = "a")

  sfm <- sfm0 |> build("input", "constant", eqn = "pulse(times, 5, 2)")
  sim <- expect_no_error(simulate(sfm |> sim_specs(language = "Julia")))
  df <- as.data.frame(sim)
  a <- df[df$variable == "a", "value"]
  expect_equal(a[which(df$time < 5)[1]], 0)
  expect_equal(a[which(dplyr::near(df$time, 5))[1]], 0)
  expect_equal(a[which(df$time > 5)[1]], 0.1 * 2) # dt * height pulse

  # Passing a NULL argument
  sfm <- expect_no_error(sfm0 |> build("input", "constant",
    eqn = "pulse(times, 10, height = 1, width = 1, repeat_interval = NULL)"
  ))
  sim <- expect_no_error(simulate(sfm |> sim_specs(language = "Julia")))
  df <- as.data.frame(sim)
  a <- df[df$variable == "a", "value"]
  expect_equal(a[which(df$time < 10)[1]], 0)
  expect_equal(a[which(dplyr::near(df$time, 5))[1]], 0)
  expect_equal(a[which(df$time > 10)[1]], 0.1 * 1) # dt * height pulse

  # Test repeating pulses
  sfm <- expect_no_error(sfm0 |> build("input", "constant",
    eqn = "pulse(times, 10, height = 1, width = 1, repeat_interval = 5)"
  ))
  sim <- expect_no_error(simulate(sfm |> sim_specs(language = "Julia")))
  df <- as.data.frame(sim)
  a <- df[df$variable == "a", "value"]
  expect_equal(a[which(df$time < 10)[1]], 0)
  expect_equal(a[which(dplyr::near(df$time, 5))[1]], 0)
  expect_equal(a[which(df$time > 10)[1]], 0.1 * 1) # dt * height pulse
  expect_equal(a[which(df$time > 15)[1]], 1 + 0.1 * 1) # dt * height pulse

  # Ensure plotting works with add_constants as these are functions
  sim <- expect_no_error(simulate(sfm |> sim_specs(language = "Julia")))
  expect_no_error(plot(sim, add_constants = TRUE))
})

test_that("ramp works", {
  times <- seq(0, 10, by = .1)

  # If there are multiple missing arguments, it always gives the last one
  expect_error(ramp(), "argument \"finish\" is missing, with no default")
  expect_error(ramp(times), "argument \"finish\" is missing, with no default")
  expect_error(ramp(times, 1), "argument \"finish\" is missing, with no default")
  expect_error(ramp(times, 5, 2), "The finish time of the ramp cannot be before the start time\\. To specify a decreasing ramp, set the height to a negative value")

  # Set-up basic sfm
  sfm0 <- xmile() |>
    sim_specs(stop = 10, dt = .1) |>
    build("a", "stock") |>
    build("b", "flow", eqn = "input(t)", to = "a")

  expect_warning(
    simulate(sfm0 |> build("input", "constant", eqn = "ramp(times, 5, 2)")),
    "An error occurred while running the R script"
  )

  expect_no_error(sfm0 |> build("input", "constant", eqn = "ramp(times, 2, 5)"))
  sfm <- sfm0 |> build("input", "constant", eqn = "ramp(times, 2, 5)")
  sim <- expect_no_error(simulate(sfm))
  df <- as.data.frame(sim)
  a <- df[df$variable == "a", "value"]
  expect_equal(a[which(df$time < 2)[1]], 0)
  expect_equal(a[which(dplyr::near(df$time, 2))[1]], 0)
  expect_equal(a[which(df$time > 2)[1]], 0) # first value is still zero
  expect_equal(a[which(df$time > 2)[2]] > 0, TRUE)

  # Ensure plotting works with add_constants as these are functions
  sim <- expect_no_error(simulate(sfm))
  expect_no_error(plot(sim, add_constants = TRUE))
})


test_that("ramp works (Julia)", {
  testthat::skip_on_cran()

  # Set-up basic sfm
  sfm0 <- xmile() |>
    sim_specs(stop = 10, dt = .1) |>
    build("a", "stock") |>
    build("b", "flow", eqn = "input(t)", to = "a")

  sfm <- sfm0 |> build("input", "constant", eqn = "ramp(times, 2, 5)")
  sim <- expect_no_error(simulate(sfm |> sim_specs(language = "Julia")))
  df <- as.data.frame(sim)
  a <- df[df$variable == "a", "value"]
  expect_equal(a[which(df$time < 2)[1]], 0)
  expect_equal(a[which(dplyr::near(df$time, 2))[1]], 0)
  expect_equal(a[which(df$time > 2)[1]], 0) # first value is still zero
  expect_equal(a[which(df$time > 2)[2]] > 0, TRUE)

  # Ensure plotting works with add_constants as these are functions
  sim <- expect_no_error(simulate(sfm |> sim_specs(language = "Julia")))
  expect_no_error(plot(sim, add_constants = TRUE))
})


test_that("seasonal works", {
  times <- seq(0, 10, by = .1)

  expect_error(seasonal(), "argument \"times\" is missing, with no default")
  expect_error(seasonal(times, -10), "The period of the seasonal wave must be greater than 0")

  # Set-up basic sfm
  sfm0 <- xmile() |>
    sim_specs(stop = 10, dt = .1) |>
    build("a", "stock") |>
    build("b", "flow", eqn = "input(t)", to = "a")

  expect_no_error(sfm0 |> build("input", "constant", eqn = "seasonal(times)"))
  sfm <- sfm0 |> build("input", "constant", eqn = "seasonal(times)")
  sim <- expect_no_error(simulate(sfm))
  df <- as.data.frame(sim)
  a <- df[df$variable == "a", "value"]
  expect_equal(a[1], 0)
  expect_equal(a[2] > 0, TRUE)

  # With shift
  expect_no_error(sfm0 |> build("input", "constant", eqn = "seasonal(times, shift = 1)"))
  sfm <- sfm0 |> build("input", "constant", eqn = "seasonal(times, shift = 1)")
  sim <- expect_no_error(simulate(sfm))
  df <- as.data.frame(sim)
  a <- df[df$variable == "a", "value"]
  expect_equal(a[1], 0)
  expect_equal(a[2] > 0, TRUE)

  # Ensure plotting works with add_constants as these are functions
  sim <- expect_no_error(simulate(sfm))
  expect_no_error(plot(sim, add_constants = TRUE))
})

test_that("seasonal works (Julia)", {
  testthat::skip_on_cran()

  # Set-up basic sfm
  sfm0 <- xmile() |>
    sim_specs(stop = 10, dt = .1) |>
    build("a", "stock") |>
    build("b", "flow", eqn = "input(t)", to = "a")

  sfm <- sfm0 |> build("input", "constant", eqn = "seasonal(times)")
  sim <- expect_no_error(simulate(sfm |> sim_specs(language = "Julia")))
  df <- as.data.frame(sim)
  a <- df[df$variable == "a", "value"]
  expect_equal(a[1], 0)
  expect_equal(a[2] > 0, TRUE)

  # With shift
  sfm <- sfm0 |> build("input", "constant", eqn = "seasonal(times, shift = 1)")
  sim <- expect_no_error(simulate(sfm |> sim_specs(language = "Julia")))
  df <- as.data.frame(sim)
  a <- df[df$variable == "a", "value"]
  expect_equal(a[1], 0)
  expect_equal(a[2] > 0, TRUE)

  # Ensure plotting works with add_constants as these are functions
  sim <- expect_no_error(simulate(sfm |> sim_specs(language = "Julia")))
  expect_no_error(plot(sim, add_constants = TRUE))
})

