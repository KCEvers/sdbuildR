# Already simulate templates in julia_vs_r, no need to do that here too

test_that("templates work", {
  testthat::skip_on_cran()
  testthat::skip_if_not(julia_status()$status == "ready")

  for (s in c("SIR", "predator_prey", "logistic_model", "Crielaard2022", "Duffing", "Chua")) {
    sfm <- xmile(s) |> sim_specs(language = "Julia", stop = 10, dt = 0.1)
    expect_no_error(plot(sfm))
    expect_no_error(as.data.frame(sfm))
    expect_true(nrow(as.data.frame(sfm)) > 0)
  }

  # Check whether coffee cup reaches room temperature
  sfm <- xmile("coffee_cup") |> sim_specs(language = "Julia")
  expect_no_error(plot(sfm))
  expect_no_error(as.data.frame(sfm))
  sim <- expect_no_error(simulate(sfm))
  expect_true(sim$success)
  expect_true(nrow(sim$df) > 0)
  expect_equal(dplyr::last(sim$df[sim$df$variable == "coffee_temperature", "value"]),
    sim$constants[["room_temperature"]],
    tolerance = .01
  )
})


test_that("output of simulate in Julia", {
  testthat::skip_on_cran()
  testthat::skip_if_not(julia_status()$status == "ready")

  sfm <- xmile("SIR") |> sim_specs(language = "Julia", start = 0, stop = 10, dt = .1)
  sim <- expect_no_error(simulate(sfm))
  expect_true(sim$success)
  expect_true(all(c("df", "init", "constants", "sfm", "script", "duration")
  %in% names(sim)))

  # Check that init and constants are not Julia objects
  expect_equal(class(sim$constants), "numeric")
  expect_equal(class(sim$init), "numeric")
  expect_equal(
    sort(names(sim$constants)),
    c("Beta", "Delay", "Effective_Contact_Rate", "Total_Population")
  )
  expect_equal(
    sort(names(sim$init)),
    c("Infected", "Recovered", "Susceptible")
  )
})


test_that("save_at works", {
  testthat::skip_on_cran()
  testthat::skip_if_not(julia_status()$status == "ready")

  # Cannot set save_at to lower than dt
  sfm <- xmile("SIR")
  expect_warning(
    sfm |> sim_specs(dt = .1, save_at = .01),
    "dt must be smaller or equal to save_at! Setting save_at equal to dt"
  )

  # Check whether dataframe is returned at save_at times
  sfm <- sfm |>
    sim_specs(save_at = 1, dt = 0.1, start = 10, stop = 20)

  sim <- simulate(sfm |> sim_specs(language = "Julia"))
  expect_equal(
    diff(sim$df[sim$df$variable == "Infected", "time"])[1],
    as.numeric(sfm$sim_specs$save_at)
  )

  # Also works with models with units
  sfm <- xmile("coffee_cup") |>
    sim_specs(language = "Julia") |>
    sim_specs(save_at = 1, dt = 0.01, start = 10, stop = 20)
  sim <- simulate(sfm |> sim_specs(language = "Julia"))
  expect_equal(
    diff(sim$df[sim$df$variable == "coffee_temperature", "time"])[1],
    as.numeric(sfm$sim_specs$save_at)
  )
})


test_that("save_from works", {
  testthat::skip_on_cran()
  testthat::skip_if_not(julia_status()$status == "ready")

  sfm <- xmile("SIR") |> sim_specs(
    start = 0, stop = 20,
    save_from = 10, language = "Julia"
  )
  sim <- expect_no_error(simulate(sfm))
  expect_equal(min(sim$df$time), 10)
  expect_equal(max(sim$df$time), 20)
  expect_no_error(plot(sim))
  expect_no_error(summary(sfm))
})


test_that("simulate with different components works", {
  testthat::skip_on_cran()
  testthat::skip_if_not(julia_status()$status == "ready")

  # Without stocks throws error
  sfm <- xmile()
  expect_warning(
    sim <- simulate(sfm |> sim_specs(language = "Julia")),
    "Your model has no stocks."
  )
  expect_false(sim$success)

  sfm <- xmile() |>
    build("a", "stock") |>
    build("b", "flow")
  expect_warning(
    sim <- simulate(sfm |> sim_specs(language = "Julia")),
    "These flows are not connected to any stock:\\n- b"
  )
  expect_false(sim$success)

  # With one stock and no flows and no parameters
  sfm <- xmile() |>
    sim_specs(start = 0, stop = 10, dt = 0.1) |>
    build("A", "stock", eqn = "100")
  sim <- expect_no_error(simulate(sfm |> sim_specs(language = "Julia"), only_stocks = FALSE))
  expect_equal(sort(names(sim$df)), c("time", "value", "variable"))
  expect_equal(unique(sim$df$variable), c("A"))

  # One stock with flows, other stock without flows
  sfm <- xmile() |>
    sim_specs(start = 0, stop = 10, dt = 0.1) |>
    build(c("A", "B"), "stock", eqn = "100") |>
    build("C", "flow", eqn = "1", to = "A")
  sim <- expect_no_error(simulate(sfm |> sim_specs(language = "Julia"), only_stocks = FALSE))
  expect_equal(sort(names(sim$df)), c("time", "value", "variable"))
  expect_equal(unique(sim$df$variable), c("A", "B", "C"))


  # With one intermediary -> error in constructing Dataframe before
  sfm <- xmile() |>
    sim_specs(start = 0, stop = 10, dt = 0.1) |>
    build("A", "stock", eqn = "100") |>
    build("B", "flow", eqn = "1", to = "A") |>
    build("C", "aux", eqn = "B + 1")
  sim <- expect_no_message(simulate(sfm |> sim_specs(language = "Julia"), only_stocks = FALSE))
  expect_equal(sort(names(sim$df)), c("time", "value", "variable"))
  expect_equal(unique(sim$df$variable), c("A", "B", "C"))

  # Stocks without flows
  sfm <- xmile() |>
    sim_specs(start = 0, stop = 10, dt = 0.1) |>
    build("A", "stock", eqn = "100") |>
    build("B", "stock", eqn = "1") |>
    build("C", "aux", eqn = "B + 1")
  sim <- expect_no_message(simulate(sfm |> sim_specs(language = "Julia"), only_stocks = FALSE))
  expect_equal(sort(names(sim$df)), c("time", "value", "variable"))
  expect_equal(unique(sim$df$variable), c("A", "B", "C"))

  # # With macros
  # sfm = xmile(start = 0, stop = 10, dt = 0.1) |>
  #   build("A", "stock", eqn = "100") |>
  #   build("B", "flow", eqn = "1 + C(t)", to = "A") |>
  #   macro("C", eqn = "function(x) x + 1")
  # expect_no_message(simulate(sfm))
  # **solve macros& functions and how to define, maybe don't use (;x) as all arguments have to be named then, but with (x) they have to be in the right order
  # **variables cannot be functions because of the name issue with translating functions to Julia
  # **sigmoid() errorsfm = xmile() |> header(name = "Maya's Burnout") |>
  # eqn = "sigmoid((workday - normal_workday), midpoint = health)"

  # Only keep stocks
  sfm <- xmile("SIR")
  sim <- simulate(sfm |> sim_specs(stop = 10, dt = 0.1, language = "Julia"),
    only_stocks = TRUE
  )
  expect_equal(length(unique(as.data.frame(sim)$variable)),
               length(names(sfm$model$variables$stock)))

  # All variables should be kept if only_stocks = FALSE
  sfm <- xmile("SIR")
  sim <- simulate(sfm |> sim_specs(language = "Julia", stop = 10, dt = 0.1),
    only_stocks = FALSE
  )
  df <- as.data.frame(sfm)
  df <- df[df$type != "constant", ]
  expect_equal(length(unique(as.data.frame(sim)$variable)), length(df$name))


  # ** some have units
})


test_that("seed works", {
  testthat::skip_on_cran()
  testthat::skip_if_not(julia_status()$status == "ready")

  # Without a seed, simulations shouldn't be the same
  sfm <- xmile("predator_prey") |>
    sim_specs(seed = NULL, stop = 10, dt = 0.1) |>
    build(c("predator", "prey"), eqn = "runif(1, 20, 50)") |>
    sim_specs(language = "Julia")
  sim1 <- simulate(sfm)
  sim2 <- simulate(sfm)
  expect_equal(sim1$df$value[1] == sim2$df$value[1], FALSE)
  expect_equal(dplyr::last(sim1$df$value) == dplyr::last(sim2$df$value), FALSE)

  # With a seed, simulations should be the same
  sfm <- sfm |> sim_specs(seed = 1)
  sim1 <- simulate(sfm)
  sim2 <- simulate(sfm)
  expect_equal(dplyr::last(sim1$df$value), dplyr::last(sim2$df$value))
})


test_that("units in stocks and flows", {
  testthat::skip_on_cran()
  testthat::skip_if_not(julia_status()$status == "ready")

  # No unit specified in stock yet stock evaluates to unit
  sfm <- xmile() |>
    sim_specs(language = "Julia", stop = 10, dt = 0.1) |>
    build("a", "stock", eqn = "round(u('100.80 kilograms'))")
  expect_no_error(simulate(sfm))

  sfm <- xmile() |>
    sim_specs(language = "Julia", stop = 10, dt = 0.1) |>
    build("a", "stock", eqn = "round(u('108.67 seconds'))")
  expect_no_error(simulate(sfm))
})


test_that("function in aux still works", {
  testthat::skip_on_cran()
  testthat::skip_if_not(julia_status()$status == "ready")

  sfm <- xmile() |>
    sim_specs(language = "Julia", start = 0, stop = 10, dt = .1) |>
    build("A", "stock") |>
    build("input", "aux", eqn = "ramp(times, 5, 10, -1)")
  sim <- expect_no_error(expect_no_message(simulate(sfm, only_stocks = FALSE)))

  # Check that input is not returned as a variable
  expect_equal(sort(unique(sim$df$variable)), c("A"))

  # Check with two intermediary variables
  sfm <- sfm |>
    build("a2", "aux", eqn = " 0.38 + input(t)")
  sim <- expect_no_error(expect_no_message(simulate(sfm, only_stocks = FALSE)))

  # Check that input is not returned as a variable
  expect_equal(sort(unique(sim$df$variable)), c("A", "a2"))
})


test_that("negative times are possible", {
  testthat::skip_on_cran()
  testthat::skip_if_not(julia_status()$status == "ready")

  sfm <- xmile("logistic_model") |> sim_specs(
    start = -1, stop = 10, dt = 0.1,
    language = "Julia"
  )
  expect_no_error({
    sim <- simulate(sfm)
  })
})


test_that("functions in Julia work", {
  testthat::skip_on_cran()
  testthat::skip_if_not(julia_status()$status == "ready")

  # round() with units
  sfm <- xmile() |>
    sim_specs(language = "Julia", stop = 1, dt = 0.1) |>
    build("a", "stock", eqn = "round(10.235)")
  expect_no_error(simulate(sfm))

  sfm <- xmile() |>
    sim_specs(language = "Julia", stop = 1, dt = 0.1) |>
    build("a", "stock", eqn = "round(u('100.80 kilograms'))")
  expect_no_error(simulate(sfm))

  sfm <- xmile() |>
    sim_specs(language = "Julia", stop = 1, dt = 0.1) |>
    build("a", "stock", eqn = "round(u('108.67 seconds'))")
  expect_no_error(simulate(sfm))

  # Cosine function needs unitless argument or argument in radians
  sfm <- xmile() |>
    sim_specs(language = "Julia", stop = 1, dt = 0.1) |>
    build("a", "stock", eqn = "cos(10)")
  expect_no_error(simulate(sfm))

  sfm <- xmile() |>
    sim_specs(language = "Julia", stop = 1, dt = 0.1) |>
    build("a", "stock", eqn = "cos(u('10meters'))")
  expect_warning(
    simulate(sfm),
    "An error occurred while running the Julia script"
  )

  sfm <- xmile() |>
    sim_specs(language = "Julia", stop = 1, dt = 0.1) |>
    build("a", "stock", eqn = "cos(u('10radians'))")
  expect_no_error(simulate(sfm))
})

