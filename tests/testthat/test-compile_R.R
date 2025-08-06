test_that("simulate R for templates", {
  for (s in c("SIR", "predator-prey", "logistic_model", "Crielaard2022", "Duffing", "Chua")) {
    sfm <- xmile(s)

    if (s == "Crielaard2022") {
      sfm <- sfm |>
        # Update initial condition to be non-stochastic
        build(c("Food_intake", "Hunger", "Compensatory_behaviour"), eqn = round(runif(3), 8))
    }

    sim1 <- simulate(sfm |> sim_specs(language = "R"), only_stocks = TRUE)
    expect_equal(sim1$success, TRUE)
    expect_equal(nrow(sim1$df) > 0, TRUE)

    sim1 <- simulate(sfm |> sim_specs(language = "R"), only_stocks = FALSE)
    expect_equal(sim1$success, TRUE)
    expect_equal(nrow(sim1$df) > 0, TRUE)

    if (s == "logistic_model") {
      # Check whether the population converges to the carrying capacity
      expect_equal(dplyr::last(sim1$df[sim1$df$variable == "X", "value"]),
        sim1$constants[["K"]],
        tolerance = .01
      )
    }
  }
})

test_that("simulate with different components works", {
  # Without stocks throws error
  sfm <- xmile() |> sim_specs(language = "R")
  expect_error(simulate(sfm), "Your model has no stocks.")

  sfm <- xmile() |>
    sim_specs(language = "R") |>
    build("a", "stock") |>
    build("b", "flow")
  expect_error(simulate(sfm), "These flows are not connected to any stock:\\n- b")

  # With one stock and no flows and no parameters
  sfm <- xmile() |>
    sim_specs(language = "R") |>
    sim_specs(start = 0, stop = 10, dt = 0.1) |>
    build("A", "stock", eqn = "100")
  sim <- expect_no_error(simulate(sfm))
  expect_equal(sort(names(sim$df)), c("time", "value", "variable"))
  expect_no_error(plot(sim))

  # One stock with flows, other stock without flows
  sfm <- xmile() |>
    sim_specs(language = "R") |>
    sim_specs(start = 0, stop = 10, dt = 0.1) |>
    build(c("A", "B"), "stock", eqn = "100") |>
    build("C", "flow", eqn = "1", to = "A")
  sim <- expect_no_error(simulate(sfm, only_stocks = FALSE))
  expect_equal(sort(names(sim$df)), c("time", "value", "variable"))
  expect_equal(unique(sim$df$variable), c("A", "B", "C"))

  # With one intermediary -> error in constructing Dataframe before in Julia
  sfm <- xmile() |>
    sim_specs(language = "R") |>
    sim_specs(start = 0, stop = 10, dt = 0.1) |>
    build("A", "stock", eqn = "100") |>
    build("B", "flow", eqn = "1", to = "A") |>
    build("C", "aux", eqn = "B + 1")
  sim <- expect_no_message(simulate(sfm, only_stocks = FALSE))
  expect_equal(sort(names(sim$df)), c("time", "value", "variable"))
  expect_equal(unique(sim$df$variable), c("A", "B", "C"))

  # Stocks without flows
  sfm <- xmile() |>
    sim_specs(language = "R") |>
    sim_specs(start = 0, stop = 10, dt = 0.1) |>
    build("A", "stock", eqn = "100") |>
    build("B", "stock", eqn = "1") |>
    build("C", "aux", eqn = "B + 1")
  sim <- expect_no_message(simulate(sfm, only_stocks = FALSE))
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
  sfm <- xmile("SIR") |> sim_specs(language = "R")
  sim <- simulate(sfm, only_stocks = TRUE)
  expect_equal(
    length(unique(as.data.frame(sim)$variable)),
    length(names(sfm$model$variables$stock))
  )

  # All variables should be kept if only_stocks = FALSE
  sfm <- xmile("SIR") |> sim_specs(language = "R")
  sim <- simulate(sfm, only_stocks = FALSE)
  df <- as.data.frame(sfm)
  df <- df[df$type != "constant", ]
  expect_equal(length(unique(as.data.frame(sim)$variable)), length(df$name))
})


test_that("equations that refer to the variable itself throw error", {

  sfm = xmile() %>%
    build("E", "stock", eqn = "E")
  expect_error(simulate(sfm), "The properties below contain references to undefined variables")

})


test_that("output of simulate in R", {
  sfm <- xmile("SIR") |> sim_specs(language = "R", start = 0, stop = 10, dt = .1)
  sim <- expect_no_error(simulate(sfm))
  expect_equal(all(c("df", "init", "constants", "sfm", "script", "duration") %in% names(sim)), TRUE)

  # Check that init and constants are not Julia objects
  expect_equal(class(sim$constants), "numeric")
  expect_equal(class(sim$init), "numeric")
  expect_equal(
    sort(names(sim$constants)),
    c("Delay", "Effective_Contact_Rate", "Total_Population")
  )
  expect_equal(
    sort(names(sim$init)),
    c("Infected", "Recovered", "Susceptible")
  )
})

test_that("throw error in compile_R for unsupported functions", {
  # No R simulations for models with units
  sfm <- xmile("coffee_cup") |> sim_specs(language = "R")
  expect_error(simulate(sfm), "The model contains unit strings u\\(''\\), which are not supported for simulations in R")

  # No R simulations for models with delay
  sfm <- xmile() |>
    sim_specs(language = "R") |>
    build("A", "stock", eqn = "100") |>
    build("B", "flow", eqn = "delay(A, 5)", to = "A")
  expect_error(simulate(sfm), "The model contains either delay\\(\\) or past\\(\\), which are not supported for simulations in R")

  # No R simulations for models with past
  sfm <- xmile() |>
    sim_specs(language = "R") |>
    build("A", "stock", eqn = "100") |>
    build("B", "flow", eqn = "past(A, 5)", to = "A")
  expect_error(simulate(sfm), "The model contains either delay\\(\\) or past\\(\\), which are not supported for simulations in R")

  # No R simulations for models with delayN
  sfm <- xmile() |>
    sim_specs(language = "R") |>
    build("A", "stock", eqn = "100") |>
    build("B", "flow", eqn = "delayN(A, 5, 3)", to = "A")
  expect_error(simulate(sfm), "The model contains either delayN\\(\\) or smoothN\\(\\), which are not supported for simulations in R")

  # No R simulations for models with smoothN
  sfm <- xmile() |>
    sim_specs(language = "R") |>
    build("A", "stock", eqn = "100") |>
    build("B", "flow", eqn = "smoothN(A, 5, 3)", to = "A")
  expect_error(simulate(sfm), "The model contains either delayN\\(\\) or smoothN\\(\\), which are not supported for simulations in R")
})


test_that("save_at works", {
  # Cannot set save_at to lower than dt
  sfm <- xmile("SIR")

  # Check whether dataframe is returned at save_at times
  sfm <- sfm |>
    sim_specs(save_at = 0.1, dt = 0.001, start = 100, stop = 200)

  sim <- simulate(sfm |> sim_specs(language = "R"))
  expect_equal(
    diff(sim$df[sim$df$variable == "Infected", "time"])[1],
    as.numeric(sfm$sim_specs$save_at)
  )
})


test_that("negative times are possible", {
  sfm <- xmile("logistic_model") |> sim_specs(start = -100, language = "R")
  expect_no_error({
    sim <- simulate(sfm)
  })
})


test_that("save_from works", {
  sfm <- xmile("SIR") |> sim_specs(
    start = 0, stop = 100,
    save_from = 10, language = "R"
  )
  sim <- expect_no_error(simulate(sfm))
  expect_equal(min(sim$df$time), 10)
  expect_equal(max(sim$df$time), 100)
  expect_no_error(plot(sim))
  expect_no_error(summary(sfm))
})


test_that("seed works", {
  # Without a seed, simulations shouldn't be the same
  sfm <- xmile("predator-prey") |>
    sim_specs(language = "R", start = 0, stop = 10, dt = 0.1) |>
    sim_specs(seed = NULL) |>
    build(c("predator", "prey"), eqn = "runif(1, 20, 50)")
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




test_that("function in aux still works", {
  sfm <- xmile() |>
    sim_specs(language = "R", start = 0, stop = 10, dt = .1) |>
    build("A", "stock") |>
    build("input", "aux", eqn = "ramp(5, 10, -1)")
  sim <- expect_no_error(simulate(sfm, only_stocks = FALSE))
  sim <- expect_no_message(simulate(sfm, only_stocks = FALSE))

  # Check that input is not returned as a variable
  expect_equal(sort(unique(sim$df$variable)), c("A"))

  # Check with two intermediary variables
  sfm <- sfm |>
    build("a2", "aux", eqn = " 0.38 + input(t)")
  sim <- expect_no_error(simulate(sfm, only_stocks = FALSE))
  sim <- expect_no_message(simulate(sfm, only_stocks = FALSE))

  # Check that input is not returned as a variable
  expect_equal(sort(unique(sim$df$variable)), c("A", "a2"))
})
