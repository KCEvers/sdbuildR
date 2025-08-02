# Already simulate templates in julia_vs_r, no need to do that here too

test_that("templates work", {


  for (s in c("SIR", "predator-prey", "logistic_model", "Crielaard2022", "Duffing", "Chua")){
    sfm = xmile(s) |> sim_specs(language = "Julia")
    expect_no_error(plot(sfm))
    expect_no_error(as.data.frame(sfm))
    expect_equal(nrow(as.data.frame(sfm)) > 0, TRUE)
  }

  # Check whether coffee cup reaches room temperature
  sfm = xmile("coffee_cup") |> sim_specs(language = "Julia")
  expect_no_error(plot(sfm))
  expect_no_error(as.data.frame(sfm))
  sim = expect_no_error(simulate(sfm))
  expect_equal(sim$success, TRUE)
  expect_equal(nrow(sim$df) > 0, TRUE)
  expect_equal(dplyr::last(sim$df[sim$df$variable == "coffee_temperature", "value"]),
               sim$constants[["room_temperature"]], tolerance = .01)


})


test_that("output of simulate in Julia", {

  sfm = xmile("SIR") |> sim_specs(language = "Julia", start = 0, stop = 10, dt = .1)
  sim = expect_no_error(simulate(sfm))
  expect_equal(all(c("df", "init", "constants", "sfm", "script", "duration") %in% names(sim)), TRUE)

  # Check that init and constants are not Julia objects
  expect_equal(class(sim$constants), "numeric")
  expect_equal(class(sim$init), "numeric")
  expect_equal(sort(names(sim$constants)),
               c("Delay", "Effective_Contact_Rate", "Total_Population"))
  expect_equal(sort(names(sim$init)),
               c("Infected", "Recovered", "Susceptible"))

})


test_that("save_at works", {

  use_julia()

  # Cannot set save_at to lower than dt
  sfm = xmile("SIR")
  expect_warning(sfm |> sim_specs(dt = .1, save_at = .01),
                 "dt must be smaller or equal to save_at! Setting save_at equal to dt")

  # Check whether dataframe is returned at save_at times
  sfm = sfm |>
    sim_specs(save_at = 0.1, dt = 0.001, start = 100, stop = 200)
  sim = simulate(sfm |> sim_specs(language = "Julia"))
  expect_equal(diff(sim$df[sim$df$variable == "Infected", "time"])[1],
               as.numeric(sfm$sim_specs$save_at))

  sim = simulate(sfm |> sim_specs(language = "R"))
  expect_equal(diff(sim$df[sim$df$variable == "Infected", "time"])[1],
               as.numeric(sfm$sim_specs$save_at))

  # Also works with models with units
  sfm = xmile("coffee_cup") |> sim_specs(language = "Julia") |>
    sim_specs(save_at = 0.1, dt = 0.001, start = 100, stop = 200)
  sim = simulate(sfm |> sim_specs(language = "Julia"))
  expect_equal(diff(sim$df[sim$df$variable == "coffee_temperature", "time"])[1],
               as.numeric(sfm$sim_specs$save_at))


})


test_that("save_from works", {

  sfm = xmile("SIR") |> sim_specs(start = 0, stop = 100,
                                   save_from = 10, language = "Julia")
  sim = expect_no_error(simulate(sfm))
  expect_equal(min(sim$df$time), 10)
  expect_equal(max(sim$df$time), 100)
  expect_no_error(plot(sim))
  expect_no_error(summary(sfm))

})


test_that("simulate with different components works", {

  # Without stocks throws error
  sfm = xmile()
  expect_error(simulate(sfm |> sim_specs(language = "Julia")),
               "Your model has no stocks.")

  sfm = xmile() |> build("a", "stock") |>
    build("b", "flow")
  expect_error(simulate(sfm |> sim_specs(language = "Julia")),
               "These flows are not connected to any stock:\\n- b")

  # With one stock and no flows and no parameters
  sfm = xmile() |> sim_specs(start = 0, stop = 10, dt = 0.1) |>
    build("A", "stock", eqn = "100")
  sim = expect_no_error(simulate(sfm |> sim_specs(language = "Julia"), only_stocks = FALSE))
  expect_equal(sort(names(sim$df)), c("time", "value", "variable"))
  expect_equal(unique(sim$df$variable), c("A"))

  # One stock with flows, other stock without flows
  sfm = xmile() |> sim_specs(start = 0, stop = 10, dt = 0.1) |>
    build(c("A", "B"), "stock", eqn = "100") |>
    build("C", "flow", eqn = "1", to = "A")
  sim = expect_no_error(simulate(sfm |> sim_specs(language = "Julia"), only_stocks = FALSE))
  expect_equal(sort(names(sim$df)), c("time", "value", "variable"))
  expect_equal(unique(sim$df$variable), c("A", "B", "C"))


  # With one intermediary -> error in constructing Dataframe before
  sfm = xmile() |> sim_specs(start = 0, stop = 10, dt = 0.1) |>
    build("A", "stock", eqn = "100") |>
    build("B", "flow", eqn = "1", to = "A") |>
    build("C", "aux", eqn = "B + 1")
  sim = expect_no_message(simulate(sfm |> sim_specs(language = "Julia"), only_stocks = FALSE))
  expect_equal(sort(names(sim$df)), c("time", "value", "variable"))
  expect_equal(unique(sim$df$variable), c("A", "B", "C"))

  # One intermediary variable that is also a stock, so it is removed -> does merging of df and intermediary_df still work?
  sfm = xmile() |> sim_specs(start = 0, stop = 10, dt = 0.1) |>
    build("A", "stock", eqn = "100") |>
    build("B", "flow", eqn = "delay(A, 5)", to = "A")
  sim = expect_no_message(simulate(sfm |> sim_specs(language = "Julia"), only_stocks = FALSE))
  expect_equal(sort(names(sim$df)), c("time", "value", "variable"))
  expect_equal(unique(sim$df$variable), c("A", "B"))

  # Stocks without flows
  sfm = xmile() |> sim_specs(start = 0, stop = 10, dt = 0.1) |>
    build("A", "stock", eqn = "100") |>
    build("B", "stock", eqn = "1") |>
    build("C", "aux", eqn = "B + 1")
  sim = expect_no_message(simulate(sfm |> sim_specs(language = "Julia"), only_stocks = FALSE))
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
  sfm = xmile("SIR")
  sim = simulate(sfm |> sim_specs(language = "Julia"), only_stocks = TRUE)
  expect_equal(length(unique(as.data.frame(sim)$variable)), length(names(sfm$model$variables$stock)))

  # All variables should be kept if only_stocks = FALSE
  sfm = xmile("SIR")
  sim = simulate(sfm |> sim_specs(language = "Julia"), only_stocks = FALSE)
  df = as.data.frame(sfm)
  df = df[df$type != "constant", ]
  expect_equal(length(unique(as.data.frame(sim)$variable)), length(df$name))


  # ** some have units

})



test_that("seed works", {

  # Without a seed, simulations shouldn't be the same
  sfm = xmile("predator-prey") |>
    sim_specs(seed = NULL) |>
    build(c("predator", "prey"), eqn = "runif(1, 20, 50)") |> sim_specs(language = "Julia")
  sim1 = simulate(sfm)
  sim2 = simulate(sfm)
  expect_equal(sim1$df$value[1] == sim2$df$value[1], FALSE)
  expect_equal(dplyr::last(sim1$df$value) == dplyr::last(sim2$df$value), FALSE)

  # With a seed, simulations should be the same
  sfm = sfm |> sim_specs(seed = 1)
  sim1 = simulate(sfm)
  sim2 = simulate(sfm)
  expect_equal(dplyr::last(sim1$df$value), dplyr::last(sim2$df$value))

})



test_that("units in stocks and flows", {

  # No unit specified in stock yet stock evaluates to unit
  sfm = xmile()  |> sim_specs(language = "Julia") |> build("a", "stock", eqn = "round(u('100.80 kilograms'))")
  expect_no_error(simulate(sfm))

  sfm = xmile()  |> sim_specs(language = "Julia") |> build("a", "stock", eqn = "round(u('108.67 seconds'))")
  expect_no_error(simulate(sfm))

})


test_that("function in aux still works", {

  sfm = xmile() |>
    sim_specs(language = "Julia", start = 0, stop = 10, dt = .1) |>
    build("A", "stock") |>
    build("input", "aux", eqn = "ramp(5, 10, -1)")
  sim = expect_no_error(simulate(sfm, only_stocks = FALSE))
  sim = expect_no_message(simulate(sfm, only_stocks = FALSE))

  # Check that input is not returned as a variable
  expect_equal(sort(unique(sim$df$variable)), c("A"))

  # Check with two intermediary variables
  sfm = sfm |>
    build("a2", "aux", eqn = " 0.38 + input(t)")
  sim = expect_no_error(simulate(sfm, only_stocks = FALSE))
  sim = expect_no_message(simulate(sfm, only_stocks = FALSE))

  # Check that input is not returned as a variable
  expect_equal(sort(unique(sim$df$variable)), c("A", "a2"))

})

