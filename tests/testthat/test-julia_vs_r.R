test_that("compare output Julia and R for templates", {
  testthat::skip_on_cran()
  testthat::skip_if_not(JuliaConnectoR::juliaSetupOk())

  sfm <- xmile("SIR") |> sim_specs(dt = 0.1, save_at = 1, stop = 10)
  sim1 <- simulate(sfm |> sim_specs(language = "R"))
  sim2 <- simulate(sfm |> sim_specs(language = "Julia"))
  comp <- compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)
  expect_equal(sim1$success, TRUE)
  expect_equal(nrow(sim1$df) > 0, TRUE)
  expect_equal(sim2$success, TRUE)
  expect_equal(nrow(sim2$df) > 0, TRUE)

  sim1 <- simulate(sfm |> sim_specs(language = "R"), only_stocks = TRUE)
  sim2 <- simulate(sfm |> sim_specs(language = "Julia"), only_stocks = TRUE)
  comp <- compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)
  expect_equal(sim1$success, TRUE)
  expect_equal(nrow(sim1$df) > 0, TRUE)
  expect_equal(sim2$success, TRUE)
  expect_equal(nrow(sim2$df) > 0, TRUE)

  sfm <- xmile("predator_prey") |> sim_specs(dt = 0.1, save_at = 1, stop = 10)
  sim1 <- simulate(sfm |> sim_specs(language = "R"))
  sim2 <- simulate(sfm |> sim_specs(language = "Julia"))
  comp <- compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)
  expect_equal(sim1$success, TRUE)
  expect_equal(nrow(sim1$df) > 0, TRUE)
  expect_equal(sim2$success, TRUE)
  expect_equal(nrow(sim2$df) > 0, TRUE)

  sim1 <- simulate(sfm |> sim_specs(language = "R"), only_stocks = TRUE)
  sim2 <- simulate(sfm |> sim_specs(language = "Julia"), only_stocks = TRUE)
  comp <- compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)
  expect_equal(sim1$success, TRUE)
  expect_equal(nrow(sim1$df) > 0, TRUE)
  expect_equal(sim2$success, TRUE)
  expect_equal(nrow(sim2$df) > 0, TRUE)

  sfm <- xmile("logistic_model") |>
    sim_specs(dt = 0.1, save_at = 10, stop = 100)
  sim1 <- simulate(sfm |> sim_specs(language = "R"))
  sim2 <- simulate(sfm |> sim_specs(language = "Julia"))
  comp <- compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)
  expect_equal(sim1$success, TRUE)
  expect_equal(nrow(sim1$df) > 0, TRUE)
  expect_equal(sim2$success, TRUE)
  expect_equal(nrow(sim2$df) > 0, TRUE)

  # Check whether the population converges to the carrying capacity
  expect_equal(dplyr::last(sim1$df[sim1$df$variable == "X", "value"]),
    sim1$constants[["K"]],
    tolerance = .01
  )
  expect_equal(dplyr::last(sim2$df[sim2$df$variable == "X", "value"]),
    sim2$constants[["K"]],
    tolerance = .01
  )

  sim1 <- simulate(sfm |> sim_specs(language = "R"), only_stocks = TRUE)
  sim2 <- simulate(sfm |> sim_specs(language = "Julia"), only_stocks = TRUE)
  comp <- compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)
  expect_equal(sim1$success, TRUE)
  expect_equal(nrow(sim1$df) > 0, TRUE)
  expect_equal(sim2$success, TRUE)
  expect_equal(nrow(sim2$df) > 0, TRUE)

  sfm <- xmile("Crielaard2022") |>
    sim_specs(dt = 0.1, save_at = 1, stop = 10) |>
    # Update initial condition to be non-stochastic
    build(c("Food_intake", "Hunger", "Compensatory_behaviour"),
      eqn = round(runif(3), 8)
    )

  sim1 <- simulate(sfm |> sim_specs(language = "R"))
  sim2 <- simulate(sfm |> sim_specs(language = "Julia"))
  comp <- compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)
  expect_equal(sim1$success, TRUE)
  expect_equal(nrow(sim1$df) > 0, TRUE)
  expect_equal(sim2$success, TRUE)
  expect_equal(nrow(sim2$df) > 0, TRUE)

  sim1 <- simulate(sfm |> sim_specs(language = "R"), only_stocks = TRUE)
  sim2 <- simulate(sfm |> sim_specs(language = "Julia"), only_stocks = TRUE)
  comp <- compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)
  expect_equal(sim1$success, TRUE)
  expect_equal(nrow(sim1$df) > 0, TRUE)
  expect_equal(sim2$success, TRUE)
  expect_equal(nrow(sim2$df) > 0, TRUE)

  # Duffing previously had an error with cos()
  sfm <- xmile("Duffing") |> sim_specs(dt = 0.1, save_at = 1, stop = 10)
  sim1 <- simulate(sfm |> sim_specs(language = "R"))
  sim2 <- simulate(sfm |> sim_specs(language = "Julia"))
  comp <- compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)
  expect_equal(sim1$success, TRUE)
  expect_equal(nrow(sim1$df) > 0, TRUE)
  expect_equal(sim2$success, TRUE)
  expect_equal(nrow(sim2$df) > 0, TRUE)

  sim1 <- simulate(sfm |> sim_specs(language = "R"), only_stocks = TRUE)
  sim2 <- simulate(sfm |> sim_specs(language = "Julia"), only_stocks = TRUE)
  comp <- compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)
  expect_equal(sim1$success, TRUE)
  expect_equal(nrow(sim1$df) > 0, TRUE)
  expect_equal(sim2$success, TRUE)
  expect_equal(nrow(sim2$df) > 0, TRUE)

  sfm <- xmile("Chua") |> sim_specs(dt = 0.1, save_at = 1, stop = 10)
  sim1 <- simulate(sfm |> sim_specs(language = "R"))
  sim2 <- simulate(sfm |> sim_specs(language = "Julia"))
  comp <- compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)
  expect_equal(sim1$success, TRUE)
  expect_equal(nrow(sim1$df) > 0, TRUE)
  expect_equal(sim2$success, TRUE)
  expect_equal(nrow(sim2$df) > 0, TRUE)

  sim1 <- simulate(sfm |> sim_specs(language = "R"), only_stocks = TRUE)
  sim2 <- simulate(sfm |> sim_specs(language = "Julia"), only_stocks = TRUE)
  comp <- compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)
  expect_equal(sim1$success, TRUE)
  expect_equal(nrow(sim1$df) > 0, TRUE)
  expect_equal(sim2$success, TRUE)
  expect_equal(nrow(sim2$df) > 0, TRUE)

  # Check whether coffee cup reaches room temperature
  sfm <- xmile("coffee_cup") |> sim_specs(
    dt = 0.1, save_at = 10, stop = 100,
    language = "Julia"
  )
  sim1 <- simulate(sfm)
  expect_equal(sim1$success, TRUE)
  expect_equal(nrow(sim1$df) > 0, TRUE)
  expect_equal(dplyr::last(sim1$df[sim1$df$variable == "coffee_temperature", "value"]), sim1$constants[["room_temperature"]], tolerance = .01)

  # Can't be simulated in R, already tested in compile_r
})



test_that("as.data.frame(sim) works", {
  sfm <- xmile("SIR") |> sim_specs(dt = 0.1, save_at = 1, stop = 10)

  sim <- simulate(sfm |> sim_specs(language = "R"))
  expect_equal(class(as.data.frame(sim)), "data.frame")
  expect_equal(nrow(as.data.frame(sim)) > 0, TRUE)

  df <- expect_no_error(as.data.frame(sim, direction = "wide"))
  expect_equal(sort(colnames(df)), c("Infected", "Recovered", "Susceptible", "time"))

  testthat::skip_on_cran()
  testthat::skip_if_not(JuliaConnectoR::juliaSetupOk())
  sim <- simulate(sfm |> sim_specs(language = "Julia"), only_stocks = TRUE)
  expect_equal(class(as.data.frame(sim)), "data.frame")
  expect_equal(nrow(as.data.frame(sim)) > 0, TRUE)
})


test_that("translating solvers works", {
  # Translate solvers
  expect_equal(solvers("euler", from = "R", to = "Julia", show_info = FALSE), "Euler()")
  expect_equal(solvers("rk4", from = "R", to = "Julia", show_info = FALSE), "RK4()")
  expect_equal(solvers("Tsit5()", from = "Julia", to = "R", show_info = FALSE), "rk45dp6")

  # Check whether solver exists
  expect_equal(solvers("euler", from = "R"), "euler")
  expect_error(solvers("Tsit5()", from = "R"), "Method Tsit5\\(\\) is not found in deSolve methods")
  expect_equal(solvers("Tsit5()", from = "Julia"), "Tsit5()")
  expect_equal(solvers("euler", from = "Julia"), "Euler()")

  # Name all solvers
  expect_error(solvers(), "Either method or from must be specified")
  expect_no_error(solvers(from = "R"))
  expect_no_error(solvers(from = "Julia"))
})
