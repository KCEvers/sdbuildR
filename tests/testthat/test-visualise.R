test_that("export_plot works", {
  skip_on_cran() # requires chrome

  # Ubuntu throws error because it cannot access chrome
  skip_on_os("linux")

  sfm = xmile("SIR") |> sim_specs(stop=10,dt=.1)

  # Plot sfm
  pl = plot(sfm)
  filepath = tempfile(fileext = ".png")
  expect_no_error(expect_no_message(expect_no_warning(export_plot(pl, filepath))))
  expect_true(file.exists(filepath))
  file.remove(filepath)

  # Plot simulation
  sim = simulate(sfm)
  pl = plot(sim)
  filepath = tempfile(fileext = ".png")

  expect_no_error(expect_no_message(expect_no_warning(export_plot(pl, filepath))))
  expect_true(file.exists(filepath))
  file.remove(filepath)

  filepath = "test"
  expect_error(export_plot(pl, filepath), "No file extension specified")

})


test_that("plot sfm", {

  # Empty models should throw error
  sfm0 <- xmile()
  expect_error(plot(sfm0), "Your model contains no variables")

  # A model with one variable should already be plottable
  sfm <- build(sfm0, "a", "constant")
  expect_no_error(expect_no_message(expect_no_warning(plot(sfm, show_constants = TRUE))))

  sfm <- build(sfm0, "a", "stock")
  expect_no_error(expect_no_message(expect_no_warning(plot(sfm))))

  sfm <- build(sfm0, "a", "aux")
  expect_no_error(expect_no_message(expect_no_warning(plot(sfm, show_aux = TRUE))))

  sfm <- build(sfm0, "a", "flow")
  expect_no_error(expect_no_message(expect_no_warning(plot(sfm))))

  # Test full models
  sfm = xmile("SIR")

  # Plot sfm
  expect_no_error(expect_no_message(expect_no_warning(plot(sfm))))

  # Try options
  expect_no_error(expect_no_message(expect_no_warning(plot(sfm, show_aux = FALSE, show_constants = TRUE, show_dependencies = FALSE))))

  expect_no_error(expect_no_message(expect_no_warning(plot(sfm, show_aux = TRUE, show_constants = TRUE, show_dependencies = FALSE))))

  expect_no_error(expect_no_message(expect_no_warning(plot(sfm, show_aux = FALSE, show_constants = TRUE, show_dependencies = TRUE))))

  expect_no_error(expect_no_message(expect_no_warning(plot(sfm, show_aux = FALSE, show_constants = FALSE, show_dependencies = FALSE))))

  expect_no_error(expect_no_message(expect_no_warning(plot(sfm, vars = "Susceptible"))))
  expect_no_error(expect_no_message(expect_no_warning(plot(sfm, vars = c("Infected", "Infection_Rate")))))


})


test_that("plot sim", {
  sfm = xmile("SIR") |> sim_specs(stop=10,dt=.1)

  # Plot simulation
  sim = simulate(sfm)

  # Try options
  expect_no_error(expect_no_message(expect_no_warning(plot(sim))))

  # vars
  expect_no_error(expect_no_message(expect_no_warning(plot(sim, vars = c() ))))
  expect_error(plot(sim, vars = c("a") ), "a is not a variable in the model")
  expect_error(plot(sim, vars = c(1) ), "vars must be a character vector")
  expect_no_error(expect_no_message(expect_no_warning(plot(sim, vars = c("Recovered") ))))

  expect_no_error(expect_no_message(expect_no_warning(plot(sim, xlim = c(20, 30), ylim = c(0, 1) ))))

})
