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

  sfm = xmile("SIR")

  # Plot sfm
  expect_no_error(expect_no_message(expect_no_warning(plot(sfm))))

  # Try options
  expect_no_error(expect_no_message(expect_no_warning(plot(sfm, show_aux = FALSE, show_constants = TRUE, show_dependencies = FALSE))))

  expect_no_error(expect_no_message(expect_no_warning(plot(sfm, show_aux = TRUE, show_constants = TRUE, show_dependencies = FALSE))))

  expect_no_error(expect_no_message(expect_no_warning(plot(sfm, show_aux = FALSE, show_constants = TRUE, show_dependencies = TRUE))))

  expect_no_error(expect_no_message(expect_no_warning(plot(sfm, show_aux = FALSE, show_constants = FALSE, show_dependencies = FALSE))))

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
