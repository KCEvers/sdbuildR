test_that("use_julia() works", {
  testthat::skip_on_cran()
  testthat::skip_if_not(JuliaConnectoR::juliaSetupOk())

  # # Set global package option to NULL to indicate use_julia() has not worked yet
  # .sdbuildR_env[[.sdbuildR_env[["P"]][["init_sdbuildR"]]]] = NULL
  #
  # .sdbuildR_env[[.sdbuildR_env[["P"]][["init_sdbuildR"]]]] = TRUE

  expect_no_error(expect_no_warning(use_julia(stop = TRUE)))
  expect_false(julia_setup_ok())

  expect_no_error(expect_no_warning(expect_message(use_julia())))

  expect_true(julia_setup_ok())
  expect_true(julia_init_ok())

  expect_no_error(use_julia(stop = TRUE))

  # This error is only thrown if use_julia() has not been run yet
  expect_error(
    use_julia(JULIA_HOME = "doesnotexist"),
    "Location doesnotexist does not exist"
  )
})
