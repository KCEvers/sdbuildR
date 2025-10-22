test_that("use_julia() works", {
  testthat::skip_on_cran()

  status <- expect_no_error(julia_status())
  expect_named(status,
               c("julia_found", "julia_path", "julia_version", "env_exists",
                 "env_instantiated", "status"),
               ignore.order = TRUE)

  testthat::skip_if_not(julia_status()$status == "ready")

  # Test installation
  expect_no_error(install_julia_env())
  expect_no_error(install_julia_env(remove = TRUE))
  expect_no_error(install_julia_env())

  expect_no_error(expect_no_warning(use_julia(stop = TRUE)))
  expect_false(julia_setup_ok())

  expect_no_error(expect_no_warning(expect_message(use_julia())))

  expect_true(julia_setup_ok())
  expect_true(julia_init_ok())

  expect_no_error(use_julia(stop = TRUE))

})
