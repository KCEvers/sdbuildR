test_that("use_julia() works", {

  # This error is only thrown if use_julia() has not been run yet
  options("initialization_sdbuildR" = NULL)
  expect_error(use_julia(JULIA_HOME = "doesnotexist"), "Location doesnotexist does not exist.")

  options("initialization_sdbuildR" = TRUE)
  expect_no_error(use_julia())
  expect_no_warning(use_julia())
  expect_no_message(use_julia())

  expect_no_error(use_julia(stop=TRUE))

})
