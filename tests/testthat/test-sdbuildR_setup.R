test_that("sdbuildR_setup() works", {

  # This error is only thrown if sdbuildR_setup() has not been run yet
  options("initialization_sdbuildR" = NULL)
  expect_error(sdbuildR_setup(JULIA_HOME = "doesnotexist"), "Location doesnotexist does not exist.")

  options("initialization_sdbuildR" = TRUE)
  expect_no_error(sdbuildR_setup())
  expect_no_warning(sdbuildR_setup())
  expect_no_message(sdbuildR_setup())

})
