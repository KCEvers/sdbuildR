test_that("sdbuildR_setup() works", {
  expect_error(sdbuildR_setup(JULIA_HOME = "doesnotexist"), "Location doesnotexist does not exist.")

})
