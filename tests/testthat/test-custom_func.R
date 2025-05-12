
test_that("logit works", {
  expect_equal(logit(.5), 0)
})

test_that("expit works", {
  expect_equal(expit(0), 0.5)
})

test_that("rbool works", {
  expect_equal(rbool(0), FALSE)
  expect_equal(rbool(1), TRUE)
})


test_that("nonnegative works", {
  expect_equal(nonnegative(-10), 0)
})

test_that("convert_u works", {
  expect_equal(convert_u(1, u('s')), 1)
})
