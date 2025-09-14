test_that("find_dependencies works", {
  sfm <- xmile() |>
    build("a", "stock", eqn = "b + c") |>
    build(c("b", "c"), "flow")

  dep <- expect_no_error(expect_no_warning(expect_no_message(find_dependencies(sfm))))
  expect_equal(sort(names(dep)), letters[1:3])
  expect_equal(dep[["a"]], c("b", "c"))
  expect_equal(dep[["b"]], character(0))
  expect_equal(dep[["c"]], character(0))

  # Reverse dependencies
  dep <- expect_no_error(expect_no_warning(expect_no_message(find_dependencies(sfm, reverse = TRUE))))
  expect_equal(sort(names(dep)), letters[1:3])
  expect_equal(dep[["a"]], character(0))
  expect_equal(dep[["b"]], "a")
  expect_equal(dep[["c"]], "a")
})
