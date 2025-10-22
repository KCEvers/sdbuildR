test_that("parse_args() works", {
  expect_equal(
    parse_args("a, c(b, d), c"),
    c("a", "c(b, d)", "c")
  )
})


test_that("clean_name() works", {
  sfm <- xmile()
  names_df <- get_names(sfm)

  # Check for syntactically correct names
  expect_equal(clean_name(c("TRUE", "T"), names_df[["name"]]), c("TRUE__1", "T_1"))
  expect_equal(clean_name(c("a", "b", "T"), names_df[["name"]]), c("a", "b", "T_1"))
  expect_equal(clean_name(c("a-1", "b!2", "c.1"), names_df[["name"]]), c("a_1", "b_2", "c_1"))
  expect_equal(clean_name(c("a-1", "a!1"), names_df[["name"]]), c("a_1", "a_1_1"))
  expect_equal(clean_name(c(" Hell0 ", "Hell0"), names_df[["name"]]), c("Hell0", "Hell0_1"))

  # Difficult, but ensure unique names
  expect_equal(clean_name(c("F"), "F_1"), c("F_1_1"))
  expect_equal(clean_name(c("-1", "_1"), names_df[["name"]]), c("X_1", "X_1_1"))
})


test_that("clean_language works", {
  expect_equal(clean_language("r"), "R")
  expect_equal(clean_language(" r "), "R")
  expect_equal(clean_language("JULIA"), "Julia")
  expect_equal(clean_language(" julia"), "Julia")
  expect_equal(clean_language("jl"), "Julia")
  expect_error(clean_language("python"), "The language python is not one of the languages available in sdbuildR")
})


test_that("get_names() works", {
  # Check no variables
  expect_equal(get_names(xmile()), data.frame(type = character(), name = character(), label = character(), units = character()))

  # Check with variables
  sfm <- xmile() |>
    build("a", "aux") |>
    build("b", "aux")
  result <- get_names(sfm)
  expected <- data.frame(
    type = c("aux", "aux"),
    name = c("a", "b"),
    label = c("a", "b"),
    units = c("1", "1")
  )
  expect_equal(result, expected)

  # Check with units
  sfm <- xmile() |>
    build("a", "stock", units = "1/s") |>
    build("b", "aux", units = "m")
  result <- get_names(sfm)
  expected <- data.frame(
    type = c("stock", "aux"),
    name = c("a", "b"),
    label = c("a", "b"),
    units = c("1/s", "m")
  )
  expect_equal(result, expected)

  # Check with label
  sfm <- xmile() |>
    build("a", "stock", label = "A") |>
    build("b", "aux", label = "B")
  result <- get_names(sfm)
  expected <- data.frame(
    type = c("stock", "aux"),
    name = c("a", "b"),
    label = c("A", "B"),
    units = c("1", "1")
  )
  expect_equal(result, expected)
})


test_that("get_range_names() works", {
  expect_equal(
    get_range_names("[a]", "a", names_with_brackets = FALSE),
    data.frame(start = 2, end = 2, name = "a")
  )

  expect_equal(
    get_range_names("[a]", "a", names_with_brackets = TRUE),
    data.frame(start = 1, end = 3, name = "a")
  )

  # Brackets in middle of text
  expect_equal(
    get_range_names("b + a + c", "a", names_with_brackets = FALSE),
    data.frame(start = 5, end = 5, name = "a")
  )

  expect_equal(
    get_range_names("[b] + [a] + [c]", "a", names_with_brackets = TRUE),
    data.frame(start = 7, end = 9, name = "a")
  )

  # Multiple occurrences
  expect_equal(
    get_range_names("[a] + a", "a", names_with_brackets = FALSE),
    data.frame(start = c(2, 7), end = c(2, 7), name = c("a", "a"))
  )

  expect_equal(
    get_range_names("[a] + [a]", "a", names_with_brackets = TRUE),
    data.frame(start = c(1, 7), end = c(3, 9), name = c("a", "a"))
  )

  # Special characters in names
  expect_equal(
    get_range_names("a.2", "a.2", names_with_brackets = FALSE),
    data.frame(start = 1, end = 3, name = "a.2")
  )

  expect_equal(
    get_range_names("[a.2]", "a.2", names_with_brackets = TRUE),
    data.frame(start = 1, end = 5, name = "a.2")
  )

  # No matches
  expect_equal(
    get_range_names("b", "a", names_with_brackets = FALSE),
    data.frame()
  )

  expect_equal(
    get_range_names("[b]", "a", names_with_brackets = TRUE),
    data.frame()
  )

  # Embedded in alphanumeric (should NOT match)
  expect_equal(
    get_range_names("aaa", "a", names_with_brackets = FALSE),
    data.frame()
  )

  expect_equal(
    get_range_names("[aaa]", "a", names_with_brackets = TRUE),
    data.frame()
  )

  # At start/end of string
  expect_equal(
    get_range_names("a1 + no + a2", c("a1", "a2"), names_with_brackets = FALSE),
    data.frame(start = c(1, 11), end = c(2, 12), name = c("a1", "a2"))
  )

  expect_equal(
    get_range_names("[a1] + no + [a2]", c("a1", "a2"), names_with_brackets = TRUE),
    data.frame(start = c(1, 13), end = c(4, 16), name = c("a1", "a2"))
  )
})
