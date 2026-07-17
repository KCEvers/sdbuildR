# ==============================================================================
# test-verify.R: Unit tests for the verify / unit_test system
# ==============================================================================

test_that("new_stockflow() initialises unit_tests as empty list", {
  sfm <- stockflow()
  expect_true("unit_tests" %in% names(sfm))
  expect_true(is.list(sfm[["unit_tests"]]))
  expect_equal(length(sfm[["unit_tests"]]), 0L)
})


# ==============================================================================
# unit_test() — building tests
# ==============================================================================

test_that("unit_test() adds a test to the list", {
    skip_on_cran()
sfm <- make_verifiable_sfm() |>
    unit_test(label = "S is non-negative", expr = all(S >= 0))

  expect_equal(length(sfm[["unit_tests"]]), 1L)
  expect_equal(sfm[["unit_tests"]][[1]][["label"]], "S is non-negative")
  expect_equal(sfm[["unit_tests"]][[1]][["expr_str"]], "all(S >= 0)")
  expect_equal(sfm[["unit_tests"]][[1]][["conditions"]], list())
  expect_true(sfm[["unit_tests"]][[1]][["active"]])

  # Still returns the full stockflow object
  expect_s3_class(sfm, "stockflow")
  expect_equal(length(sfm[["unit_tests"]]), 1L)
})

test_that("unit_test() auto-generates label from expression", {
    skip_on_cran()
sfm <- make_verifiable_sfm() |>
    unit_test(expr = all(S >= 0))

  # A human-readable label should be generated (not the raw deparse)
  labels <- vapply(sfm[["unit_tests"]], function(t) t[["label"]], character(1))
  expect_equal(length(labels), 1L)
  expect_match(labels[[1]], "S is at least 0")
})


test_that("unit_test() auto-generates unique labels for similar expressions", {
   skip_on_cran()
 sfm <- make_verifiable_sfm() |>
    unit_test(expr = mean(S >= 0)) |>
    unit_test(expr = mean(S) >= 0)

  labels <- vapply(sfm[["unit_tests"]], function(t) t[["label"]], character(1))
  expect_equal(length(labels), 2L)
  expect_match(labels[[1]], "mean of \\(S is at least 0\\)")
  expect_match(labels[[2]], "mean of S is at least 0")
})

test_that("unit_test() upserts: same label replaces existing test", {
   skip_on_cran()
 sfm <- make_verifiable_sfm() |>
    unit_test(label = "test A", expr = all(S >= 0)) |>
    unit_test(label = "test A", expr = all(S > 0)) # overwrite

  expect_equal(length(sfm[["unit_tests"]]), 1L)
  expect_equal(sfm[["unit_tests"]][[1]][["expr_str"]], "all(S > 0)")
})

test_that("unit_test() upserts: same test replaces existing test", {
   skip_on_cran()
 sfm <- make_verifiable_sfm() |>
    unit_test(label = "test A", expr = all(S >= 0)) |>
    unit_test(test = 1, expr = all(S > 0)) # overwrite

  expect_equal(length(sfm[["unit_tests"]]), 1L)
  expect_equal(sfm[["unit_tests"]][[1]][["expr_str"]], "all(S > 0)")
})

test_that("unit_test() stores conditions correctly", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(
      label = "at zero rate, S is constant",
      expr = all(diff(S) == 0),
      conditions = list(rate = 0)
    )

  conds <- sfm[["unit_tests"]][[1]][["conditions"]]
  expect_equal(conds, list(rate = 0))
})

test_that("unit_test() respects active = FALSE", {

  skip_on_cran()

  sfm <- make_verifiable_sfm() |>
    unit_test(label = "always skipped", expr = FALSE, active = FALSE)

  expect_false(sfm[["unit_tests"]][[1]][["active"]])
  v <- verify(sfm)
  expect_equal(length(v[["results"]]), 1L)
  expect_equal(v[["results"]][[1]][["status"]], "skip")
})

test_that("unit_test() allows base-R symbols in expr (e.g., Inf, pi)", {
  skip_on_cran()
  sfm <- make_verifiable_sfm()
  expect_no_error(unit_test(sfm, label = "x", expr = all(S < Inf)))
})

test_that("unit_test() errors appropriately", {
    skip_on_cran()

  sfm <- make_verifiable_sfm()

  # on invalid syntax when expr is character
  expect_error(
    unit_test(sfm, label = "bad syntax", expr = "all(S >= 0 & )"),
    regexp = "invalid R syntax|Failed to parse"
  )

# unit_test() errors on bad conditions (unknown name)
  expect_error(
    unit_test(sfm, label = "x", expr = all(S >= 0), conditions = list(nonexistent = 1)),
    regexp = "not found as stocks or constants"
  )

# unit_test() errors on unnamed conditions
  expect_error(
    unit_test(sfm, label = "x", expr = all(S >= 0), conditions = list(1)),
    regexp = "must be named"
  )

# unit_test() errors when expr references undefined variable
  expect_error(
    unit_test(sfm, label = "x", expr = all(Nonexistent >= 0)),
    regexp = "not found in model"
  )

# unit_test() errors when character expr contains multiple expressions
  expect_error(
    unit_test(sfm, label = "multi expr", expr = "all(S >= 0); all(S < 200)"),
    regexp = "exactly one expression"
  )

# unit_test() requires explicit label when auto-generated label collides
  sfm1 <- make_verifiable_sfm() |>
    unit_test(expr = all(S >= 0))

  expect_error(
    unit_test(sfm1, expr = all(S >= 0)),
    regexp = "Auto-generated label.*already exists|identical expression already exists"
  )
})

# ==============================================================================
# discard_unit_test()
# ==============================================================================

test_that("discard_unit_test() removes by label", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "test A", expr = all(S >= 0)) |>
    unit_test(label = "test B", expr = all(S < 200)) |>
    discard_unit_test("test A")

  remaining_labels <- vapply(sfm[["unit_tests"]], function(t) t[["label"]], character(1))
  expect_equal(length(sfm[["unit_tests"]]), 1L)
  expect_false("test A" %in% remaining_labels)
  expect_true("test B" %in% remaining_labels)
})

test_that("discard_unit_test() removes by integer index", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "test A", expr = all(S >= 0)) |>
    unit_test(label = "test B", expr = all(S < 200)) |>
    discard_unit_test(1L)

  remaining_labels <- vapply(sfm[["unit_tests"]], function(t) t[["label"]], character(1))
  expect_equal(length(sfm[["unit_tests"]]), 1L)
  expect_false("test A" %in% remaining_labels)
  expect_true("test B" %in% remaining_labels)
})

test_that("discard_unit_test() warns when label not found", {
  skip_on_cran()
  sfm <- make_verifiable_sfm()
  expect_error(
    discard_unit_test(sfm, "does not exist"),
    regexp = "not found"
  )
})

test_that("discard_unit_test() warns when index out of range", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "only test", expr = all(S >= 0))
  expect_error(
    discard_unit_test(sfm, 5L),
    regexp = "out of range"
  )
})


# ==============================================================================
# unit_tests() display
# ==============================================================================

test_that("unit_tests() returns unit_tests_stockflow with correct count", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "a", expr = all(S >= 0)) |>
    unit_test(label = "b", expr = all(S < 200))

  result <- unit_tests(sfm)
  expect_s3_class(result, "unit_tests_stockflow")
  expect_equal(result$n, 2L)
})

test_that("unit_tests() prints without error for empty model", {
  skip_on_cran()
  sfm <- make_verifiable_sfm()
  expect_no_error(print(unit_tests(sfm)))
})

test_that("unit_tests() snapshot for defined tests", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S is non-negative", expr = all(S >= 0)) |>
    unit_test(label = "inactive test", expr = FALSE, active = FALSE)

  expect_snapshot(print(unit_tests(sfm)))
})


# ==============================================================================
# verify.stockflow()
# ==============================================================================

test_that("verify.stockflow() returns verify_stockflow class", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S is non-negative", expr = all(S >= 0))

  result <- silence(verify(sfm))
  expect_s3_class(result, "verify_stockflow")

#  verify.stockflow() passes a correct test
  expect_equal(result[["results"]][[1]][["status"]], "pass")

  # verify result includes expr_str, conditions, and outcome for passing test
  res_entry <- result[["results"]][[1]]

  expect_equal(res_entry[["expr_str"]], "all(S >= 0)")
  expect_equal(res_entry[["conditions"]], list())
  expect_equal(res_entry[["outcome"]], TRUE)
  expect_equal(res_entry[["status"]], "pass")
})

test_that("verify.stockflow() fails an incorrect test", {

  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S is always zero", expr = all(S == 0))

  result <- silence(verify(sfm))
  expect_equal(result[["results"]][[1]][["status"]], "fail")

#verify result includes outcome = FALSE for failing test
  res_entry <- result[["results"]][[1]]

  expect_equal(res_entry[["expr_str"]], "all(S == 0)")
  expect_equal(res_entry[["conditions"]], list())
  expect_equal(res_entry[["outcome"]], FALSE)
  expect_equal(res_entry[["status"]], "fail")
})

test_that("verify.stockflow() fails FALSE character expression", {

  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "char false", expr = "all(S == 0)")

  result <- silence(verify(sfm))
  expect_equal(result[["results"]][[1]][["status"]], "fail")
})

test_that("verify.stockflow() errors if expression returns numeric scalar", {

  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "numeric output", expr = mean(S > 0.2))

  result <- silence(verify(sfm))
  expect_equal(result[["results"]][[1]][["status"]], "error")
  expect_match(result[["results"]][[1]][["message"]], "logical scalar")

# verify result includes outcome and conditions for error status
  res_entry <- result[["results"]][[1]]

  expect_equal(res_entry[["expr_str"]], "mean(S > 0.2)")
  expect_equal(res_entry[["conditions"]], list())
  expect_null(res_entry[["outcome"]])
  expect_equal(res_entry[["status"]], "error")
})

test_that("verify.stockflow() errors if expression returns logical vector", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "vector output", expr = S > 0)

  result <- silence(verify(sfm))
  expect_equal(result[["results"]][[1]][["status"]], "error")
  expect_match(result[["results"]][[1]][["message"]], "logical scalar")
})


test_that("verify.stockflow() works with conditions", {

  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(
      label      = "at zero rate, S does not decrease",
      expr       = all(diff(S) >= -1e-10), # essentially constant
      conditions = list(rate = 0)
    )

  result <- silence(verify(sfm))
  res_entry <- result[["results"]][[1]]

  expect_equal(res_entry[["expr_str"]], "all(diff(S) >= -1e-10)")
  expect_equal(res_entry[["conditions"]], list(rate = 0))
  expect_equal(res_entry[["outcome"]], TRUE)
  expect_equal(res_entry[["status"]], "pass")
})

test_that("verify.stockflow() errors when no tests defined", {
  skip_on_cran()
  sfm <- make_verifiable_sfm()
  expect_error(verify(sfm), regexp = "No unit tests")
})


# --- sims always retained ---

test_that("verify() always returns non-NULL sims (no save_sims needed)", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |> unit_test(expr = all(S >= 0))
  result <- silence(verify(sfm))
  expect_false(is.null(result[["sims"]]))
  # condition is always populated
  expect_false(is.null(result[["condition"]]))
})

test_that("verify() returns sims as nested list and j as named int vector", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S non-negative", expr = all(S >= 0))
  result <- silence(verify(sfm))
  expect_true(is.list(result[["sims"]]))
  expect_null(names(result[["sims"]]))
  expect_equal(length(result[["sims"]]), 1L)
  # sims[[j]][[i]] structure
  expect_true(is.list(result[["sims"]][[1]]))
  expect_s3_class(result[["sims"]][[1]], "simulate_stockflow")
  expect_equal(result[["condition"]], c("S non-negative" = 1L))
})

test_that("verify() deduplicates sims: two tests sharing conditions map to the same index", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S non-negative", expr = all(S >= 0)) |>
    unit_test(label = "S ends positive", expr = tail(S, 1) > 0) |>
    unit_test(
      label = "S constant at zero rate",
      expr = all(diff(S) == 0),
      conditions = list(rate = 0)
    )
  result <- silence(verify(sfm))
  # Two unique condition sets -> two sim objects
  expect_equal(length(result[["sims"]]), 2L)
  # Three tests, each mapped to a condition index
  expect_equal(length(result[["condition"]]), 3L)
  expect_setequal(
    names(result[["condition"]]),
    c("S non-negative", "S ends positive", "S constant at zero rate")
  )
  # The two baseline tests share the same index
  expect_equal(
    result[["condition"]][["S non-negative"]],
    result[["condition"]][["S ends positive"]]
  )
  # The condition test has a different index
  expect_false(
    result[["condition"]][["S non-negative"]] ==
      result[["condition"]][["S constant at zero rate"]]
  )
  # All sims are nested simulate_stockflow objects
  for (sim_list in result[["sims"]]) {
    expect_true(is.list(sim_list))
    expect_s3_class(sim_list, "simulate_stockflow")
  }
})


test_that("as.data.frame.verify_stockflow returns a data frame with expected columns", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S non-negative", expr = all(S >= 0))
  result <- silence(verify(sfm))
  df <- as.data.frame(result)
  expect_s3_class(df, "data.frame")
  expect_true(all(c("test", "label", "status") %in% names(df)))
  expect_equal(nrow(df), 1L)
})



test_that("as.data.frame.verify_stockflow test filter works", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S non-negative", expr = all(S >= 0)) |>
    unit_test(
      label = "S constant at zero rate",
      expr = all(diff(S) == 0),
      conditions = list(rate = 0)
    )
  result <- silence(verify(sfm))
  df_nr1 <- as.data.frame(result, test = 1L)
  expect_equal(nrow(df_nr1), 1L)
  expect_equal(df_nr1[["label"]], "S non-negative")
})

test_that("as.data.frame.verify_stockflow sims test display shows only requested numbers", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "A", expr = all(S >= 0)) |>
    unit_test(label = "B", expr = all(S > 0))

  result <- silence(verify(sfm))

  df_all <- as.data.frame(result, which = "sims")
  expect_true(all(df_all[["test"]] == "1, 2"))

  df_nr1 <- as.data.frame(result, which = "sims", test = 1L)
  expect_true(all(df_nr1[["test"]] == "1"))
})

test_that("head.verify_stockflow and tail.verify_stockflow return data frames", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S non-negative", expr = all(S >= 0))
  result <- silence(verify(sfm))
  expect_s3_class(head(result), "data.frame")
  expect_s3_class(tail(result), "data.frame")
})

test_that("verify result for passing test includes error_type = NA", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S is non-negative", expr = all(S >= 0))

  result <- silence(verify(sfm))
  res_entry <- result[["results"]][[1]]

  expect_true(is.na(res_entry[["error_type"]]))
  expect_equal(res_entry[["status"]], "pass")
})

test_that("verify result for failing test includes error_type = NA", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S is always zero", expr = all(S == 0))

  result <- silence(verify(sfm))
  res_entry <- result[["results"]][[1]]

  expect_true(is.na(res_entry[["error_type"]]))
  expect_equal(res_entry[["status"]], "fail")
})

test_that("error_type = 'expr_syntax' when expression has parse error", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "syntax error", expr = all(S >= 0)) # Will be modified after

  # Manually inject a broken expression that fails parsing
  sfm[["unit_tests"]][[1]][["expr_str"]] <- "all(S >= 0" # missing closing paren

  result <- silence(verify(sfm))
  res_entry <- result[["results"]][[1]]

  expect_equal(res_entry[["error_type"]], "expr_syntax")
  expect_equal(res_entry[["status"]], "error")
  expect_match(res_entry[["message"]], "Could not parse")
})

test_that("error_type = 'expr_result' when expression returns numeric instead of logical", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "numeric output", expr = mean(S > 0.2))

  result <- silence(verify(sfm))
  res_entry <- result[["results"]][[1]]

  expect_equal(res_entry[["error_type"]], "expr_result")
  expect_equal(res_entry[["status"]], "error")
  expect_match(res_entry[["message"]], "logical scalar")
})

test_that("error_type = 'expr_result' when expression returns logical vector instead of scalar", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "vector output", expr = S > 0)

  result <- silence(verify(sfm))
  res_entry <- result[["results"]][[1]]

  expect_equal(res_entry[["error_type"]], "expr_result")
  expect_equal(res_entry[["status"]], "error")
  expect_match(res_entry[["message"]], "logical scalar")
})

test_that("status = 'fail' with informative message when expression returns NA", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "na output", expr = NA)

  result <- silence(verify(sfm))
  res_entry <- result[["results"]][[1]]

  expect_equal(res_entry[["status"]], "fail")
  expect_true(is.na(res_entry[["error_type"]]))
  expect_match(res_entry[["message"]], "returned NA")
})

test_that("status = 'fail' with informative message when expression returns Inf", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "inf output", expr = Inf)

  result <- silence(verify(sfm))
  res_entry <- result[["results"]][[1]]

  expect_equal(res_entry[["status"]], "fail")
  expect_true(is.na(res_entry[["error_type"]]))
  expect_match(res_entry[["message"]], "Inf|NaN")
})

test_that("status = 'fail' with informative message when expression returns NaN", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "nan output", expr = NaN)

  result <- silence(verify(sfm))
  res_entry <- result[["results"]][[1]]

  expect_equal(res_entry[["status"]], "fail")
  expect_true(is.na(res_entry[["error_type"]]))
  expect_match(res_entry[["message"]], "Inf|NaN")
})

test_that("error_type = 'expr_eval' when expression references undefined variable", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "undefined var", expr = all(S >= 0)) # Will be modified

  # Inject an expression that references undefined variable
  sfm[["unit_tests"]][[1]][["expr_str"]] <- "all(undefined_var >= 0)"

  result <- silence(verify(sfm))
  res_entry <- result[["results"]][[1]]

  expect_equal(res_entry[["error_type"]], "expr_eval")
  expect_equal(res_entry[["status"]], "error")
  expect_match(res_entry[["message"]], "not found|undefined")
})

test_that("verify() test parameter runs only specified tests", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "A", expr = all(S >= 0)) |>
    unit_test(label = "B", expr = all(S == 0))
  result <- silence(verify(sfm, test = 1L))
  expect_equal(length(result[["results"]]), 1L)
  expect_equal(result[["test_indices"]], 1L)
  df <- as.data.frame(result)
  expect_equal(df$test, 1L)
})

test_that("as.data.frame test_indices preserved correctly", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "A", expr = all(S >= 0)) |>
    unit_test(label = "B", expr = all(S > 0))
  result <- silence(verify(sfm, test = 2L))
  df <- as.data.frame(result)
  expect_equal(df$test, 2L)
})

test_that("unit_tests() label partial match works", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S non-negative", expr = all(S >= 0)) |>
    unit_test(label = "S constant", expr = all(diff(S) == 0))
  res <- unit_tests(sfm, label = "non-neg")
  expect_equal(res$n, 1L)
  expect_equal(res$indices, 1L)
})

test_that("unit_tests() label vector matches either (OR)", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S non-negative", expr = all(S >= 0)) |>
    unit_test(label = "S constant", expr = all(diff(S) == 0))
  res <- unit_tests(sfm, label = c("non-neg", "constant"))
  expect_equal(res$n, 2L)
})

test_that("unit_tests() label is case-insensitive by default", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S Non-Negative", expr = all(S >= 0))
  res <- unit_tests(sfm, label = "non-negative")
  expect_equal(res$n, 1L)

# unit_tests() label ignore_case = FALSE respects case
  expect_warning(unit_tests(sfm, label = "non-negative", ignore_case = FALSE),
    regexp = "No tests matched"
  )

# unit_tests() label warns on no match
  expect_warning(unit_tests(sfm, label = "xyz_no_match"), regexp = "No tests matched")
})

test_that("unit_tests() test and label combine as intersection", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S non-negative", expr = all(S >= 0)) |>
    unit_test(label = "S constant", expr = all(diff(S) == 0))
  res <- unit_tests(sfm, test = 1:2, label = "non-neg")
  expect_equal(res$n, 1L)
  expect_equal(res$indices, 1L)
})

test_that("as.data.frame label filter works", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S non-negative", expr = all(S >= 0)) |>
    unit_test(label = "S constant", expr = all(diff(S) == 0))
  result <- silence(verify(sfm))
  df <- as.data.frame(result, label = "non-neg")
  expect_equal(nrow(df), 1L)
  expect_equal(df$label, "S non-negative")
})

test_that("as.data.frame label vector matches either", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S non-negative", expr = all(S >= 0)) |>
    unit_test(label = "S constant", expr = all(diff(S) == 0))
  result <- silence(verify(sfm))
  df <- as.data.frame(result, label = c("non-neg", "constant"))
  expect_equal(nrow(df), 2L)
})

test_that("as.data.frame test and label combine (intersection)", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S non-negative", expr = all(S >= 0)) |>
    unit_test(label = "S constant", expr = all(diff(S) == 0))
  result <- silence(verify(sfm))
  df <- as.data.frame(result, test = 1:2, label = "non-neg")
  expect_equal(nrow(df), 1L)
  expect_equal(df$test, 1L)
})

test_that("as.data.frame label errors no match", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S non-negative", expr = all(S >= 0))
  result <- silence(verify(sfm))
  expect_error(as.data.frame(result, label = "xyz"), regexp = "No results matched")
})


test_that("verify().results always includes error_type field", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "pass test", expr = all(S >= 0)) |>
    unit_test(label = "fail test", expr = all(S == 0)) |>
    unit_test(label = "type error", expr = mean(S > 0.2), active = FALSE)

  result <- silence(verify(sfm))

  for (res_entry in result[["results"]]) {
    expect_true("error_type" %in% names(res_entry),
      info = paste("Missing error_type in result:", res_entry[["label"]])
    )
  }
})


# ==============================================================================
# Snapshot tests
# ==============================================================================

test_that("print.verify_stockflow() snapshot for passing tests", {
  sfm <- make_verifiable_sfm() |>
    update(S, eqn = 100) |>
    unit_test(label = "S is non-negative", expr = all(S >= 0)) |>
    unit_test(label = "S starts at 100", expr = expect_equal(S[[1]], 100))

  result <- silence(verify(sfm))
  expect_snapshot(print(result))
})

test_that("print.verify_stockflow() snapshot for failing FALSE tests", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S is zero", expr = all(S == 0))

  result <- silence(verify(sfm))
  expect_snapshot(print(result))
})


# ==============================================================================
# discard() cascade — unit test clean-up
# ==============================================================================

test_that("discard() removes unit test that only references the discarded variable", {
  
  skip_on_cran()
  
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S only", expr = all(S >= 0))

  expect_warning(expect_warning(
    expect_warning(
      sfm <- discard(sfm, "S"), "Removed 1 unit test"
    ),
    "lingering reference"
  ), "lingering reference")
  expect_equal(length(sfm[["unit_tests"]]), 0L)
})

test_that("discard() keeps unit test that references other variables too", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S and rate", expr = all(S >= 0) && rate > 0)

  # drain is not referenced by the test — test kept unchanged
  sfm <- suppressWarnings(discard(sfm, "drain"))
  expect_equal(length(sfm[["unit_tests"]]), 1L)
})

test_that("discard() warns when removing a unit test", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S only", expr = all(S >= 0))

  expect_warning(expect_warning(expect_warning(
    discard(sfm, "S"), "Removed 1 unit test"
  ), "lingering reference"), "lingering reference")
})

test_that("discard() strips removed variable from conditions", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S non-neg", expr = all(S >= 0), conditions = list(rate = 0))

  expect_warning(expect_warning(sfm <- discard(sfm, "rate"), regexp = "Removed.*rate.*conditions"), "lingering reference")

  # rate removed from conditions; test itself kept (S still exists)
  expect_equal(length(sfm[["unit_tests"]]), 1L)
  expect_equal(sfm[["unit_tests"]][[1]][["conditions"]], list())
})

test_that("discard() warns about lingering expr reference when multiple vars in expr", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "both vars", expr = all(S >= 0) && drain > 0)

  # Remove drain — test kept (S also referenced) but should warn about lingering ref to drain
  expect_warning(discard(sfm, "drain"), regexp = "\\[1\\].*still references")
})


# ==============================================================================
# test_deps cache
# ==============================================================================

test_that("unit_test() populates assemble$unit_tests$deps", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S non-neg", expr = all(S >= 0), conditions = list(rate = 0))

  deps <- sfm[["assemble"]][["unit_tests"]][["deps"]]
  expect_false(is.null(deps))
  expect_equal(length(deps), 1L)
  expect_equal(deps[[1]][["expr_refs"]], "S")
  expect_equal(deps[[1]][["cond_refs"]], "rate")
})

test_that("unit_test() updates deps when modifying a test", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "test", expr = all(S >= 0)) |>
    unit_test(label = "test", expr = all(drain >= 0)) # overwrite

  deps <- sfm[["assemble"]][["unit_tests"]][["deps"]]
  expect_equal(deps[[1]][["expr_refs"]], "drain")
})

test_that("discard_unit_test() invalidates test deps cache", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "test A", expr = all(S >= 0)) |>
    discard_unit_test("test A")

  # Cache should be invalidated (NULL) after removal
  expect_null(sfm[["assemble"]][["unit_tests"]][["deps"]])
})

test_that("get_test_deps() lazily recomputes when cache is NULL", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S non-neg", expr = all(S >= 0))

  # Manually invalidate
  sfm[["assemble"]][["unit_tests"]][["deps"]] <- NULL

  td <- get_test_deps(sfm)
  expect_false(is.null(td[["deps"]]))
  expect_equal(td[["deps"]][[1]][["expr_refs"]], "S")
  # Object should now have cached deps
  expect_false(is.null(td[["object"]][["assemble"]][["unit_tests"]][["deps"]]))
})


# ==============================================================================
# unit_test() — additional edge cases
# ==============================================================================

test_that("unit_test() errors on duplicate expression", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "test A", expr = all(S >= 0))

  expect_error(
    unit_test(sfm, label = "test B", expr = all(S >= 0)),
    regexp = "identical expression already exists"
  )
})

test_that("unit_test() warns when test is not sequential (gap)", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "test A", expr = all(S >= 0))

  # Test 5 is far beyond next_test = 2
  expect_warning(
    unit_test(sfm, test = 5, label = "test B", expr = all(S > 0)),
    regexp = "will be set to the existing number of tests"
  )
})


# ==============================================================================
# discard() — additional edge cases
# ==============================================================================

test_that("discard() handles variable referenced in both expr and conditions", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "rate test", expr = all(rate > -1), conditions = list(rate = 0))

  # rate is in both expr_refs and cond_refs — test should be removed entirely
  expect_warning(expect_warning(sfm <- discard(sfm, "rate"), "Removed 1 unit test"), "lingering reference")
  expect_equal(length(sfm[["unit_tests"]]), 0L)
})

test_that("discard() with multiple variables removes affected tests and strips conditions", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S test", expr = all(S >= 0)) |>
    unit_test(label = "rate test", expr = all(drain > 0), conditions = list(rate = 0))

  # Discard S and rate — first test removed (S-only), second test loses rate condition
  sfm <- suppressWarnings(discard(sfm, c("S", "rate")))

  # "S test" removed, "rate test" kept (drain still exists) but conditions stripped
  expect_equal(length(sfm[["unit_tests"]]), 1L)
  expect_equal(sfm[["unit_tests"]][[1]][["label"]], "rate test")
  expect_equal(sfm[["unit_tests"]][[1]][["conditions"]], list())
})


# ==============================================================================
# get_test_deps() — edge cases
# ==============================================================================

test_that("get_test_deps() returns empty list for model with no unit tests", {
  skip_on_cran()
  sfm <- make_verifiable_sfm()
  td <- get_test_deps(sfm)
  expect_equal(td[["deps"]], list())
})


# ==============================================================================
# discard_unit_test() — multiple indices/labels
# ==============================================================================

test_that("discard_unit_test() removes multiple tests by index", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "A", expr = all(S >= 0)) |>
    unit_test(label = "B", expr = all(S > 0)) |>
    unit_test(label = "C", expr = all(S < 200)) |>
    discard_unit_test(c(1L, 3L))

  expect_equal(length(sfm[["unit_tests"]]), 1L)
  expect_equal(sfm[["unit_tests"]][[1]][["label"]], "B")
})


# ==============================================================================
# verify — only_stocks auto-detection
# ==============================================================================

test_that("verify.stockflow() passes test referencing a flow (non-stock)", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "drain is positive", expr = all(drain > 0))

  result <- silence(verify(sfm))
  expect_equal(result[["results"]][[1]][["status"]], "pass")
})

test_that("verify.stockflow() passes test referencing a constant (non-stock)", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "rate is 0.1", expr = expect_equal(rate[[1]], 0.1))

  result <- silence(verify(sfm))
  expect_equal(result[["results"]][[1]][["status"]], "pass")
})

test_that("verify.stockflow() passes mixed stock and non-stock tests", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S is non-negative", expr = all(S >= 0)) |>
    unit_test(label = "drain is positive", expr = all(drain > 0))

  result <- silence(verify(sfm))
  statuses <- vapply(result[["results"]], function(r) r[["status"]], character(1))
  expect_equal(statuses, c("pass", "pass"))
})


# --- Conditions are actually applied to the simulation ---

test_that("verify(): constant-only condition is present in sim", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "x", expr = all(S >= 0), conditions = list(rate = 0))
  result <- silence(verify(sfm))
  condition_idx <- result[["condition"]][[1]]
  sim <- result[["sims"]][[condition_idx]]
  expect_equal(sim[["constants"]][["rate"]], 0)
})

test_that("verify(): stock initial-value condition is present in sim", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "x", expr = all(S >= 0), conditions = list(S = 50))
  result <- silence(verify(sfm))
  condition_idx <- result[["condition"]][[1]]
  sim <- result[["sims"]][[condition_idx]]
  expect_equal(sim[["init"]][["S"]], 50)
})

test_that("verify(): mixed conditions are both present in sim", {
  skip_on_cran()
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "x", expr = all(S >= 0), conditions = list(S = 50, rate = 0))
  result <- silence(verify(sfm))
  condition_idx <- result[["condition"]][[1]]
  sim <- result[["sims"]][[condition_idx]]
  expect_equal(sim[["constants"]][["rate"]], 0)
  expect_equal(sim[["init"]][["S"]], 50)
})


# ==============================================================================
# verify() — Julia backend (n = 1 and n > 1)
# ==============================================================================

test_that("verify() with Julia backend returns verify_stockflow class (n = 1)", {
  skip_if_julia_not_ready()
  sfm <- make_verifiable_sfm(language = "Julia") |>
    unit_test(label = "S non-negative", expr = all(S >= 0))
  result <- silence(verify(sfm))
  expect_s3_class(result, "verify_stockflow")
})

test_that("verify() with Julia backend passes correct test (n = 1)", {
  skip_if_julia_not_ready()
  sfm <- make_verifiable_sfm(language = "Julia") |>
    unit_test(label = "S non-negative", expr = all(S >= 0))
  result <- silence(verify(sfm))
  expect_equal(result[["results"]][[1]][["status"]], "pass")
})

test_that("verify() with Julia backend fails incorrect test (n = 1)", {
  skip_if_julia_not_ready()
  sfm <- make_verifiable_sfm(language = "Julia") |>
    unit_test(label = "S always zero", expr = all(S == 0))
  result <- silence(verify(sfm))
  expect_equal(result[["results"]][[1]][["status"]], "fail")
})

test_that("verify() with Julia backend errors on non-logical expression (n = 1)", {
  skip_if_julia_not_ready()
  sfm <- make_verifiable_sfm(language = "Julia") |>
    unit_test(label = "numeric output", expr = mean(S > 0.2))
  result <- silence(verify(sfm))
  expect_equal(result[["results"]][[1]][["status"]], "error")
})


test_that("verify() with Julia backend passes conditioned test (n = 1)", {
  skip_if_julia_not_ready()
  sfm <- make_verifiable_sfm(language = "Julia") |>
    unit_test(
      label = "S constant at zero rate",
      expr = all(diff(S) == 0),
      conditions = list(rate = 0)
    )
  result <- silence(verify(sfm))
  expect_equal(result[["results"]][[1]][["status"]], "pass")
})

test_that("verify() with Julia backend fails conditioned test (n = 1)", {
  skip_if_julia_not_ready()
  # rate = 0 keeps S at 100; asserting S < 50 fails
  sfm <- make_verifiable_sfm(language = "Julia") |>
    unit_test(
      label = "S changes when rate is zero (will fail)",
      expr = all(diff(S) > 0),
      conditions = list(rate = 0, S = 10)
    )
  result <- silence(verify(sfm))
  expect_equal(result[["results"]][[1]][["status"]], "fail")
})

# --- Conditions are actually applied to the Julia simulation ---

test_that("verify(): constant-only condition is present in Julia sim", {
  skip_if_julia_not_ready()
  sfm <- make_verifiable_sfm(language = "Julia") |>
    unit_test(label = "x", expr = all(S >= 0), conditions = list(rate = 0))
  result <- silence(verify(sfm))
  condition_idx <- result[["condition"]][[1]]
  sim <- result[["sims"]][[condition_idx]]
  expect_equal(sim[["constants"]][["rate"]], 0)
})

test_that("verify(): stock initial-value condition is present in Julia sim", {
  skip_if_julia_not_ready()
  sfm <- make_verifiable_sfm(language = "Julia") |>
    unit_test(label = "x", expr = all(S >= 0), conditions = list(S = 50))
  result <- silence(verify(sfm))
  condition_idx <- result[["condition"]][[1]]
  sim <- result[["sims"]][[condition_idx]]
  expect_equal(sim[["init"]][["S"]], 50)
})

test_that("verify(): mixed conditions are both present in Julia sim", {
  skip_if_julia_not_ready()
  sfm <- make_verifiable_sfm(language = "Julia") |>
    unit_test(label = "x", expr = all(S >= 0), conditions = list(S = 50, rate = 0))
  result <- silence(verify(sfm))
  condition_idx <- result[["condition"]][[1]]
  sim <- result[["sims"]][[condition_idx]]
  expect_equal(sim[["constants"]][["rate"]], 0)
  expect_equal(sim[["init"]][["S"]], 50)
})

test_that("verify() augments sfm vars with test refs (fixes vars/only_stocks conflict)", {
  skip_on_cran()
  sfm <- make_verifiable_sfm()

  # User limits sim_settings to only include stock 'S'
  sfm2 <- sim_settings(sfm, vars = "S")

  # Add a test that references the flow 'drain' (not included in user vars)
  sfm2 <- unit_test(sfm2, label = "drain non-negative", expr = "all(drain >= 0)")

  res <- silence(verify(sfm2))

  # Simulation data for the first condition must contain 'drain'
  sims_j1 <- res$sims[[1]]
  expect_true("variable" %in% colnames(sims_j1$df))
  expect_true(any(sims_j1$df$variable == "drain"))
})


# ==============================================================================
# as.data.frame — condition filter (which = "tests")
# ==============================================================================



test_that("condition column in which='tests' output is an integer vector", {
  skip_on_cran()
  res <- make_two_condition_result()
  df <- as.data.frame(res)
  expect_true(is.integer(df[["condition"]]))
  expect_equal(df[["condition"]], c(1L, 2L))


# as.data.frame condition filter (tests): single condition keeps only matching tests
  df1 <- as.data.frame(res, condition = 1)
  expect_equal(nrow(df1), 1L)
  expect_equal(df1[["label"]], "S non-negative")
  expect_equal(df1[["condition"]], 1L)

  df2 <- as.data.frame(res, condition = 2)
  expect_equal(nrow(df2), 1L)
  expect_equal(df2[["label"]], "S constant at zero rate")
  expect_equal(df2[["condition"]], 2L)

#  as.data.frame condition filter (tests): vector keeps tests from all listed conditions
  df <- as.data.frame(res, condition = c(1, 2))
  expect_equal(nrow(df), 2L)
  expect_setequal(df[["condition"]], c(1L, 2L))

# as.data.frame condition filter (tests): combined with test filter gives intersection
  # test = 1 is condition 1, so condition = 1 should return 1 row
  df <- as.data.frame(res, test = 1L, condition = 1)
  expect_equal(nrow(df), 1L)
  expect_equal(df[["test"]], 1L)
  # test = 1 (condition 1) filtered to condition = 2 → no match
  expect_error(
    as.data.frame(res, test = 1L, condition = 2),
    regexp = "No tests with condition"
  )

#  as.data.frame condition filter (tests): combined with status filter gives intersection
  # Both tests pass; filtering to passing tests in condition 1 gives 1 row
  df <- as.data.frame(res, condition = 1, status = "pass")
  expect_equal(nrow(df), 1L)
  expect_equal(df[["condition"]], 1L)

# as.data.frame condition filter (tests): combined with label filter gives intersection
  df <- as.data.frame(res, condition = 1, label = "non-negative")
  expect_equal(nrow(df), 1L)
  expect_equal(df[["label"]], "S non-negative")
  # label in condition 2, filtered to condition 1 → label matches but condition eliminates it
  expect_error(
    as.data.frame(res, condition = 1, label = "zero rate"),
    regexp = "No tests with condition number"
  )

# as.data.frame condition filter (tests): out-of-range condition errors
  expect_error(
    as.data.frame(res, condition = 99),
    regexp = "Condition number.*not found"
  )

# as.data.frame condition filter (tests): non-integer condition errors
  expect_error(
    as.data.frame(res, condition = 1.5),
    regexp = "must be an integer"
  )

# as.data.frame condition filter (tests): NA condition errors
  expect_error(
    as.data.frame(res, condition = NA_integer_),
    regexp = "must be an integer"
  )

#as.data.frame condition filter (tests): no-match after other filters errors clearly
  # Filter status to "fail" first (both pass → nothing left) then condition
  expect_error(
    as.data.frame(res, status = "fail", condition = 1),
    regexp = "No tests with status|No tests with condition"
  )
})


# ==============================================================================
# as.data.frame — condition filter (which = "sims")
# ==============================================================================

test_that("as.data.frame condition filter (sims): single condition keeps only that simulation", {
  skip_on_cran()
  res <- make_two_condition_result()

  df1 <- as.data.frame(res, which = "sims", condition = 1)
  expect_true(all(df1[["condition"]] == 1L))
  expect_false(any(df1[["condition"]] == 2L))

  df2 <- as.data.frame(res, which = "sims", condition = 2)
  expect_true(all(df2[["condition"]] == 2L))
  expect_false(any(df2[["condition"]] == 1L))

# as.data.frame condition filter (sims): vector keeps all listed conditions
  df <- as.data.frame(res, which = "sims", condition = c(1, 2))
  expect_setequal(unique(df[["condition"]]), c(1L, 2L))

# as.data.frame condition filter (sims): combined with test filter is intersection
  # test 1 maps to condition 1 — requesting condition 1 is consistent
  df <- as.data.frame(res, which = "sims", test = 1L, condition = 1)
  expect_true(all(df[["condition"]] == 1L))

  # test 1 (condition 1) + condition 2 → disjoint → no rows
  expect_error(
    as.data.frame(res, which = "sims", test = 1L, condition = 2),
    regexp = "No simulations match"
  )

# as.data.frame condition filter (sims): combined with status filter is intersection
  # Both conditions pass; filtering to pass + condition 1 keeps condition 1 only
  df <- as.data.frame(res, which = "sims", condition = 1, status = "pass")
  expect_true(all(df[["condition"]] == 1L))

# as.data.frame condition filter (sims): out-of-range condition errors
  expect_error(
    as.data.frame(res, which = "sims", condition = 99),
    regexp = "Condition number.*not found"
  )

# as.data.frame condition filter (sims): non-integer condition errors
  expect_error(
    as.data.frame(res, which = "sims", condition = 1.5),
    regexp = "must be an integer"
  )

# as.data.frame condition filter (sims): NA condition errors
  expect_error(
    as.data.frame(res, which = "sims", condition = NA_integer_),
    regexp = "must be an integer"
  )

# as.data.frame condition filter (sims): wide direction still respects condition filter
  df <- as.data.frame(res, which = "sims", direction = "wide", condition = 1)
  expect_true(all(df[["condition"]] == 1L))
  # wide format: variables become columns
  expect_false("variable" %in% names(df))
})


# ============================================================================
# as.data.frame.verify_stockflow() — vars/type filtering (which = "sims")
# ============================================================================

test_that("as.data.frame(verify, which='sims', vars=) filters by variable", {
  res <- make_verify_model(n_tests = 2)
  df <- as.data.frame(res, which = "sims", vars = "S")
  expect_equal(unique(df$variable), "S")

# as.data.frame(verify, which='sims', type=) filters by type
  df <- as.data.frame(res, which = "sims", type = "stock")
  expect_true(all(df$variable == "S"))

# as.data.frame(verify, which='sims'): unknown vars errors as a typo
  expect_error(as.data.frame(res, which = "sims", vars = "nope"), "not.*variable")

# as.data.frame(verify, which='sims'): variable not saved gives informative error
  # 'drain' is a flow in the model but is not saved in the verify output by default
  expect_error(as.data.frame(res, which = "sims", vars = "drain"), "not saved in the output")

# as.data.frame(verify, which='tests') informs that vars/type apply only to sims
  expect_message(df <- as.data.frame(res, which = "tests", type = "stock"), "only apply")
  # tests table is returned unaffected by the ignored filter
  expect_false("variable" %in% names(df))
  expect_equal(df, suppressMessages(as.data.frame(res, which = "tests")))
})
