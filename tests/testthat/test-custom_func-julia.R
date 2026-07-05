# R -> Julia mapping of the custom convenience functions.
#
# test-sdbuildR_custom_func.R checks the R implementations, and test-conv_julia.R
# checks the *string* rewrite (e.g. logistic(x, slope = 2) -> logistic.(x, 2.0, ...)).
# Neither checks that the rewritten Julia code, run in the user's Julia package
# (SystemDynamicsBuildR), actually evaluates to the same number as R. That is what
# these tests do: they convert exactly as simulate() would and execute the result
# in the live Julia session, then compare against R.

# Scalar functions ----

test_that("logistic / sigmoid map to the same values in Julia", {
  ready_julia()
  expect_r_julia_equal("logistic(0.5)")
  expect_r_julia_equal("logistic(0.5, slope = 2)")
  expect_r_julia_equal("logistic(1.3, slope = 2, midpoint = 0.5, upper = 10)")
  expect_r_julia_equal("sigmoid(1.3)")
  expect_r_julia_equal("sigmoid(1, slope = 2, midpoint = 1, upper = 5)")
  # Broadcast mapping (logistic. over a vector)
  expect_r_julia_equal("logistic(c(-2, 0, 2), slope = 1.5)")
})

test_that("hill maps to the same values in Julia", {
  ready_julia()
  expect_r_julia_equal("hill(0.5)")
  expect_r_julia_equal("hill(0.7, slope = 3, midpoint = 0.4, upper = 2)")
  expect_r_julia_equal("hill(c(0.1, 1, 5), slope = 2, midpoint = 1)")
})

test_that("logit / expit map to the same values in Julia", {
  ready_julia()
  expect_r_julia_equal("logit(0.3)")
  expect_r_julia_equal("expit(1.7)")
  # Round trip
  expect_r_julia_equal("expit(logit(0.42))")
})

test_that("nonnegative maps to the same values in Julia", {
  ready_julia()
  expect_r_julia_equal("nonnegative(-4.2)")
  expect_r_julia_equal("nonnegative(3.1)")
  expect_r_julia_equal("nonnegative(0)")
})

test_that("round_IM maps to the same values in Julia", {
  ready_julia()
  expect_r_julia_equal("round_IM(0.5)")
  expect_r_julia_equal("round_IM(-0.5)")
  expect_r_julia_equal("round_IM(2.5)")
  # digits arg: the converter emits a float literal, so the Julia round_IM must
  # accept a Real (not only Int) for this to dispatch.
  expect_r_julia_equal("round_IM(2.345, 2)")
})

test_that("indexof maps to the same values in Julia", {
  ready_julia()
  expect_r_julia_equal('indexof(c("a", "b", "c"), "b")')
  expect_r_julia_equal("indexof(c(10, 20, 30), 30)")
  expect_r_julia_equal('indexof("haystack", "hay")')
  expect_r_julia_equal('indexof("haystack", "stack")')
  expect_r_julia_equal('indexof("haystack", "zzz")') # not found -> 0
})

test_that("contains_IM maps to the same values in Julia", {
  ready_julia()
  expect_r_julia_equal("contains_IM(c(1, 2, 3), 2)")
  expect_r_julia_equal("contains_IM(c(1, 2, 3), 4)")
  expect_r_julia_equal('contains_IM("haystack", "hay")')
  expect_r_julia_equal('contains_IM("haystack", "needle")')
})

test_that("length_IM maps to the same values in Julia", {
  ready_julia()
  expect_r_julia_equal('length_IM(c("a", "b", "c"))')
  expect_r_julia_equal('length_IM("abcdef")')
})

test_that("rem / mod / %REM% / %/% map to the same values in Julia", {
  ready_julia()
  # Sign handling differs between rem (follows dividend) and mod (follows divisor),
  # so cover all four sign combinations.
  for (a in c(7, -7)) {
    for (b in c(3, -3)) {
      expect_r_julia_equal(sprintf("rem(%d, %d)", a, b))
      expect_r_julia_equal(sprintf("mod(%d, %d)", a, b))
      expect_r_julia_equal(sprintf("%d %%REM%% %d", a, b))
      expect_r_julia_equal(sprintf("%d %%/%% %d", a, b)) # floor division -> Julia ⊘
    }
  }
})


# Time-input functions ----

test_that("ramp maps to the same trajectory in Julia", {
  ready_julia()
  expect_input_sim_equal("ramp(times, 5, 15, 3)")
  expect_input_sim_equal("ramp(times, 5, 15, -3)") # decreasing
})

test_that("step maps to the same trajectory in Julia", {
  ready_julia()
  expect_input_sim_equal("step(times, 8, 2)")
  expect_input_sim_equal("step(times, 8, -3)")
})

test_that("pulse maps to the same trajectory in Julia", {
  ready_julia()
  expect_input_sim_equal("pulse(times, 5, 2, 2)")
  expect_input_sim_equal("pulse(times, 2, 1, 1, 5)") # repeating
})

test_that("seasonal maps to the same trajectory in Julia", {
  ready_julia()
  expect_input_sim_equal("seasonal(times, 10, 0)")
  expect_input_sim_equal("seasonal(times, 7, 2)") # shifted peak
})


test_that("ricker maps to the same values in Julia", {
  skip_if_julia_not_ready()
  expect_r_julia_equal("ricker(1)")
  expect_r_julia_equal("ricker(2, location = 2, upper = 10, shape = 1)")
  expect_r_julia_equal("ricker(3, location = 3, upper = 10, shape = 2)")
  expect_r_julia_equal("ricker(c(0.5, 1, 3, 5), location = 2)") # broadcast
  expect_r_julia_equal("ricker(3, a = 2.5, b = 0.4)") # expanded form
  expect_r_julia_equal("ricker(3, a = 1.8, b = 0.6, shape = 2.5)")
})
