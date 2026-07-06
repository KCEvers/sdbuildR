# Tests for  compare_models(), model_properties(),


# ============================================================================
# compare_models()
# ============================================================================

test_that("compare_models: identical models have zero differences", {
  sfm <- make_basic_sfm()
  diff <- compare_models(sfm, sfm)
  expect_equal(nrow(diff$added), 0)
  expect_equal(nrow(diff$removed), 0)
  expect_equal(nrow(diff$type_changed), 0)
  expect_equal(nrow(diff$eqn_changed), 0)
  expect_equal(length(diff$sim_settings_diff), 0)
})

test_that("compare_models: detects added variable in sfm2", {
  sfm1 <- make_basic_sfm()
  sfm2 <- sfm1 |> update("new_aux", type = "aux", eqn = "1")
  diff <- compare_models(sfm1, sfm2)
  expect_equal(nrow(diff$added), 1)
  expect_true("new_aux" %in% diff$added$name)
  expect_equal(nrow(diff$removed), 0)
})

test_that("compare_models: detects variable removed from sfm1", {
  sfm1 <- make_basic_sfm() # has stock S + flow Flow1
  sfm2 <- stockflow() |> update("S", type = "stock", eqn = "1") # only stock S
  diff <- compare_models(sfm1, sfm2)
  expect_equal(nrow(diff$removed), 1)
  expect_true("Flow1" %in% diff$removed$name)
  expect_equal(nrow(diff$added), 0)
})

test_that("compare_models: detects equation change in shared variable", {
  sfm1 <- make_basic_sfm()
  sfm2 <- sfm1 |> update("S", eqn = "999")
  diff <- compare_models(sfm1, sfm2)
  expect_equal(nrow(diff$eqn_changed), 1)
  expect_true("S" %in% diff$eqn_changed$name)
  expect_equal(diff$eqn_changed$eqn_1[diff$eqn_changed$name == "S"], "1")
  expect_equal(diff$eqn_changed$eqn_2[diff$eqn_changed$name == "S"], "999")
})

test_that("compare_models: detects sim_settings change (stop time)", {
  sfm1 <- make_basic_sfm()
  sfm2 <- sim_settings(sfm1, stop = 200)
  diff <- compare_models(sfm1, sfm2)
  expect_true("stop" %in% names(diff$sim_settings_diff))
  expect_equal(as.numeric(diff$sim_settings_diff[["stop"]][["sfm2"]]), 200)
})

test_that("compare_models: returns a 'compare_stockflow' class object", {
  diff <- compare_models(make_basic_sfm(), make_basic_sfm())
  expect_s3_class(diff, "compare_stockflow")
})

test_that("compare_models: result contains 'properties' with per-model stats", {
  diff <- compare_models(make_basic_sfm(), make_basic_sfm())
  expect_true("properties" %in% names(diff))
  expect_true("sfm1" %in% names(diff$properties))
  expect_true("sfm2" %in% names(diff$properties))
  expect_true("n_stocks" %in% names(diff$properties$sfm1))
})


# ============================================================================
# model_properties()
# ============================================================================

test_that("model_properties: stock/flow counts match as.data.frame() counts", {
  sfm <- templates("sir")
  props <- model_properties(sfm)
  expect_equal(props$n_stocks, nrow(as.data.frame(sfm, type = "stock")))
  expect_equal(props$n_flows, nrow(as.data.frame(sfm, type = "flow")))
})

test_that("model_properties: returns all expected top-level fields", {
  props <- model_properties(templates("sir"))
  expect_true("n_stocks" %in% names(props))
  expect_true("n_flows" %in% names(props))
  expect_true("n_aux" %in% names(props))
  expect_true("n_constants" %in% names(props))
  expect_true("n_lookups" %in% names(props))
  expect_false("nonlinearity" %in% names(props))
})
