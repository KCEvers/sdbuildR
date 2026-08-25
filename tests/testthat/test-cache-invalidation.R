# Tests for conservative assembly cache invalidation


test_that("invalidate_assemble clears the canonical cache for every category", {
  skip_on_cran()
  categories <- c(
    "all", "variables", "static", "dynamic", "times", "funcs",
    "nonneg", "unit_tests"
  )

  for (category in categories) {
    sfm <- stockflow("sir")
    sfm <- invalidate_assemble(sfm, category)

    expect_empty_assemble_cache(sfm[["assemble"]])
  }
})


test_that("codegen sim_settings changes rebuild the base cache", {
  sfm1 <- stockflow("sir") |>
    sim_settings(language = "R", stop = 10)
  hash1 <- sfm1[["assemble"]][["input_hash"]]
  times1 <- sfm1[["assemble"]][["times"]]

  sfm2 <- sim_settings(sfm1, stop = 200)

  expect_false(is.null(sfm2[["assemble"]][["input_hash"]]))
  expect_false(identical(hash1, sfm2[["assemble"]][["input_hash"]]))
  expect_false(identical(times1, sfm2[["assemble"]][["times"]]))
})


test_that("runtime-only sim_settings do not change the base cache hash", {
  sfm <- stockflow("sir") |>
    sim_settings(language = "R", start = 0, stop = 10, dt = 0.1)
  hash <- sfm[["assemble"]][["input_hash"]]

  expect_identical(sim_settings(sfm, only_stocks = FALSE)[["assemble"]][["input_hash"]], hash)
  expect_identical(sim_settings(sfm, vars = c("susceptible"))[["assemble"]][["input_hash"]], hash)
  expect_identical(sim_settings(sfm, seed = 1)[["assemble"]][["input_hash"]], hash)
  expect_identical(sim_settings(sfm, save_sims = TRUE)[["assemble"]][["input_hash"]], hash)
})


test_that("model edits rebuild the base cache conservatively", {
  sfm1 <- stockflow("sir")
  hash1 <- sfm1[["assemble"]][["input_hash"]]

  sfm2 <- update(sfm1, recovered, eqn = 200)

  expect_false(is.null(sfm2[["assemble"]][["input_hash"]]))
  expect_false(identical(hash1, sfm2[["assemble"]][["input_hash"]]))
  expect_false(is.null(sfm2[["assemble"]][["ordering"]]))
  expect_true(nzchar(sfm2[["assemble"]][["times"]]))
})


test_that("conservative invalidation produces same simulation as full invalidation", {
  sfm <- stockflow("sir") |>
    sim_settings(stop = 50)

  sim1 <- simulate(sfm)
  sfm_cached <- sim1$object

  sfm2 <- update(sfm_cached, "contact_rate", eqn = "20")
  sim2 <- simulate(sfm2)

  sfm3 <- update(sfm_cached, "contact_rate", eqn = "20")
  sfm3 <- invalidate_assemble(sfm3, "all")
  sim3 <- simulate(sfm3)

  expect_equal(sim2$df$value, sim3$df$value)
  expect_equal(sim2$df$variable, sim3$df$variable)
  expect_equal(sim2$df$time, sim3$df$time)
})


test_that("pre_assemble_components populates summary cache", {
  sfm <- stockflow("sir")
  expect_false(is.null(sfm[["assemble"]][["summary"]]))
  expect_true(is.list(sfm[["assemble"]][["summary"]]))
  expect_equal(sfm[["assemble"]][["summary"]][["zero_equations"]][["problem"]], "warning")
})


test_that("invalidate_assemble clears summary", {
  sfm <- stockflow("sir")
  sfm <- invalidate_assemble(sfm, "variables")
  expect_null(sfm[["assemble"]][["summary"]])
})
