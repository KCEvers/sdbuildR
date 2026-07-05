test_that("sim_settings() modifies start time", {
  sfm <- stockflow() |>
    sim_settings(start = 10)

  expect_equal(as.numeric(sfm$sim_settings$start), 10)
})

test_that("sim_settings() modifies stop time", {
  sfm <- stockflow() |>
    sim_settings(stop = 200)

  expect_equal(as.numeric(sfm$sim_settings$stop), 200)
})

test_that("sim_settings() modifies dt", {
  sfm <- stockflow() |>
    sim_settings(dt = 0.1)

  expect_equal(as.numeric(sfm$sim_settings$dt), 0.1)
})

test_that("sim_settings() modifies method", {
  sfm <- stockflow() |>
    sim_settings(method = "rk4")

  expect_equal(sfm$sim_settings$method, "rk4")
})

test_that("sim_settings() preserves other fields", {
  sfm <- stockflow() |>
    sim_settings(start = 5)

  expect_true("stop" %in% names(sfm$sim_settings))
  expect_true("dt" %in% names(sfm$sim_settings))
})

test_that("sim_settings() returns stockflow object", {
  sfm <- stockflow() |>
    sim_settings(start = 0)

  expect_s3_class(sfm, "stockflow")
})

test_that("sim_settings() handles language parameter", {
  sfm <- stockflow() |>
    sim_settings(language = "R")

  expect_equal(sfm$sim_settings$language, "R")

  sfm <- stockflow() |>
    sim_settings(language = "Julia")

  expect_equal(sfm$sim_settings$language, "Julia")
})

test_that("sim_settings() modifies seed", {
  sfm <- stockflow() |>
    sim_settings(seed = 123)

  expect_equal(as.numeric(sfm$sim_settings$seed), 123)
})

test_that("sim_settings() stores vars and preserves order after deduplication", {
  sfm <- stockflow("sir") |>
    sim_settings(vars = c("susceptible", "infected", "susceptible"))

  expect_equal(sfm$sim_settings$vars, c("susceptible", "infected"))
})

test_that("sim_settings() clears vars with explicit empty values", {
  sfm <- stockflow("sir") |>
    sim_settings(vars = c("susceptible", "infected"))

  expect_null(sim_settings(sfm, vars = NULL)[["sim_settings"]][["vars"]])
  expect_null(sim_settings(sfm, vars = "")[["sim_settings"]][["vars"]])
  expect_null(sim_settings(sfm, vars = c())[["sim_settings"]][["vars"]])
  expect_null(sim_settings(sfm, vars = character(0))[["sim_settings"]][["vars"]])
})

test_that("sim_settings() rejects unknown vars", {
  sfm <- stockflow("sir")
  expect_error(sim_settings(sfm, vars = c("does_not_exist")), "Invalid variable name")
})


cli::test_that_cli(configs = c("plain", "ansi"), "clean_language() rejects invalid language", {
  expect_snapshot(clean_language("python"), error = TRUE)
  expect_snapshot(clean_language("cpp"), error = TRUE)
})


test_that("sim_settings() sets basic parameters", {
  sfm <- stockflow()
  sfm1 <- update(sfm, "Stock1", type = "stock")
  sfm2 <- suppressWarnings(sim_settings(sfm1, start = 0, stop = 10, dt = 0.5))

  expect_equal(sfm2[["sim_settings"]][["start"]], "0.0")
  expect_equal(sfm2[["sim_settings"]][["stop"]], "10.0")
  expect_equal(sfm2[["sim_settings"]][["dt"]], "0.5")
})

test_that("sim_settings() validates start < stop", {
  sfm <- stockflow()
  sfm1 <- update(sfm, "Stock1", type = "stock")

  expect_error(
    sim_settings(sfm1, start = 10, stop = 5),
    "start.*smaller than.*stop"
  )
})

test_that("sim_settings() validates numeric parameters", {
  sfm <- stockflow()
  sfm1 <- update(sfm, "Stock1", type = "stock")

  expect_error(sim_settings(sfm1, start = "abc"), "numeric")
  expect_error(sim_settings(sfm1, stop = "xyz"), "numeric")
  expect_error(sim_settings(sfm1, dt = "foo"), "numeric")
})

test_that("sim_settings() validates language parameter", {
  sfm <- stockflow()
  sfm1 <- update(sfm, "Stock1", type = "stock")

  expect_no_error(sim_settings(sfm1, language = "R"))
  expect_no_error(sim_settings(sfm1, language = "Julia"))
})

test_that("sim_settings() validates method parameter", {
  sfm <- stockflow()
  sfm1 <- update(sfm, "Stock1", type = "stock")

  expect_no_error(sim_settings(sfm1, method = "euler", language = "R"))
  expect_no_error(sim_settings(sfm1, method = "rk4", language = "R"))
})

test_that("sim_settings() handles time_units", {
  sfm <- stockflow()

  sfm <- expect_no_error(sim_settings(sfm, time_units = "days"))
  expect_equal(sfm[["sim_settings"]][["time_units"]], "days")
  sfm <- expect_no_error(sim_settings(sfm, time_units = "hours"))
  expect_equal(sfm[["sim_settings"]][["time_units"]], "hours")
  sfm <- expect_no_error(sim_settings(sfm, time_units = "years"))
  expect_equal(sfm[["sim_settings"]][["time_units"]], "years")
})

test_that("sim_settings() warns about large dt", {
  sfm <- stockflow()
  sfm1 <- update(sfm, "Stock1", type = "stock")

  expect_warning(
    sim_settings(sfm1, dt = 0.5),
    "Large timestep"
  )
})

test_that("sim_settings() returns stockflow object", {
  sfm <- stockflow()
  sfm1 <- update(sfm, "Stock1", type = "stock")
  sfm2 <- sim_settings(sfm1, start = 0, stop = 10)

  expect_s3_class(sfm2, "stockflow")
})

# --- New positive-value guards --------------------------------------------------

test_that("sim_settings() rejects non-positive dt", {
  sfm <- stockflow()
  expect_error(sim_settings(sfm, dt = 0), "positive")
  expect_error(sim_settings(sfm, dt = -0.1), "positive")
})

test_that("sim_settings() rejects non-positive save_by", {
  sfm <- stockflow()
  expect_error(sim_settings(sfm, save_by = 0), "positive")
  expect_error(sim_settings(sfm, save_by = -1), "positive")
})

# --- NSE support ---------------------------------------------------------------


test_that("sim_settings() method and language accept string variables directly", {
  sfm <- stockflow()
  lang <- "Julia"
  meth <- "rk4"

  expect_equal(
    sim_settings(sfm, language = lang)[["sim_settings"]][["language"]], "Julia"
  )
  expect_equal(
    sim_settings(sfm, method = meth)[["sim_settings"]][["method"]], "rk4"
  )
})

# --- Save parameter: defaults ---------------------------------------------------

test_that("new stockflow() has NULL save fields", {
  sfm <- stockflow()
  expect_null(sfm[["sim_settings"]][["save_by"]])
  expect_null(sfm[["sim_settings"]][["save_times"]])
  expect_null(sfm[["sim_settings"]][["save_length"]])
})

# --- Save parameter: save_by (regular interval) ---------------------------------

test_that("sim_settings() stores save_by interval", {
  sfm2 <- sim_settings(stockflow(), save_by = 1)
  expect_equal(sfm2[["sim_settings"]][["save_by"]], "1.0")
  expect_null(sfm2[["sim_settings"]][["save_times"]])
  expect_null(sfm2[["sim_settings"]][["save_length"]])
})

test_that("sim_settings() auto-corrects save_by < dt", {
  expect_warning(
    sfm2 <- sim_settings(stockflow(), save_by = 0.001),
    "Automatically setting"
  )
  expect_equal(
    as.numeric(sfm2[["sim_settings"]][["save_by"]]),
    as.numeric(sfm2[["sim_settings"]][["dt"]])
  )
})

test_that("sim_settings() rejects a vector save_by", {
  expect_error(sim_settings(stockflow(), save_by = c(1, 5)), "single number|save_times")
})

test_that("sim_settings() warns on save_by interval misalignment with stop", {
  # stop = 10, save_by = 3: 10 %% 3 = 1, stop not aligned
  expect_warning(
    sim_settings(stockflow(), stop = 10, save_by = 3),
    "Endpoint may be missing"
  )
})

test_that("sim_settings() does not warn when save_by aligns with stop", {
  # stop = 10, save_by = 2: 10 %% 2 = 0, aligned
  expect_no_warning(sim_settings(stockflow(), stop = 10, save_by = 2))
})

# --- Save parameter: save_times (explicit times) --------------------------------

test_that("sim_settings() stores save_times vector", {
  sfm2 <- sim_settings(stockflow(), save_times = c(1, 5, 10))
  expect_equal(length(sfm2[["sim_settings"]][["save_times"]]), 3L)
  expect_null(sfm2[["sim_settings"]][["save_by"]])
  expect_null(sfm2[["sim_settings"]][["save_length"]])
})

test_that("sim_settings() rejects save_times with out-of-range values", {
  expect_error(sim_settings(stockflow(), save_times = c(1, 200)), "out-of-range|within")
})

test_that("sim_settings() sorts and deduplicates save_times", {
  sfm2 <- sim_settings(stockflow(), save_times = c(10, 1, 5, 1))
  vals <- as.numeric(sfm2[["sim_settings"]][["save_times"]])
  expect_equal(vals, c(1, 5, 10))
})

# --- Save parameter: save_length ------------------------------------------------

test_that("sim_settings() stores save_length", {
  sfm2 <- sim_settings(stockflow(), save_length = 100)
  expect_equal(as.integer(sfm2[["sim_settings"]][["save_length"]]), 100L)
  expect_null(sfm2[["sim_settings"]][["save_by"]])
  expect_null(sfm2[["sim_settings"]][["save_times"]])
})

test_that("sim_settings() save_length = 1 stores correctly", {
  sfm2 <- sim_settings(stockflow(), save_length = 1)
  expect_equal(as.integer(sfm2[["sim_settings"]][["save_length"]]), 1L)
})

test_that("sim_settings() rejects invalid save_length", {
  expect_error(sim_settings(stockflow(), save_length = 0), "whole positive number")
  expect_error(sim_settings(stockflow(), save_length = -1), "whole positive number")
  expect_error(sim_settings(stockflow(), save_length = "abc"), "whole positive number")
  expect_error(sim_settings(stockflow(), save_length = 2.5), "whole positive number")
  expect_error(sim_settings(stockflow(), save_length = c(1, 2)), "whole positive number")
})

# --- Save parameter: mutual exclusion -------------------------------------------

test_that("sim_settings() errors when more than one save argument provided", {
  expect_error(sim_settings(stockflow(), save_by = 1, save_length = 100), "more than one")
  expect_error(sim_settings(stockflow(), save_by = 1, save_times = c(1, 5)), "more than one")
  expect_error(sim_settings(stockflow(), save_times = c(1, 5), save_length = 100), "more than one")
})

test_that("sim_settings() allows multiple save arguments when all but one are NA", {
  expect_no_error(sim_settings(stockflow(), save_by = NA, save_length = 100))
  expect_no_error(sim_settings(stockflow(), save_by = 1, save_length = NA))
})

# --- Save parameter: legacy names hard-deprecated -------------------------------

test_that("sim_settings() errors on legacy save_at and save_n", {
  expect_error(sim_settings(stockflow(), save_at = 1), "no longer supported")
  expect_error(sim_settings(stockflow(), save_n = 100), "no longer supported")
})

test_that("sim_settings() errors on unknown arguments", {
  expect_error(sim_settings(stockflow(), not_an_arg = 1), "Unknown argument")
})

# --- Save parameter: reset to saving all steps with NA/NULL/"" -----------------

test_that("sim_settings() resetting save_by to NA saves all steps", {
  sfm <- sim_settings(stockflow(), save_by = 1)
  sfm2 <- sim_settings(sfm, save_by = NA)
  expect_null(sfm2[["sim_settings"]][["save_by"]])
  expect_null(sfm2[["sim_settings"]][["save_times"]])
  expect_null(sfm2[["sim_settings"]][["save_length"]])
})

test_that("sim_settings() resetting save_length to NA saves all steps", {
  sfm <- sim_settings(stockflow(), save_length = 50)
  sfm2 <- sim_settings(sfm, save_length = NA)
  expect_null(sfm2[["sim_settings"]][["save_by"]])
  expect_null(sfm2[["sim_settings"]][["save_times"]])
  expect_null(sfm2[["sim_settings"]][["save_length"]])
})

# --- Save parameter: overwriting -----------------------------------------------

test_that("sim_settings() save_length overwrites previous save_by", {
  sfm <- sim_settings(stockflow(), save_by = 1)
  sfm2 <- sim_settings(sfm, save_length = 50)
  expect_equal(as.integer(sfm2[["sim_settings"]][["save_length"]]), 50L)
  expect_null(sfm2[["sim_settings"]][["save_by"]])
  expect_null(sfm2[["sim_settings"]][["save_times"]])
})

test_that("sim_settings() save_by overwrites previous save_length", {
  sfm <- sim_settings(stockflow(), save_length = 50)
  sfm2 <- sim_settings(sfm, save_by = 2)
  expect_equal(sfm2[["sim_settings"]][["save_by"]], "2.0")
  expect_null(sfm2[["sim_settings"]][["save_times"]])
  expect_null(sfm2[["sim_settings"]][["save_length"]])
})

test_that("sim_settings() default save_sims is FALSE on new model", {
  sfm <- stockflow("sir")
  expect_false(isTRUE(sfm[["sim_settings"]][["save_sims"]]))
})

test_that("verify() always retains sims regardless of save_sims", {
  sfm <- stockflow("sir") |>
    unit_test(expr = all(susceptible >= 0))

  res <- verify(sfm)
  expect_s3_class(res, "verify_stockflow")
  expect_true(!is.null(res$sims)) # always present, no save_sims needed
})


test_that("sim_settings() rejects invalid save_sims", {
  expect_error(sim_settings(stockflow(), save_sims = "notlogical"), "Invalid")
})
