# Tests for ensemble() and plot.ensemble_stockflow()

# Input validation ----------------------------

test_that("ensemble() passes additional arguments (language) to sim_settings", {
  sfm <- make_basic_sfm()
  expect_error(
    ensemble(sfm, language = "unsupported"),
    "Invalid.*language",
    ignore.case = TRUE
  )
})

test_that("ensemble() validates inputs", {
  sfm <- make_ensemble_error_sfm()
  expect_error(ensemble(sfm, n = 0), "must be greater than")
  expect_error(ensemble(sfm, n = -5), "must be greater than")
  expect_error(ensemble(sfm, n = "ten"), "must be.*numeric")

# ensemble() validates quantiles argument
  expect_error(ensemble(sfm, quantiles = 0.5), "at least.*2.*unique")
  expect_error(ensemble(sfm, quantiles = c(0.5, 0.5)), "at least.*2.*unique")
  expect_error(ensemble(sfm, quantiles = c(-0.1, 0.5)), "between.*0.*and.*1")
  expect_error(ensemble(sfm, quantiles = c(0.5, 1.1)), "between.*0.*and.*1")
  expect_error(ensemble(sfm, quantiles = "high"), "must be.*numeric")

# ensemble() validates logical arguments
  expect_error(ensemble(sfm, cross = "yes"), "must be.*TRUE.*FALSE")
  expect_error(ensemble(sfm, save_sims = 1), "must be.*TRUE.*FALSE")
  expect_error(ensemble(sfm, only_stocks = "all"), "must be.*TRUE.*FALSE")
 
 # ensemble() validates conditions is a named list
  expect_error(ensemble(sfm, conditions = "S"), "must be a.*list")
  expect_error(ensemble(sfm, conditions = list()), "at least one parameter")
  expect_error(ensemble(sfm, conditions = list(0.1, 0.2)), "must be named")

# ensemble() validates conditions elements are numeric
  expect_error(
    ensemble(sfm, conditions = list("S" = "abc")),
    "must be.*numeric"
  )
  expect_error(
    ensemble(sfm, conditions = list("S" = c(1, 2), "k" = c("a", "b"))),
    "must be.*numeric"
  )

# ensemble() validates conditions names are unique
  expect_error(
    ensemble(sfm, conditions = list("S" = c(1, 2), "S" = c(3, 4))),
    "must be unique"
  )

# ensemble() validates conditions names exist in model
  expect_error(
    ensemble(sfm, conditions = list("nonexistent" = c(1, 2))),
    "do not exist in the model"
  )

# ensemble() rejects flows and auxiliaries in conditions
  expect_error(
    ensemble(sfm, conditions = list("Flow1" = c(1, 2))),
    "Flows or auxiliaries cannot be varied"
  )

# ensemble() validates equal conditions lengths when cross = FALSE
  expect_error(
    ensemble(sfm,
      conditions = list("S" = c(1, 2, 3), "k" = c(0.1, 0.2)),
      cross = FALSE
    ),
    "equal length"
  )
})


# Basic ensemble ----------------------------------------

test_that("ensemble() runs successfully", {
  skip_if_julia_not_ready()
  sfm <- make_jl_ensemble_sfm()

  sims <- silence(ensemble(sfm, n = 3))

  # correct structure
  expect_successful_ensemble(sims, c(
    "success", "df", "summary", "n", "n_total",
    "n_conditions", "conditions", "init", "constants",
    "script", "duration"
  ))
})


test_that("ensemble() handles models with no constants", {
  skip_if_julia_not_ready()
  sfm <- make_basic_sfm() |>
    sim_settings(language = "jl", start = 0, stop = 10, dt = 0.1, save_by = 1)

  sims <- silence(ensemble(sfm, n = 3, save_sims = TRUE, quiet = TRUE))

  expect_true(sims[["success"]])
  expect_equal(nrow(sims[["constants"]][["df"]]), 0)
  expect_equal(nrow(sims[["constants"]][["summary"]]), 0)
  expect_equal(
    sort(names(sims[["constants"]][["df"]])),
    c("condition", "sim", "value", "variable")
  )
  expect_true(
    all(c("condition", "variable", "mean") %in%
      names(sims[["constants"]][["summary"]]))
  )
})


test_that("ensemble() of model with only stocks", {
  skip_if_julia_not_ready()
  sfm <- stockflow() |>
    stock("Stock1", eqn = 100) |>
    stock("Stock2", eqn = 50) |>
    sim_settings(language = "Julia", start = 0, stop = 10, dt = 0.1, save_by = 1)

  sims <- silence(ensemble(sfm, n = 3, only_stocks = TRUE, save_sims = TRUE, quiet = TRUE))
  expect_true(sims[["success"]])
  expect_equal(
    sort(unique(sims[["summary"]][["variable"]])),
    c("Stock1", "Stock2")
  )
})


test_that("ensemble() returns all variables with only_stocks = FALSE", {
  skip_if_julia_not_ready()
  sfm <- make_jl_ensemble_sfm()

  df <- as.data.frame(sfm, properties = "eqn")
  n_all <- nrow(df[df[["type"]] %in% c("stock", "flow", "aux"), ])

  sims <- silence(ensemble(sfm, n = 3, only_stocks = FALSE, save_sims = TRUE))
  expect_true(sims[["success"]])
  expect_false(is.null(sims[["summary"]]))
  expect_false(is.null(sims[["df"]]))
  expect_equal(
    length(unique(sims[["summary"]][["variable"]])),
    n_all
  )
})

test_that("ensemble() with Julia filters outputs to vars", {
  skip_if_julia_not_ready()

  sfm <- make_jl_ensemble_sfm() |>
    sim_settings(vars = c("Food_intake", "Hunger"))

  sims <- silence(ensemble(sfm, n = 3, save_sims = TRUE))

  expect_true(sims[["success"]])
  expect_equal(sort(unique(sims[["summary"]][["variable"]])), c("Food_intake", "Hunger"))
  expect_equal(sort(unique(sims[["df"]][["variable"]])), c("Food_intake", "Hunger"))
})

test_that("ensemble() with Julia vars overrides only_stocks", {
  skip_if_julia_not_ready()

  sfm <- make_jl_ensemble_sfm() |>
    sim_settings(only_stocks = TRUE, vars = c("Satiety"))

  sims <- silence(ensemble(sfm, n = 3, save_sims = TRUE))

  expect_true(sims[["success"]])
  expect_equal(unique(sims[["summary"]][["variable"]]), "Satiety")
  expect_equal(unique(sims[["df"]][["variable"]]), "Satiety")
})

test_that("ensemble() with Julia handles integer start and fractional dt with intermediaries", {
  skip_if_julia_not_ready()

  sfm <- withr::with_envvar(new = c(SDBUILDR_DEFER_CODEGEN = "true"), {
    stockflow() |>
      sim_settings(language = "Julia", start = 0, stop = 0.02, dt = 0.01, save_sims = TRUE) |>
      update("k", type = "constant", eqn = "0.1") |>
      update("s", type = "stock", eqn = "runif(1, 0.9, 1.1)") |>
      update("f", type = "flow", eqn = "k * s * (1.0 + exp(-s))", from = "s")
  })

  sims <- silence(ensemble(sfm, n = 2, quiet = TRUE))

  expect_true(sims[["success"]])
  expect_equal(sort(unique(sims[["df"]][["time"]])), c(0, 0.01, 0.02), tolerance = 1e-12)
})

test_that("ensemble() summary has consistent timeseries lengths", {
  skip_if_julia_not_ready()

  sfm <- make_jl_ensemble_sfm()

  sims <- silence(ensemble(sfm, n = 3, only_stocks = FALSE))

  table_lengths <- with(sims[["summary"]], table(variable))
  expect_equal(length(unique(table_lengths)), 1)
})

test_that("ensemble() only_stocks = FALSE returns correct n properties and full sim coverage", {
  skip_if_julia_not_ready()

  nr_sims <- 5
  sfm <- make_jl_ensemble_sfm()

  sims <- silence(ensemble(
    sfm,
    n = nr_sims,
    only_stocks = FALSE,
    save_sims = TRUE
  ))

  expect_true(sims[["success"]])

  # n properties (no conditions)
  expect_equal(sims[["n"]], nr_sims)
  expect_equal(sims[["n_total"]], nr_sims)
  expect_equal(sims[["n_conditions"]], 1)
  expect_unique_values(sims[["df"]], "condition", 1)
  expect_unique_values(sims[["summary"]], "condition", 1)

  # constants and init summaries
  expect_equal(
    sort(unique(sims[["constants"]][["summary"]][["variable"]])),
    c("a0", "a1", "a2")
  )
  expect_equal(
    sort(unique(sims[["init"]][["summary"]][["variable"]])),
    c("Compensatory_behaviour", "Food_intake", "Hunger")
  )

  df <- sims[["df"]]
  constants_df <- sims[["constants"]][["df"]]
  init_df <- sims[["init"]][["df"]]

  stock_names <- as.data.frame(sfm, properties = "eqn")
  stock_names <- stock_names[stock_names[["type"]] == "stock", "name", drop = TRUE]
  non_stock_names <- setdiff(unique(df[["variable"]]), stock_names)

  expect_ensemble_sim_coverage(nr_sims, df, constants_df, init_df)

  for (nm in stock_names) {
    i_vals <- sort(unique(df[df[["variable"]] == nm, "sim", drop = TRUE]))
    expect_equal(i_vals, seq_len(nr_sims))
  }

  expect_true(length(non_stock_names) > 0)
  for (nm in non_stock_names) {
    i_vals <- sort(unique(df[df[["variable"]] == nm, "sim", drop = TRUE]))
    expect_equal(i_vals, seq_len(nr_sims))
  }
})

test_that("ensemble() only_stocks = TRUE keeps full sim coverage for stocks constants and init", {
  skip_if_julia_not_ready()

  nr_sims <- 5
  sfm <- make_jl_ensemble_sfm()

  sims <- silence(ensemble(
    sfm,
    n = nr_sims,
    only_stocks = TRUE,
    save_sims = TRUE
  ))

  expect_true(sims[["success"]])

  df <- sims[["df"]]
  constants_df <- sims[["constants"]][["df"]]
  init_df <- sims[["init"]][["df"]]

  stock_names <- as.data.frame(sfm, properties = "eqn")
  stock_names <- stock_names[stock_names[["type"]] == "stock", "name", drop = TRUE]

  expect_equal(sort(unique(df[["variable"]])), sort(stock_names))
  expect_equal(length(unique(sims[["summary"]][["variable"]])), length(stock_names))
  expect_ensemble_sim_coverage(nr_sims, df, constants_df, init_df)

  for (nm in stock_names) {
    i_vals <- sort(unique(df[df[["variable"]] == nm, "sim", drop = TRUE]))
    expect_equal(i_vals, seq_len(nr_sims))
  }
})

test_that("ensemble() custom quantiles", {
  skip_if_julia_not_ready()

  sfm <- make_jl_ensemble_sfm()
  sims <- silence(ensemble(sfm, n = 3, quantiles = c(0.1, 0.5, 0.9, 1)))
  expect_true(sims[["success"]])

  q_cols <- grep("^q", colnames(sims[["summary"]]), value = TRUE)
  expect_equal(length(q_cols), 4)
})


# Conditions ------------------------------------

test_that("ensemble() works with single variable conditions", {
  skip_if_julia_not_ready()

  sfm <- make_jl_ensemble_sfm()

  sims <- silence(ensemble(sfm,
    conditions = list("a2" = seq(0.2, 0.8, by = 0.05)),
    n = 3
  ))
  expect_true(sims[["success"]])
})

test_that("ensemble() crossed design computes correct conditions", {
  skip_if_julia_not_ready()

  sfm <- make_jl_ensemble_sfm()

  n <- 3
  sims <- silence(ensemble(sfm,
    conditions = list(
      "a1" = c(1.1, 1.2, 1.3),
      "a2" = c(1.2, 1.3, 1.4)
    ),
    cross = TRUE, n = n, save_sims = FALSE
  ))
  expect_true(sims[["success"]])
  expect_equal(sims[["n"]], n)
  expect_equal(sims[["n_total"]], n * 9)
  expect_equal(sims[["n_conditions"]], 9)
  expect_equal(sort(unique(sims[["summary"]][["condition"]])), 1:9)
})

test_that("ensemble() non-crossed design pairs values", {
  skip_if_julia_not_ready()

  nr_sims <- 3
  nr_cond <- 3
  sfm <- make_jl_ensemble_sfm()

  sims <- silence(ensemble(sfm,
    conditions = list(
      "a2" = c(1.2, 1.3, 1.4),
      "a1" = c(1.1, 1.2, 1.3)
    ),
    cross = FALSE, n = nr_sims, save_sims = TRUE
  ))
  expect_true(sims[["success"]])
  expect_equal(sims[["n"]], nr_sims)
  expect_equal(sims[["n_total"]], nr_sims * nr_cond)
  expect_equal(sort(unique(sims[["df"]][["sim"]])), 1:nr_sims)
  expect_equal(sort(unique(sims[["df"]][["condition"]])), 1:nr_cond)
  expect_equal(sort(unique(sims[["summary"]][["condition"]])), 1:nr_cond)

  cond_df <- as.data.frame(sims[["conditions"]])
  expect_equal(cond_df[["condition"]], 1:nr_cond)
  # Conditions are alphabetically sorted, so a1 comes before a2
  expect_equal(colnames(cond_df), c("condition", "a1", "a2"))
  expect_equal(cond_df[["a1"]], c(1.1, 1.2, 1.3))
  expect_equal(cond_df[["a2"]], c(1.2, 1.3, 1.4))
  expect_equal(unique(sims[["constants"]][["df"]][["sim"]]), 1:nr_sims)
  expect_equal(unique(sims[["constants"]][["df"]][["condition"]]), 1:nr_cond)


  # condition column in summary and df should match conditions table
  df <- as.data.frame(sims, which = "summary")
  expect_equal(sort(unique(df[["condition"]])), seq_len(nr_cond))

  df <- as.data.frame(sims, which = "sims")
  expect_equal(sort(unique(df[["condition"]])), seq_len(nr_cond))
})


test_that("ensemble() with mixed stock and constant in conditions", {
  skip_if_julia_not_ready()

  sfm <- stockflow("predator_prey") |>
    update(c("predator", "prey"), eqn = "runif(1, 30, 50)") |>
    sim_settings(
      language = "Julia",
      dt = 0.1, save_by = 10,
      start = 0, stop = 200
    )

  nr_sims <- 3
  sims <- silence(ensemble(sfm,
    conditions = list(
      "prey" = c(40, 50, 60),
      "delta" = seq(0.015, 0.03, by = 0.005)
    ),
    cross = TRUE, n = nr_sims, save_sims = TRUE,
    only_stocks = TRUE
  ))
  expect_true(sims[["success"]])
  expect_false(is.null(sims[["summary"]]))
  expect_false(is.null(sims[["df"]]))
  expect_equal(length(unique(sims[["summary"]][["variable"]])), 2) # 2 stocks

  nr_cond <- 3 * 4
  expect_equal(sims[["n"]], nr_sims)
  expect_equal(sims[["n_total"]], nr_cond * nr_sims)
  expect_equal(sort(unique(sims[["df"]][["sim"]])), 1:nr_sims)
  expect_equal(sort(unique(sims[["df"]][["condition"]])), 1:nr_cond)
  expect_equal(sort(unique(sims[["summary"]][["condition"]])), 1:nr_cond)
})


# Reproducibility ----------------------------------------

test_that("ensemble in Julia is reproducible with seed", {
  skip_if_julia_not_ready()

  sfm <- stockflow("predator_prey") |>
    sim_settings(
      language = "Julia",
      start = 0, stop = 10, dt = 0.1,
      seed = 123
    ) |>
    update(c("predator", "prey"), eqn = "runif(1)")

  # Should not modify global seed state
  withr::local_seed(123) # ensure .Random.seed exists before capturing it
  orig_seed <- .Random.seed

  sims1 <- silence(ensemble(sfm, n = 3, save_sims = TRUE))
  sims2 <- silence(ensemble(sfm, n = 3, save_sims = TRUE))

  new_seed <- .Random.seed
  expect_equal(orig_seed, new_seed)

  df1 <- as.data.frame(sims1, which = "summary")
  df2 <- as.data.frame(sims2, which = "summary")
  cols <- setdiff(colnames(df1), "variable")
  tol <- 1e-5
  expect_equal(df1[, cols], df2[, cols], tolerance = tol)
  df1 <- as.data.frame(sims1, which = "sims")
  df2 <- as.data.frame(sims2, which = "sims")
  cols <- setdiff(colnames(df1), "variable")
  expect_equal(df1[, cols], df2[, cols], tolerance = tol)

  # Each simulation within an ensemble should be different
  cols <- c("time", "value")
  df1a <- as.data.frame(sims1, which = "sims", sim = 1)
  df1b <- as.data.frame(sims1, which = "sims", sim = 2)
  df1c <- as.data.frame(sims1, which = "sims", sim = 3)
  expect_true(abs(sum(df1a[, cols] - df1b[, cols])) > tol)
  expect_true(abs(sum(df1a[, cols] - df1c[, cols])) > tol)
  expect_true(abs(sum(df1b[, cols] - df1c[, cols])) > tol)

  df2a <- as.data.frame(sims2, which = "sims", sim = 1)
  df2b <- as.data.frame(sims2, which = "sims", sim = 2)
  df2c <- as.data.frame(sims2, which = "sims", sim = 3)
  expect_true(abs(sum(df2a[, cols] - df2b[, cols])) > tol)
  expect_true(abs(sum(df2a[, cols] - df2c[, cols])) > tol)
  expect_true(abs(sum(df2b[, cols] - df2c[, cols])) > tol)
})


test_that("ensemble in Julia with threads is reproducible with seed", {
  skip_if_julia_not_ready()
  use_julia(nthreads = 2)
  on.exit(use_julia(stop = TRUE))

  sfm <- stockflow("predator_prey") |>
    sim_settings(
      language = "Julia",
      start = 0, stop = 10, dt = 0.1,
      seed = 123
    ) |>
    update(c("predator", "prey"), eqn = "runif(1)")

  # Should not modify global seed state
  withr::local_seed(123) # ensure .Random.seed exists before capturing it
  orig_seed <- .Random.seed

  sims1 <- silence(ensemble(sfm, n = 3, save_sims = TRUE))
  sims2 <- silence(ensemble(sfm, n = 3, save_sims = TRUE))

  new_seed <- .Random.seed
  expect_equal(orig_seed, new_seed)

  expect_equal(sims1[["summary"]], sims2[["summary"]])
  expect_equal(sims1[["df"]], sims2[["df"]])

  # Each simulation within an ensemble should be different
  cols <- c("time", "value")
  tol <- 1e-5
  df1a <- as.data.frame(sims1, which = "sims", sim = 1)
  df1b <- as.data.frame(sims1, which = "sims", sim = 2)
  df1c <- as.data.frame(sims1, which = "sims", sim = 3)
  expect_true(abs(sum(df1a[, cols] - df1b[, cols])) > tol)
  expect_true(abs(sum(df1a[, cols] - df1c[, cols])) > tol)
  expect_true(abs(sum(df1b[, cols] - df1c[, cols])) > tol)

  df2a <- as.data.frame(sims2, which = "sims", sim = 1)
  df2b <- as.data.frame(sims2, which = "sims", sim = 2)
  df2c <- as.data.frame(sims2, which = "sims", sim = 3)
  expect_true(abs(sum(df2a[, cols] - df2b[, cols])) > tol)
  expect_true(abs(sum(df2a[, cols] - df2c[, cols])) > tol)
  expect_true(abs(sum(df2b[, cols] - df2c[, cols])) > tol)
})


test_that("ensemble in Julia without seed", {
  skip_if_julia_not_ready()

  sfm <- stockflow("predator_prey") |>
    sim_settings(
      language = "Julia",
      start = 0, stop = 10, dt = 0.1,
      seed = NULL
    ) |>
    update(c("predator", "prey"), eqn = "runif(1)")

  sims1 <- silence(ensemble(sfm, n = 3, save_sims = TRUE))
  sims2 <- silence(ensemble(sfm, n = 3, save_sims = TRUE))

  tol <- 1e-5
  df1 <- as.data.frame(sims1, which = "summary")
  df2 <- as.data.frame(sims2, which = "summary")
  cols <- setdiff(colnames(df1), "variable")

  expect_true(abs(sum(df1[, cols] - df2[, cols])) > tol)
  cols <- c("time", "value")
  df1 <- as.data.frame(sims1, which = "sims")
  df2 <- as.data.frame(sims2, which = "sims")
  expect_true(abs(sum(df1[, cols] - df2[, cols])) > tol)
})

# Edge cases --------------------------------------------

test_that("ensemble() works with single time point", {
  skip_if_julia_not_ready()

  sfm <- stockflow("predator_prey") |>
    sim_settings(
      language = "Julia",
      start = 0, stop = 5, dt = 0.1,
      save_length = 1
    ) |>
    update(c("predator", "prey"), eqn = "runif(1)")

  sims <- silence(ensemble(sfm, n = 3))
  expect_true(sims[["success"]])
  expect_equal(length(unique(sims[["summary"]][["time"]])), 1)
  expect_silent(plot(sims))

  # Also works with save_sims
  sims <- silence(ensemble(sfm, n = 3, save_sims = TRUE))
  expect_true(sims[["success"]])
  expect_silent(plot(sims))
  expect_silent(plot(sims, which = "sims"))
})

test_that("ensemble() works with interpolation function", {
  skip_if_julia_not_ready()

  sfm <- stockflow("logistic_model") |>
    sim_settings(
      language = "Julia",
      start = 0, stop = 50, dt = 0.1,
      save_length = 1
    ) |>
    update("X", eqn = "runif(1, 0, K)") |>
    update("input", "constant", eqn = "pulse(times, 10, width = dt, height = .01)") |>
    update("inflow2", "flow", eqn = "input(t)", to = "X")

  sims <- silence(ensemble(sfm, n = 3))
  expect_true(sims[["success"]])
  expect_silent(plot(sims))
})


# Quiet messages ------------------------------------------

test_that("ensemble() prints simulation count", {
  skip_if_julia_not_ready()

  sfm <- make_jl_ensemble_sfm()

  # Basic ensemble
  expect_message(
    ensemble(sfm, n = 3, quiet = FALSE),
    "Starting"
  )

  # Ensemble with conditions: 3 x 3 = 9 conditions, 15 per condition = 135 total
  expect_message(
    ensemble(sfm,
      conditions = list(
        "a1" = c(1.1, 1.2, 1.3),
        "a2" = c(1.2, 1.3, 1.4)
      ),
      cross = TRUE, n = 3, quiet = FALSE,
      save_sims = TRUE
    ),
    "conditions"
  )
})


# Snapshot tests for error messages ----------------------------------------

cli::test_that_cli(configs = c("plain", "ansi"), "ensemble() error: invalid n", {
  sfm <- make_ensemble_error_sfm()
  expect_snapshot(ensemble(sfm, n = 0), error = TRUE)
})

cli::test_that_cli(configs = c("plain", "ansi"), "ensemble() error: invalid quantiles", {
  sfm <- make_ensemble_error_sfm()
  expect_snapshot(ensemble(sfm, n = 3, quantiles = 0.5), error = TRUE)
})

cli::test_that_cli(configs = c("plain", "ansi"), "ensemble() error: non-numeric conditions", {
  sfm <- make_ensemble_error_sfm()
  expect_snapshot(ensemble(sfm, n = 3, conditions = list("S" = "abc")), error = TRUE)
})


# check_ensemble_stockflow() ---------------------------------------------------

# Error conditions require corrupted/wrong-class objects

test_that("check_ensemble_stockflow() rejects non-ensemble objects", {
  expect_error(check_ensemble_stockflow(list(success = TRUE)), "ensemble_stockflow")
  expect_error(check_ensemble_stockflow("string"), "ensemble_stockflow")
  expect_error(check_ensemble_stockflow(42), "ensemble_stockflow")
})

test_that("validate_ensemble_stockflow() assesses success, summary, duration", {
  skip_if_julia_not_ready()
  sims0 <- sims <- silence(ensemble(make_jl_ensemble_sfm(), n = 3))
  sims$success <- "yes"
  expect_error(validate_ensemble_stockflow(sims), "success")

  sims$success <- c(TRUE, FALSE)
  expect_error(validate_ensemble_stockflow(sims), "success")

  sims <- sims0
  sims$summary <- NULL
  expect_error(validate_ensemble_stockflow(sims), "summary")

  sims$summary <- "not a df"
  expect_error(validate_ensemble_stockflow(sims), "summary")

  sims <- sims0
  sims$duration <- NULL
  expect_error(validate_ensemble_stockflow(sims), "duration")

  # check_ensemble_stockflow() passes for valid successful ensemble
  expect_no_error(check_ensemble_stockflow(sims0))
  expect_invisible(check_ensemble_stockflow(sims0))
})


# print.ensemble_stockflow() ---------------------------------------------------

test_that("print() returns x invisibly", {
  skip_if_julia_not_ready()
  sims <- silence(ensemble(make_jl_ensemble_sfm(), n = 3))
  expect_invisible(silence(print(sims)))
  expect_identical(silence(withVisible(print(sims))$value), sims)
})

cli::test_that_cli(configs = "plain", "print() success output matches snapshot", {
  sfm <- make_r_ensemble_random_sfm() |>
    meta(name = "Demo model")

  sims <- silence(ensemble(sfm, n = 3, quiet = TRUE))
  sims$duration <- structure(1.234, class = "difftime", units = "secs")
  expect_snapshot(print(sims))
})

cli::test_that_cli(configs = "plain", "print() success with conditions lists changed parameters", {
  sfm <- make_r_ensemble_random_sfm() |>
    meta(name = "Demo model")

  sims <- silence(ensemble(sfm,
    n = 3,
    save_sims = TRUE,
    conditions = list(
      contact_rate = c(1.5, 2.5),
      infection_rate = c(1, 3)
    ),
    quiet = TRUE
  ))
  sims$duration <- structure(1.234, class = "difftime", units = "secs")
  expect_snapshot(print(sims))
})

cli::test_that_cli(configs = "plain", "ensemble setup rejects malformed equations early", {
  expect_error(
    stockflow() |>
      meta(name = "Demo model") |>
      update("S", type = "stock", eqn = "1 // 2") |>
      update("F1", type = "flow", eqn = "S", to = "S") |>
      sim_settings(language = "R"),
    "Could not parse the equation for .*S",
    ignore.case = TRUE
  )
})


# as.data.frame.ensemble_stockflow() -------------------------------------------


test_that("as.data.frame() and summary() return the summary df", {
  skip_if_julia_not_ready()
  sims <- silence(ensemble(make_jl_ensemble_sfm(), n = 3))
  df <- as.data.frame(sims)
  expect_s3_class(df, "data.frame")
  # Default and which = "summary" both return the summary df
  expect_identical(df, sims[["summary"]])
  expect_identical(as.data.frame(sims, which = "summary"), sims[["summary"]])
  # summary() is an alias for the as.data.frame() default
  expect_identical(summary(sims), sims[["summary"]])
  expect_identical(summary(sims), as.data.frame(sims))
})

test_that("as.data.frame() which = 'sims' returns individual sims df", {
  skip_if_julia_not_ready()
  sims <- silence(ensemble(make_jl_ensemble_sfm(), n = 3, save_sims = TRUE))
  df <- as.data.frame(sims, which = "sims")
  expect_identical(df, sims[["df"]])
  expect_s3_class(df, "data.frame")
})

test_that("as.data.frame() which = 'sims' errors when save_sims = FALSE", {
  skip_if_julia_not_ready()
  sims <- silence(ensemble(make_jl_ensemble_sfm(), n = 3, save_sims = FALSE))
  expect_error(as.data.frame(sims, which = "sims"), "save_sims")
})

test_that("as.data.frame() individual sims df has expected columns", {
  skip_if_julia_not_ready()
  sims <- silence(ensemble(make_jl_ensemble_sfm(), n = 3, save_sims = TRUE))
  df <- as.data.frame(sims, which = "sims")
  expect_true(all(c("sim", "condition", "variable", "time", "value") %in% names(df)))
})

test_that("as.data.frame() direction = 'wide' widens summary and sims", {
  skip_if_julia_not_ready()
  sims <- silence(ensemble(make_jl_ensemble_sfm(), n = 3, save_sims = TRUE))
  df_long <- as.data.frame(sims, direction = "long")
  df_wide <- as.data.frame(sims, direction = "wide")
  # Wide has fewer rows (one per condition+time, not per condition+time+variable)
  expect_lt(nrow(df_wide), nrow(df_long))
  # 'variable' column should be absorbed into column names
  expect_false("variable" %in% names(df_wide))
  # condition and time remain as id columns
  expect_true("condition" %in% names(df_wide))
  expect_true("time" %in% names(df_wide))

  # as.data.frame() direction = 'wide' widens individual sims
  df_long <- as.data.frame(sims, which = "sims", direction = "long")
  df_wide <- as.data.frame(sims, which = "sims", direction = "wide")
  expect_lt(nrow(df_wide), nrow(df_long))
  expect_false("variable" %in% names(df_wide))
  expect_true(all(c("sim", "condition", "time") %in% names(df_wide)))
  # Variable names appear as columns (there are stocks in crielaard2022)
  stock_names <- unique(df_long[["variable"]])
  expect_true(any(stock_names %in% names(df_wide)))
})

test_that("as.data.frame() rejects invalid direction", {
  skip_if_julia_not_ready()
  sims <- silence(ensemble(make_jl_ensemble_sfm(), n = 3))
  expect_error(as.data.frame(sims, direction = "diagonal"), "direction")
  expect_error(as.data.frame(sims, direction = ""), "direction")
})

test_that("as.data.frame() row.names sets row names", {
  skip_if_julia_not_ready()
  sims <- silence(ensemble(make_jl_ensemble_sfm(), n = 3))
  df <- as.data.frame(sims)
  rn <- paste0("row", seq_len(nrow(df)))
  df_named <- as.data.frame(sims, row.names = rn)
  expect_equal(rownames(df_named), rn)
})

test_that("as.data.frame() row.names length mismatch errors", {
  skip_if_julia_not_ready()
  sims <- silence(ensemble(make_jl_ensemble_sfm(), n = 3))
  expect_error(as.data.frame(sims, row.names = c("a", "b")), "mismatch|row.names|row names")
})


# head.ensemble_stockflow() / tail.ensemble_stockflow() ------------------------

test_that("head() and tail() return a data.frame with correct number of rows from summary", {
  skip_if_julia_not_ready()
  sims <- silence(ensemble(make_jl_ensemble_sfm(), n = 3))
  h <- head(sims, n = 4L)
  expect_s3_class(h, "data.frame")
  expect_equal(nrow(h), 4L)
  expect_equal(h, head(sims[["summary"]], 4L))

  t <- tail(sims, n = 4L)
  expect_s3_class(t, "data.frame")
  expect_equal(nrow(t), 4L)
  expect_equal(
    unname(as.matrix(t)),
    unname(as.matrix(tail(sims[["summary"]], 4L)))
  )

  # direction = "wide" is passed through to as.data.frame()
  h_long <- head(sims, n = 3L, direction = "long")
  h_wide <- head(sims, n = 3L, direction = "wide")
  expect_true("variable" %in% names(h_long))
  expect_false("variable" %in% names(h_wide))
})


test_that("head() and tail() pass which = 'sims' through to individual sims", {
  skip_if_julia_not_ready()
  sims <- silence(ensemble(make_jl_ensemble_sfm(), n = 3, save_sims = TRUE))
  h <- head(sims, n = 3L, which = "sims")
  expect_equal(h, head(sims[["df"]], 3L))

  t <- tail(sims, n = 3L, which = "sims")
  expect_equal(
    unname(as.matrix(t)),
    unname(as.matrix(tail(sims[["df"]], 3L)))
  )
})


# summary.ensemble_stockflow() -------------------------------------------------

test_that("summary() result has expected columns", {
  skip_if_julia_not_ready()
  sims <- silence(ensemble(make_jl_ensemble_sfm(), n = 3))
  s <- summary(sims)
  expect_true(all(c("condition", "variable", "time", "mean", "median") %in% names(s)))
  q_cols <- grep("^q", names(s), value = TRUE)
  expect_gt(length(q_cols), 0)
})
