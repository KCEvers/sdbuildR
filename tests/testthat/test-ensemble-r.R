# Tests for ensemble_r() — R backend for ensemble simulations


# Basic R ensemble --------------------------------------------------------

test_that("ensemble() runs in R", {
  sfm <- make_r_ensemble_random_sfm()
  sims <- silence(ensemble(sfm, n = 2, quiet = TRUE))

  # ensemble() R returns correct structure
  expect_successful_ensemble(sims, c(
    "success", "df", "summary", "n", "n_total",
    "n_conditions", "conditions", "init", "constants",
    "duration"
  ))
})

test_that("ensemble() R handles models with no constants", {
  sfm <- make_basic_sfm() |>
    sim_settings(language = "R", start = 0, stop = 10, dt = 0.1, save_by = 1)

  sims <- silence(ensemble(sfm,
    n = 2, save_sims = TRUE,
    central = c("mean", "median"), spread = c("quantile", "sd", "range"),
    quiet = TRUE
  ))

  expect_true(sims[["success"]])
  expect_equal(nrow(sims[["constants"]][["df"]]), 0)
  expect_equal(nrow(sims[["constants"]][["summary"]]), 0)
  expect_equal(
    names(sims[["constants"]][["df"]]),
    c("sim", "condition", "variable", "value")
  )
  expect_true(
    all(c("condition", "variable", "mean", "median", "sd", "min", "max", "quant1", "quant2") %in%
      names(sims[["constants"]][["summary"]]))
  )
})

test_that("ensemble() accepts lenient central/spread spellings", {
  sfm <- make_basic_sfm() |>
    sim_settings(language = "R", start = 0, stop = 10, dt = 0.1, save_by = 1)

  # Plurals and case variants canonicalise to the catalog column names.
  # "range" expands to min/max columns.
  sims <- silence(ensemble(sfm,
    n = 2, central = c("Means", "medians"), spread = c("SDs", "ranges"),
    quiet = TRUE
  ))

  expect_true(sims[["success"]])
  expect_true(all(c("mean", "median", "sd", "min", "max") %in% names(sims[["summary"]])))

  # Unrecognised choices are still rejected.
  expect_error(
    silence(ensemble(sfm, n = 2, spread = "iqr", quiet = TRUE)),
    "Invalid"
  )
  expect_error(
    silence(ensemble(sfm, n = 2, central = "avg2", quiet = TRUE)),
    "Invalid"
  )
})

test_that("ensemble() R respects only_stocks = TRUE", {
  sfm <- make_r_ensemble_random_sfm()
  df <- as.data.frame(sfm, properties = "eqn")
  n_stocks <- nrow(df[df[["type"]] == "stock", ])

  sims <- silence(ensemble(sfm, n = 2, only_stocks = TRUE, quiet = TRUE))
  expect_true(sims[["success"]])
  expect_equal(
    length(unique(sims[["summary"]][["variable"]])),
    n_stocks
  )
})

test_that("ensemble() R returns all variables with only_stocks = FALSE", {
  sfm <- make_r_ensemble_random_sfm()
  df <- as.data.frame(sfm, properties = "eqn")
  n_all <- nrow(df[df[["type"]] %in% c("stock", "flow", "aux"), ])

  sims <- silence(ensemble(sfm,
    n = 2, only_stocks = FALSE,
    quiet = TRUE
  ))
  expect_true(sims[["success"]])
  expect_equal(
    length(unique(sims[["summary"]][["variable"]])),
    n_all
  )
})

test_that("ensemble() R filters outputs to vars", {
  sfm <- make_r_ensemble_random_sfm() |>
    sim_settings(vars = c("susceptible", "new_infections"))

  sims <- silence(ensemble(sfm,
    n = 2,
    save_sims = TRUE,
    quiet = TRUE
  ))

  expect_true(sims[["success"]])
  expect_equal(sort(unique(sims[["summary"]][["variable"]])), c("new_infections", "susceptible"))
  expect_equal(sort(unique(sims[["df"]][["variable"]])), c("new_infections", "susceptible"))
})

test_that("ensemble() R returns correct n properties", {
  nr_sims <- 2
  sfm <- make_r_ensemble_random_sfm()

  sims <- silence(ensemble(sfm,
    n = nr_sims, save_sims = TRUE,
    quiet = TRUE
  ))
  expect_equal(sims[["n"]], nr_sims)
  expect_equal(sims[["n_total"]], nr_sims)
  expect_equal(sims[["n_conditions"]], 1)
  expect_unique_values(sims[["df"]], "sim", seq_len(nr_sims))
  expect_unique_values(sims[["df"]], "condition", 1)
  expect_unique_values(sims[["summary"]], "condition", 1)
})

test_that("ensemble() R custom quantiles", {
  sfm <- make_r_ensemble_random_sfm()
  sims <- silence(ensemble(sfm,
    n = 2, quantiles = c(0.1, 0.5, 0.9, 1),
    quiet = TRUE
  ))
  expect_true(sims[["success"]])
  q_cols <- grep("^q", colnames(sims[["summary"]]), value = TRUE)
  expect_equal(length(q_cols), 4)
})


# Conditions in R ---------------------------------------------------------

test_that("ensemble() R works with single variable conditions", {
  sfm <- make_r_ensemble_random_sfm()
  sims <- silence(ensemble(sfm,
    conditions = list("contact_rate" = c(1.5, 2, 2.5)),
    n = 2, quiet = TRUE
  ))
  expect_true(sims[["success"]])
  expect_equal(sims[["n_conditions"]], 3)
})

test_that("ensemble() R crossed design computes correct conditions", {
  sfm <- make_r_ensemble_random_sfm()
  n <- 2
  sims <- silence(ensemble(sfm,
    conditions = list(
      "contact_rate" = c(1.5, 2.5),
      "infection_rate" = c(1, 3)
    ),
    cross = TRUE, n = n, quiet = TRUE
  ))
  expect_true(sims[["success"]])
  expect_equal(sims[["n"]], n)
  expect_equal(sims[["n_total"]], n * 4)
  expect_equal(sims[["n_conditions"]], 4)
  expect_equal(sort(unique(sims[["summary"]][["condition"]])), 1:4)
})

test_that("ensemble() R non-crossed design pairs values", {
  sfm <- make_r_ensemble_random_sfm()
  nr_sims <- 2
  nr_cond <- 3
  sims <- silence(ensemble(sfm,
    conditions = list(
      "contact_rate" = c(1.5, 2, 2.5),
      "infection_rate" = c(1, 2, 3)
    ),
    cross = FALSE, n = nr_sims, save_sims = TRUE, quiet = TRUE
  ))
  expect_true(sims[["success"]])
  expect_equal(sims[["n"]], nr_sims)
  expect_equal(sims[["n_total"]], nr_sims * nr_cond)
  expect_unique_values(sims[["df"]], "sim", seq_len(nr_sims))
  expect_unique_values(sims[["df"]], "condition", seq_len(nr_cond))


  # ensemble() R conditions data frame is correct
  cond_df <- as.data.frame(sims[["conditions"]])
  expect_equal(cond_df[["condition"]], 1:nr_cond)
  # Alphabetically sorted: infection_rate before contact_rate
  expect_equal(names(cond_df), c("condition", "contact_rate", "infection_rate"))
  expect_equal(cond_df[["infection_rate"]], c(1, 2, 3))
  expect_equal(cond_df[["contact_rate"]], c(1.5, 2, 2.5))
})


# Output compatibility ----------------------------------------------------

test_that("ensemble() R result works with as.data.frame(), summary() and print()", {
  sfm <- make_r_ensemble_random_sfm()
  sims <- silence(ensemble(sfm, n = 2, save_sims = TRUE, quiet = TRUE))

  df_summary <- as.data.frame(sims, which = "summary")
  expect_s3_class(df_summary, "data.frame")
  expect_true(all(c("condition", "variable", "time", "mean", "median") %in% names(df_summary)))

  df_sims <- as.data.frame(sims, which = "sims")
  expect_s3_class(df_sims, "data.frame")
  expect_true(all(c("sim", "condition", "variable", "time", "value") %in% names(df_sims)))

  df_constants <- as.data.frame(sims, which = "sims", type = "constant")
  expect_s3_class(df_constants, "data.frame")
  expect_equal(df_constants, sims[["constants"]][["df"]])
  expect_gt(nrow(df_constants), 0L)
  expect_true(all(c("sim", "condition", "variable", "value") %in% names(df_constants)))

  h_constants <- head(sims, n = 1L, which = "sims", type = "constant")
  expect_s3_class(h_constants, "data.frame")
  expect_equal(h_constants, head(sims[["constants"]][["df"]], 1L))

  summary_constants <- as.data.frame(sims, which = "summary", type = "constant")
  expect_s3_class(summary_constants, "data.frame")
  expect_equal(summary_constants, sims[["constants"]][["summary"]])
  expect_gt(nrow(summary_constants), 0L)

  # Check that head() and tail() work on the ensemble result
  h <- head(sims, n = 3L)
  t <- tail(sims, n = 3L)
  expect_s3_class(h, "data.frame")
  expect_s3_class(t, "data.frame")
  expect_equal(nrow(h), 3L)
  expect_equal(nrow(t), 3L)

  # Check that summary() works on the ensemble result
  s <- summary(sims)
  expect_identical(s, sims[["summary"]])


  # Check that print() works on the ensemble result
  expect_invisible(silence(print(sims)))
})


# Quiet messages ----------------------------------------------------------

test_that("ensemble() R verbosity is controlled by quiet", {
  sfm <- make_r_ensemble_random_sfm()

  expect_message(
    ensemble(sfm, n = 2, quiet = FALSE),
    "Starting"
  )
  expect_no_message(ensemble(sfm, n = 2, quiet = TRUE))
})

test_that("ensemble() R accepts deprecated verbose argument", {
  sfm <- make_r_ensemble_random_sfm()

  expect_warning(
    expect_no_message(ensemble(sfm, n = 2, verbose = FALSE)),
    "deprecated"
  )
})

test_that("progressr_lapply() signals one progress update per task when not quiet", {
  events <- character()
  handler <- progressr::make_progression_handler(
    name = "test",
    enable = TRUE,
    reporter = list(
      update = function(config, state, progression, ...) {
        if (progression$amount > 0) {
          events <<- c(events, paste0("update:", state$step))
        }
      }
    )
  )

  out <- progressr_lapply(
    as.list(1:3),
    function(x) x * 2,
    quiet = FALSE,
    handler = handler
  )

  expect_equal(out, list(2, 4, 6))
  expect_equal(events, c("update:1", "update:2", "update:3"))
})

test_that("progressr_lapply() suppresses progress updates when quiet", {
  events <- character()
  handler <- progressr::make_progression_handler(
    name = "test",
    enable = TRUE,
    reporter = list(
      update = function(config, state, progression, ...) {
        if (progression$amount > 0) {
          events <<- c(events, paste0("update:", state$step))
        }
      }
    )
  )

  out <- progressr_lapply(
    as.list(1:3),
    function(x) x * 2,
    quiet = TRUE,
    handler = handler
  )

  expect_equal(out, list(2, 4, 6))
  expect_equal(events, character(0))
})

test_that("progressr_lapply() default cli handler runs without type errors", {
  out <- expect_no_error(progressr_lapply(
    as.list(1:3),
    function(x) x * 2,
    quiet = FALSE
  ))

  expect_equal(out, list(2, 4, 6))
})


# Edge cases --------------------------------------------------------------

test_that("ensemble() R works with n = 1", {
  sfm <- make_r_ensemble_random_sfm()
  sims <- silence(ensemble(sfm, n = 1, quiet = TRUE))
  expect_true(sims[["success"]])
  expect_equal(sims[["n"]], 1)
  expect_equal(sims[["n_total"]], 1)
})

test_that("ensemble() R works with conditions and n = 1", {
  sfm <- make_r_ensemble_random_sfm()
  sims <- silence(ensemble(sfm,
    n = 1,
    conditions = list("contact_rate" = c(1.5, 2.5)),
    quiet = TRUE
  ))
  expect_true(sims[["success"]])
  expect_equal(sims[["n"]], 1)
  expect_equal(sims[["n_total"]], 2)
  expect_equal(sims[["n_conditions"]], 2)
})

test_that("ensemble() in R respects seed", {
  sfm <- make_r_ensemble_random_sfm() |> sim_settings(seed = 123)

  # Should not modify global seed state
  withr::local_seed(123) # ensure .Random.seed exists before capturing it
  orig_seed <- .Random.seed

  sims1 <- silence(ensemble(sfm, n = 2, quiet = TRUE, save_sims = TRUE))
  sims2 <- silence(ensemble(sfm, n = 2, quiet = TRUE, save_sims = TRUE))

  new_seed <- .Random.seed
  expect_true(identical(orig_seed, new_seed))

  expect_equal(sims1[["summary"]], sims2[["summary"]])
  expect_equal(sims1[["df"]], sims2[["df"]])

  # Each simulation within an ensemble should be different
  cols <- c("time", "value")
  tol <- 1e-5
  df1a <- as.data.frame(sims1, which = "sims", sim = 1)
  df1b <- as.data.frame(sims1, which = "sims", sim = 2)
  expect_true(abs(sum(df1a[, cols] - df1b[, cols])) > tol)

  df2a <- as.data.frame(sims2, which = "sims", sim = 1)
  df2b <- as.data.frame(sims2, which = "sims", sim = 2)
  expect_true(abs(sum(df2a[, cols] - df2b[, cols])) > tol)
})


test_that("ensemble() in R with parallel execution respects seed", {
  skip_on_cran()

  sfm <- make_r_ensemble_random_sfm() |> sim_settings(seed = 123)

  future::plan(future::multisession, workers = 2)
  on.exit(future::plan(future::sequential), add = TRUE)

  # Should not modify global seed state
  withr::local_seed(123) # ensure .Random.seed exists before capturing it
  orig_seed <- .Random.seed

  sims1 <- silence(ensemble(sfm, n = 2, quiet = TRUE, save_sims = TRUE))
  sims2 <- silence(ensemble(sfm, n = 2, quiet = TRUE, save_sims = TRUE))

  new_seed <- .Random.seed
  expect_equal(orig_seed, new_seed)

  expect_equal(sims1[["summary"]], sims2[["summary"]])
  expect_equal(sims1[["df"]], sims2[["df"]])

  # Each simulation within an ensemble should be different
  cols <- c("time", "value")
  tol <- 1e-5
  df1a <- as.data.frame(sims1, which = "sims", sim = 1)
  df1b <- as.data.frame(sims1, which = "sims", sim = 2)
  expect_true(abs(sum(df1a[, cols] - df1b[, cols])) > tol)

  df2a <- as.data.frame(sims2, which = "sims", sim = 1)
  df2b <- as.data.frame(sims2, which = "sims", sim = 2)
  expect_true(abs(sum(df2a[, cols] - df2b[, cols])) > tol)
})


test_that("ensemble() in R without seed", {
  sfm <- make_r_ensemble_random_sfm() |> sim_settings(seed = NULL)
  sims1 <- silence(ensemble(sfm, n = 2, quiet = TRUE, save_sims = TRUE))
  sims2 <- silence(ensemble(sfm, n = 2, quiet = TRUE, save_sims = TRUE))

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

# Parallel execution via user-managed future plan -------------------------

test_that("ensemble() R runs sequentially with future::sequential plan", {
  future::plan(future::sequential)
  on.exit(future::plan(future::sequential), add = TRUE)

  sfm <- make_r_ensemble_random_sfm()
  sims <- silence(ensemble(sfm, n = 2, quiet = TRUE))

  expect_true(sims[["success"]])
  expect_equal(future::nbrOfWorkers(), 1L)
})

test_that("ensemble() R uses parallel path when future plan has multiple workers", {
  skip_on_cran()

  future::plan(future::multisession, workers = 2)
  on.exit(future::plan(future::sequential), add = TRUE)

  n <- 4
  sfm <- make_r_ensemble_random_sfm()
  sims <- silence(ensemble(sfm, n = n, quiet = TRUE))

  expect_true(sims[["success"]])
  expect_equal(sims[["n"]], n)
  expect_gt(future::nbrOfWorkers(), 1L)
})

test_that("ensemble() R reports progress without error under future plan", {
  skip_on_cran()

  future::plan(future::multisession, workers = 2)
  on.exit(future::plan(future::sequential), add = TRUE)

  sfm <- make_r_ensemble_random_sfm()
  sims <- suppressWarnings(expect_no_error(ensemble(sfm, n = 2, quiet = FALSE)))

  expect_true(sims[["success"]])
  expect_gt(future::nbrOfWorkers(), 1L)
})


test_that("ensemble respects sim_settings save_sims and per-call override via ...", {
  sfm <- stockflow("sir") |>
    sim_settings(save_sims = TRUE)

  ens_keep <- ensemble(sfm, n = 2)
  expect_true(!is.null(ens_keep$df))

  ens_drop <- ensemble(sfm, n = 2, save_sims = FALSE)
  expect_null(ens_drop$df)
})


# ============================================================================
# as.data.frame.ensemble_stockflow() — vars/type filtering
# ============================================================================

test_that("as.data.frame(ens, which='summary', type=) filters by type", {
  ens <- make_r_ens(n = 3, save_sims = TRUE)
  df <- as.data.frame(ens, which = "summary", type = "stock")
  stock_names <- as.data.frame(stockflow("sir"), type = "stock")$name
  expect_true(all(unique(df$variable) %in% stock_names))
})

test_that("as.data.frame(ens, which='sims', vars=) accepts bare names", {
  ens <- make_r_ens(n = 3, save_sims = TRUE)
  expect_equal(unique(as.data.frame(ens, which = "sims", vars = infected)$variable), "infected")
  expect_equal(unique(as.data.frame(ens, which = "sims", vars = "infected")$variable), "infected")
})

test_that("as.data.frame(ens) vars filter is respected in wide format", {
  ens <- make_r_ens(n = 3, save_sims = TRUE)
  w <- as.data.frame(ens, which = "sims", vars = "infected", direction = "wide")
  expect_true("infected" %in% names(w))
  expect_false("susceptible" %in% names(w))
})

test_that("as.data.frame(ens): both vars and type warns and uses vars", {
  ens <- make_r_ens(n = 3, save_sims = TRUE)
  expect_warning(df <- as.data.frame(ens, which = "summary", type = "stock", vars = "infected"))
  expect_equal(unique(df$variable), "infected")
})

test_that("as.data.frame(ens): unknown vars errors as a typo", {
  ens <- make_r_ens(n = 3, save_sims = TRUE)
  expect_error(as.data.frame(ens, which = "summary", vars = "does_not_exist"), "not.*variable")
})

test_that("as.data.frame(ens): variable in model but not saved gives informative error", {
  ens <- make_r_ens(n = 3, save_sims = TRUE)
  # SIR ensemble saves stocks only by default; flows exist in the model but are unsaved
  expect_error(as.data.frame(ens, which = "summary", vars = "new_infections"), "not saved in the output")
  expect_error(as.data.frame(ens, which = "summary", vars = "new_infections"), "only_stocks = FALSE")
})
