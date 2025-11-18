test_that("ensemble works", {
  testthat::skip_on_cran()
  testthat::skip_if_not(julia_status()$status == "ready")

  # If you already have random elements in the model, no need to specify what to vary
  sfm <- xmile("Crielaard2022") |> sim_specs(
    language = "Julia",
    start = 0, stop = 10,
    dt = .1,
    save_from = 5, save_at = 1
  )
  df <- as.data.frame(sfm, properties = "eqn")

  sims <- expect_error(
    ensemble(sfm, n = 0),
    "The number of simulations must be greater than 0"
  )
  sims <- expect_no_error(ensemble(sfm))
  expect_true(sims$success)

  # Specifying quantiles
  expect_error(
    ensemble(sfm, quantiles = 0.1),
    "quantiles should have a minimum length of two"
  )
  expect_error(
    ensemble(sfm, quantiles = c(0.1, 0.1)),
    "quantiles should have a minimum length of two"
  )
  expect_error(
    ensemble(sfm, quantiles = c(-0.1, 0.1)),
    "quantiles should be between 0 and 1"
  )
  expect_error(
    ensemble(sfm, quantiles = c(0.7, 1.1)),
    "quantiles should be between 0 and 1"
  )
  sims <- expect_no_error(ensemble(sfm, quantiles = c(0.1, 0.5, 0.9, 1)))
  expect_true(sims$success)
  expect_equal(sum(grepl("^q", colnames(sims$summ))), 4) # 4 quantiles

  # Only stocks
  sims <- expect_no_error(ensemble(sfm, n = 15, only_stocks = TRUE, return_sims = FALSE))
  expect_true(sims$success)
  expect_equal(length(unique(sims$summ$variable)), nrow(df[df[["type"]] == "stock", ])) # 3 stocks

  # All variables
  nr_sims <- 15
  sims <- expect_no_error(ensemble(sfm,
    n = nr_sims,
    only_stocks = FALSE, return_sims = TRUE
  ))
  expect_true(sims$success)
  expect_equal(!is.null(sims[["summary"]]), TRUE)
  expect_equal(!is.null(sims[["df"]]), TRUE)
  expect_equal(
    length(unique(sims$summ$variable)),
    nrow(df[df[["type"]] %in% c("stock", "flow", "aux"), ])
  ) # 3 stocks

  # Check whether all variables have the same timeseries length
  table_lengths <- with(sims$summ, table(variable))
  expect_equal(length(unique(table_lengths)), 1)

  # Check returned properties
  expect_equal(sims[["n"]], nr_sims)
  expect_equal(sims[["n_total"]], nr_sims)
  expect_equal(sims[["n_conditions"]], 1)
  expect_equal(sort(unique(sims[["df"]][["i"]])), 1:nr_sims)
  expect_equal(sort(unique(sims[["df"]][["j"]])), 1)
  expect_equal(sort(unique(sims[["summary"]][["j"]])), 1)

  # Check returned constants and init
  expect_equal(sort(unique(sims[["constants"]][["summary"]][["variable"]])),
               c("a0", "a1", "a2"))
  expect_equal(sort(unique(sims[["init"]][["summary"]][["variable"]])), c(
    "Compensatory_behaviour",
    "Food_intake",
    "Hunger"
  ))
  expect_equal(max(as.numeric(sims[["constants"]][["df"]][["i"]])), nr_sims)
  expect_equal(max(as.numeric(sims[["init"]][["df"]][["i"]])), nr_sims)

  # Check plot
  expect_no_error(expect_no_message(plot(sims)))
  expect_no_error(plot(sims, j = 1))
  expect_error(plot(sims, type = "NA"), "type must be one of 'summary' or 'sims")
  expect_error(plot(sims, j = c(3, 6, 9)), "There is only one condition\\. Set j = 1")
  expect_message(
    plot(sims, i = nr_sims - 1),
    "i is not used when type = 'summary'\\. Set type = 'sims' to plot individual trajectories"
  )
  expect_no_error(plot(sims, type = "sims", i = nr_sims - 1))
  expect_no_error(plot(sims, central_tendency = "median"))
  expect_error(
    plot(sims, central_tendency = "medians"),
    "central_tendency must be 'mean', 'median', or FALSE"
  )


  # Message printed
  expect_message(
    ensemble(sfm,
      range = list(
        "a1" = c(1.1, 1.2, 1.3),
        "a2" = c(1.2, 1.3, 1.4)
      ),
      cross = TRUE, n = 15, verbose = TRUE,
      return_sims = TRUE
    ),
    "Running a total of 135 simulations for 9 conditions \\(15 simulations per condition\\)"
  )

  # Check duplicates in range
  expect_error(ensemble(sfm,
    range = list(
      "a2" = seq(0.2, 0.8, by = 0.05),
      "a2" = c(1.3, 1.4, 1.5)
    ),
    n = 100
  ), "All names in range must be unique")

  # Check output in sims
  sims <- expect_no_error(ensemble(sfm,
    range = list(
      "a2" = c(0.2, 0.3, 0.4),
      "a1" = c(1.3, 1.4, 1.5)
    ),
    cross = FALSE,
    n = 10, return_sims = TRUE
  ))
  expect_true(sims$success)
  expect_equal(as.data.frame(sims$conditions)$j, 1:3)
  expect_equal(as.data.frame(sims$conditions)$a2, c(0.2, 0.3, 0.4))
  expect_equal(as.data.frame(sims$conditions)$a1, c(1.3, 1.4, 1.5))
  expect_equal(unique(sims$constants$df$i), 1:10)
  expect_equal(unique(sims$constants$df$j), 1:3)
})


test_that("plotting ensemble also works with singular time point", {
  testthat::skip_on_cran()
  testthat::skip_if_not(julia_status()$status == "ready")

  # If you already have random elements in the model, no need to specify what to vary
  sfm <- xmile("predator_prey") |>
    sim_specs(
      language = "Julia",
      start = 0, stop = 5,
      dt = .1,
      save_from = 5
    ) |>
    build(c("predator", "prey"), eqn = "runif(1)")
  sims <- ensemble(sfm)
  expect_true(sims$success)
  expect_equal(length(unique(sims$summary$time)), 1)
  expect_no_error(expect_no_warning(expect_no_message(plot(sims))))

  # with sims
  sims <- ensemble(sfm, return_sims = TRUE)
  expect_true(sims$success)
  expect_equal(length(unique(sims$summary$time)), 1)
  expect_no_error(expect_no_warning(expect_no_message(plot(sims))))
  expect_no_error(expect_no_warning(expect_no_message(plot(sims, type = "sims"))))
})


test_that("ensemble works with specified range", {
  testthat::skip_on_cran()
  testthat::skip_if_not(julia_status()$status == "ready")

  # If you already have random elements in the model, no need to specify what to vary
  sfm <- xmile("Crielaard2022") |> sim_specs(
    language = "Julia",
    start = 0, stop = 10,
    dt = .1,
    save_from = 5, save_at = 1
  )

  # Run ensemble with specified range
  expect_error(ensemble(sfm, range = list()), "range must be a named list with at least one element")
  expect_error(ensemble(sfm, range = list(.1, .2, .3)), "range must be a named list")
  expect_error(
    ensemble(sfm, range = list("b1" = c(.1, .2, .3))),
    "The following names in range do not exist in the model"
  )
  expect_error(
    ensemble(sfm, range = list(
      "a1" = c(.1, .2, .3),
      "a2" = c(5, 6, 7, 8),
      "Hunger" = c(3, 4, 5)
    ), cross = FALSE),
    "All ranges must be of the same length when cross = FALSE! Please check the lengths of the ranges in range"
  )
  expect_error(
    ensemble(sfm, range = list(
      "a1" = c(1, 2, 3),
      "a2" = c(5, 6, 7),
      "Satiety" = c(3, 4, 5)
    )),
    "Only constants or the initial value of stocks can be varied. Please exclude"
  )
  expect_error(
    ensemble(sfm, range = list(
      "a1" = c(1, 2, 3),
      "a2" = "c(5,6,7)"
    )),
    "All elements in range must be numeric vectors"
  )
  expect_error(
    ensemble(sfm, range = list(
      "a1" = c(1, 2, 3),
      "b1" = c("a", "b", "d")
    )),
    "All elements in range must be numeric vectors"
  )

  sims <- expect_no_error(ensemble(sfm,
    range = list(
      "a1" = c(.1, .2, .3),
      "a2" = c(.5, .6, .7),
      "Hunger" = c(.3, .4, .5)
    ),
    return_sims = FALSE
  ))

  # Also works with a single variable
  sims <- expect_no_error(ensemble(sfm,
    range = list("a2" = seq(0.2, 0.8, by = 0.05)),
    n = 10
  ))

  # Crossed design
  sims <- expect_no_error(ensemble(sfm,
    range = list(
      "a1" = c(1.1, 1.2, 1.3),
      "a2" = c(1.2, 1.3, 1.4)
    ),
    cross = TRUE, n = 3, return_sims = FALSE
  ))
  expect_true(sims$success)
  expect_equal(sims[["n"]], 3)
  expect_equal(sims[["n_total"]], 27)
  expect_equal(sort(unique(sims[["summary"]][["j"]])), 1:9)
  expect_no_error(expect_no_message(plot(sims)))
  expect_no_error(plot(sims, j = c(3, 5, 8), nrows = 4))
  expect_error(
    plot(sims, type = "sims"),
    "No simulation data available! Run ensemble\\(\\) with return_sims = TRUE"
  )

  # Specify both i and j
  expect_no_error(plot(sims, i = 5:15, j = 3:8, type = "summary"))

  # Non-crossed design
  nr_sims <- 15
  nr_cond <- 3
  sims <- ensemble(sfm,
    range = list(
      "a1" = c(1.1, 1.2, 1.3),
      "a2" = c(1.2, 1.3, 1.4)
    ),
    cross = FALSE, n = nr_sims, return_sims = TRUE
  )
  expect_true(sims$success)
  expect_equal(sims[["n"]], nr_sims)
  expect_equal(sims[["n_total"]], nr_sims * nr_cond)
  expect_equal(sort(unique(sims[["df"]][["i"]])), 1:nr_sims)
  expect_equal(sort(unique(sims[["df"]][["j"]])), 1:nr_cond)
  expect_equal(sort(unique(sims[["summary"]][["j"]])), 1:nr_cond)
  expect_no_error(expect_no_message(plot(sims)))
  expect_error(plot(sims, j = nr_cond + 1), "j must be a vector with integers between 1 and 3")
  expect_no_error(expect_no_message(plot(sims, i = (nr_sims - 1):nr_sims, type = "sims")))
  expect_no_error(plot(sims, j = 1:nr_cond, type = "sims"))
})


test_that("ensemble works with units", {
  testthat::skip_on_cran()
  testthat::skip_if_not(julia_status()$status == "ready")

  # Test ensemble with model with units
  sfm <- xmile("coffee_cup") |>
    sim_specs(language = "Julia", stop = 10, dt = 0.1) |>
    build("coffee_temperature", eqn = "runif(1, 20, 150)")
  sims <- expect_no_error(ensemble(sfm))

  nr_sims <- 15
  sims <- expect_no_error(ensemble(sfm, n = nr_sims, only_stocks = FALSE, return_sims = TRUE))
  expect_true(sims$success)
  expect_equal(!is.null(sims[["summary"]]), TRUE)
  expect_equal(!is.null(sims[["df"]]), TRUE)
  expect_equal(sims[["n"]], nr_sims)
  expect_equal(sims[["n_total"]], nr_sims)
  expect_equal(sort(unique(sims[["df"]][["i"]])), 1:nr_sims)
  expect_equal(sort(unique(sims[["df"]][["j"]])), 1)
  expect_equal(sort(unique(sims[["summary"]][["j"]])), 1)
  expect_no_error(expect_no_message(plot(sims)))
  expect_no_error(expect_no_message(plot(sims, type = "sims")))
})


test_that("ensemble works with NA", {
  testthat::skip_on_cran()
  testthat::skip_if_not(julia_status()$status == "ready")

  # Combine varying initial condition and parameters
  sfm <- xmile("predator_prey") |>
    build(c("predator", "prey"), eqn = "runif(1, 30, 50)") |>
    sim_specs(
      language = "Julia",
      dt = 0.1,
      save_at = 10, save_from = 150,
      start = 0, stop = 200
    )
  nr_sims <- 5
  sims <- expect_no_error(ensemble(sfm,
    range = list(
      "prey" = c(40, 50, 60),
      "delta" = seq(.015, .03, by = .005)
    ),
    cross = TRUE, n = nr_sims, return_sims = TRUE,
    only_stocks = TRUE
  ))
  expect_true(sims$success)
  expect_equal(!is.null(sims[["summary"]]), TRUE)
  expect_equal(!is.null(sims[["df"]]), TRUE)
  expect_equal(length(unique(sims$summ$variable)), 2) # 2 stocks
  nr_cond <- 3 * 4
  expect_equal(sims[["n"]], nr_sims)
  expect_equal(sims[["n_total"]], nr_cond * nr_sims)
  expect_equal(sort(unique(sims[["df"]][["i"]])), 1:nr_sims)
  expect_equal(sort(unique(sims[["df"]][["j"]])), 1:nr_cond)
  expect_equal(sort(unique(sims[["summary"]][["j"]])), 1:nr_cond)
  expect_no_error(expect_no_message(plot(sims)))
  expect_no_error(plot(sims, j = 1:5))
  expect_no_error(expect_no_message(plot(sims, type = "sims")))
  expect_no_error(plot(sims, i = nr_sims - 1, type = "sims"))
})


test_that("ensemble: order of range parameters", {
  testthat::skip_on_cran()
  testthat::skip_if_not(julia_status()$status == "ready")

  # In an earlier version, the order of the range parameters was not preserved
  sfm <- xmile() |>
    sim_specs(language = "Julia") |>
    sim_specs(stop = 12, dt = 0.1, save_at = 1, time_units = "month") |>
    header(name = "Maya's Burnout") |>
    build("workload", "stock",
      eqn = 4
    ) |>
    build("new_tasks", "flow",
      eqn = "workload * work_growth",
      to = "workload"
    ) |>
    build("work_growth", "constant",
      eqn = 1.5
    ) |>
    build(c("sleep", "necessary_sleep", "worry_factor"),
      c("stock", "constant", "constant"),
      eqn = c("necessary_sleep", 8, .1)
    ) |>
    build("worry_about_work", "flow",
      eqn = "workload * worry_factor",
      from = "sleep"
    ) |>
    build("need_for_rest", "flow",
      eqn = "workload * necessary_sleep / sleep",
      from = "workload"
    )

  sims <- ensemble(sfm,
    n = 10, return_sims = TRUE,
    range = list(
      "work_growth" = c(1.5),
      "necessary_sleep" = c(8)
    )
  )

  expect_true(sims$success)
  expect_equal(as.data.frame(sims$conditions)$work_growth, 1.5)
  expect_equal(as.data.frame(sims$conditions)$necessary_sleep, 8)
})


test_that("ensemble works with interpolation function", {
  testthat::skip_on_cran()
  testthat::skip_if_not(julia_status()$status == "ready")

  sfm <- xmile("logistic_model") |>
    sim_specs(
      language = "Julia",
      start = 0, stop = 50,
      dt = .1,
      save_from = 50
    ) |>
    build("X", eqn = "runif(1, 0, K)") |>
    build("input", "constant", eqn = "pulse(times, 10, width = dt, height = .01)") |>
    build("inflow2", "flow", eqn = "input(t)", to = "X")

  sims <- expect_no_error(ensemble(sfm))
  expect_true(sims$success)
  expect_no_error(plot(sims))
})
