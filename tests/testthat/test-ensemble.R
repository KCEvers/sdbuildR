

test_that("ensemble works", {

  # If you already have random elements in the model, no need to specify what to vary
  sfm = xmile("Crielaard2022") %>% sim_specs(language = "Julia",
                                             start = 0, stop = 500,
                                             dt = .1,
                                             save_from = 400, save_at = 1)
  df = as.data.frame(sfm, properties = "eqn")

  sims = expect_error(ensemble(sfm, n = 0),
                      "The number of simulations must be greater than 0")
  sims = expect_no_error(ensemble(sfm, threaded = TRUE))
  sims = expect_no_error(ensemble(sfm, threaded = FALSE))

  # Only stocks
  sims = expect_no_error(ensemble(sfm, n = 15, only_stocks = TRUE, return_sims = FALSE))
  expect_equal(length(unique(sims$summ$variable)), nrow(df[df[["type"]] == "stock", ])) # 3 stocks

  # All variables
  nr_sims = 15
  sims = expect_no_error(ensemble(sfm, n = nr_sims, only_stocks = FALSE, return_sims = TRUE))
  expect_equal(!is.null(sims[["summary"]]), TRUE)
  expect_equal(!is.null(sims[["df"]]), TRUE)
  expect_equal(length(unique(sims$summ$variable)), nrow(df[df[["type"]] %in% c("stock", "flow", "aux"), ])) # 3 stocks

  # Check returned properties
  expect_equal(sims[["n"]], nr_sims)
  expect_equal(sims[["total_n"]], nr_sims)
  expect_equal(sort(unique(sims[["df"]][["i"]])), 1:nr_sims)
  expect_equal(sort(unique(sims[["df"]][["j"]])), 1)
  expect_equal(sort(unique(sims[["summary"]][["j"]])), 1)

  # Check plot
  expect_no_error(plot(sims))
  expect_no_error(plot(sims, i = nr_sims - 1))
  expect_no_error(plot(sims, type = "sims"))

  # Run ensemble with specified range
  expect_error(ensemble(sfm, range = list()), "range must be a named list with at least one element")
  expect_error(ensemble(sfm, range = list(.1, .2, .3)), "range must be a named list")
  expect_error(ensemble(sfm, range = list("b1" = c(.1,.2,.3))),
               "The following names in range do not exist in the model")
  expect_error(ensemble(sfm, range = list("a1" = c(.1,.2,.3),
                                                   "a2" = c(5,6,7,8),
                                                   "Hunger" = c(3,4,5)), cross = FALSE),
               "All ranges must be of the same length when cross = FALSE! Please check the lengths of the ranges in range")
  expect_error(ensemble(sfm, range = list("a1" = c(1,2,3),
                                                   "a2" = c(5,6,7),
                                                   "Satiety" = c(3,4,5))),
               "Only constants or the initial value of stocks can be varied. Please exclude")
  expect_error(ensemble(sfm, range = list("a1" = c(1,2,3),
                                                   "a2" = "c(5,6,7)")),
               "All elements in range must be numeric vectors")
  expect_error(ensemble(sfm, range = list("a1" = c(1,2,3),
                                                   "b1" = c("a", "b", "d"))),
               "All elements in range must be numeric vectors")

  sims = expect_no_error(ensemble(sfm, range = list("a1" = c(.1,.2,.3),
                                                   "a2" = c(.5,.6,.7),
                                                   "Hunger" = c(.3,.4,.5)),
                                  return_sims = FALSE))

  # Crossed design
  sims = expect_no_error(ensemble(sfm, range = list("a1" = c(1.1,1.2,1.3),
                                                    "a2" = c(1.2,1.3,1.4)),
                                  cross = TRUE, n = 3, return_sims = FALSE))
  expect_equal(sims[["n"]], 3)
  expect_equal(sims[["total_n"]], 27)
  expect_equal(sort(unique(sims[["summary"]][["j"]])), 1:9)
  expect_no_error(plot(sims))
  expect_no_error(plot(sims, j = 2))
  expect_no_error(plot(sims, j = 9))
  expect_error(plot(sims, type = "sims"), "No simulation data available! Run ensemble\\(\\) with return_sims = TRUE")

  # Non-crossed design
  sims = ensemble(sfm, range = list("a1" = c(1.1,1.2,1.3),
                                    "a2" = c(1.2,1.3,1.4)),
                  cross = FALSE, n = 15)
  expect_equal(sims[["n"]], 15)
  expect_equal(sims[["total_n"]], 15*3)
  expect_equal(sort(unique(sims[["df"]][["i"]])), 1:15)
  expect_equal(sort(unique(sims[["df"]][["j"]])), 1:3)
  expect_equal(sort(unique(sims[["summary"]][["j"]])), 1:3)
  expect_no_error(plot(sims))
  expect_error(plot(sims, j = 4), "Condition j = 4 is not a valid index\\. Must be one of: 1, 2, 3")
  expect_no_error(plot(sims, i = 5:15, type = "sims"))
  expect_no_error(plot(sims, j = 2, type = "sims"))

  # Message printed
  expect_message(ensemble(sfm, range = list("a1" = c(1.1,1.2,1.3),
                                            "a2" = c(1.2,1.3,1.4)),
                          cross = TRUE, n = 1, verbose = TRUE),
                 "Running a total of 9 simulations for 9 conditions \\(1 simulations per condition\\)")

  # Test ensemble with model with units
  sfm = xmile("coffee_cup") %>% sim_specs(language = "Julia") %>%
    build("coffee_temperature", eqn = "runif(1, 20, 150)")
  sims = expect_no_error(ensemble(sfm))

  nr_sims = 15
  sims = expect_no_error(ensemble(sfm, n = nr_sims, only_stocks = FALSE, return_sims = TRUE))
  expect_equal(!is.null(sims[["summary"]]), TRUE)
  expect_equal(!is.null(sims[["df"]]), TRUE)
  expect_equal(sims[["n"]], nr_sims)
  expect_equal(sims[["total_n"]], nr_sims)
  expect_equal(sort(unique(sims[["df"]][["i"]])), 1:nr_sims)
  expect_equal(sort(unique(sims[["df"]][["j"]])), 1)
  expect_equal(sort(unique(sims[["summary"]][["j"]])), 1)
  expect_no_error(plot(sims))
  expect_no_error(plot(sims, j = 1))
  expect_no_error(plot(sims, type = "sims"))

  # Combine varying initial condition and parameters
  sfm = xmile("predator-prey") %>% sim_specs(language = "Julia",
                                             dt = 0.1,
                                             save_at = 1, save_from = 150,
                                             start = 0, stop = 200)
  nr_sims = 10
  sims = expect_no_error(ensemble(sfm, range = list("prey" = c(10),
                                                    "predator" = c(40, 60),
                                                    "delta" = c(.01, .02, .03, .04, .05)),
                                  cross = TRUE, n = nr_sims, return_sims = TRUE,
                                  only_stocks = TRUE))
  expect_equal(!is.null(sims[["summary"]]), TRUE)
  expect_equal(!is.null(sims[["df"]]), TRUE)
  expect_equal(length(unique(sims$summ$variable)), 2) # 2 stocks
  nr_cond = 1 * 2 * 5
  expect_equal(sims[["n"]], nr_sims)
  expect_equal(sims[["total_n"]], nr_cond * nr_sims)
  expect_equal(sort(unique(sims[["df"]][["i"]])), 1:nr_sims)
  expect_equal(sort(unique(sims[["df"]][["j"]])), 1:nr_cond)
  expect_equal(sort(unique(sims[["summary"]][["j"]])), 1:nr_cond)
  expect_no_error(plot(sims))
  expect_no_error(plot(sims, j = 1))
  expect_no_error(plot(sims, type = "sims"))
  expect_no_error(plot(sims, i = nr_sims - 1, type = "sims"))

})

