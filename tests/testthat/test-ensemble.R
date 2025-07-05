test_that("ensemble works", {

  # If you already have random elements in the model, no need to specify what to vary
  sfm = xmile("Crielaard2022") %>% sim_specs(language = "Julia")

  sims = expect_error(ensemble(sfm, n = 1),
                      "The number of simulations must be greater than 1")
  sims = expect_no_error(ensemble(sfm))
  sims = expect_no_error(ensemble(sfm, n = 100))

  sims = expect_no_error(ensemble(sfm, n = 10, return_sims = TRUE))
  expect_equal("df" %in% names(sims), TRUE)

  sims = expect_no_error(ensemble(sfm, n = 10, summary = TRUE,
                                  return_sims = TRUE))
  expect_equal("summ" %in% names(sims), TRUE)
  expect_equal("df" %in% names(sims), TRUE)

  expect_error(ensemble(sfm, summary = FALSE, return_sims = FALSE),
               "Set either return_sims or summary to TRUE to obtain simulation output")

  sims = expect_no_error(ensemble(sfm, n = 10, only_stocks = TRUE))
  df = as.data.frame(sfm, properties = "label")
  stock_names = df[df["type"] == "stock", "name"]
  expect_equal(length(setdiff(stock_names, unique(sims$summ$variable))), 0)


  # Run ensemble with specified range
  expect_error(ensemble(sfm, range = list()), "range must be a named list with at least one element")
  expect_error(ensemble(sfm, range = list(.1, .2, .3)), "range must be a named list")
  expect_error(ensemble(sfm, range = list("b1" = c(.1,.2,.3))),
               "The following names in range do not exist in the model")
  expect_error(ensemble(sfm, range = list("a1" = c(.1,.2,.3),
                                                   "a2" = c(5,6,7,8),
                                                   "Hunger" = c(3,4,5))),
               "All ranges must be of the same length! Please check the lengths of the ranges in range")
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

  # expect_no_error(plot(sims, type = "summary"))
  # expect_error(plot(sims, type = "sims"))

  #
  # microbenchmark::microbenchmark(
  #   ensemble(sfm, n = 10),
  #   ensemble(sfm, n = 100),
  #   ensemble(sfm, n = 10, return_sims = TRUE),
  #   ensemble(sfm, n = 100, return_sims = TRUE),
  #   times = 10
  # )
  #


})
