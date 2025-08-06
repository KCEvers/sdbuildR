test_that("delay() works", {
  # Check delay() with wrong building block throws error
  expect_error(
    {
      xmile() |> build("a", "stock", eqn = "delay(a, 1)")
    },
    "Adjust equation of a: delay\\(\\) cannot be used for a stock"
  )

  # Check delay() with too few arguments throws error
  expect_error(
    {
      xmile() |> build("a", "aux", eqn = "delay()")
    },
    "Obligatory arguments variable, length are missing for function delay"
  )
  expect_error(
    {
      xmile() |> build("a", "aux", eqn = "delay(a)")
    },
    "Obligatory argument length is missing for function delay"
  )


  # Check that multiple delays are saved
  sfm <- xmile() |>
    build("Outflow", "flow", eqn = "delay(Inflow, AverageDelay, 100) + delay(Inflow, 100) + delay(Inflow, 50, 4)")
  expect_equal(length(sfm$model$variables$flow$Outflow$func$delay), 3)
  expect_equal(
    sort(names(sfm$model$variables$flow$Outflow$func$delay)),
    paste0("Outflow", .sdbuildR_env[["P"]][["delay_suffix"]], 1:3)
  )
  expect_equal(sfm$model$variables$flow$Outflow$func$delay[[1]]$var, "Inflow")
  expect_equal(sfm$model$variables$flow$Outflow$func$delay[[2]]$var, "Inflow")
  expect_equal(sfm$model$variables$flow$Outflow$func$delay[[3]]$var, "Inflow")
  expect_equal(sfm$model$variables$flow$Outflow$func$delay[[1]]$length, "AverageDelay")
  expect_equal(sfm$model$variables$flow$Outflow$func$delay[[2]]$length, "100.0")
  expect_equal(sfm$model$variables$flow$Outflow$func$delay[[3]]$length, "50.0")
  expect_equal(sfm$model$variables$flow$Outflow$func$delay[[1]]$initial, "100.0")
  expect_equal(sfm$model$variables$flow$Outflow$func$delay[[2]]$initial, "nothing")
  expect_equal(sfm$model$variables$flow$Outflow$func$delay[[3]]$initial, "4.0")

  # Chapter 4 Duggan: Fixed Delay
  # https://github.com/JimDuggan/SDMR/blob/master/models/04%20Chapter/Extra%20Examples/FixedDelay.R

  sfm <- xmile() |>
    sim_specs(language = "Julia") |>
    sim_specs(start = 0, stop = 20, dt = 0.1) |>
    build("MaterialinTransit", "stock", eqn = "400") |>
    build("Inflow", "flow",
      eqn = "ifelse(t < 8, 100, 200)",
      to = "MaterialinTransit"
    ) |>
    build("Outflow", "flow",
      eqn = "delay(Inflow, AverageDelay, 100)",
      from = "MaterialinTransit"
    ) |>
    build("AverageDelay", "constant", eqn = 4)
  expect_no_error(plot(sfm))

  testthat::skip_on_cran()

  sim <- expect_no_error(simulate(sfm, only_stocks = FALSE))
  inflow <- sim$df[sim$df$variable == "Inflow", "value"]
  outflow <- sim$df[sim$df$variable == "Inflow", "value"]
  t <- sim$df[sim$df$variable == "Inflow", "time"]
  expect_equal(inflow[which(t < 8)[1]], 100)
  expect_equal(inflow[which(t > 8)[1]], 200)
  expect_equal(outflow[which(t < 8 + 4)[1]], 100)
  expect_equal(outflow[which(t >= 8 + 4)[1]], 200)

  # **Check whether first argument to delay() is in variables
})



test_that("past() works", {
  # Check past() with wrong building block throws error
  expect_error(
    {
      xmile() |> build("a", "stock", eqn = "past(a, 1)")
    },
    "Adjust equation of a: past\\(\\) cannot be used for a stock"
  )

  # Check past() with too few arguments throws error
  expect_error(
    {
      xmile() |> build("a", "aux", eqn = "past()")
    },
    "Obligatory argument variable is missing for function past"
  )


  sfm <- xmile() |>
    sim_specs(language = "Julia") |>
    sim_specs(start = 0, stop = 20, dt = 0.1) |>
    build("MaterialinTransit", "stock", eqn = "400") |>
    build("Inflow", "flow", eqn = "ifelse(t < 8, 100, 200)", to = "MaterialinTransit") |>
    build("Outflow", "flow", eqn = "max(past(Inflow, AverageDelay))", from = "MaterialinTransit") |>
    build("AverageDelay", "constant", eqn = 4)
  expect_no_error(plot(sfm))

  testthat::skip_on_cran()

  sim <- expect_no_error(simulate(sfm, only_stocks = FALSE))
  inflow <- sim$df[sim$df$variable == "Inflow", "value"]
  outflow <- sim$df[sim$df$variable == "Inflow", "value"]
  t <- sim$df[sim$df$variable == "Inflow", "time"]
  expect_equal(inflow[which(t < 8)[1]], 100)
  expect_equal(inflow[which(t > 8)[1]], 200)
  expect_equal(outflow[which(t < 8 + 4)[1]], 100)
  expect_equal(outflow[which(t >= 8 + 4)[1]], 200)
})



test_that("delayN() works", {
  # Check delayN() with wrong building block throws error
  expect_error(
    {
      xmile() |> build("a", "stock", eqn = "delayN(a, 1, 2)")
    },
    "Adjust equation of a: delayN\\(\\) cannot be used for a stock"
  )

  # Check delayN() with too few arguments throws error
  expect_error(
    {
      xmile() |> build("a", "aux", eqn = "delayN()")
    },
    "Obligatory arguments variable, length, order are missing for function delayN"
  )
  expect_error(
    {
      xmile() |> build("a", "aux", eqn = "delayN(a)")
    },
    "Obligatory arguments length, order are missing for function delayN"
  )
  expect_error(
    {
      xmile() |> build("a", "aux", eqn = "delayN(a, 1)")
    },
    "Obligatory argument order is missing for function delayN"
  )


  # Test delayN() in simple model
  sfm <- expect_no_error(xmile() |> sim_specs(language = "Julia") |>
    sim_specs(start = 0, stop = 20, dt = 0.1) |> # dt=.125->dt=.1
    build("MaterialinTransit", "stock", eqn = "400") |>
    build("Inflow", "flow", eqn = "ifelse(t < 8, 100, 200)", to = "MaterialinTransit") |>
    build("Outflow", "flow", eqn = "delayN(Inflow, AverageDelay, 5, 0)", from = "MaterialinTransit") |>
    build("AverageDelay", "constant", eqn = 4))
  expect_no_error(plot(sfm))

  testthat::skip_on_cran()

  sim <- expect_no_error(simulate(sfm, only_stocks = FALSE))
  inflow <- sim$df[sim$df$variable == "Inflow", "value"]
  t <- sim$df[sim$df$variable == "Inflow", "time"]
  expect_equal(inflow[which(t < 8)[1]], 100)
  expect_equal(inflow[which(t > 8)[1]], 200)

  sfm <- expect_no_error(xmile() |>
    sim_specs(language = "Julia", time_units = "days") |>
    build("effort", "stock", eqn = .5) |>
    build("goal", "constant", eqn = 1) |>
    build("delayed", "aux", "smoothN(effort, .1, 3)") |>
    build("discrepancy", "aux", "goal - smoothN(effort, .1, 3)") |>
    build("exertion", "flow", "0.1 * discrepancy", to = "effort"))

  sim <- expect_no_error(simulate(sfm, only_stocks = FALSE))
  expect_no_error(plot(sim, add_constants = TRUE))

  # **
})


test_that("smoothN() works", {
  # Check smoothN() with wrong building block throws error
  expect_error(
    {
      xmile() |> build("a", "stock", eqn = "smoothN(a, 1, 2)")
    },
    "Adjust equation of a: smoothN\\(\\) cannot be used for a stock"
  )

  # Check smoothN() with too few arguments throws error
  expect_error(
    {
      xmile() |> build("a", "aux", eqn = "smoothN()")
    },
    "Obligatory arguments variable, length, order are missing for function smoothN"
  )
  expect_error(
    {
      xmile() |> build("a", "aux", eqn = "smoothN(a)")
    },
    "Obligatory arguments length, order are missing for function smoothN"
  )
  expect_error(
    {
      xmile() |> build("a", "aux", eqn = "smoothN(a, 1)")
    },
    "Obligatory argument order is missing for function smoothN"
  )
})


test_that("mixing delay family functions works", {
  sfm <- expect_no_error(xmile() |>
    build("Outflow", "flow", eqn = "delay(Inflow, 10*3, 1/4) + past(Inflow, 100) + delayN(Inflow, 50, 4) + smoothN(Inflow,150,3)"))
  expect_equal(length(sfm$model$variables$flow$Outflow$func$delay), 1)
  expect_equal(length(sfm$model$variables$flow$Outflow$func$past), 1)
  expect_equal(length(sfm$model$variables$flow$Outflow$func$delayN), 1)
  expect_equal(length(sfm$model$variables$flow$Outflow$func$smoothN), 1)
  expect_equal(
    names(sfm$model$variables$flow$Outflow$func$delay),
    paste0("Outflow", .sdbuildR_env[["P"]][["delay_suffix"]], 1)
  )
  expect_equal(
    names(sfm$model$variables$flow$Outflow$func$past),
    paste0("Outflow", .sdbuildR_env[["P"]][["past_suffix"]], 1)
  )
  expect_equal(
    names(sfm$model$variables$flow$Outflow$func$delayN),
    paste0("Outflow", .sdbuildR_env[["P"]][["delayN_suffix"]], 1)
  )
  expect_equal(
    names(sfm$model$variables$flow$Outflow$func$smoothN),
    paste0("Outflow", .sdbuildR_env[["P"]][["smoothN_suffix"]], 1)
  )
  expect_equal(sfm$model$variables$flow$Outflow$func$delay[[1]]$var, "Inflow")
  expect_equal(sfm$model$variables$flow$Outflow$func$delay[[1]]$length, "10.0 .* 3.0")
  expect_equal(sfm$model$variables$flow$Outflow$func$delay[[1]]$initial, "1.0 ./ 4.0")
  expect_equal(sfm$model$variables$flow$Outflow$func$past[[1]]$var, "Inflow")
  expect_equal(sfm$model$variables$flow$Outflow$func$past[[1]]$length, "100.0")
  expect_equal(sfm$model$variables$flow$Outflow$func$delayN[[1]]$var, "Inflow")
  expect_equal(sfm$model$variables$flow$Outflow$func$delayN[[1]]$length, "50.0")
  expect_equal(sfm$model$variables$flow$Outflow$func$delayN[[1]]$order, "4.0")
  expect_equal(sfm$model$variables$flow$Outflow$func$delayN[[1]]$initial, "Inflow")
  expect_equal(sfm$model$variables$flow$Outflow$func$smoothN[[1]]$var, "Inflow")
  expect_equal(sfm$model$variables$flow$Outflow$func$smoothN[[1]]$length, "150.0")
  expect_equal(sfm$model$variables$flow$Outflow$func$smoothN[[1]]$order, "3.0")
  expect_equal(sfm$model$variables$flow$Outflow$func$smoothN[[1]]$initial, "Inflow")
})


test_that("delay functions correspond to Insight Maker", {
  # Where to put data for automated tests:
  # https://stackoverflow.com/questions/32328802/where-should-i-put-data-for-automated-tests-with-testthat

  # Model containing delay, delayN, smoothN
  URL <- "https://insightmaker.com/insight/2QkDnwi3QByFEOGV8Tdxi2/Test-delay-functions"
  sfm <- expect_no_error(insightmaker_to_sfm(URL))
  expect_no_error(summary(sfm))
  df <- expect_no_error(as.data.frame(sfm))

  testthat::skip_on_cran()

  sim <- expect_no_error(simulate(sfm, only_stocks = FALSE))

  filepath <- testthat::test_path("testdata", "delay_data_2025_07_12.csv")
  sim_IM <- read.csv(filepath)

  # Rename column F to F_1 because F is a syntactically invalid name in R
  colnames(sim_IM)[colnames(sim_IM) == "F"] <- "F_1"

  s1 <- unique(sim$df$variable)
  s2 <- setdiff(colnames(sim_IM), c("Time", df[df[["type"]] %in% c("constant"), "name"], "input"))
  expect_equal(all(s2 %in% s1), TRUE)
  expect_equal(names(sim$constants), "tau_fixed")

  # #   # Check that all variables match
  #   for (var in unique(sim$df$variable)){
  #     print(var)
  #     expect_equal(sim$df[sim$df$variable == var, "value"], sim_IM[[var]])
  #   }

  # ** test with saveat...
})
