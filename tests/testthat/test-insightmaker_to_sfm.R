test_that("translating Insight Maker models works", {
  URL <- "https:"
  expect_error(
    insightmaker_to_sfm(URL = URL),
    "This is not a URL to an Insight Maker model"
  )

  expect_error(
    insightmaker_to_sfm(URL = URL, filepath_IM = ""),
    "Either URL or filepath_IM needs to be specified, not both"
  )

  sfm_list <- list()

  URL <- "https://insightmaker.com/insight/3xgsvC7QKgPktHWZuXyGAl/Clone-of-Global-Climate-Change"
  sfm_list[[1]] <- sfm <- expect_no_error(insightmaker_to_sfm(URL = URL))
  expect_no_error(as.data.frame(sfm))
  df <- as.data.frame(sfm)
  expect_equal(nrow(df) > 0, TRUE)
  expect_equal("macro" %in% df$type, TRUE)
  expect_equal("model_units" %in% df$type, TRUE)

  # Contains graphical functions; check whether xpts and ypts were concatenated
  expect_equal("xpts" %in% names(df), TRUE)
  expect_equal("ypts" %in% names(df), TRUE)

  expect_error(
    simulate(sfm |> sim_specs(language = "R")),
    "The model contains unit strings u\\(''\\), which are not supported for simulations in R"
  )

  URL <- "https://insightmaker.com/insight/5LxQr0waZGgBcPJcNTC029/Crielaard-et-al-2022"
  sfm_list[[2]] <- sfm <- expect_no_error(insightmaker_to_sfm(URL = URL))
  expect_no_error(as.data.frame(sfm))
  df <- as.data.frame(sfm)
  expect_equal(nrow(df) > 0, TRUE)
  expect_equal("macro" %in% df$type, TRUE)

  sim <- expect_no_error(simulate(sfm |> sim_specs(
    language = "R", start = 0,
    dt = 0.1, stop = 10
  )))
  expect_equal(sim$success, TRUE)
  expect_equal(nrow(sim$df) > 0, TRUE)

  URL <- "https://insightmaker.com/insight/75PvtT7zp43wI7ofBOM9Sm/Clone-of-HYSTERESIS"
  sfm_list[[3]] <- sfm <- expect_no_error(insightmaker_to_sfm(URL = URL))
  expect_no_error(as.data.frame(sfm))
  df <- as.data.frame(sfm)
  expect_equal(nrow(df) > 0, TRUE)
  expect_equal("macro" %in% df$type, TRUE)

  # This model uses unit strings u(''), which are not supported in R
  expect_error(
    simulate(sfm |> sim_specs(language = "R")),
    "The model contains unit strings u\\(''\\), which are not supported for simulations in R"
  )

  testthat::skip_on_cran()

  lapply(sfm_list, function(sfm) {
    # For some models with units, save_at and save_from create error
    sim <- expect_no_error(simulate(sfm |> sim_specs(
      language = "Julia",
      # dt = 0.1, start = 0,
      # stop = 10
    )))
    expect_equal(sim$success, TRUE)
    expect_equal(nrow(sim$df) > 0, TRUE)
  })
})
