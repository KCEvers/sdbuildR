
test_that("translating Insight Maker models works", {

  URL = "https:"
  expect_error(insightmaker_to_sfm(URL = URL), "This is not a URL to an Insight Maker model")

  # ** add stop() with useful message when there is a problem in translation
  # ** test arguments to insightmaker_to_sfm()

  URL = "https://insightmaker.com/insight/3xgsvC7QKgPktHWZuXyGAl/Clone-of-Global-Climate-Change"
  sfm = expect_no_error(insightmaker_to_sfm(URL = URL))
  expect_no_error(as.data.frame(sfm))
  df = as.data.frame(sfm)
  expect_equal(nrow(df) > 0, TRUE)
  expect_equal("macro" %in% df$type, TRUE)
  expect_equal("model_units" %in% df$type, TRUE)

  # Contains graphical functions; check whether xpts and ypts were concatenated
  expect_equal("xpts" %in% names(df), TRUE)
  expect_equal("ypts" %in% names(df), TRUE)

  sim = expect_no_error(simulate(sfm |> sim_specs(language = "Julia")))
  expect_equal(sim$success, TRUE)
  expect_equal(nrow(sim$df) > 0, TRUE)

  expect_error(simulate(sfm |> sim_specs(language = "R")),
               "The model contains unit strings u\\(''\\), which are not supported for simulations in R")


  URL = 'https://insightmaker.com/insight/5LxQr0waZGgBcPJcNTC029/Crielaard-et-al-2022'
  sfm = expect_no_error(insightmaker_to_sfm(URL = URL))
  expect_no_error(as.data.frame(sfm))
  df = as.data.frame(sfm)
  expect_equal(nrow(df) > 0, TRUE)
  expect_equal("macro" %in% df$type, TRUE)

  sim = expect_no_error(simulate(sfm |> sim_specs(language = "Julia")))
  expect_equal(sim$success, TRUE)
  expect_equal(nrow(sim$df) > 0, TRUE)

  sim = expect_no_error(simulate(sfm |> sim_specs(language = "R")))
  expect_equal(sim$success, TRUE)
  expect_equal(nrow(sim$df) > 0, TRUE)

  URL = "https://insightmaker.com/insight/75PvtT7zp43wI7ofBOM9Sm/Clone-of-HYSTERESIS"
  sfm = expect_no_error(insightmaker_to_sfm(URL = URL))
  expect_no_error(as.data.frame(sfm))
  df = as.data.frame(sfm)
  expect_equal(nrow(df) > 0, TRUE)
  expect_equal("macro" %in% df$type, TRUE)

  sim = expect_no_error(simulate(sfm |> sim_specs(language = "Julia")))
  expect_equal(sim$success, TRUE)
  expect_equal(nrow(sim$df) > 0, TRUE)

  # This model uses unit strings u(''), which are not supported in R
  expect_error(simulate(sfm |> sim_specs(language = "R")),
               "The model contains unit strings u\\(''\\), which are not supported for simulations in R")

})
