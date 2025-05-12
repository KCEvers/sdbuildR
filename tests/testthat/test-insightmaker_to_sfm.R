test_that("translating Insight Maker models works", {

  URL = "https://insightmaker.com/insight/3xgsvC7QKgPktHWZuXyGAl/Clone-of-Global-Climate-Change"
  expect_no_error(insightmaker_to_sfm(URL = URL))
  sfm = insightmaker_to_sfm(URL = URL)
  expect_no_error(as.data.frame(sfm))
  df = as.data.frame(sfm)
  expect_equal(nrow(df) > 0, TRUE)
  expect_equal("macro" %in% df$type, TRUE)
  expect_equal("model_units" %in% df$type, TRUE)

  # Contains graphical functions; check whether xpts and ypts were concatenated
  expect_equal("xpts" %in% names(df), TRUE)
  expect_equal("ypts" %in% names(df), TRUE)

  expect_no_error(simulate(sfm))

  sim = simulate(sfm)
  expect_equal(sim$success, TRUE)
  expect_equal(nrow(sim$df) > 0, TRUE)

  URL = 'https://insightmaker.com/insight/5LxQr0waZGgBcPJcNTC029/Crielaard-2022'
  expect_no_error(insightmaker_to_sfm(URL = URL))
  sfm = insightmaker_to_sfm(URL = URL)
  expect_no_error(as.data.frame(sfm))
  df = as.data.frame(sfm)
  expect_equal(nrow(df) > 0, TRUE)
  expect_equal("macro" %in% df$type, TRUE)

  expect_no_error(simulate(sfm))

  sim = simulate(sfm)
  expect_equal(sim$success, TRUE)
  expect_equal(nrow(sim$df) > 0, TRUE)

  URL = "https://insightmaker.com/insight/75PvtT7zp43wI7ofBOM9Sm/Clone-of-HYSTERESIS"
  expect_no_error(insightmaker_to_sfm(URL = URL))
  sfm = insightmaker_to_sfm(URL = URL)
  expect_no_error(as.data.frame(sfm))
  df = as.data.frame(sfm)
  expect_equal(nrow(df) > 0, TRUE)
  expect_equal("macro" %in% df$type, TRUE)

  expect_no_error(simulate(sfm))

  sim = simulate(sfm)
  expect_equal(sim$success, TRUE)
  expect_equal(nrow(sim$df) > 0, TRUE)
})
