test_that("stocks change over time in Julia", {
  skip_if_no_internet()
  skip_if_julia_not_ready()

  # Import Romeo-Juliet model
  sfm <- suppressWarnings(import_insightmaker(
    url = "https://insightmaker.com/insight/43tz1nvUgbIiIOGSGtzIzj/Romeo-Juliet"
  ))

  # Switch to Julia
  sfm <- sim_settings(sfm, language = "Julia", stop = 10)

  # Check sum_eqn is NOT "0.0"
  stocks <- sfm$variables[sfm$variables$type == "stock", ]
  expect_false(any(stocks$sum_eqn == "0.0", na.rm = TRUE))

  # Simulate
  sim <- simulate(sfm)

  # Stocks should change over time
  romeo_vals <- sim$df[sim$df$variable == "Romeo", "value"]
  juliet_vals <- sim$df[sim$df$variable == "Juliet", "value"]

  # Range should be > 0.01 (stocks are changing, not constant)
  expect_gt(diff(range(romeo_vals)), 0.01)
  expect_gt(diff(range(juliet_vals)), 0.01)
})

test_that("switching Julia -> R works", {
  skip_if_julia_not_ready()
  skip_if_no_internet()

  sfm <- suppressWarnings(import_insightmaker(
    url = "https://insightmaker.com/insight/43tz1nvUgbIiIOGSGtzIzj/Romeo-Juliet"
  ))

  # Start with Julia
  sfm <- sim_settings(sfm, language = "Julia", stop = 10)
  sim1 <- simulate(sfm)
  expect_s3_class(sim1, "simulate_stockflow")

  # Switch to R - should NOT crash
  expect_no_error({
    sfm <- sim_settings(sfm, language = "R")
    sim2 <- simulate(sfm)
  })

  expect_s3_class(sim2, "simulate_stockflow")

  # Both should produce results
  expect_gt(nrow(sim1$df), 0)
  expect_gt(nrow(sim2$df), 0)
})

test_that("multiple language switches maintain integrity", {
  skip_if_julia_not_ready()

  sfm <- stockflow() |>
    update(name = "S", type = "stock", eqn = "100") |>
    update(name = "births", type = "flow", eqn = "0.1 * S", to = "S") |>
    update(name = "deaths", type = "flow", eqn = "0.05 * S", from = "S") |>
    sim_settings(stop = 5)

  # Switch R → Julia → R → Julia (3 rounds)
  for (i in 1:3) {
    sfm <- sim_settings(sfm, language = "Julia")
    sim_j <- simulate(sfm)

    sfm <- sim_settings(sfm, language = "R")
    sim_r <- simulate(sfm)

    # Both should complete successfully
    expect_s3_class(sim_j, "simulate_stockflow")
    expect_s3_class(sim_r, "simulate_stockflow")

    # Results should be reasonable (stocks not all zero)
    s_vals_j <- sim_j$df[sim_j$df$variable == "S", "value"]
    s_vals_r <- sim_r$df[sim_r$df$variable == "S", "value"]

    expect_gt(max(s_vals_j), 0)
    expect_gt(max(s_vals_r), 0)
  }
})

test_that("R and Julia produce consistent results", {
  skip_if_julia_not_ready()

  sfm <- stockflow() |>
    update(name = "Population", type = "stock", eqn = "100") |>
    update(name = "births", type = "flow", eqn = "0.05 * Population", to = "Population") |>
    update(name = "deaths", type = "flow", eqn = "0.03 * Population", from = "Population") |>
    sim_settings(stop = 10, dt = 0.1, save_by = 0.5)

  # Simulate with R
  sfm_r <- sim_settings(sfm, language = "R")
  sim_r <- simulate(sfm_r)

  # Simulate with Julia
  sfm_j <- sim_settings(sfm, language = "Julia")
  sim_j <- simulate(sfm_j)

  # Extract Population values
  pop_r <- sim_r$df[sim_r$df$variable == "Population", "value"]
  pop_j <- sim_j$df[sim_j$df$variable == "Population", "value"]

  # Should be very close (allowing for small numerical differences)
  expect_equal(pop_r, pop_j, tolerance = 1e-6)
})

test_that("switching languages preserves variable structure", {
  skip_if_julia_not_ready()

  sfm <- stockflow() |>
    update(name = "S", type = "stock", eqn = "100") |>
    update(name = "rate", type = "aux", eqn = "0.5") |>
    update(name = "inflow", type = "flow", eqn = "rate * S", to = "S")

  # Get initial variable count
  initial_vars <- nrow(sfm$variables)

  # Switch R → Julia
  sfm <- sim_settings(sfm, language = "Julia")
  expect_equal(nrow(sfm$variables), initial_vars)

  # Switch Julia → R
  sfm <- sim_settings(sfm, language = "R")
  expect_equal(nrow(sfm$variables), initial_vars)

  # All variables should still exist
  expect_true("S" %in% sfm$variables$name)
  expect_true("rate" %in% sfm$variables$name)
  expect_true("inflow" %in% sfm$variables$name)
})

test_that("language switching works with multiple stocks and flows", {
  skip_if_julia_not_ready()

  sfm <- stockflow() |>
    update(name = "S", type = "stock", eqn = "900") |>
    update(name = "I", type = "stock", eqn = "100") |>
    update(name = "R", type = "stock", eqn = "0") |>
    update(name = "beta", type = "aux", eqn = "0.5") |>
    update(name = "gamma", type = "aux", eqn = "0.1") |>
    update(name = "infection", type = "flow", eqn = "beta * S * I / 1000", from = "S", to = "I") |>
    update(name = "recovery", type = "flow", eqn = "gamma * I", from = "I", to = "R") |>
    sim_settings(stop = 50, dt = 0.1)

  # R simulation
  sfm_r <- sim_settings(sfm, language = "R")
  sim_r <- simulate(sfm_r)

  # Julia simulation
  sfm_j <- sim_settings(sfm, language = "Julia")
  sim_j <- simulate(sfm_j)

  # Both should complete
  expect_s3_class(sim_r, "simulate_stockflow")
  expect_s3_class(sim_j, "simulate_stockflow")

  # Check variables exist in both
  expect_true(all(c("S", "I", "R") %in% unique(sim_r$df$variable)))
  expect_true(all(c("S", "I", "R") %in% unique(sim_j$df$variable)))

  # Results should be close
  s_r <- sim_r$df[sim_r$df$variable == "S", "value"]
  s_j <- sim_j$df[sim_j$df$variable == "S", "value"]
  expect_equal(s_r, s_j, tolerance = 1e-5)
})


test_that("language switching works with isolated stocks", {
  skip_if_julia_not_ready()

  sfm <- stockflow() |>
    update(name = "IsolatedStock", type = "stock", eqn = "42")

  # R simulation
  sfm_r <- sim_settings(sfm, language = "R", stop = 5)
  sim_r <- simulate(sfm_r)

  # Julia simulation
  sfm_j <- sim_settings(sfm, language = "Julia", stop = 5)
  sim_j <- simulate(sfm_j)

  # Both should keep stock constant at 42
  expect_equal(unique(sim_r$df$value), 42)
  expect_equal(unique(sim_j$df$value), 42)
})
