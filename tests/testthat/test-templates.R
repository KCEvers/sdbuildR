# Tests for templates.R
# Covers all 13 templates: creation, structure, simulation, plot, accuracy

test_that("templates() without argument produces a vector of template names", {
  expect_true(is.character(templates()))
  expect_true(length(templates()) > 0)
})

test_that("templates() with unknown name throws an error", {
  expect_error(templates("nonexistent_XYZZY_99"))
})


# ============================================================================
# Clean creation: every template loads without error, warning, or message
# ============================================================================

test_that("templates() with valid name creates stockflow without error/warning/message", {
  for (nm in templates()) {
    expect_no_error(sfm <- templates(nm))
    expect_no_warning(sfm <- templates(nm))
    expect_no_message(sfm <- templates(nm))
    expect_s3_class(sfm, "stockflow")
    expect_gt(nrow(as.data.frame(sfm)), 0)
  }
})


# ============================================================================
# Structural checks — stock counts only (no variable name assumptions)
# ============================================================================

test_that("SIR template has exactly 3 stocks", {
  expect_equal(nrow(as.data.frame(templates("sir"), type = "stock")), 3)
})

test_that("Lorenz template has exactly 3 stocks", {
  expect_equal(nrow(as.data.frame(templates("Lorenz"), type = "stock")), 3)
})

test_that("Rossler template has exactly 3 stocks", {
  expect_equal(nrow(as.data.frame(templates("Rossler"), type = "stock")), 3)
})

test_that("predator_prey template has exactly 2 stocks", {
  expect_equal(nrow(as.data.frame(templates("predator_prey"), type = "stock")), 2)
})

test_that("bank_account template has exactly 1 stock", {
  expect_equal(nrow(as.data.frame(templates("bank_account"), type = "stock")), 1)
})

test_that("logistic_model template has exactly 1 stock", {
  expect_equal(nrow(as.data.frame(templates("logistic_model"), type = "stock")), 1)
})


# ============================================================================
# Simulation + plot: all templates simulate and produce a plotly object
# Simulation tests skip if Julia is not available (some templates use Julia)
# ============================================================================

test_that("templates() with each template name simulates and produces a plotly object", {
  for (nm in templates()) {
    skip_if_julia_not_ready()
    sfm <- templates(nm)
    sim <- simulate(sfm, only_stocks = FALSE, seed = 42)
    expect_true(sim$success)
    wide <- as.data.frame(sim, direction = "wide")
    all_vars <- setdiff(names(wide), "time")
    expect_gt(length(all_vars), 0)
    pl <- expect_no_error(plot(sim, vars = all_vars, webgl = FALSE))
    expect_plotly(pl)
  }
})

# ============================================================================
# Simulation accuracy — conservation and convergence (language = "R" where possible)
# ============================================================================

test_that("SIR: sum of all stocks is constant over time (population conservation)", {
  skip_if_julia_not_ready()
  sfm <- sim_settings(templates("sir"), only_stocks = TRUE)
  sim <- simulate(sfm, seed = 42)
  wide <- as.data.frame(sim, direction = "wide")
  stock_cols <- setdiff(names(wide), "time")
  N_total <- rowSums(wide[, stock_cols, drop = FALSE])
  expect_equal(diff(range(N_total)), 0, tolerance = 1e-3)
})

test_that("logistic_model: stock eventually converges within 2% of carrying capacity K", {
  sfm <- sim_settings(templates("logistic_model"),
    stop = 120, dt = 0.1,
    language = "R"
  )
  sim <- simulate(sfm, seed = 42)
  wide <- as.data.frame(sim, direction = "wide")
  stock_name <- as.data.frame(templates("logistic_model"), type = "stock")$name
  const_df <- as.data.frame(templates("logistic_model"), type = "constant")
  K_val <- max(as.numeric(const_df$eqn), na.rm = TRUE)
  final_mean <- mean(tail(wide[[stock_name]], 10))
  expect_equal(final_mean, K_val, tolerance = K_val * 0.02)
})

test_that("bank_account: the single stock strictly increases over time", {
  sfm <- sim_settings(templates("bank_account"), language = "R")
  sim <- simulate(sfm, seed = 42)
  wide <- as.data.frame(sim, direction = "wide")
  stock_name <- as.data.frame(templates("bank_account"), type = "stock")$name
  expect_true(all(diff(wide[[stock_name]]) > 0))
})

test_that("predator_prey: both stocks remain non-negative throughout simulation", {
  skip_if_julia_not_ready()
  sfm <- sim_settings(templates("predator_prey"), stop = 50, dt = 0.01)
  sim <- simulate(sfm, only_stocks = TRUE, seed = 42)
  wide <- as.data.frame(sim, direction = "wide")
  stock_names <- as.data.frame(templates("predator_prey"), type = "stock")$name
  for (s in stock_names) {
    expect_true(all(wide[[s]] >= -1e-6),
      info = sprintf("Stock '%s' went negative", s)
    )
  }
})
