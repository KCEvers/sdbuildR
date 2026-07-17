# Structural layout invariants
#
# The correctness of generated code depends on cross-cutting invariants that are
# maintained by convention across prep_stock_change(), sanitize_stockflow() and
# the compile step. These tests pin those invariants down directly, and fuzz
# arbitrary mutation sequences to catch ordering/index bugs that only surface in
# specific histories (e.g. the change_type() dSdt[] swap). All run in pure R.

# --- validate_layout() catches corruption ------------------------------------

test_that("validate_layout() catches misaligned stock derivative slots", {
  sfm <- stockflow() |>
    update("a", type = "stock", eqn = "1") |>
    update("b", type = "stock", eqn = "2") |>
    sim_settings(language = "Julia")

  # Healthy model passes
  expect_invisible(validate_layout(sfm))

  # Corrupt: swap the two stocks' dSdt[] slots
  stock_idx <- which(sfm[["variables"]][["type"]] == "stock")
  sfm[["variables"]][stock_idx, "sum_name"] <- c("dSdt[2]", "dSdt[1]")

  expect_error(validate_layout(sfm), class = "stockflow_layout_error")
})

test_that("validate_layout() catches non-contiguous stock derivative slots", {
  sfm <- stockflow() |>
    update("a", type = "stock", eqn = "1") |>
    update("b", type = "stock", eqn = "2") |>
    sim_settings(language = "Julia")

  stock_idx <- which(sfm[["variables"]][["type"]] == "stock")
  sfm[["variables"]][stock_idx, "sum_name"] <- c("dSdt[1]", "dSdt[3]")

  expect_error(validate_layout(sfm), class = "stockflow_layout_error")
})

test_that("validate_layout() catches duplicate variable names", {
  sfm <- stockflow() |> update("a", type = "stock", eqn = "1")
  sfm[["variables"]] <- rbind(sfm[["variables"]], sfm[["variables"]])

  expect_error(validate_layout(sfm), class = "stockflow_layout_error")
})

test_that("validate_layout() is a no-op for empty models", {
  expect_invisible(validate_layout(stockflow()))
})

test_that("pre_assemble_components() validates layout while (re)building", {
  # validate_layout() runs as part of a rebuild. A fresh assembly of a healthy
  # model must succeed and produce aligned indices. (Direct corruption detection
  # is covered by the validate_layout() tests above; a hash-matched cache is
  # trusted and not re-validated, which is why this checks the build path.)
  sfm <- stockflow() |>
    update("a", type = "stock", eqn = "1") |>
    update("b", type = "stock", eqn = "2") |>
    sim_settings(language = "Julia")

  sfm <- pre_assemble_components(invalidate_assemble(sfm, "all"))
  expect_stock_indices_aligned(sfm)
})

# --- Cache consistency: incremental == from scratch --------------------------

test_that("incrementally built model compiles identically to a fresh rebuild", {
  # The original dSdt[] bug was a cache/order interaction: an incrementally
  # updated model diverged from one rebuilt from scratch. Assert they match.
  sfm <- stockflow("predator_prey") |>
    sim_settings(language = "Julia") |>
    change_type("delta", new_type = "stock") |>
    update("extra", type = "stock", eqn = "1")

  script_incremental <- compile(sfm, filepath_sim = "sim.csv")$script

  # Force a full rebuild by discarding the assemble cache
  sfm_fresh <- invalidate_assemble(sfm, "all")
  script_fresh <- compile(sfm_fresh, filepath_sim = "sim.csv")$script

  expect_identical(script_incremental, script_fresh)
})

# --- Property / fuzz: random mutation sequences preserve alignment -----------

test_that("random mutation sequences keep stock dSdt[] indices aligned", {

  skip_on_cran()

  withr::local_seed(20240613)

  # Apply a randomly chosen, always-valid mutation. Returns the (possibly
  # unchanged) model; never throws.
  apply_random_mutation <- function(sfm) {
    vars <- sfm[["variables"]]
    names_all <- vars[["name"]]
    stocks <- vars[vars[["type"]] == "stock", "name"]
    non_stocks <- vars[vars[["type"]] %in% c("constant", "aux"), "name"]

    ops <- "add" # always possible
    if (length(non_stocks)) ops <- c(ops, "to_stock")
    if (length(stocks)) ops <- c(ops, "from_stock", "discard", "rename")

    op <- sample(ops, 1)
    fresh <- paste0("v", sample(1e6, 1))

    out <- tryCatch(
      silence(switch(op,
        add = {
          type <- sample(c("stock", "constant", "aux"), 1)
          update(sfm, fresh, type = type, eqn = "1")
        },
        to_stock = change_type(sfm, sample(non_stocks, 1), new_type = "stock"),
        from_stock = change_type(sfm, sample(stocks, 1),
          new_type = sample(c("constant", "aux"), 1)
        ),
        discard = discard(sfm, sample(stocks, 1)),
        rename = change_name(sfm, sample(names_all, 1), new_name = fresh)
      )),
      error = function(e) sfm
    )

    out
  }

  sfm <- stockflow() |>
    update("seed_stock", type = "stock", eqn = "1") |>
    sim_settings(language = "Julia")

  for (step in seq_len(15)) {
    sfm <- apply_random_mutation(sfm)

    # Core invariant after every step
    expect_stock_indices_aligned(sfm)

    # No duplicate names ever
    expect_false(any(duplicated(sfm[["variables"]][["name"]])))

    # The model must always pass its own structural validator
    expect_invisible(validate_layout(sfm))
  }
})
