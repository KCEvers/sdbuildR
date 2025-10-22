test_that("find_dependencies works", {
  sfm <- xmile() |>
    build("a", "stock", eqn = "b + c") |>
    build(c("b", "c"), "flow")

  dep <- expect_no_error(expect_no_warning(expect_no_message(find_dependencies(sfm))))
  expect_equal(sort(names(dep)), letters[1:3])
  expect_equal(dep[["a"]], c("b", "c"))
  expect_equal(dep[["b"]], character(0))
  expect_equal(dep[["c"]], character(0))

  # Reverse dependencies
  dep <- expect_no_error(expect_no_warning(expect_no_message(find_dependencies(sfm, reverse = TRUE))))
  expect_equal(sort(names(dep)), letters[1:3])
  expect_equal(dep[["a"]], character(0))
  expect_equal(dep[["b"]], "a")
  expect_equal(dep[["c"]], "a")
})


test_that("find_dependencies works", {
  sfm <- xmile("SIR")
  dep <- expect_no_error(expect_no_message(find_dependencies(sfm)))
  expect_equal(dep[["Infected"]], character(0))
  expect_equal(dep[["Beta"]], c("Effective_Contact_Rate", "Total_Population"))

  expect_equal(
    sort(names(dep)),
    sort(as.data.frame(sfm)[["name"]])
  )

  dep_rev <- find_dependencies(sfm, reverse = TRUE)
  expect_equal(dep_rev[["Infected"]], c("Lambda", "Recovery_Rate"))
  expect_equal(dep_rev[["Beta"]], "Lambda")

  expect_equal(
    sort(names(dep_rev)),
    sort(as.data.frame(sfm)[["name"]])
  )
})



test_that("get_build_code() works", {
  expect_no_error(get_build_code(xmile()))

  for (s in c("SIR", "Crielaard2022")) {
    # Replicate with get_build_code
    sfm <- xmile(s) |> sim_specs(save_at = 1, start = 0, stop = 10)

    if (s == "Crielaard2022") {
      sfm <- sfm |>
        build(c("Food_intake", "Hunger", "Compensatory_behaviour"),
          eqn = c(.5, .3, .1)
        )
    }

    sim1 <- simulate(sfm)
    script <- expect_no_error(get_build_code(sfm))

    # Create a new environment to collect variables
    envir <- new.env()
    expect_no_error(expect_no_message(eval(parse(text = script), envir = envir)))
    sfm2 <- envir[["sfm"]]
    sim2 <- simulate(sfm2)
    expect_identical(sim1$df$value, sim2$df$value)
  }
})




test_that("save_at works", {
  sfm <- xmile("SIR") |> sim_specs(language = "R", stop = 10, save_at = 1)
  sim <- simulate(sfm)
  df <- as.data.frame(sim)
  expect_equal(unique(round(diff(sort(unique(df[["time"]]))), 4)), 1)

  testthat::skip_on_cran()
  testthat::skip_if_not(julia_status()$status == "ready")
  sfm <- sfm |> sim_specs(language = "Julia")
  sim <- simulate(sfm)
  df <- as.data.frame(sim)
  expect_equal(unique(round(diff(sort(unique(df[["time"]]))), 4)), 1)
})
