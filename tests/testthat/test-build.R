
test_that("xmile() works", {

  expect_equal(class(xmile()), "sdbuildR_xmile")
  expect_s3_class(xmile(), "sdbuildR_xmile")

  # Check time units
  sfm = xmile()
  expect_equal(sim_specs(sfm, time_units = "d")$sim_specs$time_units, "d")
  expect_equal(sim_specs(sfm, time_units = "day")$sim_specs$time_units, "d")
  expect_equal(sim_specs(sfm, time_units = "weeks")$sim_specs$time_units, "wk")
  expect_equal(sim_specs(sfm, time_units = " months ")$sim_specs$time_units, "month")

  # Check start, stop, dt - no scientific notation
  expect_equal(sim_specs(sfm, start = 1990, stop = 2000)$sim_specs$start, "1990.0")
  expect_equal(sim_specs(sfm, start = 1, stop = 1e+06)$sim_specs$stop, "1000000.0")
  expect_equal(sim_specs(sfm, dt = 1e-08)$sim_specs$dt, "0.00000001")
  expect_warning(sim_specs(sfm, dt = .1, save_at = .01), "dt must be smaller or equal to save_at! Setting save_at equal to dt")


  # Non existing template
  expect_error(xmile("A"), "A is not an available template. The available templates are")
  expect_error(xmile(""), "is not an available template. The available templates are")
  expect_error(xmile(" "), "  is not an available template. The available templates are")

})


test_that("sim_specs() works", {

  # Ensure that default simulation specifications are with digits
  sfm = xmile()
  expect_equal(sfm$sim_specs$start, "0.0")
  expect_equal(sfm$sim_specs$stop, "100.0")
  expect_equal(sfm$sim_specs$dt, "0.01")
  expect_equal(sfm$sim_specs$save_at, "0.01")
  expect_equal(sfm$sim_specs$save_from, sfm$sim_specs$start)

  # Check that empty sim_specs() doesn't change anything
  sfm = xmile()
  sfm = sfm |> sim_specs()
  expect_equal(sfm$sim_specs$start, "0.0")

  # Check that start and stop are set correctly
  sfm = xmile()
  expect_error(sfm |> sim_specs(start = 2000, stop = 1990), "Start time must be smaller than stop time!")
  expect_error(sfm |> sim_specs(stop = -100), "Start time must be smaller than stop time!")
  expect_error(sfm |> sim_specs(start = 200), "Start time must be smaller than stop time!")

  # Check that seed must be an integer
  expect_error(sfm |> sim_specs(seed = "a"), "seed must be an integer!")
  expect_error(sfm |> sim_specs(seed = 1.5), "seed must be an integer!")

  # Check that empty seed becomes NULL
  expect_equal(sim_specs(sfm, seed = "")$sim_specs$seed, NULL)

  # Check that removing seed works
  sfm = sfm |> sim_specs(seed = 1)
  expect_equal(sim_specs(sfm, seed = NULL)$sim_specs$seed, NULL)

  # Check that dt must be smaller than stop - start
  expect_error(xmile() |> sim_specs(start = 0, stop = .05, dt = .1), "dt must be smaller than the difference between start and stop!")
  expect_error(xmile() |> sim_specs(start = 0, stop = 1, save_at = 2), "save_at must be smaller than the difference between start and stop!")

  # save_at and dt
  expect_no_error(xmile() |> sim_specs(dt = .1))
  sfm = xmile() |> sim_specs(dt = 0.1)
  expect_equal(sfm$sim_specs$dt, "0.1")
  expect_equal(sfm$sim_specs$save_at, "0.1")


  # save_from
  sfm = xmile() |> sim_specs(start = 0, stop = 100, dt = .01, save_at = .1)
  expect_error(sfm |> sim_specs(save_from = -1),
               "save_from must be within the start \\(0\\) and stop \\(100\\) time of the simulation")
  expect_error(sfm |> sim_specs(save_from = 101),
               "save_from must be within the start \\(0\\) and stop \\(100\\) time of the simulation")
  sfm = expect_no_error(sfm |> sim_specs(save_from = 10))
  expect_equal(sfm$sim_specs$save_from, "10.0")

  # Check that save_at is updated if start and stop are updated
  sfm = xmile("SIR") |> sim_specs(start = 0, stop = 20) |>
    sim_specs(save_at = 0.1, dt = 0.001, start = 100, stop = 200)
  expect_equal(sfm$sim_specs$save_from, "100.0")
  sfm = xmile("SIR") |> sim_specs(start = 50, stop = 100) |>
    sim_specs(save_at = 0.1, dt = 0.001, start = 0, stop = 75)
  expect_equal(sfm$sim_specs$save_from, "0.0")
  sfm = xmile("coffee_cup") |> sim_specs(start = 0, stop = 100) |>
    sim_specs(save_at = 0.1, dt = 0.001, start = 100, stop = 200)
  expect_equal(sfm$sim_specs$save_from, "100.0")

  # warning for large dt
  expect_warning(xmile() |> sim_specs(dt = 2), "dt is larger than 0\\.1! This will likely lead to inaccuracies in the simulation.")
  expect_warning(xmile() |> sim_specs(dt = .5), "dt is larger than 0\\.1! This will likely lead to inaccuracies in the simulation.")

  # Check that all time units are correctly converted
  expect_error(xmile() |> sim_specs(time_units = "10s"), "time_units can only contain letters")
  expect_equal(sim_specs(xmile(), time_units = "Sec")$sim_specs$time_units, "s")
  expect_equal(sim_specs(xmile(), time_units = " minutes ")$sim_specs$time_units, "minute")
  expect_equal(sim_specs(xmile(), time_units = "Common years")$sim_specs$time_units, "common_yr")
  expect_equal(sim_specs(xmile(), time_units = "Years")$sim_specs$time_units, "yr")
  expect_equal(sim_specs(xmile(), time_units = "Months")$sim_specs$time_units, "month")
  expect_equal(sim_specs(xmile(), time_units = "Quarters")$sim_specs$time_units, "quarter")

})


test_that("flow cannot have same stock as to and from", {
  sfm = xmile() |> build("a", "stock")
  expect_error(sfm |> build("b", "flow", to = "a", from = "a", eqn = "1"),
               "A flow cannot flow to and from the same stock")

  # Check that this works with multiple variables
  expect_error(sfm |> build(c("b", "c"), c("flow", "flow"),
                             to = c("a", "d"), from = c("a", "a")),
               "A flow cannot flow to and from the same stock")
  expect_error(sfm |> build(c("b", "c"), c("flow", "flow"),
                             to = c("a", "d"), from = c("a", "d")),
               "A flow cannot flow to and from the same stock")

  # Check that this works when adding to or from later
  sfm = xmile() |> build("b", "flow", to = "a")
  expect_message(sfm |> build("b", from = "a"),
               "b is flowing to and from the same variable \\(a\\)")

  sfm = xmile() |> build("b", "flow", from = "a")
  expect_message(sfm |> build("b", to = "a"),
                 "b is flowing to and from the same variable \\(a\\)")

})


test_that("add and change variable with build() simultaneously", {
  sfm = xmile() |>
    build("a", "stock")

  expect_error(sfm |> build(c("a", "b"), eqn = 10), "The variable b does not exist in your model")
  sfm = expect_no_error(sfm |> build(c("a", "b"), "stock", eqn = 10))
  expect_equal(sort(get_names(sfm)[["name"]]), c("a", "b"))
  expect_equal(sfm$model$variables$stock$a$eqn, "10")
  expect_equal(sfm$model$variables$stock$b$eqn, "10")

})



test_that("overwriting to and from of a flow works", {

  sfm = xmile() |> build("a", "stock") |>
    build("b", "flow", to = "a") |>
    build("b", to = "c", from = "d")
  expect_equal(sfm$model$variables$flow$b$to, "c")
  expect_equal(sfm$model$variables$flow$b$from, "d")
})


test_that("incorrect equations throw error", {

  expect_error({xmile() |> build("a", "aux", eqn = "a + (1,")}, "Parsing equation of a failed")
  expect_error({xmile() |> build("a", "aux", eqn = "<- (1)")}, "Parsing equation of a failed")

})


test_that("change_name and change_type in build()", {

  # Change name
  sfm = xmile() |> build("a", "aux", eqn = 10)
  expect_no_error(sfm |> build("a", change_name = "b"))
  expect_no_message(sfm |> build("a", change_name = "b"))
  sfm = sfm |> build("a", change_name = "b")
  expect_equal(names(sfm$model$variables$aux), "b")
  expect_equal(as.data.frame(sfm)$name, "b")
  expect_equal(sfm$model$variables$aux$b$name, "b")
  expect_equal(sfm$model$variables$aux$b$label, "b")
  expect_equal(sfm$model$variables$aux$a, NULL)

  # Check build() with change_name and other modified properties
  sfm = sfm |> build("b", change_name = "c", eqn = "100", units = "kg")
  expect_equal(names(sfm$model$variables$aux), "c")
  expect_equal(sfm$model$variables$aux$b, NULL)
  expect_equal(as.data.frame(sfm)$name, "c")
  expect_equal(sfm$model$variables$aux$c$eqn, "100")
  expect_equal(sfm$model$variables$aux$c$label, "c")
  expect_equal(sfm$model$variables$aux$c$units, "kg")

  # Check build() with change_type; are the properties copied?
  sfm = xmile() |> build("K", "aux", eqn = 100, label = "K", units = "kg")
  expect_no_error(sfm |> build("K", change_type = "stock"))
  expect_no_message(sfm |> build("K", change_type = "stock"))
  sfm = sfm |> build("K", change_type = "stock")
  expect_equal(sfm$model$variables$aux$K, NULL)
  expect_equal(sfm$model$variables$stock$K$eqn, "100")
  expect_equal(sfm$model$variables$stock$K$label, "K")
  expect_equal(sfm$model$variables$stock$K$units, "kg")
  expect_equal(sfm$model$variables$stock$K$from, NULL)
  expect_equal(sfm$model$variables$stock$K$to, NULL)
  expect_equal(as.data.frame(sfm)$type, "stock")

  # Check build() with change_type and change_name
  sfm = xmile() |> build("H", "flow", eqn = 100)
  sfm = sfm |> build("H", change_type = "aux", change_name = "H_new")
  expect_equal(sfm$model$variables$flow[["H"]], NULL)
  expect_equal(sfm$model$variables$flow[["H_new"]], NULL)
  expect_equal(sfm$model$variables$aux[["H"]], NULL)
  expect_equal(sfm$model$variables$aux$H_new$eqn, "100")
  expect_no_error(as.data.frame(sfm))
  expect_no_message(as.data.frame(sfm))
  df = as.data.frame(sfm)
  expect_equal(as.data.frame(sfm)$name, "H_new")
  expect_equal(as.data.frame(sfm)$type, "aux")

  # Check that changing type from stock -> aux removes the stock from `to` and `from` properties of flows
  sfm = xmile() |> build("G", "stock") |> build("to_G", "flow", to = "G")
  expect_message({sfm |> build("G", change_type = "aux")},
                 "to_G is flowing to a variable which is not a stock")
  suppressMessages({sfm = sfm |> build("G", change_type = "aux")})
  expect_equal(sfm$model$variables$flow$to_G$to, "")
  expect_equal(sfm$model$variables$stock$G, NULL)
  expect_equal(sfm$model$variables$aux$G$eqn, "0.0")
  expect_equal(sfm$model$variables$aux$G$units, "1")

  # Test that properties are not affected if no change is made
  sfm = xmile() |> build("abc", "aux", eqn = "def") |> build("abc")
  expect_equal(sfm$model$variables$aux$abc$eqn, "def")

  sfm = xmile() |> build("abc", "aux", eqn = "def") |> build("abc", "aux")
  expect_equal(sfm$model$variables$aux$abc$eqn, "def")

  # Check that change_name updates delay equation -> also important for change_type!
  sfm = xmile() |> build("abc", "aux", eqn = "delayN(a, 10, 2)") |> build("abc", change_name = "xyz")
  expect_equal(sfm$model$variables$aux$abc, NULL)
  expect_equal(sfm$model$variables$aux$xyz$eqn, "delayN(a, 10, 2)")
  expect_equal(names(sfm$model$variables$aux$xyz$func), "delayN")
  expect_equal(names(sfm$model$variables$aux$xyz$func[[1]]), "xyz_delayN1")

  sfm = xmile() |> build("abc", "aux", eqn = "delayN(a, 10, 2)") |> build("abc", change_type = "flow")
  expect_equal(sfm$model$variables$aux$abc, NULL)
  expect_equal(sfm$model$variables$flow$abc$eqn, "delayN(a, 10, 2)")
  expect_equal(names(sfm$model$variables$flow$abc$func), "delayN")
  expect_equal(names(sfm$model$variables$flow$abc$func[[1]]), "abc_delayN1")


  # Throw warning if change_type = "constant"
  sfm = xmile() |> build("abc", "aux", eqn = "delayN(a, 10, 2)")
  expect_error(sfm |> build("abc", change_type = "constant"), "Adjust equation of abc: delayN\\(\\) cannot be used for a constant")
  expect_error(sfm |> build("abc", change_type = "stock"), "Adjust equation of abc: delayN\\(\\) cannot be used for a stock")
  expect_equal(sfm$model$variables$aux$abc$eqn, "delayN(a, 10, 2)")

  # Check multiple change names
  sfm = template("predator-prey")
  expect_error(sfm |> build("prey", change_name = c("prey1", "prey2")), "You can only change the name of one variable at a time")
  expect_error(sfm |> build(c("prey", "predator"), change_name = c("prey1", "prey2")), "You can only change the name of one variable at a time")

  expect_error(sfm |> build("prey", change_type = c("stock", "aux")), "You can only change the type of one variable at a time")
  expect_error(sfm |> build(c("prey", "predator"), change_type = "stock"), "You can only change the type of one variable at a time")

  sfm = sfm |> build("prey", change_name = "frustration") |> build("predator", change_name = "drugs")
  df = as.data.frame(sfm, type = "stock")
  expect_equal(sort(df$name), c("drugs", "frustration"))
  expect_equal(sort(df$label), c("Predator", "Prey")) # Label is unchanged
  df = as.data.frame(sfm)
  expect_equal(grepl("frustration", df[df$name == "predator_births", "eqn"]), TRUE)
  expect_equal(grepl("prey", df[df$name == "predator_births", "eqn"]), FALSE)

  expect_equal(grepl("drugs", df[df$name == "predator_deaths", "eqn"]), TRUE)
  expect_equal(grepl("predator", df[df$name == "predator_deaths", "eqn"]), FALSE)

  expect_equal(grepl("frustration", df[df$name == "prey_births", "eqn"]), TRUE)
  expect_equal(grepl("prey", df[df$name == "prey_births", "eqn"]), FALSE)

  expect_equal(grepl("frustration", df[df$name == "prey_deaths", "eqn"]), TRUE)
  expect_equal(grepl("prey", df[df$name == "prey_deaths", "eqn"]), FALSE)

  expect_equal(grepl("drugs", df[df$name == "prey_deaths", "eqn"]), TRUE)
  expect_equal(grepl("predator", df[df$name == "prey_deaths", "eqn"]), FALSE)


  # Check that no message or warning is thrown
  sfm = xmile() |>
    build("a", "aux")

  expect_no_error(sfm |> build("a", change_type = "flow", from = "b"))
  expect_no_warning(sfm |> build("a", change_type = "flow", from = "b"))
  expect_no_message(sfm |> build("a", change_type = "flow", from = "b"))

  # Check that label is retained
  sfm = sfm |> build("a", change_type = "flow", label = "B")
  expect_equal(sfm$model$variables$flow$a$label, "B")

})



test_that("erase in build() works", {

  sfm = xmile("Lorenz")
  expect_no_error(sfm |> build("x", erase = TRUE))
  expect_no_message(sfm |> build("x", erase = TRUE))
  sfm = sfm |> build("x", erase = TRUE)
  expect_equal(sfm$model$variables$stock$x, NULL)
  expect_equal(sfm$model$variables$flow$dx_dt$to, NULL)
  df = as.data.frame(sfm, type = "stock")
  expect_equal(sort(df$name), c("y", "z"))

  # erase while specifying wrong type
  sfm = xmile("Lorenz")
  expect_error(sfm |> build("dy_dt", "stock", erase = TRUE),
               "These variables exist in your model but not as the type specified")

  # erase while specifying type and other properties; these should be ignored
  expect_no_error(sfm |> build("x", "stock", eqn = "10", units = "kg", erase = TRUE))
  expect_no_message(sfm |> build("x", "stock", eqn = "10", units = "kg", erase = TRUE))
  sfm = sfm |> build("sigma", "constant", eqn = "10", units = "kg", erase = TRUE)
  expect_equal(sfm$model$variables$constant$sigma, NULL)
  df = as.data.frame(sfm)
  expect_equal("sigma" %in% df$name, FALSE)

})


test_that("inappropriate properties throw warning", {

  expect_warning(xmile() |> build("c", "aux", from = "a"),
                 "These properties are not appropriate for the specified type")
  sfm = suppressWarnings(xmile() |> build("c", "aux", from = "a"))
  expect_equal(sfm$model$variables$aux$c$from, NULL)
  expect_equal(sfm$model$variables$aux$c$to, NULL)
  expect_equal(sfm$model$variables$aux$c$eqn, "0.0")

  expect_warning(xmile() |> build(c("a", "b", "c"), c("aux", "stock", "flow"),
                                   eqn = 1,
                                   from = "b"),
                 "These properties are not appropriate for all specified types \\(aux, stock, flow\\):\\n- from\nThese will be ignored")
  sfm = suppressWarnings(xmile() |> build(c("a", "b", "c"),
                                           c("aux", "stock", "flow"),
                                           eqn = 1,
                                           from = "b"))
  expect_equal(sfm$model$variables$aux$a$from, NULL)
  expect_equal(sfm$model$variables$stock$b$from, NULL)
  expect_equal(sfm$model$variables$flow$c$from, "b")
  df = data.frame(sfm)
  expect_equal(is.na(df[df$name == "a", "from"]), TRUE)
  expect_equal(is.na(df[df$name == "b", "from"]), TRUE)
  expect_equal(df[df$name == "c", "from"], "b")


})


test_that("Julia equations are added in build()", {

  # Ensure Julia equations are added immediately
  sfm = xmile() |> build("c", "aux", eqn = "a")
  expect_equal("eqn_julia" %in% names(sfm$model$variables$aux$c), TRUE)
  expect_equal(sfm$model$variables$aux$c$eqn_julia, "a")

  # Ensure Julia equation is updated with changed equation
  sfm = sfm |> build("c", eqn = "90")
  expect_equal(sfm$model$variables$aux$c$eqn_julia, "90.0")

  # Ensure this works with multiple variables
  sfm = xmile() |> build(c("a", "b", "c"), c("stock", "flow", "aux"), eqn = c("1", "2", "3"))
  expect_equal(sfm$model$variables$stock$a$eqn_julia, "1.0")
  expect_equal(sfm$model$variables$flow$b$eqn_julia, "2.0")
  expect_equal(sfm$model$variables$aux$c$eqn_julia, "3.0")

  # Ensure Julia equation is updated with changed equation
  sfm = sfm |> build(c("a", "b", "c"), eqn = c("40", "50", "60"))
  expect_equal(sfm$model$variables$stock$a$eqn_julia, "40.0")
  expect_equal(sfm$model$variables$flow$b$eqn_julia, "50.0")
  expect_equal(sfm$model$variables$aux$c$eqn_julia, "60.0")

})


test_that("vectorized adding variables works in build()", {

  sfm = xmile() |> build(c("a", "b", "c"), c("stock", "flow", "aux"), eqn = c("1", "2", "3"))
  expect_equal(sfm$model$variables$stock$a$eqn, "1")
  expect_equal(sfm$model$variables$flow$b$eqn, "2")
  expect_equal(sfm$model$variables$aux$c$eqn, "3")
  expect_equal(sfm$model$variables$stock$a$units, "1")
  expect_equal(sfm$model$variables$flow$b$units, "1")
  expect_equal(sfm$model$variables$aux$c$units, "1")
  expect_equal(sfm$model$variables$stock$a$label, "a")
  expect_equal(sfm$model$variables$flow$b$label, "b")
  expect_equal(sfm$model$variables$aux$c$label, "c")
  expect_equal(sfm$model$variables$stock$a$from, NULL)
  expect_equal(sfm$model$variables$flow$b$from, NULL)
  df = as.data.frame(sfm)
  expect_equal(sort(df$name), c("a", "b", "c"))
  expect_equal(sort(df$type), c("aux", "flow", "stock"))

  # Add some vectorized properties and some single properties, as well as "wrong" properties for that type
  expect_warning(xmile() |> build(c("x", "y", "z"), "stock", eqn = 300,
                                   units = c("kilograms", "10 meters", "3 Sec"),
                                   label = c("X", "Y", "Z"), from = c("a", "b", "c"),
                                   xpts = c(1, 2, 3), ypts = c(4, 5, 6)),
                 "These properties are not appropriate for the specified type \\(stock\\):\\n- from, xpts, ypts")
  sfm = suppressWarnings(xmile() |> build(c("x", "y", "z"), "stock", eqn = 300,
                                           units = c("kilograms", "10 meters", "3 Sec"),
                                           label = c("X", "Y", "Z"), from = c("a", "b", "c"),
                                           xpts = c(1, 2, 3), ypts = c(4, 5, 6)))
  expect_equal(sfm$model$variables$stock$x$eqn, "300")
  expect_equal(sfm$model$variables$stock$y$eqn, "300")
  expect_equal(sfm$model$variables$stock$z$eqn, "300")
  expect_equal(sfm$model$variables$stock$x$units, "kg")
  expect_equal(sfm$model$variables$stock$y$units, "10m")
  expect_equal(sfm$model$variables$stock$z$units, "3s")
  expect_equal(sfm$model$variables$stock$x$from, NULL)
  expect_equal(sfm$model$variables$stock$y$from, NULL)
  expect_equal(sfm$model$variables$stock$z$from, NULL)
  expect_equal(sfm$model$variables$stock$x$xpts, NULL)
  expect_equal(sfm$model$variables$stock$y$xpts, NULL)
  expect_equal(sfm$model$variables$stock$z$xpts, NULL)
  expect_equal(sfm$model$variables$stock$x$ypts, NULL)
  expect_equal(sfm$model$variables$stock$y$ypts, NULL)
  expect_equal(sfm$model$variables$stock$z$ypts, NULL)

})


test_that("flows always have a from and to property", {

  sfm = xmile() |> build("a", "flow")
  expect_equal("from" %in% names(sfm$model$variables$flow$a), TRUE)
  expect_equal("to" %in% names(sfm$model$variables$flow$a), TRUE)

  sfm = xmile() |> build("a", "flow", to = "a")
  expect_equal("from" %in% names(sfm$model$variables$flow$a), TRUE)
  expect_equal("to" %in% names(sfm$model$variables$flow$a), TRUE)


})

test_that("build() works", {

  # Empty build() gives error
  expect_error({build()}, "No model specified")
  expect_error({xmile() |> build()}, "name must be specified")

  sfm = xmile()
  sfm = sfm |> build("a", "aux", eqn = 10)

  # Try to add wrong type
  expect_error({sfm |> build("a", "Non")}, "type needs to be one of 'stock', 'flow', 'constant', 'aux', or 'gf'")

  # Add auxiliary
  expect_equal(names(sfm$model$variables$aux), "a")
  expect_equal(sfm$model$variables$aux$a$eqn, "10")

  # Try to modify variable and add new variable simultaneously
  sfm = xmile() |> build("a", "stock")
  sfm = sfm |> build(c("a", "b"), c("stock", "flow"), eqn = c("100", "1000"), units = "seconds")

  expect_equal(sfm$model$variables$stock$a$eqn, "100")
  expect_equal(sfm$model$variables$flow$b$eqn, "1000")
  expect_equal(sfm$model$variables$stock$a$units, "s")
  expect_equal(sfm$model$variables$flow$b$units, "s")

  # Try to overwrite existing variable whilst specifying the type
  sfm = sfm |> build("a", "stock", eqn = "10000")
  expect_equal(sfm$model$variables$stock$a$eqn, "10000")

  # Try to overwrite existing variable whilst specifying the wrong type
  expect_error({sfm |> build("a", "flow", eqn = "90")}, "These variables already exist in your model, but not as the type specified")
  expect_equal(sfm$model$variables$stock$a$eqn, "10000")

  # Try to modify non-existing variable
  expect_error(xmile() |> build("b", change_name = "c"), "b does not exist in your model!")

  # Ensure NULL changes to "0"
  expect_warning(xmile() |> build("c", "aux", eqn = NULL), "Equation cannot be NULL!")
  suppressWarnings({sfm = xmile() |> build("c", "aux", eqn = NULL)})
  expect_equal(sfm$model$variables$aux$c$eqn, "0.0")
  expect_equal(sfm$model$variables$aux$c$eqn_julia, "0.0")

  # Ensure empty equation changes to "0"
  expect_warning(xmile() |> build("c", "aux", eqn = ""), "Equation cannot be empty!")
  suppressWarnings({sfm = xmile() |> build("c", "aux", eqn = "")})
  expect_equal(sfm$model$variables$aux$c$eqn, "0.0")
  expect_equal(sfm$model$variables$aux$c$eqn_julia, "0.0")

  # Ensure equation with missing brackets throws useful error
  expect_error(xmile() |> build("c", "aux", eqn = "a + (1, "), "Parsing equation of c failed")

  # # Multiple equations but only one name should throw error
  # expect_error({xmile() |> build("a", eqn = c("1", "2"))},
  # "The length of eqn =  must be either 1 or equal to the length of name = 'a'.")


})


test_that("ensure_length() works", {

  # Check that error is thrown when length(arg) != length(target)
  eqn = c("1", "2", "3")
  name = c("a")
  expect_error(ensure_length(eqn, name), "The length of eqn = 1, 2, 3 must be either 1 or equal to the length of name = a")

  # Should work when length(arg) == length(target)
  eqn = c("1", "2", "3")
  name = c("a", "b", "c")
  expect_no_error(ensure_length(eqn, name))

  eqn = c("1")
  name = c("a", "b", "c")
  expect_no_error(ensure_length(eqn, name))

  # Check whether length of arg is changed
  eqn = c("1")
  name = c("a", "b", "c")
  expect_equal(ensure_length(eqn, name), c("1", "1", "1"))

})


test_that("model_units() works", {

  sfm = xmile()
  sfm = sfm |> model_units("abc", "0")
  result = names(sfm$model_units)
  expected = "abc"
  expect_equal(result, expected)
  expect_no_error(as.data.frame(sfm, type = "model_units"))
  expect_no_message(as.data.frame(sfm, type = "model_units"))
  expect_equal(as.data.frame(sfm, type = "model_units")$name, "abc")
  expect_equal(as.data.frame(sfm, type = "model_units")$eqn, "0")

  # Check that model units definition can be overwritten
  sfm = sfm |> model_units("abc", "10 meters")
  result = sfm$model_units$abc$eqn
  expected = "10m"
  expect_equal(result, expected)
  expect_equal(as.data.frame(sfm, type = "model_units")$eqn, "10m")

  # Check that doc is overwritten
  sfm = sfm |> model_units("abc", doc = "New doc")
  result = sfm$model_units$abc$doc
  expected = "New doc"
  expect_equal(result, expected)
  expect_equal(as.data.frame(sfm, type = "model_units")$doc, "New doc")

  # Default eqn
  sfm = xmile() |> model_units("abc")
  result = sfm$model_units$abc$eqn
  expected = "1"
  expect_equal(result, expected)

  # Check overwriting with multiple units
  sfm = xmile() |> model_units(c("abc", "def"), "10 meters")
  expect_equal(sfm$model_units$abc$eqn, "10m")
  expect_equal(sfm$model_units$def$eqn, "10m")
  sfm = xmile() |> model_units(c("abc", "def"), "100 kilograms/40 sec")
  expect_equal(sfm$model_units$abc$eqn, "100kg/40s")
  expect_equal(sfm$model_units$def$eqn, "100kg/40s")

  # Vector of model units
  sfm = sfm |> model_units(c("abc", "def"), "1")
  result = names(sfm$model_units)
  expected = c("abc", "def")
  expect_equal(result, expected)

  result = purrr::map_vec(sfm$model_units, "eqn") |> unname()
  expected = c("1", "1")
  expect_equal(result, expected)


  # Multiple dependent model units
  sfm = xmile() |>
    model_units("stressors") |> model_units("challenge", eqn = "stressors/d")
  result = sort(names(sfm$model_units))
  expected = c("challenge", "stressors")
  expect_equal(result, expected)

  # Check written powers and per
  sfm = xmile() |> model_units("BMI", eqn = "kilograms per meters squared",
              doc = "Body Mass Index")
  result = sfm$model_units$BMI$eqn
  expected = "kg/m^2"
  expect_equal(result, expected)
  df = as.data.frame(sfm)
  expect_equal(df[df$name == "BMI", "eqn"], "kg/m^2")
  expect_equal(df[df$name == "BMI", "doc"], "Body Mass Index")

  # Check use of per in custom units
  expect_warning(xmile() |> model_units("Person per year"), "The custom unit name Person per year was modified to Person_yr to comply with Julia's syntactic rules")

})


test_that("find_dependencies works", {

  sfm = xmile("SIR")
  expect_no_error(find_dependencies(sfm))
  expect_no_message(find_dependencies(sfm))

})


test_that("unique unit names in model_units()", {

  # Existing unit cannot be overwritten
  expect_error(xmile() |> model_units("d"), "The custom unit name d matches the standard unit d, which cannot be overwritten")
  expect_error(xmile() |> model_units("a"), "The custom unit name a matches the standard unit a, which cannot be overwritten")
  expect_error(xmile() |> model_units("kg"), "The custom unit name kg matches the standard unit kg, which cannot be overwritten")
  expect_error(xmile() |> model_units("$$$"), "The custom unit name \\$\\$\\$ matches the standard unit USD, which cannot be overwritten")
  expect_error(xmile() |> model_units("€"), "The custom unit name € matches the standard unit EUR, which cannot be overwritten") # \\u20AC
  expect_error(xmile() |> model_units("Ohm"), "The custom unit name Ohm matches the standard unit Ohm, which cannot be overwritten")

  # Custom unit names should contain at least one letter
  expect_error(xmile() |> model_units("*"), "Each custom unit name needs at least one letter or number.")
  expect_error(xmile() |> model_units("%"), "Each custom unit name needs at least one letter or number.")

  # Existing unit cannot be overwritten, also when not using the standard symbol but something that is translated to the standard symbol
  expect_error(xmile() |> model_units("kilograms"), "The custom unit name kilograms matches the standard unit kg, which cannot be overwritten")
  expect_error(xmile() |> model_units("meters"), "The custom unit name meters matches the standard unit m, which cannot be overwritten")
  expect_error(xmile() |> model_units("milliseconds"), "The custom unit name milliseconds matches the standard unit ms, which cannot be overwritten")

  # Throw message if unit name was changed
  expect_warning(xmile() |> model_units("CO^2"), "The custom unit name CO\\^2 was modified to CO_2 to comply with Julia's syntactic rules")
  expect_warning(xmile() |> model_units("life-years"), "The custom unit name life-years was modified to life_yr to comply with Julia's syntactic rules")
  expect_warning(xmile() |> model_units("Beck Depression Inventory"), "The custom unit name Beck Depression Inventory was modified to BeckDepressionInventory to comply with Julia's syntactic rules")
  expect_warning(xmile() |> model_units("10M!"), "The custom unit name 10M! was modified to _10M_ to comply with Julia's syntactic rules")

  # Throw message if unit name was changed with multiple units
  # one unit is fine, the other not
  expect_warning(xmile() |> model_units(c("S&P", "myunit")), "The custom unit name S&P was modified to S_P to comply with Julia's syntactic rules")

  # both are changed
  expect_warning(xmile() |> model_units(c("%household", "(myunit)")), "The custom unit names %household, \\(myunit\\) were modified to _household, _myunit_ to comply with Julia's syntactic rules")
  # both are fine
  expect_no_error(xmile() |> model_units(c("joulesperhour", "MilesWalked")))
  expect_no_warning(xmile() |> model_units(c("joulesperhour", "MilesWalked")))

})



test_that("erase in model_units() works", {

  # Erase units
  sfm = xmile() |> model_units("abc", eqn = "def") |> model_units("abc", erase = TRUE)
  expect_equal(length(names(sfm$model_units)), 0)
  df = as.data.frame(sfm, type = "model_units")
  expect_equal(nrow(df), 0)

  # Erase multiple units
  sfm = xmile() |> model_units(c("abc", "def", "ghi")) |> model_units(c("abc", "def"), erase = TRUE)
  expect_equal(length(names(sfm$model_units)), 1)
  df = as.data.frame(sfm, type = "model_units")
  expect_equal(nrow(df), 1)
  expect_equal(df$name, "ghi")


})


test_that("change_name in model_units() works", {
  # Change name
  sfm = xmile() |> model_units("abc", eqn = "def")
  expect_no_error(sfm |> model_units("abc", change_name = "xyz"))
  expect_no_message(sfm |> model_units("abc", change_name = "xyz"))
  sfm = sfm |> model_units("abc", change_name = "xyz")
  expect_equal(names(sfm$model_units), "xyz")
  expect_equal(sfm$model_units$xyz$eqn, "def")
  expect_equal(sfm$model_units$abc, NULL)
  df = as.data.frame(sfm, type = "model_units")
  expect_equal(df$name, "xyz")


  # Test that properties are not affected if no change is made
  sfm = xmile() |> model_units("abc", eqn = "def") |> model_units("abc")
  expect_equal(sfm$model_units$abc$eqn, "def")


})



test_that("get_names() works", {

  # Check no variables
  expect_equal(get_names(xmile()), data.frame(type = character(), name = character(), label = character(), units = character()))

  # Check with variables
  sfm = xmile() |> build("a", "aux") |> build("b", "aux")
  result = get_names(sfm)
  expected = data.frame(
    type = c("aux", "aux"),
    name = c("a", "b"),
    label = c("a", "b"),
    units = c("1", "1")
  )
  expect_equal(result, expected)

  # Check with units
  sfm = xmile() |> build("a", "stock", units = "1/s") |> build("b", "aux", units = "m")
  result = get_names(sfm)
  expected = data.frame(
    type = c("stock", "aux"),
    name = c("a", "b"),
    label = c("a", "b"),
    units = c("1/s", "m")
  )
  expect_equal(result, expected)

  # Check with label
  sfm = xmile() |> build("a", "stock", label = "A") |> build("b", "aux", label = "B")
  result = get_names(sfm)
  expected = data.frame(
    type = c("stock", "aux"),
    name = c("a", "b"),
    label = c("A", "B"),
    units = c("1", "1")
  )
  expect_equal(result, expected)

})


test_that("create_R_names() works", {

  sfm = xmile()
  names_df = get_names(sfm)

  # Check for syntactically correct names
  expect_equal(create_R_names(c("TRUE", "T"), names_df), c("TRUE__1", "T_1"))
  expect_equal(create_R_names(c("a", "b", "T"), names_df), c("a", "b", "T_1"))
  expect_equal(create_R_names(c("a-1", "b!2", "c.1"), names_df), c("a_1", "b_2", "c_1"))
  expect_equal(create_R_names(c("a-1", "a!1"), names_df), c("a_1", "a_1_1"))
  expect_equal(create_R_names(c(" Hell0 ", "Hell0"), names_df), c("Hell0", "Hell0_1"))

  # Difficult, but ensure unique names
  expect_equal(create_R_names(c("F"), data.frame(name = "F_1")), c("F_1_1"))
  expect_equal(create_R_names(c("-1", "_1"), names_df), c("X_1", "X_1_1"))


})


test_that("debugger() works", {

  expect_message({debugger(xmile("SIR"))}, "No problems detected")
  expect_message({debugger(xmile("SIR"))}, "These variables have an equation of 0:\\n- Recovered")
  expect_message({debugger(xmile("predator-prey"))}, "No problems detected!")
  expect_message({debugger(xmile("logistic_model"))}, "No problems detected!")
  expect_message({debugger(xmile("Crielaard2022"))}, "No problems detected!")

  # Detect absence of stocks or flows
  expect_message({debugger(xmile())}, "Your model has no stocks")

  # Detect stocks without inflows or outflows
  expect_message({debugger(xmile() |> build("Prey", "stock"))}, "Your model has no flows.")

  # Detect one stock without inflows or outflows
  sfm = xmile() |>
    build("Prey", "stock") |>
    build("Predator", "stock") |>
    build("births", "flow", eqn = "0.1 * Predator", to = "Prey")
  expect_message({debugger(sfm)}, "These stocks are not connected to any flows:\\n- Predator")

  # Detect circularities in equation definitions
  expect_message({debugger(sfm = xmile() |> build("Prey", "stock", eqn = "Predator") |>
                             build("Predator", "stock", eqn = "Prey"))}, "Ordering static equations failed. Circular dependencies detected involving variables: Predator, Prey")

  sfm = xmile("logistic_model")
  sfm = sfm |>
    build("X", change_name = "tasks") |>
    build("K", label = "Resource") |>
    build("K", change_type = "stock")
  expect_message(debugger(sfm), "These stocks are not connected to any flows:\\n- K")



  # **to do: dependency on itself
  # sfm = xmile() |> build("a", "stock", eqn = "cos(a)")
  # sim = simulate(sfm)

})


test_that("detect_undefined_var() works", {

  # Check that undefined variables are detected
  sfm = xmile() |> build("a", "aux", eqn = "b + c")
  out = detect_undefined_var(sfm)
  expect_equal(grepl("The properties below contain references to undefined variables.\nPlease define the missing variables or correct any spelling mistakes", out$msg), TRUE)

  # Check that no error is thrown for defined variables
  sfm = xmile() |> build("a", "aux", eqn = "b + c") |>
    build("b", "aux") |> build("c", "aux")
  out = detect_undefined_var(sfm)
  expect_equal(out$issue, FALSE)

  #** macros should also be found

})


test_that("detect_undefined_units() works", {

  # Check that undefined variables are detected
  sfm = xmile() |> build("a", "aux", units = "BMI")
  expect_message(debugger(sfm), "These units are not defined:\\n- BMI")

  sfm = xmile() |> build("a", "aux", units = "BMI/year")
  expect_message(debugger(sfm), "These units are not defined:\\n- BMI")

  # Check that no error is thrown for defined units
  sfm = xmile() |> build("a", "aux", units = "BMI/year") |> model_units("BMI", eqn = "kilograms/meters^2")
  out = debugger(sfm, quietly = TRUE)
  expect_equal(grepl("These units are not defined", out$problems), FALSE)
})



test_that("get_build_code() works", {

  expect_no_error(get_build_code(xmile()))
  expect_no_message(get_build_code(xmile()))

  for (s in c("SIR", "Crielaard2022")){

    # Replicate with get_build_code
    sfm = xmile(s)

    if (s == "Crielaard2022"){
      sfm = sfm |>
        build(c("Food_intake", "Hunger", "Compensatory_behaviour"), eqn = c(.5, .3, .1))
    }

    sim1 = simulate(sfm)
    script = expect_no_error(get_build_code(sfm))
    script = expect_no_message(get_build_code(sfm))

    # Create a new environment to collect variables
    envir <- new.env()
    expect_no_error(eval(parse(text = script), envir = envir))
    expect_no_message(eval(parse(text = script), envir = envir))
    sfm2 = envir[["sfm"]]
    sim2 = simulate(sfm2)
    expect_identical(sim1$df$value, sim2$df$value)
  }

})


test_that("as.data.frame(sfm) works", {

  # Check that as.data.frame() works
  sfm = xmile()
  expect_equal(class(as.data.frame(sfm)), "data.frame")
  expect_equal(nrow(as.data.frame(sfm)), 0)

  # Check that as.data.frame() works with variables
  sfm = sfm |> build("a", "aux", eqn = "10")
  df = as.data.frame(sfm)
  expect_equal(class(df), "data.frame")
  expect_equal(all(c("type", "name", "eqn", "label", "units") %in% names(df)), TRUE)
  expect_equal(any(c("intermediary", "func") %in% names(df)), FALSE)

  # Check that it works with templates
  sfm = xmile("predator-prey")
  expect_equal(nrow(as.data.frame(sfm)) > 0, TRUE)

  # Specify type
  sfm = xmile("predator-prey")
  expect_error(as.data.frame(sfm, type = "auxiliary"), "type needs to be one or more of")
  expect_no_error(as.data.frame(sfm, type = "Stock")) # works with capital letters
  expect_no_error(as.data.frame(sfm, type = c("stock", "gf"))) # works with multiple types
  expect_no_error(as.data.frame(sfm, type = c("gf"))) # works when type doesn't exist
  expect_equal(nrow(as.data.frame(sfm, type = c("gf"))), 0) # works when type doesn't exist
  expect_no_error(as.data.frame(sfm, type = c("gf"))) # works with model units

  # Specify name
  sfm = xmile("Lorenz")
  expect_error(as.data.frame(sfm, name = "a"), "a does not exist in your model")
  expect_error(as.data.frame(sfm, name = c("sigma", "rho", "X")), "X does not exist in your model")
  expect_error(as.data.frame(sfm, name = ""), "At least one name must be specified")
  expect_no_error(as.data.frame(sfm, name = "x"))
  expect_no_error(as.data.frame(sfm, name = c("x", "dy_dt", "sigma")))

  # Specify properties
  sfm = xmile("predator-prey")
  expect_error(as.data.frame(sfm, properties = "a"), "a is not an existing property")
  expect_error(as.data.frame(sfm, properties = c("a", "b")), "a, b are not existing properties")
  expect_error(as.data.frame(sfm, properties = c("a", "eqn")), "a is not an existing property")
  expect_error(as.data.frame(sfm, properties = ""), "At least one property must be specified")
  expect_no_error(as.data.frame(sfm, properties = c("eqn", "units")))
  expect_equal(names(as.data.frame(sfm, properties = c("eqn", "units"))), c("type", "name", "eqn", "units"))

  # Works with model units
  sfm = xmile() |> model_units("BMI", eqn = "kilograms/meters^2", doc = "Body Mass Index") |>
    model_units("BAC", eqn = "grams/deciliter",
                doc = "Blood Alcohol Concentration, grams of alcohol per deciliter of blood") |>
    model_units("bottle", eqn = "2liters") |>
    model_units("meal", eqn = "700kcal")

  df = as.data.frame(sfm)
  expect_equal(all(df$type == "model_units"), TRUE)
  expect_equal(all(c("BMI", "BAC", "bottle", "meal") %in% df$name), TRUE)
  expect_equal(as.data.frame(sfm, name = "BMI")$name, "BMI")
  expect_equal(names(as.data.frame(sfm, properties = "eqn")), c("type", "name", "eqn"))
  expect_equal(nrow(as.data.frame(sfm, type = c("gf"))), 0)
  expect_no_error(as.data.frame(sfm, type = c("model_units")))

  sfm = xmile() |> model_units("abc") |> model_units("abc", erase = TRUE)
  expect_no_error(as.data.frame(sfm, type = "model_units"))
  expect_no_message(as.data.frame(sfm, type = "model_units"))

  # Works with macros
  sfm = xmile() |> macro("a", eqn = "1", doc = "a macro") |>
    macro("b") |>
    macro("c")
  expect_no_error(as.data.frame(sfm))
  df = as.data.frame(sfm)
  expect_equal(all(df$type == "macro"), TRUE)
  expect_equal(all(c("a", "b", "c") %in% df$name), TRUE)
  expect_equal(as.data.frame(sfm, name = "a")$name, "a")
  expect_equal(names(as.data.frame(sfm, properties = "eqn")), c("type", "name", "eqn"))
  expect_equal(nrow(as.data.frame(sfm, type = c("aux"))), 0)

  # Combine type, name, properties
  sfm = xmile("Lorenz")
  expect_no_error(as.data.frame(sfm, name = c("x", "y", "z"), type = c("stock", "flow", "aux")))
  expect_no_error(as.data.frame(sfm, name = c("x", "y", "z"), type = c("stock", "flow", "aux"), properties = c("eqn", "units", "label", "doc", "from")))

  df = as.data.frame(sfm, name = c("x", "y", "z"), type = c("stock", "flow", "aux"), properties = c("eqn", "units", "label", "doc", "from"))
  expect_equal(names(df), c("type", "name", "eqn", "units", "label")) # "doc", "from" are not recorded for these variables
  expect_equal(nrow(df), 3)

  # Check with Julia properties
  expect_no_error(as.data.frame(xmile("SIR"), properties = c("type", "name", "eqn_julia")))
  expect_equal(colnames(as.data.frame(xmile("SIR"), properties = c("eqn_julia"))), c("type", "name", "eqn_julia"))
  expect_equal(sort(colnames(as.data.frame(xmile("SIR"), properties = c("eqn_julia", "eqn")))), c("eqn", "eqn_julia", "name", "type"))

})


test_that("summary() works", {

  ans = summary(xmile("SIR"))
  expect_equal(!grepl("NULL", ans), TRUE)


})


test_that("macro() works", {

  # No name throws error
  expect_error(xmile() |> macro(), "name must be specified!")

  # Default properties
  sfm = xmile() |> macro("abc")
  expect_equal(sfm$macro$abc$eqn, "0.0")
  expect_equal(sfm$macro$abc$doc, "")

  # Test that properties are not affected if no change is made
  sfm = xmile() |> macro("abc", eqn = "def") |> macro("abc")
  expect_equal(sfm$macro$abc$eqn, "def")

  sfm = xmile() |> macro("abc", eqn = "def", doc = "Don't edit") |> macro("abc", doc = "Edit me")
  expect_equal(sfm$macro$abc$eqn, "def")
  expect_equal(sfm$macro$abc$doc, "Edit me")

  # Multiple macros
  sfm = xmile() |> macro(c("abc", "def"), eqn = c("1", "2"), doc = c("A", "B"))
  expect_equal(sfm$macro$abc$eqn, "1")
  expect_equal(sfm$macro$abc$eqn_julia, "abc = 1.0")
  expect_equal(sfm$macro$def$eqn, "2")
  expect_equal(sfm$macro$def$eqn_julia, "def = 2.0")
  expect_equal(sfm$macro$abc$doc, "A")
  expect_equal(sfm$macro$def$doc, "B")

  # Check valid names
  sfm = xmile()
  expect_warning(sfm |> macro("F"), "Names were changed to be syntactically valid and/or avoid overlap: F -> F_1")
  expect_warning(sfm |> macro("TRUE"), "Names were changed to be syntactically valid and/or avoid overlap: TRUE -> TRUE_")
  expect_warning(sfm |> macro("function"), "Names were changed to be syntactically valid and/or avoid overlap: function -> function_")
  expect_warning(sfm |> macro("while"), "Names were changed to be syntactically valid and/or avoid overlap: while -> while_")
  expect_warning(sfm |> macro("for"), "Names were changed to be syntactically valid and/or avoid overlap: for -> for_")

  # Translating functions works
  sfm = xmile() |> build("X", "aux", eqn = "K(t)") |>
    macro("K", eqn = "function(x) 1 + x")
  expect_equal(sfm$model$variables$aux$X$eqn, "K(t)")
  expect_equal(sfm$model$variables$aux$X$eqn_julia, "K(t)")
  expect_equal(sfm$macro$K$eqn, "function(x) 1 + x")
  expect_equal(sfm$macro$K$eqn_julia, "function K(x)\n 1.0 .+ x\nend")

  # change_name
  sfm = xmile() |> macro("abc", eqn = "def") |> macro("abc", change_name = "xyz")
  expect_equal(sfm$macro$xyz$eqn, "def")
  expect_equal(sfm$macro$abc, NULL)
  df = as.data.frame(sfm, type = "macro")
  expect_equal(df$name, "xyz")

  sfm = xmile() |> build("X", "aux", eqn = "G(t)") |>
    macro("G", eqn = "function(x) 1 + x")
  expect_equal(sfm$model$variables$aux$X$eqn, "G(t)")
  expect_equal(sfm$model$variables$aux$X$eqn_julia, "G(t)")
  expect_equal(sfm$macro$G$eqn, "function(x) 1 + x")
  expect_equal(sfm$macro$G$eqn_julia, "function G(x)\n 1.0 .+ x\nend")
  expect_equal(sfm$macro$F1, NULL)


  # Check that change_name is changed throughout the model
  sfm = xmile() |> build("X", "aux", eqn = "F1(t)") |>
    macro("F1", eqn = "function(x) 1 + x") |> macro("F1", change_name = "G")
  expect_equal(sfm$model$variables$aux$X$eqn, "G(t)")
  expect_equal(sfm$model$variables$aux$X$eqn_julia, "G(t)")
  expect_equal(sfm$macro$G$eqn, "function(x) 1 + x")
  expect_equal(sfm$macro$G$eqn_julia, "function G(x)\n 1.0 .+ x\nend")
  expect_equal(sfm$macro$F1, NULL)

  # Try to add delay()
  sfm = xmile()
  expect_error(sfm |> macro("a", eqn = "delay()"), "Obligatory arguments variable, length are missing for function delay")
  expect_error(sfm |> macro("a", eqn = "past()"), "Obligatory argument variable is missing for function past")
  expect_error(sfm |> macro("a", eqn = "delayN()"), "Obligatory arguments variable, length, order are missing for function delayN")
  expect_error(sfm |> macro("a", eqn = "smoothN()"), "Obligatory arguments variable, length, order are missing for function smoothN")
  # expect_error(sfm |> macro("a", eqn = "delay()"), "Adjust equation of a: delay\\(\\) cannot be used for a macro")
  # expect_error(sfm |> macro("a", eqn = "past()"), "Adjust equation of a: past\\(\\) cannot be used for a macro")
  # expect_error(sfm |> macro("a", eqn = "delayN()"), "Adjust equation of a: delayN\\(\\) cannot be used for a macro")
  # expect_error(sfm |> macro("a", eqn = "smoothN()"), "Adjust equation of a: smoothN\\(\\) cannot be used for a macro")

})


test_that("delay family is updated with new eqn in build()", {

  sfm = xmile() |> build("abc", "aux", eqn = "delayN(a, 10, 2)") |> build("abc", eqn = "0")
  expect_equal(sfm$model$variables$aux$abc$eqn, "0")
  expect_equal(length(sfm$model$variables$aux$abc$func), 0)

  sfm = xmile() |> build("abc", "aux", eqn = "delayN(a, 10, 2)") |> build("abc", eqn = "past(a, 10)")
  expect_equal(sfm$model$variables$aux$abc$eqn, "past(a, 10)")
  expect_equal(length(sfm$model$variables$aux$abc$func), 1)
  expect_equal(names(sfm$model$variables$aux$abc$func), "past")
  expect_equal(names(sfm$model$variables$aux$abc$func$past), paste0("abc",.sdbuildR_env[["P"]][["past_suffix"]], "1"))

})

