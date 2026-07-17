# Tests for Insight Maker model import functionality

test_that("import_insightmaker() validates input arguments", {
  # No arguments
  expect_error(
    import_insightmaker(),
    class = "rlang_error"
  )

  # Invalid URL
  expect_error(
    import_insightmaker(url = "https://example.com"),
    class = "rlang_error"
  )

  # Non-existent file
  expect_error(
    import_insightmaker(file = "nonexistent.InsightMaker"),
    class = "rlang_error"
  )

  # Wrong file extension
  expect_error(
    import_insightmaker(file = "test.txt"),
    class = "rlang_error"
  )

  # Both URL and file specified
  expect_error(
    import_insightmaker(
      url = "https://insightmaker.com/test",
      file = "test.InsightMaker"
    ),
    class = "rlang_error"
  )
})


test_that("replace_safely() avoids strings, protected names, and partial overlaps", {
  dict <- c("\\bfoo\\b" = "bar")
  expect_equal(
    replace_safely('Foo + food + "foo" + [Foo]', dict, var_names = "Foo", ignore_case = TRUE),
    'Foo + food + "foo" + [Foo]'
  )
  expect_equal(
    replace_safely("foo + food + my_foo", dict, var_names = character(0), ignore_case = TRUE),
    "bar + food + my_foo"
  )
})


test_that("import_metadata structure is created correctly", {
  # Get path to the cran folder with test models
  folder <- test_path("testdata", "insightmaker", "cran")

  # Get a .InsightMaker file
  model_file <- list.files(
    path = folder,
    pattern = "\\.InsightMaker$",
    full.names = TRUE
  )[1]

  sfm <- expect_no_error({
    suppressWarnings({
      import_insightmaker(file = model_file)
    })
  })

  # Check import_metadata exists and has correct structure
  expect_true(!is.null(sfm[["import_metadata"]]))

  im <- sfm[["import_metadata"]]

  # Check required fields
  expect_equal(im$vendor, "insightmaker")
  expect_equal(im$file_path, model_file)
  expect_null(im$url)
  expect_s3_class(im$import_time, "POSIXct")
  expect_true(!is.null(im$raw_model))

  # Check original_variables is a data frame with correct columns
  expect_s3_class(im$original_variables, "data.frame")
  expect_true(all(c(
    "name", "original_id", "original_name",
    "original_eqn"
  ) %in% names(im$original_variables)))
  expect_true(nrow(im$original_variables) > 0)

  # Check vendor_meta is a list
  expect_type(im$vendor_meta, "list")



  ## import_metadata preserves original InsightMaker info
  # Original variables should match current variables in count
  expect_equal(nrow(im$original_variables), nrow(sfm[["variables"]]))

  # Names in original_variables$name should match sfm$variables$name
  expect_equal(
    sort(im$original_variables$name),
    sort(sfm[["variables"]]$name)
  )

  # Original IDs should be non-empty for InsightMaker models
  expect_true(all(!is.na(im$original_variables$original_id)))


  ## import_metadata is NOT in as.data.frame() output
  df <- as.data.frame(sfm)

  # InsightMaker-specific columns should NOT be in data frame output
  expect_false("eqn_insightmaker" %in% names(df))
  expect_false("name_insightmaker" %in% names(df))
  expect_false("id_insightmaker" %in% names(df))
})


test_that("import_metadata raw_model contains the complete original model", {
  folder <- test_path("testdata", "insightmaker", "cran")

  model_file_im <- list.files(
    path = folder,
    pattern = "\\.InsightMaker$",
    full.names = TRUE
  )[1]

  model_file_json <- list.files(
    path = folder,
    pattern = "\\.json$",
    full.names = TRUE
  )[1]

  # Test InsightMaker format
  sfm_im <- expect_no_error({
    suppressWarnings({
      import_insightmaker(file = model_file_im)
    })
  })

  # raw_model should be an xml_document for .InsightMaker files
  expect_s3_class(sfm_im[["import_metadata"]]$raw_model, "xml_document")

  # Test JSON format
  sfm_json <- expect_no_error({
    suppressWarnings({
      import_insightmaker(file = model_file_json)
    })
  })

  # raw_model should be a list for .json files
  expect_type(sfm_json[["import_metadata"]]$raw_model, "list")
})


test_that("translating .InsightMaker models works", {
  keep_nonnegative_flow <- TRUE
  keep_nonnegative_stock <- FALSE # TRUE
  only_stocks <- TRUE
  dt <- .1
  save_by <- 1
  seed <- 123

  folder <- test_path("testdata", "insightmaker", "cran")

  model_files_IM <- list.files(
    path = folder,
    pattern = "\\.InsightMaker$",
    full.names = TRUE
  )

  model_files_json <- list.files(
    path = folder,
    pattern = "\\.json$",
    full.names = TRUE
  )

  expect_equal(length(model_files_json), length(model_files_IM))
  model_indices <- seq_along(model_files_IM)
  if (Sys.getenv("NOT_CRAN") != "true") {
    model_indices <- model_indices[1]
  }

  for (i in model_indices) {
    # print(i)

    sfm_IM <- expect_no_error({
      silence(
        import_insightmaker(
          file = model_files_IM[i],
          keep_nonnegative_flow = keep_nonnegative_flow,
          keep_nonnegative_stock = keep_nonnegative_stock
        )
      )
    })

    df <- expect_no_error(as.data.frame(sfm_IM))
    expect_true(nrow(df) > 0)

    # Check import_metadata exists
    expect_true(!is.null(sfm_IM[["import_metadata"]]))
    expect_equal(sfm_IM[["import_metadata"]]$vendor, "insightmaker")

    expect_silent(plot(sfm_IM))
    expect_silent(s <- summary(sfm_IM))

    contains_stocks <- any(df[["type"]] == "stock")

    if (contains_stocks) {
      sim_IM <- expect_successful_simulation(
        sim_settings(sfm_IM,
          seed = seed, dt = dt, save_by = save_by
        ),
        only_stocks = only_stocks
      )
    }

    # Test JSON version
    sfm_json <- expect_no_error({
      silence(
        import_insightmaker(
          file = model_files_json[i],
          keep_nonnegative_flow = keep_nonnegative_flow,
          keep_nonnegative_stock = keep_nonnegative_stock
        )
      )
    })

    df <- expect_no_error(as.data.frame(sfm_json))
    expect_true(nrow(df) > 0)

    expect_silent(plot(sfm_json))
    expect_silent(s <- summary(sfm_json))

    # Check import_metadata exists
    expect_true(!is.null(sfm_json[["import_metadata"]]))

    if (contains_stocks) {
      sim_json <- expect_successful_simulation(
        sim_settings(sfm_json,
          seed = seed, dt = dt, save_by = save_by
        ),
        only_stocks = only_stocks
      )

      # Compare simulations
      comp <- compare_sim(sim_IM, sim_json)
      expect_true(comp[["equal"]])
    }

    # Compare variable properties **
  }
})


test_that("ABM model issues error", {
  folder <- test_path("testdata", "insightmaker", "abm")
  skip_if_not(dir.exists(folder))

  model_files_IM <- list.files(
    path = folder,
    pattern = "\\.InsightMaker$",
    full.names = TRUE
  )

  model_files_json <- list.files(
    path = folder,
    pattern = "\\.json$",
    full.names = TRUE
  )

  for (file in model_files_IM) {
    expect_error(
      import_insightmaker(file = file),
      "Agent-Based Modelling"
    )
  }

  for (file in model_files_json) {
    expect_error(
      import_insightmaker(file = file),
      "Agent-Based Modelling"
    )
  }
})
