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
  # Names are literal element names; replace_safely() quotes them itself
  dict <- c("foo" = "bar")
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


# Structural edge cases are built inline rather than shipped as files, so the
# repository does not need to carry deliberately degenerate models.
im_setting <- paste0(
  '<Setting Version="38" TimeLength="10" TimeStart="0" TimeStep="0.1"',
  ' TimeUnits="Months" SolutionAlgorithm="RK4" id="2"><mxCell parent="1"/></Setting>'
)
im_stock <- '<Stock name="A" InitialValue="1" id="10"><mxCell parent="1"/></Stock>'
im_variable <- '<Variable name="k" Equation="0.5" id="11"><mxCell parent="1"/></Variable>'

im_header <- function(title = "Minimal") {
  sprintf(
    '<header model_id="1" model_title="%s" model_author_id="2" model_author_name="K"/>',
    title
  )
}

im_write <- function(body) {
  file <- tempfile(fileext = ".InsightMaker")
  writeLines(body, file)
  file
}

im_model <- function(...) {
  im_write(paste0("<insightmakermodel><root>", paste0(...), "</root></insightmakermodel>"))
}


test_that("models without flows or links import", {
  # get_map() used to return NULL for an empty node set, which collapsed the
  # source/target data frame to a single column and then to a bare vector.
  file <- im_model(im_header(), im_setting, im_stock, im_variable)
  expect_s3_class(import_insightmaker(file = file), "stockflow")
})


test_that("a missing or non-numeric Version does not abort the import", {
  no_version <- im_model(
    im_header(),
    sub('Version="38" ', "", im_setting, fixed = TRUE),
    im_stock, im_variable
  )
  expect_s3_class(import_insightmaker(file = no_version), "stockflow")

  bad_version <- im_model(
    im_header(),
    sub('Version="38"', 'Version="v38"', im_setting, fixed = TRUE),
    im_stock, im_variable
  )
  expect_s3_class(import_insightmaker(file = bad_version), "stockflow")
})


test_that("a missing Setting element gives a clear error", {
  file <- im_model(im_header(), im_stock, im_variable)
  expect_error(import_insightmaker(file = file), "Setting")
})


test_that("the model is found when the document root has several children", {
  # xml_name() is vectorised over the node set, so comparing it to "root"
  # directly raised "the condition has length > 1".
  file <- im_write(paste0(
    "<insightmakermodel><root>",
    im_header(), im_setting, im_stock, im_variable,
    "</root><extra/></insightmakermodel>"
  ))
  expect_s3_class(import_insightmaker(file = file), "stockflow")
})


test_that("header meta-data survives separators in the model title", {
  # The header used to be a `key="value", key="value"` string re-split on ","
  # and "=", so a title containing either was silently truncated.
  for (title in c("Romeo &amp; Juliet", "Romeo, Juliet", "a=b", "Plain")) {
    file <- im_model(im_header(title), im_setting, im_stock, im_variable)
    expected <- gsub("&amp;", "&", title, fixed = TRUE)
    expect_equal(import_insightmaker(file = file)[["meta"]][["name"]], expected)
  }
})


test_that("headers written by earlier versions still read", {
  # Older files store the meta-data as text rather than as attributes.
  legacy <- paste0(
    "<header> model_id=\"1\", model_title=\"Romeo, Juliet\",",
    " model_author_id=\"2\", model_author_name=\"Kyra Evers\" </header>"
  )
  file <- im_model(legacy, im_setting, im_stock, im_variable)
  meta <- import_insightmaker(file = file)[["meta"]]

  expect_equal(meta[["name"]], "Romeo, Juliet")
  expect_equal(meta[["author"]], "Kyra Evers")
})


test_that("connectors without a BiDirectional attribute do not inject NA", {
  attrs <- list(list(source = "11", target = "10", id = "12"))
  dict <- get_source_target_IM(attrs, "link", type = "InsightMaker")

  expect_false(anyNA(dict[["sources"]]))
  expect_false(anyNA(dict[["targets"]]))
  expect_equal(dict[["sources"]], "11")
  expect_equal(dict[["targets"]], "10")
})


test_that("get_map() returns an empty character vector, not NULL", {
  expect_identical(get_map(list(), "anything"), character(0))
})


test_that("replace_safely() treats dictionary names as literal text", {
  # Names are model element names, so a regex metacharacter must not widen the
  # match, and an unbalanced bracket must not raise a regex error.
  expect_equal(
    replace_safely("a.b + axb", c("a.b" = "Z"), var_names = character(0)),
    "Z + axb"
  )
  expect_equal(
    replace_safely("rate(1)", c("rate" = "Z"), var_names = character(0)),
    "Z(1)"
  )
})


test_that("numeric fields of a json model keep their value", {
  model <- list(
    name = "Tiny", description = "",
    simulation = list(
      algorithm = "RK4", time_start = 0, time_length = 10,
      time_step = 0.1, time_units = "SECONDS"
    ),
    elements = list(
      list(type = "STOCK", name = "A", behavior = list(initial_value = 1)),
      list(type = "VARIABLE", name = "k", behavior = list(value = "0.5"))
    )
  )

  file <- tempfile(fileext = ".json")
  writeLines(jsonlite::toJSON(model, auto_unbox = TRUE, null = "null"), file)

  object <- import_insightmaker(file = file)
  stock_eqn <- object[["variables"]][object[["variables"]][["name"]] == "A", "eqn"]

  # apply() used to route the row through as.matrix(), turning 1 into " 1.0"
  expect_equal(stock_eqn, "1")
})


test_that("a model imports from a live Insight Maker URL", {
  skip_if_no_internet()

  url <- "https://insightmaker.com/insight/43tz1nvUgbIiIOGSGtzIzj/Romeo-Juliet"
  object <- import_insightmaker(url = url)

  expect_s3_class(object, "stockflow")
  expect_equal(object[["meta"]][["name"]], "Romeo & Juliet")
  expect_gt(nrow(object[["variables"]]), 0)
})
