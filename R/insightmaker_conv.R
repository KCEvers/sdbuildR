#' Extract Insight Maker model from URL
#'
#' Create XML string from Insight Maker URL. For internal use; use `import_insightmaker()` to import an Insight Maker model.
#'
#' @param url String with URL to an Insight Maker model
#' @param file If specified, file path to save Insight Maker model to. If NULL, do not save model.
#'
#' @returns XML string with Insight Maker model
#' @seealso [import_insightmaker()]
#' @export
#' @concept importExport
#' @examplesIf has_internet()
#' url <- "https://insightmaker.com/insight/43tz1nvUgbIiIOGSGtzIzj/Romeo-Juliet"
#' xml <- url_to_insightmaker(url)
#'
#' # Save model to file
#' file <- tempfile(fileext = ".InsightMaker")
#' xml <- url_to_insightmaker(url, file = file)
#' file.remove(file)
url_to_insightmaker <- function(url, file = NULL) {

  .check_insightmaker_url(url)

  if (!is.null(file)) {
    .check_insightmaker_file(file, must_exist = FALSE)
  }

  tryCatch(
    {
      .url_to_insightmaker(url, file)
    },
    error = function(e) {
      cli::cli_abort(c(
        "x" = "Failed to extract Insight Maker model from URL.",
        "i" = "Original error: {conditionMessage(e)}.",
        ">" = "Ensure the URL is correct and points to a public Insight Maker model."
      ))
    }
  )
  
}


.url_to_insightmaker <- function(url, file) {

  # Read URL
  url_data <- xml2::read_html(url)

  # Get link from page
  iframe_src <- url_data |>
    xml2::xml_find_first(".//iframe") |>
    xml2::xml_attr("src")

  # Create absolute link out of relative link
  full_iframe_url <- xml2::url_absolute(iframe_src, url)
  iframe_page <- xml2::read_html(full_iframe_url)

  # Get elements with <script tag
  script_texts <- xml2::xml_find_all(iframe_page, ".//script")

  # Keep script with certain keywords
  script_texts_xml <- xml2::xml_text(script_texts, trim = TRUE)
  script_model <- as.character(
    script_texts[grepl("model_id", script_texts_xml) &
      grepl("model_title", script_texts_xml)]
  )

  # Extract part of interest; the model is embedded as a JSON object in the script
  json_line <- trimws(grep("initialModel", unlist(strsplit(script_model, "\n")),
    fixed = TRUE, value = TRUE
  ))

  if (length(json_line) == 0) {
    stop("Could not find model in the script tags of the html page")
  }

  # Parsing the JSON unescapes the model XML
  model <- jsonlite::fromJSON(
    sub(";$", "", substring(json_line[1], regexpr("{", json_line[1], fixed = TRUE)))
  )

  xml_str <- gsub("mxGraphModel", "insightmakermodel", model[["model_xml"]], fixed = TRUE)

  # Extract meta-data - this is embedded in the webpage, but not saved in the .InsightMaker file. Add to .InsightMaker file to preserve the original author of the model.
  meta_names <- c("model_id", "model_title", "model_author_id", "model_author_name")
  meta_info <- model[meta_names]
  names(meta_info) <- meta_names
  meta_info <- lapply(compact_(meta_info), as.character)

  # Insert meta as attributes of a <header> node. Storing them as attributes
  # rather than as delimited text lets the XML writer escape whatever the model
  # happens to contain, so a value with a comma or a quote cannot break parsing.
  doc <- xml2::read_xml(xml_str)
  root_node <- xml2::xml_find_first(doc, "root")

  if (inherits(root_node, "xml_missing")) {
    stop("the extracted model has no <root> element")
  }

  do.call(
    xml2::xml_add_child,
    c(list(root_node, "header", .where = 0), meta_info)
  )

  # Save and read .InsightMaker file to ensure it is the correct format
  if (is.null(file)) {
    delete_after <- TRUE
    file <- tempfile(fileext = ".InsightMaker")
  } else {
    delete_after <- FALSE
  }

  # If no file path was specified before, delete file
  if (delete_after) {
    on.exit(remove_files(file), add = TRUE)
  }

  # Write model to file and read back in to ensure it is valid
  xml2::write_xml(doc, file)
  xml2::read_xml(file)

}


#' Read .InsightMaker or .json file
#'
#' @param file File path
#' @param fileext Allowed file extensions
#'
#' @returns Parsed file
#' @noRd
read_IM_file <- function(file, fileext) {

  # Check if file exists
  if (!file.exists(file)) {
    cli::cli_abort(c(
      "x" = "File not found.",
      "i" = "The specified {.arg file} does not exist: {.file {file}}.",
      ">" = "Check the file path."
    ))
  }

  # Get file extension
  ext <- tools::file_ext(file)

  if (!ext %in% fileext) {
    expected_exts <- paste0(paste0(".", fileext), collapse = " or ")
    download_instructions <- paste0(
      ifelse("InsightMaker" %in% fileext, "'Download Insight Maker File'", ""),
      ifelse(length(fileext) > 1, " or ", ""),
      ifelse("json" %in% fileext, "'ModelJSON File'", "")
    )
    cli::cli_abort(c(
      "Invalid file extension.",
      "x" = "The {.arg file} does not have the required extension {.code {expected_exts}}.",
      "i" = "Download an {.pkg InsightMaker} model from the share button (top right).",
      ">" = "Go to 'Import/Export', click the down arrow, and select {download_instructions}."
    ))
  }

  # Read file
  read_file <- tryCatch(
    {
      if (ext == "InsightMaker") {
        xml2::read_xml(file)
      } else if (ext == "json") {
        jsonlite::fromJSON(file, simplifyVector = TRUE, flatten = TRUE)
      }
    },
    error = function(e) {
      cli::cli_abort(c(
        "x" = "Failed to parse file.",
        "i" = "Could not parse the file: {.file {file}}.",
        "i" = "Original error: {conditionMessage(e)}.",
        ">" = "Ensure the file is a valid {.pkg InsightMaker} or {.code .json} file."
      ))
    }
  )

  read_file
}


.check_insightmaker_url <- function(url) {

  is_valid_url <- stringr::str_detect(
    url,
    stringr::regex("http[s]?\\:\\/\\/[www\\.]?insightmaker")
  )

  if (!is_valid_url) {
    cli::cli_abort(c(
      "Invalid {.pkg InsightMaker} URL.",
      "x" = "The {.arg url} is not a valid {.pkg InsightMaker} model URL.",
      "i" = "URLs must start with {.code http://insightmaker} or {.code https://insightmaker}.",
      ">" = "Provide a valid {.pkg InsightMaker} URL."
    ))
  }

  invisible(TRUE)
}


.check_insightmaker_file <- function(file, must_exist = TRUE) {

  # Check if file exists
  if (must_exist && !file.exists(file)) {
    cli::cli_abort(c(
      "x" = "File not found.",
      "i" = "The specified {.arg file} does not exist: {.file {file}}.",
      ">" = "Check the file path."
    ))
  }

  if (!grepl("\\.InsightMaker$", file, ignore.case = TRUE)) {
    cli::cli_abort(c(
      "Invalid file extension.",
      "x" = "The {.arg file} must have a {.code .InsightMaker} extension.",
      ">" = "Provide a valid file path with the correct extension."
    ))
  }

  invisible(TRUE)
}


get_insightmaker_model <- function(url, file, fileext = c("InsightMaker", "json")) {
  # Validate inputs
  url_spec <- !missing(url) && !is.null(url) && !is.na(url)
  file_spec <- !missing(file) && !is.null(file) && !is.na(file)

  if (!url_spec && !file_spec) {
    cli::cli_abort(c(
      "Missing required arguments.",
      "x" = "Either {.arg url} or {.arg file} must be specified.",
      ">" = "Provide one of these arguments to import a model."
    ))
  }

  if (url_spec && file_spec) {
    cli::cli_abort(c(
      "Too many arguments specified.",
      "x" = "Both {.arg url} and {.arg file} were specified.",
      "i" = "Only one input method can be used at a time.",
      ">" = "Specify either {.arg url} or {.arg file}, not both."
    ))
  }

  # Load XML file
  if (url_spec) {
    file <- NULL

    .check_insightmaker_url(url)

    ext <- "InsightMaker"
    tryCatch(
      {
        read_file <- url_to_insightmaker(url, file)
      },
      error = function(e) {
        cli::cli_abort(c(
          "x" = "Could not download {.pkg InsightMaker} model from the URL.",
          "i" = "Original error: {conditionMessage(e)}.",
          ">" = "Check your internet connection and ensure the URL is accessible."
        ))
      }
    )
  } else {

    # Read file
    ext <- tools::file_ext(file)
    read_file <- read_IM_file(file, fileext = fileext)
  }

  list(read_file = read_file, ext = ext)
}


#' Create import metadata structure
#'
#' Creates a standardized metadata structure for imported models. This structure
#' preserves the original model information and is immutable after import.
#'
#' @param vendor Character. The source vendor (e.g., "insightmaker", "stella", "vensim").
#' @param file_path Character or NULL. The file path used for import.
#' @param url Character or NULL. The URL used for import.
#' @param raw_model The complete original model (parsed XML/JSON) before any transformations.
#' @param vendor_meta List. Vendor-specific meta information.
#' @param original_variables Data frame with columns: name (current), original_id,
#'   original_name, original_eqn.
#' @param original_macros Data frame with columns: name (current), original_name,
#'   original_eqn. Can be NULL if no macros.
#'
#' @returns A list structure containing all import metadata.
#' @noRd
create_import_metadata <- function(vendor,
                                   file_path = NULL,
                                   url = NULL,
                                   raw_model = NULL,
                                   vendor_meta = list(),
                                   original_variables = NULL,
                                   original_macros = NULL) {
  # Create empty data frames if not provided
  if (is.null(original_variables)) {
    original_variables <- data.frame(
      name = character(0),
      original_id = character(0),
      original_name = character(0),
      original_eqn = character(0),
      stringsAsFactors = FALSE
    )
  }

  if (is.null(original_macros)) {
    original_macros <- data.frame(
      name = character(0),
      original_name = character(0),
      original_eqn = character(0),
      stringsAsFactors = FALSE
    )
  }

  list(
    vendor = vendor,
    file_path = file_path,
    url = url,
    import_time = Sys.time(),
    raw_model = raw_model,
    vendor_meta = vendor_meta,
    original_variables = original_variables,
    original_macros = original_macros
  )
}


validate_json_path <- function(path, add_fileext = TRUE) {
  # Check it's a character string
  if (!is.character(path) || length(path) != 1) {
    cli::cli_abort(c(
      "Invalid {.arg path} argument.",
      "x" = "The {.arg path} argument must be a single {.cls character} string.",
      ">" = "Provide the path as a single string."
    ))
  }

  # Check extension
  ext <- tools::file_ext(path)
  if (ext == "" && add_fileext) {
    path <- paste0(path, ".json")
  } else if (ext != "json") {
    cli::cli_abort(c(
      "Invalid file extension.",
      "x" = "The {.arg path} must have a {.code .json} extension.",
      ">" = "Specify a path ending with {.code .json}."
    ))
  }

  # Check directory exists (if creating file)
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) {
    cli::cli_abort(c(
      "Directory not found.",
      "x" = "The directory does not exist: {.file {dir_path}}.",
      ">" = "Create the directory or specify a valid path."
    ))
  }

  # # Optional: check if file already exists
  # if (file.exists(path)) {
  #   warning("File already exists and will be overwritten: ", path)
  # }

  return(path)
}


#' Convert .InsightMaker file to .json file
#'
#' Import and convert a stock-and-flow model from [Insight Maker](https://insightmaker.com/) to a .json file. Models may be your own or another user's. Importing causal loop diagrams or agent-based models is not supported.
#'
#' Insight Maker models can be imported using a URL or Insight Maker file. Ensure the URL refers to a public (not private) model. To download a model file from Insight Maker, first clone the model if it is not your own. Then, go to "Share" (top right), "Export", and "Download Insight Maker file".
#'
#' @param url URL to Insight Maker model. Character.
#' @param file File path to Insight Maker model. Only used if url is not specified. Needs to be a character with suffix .InsightMaker.
#' @param destfile Output file path. Must have extension .json or no extension. Overwrites file if it already exists. If not provided, return model in json format.
#'
#' @returns If destfile is not provided; object of class "json". If destfile provided, invisibly returns destfile (character string).
#' @export
#' @concept importExport
#'
#' @examplesIf has_internet() && Sys.getenv("NOT_CRAN") == "true"
#' # Convert a model from Insight Maker to json
#' destfile <- tempfile(fileext = ".json")
#' json <- insightmaker_to_json(
#'   url =
#'     "https://insightmaker.com/insight/43tz1nvUgbIiIOGSGtzIzj/Romeo-Juliet",
#'   destfile = destfile
#' )
#' file.remove(destfile)
#'
insightmaker_to_json <- function(url, file, destfile = NULL) {
  if (!is.null(destfile)) {
    destfile <- validate_json_path(destfile)
    save_file <- TRUE
  } else {
    save_file <- FALSE
  }

  # Read .InsightMaker file
  out <- get_insightmaker_model(url, file, fileext = "InsightMaker")
  read_file <- out[["read_file"]]

  # Prepare .InsightMaker file into more common intermediate format
  out <- prep_IM(read_file)

  # Unpack
  settings <- out[["settings"]]
  meta <- out[["meta"]]
  model_elements <- out[["model_elements"]]
  macros <- out[["macros"]]


  # Find old and new names for replacement
  # old_names <- unname(get_map(model_elements, "name_insightmaker"))
  new_names <- unname(get_map(model_elements, "name"))

  json_elements <- lapply(model_elements, function(x) {
    eqn_name <- switch(x[["type"]],
      "stock" = "initial_value",
      "flow" = "value",
      "variable" = "value",
      "none"
    )

    if (eqn_name != "none") {
      eqn <- x[["eqn_insightmaker"]]

      x[["behavior"]] <- list()
      x[["behavior"]][[eqn_name]] <- eqn
      x[["behavior"]][["description"]] <- x[["doc"]]


      if (x[["type"]] %in% c("stock", "flow") && is_defined(x[["non_negative"]])) {
        x[["behavior"]][["non_negative"]] <- x[["non_negative"]]
      }
    }

    if (x[["type"]] == "flow") {
      # from and to should be NA (i.e., null in JSON) if empty
      if (!is_defined(x[["from"]])) {
        x[["from"]] <- NA
      }
      if (!is_defined(x[["to"]])) {
        x[["to"]] <- NA
      }
    }

    if (x[["type"]] == "converter") {
      if (is_defined(x[["source"]])) {
        if (x[["source"]] == P[["time_name"]]) {
          x[["behavior"]][["input"]] <- "TIME"
        } else {
          x[["behavior"]][["input"]] <- "ELEMENT"
          x[["behavior"]][["input_element"]] <- x[["source"]]
        }
      }

      x[["behavior"]][["interpolation"]] <- toupper(x[["interpolation"]])

      if (is_defined(x[["xpts"]]) && is_defined(x[["ypts"]])) {
        df <- data.frame(x = x[["xpts"]], y = x[["ypts"]])
        x[["behavior"]][["data"]] <- asplit(as.matrix(df), 1) # Split by row
      }
    }

    x[["type"]] <- toupper(x[["type"]])

    x <- x[names(x) %in% c("type", "name", "behavior", "from", "to")]

    return(x)
  })
  names(json_elements) <- NULL

  # Find dependencies
  deps <- lapply(json_elements, function(x) {
    d <- c()

    eqn_name <- switch(tolower(x[["type"]]),
      "stock" = "initial_value",
      "flow" = "value",
      "variable" = "value",
      "none"
    )

    if (eqn_name != "none") {
      eqn <- x[["behavior"]][[eqn_name]]

      idx_df <- get_range_names(eqn,
        var_names = new_names,
        names_with_brackets = TRUE
      )
      d <- c(d, idx_df[["name"]])
    }

    if (is_defined(x[["from"]])) {
      d <- c(d, x[["from"]])
    }
    if (is_defined(x[["to"]])) {
      d <- c(d, x[["to"]])
    }
    if (is_defined(x[["input_element"]])) {
      d <- c(d, x[["input_element"]])
    }

    return(d)
  })
  names(deps) <- new_names
  deps <- compact_(deps)

  # Create links list from dependencies
  if (length(deps) > 0) {
    l <- lapply(deps, length)
    to_vec <- rep(names(deps), l)
    from_vec <- unlist(unname(deps))
    n <- length(to_vec)
    links <- lapply(seq_len(n), function(i) {
      list(
        type = "LINK",
        from = from_vec[i],
        to = to_vec[i]
      )
    })
  } else {
    links <- list()
  }

  json_elements <- append(json_elements, links)

  # Compile json file
  json <- list()
  json[["engine"]] <- "SIMULATION_PACKAGE"
  json[["name"]] <- meta[["name"]]
  json[["description"]] <- meta[["caption"]]

  if (!is.null(settings)) {
    # Prepare settings with canonical names for ModelJSON
    replacements <- c(
      "algorithm" = "method", "time_start" = "start",
      "time_units" = "time_units",
      "version" = "version",
      "time_length" = "length", "time_step" = "dt"
    )
    settings <- settings[names(settings) %in% unname(replacements)]

    if (length(settings) > 0) {
      replacements <- stats::setNames(names(replacements), unname(replacements))
      names(settings) <- ifelse(names(settings) %in% names(replacements),
        replacements[names(settings)], names(settings)
      )

      if (is_defined(settings[["algorithm"]])) {
        settings[["algorithm"]] <- toupper(settings[["algorithm"]])
      }
      # if (is_defined(settings[["time_units"]])) {
      #   settings[["time_units"]] <- toupper(settings[["time_units"]])
      # }

      json[["simulation"]] <- as.list(settings)
    }
  }
  json[["elements"]] <- json_elements

  json[["engine_settings"]] <- list()

  # Macros
  if (is_defined(macros)) {
    json[["engine_settings"]][["globals"]] <- macros
  }


  if (save_file) {
    jsonlite::write_json(json, destfile,
      pretty = TRUE,
      auto_unbox = TRUE # Necessary to keep booleans JSON valid
    )
    return(invisible(destfile))
  } else {
    json_formatted <- jsonlite::toJSON(json,
      pretty = TRUE,
      auto_unbox = TRUE # Necessary to keep booleans JSON valid)
    )
    return(json_formatted)
  }
}


#' Convert XML nodes to list
#'
#' @param read_file XML structured model
#'
#' @returns List
#' @noRd
#'
prep_IM <- function(read_file) {
  type <- "InsightMaker"

  # Get the children nodes
  children <- xml2::xml_children(read_file)

  # Descend into <root> if present. xml_name() is vectorised over the node set,
  # so this has to select the node rather than compare the whole vector.
  root_idx <- which(xml2::xml_name(children) == "root")
  if (length(root_idx) > 0) {
    children <- xml2::xml_children(children[[root_idx[1]]]) # Remove nested layer
  }

  # Get attributes, also of children
  tags <- c("Setting", "Variable", "Converter", "Stock", "Flow", "Link", "Ghost")
  node_types <- xml2::xml_name(children)

  # Locate the <header> node holding the meta-data added by url_to_insightmaker()
  header_idx <- match("header", node_types)
  header_node <- if (is.na(header_idx)) NULL else children[[header_idx]]

  # Get attributes as well as deeper attributes
  children_attrs <- lapply(children, function(x) {
    c(unlist(xml2::xml_attrs(x)), unlist(xml2::xml_attrs(xml2::xml_children(x))))
  })

  # Check whether the model has any components
  is_sfm_IM(node_types, get_map(lapply(children_attrs, as.list), "name"))

  # Only keep nodes that are in tags
  children_attrs <- lapply(children_attrs[node_types %in% tags], as.list)
  node_types <- node_types[node_types %in% tags]

  # Turn nodes and entry names to lowercase
  node_types <- tolower(node_types)
  children_attrs <- lapply(children_attrs, function(x) {
    names(x) <- tolower(names(x))
    return(x)
  })


  # Get first setting (multiple exist sometimes)
  setting_idx <- match("setting", node_types)
  if (is.na(setting_idx)) {
    cli::cli_abort(c(
      "Missing simulation settings.",
      "x" = "The model contains no {.code <Setting>} element.",
      "i" = "{.pkg InsightMaker} stores the solver and time settings there.",
      ">" = "Re-export the model from {.pkg InsightMaker}."
    ))
  }
  settings <- children_attrs[[setting_idx]]
  macros <- settings[["macros"]]

  # Prepare settings with canonical names
  settings <- prep_settings_IM(settings, type = type)
  meta <- prep_meta_IM(header_node, settings, type = type)

  # Find source-target dictionary for changing names
  source_target_dict <- get_source_target_IM(children_attrs, node_types, type = type)

  # Prepare model elements
  model_elements <- prep_model_elements_IM(children_attrs, node_types, type = type)

  # Change links to access_ids; change names
  model_elements <- change_names_IM(model_elements, source_target_dict, type = type)

  return(
    list(
      settings = settings,
      meta = meta,
      model_elements = model_elements,
      macros = macros
    )
  )
}


#' Source-specific field mapping for Insight Maker imports
#'
#' @param type Source type, either `.InsightMaker` XML or ModelJSON.
#'
#' @returns List of source-specific field names and policies.
#' @noRd
im_source_adapter <- function(type = c("InsightMaker", "json")) {
  type <- match.arg(type)

  if (type == "InsightMaker") {
    list(
      type = type,
      settings_replacements = c(
        "solutionalgorithm" = "method", "timestart" = "start",
        "timeunits" = "time_units",
        "timelength" = "length", "timestep" = "dt"
      ),
      eqn_fields = list(
        variable = "equation",
        stock = "initialvalue",
        flow = "flowrate"
      ),
      doc_field = "note",
      connector_source_field = "source",
      connector_target_field = "target",
      flow_connector_id_field = "id",
      converter_source_field = "source",
      use_id_for_names = TRUE,
      meta_from_header = TRUE
    )
  } else {
    list(
      type = type,
      settings_replacements = c(
        "algorithm" = "method", "time_start" = "start",
        "time_length" = "length", "time_step" = "dt"
      ),
      eqn_fields = list(
        variable = "value",
        stock = "initial_value",
        flow = "value"
      ),
      doc_field = "description",
      connector_source_field = "from",
      connector_target_field = "to",
      flow_connector_id_field = "name",
      converter_source_field = "input_element",
      use_id_for_names = FALSE,
      meta_from_header = FALSE
    )
  }
}


#' Equation field for a source element type
#'
#' @param adapter Source adapter from im_source_adapter().
#' @param element_type Element type.
#'
#' @returns Source field name or "none".
#' @noRd
im_eqn_field <- function(adapter, element_type) {
  adapter[["eqn_fields"]][[element_type]] %||% "none"
}


#' Read the meta-data header of an .InsightMaker file
#'
#' The header is written by [url_to_insightmaker()] to preserve the original
#' author, which Insight Maker embeds in the webpage but not in the model file.
#'
#' @param header_node `<header>` node, or NULL if the file has none.
#'
#' @returns Named list of meta-data; empty list if there is no header.
#' @noRd
read_header_IM <- function(header_node) {
  if (is.null(header_node)) {
    return(list())
  }

  # Meta-data is stored as XML attributes, so that the parser takes care of
  # quoting and escaping and no value can break the format.
  attrs <- xml2::xml_attrs(header_node)
  if (length(attrs) > 0) {
    return(as.list(attrs))
  }

  # Fall back to the `key="value", key="value"` text used by earlier versions of
  # sdbuildR. Matching quoted values rather than splitting on the separators
  # keeps values containing a comma or an "=" intact.
  meta_str <- xml2::xml_text(header_node, trim = TRUE)
  if (length(meta_str) != 1 || !nzchar(meta_str)) {
    return(list())
  }

  pairs <- regmatches(
    meta_str,
    gregexpr('[^=,[:space:]]+="[^"]*"', meta_str)
  )[[1]]

  if (length(pairs) == 0) {
    return(list())
  }

  keys <- sub('=".*$', "", pairs)
  values <- sub('"$', "", sub('^[^=]*="', "", pairs))

  stats::setNames(as.list(values), keys)
}


#' Numeric value of a single setting
#'
#' @param settings List of settings.
#' @param key Name of the setting.
#'
#' @returns Numeric of length 1, or NA_real_ if absent or not parseable.
#' @noRd
setting_num_IM <- function(settings, key) {
  value <- settings[[key]]

  if (length(value) != 1) {
    return(NA_real_)
  }

  suppressWarnings(as.numeric(value))
}


prep_meta_IM <- function(header_node, settings, name = NULL, caption = NULL,
                         type = c("InsightMaker", "json")) {
  adapter <- im_source_adapter(type)

  if (adapter[["meta_from_header"]]) {
    meta <- read_header_IM(header_node)

    if (length(meta) > 0) {
      # Rename elements in meta
      new_names <- names(meta)
      new_names[new_names == "model_author_name"] <- "author"
      new_names[new_names == "model_author_id"] <- "insightmaker_author_id"
      new_names[new_names == "model_title"] <- "name"
      new_names[new_names == "model_id"] <- "insightmaker_model_id"
      names(meta) <- new_names
    }


    # Add version to meta
    meta[["insightmaker_version"]] <- settings[["version"]]
    meta[["insightmaker_method"]] <- settings[["method"]]
  } else {
    meta <- list()
    meta[["name"]] <- name
    meta[["caption"]] <- caption
  }

  meta <- compact_(meta)

  return(meta)
}


prep_settings_IM <- function(settings, type = c("InsightMaker", "json")) {
  adapter <- im_source_adapter(type)

  if (!is.null(settings)) {
    replacements <- adapter[["settings_replacements"]]
    names(settings) <- ifelse(names(settings) %in% names(replacements),
      replacements[names(settings)], names(settings)
    )
  }

  if (adapter[["type"]] == "InsightMaker") {
    # Version may be absent or non-numeric; skip both checks rather than error
    version <- setting_num_IM(settings, "version")

    # Check whether the model uses an early version of Insight Maker
    if (!is.na(version) && version < 37) {
      cli::cli_warn(c(
        "Old {.pkg InsightMaker} version detected.",
        "i" = "This model uses version {.val {settings[[\"version\"]]}} where links were bi-directional by default.",
        "!" = "This may cause issues in translating the model.",
        ">" = "Clone the model in {.pkg InsightMaker} and provide the URL to the updated version."
      ))
    }


    # Check whether model is later version of Insight Maker than the package was made for
    if (!is.na(version) && version > P[["insightmaker_version"]]) {
      cli::cli_warn(c(
        "Newer {.pkg InsightMaker} version detected.",
        "i" = "This model uses version {.val {settings[[\"version\"]]}}, but {.pkg sdbuildR} was based on version {.val {P[[\"insightmaker_version\"]]}}.",
        "!" = "Some features may not be available or may behave differently."
      ))
    }
  }

  return(settings)
}


prep_model_elements_IM <- function(children_attrs, node_types, type = c("InsightMaker", "json")) {
  adapter <- im_source_adapter(type)

  keep_idx <- node_types %in% c("variable", "converter", "stock", "flow")
  model_elements <- children_attrs[keep_idx]
  model_element_types <- node_types[keep_idx]

  model_elements <- lapply(seq_along(model_elements), function(y) {
    x <- model_elements[[y]]

    x[["type"]] <- model_element_types[y]

    eqn_name <- im_eqn_field(adapter, x[["type"]])


    if (eqn_name != "none") {
      x[["eqn_insightmaker"]] <- gsub("\\n", "\n", trimws(x[[eqn_name]]), fixed = TRUE)

      # Default is zero
      if (!is_defined(x[["eqn_insightmaker"]])) {
        x[["eqn_insightmaker"]] <- "0"
      }

      # Remove the element # don't to make IM_to_json() work
      # x <- x[names(x) != eqn_name]
      # x[eqn_name] <- NULL
    } else if (x[["type"]] %in% c("variable", "stock", "flow")) {
      # The default value of a stock/flow/variable is 0 - add in case left unspecified
      x[["eqn_insightmaker"]] <- "0"
    }


    # Rename note to doc
    doc_name <- adapter[["doc_field"]]
    if (doc_name %in% names(x)) {
      x[["doc"]] <- clean_doc(x[[doc_name]])
      x[[doc_name]] <- NULL
    } else {
      x[["doc"]] <- ""
    }

    # Rename constraints
    if (adapter[["type"]] == "InsightMaker") {
      # Constraints are no longer supported
      # if ("minconstraint" %in% names(x)) {
      #   x[["min"]] <- ifelse(x[["minconstraintused"]] == "true", x[["minconstraint"]], "")
      #   x[["max"]] <- ifelse(x[["maxConstraintused"]] == "true", x[["maxconstraint"]], "")
      #   x[c("minconstraint", "minconstraintused", "maxconstraint", "maxconstraintused")] <- NULL
      # }

      if (x[["type"]] == "flow") {
        if (!is_defined(x[["source"]])) {
          x[["from"]] <- ""
        } else {
          x[["from"]] <- x[["source"]]
          x[["source"]] <- NULL
        }
        if (!is_defined(x[["target"]])) {
          x[["to"]] <- ""
        } else {
          x[["to"]] <- x[["target"]]
          x[["target"]] <- NULL
        }
      }

      # Older Insight Maker version uses "onlypositive" instead of "non_negative";
      # Newer Insight Maker version uses "allownegatives" instead of "non_negative"
      x[["non_negative"]] <- FALSE
      if (is_defined(x[["onlypositive"]])) {
        # Insight Maker both uses "true" and "false", as well as "-1" -> FALSE
        x[["non_negative"]] <- as.logical(x[["onlypositive"]])
      }

      if (is_defined(x[["nonnegative"]])) {
        # Insight Maker both uses "true" and "false", as well as "-1" -> FALSE
        x[["non_negative"]] <- as.logical(x[["nonnegative"]])
      }

      # Newer Insight Maker version uses "allownegatives" instead of "non_negative"
      if (is_defined(x[["allownegatives"]])) {
        x[["non_negative"]] <- !as.logical(x[["allownegatives"]])
      }

      if (is.na(x[["non_negative"]])) {
        x[["non_negative"]] <- FALSE
      }

      # Get x and y data for interpolation function
      if (is_defined(x[["data"]])) {
        data_split <- strsplit(x[["data"]], ";")[[1]] |>
          strsplit(",") |>
          do.call(rbind, args = _)
        x[["xpts"]] <- as.numeric(trimws(data_split[, 1]))
        x[["ypts"]] <- as.numeric(trimws(data_split[, 2]))
        x[["data"]] <- NULL
      }

      if (x[["type"]] == "converter" && is_defined(x[["source"]])) {
        if (tolower(x[["source"]]) == "time") {
          x[["source"]] <- P[["time_name"]]
        }
      }
    } else {
      if (!is_defined(x[["non_negative"]])) {
        x[["non_negative"]] <- FALSE
      } else {
        x[["non_negative"]] <- as.logical(x[["non_negative"]])
        if (is.na(x[["non_negative"]])) {
          x[["non_negative"]] <- FALSE
        }
      }

      if (x[["type"]] == "flow") {
        if (!is_defined(x[["from"]])) {
          x[["from"]] <- ""
        }
        if (!is_defined(x[["to"]])) {
          x[["to"]] <- ""
        }
      }

      # Graphical source
      if (is_defined(x[["input"]])) {
        # x[["input"]] contains NA, "TIME", or "ELEMENT"; in case of the latter,
        # x[["input_element"]] then specifies which element

        if (tolower(x[["input"]]) == "element") {
          x[["input"]] <- x[["input_element"]]
        } else if (tolower(x[["input"]]) == "time") {
          x[["input"]] <- P[["time_name"]]
        }
        x[["source"]] <- x[["input"]]
      }


      # Get x and y data for interpolation function
      if (is_defined(x[["data"]])) {
        x[["xpts"]] <- as.numeric(x[["data"]][, 1])
        x[["ypts"]] <- as.numeric(x[["data"]][, 2])
        x[["data"]] <- NULL
      }
    }


    x[["id_insightmaker"]] <- x[["id"]]

    return(x)
  })

  return(model_elements)
}


#' Convert json structure to list
#'
#' @param read_file json structured model
#'
#' @returns List
#' @noRd
#'
prep_json <- function(read_file) {
  # Read JSON file; get components
  model_df <- read_file[["elements"]]
  settings <- read_file[["simulation"]]
  meta <- prep_meta_IM(
    name = read_file[["name"]],
    caption = read_file[["description"]], type = "json"
  )
  macros <- read_file[["engine_settings"]][["globals"]]

  if (!is_defined(macros)) {
    macros <- ""
  }


  settings <- prep_settings_IM(settings, type = "json")

  # Check if file is stock-and-flow model (and not e.g., agent-based)
  is_sfm_IM(model_df[["type"]], model_df[["name"]])

  # Only keep specific types and relevant columns
  model_df[["type"]] <- tolower(model_df[["type"]])

  model_df <- model_df[
    model_df[["type"]] %in% c("stock", "flow", "variable", "converter", "link"),
    !grepl("^display", colnames(model_df))
  ]
  colnames(model_df) <- gsub("^behavior.", "", colnames(model_df))

  # Convert each row to a named list element. apply() would route the whole data
  # frame through as.matrix(), turning every atomic field into a format()ted
  # string (1 becomes " 1.0"), so go row by row instead. List columns such as
  # the lookup data of a converter are unwrapped to the value they hold.
  model_elements <- lapply(seq_len(nrow(model_df)), function(i) {
    row <- as.list(model_df[i, , drop = FALSE])
    lapply(row, function(value) {
      if (is.list(value) && length(value) == 1) value[[1]] else value
    })
  })

  # Assign id
  ids <- as.character(seq_along(model_elements))
  model_elements <- Map(function(x, id) {
    x[["id"]] <- id
    return(x)
  }, model_elements, ids)

  # Find source and target dictionary for changing names
  source_target_dict <- get_source_target_IM(
    children_attrs = model_elements,
    node_types = model_df[["type"]],
    type = "json"
  )

  model_elements <- prep_model_elements_IM(
    children_attrs = model_elements,
    node_types = model_df[["type"]],
    type = "json"
  )


  # Change links to access_ids; change names
  model_elements <- change_names_IM(model_elements, source_target_dict,
    type = "json"
  )


  return(
    list(
      settings = settings,
      meta = meta,
      model_elements = model_elements,
      macros = macros
    )
  )
}


#' Add parsed Insight Maker metadata to import context
#'
#' @param ctx Import context.
#' @param settings Parsed simulation settings.
#' @param meta Parsed model metadata.
#' @param macros Raw macro text.
#'
#' @returns Updated import context.
#' @noRd
ctx_add_parsed_IM_info <- function(ctx, settings, meta, macros) {
  ctx$settings <- settings
  ctx$meta <- meta
  ctx$macros_raw <- macros %||% ""

  if (!is.null(settings)) {
    ctx$object <- sim_settings_IM(ctx$object,
      method = settings[["method"]],
      time_units = settings[["time_units"]],
      start = settings[["start"]],
      length = settings[["length"]],
      dt = settings[["dt"]]
    )
  }

  if (!is.null(meta)) {
    ctx <- ctx_add_meta(ctx, meta)

    vendor_fields <- c(
      "insightmaker_version", "insightmaker_method",
      "insightmaker_author_id", "insightmaker_model_id"
    )
    for (field in vendor_fields) {
      if (field %in% names(meta)) {
        ctx$vendor_meta[[field]] <- meta[[field]]
      }
    }
  }

  ctx
}


#' Remove comments from imported model element equations
#'
#' @param model_elements List of normalized source model elements.
#'
#' @returns Updated model element list.
#' @noRd
prep_model_element_equations_IM <- function(model_elements) {
  lapply(model_elements, function(x) {
    if (is_defined(x[["eqn_insightmaker"]])) {
      out <- prep_eqn_IM(x[["eqn_insightmaker"]])
      x[["eqn_insightmaker"]] <- out[["eqn"]]
      x[["doc"]] <- paste0(x[["doc"]], out[["doc"]])
    }
    x
  })
}


#' Convert source element types to sdbuildR import types
#'
#' @param model_elements List of normalized source model elements.
#' @param model_element_types Original source element types.
#'
#' @returns Updated model element list.
#' @noRd
normalize_model_element_types_IM <- function(model_elements, model_element_types) {
  idx <- model_element_types == "converter"
  model_elements[idx] <- lapply(model_elements[idx], function(x) {
    x[["type"]] <- "lookup"

    x[["interpolation"]] <- tolower(x[["interpolation"]])
    if (x[["interpolation"]] == "none") {
      x[["interpolation"]] <- "constant"
    }
    x[["extrapolation"]] <- "nearest"
    x[["eqn_insightmaker"]] <- NULL

    x
  })

  idx <- model_element_types == "variable"
  model_elements[idx] <- lapply(model_elements[idx], function(x) {
    x[["type"]] <- "aux"
    x
  })

  idx <- model_element_types == "stock"
  any_conveyors <- any(vapply(model_elements[idx], function(x) {
    is_defined(x[["stockmode"]]) && tolower(x[["stockmode"]]) == "conveyor"
  }, logical(1)))
  if (any_conveyors) {
    cli::cli_inform(c(
      "!" = "Conveyor stocks detected.",
      "i" = "sdbuildR does not support conveyors; they will be treated as regular stocks."
    ))
  }

  model_elements
}


#' Keep only import properties used downstream
#'
#' @param model_elements List of normalized model elements.
#'
#' @returns Filtered model element list.
#' @noRd
keep_import_model_properties_IM <- function(model_elements) {
  keep_prop <- get_building_block_prop()
  all_prop <- c(
    unique(unlist(keep_prop)),
    "eqn_insightmaker", "name_insightmaker", "id_insightmaker"
  )

  lapply(model_elements, function(x) x[names(x) %in% all_prop])
}


#' Order imported model elements for object construction
#'
#' @param model_elements List of normalized model elements.
#'
#' @returns Ordered model element list.
#' @noRd
order_import_model_elements_IM <- function(model_elements) {
  model_elements <- stats::setNames(
    model_elements,
    unname(get_map(model_elements, "name"))
  )

  final_types <- vapply(model_elements, function(x) x[["type"]] %||% "", character(1))
  c(
    model_elements[final_types == "stock"],
    model_elements[final_types == "aux"],
    model_elements[final_types == "flow"],
    model_elements[final_types == "lookup"]
  )
}


#' Prepare imported model elements for adding to context
#'
#' @param model_elements List of parsed source model elements.
#'
#' @returns Ordered import-ready model element list.
#' @noRd
prepare_import_model_elements_IM <- function(model_elements) {
  model_element_types <- vapply(model_elements, function(x) x[["type"]] %||% "", character(1))

  model_elements <- prep_model_element_equations_IM(model_elements)
  model_elements <- normalize_model_element_types_IM(model_elements, model_element_types)
  model_elements <- keep_import_model_properties_IM(model_elements)
  order_import_model_elements_IM(model_elements)
}


#' Print debug summary for imported model context
#'
#' @param ctx Import context.
#'
#' @returns Invisibly returns NULL.
#' @noRd
debug_import_context_IM <- function(ctx) {
  if (!P[["debug"]]) {
    return(invisible(NULL))
  }

  n_stocks <- sum(ctx$object[["variables"]][["type"]] == "stock")
  n_flows <- sum(ctx$object[["variables"]][["type"]] == "flow")
  n_auxs <- sum(ctx$object[["variables"]][["type"]] == "aux")
  n_gfs <- sum(ctx$object[["variables"]][["type"]] == "lookup")

  cli::cli_inform(c(
    "Model elements detected:",
    "i" = "Stocks: {.val {n_stocks}}",
    "i" = "Flows: {.val {n_flows}}",
    "i" = "Auxiliaries: {.val {n_auxs}}",
    "i" = "Graphical Functions: {.val {n_gfs}}"
  ))

  if (!is.null(ctx$object[["macros_temp"]]) && nzchar(ctx$object[["macros_temp"]][["eqn"]])) {
    cli::cli_inform(c("i" = "User-defined macros and globals detected in model."))
  } else {
    cli::cli_inform(c("i" = "No user-defined macros or globals detected."))
  }

  invisible(NULL)
}


#' Convert .InsightMaker or .json file to import context
#'
#' Parses the file and populates an import context with the intermediate
#' representation. Does NOT call update() - the caller is responsible for
#' running conversions and then finalizing.
#'
#' @param read_file XML or json structured model
#' @param ext File extension
#'
#' @returns Import context with parsed model data
#' @noRd
#'
file_to_stockflow <- function(read_file, ext) {
  # Create import context
  ctx <- create_import_context(vendor = "insightmaker")

  # Prepare .InsightMaker or .json file into more common intermediate format
  if (ext == "InsightMaker") {
    out <- prep_IM(read_file)
  } else if (ext == "json") {
    out <- prep_json(read_file)
  }

  # Unpack
  settings <- out[["settings"]]
  meta <- out[["meta"]]
  model_elements <- out[["model_elements"]]
  macros <- out[["macros"]]

  ctx <- ctx_add_parsed_IM_info(ctx, settings, meta, macros)

  # Store variables in context
  ctx$variables <- prepare_import_model_elements_IM(model_elements)

  # Capture original variable info for import_metadata BEFORE any conversions
  ctx <- ctx_capture_original_variables(ctx)

  # Capture original macro info
  ctx <- ctx_capture_original_macros(ctx)

  # Add variables to object using add_variable_row()
  # The conversion functions work on object[["variables"]], so we need to populate it
  ctx <- ctx_add_variables(ctx)

  # Prepare globals/macros and converters
  # These work on object[["variables"]] and object temporary fields
  ctx$object <- prep_globals_IM(ctx$object, macros)
  ctx$object <- prep_converters_IM(ctx$object)

  debug_import_context_IM(ctx)

  ctx
}


sim_settings_IM <- function(object, method, time_units, start, length, dt) {
  # Not every simulation specification may be specified in an Insight Maker model
  args <- compact_(as.list(environment()))
  # print(args)
  # print(names(args))

  # # Ensure year and month match Insight Maker's unit definition - a year in Insight Maker is 365 days, not 365.25 days
  if ("time_units" %in% names(args)) {
    args[["time_units"]] <- stringr::str_replace_all(
      tolower(args[["time_units"]]),
      stringr::regex(c(
        "[Y|y]ear[s]?" = "years",
        "[Q|q]uarter[s]?" = "quarters",
        "[M|m]onth[s]?" = "months",
        "[W|w]eek[s]?" = "weeks",
        "[D|d]ay[s]?" = "days",
        "[H|h]our[s]?" = "hours",
        "[M|m]inute[s]?" = "minutes",
        "[S|s]econd[s]?" = "seconds"
      ), ignore_case = TRUE)
    )
  }

  if ("method" %in% names(args)) {
    args[["method"]] <- tolower(args[["method"]])
    args[["method"]] <- switch(args[["method"]],
      # "euler" = "euler",
      "rk1" = "euler",
      # "rk4" = "rk4",
      args[["method"]]
    )
  }

  if ("dt" %in% names(args)) {
    dt <- setting_num_IM(args, "dt")
    if ("method" %in% names(args) && !is.na(dt) && dt >= 1 && args[["method"]] == "rk4") {
      cli::cli_inform(c(
        "Adjusting timestep for solver.",
        "i" = "The timestep {.code dt = {args[[\"dt\"]]}} is too large for {.fn rk4} solver.",
        ">" = "Setting {.code dt = 0.1} for better accuracy."
      ))
      args[["dt"]] <- ".1"
    }
  }

  if ("start" %in% names(args)) {
    start <- args[["start"]]
  } else {
    start <- 0
  }

  if ("length" %in% names(args)) {
    args[["stop"]] <- as.numeric(start) + as.numeric(args[["length"]])
    args[["length"]] <- NULL
  }

  do.call(sim_settings, args)
}


#' Check whether Insight Maker model is a stock-and-flow model
#'
#' @param type Types of elements
#' @param name Names of elements
#'
#' @returns NULL; called for side effects
#' @noRd
is_sfm_IM <- function(type, name) {
  type <- tolower(type)
  if (!any(c("variable", "stock", "flow") %in% tolower(type))) {
    cli::cli_abort(c(
      "Model contains no stock-and-flow elements.",
      "x" = "The imported model contains no variables, stocks, or flows.",
      "i" = "{.pkg sdbuildR} only supports stock-and-flow models.",
      ">" = "Import a model with at least one variable, stock, or flow."
    ))
  }


  # Check for Agent-Based Model (ABM) elements
  idx <- which(type %in% c("state", "transition"))
  if (length(idx) > 0) {
    cli::cli_abort(c(
      "Unsupported model type detected.",
      "x" = "Agent-Based Modelling elements found: {paste0(name[idx], collapse = \", \")}.",
      "i" = "{.pkg sdbuildR} only supports stock-and-flow models.",
      ">" = "Remove agent-based elements or import a different model."
    ))
  }

  return(invisible())
}


new_names_IM <- function(old_names) {
  # Variables cannot be the same name as Insight Maker functions
  IM_func_names <- syntax_IM[["conv_df"]][["insightmaker"]]
  new_names <- clean_name(old_names,
    protected = c(
      IM_func_names, tolower(IM_func_names),
      toupper(IM_func_names)
    )
  )
  new_names
}


get_source_target_IM <- function(children_attrs, node_types,
                                 type = c("InsightMaker", "json")) {
  # Construct dictionary for replacement of id
  adapter <- im_source_adapter(type)

  keep_idx <- node_types %in% c("link", "flow")
  children_connectors <- children_attrs[keep_idx]
  connector_names <- node_types[keep_idx]

  sources <- get_map(children_connectors, adapter[["connector_source_field"]])
  targets <- get_map(children_connectors, adapter[["connector_target_field"]])
  flow_ids <- get_map(children_connectors, adapter[["flow_connector_id_field"]])

  add_stock_sources <- c(sources[connector_names == "flow"], targets[connector_names == "flow"])
  add_stock_targets <- c(flow_ids[connector_names == "flow"], flow_ids[connector_names == "flow"])

  keep_idx <- node_types %in% c("converter")
  converters <- children_attrs[keep_idx]
  converter_names <- get_map(converters, "name")
  converter_sources <- get_map(converters, adapter[["converter_source_field"]])

  if (adapter[["type"]] == "InsightMaker") {
    # Bidirectional links and flows
    bidirectional <- get_map(children_connectors, "bidirectional")

    # Replace ghost ids with original
    if (any(node_types == "ghost")) {
      ghost_sources <- get_map(children_attrs[node_types == "ghost"], "source")
      ghost_ids <- get_map(children_attrs[node_types == "ghost"], "id")
      replace_dict <- stats::setNames(ghost_sources, ghost_ids)
      sources <- stringr::str_replace_all(sources, replace_dict)
      targets <- stringr::str_replace_all(targets, replace_dict)
    }

    # In case of a bidirectional link, switch around source and target and add.
    # which() drops connectors without a BiDirectional attribute, which would
    # otherwise index with NA and inject NA into the dictionary.
    idx_bi <- which(bidirectional == "true")
    add_bi_targets <- sources[idx_bi]
    add_bi_sources <- targets[idx_bi]

    idx <- tolower(converter_sources) == "time"
    converter_sources[idx] <- ""

    targets <- c(targets, add_bi_targets, add_stock_targets, converter_names)
    sources <- c(sources, add_bi_sources, add_stock_sources, converter_sources)
  } else {
    targets <- c(targets, add_stock_targets, converter_names)
    sources <- c(sources, add_stock_sources, converter_sources)

    # Remove NAs
    idx <- is.na(sources) | is.na(targets)
    if (any(idx)) {
      sources <- sources[!idx]
      targets <- targets[!idx]
    }

    # Remove empty strings
    idx <- !nzchar(sources) | !nzchar(targets)
    if (any(idx)) {
      sources <- sources[!idx]
      targets <- targets[!idx]
    }


    # Replace names with ids
    keep_idx <- node_types %in% c("converter", "stock", "flow", "variable")
    model_elements <- children_attrs[keep_idx]

    ids <- unname(get_map(model_elements, "id"))
    names <- unname(get_map(model_elements, "name"))
    sources <- ids[match(sources, names)]
    targets <- ids[match(targets, names)]
  }


  # Remove doubles
  temp <- data.frame(target = targets, source = sources)
  temp <- temp[!duplicated(temp), , drop = FALSE]
  targets <- temp[["target"]]
  sources <- temp[["source"]]


  return(list(sources = sources, targets = targets))
}


change_names_IM <- function(model_elements, source_target_dict,
                            type = c("InsightMaker", "json")) {
  adapter <- im_source_adapter(type)
  sources <- source_target_dict[["sources"]]
  targets <- source_target_dict[["targets"]]

  model_element_types <- get_map(model_elements, "type")
  ids <- get_map(model_elements, "id")
  old_names <- unname(get_map(model_elements, "name"))
  new_names <- new_names_IM(old_names)

  # Add to which ids it has access
  model_elements <- lapply(model_elements, function(x) {
    x[["access_ids"]] <- Filter(nzchar, compact_(sources[targets == x[["id"]]]))
    x[["access"]] <- new_names[match(x[["access_ids"]], ids)]
    return(x)
  })

  use_id <- adapter[["use_id_for_names"]]

  find_name <- function(old, access_ids, use_id) {
    if (use_id) {
      name <- new_names[match(old, ids)]
    } else {
      name <- new_names[old == old_names & ids %in% access_ids]
    }
    if (length(name) == 0 || is.na(name)) {
      name <- ""
    }
    name
  }


  # Replace old names with new names
  model_elements <- lapply(model_elements, function(x) {
    # Which stocks are the flows connected to?
    if (x[["type"]] == "flow") {
      # x[["from"]] <- ifelse("from" %in% names(x), new_names[match(x[["from"]], ids)], "")
      # x[["to"]] <- ifelse("to" %in% names(x), new_names[match(x[["to"]], ids)], "")
      x[["from"]] <- find_name(x[["from"]], x[["access_ids"]], use_id)
      x[["to"]] <- find_name(x[["to"]], x[["access_ids"]], use_id)
    }


    if (x[["type"]] == "converter") {
      if (is_defined(x[["source"]])) {
        # x[["source"]] <- ifelse(tolower(x[["source"]]) == "time",
        #                         P[["time_name"]],
        #                         find_name(x[["source"]], x[["access_ids"]], use_id))

        if (tolower(x[["source"]]) != P[["time_name"]]) {
          x[["source"]] <- find_name(x[["source"]], x[["access_ids"]], use_id)
        }
      }
    }


    if (is_defined(x[["eqn_insightmaker"]])) {
      # Replace old names with new names
      original <- c(old_names[ids %in% x[["access_ids"]]])
      replacement <- c(new_names[ids %in% x[["access_ids"]]])

      if (x[["type"]] == "flow") {
        # Though no longer supported in Insight Maker version 39, in older versions:
        # Flows can use [Alpha] and [Omega] to refer to the stock they flow from and to, respectively. Change to new variable names:
        original <- c(original, "Alpha", "Omega")
        replacement <- c(replacement, x[["from"]], x[["to"]])
      }

      x[["eqn_insightmaker"]] <- replace_names_IM(x[["eqn_insightmaker"]],
        original = original,
        replacement = replacement,
        with_brackets = TRUE, ignore_case = TRUE
      )
    }

    x[["name_insightmaker"]] <- x[["name"]]
    x[["name"]] <- find_name(x[["id"]], ids, use_id = TRUE)

    return(x)
  })

  names(model_elements) <- model_element_types

  return(model_elements)
}


clean_doc <- function(x) {
  # Some notes still have HTML? tags
  x <- gsub("<span[^>]*>", " ", x) # Remove opening <span> tags
  x <- gsub("</span>", " ", x) # Remove closing </span> tags
  x <- gsub("<a[^>]*>", " ", x) # Remove opening <a> tags
  x <- gsub("</a>", " ", x) # Remove closing </a> tags
  x <- gsub("<br>", "\n", x) # Remove closing line breaks
  x <- gsub("<div[^>]*>", " ", x) # Remove opening <div> tags
  x <- gsub("</div>", " ", x) # Remove closing </div> tags
  x <- gsub("&nbsp;", " ", x)
  return(x)
}


prep_globals_IM <- function(object, macros) {
  # Add globals to define at top of script
  # Handle NULL or empty macros
  if (is.null(macros) || !nzchar(macros)) {
    out_global <- list(eqn = "", doc = "")
  } else {
    out_global <- prep_eqn_IM(macros |>
      # Remove leading and last \" before replacing \" with "
      stringr::str_replace("^\\\"", "") |>
      stringr::str_replace("\\\"$", "") |> trimws())
  }

  # Temporary entries in object
  object[["macros_temp"]] <- list(
    eqn = out_global[["eqn"]],
    eqn_insightmaker = out_global[["eqn"]],
    doc = out_global[["doc"]]
  )
  object
}


prep_converters_IM <- function(object) {
  # Get graphical function names and sources
  gf_idx <- object[["variables"]][["type"]] == "lookup"
  converters <- character(0)
  converters_sources <- character(0)

  if (any(gf_idx)) {
    converters <- object[["variables"]][gf_idx, "name"]
    converters_sources <- object[["variables"]][gf_idx, "source"]
  }

  if (length(converters) > 0) {
    # # Ensure correct referencing of converters
    # dict_t <- stats::setNames(
    #   paste0("(", P[["time_name"]], ")"),
    #   paste0("([", P[["time_name"]], "])")
    # )

    dict <- paste0(
      "[", converters, "]", "(",
      ifelse(converters_sources == P[["time_name"]], "", "["),
      converters_sources,
      ifelse(converters_sources == P[["time_name"]], "", "]"),
      ")"
    ) |>
      # # Some sources are time t, remove [t]
      # stringr::str_replace_all(stringr::fixed(dict_t)) |>
      stats::setNames(paste0("[", converters, "]"))

    # Some sources are other graphical functions, replace there as well
    dict_extra <- stats::setNames(paste0("(", unname(dict), ")"), paste0("(", names(dict), ")"))
    dict_extra <- stringr::fixed(dict_extra)

    dict <- stats::setNames(stringr::str_replace_all(unname(dict), dict_extra), names(dict))

    # Temporary placeholders to avoid overlapping replacements
    placeholders <- vapply(seq_along(dict), function(x) {
      paste0(sample(c(letters, LETTERS, 0:9), 12, replace = TRUE), collapse = "")
    }, character(1))

    dict_temp <- stats::setNames(placeholders, names(dict))
    dict_real <- stats::setNames(unname(dict), unname(dict_temp))

    dict_temp <- stringr::fixed(dict_temp)
    dict_real <- stringr::fixed(dict_real)

    # Replace graphical functions in all equations
    for (i in seq_len(nrow(object[["variables"]]))) {
      if ("eqn_insightmaker" %in% colnames(object[["variables"]])) {
        if (!is.na(object[["variables"]][i, "eqn_insightmaker"])) {
          object[["variables"]][i, "eqn_insightmaker"] <- stringr::str_replace_all(
            object[["variables"]][i, "eqn_insightmaker"], dict_temp
          ) |>
            stringr::str_replace_all(dict_real)
        }
      }
    }
  }

  object
}


#' Prepare Insight Maker equation
#'
#' @param eqn Insight Maker equation to prepare
#'
#' @returns List with eqn and doc
#' @noRd
prep_eqn_IM <- function(eqn) {
  # HTML and escape character replacements
  eqn <- textutils::HTMLdecode(eqn) |>
    gsub("\"", "'", x = _, fixed = TRUE) |>
    gsub("\'", "'", x = _, fixed = TRUE) |>
    gsub("\\n", "\n", x = _, fixed = TRUE) |>
    # stringr::str_replace_all(stringr::fixed("\""), "'") |>
    # stringr::str_replace_all(stringr::fixed("\'"), "'") |>
    # stringr::str_replace_all(stringr::fixed("\\n"), "\n") |>
    trimws() |>
    # Ensure there is no scientific notation
    scientific_notation()

  # Replace_comments
  eqn <- replace_comments(eqn)

  # Extract and remove comments
  out <- remove_comments(eqn)
  return(out)
}


#' Replace Insight Maker ids
#'
#' Replace Insight Maker name with id, replace id with name
#'
#' @param string String to apply replacement to
#' @param original Vector with strings to replace
#' @param replacement Vector with strings as replacements
#' @param with_brackets Boolean; whether to include square brackets around the match and replacement
#' @param ignore_case Logical; whether to replace names in a case-insensitive manner. By default TRUE, as Insight Maker is generally not case-sensitive.
#'
#' @returns Updated string
#' @noRd
#'
replace_names_IM <- function(string, original, replacement,
                             with_brackets = TRUE,
                             ignore_case = TRUE) {
  if (is.null(string)) {
    return(string)
  } else {
    replace_dict <- stats::setNames(
      paste0(
        ifelse(with_brackets, "\\[", ""), replacement,
        ifelse(with_brackets, "\\]", "")
      ),
      # First make string suitable for regular expressions by adding escape characters, then add brackets around which cannot be preceded by a square bracket (in order to correctly translate conveyors)
      paste0(
        ifelse(with_brackets, "(?<!\\[)\\[", ""),
        stringr::str_escape(original),
        ifelse(with_brackets, "\\]", "")
      )
    )

    new_string <- stringr::str_replace_all(
      string,
      stringr::regex(replace_dict, ignore_case = ignore_case)
    )
    return(new_string)
  }
}


#' Strip units from model equations and globals
#'
#' @inheritParams update.stockflow
#'
#' @returns Updated object
#' @noRd
#'
strip_units_IM <- function(object) {
  # Get names of all model elements
  var_names <- get_model_var(object)

  # Detected units
  found_units <- list()

  # Replace units in macros - only one macro in case of Insight Maker model
  if (!is.null(object[["macros_temp"]]) && nzchar(object[["macros_temp"]][["eqn"]] %||% "")) {
    out <- strip_units_curly(object[["macros_temp"]][["eqn"]], var_names)
    object[["macros_temp"]][["eqn"]] <- out[["x"]]
    found_units <- c(found_units, list(out[["replacement"]]))
  }

  # Replace units in equations and unit definition in data frame
  for (i in seq_len(nrow(object[["variables"]]))) {
    if (!is.na(object[["variables"]][i, "eqn"])) {
      out <- strip_units_curly(object[["variables"]][i, "eqn"], var_names)
      object[["variables"]][i, "eqn"] <- out[["x"]]
      found_units <- c(found_units, list(out[["replacement"]]))
    }
  }

  # Warn if units were detected and stripped
  found_units <- do.call(rbind, found_units)
  has_units <- nrow(found_units) > 0

  if (has_units) {
    cli::cli_warn(c(
      "!" = "Units detected in imported Insight Maker model.",
      "i" = "sdbuildR does not support units; units will be stripped."
    ))
  }

  object
}


#' Clean units contained in curly brackets
#'
#' @noRd
strip_units_curly <- function(x, var_names) {
  old_x <- x
  default_return <- list(old_x = old_x, replacement = data.frame(match = character(0), replacement = character(0)), x = x)

  # First check if there are any curly brackets
  if (!grepl("\\{", x)) {
    return(default_return)
  }

  # Get indices of curly brackets
  paired_idxs <- get_range_all_pairs(x, var_names, type = "curly", names_with_brackets = TRUE)

  if (nrow(paired_idxs) == 0) {
    return(default_return)
  }

  # Keep only matches that contain at least one letter and no commas
  paired_idxs <- paired_idxs[
    stringr::str_detect(paired_idxs[["match"]], "[a-zA-Z]") &
      !stringr::str_detect(paired_idxs[["match"]], ","),
  ]

  if (nrow(paired_idxs) == 0) {
    return(default_return)
  }

  # These matches contain units. Only retain the first numeric up until the first letter, and ignore the rest (units). This is a simplification, but Insight Maker's unit annotations can be complex and we want to preserve at least the numeric part. For example, {10 people/month} would be replaced with 10. Default to "" if no numeric literal is found, e.g., {people/month}.

  if (nrow(paired_idxs) == 0) {
    return(default_return)
  }

  # Compute replacements
  paired_idxs[["without_braces"]] <- trimws(stringr::str_sub(x, paired_idxs[, "start"] + 1, paired_idxs[, "end"] - 1))

  # Find first part before a space
  paired_idxs[["replacement"]] <- trimws(stringr::str_sub(paired_idxs[["without_braces"]], 1, regexpr("\\s", paired_idxs[["without_braces"]]) - 1))

  # replacement should contain at least one number, and only contain 0-9, ".", ",", "E", "e", "+", "-". If not, replace with "".
  idx <- !grepl("[0-9]+", paired_idxs[["replacement"]]) | grepl("[^0-9.,Ee+\\-]", paired_idxs[["replacement"]])
  paired_idxs[["replacement"]][idx] <- ""

  # Process in reverse order so replacements don't shift indices
  x <- apply_replacements_reversed(x, paired_idxs)

  list(
    old_x = x,
    replacement = paired_idxs[, c("match", "replacement")],
    x = x
  )
}


#' Check non-negative stocks and flows
#'
#' @inheritParams update.stockflow
#' @inheritParams import_insightmaker
#'
#' @returns Updated object
#' @noRd
#'
check_nonnegativity <- function(object, keep_nonnegative_flow,
                                keep_nonnegative_stock) {
  # Non-negative Stocks and Flows
  stock_idx <- object[["variables"]][["type"]] == "stock"
  if (!any(stock_idx)) {
    return(object)
  }

  # Check for non-negative stocks
  if ("non_negative" %in% colnames(object[["variables"]])) {
    nonneg_stock_idx <- stock_idx & object[["variables"]][["non_negative"]]
    nonneg_stock <- which(nonneg_stock_idx)
  } else {
    nonneg_stock <- integer(0)
  }

  if (keep_nonnegative_stock && length(nonneg_stock) > 0) {
    cli::cli_inform(c(
      "Adjusting solver for non-negative stocks.",
      "i" = "Non-negative stocks detected in the model.",
      ">" = "Switching ODE solver to {.fn lsoda} for consistency with {.pkg InsightMaker}.",
      "i" = "Disable this by setting {.arg keep_nonnegative_stock = FALSE}."
    ))
    object[["sim_settings"]][["insightmaker_method"]] <- object[["sim_settings"]][["method"]]
    object[["sim_settings"]][["method"]] <- "lsoda"
  }

  object
}


#' Convert global Insight Maker script to macros
#'
#' @inheritParams update.stockflow
#'
#' @returns Updated stock-and-flow model with macros
#' @noRd
convert_macros_IM_wrapper <- function(object) {
  # Early return if no macros
  if (is.null(object[["macros_temp"]]) || !nzchar(object[["macros_temp"]][["eqn"]] %||% "")) {
    object[["macros_temp"]] <- NULL
    return(object)
  }

  if (nzchar(object[["macros_temp"]][["eqn"]])) {
    # Convert each equation and create list of model elements to add
    var_names <- get_model_var(object)

    object <- replace_macro_names_IM(object)

    # Convert equations in macro
    out <- convert_equations_IM(
      type = P[["func_name"]],
      name = P[["func_name"]],
      eqn = object[["macros_temp"]][["eqn"]],
      var_names = var_names
    )

    if (P[["debug"]]) {
      cli::cli_inform(c(
        "i" = "Conversion output: {out}"
      ))
    }

    object[["macros_temp"]][["eqn"]] <- out[["eqn"]]

    # Extract names and separate equations
    object <- split_macros_IM(object)
  }

  # Remove placeholder
  object[["macros_temp"]] <- NULL

  return(object)
}


#' Replace Insight Maker names in macros with syntactically valid names
#'
#' @inheritParams update.stockflow
#'
#' @returns Updated stock-and-flow model with replaced macro names all throughout the model
#' @noRd
replace_macro_names_IM <- function(object) {
  eqn <- object[["macros_temp"]][["eqn"]]

  # Get names of all model elements
  var_names <- get_model_var(object)

  # Variables cannot be the same name as Insight Maker functions
  IM_func_names <- syntax_IM[["conv_df"]][["insightmaker"]]

  # Find functions
  functions <- stringr::str_locate_all(eqn, stringr::regex("(\\n|^)[ ]*function\\b", ignore_case = TRUE))[[1]]

  # For each <-, find preceding \n and next <-
  newlines <- unique(c(1, stringr::str_locate_all(eqn, "\\n")[[1]][, "start"], nchar(eqn)))
  assignment_op <- stringr::str_locate_all(eqn, "<-")[[1]]

  # Exclude <- & \n in comments and strings
  seq_quot <- get_seq_exclude(eqn, var_names = NULL, type = "quot")

  functions <- functions[!(functions[, "start"] %in% seq_quot), , drop = FALSE]
  assignment_op <- assignment_op[!(assignment_op[, "start"] %in% seq_quot), , drop = FALSE]
  newlines <- newlines[!(newlines %in% seq_quot)]

  # Merge functions and assignments
  assignment <- rbind(assignment_op, functions)
  assignment <- assignment[order(assignment[, "start"]), , drop = FALSE]

  if (nrow(assignment) > 0 && length(newlines) > 0) {
    # Find preceding newline before assignment
    start_idxs <- vapply(assignment[, "start"], function(idx) {
      idxs_newline <- which(newlines <= idx)
      newlines[idxs_newline[length(idxs_newline)]] # select last newline before assignment
    }, numeric(1))

    # Split macros by assignment
    split_macros <- lapply(seq_len(nrow(assignment)), function(i) {
      # Extract equation indices
      start_eqn <- start_idxs[i]
      end_eqn <- ifelse(i == nrow(assignment), nchar(eqn), start_idxs[i + 1] - 1)

      # Extract name
      name_insightmaker <- trimws(stringr::str_sub(eqn, start_eqn, assignment[i, "start"] - 1))

      is_multiline_function <- stringr::str_detect(
        stringr::str_sub(eqn, start_eqn, start_eqn + nchar("function")),
        stringr::regex("function", ignore_case = TRUE)
      )

      names_arg <- names_arg_insightmaker <- c()
      if (is_multiline_function) {
        # Extract function name; In a multiline function, the function name comes after function
        sub_eqn <- stringr::str_sub(eqn, start_eqn, end_eqn)
        name_insightmaker <- trimws(sub("^function", "",
          sub("\\(.*", "", sub_eqn, ignore.case = TRUE),
          ignore.case = TRUE
        ))

        # Variables cannot be the same name as Insight Maker functions
        name <- clean_name(name_insightmaker,
          protected = c(
            var_names,
            IM_func_names, tolower(IM_func_names),
            toupper(IM_func_names)
          )
        )

        # Add opening bracket
        name_insightmaker <- paste0(name_insightmaker, "(")
        name <- paste0(name, "(")
      } else {
        # Take care of function assignment
        is_function <- stringr::str_detect(name_insightmaker, "\\(")

        if (is_function) {
          # Extract argument names
          arg <- trimws(stringr::str_extract(name_insightmaker, "\\(.*\\)$"))
          arg <- parse_args(stringr::str_replace_all(arg, c("^\\(" = "", "\\)$" = "")))

          # Find names and values of arguments
          # contains_name <- stringr::str_detect(arg, "=")
          arg_split <- stringr::str_split_fixed(arg, "=", n = 2)
          names_arg_insightmaker <- arg_split[, 1]
          names_arg <- clean_name(names_arg_insightmaker,
            protected = c(
              var_names,
              IM_func_names, tolower(IM_func_names),
              toupper(IM_func_names)
            )
          )

          # Extract function name, but add opening bracket
          name_insightmaker <- trimws(sub("\\(.*", "", name_insightmaker))

          name <- clean_name(name_insightmaker,
            protected = c(
              var_names,
              IM_func_names, tolower(IM_func_names),
              toupper(IM_func_names)
            )
          )

          # Add opening bracket
          name_insightmaker <- paste0(name_insightmaker, "(")
          name <- paste0(name, "(")
        } else {
          # Variables cannot be the same name as Insight Maker functions
          name <- clean_name(name_insightmaker,
            protected = c(
              var_names,
              IM_func_names, tolower(IM_func_names),
              toupper(IM_func_names)
            )
          )
        }
      }

      z <- list(
        start_eqn = start_eqn,
        end_eqn = end_eqn,
        name = name,
        name_insightmaker = name_insightmaker,
        names_arg = names_arg,
        names_arg_insightmaker = names_arg_insightmaker
      )
      return(z)
    })


    # Replace argument names only in those parts that are functions. This needs to be done after split_macros to preserve indices.
    # Reverse order indices
    for (i in rev(seq_len(nrow(assignment)))) {
      if (length(split_macros[[i]][["names_arg"]]) > 0) {
        # Construct replacement dictionary for replacing argument names in this equation
        dict <- stats::setNames(split_macros[[i]][["names_arg"]], split_macros[[i]][["names_arg_insightmaker"]])

        # Important! Don't simply replace names **
        # Even arguments are not case-sensitive
        stringr::str_sub(eqn, split_macros[[i]][["start_eqn"]], split_macros[[i]][["end_eqn"]]) <- replace_safely(
          eqn = stringr::str_sub(eqn, split_macros[[i]][["start_eqn"]], split_macros[[i]][["end_eqn"]]),
          dict = dict,
          var_names = var_names,
          ignore_case = TRUE
        )
      }
    }

    # Construct replacement dictionary for replacing names in macros and other equations
    # Important: even if the name did not need to be changed, still apply dict because of differences in case
    old_names <- vapply(split_macros, `[[`, character(1), "name_insightmaker")
    new_names <- vapply(split_macros, `[[`, character(1), "name")
    dict <- stats::setNames(new_names, old_names)

    # Insight Maker is not case-sensitive!
    # Important! Don't simply replace names **
    object[["macros_temp"]][["eqn"]] <- replace_safely(
      eqn = object[["macros_temp"]][["eqn"]],
      dict = dict,
      var_names = var_names, ignore_case = TRUE
    )

    # Use same dictionary to replace macro names in other equations
    # Replace in equations in data frame
    for (i in seq_len(nrow(object[["variables"]]))) {
      if (!is.na(object[["variables"]][i, "eqn"])) {
        # Important! Don't simply replace names **
        object[["variables"]][i, "eqn"] <- replace_safely(
          eqn = object[["variables"]][i, "eqn"],
          dict = dict,
          var_names = var_names, ignore_case = TRUE
        )
      }
    }
  }

  return(object)
}


#' Replace dictionary matches in equation safely
#'
#' @param eqn Equation
#' @param dict Dictionary
#' @param var_names Variable names
#' @param ignore_case If TRUE, ignore case. Defaults to TRUE.
#'
#' @returns Updated equation
#' @noRd
replace_safely <- function(eqn, dict, var_names, ignore_case = TRUE) {
  # Remove those matches that are in quotation marks or names
  idxs_exclude <- get_seq_exclude(eqn, var_names)

  idx_df <- lapply(seq_along(dict), function(i) {
    # Names are model element names, not patterns: quote them with \Q...\E so a
    # name containing a regex metacharacter matches literally, and require a
    # word boundary so partial overlaps are left alone.
    pattern <- paste0("\\b\\Q", names(dict)[i], "\\E\\b")
    matches <- gregexpr(pattern, eqn, perl = TRUE, ignore.case = ignore_case)[[1]]

    if (matches[1] == -1) {
      return(data.frame(start = integer(), end = integer()))
    } else {
      data.frame(
        match = rep(names(dict)[i], length(matches)),
        replacement = rep(unname(dict)[i], length(matches)),
        start = as.integer(matches),
        end = as.integer(matches + attr(matches, "match.length") - 1)
      )
    }
  }) |>
    do.call(rbind, args = _) |>
    as.data.frame()


  if (nrow(idx_df) > 0) idx_df <- idx_df[!(idx_df[["start"]] %in% idxs_exclude | idx_df[["end"]] %in% idxs_exclude), ]
  if (nrow(idx_df) > 0) {
    # Replace in reverse order
    eqn <- apply_replacements_reversed(eqn, idx_df)
  }

  return(eqn)
}


#' Split macro equation and names
#'
#' @inheritParams update.stockflow
#'
#' @returns Updated stock-and-flow model with split macros
#' @noRd
split_macros_IM <- function(object) {
  eqn <- object[["macros_temp"]][["eqn"]]
  assignment <- stringr::str_locate_all(eqn, "=")[[1]]
  newlines <- unique(c(1, stringr::str_locate_all(eqn, "\\n")[[1]][, "start"], nchar(eqn)))

  # Exclude <- & \n in comments and strings
  seq_quot <- get_seq_exclude(eqn, var_names = NULL, type = "quot")
  assignment <- assignment[!(assignment[, "start"] %in% seq_quot), , drop = FALSE]
  newlines <- newlines[!(newlines %in% seq_quot)]

  if (nrow(assignment) > 0) {
    # Identify curly brackets to exclude assignment matches in for-loops, while-loops, ifelse etc.
    paired_idxs <- get_range_all_pairs(eqn, var_names = NULL, type = "curly", names_with_brackets = FALSE)

    if (nrow(paired_idxs) > 0) {
      idxs_curly <- unlist(mapply(seq, paired_idxs[, "start"], paired_idxs[, "end"], SIMPLIFY = FALSE))
      assignment <- assignment[!(assignment[, "start"] %in% idxs_curly), , drop = FALSE]
      newlines <- newlines[!(newlines %in% idxs_curly)]
    }

    # Find preceding newline before assignment
    start_idxs <- vapply(assignment[, "start"], function(idx) {
      idxs_newline <- which(newlines <= idx)
      newlines[idxs_newline[length(idxs_newline)]] # select last newline before assignment
    }, numeric(1))

    # Split macros by assignment
    split_macros <- lapply(seq_len(nrow(assignment)), function(i) {
      # Extract equation indices
      start_eqn <- start_idxs[i]
      end_eqn <- ifelse(i == nrow(assignment), nchar(eqn), start_idxs[i + 1] - 1)

      # Extract name
      name <- trimws(stringr::str_sub(eqn, start_eqn, assignment[i, "start"] - 1))
      sub_eqn <- trimws(stringr::str_sub(eqn, assignment[i, "end"] + 1, end_eqn))
      return(list(name = name, eqn = sub_eqn))
    })

    new_macros <- stats::setNames(split_macros, vapply(split_macros, `[[`, character(1), "name"))

    # Add original equation and documentation to first macro - it will be deleted after
    new_macros[[1]][["eqn_insightmaker"]] <- object[["macros_temp"]][["eqn_insightmaker"]]
    new_macros[[1]][["doc"]] <- object[["macros_temp"]][["doc"]]

    # Add each macro as a func-type variable row
    for (m in new_macros) {
      func_name <- m[["name"]]
      func_eqn <- m[["eqn"]]
      func_doc <- m[["doc"]] %||% ""
      # Use placeholder name for unnamed macros
      if (!nzchar(func_name)) {
        func_name <- paste0(".im_script_", sum(object[["variables"]][["type"]] == "func") + 1)
      }
      object <- add_variable_row(object,
        name = func_name,
        type = "func",
        eqn = func_eqn,
        label = func_name,
        doc = func_doc
      )
    }
  }

  return(object)
}


#' Convert Insight Maker equations to R code
#'
#' @inheritParams update.stockflow
#'
#' @returns Updated stock-and-flow model with converted equations to R.
#' @noRd
#'
convert_equations_IM_wrapper <- function(object) {
  # Convert each equation and create list of model elements to add
  var_names <- get_model_var(object)

  # Get variables to convert (stock, aux, flow)
  convert_idx <- object[["variables"]][["type"]] %in% c("stock", "aux", "flow")

  if (!any(convert_idx)) {
    return(object)
  }

  # Accumulate variables to add to the model
  accumulated_add_vars <- data.frame()

  # Convert equations for each variable using flat list structure
  for (i in which(convert_idx)) {
    var_name <- object[["variables"]][i, "name"]
    var_type <- object[["variables"]][i, "type"]
    eqn_before <- object[["variables"]][i, "eqn"]

    # Convert equation
    out <- convert_equations_IM(
      type = var_type,
      name = var_name,
      eqn = eqn_before,
      var_names = var_names
    )

    # Update the variables data frame with converted equation
    object[["variables"]][i, "eqn"] <- out[["eqn"]]

    # Accumulate auxiliary and graphical function variables
    if (nrow(out[["add_vars"]])) {
      accumulated_add_vars <- rbind(accumulated_add_vars, out[["add_vars"]])
    }
  }

  # Add accumulated auxiliary and graphical function variables to the model
  if (nrow(accumulated_add_vars)) {
    # Some Insight Maker columns may be missing, e.g., eqn_insightmaker
    missing_cols <- setdiff(colnames(object[["variables"]]), colnames(accumulated_add_vars))
    for (col in missing_cols) {
      accumulated_add_vars[[col]] <- NA
    }

    object[["variables"]] <- rbind(object[["variables"]], accumulated_add_vars)
  }

  object
}


#' Remove brackets around R names
#'
#' Add sources for graphical functions
#'
#' @inheritParams update.stockflow
#'
#' @returns Updated object
#' @noRd
#'
remove_brackets_from_names <- function(object) {
  # Remove brackets
  var_names <- get_model_var(object)
  dict <- stringr::fixed(stats::setNames(var_names, paste0("[", var_names, "]")))

  for (i in seq_len(nrow(object[["variables"]]))) {
    if (!is.na(object[["variables"]][i, "eqn"])) {
      object[["variables"]][i, "eqn"] <- stringr::str_replace_all(
        object[["variables"]][i, "eqn"], dict
      )
    }
  }

  return(object)
}


#' Split auxiliaries into static parameters or dynamic variables
#'
#' @inheritParams update.stockflow
#'
#' @returns Vector of variable names that are constants
#' @noRd
#'
split_aux_wrapper <- function(object) {
  # Get names
  var_names <- get_model_var(object)

  # Get auxiliary variables
  aux_idx <- object[["variables"]][["type"]] == "aux"
  if (!any(aux_idx)) {
    return(object)
  }

  aux_eqns <- object[["variables"]][aux_idx, "eqn"]
  names(aux_eqns) <- object[["variables"]][aux_idx, "name"]

  # Separate auxiliary variables into static parameters and dynamically updated auxiliaries
  deps <- .dependencies(object, eqns = aux_eqns, only_model_var = FALSE)

  # Constants are not dependent on time, have no dependencies in names, or are only dependent on constants
  temp <- deps
  temp <- temp[vapply(temp, function(x) {
    (!P[["time_name"]] %in% x) & (length(intersect(x, var_names)) == 0)
  }, logical(1))]
  constants <- names(temp)
  rm(temp)

  # Iteratively find constants
  done <- FALSE
  if (!done) {
    old_constants <- constants

    # Are there any remaining auxiliary variables to be split into constants or aux?
    aux_names <- object[["variables"]][aux_idx, "name"]
    remaining_aux <- setdiff(aux_names, constants)
    if (length(remaining_aux) == 0) {
      done <- TRUE
    } else {
      new_constants <- deps[remaining_aux]
      idx <- unlist(lapply(new_constants, function(x) {
        (!P[["time_name"]] %in% x) & all(intersect(x, var_names) %in% constants)
      }))
      new_constants <- new_constants[idx]
      constants <- c(constants, names(new_constants))

      # While-loop ends if there is no change
      if (setequal(old_constants, constants)) {
        done <- TRUE
      }
    }
  }

  # Update variable types in data frame
  for (const_name in constants) {
    idx <- object[["variables"]][["name"]] == const_name
    object[["variables"]][idx, "type"] <- "constant"
  }

  return(object)
}
