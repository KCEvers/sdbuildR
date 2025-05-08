
#' Extract InsightMaker model XML string from URL
#'
#' @param URL String with URL to an InsightMaker model
#' @param filepath_IM String with file path to an InsightMaker model with suffix .InsightMaker
#' @param directory Directory to save output in, optional
#'
#' @return String with InsightMaker model
#' @export
#'
url_to_IM = function(URL, filepath_IM, directory){

  # Read URL
  url_data = rvest::read_html(URL)

  # Get link from page
  iframe_src = url_data %>% rvest::html_element("iframe") %>% rvest::html_attr("src")
  full_iframe_url <- rvest::url_absolute(iframe_src, URL) # Create absolute link out of relative link
  iframe_page <- rvest::read_html(full_iframe_url)

  # Get elements with <script tag
  script_texts = iframe_page %>% rvest::html_elements("script")

  # Keep script with certain keywords
  script_model <- script_texts[stringr::str_detect(script_texts %>% rvest::html_text(trim = T), "model_id") & stringr::str_detect(script_texts %>% rvest::html_text(trim = T), "model_title")] %>%
    as.character()

  # Extract part of interest
  xml_str = stringr::str_match_all(script_model,
                                   stringr::regex("<mxGraphModel>(.*?)</mxGraphModel>", dotall = T))[[1]][1] %>%
    stringr::str_replace_all("mxGraphModel", "InsightMakerModel")  %>%
    # Remove escape characters for writing an XML file
    stringr::str_replace_all(stringr::fixed("\\\\\""), "\\\"") %>%
    stringr::str_replace_all(stringr::fixed("\\\""), "\"") %>%
    stringr::str_replace_all(stringr::fixed("\\\\n"), "\\n")
  xml_str

  # Extract meta-data - this is embedded in the webpage, but not saved in the .InsightMaker file. Add to .InsightMaker file to preserve the original author of the model.
  header_names = c("model_id", "model_title", "model_author_id", "model_author_name")
  header_info = sapply(header_names, function(x){
    stringr::str_match(script_model,
                       sprintf("\"%s\":\"(.*?)\"", x))[,2]}) %>% as.list()
  # header_info$URL = URL
  header_str = sprintf("<header> %s </header>", paste0(names(header_info), "=\"", unname(textutils::HTMLencode(header_info, encode.only = c("&", "<", ">"))), "\"", collapse = ", "))

  # Insert header
  idx_root = stringr::str_locate(xml_str, "<root>")
  stringr::str_sub(xml_str, idx_root[, "start"], idx_root[, "end"]) = paste0("<root> \\n", header_str)

  # Save data to .InsightMaker file
  if (is.null(filepath_IM)){
    filepath_IM = file.path(directory, sprintf("%s_%s_%s_%s.InsightMaker", header_info[["model_title"]], header_info[["model_author_name"]], header_info[["model_id"]], format(Sys.time(), "%Y_%m_%d_%H_%M")) %>%
                              # Ensure valid file name
                              path_sanitize())
    if (!file.exists(dirname(filepath_IM))){
      dir.create(dirname(filepath_IM), recursive = T)
    }
  }
  # xml2::write_xml(xml_str %>% xml2::read_xml(), filepath_IM, options = c("no_declaration", "format_whitespace", "as_xml"))
  writeLines(xml_str, filepath_IM)
  xml_file = xml2::read_xml(filepath_IM)

  # meta$filepath_IM = filepath_IM

  return(list(xml_file = xml_file,
              header_info = header_info,
              filepath_IM = filepath_IM))
}


#' Sanitize a filename by removing directory paths and invalid characters
#' Copied from the fs package to avoid importing the entire package (https://github.com/r-lib/fs/blob/main/R/sanitize.R); all credit goes to fs.
#'
#' `path_sanitize()` removes the following:
#' - [Control characters](https://en.wikipedia.org/wiki/C0_and_C1_control_codes)
#' - [Reserved characters](https://web.archive.org/web/20230126161942/https://kb.acronis.com/content/39790)
#' - Unix reserved filenames (`.` and `..`)
#' - Trailing periods and spaces (invalid on Windows)
#' - Windows reserved filenames (`CON`, `PRN`, `AUX`, `NUL`, `COM1`, `COM2`,
#'   `COM3`, COM4, `COM5`, `COM6`, `COM7`, `COM8`, `COM9`, `LPT1`, `LPT2`,
#'   `LPT3`, `LPT4`, `LPT5`, `LPT6`, LPT7, `LPT8`, and `LPT9`)
#' The resulting string is then truncated to [255 bytes in length](https://en.wikipedia.org/wiki/Comparison_of_file_systems#Limits)
#' @param filename A character vector to be sanitized.
#' @param replacement A character vector used to replace invalid characters.
#' @seealso <https://www.npmjs.com/package/sanitize-filename>, upon which this
#'   function is based.
path_sanitize <- function(filename, replacement = "") {
  illegal <- "[/\\?<>\\:*|\":]"
  control <- "[[:cntrl:]]"
  reserved <- "^[.]+$"
  windows_reserved <- "^(con|prn|aux|nul|com[0-9]|lpt[0-9])([.].*)?$"
  windows_trailing <- "[. ]+$"

  filename <- gsub(illegal, replacement, filename)
  filename <- gsub(control, replacement, filename)
  filename <- gsub(reserved, replacement, filename)
  filename <- gsub(windows_reserved, replacement, filename, ignore.case = TRUE)
  filename <- gsub(windows_trailing, replacement, filename)

  # TODO: this substr should really be unicode aware, so it doesn't chop a
  # multibyte code point in half.
  filename <- stringr::str_sub(filename, 1, 255)
  if (replacement == "") {
    return(filename)
  }
  path_sanitize(filename, "")
}


#' Convert XML nodes to list
#'
#' @inheritParams url_to_IM
#' @inheritParams insightmaker_to_sfm
#' @param InsightMaker_version Integer with InsightMaker version the package sdbuildR was built with
#'
#' @return Nested list in XMILE style
#'
IM_to_xmile <- function(filepath_IM, InsightMaker_version = 37, debug) {

  # Read file
  xml_file = xml2::read_xml(filepath_IM)

  # Get the children nodes
  children <- xml2::xml_children(xml_file)
  if (xml2::xml_name(children) == "root"){
    children <- xml2::xml_children(children) # Double to remove nested layer
  }

  # Get attributes, also of children
  tags = c("Setting", "Display", "Variable", "Converter", "Stock", "Flow", "Link", "Ghost")
  node_names = xml2::xml_name(children)

  # Check whether the model has any components
  if (!any(c("Variable", "Stock", "Flow") %in% node_names)){
    print("Your model does not contain any Variables, Stocks, or Flows!")
    stop()
  }

  if ("header" %in% node_names){
    # Step 1: Split by comma and trim spaces
    header_str = children[match("header", node_names)] %>%
      xml2::xml_text(trim = TRUE)
    pairs <- strsplit(header_str, ",\\s*")[[1]]

    # Step 2: Split each pair into name and value, then clean up extra quotes
    header_list <- sapply(pairs, function(pair) {
      key_value <- strsplit(pair, "=\\s*")[[1]]
      key <- key_value[1]
      value <- gsub('\"', '', key_value[2])  # Remove extra quotes
      return(value)
    })

    # Convert to named list
    names(header_list) <- sapply(pairs, function(pair) {
      strsplit(pair, "=\\s*")[[1]][1]
    })
    header_list = as.list(header_list)

  } else {
    header_list = list()
  }

  # Get attributes
  children_attrs = lapply(children, function(x){
      c(unlist(xml2::xml_attrs(x)), unlist(xml2::xml_attrs(xml2::xml_children(x))))
    })
  children_attrs = lapply(children_attrs[node_names %in% tags], as.list)
  node_names = node_names[node_names %in% tags]

  # Get first setting
  settings = children_attrs[[match("Setting", node_names)[1]]]

#   settings$method = ifelse(settings$TimeStep == 1, "iteration",
#                            ifelse(settings$SolutionAlgorithm == "RK1", "euler",
#          ifelse(settings$SolutionAlgorithm == "RK4", "rk4", settings$SolutionAlgorithm)))

  settings$method = ifelse(settings$SolutionAlgorithm == "RK1", "euler",
                                  ifelse(settings$SolutionAlgorithm == "RK4", "rk4", settings$SolutionAlgorithm))

  if (as.numeric(settings$TimeStep) >= 1 & settings$method == "rk4"){
    message(sprintf("The chosen timestep %s is not suitable for the Runge-Kutta 4th order solver. Setting timestep to 0.1...", settings$TimeStep))
    settings$TimeStep = ".1"
  }


  # Check whether the model uses an early version of Insight Maker
  if (as.numeric(settings$Version) < 37){
    message(sprintf("This model uses an earlier version of InsightMaker (%s), in which links were bi-directional by default. This may cause issues in translating the model. Please choose 'Clone Insight' in Insight Maker, and provide the URL to the updated model.", settings$Version))
  }


  # Check whether model is later version of Insight Maker than the package was made for
  if (as.numeric(settings$Version) > InsightMaker_version){
    message(sprintf("This model uses Insight Maker version %s, whereas the package was based on Insight Maker version %d. Some features may not be available.", settings$Version,
                    InsightMaker_version))
  }

  # Add version to header
  header_list$InsightMaker_version = settings$Version

  # Change links to access_ids
  keep_idx = node_names %in% c("Link", "Flow")
  children_connectors = children_attrs[keep_idx]
  connector_names = node_names[keep_idx]
  bidirectional = children_connectors %>% get_map("BiDirectional")
  ids = children_connectors %>% get_map("id")
  sources = children_connectors %>% get_map("source")
  targets = children_connectors %>% get_map("target")

  # Add Stocks as sources for Flows as targets
  add_stock_sources = c(sources[connector_names == "Flow"], targets[connector_names == "Flow"])
  add_stock_targets = c(ids[connector_names == "Flow"], ids[connector_names == "Flow"])

  # Replace ghost ids with original
  if (any(node_names == "Ghost")){
    ghost_sources = children_attrs[node_names == "Ghost"] %>% get_map("Source")
    ghost_ids = children_attrs[node_names == "Ghost"] %>% get_map("id")
    replace_dict = stats::setNames(ghost_sources, ghost_ids)
    sources = stringr::str_replace_all(sources, replace_dict)
    targets = stringr::str_replace_all(targets, replace_dict)
  }

  # In case of a bidirectional link, switch around source and target
  add_bi_targets = sources[bidirectional == "true"]
  add_bi_sources = targets[bidirectional == "true"]
  targets = c(targets, add_bi_targets, add_stock_targets)
  sources = c(sources, add_bi_sources, add_stock_sources)

  # Remove doubles
  temp = data.frame(targets = targets, sources = sources) #%>% dplyr::distinct()
  temp <- temp[!duplicated(temp), ]
  targets = temp$targets
  sources = temp$sources

  # Construct dictionary for replacement of id
  keep_idx = node_names %in% c("Variable", "Converter", "Stock", "Flow")
  model_element_names = node_names[keep_idx]
  ids = children_attrs[keep_idx] %>% get_map("id")
  old_names = children_attrs[keep_idx] %>% get_map("name")
  stock_names = children_attrs[keep_idx][model_element_names == "Stock"] %>% get_map("name")

  # Variables cannot be the same name as Insight Maker functions
  IM_func_names = get_syntax_IM()$conv_df$InsightMaker
  new_names = create_R_names(old_names,
                             # Normally, names_df is specified here
                             data.frame(name = NULL),
                             protected = c(IM_func_names, tolower(IM_func_names),
                                                                   toupper(IM_func_names)))

  # Add to which ids it has access
  model_elements = children_attrs[keep_idx] %>%
    # stats::setNames(model_element_names) %>%
    lapply(., function(x){

      x$access_ids = sources[targets == x$id]
      x$access = new_names[match(x$access_ids, ids)]
      x
    }) %>%
    # Rename to eqn_InsightMaker
    purrr::imap(function(x, y){
      # print(x)
      # print(y)

      idx = which(names(x) %in% c("FlowRate", "Equation", "InitialValue"))

      # print("idx")
      # print(idx)

      if (length(idx) != 0){
        x$eqn_InsightMaker = x[[idx]]

        if (!nzchar(x$eqn_InsightMaker)){
          x$eqn_InsightMaker = "0.0"
        }

        x[idx] = NULL
      } else if (length(idx) == 0 & model_element_names[y] %in% c("Variable", "Stock", "Flow")){
        # The default value of a Stock/Flow/Variable is 0 - add in case left unspecified
        x$eqn_InsightMaker = "0.0"
      }

      # Rename Note to doc
      if ("Note" %in% names(x)){
        x$doc = x$Note
        x$Note = NULL
      } else {
        x$doc = ""
      }

      # Rename constraints
      if ("MinConstraint" %in% names(x)){
        x$min = ifelse(x$MinConstraintUsed == "true", x$MinConstraint, "")
        x$max = ifelse(x$MaxConstraintUsed == "true", x$MaxConstraint, "")
        x[c("MinConstraint", "MinConstraintUsed", "MaxConstraint", "MaxConstraintUsed")] = NULL
      }

      return(x)
    }) %>%
    # Replace old names with new names
    lapply(., function(x){
      x$eqn_InsightMaker = stringr::str_replace_all(x$eqn_InsightMaker, stringr::fixed(c("\\n" = "\n")))
      x$eqn_InsightMaker = replace_names_IM(x$eqn_InsightMaker,
                                     original = old_names[ids %in% x$access_ids],
                                     replacement = new_names[ids %in% x$access_ids])
      x$name_InsightMaker = x$name
      x$name = replace_names_IM(x$name_InsightMaker,
                             original = old_names[match(x$id, ids)],
                             replacement = new_names[match(x$id, ids)], with_brackets = FALSE)
      return(x)
    }) %>%
    # Remove comments from equations
    purrr::imap(function(x, i){

      # print(x$name)
      # print(i)
      # print("original eqn")
      # print(x$eqn_InsightMaker)

      # Graphical functions won't have an equation
      if (is_defined(x$eqn_InsightMaker)){
        out = prep_eqn_IM(x$eqn_InsightMaker)
        x$eqn_InsightMaker = out$eqn
        x$doc = paste0(x$doc, out$doc)

        # print("out$eqn")
        # print(out$eqn)
        # print(x$doc)
        # print("x$doc")
      }
      return(x)
    })
  model_elements

  # Converters
  converter_prop = c("doc", "units_InsightMaker", "name_InsightMaker", "type", "name", "min", "max", "xpts", "ypts", "interpolation", "source", "extrapolation",
                     # "access", "access_ids",
                     "id")
  model_elements[model_element_names == "Converter"] = model_elements[model_element_names == "Converter"] %>%
    lapply(., function(x){

      # Get x and y data for interpolation function
      data_split = strsplit(x$Data, ";")[[1]] %>%
        strsplit(",") %>%
        do.call(rbind, .)
      x$type = "gf"
      x$xpts = as.numeric(trimws(data_split[,1]))
      x$ypts = as.numeric(trimws(data_split[,2]))
      x$interpolation = ifelse(x$Interpolation == "None", "constant", tolower(x$Interpolation))
      x$source = ifelse(x$Source == "Time", P$time_name, new_names[x$Source == ids])
      x$extrapolation = "nearest" # Default
      x$units_InsightMaker = x$Units

      # Only keep selected properties
      x = x[names(x) %in% converter_prop]

      return(x)
    })



  # Variables -> Auxiliaries
  variable_prop = c("doc", "eqn_InsightMaker", "units_InsightMaker", "name_InsightMaker", "type", "name", "min", "max",
                    # "access", "access_ids",
                    "id")
  model_elements["Variable" == model_element_names] = model_elements["Variable" == model_element_names] %>%
    lapply(., function(x){
      x$units_InsightMaker = x$Units

      # Only keep selected properties
      x$type = "aux"
      x = x[names(x) %in% variable_prop]

      return(x)
    })

  # Stocks
  stock_prop = c("doc", "units_InsightMaker", "name_InsightMaker", "type", "name", "min", "max",
                 # "delayn",
                 "eqn_InsightMaker",
                 # "StockMode", "Delay",
                 "conveyor", "len",
                 "non_negative",
                 # "access", "access_ids",
                 "id")
  flows = model_elements["Flow" == model_element_names]
  flow_ids = flows %>% get_map("id")
  flow_sources = flows %>% get_map("source")
  flow_targets = flows %>% get_map("target")
  model_elements["Stock" == model_element_names] = model_elements["Stock" == model_element_names] %>%
    # Only keep selected properties and rename
    lapply(., function(x){
      # x$delayn = FALSE # Indicate it is not a delay variable
      x$non_negative = ifelse(x$NonNegative == "true", TRUE, FALSE)
      x$units_InsightMaker = x$Units
      x$type = "stock"

      if (x$StockMode == "Conveyor"){
        x$conveyor = TRUE
        x$len = x$Delay
    }

      # Only keep selected properties
      x = x[names(x) %in% stock_prop]
      return(x)
    }) # %>%
    # # Add inflows and outflows
    # purrr::map(function(x){
    #   x$inflow = new_names[match(flow_ids[x$id == flow_targets], ids)]
    #   x$outflow = new_names[match(flow_ids[x$id == flow_sources], ids)]
    #   x
    # })

  # Flows
  flow_prop = c("doc", "eqn_InsightMaker", "units_InsightMaker", "name_InsightMaker", "type", "name", "min", "max", "from", "to", "non_negative",
                # "access", "access_ids",
                "id")
  model_elements["Flow" == model_element_names] = model_elements["Flow" == model_element_names] %>%
    # Only keep selected properties and rename
    lapply(., function(x){

      # Which Stocks are the Flows connected to?
      x$from = ifelse("source" %in% names(x), new_names[x$source == ids], "")
      x$to = ifelse("target" %in% names(x), new_names[x$target == ids], "")
      x$type = "flow"

      # Flows can use [Alpha] and [Omega] to refer to the stock they flow from and to, respectively. Change to new variable names
      x$eqn_InsightMaker = stringr::str_replace_all(x$eqn_InsightMaker,
                                                stringr::regex(c("\\[Alpha\\]" = paste0("[", x$from, "]"),
                                                                 "\\[Omega\\]" = paste0("[", x$to, "]")), ignore_case = T))

      x$non_negative = ifelse(x$OnlyPositive == "true", TRUE, FALSE)

      x$units_InsightMaker = x$Units

      # Only keep selected properties
      x = x[names(x) %in% flow_prop]
      x
    })


  # Name elements
  model_elements = model_elements %>%
    stats::setNames(model_elements %>% get_map("name"))


  # Find which primitives to plot
  displays = children_attrs[match("Display", node_names)]
  display_types = displays %>% get_map("Type")
  # Get the first time series display
  display = displays[[which(display_types == "Time Series")[1]]]
  # Get all ids which are plotted; sometimes, the primitives are listed as "Primitives", "Primitives2"
  display_ids = display[grepl("^Primitives", names(display))] %>% unlist() %>% unname() %>% Filter(nzchar, .) %>%
    paste0(collapse = ",")
  display_ids_split = trimws(strsplit(display_ids, ",")[[1]])
  # If display ids is empty, use Stocks
  if (length(display_ids_split) > 0){
    display_var = new_names[match(display_ids_split, ids)]
  } else {
    display_var = names(model_elements["Stock" == model_element_names])
  }


  # Ensure year and month match Insight Maker's unit definition - a year in Insight Maker is 365 days, not 365.25 days
  time_units = settings$TimeUnits %>%
    stringr::str_replace_all(stringr::regex(c("[Y|y]ear[s]?" = "common_yr",
                                              "[Q|q]uarter[s]?" = "common_quarter",
                                              "[M|m]onth[s]?" = "common_month"), ignore_case = T))

  # Set-up basic structure
  sfm = xmile() %>%
    sim_specs(method = settings$method,
              time_units = time_units,
              start = as.numeric(settings$TimeStart),
              stop = as.numeric(settings$TimeStart) + as.numeric(settings$TimeLength),
              dt = as.numeric(settings$TimeStep),
              saveat = as.numeric(settings$TimeStep))

  # Header
  # header_list = list(InsightMaker_version = settings$Version) %>% utils::modifyList(header_info)
  sfm$header = sfm$header %>% utils::modifyList(header_list)

  # Variables
  sfm$model$variables = sfm$model$variables %>%
    utils::modifyList(list(stock = model_elements["Stock" == model_element_names],
                                                 aux = model_elements["Variable" == model_element_names],
                                                 flow = model_elements["Flow" == model_element_names],
                                                 gf = model_elements["Converter" == model_element_names]))

  # Add globals to define at top of script
  out_global = prep_eqn_IM(settings$Macros %>%
    # Remove leading and last \" before replacing \" with "
    stringr::str_replace("^\\\"", "")  %>%
    stringr::str_replace("\\\"$", ""))

  # Temporary
  sfm$model_units_str = settings$Units
  sfm$global = list(eqn = out_global$eqn,
                        eqn_InsightMaker = out_global$eqn,
                        doc = out_global$doc)

  # ** very important: all global variable names have to be replaced by syntactically valid R names (everywhere in the model as well), before any replacement takes place. e.g. if someone creates a global variable called F, this will be replaced to false in Julia, and there is no way to trace it back.


  sfm$display = list(variables = display_var)

  if (debug){
    print(sprintf("Detected %d Stock%s, %d Flow%s, %d Auxiliar%s, and %d Graphical Function%s",
                  length(sfm$model$variables$stock),
                  ifelse(length(sfm$model$variables$stock) == 1, "", "s"),
                  length(sfm$model$variables$flow),
                  ifelse(length(sfm$model$variables$flow) == 1, "", "s"),
                  length(sfm$model$variables$aux),
                  ifelse(length(sfm$model$variables$aux) == 1, "y", "ies"),
                  length(sfm$model$variables$gf),
                  ifelse(length(sfm$model$variables$gf) == 1, "", "s")))

    if (nzchar(out_global$eqn)){
      print("User-defined macros and globals detected")
    } else {
      print("No user-defined macros and globals detected")
    }
  }

  # ** add detected nonnegative and constraints


  # Conveyors Stocks have a fixed delay with a specified delay length. If a stock "A" is referred to as [A], it refers to the delayed value of A, whereas [[A]] refers to the accumulated value to the 'true' current value of A. If the stock is of StockMode Conveyor, any reference to [A] refers to the delayed value of A.
  # In short:
  # for a stock A which is a conveyor,
  # [A] refers to A_conveyor
  # [[A]] refers to A
  conveyor_stocks = sfm$model$variables$stock %>% purrr::map("conveyor") %>% unlist() %>% names()
  if (length(conveyor_stocks) > 0){

    # Ensure correct referencing of conveyors
    dict = paste0("[", conveyor_stocks, P$conveyor_suffix, "]") %>%
      stats::setNames(paste0("[[", conveyor_stocks, "]]"))

    sfm$model$variables = lapply(sfm$model$variables, function(x){
      lapply(x, function(y){
        if ("eqn_InsightMaker" %in% names(y)){
          y$eqn_InsightMaker = stringr::str_replace_all(y$eqn_InsightMaker, stringr::fixed(dict, ignore_case = T))

          # Remove any left-over double bracket notations - these should be []
          y$eqn_InsightMaker = stringr::str_replace_all(y$eqn_InsightMaker, stringr::fixed(c("[[" = "[", "]]" = "]")))

        }
        return(y)
      })
    })
  }

  converters = names(sfm$model$variables$gf)
  converters_sources = unlist(lapply(sfm$model$variables$gf, `[[`, "source"))
  if (length(converters) > 0){

    # Ensure correct referencing of converters
    dict_t = stats::setNames(paste0("(", P$time_name, ")"), paste0("([", P$time_name, "])"))
    dict = paste0("[", converters, "]", "([", converters_sources, "])") %>%
      # Some sources are time t, remove [t]
      stringr::str_replace_all(stringr::fixed(dict_t)) %>%
      stats::setNames(paste0("[", converters, "]"))

    sfm$model$variables = lapply(sfm$model$variables, function(x){
        lapply(x, function(y){
          if ("eqn_InsightMaker" %in% names(y)){
            y$eqn_InsightMaker = stringr::str_replace_all(y$eqn_InsightMaker, stringr::fixed(dict))
          }
          return(y)
        })
      })

  }

  # Already add eqn and units
  sfm$model$variables = lapply(sfm$model$variables, function(y){
    lapply(y, function(x){

      if ("eqn_InsightMaker" %in% names(x)){
        x$eqn = trimws(x$eqn_InsightMaker)
        if (!nzchar(x$eqn)){
          x$eqn = "0.0"
        }
      }

      if ("units_InsightMaker" %in% names(x)){
        x$units = trimws(x$units_InsightMaker)
        if (!nzchar(x$units)){
          x$units = "1"
        }
      }
      return(x)
    })
  })

  sfm = validate_xmile(sfm)

  return(sfm)
}



#' Prepare Insight Maker equation
#'
#' @param eqn Insight Maker equation to prepare
#'
#' @returns List with eqn and doc
prep_eqn_IM = function(eqn){
  # HTML and escape character replacements
  eqn = eqn %>% textutils::HTMLdecode() %>%
    stringr::str_replace_all(stringr::fixed("\""), "'") %>%
    stringr::str_replace_all(stringr::fixed("\'"), "'") %>%
    stringr::str_replace_all(stringr::fixed("\\n"), "\n") %>% trimws() %>%
    # Ensure there is no scientific notation
    scientific_notation()

  # Replace_comments
  eqn = replace_comments(eqn)

  # Extract and remove comments
  out = remove_comments(eqn)
}


#' Replace InsightMaker ids
#'
#' Replace InsightMaker name with id, replace id with name
#'
#' @param string String to apply replacement to
#' @param original Vector with strings to replace
#' @param replacement Vector with strings as replacements
#' @param with_brackets Boolean; whether to include square brackets around the match and replacement
#'
#' @return Updated string
#'
replace_names_IM = function(string, original, replacement, with_brackets = TRUE){
  if (is.null(string)){
    return(string)
  } else {
    replace_dict = stats::setNames(paste0(ifelse(with_brackets, "\\[", ""), replacement, ifelse(with_brackets, "\\]", "")),
                                   # First make string suitable for regular expressions by adding escape characters, then add brackets around which cannot be preceded by a square bracket (in order to correctly translate conveyors)
                                   # paste0("([^\\[]]*)\\[", stringr::str_escape(original), "\\]"))
                                   paste0( ifelse(with_brackets, "(?<!\\[)\\[", ""), stringr::str_escape(original), ifelse(with_brackets, "\\]", "")))

    new_string = stringr::str_replace_all(string,
                                          # InsightMaker is not case-sensitive in the use of names
                                          stringr::regex(replace_dict, ignore_case = T))
    return(new_string)
  }
}




#' Prepare units for Julia's Unitful package
#'
#' @inheritParams build
#' @inheritParams clean_unit
#'
#' @return Updated sfm
#'
clean_units_IM = function(sfm, regex_units) {

  # Get names of all model elements
  # names_df = get_names(sfm)
  var_names = get_model_var(sfm)

  # Remove year because Insight Maker's year is 365 days
  regex_units = regex_units[regex_units != "yr"]
  regex_units = c(regex_units, c("^[Y|y]ear[s]?$" = "common_yr",
                                             "^[Q|q]uarter[s]?$" = "common_quarter",
                                             "^[M|m]onth[s]?$" = "common_month"))


  # Define custom units
  if (nzchar(sfm$model_units_str)){

    # Create dataframe with custom units, splitting by <> to separate name, eqn, and alias
    custom_units_df = strsplit(
      # In Insight Maker, units are not case-sensitive
      tolower(sfm$model_units_str), "\n")[[1]] %>%
      lapply(., function(x){
        stringr::str_split_fixed(x, "<>", n = 3)
      }) %>% do.call(rbind, .) %>%
      magrittr::set_colnames(c("name", "eqn", "alias")) %>%
      as.data.frame() %>%
      dplyr::rowwise() %>%
      # Create new equation if alias is defined; alias can now be discarded
      dplyr::mutate(eqn = ifelse(nzchar(.data$alias),
                                 paste0(.data$eqn, " ", .data$alias),
                                 .data$eqn)) %>%
      dplyr::ungroup()

    # Clean units and keep mapping between old and new unit
    name_translation = lapply(custom_units_df$name, function(y){clean_unit(y, regex_units, ignore_case = TRUE, include_translation = TRUE, unit_name = TRUE)})
    eqn_translation = lapply(custom_units_df$eqn, function(y){clean_unit(y, regex_units, ignore_case = TRUE, include_translation = TRUE)})

    # Add translated units
    custom_units_df$new_name = unlist(lapply(name_translation, `[[`, "x_new"))
    custom_units_df$new_eqn = unlist(lapply(eqn_translation, `[[`, "x_new"))

    # Remove custom units of which all parts already exist
    idx_keep = purrr::imap(name_translation, function(x, i){
      # Check whether all parts already exist; only check for parts with letters in them
      not_all_parts_exist = !all(x$x_parts[grepl("[a-zA-Z]", x$x_parts)] %in% c(custom_units_df$new_name[-i], unname(regex_units)))
      # If the equation isn't zero, the existing unit is otherwise defined
      not_all_parts_exist | custom_units_df$new_eqn[i] != "1"
    }) %>% unlist()

    if (any(idx_keep)){
      custom_units_df = custom_units_df[idx_keep, ]

      # Create list of model units to add
      # Both name and eqn now need to be cleaned and split into parts; we cannot define units with a name such as "whales^2"; instead we should define "whale"
      add_model_units = detect_undefined_units(sfm, new_eqns = "",
                                         new_units = c(custom_units_df$new_name,
                                                            custom_units_df$new_eqn),
                                         regex_units = regex_units, R_or_Julia = "R")

      # Create list with custom unit definitions
      custom_units_list = lapply(1:nrow(custom_units_df), function(i){
        custom_unit = custom_units_df[i, ]$new_name

        list(name = custom_unit, eqn = custom_units_df[i, ]$new_eqn, prefix = FALSE)
      }) %>% stats::setNames(custom_units_df$new_name)

      # Add to regex dictionary
      custom_dict = c(unlist(lapply(name_translation, `[[`, "x_parts")),
                      unlist(lapply(eqn_translation, `[[`, "x_parts")))
      custom_dict = custom_dict[unname(custom_dict) %in% names(add_model_units)]
      custom_dict = custom_dict[!duplicated(custom_dict)] # Remove duplicate entries

      if (length(custom_dict)){
        names(custom_dict) = paste0("^", stringr::str_escape(names(custom_dict)), "$")
        regex_units = c(custom_dict, regex_units)
      }

      # Create custom model units
      sfm$model_units = add_model_units %>%
        # Overwrite units which already have a definition in custom_units
        utils::modifyList(custom_units_list[names(custom_units_list) %in% names(add_model_units)])

    }

  }

  sfm$model_units_str = NULL

  # Define function to clean units contained in curly brackets
  clean_units_curly = function(x, regex_units){

    # # Clean x
    # x = x %>%
    #   # Replacements have to be made in order to make get_range_all_pairs() work
    #   stringr::str_replace_all(stringr::fixed("\\n"), "\n")

    # Get indices of curly brackets
    # paired_idxs = lapply(x, get_range_all_pairs,
    #                    names_df, type = "curly") %>% do.call(rbind, .)
    paired_idxs = get_range_all_pairs(x, var_names, type = "curly", names_with_brackets = TRUE)

    if (nrow(paired_idxs) > 0){

      # At least one letter needs to be in between the curly brackets and there cannot be commas
      paired_idxs = paired_idxs[stringr::str_detect(paired_idxs$match, "[a-zA-Z]") &
          !stringr::str_detect(paired_idxs$match, ","), ]

      if (nrow(paired_idxs) > 0){

        # Apply clean unit here already, because Insight Maker doesn't care about case, though it will also be applied when converting equations
        # Find replacements by applying clean_unit()
        replacements = lapply(seq.int(nrow(paired_idxs)),
                              function(i){
                                # Remove curly brackets
                                # Insight Maker treats units as case-insensitive
                                stringr::str_sub(tolower(x), paired_idxs[i, "start"] + 1, paired_idxs[i, "end"] - 1) %>%
                                  #stringr::str_replace("^\\{", "") %>% stringr::str_replace("\\}$", "") %>%
                                  clean_unit(., regex_units, ignore_case = TRUE)
                              })

        # Replace in reverse order
        for (i in rev(seq.int(nrow(paired_idxs)))){
          stringr::str_sub(x, paired_idxs[i, "start"], paired_idxs[i, "end"]) = paste0("u(\"", replacements[[i]], "\")")
        }

        # # Find replacements by applying clean_unit()
        # replacements = lapply(seq.int(nrow(paired_idxs)),
        #                       function(i){
        #                         # Remove curly brackets
        #                         paste0("u(\"",
        #                                stringr::str_sub(x, paired_idxs[i, "start"] + 1, paired_idxs[i, "end"] - 1),
        #                                "\")")
        #                       })

        # Replace in reverse order
        # for (i in rev(seq.int(nrow(paired_idxs)))){
        #   stringr::str_sub(x, paired_idxs[i, "start"], paired_idxs[i, "end"]) = paste0("u(\"", stringr::str_sub(x, paired_idxs[i, "start"] + 1, paired_idxs[i, "end"] - 1), "\")")
        # }



      }
    }


   return(x)
  }


  # Replace units in macros - only one macro in case of Insight Maker model
  sfm$global$eqn = clean_units_curly(sfm$global$eqn, regex_units)

  # Replace units in equations and unit definition
  sfm$model$variables = sfm$model$variables %>%
    purrr::map_depth(2, function(x){
      if (is_defined(x$eqn)){
        x$eqn = clean_units_curly(x$eqn, regex_units)
      }
      if (is_defined(x$units)){
        x$units = clean_unit(tolower(x$units), regex_units, ignore_case = TRUE)
      }

      return(x)
    })

  # Ensure all units are defined
  add_model_units = detect_undefined_units(sfm,
                                     new_eqns = c(sfm$model$variables %>%
                                                    lapply(function(x){lapply(x, `[[`, "eqn")}) %>% unlist(),
                                                  sfm$global$eqn_Julia,
                                                  unlist(lapply(sfm$macro, `[[`, "eqn"))),
                                     new_units = sfm$model$variables %>%
                                       lapply(function(x){lapply(x, `[[`, "units")}) %>% unlist(),
                                     regex_units = regex_units, R_or_Julia = "R")
  sfm$model_units = add_model_units %>% utils::modifyList(sfm$model_units)


  sfm = validate_xmile(sfm)

  return(sfm)

}




#' Check non-negative Stocks and Flows
#'
#' @inheritParams build
#' @inheritParams insightmaker_to_sfm
#'
#' @return Updated sfm
#'
check_nonnegativity = function(sfm, keep_nonnegative_flow, keep_nonnegative_stock, keep_solver){

  # Non-negative Stocks and Flows
  # nonneg_stock = sfm$model$variables$stock %>% purrr::map("non_negative") %>% unlist() %>% which()
  nonneg_stock = which(unlist(lapply(sfm$model$variables$stock, `[[`, "non_negative")))
  # nonneg_flow = sfm$model$variables$flow %>% purrr::map("non_negative") %>% unlist() %>% which()

  if (keep_nonnegative_stock & length(nonneg_stock) > 0){
    # sfm$behavior$stock = names(nonneg_stock)

    if (!keep_solver & sfm$sim_specs$method == "rk4"){
      print("Non-negative Stocks detected! Switching the ODE solver to Euler to ensure Insight Maker and sdbuildR produce the same output. Turn off by setting keep_solver = TRUE.")
      sfm$sim_specs$method_InsightMaker = sfm$sim_specs$method
      sfm$sim_specs$method = "euler"
    }
  }
  # if (keep_nonnegative_flow & length(nonneg_flow) > 0){
  #   sfm$behavior$flow = names(nonneg_flow)
  # }
  # sfm = validate_xmile(sfm)


  return(sfm)
}



#' Convert InsightMaker equations to R code
#'
#' @inheritParams build
#' @inheritParams clean_unit
#' @param debug Boolean; whether to print output; defaults to FALSE
#'
#' @return Updated sfm
#'
convert_equations_IM_wrapper = function(sfm, debug, regex_units){

  # Convert each equation and create list of model elements to add
  # names_df = get_names(sfm)
  var_names = get_model_var(sfm)

  add_model_elements = sfm$model$variables[c("stock", "aux", "flow")] %>%
    purrr::list_flatten() %>% unname() %>%
    lapply(., function(x){

      if (debug){
        print(x$name)
        print(x$eqn)
        # if (!nzchar(x$eqn)){
        #   message("Is empty!")
        # }
      }

      # Convert equation
      out = convert_equations_IM(
        type = x$type,
        name = x$name,
        eqn = x$eqn,
        var_names = var_names,
                         regex_units = regex_units,
                         debug = debug)

      if (debug){
        print(out)
      }
      return(out)

    }) %>% purrr::flatten()

  # Add names
  add_model_elements = add_model_elements %>%
    lapply(., function(x){
      purrr::imap(x,
      function(y, name){

        # if (debug){
        #   print("name")
        #   print(name)
        #   print("y")
        #   print(y)
        # }

        y$name = name
        return(y)
      })
  })

  add_model_elements
  add_model_elements %>% length()

  # Add to sfm
  for (i in 1:length(add_model_elements)){
    sfm$model$variables = sfm$model$variables %>% utils::modifyList(add_model_elements[i])
  }




    # out = convert_equations_IM(type = "macro",
    #                            name = "macro",
    #                            eqn = sfm$macro[[1]]$eqn,
    #                            var_names = var_names,
    #                            regex_units = regex_units,
    #                            debug = debug)
  sfm = convert_global_IM(sfm, regex_units = regex_units,
                               debug = debug)


  sfm = validate_xmile(sfm)

  return(sfm)
}



convert_global_IM = function(sfm, eqn, var_names, regex_units, debug = FALSE){

  # names_df = get_names(sfm)
  var_names = get_model_var(sfm)
  #
  # # Global variable names still need to obey R's and Julia's syntactic rules
  # # For example, a function name cannot be F.
  # replace_global_IM_names
  #
  # new_names = create_R_names(old_names,
  #                            # Normally, names_df is specified here
  #                            data.frame(name = NULL),
  #                            protected = c(IM_func_names, tolower(IM_func_names),
  #                                          toupper(IM_func_names)))

  # If macros are specified, convert equations
  if (nzchar(sfm$global$eqn)){

  # Convert equation
  out = convert_equations_IM(
    type = "global",
    name = "global",
    eqn = sfm$global$eqn,
    var_names = var_names,
    regex_units = regex_units,
    debug = debug)

  if (debug){
    print(out)
  }
  sfm$global$eqn = unname(unlist(out$global$global$eqn))

  }


  return(sfm)
}



#' Remove brackets around R names
#'
#' Add units and add sources for graphical functions
#'
#' @inheritParams clean_units_IM
#' @inheritParams build
#'
#' @return Updated sfm
#'
remove_brackets_from_names = function(sfm){

  # # Add source to graphical function
  # gf_names = sfm$model$variables$gf %>%
  #   purrr::map(\(x) paste0(x$name, "(", x$source, ")")) %>% unlist()
  #
  # # Remove source property from graphical functions
  # sfm$model$variables$gf = sfm$model$variables$gf %>%
  #   purrr::discard_at("source")

  # Remove brackets
  # names_df = get_names(sfm)
  var_names = get_model_var(sfm)
  dict = stats::setNames(var_names, paste0("[", var_names, "]"))

  sfm$model$variables = lapply(sfm$model$variables, function(y){
    lapply(y, function(x){

      x$eqn = stringr::str_replace_all(x$eqn, stringr::fixed(dict))

      return(x)
    })
  })

  return(sfm)
}



#' Split auxiliaries into static parameters or dynamic variables
#'
#' @inheritParams build
#'
#' @return Vector of variable names that are constants
#'
split_aux_wrapper = function(sfm){

  # Get names
  # names_df = get_names(sfm)
  var_names = get_model_var(sfm)

  # # DelayN variables
  # delay_var = sfm$model$variables$aux %>% purrr::map("delayN") %>% purrr::compact() %>% names()

  # Separate auxiliary variables into static parameters and dynamically updated auxiliaries
  dependencies = lapply(sfm$model$variables$aux, `[[`, "eqn") %>%
    find_dependencies(sfm, ., only_model_var = FALSE)

  # Constants are not dependent on time, have no dependencies in names, or are only dependent on constants
  # constants = names(dependencies %>% purrr::keep(\(x) length(x) == 0))
  constants = names(dependencies %>%
                      purrr::keep(\(x) (!P$time_name %in% x) & (length(intersect(x, var_names)) == 0)))  #%>% setdiff(delay_var)
  # dependencies %>% purrr::map(\(x) intersect(x, names_df$name))

  # Iteratively find constants
  done = FALSE
  if (!done){

    old_constants = constants

    # Are there any remaining auxiliary variables to be split into constants or aux?
    remaining_aux = setdiff(names(sfm$model$variables$aux), constants)
    if (length(remaining_aux) == 0){
      done = TRUE
    } else {

      new_constants = dependencies[remaining_aux] %>%
        purrr::keep(\(x) (!P$time_name %in% x) & all(intersect(x, var_names) %in% constants))
      constants = c(constants, names(new_constants)) #%>% setdiff(delay_var)

      # While-loop ends if there is no change
      if (setequal(old_constants, constants)){ done = TRUE }
    }
  }

  sfm$model$variables$constant = sfm$model$variables$aux[constants]
  sfm$model$variables$aux[constants] = NULL
  sfm$model$variables$constant = sfm$model$variables$constant %>%
    lapply(., function(x){
      x$type = "constant"
      return(x)
    })

  sfm = validate_xmile(sfm)

  return(sfm)

}




