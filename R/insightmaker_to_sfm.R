#' Import Insight Maker model
#'
#' Import a stock-and-flow model from [Insight Maker](https://insightmaker.com/). Models may be your own or from another user. Importing causal loop diagrams or agent-based models is not supported.
#'
#' Insight Maker models can be imported using either a URL or an Insight Maker file. Ensure the URL refers to a public (not private) model. To download a model file from Insight Maker, first clone the model if it is not your own. Then, go to "Share" (top right), "Export", and "Download Insight Maker file".
#'
#' @param URL URL to Insight Maker model. Character.
#' @param file File path to Insight Maker model. Only used if URL is not specified. Needs to be a character with suffix .InsightMaker.
#' @param keep_nonnegative_flow If TRUE, keeps original non-negativity setting of flows. Defaults to TRUE.
#' @param keep_nonnegative_stock If TRUE, keeps original non-negativity setting of stocks Defaults to FALSE.
#' @param keep_solver If TRUE, keep the ODE solver as it is. If FALSE, switch to Euler integration in case of non-negative stocks to reproduce the Insight Maker data exactly. Defaults to FALSE.
#'
#' @return Stock-and-flow model of class xmile.
#' @export
#' @family insightmaker
#' @seealso [build()], [xmile()]
#'
#' @examplesIf has_internet()
#' \dontrun{ # requires internet
#' # Load a model from Insight Maker
#' sfm <- insightmaker_to_sfm(
#'   URL =
#'     "https://insightmaker.com/insight/43tz1nvUgbIiIOGSGtzIzj/Romeo-Juliet"
#' )
#' plot(sfm)
#'
#' \dontshow{
#' sfm <- sim_specs(sfm, save_at = .5)
#' }
#'
#' # Simulate the model
#' sim <- simulate(sfm)
#' plot(sim)
#' }
insightmaker_to_sfm <- function(URL,
                                file,
                                keep_nonnegative_flow = TRUE,
                                keep_nonnegative_stock = FALSE,
                                keep_solver = FALSE) {
  if (.sdbuildR_env[["P"]][["debug"]]) {
    message("URL: ", URL)
    message("file: ", file)
  }

  # Validate inputs
  if (missing(URL) & missing(file)) {
    stop("Either URL or file needs to be specified!")
  }

  if (!missing(URL) & !missing(file)) {
    stop("Either URL or file needs to be specified, not both!")
  }

  # Load XML file
  if (!missing(URL)) {
    file <- NULL

    is_valid_URL <- stringr::str_detect(
      URL,
      stringr::regex("http[s]?\\:\\/\\/[www\\.]?insightmaker")
    )

    if (!is_valid_URL) {
      stop("This is not a URL to an Insight Maker model! URL must start with http://insightmaker or https://insightmaker")
    }

    tryCatch(
      {
        xml_file <- url_to_IM(URL, file)
      },
      error = function(e) {
        stop("Failed to download Insight Maker model from URL.\n",
          "Original error: ", conditionMessage(e),
          call. = FALSE
        )
      }
    )
  } else {
    # Validate file path
    if (tools::file_ext(file) != "InsightMaker") {
      stop(
        "Your file does not have the file extension .InsightMaker.\n",
        "Download your InsightMaker model by clicking on the share button in the top right corner,\n",
        "clicking 'Import/Export', the down arrow, and 'Download Insight Maker File'."
      )
    }

    if (!file.exists(file)) {
      stop("Your file refers to a file that does not exist: ", file)
    }

    # Read file
    xml_file <- tryCatch(
      {
        xml2::read_xml(file)
      },
      error = function(e) {
        stop("Failed to parse the Insight Maker XML file.\n",
          "File: ", file, "\n",
          "Original error: ", conditionMessage(e),
          call. = FALSE
        )
      }
    )
  }

  # Create model structure
  sfm <- tryCatch(
    {
      IM_to_xmile(xml_file)
    },
    error = function(e) {
      stop("Failed to convert Insight Maker model structure to xmile format.\n",
        "Original error: ", conditionMessage(e),
        call. = FALSE
      )
    }
  )

  # Add URL to header
  if (!missing(URL)) {
    sfm[["header"]][["URL"]] <- URL
  }

  # Clean up units
  if (.sdbuildR_env[["P"]][["debug"]]) {
    message("Cleaning units...")
  }

  regex_units <- get_regex_units()

  sfm <- tryCatch(
    {
      clean_units_IM(sfm, regex_units)
    },
    error = function(e) {
      stop("Failed to clean units in the model.\n",
        "Original error: ", conditionMessage(e),
        call. = FALSE
      )
    }
  )

  # Check non-negativity for flows and stocks
  sfm <- tryCatch(
    {
      check_nonnegativity(sfm, keep_nonnegative_flow, keep_nonnegative_stock, keep_solver)
    },
    error = function(e) {
      stop("Failed to check non-negativity constraints.\n",
        "Original error: ", conditionMessage(e),
        call. = FALSE
      )
    }
  )

  # Convert macros
  if (.sdbuildR_env[["P"]][["debug"]]) {
    message("Converting macros from Insight Maker to R...")
  }

  sfm <- tryCatch(
    {
      convert_macros_IM_wrapper(sfm, regex_units = regex_units)
    },
    error = function(e) {
      stop("Failed to convert macros from Insight Maker format.\n",
        "Check for unsupported macro syntax or functions.\n",
        "Original error: ", conditionMessage(e),
        call. = FALSE
      )
    }
  )

  # Convert equations in model variables
  if (.sdbuildR_env[["P"]][["debug"]]) {
    message("Converting equations from Insight Maker to R...")
  }

  sfm <- tryCatch(
    {
      convert_equations_IM_wrapper(sfm, regex_units = regex_units)
    },
    error = function(e) {
      stop("Failed to convert equations from Insight Maker format.\n",
        "Check for unsupported functions or syntax in your model equations.\n",
        "Original error: ", conditionMessage(e),
        call. = FALSE
      )
    }
  )

  # Finalize equations by removing brackets from names
  sfm <- tryCatch(
    {
      remove_brackets_from_names(sfm)
    },
    error = function(e) {
      stop("Failed to clean variable names.\n",
        "Original error: ", conditionMessage(e),
        call. = FALSE
      )
    }
  )

  # Convert equations and macros to Julia
  sfm <- tryCatch(
    {
      convert_equations_julia_wrapper(sfm, regex_units = regex_units)
    },
    error = function(e) {
      stop("Failed to convert equations to Julia format.\n",
        "Original error: ", conditionMessage(e),
        call. = FALSE
      )
    }
  )

  # Split auxiliaries into constants and auxiliaries
  sfm <- tryCatch(
    {
      split_aux_wrapper(sfm)
    },
    error = function(e) {
      stop("Failed to split auxiliary variables into constants and auxiliaries.\n",
        "Original error: ", conditionMessage(e),
        call. = FALSE
      )
    }
  )

  # Determine simulation language: if using units, set to Julia
  unit_strings <- find_unit_strings(sfm)
  df <- as.data.frame(sfm, type = c("stock", "aux", "constant", "gf"))

  if (length(unit_strings) > 0 | length(sfm[["model_units"]]) > 0 |
    any(df[["units"]] != "1")) {
    message("Detected use of units. Setting simulation language to Julia.")
    sfm <- sim_specs(sfm, language = "Julia")
  }

  return(sfm)
}
