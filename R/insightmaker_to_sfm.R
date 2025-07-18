
#' Import Insight Maker model
#'
#' Import a stock-and-flow model from [Insight Maker](https://insightmaker.com/). Models may be your own or from another user. Importing causal loop diagrams or agent-based models is not supported.
#'
#' @param URL URL to Insight Maker model. Character.
#' @param filepath_IM File path to Insight Maker model. Only used if URL is not specified. Needs to be a character with suffix .InsightMaker.
#' @param directory Directory to save translated Insight Maker model in (optional). Defaults to temporary directory.
#' @param keep_nonnegative_flow If TRUE, keeps original non-negativity setting of flows. Defaults to TRUE.
#' @param keep_nonnegative_stock If TRUE, keeps original non-negativity setting of stocks Defaults to TRUE.
#' @param keep_solver If TRUE, keep the ODE solver as it is. If FALSE, switch to Euler integration in case of non-negative stocks to reproduce the Insight Maker data exactly. Defaults to FALSE.
#'
#' @return Stock-and-flow model of class xmile.
#' @export
#' @family insightmaker
#' @seealso [build()], [xmile()]
#'
#' @examples
#' # Load a model from Insight Maker
#' sfm = insightmaker_to_sfm(URL =
#'  'https://insightmaker.com/insight/5LxQr0waZGgBcPJcNTC029/Crielaard-et-al-2022')
#' plot(sfm)
#'
#' # Simulate the model
#' sim = simulate(sfm)
#' plot(sim)
#'
insightmaker_to_sfm = function(URL,
                             filepath_IM,
                             directory = tempdir(),
                             keep_nonnegative_flow = TRUE,
                             keep_nonnegative_stock = FALSE,
                             keep_solver = FALSE
                             ){

  if (P[["debug"]]){
    print(URL)
    print(filepath_IM)
  }

  if (missing(URL) & missing(filepath_IM)){
    message("Either URL or filepath_IM needs to be specified!")
    stop()
  }

  if (missing(filepath_IM)){
    filepath_IM = NULL
  }

  # Check whether URL is InsightMaker model; if so, create .InsightMaker file
  if (!missing(URL)){
    is_valid_URL = ifelse(!missing(URL), stringr::str_detect(URL, stringr::regex("http[s]?\\:\\/\\/[www\\.]?insightmaker")), 0)
    if (is_valid_URL){
      # URL to .InsightMaker file
      URL_XML = url_to_IM(URL, filepath_IM, directory)
      header_info = URL_XML$header_info
      filepath_IM = URL_XML$filepath_IM # File path may have been overwritten
    } else {
      stop("This is not a URL to an Insight Maker model!")
    }
  } else {
    header_info = list()
  }

  is_file = file.exists(filepath_IM)
  if (!is_file){
    stop("Your filepath_IM refers to a file that does not exist.")
  } else {
    # Check whether it is an InsightMaker file
    if (tools::file_ext(filepath_IM) != "InsightMaker"){
      stop("Your filepath does not have the file extension .InsightMaker. Download your InsightMaker model by clicking on the share button in the top right corner, clicking 'Import/Export', the down arrow, and 'Download Insight Maker File'.")
    }
  }

  # Create model structure
  sfm = IM_to_xmile(filepath_IM)

  # Add header information
  if (length(header_info) > 0){
    sfm$header$author = header_info[["model_author_name"]]
    sfm$header$name = header_info[["model_title"]]
    sfm$header$caption = header_info[["model_description"]]
    sfm$header$version = header_info[["model_version"]]
    sfm$header$InsightMaker_id = header_info[["model_id"]]
  }

  # Clean up units
  if (P[["debug"]]){
    print("Cleaning units...")
  }
  # Get regular expressions to convert units to Julia
  regex_units = get_regex_units()
  sfm = clean_units_IM(sfm, regex_units)

  # Check non-negativity for flows and stocks
  sfm = check_nonnegativity(sfm, keep_nonnegative_flow, keep_nonnegative_stock, keep_solver)

  # Convert Insight Maker equation to R, including macros
  if (P[["debug"]]){
    print("Converting equations from Insight Maker to R...")
  }

  # Convert macros - important to do before equations, as some macros may have
  # syntactically invalid names
  sfm = convert_macros_IM_wrapper(sfm, regex_units = regex_units)

  # Convert equations in model variables
  sfm = convert_equations_IM_wrapper(sfm, regex_units = regex_units)

  # Finalize equations by removing brackets from names
  sfm = remove_brackets_from_names(sfm)

  # Convert equations and macros to Julia
  sfm = convert_equations_julia_wrapper(sfm, regex_units = regex_units)

  # As a last step in the translation, split auxiliaries into constants and auxiliaries
  sfm = split_aux_wrapper(sfm)

  # Determine simulation language: if using units or delay functions, set to Julia
  delayN_smoothN = get_delayN_smoothN(sfm)
  delay_past = get_delay_past(sfm)
  unit_strings = find_unit_strings(sfm)
  df = as.data.frame(sfm, type = c("stock", "aux", "constant", "gf"))

  if (length(delayN_smoothN) > 0 | length(delay_past) > 0 | length(unit_strings) > 0 | length(sfm$model_units) > 0 | any(df$units != "1")){
    sfm = sfm %>% sim_specs(language = "Julia")
  }

  return(sfm)
}


