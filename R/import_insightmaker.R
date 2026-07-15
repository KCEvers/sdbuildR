#' Run an import step, re-raising failures with contextual guidance
#'
#' Wraps an import stage in a `tryCatch` that, on error, aborts with a friendly
#' message explaining which stage failed plus the original error. Collapses the
#' repeated `tryCatch(..., error = cli_abort(...))` blocks in
#' [import_insightmaker()].
#'
#' @param expr Expression to evaluate (lazily; errors are caught).
#' @param x Headline (`"x"`) bullet describing the failure.
#' @param i Optional hint (`"i"`) bullet.
#' @param error_as Name of the bullet (`">"` or `"i"`) under which the original
#'   error message is shown.
#' @returns The value of `expr` on success; otherwise aborts.
#' @noRd
#'
.import_step <- function(expr, x, i = NULL, error_as = c(">", "i")) {
  error_as <- match.arg(error_as)
  tryCatch(
    expr,
    error = function(e) {
      bullets <- c("x" = x)
      if (!is.null(i)) bullets <- c(bullets, "i" = i)
      bullets <- c(bullets, stats::setNames("Original error: {conditionMessage(e)}", error_as))
      cli::cli_abort(bullets, call = NULL)
    }
  )
}


#' Attach raw source information to an import context
#'
#' @param ctx Import context.
#' @param read_file Raw parsed source model.
#' @param url URL argument value.
#' @param file File argument value.
#' @param url_spec Logical; whether `url` was supplied.
#' @param file_spec Logical; whether file was supplied.
#'
#' @returns Updated import context.
#' @noRd
attach_import_source <- function(ctx, read_file, url, file, url_spec, file_spec) {
  ctx$raw_model <- read_file
  ctx$file_path <- if (file_spec && !is.null(file)) file else NULL
  ctx$url <- if (url_spec && !is.null(url)) url else NULL

  if (url_spec) {
    ctx$object[["meta"]][["URL"]] <- url
  }

  ctx
}


#' Convert imported Insight Maker temporary fields to model-ready equations
#'
#' @param object Stock-and-flow object from the import context.
#' @inheritParams import_insightmaker
#'
#' @returns Converted stock-and-flow object.
#' @noRd
convert_imported_IM_object <- function(object, keep_nonnegative_flow, keep_nonnegative_stock) {
  object <- .import_step(
    check_nonnegativity(object, keep_nonnegative_flow, keep_nonnegative_stock),
    x = "Failed to check non-negativity constraints.",
    i = "Review your keep_nonnegative_flow and keep_nonnegative_stock settings."
  )

  if (P[["debug"]]) {
    cli::cli_inform(c("i" = "Converting macros"))
  }

  object <- .import_step(
    convert_macros_IM_wrapper(object),
    x = "Failed to convert macros from Insight Maker format.",
    i = "Check for unsupported macro syntax or functions."
  )

  if (P[["debug"]]) {
    cli::cli_inform(c("i" = "Converting equations"))
  }

  object <- .import_step(
    convert_equations_IM_wrapper(object),
    x = "Failed to convert equations from Insight Maker format.",
    i = "Check for unsupported functions or syntax in your model equations."
  )

  object <- .import_step(
    remove_brackets_from_names(object),
    x = "Failed to clean variable names.",
    error_as = "i"
  )

  object <- .import_step(
    split_aux_wrapper(object),
    x = "Failed to split auxiliary variables into constants and auxiliaries.",
    error_as = "i"
  )

  object
}


#' Finalize an imported Insight Maker object
#'
#' @param ctx Import context.
#' @param object Converted stock-and-flow object.
#' @inheritParams import_insightmaker
#'
#' @returns Final validated stock-and-flow object.
#' @noRd
finalize_imported_IM_object <- function(ctx, object, keep_nonnegative_flow, keep_nonnegative_stock) {
  object <- prep_equations_variables(object)
  object <- prep_stock_change(object)
  object <- sim_settings(object,
    keep_nonnegative_flow = keep_nonnegative_flow,
    keep_nonnegative_stock = keep_nonnegative_stock
  )

  allowed_cols <- colnames(empty_variables())
  object[["variables"]] <- object[["variables"]][, colnames(object[["variables"]]) %in% allowed_cols]

  ctx$object <- object
  object[["import_metadata"]] <- ctx_build_import_metadata(ctx)

  object <- sanitize_stockflow(object)
  validate_stockflow(object)

  object
}


#' Import Insight Maker model
#'
#' Import a stock-and-flow model from [Insight Maker](https://insightmaker.com/). Models may be your own or another user's. Importing causal loop diagrams or agent-based models is not supported.
#'
#' Insight Maker models can be imported using a URL, Insight Maker file, or ModelJSON file. Ensure the URL refers to a public (not private) model. To download a model file from Insight Maker, first clone the model if it is not your own. Then, go to "Share" (top right), "Export", and "Download Insight Maker file" or "ModelJSON File".
#'
#' @param url URL to Insight Maker model. Character.
#' @param file File path to Insight Maker model. Only used if url is not specified. Needs to be a character with suffix .InsightMaker or .json.
#' @param keep_nonnegative_flow If TRUE, keeps original non-negativity setting of flows. Defaults to TRUE.
#' @param keep_nonnegative_stock If TRUE, keeps original non-negativity setting of stocks. Defaults to FALSE.
#'
#' @returns A stock-and-flow model object of class [`stockflow`][stockflow].
#' @export
#' @concept importExport
#' @seealso [update()], [stockflow()]
#'
#' @examplesIf has_internet() && Sys.getenv("NOT_CRAN") == "true"
#' # Load a model from Insight Maker
#' sfm <- import_insightmaker(
#'   url =
#'     "https://insightmaker.com/insight/43tz1nvUgbIiIOGSGtzIzj/Romeo-Juliet"
#' )
#' plot(sfm)
#'
#' \dontshow{
#' sfm <- sim_settings(sfm, save_by = .5)
#' }
#'
#' # Simulate the model
#' sim <- simulate(sfm)
#' plot(sim)
#'
import_insightmaker <- function(url,
                                file,
                                keep_nonnegative_flow = TRUE,
                                keep_nonnegative_stock = FALSE) {
  url_spec <- !missing(url)
  file_spec <- !missing(file)

  if (P[["debug"]]) {
    cli::cli_inform(c("i" = "url: {url}"))
    cli::cli_inform(c("i" = "file: {file}"))
  }

  # Get Insight Maker model
  out <- get_IM_model(url, file)
  read_file <- out[["read_file"]]
  ext <- out[["ext"]]

  # Parse model into import context
  # file_to_stockflow() returns a context with:
  # - ctx$object: object with sim_settings, meta, and variables added (no Julia conversion yet)
  # - ctx$variables: original variable list (for reference)
  # - ctx$original_variables: data frame for import_metadata
  # - ctx$original_macros: data frame for import_metadata
  # - ctx$vendor_meta: meta info for import_metadata
  ctx <- .import_step(
    file_to_stockflow(read_file, ext),
    x = "Failed to convert Insight Maker model structure.",
    i = "Check for unsupported Insight Maker syntax or model structure."
  )

  ctx <- attach_import_source(ctx, read_file, url, file, url_spec, file_spec)

  # Extract object for convenience (conversion functions work on object)
  object <- ctx$object

  object <- convert_imported_IM_object(object, keep_nonnegative_flow, keep_nonnegative_stock)
  finalize_imported_IM_object(ctx, object, keep_nonnegative_flow, keep_nonnegative_stock)
}
