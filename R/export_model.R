#' Export a stock-and-flow model
#'
#' Export a model of class [`stockflow`][stockflow] to another format.
#'
#' ## sdbuildR format (`format = "sdbuildR"`)
#' Returns R code that reconstructs the model using sdbuildR functions.
#' When `file = NULL`, returns a character string.
#' When `file` is provided, writes an `.R` file and returns the path invisibly.
#' If `file` has no `.R` extension, one is appended.
#'
#' ## deSolve format (`format = "deSolve"`)
#' Returns a standalone R script using [deSolve::ode()] directly — no sdbuildR
#' dependency required to run the output.
#' When `file = NULL`, returns a character string.
#' When `file` is provided, writes an `.R` file and returns the path invisibly.
#' If `file` has no `.R` extension, one is appended.
#' Requires `sim_settings(language = "R")` (the default).
#'
#' ## Psychomodels format (`format = "psychomodels"`)
#' Generates a JSON record for upload to
#' [Psychomodels](https://www.psychomodels.org/models/).
#' When `file = NULL`, returns a JSON character string.
#' When `file` is provided, writes a `.json` file and returns the path invisibly.
#' If `file` has no `.json` extension, one is appended.
#'
#' @inheritParams update.stockflow
#' @param format Export format. One of `"sdbuildR"`, `"deSolve"`, or
#'   `"psychomodels"`.
#' @param file Output file path, or `NULL` to return the result directly.
#' @param title \[psychomodels\] Model title. Defaults to `object[["meta"]][["name"]]`.
#' @param description \[psychomodels\] Model description.
#'   Defaults to `object[["meta"]][["caption"]]`.
#' @param explanation \[psychomodels\] Free-text explanation. Defaults to `description`.
#' @param publication_doi \[psychomodels\] DOI for the associated publication.
#' @param publication_citation \[psychomodels\] Citation text.
#' @param framework \[psychomodels\] Modeling framework.
#'   Defaults to `"Ordinary Differential Equations"`.
#' @param programming_language \[psychomodels\] Programming language.
#' @param psychology_discipline \[psychomodels\] Discipline id(s), comma-separated.
#' @param software_package \[psychomodels\] Package id(s), comma-separated.
#' @param model_variable \[psychomodels\] Variable id(s), comma-separated.
#' @param code_repository_url \[psychomodels\] URL to code repository.
#' @param data_url \[psychomodels\] URL to model data.
#' @param submission_remarks \[psychomodels\] Optional remarks.
#' @param created_by \[psychomodels\] Identifier of creating user.
#' @param updated_by \[psychomodels\] Identifier of last updating user.
#' @param published_by \[psychomodels\] Identifier of publishing user.
#' @param published_at \[psychomodels\] Publication timestamp. Defaults to current time.
#' @param published_pending_moderation_at \[psychomodels\] Moderation timestamp.
#' @param publication_citation_fetched_at \[psychomodels\] Citation fetch timestamp.
#' @param publication_csl_fetched_at \[psychomodels\] CSL fetch timestamp.
#' @param publication_csl_json \[psychomodels\] CSL JSON text.
#' @param id \[psychomodels\] Optional record id.
#' @param slug \[psychomodels\] Optional slug. Generated from `title` if `NULL`.
#' @param include_latex \[psychomodels\] If `TRUE`, append LaTeX equations to explanation.
#' @param pretty \[psychomodels\] If `TRUE`, pretty-print output JSON.
#'
#' @returns For `file = NULL`: a character string containing the exported content.
#' For `file` specified: invisibly returns the file path.
#' @export
#' @concept importExport
#' @seealso [import_insightmaker()], [import_desolve()]
#'
#' @examples
#' sfm <- stockflow("sir")
#'
#' # Get sdbuildR reconstruction code
#' cat(export_model(sfm, format = "sdbuildR"))
#'
#' # Get standalone deSolve script
#' cat(export_model(sfm, format = "deSolve"))
#'
#' # Export to Psychomodels JSON
#' \dontrun{
#' json <- export_model(sfm,
#'   format = "psychomodels",
#'   publication_doi = "10.0000/example"
#' )
#' }
export_model <- function(object,
                         format = c("sdbuildR", "deSolve", "psychomodels"),
                         file = NULL,
                         # psychomodels-specific params
                         title = object[["meta"]][["name"]],
                         description = object[["meta"]][["caption"]],
                         explanation = description,
                         publication_doi = "",
                         publication_citation = "",
                         framework = "Ordinary Differential Equations",
                         programming_language = "R",
                         psychology_discipline = "",
                         software_package = "",
                         model_variable = "",
                         code_repository_url = "",
                         data_url = "",
                         submission_remarks = "",
                         created_by = "",
                         updated_by = "",
                         published_by = "",
                         published_at = Sys.time(),
                         published_pending_moderation_at = Sys.time(),
                         publication_citation_fetched_at = Sys.time(),
                         publication_csl_fetched_at = Sys.time(),
                         publication_csl_json = "",
                         id = NA,
                         slug = NULL,
                         include_latex = TRUE,
                         pretty = TRUE) {
  if (missing(object)) {
    missing_arg("object")
  }
  check_stockflow(object)

  format <- match.arg(format, c("sdbuildR", "deSolve", "psychomodels"))

  switch(format,
    sdbuildR = {
      code <- build_stockflow_code_(object)
      if (is.null(file)) {
        return(code)
      }
      file <- .validate_r_path(file)
      writeLines(code, file)
      invisible(file)
    },
    deSolve = {
      code <- export_desolve_(object)
      if (is.null(file)) {
        return(code)
      }
      file <- .validate_r_path(file)
      writeLines(code, file)
      invisible(file)
    },
    psychomodels = export_psychomodels(
      object                            = object,
      include_latex                     = include_latex,
      destfile                          = file,
      title                             = title,
      description                       = description,
      explanation                       = explanation,
      publication_doi                   = publication_doi,
      publication_citation              = publication_citation,
      framework                         = framework,
      programming_language              = programming_language,
      psychology_discipline             = psychology_discipline,
      software_package                  = software_package,
      model_variable                    = model_variable,
      code_repository_url               = code_repository_url,
      data_url                          = data_url,
      submission_remarks                = submission_remarks,
      created_by                        = created_by,
      updated_by                        = updated_by,
      published_by                      = published_by,
      published_at                      = published_at,
      published_pending_moderation_at   = published_pending_moderation_at,
      publication_citation_fetched_at   = publication_citation_fetched_at,
      publication_csl_fetched_at        = publication_csl_fetched_at,
      publication_csl_json              = publication_csl_json,
      id                                = id,
      slug                              = slug,
      pretty                            = pretty
    )
  )
}


export_desolve_ <- function(object) {
  if (tolower(object[["sim_settings"]][["language"]]) != "r") {
    cli::cli_abort(c(
      "x" = '{.code format = "deSolve"} requires {.code language = "R"}.',
      "i" = 'Set {.code sim_settings(object, language = "R")} first.'
    ))
  }

  only_stocks <- isTRUE(object[["sim_settings"]][["only_stocks"]])

  # Pre-assemble all static components (times, funcs, static, nonneg, ordering)
  object <- pre_assemble_components(object)

  # Always compile ODE fresh to respect only_stocks
  object[["assemble"]][["ode"]] <- compile_ode(
    object,
    only_stocks = only_stocks,
    language    = "R",
    is_ensemble = FALSE
  )

  run_ode <- compile_run_ode(object, only_stocks = only_stocks, language = "R")

  seed <- object[["sim_settings"]][["seed"]]
  seed_str <- if (is_defined(seed)) paste0("set.seed(", seed, ")") else ""

  paste0(c(
    "library(deSolve)",
    seed_str,
    object[["assemble"]][["times"]],
    object[["assemble"]][["funcs"]],
    object[["assemble"]][["nonneg_stocks"]][["func_def"]],
    object[["assemble"]][["ode"]],
    object[["assemble"]][["static"]][["script"]],
    run_ode
  ), collapse = "\n")
}

.fmt_num <- function(x) {
  x <- as.numeric(x)
  if (x == round(x)) as.character(as.integer(x)) else as.character(x)
}


.validate_r_path <- function(path) {
  if (!grepl("\\.R$", path, ignore.case = TRUE)) path <- paste0(path, ".R")
  path
}


build_stockflow_code_ <- function(object, format_with_nse = FALSE) {
  check_stockflow(object)

  # How to emit an NSE argument (variable/function name, cross-reference, or
  # equation): as a bare symbol/expression when format_with_nse is TRUE,
  # otherwise as a quoted string. Both forms are accepted by the building-block
  # helpers (stock(), custom_func(), ...), but quoting is the safer default
  # since it also handles non-syntactic names and equations containing quotes.
  fmt_nse <- function(x) {
    if (format_with_nse) x else encodeString(x, quote = "\"")
  }

  # Simulation specifications — filter out defaults. Use the realised defaults
  # (new_sim_settings()) rather than formals(), since some defaults (central,
  # spread, quantiles) are vector-valued and unevaluated in formals().
  sim_settings_list <- object[["sim_settings"]]
  ss_defaults <- new_sim_settings()

  # Save fields (save_by/save_times/save_length) are stored under their public
  # names but as character strings; emit them as unquoted numbers instead.
  save_args <- list()
  if (!is.null(sim_settings_list[["save_by"]])) {
    save_args[["save_by"]] <- as.numeric(sim_settings_list[["save_by"]])
  } else if (!is.null(sim_settings_list[["save_times"]])) {
    save_args[["save_times"]] <- as.numeric(sim_settings_list[["save_times"]])
  } else if (!is.null(sim_settings_list[["save_length"]])) {
    save_args[["save_length"]] <- as.integer(sim_settings_list[["save_length"]])
  }

  # Drop the (string-valued) save fields before filtering the remaining settings.
  sim_settings_list <- sim_settings_list[
    !names(sim_settings_list) %in% c("save_by", "save_times", "save_length")
  ]

  sim_settings_list <- sim_settings_list[vapply(names(sim_settings_list), function(nm) {
    val <- sim_settings_list[[nm]]
    !nm %in% names(ss_defaults) || !identical(val, ss_defaults[[nm]])
  }, logical(1))]

  # Append the public save arguments (if any) after the default-filtered settings.
  sim_settings_list <- c(sim_settings_list, save_args)

  # Serialize each value to R source, handling character/numeric scalars and
  # vectors (e.g. central = c("mean", "median"), quantiles = c(0.025, 0.975)).
  fmt_setting <- function(z) {
    if (is.character(z)) z <- paste0("\"", z, "\"")
    if (length(z) != 1) paste0("c(", paste0(z, collapse = ", "), ")") else as.character(z)
  }
  sim_settings_list <- lapply(sim_settings_list, fmt_setting)
  sim_settings_str <- if (length(sim_settings_list) > 0) {
    paste0(" |>\n\tsim_settings(", paste0(names(sim_settings_list), " = ", unname(sim_settings_list), collapse = ", "), ")")
  } else {
    ""
  }

  # Funcs (custom functions); if format_with_nse is TRUE, name is NSE (bare symbol, no quotes)
  func_df <- get_funcs(object)
  if (nrow(func_df) > 0) {
    func_cols <- intersect(c("name", "eqn", "doc"), names(func_df))
    func_df <- func_df[, func_cols, drop = FALSE]

    cf_defaults <- formals(custom_func)
    cf_defaults <- cf_defaults[!names(cf_defaults) %in% c("object", "name", "label")]

    func_str <- vapply(seq_len(nrow(func_df)), function(i) {
      row <- as.list(func_df[i, , drop = FALSE])
      func_name <- row[["name"]]
      row[["name"]] <- NULL

      # Filter out defaults
      row <- row[vapply(names(row), function(nm) {
        !nm %in% names(cf_defaults) || !identical(row[[nm]], cf_defaults[[nm]])
      }, logical(1))]

      args_str <- vapply(names(row), function(nm) {
        val <- row[[nm]]
        # eqn is NSE in custom_func() — bare expression or quoted string
        if (nm == "eqn") {
          paste0(nm, " = ", fmt_nse(val))
        } else if (!is.character(val)) {
          paste0(nm, " = ", val)
        } else {
          paste0(nm, " = \"", val, "\"")
        }
      }, character(1))

      paste0("custom_func(", paste(c(fmt_nse(func_name), args_str), collapse = ", "), ")")
    }, character(1)) |>
      paste0(collapse = " |>\n\t")

    func_str <- paste0(" |>\n\t", func_str)
  } else {
    func_str <- ""
  }

  # Meta-information string
  h <- object[["meta"]]
  defaults_meta <- formals(meta)
  defaults_meta <- defaults_meta[!names(defaults_meta) %in%
    c("object", "created", "...")]

  # Find which elements in h are identical to those in defaults_meta
  h <- h[vapply(names(h), function(name) {
    !name %in% names(defaults_meta) || !identical(
      h[[name]],
      defaults_meta[[name]]
    )
  }, logical(1))]

  h <- lapply(h, function(z) {
    if (is.character(z) |
      inherits(z, "POSIXt")) {
      paste0("\"", z, "\"")
    } else {
      z
    }
  })

  meta_str <- paste0(
    " |>\n\tmeta(",
    paste0(names(h), " = ", unname(h), collapse = ", "), ")"
  )

  # Variables — use type-specific helpers; name/to/from/source are NSE (bare symbols)
  # if format_with_nse is TRUE;
  # func-type variables are handled above via custom_func(), so exclude them here
  vars_df <- object[["variables"]]
  vars_df <- vars_df[vars_df[["type"]] != "func", , drop = FALSE]

  if (nrow(vars_df) > 0) {
    type_to_func <- c(
      stock = "stock", flow = "flow", constant = "constant",
      aux = "aux", lookup = "lookup"
    )

    # Args emitted unquoted — NSE in the target functions
    # nse_skip: also skip when empty/NA (variable cross-references)
    nse_skip <- c("to", "from", "source")
    # nse_expr: unquoted but filtered by defaults normally (expressions)
    nse_expr <- c("eqn")
    # Args stored as list columns containing numeric vectors
    vec_args <- c("xpts", "ypts")

    # Pre-compute defaults for each helper (exclude object, name, label, vec args)
    helper_defaults_list <- lapply(type_to_func, function(fn) {
      d <- formals(get(fn))
      d[!names(d) %in% c("object", "name", "label", vec_args, "...")]
    })

    keep_prop <- get_building_block_prop()

    var_str <- split(vars_df, seq_len(nrow(vars_df))) |>
      lapply(function(y) {
        z <- as.list(y)
        var_name <- z[["name"]]
        var_type <- z[["type"]]
        func_name <- type_to_func[[var_type]]
        helper_defaults <- helper_defaults_list[[var_type]]

        # Keep only relevant properties for this type, excluding name/type/_julia cols
        type_props <- setdiff(keep_prop[[var_type]], c("name", "type"))
        type_props <- type_props[!grepl("_julia", type_props)]
        z <- z[intersect(type_props, names(z))]

        # Skip label if it equals the variable name (default is label = name)
        if (!is.null(z[["label"]]) && identical(z[["label"]], var_name)) {
          z[["label"]] <- NULL
        }

        # Filter out defaults; skip empty/NA cross-references (nse_skip)
        z <- z[vapply(names(z), function(nm) {
          val <- z[[nm]]
          if (nm %in% vec_args) {
            if (is.list(val)) val <- val[[1]]
            return(length(val) > 0 && !all(is.na(val)))
          }
          if (nm %in% nse_skip) {
            return(!is.null(val) && !identical(val, "") && !is.na(val))
          }
          !nm %in% names(helper_defaults) || !identical(val, helper_defaults[[nm]])
        }, logical(1))]

        # Format each argument value
        args_str <- vapply(names(z), function(nm) {
          val <- z[[nm]]
          if (nm %in% vec_args) {
            if (is.list(val)) val <- val[[1]]
            formatted <- if (length(val) == 1) {
              as.character(val)
            } else {
              paste0("c(", paste(val, collapse = ", "), ")")
            }
            paste0(nm, " = ", formatted)
          } else if (nm %in% nse_expr) {
            # equation: bare expression (NSE) or quoted string
            paste0(nm, " = ", fmt_nse(val))
          } else if (nm %in% nse_skip) {
            # variable cross-reference: bare symbol (NSE) or quoted string
            paste0(nm, " = ", fmt_nse(val))
          } else if (is.character(val)) {
            paste0(nm, " = \"", val, "\"")
          } else {
            paste0(nm, " = ", val)
          }
        }, character(1))

        paste0(func_name, "(", paste(c(fmt_nse(var_name), args_str), collapse = ", "), ")")
      })
    var_str <- paste0(" |>\n\t", paste0(unlist(var_str), collapse = " |>\n\t"))
  } else {
    var_str <- ""
  }

  script <- sprintf(
    "sfm <-\tstockflow()%s%s%s%s", sim_settings_str,
    meta_str, var_str, func_str
  )

  paste0(script, "\n")
}
