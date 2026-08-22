#' Run ensemble simulation
#'
#' Run large-scale (i.e., ensemble) simulations of stock-and-flow models, varying initial
#' conditions and/or constants specified in `conditions`.
#'
#' It is strongly recommended to reduce the size of the simulation output by
#' saving fewer values with `save_by`, `save_times`, or `save_length` in
#' [sim_settings()].
#'
#' By default, only summary statistics across simulations are returned.
#' To return individual simulations, set `save_sims = TRUE` in [sim_settings()] or
#' pass `save_sims = TRUE` via `...` in [ensemble()].
#' Note that returning individual simulations can consume a lot of memory for large ensembles.
#'
#' For simulations in Julia, the ensemble can be run in parallel using multiple threads
#' by setting `nthreads` in [use_julia()]. For simulations in R, use
#' [future::plan()] to control parallel execution.
#'
#' To create a reproducible ensemble simulation, set a seed using [sim_settings()].
#'
#' If you do not see any variation within a condition of the ensemble (i.e., the
#' confidence bands are virtually non-existent), there are likely no random
#' elements in your model. Without these, there can be no variability in the
#' model. Try specifying a random initial condition or adding randomness to
#' other model elements (see examples).
#'
#' @inheritParams update.stockflow
#' @inheritParams simulate.stockflow
#' @param n Number of simulations to run in the ensemble. When conditions is
#'   specified, n defines the number of simulations to run per condition. If
#'   each condition only needs to be run once, set `n = 1`. Defaults to `n = 10`.
#' @param conditions A named list specifying **fixed** values for ensemble
#'   conditions. Names must correspond to stocks or constants
#'   in the model. Each list element should be a numeric vector of values
#'   to test.
#'
#'   If cross = `TRUE` (default), all combinations of values are generated. For
#'   example, `list(param1 = c(1, 2), param2 = c(10, 20))` creates 4 conditions:
#'   `(1,10), (1,20), (2,10), (2,20)`.
#'
#'   If cross = `FALSE`, values are paired element-wise, requiring all vectors to
#'   have equal length. For example, `list(param1 = c(1, 2, 3), param2 =
#'   c(10, 20, 30))` creates 3 conditions: `(1,10), (2,20), (3,30)`.
#'   Defaults to `NULL` (no parameter variation).
#'
#' @param cross If `TRUE`, cross the parameters in the conditions list to
#'   generate all possible combinations of parameters. Defaults to `TRUE`.
#' @param central Which central-tendency statistic(s) to compute across the
#'   simulations at each time point: any of `"mean"`, `"median"`, or `"none"`.
#'   Each one becomes a column of the same name in `summary`. Defaults to the
#'   model's `central` setting (see [sim_settings()]); set it here to override
#'   that for this run. Note that `central` and `spread` choose what is
#'   *computed*; [plot.ensemble_stockflow()] then chooses what to *show*.
#' @param spread Which measures of spread to compute: `"quantile"` (columns
#'   `quant1`, `quant2`, ... at the probabilities in `quantiles`), `"sd"`,
#'   `"range"` (a `min` and a `max` column), or `"none"`. Several can be
#'   combined, e.g. `c("quantile", "sd")`. Defaults to the model's `spread`
#'   setting.
#' @param quantiles Probabilities for the quantile columns, used when `spread`
#'   includes `"quantile"`, e.g. `c(0.025, 0.975)`. They become columns
#'   `quant1`, `quant2`, ... in the order given (so here `quant1` is the 2.5%
#'   quantile and `quant2` the 97.5%); the probabilities themselves are kept in
#'   `sims$quantiles`. Defaults to the model's `quantiles` setting.
#' @param quiet If `TRUE`, suppress informational messages such as progress and
#'   status updates. Warnings and errors are always shown. Defaults to `FALSE`.
#'   R ensembles show a cli progress bar via [progressr::handler_cli()],
#'   including when simulations run through a parallel [future::plan()].
#' @param ... Optional arguments passed to [sim_settings()]; these can be used to override the simulation specifications set in the model object.
#'
#' @returns Object of class [`ensemble_stockflow`][ensemble()], which is a list
#'   containing:
#' \describe{
#'  \item{success}{If `TRUE`, simulation was successful. If `FALSE`, simulation
#'    failed.}
#'
#'  \item{error_message}{If success is `FALSE`, contains the error message.}
#'  \item{df}{data.frame with simulation results in long format, if save_sims
#'    is `TRUE`. The iteration number is indicated by column "sim". If conditions
#'    was specified, the condition is indicated by column "condition".}
#'  \item{summary}{data.frame with summary statistics of the ensemble. Contains
#'    the statistics requested via `central` and `spread` (as columns named after
#'    each statistic, plus `quant1`, `quant2`, ... when `spread` includes
#'    `"quantile"`), as well as a `missing_count` column. If conditions was
#'    specified, summary statistics are calculated for each condition in the
#'    ensemble.}
#'  \item{n}{Number of simulations run in the ensemble (per condition if
#'    conditions is specified).}
#'  \item{n_total}{Total number of simulations run in the ensemble (across all
#'    conditions if conditions is specified).}
#'  \item{n_conditions}{Total number of conditions.}
#'  \item{conditions}{data.frame with the conditions used in the ensemble, if
#'    conditions was specified.}
#'  \item{init}{List with df (if save_sims = TRUE) and summary, containing
#'    data.frame with the initial values of the stocks used in the ensemble.}
#'  \item{constants}{List with df (if save_sims = TRUE) and summary,
#'    containing data.frame with the constant parameters used in the ensemble.}
#'  \item{script}{Script used for the ensemble simulation.}
#'  \item{duration}{Duration of the simulation in seconds.}
#'  \item{...}{Other parameters passed to ensemble}
#'  }
#' @export
#' @concept ensemble
#' @seealso [update()], [stockflow()], [sim_settings()], [use_julia()],
#'   [future::plan()]
#'
#' @examples
#' # Ensemble simulation in R (no parallelization)
#' # Load example model
#' sfm <- stockflow("predator_prey")
#'
#' # Ensemble simulations can only show variation 
#' # if there is some randomness in the model. 
#' # For example, we can initialize both stocks with
#' # random values between 20 and 80:
#' sfm <- update(sfm, c(predator, prey),
#'   eqn = runif(1, min = 20, max = 80)
#' )
#'
#' # Saving all timepoints is computationally expensive, 
#' # so we save only 20 values per simulation:
#' sfm <- sim_settings(sfm, save_length = 20)
#'
#' # Run ensemble simulation
#' sims <- ensemble(sfm, n = 3)
#' plot(sims)
#'
#' @examplesIf Sys.getenv("NOT_CRAN") == "true"
#' # To plot individual trajectories, rerun the ensemble with save_sims = TRUE.
#' # Note that this can consume a lot of memory for large simulations.
#' sims <- ensemble(sfm, n = 10, save_sims = TRUE)
#' plot(sims, which = "sims")
#'
#' # Specify which trajectories to plot
#' plot(sims, which = "sims", sim = 1:2)
#'
#' # Don't plot the central tendency 
#' # with darker individual trajectories
#' plot(sims, central = "none", which = "sims", alpha = 0.7)
#'
#' # We can speed up ensemble simulations using parallelization
#' future::plan(future::multisession, workers = 2)
#' sims <- ensemble(sfm, n = 25, save_sims = TRUE)
#'
#' # Ensembles can also be run with exact values for the initial conditions
#' # and parameters. Below, we vary the initial values of the predator and the
#' # birth rate of the predators (delta). We generate a hundred samples per
#' # condition. By default, the parameters are crossed, meaning that all
#' # combinations of the parameters are run.
#' sims <- ensemble(sfm,
#'   n = 50,
#'   conditions = list(predator = c(10, 50),
#'                     delta = c(.025, .05))
#' )
#'
#' plot(sims)
#'
#' # By default, a maximum of nine conditions is plotted.
#' # Plot specific conditions:
#' plot(sims, condition = c(1, 3), nrows = 1)
#'
#' # Generate a non-crossed design, where the length of each conditions vector
#' # needs to be equal:
#' sims <- ensemble(sfm,
#'   n = 10, cross = FALSE,
#'   conditions = list(
#'     predator = c(10, 20, 30),
#'     delta = c(.020, .025, .03)
#'   )
#' )
#' plot(sims, nrows = 3)
#'
#' # Stop parallelization after use
#' future::plan(future::sequential)
#'
ensemble <- function(object,
                     n = 10,
                     conditions = NULL,
                     cross = TRUE,
                     central,
                     spread,
                     quantiles,
                     quiet = FALSE, ...) {
  check_stockflow(object)

  # Override sim_settings with any arguments passed via ... or via the
  # central/spread/quantiles arguments (which mirror sim_settings()).
  varargs <- list(...)
  if ("verbose" %in% names(varargs)) {
    cli::cli_warn(c(
      "{.arg verbose} is deprecated.",
      "i" = "Use {.arg quiet} instead."
    ))
    if (missing(quiet)) {
      quiet <- !isTRUE(varargs[["verbose"]])
    }
    varargs[["verbose"]] <- NULL
  }
  if (!missing(central)) varargs[["central"]] <- central
  if (!missing(spread)) varargs[["spread"]] <- spread
  if (!missing(quantiles)) varargs[["quantiles"]] <- quantiles
  if (length(varargs) > 0) {
    object <- do.call(sim_settings, c(list(object), varargs))
  }
  language <- tolower(object[["sim_settings"]][["language"]])
  only_stocks <- object[["sim_settings"]][["only_stocks"]]
  vars <- object[["sim_settings"]][["vars"]]

  check_summary_diagnostics(object)

  # Persistent meta-setting: read save_sims from sim_settings AFTER applying varargs
  save_sims <- isTRUE(object[["sim_settings"]][["save_sims"]])

  output_args <- resolve_sim_output_args(object, only_stocks, vars)
  only_stocks <- output_args[["only_stocks"]]
  vars <- output_args[["vars"]]

  # Read effective summary choices from the model (object defaults, overridden by
  # any central/spread/quantiles supplied above) and resolve them to the internal
  # statistic catalog.
  central <- object[["sim_settings"]][["central"]]
  spread <- object[["sim_settings"]][["spread"]]
  quantiles <- object[["sim_settings"]][["quantiles"]]

  resolved <- resolve_ensemble_stats(central, spread)
  summary_stats <- resolved[["summary_stats"]]
  # Only compute quantile columns when requested via spread = "quantile".
  quantiles <- if (resolved[["want_quantile"]]) quantiles else numeric(0)

  validate_ensemble_args(n = n, cross = cross)
  normalized_conditions <- normalize_ensemble_conditions(
    object = object,
    conditions = conditions,
    cross = cross
  )
  conditions <- normalized_conditions[["conditions"]]
  n_conditions <- normalized_conditions[["n_conditions"]]

  total_sims <- n * n_conditions
  if (!quiet) {
    sim_word <- ifelse(total_sims == 1, "simulation", "simulations")
    if (is.null(conditions)) {
      msg <- c(
        "Starting ensemble simulation in {.val {title_case_ascii(language)}} with {.val {total_sims}} {sim_word}."
      )
    } else {
      cond_word <- ifelse(n_conditions == 1, "condition", "conditions")
      sim_per_word <- ifelse(n == 1, "simulation", "simulations")
      msg <- c(
        "Starting ensemble simulation in {.val {title_case_ascii(language)}} with {.val {total_sims}} {sim_word} in total.",
        "i" = "{.val {n_conditions}} {cond_word} x {.val {n}} {sim_per_word} per condition."
      )
    }
    cli::cli_inform(msg)
  }

  # Dispatch to language-specific backend
  if (language == "julia") {
    out <- ensemble_julia(
      object = object, n = n, save_sims = save_sims,
      conditions = conditions, cross = cross, quantiles = quantiles,
      summary_stats = summary_stats,
      only_stocks = only_stocks, vars = vars, quiet = quiet,
      n_conditions = n_conditions, total_sims = total_sims
    )
  } else if (language == "r") {
    out <- ensemble_r(
      object = object, n = n, save_sims = save_sims,
      conditions = conditions, cross = cross, quantiles = quantiles,
      summary_stats = summary_stats,
      only_stocks = only_stocks, vars = vars, quiet = quiet,
      n_conditions = n_conditions, total_sims = total_sims
    )
  } else {
    cli::cli_abort(c(
      "Unsupported simulation language: {.val {language}}.",
      "i" = "Ensemble simulations support {.code 'R'} and {.code 'Julia'}.",
      ">" = "Set via {.fn sim_settings}(object, language = {.code 'R'}) or {.code 'Julia'}."
    ))
  }

  # Echo the user-facing summary choices back on the object (mirroring how
  # `quantiles` is stored), so they are discoverable and so the validator can
  # re-derive the expected summary columns.
  out[["central"]] <- central
  out[["spread"]] <- spread

  validate_ensemble_stockflow(out)
  out
}


#' Resolve `central`/`spread` choices to internal summary statistics
#'
#' Maps the user-facing `central` and `spread` keywords to the subset of the
#' `ensemble_stat_funs` catalog to compute. `"range"` expands to `min` + `max`;
#' `"quantile"` is reported via `want_quantile` (handled via the `quantiles`
#' argument, not as a catalog statistic). `missing_count` is always included.
#'
#' @param central Canonicalised character vector (mean/median/none).
#' @param spread Canonicalised character vector (quantile/sd/range/none).
#' @returns List with `summary_stats` (character vector in catalog order) and
#'   `want_quantile` (logical).
#' @noRd
resolve_ensemble_stats <- function(central, spread) {
  central <- setdiff(central, "none")
  spread <- setdiff(spread, "none")

  spread_stats <- character(0)
  if ("sd" %in% spread) spread_stats <- c(spread_stats, "sd")
  if ("range" %in% spread) spread_stats <- c(spread_stats, "min", "max")

  stats <- unique(c(central, spread_stats, "missing_count"))
  # Order by catalog for consistency between the R and Julia backends.
  stats <- intersect(names(ensemble_stat_funs()), stats)

  list(
    summary_stats = stats,
    want_quantile = "quantile" %in% spread
  )
}


#' Validate ensemble control arguments
#'
#' Validates only the arguments handled directly by [ensemble()]. The summary
#' choices (`central`, `spread`, `quantiles`) are validated by [sim_settings()].
#'
#' @inheritParams ensemble
#'
#' @returns Invisibly returns NULL.
#' @noRd
validate_ensemble_args <- function(n, cross) {
  if (!is.numeric(n)) {
    abort_ensemble(c(
      "x" = "The {.arg n} argument must be {.cls numeric}.",
      "i" = "Received: {.cls {typeof(n)}}"
    ))
  }

  if (n <= 0) {
    abort_ensemble(c(
      "x" = "The {.arg n} argument must be greater than {.val {0}}."
    ))
  }

  if (!is.logical(cross)) {
    abort_ensemble(c(
      "x" = "The {.arg cross} argument must be {.code TRUE} or {.code FALSE}.",
      "i" = "Received: {.val {cross}}",
      ">" = "Use {.code cross = TRUE} for all combinations or {.code cross = FALSE} for paired values."
    ))
  }

  invisible(NULL)
}


#' Validate and normalize ensemble conditions
#'
#' @inheritParams ensemble
#'
#' @returns List with sorted `conditions` and scalar `n_conditions`.
#' @noRd
normalize_ensemble_conditions <- function(object, conditions, cross) {
  if (is.null(conditions)) {
    return(list(conditions = NULL, n_conditions = 1))
  }

  if (!is.list(conditions)) {
    abort_ensemble(c(
      "x" = "The {.arg conditions} argument must be a {.cls list}.",
      "i" = "Received: {.cls {typeof(conditions)}}",
      ">" = "Use format: {.code conditions = list(param1 = c(1, 2), param2 = c(10, 20))}."
    ))
  }

  if (length(conditions) == 0) {
    abort_ensemble(c(
      "x" = "The {.arg conditions} argument must have at least one parameter.",
      ">" = "Specify like: {.code conditions = list(param = c(1, 2, 3))}."
    ))
  }

  if (is.null(names(conditions))) {
    abort_ensemble(c(
      "x" = "The {.arg conditions} list elements must be named.",
      "i" = "Names correspond to parameter/stock names in your model.",
      ">" = "Use: {.code conditions = list(paramname = values, ...)}."
    ))
  }

  if (!all(vapply(conditions, is.numeric, logical(1)))) {
    abort_ensemble(c(
      "x" = "All {.arg conditions} elements must be {.cls numeric} vectors.",
      ">" = "Example: {.code conditions = list(param1 = c(1, 2, 3))}."
    ))
  }

  if (length(unique(names(conditions))) != length(conditions)) {
    abort_ensemble(c(
      "x" = "All {.arg conditions} names must be unique."
    ))
  }

  names_df <- get_names(object)
  names_conditions <- names(conditions)
  allowed_names <- names_df[names_df[["type"]] %in% c("stock", "constant"), "name"]

  idx <- names_conditions %in% names_df[["name"]]
  if (any(!idx)) {
    missing_names <- names_conditions[!idx]
    abort_ensemble(c(
      "x" = "Unknown parameters in {.arg conditions}.",
      "i" = "The following parameters do not exist in the model: {.code {missing_names}}.",
      ">" = "Available variables to vary: {.code {allowed_names}}."
    ))
  }

  idx <- names_conditions %in% allowed_names
  if (any(!idx)) {
    invalid_names <- names_conditions[!idx]
    abort_ensemble(c(
      "x" = "Flows or auxiliaries cannot be varied, only stocks and constants.",
      "i" = "Cannot vary: {.code {invalid_names}}.",
      ">" = "Available variables to vary: {.code {allowed_names}}"
    ))
  }

  conditions_lengths <- vapply(conditions, length, numeric(1))
  if (!cross) {
    if (length(unique(conditions_lengths)) != 1) {
      abort_ensemble(c(
        "x" = "Mismatched conditions lengths with {.arg cross = FALSE}.",
        "i" = "When {.arg cross = FALSE}, all conditions vectors must have equal length.",
        "i" = "Found lengths: {.code {unique(conditions_lengths)}} for parameters {.code {names(conditions)}}",
        ">" = "Either use {.code cross = TRUE} or ensure all conditions vectors have equal lengths."
      ))
    }

    n_conditions <- unique(conditions_lengths)
  } else {
    n_conditions <- prod(conditions_lengths)
  }

  list(
    conditions = conditions[sort(names(conditions))],
    n_conditions = n_conditions
  )
}


#' Abort from ensemble internals while reporting the public call
#'
#' @param message cli message vector.
#'
#' @returns Never returns; throws an error.
#' @noRd
abort_ensemble <- function(message) {
  cli::cli_abort(message, call = quote(ensemble()), .envir = parent.frame())
}


#' Check class [`ensemble_stockflow`][ensemble()]
#'
#' @param x An ensemble simulation of class [`ensemble_stockflow`][ensemble()]
#'
#' @returns Invisible x if valid, otherwise an error is thrown
#' @noRd
#'
check_ensemble_stockflow <- function(x) {
  if (!inherits(x, "ensemble_stockflow")) {
    cli::cli_abort(c(
      "x" = "Invalid object type.",
      "!" = "Expected object of class {.cls ensemble_stockflow}.",
      ">" = "Use {.fn ensemble} to create a valid ensemble object."
    ))
  }

  if (!x[["success"]]) {
    cli::cli_abort(c(
      "x" = "Ensemble simulation failed.",
      ">" = "Check your model specification and try again."
    ))
  }

  invisible(x)
}


#' @export
#' @concept ensemble
#' @method print ensemble_stockflow
print.ensemble_stockflow <- function(x, ...) {
  model_name <- x[["object"]][["meta"]][["name"]]
  default_name <- formals(meta)[["name"]]
  has_name <- !is.null(model_name) && nzchar(model_name) && model_name != default_name

  if (has_name) {
    cli::cli_h1("Stock-and-Flow Ensemble Simulation: {model_name}")
  } else {
    cli::cli_h1("Stock-and-Flow Ensemble Simulation")
  }

  has_counts <- all(!vapply(x[c("n_total", "n_conditions", "n")], is.null, logical(1)))
  if (has_counts) {
    cond_word <- if (x[["n_conditions"]] == 1) "condition" else "conditions"
    cli::cli_inform(c("i" = "{x[['n_total']]} total simulations \u2022 {x[['n_conditions']]} {cond_word} \u2022 {x[['n']]} per condition"))
  }

  if (x[["success"]]) {
    cli::cli_alert_success("Completed in {round(as.numeric(x[['duration']]), 4)} seconds")

    if (!is.null(x[["conditions"]])) {
      changed_pars <- setdiff(colnames(x[["conditions"]]), "condition")
      if (length(changed_pars) > 0) {
        cli::cli_inform(c(
          "i" = "Parameters changed across conditions: {paste0(changed_pars, collapse = ', ')}"
        ))
      }
    }

    has_df <- !is.null(x[["df"]])
    sims_saved <- if (has_df) "yes" else "no"
    cli::cli_inform(c(
      "i" = "Individual simulations saved: {sims_saved}"
    ))

    n_time <- NA_integer_
    if (!is.null(x[["summary"]]) && "time" %in% names(x[["summary"]])) {
      n_time <- length(unique(x[["summary"]][["time"]]))
    } else if (has_df && "time" %in% names(x[["df"]])) {
      n_time <- length(unique(x[["df"]][["time"]]))
    }
    if (!is.na(n_time)) {
      cli::cli_inform(c(
        "i" = "Time points saved per simulation: {n_time}"
      ))
    }
  } else {
    cli::cli_alert_danger("Simulation failed")
    cli::cli_inform(c(
      "i" = "Inspect the error message with: {.code x$error_message}"
    ))
  }
  invisible(x)
}


#' Create data frame of simulation results
#'
#' Convert simulation results to a data.frame.
#'
#' @inheritParams plot.simulate_stockflow
#' @inheritParams as.data.frame.simulate_stockflow
#' @param which Type of data to return. Either `"summary"` for a summary statistics, or `"sims"` for individual simulation trajectories. Defaults to `"summary"`.
#' @param sim Indices of the individual trajectories to include if which = `"sims"`. Defaults to `NULL`, which includes all trajectories. Including a high number of trajectories will create a large object.
#' @param condition Indices of the conditions to include. Defaults to `NULL`, which includes all conditions.
#' @param row.names NULL or a character vector giving the row names for the data frame. Missing values are not allowed.
#' @param optional Ignored parameter.
#'
#' @returns A data.frame with simulation results. For \code{direction = "long"} (default),
#'   the data frame has three columns: \code{time}, \code{variable}, and \code{value}.
#'   For \code{direction = "wide"}, the data frame has columns \code{time} followed by
#'   one column per variable.
#' @export
#' @concept ensemble
#' @method as.data.frame ensemble_stockflow
#' @seealso [`ensemble()`][ensemble()], [stockflow()]
#'
#' @examples
#' sfm <- stockflow("sir") |>
#'   # Randomize initial values of all stocks to show variation in the ensemble
#'   update(c(susceptible, infected, recovered),
#'          eqn = runif(1, min = 20, max = 800))
#' 
#' # Run ensemble simulation with 3 simulations,
#' # saving only 20 timepoints per simulation
#' sims <- ensemble(sfm, n = 3, save_length = 20, save_sims = TRUE)
#' 
#' # Get summary statistics in long format
#' df <- as.data.frame(sims)
#' head(df, n = 1)
#'
#' # Get summary statistics in wide format
#' df_wide <- as.data.frame(sims, direction = "wide")
#' head(df_wide, n = 1)
#' 
#' # Get individual simulations in wide format
#' df_wide_sims <- as.data.frame(sims, which = "sims", direction = "wide")
#' head(df_wide_sims, n = 1)
#'
as.data.frame.ensemble_stockflow <- function(
  x, row.names = NULL,
  optional = FALSE,
  which = c("summary", "sims")[1],
  direction = "long",
  sim = NULL,
  condition = NULL,
  vars = NULL, type = NULL,
  ...
) {
  vars <- .expr_to_char(rlang::enexpr(vars))
  check_ensemble_stockflow(x)

  which <- .clean_which(which)

  direction <- trimws(tolower(direction))
  if (!direction %in% c("long", "wide")) {
    cli::cli_abort(c(
      "Invalid {.arg direction} argument.",
      "x" = "Must be either {.code 'long'} or {.code 'wide'}."
    ))
  }

  # Validate condition
  if (!is.null(condition)) {
    .check_condition_index(condition, x[["n_conditions"]])
  }

  constants_requested <- FALSE
  if (is.null(vars) && !is.null(type)) {
    type <- .validate_type_arg(type, arg_name = "type")
    constants_requested <- "constant" %in% type
    if (constants_requested && length(type) > 1L) {
      cli::cli_abort(c(
        "Invalid {.arg type} argument.",
        "x" = "Constants are stored separately from time-series output.",
        "i" = "Use {.code type = 'constant'} by itself to return constants."
      ))
    }
  }

  if (which == "sims") {
    if (constants_requested) {
      df <- x[["constants"]][["df"]]
      if (is.null(df)) {
        cli::cli_abort(c(
          "No individual constant data available.",
          "!" = "Re-run {.fn ensemble} with {.code save_sims = TRUE}."
        ))
      }
    } else if (is.null(x[["df"]])) {
      cli::cli_abort(c(
        "No individual simulation data available.",
        "!" = "Re-run {.fn ensemble} with {.code save_sims = TRUE}."
      ))
    } else {
      df <- x[["df"]]
    }

    # Validate and apply sim filter
    if (!is.null(sim)) {
      .check_sim_index(sim, x[["n"]])
      df <- df[df[["sim"]] %in% sim, , drop = FALSE]
    }

    # Apply condition filter
    if (!is.null(condition)) {
      df <- df[df[["condition"]] %in% condition, , drop = FALSE]
    }

    if (!constants_requested) {
      # Filter by variable and/or type
      df <- .filter_long_by_vars_type(df, x[["object"]], vars = vars, type = type)
    }

    if (direction == "wide") {
      df <- stats::reshape(df,
        timevar = "variable",
        idvar = if (constants_requested) c("condition", "sim") else c("condition", "sim", "time"),
        direction = "wide"
      )
      names(df) <- sub("^value\\.", "", names(df))
      rownames(df) <- NULL
    }
  } else if (which == "summary") {
    if (!is.null(sim)) {
      cli::cli_inform(c(
        "sim" = "The {.arg sim} argument is ignored for {.code which = 'summary'}.",
        ">" = "Set {.code which = 'sims'} to filter by individual trajectory."
      ))
    }
    df <- if (constants_requested) x[["constants"]][["summary"]] else x[["summary"]]

    # Apply condition filter
    if (!is.null(condition)) {
      df <- df[df[["condition"]] %in% condition, , drop = FALSE]
    }

    if (!constants_requested) {
      # Filter by variable and/or type
      df <- .filter_long_by_vars_type(df, x[["object"]], vars = vars, type = type)
    }

    if (direction == "wide") {
      df <- stats::reshape(df,
        timevar = "variable",
        idvar = if (constants_requested) "condition" else c("condition", "time"),
        direction = "wide"
      )
      rownames(df) <- NULL
    }
  }

  if (!is.null(row.names)) {
    if (length(row.names) != nrow(df)) {
      cli::cli_abort(c(
        "x" = "Length mismatch in {.arg row.names}.",
        "i" = "Got {length(row.names)} name{?s} but {nrow(df)} row{?s}."
      ))
    }
    rownames(df) <- row.names
  }

  df
}

#' @export
#' @concept ensemble
#' @method head ensemble_stockflow
head.ensemble_stockflow <- function(x, n = 6L, ...) {
  df <- as.data.frame(x, ...)
  head(df, n)
}


#' @export
#' @concept ensemble
#' @method tail ensemble_stockflow
tail.ensemble_stockflow <- function(x, n = 6L, ...) {
  df <- as.data.frame(x, ...)
  tail(df, n)
}


#' @export
#' @concept ensemble
#' @method summary ensemble_stockflow
summary.ensemble_stockflow <- function(object, ...) {
  check_ensemble_stockflow(object)
  object[["summary"]]
}


#' Constructor for [`ensemble_stockflow`][ensemble()]
#'
#' @noRd
new_ensemble_stockflow <- function(success = FALSE,
                                   error_message = NULL,
                                   df = NULL,
                                   summary = NULL,
                                   n = NULL,
                                   n_total = NULL,
                                   n_conditions = NULL,
                                   conditions = NULL,
                                   init = NULL,
                                   constants = NULL,
                                   script = NULL,
                                   duration = NULL,
                                   cross = TRUE,
                                   quantiles = NULL,
                                   central = NULL,
                                   spread = NULL,
                                   object = NULL) {
  obj <- list(
    success = success,
    error_message = error_message,
    df = df,
    summary = summary,
    n = n,
    n_total = n_total,
    n_conditions = n_conditions,
    conditions = conditions,
    init = init,
    constants = constants,
    script = script,
    duration = duration,
    cross = cross,
    quantiles = quantiles,
    central = central,
    spread = spread,
    object = object
  )
  structure(obj, class = "ensemble_stockflow")
}


#' Deep validator for [`ensemble_stockflow`][ensemble()]
#'
#' @noRd
validate_ensemble_stockflow <- function(x) {
  if (!inherits(x, "ensemble_stockflow")) {
    cli::cli_abort(c(
      "x" = "Invalid object type.",
      "!" = "Expected object of class {.cls ensemble_stockflow}.",
      ">" = "Use {.fn ensemble} to create a valid ensemble object."
    ))
  }

  # Check all required fields are present
  required <- c(
    "success", "error_message", "df", "summary", "n", "n_total",
    "n_conditions", "conditions", "init", "constants", "script",
    "duration", "cross", "quantiles", "central", "spread", "object"
  )
  missing_fields <- setdiff(required, names(x))
  if (length(missing_fields) > 0) {
    cli::cli_abort(c(
      "x" = "Ensemble object is missing fields.",
      "!" = "Missing: {.field {missing_fields}}."
    ))
  }

  if (!is.logical(x[["success"]]) || length(x[["success"]]) != 1) {
    cli::cli_abort(c(
      "x" = "Field {.field success} should be a single {.cls logical} value.",
      "i" = "Received: {.cls {class(x[['success']])}} with length {length(x[['success']])}."
    ))
  }

  if (x[["success"]]) {
    # Validate summary structure: grouping columns plus the requested stats,
    # resolved from the stored central/spread choices.
    resolved <- resolve_ensemble_stats(x[["central"]], x[["spread"]])
    expected_summary_cols <- c(
      "condition", "variable", "time", resolved[["summary_stats"]]
    )
    missing_summary_cols <- setdiff(expected_summary_cols, names(x[["summary"]]))
    if (length(missing_summary_cols) > 0) {
      cli::cli_abort(c(
        "x" = "Ensemble {.arg summary} is missing expected columns.",
        "!" = "Missing: {.field {missing_summary_cols}}."
      ))
    }

    # Quantile columns are only expected when quantiles were requested (i.e.
    # spread included "quantile"). An empty `quantiles` field means none.
    if (length(x[["quantiles"]]) > 0) {
      q_cols <- grep("^quant[0-9]+$", names(x[["summary"]]), value = TRUE)
      if (length(q_cols) == 0) {
        cli::cli_abort(c(
          "x" = "Ensemble {.arg summary} has no quantile columns.",
          "i" = "Expected columns named {.code quant1}, {.code quant2}, etc."
        ))
      }
    }

    # Validate individual sims df structure (if present)
    if (!is.null(x[["df"]])) {
      expected_df_cols <- c("sim", "condition", "variable", "time", "value")
      missing_df_cols <- setdiff(expected_df_cols, names(x[["df"]]))
      if (length(missing_df_cols) > 0) {
        cli::cli_abort(c(
          "x" = "Ensemble {.arg df} is missing expected columns.",
          "!" = "Missing: {.field {missing_df_cols}}."
        ))
      }
    }

    # Validate n fields are numeric
    for (field in c("n", "n_total", "n_conditions")) {
      if (!is.null(x[[field]]) && !is.numeric(x[[field]])) {
        cli::cli_abort(c(
          "x" = "Field {.field {field}} should be {.cls numeric}.",
          "i" = "Received: {.cls {class(x[[field]])}}"
        ))
      }
    }

    if (is.null(x$summary) || !is.data.frame(x$summary)) {
      cli::cli_abort(c(
        "x" = "Missing or invalid summary data.",
        "i" = "Successful ensemble must have a {.cls data.frame} in {.arg summary}."
      ))
    }
  } else if (!x$success) {
    if (is.null(x$error_message) || !nzchar(x$error_message)) {
      cli::cli_abort(c(
        "x" = "Missing error message.",
        "i" = "Failed ensemble must have a non-empty {.arg error_message}."
      ))
    }
  }

  invisible(x)
}
