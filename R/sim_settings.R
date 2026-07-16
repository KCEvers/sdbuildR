#' Modify simulation specifications
#'
#' Simulation specifications are the settings that determine how the model is
#' simulated, such as the integration method (i.e., solver), start and stop time,
#' and timestep. Modify these specifications for an existing stock-and-flow model.
#'
#' @inheritParams update.stockflow
#' @param method Integration method. Defaults to `"euler"`.
#' @param start Start time of simulation. Defaults to `0`.
#' @param stop End time of simulation. Defaults to `100`.
#' @param dt Timestep of solver; controls simulation accuracy. Smaller = more
#'   accurate but slower. Defaults to `0.01`.
#' @param save_by Save output at a regular interval, mirroring the `by` argument
#'   of [seq()]: output times are `seq(start, stop, by = save_by)`. Must be a
#'   single number `>= dt`. Use a value larger than `dt` to reduce output size
#'   without sacrificing accuracy. Example: `dt = 0.01`, `save_by = 1` saves every
#'   100th computed point. Pass `NA`, `NULL`, or `""` to reset to saving all dt
#'   steps. Mutually exclusive with `save_times` and `save_length`. Defaults to
#'   `NULL` (save all).
#' @param save_times Numeric vector of explicit output times to save. Values must
#'   lie within `[start, stop]`. Example: `save_times = c(0, 10)` saves exactly
#'   those two times. Pass `NA`, `NULL`, or `""` to reset to saving all dt steps.
#'   Mutually exclusive with `save_by` and `save_length`. Defaults to `NULL`
#'   (save all).
#' @param save_length Number of output times to save, mirroring the `length.out`
#'   argument of [seq()]: output times are `seq(start, stop, length.out =
#'   save_length)`. Must be a single whole positive number. `save_length = 1`
#'   saves only the final time point (stop). Pass `NA`, `NULL`, or `""` to reset
#'   to saving all dt steps. Mutually exclusive with `save_by` and `save_times`.
#'   Defaults to `NULL` (save all).
#' @param seed Seed number to ensure reproducibility across runs in case of
#'   random elements. Must be an integer. Defaults to `NULL` (no seed).
#' @param time_units Simulation time unit. Defaults to `"seconds"`.
#' @param language Coding language in which to simulate model. Either `"R"` or
#'   `"Julia"`. Defaults to `"R"`.
#' @param only_stocks If `TRUE`, only return stocks in output, discarding flows
#'   and auxiliaries. If `FALSE`, flows and auxiliaries are saved, which slows
#'   down the simulation. Defaults to `TRUE`.
#' @param vars Character vector of variable names to save in simulation output.
#'   Use this to keep selected flows or auxiliaries without saving every model
#'   variable. If specified, this overrides `only_stocks`. Pass `NULL`, `""`,
#'   or an empty character vector to clear a previous `vars` setting.
#' @param keep_nonnegative_stock If `TRUE`, keeps original non-negativity setting
#'   of stocks. Defaults to `FALSE`.
#' @param keep_nonnegative_flow If `TRUE`, keeps original non-negativity setting
#'   of flows. Defaults to `TRUE`.
#' @param save_sims If `TRUE`, individual simulations are retained in
#'   [ensemble()] output. Defaults to `FALSE`.
#' @param central Central-tendency statistics to compute across simulations in
#'   [ensemble()]. One or more of `"mean"`, `"median"`, or `"none"` (compute no
#'   central tendency). Each requested statistic becomes a column in the ensemble
#'   summary. Defaults to `c("mean", "median")`. Can be overridden per call via
#'   the `central` argument of [ensemble()].
#' @param spread Spread (uncertainty) statistics to compute across simulations in
#'   [ensemble()]. One or more of `"quantile"` (quantile columns at the
#'   probabilities given by `quantiles`), `"sd"` (standard deviation), `"range"`
#'   (minimum and maximum, returned as `min`/`max` columns), or `"none"` (compute
#'   no spread). Defaults to `"quantile"`. Can be overridden per call via the
#'   `spread` argument of [ensemble()].
#' @param quantiles Quantile probabilities to compute when `spread` includes
#'   `"quantile"`, e.g. `c(0.025, 0.975)`. Returned as columns `quant1`,
#'   `quant2`, ... in the order given. At least two unique values are required.
#'   Defaults to `c(0.025, 0.975)`. Can be overridden per call via the
#'   `quantiles` argument of [ensemble()].
#' @param ... Reserved for future use. Passing unknown arguments (including the
#'   removed `save_at` and `save_n`) raises an error.
#'
#' @returns A stock-and-flow model object of class [`stockflow`][stockflow]
#' @concept simulate
#' @seealso [sim_methods()]
#' @export
#'
#' @examples
#' sfm <- stockflow("predator_prey") |>
#'   sim_settings(start = 0, stop = 50, dt = 0.1)
#' sim <- simulate(sfm)
#' plot(sim)
#'
#' # Change the simulation method to "rk4"
#' sfm <- sim_settings(sfm, method = "rk4")
#'
#' # Change the time units to "years", such that one time unit is one year
#' sfm <- sim_settings(sfm, time_units = "years")
#'
#' # Save at a regular interval to reduce output size without affecting accuracy
#' sfm <- sim_settings(sfm, save_by = 1)
#' sim <- simulate(sfm)
#' head(as.data.frame(sim))
#'
#' # Save exactly 11 evenly-spaced time points (t=0, 5, 10, ..., 50)
#' sfm <- sim_settings(sfm, save_length = 11)
#' sim <- simulate(sfm)
#' head(as.data.frame(sim))
#'
#' # Save only specific time points
#' sfm <- sim_settings(sfm, save_times = c(0, 25, 50))
#' sim <- simulate(sfm)
#' as.data.frame(sim)
#'
#' # Add stochastic initial condition but specify seed to obtain same result
#' sfm <- sim_settings(sfm, seed = 1) |>
#'   update(c(predator, prey), eqn = runif(1, 20, 50))
#'
sim_settings <- function(object,
                         method = "euler",
                         start = 0,
                         stop = 100,
                         dt = 0.01,
                         save_by = NULL,
                         save_times = NULL,
                         save_length = NULL,
                         seed = NULL,
                         time_units = "seconds",
                         language = "R",
                         only_stocks = TRUE,
                         vars = NULL,
                         keep_nonnegative_stock = FALSE,
                         keep_nonnegative_flow = TRUE,
                         save_sims = FALSE,
                         central = c("mean", "median"),
                         spread = "quantile",
                         quantiles = c(0.025, 0.975),
                         ...) {
  # Basic check
  if (missing(object)) {
    missing_arg("object")
  }
  check_stockflow(object)


  dots <- list(...)
  if (length(dots) > 0) {
    dot_names <- names(dots) %||% rep("", length(dots))
    # legacy <- intersect(dot_names, c("save_at", "save_n"))
    # if (length(legacy) > 0) {
    #   cli::cli_abort(c(
    #     "{.arg {legacy}} {?is/are} no longer supported.",
    #     "i" = "Use {.arg save_by} (regular interval, mirrors {.code seq(by=)}), {.arg save_times} (explicit output times), or {.arg save_length} (number of output times, mirrors {.code seq(length.out=)}).",
    #     ">" = "Replace {.code save_at = <interval>} with {.arg save_by}, {.code save_at = <vector>} with {.arg save_times}, and {.code save_n} with {.arg save_length}."
    #   ))
    # }
    unknown <- dot_names[!nzchar(dot_names)] # dot_names[!nzchar(dot_names) | !dot_names %in% c("save_at", "save_n")]
    cli::cli_abort(c(
      "Unknown argument{?s} passed to {.fn sim_settings}: {.arg {unknown}}."
    ))
  }


  # --- Time argument validation ---
  user_time <- list()
  if (!missing(start)) user_time$start <- start
  if (!missing(stop)) user_time$stop <- stop
  if (!missing(dt)) user_time$dt <- dt

  if (length(user_time) > 0) {
    # Merge object defaults with user values, then validate the combined state
    eff_time <- list(
      start = object[["sim_settings"]][["start"]],
      stop  = object[["sim_settings"]][["stop"]],
      dt    = object[["sim_settings"]][["dt"]]
    )
    eff_time[names(user_time)] <- user_time
    time_vals <- .validate_sim_time_args(eff_time)
  } else {
    time_vals <- list()
  }

  # --- Save parameter handling ---

  # The public API exposes three mutually exclusive ways to choose which time
  # points are saved (save_by / save_times / save_length). These are stored under
  # the same names internally; at most one is ever non-NULL.
  save_set <- character(0)
  if (!missing(save_by) && !.is_unset(save_by)) save_set <- c(save_set, "save_by")
  if (!missing(save_times) && !.is_unset(save_times)) save_set <- c(save_set, "save_times")
  if (!missing(save_length) && !.is_unset(save_length)) save_set <- c(save_set, "save_length")

  if (length(save_set) > 1) {
    cli::cli_abort(c(
      "Cannot specify more than one of {.arg save_by}, {.arg save_times}, and {.arg save_length}.",
      "x" = "You set {.arg {save_set}}.",
      ">" = "Choose one method for specifying which time points are saved."
    ))
  }

  # save argg: entries added here go into object$sim_settings
  argg <- time_vals

  save_passed <- !missing(save_by) || !missing(save_times) || !missing(save_length)
  if (save_passed) {
    # Clear all three; at most one is set below. Absence of all three means
    # "save every dt step".
    argg["save_by"] <- list(NULL)
    argg["save_times"] <- list(NULL)
    argg["save_length"] <- list(NULL)
    if (identical(save_set, "save_by")) {
      argg[["save_by"]] <- .validate_save_by(save_by, object, time_vals)
    } else if (identical(save_set, "save_times")) {
      argg[["save_times"]] <- .validate_save_times(save_times, object, time_vals)
    } else if (identical(save_set, "save_length")) {
      argg[["save_length"]] <- .validate_save_length(save_length)
    }
  }

  # Ensure time_units are formatted correctly
  if (!missing(time_units)) {
    if (length(time_units) != 1) {
      cli::cli_abort(c(
        "Invalid {.arg time_units} argument.",
        "x" = "The {.arg time_units} argument must be a single {.cls character} string."
      ))
    }

    # Time units are merely the x-axis label; can be whatever the user specifies as long as it is a string
    time_units <- as.character(time_units)
  }

  # Validate method
  method_auto_set <- FALSE
  if (!missing(method)) {
    if (is.null(method) || any(is.na(method)) || !inherits(method, "character") || length(method) > 1) {
      cli::cli_abort(c(
        "Invalid {.arg method} argument.",
        "x" = "The {.arg method} argument must be a single {.cls character} string."
      ))
    }
    method <- trimws(method)
  }

  # Check coding language and translate method if needed
  if (!missing(language)) {
    language <- clean_language(language)

    old_language <- object[["sim_settings"]][["language"]]
    if (missing(method) && language != old_language) {
      # Method not specified: translate the current method to the new language
      method <- sim_methods(object[["sim_settings"]][["method"]], from = old_language, to = language)
      method_auto_set <- TRUE
    } else if (!missing(method)) {
      # Method specified: validate it against the new language
      method <- sim_methods(method, from = language)
    }
  } else if (!missing(method)) {
    # Language not changing: validate method against the current language
    method <- sim_methods(method, from = object[["sim_settings"]][["language"]])
  }

  # Seed must be NULL or an integer
  if (!missing(seed) && !is.null(seed)) {
    if (is.na(seed)) {
      seed <- NULL
    } else if (isFALSE(seed)) {
      seed <- NULL
    } else if (nzchar(seed)) {
      seed <- strtoi(seed)
      if (is.na(seed)) {
        cli::cli_abort(c(
          "Invalid {.arg seed} argument.",
          "x" = "The {.arg seed} argument must be an {.cls integer}."
        ))
      }
      seed <- as.character(seed)
    } else {
      seed <- NULL
    }
  }

  # Validate only_stocks
  if (!missing(only_stocks)) {
    if (!is.logical(only_stocks) || length(only_stocks) != 1 || is.na(only_stocks)) {
      cli::cli_abort(c(
        "Invalid {.arg only_stocks} argument.",
        "x" = "The {.arg only_stocks} argument must be {.code TRUE} or {.code FALSE}."
      ))
    }
  }

  # Validate vars
  if (!missing(vars)) {
    vars <- validate_sim_vars(object, vars)
  }

  # Validate ensemble summary choices (runtime-only; do not affect base codegen)
  if (!missing(central)) central <- .validate_central(central)
  if (!missing(spread)) spread <- .validate_spread(spread)
  if (!missing(quantiles)) quantiles <- .validate_ensemble_quantiles(quantiles)

  # Collect remaining validated args
  if (!missing(method) || method_auto_set) argg$method <- method
  if (!missing(time_units)) argg$time_units <- time_units
  if (!missing(language)) argg$language <- language
  if (!missing(only_stocks)) argg$only_stocks <- only_stocks
  if (!missing(vars)) argg["vars"] <- list(vars)
  if (!missing(keep_nonnegative_stock)) argg$keep_nonnegative_stock <- keep_nonnegative_stock
  if (!missing(keep_nonnegative_flow)) argg$keep_nonnegative_flow <- keep_nonnegative_flow
  if (!missing(central)) argg$central <- central
  if (!missing(spread)) argg$spread <- spread
  if (!missing(quantiles)) argg$quantiles <- quantiles
  # seed handled separately: c() preserves NULL elements, unlike $ assignment
  if (!missing(seed)) argg <- c(argg, list(seed = seed))

  # Check if language is changing
  language_changed <- "language" %in% names(argg) &&
    argg[["language"]] != object[["sim_settings"]][["language"]]

  # Overwrite simulation specifications (use list-subset assignment to preserve NULLs)
  for (nm in names(argg)) {
    object[["sim_settings"]][nm] <- list(argg[[nm]])
  }

  # If language changed, clear entire cache and regenerate equation strings
  if (language_changed) {
    object <- invalidate_assemble(object, "all")

    if (nrow(object[["variables"]])) {
      object <- prep_equations_variables(object)
      object <- prep_stock_change(object)
    }
  }

  # Invalidate only for settings that affect the base generated code. Runtime
  # output choices (seed, vars, only_stocks, save_sims) are handled at compile or
  # simulate time and do not invalidate the base cache.
  if (!language_changed && any(names(argg) %in% codegen_sim_setting_names())) {
    object <- invalidate_assemble(object, "all")
  }

  object <- sanitize_stockflow(object)

  # --- save_sims handling (meta-setting, does NOT invalidate cache) ---
  if (!missing(save_sims)) {
    if (!is.logical(save_sims) || length(save_sims) != 1L || is.na(save_sims)) {
      cli::cli_abort(c(
        "Invalid {.arg save_sims} argument.",
        "x" = "The {.arg save_sims} argument must be {.code TRUE} or {.code FALSE}.",
        "i" = "Received: {.val {save_sims}}"
      ))
    }
    object[["sim_settings"]][["save_sims"]] <- save_sims
    # Intentionally do not call invalidate_assemble(): this is a meta-setting only
  }

  # Pre-assemble script components so they're available for modification before
  # simulate() (deferrable; see maybe_pre_assemble)
  object <- maybe_pre_assemble(object)

  object
}


#' @noRd
.validate_sim_numeric <- function(x, arg_name) {
  x <- suppressWarnings(as.numeric(x))
  if (is.na(x)) {
    cli::cli_abort(c(
      "Invalid {.arg {arg_name}} argument.",
      "x" = "The {.arg {arg_name}} argument must be {.cls numeric}."
    ))
  }
  x
}


#' Test whether a save parameter value should be treated as "unset"
#' @noRd
.is_unset <- function(x) {
  is.null(x) || (length(x) == 1 && (is.na(x) || identical(x, "")))
}


#' Validate cross-checked simulation time arguments
#'
#' Receives the effective merged state of start/stop/dt (object defaults
#' overwritten by user overrides), coerces to numeric, runs cross-argument
#' checks, and strips scientific notation.
#'
#' @param args Named list with keys `start`, `stop`, `dt` (all three required).
#' @returns The validated, formatted named list.
#' @noRd
.validate_sim_time_args <- function(args) {
  for (nm in c("start", "stop", "dt")) {
    args[[nm]] <- .validate_sim_numeric(args[[nm]], nm)
  }

  if (args$dt <= 0) {
    cli::cli_abort(c(
      "Invalid {.arg dt} argument.",
      "x" = "{.arg dt} argument must be positive."
    ))
  }

  if (args$dt != 1 && args$dt > 0.1) {
    cli::cli_warn(c(
      "Large timestep detected ({.arg dt} = {.val {args$dt}}).",
      "i" = "This may lead to simulation inaccuracies.",
      ">" = "Consider using smaller timesteps for better accuracy."
    ))
  }

  if (args$start >= args$stop) {
    cli::cli_abort(c(
      "Invalid time interval.",
      "x" = "{.arg start} ({.val {args$start}}) must be smaller than {.arg stop} ({.val {args$stop}})."
    ))
  }

  if (args$dt > (args$stop - args$start)) {
    cli::cli_abort(c(
      "Invalid {.arg dt} argument.",
      "x" = "{.arg dt} ({.val {args$dt}}) must be smaller than the time interval ({.arg stop} - {.arg start} = {.val {args$stop - args$start}})."
    ))
  }

  for (nm in c("start", "stop", "dt")) {
    args[[nm]] <- replace_digits_with_floats(scientific_notation(args[[nm]]), NULL)
  }

  args
}


#' Validate the save_by argument (regular save interval)
#'
#' Returns a character scalar (interval). Mirrors `seq(..., by = save_by)`.
#'
#' @param save_by Numeric scalar supplied by the user.
#' @param object A `stockflow` model object (for fallback effective values).
#' @param time_vals Named list of validated time args (start/stop/dt), may be empty.
#' @noRd
.validate_save_by <- function(save_by, object, time_vals) {
  eff_start <- as.numeric(time_vals[["start"]] %||% object[["sim_settings"]][["start"]])
  eff_stop <- as.numeric(time_vals[["stop"]] %||% object[["sim_settings"]][["stop"]])
  eff_dt <- as.numeric(time_vals[["dt"]] %||% object[["sim_settings"]][["dt"]])

  if (length(save_by) != 1) {
    cli::cli_abort(c(
      "Invalid {.arg save_by} argument.",
      "x" = "{.arg save_by} must be a single number (a regular save interval).",
      ">" = "To save specific time points, use {.arg save_times} instead."
    ))
  }

  val <- suppressWarnings(as.numeric(save_by))
  if (is.na(val)) {
    cli::cli_abort(c(
      "Invalid {.arg save_by} argument.",
      "x" = "{.arg save_by} must be numeric."
    ))
  }
  if (val <= 0) {
    cli::cli_abort(c(
      "Invalid {.arg save_by} argument.",
      "x" = "{.arg save_by} argument must be positive."
    ))
  }

  # save_by < dt → auto-correct to dt
  if (val < eff_dt) {
    cli::cli_warn(c(
      "Invalid {.arg save_by} and {.arg dt} relationship.",
      "x" = "{.arg save_by} ({.val {val}}) must be >= {.arg dt} ({.val {eff_dt}}).",
      "i" = "Automatically setting {.arg save_by} equal to {.arg dt}."
    ))
    val <- eff_dt
  }

  # Warn if stop doesn't align with the interval
  remainder <- (eff_stop - eff_start) %% val
  tol <- sqrt(.Machine$double.eps) * max(abs(eff_stop), 1)
  if (remainder > tol && abs(remainder - val) > tol) {
    cli::cli_warn(c(
      "Endpoint may be missing.",
      "i" = paste0(
        "{.arg stop} ({.val {eff_stop}}) may not appear in output ",
        "({.arg stop} is not a multiple of {.arg save_by} = {.val {val}})."
      ),
      ">" = "If {.arg stop} should be included, use {.arg save_length} instead of {.arg save_by}, or list explicit time points with {.arg save_times}."
    ))
  }

  replace_digits_with_floats(scientific_notation(val), NULL)
}


#' Validate the save_times argument (explicit output times)
#'
#' Returns a character vector of output times (sorted, deduplicated).
#'
#' @param save_times Numeric vector supplied by the user.
#' @param object A `stockflow` model object (for fallback effective values).
#' @param time_vals Named list of validated time args (start/stop/dt), may be empty.
#' @noRd
.validate_save_times <- function(save_times, object, time_vals) {
  eff_start <- as.numeric(time_vals[["start"]] %||% object[["sim_settings"]][["start"]])
  eff_stop <- as.numeric(time_vals[["stop"]] %||% object[["sim_settings"]][["stop"]])

  vals <- suppressWarnings(as.numeric(save_times))
  if (any(is.na(vals))) {
    cli::cli_abort(c(
      "Invalid {.arg save_times} argument.",
      "x" = "All {.arg save_times} values must be numeric."
    ))
  }
  vals <- sort(unique(vals))
  out_of_range <- vals < eff_start | vals > eff_stop
  if (any(out_of_range)) {
    bad <- vals[out_of_range]
    cli::cli_abort(c(
      "Invalid {.arg save_times} values.",
      "x" = "All values must be within [{.val {eff_start}}, {.val {eff_stop}}].",
      ">" = "Out-of-range: {.val {bad}}."
    ))
  }
  vapply(vals, function(x) {
    replace_digits_with_floats(scientific_notation(x), NULL)
  }, character(1))
}


#' Validate the save_length argument (number of output times)
#'
#' Returns a character integer string. Mirrors `seq(..., length.out = save_length)`.
#'
#' @param save_length Single whole positive number supplied by the user.
#' @noRd
.validate_save_length <- function(save_length) {
  bad <- function() {
    cli::cli_abort(c(
      "Invalid {.arg save_length} argument.",
      "x" = "{.arg save_length} must be a single whole positive number (the number of output times).",
      "i" = "Output times are {.code seq(start, stop, length.out = save_length)}; {.arg save_length} = 1 saves only the final time point."
    ))
  }

  if (length(save_length) != 1) bad()
  val <- suppressWarnings(as.numeric(save_length))
  if (is.na(val) || val < 1 || val != round(val)) bad()
  as.character(as.integer(val))
}


#' Validate the ensemble `central` argument
#'
#' Canonicalises lenient spellings and checks the values against the allowed set.
#'
#' @param central Character vector (or `FALSE`) supplied by the user.
#' @returns Canonicalised character vector.
#' @noRd
.validate_central <- function(central) {
  if (isFALSE(central)) central <- "none"
  if (!is.character(central) || length(central) == 0) {
    cli::cli_abort(c(
      "Invalid {.arg central} argument.",
      "x" = "The {.arg central} argument must be a non-empty {.cls character} vector.",
      "i" = "Choose one or more of {.val mean}, {.val median}, or {.val none}."
    ))
  }
  central <- normalize_synonyms(central, central_synonyms())
  invalid <- setdiff(central, c("mean", "median", "none"))
  if (length(invalid) > 0) {
    cli::cli_abort(c(
      "Invalid {.arg central} value{?s}: {.val {invalid}}.",
      "i" = "The {.arg central} argument must be one or more of {.val mean}, {.val median}, or {.val none}."
    ))
  }
  central
}


#' Validate the ensemble `spread` argument
#'
#' Canonicalises lenient spellings and checks the values against the allowed set.
#'
#' @param spread Character vector (or `FALSE`) supplied by the user.
#' @returns Canonicalised character vector.
#' @noRd
.validate_spread <- function(spread) {
  if (isFALSE(spread)) spread <- "none"
  if (!is.character(spread) || length(spread) == 0) {
    cli::cli_abort(c(
      "Invalid {.arg spread} argument.",
      "x" = "The {.arg spread} argument must be a non-empty {.cls character} vector.",
      "i" = "Choose one or more of {.val quantile}, {.val sd}, {.val range}, or {.val none}."
    ))
  }
  spread <- normalize_synonyms(spread, spread_synonyms())
  invalid <- setdiff(spread, c("quantile", "sd", "range", "none"))
  if (length(invalid) > 0) {
    cli::cli_abort(c(
      "Invalid {.arg spread} value{?s}: {.val {invalid}}.",
      "i" = "The {.arg spread} argument must be one or more of {.val quantile}, {.val sd}, {.val range}, or {.val none}."
    ))
  }
  spread
}


#' Validate the ensemble `quantiles` argument
#'
#' @param quantiles Numeric vector of probabilities supplied by the user.
#' @returns The validated numeric vector.
#' @noRd
.validate_ensemble_quantiles <- function(quantiles) {
  if (!is.numeric(quantiles)) {
    cli::cli_abort(c(
      "Invalid {.arg quantiles} argument.",
      "x" = "The {.arg quantiles} argument must be {.cls numeric}.",
      ">" = "Use a numeric vector, e.g., {.code quantiles = c(0.025, 0.975)}."
    ))
  }
  if (length(unique(quantiles)) < 2) {
    cli::cli_abort(c(
      "Invalid {.arg quantiles} argument.",
      "x" = "The {.arg quantiles} argument must have at least {.val {2}} unique values.",
      ">" = "Provide at least 2 quantiles, e.g., {.code quantiles = c(0.025, 0.975)}."
    ))
  }
  if (any(quantiles < 0 | quantiles > 1)) {
    cli::cli_abort(c(
      "Invalid {.arg quantiles} argument.",
      "x" = "All values in {.arg quantiles} must be between {.val {0}} and {.val {1}}.",
      ">" = "Use values like {.code c(0.025, 0.5, 0.975)} for 2.5%, 50%, 97.5% quantiles."
    ))
  }
  quantiles
}
