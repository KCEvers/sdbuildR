#' Registry of supported ensemble summary statistics
#'
#' Single source of truth for the named summary statistics that [ensemble()] can
#' compute. The names (in this order) define the column order of the summary
#' output. The user-facing `central`/`spread` choices in [ensemble()] resolve to
#' a subset of these via `resolve_ensemble_stats()`. The Julia backend mirrors
#' this catalog (see `ensemble_summ` in SystemDynamicsBuildR.jl); it also defines
#' a `var` statistic, which the R side no longer requests.
#'
#' @noRd
ensemble_stat_funs <- function() {
  list(
  mean          = function(v) mean(v, na.rm = TRUE),
  median        = function(v) stats::median(v, na.rm = TRUE),
  sd            = function(v) stats::sd(v, na.rm = TRUE),
  min           = function(v) min(v, na.rm = TRUE),
  max           = function(v) max(v, na.rm = TRUE),
  missing_count = function(v) sum(is.na(v))
)
}


#' Compute summary stats for a numeric vector
#'
#' @param vals Numeric vector.
#' @param stats Character vector of summary statistic names (subset of
#'   `names(ensemble_stat_funs())`), in catalog order.
#' @param quantiles Numeric vector of quantile probabilities.
#' @param q_names Character vector of quantile column names.
#' @returns Named list of summary statistics.
#' @noRd
ensemble_summary_stats <- function(vals, stats, quantiles, q_names) {
  c(
    lapply(ensemble_stat_funs()[stats], function(f) f(vals)),
    stats::setNames(
      lapply(quantiles, function(q) {
        stats::quantile(vals, probs = q, na.rm = TRUE, names = FALSE)
      }),
      q_names
    )
  )
}


#' Summarize a long data frame by groups
#'
#' Groups `df` by the columns in `by` and computes summary stats on the `value`
#' column using data.table. `keyby` returns the rows sorted by the grouping keys
#' (matching the previous base-R ordering), and `ensemble_summary_stats()`
#' returns a named list per group that data.table expands into the columns
#' `c(by, stats, q_names)`, in that order.
#'
#' @param df Data frame (or data.table) with a `value` column and the grouping columns.
#' @param by Character vector of grouping column names.
#' @param stats Character vector of summary statistic names, in catalog order.
#' @param quantiles Numeric vector of quantile probabilities.
#' @param q_names Character vector of quantile column names.
#' @returns Data frame of summary statistics.
#' @importFrom data.table .SD
#' @noRd
summarise_by <- function(df, by, stats, quantiles, q_names) {
  if (nrow(df) == 0L) {
    empty <- data.frame(matrix(
      ncol = length(by) + length(stats) + length(q_names),
      nrow = 0L
    ))
    names(empty) <- c(by, stats, q_names)
    return(empty)
  }
  dt <- data.table::as.data.table(df)
  # `.SDcols = "value"` keeps `.SD` to the single value column per group, so we
  # avoid a bare `value` symbol (which would be an R CMD check NOTE).
  out <- dt[, ensemble_summary_stats(.SD[[1L]], stats, quantiles, q_names),
    keyby = by, .SDcols = "value"
  ]
  as.data.frame(out)
}


#' Apply with progressr progress reporting
#'
#' @param X List to iterate over.
#' @param FUN Function to apply to each element of `X`.
#' @param quiet Logical; if TRUE, suppress progress output.
#' @param apply_fun Apply function, e.g. [lapply()] or
#'   [future.apply::future_lapply()].
#' @param handler A progressr progression handler.
#' @param ... Additional arguments passed to `apply_fun`.
#' @returns A list, as in [lapply()].
#' @noRd
progressr_lapply <- function(X, FUN, quiet, apply_fun = lapply,
                             handler = progressr::handler_cli(), ...) {
  if (length(X) == 0L) {
    return(apply_fun(X, FUN, ...))
  }

  run <- function() {
    progress <- progressr::progressor(
      steps = length(X),
      label = "Simulations",
      enable = !quiet
    )

    apply_fun(X, function(x) {
      out <- FUN(x)
      progress()
      out
    }, ...)
  }

  if (quiet) {
    progressr::without_progress(run())
  } else {
    progressr::with_progress(
      run(),
      handlers = handler,
      enable = TRUE
    )
  }
}


#' Run ensemble simulation in R
#'
#' Called by [ensemble()] when language is R. Runs multiple simulations using
#' `simulate()`, optionally in parallel via `future.apply::future_lapply()` when
#' an active `future::plan()` has multiple workers.
#'
#' @inheritParams ensemble
#' @param n_conditions Integer; number of conditions.
#' @param total_sims Integer; total simulations across all conditions.
#'
#' @returns Object of class [`ensemble_stockflow`][ensemble()]
#' @noRd
ensemble_r <- function(object, n, save_sims, conditions, cross,
                       quantiles, summary_stats, only_stocks, vars = NULL,
                       quiet, n_conditions, total_sims) {
  # Find seed if specified
  has_seed <- !is.null(object[["sim_settings"]][["seed"]])
  if (has_seed) {
    # withr::local_seed(seed_nr)
    seed_nr <- as.numeric(object[["sim_settings"]][["seed"]])
  } else {
    seed_nr <- TRUE
  }

  start_t <- Sys.time()

  # Build conditions grid
  if (!is.null(conditions)) {
    if (cross) {
      cond_grid <- expand.grid(conditions, KEEP.OUT.ATTRS = FALSE)
    } else {
      cond_grid <- as.data.frame(conditions)
    }
    # Sort columns alphabetically (conditions was already sorted)
    cond_grid <- cond_grid[, sort(names(cond_grid)), drop = FALSE]
    cond_matrix <- cbind(condition = seq_len(nrow(cond_grid)), cond_grid)
  } else {
    cond_grid <- NULL
    cond_matrix <- NULL
  }

  # Use parallel apply only if the user configured an active future plan with
  # more than one worker.
  use_par <- future::nbrOfWorkers() > 1L
  apply_fun <- if (use_par) future.apply::future_lapply else lapply

  # --- Pre-compile scripts (avoids redundant compile() per simulation) --------
  # Compile the base model once to populate the assembly cache
  base_compiled <- compile(object, only_stocks = only_stocks, vars = vars)
  object <- base_compiled$object

  if (is.null(cond_grid)) {
    # No conditions: one script for all runs
    scripts <- list(base_compiled$script)
  } else {
    # With conditions: one script per condition
    # Starting from the cached object so only invalidated parts (static) rebuild
    scripts <- vector("list", n_conditions)
    for (j_idx in seq_len(n_conditions)) {
      obj_mod <- object
      for (nm in names(cond_grid)) {
        val <- cond_grid[j_idx, nm]
        obj_mod <- update(obj_mod, !!nm, eqn = !!val)
      }
      compiled <- compile(obj_mod, only_stocks = only_stocks, vars = vars)
      scripts[[j_idx]] <- compiled$script
    }
  }

  # Parse once per condition to avoid parse() overhead per simulation run
  parsed_scripts <- lapply(scripts, function(script) parse(text = script))

  # Build task list: list of (condition, sim) pairs
  tasks <- vector("list", total_sims)
  idx <- 0L
  for (j_idx in seq_len(n_conditions)) {
    for (i_idx in seq_len(n)) {
      idx <- idx + 1L
      tasks[[idx]] <- list(condition = j_idx, sim = i_idx)
    }
  }


  # Run simulations (each task just evals a pre-compiled script)
  sim_results <- tryCatch(
    {
      if (use_par) {
        run_par <- function() {
          progressr_lapply(tasks, function(task) {
            # Resolve the internal worker function from the namespace on the
            # worker itself (sdbuildR is loaded via future.packages). Serializing
            # it as a captured global strips its namespace environment, which
            # breaks lookups of internal objects such as `P` (object 'P' not
            # found).
            eval_sim <- utils::getFromNamespace("eval_sim_script_r", "sdbuildR")
            eval_sim(
              parsed_expr = parsed_scripts[[task[["condition"]]]],
              condition = task[["condition"]], sim = task[["sim"]]
            )
          }, quiet = quiet, apply_fun = apply_fun,
          future.seed = seed_nr, future.packages = c("sdbuildR", "progressr"))
        }
        # With a numeric seed, per-future seeds are derived deterministically, so
        # restore the global RNG to avoid leaking state. Without a seed,
        # future.seed = TRUE consumes the global RNG so that consecutive runs
        # differ; preserving it would make them identical.
        if (has_seed) withr::with_preserve_seed(run_par()) else run_par()
      } else {
        do_run <- function() {
          progressr_lapply(tasks, function(task) {
            eval_sim_script_r(
              parsed_expr = parsed_scripts[[task[["condition"]]]],
              condition = task[["condition"]], sim = task[["sim"]]
            )
          }, quiet = quiet)
        }

        if (has_seed) withr::with_seed(seed_nr, do_run()) else do_run()
      }
    },
    error = function(e) {
      cli::cli_warn(c(
        "!" = "An error occurred during R ensemble simulation.",
        "i" = "Error: {e[['message']]}"
      ))
      structure(list(), error_message = e[["message"]])
    }
  )

  end_t <- Sys.time()

  if (length(sim_results) == 0L) {
    err_msg <- attr(sim_results, "error_message") %||%
      "Ensemble simulation failed."
    return(new_ensemble_stockflow(
      success = FALSE,
      error_message = err_msg,
      object = object
    ))
  }

  # Check for any failed simulations
  failed <- vapply(sim_results, function(x) !x[["success"]], logical(1))
  if (all(failed)) {
    return(new_ensemble_stockflow(
      success = FALSE,
      error_message = sim_results[[1]][["error_message"]],
      object = object
    ))
  }
  if (any(failed)) {
    n_failed <- sum(failed)
    cli::cli_warn(c(
      "!" = "{n_failed}/{total_sims} simulations failed.",
      "i" = "First error: {sim_results[failed][[1]][['error_message']]}"
    ))
  }

  # Filter to only successful results
  good_results <- sim_results[!failed]

  if (!quiet) {
    elapsed <- round(as.numeric(end_t) - as.numeric(start_t), 4)
    cli::cli_inform(c(
      "v" = "Ensemble simulation completed in {.val {elapsed}} seconds."
    ))
  }

  # Assemble results
  assemble_ensemble_results_r(
    good_results = good_results,
    n = n, n_conditions = n_conditions, total_sims = total_sims,
    save_sims = save_sims, only_stocks = only_stocks,
    vars = vars,
    summary_stats = summary_stats, quantiles = quantiles, cross = cross,
    cond_matrix = cond_matrix, object = object,
    duration = end_t - start_t
  )
}


#' Evaluate a pre-compiled R simulation script
#'
#' Lightweight alternative to run_single_sim_r() that skips compilation
#' entirely. The script is evaluated in a fresh environment; stochastic
#' equations (e.g., `runif()`) produce different results on each call.
#'
#' @param parsed_expr Parsed R expression for a compiled simulation script.
#' @param condition Integer; condition index.
#' @param sim Integer; simulation index.
#'
#' @returns List with success, df, init, constants, condition, sim
#' @noRd
eval_sim_script_r <- function(parsed_expr, condition, sim) {
  envir <- new.env()
  tryCatch(
    {
      eval(parsed_expr, envir = envir)
      list(
        success = TRUE,
        df = envir[[P[["sim_df_name"]]]],
        init = unlist(envir[[P[["initial_value_name"]]]]),
        constants = unlist(Filter(Negate(is.function), envir[[P[["parameter_name"]]]])),
        condition = condition, sim = sim
      )
    },
    error = function(e) {
      list(
        success = FALSE,
        error_message = e[["message"]],
        condition = condition, sim = sim
      )
    }
  )
}


#' Assemble R ensemble results into ensemble_stockflow object
#'
#' @param good_results List of successful simulation results
#' @param n Simulations per condition
#' @param n_conditions Number of conditions
#' @param total_sims Total simulations
#' @param save_sims Logical; include individual sim data
#' @param only_stocks Logical; filter to stocks only
#' @param quantiles Numeric vector of quantiles
#' @param cross Logical; crossed design
#' @param cond_matrix Data frame of conditions or NULL
#' @param object Stock-and-flow model
#' @param duration Time elapsed
#'
#' @returns Object of class [`ensemble_stockflow`][ensemble()]
#' @noRd
assemble_ensemble_results_r <- function(good_results, n, n_conditions,
                                        total_sims, save_sims,
                                        only_stocks, vars = NULL,
                                        summary_stats, quantiles, cross,
                                        cond_matrix, object, duration) {
  # Build individual simulation data frames
  all_dfs <- vector("list", length(good_results))
  all_init <- vector("list", length(good_results))
  all_constants <- vector("list", length(good_results))

  for (k in seq_along(good_results)) {
    res <- good_results[[k]]
    df_k <- res[["df"]]
    df_k[["sim"]] <- res[["sim"]]
    df_k[["condition"]] <- res[["condition"]]
    all_dfs[[k]] <- df_k

    # Init values
    init_vals <- res[["init"]]
    all_init[[k]] <- data.frame(
      sim = res[["sim"]], condition = res[["condition"]],
      variable = names(init_vals),
      value = unname(init_vals),
      stringsAsFactors = FALSE
    )

    # Constants
    const_vals <- res[["constants"]]
    if (length(const_vals) == 0L) {
      all_constants[[k]] <- data.frame(
        sim = integer(0),
        condition = integer(0),
        variable = character(0),
        value = numeric(0),
        stringsAsFactors = FALSE
      )
    } else {
      all_constants[[k]] <- data.frame(
        sim = res[["sim"]], condition = res[["condition"]],
        variable = names(const_vals),
        value = unname(const_vals),
        stringsAsFactors = FALSE
      )
    }
  }

  combined_df <- data.table::rbindlist(all_dfs, use.names = TRUE)
  combined_df <- filter_sim_df_vars(combined_df, vars)
  combined_init <- data.table::rbindlist(all_init, use.names = TRUE)
  combined_constants <- data.table::rbindlist(all_constants, use.names = TRUE)

  # Compute summary statistics using base R (avoids data.table [.data.table
  # dispatch issues when the package is not attached)
  # Quantile columns are named positionally (quant1, quant2, ...) in the order
  # of `quantiles`; the probabilities are recovered from the object's
  # `quantiles` field. Order requested stats by catalog order for consistency
  # with the Julia backend.
  summary_stats <- intersect(names(ensemble_stat_funs()), summary_stats)
  # Guard against length-0 quantiles: paste0("quant", integer(0)) returns
  # "quant" (length 1), not character(0), which would desync q_names from the
  # (empty) quantile list.
  q_names <- if (length(quantiles) > 0) paste0("quant", seq_along(quantiles)) else character(0)

  summary_df <- summarise_by(
    combined_df,
    by = c("condition", "variable", "time"),
    stats = summary_stats, quantiles = quantiles, q_names = q_names
  )

  # Build init summary
  init_summary <- summarise_by(
    combined_init,
    by = c("condition", "variable"),
    stats = summary_stats, quantiles = quantiles, q_names = q_names
  )

  # Build constants summary
  constants_summary <- summarise_by(
    combined_constants,
    by = c("condition", "variable"),
    stats = summary_stats, quantiles = quantiles, q_names = q_names
  )

  # Prepare init and constants output
  init_out <- list(summary = init_summary)
  constants_out <- list(summary = constants_summary)

  if (save_sims) {
    df_out <- as.data.frame(combined_df)
    init_out[["df"]] <- as.data.frame(combined_init)
    constants_out[["df"]] <- as.data.frame(combined_constants)
  } else {
    df_out <- NULL
  }

  new_ensemble_stockflow(
    success = TRUE,
    df = df_out,
    summary = summary_df,
    n = n,
    n_total = total_sims,
    n_conditions = n_conditions,
    conditions = cond_matrix,
    init = init_out,
    constants = constants_out,
    script = NULL,
    duration = duration,
    cross = cross,
    quantiles = quantiles,
    object = object
  )
}
