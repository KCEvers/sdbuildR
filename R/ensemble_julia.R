# ==============================================================================
# Julia ensemble backend
# ==============================================================================

#' Run ensemble simulation via Julia
#'
#' Called by [ensemble()] when language is Julia.
#'
#' @inheritParams ensemble
#' @param n_conditions Integer; number of conditions.
#' @param total_sims Integer; total simulations across all conditions.
#'
#' @returns Object of class [`ensemble_stockflow`][ensemble()]
#' @noRd
ensemble_julia <- function(object, n, save_sims, conditions, cross,
                           quantiles, summary_stats, only_stocks, vars = NULL,
                           quiet, n_conditions, total_sims) {
  use_julia(quiet = quiet)

  # Evaluate script
  start_t <- Sys.time()

  # Order requested stats by catalog order for consistency with the R backend.
  summary_stats <- intersect(names(ensemble_stat_funs()), summary_stats)

  # Create ensemble parameters
  ensemble_pars <- list(
    n = n,
    quantiles = quantiles,
    summary_stats = summary_stats,
    save_sims = save_sims,
    conditions = conditions,
    cross = cross
  )

  ensemble_pars[["threaded"]] <- .sdbuildR_env[["jl"]][["use_threads"]]

  # Get output filepaths
  ensemble_pars[["filepath_df"]] <- c(
    "df" = get_tempfile(fileext = ".csv"),
    "constants" = get_tempfile(fileext = ".csv"),
    "init" = get_tempfile(fileext = ".csv")
  )
  ensemble_pars[["filepath_summary"]] <- c(
    "df" = get_tempfile(fileext = ".csv"),
    "constants" = get_tempfile(fileext = ".csv"),
    "init" = get_tempfile(fileext = ".csv")
  )
  filepath <- get_tempfile(fileext = ".jl")

  # Compile ensemble script - returns list with script and modified object
  result <- compile_ensemble(object,
    ensemble_pars = ensemble_pars,
    only_stocks = only_stocks
  )

  script <- result$script
  object <- result$object # Get updated object with cache populated
  seed_nr <- object[["sim_settings"]][["seed"]]

  write_script(script, filepath)
  script <- paste0(readLines(filepath), collapse = "\n")

  on.exit(
    # Ensure files are deleted even if an error occurs
    remove_files(c(
      filepath,
      ensemble_pars[["filepath_df"]],
      ensemble_pars[["filepath_summary"]]
    )),
    add = TRUE
  )

  # Evaluate script
  sim <- tryCatch(
    {
      # Wrap in invisible and capture.output to keep Julia output quiet
      invisible(utils::capture.output(
        julia_eval(jl_include_command(filepath, seed_nr))
      ))

      end_t <- Sys.time()

      if (!quiet) {
        elapsed <- round(as.numeric(end_t) - as.numeric(start_t), 4)
        cli::cli_inform(c(
          "v" = "Ensemble simulation completed in {.val {elapsed}} seconds."
        ))
      }

      # Read the number of simulations per condition
      n_val <- julia_eval(P[["ensemble_n"]])

      # Read the ensemble conditions
      if (!is.null(ensemble_pars[["conditions"]])) {
        cond_matrix <- julia_eval(
          paste0("Matrix(hcat(", P[["ensemble_pars"]], "...)')")
        )
        colnames(cond_matrix) <- names(conditions)
        cond_matrix <- cbind(condition = seq_len(nrow(cond_matrix)), cond_matrix)
      } else {
        cond_matrix <- NULL
      }

      constants_out <- list()
      init_out <- list()

      # Get stock names for filtering when only_stocks = TRUE
      stock_names <- get_variables_by_type(object, "stock")[["name"]]

      # Read the simulation results
      if (save_sims) {
        # First check whether files exist
        if (!file.exists(ensemble_pars[["filepath_df"]][["df"]]) ||
          !file.exists(ensemble_pars[["filepath_df"]][["constants"]]) ||
          !file.exists(ensemble_pars[["filepath_df"]][["init"]])) {
          cli::cli_abort(c(
            "x" = "Julia simulation failed to store output files.",
            ">" = "Try running a smaller ensemble to see if the issue persists."
          ))
        } else {
          # Read the simulation results
          df <- read_sim_csv(ensemble_pars[["filepath_df"]][["df"]])
          constants_out[["df"]] <- read_sim_csv(ensemble_pars[["filepath_df"]][["constants"]])
          init_out[["df"]] <- read_sim_csv(ensemble_pars[["filepath_df"]][["init"]])

          # Filter to stocks only if requested
          if (only_stocks) {
            df <- df[df[["variable"]] %in% stock_names, , drop = FALSE]
          }
          df <- filter_sim_df_vars(df, vars)
        }
      } else {
        df <- NULL
      }

      # Read the summary file
      summary_df <- read_sim_csv(ensemble_pars[["filepath_summary"]][["df"]])
      constants_out[["summary"]] <- read_sim_csv(ensemble_pars[["filepath_summary"]][["constants"]])
      init_out[["summary"]] <- read_sim_csv(ensemble_pars[["filepath_summary"]][["init"]])

      # Filter summary to stocks only if requested
      if (only_stocks) {
        summary_df <- summary_df[
          summary_df[["variable"]] %in% stock_names, ,
          drop = FALSE
        ]
      }
      summary_df <- filter_sim_df_vars(summary_df, vars)

      new_ensemble_stockflow(
        success = TRUE,
        df = df,
        summary = summary_df,
        n = n_val,
        n_total = total_sims,
        n_conditions = n_conditions,
        conditions = cond_matrix,
        init = init_out,
        constants = constants_out,
        script = script,
        duration = end_t - start_t,
        cross = cross,
        quantiles = quantiles,
        object = object
      )
    },
    error = function(e) {
      cli::cli_warn(c(
        "!" = "Julia execution failed.",
        "i" = "An error occurred while running the Julia script.",
        ">" = "Error: {e[['message']]}"
      ))
      new_ensemble_stockflow(
        success = FALSE,
        error_message = e[["message"]],
        script = script,
        object = object
      )
    }
  )

  sim
}
