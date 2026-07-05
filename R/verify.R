# ==============================================================================
# VERIFY GENERIC
# ==============================================================================

#' Verify model behavior with unit tests
#'
#' @param object A model object to verify.
#' @param ... Additional arguments passed to specific methods.
#' @export
#' @concept unitTest
verify <- function(object, ...) {
  UseMethod("verify")
}


#' Verify unit tests against simulation results
#'
#' Run all active unit tests defined on a stock-and-flow model. Use
#' [unit_test()] to define tests; use [unit_tests()] to display them.
#'
#' Calling `verify()` on a `stockflow` model will first simulate the model, then
#' run all tests — including those that require re-simulation under alternative
#' [conditions][unit_test()]. Simulations are always retained in the returned
#' object so that [plot.verify_stockflow()] works without any extra arguments.
#'
#' For repeated-run robustness testing use [ensemble()] instead.
#'
#' @param object An [`stockflow`][stockflow] object.
#' @param test Integer vector of test number(s) to run (numbers-based, as shown by [unit_tests()]).
#'   Defaults to `NULL` (run all tests).
#' @param ... Additional arguments passed to [sim_settings()] (e.g., `seed`,
#'   `dt`).
#'
#' @returns An object of class `verify_stockflow`, returned invisibly. Use
#'   [as.data.frame()] to extract results as a data frame and [plot()] to
#'   visualize the simulations used. The object contains:
#'   \describe{
#'     \item{results}{List of test result entries, one per test (including inactive
#'       tests, which appear with `status = "skip"`). Each entry has `label`,
#'       `expr_str`, `conditions`, `status`, `error_type`, `message`, and `outcome`.}
#'     \item{object}{The `stockflow` model the tests were run against.}
#'     \item{sims}{Nested list of `simulate_stockflow` objects used internally by
#'       [plot.verify_stockflow()]. Always present (never `NULL`).}
#'     \item{condition}{Named integer vector mapping each test label to its condition
#'       index. Used internally by [plot.verify_stockflow()].}
#'     \item{n_conditions}{Number of unique simulation conditions.}
#'     \item{test_indices}{Integer vector of the original 1-based test numbers that
#'       were run (as shown by [unit_tests()]). Equal to `seq_along(results)` when
#'       `test = NULL` (all tests run).}
#'   }
#'
#' @export
#' @concept unitTest
#' @method verify stockflow
#' @seealso [unit_test()], [unit_tests()], [simulate.stockflow()],
#'   [as.data.frame.verify_stockflow()], [plot.verify_stockflow()]
#'
#' @examples
#' sfm <- stockflow("sir") |>
#'   unit_test(expr = all(susceptible >= 0)) |>
#'   unit_test(
#'     label = "recovered increases over time",
#'     expr = all(diff(recovered) >= 0)
#'   )
#'
#' verify(sfm)
verify.stockflow <- function(object, test = NULL, ...) {
  check_stockflow(object)

  # Override sim_settings with any arguments passed via ...
  varargs <- list(...)
  if (length(varargs) > 0) {
    object <- do.call(sim_settings, c(list(object), varargs))
  }

  check_summary_diagnostics(object)

  tests <- object[["unit_tests"]]
  if (length(tests) == 0) {
    cli::cli_abort(c(
      "x" = "No unit tests defined.",
      ">" = "Add tests with {.fn unit_test}."
    ))
    return(invisible(new_verify_stockflow(
      success = FALSE,
      error_message = "No unit tests defined.",
      results = list(), object = object
    )))
  }

  # Subset tests if test supplied
  test_indices <- seq_along(tests)
  if (!is.null(test)) {
    .check_test_index(test, length(tests))
    tests <- tests[test]
    test_indices <- as.integer(test)
  }

  # Group tests by unique conditions to minimise re-simulations.
  # Use a sentinel for the no-conditions baseline.
  .BASELINE <- ".__baseline__."

  condition_keys <- vapply(tests, function(t) {
    if (length(t[["conditions"]]) == 0) {
      return(.BASELINE)
    }
    paste(names(t[["conditions"]]), unlist(t[["conditions"]]),
      sep = "=", collapse = ";"
    )
  }, character(1))

  unique_keys <- unique(condition_keys)
  n_conditions <- length(unique_keys)

  # All test simulations should return the same variables
  # Get test dependencies to determine which variables are needed for the simulations
  td <- get_test_deps(object)
  object <- td[["object"]]
  deps <- td[["deps"]]

  # Compute all variables referenced by tests across the whole test set
  all_refs <- unique(unlist(lapply(seq_along(deps), function(k) deps[[k]][["expr_refs"]])))

  # Get specified vars and only_stocks; vars overrides only_stocks
  vars <- object[["sim_settings"]][["vars"]]
  only_stocks <- object[["sim_settings"]][["only_stocks"]]

  # Compute sim_vars with precedence:
  # 1) If user provided `vars` explicitly -> include those + test refs
  if (!is.null(vars)) {
    sim_vars <- sort(unique(c(vars, all_refs)))
    # 2) Else if user set only_stocks = FALSE -> simulate all model variables
  } else if (!only_stocks) {
    all_model_vars <- get_model_var(object)
    sim_vars <- sort(all_model_vars)
    # 3) Else (user did not specify vars, and only_stocks = TRUE) -> default to stocks + test refs
  } else {
    df <- get_names(object)
    all_stock_names <- df[df[["type"]] == "stock", "name"]
    sim_vars <- sort(unique(c(all_stock_names, all_refs)))
  }

  # Set sim_vars in simulation specifications. Each simulate() call receives the same centrally-computed `sim_vars` so all conditions return consistent variable sets.
  object <- sim_settings(object, vars = sim_vars)

  # Run one simulation per unique condition set.
  sim_cache <- vector("list", n_conditions)
  for (j_idx in seq_along(unique_keys)) {
    key <- unique_keys[[j_idx]]

    if (key == .BASELINE) {
      obj_for_cond <- object
    } else {
      obj_for_cond <- object
      first_test_idx <- which(condition_keys == key)[[1]]
      conds <- tests[[first_test_idx]][["conditions"]]
      for (nm in names(conds)) {
        eqn_val <- as.character(conds[[nm]])
        obj_for_cond <- rlang::inject(update(obj_for_cond, name = !!nm, eqn = !!eqn_val))
      }
    }

    # simulate() takes care of seed handling internally, so we don't need to do anything special here for reproducibility
    sim_cache[[j_idx]] <- simulate(obj_for_cond)
  }

  # Evaluate each test against the single simulation for its condition.
  results <- lapply(tests, function(test) {
    key <- if (length(test[["conditions"]]) == 0) {
      .BASELINE
    } else {
      paste(names(test[["conditions"]]), unlist(test[["conditions"]]), sep = "=", collapse = ";")
    }
    j_idx <- match(key, unique_keys)
    .run_one_unit_test(test, sim_cache[[j_idx]])
  })

  # condition always populated: named integer vector mapping test label -> condition index
  condition <- stats::setNames(
    match(condition_keys, unique_keys),
    vapply(tests, function(t) t[["label"]], character(1))
  )

  sims <- sim_cache

  result_obj <- new_verify_stockflow(
    success = TRUE,
    results = results, object = object,
    sims = sims, condition = condition,
    n_conditions = n_conditions,
    test_indices = test_indices
  )
  # invisible(result_obj)
  result_obj
}


check_verify_stockflow <- function(x) {
  if (!inherits(x, "verify_stockflow")) {
    cli::cli_abort(c(
      "x" = "Invalid object class.",
      "i" = "This is not an object of class {.cls verify_stockflow}.",
      ">" = "Generate a unit test run with {.fn verify}."
    ))
  }

  if (!x[["success"]]) {
    cli::cli_abort(c(
      "x" = "Unit test run failed.",
      ">" = "Check your model specification and try again."
    ))
  }

  invisible(x)
}


# ==============================================================================
# UNIT TEST BUILDER
# ==============================================================================

#' Add or modify unit tests
#'
#' Unit tests are assertions about model behavior that can be evaluated against
#' simulation results. For example, you might assert that a stock remains
#' non-negative, or that a certain variable reaches a threshold by the end of
#' the simulation. Unit tests can be added to a model such that they can be
#' evaluated with [verify()]. All unit tests can be displayed with
#' `unit_tests()`.
#'
#' The `expr` argument accepts a plain logical expression:
#' - **Logical**: `all(S >= 0)`, `cor(D, C) < -.5`.
#'
#' When `label` is omitted, a human-readable label is generated automatically
#' by parsing the expression (e.g., `all(S >= 0)` →
#' `"S is at least 0 (for all values)"`).
#'
#' @section Adding vs. modifying:
#' - **Add** a new test: omit `test` (and provide a `label` that does not match
#'   any existing test, or omit `label` to auto-generate one).
#' - **Modify** an existing test by number: supply `test` (integer).
#' - **Modify** an existing test by label: supply a `label` that matches an
#'   existing test (without specifying `test`).
#'
#' When modifying, only the arguments you explicitly supply are changed; all
#' other fields keep their current value.
#'
#' @section Uniqueness:
#' Labels must be unique across all unit tests. An error is thrown if a new
#' or modified label would create a duplicate. Expressions must also be unique;
#' an error is thrown if an identical `expr` already exists on another test.
#'
#' @inheritParams update.stockflow
#' @param test Integer number of the test to modify. Must be a positive integer
#'   (a warning is issued and the value rounded when a non-integer is
#'   supplied). When `test` exceeds the current number of tests a warning is
#'   issued and a new test is appended instead. Can be omitted when adding a
#'   new test.
#' @param expr An expression to evaluate against simulation results. Variable
#'   names in the expression refer to model variables; each resolves to a
#'   numeric vector of time-series values. Required when adding a new test;
#'   optional when modifying (keeps the current expression if omitted).
#' @param label A descriptive label for the test. If omitted when adding,
#'   auto-generated from `expr`. If omitted when modifying, the current label
#'   is kept. Labels must be unique.
#' @param conditions A named list of constant or initial stock overrides used
#'   when evaluating this test. If non-empty, [verify.stockflow()] will
#'   re-simulate the model with these parameter values before evaluating `expr`.
#' @param active If `FALSE`, the test is defined but skipped during
#'   [verify()]. Defaults to `TRUE`.
#'
#' @returns The model object with the unit test added or modified, invisibly.
#'
#' @export
#' @concept unitTest
#' @seealso [verify()], [unit_tests()], [discard_unit_test()]
#'
#' @examples
#' sfm <- stockflow("sir") |>
#'   unit_test(expr = all(susceptible >= 0))
#'
#' # Run unit tests
#' verify(sfm)
#'
#' # Add test with label
#' sfm <- unit_test(sfm,
#'   label = "recovered increases",
#'   expr = all(diff(recovered) >= 0)
#' )
#' verify(sfm)
#'
#' # Add test with conditions
#' sfm <- unit_test(sfm,
#'   expr = all(infected == infected[1]),
#'   label = "When infection_rate is zero, no one gets infected",
#'   conditions = list(infection_rate = 0)
#' )
#' verify(sfm)
#'
#' # View all tests
#' unit_tests(sfm)
#'
#' # Deactivate test test 1
#' sfm <- unit_test(sfm, test = 1, active = FALSE)
#' verify(sfm)
#'
#' # Modify test by label, e.g., to change the expression
#' sfm <- unit_test(sfm,
#'   label = "recovered increases over time",
#'   expr = all(diff(recovered) > -1)
#' )
#' verify(sfm)
#'
unit_test <- function(object, test, expr, label, conditions = list(), active = TRUE) {
  check_stockflow(object)

  tests <- object[["unit_tests"]]
  n_tests <- length(tests)

  # --- Capture arguments & missingness flags ---
  expr_captured <- rlang::enexpr(expr)
  expr_missing <- missing(expr)
  label_missing <- missing(label)
  cond_missing <- missing(conditions)
  active_missing <- missing(active)

  .normalize_expr <- function(expr_obj) {
    if (is.character(expr_obj)) {
      if (length(expr_obj) != 1L || is.na(expr_obj) || !nzchar(trimws(expr_obj))) {
        cli::cli_abort(c(
          "Invalid {.arg expr} argument.",
          "x" = "Character {.arg expr} must be a single non-empty string."
        ))
      }
      parsed_all <- tryCatch(
        parse(text = expr_obj, keep.source = FALSE),
        error = function(e) {
          cli::cli_abort(c(
            "x" = "Expression has invalid R syntax.",
            "!" = "Failed to parse: {.code {expr_obj}}.",
            "i" = "{conditionMessage(e)}"
          ))
        }
      )
      if (length(parsed_all) != 1L) {
        cli::cli_abort(c(
          "x" = "Invalid {.arg expr} argument.",
          "!" = "Character {.arg expr} must contain exactly one expression."
        ))
      }
      expr_obj <- parsed_all[[1L]]
    }

    expr_str_local <- paste(deparse(expr_obj, width.cutoff = 500L), collapse = " ")
    parsed_roundtrip <- tryCatch(
      parse(text = expr_str_local, keep.source = FALSE),
      error = function(e) {
        cli::cli_abort(c(
          "x" = "Expression has invalid R syntax.",
          "!" = "Failed to parse: {.code {expr_str_local}}.",
          "i" = "{conditionMessage(e)}"
        ))
      }
    )
    if (length(parsed_roundtrip) != 1L) {
      cli::cli_abort(c(
        "x" = "Invalid {.arg expr} argument.",
        "!" = "Expression must contain exactly one expression."
      ))
    }

    list(expr_obj = parsed_roundtrip[[1L]], expr_str = expr_str_local)
  }

  # --- Validate test ---
  test_missing <- missing(test)
  if (!test_missing) {
    if (!is.numeric(test) || length(test) != 1L || is.na(test)) {
      cli::cli_abort(c(
        "x" = "Invalid {.arg test} argument.",
        "!" = "{.arg test} must be a single integer."
      ))
    }
    if (test != round(test)) {
      test_old <- test
      test <- as.integer(round(test))
      cli::cli_warn(c(
        "x" = "{.arg test} must be an integer.",
        "!" = "{.val {test_old}} will be replaced by [{test}]."
      ))
    }
    test <- as.integer(test)
    if (test < 1L) {
      cli::cli_abort(c(
        "x" = "Invalid {.arg test} argument.",
        "!" = "{.arg test} must be a positive integer."
      ))
    }
  }

  # --- Determine mode: add vs. modify ---
  modifying <- FALSE
  modify_pos <- NULL

  if (!test_missing) {
    if (test <= n_tests) {
      modifying <- TRUE
      modify_pos <- test
    } else {
      next_nr <- n_tests + 1L
      if (test > next_nr) {
        cli::cli_warn(c(
          "!" = "Invalid {.arg test} ({.val {test}}).",
          "i" = "{.arg test} does not need to be specified when adding a new test.",
          ">" = "{.arg test} will be set to the existing number of tests + 1 ({.arg test} = {.val {next_nr}})."
        ))
      }
    }
  } else if (!label_missing && is.character(label) && length(label) == 1L) {
    existing_labels <- if (n_tests > 0) {
      vapply(tests, function(t) t[["label"]], character(1))
    } else {
      character(0)
    }
    pos <- match(label, existing_labels)
    if (!is.na(pos)) {
      modifying <- TRUE
      modify_pos <- pos
    }
  }

  # --- Resolve fields (partial update when modifying) ---
  if (modifying) {
    current <- tests[[modify_pos]]

    if (!expr_missing) {
      normalized <- .normalize_expr(expr_captured)
      expr_captured <- normalized[["expr_obj"]]
      expr_str <- normalized[["expr_str"]]
    } else {
      expr_str <- current[["expr_str"]]
      expr_captured <- parse(text = expr_str, keep.source = FALSE)[[1]]
    }

    if (label_missing) {
      label <- current[["label"]]
    }

    if (cond_missing) {
      conditions <- current[["conditions"]]
    }

    if (active_missing) {
      active <- current[["active"]]
    }
  } else {
    # Adding: expr is required
    if (expr_missing) {
      cli::cli_abort(c(
        "x" = "{.arg expr} is required when adding a new unit test.",
        "i" = "To modify an existing test, specify its {.arg test} or {.arg label}."
      ))
    }
    normalized <- .normalize_expr(expr_captured)
    expr_captured <- normalized[["expr_obj"]]
    expr_str <- normalized[["expr_str"]]
  }

  # --- Auto-generate label if needed (only when adding + label omitted) ---
  if (label_missing && !modifying) {
    label <- tryCatch(
      interpret(expr_captured),
      error = function(e) paste(deparse(expr_captured, width.cutoff = 500L), collapse = " ")
    )
    if (length(conditions) > 0) {
      cond_str <- paste(
        names(conditions), "=",
        vapply(conditions, format, character(1)),
        collapse = ", "
      )
      label <- paste0(label, " (", cond_str, ")")
    }
  }

  # --- Validate label ---
  if (!is.character(label) || length(label) != 1L || is.na(label) || !nzchar(trimws(label))) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg label} argument.",
      "!" = "{.arg label} must be a single non-empty character string."
    ))
  }
  label <- trimws(label)

  # --- Check label uniqueness ---
  existing_labels <- if (n_tests > 0) {
    vapply(tests, function(t) t[["label"]], character(1))
  } else {
    character(0)
  }

  if (!modifying && label_missing && label %in% existing_labels) {
    dup_pos <- match(label, existing_labels)
    cli::cli_abort(c(
      "x" = "Auto-generated label {.val {label}} already exists ([{dup_pos}]).",
      ">" = "Provide a unique {.arg label} explicitly."
    ))
  }

  if (modifying) {
    other_labels <- existing_labels[-modify_pos]
    if (label %in% other_labels) {
      dup_pos <- match(label, existing_labels)
      cli::cli_abort(c(
        "x" = "Label {.val {label}} is already used by unit test [{dup_pos}].",
        "!" = "Labels must be unique."
      ))
    }
  } else {
    if (label %in% existing_labels) {
      dup_pos <- match(label, existing_labels)
      cli::cli_abort(c(
        "x" = "A unit test with label {.val {label}} already exists ([{dup_pos}]).",
        "!" = "Labels must be unique. Use {.code unit_test(object, test = {dup_pos})} to modify the existing test."
      ))
    }
  }

  # --- Check for duplicate expr ---
  existing_exprs <- if (n_tests > 0) {
    vapply(tests, function(t) t[["expr_str"]], character(1))
  } else {
    character(0)
  }
  check_exprs <- if (modifying) existing_exprs[-modify_pos] else existing_exprs
  if (expr_str %in% check_exprs) {
    dup_idx <- which(existing_exprs == expr_str)
    if (modifying) dup_idx <- setdiff(dup_idx, modify_pos)
    dup_idx <- dup_idx[vapply(dup_idx, function(i) {
      identical(tests[[i]][["conditions"]], conditions)
    }, logical(1))]
    if (length(dup_idx) > 0) {
      cli::cli_abort(c(
        "x" = "An identical expression already exists in unit test number {dup_idx[1]}.",
        ">" = "Use {.code unit_test(object, test = {dup_idx[1]})} to modify the existing test."
      ))
    }
  }

  # --- Validate expr variables ---
  model_names <- get_model_var(object)
  ut_vars <- .ut_expr_vars(expr_str, model_names)
  if (length(ut_vars[["unknown"]]) > 0) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg expr}: variable{?s} not found in model: {.val {ut_vars[['unknown']]}}.",
      ">" = "Check for typos. Available variables: {.val {model_names}}."
    ))
  }

  # --- Validate conditions ---
  if (!is.list(conditions)) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg conditions} argument.",
      "!" = "{.arg conditions} must be a named list of parameter overrides.",
      ">" = "Example: {.code conditions = list(beta = 0.1)}"
    ))
  }
  if (length(conditions) > 0) {
    if (is.null(names(conditions)) || any(!nzchar(names(conditions)))) {
      cli::cli_abort(c(
        "x" = "Invalid {.arg conditions} argument.",
        "!" = "All elements of {.arg conditions} must be named."
      ))
    }
    valid_names <- object[["variables"]][
      object[["variables"]][["type"]] %in% c("stock", "constant"), "name"
    ]
    bad_names <- setdiff(names(conditions), valid_names)
    if (length(bad_names) > 0) {
      cli::cli_abort(c(
        "x" = "Invalid {.arg conditions} argument.",
        "!" = "Names not found as stocks or constants: {.val {bad_names}}.",
        ">" = "Only stocks and constants can be specified as conditions. Available: {.val {valid_names}}."
      ))
    }
  }

  # --- Validate active ---
  if (!is.logical(active) || length(active) != 1L || is.na(active)) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg active} argument.",
      "!" = "{.arg active} must be {.val TRUE} or {.val FALSE}."
    ))
  }

  # --- Build entry ---
  entry <- list(
    label      = label,
    expr_str   = expr_str,
    conditions = conditions,
    active     = active
  )

  # --- Insert or replace ---
  if (modifying) {
    object[["unit_tests"]][[modify_pos]] <- entry
  } else {
    object[["unit_tests"]] <- c(object[["unit_tests"]], list(entry))
  }

  # --- Eagerly recompute test deps cache ---
  object[["assemble"]][["unit_tests"]][["deps"]] <- .compute_test_deps(object)

  object
}


# ==============================================================================
# DISCARD UNIT TEST
# ==============================================================================

#' Remove a unit test from a stock-and-flow model
#'
#' Remove one or more unit tests by `test` (integer position as shown by
#' [unit_tests()]) or by `label` (character). Warns if a label or index is
#' not found. Remaining tests are renumbered sequentially after removal.
#'
#' @inheritParams update.stockflow
#' @param test Integer index/indices of the test(s) to remove. Corresponds to
#'   the order shown by [unit_tests()].
#' @param label Character label(s) of the test(s) to remove. Supports NSE
#'   (bare symbol or string). For backward compatibility, integer values
#'   passed via `label` are also accepted.
#'
#' @returns The model object with the specified test(s) removed.
#'
#' @export
#' @concept unitTest
#' @seealso [unit_test()], [unit_tests()]
#'
#' @examples
#' sfm <- stockflow("sir") |>
#'   unit_test(label = "susceptible is non-negative", expr = all(susceptible >= 0)) |>
#'   unit_test(label = "recovered increases", expr = all(diff(recovered) >= 0))
#'
#' # Remove by test
#' sfm <- discard_unit_test(sfm, test = 1)
#'
#' # Remove by label
#' sfm <- discard_unit_test(sfm, label = "recovered increases")
discard_unit_test <- function(object, label, test) {
  check_stockflow(object)

  n_tests <- length(object[["unit_tests"]])

  if (!missing(test)) {
    # --- Explicit test path ---
    if (!is.numeric(test)) {
      cli::cli_abort(c(
        "x" = "Invalid {.arg test} argument.",
        "!" = "{.arg test} must be an integer vector."
      ))
    }
    idx <- as.integer(test)

    bad_idx <- idx[idx < 1L | idx > n_tests]
    if (length(bad_idx) > 0) {
      cli::cli_abort(c(
        "x" = "{cli::qty(length(bad_idx))}Index{?es} out of range: {.val {bad_idx}}.",
        "i" = "Model has {n_tests} unit test{?s}."
      ))
    }
    idx_to_remove <- idx[idx >= 1L & idx <= n_tests]
  } else if (!missing(label)) {
    # --- label path (backward-compatible: handles int or char) ---
    label_expr <- rlang::enexpr(label)

    if (is.numeric(label_expr) || (rlang::is_call(label_expr) && identical(rlang::call_name(label_expr), "c"))) {
      idx <- eval(label_expr)
      if (!is.integer(idx)) idx <- as.integer(idx)

      bad_idx <- idx[idx < 1L | idx > n_tests]
      if (length(bad_idx) > 0) {
        cli::cli_abort(c(
          "x" = "{cli::qty(length(bad_idx))}Index{?es} out of range: {.val {bad_idx}}.",
          "i" = "Model has {n_tests} unit test{?s}."
        ))
      }
      idx_to_remove <- idx[idx >= 1L & idx <= n_tests]
    } else {
      all_labels <- vapply(object[["unit_tests"]], function(t) t[["label"]], character(1))
      labels_wanted <- .expr_to_char(label_expr)
      not_found <- setdiff(labels_wanted, all_labels)
      if (length(not_found) > 0) {
        cli::cli_abort(c(
          "x" = "{cli::qty(length(not_found))}Unit test{?s} not found: {.val {not_found}}."
        ))
      }
      idx_to_remove <- which(all_labels %in% labels_wanted)
    }
  } else {
    cli::cli_abort(c("x" = "Please specify {.arg test} or {.arg label} to identify the test(s) to remove."))
  }

  # Batch remove (remaining tests are renumbered implicitly by position)
  if (length(idx_to_remove) > 0) {
    object[["unit_tests"]] <- object[["unit_tests"]][-idx_to_remove]
  }

  # Invalidate cached test deps (positions shifted after removal)
  object <- invalidate_assemble(object, "unit_tests")

  object
}


# ==============================================================================
# DISPLAY UNIT TESTS
# ==============================================================================

#' Display unit tests defined on a stock-and-flow model
#'
#' Returns an overview of all unit tests attached to the model. The result
#' has a `print()` method.
#'
#' @inheritParams update.stockflow
#' @param test Integer vector of test number(s) to display (1-based). Defaults to
#'   `NULL` (show all tests). Can be combined with `label` (intersection).
#' @param label Character vector of regex patterns for partial, case-insensitive
#'   label matching. A test is included if its label matches *any* pattern.
#'   E.g., `c("non-neg", "beta")` returns tests matching either fragment.
#'   Can be combined with `test` (intersection).
#' @param ignore_case Logical; whether `label` matching is case-insensitive.
#'   Default `TRUE`.
#'
#' @returns An object of class `unit_tests_stockflow`, printed automatically.
#'
#' @export
#' @concept unitTest
#' @seealso [unit_test()], [verify()]
#'
#' @examples
#' sfm <- stockflow("sir") |>
#'   unit_test(expr = all(susceptible >= 0)) |>
#'   unit_test(
#'     label = "recovered increases over time",
#'     expr = all(diff(recovered) >= 0)
#'   )
#'
#' unit_tests(sfm)
#' unit_tests(sfm, test = 1L)
#' unit_tests(sfm, label = "increases")
unit_tests <- function(object, test = NULL, label = NULL, ignore_case = TRUE) {
  check_stockflow(object)
  tests <- object[["unit_tests"]]
  indices <- seq_along(tests)
  if (!is.null(test)) {
    .check_test_index(test, length(tests))
    tests <- tests[as.integer(test)]
    indices <- as.integer(test)
  }
  if (!is.null(label)) {
    if (!is.character(label) || length(label) == 0L || any(is.na(label)) || any(!nzchar(label))) {
      cli::cli_abort(c(
        "x" = "Invalid {.arg label}.",
        "!" = "{.arg label} must be a character vector of non-empty regex pattern(s)."
      ))
    }
    current_labels <- vapply(tests, function(t) t[["label"]], character(1L))
    hits <- Reduce("|", lapply(label, grepl, x = current_labels, ignore.case = ignore_case))
    matched <- which(hits)
    if (length(matched) == 0) {
      cli::cli_warn(c(
        "x" = "No tests matched pattern{?s} {.val {label}}.",
        "i" = "Available labels: {.val {current_labels}}."
      ))
      tests <- list()
      indices <- integer(0L)
    } else {
      tests <- tests[matched]
      indices <- indices[matched]
    }
  }
  result <- structure(
    list(tests = tests, n = length(tests), indices = indices),
    class = "unit_tests_stockflow"
  )
  result
}


#' @export
#' @concept unitTest
print.unit_tests_stockflow <- function(x, ...) {
  if (x$n == 0) {
    cli::cli_inform(c("i" = "No unit tests defined. Add tests with {.fn unit_test}."))
    return(invisible(x))
  }

  # Number of tests, active tests, and tests with conditions
  n_active <- sum(vapply(x$tests, function(t) isTRUE(t[["active"]]), logical(1L)))
  n_cond <- sum(vapply(x$tests, function(t) length(t[["conditions"]]) > 0, logical(1L)))

  cli::cli_h1("Stock-and-Flow Unit Tests")
  cli::cli_text(paste0(c("{x$n} tests", "{n_active}/{x$n} active", "{n_cond}/{x$n} include conditions"), collapse = " \u2022 "))


  for (i in seq_along(x$tests)) {
    t <- x$tests[[i]]
    original_nr <- if (is.null(x[["indices"]])) i else x[["indices"]][[i]]
    icon <- if (isTRUE(t[["active"]])) cli::col_green(cli::symbol$bullet) else cli::symbol$circle_dotted
    cli::cli_text("{icon} {original_nr}. {t[['label']]}")
    cli::cli_bullets(c(" " = "{.code {t$expr_str}}"))
    if (length(t[["conditions"]]) > 0) {
      cond_str <- paste(names(t[["conditions"]]), unlist(t[["conditions"]]),
        sep = " = ", collapse = ", "
      )
      cli::cli_bullets(c(" " = "Conditions: {cond_str}"))
    }
  }

  invisible(x)
}


# ==============================================================================
# RESULT CONSTRUCTOR + PRINT
# ==============================================================================

#' @noRd
new_verify_stockflow <- function(success = FALSE,
                                 error_message = NULL,
                                 results, object, sims = NULL, condition = NULL,
                                 n_conditions = 1L, test_indices = NULL) {
  if (is.null(test_indices)) test_indices <- seq_along(results)
  structure(
    list(
      success = success,
      error_message = error_message,
      results = results, object = object, sims = sims, condition = condition,
      n_conditions = n_conditions,
      test_indices = as.integer(test_indices)
    ),
    class = "verify_stockflow"
  )
}


.unit_test_expected_actual_message <- function(actual, expected = TRUE) {
  actual_str <- if (length(actual) == 1L && is.na(actual)) "NA" else as.character(actual)
  paste0("Expected: ", as.character(expected), "\nActual: ", actual_str)
}


#' Print a unit-test result message as indented cli bullets
#'
#' Used by print.verify_stockflow() for failed, errored, and skipped tests. The
#' (possibly multi-line) message is split into one bullet per line.
#'
#' @param message Message string, or `NULL`/empty to print nothing.
#' @param emph Whether to wrap each line in `{.emph ...}` (used for skipped tests).
#' @noRd
#'
.emit_test_message <- function(message, emph = FALSE) {
  if (is.null(message) || !nzchar(message)) {
    return(invisible())
  }
  lines <- strsplit(message, "\\n", fixed = TRUE)[[1L]]
  if (emph) {
    lines <- vapply(lines, function(L) paste0("{.emph ", L, "}"), character(1))
  }
  bullets <- stats::setNames(lines, rep(" ", length(lines)))
  cli::cli_bullets(bullets)
}


#' @export
#' @concept unitTest
print.verify_stockflow <- function(x, ...) {
  check_verify_stockflow(x)

  results <- x[["results"]]

  if (length(results) == 0) {
    cli::cli_inform(c("i" = "No unit tests to report."))
    return(invisible(x))
  }

  n_pass <- sum(vapply(results, function(r) identical(r[["status"]], "pass"), logical(1L)))
  n_fail <- sum(vapply(results, function(r) identical(r[["status"]], "fail"), logical(1L)))
  n_skip <- sum(vapply(results, function(r) identical(r[["status"]], "skip"), logical(1L)))
  n_error <- sum(vapply(results, function(r) identical(r[["status"]], "error"), logical(1L)))
  n_run <- n_pass + n_fail + n_error
  n_total <- length(results)

  cli::cli_h1("Stock-and-Flow Unit Test Results")

  if (n_skip > 0 && n_run == 0) {
    cli::cli_text("{n_skip}/{n_total} test{?s} skipped.")
  } else {
    cli::cli_text("{n_pass}/{n_run} test{?s} passed.")
  }

  test_indices <- x[["test_indices"]]
  for (i in seq_along(results)) {
    r <- results[[i]]
    original_nr <- if (!is.null(test_indices)) test_indices[[i]] else i

    switch(r[["status"]],
      pass = {
        cli::cli_inform(c("v" = "{original_nr}. {r[['label']]}"))
      },
      fail = {
        cli::cli_inform(c("x" = "{original_nr}. {r[['label']]}"))
        .emit_test_message(r[["message"]])
      },
      error = {
        cli::cli_inform(c("!" = "{original_nr}. {r[['label']]}"))
        .emit_test_message(r[["message"]])
      },
      skip = {
        cli::cli_inform(c("i" = "{original_nr}. {r[['label']]}"))
        .emit_test_message(r[["message"]], emph = TRUE)
      }
    )
  }

  invisible(x)
}


# ==============================================================================
# AS.DATA.FRAME / HEAD / TAIL
# ==============================================================================

#' Convert verify() results to a data frame
#'
#' Converts a `verify_stockflow` object to a data frame.
#'
#' **`which = "tests"` (default)** returns one row per unit test with columns
#' `test`, `label`, `status`, `outcome`, `expr_str`, `conditions`, and `message`.
#' Use `test`, `label`, and `status` to filter.
#'
#' **`which = "sims"`** returns the underlying simulation time-series in long
#' format with columns `test` (test number(s) that used this simulation, as a
#' comma-separated string), `conditions` (specified conditions per test, if any),
#' `time`, `variable`, and `value`. Each unique condition generates one
#' simulation; if multiple tests share a condition their numbers are combined
#' in `test` (e.g. `"1, 3"`). When filtering with `test`, the displayed `test` value
#' shows only the requested matching test number(s) for the retained simulation
#' row(s). Use `direction = "wide"` to pivot variables into columns.
#'
#' @param x A `verify_stockflow` object (output of [verify()]).
#' @param row.names `NULL` or a character vector giving row names (optional).
#' @param optional Ignored; present for compatibility.
#' @param which Character. `"tests"` (default) or `"sims"`. Partial matching
#'   supported.
#' @param direction Character. `"long"` (default) or `"wide"`. Only used when
#'   `which = "sims"`.
#' @param status Optional character vector of statuses to include (e.g.,
#'   `c("fail", "error")`). Defaults to all statuses.
#'   - `which = "tests"`: filters rows by test status.
#'   - `which = "sims"`: filters to conditions that have at least one test
#'     with a matching status.
#' @param condition Optional integer vector of condition numbers to filter by.
#'   For `which = "sims"`, keeps only the matching condition simulations.
#'   For `which = "tests"`, keeps only tests belonging to those conditions.
#' @param vars Variable names to retain in the data frame. Only applies when
#'   `which = "sims"`. Defaults to `NULL` to include all variables.
#' @param type Variable types to retain in the data frame. Must be one or more of
#'   'stock', 'flow', 'constant', 'aux', 'lookup', or 'func'. Only applies when
#'   `which = "sims"`. Defaults to `NULL` to include all types.
#' @inheritParams unit_tests
#' @param ... Additional arguments (unused).
#'
#' @returns A `data.frame`. Column set depends on `which`:
#'   - `"tests"`: `test`, `label`, `status`, `outcome`, `expr_str`, `condition`, `conditions`,
#'     `message`.
#'   - `"sims"` (long): `test`, `condition`, `conditions`, `time`, `variable`, `value`.
#'   - `"sims"` (wide): `test`, `condition`, `conditions`, `time`, then one column per variable.
#' @export
#' @concept unitTest
#' @method as.data.frame verify_stockflow
#'
#' @examples
#' # Create model with 2 unit tests
#' sfm <- stockflow("sir") |>
#'   unit_test(expr = all(susceptible >= 0)) |>
#'   # Add test with conditions
#'   unit_test(
#'     label = "lower infection rate",
#'     expr = all(susceptible >= 0),
#'     conditions = list(infection_rate = 0.1)
#'   )
#' res <- verify(sfm)
#'
#' # Test results (default)
#' as.data.frame(res)
#'
#' # Simulation time-series (long format)
#' as.data.frame(res, which = "sims") |> head()
#'
#' # Simulation time-series (wide format)
#' as.data.frame(res, which = "sims", direction = "wide") |> head()
#'
#' # Filter to simulation for test 2 only
#' as.data.frame(res, which = "sims", test = 2) |> head()
#'
#' # Only simulations for passing tests
#' as.data.frame(res, which = "sims", status = "pass") |> head()
as.data.frame.verify_stockflow <- function(x, row.names = NULL, optional = FALSE,
                                           which = c("tests", "sims")[1],
                                           direction = "long",
                                           test = NULL, label = NULL, ignore_case = TRUE,
                                           status = c("pass", "fail", "error", "skip"),
                                           condition = NULL,
                                           vars = NULL, type = NULL,
                                           ...) {
  vars <- .expr_to_char(rlang::enexpr(vars))
  check_verify_stockflow(x)

  which <- .clean_which_verify(which)

  # vars/type only filter simulation time-series, not the test results table
  if (which != "sims" && (!is.null(vars) || !is.null(type))) {
    cli::cli_inform(c(
      "i" = "The {.arg vars} and {.arg type} arguments only apply to {.code which = 'sims'}.",
      ">" = "Set {.code which = 'sims'} to filter the time-series by variable."
    ))
  }

  # ---- which = "sims": return simulation time-series -------------------------
  if (which == "sims") {
    direction <- trimws(tolower(direction))
    if (!direction %in% c("long", "wide")) {
      cli::cli_abort(c(
        "x" = "Invalid {.arg direction} argument.",
        ">" = "Must be either {.code 'long'} or {.code 'wide'}."
      ))
    }

    condition_vec <- x[["condition"]] # named int: test_label -> condition_index
    test_indices_all <- x[["test_indices"]]

    # For each condition index, collect the corresponding test numbers
    nr_by_ji <- lapply(seq_len(x[["n_conditions"]]), function(ji) {
      positions <- base::which(condition_vec == ji)
      sort(test_indices_all[positions])
    })
    # Human-readable "test" string per condition (e.g. "1" or "1, 3")
    nr_strs <- vapply(nr_by_ji, function(nrs) paste(nrs, collapse = ", "), character(1))
    nr_display_strs <- nr_strs

    # Validate and apply test filter: keep conditions that include any requested test
    if (!is.null(test)) {
      if (!is.numeric(test) || any(is.na(test)) || any(test != as.integer(test))) {
        cli::cli_abort(c(
          "x" = "Invalid {.arg test}.",
          ">" = "{.arg test} must be an integer vector of test numbers."
        ))
      }
      test <- as.integer(test)
      if (!all(test %in% test_indices_all)) {
        bad <- test[!test %in% test_indices_all]
        cli::cli_abort(c(
          "x" = "Test number{?s} not found in this result: {.val {bad}}.",
          "i" = "Available: {.val {test_indices_all}}."
        ))
      }
      keep_ji <- vapply(nr_by_ji, function(nrs) any(nrs %in% test), logical(1))
      if (!any(keep_ji)) {
        cli::cli_abort(c("x" = "No simulations match the requested {.arg test}."))
      }
      nr_display_strs[keep_ji] <- vapply(nr_by_ji[keep_ji], function(nrs) {
        paste(intersect(nrs, test), collapse = ", ")
      }, character(1))
      ji_to_use <- base::which(keep_ji)
    } else {
      ji_to_use <- seq_len(x[["n_conditions"]])
    }

    # Apply condition filter: keep only specified condition indices
    if (!is.null(condition)) {
      if (!is.numeric(condition) || any(is.na(condition)) || any(condition != as.integer(condition))) {
        cli::cli_abort(c(
          "x" = "Invalid {.arg condition}.",
          ">" = "{.arg condition} must be an integer vector of condition numbers."
        ))
      }
      condition <- as.integer(condition)
      valid_conds <- seq_len(x[["n_conditions"]])
      if (!all(condition %in% valid_conds)) {
        bad <- condition[!condition %in% valid_conds]
        cli::cli_abort(c(
          "x" = "Condition number{?s} not found: {.val {bad}}.",
          "i" = "Available: {.val {valid_conds}}."
        ))
      }
      ji_to_use <- intersect(ji_to_use, condition)
      if (length(ji_to_use) == 0) {
        cli::cli_abort(c("x" = "No simulations match the requested {.arg condition}."))
      }
    }

    # Apply status filter: keep conditions with at least one test matching status
    if (!is.null(status)) {
      status_clean <- clean_status(status)
      keep_status <- vapply(ji_to_use, function(ji) {
        any(vapply(nr_by_ji[[ji]], function(nr_val) {
          pos <- base::which(test_indices_all == nr_val)[1L]
          x[["results"]][[pos]][["status"]] %in% status_clean
        }, logical(1)))
      }, logical(1))
      if (!any(keep_status)) {
        cli::cli_abort(c("x" = "No simulations match the requested {.arg status}."))
      }
      ji_to_use <- ji_to_use[keep_status]
    }

    # Build conditions string per condition index
    .cond_str <- function(conds) {
      if (is.null(conds) || length(conds) == 0) {
        return("")
      }
      paste(names(conds), unlist(conds), sep = " = ", collapse = ", ")
    }
    condition_strs <- vapply(seq_len(x[["n_conditions"]]), function(ji) {
      pos <- base::which(condition_vec == ji)[1L]
      if (is.na(pos)) {
        return("")
      }
      .cond_str(x[["results"]][[pos]][["conditions"]])
    }, character(1))

    sims_dfs <- lapply(ji_to_use, function(ji) {
      sim <- x[["sims"]][[ji]]
      if (!isTRUE(sim[["success"]])) {
        cli::cli_warn(c("!" = "Simulation for condition {ji} (test {nr_strs[ji]}) did not succeed; skipping."))
        return(NULL)
      }
      df <- sim[["df"]]
      df[["test"]] <- nr_display_strs[ji]
      df[["condition"]] <- ji
      df[["conditions"]] <- condition_strs[ji]
      df[, c("test", "condition", "conditions", "time", "variable", "value")]
    })
    sims_dfs <- Filter(Negate(is.null), sims_dfs)

    if (length(sims_dfs) == 0) {
      df <- data.frame(
        test = character(0),
        condition = numeric(0),
        conditions = character(0),
        time = numeric(0), variable = character(0), value = numeric(0),
        stringsAsFactors = FALSE
      )
    } else {
      df <- do.call(rbind, c(sims_dfs, list(make.row.names = FALSE)))
    }

    # Filter by variable and/or type
    df <- .filter_long_by_vars_type(df, x[["object"]], vars = vars, type = type)

    if (direction == "wide") {
      df <- stats::reshape(df,
        timevar = "variable",
        idvar = c("test", "conditions", "time"),
        direction = "wide"
      )
      names(df) <- sub("^value\\.", "", names(df))
      rownames(df) <- NULL
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

    return(df)
  }

  # ---- which = "tests": return unit test metadata (existing behaviour) -------
  results <- x[["results"]]
  test_indices <- if (is.null(x[["test_indices"]])) seq_along(results) else x[["test_indices"]]
  condition_indices <- as.integer(x[["condition"]])

  if (!is.null(test)) {
    if (!all(test %in% test_indices)) {
      bad <- test[!test %in% test_indices]
      cli::cli_abort(c(
        "x" = "Test number{?s} not found in this result: {.val {bad}}.",
        "i" = "Available: {.val {test_indices}}."
      ))
    }
    keep <- which(test_indices %in% test)
    results <- results[keep]
    test_indices <- test_indices[keep]
    condition_indices <- condition_indices[keep]
  }

  if (!is.null(label)) {
    if (!is.character(label) || length(label) == 0L || any(is.na(label)) || any(!nzchar(label))) {
      cli::cli_abort(c(
        "x" = "Invalid {.arg label}.",
        "i" = "{.arg label} must be a character vector of non-empty regex pattern(s)."
      ))
    }
    result_labels <- vapply(results, function(r) r[["label"]], character(1L))
    hits <- Reduce("|", lapply(label, grepl, x = result_labels, ignore.case = ignore_case))
    keep <- which(hits)
    if (length(keep) == 0) {
      cli::cli_abort(c(
        "x" = "No results matched pattern{?s} {.val {label}}.",
        "i" = "Available labels: {.val {result_labels}}."
      ))
    } else {
      results <- results[keep]
      test_indices <- test_indices[keep]
      condition_indices <- condition_indices[keep]
    }
  }

  if (!is.null(status)) {
    status <- clean_status(status)
    keep <- which(vapply(results, function(r) r[["status"]] %in% status, logical(1L)))
    if (length(keep) == 0) {
      cli::cli_abort(c(
        "x" = "No tests with status {.val {status}} found."
      ))
    } else {
      results <- results[keep]
      test_indices <- test_indices[keep]
      condition_indices <- condition_indices[keep]
    }
  }

  if (!is.null(condition)) {
    if (!is.numeric(condition) || any(is.na(condition)) || any(condition != as.integer(condition))) {
      cli::cli_abort(c(
        "x" = "Invalid {.arg condition}.",
        ">" = "{.arg condition} must be an integer vector of condition numbers."
      ))
    }
    condition <- as.integer(condition)
    valid_conds <- seq_len(x[["n_conditions"]])
    if (!all(condition %in% valid_conds)) {
      bad <- condition[!condition %in% valid_conds]
      cli::cli_abort(c(
        "x" = "Condition number{?s} not found: {.val {bad}}.",
        "i" = "Available: {.val {valid_conds}}."
      ))
    }
    keep <- which(condition_indices %in% condition)
    if (length(keep) == 0) {
      cli::cli_abort(c("x" = "No tests with condition number{?s} {.val {condition}} found."))
    }
    results <- results[keep]
    test_indices <- test_indices[keep]
    condition_indices <- condition_indices[keep]
  }

  if (length(results) == 0) {
    df <- data.frame(
      test = integer(0), label = character(0), status = character(0),
      outcome = logical(0), expr_str = character(0),
      condition = numeric(0),
      conditions = character(0), message = character(0),
      stringsAsFactors = FALSE
    )
    return(df)
  }

  .null_chr <- function(v) if (is.null(v)) "" else v

  df <- data.frame(
    test = test_indices,
    label = vapply(results, function(r) .null_chr(r[["label"]]), character(1)),
    status = vapply(results, function(r) .null_chr(r[["status"]]), character(1)),
    outcome = vapply(results, function(r) {
      v <- r[["outcome"]]
      if (is.null(v) || (length(v) == 1L && is.na(v))) NA else isTRUE(v)
    }, logical(1)),
    expr_str = vapply(results, function(r) .null_chr(r[["expr_str"]]), character(1)),
    # Condition number
    condition = condition_indices,
    # Condition string (e.g. "beta = 0.1, gamma = 0.05")
    conditions = vapply(results, function(r) {
      conds <- r[["conditions"]]
      if (is.null(conds) || length(conds) == 0) {
        ""
      } else {
        paste(names(conds), unlist(conds), sep = " = ", collapse = ", ")
      }
    }, character(1)),
    message = vapply(results, function(r) .null_chr(r[["message"]]), character(1)),
    stringsAsFactors = FALSE
  )

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


#' Print first rows of verify results
#'
#' Wrapper around [head()] that first converts the results to a data frame using
#' [as.data.frame.verify_stockflow()].
#'
#' @param x A `verify_stockflow` object.
#' @param n Number of rows. Defaults to 6.
#' @param ... Other arguments passed to [as.data.frame.verify_stockflow()].
#'
#' @returns A `data.frame`.
#' @export
#' @concept unitTest
#' @importFrom utils head
#' @method head verify_stockflow
#'
#' @examples
#' sfm <- stockflow("sir") |>
#'   unit_test(expr = all(susceptible >= 0))
#' res <- verify(sfm)
#' head(res)
head.verify_stockflow <- function(x, n = 6L, ...) {
  df <- as.data.frame(x, ...)
  head(df, n)
}


#' Print last rows of verify results
#'
#' Wrapper around [tail()] that first converts the results to a data frame using
#' [as.data.frame.verify_stockflow()].
#'
#' @param x A `verify_stockflow` object.
#' @param n Number of rows. Defaults to 6.
#' @param ... Other arguments passed to [as.data.frame.verify_stockflow()].
#'
#' @returns A `data.frame`.
#' @export
#' @concept unitTest
#' @importFrom utils tail
#' @method tail verify_stockflow
#'
#' @examples
#' sfm <- stockflow("sir") |>
#'   unit_test(expr = all(susceptible >= 0))
#' res <- verify(sfm)
#' tail(res)
tail.verify_stockflow <- function(x, n = 6L, ...) {
  df <- as.data.frame(x, ...)
  tail(df, n)
}


# ==============================================================================
# INTERNAL HELPERS
# ==============================================================================


#' Build an evaluation environment from a simulation data frame
#'
#' Converts the long-format `df` (columns: time, variable, value) to a named
#' environment where each variable maps to its full time-series vector.
#'
#' @param df A data frame with columns `time`, `variable`, `value`
#' @return A new environment suitable for `eval()`
#' @noRd
.build_eval_env <- function(df) {
  vars <- unique(df[["variable"]])
  var_list <- lapply(
    stats::setNames(vars, vars),
    function(v) df[df[["variable"]] == v, "value"]
  )
  if (!"time" %in% vars) {
    var_list[["time"]] <- sort(unique(df[["time"]]))
  }
  list2env(var_list)
}


#' Evaluate a single unit test against a simulation result
#'
#' @param test A single unit test entry
#' @param sim A `simulate_stockflow` object
#' @return A named list with fields: `label`, `expr_str`, `conditions`, `status`, `error_type`,
#'   `message`, `outcome`. The `error_type` field is `NA` for pass/fail/skipped results,
#'   and one of `"expr_syntax"` (parse error), `"expr_result"` (type validation error),
#'   `"expr_eval"` (runtime evaluation error), `"simulation"` (simulation failure), or
#'   `"model_update"` (conditions application failure) for error results.
#' @noRd
.run_one_unit_test <- function(test, sim) {
  label <- test[["label"]]
  expr_str <- test[["expr_str"]]
  conditions <- if (is.null(test[["conditions"]])) list() else test[["conditions"]]

  if (!isTRUE(test[["active"]])) {
    return(list(label = label, expr_str = expr_str, conditions = conditions, status = "skip", error_type = NA, message = "Test is inactive.", outcome = NULL))
  }

  if (!sim[["success"]]) {
    return(list(
      label = label, expr_str = expr_str, conditions = conditions, status = "error", error_type = "simulation",
      message = paste0("Simulation failed: ", sim[["error_message"]]), outcome = NULL
    ))
  }

  env <- .build_eval_env(sim[["df"]])

  # Inject constants into eval environment (they are not in sim$df)
  constants <- sim[["constants"]]
  if (!is.null(constants)) {
    for (nm in names(constants)) {
      env[[nm]] <- constants[[nm]]
    }
  }

  expr_parsed <- tryCatch(
    parse(text = test[["expr_str"]], keep.source = FALSE)[[1]],
    error = function(e) NULL
  )
  if (is.null(expr_parsed)) {
    return(list(
      label = label, expr_str = expr_str, conditions = conditions, status = "error", error_type = "expr_syntax",
      message = paste0("Could not parse expression: ", expr_str), outcome = NULL
    ))
  }

  # Heuristic: calls that look like assertions (e.g., expect_equal(...))
  # are allowed to return non-logical values when they complete without failure.
  is_assertion_call <- is.call(expr_parsed) && {
    head <- expr_parsed[[1]]
    if (is.symbol(head)) {
      grepl("^(expect|assert)", as.character(head))
    } else if (is.call(head) && as.character(head[[1]]) %in% c("::", ":::")) {
      fn <- head[[3]]
      is.symbol(fn) && grepl("^(expect|assert)", as.character(fn))
    } else {
      FALSE
    }
  }

  result <- tryCatch(
    {
      val <- eval(expr_parsed, envir = env)

      if (is.logical(val) && length(val) == 1L && is.na(val)) {
        if (is_assertion_call) {
          return(list(label = label, expr_str = expr_str, conditions = conditions, status = "pass", error_type = NA, message = "", outcome = TRUE))
        }
        return(list(
          label = label, expr_str = expr_str, conditions = conditions,
          status = "fail", error_type = NA, outcome = FALSE,
          message = "Expression returned NA. The simulation likely contains NaN values (e.g., from division by zero). Check conditions for potential singularities, or use `isTRUE(all(...))` to treat NA as FALSE."
        ))
      }

      if (is.numeric(val) && length(val) == 1L && (is.infinite(val) || is.nan(val))) {
        if (is_assertion_call) {
          return(list(label = label, expr_str = expr_str, conditions = conditions, status = "pass", error_type = NA, message = "", outcome = TRUE))
        }
        return(list(
          label = label, expr_str = expr_str, conditions = conditions,
          status = "fail", error_type = NA, outcome = FALSE,
          message = sprintf(
            "Expression returned %s. The simulation likely contains Inf or NaN values. Check for division by zero or overflow in the model equations.",
            val
          )
        ))
      }

      if (!is.logical(val) || length(val) != 1L) {
        if (is_assertion_call) {
          return(list(label = label, expr_str = expr_str, conditions = conditions, status = "pass", error_type = NA, message = "", outcome = TRUE))
        }
        val_class <- paste(class(val), collapse = "/")
        val_len <- length(val)
        hint <- if (is.logical(val)) " Did you mean `all(...)` or `any(...)`?" else ""
        cond <- simpleError(
          sprintf(
            "Expression must return a non-missing logical scalar, got %s of length %d.%s",
            val_class, val_len, hint
          )
        )
        class(cond) <- c("expr_type_error", class(cond))
        stop(cond)
      }

      if (!isTRUE(val)) {
        return(list(
          label = label, expr_str = expr_str, conditions = conditions,
          status = "fail", error_type = NA, outcome = val,
          message = .unit_test_expected_actual_message(val)
        ))
      }

      list(label = label, expr_str = expr_str, conditions = conditions, status = "pass", error_type = NA, message = "", outcome = val)
    },
    expectation_failure = function(e) {
      list(label = label, expr_str = expr_str, conditions = conditions, status = "fail", error_type = NA, message = conditionMessage(e), outcome = FALSE)
    },
    expr_type_error = function(e) {
      list(label = label, expr_str = expr_str, conditions = conditions, status = "error", error_type = "expr_result", message = conditionMessage(e), outcome = NULL)
    },
    error = function(e) {
      list(label = label, expr_str = expr_str, conditions = conditions, status = "error", error_type = "expr_eval", message = conditionMessage(e), outcome = NULL)
    }
  )

  result
}


#' Extract model-variable references from a unit-test expression string
#'
#' Returns a list with two character vectors:
#' - `model_refs`: symbols that appear in `expr_str` AND are model variables
#' - `unknown`:    symbols that appear in `expr_str` AND are neither model
#'                 variables nor objects exported by base R
#'
#' Uses `all.vars()` so function names (e.g., `all`, `diff`, `expect_equal`)
#' are never included — only value-position symbols are checked.
#'
#' @param expr_str    Deparsed expression string (as stored in `unit_tests`)
#' @param model_names Character vector of model variable names
#' @return Named list with `model_refs` and `unknown` character vectors
#' @noRd
.ut_expr_vars <- function(expr_str, model_names) {
  parsed <- tryCatch(
    parse(text = expr_str, keep.source = FALSE)[[1]],
    error = function(e) NULL
  )
  if (is.null(parsed)) {
    return(list(model_refs = character(0), unknown = character(0)))
  }

  ev <- all.vars(parsed)
  not_in_model <- setdiff(ev, model_names)
  unknown <- not_in_model[
    !vapply(not_in_model, exists, logical(1), envir = baseenv(), inherits = FALSE)
  ]

  list(
    model_refs = intersect(ev, model_names),
    unknown    = unknown
  )
}


#' Compute unit test dependencies for all tests
#'
#' For each unit test, extracts `expr_refs` (model variables referenced in the
#' expression) and `cond_refs` (variable names used as condition keys). Returns
#' a positionally-indexed list matching `object[["unit_tests"]]`.
#'
#' @inheritParams update.stockflow
#' @return List of `list(expr_refs = character(), cond_refs = character())`
#' @noRd
.compute_test_deps <- function(object) {
  tests <- object[["unit_tests"]]
  if (length(tests) == 0L) {
    return(list())
  }

  model_names <- get_model_var(object)

  lapply(tests, function(test) {
    expr_refs <- .ut_expr_vars(test[["expr_str"]], model_names)[["model_refs"]]
    cond_refs <- names(test[["conditions"]])
    if (is.null(cond_refs)) cond_refs <- character(0)
    list(expr_refs = expr_refs, cond_refs = cond_refs)
  })
}


#' Get (lazily computed) unit test dependencies
#'
#' Returns the cached test deps from `object[["assemble"]][["unit_tests"]][["deps"]]`
#' if available; otherwise computes via `.compute_test_deps()`, stores the result,
#' and returns it. The caller is responsible for assigning the returned object
#' back if caching is desired.
#'
#' @inheritParams update.stockflow
#' @return A list with two elements:
#'   - `object`: the (possibly updated) model object
#'   - `deps`: positionally-indexed list of `list(expr_refs, cond_refs)`
#' @noRd
get_test_deps <- function(object) {
  deps <- object[["assemble"]][["unit_tests"]][["deps"]]
  if (is.null(deps)) {
    deps <- .compute_test_deps(object)
    object[["assemble"]][["unit_tests"]][["deps"]] <- deps
  }
  list(object = object, deps = deps)
}
