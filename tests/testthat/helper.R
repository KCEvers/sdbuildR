# Test Helper Functions
expect_snapshot_plot <- function(name, code, fileext = NULL, width = 4, height = 4) {
  withr::local_pdf(NULL)

  if (!is.character(name) || length(name) == 0L || any(!nzchar(name))) {
    stop("Snapshot name must be a non-empty character vector.")
  }

  plots <- if (length(name) == 1L) {
    list(code)
  } else {
    if (!is.list(code) || inherits(code, c("plotly", "grViz"))) {
      stop("When name has length > 1, code must be a list of plot objects with the same length.")
    }
    code
  }

  if (length(plots) != length(name)) {
    stop("name and code must have the same length.")
  }

  plot_types <- vapply(plots, function(pl) {
    if (inherits(pl, "plotly")) {
      "plotly"
    } else if (inherits(pl, "grViz")) {
      "grViz"
    } else {
      NA_character_
    }
  }, character(1))

  if (anyNA(plot_types)) {
    stop("Unable to determine file extension for plot snapshot. Please specify fileext.")
  }

  if (length(unique(plot_types)) > 1L) {
    stop("All plots must be of the same type (plotly or grViz).")
  }

  if (is.null(fileext)) {
    fileext <- ifelse(plot_types == "plotly", ".png", ".svg")
  } else {
    fileext <- rep_len(fileext, length(plots))
  }
  width <- rep_len(width, length(plots))
  height <- rep_len(height, length(plots))

  plotly_idx <- which(plot_types == "plotly")

  # Announce all plotly files before touching skips. This way, if a skip aborts
  # the test, testthat will not auto-delete later snapshot files from the group.
  if (length(plotly_idx) > 0L) {
    for (i in plotly_idx) {
      announce_snapshot_file(name = paste0(name[[i]], ".json"))
      announce_snapshot_file(name = paste0(name[[i]], ".png"))
    }
  }

  skip_on_cran()
  skip_if(
    length(plotly_idx) > 0L && !has_internet(),
    "No internet connection for plot snapshot test"
  )

  for (i in seq_along(plots)) {
    pl <- plots[[i]]

    if (plot_types[[i]] == "grViz") {
      # DiagrammeR
      expect_snapshot_value(pl[["x"]][["diagram"]], style = "json")
    } else {
      # skip_on_os("mac") # floating point differences cause snapshot failures on GitHub Actions macOS runners
      # skip_on_os("linux")

      # Stable assertion on structure
      json <- normalize_plotly(pl) |> jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE)

      json_path <- tempfile(fileext = ".json")
      writeLines(json, json_path)
      expect_snapshot_file(json_path, name = paste0(name[[i]], ".json"))
    }
  }

  # Visual artifacts for manual inspection, never fail. Run these only after all
  # stable JSON snapshots have been compared.
  if (length(plotly_idx) > 0L) {
    skip_if(
      Sys.getenv("SDBUILDR_CREATE_TEST_FIGS") != "true",
      "Skipping plot snapshot creation (SDBUILDR_CREATE_TEST_FIGS not set to 'true')"
    )

    if ("plotly" %in% plot_types) {
      skip_if_not_installed("webshot2")
      skip_if_not_installed("htmlwidgets")
    }

    if ("grViz" %in% plot_types) {
      skip_if_not_installed("DiagrammeRsvg")
      skip_if_not_installed("rsvg")
    }

    for (i in plotly_idx) {
      img_path <- tempfile(fileext = fileext[[i]])
      tryCatch(
        {
          # This block never runs on CRAN, so reuse one browser session
          # across the loop for speed instead of spawning a subprocess per
          # export; the browser is cleaned up when the test process exits.
          export_plot(plots[[i]],
            file = img_path, width = width[[i]], height = height[[i]],
            close_browser = FALSE
          )
          expect_snapshot_file(img_path,
            name = paste0(name[[i]], fileext[[i]]),
            compare = function(old, new) TRUE
          )
        },
        stockflow_export_error = function(e) {
          testthat::skip(paste("Plot export unavailable:", conditionMessage(e)))
        }
      )
    }
  }

  invisible()
}

normalize_plotly <- function(pl, digits = 2) {
  # # normalize plotly's random internal IDs
  # json <- gsub('"[a-f0-9]{8,}"', '"ID"', json)
  b <- plotly::plotly_build(pl)$x
  rn <- function(z) {
    if (is.numeric(z)) {
      round(z, digits)
    } else if (is.list(z)) lapply(z, rn) else z
  }
  list(data = lapply(b$data, rn), layout = rn(b$layout))
}

#' Helper to skip test if Julia is not ready
#'
skip_if_julia_not_ready <- function() {
  testthat::skip_on_cran()

  env_setup <- tryCatch(
    {
      suppressWarnings({
        is_julia_env_setup()
      })
    },
    error = function(e) {
      return(FALSE)
    }
  )

  if (!env_setup) {
    testthat::skip()
  }

  invisible()
}


#' Expect successful simulation
#'
#' Helper to verify a simulation completes successfully
#'
#' @param sfm A stock-and-flow model
#' @param ... Additional arguments passed to simulate()
#' @returns Simulation result
expect_successful_simulation <- function(sfm, ...) {
  sim <- expect_no_error(simulate(sfm, ...))
  expect_true(sim$success)
  expect_true(nrow(sim$df) > 0)
  expect_true("time" %in% colnames(sim$df))

  # Time range should be correct
  expect_equal(max(sim$df$time), as.numeric(sfm$sim_settings$stop))

  invisible(sim)
}


expect_named_elements <- function(x, expected) {
  missing <- setdiff(expected, names(x))
  expect_equal(missing, character(0), info = paste("Missing field(s):", paste(missing, collapse = ", ")))
}


expect_successful_ensemble <- function(x, expected_fields = NULL) {
  expect_s3_class(x, "ensemble_stockflow")
  expect_true(x[["success"]])
  expect_false(is.null(x[["summary"]]))

  if (!is.null(expected_fields)) {
    expect_named_elements(x, expected_fields)
  }

  invisible(x)
}


expect_unique_values <- function(df, column, expected) {
  expect_equal(sort(unique(df[[column]])), expected)
}


expect_ensemble_sim_coverage <- function(n, ...) {
  dfs <- list(...)
  for (df in dfs) {
    expect_unique_values(df, "sim", seq_len(n))
  }
}

#' Execute code quietly (suppressing messages and warnings)
#'
#' @noRd
silence <- function(expr) {
  suppressMessages(suppressWarnings(expr))
}


#' Simulate the SIR model
#'
#' Creates and simulates the SIR model, allowing overrides such as only_stocks.
sir_sim <- function(..., only_stocks = TRUE, seed = 123) {
  simulate(stockflow("sir"), only_stocks = only_stocks, seed = seed, ...)
}


#' Create a basic stock-and-flow model for tests
#'
#' Returns an sdbuildR model with one stock S (eqn=1) and one flow Flow1 (eqn=S, to=S)
#' to reduce duplication across tests. Note: uses "Flow1" instead of "F" to avoid
#' name conflict with R's FALSE constant.
make_basic_sfm <- function() {
  stockflow() |>
    update("S", type = "stock", eqn = "1") |>
    update("Flow1", type = "flow", eqn = "S", to = "S")
}


# Skip tests if internet not available or on CRAN
skip_if_no_internet <- function() {
  if (!has_internet()) {
    skip("No internet connection")
  }
  skip_on_cran()
}


# Local helper: model with stock, flow, constant, language = Julia
# Validation in ensemble() fails before Julia execution, so no Julia needed
make_ensemble_error_sfm <- function() {
  stockflow() |>
    update("S", type = "stock", eqn = "1") |>
    update("Flow1", type = "flow", eqn = "S", to = "S") |>
    update("k", type = "constant", eqn = "0.5") |>
    sim_settings(language = "Julia")
}


# Helper: standard sfm for method tests
make_jl_ensemble_sfm <- function() {
  stockflow("crielaard2022") |>
    sim_settings(start = 0, stop = 10, dt = 0.1, save_by = 1, language = "Julia")
}


make_r_ensemble_random_sfm <- function() {
  stockflow("sir") |>
    update(c(susceptible, infected, recovered), eqn = "runif(1, 1, 1000)") |>
    sim_settings(language = "R", start = 0, stop = 10, dt = 0.1, save_by = 1, seed = 42)
}


# Assert the core ODE layout invariant for a Julia-language model:
# the k-th stock (in variables-table order, which is the order used to build the
# state vector / init / unpacking) must have sum_name == "dSdt[k]". A permuted
# or non-contiguous mapping means dSdt[] is misaligned with the state vector,
# which silently swaps stock dynamics (see change_type/sanitize index bug).
# Runs in pure R (codegen only); no Julia installation required.
expect_stock_indices_aligned <- function(sfm) {
  stopifnot(identical(sfm[["sim_settings"]][["language"]], "Julia"))
  stocks <- sfm[["variables"]][sfm[["variables"]][["type"]] == "stock", ]
  expect_equal(
    stocks[["sum_name"]],
    paste0("dSdt[", seq_len(nrow(stocks)), "]"),
    info = paste0(
      "Stock dSdt[] indices must match state-vector order. Stocks: ",
      paste(stocks[["name"]], collapse = ", ")
    )
  )
}


# Helper: a small model with a stock, flow, and constant
# Pass language = "Julia" to use the Julia backend instead of R.
make_verifiable_sfm <- function(language = "R") {
  stockflow() |>
    update("S", type = "stock", eqn = runif(1, 1, 100)) |>
    update("drain", type = "flow", eqn = "rate * S", from = "S") |>
    update("rate", type = "constant", eqn = "0.1") |>
    sim_settings(stop = 10, dt = 0.1, save_by = 1, language = language, seed = 123)
}


# Helper: build a verify_stockflow result with configurable tests.
#   n_tests = 1 : one test ("S non-negative")
#   n_tests = 2 : adds a conditioned test ("S constant at zero rate")
#   with_fail   : adds an intentionally failing test instead of the conditioned one
make_verify_model <- function(n_tests = 1, with_fail = FALSE) {
  sfm <- make_verifiable_sfm() |>
    update(S, eqn = 10) |>
    unit_test(label = "S non-negative", expr = "all(S >= 0)")

  if (n_tests >= 2 && !with_fail) {
    sfm <- sfm |> unit_test(
      label = "S constant at zero rate",
      expr = "all(diff(S) == 0)",
      conditions = list(rate = 0)
    )
  } else if (with_fail) {
    sfm <- sfm |> unit_test(label = "S always zero", expr = "all(S == 0)")
  }

  silence(verify(sfm))
}


# Helper: deterministic R ensemble (seed fixed for snapshot reproducibility).
# save_sims = TRUE  → individual trajectories are stored in the result.
# conditions          → optional named list passed to ensemble().
make_r_ens <- function(n = 5, save_sims = FALSE, conditions = NULL, ...) {
  sfm <- make_r_ensemble_random_sfm()
  args <- list(sfm, n = n, save_sims = save_sims, quiet = TRUE, ...)
  if (!is.null(conditions)) args$conditions <- conditions
  silence(do.call(ensemble, args))
}

make_r_ens_2cond <- function(n = 3, ...) {
  make_r_ens(n = n, conditions = list("contact_rate" = c(1.5, 2.5)), ...)
}

# ##### Plotly attribute helpers #####

#' Expect a plotly object
#'
#' Small helper to assert an object is a plotly visualization.
expect_plotly <- function(x) {
  testthat::expect_s3_class(x, "plotly")
}

#' Built animation frames of a plotly object
plotly_frames <- function(pl) {
  plotly::plotly_build(pl)[["x"]][["frames"]]
}

#' Names of the built animation frames
plotly_frame_names <- function(pl) {
  frames <- plotly_frames(pl)
  vapply(frames, function(frame) frame[["name"]], character(1))
}

##### Plotly attribute helpers #####

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Normalize a Plotly color string to #RRGGBB uppercase when possible
#'
#' Accepts hex (#RRGGBB[AA]), rgb()/rgba() and named R colours; returns
#' uppercase #RRGGBB (alpha stripped) or NA_character_.
normalize_color_string <- function(col) {
  if (is.null(col)) {
    return(NA_character_)
  }
  col_chr <- as.character(col)[1L]
  if (!nzchar(col_chr)) {
    return(NA_character_)
  }

  if (grepl("^rgba?\\(", col_chr)) { # rgb()/rgba()
    nums <- as.numeric(strsplit(gsub("rgba?\\(|\\)", "", col_chr), ",")[[1L]])
    if (length(nums) >= 3) {
      rgb <- nums[1:3]
      if (max(rgb, na.rm = TRUE) <= 1) rgb <- round(rgb * 255)
      return(toupper(grDevices::rgb(rgb[1], rgb[2], rgb[3], maxColorValue = 255)))
    }
  }
  if (grepl("^#", col_chr)) { # hex, strip any alpha
    return(toupper(if (nchar(col_chr) >= 7) substr(col_chr, 1, 7) else col_chr))
  }
  v <- tryCatch(grDevices::col2rgb(col_chr), error = function(e) NULL) # named
  if (!is.null(v)) {
    return(toupper(grDevices::rgb(v[1], v[2], v[3], maxColorValue = 255)))
  }
  NA_character_
}

#' One comprehensive trace-level data frame (colour column normalized)
plotly_traces <- function(pl) {
  traces <- plotly::plotly_build(pl)[["x"]][["data"]]
  if (!length(traces)) {
    return(data.frame())
  }

  scal <- function(x) if (is.null(x) || !nzchar(as.character(x)[1L])) NA_character_ else as.character(x)[1L]

  data.frame(
    trace = seq_along(traces),
    name = vapply(traces, function(t) scal(t[["name"]]), character(1)),
    legendgroup = vapply(traces, function(t) scal(t[["legendgroup"]]), character(1)),
    type = vapply(traces, function(t) scal(t[["type"]]), character(1)),
    mode = vapply(traces, function(t) scal(t[["mode"]]), character(1)),
    showlegend = vapply(traces, function(t) isTRUE(t[["showlegend"]]), logical(1)),
    visible = vapply(traces, function(t) {
      v <- t[["visible"]]
      if (is.null(v)) {
        "TRUE"
      } else if (is.logical(v)) {
        if (isTRUE(v[1L])) "TRUE" else "FALSE"
      } else {
        as.character(v)[1L]
      }
    }, character(1)),
    xaxis = vapply(traces, function(t) t[["xaxis"]] %||% "x", character(1)),
    yaxis = vapply(traces, function(t) t[["yaxis"]] %||% "y", character(1)),
    color = vapply(traces, function(t) {
      col <- t[["line"]][["color"]] %||% t[["marker"]][["color"]] %||% t[["fillcolor"]]
      normalize_color_string(col)
    }, character(1)),
    stringsAsFactors = FALSE
  )
}

#' Built layout as a list, with any color-named field normalized
plotly_layout <- function(pl) {
  norm <- function(x) {
    if (is.list(x)) {
      nms <- names(x) %||% rep("", length(x))
      x[] <- Map(
        function(val, nm) {
          if (is.character(val) && length(val) == 1L && grepl("color", nm, ignore.case = TRUE)) {
            normalize_color_string(val)
          } else {
            norm(val)
          }
        },
        x, nms
      )
    }
    x
  }
  norm(plotly::plotly_build(pl)[["x"]][["layout"]])
}

#' Check that each legend swatch matches the traces it represents
#' (and, optionally, an expected named palette)
plotly_check_legend_colors <- function(pl, expected = NULL) {
  df <- plotly_traces(pl)
  if (!nrow(df)) {
    return(data.frame())
  }

  df$group <- ifelse(is.na(df$legendgroup), df$name, df$legendgroup)
  df <- df[!is.na(df$group) & !is.na(df$color), , drop = FALSE]
  if (!nrow(df)) {
    return(data.frame())
  }

  res <- do.call(rbind, lapply(split(df, df$group), function(rows) {
    g <- rows$group[1L]
    legend_color <- unique(rows$color[rows$showlegend])
    trace_colors <- unique(rows$color)
    out <- data.frame(
      group = g,
      legend_color = paste(legend_color, collapse = "|"),
      trace_colors = paste(trace_colors, collapse = "|"),
      n_legend = sum(rows$showlegend),
      ok = length(legend_color) == 1L && all(trace_colors == legend_color),
      stringsAsFactors = FALSE
    )
    if (!is.null(expected)) {
      exp <- if (g %in% names(expected)) normalize_color_string(expected[[g]]) else NA_character_
      out$expected <- exp
      out$matches_expected <- length(legend_color) == 1L && identical(legend_color, exp)
    }
    out
  }))
  rownames(res) <- NULL
  res
}

#' Subplot grid properties from a built plotly object
#'
#' shareX collapses x-axes within a COLUMN (observable only with >1 row);
#' shareY collapses y-axes within a ROW (observable only with >1 column).
#' Returns NA where sharing has no observable effect.
plotly_subplot_grid <- function(pl) {
  b <- plotly::plotly_build(pl)[["x"]]
  lay <- b[["layout"]]

  xkeys <- grep("^xaxis", names(lay), value = TRUE)
  ykeys <- grep("^yaxis", names(lay), value = TRUE)
  n_xaxes <- length(xkeys)
  n_yaxes <- length(ykeys)

  # distinct horizontal extents = columns; distinct vertical extents = rows
  dom <- function(keys) unique(lapply(keys, function(k) round(lay[[k]][["domain"]] %||% c(0, 1), 6)))
  ncols <- max(length(dom(xkeys)), 1L)
  nrows <- max(length(dom(ykeys)), 1L)

  # one panel = one distinct (xaxis, yaxis) reference
  pairs <- unique(vapply(b[["data"]], function(tr) {
    paste(tr[["xaxis"]] %||% "x", tr[["yaxis"]] %||% "y")
  }, character(1)))
  n_panels <- length(pairs)

  shareX <- if (nrows > 1L) n_xaxes == ncols else NA
  shareY <- if (ncols > 1L) n_yaxes == nrows else NA

  list(
    is_subplot = n_panels > 1L,
    nrows = nrows, ncols = ncols, n_panels = n_panels,
    n_xaxes = n_xaxes, n_yaxes = n_yaxes,
    shareX = shareX, shareY = shareY
  )
}

plotly_dedupe_legend <- function(traces) {
  if (!nrow(traces)) {
    return(traces)
  }

  # legend identity: legendgroup, else name; anonymous traces stay distinct
  key <- ifelse(!is.na(traces$legendgroup), traces$legendgroup, traces$name)
  na_key <- is.na(key)
  key[na_key] <- paste0("__trace", traces$trace[na_key])

  # order groups by first appearance, then put showlegend = TRUE first within each
  ord <- order(match(key, unique(key)), !traces$showlegend)
  t2 <- traces[ord, , drop = FALSE]
  out <- t2[!duplicated(key[ord]), , drop = FALSE]
  rownames(out) <- NULL
  out
}

##### Helpers for plot.stockflow() #####
#' Extract node info from a plot.stockflow() diagram
#'
#' @param pl Output of plot.stockflow() (a DiagrammeR htmlwidget)
#' @return A data.frame, one row per node, with a `name` column plus one
#'   column per attribute found (id, label, xlabel, tooltip, ...). Missing
#'   attributes are NA.
extract_diagram_nodes <- function(pl) {
  diagram <- pl[["x"]][["diagram"]]
  if (is.null(diagram)) stop("No diagram string found in plot object.")

  # Match node definitions "name" [ ... ] across the whole string. The bracket
  # body may span multiple lines because long (wrapped) labels can contain
  # embedded newlines. Edges ("a" -> "b") have no bracket, and style lines
  # (node [...]/edge [...]) have no quoted name, so both are excluded.
  # defs <- regmatches(
  #   diagram,
  #   gregexpr('"[^"]+"\\s*\\[[^\\]]*\\]', diagram, perl = TRUE)
  # )[[1]]

  # Remove edges first
  lines <- strsplit(diagram, "\n", fixed = TRUE)[[1]]
  node_lines <- lines[!grepl("->", lines, fixed = TRUE)]
  node_diagram <- paste(node_lines, collapse = "\n")

  defs <- regmatches(
    node_diagram,
    gregexpr('"[^"]+"\\s*\\[[^\\]]*\\]', node_diagram, perl = TRUE)
  )[[1]]

  if (length(defs) == 0L) {
    return(data.frame())
  }

  # Parse a bracket body ("key = value, key = value, ...") into a named list.
  # value may be "...", '...', or <...> (an HTML-like label, whose content can
  # itself contain < and >, e.g. <BR/> and <FONT ...>...</FONT>). A single regex
  # cannot handle the nested angle brackets, so scan character by character,
  # tracking quotes and angle-bracket depth.
  parse_attrs <- function(s) {
    kv <- list()
    i <- 1L
    n <- nchar(s)
    is_space <- function(ch) ch %in% c(" ", "\t", "\n", "\r")
    while (i <= n) {
      while (i <= n && (is_space(substr(s, i, i)) || substr(s, i, i) == ",")) i <- i + 1L
      if (i > n) break

      # key (letters/digits/underscore)
      key_start <- i
      while (i <= n && grepl("[A-Za-z0-9_]", substr(s, i, i))) i <- i + 1L
      key <- substr(s, key_start, i - 1L)

      # '=' and surrounding whitespace
      while (i <= n && is_space(substr(s, i, i))) i <- i + 1L
      if (i <= n && substr(s, i, i) == "=") i <- i + 1L
      while (i <= n && is_space(substr(s, i, i))) i <- i + 1L
      if (i > n) break

      ch <- substr(s, i, i)
      if (ch == '"' || ch == "'") { # quoted string
        i <- i + 1L
        val_start <- i
        while (i <= n && substr(s, i, i) != ch) i <- i + 1L
        val <- substr(s, val_start, i - 1L)
        i <- i + 1L # consume closing quote
      } else if (ch == "<") { # HTML-like label: read to the matching '>'
        depth <- 0L
        val_start <- i + 1L
        while (i <= n) {
          c2 <- substr(s, i, i)
          if (c2 == "<") {
            depth <- depth + 1L
          } else if (c2 == ">") {
            depth <- depth - 1L
            if (depth == 0L) break
          }
          i <- i + 1L
        }
        val <- substr(s, val_start, i - 1L)
        i <- i + 1L # consume closing '>'
      } else { # bare token
        val_start <- i
        while (i <= n && !(is_space(substr(s, i, i)) || substr(s, i, i) == ",")) i <- i + 1L
        val <- substr(s, val_start, i - 1L)
      }

      # NB: embedded newlines (in wrapped labels) and HTML tags are preserved,
      # so callers can verify wrapping, e.g. grepl("\n", nodes$label), or HTML,
      # e.g. grepl("<FONT", nodes$label).
      if (nzchar(key)) kv[[key]] <- trimws(val)
    }
    kv
  }

  parsed <- lapply(defs, function(def) {
    parts <- regmatches(def, regexec('(?s)^"([^"]+)"\\s*\\[(.*)\\]$', def, perl = TRUE))[[1]]
    c(list(name = parts[2]), parse_attrs(parts[3]))
  })

  # Bind rows on the union of all attribute names
  all_cols <- unique(c("name", unlist(lapply(parsed, names))))
  df <- do.call(rbind, lapply(parsed, function(x) {
    row <- setNames(rep(NA_character_, length(all_cols)), all_cols)
    row[names(x)] <- unlist(x)
    as.data.frame(as.list(row), stringsAsFactors = FALSE)
  }))
  rownames(df) <- NULL
  df
}


#' Extract edge info from a plot.stockflow() diagram
#'
#' @param pl Output of plot.stockflow()
#' @return A data.frame with `from`, `to`, an edge `type` (from the section
#'   comment), plus one column per shared edge attribute (color, arrowhead, ...).
extract_diagram_edges <- function(pl) {
  diagram <- pl[["x"]][["diagram"]]
  if (is.null(diagram)) stop("No diagram string found in plot object.")

  lines <- trimws(unlist(strsplit(diagram, "\n")))

  parse_attrs <- function(attrs_str) {
    pat <- '(\\w+)\\s*=\\s*("[^"]*"|\'[^\']*\'|<[^>]*>|[^,\\]]+)'
    m <- regmatches(attrs_str, gregexpr(pat, attrs_str, perl = TRUE))[[1]]
    kv <- list()
    for (pair in m) {
      key <- trimws(sub("\\s*=.*$", "", pair))
      val <- sub("^[^=]*=\\s*", "", pair)
      val <- sub('^["\'<](.*)["\'>]$', "\\1", val)
      kv[[key]] <- trimws(val)
    }
    kv
  }

  current_type <- NA_character_
  current_attrs <- list()
  rows <- list()

  for (line in lines) {
    if (grepl("^#", line)) { # section comment -> edge type
      current_type <- trimws(sub("^#\\s*", "", line))
    } else if (grepl("^edge\\s*\\[", line)) { # shared style for the group
      current_attrs <- parse_attrs(sub("^edge\\s*\\[(.*)\\]\\s*$", "\\1", line))
    } else if (grepl('"[^"]+"\\s*->\\s*"[^"]+"', line)) { # an actual edge
      m <- regmatches(line, regexec('"([^"]+)"\\s*->\\s*"([^"]+)"', line))[[1]]
      rows[[length(rows) + 1L]] <- c(
        list(from = m[2], to = m[3], type = current_type),
        current_attrs
      )
    }
  }
  if (length(rows) == 0L) {
    return(data.frame())
  }

  all_cols <- unique(unlist(lapply(rows, names)))
  df <- do.call(rbind, lapply(rows, function(x) {
    row <- setNames(rep(NA_character_, length(all_cols)), all_cols)
    row[names(x)] <- unlist(x)
    as.data.frame(as.list(row), stringsAsFactors = FALSE)
  }))
  rownames(df) <- NULL
  df
}


#' Extract {rank=same; ...} groupings from a plot.stockflow() diagram
#'
#' @param pl Output of plot.stockflow()
#' @return A list of character vectors, one per `{rank=same; ...}` statement,
#'   each holding the (unquoted) node names in that group.
extract_diagram_ranks <- function(pl) {
  diagram <- pl[["x"]][["diagram"]]
  if (is.null(diagram)) stop("No diagram string found in plot object.")

  stmts <- regmatches(
    diagram,
    gregexpr("\\{\\s*rank\\s*=\\s*same\\s*;[^}]*\\}", diagram, perl = TRUE)
  )[[1]]
  lapply(stmts, function(s) {
    names <- regmatches(s, gregexpr('"([^"]+)"', s, perl = TRUE))[[1]]
    gsub('"', "", names, fixed = TRUE)
  })
}


# Helper models used across import-export tests
logistic_model_deSolve <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    dN <- r * N * (1 - N / K)
    list(c(dN))
  })
}

sir_model_deSolve <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    SI <- beta * S * I / N
    IR <- gamma * I
    dS <- -SI
    dI <- SI - IR
    dR <- IR
    list(c(dS, dI, dR))
  })
}


expect_empty_assemble_cache <- function(assemble) {
  expect_equal(length(assemble), length(empty_assemble()))
  expect_setequal(names(assemble), names(empty_assemble()))
  expect_null(assemble[["language"]])
  expect_null(assemble[["input_hash"]])
  expect_null(assemble[["eqn_cache"]])
  expect_null(assemble[["ordering"]])
  expect_equal(assemble[["times"]], "")
  expect_equal(assemble[["static"]][["script"]], "")
}


julia_ast_vnames <- function() {
  c("r", "X", "K", "a", "b", "c", "d", "x", "y", "gf1", "Stock1", "dt")
}


# Skip unless Julia is set up, and start the sdbuildR Julia session (loading the
# SystemDynamicsBuildR functions) so direct julia_eval() calls resolve them. A bare
# JuliaConnectoR session would not have run init.jl. use_julia() is idempotent.
ready_julia <- function() {
  skip_if_julia_not_ready()
  use_julia()
}

# Evaluate an R expression in R, and its converted form in Julia, and assert the
# two results are equal. This exercises the whole mapping: the converter plus the
# Julia function it dispatches to. Literal arguments are used throughout so no
# variable binding into Julia is required.
expect_r_julia_equal <- function(expr, tolerance = 1e-9, var_names = character(0)) {
  r_val <- eval(parse(text = expr), envir = globalenv())
  jl_code <- decode_unicode(convert_equations_julia("aux", "z", expr, var_names)[["eqn"]])
  jl_val <- julia_eval(jl_code)
  expect_equal(r_val, jl_val, tolerance = tolerance, info = paste0(expr, "  ->  ", jl_code))
}

# Simulate a one-stock model driven by a time-input function in both languages and
# compare the stock trajectory. This is the mapping test for the interpolation
# functions (ramp/step/pulse/seasonal), which return time-dependent functions and
# so cannot be compared as scalars.
expect_input_sim_equal <- function(input_eqn, tolerance = 1e-4) {
  sfm <- stockflow() |>
    update("a", "stock") |>
    update("input", "constant", eqn = !!input_eqn) |>
    update("inflow", "flow", eqn = "input(t)", to = "a") |>
    sim_settings(start = 0, stop = 20, dt = 0.1, save_by = 1)

  r <- silence(simulate(sim_settings(sfm, language = "R"), only_stocks = FALSE))
  j <- silence(simulate(sim_settings(sfm, language = "Julia"), only_stocks = FALSE))

  ra <- r[["df"]][r[["df"]][["variable"]] == "a", "value"]
  ja <- j[["df"]][j[["df"]][["variable"]] == "a", "value"]
  expect_equal(ra, ja, tolerance = tolerance, info = input_eqn)
}
