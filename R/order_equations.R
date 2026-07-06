#' Topologically sort equations according to their dependencies
#'
#' @param dependencies_dict Named list with dependencies for each equation; names are equation names and entries are dependencies
#'
#' @returns Equation names ordered according to their dependencies
#' @noRd
#'
topological_sort <- function(dependencies_dict) {
  if (length(dependencies_dict) == 0) {
    return(list(issue = FALSE, msg = "", order = c(), cycle_nodes = NULL, edge_list = NULL))
  }

  # Get equation names and dependencies
  eq_names <- names(dependencies_dict)
  deps <- unname(dependencies_dict)

  # Ensure all dependencies are in eq_names, otherwise these result in NAs
  deps <- lapply(deps, function(x) {
    new_dependencies <- intersect(x, eq_names)
    if (length(new_dependencies) == 0) {
      return("")
    } else {
      return(new_dependencies)
    }
  })

  # Order parameters according to dependencies
  edges <- lapply(seq_along(deps), function(i) {
    # If no dependencies, repeat own name
    if (all(deps[[i]] == "")) {
      edge <- rep(eq_names[i], 2)
    } else {
      edge <- cbind(deps[[i]], rep(eq_names[i], length(deps[[i]])))
    }
    return(edge)
  }) |>
    do.call(rbind, args = _) |>
    set_rownames(NULL) |>
    # Turn into vector by row
    as.data.frame()
  edges <- edges[!duplicated(edges), ] # Remove duplicates
  edges <- c(t(as.matrix(edges)))

  # Create a directed graph from the edges
  g <- igraph::make_graph(edges, directed = TRUE)

  # Get correct order using topological sort
  out <- tryCatch(
    {
      list(
        order = igraph::topo_sort(g, mode = "out") |> names(), issue = FALSE, msg = "",
        cycle_nodes = NULL, edge_list = NULL
      )
    },
    error = function(msg) {
      out <- circularity(g)
      list(
        order = eq_names, issue = out[["issue"]], msg = out[["msg"]],
        cycle_nodes = out[["cycle_nodes"]], edge_list = out[["edge_list"]]
      )
    }
  )

  out
}


#' Detect circular dependencies in equations
#'
#' @param g Graph object
#'
#' @returns List with issue and message
#' @noRd
#'
circularity <- function(g) {
  # Check for cycles by finding strongly connected components
  scc <- igraph::components(g, mode = "strong")
  if (any(scc[["csize"]] > 1)) {
    # Identify vertices in cycles (strongly connected components with more than one node)
    cycle_nodes <- names(scc[["membership"]])[scc[["membership"]] %in% which(scc[["csize"]] > 1)]
    cycle_message <- paste(
      "Circular dependencies detected involving variables:",
      paste(cycle_nodes, collapse = ", ")
    )

    # Find the specific edges in the cycles
    sub_g <- igraph::induced_subgraph(g, cycle_nodes)
    cycle_edges <- igraph::as_edgelist(sub_g)
    edge_message <- paste0(paste0(
      "- ", cycle_edges[, 1], " depends on ",
      cycle_edges[, 2]
    ), collapse = "\n")

    msg <- paste0(c(cycle_message, edge_message), collapse = "\n")
    return(list(issue = TRUE, msg = msg, cycle_nodes = cycle_nodes, edge_list = cycle_edges))
  } else {
    return(list(issue = FALSE, msg = "", cycle_nodes = NULL, edge_list = NULL))
  }
}


#' Find newly defined variables in equation
#'
#' @param eqn Equation
#'
#' @returns Vector of newly defined variables
#' @noRd
find_newly_defined_var <- function(eqn) {
  # For each =, find preceding \n and next =
  newlines <- unique(c(
    1, stringr::str_locate_all(eqn, "\\n")[[1]][, "start"],
    nchar(eqn)
  ))
  assignment <- stringr::str_locate_all(eqn, "=")[[1]]

  # Exclude <- & \n in comments and strings
  seq_quot <- get_seq_exclude(eqn, var_names = NULL, type = "quot")

  assignment <- assignment[!(assignment[, "start"] %in% seq_quot), , drop = FALSE]
  newlines <- newlines[!(newlines %in% seq_quot)]

  new_var <- c()
  if (nrow(assignment) > 0 && length(newlines) > 0) {
    # Find preceding newline before assignment
    start_idxs <- vapply(assignment[, "start"], function(idx) {
      idxs_newline <- which(newlines <= idx)
      newlines[idxs_newline[length(idxs_newline)]] # select last newline before assignment
    }, numeric(1))

    # Isolate defined variables
    new_var <- lapply(seq_len(nrow(assignment)), function(i) {
      # Extract equation indices
      trimws(stringr::str_sub(eqn, start_idxs[i], assignment[i, "start"] - 1))
    })
    new_var <- unlist(new_var)
  }

  return(new_var)
}


#' Find dependencies
#'
#' Find which other variables each variable is dependent on.
#'
#' @inheritParams update.stockflow
#' @param name Variable names to find dependencies for. Defaults to `NULL` to include all variables.
#' @param type Variable types to find dependencies for. Must be one or more of 'stock', 'flow', 'constant', 'aux', 'lookup', or 'func'. Defaults to `NULL` to include all types.
#' @param reverse If FALSE, list for each variable X which variables Y it depends on for its equation definition. If TRUE, don't show dependencies but dependents. This reverses the dependencies, such that for each variable X, it lists what other variables Y depend on X.
#'
#' @returns List, with for each model variable what other variables it depends on, or if \code{reverse = TRUE}, which variables depend on it
#' @concept build
#' @export
#'
#' @examples
#' sfm <- stockflow("sir")
#' dependencies(sfm)
#'
dependencies <- function(object, name = NULL, type = NULL, reverse = FALSE) {
  name <- .expr_to_char(rlang::enexpr(name))
  check_stockflow(object)

  # Check for mutually exclusive parameters
  if (!is.null(name) && !is.null(type)) {
    cli::cli_warn(c("!" = "Both {.arg name} and {.arg type} specified; ignoring {.arg type} and using {.arg name} only."))
    type <- NULL
  }

  # Validate parameters
  if (!is.null(name)) {
    .validate_name_arg(name, arg_name = "name")
  }

  if (!is.null(type)) {
    type <- .validate_type_arg(type, arg_name = "type")

    if (length(type) == 0) {
      cli::cli_abort(c("x" = "At least one {.arg type} must be specified"))
    }
  }

  dep <- .dependencies(object, eqns = NULL, only_var = TRUE, only_model_var = TRUE)

  if (reverse) {
    dep <- reverse_dep(dep)
  }

  # Filter by name if specified
  if (!is.null(name)) {
    # Clean names
    name <- Filter(nzchar, unique(name))

    if (length(name) == 0) {
      cli::cli_abort(c("x" = "At least one {.arg name} must be specified"))
    }

    # Check if names exist
    idx_exist <- name %in% names(dep)
    if (!all(idx_exist)) {
      missing_names <- name[!idx_exist]
      cli::cli_abort(c(
        "Variable{cli::qty(length(missing_names))}{?s} not found in model.",
        "x" = "{.code {missing_names}} {cli::qty(length(missing_names))}{?does/do} not exist."
      ))
    }
    dep <- dep[name]
  }

  # Filter by type if specified
  if (!is.null(type)) {
    # Get types of all variables in dep
    var_types <- object[["variables"]][object[["variables"]][["name"]] %in% names(dep), c("name", "type")]
    vars_to_keep <- var_types[var_types[["type"]] %in% type, "name"]
    dep <- dep[names(dep) %in% vars_to_keep]
  }

  dep
}


#' Reverse dependencies
#'
#' @param dep List of dependencies
#'
#' @returns List with reversed dependencies, showing dependents instead.
#' @noRd
reverse_dep <- function(dep) {
  reverse_dep <- list()

  # Initialize empty lists for all variables that appear as dependencies
  all_dependencies <- unique(unlist(dep))
  for (var in all_dependencies) {
    reverse_dep[[var]] <- character(0)
  }

  # Also initialize for variables that have dependencies (they might not be dependencies themselves)
  for (var in names(dep)) {
    if (!var %in% names(reverse_dep)) {
      reverse_dep[[var]] <- character(0)
    }
  }

  # Build reverse mapping
  for (target_var in names(dep)) {
    source_vars <- dep[[target_var]]
    if (length(source_vars) > 0) {
      for (source_var in source_vars) {
        reverse_dep[[source_var]] <- c(reverse_dep[[source_var]], target_var)
      }
    }
  }

  # Remove duplicates (shouldn't happen but just in case)
  reverse_dep <- lapply(reverse_dep, unique)

  reverse_dep
}


#' Find dependencies in equation (only for internal use)
#'
#' @param eqns String with equation to find dependencies in; defaults to NULL to find dependencies of all variables.
#' @inheritParams update.stockflow
#' @param only_var If TRUE, only look for variable names, not functions.
#' @param only_model_var If TRUE, only look for dependencies on other model variables.
#'
#' @returns Vector of dependencies (variable names in equation)
#' @noRd
#'
.dependencies <- function(object, eqns = NULL, only_var = TRUE, only_model_var = TRUE) {
  var_names <- get_model_var(object)

  # Funcs and graphical functions can be functions
  gf_names <- object[["variables"]][object[["variables"]][["type"]] == "lookup", "name"]
  func_names <- get_funcs(object)[["name"]]
  possible_func_in_model <- c(func_names, gf_names, var_names)

  # If no equations are provided, use all equations in the model
  if (is.null(eqns)) {
    vars <- object[["variables"]]

    eqn_idx <- vars[["type"]] %in% c("stock", "flow", "aux", "constant")
    eqns <- vars[eqn_idx, "eqn"]
    names(eqns) <- vars[eqn_idx, "name"]

    # Add graphical function dependencies on source
    gf_idx <- vars[["type"]] == "lookup"
    if (any(gf_idx)) {
      gf_source <- vars[gf_idx, "source"]
      gf_names_all <- vars[gf_idx, "name"]
      has_source <- nzchar(gf_source)
      eqns <- c(eqns, stats::setNames(gf_source[has_source], gf_names_all[has_source]))
    }
  }

  deps <- vector("list", length(eqns))
  eqn_names <- names(eqns)

  for (i in seq_along(eqns)) {
    eqn <- eqns[[i]]
    expr <- tryCatch(parse(text = eqn), error = function(e) NULL)

    if (is.null(expr)) {
      deps[[i]] <- character(0)
      next
    }

    # Omit variables that are defined in the expression itself. Most equations
    # do not contain assignments, so avoid the more expensive local scan unless
    # it can matter.
    new_var <- if (grepl("=", eqn, fixed = TRUE)) {
      find_newly_defined_var(eqn)
    } else {
      character(0)
    }
    d <- setdiff(all.names(expr, functions = FALSE, unique = TRUE), new_var)

    if (only_model_var) {
      all_d <- setdiff(all.names(expr, functions = TRUE, unique = TRUE), new_var)
      d_func <- setdiff(all_d, d)
      deps[[i]] <- unique(c(
        d[d %in% var_names],
        d_func[d_func %in% possible_func_in_model]
      ))
    } else if (!only_var) {
      deps[[i]] <- unique(setdiff(all.names(expr, functions = TRUE, unique = TRUE), new_var))
    } else {
      deps[[i]] <- unique(d)
    }
  }

  # Preserve names (if eqns were named); helps plotting dependencies
  if (!is.null(eqn_names)) {
    names(deps) <- eqn_names
  }

  deps
}


#' Order equations of static and dynamic part of stock-and-flow model
#'
#' @inheritParams update.stockflow
#' @param print_msg If TRUE, print message if the ordering fails; defaults to TRUE.
#'
#' @returns List with order of static and dynamic variables
#' @noRd
#'
order_equations <- function(object, print_msg = TRUE) {
  # Handle empty model: no variables to order
  if (is.null(object[["variables"]]) || nrow(object[["variables"]]) == 0) {
    empty <- topological_sort(list())
    return(list(
      static = empty,
      dynamic = empty,
      static_and_dynamic = empty
    ))
  }

  var_names <- unique(get_model_var(object))

  # Exclude func-type variables from ordering (they compile separately in the preamble)
  vars_to_order <- object[["variables"]][object[["variables"]][["type"]] != "func", ]

  # Separate auxiliary variables into static parameters and dynamically updated auxiliaries
  deps <- split(vars_to_order, seq_len(nrow(vars_to_order))) |>
    lapply(function(x) {
      if (is_defined(x[["eqn"]])) {
        d <- unlist(.dependencies(object, x[["eqn"]],
          only_var = TRUE, only_model_var = TRUE
        ))
      } else {
        d <- c()
      }

      return(list(
        name = x[["name"]],
        type = x[["type"]],
        d = d
      ))
    })

  # Organize dependencies by type (type-stable)
  deps_by_name <- stats::setNames(
    lapply(deps, function(x) x$d),
    vapply(deps, function(x) x$name, character(1))
  )
  dep_types <- vapply(deps, function(x) x$type, character(1))
  deps_by_type <- if (length(deps_by_name) == 0) list() else split(deps_by_name, dep_types)

  # Create type-specific dependency lists
  deps <- list(
    lookup = if ("lookup" %in% names(deps_by_type)) deps_by_type[["lookup"]] else list(),
    constant = if ("constant" %in% names(deps_by_type)) deps_by_type[["constant"]] else list(),
    stock = if ("stock" %in% names(deps_by_type)) deps_by_type[["stock"]] else list(),
    aux = if ("aux" %in% names(deps_by_type)) deps_by_type[["aux"]] else list(),
    flow = if ("flow" %in% names(deps_by_type)) deps_by_type[["flow"]] else list()
  )

  # Try to sort static and dynamic equations together
  # in case a static variable depends on a dynamic variable
  dependencies_dict <- c(
    deps[["lookup"]],
    deps[["constant"]],
    deps[["stock"]],
    deps[["aux"]],
    deps[["flow"]]
  ) |>
    flatten()
  static_and_dynamic <- topological_sort(dependencies_dict)

  # Topological sort of static equations
  static_dependencies_dict <- c(
    deps[["lookup"]],
    deps[["constant"]],
    deps[["stock"]]
  ) |>
    flatten()

  if (static_and_dynamic[["issue"]]) {
    if (any(unname(static_dependencies_dict) %in% c(
      names(deps[["aux"]]),
      names(deps[["flow"]])
    ))) {
      cli::cli_warn(c(
        "!" = "Could not resolve equation ordering.",
        "i" = "Topological sorting of all equations failed.",
        "i" = static_and_dynamic[["msg"]],
        ">" = "Check for circular dependencies in your model equations."
      ))
    }
  }

  static <- topological_sort(static_dependencies_dict)
  if (print_msg && static[["issue"]]) {
    cli::cli_warn(c(
      "Could not order static equations.",
      "!" = "Topological sorting of static equations failed.",
      "i" = static[["msg"]],
      ">" = "Check constant and stock definitions for circular dependencies."
    ))
  }


  # Topological ordering
  dependencies_dict <- c(
    deps[["aux"]],
    deps[["flow"]]
  ) |>
    flatten()
  dynamic <- topological_sort(dependencies_dict)

  if (print_msg && dynamic[["issue"]]) {
    cli::cli_warn(c(
      "Could not order dynamic equations.",
      "!" = "Topological sorting of auxiliary and flow equations failed.",
      "i" = dynamic[["msg"]],
      ">" = "Check for circular dependencies in your auxiliary and flow equations."
    ))
  }

  return(list(
    static = static,
    dynamic = dynamic,
    static_and_dynamic = static_and_dynamic,
    deps_by_name = deps_by_name # Store for cache comparison
  ))
}
