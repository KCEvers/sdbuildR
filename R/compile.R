


#' Simulate stock-and-flow model
#'
#' Simulate a stock-and-flow model with simulation specifications defined by `sim_specs()`. If not already run, the Julia environment will be set up with `use_julia()`. If any problems are detected by `debugger()`, the model cannot be simulated.
#'
#' @inheritParams insightmaker_to_sfm
#' @inheritParams build
#' @param format_code If TRUE, format the R script with the styler package; only works if language is set to "R" in sim_specs. Defaults to TRUE.
#' @param keep_unit If TRUE, keeps units of variables. Defaults to TRUE.
#' @param verbose If TRUE, update on progress. Defaults to FALSE.
#' @param debug If TRUE, print output for debugging. Defaults to FALSE.
#' @param only_stocks If TRUE, only save stocks. If FALSE, auxiliaries and flows are saved using a callback function. Only applies if language is set to "Julia" in sim_specs() and no delay functions are used. Defaults to FALSE.
#' @param ... Optional arguments
#'
#' @return Object of class sdbuildR_sim, which is a list containing:
#' \describe{
#'   \item{df}{Dataframe, timeseries of computed variables in the ODE}
#'   \item{constants}{Constant parameters}
#'   \item{xstart}{Initial value of stocks}
#'   \item{script}{Simulation script}
#'   \item{duration}{Duration of simulation}
#'   \item{success}{If TRUE, simulation was successful. If FALSE, simulation failed.}
#'   \item{...}{Other parameters passed to simulate}
#' }
#' @export
#'
#' @examples sfm = xmile("SIR")
#' sim = simulate(sfm)
#' plot(sfm)
#'
#' # Use Julia for models with units or delay functions
#' sfm = xmile("coffee_cup") %>% sim_specs(language = "Julia")
#' use_julia()
#' sim = simulate(sfm)
#' plot(sfm)
#'
#' # Close Julia session
#' use_julia(stop = TRUE)
#'
simulate = function(sfm,
                    format_code=TRUE,
                    keep_nonnegative_flow = TRUE,
                    keep_nonnegative_stock = FALSE,
                    keep_unit = TRUE,
                    only_stocks = FALSE,
                    verbose = FALSE,
                    debug = FALSE,  ...){

  # First assess whether the model is valid
  problems = debugger(sfm, quietly = TRUE)
  if (nzchar(problems$problems)){
    stop(problems$problems)
  }

  if (tolower(sfm$sim_specs$language) == "julia"){
    return(simulate_julia(sfm,
                          format_code=format_code,
                          keep_nonnegative_flow = keep_nonnegative_flow,
                          keep_nonnegative_stock = keep_nonnegative_stock,
                          keep_unit = keep_unit, only_stocks = only_stocks,
                          verbose = verbose, debug = debug))

  } else if (tolower(sfm$sim_specs$language) == "r"){

    # Remove - deSolve is now a required installation
    # if (!requireNamespace("deSolve", quietly = TRUE)){
    #   stop("deSolve is not installed! Please install deSolve to simulate in R,\nor simulate in Julia by setting\nsfm %>% sim_specs(language = 'Julia')")
    # }

    return(simulate_R(sfm,
                      format_code=format_code,
                      keep_nonnegative_flow = keep_nonnegative_flow,
                      keep_nonnegative_stock = keep_nonnegative_stock,
                      only_stocks = only_stocks,
                      verbose = verbose, debug = debug))
  } else {
    stop("Language not supported.\nPlease run either sfm %>% sim_specs(language = 'Julia') (recommended) or sfm %>% sim_specs(language = 'R') (no unit support).")
  }

}





#' Detect undefined variables in equations
#'
#' @inheritParams build
#'
#' @return List with issue and message
#' @noRd
#'
detect_undefined_var = function(sfm){

  # Get names
  var_names = get_model_var(sfm)

  # Macros and graphical functions can be functions
  possible_func_in_model = c(names(sfm[["macro"]]), names(sfm[["model"]][["variables"]]$gf))

  # ** Many more possible functions, e.g. * / + etc.
  possible_func = c(possible_func_in_model,
                    get_syntax_julia()$syntax_df$R_first_iter,
                    unlist(P),
                    # Remove base R names
                    "pi", "letters", "LETTERS",
                    "month.abb", "month.name")

  # ** to do: check whether gf or macro functions are used without brackets

  # Find references to variables which are not in names_df$name
  missing_ref = unlist(sfm[["model"]][["variables"]], recursive = FALSE, use.names = FALSE) %>%
    lapply(., function(x){

      # Find dependencies, and find which ones are not in names_df$name
      y = x[names(x) %in% c("eqn", "to", "from")]
      y = y[sapply(y, is_defined)]

      A = sapply(y, function(z){

        dependencies = find_dependencies(sfm, z, only_var = TRUE, only_model_var = FALSE)

        # # Check whether the function exists
        # set only_var = FALSE
        # is_existing_function = sapply(dependencies,
        #        function(name){
        #          exists(name, mode = "function", inherits = TRUE) &&
        #            is.function(get(name, mode = "function", inherits = TRUE))
        #        })
        #
        # dependencies = dependencies[!is_existing_function]

        # Find all undefined variables and functions
        setdiff(unlist(dependencies),
                c(possible_func, var_names))
      })
      A = A[lengths(A) > 0]
      if (length(A) == 0){
        return(NULL)
      } else {
        return(stats::setNames(list(A), x$name))
      }
    })

  missing_ref = unlist(missing_ref, recursive = FALSE)

  if (length(missing_ref) > 0){

    missing_ref_format = missing_ref %>% purrr::imap(function(x, name){
      purrr::imap(x, function(y, prop){
        paste0("- ", name, "$", prop, ": ", paste0(unname(y), collapse = ", "))
      })
    }) %>% unlist() %>% unname()

    return(list(issue = TRUE,
                msg = paste0(c("The properties below contain references to undefined variables.\nPlease define the missing variables or correct any spelling mistakes.",
                               paste0(missing_ref_format, collapse = "\n")), collapse = "\n")))
  } else {
    return(list(issue = FALSE))
  }

  return(NULL)
}







#' Topologically sort equations according to their dependencies
#'
#' @param dependencies_dict Named list with dependencies for each equation; names are equation names and entries are dependencies
#'
#' @return Equation names ordered according to their dependencies
#' @noRd
#'
topological_sort = function(dependencies_dict){

  if (length(dependencies_dict) == 0){
    return(list(issue = FALSE, msg = "", order = c()))
  }

  # Get equation names and dependencies
  eq_names = names(dependencies_dict)
  dependencies = unname(dependencies_dict)

  # Ensure all dependencies are in eq_names, otherwise these result in NAs
  dependencies = lapply(dependencies, function(x){
    new_dependencies = intersect(x, eq_names)
    if (length(new_dependencies) == 0){
      return("")
    } else {
      return(new_dependencies)
    }
  })

  # Order parameters according to dependencies
  edges = lapply(1:length(dependencies), function(i){

    # If no dependencies, repeat own name
    if (all(dependencies[[i]] == "")){
      edge = rep(eq_names[i], 2)
    } else {
      edge <- cbind(dependencies[[i]], rep(eq_names[i], length(dependencies[[i]])))
    }
    return(edge)
  }) %>% do.call(rbind, .) %>% magrittr::set_rownames(NULL) %>%
    # Turn into vector by row
    as.data.frame() # %>% dplyr::distinct() %>% as.matrix() %>% t() %>% c()
  edges = edges[!duplicated(edges), ] # Remove duplicates
  edges = as.matrix(edges) %>% t() %>% c()

  edges

  # Create a directed graph from the edges
  g <- igraph::make_graph(edges, directed = TRUE)

  # Get correct order using topological sort
  out = tryCatch(
    {
      list(order = igraph::topo_sort(g, mode = "out") %>% names(.), issue = FALSE, msg = "")
    }, error = function(msg){
      # print("Something went wrong when attempting to order the equations in your ODE, which may be because of circular definition (e.g. x = y, y = x). The correct order is important as e.g. for x = 1/a, a needs to be defined before x. Please check the order manually.")
      out = circularity(g)

      list(order = eq_names, issue = out$issue, msg = out$msg)
    })

  return(out)
}



#' Detect circular dependencies in equations
#'
#' @param g Graph object
#'
#' @returns List with issue and message
#' @noRd
#'
circularity = function(g){
  # Check for cycles by finding strongly connected components
  scc <- igraph::components(g, mode = "strong")
  if (any(scc$csize > 1)) {
    # Identify vertices in cycles (strongly connected components with more than one node)
    cycle_nodes <- names(scc$membership)[scc$membership %in% which(scc$csize > 1)]
    cycle_message <- paste("Circular dependencies detected involving variables:",
                           paste(cycle_nodes, collapse = ", "))

    # Find the specific edges in the cycles
    sub_g <- igraph::induced_subgraph(g, cycle_nodes)
    cycle_edges <- igraph::as_edgelist(sub_g)
    edge_message <- paste0(paste0("- ", cycle_edges[,1], " depends on ", cycle_edges[,2]), collapse = "\n")

    msg = paste0(c(cycle_message, edge_message), collapse = "\n")
    return(list(issue = TRUE, msg = msg))
  } else {
    return(list(issue = FALSE, msg = ""))
  }

}



#' Find newly defined variables in equation
#'
#' @param eqn Equation
#'
#' @returns Vector of newly defined variables
#' @noRd
find_newly_defined_var = function(eqn){

  # For each =, find preceding \n and next =
  newlines = unique(c(1, stringr::str_locate_all(eqn, "\\n")[[1]][, "start"], nchar(eqn)))
  assignment = stringr::str_locate_all(eqn, "=")[[1]]

  # Exclude <- & \n in comments and strings
  seq_quot = get_seq_exclude(eqn, var_names = NULL, type = "quot")

  assignment = assignment[!(assignment[, "start"] %in% seq_quot), , drop = FALSE]
  newlines = newlines[!(newlines %in% seq_quot)]

  new_var = c()
  if (nrow(assignment) > 0 & length(newlines) > 0){

    # Find preceding newline before assignment
    start_idxs = sapply(assignment[, "start"], function(idx){

      idxs_newline = which(newlines <= idx)
      newlines[idxs_newline[length(idxs_newline)]] # select last newline before assignment

    })

    # Isolate defined variables
    new_var = lapply(1:nrow(assignment), function(i){

      # Extract equation indices
      trimws(stringr::str_sub(eqn, start_idxs[i], assignment[i, "start"] - 1))
    })
    new_var = unlist(new_var)

  }

  return(new_var)
}




#' Find dependencies in equation
#'
#' @param eqns String with equation to find dependencies in; defaults to NULL to find dependencies of all variables.
#' @inheritParams build
#' @param only_var If TRUE, only look for variable names, not functions.
#' @param only_model_var If TRUE, only look for dependencies on other model variables.
#'
#' @return Vector of dependencies (variable names in equation)
#' @noRd
#'
find_dependencies = function(sfm, eqns = NULL, only_var = TRUE, only_model_var = TRUE){

  var_names = unique(get_model_var(sfm))

  # # Add .outflow to also detect delayed variables
  # var_names = c(var_names, paste0(var_names[grepl(paste0(P$delayN_suffix, "[0-9]+$|",
  # P$smoothN_suffix, "[0-9]+$"), var_names)], P$outflow_suffix))

  # Macros and graphical functions can be functions
  possible_func_in_model = c(names(sfm$macro),
                             names(sfm[["model"]][["variables"]]$gf),
                             var_names) # Some aux are also functions, such as pulse/step/ramp/seasonal

  # If no equations are provided, use all equations in the model
  if (is.null(eqns)){
    eqns = unlist(unname(lapply(sfm[["model"]][["variables"]],
                                function(x){lapply(x, `[[`, "eqn")})),
                  recursive = FALSE)
  }

  # Find dependencies in each equation
  dependencies = lapply(eqns, function(eqn){

    # Parse the line as an expression
    expr <- tryCatch(parse(text = eqn), error = function(e) NULL)

    # If parsing was successful, extract variable names from equations
    if (!is.null(expr)) {

      # Omit variables that are defined in the expression itself
      new_var = find_newly_defined_var(eqn)

      # Get all dependencies
      all_d = setdiff(all.names(expr, functions = TRUE, unique = TRUE), new_var)
      d = setdiff(all.names(expr, functions = FALSE, unique = TRUE), new_var)
      d_func = setdiff(all_d, d)

      if (only_model_var){
        d = c(d[d %in% var_names], d_func[d_func %in% possible_func_in_model])
      } else if (!only_var){
        d = all_d
      }

    } else {
      d <- NA
    }

    return(d)
  })

  return(dependencies)
}




#' Order equations of static and dynamic part of stock-and-flow model
#'
#' @inheritParams build
#' @param print_msg If TRUE, print message if the ordering fails; defaults to TRUE.
#'
#' @return List with order of static and dynamic variables
#' @noRd
#'
order_equations <- function(sfm, print_msg = TRUE){

  # Add .outflow to detect delayed variables
  var_names = unique(get_model_var(sfm))
  idx_delay = grepl(paste0(P$delayN_suffix, "[0-9]+$|",
                           P$smoothN_suffix, "[0-9]+$"), var_names)
  delay_var = var_names[idx_delay]
  delay_pattern = paste0(var_names[idx_delay], stringr::str_escape(P$outflow_suffix))

  # Separate auxiliary variables into static parameters and dynamically updated auxiliaries
  dependencies = lapply(sfm[["model"]][["variables"]], function(y){
    lapply(y, function(x){

      if (is_defined(x[["eqn"]])){

        d = unlist(find_dependencies(sfm, x[["eqn"]],
                                     only_var = TRUE, only_model_var = TRUE))

        # For delay family variables, find .outflow in eqn_julia
        if (length(delay_var) > 0){

          idx = stringr::str_detect(x[["eqn_julia"]], delay_pattern)
          d = c(d, delay_var[idx])
        }

      } else {
        d = c()
      }

      return(d)
    })
  })

  # Try to sort static and dynamic equations together
  # in case a static variable depends on a dynamic variable
  dependencies_dict = unlist(unname(dependencies), recursive = FALSE)
  static_and_dynamic = topological_sort(dependencies_dict)

  # Topological sort of static equations
  static_dependencies_dict = c(dependencies$gf,
                               dependencies$constant,
                               dependencies$stock) %>%
    purrr::list_flatten()

  static = topological_sort(static_dependencies_dict)
  if (print_msg & static$issue){
    message(paste0("Ordering static equations failed. ", static$msg, collapse = ""))
  }


  # Topological ordering
  dependencies_dict = c(dependencies$aux,
                        dependencies$flow) %>%
    purrr::list_flatten()
  dynamic = topological_sort(dependencies_dict)
  if (print_msg & dynamic$issue){
    message(paste0("Ordering dynamic equations failed. ", dynamic$msg, collapse = ""))
  }

  return(list(static = static, dynamic = dynamic,
              static_and_dynamic = static_and_dynamic))

}



#' Compare two simulations
#'
#' @param sim1 Simulation 1
#' @param sim2 Simulation 2
#' @param tolerance Numeric; tolerance for comparing values. Defaults to 0.00001.
#'
#' @returns List with comparison results
#' @export
#'
compare_sim = function(sim1, sim2, tolerance = .00001){

  if (sim1$success & !sim2$success){
    return(c(equal = FALSE,
                msg = "Simulation 1 was successful, but simulation 2 failed."))
  }

  if (!sim1$success & sim2$success){
    return(c(equal = FALSE,
                msg = "Simulation 2 was successful, but simulation 1 failed."))
  }

  get_prop = function(sim){
    list(colnames = colnames(sim[[P$sim_df_name]]),
         nrow = nrow(sim[[P$sim_df_name]]),
         ncol = ncol(sim[[P$sim_df_name]]),
         n_xstart = length(sim[[P$initial_value_name]]),
         n_pars = length(sim[[P$parameter_name]]),
         language = sim$sfm$sim_specs$language,
         method = sim$sfm$sim_specs$method
         )
  }

  prop1 = get_prop(sim1)
  prop2 = get_prop(sim2)

  overlapping_colnames = intersect(prop1$colnames, prop2$colnames)
  nonoverlapping_colnames = setdiff(union(prop1$colnames, prop2$colnames), overlapping_colnames)

  check_diff = function(col1, col2){

    col1 = as.numeric(col1)
    col2 = as.numeric(col2)

    if (length(col1) != length(col2)){
      return(c(equal = FALSE,
                  msg = paste0("Column lengths are not equal: ", length(col1)," (sim1) vs ", length(col2), " (sim2)")))
    }

    # Calculate Euclidean distance, ignoring NAs
    return(c(
      equal = all(abs(col1 - col2) < tolerance, na.rm = TRUE),
      first_diff = which(abs(col1 - col2) > tolerance)[1],
      nr_diff = sum(abs(col1 - col2) > tolerance, na.rm = TRUE),
      max_diff = max(abs(col1 - col2), na.rm = TRUE),
      sqrt_sum_diff = sqrt(sum((col1 - col2)^2, na.rm = TRUE))))
  }

  df = lapply(overlapping_colnames,
         function(name){c(name = name,
                          check_diff(sim1$df[[name]], sim2$df[[name]]))}) %>%
    do.call(dplyr::bind_rows, .) %>%
    as.data.frame()

  return(list(
    equal = all(as.logical(as.numeric(df$equal))),
    overlapping_colnames = overlapping_colnames,
    nonoverlapping_colnames = nonoverlapping_colnames,
    msg = paste0("The following columns are not equal:\n",
                 paste0(df$name, ": ", df$first_diff, " (", df$nr_diff, " differences, max diff: ", df$max_diff, ")\n", collapse = ""),
                 "\n"),
    prop1 = prop1,
    prop2 = prop2,
    df = df)
  )


}


