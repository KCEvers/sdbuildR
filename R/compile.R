

#' Simulate stock-and-flow model
#'
#' Simulate a stock-and-flow model with simulation specifications defined by `sim_specs()`. If not already run, the Julia environment will be set up with `use_julia()`. If any problems are detected by `debugger()`, the model cannot be simulated.
#'
#' @inheritParams insightmaker_to_sfm
#' @inheritParams build
#' @param format_code If TRUE, format the R script with the styler package; only works if language is set to "R" in sim_specs. Defaults to TRUE.
#' @param keep_unit If TRUE, keeps units of variables. Defaults to TRUE.
#' @param verbose If TRUE, update on progress. Defaults to FALSE.
#' @param only_stocks If TRUE, only save stocks. If FALSE, auxiliaries and flows are saved using a callback function. Only applies if language is set to "Julia" in sim_specs() and no delay functions are used. Defaults to FALSE.
#' @param ... Optional arguments
#'
#' @return Object of class sdbuildR_sim, which is a list containing:
#' \describe{
#'   \item{df}{Dataframe, timeseries of computed variables in the ODE}
#'   \item{init}{Initial value of stocks}
#'   \item{constants}{Constant parameters}
#'   \item{script}{Simulation script}
#'   \item{duration}{Duration of simulation}
#'   \item{success}{If TRUE, simulation was successful. If FALSE, simulation failed.}
#'   \item{...}{Other parameters passed to simulate}
#' }
#' @export
#' @family simulate
#' @seealso [build()], [xmile()], [debugger()], [sim_specs()], [use_julia()]
#'
#' @examples sfm = xmile("SIR")
#' sim = simulate(sfm)
#' plot(sim)
#'
#' # Obtain all model variables
#' sim = simulate(sfm, only_stocks = FALSE)
#' plot(sim, add_constants = TRUE)
#'
#' # Use Julia for models with units or delay functions
#' sfm = xmile("coffee_cup") %>% sim_specs(language = "Julia")
#' use_julia()
#' sim = simulate(sfm)
#' plot(sim)
#'
#' # Close Julia session
#' use_julia(stop = TRUE)
#'
simulate = function(sfm,
                    format_code=TRUE,
                    keep_nonnegative_flow = TRUE,
                    keep_nonnegative_stock = FALSE,
                    keep_unit = TRUE,
                    only_stocks = TRUE,
                    verbose = FALSE,
                    ...){

  # First assess whether the model is valid
  problems = debugger(sfm, quietly = TRUE)
  if (nzchar(problems[["problems"]])){
    stop(problems[["problems"]])
  }

  if (tolower(sfm[["sim_specs"]][["language"]]) == "julia"){
    return(simulate_julia(sfm,
                          keep_nonnegative_flow = keep_nonnegative_flow,
                          keep_nonnegative_stock = keep_nonnegative_stock,
                          keep_unit = keep_unit, only_stocks = only_stocks,
                          verbose = verbose))

  } else if (tolower(sfm[["sim_specs"]][["language"]]) == "r"){

    # Remove - deSolve is now a required installation
    # if (!requireNamespace("deSolve", quietly = TRUE)){
    #   stop("deSolve is not installed! Please install deSolve to simulate in R,\nor simulate in Julia by setting\nsfm %>% sim_specs(language = 'Julia')")
    # }

    return(simulate_R(sfm,
                      format_code=format_code,
                      keep_nonnegative_flow = keep_nonnegative_flow,
                      keep_nonnegative_stock = keep_nonnegative_stock,
                      only_stocks = only_stocks,
                      verbose = verbose))
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
  possible_func_in_model = c(names(sfm[["macro"]]), names(sfm[["model"]][["variables"]][["gf"]]))

  possible_func = c(possible_func_in_model,
                    get_syntax_julia()[["syntax_df"]][["R_first_iter"]],
                    unlist(P),
                    # Remove base R names
                    "pi", "letters", "LETTERS",
                    "month.abb", "month.name")

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

    missing_ref_format = missing_ref %>%
      purrr::imap(function(x, name){
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
    as.data.frame()
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
#' @family internal
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
         var_names = unique(sim[[P$sim_df_name]][["variable"]]),
         nrow = nrow(sim[[P$sim_df_name]]),
         ncol = ncol(sim[[P$sim_df_name]]),
         n_pars = length(sim[[P[["parameter_name"]]]]),
         language = sim$sfm[["sim_specs"]][["language"]],
         method = sim$sfm$sim_specs$method
    )
  }

  prop1 = get_prop(sim1)
  prop2 = get_prop(sim2)

  overlapping_var_names = intersect(prop1$var_names, prop2$var_names)
  nonoverlapping_var_names = setdiff(union(prop1$var_names, prop2$var_names), overlapping_var_names)

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

  df = lapply(overlapping_var_names,
              function(name){c(name = name,
                               check_diff(sim1$df[sim1$df$variable == name, "value"],
                                          sim2$df[sim2$df$variable == name, "value"]))}) %>%
    do.call(dplyr::bind_rows, .) %>%
    as.data.frame()

  return(list(
    equal = all(as.logical(as.numeric(df$equal))),
    overlapping_var_names = overlapping_var_names,
    nonoverlapping_var_names = nonoverlapping_var_names,
    msg = paste0("The following columns are not equal:\n",
                 paste0(df$name, ": ", df$first_diff, " (", df$nr_diff, " differences, max diff: ", df$max_diff, ")\n", collapse = ""),
                 "\n"),
    prop1 = prop1,
    prop2 = prop2,
    df = df)
  )


}


#' Run ensemble simulation
#'
#' Run an ensemble simulation of a stock-and-flow model, varying initial conditions and/or parameters in the range specified in `range`. By default, the ensemble is run in parallel using multiple threads. The results are returned as a dataframe with summary statistics and optionally individual simulations.
#'
#' To run large simulations, it is recommended to limit the output size by saving fewer values. Note that if specified, the seed in sim_specs() is ignored for ensemble simulations.
#'
#' If you do not see any variation within one condition of the ensemble (i.e. the confidence bands are very narrow), there are likely no random elements in your model. Without these, there can be no variability within a condition. Try specifying a random initial condition or adding randomness to other model elements.
#'
#' @inheritParams build
#' @inheritParams simulate
#' @param n Number of simulations to run in the ensemble. When range is specified, n defines the number of simulations to run per condition. If each condition only needs to be run once, set n = 1. Defaults to 10.
#' @param threaded If TRUE, run the simulation on multiple threads. The number of threads is taken from the use_julia() argument JULIA_NUM_THREADS, which defaults to four. Defaults to TRUE.
#' @param return_sims If TRUE, return the individual simulations in the ensemble. Set to FALSE to save memory. Defaults to FALSE.
#' @param range List of ranges to vary parameters in the ensemble. Only stocks and constants can be specified. All ranges have to be of the same length if cross = FALSE. Defaults to NULL.
#' @param cross If TRUE, cross the parameters in the range list to generate all possible combinations of parameters. Defaults to TRUE.
#' @param quantiles Quantiles to calculate in the summary, e.g. c(0.025, 0.975).
#'
#' @returns Object of class sdbuildR_ensemble, which is a list containing:
#' \describe{
#'  \item{success}{If TRUE, simulation was successful. If FALSE, simulation failed.}
#'  \item{error_message}{If success is FALSE, contains the error message.}
#'  \item{df}{Dataframe with simulation results in long format, if return_sims is TRUE. The iteration number is indicated by column "i". If range was specified, the condition is indicated by column "j".}
#'  \item{summary}{Dataframe with summary statistics of the ensemble, including quantiles specified in quantiles. If range was specified, summary statistics are calculated for each condition (j) in the ensemble.}
#'  \item{n}{Number of simulations run in the ensemble (per condition j if range is specified).}
#'  \item{n_total}{Total number of simulations run in the ensemble (across all conditions if range is specified).}
#'  \item{n_conditions}{Total number of conditions.}
#'  \item{conditions}{Dataframe with the conditions used in the ensemble, if range is specified.}
#'  \item{init}{Dataframe with the initial values of the stocks used in the ensemble. The first two columns are "j" (iteration number) and "i" (parameter index), followed by the stock names.}
#'  \item{constants}{Dataframe with the constant parameters used in the ensemble. The first two columns are "j" (iteration number) and "i" (parameter index), followed by the parameter names.}
#'  \item{script}{Julia script used for the ensemble simulation.}
#'  \item{duration}{Duration of the simulation in seconds.}
#'  \item{...}{Other parameters passed to ensemble}
#'  }
#' @export
#' @family simulate
#' @seealso [build()], [xmile()], [sim_specs()], [use_julia()]
#'
#' @examples
#'# Load example and set simulation language to Julia
#'sfm = xmile("predator-prey") %>% sim_specs(language = "Julia")
#'
#'# Set random initial conditions
#'sfm = sfm %>%
#'  build(c("predator", "prey"), eqn = "runif(1, min = 20, max = 80)")
#'
#'# For ensemble simulations, it is highly recommended to reduce the
#'# returned output. For example, to save only every 1 time units and discard
#'# the first 100 time units, use:
#'sfm = sfm %>% sim_specs(save_at = 1, save_from = 100)
#'
#'# Run ensemble simulation with 100 simulations
#'sims = ensemble(sfm, n = 100)
#'plot(sims)
#'
#'# Plot individual trajectories
#'sims = ensemble(sfm, n = 10, return_sims = TRUE)
#'plot(sims, type = "sims")
#'
#'# Specify which trajectories to plot
#'plot(sims, type = "sims", i = 1)
#'
#'# Plot the median with lighter individual trajectories
#'plot(sims, central_tendency = "median", type = "sims", alpha = 0.1)
#'
#'# Ensembles can also be run with exact values for the initial conditions
#'# and parameters. Below, we vary the initial values of the predator and the
#'# birth rate of the predators (delta). We generate a hunderd samples per
#'# condition. By default, the parameters are crossed, meaning that all
#'# combinations of the parameters are run.
#'sims = ensemble(sfm, n = 50,
#'                range = list("predator" = c(10, 50),
#'                             "delta" = c(.025, .05)))
#'
#'plot(sims)
#'
#'# By default, a maximum of nine conditions is plotted.
#'# Plot specific conditions:
#'plot(sims, j = c(1, 3), nrows = 1)
#'
#'# Generate a non-crossed design, where the length of each range needs to be
#'# equal:
#'sims = ensemble(sfm, n = 10,
#'                range = list("predator" = c(10, 20, 30),
#'                             "delta" = c(.020, .025, .03)), cross = FALSE)
#'plot(sims, nrows = 3)
#'
#'# Run simulation not in parallel
#'sims = ensemble(sfm, n = 10, threaded = FALSE)
#'
#'# To run simulations with more threads than the default, set e.g.:
#'use_julia(JULIA_NUM_THREADS = 10)
#'
#' # Close Julia
#' use_julia(stop = TRUE)
#'
ensemble = function(sfm,
                    n = 10,
                    threaded = TRUE,
                    return_sims = FALSE,
                    range = NULL,
                    cross = TRUE,
                    quantiles = c(0.025, 0.975),
                    keep_nonnegative_flow = TRUE,
                    keep_nonnegative_stock = FALSE,
                    keep_unit = TRUE,
                    only_stocks = TRUE,
                    verbose = TRUE
){

  check_xmile(sfm)

  # Collect arguments
  argg <- c(
    as.list(environment()))
  # Remove NULL arguments
  argg = argg[!lengths(argg) == 0]
  # Remove some elements
  # argg[c("sfm")] = NULL

  if (tolower(sfm[["sim_specs"]][["language"]]) != "julia"){
    stop("Ensemble simulations are only supported for Julia models. Please set sfm %>% sim_specs(language = 'Julia').")
  }

  if (!is.numeric(n)){
    stop("n should be a numerical value!")
  }

  if (n <= 0){
    stop("The number of simulations must be greater than 0!")
  }

  if (!is.numeric(quantiles)){
    stop("quantiles should be a numerical vector with quantiles to calculate!")
  }

  if (length(unique(quantiles)) < 2){
    stop("quantiles should have a minimum length of two!")
  }

  if (any(quantiles < 0 | quantiles > 1)){
    stop("quantiles should be between 0 and 1!")
  }

  if (!is.logical(cross)){
    stop("cross should be TRUE or FALSE!")
  }

  if (!is.logical(threaded)){
    stop("threaded should be TRUE or FALSE!")
  }

  if (!is.logical(return_sims)){
    stop("return_sims should be TRUE or FALSE!")
  }

  if (!is.logical(only_stocks)){
    stop("only_stocks should be TRUE or FALSE!")
  }

  if (!is.null(range)){

    if (!is.list(range)){
      stop("range must be a named list! Please provide a named list with ranges for the parameters to vary in the ensemble.")
    }

    if (length(range) == 0){
      stop("range must be a named list with at least one element! Please provide a named list with ranges for the parameters to vary in the ensemble.")
    }

    if (is.null(names(range))){
      stop("range must be a named list! Please provide a named list with ranges for the parameters to vary in the ensemble.")
    }

    # All must be numerical values
    if (!all(sapply(range, is.numeric))){
      stop("All elements in range must be numeric vectors!")
    }

    # Test that names are unique
    if (length(unique(names(range))) != length(range)){
      stop("All names in range must be unique! Please check the names of the elements in range.")
    }

    # All varied elements must exist in the model
    names_df = get_names(sfm)
    names_range = names(range)
    idx = names_range %in% names_df$name
    if (any(!idx)){
      stop(paste0("The following names in range do not exist in the model: ",
                  paste0(names_range[!idx], collapse = ", ")))
    }

    # All varied elements must be a stock or constant
    idx = names_range %in% c(names_df[names_df[["type"]] %in% c("stock", "constant"), "name"])
    if (any(!idx)){
      stop(paste0("Only constants or the initial value of stocks can be varied. Please exclude: ",
                  paste0(names_range[!idx], collapse = ", ")))
    }

    # All ranges must be of the same length if not a crossed design
    range_lengths = sapply(range, length)
    if (!cross){
      if (length(unique(range_lengths)) != 1){
        stop("All ranges must be of the same length when cross = FALSE! Please check the lengths of the ranges in range.")
      }

      n_conditions = unique(range_lengths)
    } else {
      # Compute the total number of conditions
      n_conditions = prod(range_lengths)
    }

    # Alphabetically sort the ensemble parameters
    range = range[sort(names(range))]


  } else {
    n_conditions = 1
  }


  if (verbose){
    message(paste0("Running a total of ", n * n_conditions,
                   " simulation", ifelse((n * n_conditions) == 1, "", "s"),
                   ifelse(is.null(range), "", paste0(" for ", n_conditions, " condition",
                                                     ifelse(n_conditions == 1, "", "s"),
                   " (",
                                                     n, " simulation",
                                                     ifelse(n == 1, "", "s"),
                                                     " per condition)")), "\n"))
  }

  # Create ensemble parameters
  ensemble_pars = list(n = n,
                       threaded = threaded,
                       quantiles = quantiles,
                       return_sims = return_sims,
                       range = range, cross = cross)


  # Get output filepaths
  ensemble_pars[["filepath_df"]] = get_tempfile(fileext = ".csv")
  ensemble_pars[["filepath_summ"]] = get_tempfile(fileext = ".csv")
  filepath = get_tempfile(fileext = ".jl")

  # Compile script
  script = compile_julia(sfm, filepath_sim = "",
                         ensemble_pars = ensemble_pars,
                         keep_nonnegative_flow = keep_nonnegative_flow,
                         keep_nonnegative_stock = keep_nonnegative_stock,
                         only_stocks = only_stocks,
                         keep_unit = keep_unit)
  write_script(script, filepath)
  script = paste0(readLines(filepath), collapse = "\n")


  # Evaluate script
  sim = tryCatch({

    # Evaluate script
    use_julia()

    start_t = Sys.time()

    # Wrap in invisible and capture.output to not show message of units module being overwritten
    invisible(utils::capture.output(
      JuliaConnectoR::juliaEval(paste0('include("', filepath, '")'))
      ))

    end_t = Sys.time()

    if (verbose){
      message(paste0("Simulation took ", round(end_t - start_t, 4), " seconds\n"))
    }

    # Delete file
    file.remove(filepath)

    # Read the total number of simulations
    n = JuliaConnectoR::juliaEval(P[["ensemble_n"]])
    n_total = JuliaConnectoR::juliaEval(P[["ensemble_total_n"]])

    # Read the ensemble conditions
    if (!is.null(ensemble_pars[["range"]])){

      conditions = JuliaConnectoR::juliaEval(paste0("Matrix(hcat(", P[["ensemble_pars"]], "...)')"))
      colnames(conditions) = names(ensemble_pars[["range"]])
      conditions = cbind(j = 1:nrow(conditions), conditions)

      } else {

        conditions = NULL

    }

    # Read the constants
    constants = JuliaConnectoR::juliaEval(P[["parameter_name"]])
    colnames(constants) = c("j", "i", JuliaConnectoR::juliaEval(P[["parameter_names"]]))

    # Read the initial values of stocks
    init = JuliaConnectoR::juliaEval(P[["initial_value_name"]])
    colnames(init) = c("j", "i", JuliaConnectoR::juliaEval(P[["initial_value_names"]]))

    # Read the simulation results
    if (return_sims){
      df = as.data.frame(data.table::fread(ensemble_pars[["filepath_df"]], na.strings = c("", "NA")))

      # Delete files
      file.remove(ensemble_pars[["filepath_df"]])
    } else {
      df = NULL
    }

    # Read the summary file
    summary = as.data.frame(data.table::fread(ensemble_pars[["filepath_summ"]], na.strings = c("", "NA")))

    # Delete files
    file.remove(ensemble_pars[["filepath_summ"]])

    list(success = TRUE,
         df = df,
         summary = summary,
         n = n,
         n_total = n_total,
         n_conditions = n_conditions,
         conditions = conditions,
         init = init,
         constants = constants,
         script = script,
         duration = end_t - start_t) %>% utils::modifyList(argg) %>%
      structure(., class = "sdbuildR_ensemble")

  },
  error = function(e) {
    warning("\nAn error occurred while running the Julia script.")
    list(success = FALSE, error_message = e[["message"]],
         script = script) %>% utils::modifyList(argg) %>%
      structure(., class = "sdbuildR_ensemble")
  })


  return(sim)

}


