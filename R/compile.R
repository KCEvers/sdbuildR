
#' Simulate stock-and-flow model
#'
#' Simulate a stock-and-flow model with simulation specifications defined by `sim_specs()`. If not already run, the Julia environment will be set up with `sdbuildR_setup()`. If any problems are detected by `debugger()`, the model cannot be simulated.
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
#' @return List with variables created in the simulation script
#' \describe{
#'   \item{dt}{Numeric, the timestep}
#'   \item{times}{Numeric, sequence of time values}
#'   \item{ode_func}{Function, the ODE function}
#'   \item{pars}{List, constant parameters (i.e. static Auxiliaries)}
#'   \item{xstart}{Numeric, initial value of Stocks}
#'   \item{df}{Dataframe, timeseries of computed variables in the ODE}
#'   \item{...}{Other variables created in the simulation script.}
#' }
#' @export
#'
#' @examples sfm = xmile("SIR")
#' sim = simulate(sfm)
#' plot(sfm)
#'
#' sfm = xmile("logistic_model")
#' sim = simulate(sfm)
#' plot(sfm)
#'
#' sfm = xmile("predator-prey")
#' sim = simulate(sfm)
#' plot(sfm)
#'
simulate = function(sfm,
                    format_code=TRUE,
                    keep_nonnegative_flow = TRUE,
                    keep_nonnegative_stock = FALSE,
                    keep_unit = TRUE,
                    verbose = FALSE,
                    debug = FALSE, only_stocks = FALSE, ...){

  # First assess whether the model is valid
  problems = debugger(sfm, quietly = TRUE)
  if (nzchar(problems$problems)){
    stop(problems$problems)
  }

  if (tolower(sfm$sim_specs$language) == "julia"){
    return(simulate_Julia(sfm,
                          format_code=format_code,
                          keep_nonnegative_flow = keep_nonnegative_flow,
                          keep_nonnegative_stock = keep_nonnegative_stock,
                          keep_unit = keep_unit, verbose = verbose, debug = debug, only_stocks = only_stocks))
  } else if (tolower(sfm$sim_specs$language) == "r"){

    if (!requireNamespace("deSolve", quietly = TRUE)){
      stop("deSolve is not installed! Please install deSolve to simulate in R, or simulate in Julia by setting\nsfm %>% sim_specs(language = 'Julia')")
    }

    return(simulate_R(sfm,
                      format_code=format_code,
                      keep_nonnegative_flow = keep_nonnegative_flow,
                      keep_nonnegative_stock = keep_nonnegative_stock,
                      verbose = verbose, debug = debug))
  } else {
    stop("Language not supported. Please run either sfm %>% sim_specs(language = 'Julia') (recommended) or sfm %>% sim_specs(language = 'R') (no unit support).")
  }

}



#' Compile script to simulate stock-and-flow model in either Julia or R
#'
#' @inheritParams build
#' @inheritParams insightmaker_to_sfm
#' @inheritParams simulate
#'
#' @return Julia or R script
#'
compile = function(sfm,
                   format_code=TRUE,
                   keep_nonnegative_flow = TRUE,
                   keep_nonnegative_stock = FALSE,
                   keep_unit = TRUE, verbose = FALSE, debug = FALSE, only_stocks = FALSE){


  if (tolower(sfm$sim_specs$language) == "julia"){
    return(compile_Julia(sfm,
                                    format_code=format_code,
                                    keep_nonnegative_flow = keep_nonnegative_flow,
                                    keep_nonnegative_stock = keep_nonnegative_stock,
                                    keep_unit = keep_unit, verbose = verbose, debug = debug, only_stocks = only_stocks))
  } else if (tolower(sfm$sim_specs$language) == "r"){
    return(compile_R(sfm,
                     format_code=format_code,
                     keep_nonnegative_flow = keep_nonnegative_flow,
                     keep_nonnegative_stock = keep_nonnegative_stock,
                     verbose = verbose, debug = debug))
  } else {
    stop("Language not supported. Please run either sfm %>% sim_specs(language = 'Julia') (recommended) or sfm %>% sim_specs(language = 'R') (no unit support).")
  }

}




#' Detect undefined variables in equations
#'
#' @inheritParams build
#'
#' @return List with issue and message
#'
detect_undefined_var = function(sfm){

  # Get names
  # names_df = get_names(sfm)
  var_names = get_model_var(sfm)

  # Find references to variables which are not in names_df$name
  missing_ref = unlist(sfm$model$variables, recursive = FALSE, use.names = FALSE) %>%
    lapply(., function(x){
    # Find dependencies, and find which ones are not in names_df$name
      y = x[names(x) %in% c("eqn", "to", "from")]
      y = y[sapply(y, is_defined)]
    A = sapply(y, function(z){
      setdiff(unlist(find_dependencies(sfm, z, only_var = TRUE, only_model_var = FALSE)),
              c(unlist(P), var_names,
                # Remove base R names
                "pi", "letters", "LETTERS",
                "month.abb", "month.name"))
    })
    A = A[lengths(A) > 0]
    if (length(A) == 0){
      return(NULL)
    } else {
      return(list(A) %>% stats::setNames(., x$name))
    }
})
  # missing_ref = missing_ref[!unlist(lapply(missing_ref, is.null))]
  missing_ref = unlist(missing_ref, recursive = FALSE)

  if (length(missing_ref) > 0){

    missing_ref_format = missing_ref %>% purrr::imap(function(x, name){
      purrr::imap(x, function(y, prop){
        paste0("- ", name, "$", prop, ": ", paste0(unname(y), collapse = ", "))
      })
    }) %>% unlist() %>% unname()

    return(list(issue = TRUE,
                msg = paste0(c("The variable properties below contain references to undefined variables. Please define the missing variables or correct any spelling mistakes.",
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
    as.data.frame() %>% dplyr::distinct() %>% as.matrix() %>% t() %>% c()
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



#' Find dependencies in equation
#'
#' @param eqns String with equation to find dependencies in; defaults to NULL to find dependencies of all variables.
#' @inheritParams build
#' @param only_var If TRUE, only look for variable names, not functions.
#' @param only_model_var If TRUE, only look for dependencies on other model variables.
#'
#' @return Vector of dependencies (variable names in equation)
#'
find_dependencies = function(sfm, eqns = NULL, only_var = TRUE, only_model_var = TRUE){

  var_names = get_model_var(sfm)

  if (is.null(eqns)){
    eqns = unlist(unname(lapply(sfm$model$variables, function(x){lapply(x, `[[`, "eqn")})), recursive = FALSE)
  }

  dependencies = lapply(eqns, function(eqn){

    # Parse the line as an expression
    expr <- tryCatch(parse(text = eqn), error = function(e) NULL)

    # If parsing was successful, extract variable names from equations
    if (!is.null(expr)) {
      d = all.names(expr, functions = !only_var, unique = TRUE)

      if (only_model_var){
        d = d[d %in% var_names]
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
#'
order_equations <- function(sfm, print_msg = TRUE){

  # Separate auxiliary variables into static parameters and dynamically updated auxiliaries
  dependencies = sfm$model$variables %>%
    purrr::map_depth(2, function(x){

      if (is_defined(x$eqn)){
        d = unlist(find_dependencies(sfm, x$eqn, only_var = TRUE, only_model_var = TRUE))
      } else {
        d = c()
      }
      return(d)
    })


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

  return(list(static = static, dynamic = dynamic))

}



