
#' Create a new stock-and-flow model
#'
#' Initialize an empty stock-and-flow model of class sdbuildR_xmile. You can either create an empty stock-and-flow model or load a template from the model library.
#'
#' @param name Name of the template to load. If NULL, an empty stock-and-flow model will be created with default simulation parameters and a default header.
#'
#' @return Stock-and-flow model of class sdbuildR_xmile.
#' @export
#' @seealso [build()]
#'
#' @examples sfm = xmile()
#' summary(sfm)
#'
#' # Load a template
#' sfm = xmile("Lorenz")
#' sim = simulate(sfm)
#' plot(sim)
xmile <- function(name = NULL){

  if (!is.null(name)){
    return(template(name))
  }

  header_defaults <- as.list(formals(header))
  header_defaults <- header_defaults[!names(header_defaults) %in% c("sfm", "...")]

  spec_defaults <- as.list(formals(sim_specs))
  spec_defaults <- spec_defaults[!names(spec_defaults) %in% c("sfm", "...")]
  spec_defaults$saveat = spec_defaults$dt

  obj = list(
    header = header_defaults,
    sim_specs = spec_defaults,
    # global = list(),
    # behavior = list(stock = list(non_negative = "FALSE"),
    #                 flow = list(non_negative = "FALSE")),
             model = list(
               variables = list(
                 stock = list(),
                 constant = list(),
                              aux = list(),
                              flow = list(),
                 # module = list(), # for submodels
                              gf = list())),
             macro = list(),
             model_units = list(),
             display = list(variables = c()))

  sfm = structure(obj, class = "sdbuildR_xmile")

  return(sfm)
}



#' Safely check whether value is defined
#'
#' @param x Value
#'
#' @return Boolean; whether the value is defined
#' @noRd
is_defined = function(x){

  # Safely check whether x is defined
  if (length(x) == 0){
    return(FALSE)
  } else {
    if (any(is.na(x))){
      return(FALSE)
    } else {
      return(any(nzchar(x)))
    }
  }

}



#' Plot stock-and-flow diagram
#'
#' Visualise a stock-and-flow diagram using DiagrammeR. Stocks are represented as boxes. Flows are represented as arrows between stocks and/or double circles, where the latter represent what it outside of the model boundary.
#'
#' @param x Stock-and-flow model of class sdbuildR_xmile
#' @param format_label If TRUE, apply default formatting to labels if labels are the same as variable names.
#' @param wrap_width Width of text wrapping for labels. Must be an integer.
#' @param center_stocks If TRUE, stocks are centered in the diagram. Defaults to FALSE.
#' @param ... Optional arguments
#'
#' @return Stock-and-flow diagram plotted with DiagrammeR()
#' @export
#' @method plot sdbuildR_xmile
#' @seealso [insightmaker_to_sfm()]
#'
#' @examples
#' sfm = xmile("SIR")
#' plot(sfm)
plot.sdbuildR_xmile = function(x, format_label = TRUE, wrap_width = 25, center_stocks = FALSE, ...){

  check_xmile(x)
  sfm = x

  # Check whether there are any variables
  nr_var = sum(lengths(sfm$model$variables))
  if (nr_var == 0){
    stop("Your model contains no variables!")
  }

  # names_df = get_names(sfm)
  df = as.data.frame(sfm, properties = c("type", "name", "label", "eqn"))

  if (format_label){
    df$label = ifelse(df$name == df$label, stringr::str_replace_all(df$label, c("_" = " ", "\\." = " ", "  " = " ")), df$label)
  }

  # Text wrap to prevent long names
  df$label = stringr::str_wrap(df$label, width = wrap_width)
  dict = stats::setNames(df$label, df$name)

  # Get equations and remove quotation marks from unit strings
  dict_eqn = stats::setNames(stringr::str_replace_all(df$eqn, c("'" = "", "\"" = "")), df$name)

  stock_names = df[df$type == "stock", "name"]
  flow_names = df[df$type == "flow", "name"]

  # Prepare all nodes
  if (length(stock_names) > 0){
    stock_nodes = sprintf("%s [label='%s',tooltip = 'eqn = %s',shape=box,style=filled,fillcolor=lightblue,fontsize=18,fontname='times bold']", paste0("'", stock_names, "'"), dict[stock_names], dict_eqn[stock_names])
  } else {
    stock_nodes = ""
  }

  cloud_nodes = flow_edges = ""
  if (length(flow_names) > 0){

    # Create dataframe with direction of flows
    flow_df <- get_flow_df(sfm)
    # If the flow is to a stock that doesn't exist, remove
    flow_df$from = ifelse(flow_df$from %in% stock_names, flow_df$from , "")
    flow_df$to = ifelse(flow_df$to %in% stock_names, flow_df$to , "")
    flow_df = as.matrix(flow_df)

    flow_df

    # Fill in NA in flow_df with numbered clouds; these are flows that are flowing from the external environment, not another stock
    idxs = which(flow_df == "")
    if (length(idxs) > 0){
      cloud_names = paste0("Cloud", 1:length(idxs))
      flow_df[idxs] = cloud_names
      # External environment is represented as a cloud
      cloud_nodes = sprintf("%s [label='', tooltip = '',shape=doublecircle, fixedsize=true, width = .25, height = .25,orientation=15]",
                            paste0("'", cloud_names, "'"))
    }

    # Define edges
    flow_edges = paste0(
      paste0("'", flow_df[, "from"], "'"),
      " -> ",
      paste0("'", flow_df[, "to"], "'"),
      " [arrowhead='normal', label='", dict[flow_df[, "name"]], "', tooltip = 'eqn = ", dict_eqn[flow_df[, "name"]], "', fontsize=18,fontname='times bold', color='black:LightSalmon:black',arrowsize = 1.2,penwidth=1.1,minlen=3]")


  }

  # Compile string for diagram
  viz_str = sprintf("
    digraph stock_and_flow {

      graph [layout = dot, rankdir = LR, center=true]
      margin=0 # in inches

      # Define stock nodes
      %s

      # Define external cloud nodes
      %s

      # Define edges between clouds and flows and between flows and stocks
      %s

      %s
    }
          ", stock_nodes %>% rev() %>% paste0(collapse = "\n\t\t"),
                    cloud_nodes %>% paste0(collapse = "\n\t\t"),
                    flow_edges %>% paste0(collapse = "\n\t\t"),
                    ifelse(center_stocks, paste0("{rank = same; ", paste0(stock_names, collapse = ", "), "}"), "")

  )

  # Assign levels (ranks)
  #{ rank = min; %s }      # Top level
  #{ rank = same; %s }   # Middle level
  #{ rank = max; %s }      # Bottom level

  pl = DiagrammeR::grViz(viz_str)
  pl
  return(pl)
}





#' Get the sources and destinations of flows
#'
#' @inheritParams build
#'
#' @returns Dataframe with for each flow which stock and flow to and/or from
#' @noRd
get_flow_df = function(sfm){

  check_xmile(sfm)

  flow_to = sfm$model$variables$flow %>% get_map("to")
  flow_from = sfm$model$variables$flow %>% get_map("from")

  dplyr::bind_cols(name = names(flow_to),
                   to = unname(flow_to),
                   from = unname(flow_from)) %>% as.data.frame()
}



#' Plot timeseries of simulation
#'
#' Visualise simulation results of a stock-and-flow model. Plot the evolution of stocks over time, with the option of also showing other model variables.
#'
#' @param x Output of simulate().
#' @param add_constants If TRUE, include constants in plot. Defaults to FALSE.
#' @param palette Colour palette. Must be one of hcl.pals().
#' @param colors Vector of colours. If NULL, the colour palette will be used. If specified, will override palette. The number of colours must be equal to the number of variables in the simulation dataframe. Defaults to NULL.
#' @param title Plot title. Character. Defaults to the name in the header of the model.
#' @param family Font family. Defaults to "Times New Roman".
#' @param size Font size. Defaults to 16.
#' @param ... Optional parameters
#'
#' @return Plot object
#' @export
#' @seealso [simulate()]
#' @method plot sdbuildR_sim
#'
#' @examples
#' sfm = xmile("SIR")
#' sim = simulate(sfm)
#' plot(sim)
plot.sdbuildR_sim = function(x, add_constants = FALSE,
                             palette = "Dark 2",
                             colors = NULL,
                             title=x[["sfm"]][["header"]][["name"]],
                             family = "Times New Roman",
                             size = 16,
                             ...){


  #@param pkg Plotting package. Defaults to "plotly".

  if (missing(x)){
    stop("No simulation data provided! Use simulate() to create a simulation.")
  }

  # Check whether it is an xmile object
  if (!inherits(x, "sdbuildR_sim")){
    stop("This is not an object of class sdbuildR_sim! Simulate a stock-and-flow model with simulate().")
  }

  if (x$success == FALSE){
    stop("Simulation failed!")
  }

  names_df = get_names(x$sfm)
  stock_names = names_df[names_df$type == "stock", ]
  stock_names = stats::setNames(stock_names$name, stock_names$label)
  nonstock_names = names_df[names_df$type != "stock", ]
  nonstock_names = stats::setNames(nonstock_names$name, nonstock_names$label)

  if (length(stock_names) == 0){
    stop("No stocks to plot!")
  }

  if (nrow(x$df) == 0){
    stop("Dataframe has no rows!")
  }

  if (add_constants){
    if (length(x$pars) > 0){
      x$df = x$df %>% cbind(unclass(x$pars))
    }
  }

  nonstock_names = nonstock_names[unname(nonstock_names) %in% colnames(x$df)]

  if (requireNamespace("plotly", quietly = TRUE)){

    # Put dataframe in long format
    x_col = "time"

    x$df = x$df[, intersect(colnames(x$df),
                            c(x_col, unname(stock_names), unname(nonstock_names))),
                drop = FALSE]

    # Wide to long
    df_long <- stats::reshape(
      data = as.data.frame(x$df),
      direction = "long",
      idvar = "time",
      varying = colnames(x$df)[colnames(x$df) != "time"],
      # varying = which(colnames(x$df) != "time"),
      v.names = "value",
      timevar = "variable",
      # Ensure variable names are used
      times = colnames(x$df)[colnames(x$df) != "time"]
    ) %>% magrittr::set_rownames(NULL)

    n = length(unique(df_long$variable))

    # Don't add check for template in hcl.pals(), because hcl is more flexible in palette names matching.

    if (is.null(colors)){
      colors = grDevices::hcl.colors(n = n, palette = palette)
    } else {
      # Ensure there are enough colors
      if (length(colors) < n){
        stop(paste0("Length of colors (", length(colors), ") must be equal to the number of variables in the simulation dataframe (", n, ").\nUsing template instead..."))
        colors = grDevices::hcl.colors(n = n, palette = palette)
      } else {
        # Cut number of colors to number of variables
        colors = colors[1:n]
      }
    }

    # The colors are unintuitively plotted from back to front
    colors = rev(colors)

    # Initialize plotly object
    pl <- plotly::plot_ly()

    # Add traces for stock variables (visible = TRUE)
    if (length(stock_names) > 0) {
      # for (var in stock_names) {
      pl <- pl %>%
        plotly::add_trace(
          data = df_long[df_long$variable %in% stock_names, ],
          x = ~get(x_col),
          y = ~value,
          color = ~variable,
          type = "scatter",
          mode = "lines",
          colors = colors,
          visible = TRUE
        )
      # }
    }

    # Add traces for non-stock variables (visible = "legendonly")
    if (length(nonstock_names) > 0) {
      pl <- pl %>%
        plotly::add_trace(
          data = df_long[df_long$variable %in% nonstock_names, ],
          x = ~get(x_col),
          y = ~value,
          color = ~variable,
          type = "scatter",
          mode = "lines",
          visible = "legendonly"
        )
    }

    matched_time_unit = find_matching_regex(x$sfm$sim_specs$time_units, get_regex_time_units())

    # Customize layout
    pl <- pl %>% plotly::layout(
      showlegend = TRUE,
      title = title,
      xaxis = list(title = paste0("Time (", matched_time_unit, ")")),
      yaxis = list(title = ""),
      font=list(
        family = family, size = size),
      margin = list(t = 50, b = 50, l = 50, r = 50)  # Increase top margin to 100 pixels
    )

  } else {

    message("No installation of plotly found, reverting to base R...")

    # Fallback to base R plotting
    plot(x$df$time, x$df[[stock_names[1]]], type = "l", col = "blue", lwd = 2,
         xlab = paste0("Time (", x$sfm$sim_specs$time_units, ")"), ylab = "",
         main = title)

    for (var in stock_names[-1]) {
      graphics::lines(x$df$time, x$df[[var]], lwd = 2)
    }

    if (length(nonstock_names) > 0) {
      for (var in nonstock_names) {
        graphics::lines(x$df$time, x$df[[var]], lwd = 2, lty = 2)
      }
    }
  }

  return(pl)
}


#' Create dataframe of simulation results
#'
#' Convert simulation results to a dataframe. The first column is time, followed by all stocks, and then all other auxiliary and flow variables.
#'
#' @inheritParams plot.sdbuildR_sim
#'
#' @returns Dataframe with simulation results
#' @export
#' @seealso [simulate()], [xmile()]
#' @method as.data.frame sdbuildR_sim
#'
#' @examples
#' sfm = xmile("SIR")
#' sim = simulate(sfm)
#' head(as.data.frame(sim))
#'
#'
as.data.frame.sdbuildR_sim = function(x, ...){
  # Check whether it is an xmile object
  if (!inherits(x, "sdbuildR_sim")){
    stop("This is not an object of class sdbuildR_sim! Simulate a stock-and-flow model with simulate().")
  }

  return(as.data.frame(x$df))
}



#' Print overview of stock-and-flow model
#'
#' Print summary of stock-and-flow model, including number of stocks, flows, constants, auxiliaries, graphical functions, macros, custom model units, and use of delay functions. Also prints simulation specifications.
#'
#' @param object Stock-and-flow model of class sdbuildR_xmile
#' @inheritParams plot.sdbuildR_xmile
#'
#' @return NULL
#' @export
#' @seealso [build()]
#'
#' @examples
#' sfm = xmile("SIR")
#' summary(sfm)
summary.sdbuildR_xmile <- function(object, ...) {

  stocks = names(object$model$variables$stock)
  flows = names(object$model$variables$flow)
  constants = names(object$model$variables$constant)
  auxs = names(object$model$variables$aux)
  gfs = names(object$model$variables$gf)
  model_units_str = names(object$model_units)
  macro_str = lapply(object$macro, `[[`, "property") %>% unlist() %>% Filter(nzchar, .)

  ans = ""
  ans = paste0(ans, "Your model contains:\n")
  ans = paste0(ans, sprintf("* %d Stocks%s%s\n", length(stocks), ifelse(length(stocks) > 0, ": ", ""), paste0(stocks, collapse = ", ")))
  ans = paste0(ans, sprintf("* %d Flows%s%s\n", length(flows), ifelse(length(flows) > 0, ": ", ""), paste0(flows, collapse = ", ")))
  ans = paste0(ans, sprintf("* %d Constants%s%s\n", length(constants), ifelse(length(constants) > 0, ": ", ""), paste0(constants, collapse = ", ")))
  ans = paste0(ans, sprintf("* %d Auxiliaries%s%s\n", length(auxs), ifelse(length(auxs) > 0, ": ", ""), paste0(auxs, collapse = ", ")))
  ans = paste0(ans, sprintf("* %d Graphical Functions%s%s\n", length(gfs), ifelse(length(gfs) > 0, ": ", ""), paste0(gfs, collapse = ", ")))
  ans = paste0(ans, sprintf("* %d Custom model units%s%s\n", length(model_units_str),
              ifelse(length(model_units_str) > 0, ": ", ""), paste0(model_units_str, collapse = ", ")))
  ans = paste0(ans, sprintf("* %d Macro%s\n", length(macro_str), ifelse(length(macro_str) == 1, "", "s")))


  # Check for use of past() or delay()
  # intermediary = unname(object$model$variables) %>% purrr::list_flatten() %>%
  #   purrr::map("intermediary") %>% purrr::compact()
  # past_list = list_extract(object$model$variables, "past")
  # delay_list = list_extract(object$model$variables, "delay")
  # intermediary = c(past_list, delay_list)
  delay_past = get_delay_past(object)

  if (length(delay_past) > 0){

    names_delay = unique(names(delay_past))

    ans = paste0(ans, "\nDelay family functions:\n")
    ans = paste0(ans, sprintf("* %d variable%s uses past() or delay(): %s\n",
                length(names_delay),
                ifelse(length(names_delay) == 1, "", "s"), paste0(names_delay, collapse = ", ")))
  }

  # Check for use of delayN() and smoothN()
  delay_func = get_delayN_smoothN(object)

  if (length(delay_func) > 0){

    delay_func_names = unique(names(delay_func))
    ans = paste0(ans, "\n\nDelay family functions:\n")
    ans = paste0(ans, sprintf("* %d variable%s uses delayN() or smoothN(): %s\n", length(delay_func_names),
                ifelse(length(delay_func_names) == 1, "", "s"), paste0(delay_func_names, collapse = ", ")))
  }

  matched_time_unit = find_matching_regex(object$sim_specs$time_units, get_regex_time_units())

  # Simulation specifications
  ans = paste0(ans, paste0("\nSimulation time: ", object$sim_specs$start, " to ", object$sim_specs$stop, " ", matched_time_unit, " (dt = ", object$sim_specs$dt, ifelse(object$sim_specs$saveat == object$sim_specs$dt, "", paste0(", saveat = ", object$sim_specs$saveat)), ")\nSimulation settings: solver ", object$sim_specs$method, ifelse(is_defined(object$sim_specs$seed), paste0(" and seed ", object$sim_specs$seed), ""), " in ", object$sim_specs$language))

  class(ans) <- "summary.sdbuildR_xmile"
  cat(ans)
  invisible(ans)

}



#' Find longest regex match
#'
#' @param x Value
#' @param regex_units Regex units dictionary
#'
#' @returns Longest cleaned regex match
#' @noRd
find_matching_regex = function(x, regex_units){

  matches = names(regex_units[regex_units == x])

  # Clean regex and select longest match
  matches = sub("\\$$", "", sub("^\\^", "", matches))
  matches = sub("\\[s\\]\\?", "s", matches)

  matches = unique(tolower(stringr::str_replace_all(matches, "\\[([a-zA-Z])\\|([a-zA-Z])\\]", "\\1")))
  matches[which.max(nchar(matches))] # Return longest match

}


#' Get delayN and smoothN from stock-and-flow model
#'
#' @inheritParams build
#'
#' @returns List with delayN and smoothN functions
#' @noRd
get_delayN_smoothN = function(sfm){

  z = unlist(unname(sfm$model$variables), recursive = FALSE, use.names = TRUE)
  z = lapply(z, function(x){
      c(x[["func"]][["delayN"]], x[["func"]][["smoothN"]])
    })
  z = z[lengths(z) > 0]
  return(z)
}


#' Get delay and past from stock-and-flow model
#'
#' @inheritParams build
#'
#' @returns List with delay and past functions
#' @noRd
get_delay_past = function(sfm){

  z = unlist(unname(sfm$model$variables), recursive = FALSE, use.names = TRUE)
  z = lapply(z, function(x){
    c(x[["func"]][["delay"]], x[["func"]][["past"]])
  })
  z = z[lengths(z) > 0]
  return(z)
}



#' Extract entries from a nested list
#'
#' @param nested_list List to extract from
#' @param entry Name of entry to extract
#' @param keep_entry_name If TRUE, keep upper level name.
#'
#' @returns List with extracted entries
#' @noRd
list_extract <- function(nested_list, entry, keep_entry_name = FALSE) {
  result <- list()

  # Helper function to traverse the list
  traverse <- function(x) {
    if (is.list(x)) {
      for (name in names(x)) {
        if (name == entry) {
          if (keep_entry_name){
            result <<- c(result, stats::setNames(list(x[[name]]), name))
          } else {
            result <<- c(result, x[[name]])
          }
        } else {
          traverse(x[[name]])
        }
      }
    }
  }

  traverse(nested_list)
  return(result)
}



#' Ensure length of arg is same as target
#'
#' @param arg Vector
#' @param target Target object to match length of
#'
#' @return arg with same length as target
#' @noRd
#'
ensure_length <- function(arg, target) {
  if (length(arg) != 1 && length(arg) != length(target)) {
    stop(sprintf("The length of %s = %s must be either 1 or equal to the length of %s = %s.",
                 deparse(substitute(arg)), paste0(arg, collapse = ", "),
                 deparse(substitute(target)), paste0(target, collapse = ", ")))
  } else if (length(arg) < length(target)) {
    arg <- rep(arg, length.out = length(target))  # Repeat to match the target length
  }
  return(arg)
}



#' Check whether object is of class sdbuildR_xmile
#'
#' @inheritParams build
#'
#' @returns NULL
#' @noRd
check_xmile = function(sfm){
  # Check whether it is an xmile object
  if (!inherits(sfm, "sdbuildR_xmile")){
    stop("This is not an object of class sdbuildR_xmile! Create a stock-and-flow model with xmile() or insightmaker_to_sfm().")
  }
}


#' Validate sdbuildR_xmile class
#'
#' Internally used function to ensure that the stock-and-flow model is valid and contains all necessary properties.
#'
#' @inheritParams build
#'
#' @return Validated stock-and-flow model of class sdbuildR_xmile
#' @noRd
#'
validate_xmile = function(sfm){

  check_xmile(sfm)

  # No need to validate model variables if there are no variables
  nr_var = sum(lengths(sfm$model$variables))
  if (nr_var > 0){

    # Make sure name property matches with the name of the list entry
    type_names = names(sfm$model$variables)
    sfm$model$variables = lapply(seq_along(sfm$model$variables),
                                 function(i){
        x = sfm$model$variables[[i]]

        if (length(x) == 0){
          x = list()
        } else {

          var_names = unname(unlist(lapply(x, `[[`, "name")))
          x = stats::setNames(x, var_names)

          # Make sure the type matches
          type = type_names[i]
          x = lapply(x, function(y){
            y$type = type
            return(y)
          })

        }
        return(x)
      })
    names(sfm$model$variables) = type_names

    # Ensure each variable has the necessary properties for its building block; otherwise, add defaults.
    keep_prop = get_building_block_prop()
    defaults <- as.list(formals(build))
    defaults <- defaults[!names(defaults) %in% c("sfm", "name", "type", "label", "erase",
                                                 "change_name", "change_type", "...")]

    # Process variables
    type_names = names(sfm$model$variables)
    sfm$model$variables <- lapply(names(sfm$model$variables), function(type) {
      vars <- sfm$model$variables[[type]]
      # Pre-compute type-specific defaults
      type_defaults <- defaults[names(defaults) %in% keep_prop[[type]]]

      lapply(vars, function(y) {
        # Add label, eqn, eqn_julia if missing
        if (is.null(y$label)) y$label <- y$name
        if (is.null(y$eqn)) y$eqn <- "0.0"
        if (is.null(y$eqn_julia)) y$eqn_julia <- "0.0"

        # Merge with type-specific defaults
        utils::modifyList(type_defaults, y)
      })
    })
    names(sfm$model$variables) <- type_names  # Preserve names

    # Ensure to and from in flows are only referring to stocks
    names_df = get_names(sfm)
    stock_names = names_df[names_df$type == "stock", "name"]
    nonstock_names = names_df[names_df$type != "stock", "name"]

    sfm$model$variables$flow = lapply(sfm$model$variables$flow, function(x){

        if (is_defined(x$from)){
          # If from is not in stocks but is another variable, remove
          non_stocks = x$from[!x$from %in% stock_names & x$from %in% nonstock_names]
          if (length(non_stocks) > 0){
            message(paste0(x$name, " is flowing from variables which are not stocks (", paste0(non_stocks, collapse = ", "), ")! Removing non-stocks from `from`..." ))
            x$from = intersect(x$from, stock_names)
            if (length(x$from) == 0){
              x$from = ""
            }
          }
        }

        if (is_defined(x$to)){
          # If to is not in stocks but is another variable, remove
          non_stocks = x$to[!x$to %in% stock_names & x$to %in% nonstock_names]
          if (length(non_stocks) > 0){
            message(paste0(x$name, " is flowing to a variable which is not a stock (", paste0(non_stocks, collapse = ", "), ")! Removing ", paste0(non_stocks, collapse = ", "), " from `to`..." ))
            x$to = intersect(x$to, stock_names)
            if (length(x$to) == 0){
              x$to = ""
            }
          }
        }

        return(x)
      })

    # Ensure sfm$behavior matches non-negativity properties of variables: Get non-negative Stocks and Flows
    nonneg_stock = names(sfm$model$variables$stock)[sapply(sfm$model$variables$stock, function(x) x[["non_negative"]] == TRUE)]
    nonneg_flow = names(sfm$model$variables$flow)[sapply(sfm$model$variables$flow, function(x) x[["non_negative"]] == TRUE)]
    sfm$behavior$stock$non_negative = nonneg_stock
    sfm$behavior$flow$non_negative = nonneg_flow


  }

# **ensure from and to are length 1

  # Ensure model units have default properties
  defaults <- as.list(formals(model_units))
  defaults <- defaults[!names(defaults) %in% c("sfm", "name", "erase", "change_name")]
  sfm$model_units = lapply(sfm$model_units, function(x){

    x$prefix = FALSE

    # Merge with defaults
    utils::modifyList(defaults, x)
  })

  # Ensure names are the same as names properties
  names(sfm$model_units) = unname(unlist(lapply(sfm$model_units, `[[`, "name")))

  # Ensure macros have default properties
  defaults <- as.list(formals(macro))
  defaults <- defaults[!names(defaults) %in% c("sfm", "name", "erase", "change_name")]
  sfm$macro = lapply(sfm$macro, function(x){

    if (is.null(x$eqn)) x$eqn <- "0.0"
    if (is.null(x$eqn_julia)) x$eqn_julia <- "0.0"

    # Merge with defaults
    utils::modifyList(defaults, x)
  })

  # Ensure names are the same as names properties
  names(sfm$macro) = unname(unlist(lapply(sfm$macro, `[[`, "name")))



  #** don't do this, gives priority to one entry ('from' from flow or 'inflow' from stock)
  # # Ensure stock "inflow" and "outflow" entries match flow "to" and "from" entries
  # stock_inflow = sfm$model$variables$stock %>% purrr::map("inflow")
  # stock_outflow = sfm$model$variables$stock %>% purrr::map("outflow")
  # flow_to = sfm$model$variables$flow %>% purrr::map("to")
  # flow_from = sfm$model$variables$flow %>% purrr::map("from")
  #
  # # Switch names and entries
  # stock_inflow_switch = switch_list(stock_inflow)
  # stock_outflow_switch = switch_list(stock_outflow)
  # flow_to_switch = switch_list(flow_to)
  # flow_from_switch = switch_list(flow_from)
  #
  # sfm$model$variables = sfm$model$variables %>%
  #   purrr::imap(function(x, type){
  #     purrr::imap(x, function(y, name){
  #       if (type == "stock"){
  #         y$inflow = c(stock_inflow[[name]], flow_to_switch[[name]]) %>% unique() %>% Filter(nzchar, .)
  #         y$outflow = c(stock_outflow[[name]], flow_from_switch[[name]]) %>% unique() %>% Filter(nzchar, .)
  #       } else if (type == "flow"){
  #         y$to = c(flow_to[[name]], stock_inflow_switch[[name]]) %>% unique() %>% Filter(nzchar, .)
  #         y$from = c(flow_from[[name]], stock_outflow_switch[[name]]) %>% unique() %>% Filter(nzchar, .)
  #       }
  #       return(y)
  #     })
  #
  #   })


  # ** don't do this: the simulation time unit may change, leading to unexpected results. you can do this in compile()
  # # Dimensional consistency: Units of Flows need to be units of corresponding Stocks divided by sfm$sim_specs$time_units or some time unit
  # names_df = get_names(sfm)
  # sfm$model$variables$flow = sfm$model$variables$flow %>%
  #   purrr::map(function(x, name){
  #
  #   if (is_defined(x$to)){
  #     stock_name = x$to
  #
  #   } else if (is_defined(x$from)){
  #     stock_name = x$from
  #
  #   } else {
  #     return(x)
  #   }
  #
  #   # Get unit of Stock and define unit for Flow
  #   stock_unit = names_df[names_df$name == stock_name, "units"]
  #   target_unit = paste0(stock_unit, "/", sfm$sim_specs$time_units)
  #
  #   if (!is_defined(x$units) | x$units == "1"){
  #     x$units = target_unit
  #   }
  #   # # Is x$units the units of the Stock divided by some time unit? If not, add units of the Stock divided by simulation time unit
  #   #
  #   # #** add custom units to julia for valid comparison
  #   # same_unit = juliaCall::julia_eval(paste0("using Unitful; ", x$units, "==",target_unit))
  #   #
  #   # if (!same_unit){
  #   #   #** compare to all other time units
  #   #   print(paste0("Overwriting unit of ", name, " to match corresponding Stock ", stock_name))
  #   #   x$units = target_unit
  #   # }
  #   # # if (Ryacas::yac_str(paste0("Simplify(", x$units, " == ", eq, ")")))
  #
  #   return(x)
  #
  # })

  # column_names = c("name", "inflow", "outflow", "to", "from")
  # suppressWarnings(
  #   edge_df <- sfm$model$variables[c("stock", "flow")] %>% purrr::flatten() %>%
  #   purrr::map(\(x){ x[column_names]}) %>%
  #   data.table::rbindlist(fill = F, use.names=FALSE) %>%
  #   magrittr::set_colnames(column_names) %>%
  #   as.data.frame()
  # )
  # edge_df
  #
  # sfm$model$variables = sfm$model$variables %>%
  #   purrr::imap(\(x, y){
  #                     purrr::map(x, function(z){
  #                       if (y == "stock"){
  #
  #                         z$inflow = Filter(nzchar, c(z$inflow, edge_df %>% dplyr::filter(.data$to == z$name) %>% dplyr::pull(.data$name)) %>% unique())
  #                         z$outflow = Filter(nzchar, c(z$outflow, edge_df %>% dplyr::filter(.data$from == z$name) %>% dplyr::pull(.data$name)) %>% unique())
  #
  #                       } else if (y == "flow"){
  #                         z$to = Filter(nzchar, c(z$to, edge_df %>% dplyr::filter(.data$inflow == z$name) %>% dplyr::pull(.data$name)) %>% unique())
  #                         z$from = Filter(nzchar, c(z$from, edge_df %>% dplyr::filter(.data$outflow == z$name) %>% dplyr::pull(.data$name)) %>% unique())
  #                       }
  #                       return(z)
  #                     })
  #                   })



  # **massively slows down code, make sure eqn_julia exists in build()
  # # Check julia translation
  # sfm$model$variables = sfm$model$variables %>%
  #   purrr::map_depth(2, \(x){
  #
  #     # Ensure every equation is a character string
  #     if (is_defined(x$eqn)){
  #       x$eqn = as.character(x$eqn)
  #     }
  #
  #     # Ensure every equation has a julia translation
  #     if (is_defined(x$eqn) & !is_defined(x$eqn_julia)){
  #       x$eqn_julia = convert_equations_julia(sfm, x$eqn, names_df,
  #                                             regex_units = regex_units, debug = F)
  #     }
  #
  #     return(x)
  #   })
#
#   sfm$macro = sfm$macro %>% purrr::map(function(x){
#     # Ensure every equation has a julia translation
#     if (is_defined(x$eqn) & !is_defined(x$eqn_julia)){
#       x$eqn_julia = convert_equations_julia(sfm, x$eqn, names_df,
#                                             regex_units = regex_units,
#                                             debug = F)
#     } else if (x$eqn == "" & !is_defined(x$eqn_julia)){
#       x$eqn_julia = ""
#     }
#     return(x)
#   })

  # **move to compile(), massively slows down build()
  # # Ensure all units are defined
  # add_model_units = detect_undefined_units(sfm,
  #                                    new_eqns = c(sfm$model$variables %>% purrr::map_depth(2, "eqn_julia") %>% unlist(),
  #                                                 sfm$macro %>% purrr::map_vec("eqn_julia")))
  # sfm$model_units = sfm$model_units %>% utils::modifyList(add_model_units)


  return(sfm)
}



#' Switch names and values of list, handling different lengths in entries
#'
#' @param x List
#' @return List
#' @noRd
switch_list = function(x){

  # Switch names and values
  new_list <- unlist(lapply(names(x), function(name) {
    stats::setNames(rep(name, length(x[[name]])), x[[name]])
  }), recursive = FALSE)

  return(as.list(new_list))
}



#' Create, modify or remove custom units
#'
#' A large library of units already exists, but you may want to define your own custom units. Use `model_units()` to add, change, or erase custom units from a stock-and-flow model. Custom units may be new base units, or may be defined in terms of other (custom) units. See `?u()` for more information on the rules of specifying units. Note that units are only supported in Julia, not in R.
#'
#' @inheritParams build
#' @param name Name of unit. A character vector.
#' @param eqn Definition of unit. String or vector of unit definitions. Defaults to "1" to indicate a base unit not defined in terms of other units.
#' @param doc Documentation of unit.
#' @param erase If TRUE, remove model unit from the model. Defaults to FALSE.
#' @param change_name New name for model unit. Defaults to NULL to indicate no change.
#'
#' @return The modified stock-and-flow object with the specified unit(s) added or removed.
#'
#' @export
#' @seealso [unit_prefixes()]
#'
#' @examples
#' sfm = xmile("Crielaard2022")
#' sfm = sfm %>% model_units("BMI", eqn = "kg/m^2", doc = "Body Mass Index")
#'
#' # You may also use words rather than symbols for the unit definition.
#' # The following modifies the unit BMI:
#' sfm = sfm %>% model_units("BMI", eqn = "kilogram/meters^2")
#'
#' # Remove unit:
#' sfm = sfm %>% model_units("BMI", erase = TRUE)
#'
#' # Unit names may be changed to be syntactically valid and avoid overlap:
#' sfm = xmile() %>% model_units("C0^2")
#'
model_units = function(sfm, name, eqn = "1", doc = "", erase = FALSE, change_name = NULL){

  # Basic check
  if (missing(sfm)){
    stop("No model specified!")
  }

  if (missing(name)){
    stop("name must be specified!")
  }

  check_xmile(sfm)

  idx_nonexist = which(!name %in% names(sfm$model_units))

  # Remove unit from model
  if (erase){

    if (length(idx_nonexist) == 0){
      sfm$model_units[name] = NULL
    } else {
      stop(paste0(paste0(name[idx_nonexist], collapse = ", "),
                  ifelse(length(idx_nonexist) == 1,
                         " does not exist as a custom unit!",
                         " do not exist as custom units!"),
                  ifelse(length(sfm$model_units) > 0,
                         paste0("\nExisting model units: ",
                                paste0(names(sfm$model_units), collapse = ", ")),
                         "Your model has no custom units."))
                  )
    }
  } else {

    # Change units to units valid for Julia's Unitful package
    regex_units = get_regex_units()

    if (!is.null(change_name)){

      if (length(name) > 1 | length(change_name) > 1){
        stop("You can only change the name of one custom unit at a time.")
      }

      old_name = name
      chosen_name = change_name

    } else {
      chosen_name = name
    }

    name = sapply(chosen_name, function(x){clean_unit(x, regex_units, unit_name = TRUE)}) %>% unname()

    # Keep existing names the same
    name[!idx_nonexist] = chosen_name[!idx_nonexist]

    idx_changed = name != chosen_name

    # Check if unit already exists in unit package. Default units cannot be overwritten
    name_in_units = name %in% unname(regex_units)

    if (any(name_in_units)){

      stop(sprintf("The custom unit name%s %s match%s the standard unit%s %s, which cannot be overwritten.\nPlease choose %sunique name%s for: %s ",
                   ifelse(sum(name_in_units) > 1, "s", ""),
                   paste0(chosen_name[name_in_units], collapse = ", "),
                   ifelse(sum(name_in_units) > 1, "", "es"),
                   ifelse(sum(name_in_units) > 1, "s", ""),
                   paste0(name[name_in_units], collapse = ", "),
                   ifelse(sum(name_in_units) > 1, "", "a "),
                   ifelse(sum(name_in_units) > 1, "s", ""),
                   paste0(chosen_name[name_in_units], collapse = ", ")
      ))
    }

    # Check if all unit names contain at least one letter or digit
    idx_invalid = !grepl("[a-zA-Z0-9]", name)

    if (any(idx_invalid)){
      stop(sprintf("Each custom unit name needs at least one letter or number.\nPlease choose %sdifferent name%s for: %s ",
                   ifelse(sum(name_in_units) > 1, "", "a "),
                   ifelse(sum(name_in_units) > 1, "s", ""),
                   paste0(chosen_name[idx_invalid], collapse = ", ")
      ))
    }

    if (any(idx_changed)){

      warning(sprintf("The custom unit name%s %s %s modified to %s to comply with Julia's syntactic rules.\nUse sfm %%>%% model_units('old_name', change_name = 'new_name') to update the name%s in your model.",
                      ifelse(sum(idx_changed) > 1, "s", ""),
                      paste0(chosen_name[idx_changed], collapse = ", "),
                      ifelse(sum(idx_changed) > 1, "were", "was"),
                      paste0(name[idx_changed], collapse = ", "),
                      ifelse(sum(idx_changed) > 1, "s", "")
      ))


    }


    if (!is.null(change_name)){
      # Check if name is already in use
      unit_exists = name %in% setdiff(names(sfm$model_units), old_name)

      if (!unit_exists){
        sfm$model_units[name] = sfm$model_units[old_name]
        sfm$model_units[old_name] = NULL

        # Ensure the unit is translated in the entire model
        dict = stats::setNames(name, paste0("^", old_name, "$"))

        sfm$model_units = lapply(sfm$model_units,
                                 function(x){
            if (is_defined(x$eqn)){
              x$eqn = clean_unit(x$eqn, dict)
            }
            return(x)
          })

        var_names = get_model_var(sfm)
        sfm$model$variables = lapply(sfm$model$variables,
                                     function(y){
                                       lapply(y, function(x){
                                         if (is_defined(x$units)){
                                           x$units = clean_unit(x$units, dict)
                                         }

                                         if (is_defined(x$eqn)){
                                           old_eqn = x$eqn
                                           x$eqn = clean_unit_in_u(x$eqn, dict)

                                           # If equation changed, redo Julia translation
                                           if (old_eqn != x$eqn){
                                              x$eqn_julia = convert_equations_julia(sfm, x$type,
                                                                               x$name, x$eqn,
                                                                               var_names,
                                                                               regex_units = dict,
                                                                               debug = FALSE)
                                           }
                                         }
                                         return(x)
                                       })
          })

      } else {
        stop(sprintf("%s already exists as a custom unit! Choose a different new name for %s",
                     name, old_name))
      }
    }

    # Get names of passed arguments
    passed_arg = names(as.list(match.call())[-1]) %>%
      # Remove some arguments
      setdiff(c("sfm", "erase", "change_name"))
    argg <- list()
    argg$name = name

    if ("eqn" %in% passed_arg){
      eqn = sapply(eqn, clean_unit, regex_units) %>% unname()
      eqn = ensure_length(eqn, name)
      argg$eqn = eqn
    }

    if ("doc" %in% passed_arg){
      doc = ensure_length(doc, name)
      argg$doc = doc
    }

    new_units = stats::setNames(purrr::transpose(argg), name)

    # Add units to model (in for-loop, as otherwise not all elements are added or overwritten)
    for (i in seq_along(name)){
      sfm$model_units = sfm$model_units %>%
        utils::modifyList(new_units[i])
    }

  }

  sfm = validate_xmile(sfm)

  return(sfm)

}




#' Create, modify or remove a global variable or function
#'
#' Macros are global variables or functions that can be used throughout your stock-and-flow model. `macro()` adds, changes, or erases a macro.
#'
#' @inheritParams build
#' @param name Name of the macro. The equation will be assigned to this name.
#' @param eqn Equation of the macro. A character vector. Defaults to "0.0".
#' @param doc Documentation of the macro. Defaults to "".
#' @param change_name New name for macro (optional). Defaults to NULL to indicate no change.
#' @param erase If TRUE, remove macro from the model. Defaults to FALSE.
#'
#' @return Updated stock-and-flow model
#' @export
#'
#' @examples
#' # If the sigmoid() function did not exist, you could create it yourself:
#' sfm = xmile() %>%
#' macro("sig", eqn = "function(x, slope = 1, midpoint = .5) 1 / (1 + exp(-slope*(x-midpoint)))")
#'
macro = function(sfm, name, eqn = "0.0", doc = "", change_name = NULL, erase = FALSE){

  # Basic check
  if (missing(sfm)){
    stop("No model specified!")
  }

  check_xmile(sfm)

  if (missing(name)){
    stop("name must be specified!")
  }

  # Check change name of variable
  if (!is.null(change_name)){
    if (length(change_name) > 1 | length(name) > 1){
      stop("You can only change the name of one variable at a time!")
    }
  }

  passed_arg = names(as.list(match.call())[-1]) %>%
    # Remove some arguments
    setdiff(c("sfm", "erase", "change_name"))
  argg = list()

  names_df = get_names(sfm)
  var_names = get_model_var(sfm)

  idx_exist = name %in% names(sfm$macro)

  if (erase){

    if (any(!idx_exist) == 0){
      sfm$macro[name] = NULL
    } else {
      stop(sprintf("%s do%s not exist as %scustom macro%s! %s",
                   paste0(name[!idx_exist], collapse = ", "),
                   ifelse(length(name[!idx_exist]) > 1, "", "es"),
                   ifelse(length(name[!idx_exist]) > 1, "", "a "),
                   ifelse(length(name[!idx_exist]) > 1, "s", ""),
                   ifelse(length(sfm$macro) > 0, paste0("Existing macros: ",
                                                        paste0(names(sfm$macro), collapse = ", ")),
                          "Your model has no custom macros."))
      )
    }
  } else {


    # If overwriting name with change_name
    if (!is.null(change_name)){

      # Ensure new name is syntactically valid
      chosen_new_name = change_name
      change_name = create_R_names(change_name, names_df)
      report_name_change(chosen_new_name, change_name)

      # Overwrite name
      macro_names = names(sfm$macro)
      macro_names[macro_names == name] = change_name
      names(sfm$macro) = macro_names
      sfm$macro[[change_name]][["name"]] = change_name

      # Replace references to name with change_name everywhere
      sfm$model$variables = lapply(sfm$model$variables, function(y){
        lapply(y, function(x){

          if (is_defined(x$eqn)){
            idx_df = get_range_names(x$eqn, name, names_with_brackets = FALSE)
            if (nrow(idx_df) > 0){
              # Reverse indices to replace correctly
              for (i in rev(1:nrow(idx_df))){
                stringr::str_sub(x$eqn, idx_df[i, "start"], idx_df[i, "end"]) = change_name
              }

              # Update Julia translation
              idx_df = get_range_names(x$eqn_julia, name, names_with_brackets = FALSE)
              if (nrow(idx_df) > 0){
                # Reverse indices to replace correctly
                for (i in rev(1:nrow(idx_df))){
                  stringr::str_sub(x$eqn_julia, idx_df[i, "start"], idx_df[i, "end"]) = change_name
                }
              }
            }
          }
          return(x)
        })
      })

      name = change_name

      # Redo equation (below)
      if (!"eqn" %in% passed_arg){
        eqn = sfm$macro[[name]][["eqn"]]
        passed_arg = c(passed_arg, "eqn")
      }

      # Update
      var_names = get_model_var(sfm)
      idx_exist = name %in% names(sfm$macro)

    }

  # Ensure names are valid of new variables
  if (any(!idx_exist)){

    # Create syntactically valid, unique names (this also avoids overlap with previous names, but we stopped the function already if this is the case)
    new_names = create_R_names(name[!idx_exist], names_df)

    # Warning if specified name changed
    report_name_change(name[!idx_exist], new_names)

    # Change name
    name[!idx_exist] = new_names

  }


  if ("eqn" %in% passed_arg){

    regex_units = get_regex_units()

    if (any(is.null(eqn))){
      warning("Equation cannot be NULL! Setting empty equations to 0...")
      eqn[is.null(eqn)] = "0.0"
    }

    if (any(!nzchar(eqn))){
      warning("Equation cannot be empty! Setting empty equations to 0...")
      eqn[!nzchar(eqn)] = "0.0"
    }

    # Change all equations to characters
    if (!is.null(eqn)){
      eqn = as.character(eqn)
    }

    # Ensure units are cleaned in u() in eqn
    # eqn = sapply(eqn, function(x){clean_unit_in_u(x, regex_units)}) %>% unname()
    eqn = clean_unit_in_u(eqn, regex_units)
    eqn = ensure_length(eqn, name)

    # Convert equation to Julia
    eqn_julia = sapply(1:length(name), function(i){

      # Assign name already to convert functions correctly
      x = paste0(name[i], " = ", eqn[i])

      convert_equations_julia(sfm, type = "macro", name = name[i], eqn = x, var_names = var_names,
                                                                regex_units = regex_units,
                                                                debug = FALSE)$eqn_julia
      # No need to save $func because delay family cannot be used for macros
      }) %>% unname()

    argg$eqn = eqn
    argg$eqn_julia = eqn_julia

  }

    argg$name = name

    if ("doc" %in% passed_arg){
      doc = ensure_length(doc, name)
      argg$doc = doc
    }

    new_macros = argg %>% purrr::transpose() %>% stats::setNames(name)

    # Add elements to model (in for-loop, as otherwise not all elements are added or overwritten)
    for (i in seq_along(name)){
      sfm$macro = sfm$macro %>%
        utils::modifyList(new_macros[i])
    }


  }

  sfm = validate_xmile(sfm)

  return(sfm)
}


#' Modify header of stock-and-flow model
#'
#' The header of a stock-and-flow model contains metadata about the model, such as the name, author, and version. Modify the header of an existing model with standard or custom properties.
#'
#' @inheritParams build
#' @param name Model name. Defaults to "My Model".
#' @param caption Model description. Defaults to "My Model Description".
#' @param created Date the model was created. Defaults to Sys.time().
#' @param author Creator of the model. Defaults to "Me".
#' @param version Model version. Defaults to "1.0".
#' @param URL URL associated with model. Defaults to "".
#' @param doi DOI associated with the model. Defaults to "".
#' @param ... Optional other entries to add to the header.
#'
#' @return Updated stock-and-flow model.
#' @export
#'
#' @examples
#' sfm = xmile() %>%
#'   header(name = "My first model",
#'   caption = "This is my first model",
#'   author = "Kyra Evers",
#'   version = "1.1")
header = function(sfm, name = "My Model", caption = "My Model Description",
                  created = Sys.time(), author = "Me", version = "1.0", URL = "", doi = "", ...){

  # Basic check
  if (missing(sfm)){
    stop("No model specified!")
  }

  check_xmile(sfm)

  # Get names of passed arguments
  passed_arg = names(as.list(match.call())[-1]) %>%
    # Remove some arguments
    setdiff(c("sfm", "..."))
  # Collect all arguments
  argg <- c(
    as.list(environment()),
    list(...))[unique(passed_arg)]

  sfm$header = sfm$header %>% utils::modifyList(argg)

  sfm = validate_xmile(sfm)

  return(sfm)
}





#' Modify simulation specifications
#'
#' Simulation specifications are the settings that determine how the model is simulated, such as the integration method, start and stop time, and timestep. Modify these specifications for an existing stock-and-flow model.
#'
#' @inheritParams build
#' @param method Integration method. Defaults to "euler".
#' @param start Start time of simulation. Defaults to 0.
#' @param stop End time of simulation. Defaults to 100.
#' @param dt Timestep of solver. Defaults to 0.01.
#' @param saveat Timestep at which to save computed values. Defaults to dt.
#' @param adaptive If TRUE, an adaptive (instead of a fixed) time step is used. Defaults to FALSE.
#' @param seed Seed number to ensure reproducibility across runs in case of random elements. Must be an integer. Defaults to NULL (no seed).
#' @param time_units Simulation time unit, e.g. 's' (second). Defaults to "s".
#' @param language Coding language in which to simulate model. Either "R" or "Julia". Julia is necessary for using units or delay functions. Defaults to "R".
#' @param ... Optional additional parameters
#'
#' @return Updated stock-and-flow model with new simulation specifications
#' @export
#'
#' @examples sfm = xmile("predator-prey") %>%
#' sim_specs(start = 1960, stop = 2010, dt = 0.1)
#'sim = simulate(sfm)
#'plot(sim)
#'
#'# Change the simulation method to "rk4"
#'sfm = sfm %>% sim_specs(method = "rk4")
#'
#'# Change the time units to "years", such that one time unit is one year
#'sfm = sfm %>% sim_specs(time_units = "years")
#'
#'# To save storage but not affect accuracy, use saveat
#'sfm = sfm %>% sim_specs(saveat = 0.1, dt = 0.001)
#'sim = simulate(sfm)
#'head(as.data.frame(sim))
#'
#'# Specify seed for reproducibility
#'sfm = sfm %>% sim_specs(seed = 1) %>%
#'  # Add stochastic initial condition
#'  build(c("predator", "prey"), eqn = "runif(1, 20, 50)")
#'sim1 = simulate(sfm)
#'sim2 = simulate(sfm)
#'plot(sim1)
#'plot(sim2)
#'
#'# Removing the seed yields variation
#'sfm = sfm %>% sim_specs(seed = NULL)
#'sim1 = simulate(sfm)
#'sim2 = simulate(sfm)
#'plot(sim1)
#'plot(sim2)
#'
#'# Change the simulation language to Julia to use units and delay functions
#'sfm = sfm %>% sim_specs(language = "Julia")
#'
sim_specs = function(sfm,
                     method = "euler",
                     start = "0.0",
                     stop = "100.0",
                     dt = "0.01",
                     saveat = dt,
                     adaptive = FALSE,
                     seed = NULL,
                     time_units = "s",
                     language = "R", ...){

  # Basic check
  if (missing(sfm)){
    stop("No model specified!")
  }

  check_xmile(sfm)
  var_names = get_model_var(sfm)

  # Get names of passed arguments
  passed_arg = names(as.list(match.call())[-1]) %>%
    # Remove some arguments
    setdiff(c("sfm", "..."))

  if (!missing(start)){
    start = suppressWarnings(as.numeric(start))
    if (is.na(start)){
      stop("Start time must be a number!")
    }
  }

  if (!missing(stop)){
    stop = suppressWarnings(as.numeric(stop))
    if (is.na(stop)){
      stop("Stop time must be a number!")
    }
  }

  if (!missing(dt)){
    dt = suppressWarnings(as.numeric(dt))
    if (is.na(dt)){
      stop("dt must be a number!")
    }

    if (dt != 1){
      if (dt > .1){
        warning(paste0("dt is larger than 0.1! This will likely lead to inaccuracies in the simulation. To reduce the size of the simulaton dataframe, use saveat = ", dt, ", and keep dt to a smaller value. To simulate in discrete time, set dt = 1."))
      }
    }

  }

  if (!missing(saveat)){
    saveat = suppressWarnings(as.numeric(saveat))
    if (is.na(saveat)){
      stop("saveat must be a number!")
    }
  }


  # Ensure time_units are formatted correctly
  if (!missing(time_units)){

    if (length(time_units) != 1){
      stop("time_units must be a single string!")
    }

    # Time units can only contain letters or spaces
    if (any(grepl("[^a-zA-Z _]", time_units))){
      stop("time_units can only contain letters, spaces, or underscores!")
    }
    time_units = clean_unit(time_units, get_regex_time_units()) # Units are not used in R, so translate to julia directly

  }

  # Check whether method is a valid deSolve method
  # ** https://docs.sciml.ai/DiffEqDocs/stable/solvers/ode_solve/
  if (!missing(method)){
    method = tolower(method)
    if (!method %in% c("euler", "rk4")){
      stop(sprintf("The method %s is not one of the methods available. Choose either 'euler' or 'rk4' (soon to be expanded)."))
    }
  }




  # # Check simulation method
  # if (sfm$sim_specs$language == "R"){
  #   if (!requireNamespace("deSolve", quietly = TRUE)){
  #     warning("deSolve is not installed! Please install deSolve to simulate in R, or simulate in julia by setting\nsdm %>% sim_specs(language = 'julia')")
  #   } else {
  #     if (!sfm$sim_specs$method %in% deSolve::rkMethod()){
  #       warning("Invalid simulation method for deSolve! Use one of ", paste0(paste0("'", deSolve::rkMethod(), "'"), collapse = ", "), ".")
  #       sfm$sim_specs$method = formals(sim_specs)$method
  #     }
  #   }
  # } else if (sfm$sim_specs$language == "julia"){
  #
  #   # Try
  #
  #   if (!sfm$sim_specs$method %in% c("euler")){
  #     warning("Invalid simulation method for julia! Use one of 'VODE', 'CVODE', 'Radau', 'LSODA', or 'LSODAR'.")
  #   }
  # }


  # Check whether start is smaller than stop
  if (!missing(start)){
    if (missing(stop)){
      stop = as.numeric(sfm$sim_specs$stop)
    }
    if (start >= stop){
      stop("Start time must be smaller than stop time!")
    }
  }

  if (!missing(stop)){
    if (missing(start)){
      start = as.numeric(sfm$sim_specs$start)
    }
    if (start >= stop){
      stop("Start time must be smaller than stop time!")
    }
  }

  # Check whether dt is smaller than stop; if not, stop
  if (!missing(dt)){
    if (missing(stop)){
      stop = as.numeric(sfm$sim_specs$stop)
    }
    if (missing(start)){
      start = as.numeric(sfm$sim_specs$start)
    }
    if (dt > (stop - start)){
      stop("dt must be smaller than the difference between start and stop!")
    }
  }

  # Check whether saveat is smaller than stop; if not, stop
  if (!missing(saveat)){
    if (missing(stop)){
      stop = as.numeric(sfm$sim_specs$stop)
    }
    if (missing(start)){
      start = as.numeric(sfm$sim_specs$start)
    }
    if (saveat > (stop - start)){
      stop("saveat must be smaller than the difference between start and stop!")
    }
  }

  # Check whether dt is smaller than saveat; if not, set saveat to dt
  if (!missing(dt)){
    if (!missing(saveat)){
      if (dt > saveat){
        warning("dt must be smaller or equal to saveat! Setting saveat equal to dt...")
        saveat = dt
        passed_arg = c(passed_arg, "saveat")
      }
    } else if (missing(saveat)){
      if (is_defined(sfm$sim_specs$saveat)){
        if (dt > as.numeric(sfm$sim_specs$saveat)){
          # warning("dt must be smaller or equal to saveat! Setting saveat equal to dt...")
          saveat = dt
          passed_arg = c(passed_arg, "saveat")
        }
      } else {
        saveat = dt
        passed_arg = c(passed_arg, "saveat")
      }
    }
  } else if (!missing(saveat)){
    # The above ifelse takes care of when saveat and dt are both not NULL; now only saveat can be not NULL
    if (is_defined(sfm$sim_specs$dt)){
      if (saveat < as.numeric(sfm$sim_specs$dt)){
        warning("dt must be smaller or equal to saveat! Setting saveat equal to dt...")
        saveat = dt
        passed_arg = c(passed_arg, "saveat")
      }
    }
  }

  # Seed must be NULL or an integer
  if (!missing(seed)){

    if (!is.null(seed)){
      if (nzchar(seed)){

        seed = strtoi(seed)

        if (is.na(seed)){
          stop("seed must be an integer!")
        }
        seed = as.character(seed)
      } else {
        seed = NULL
      }
    }
  }

  # Check coding language
  if (!missing(language)){
    if (!tolower(language) %in% c("r", "julia")){
      stop(sprintf("The language %s is not one of the languages available in sdbuildR. The available languages are 'Julia' (recommended) or 'R'.", language))
    } else {
      language = stringr::str_to_title(language)
    }
  }

  # Ensure no scientific notation is present
  if ("start" %in% passed_arg){
    start = replace_digits_with_floats(scientific_notation(start), var_names)
  }
  if ("stop" %in% passed_arg){
    stop = replace_digits_with_floats(scientific_notation(stop), var_names)
  }
  if ("dt" %in% passed_arg){
    dt = replace_digits_with_floats(scientific_notation(dt), var_names)
  }
  if ("saveat" %in% passed_arg){
    saveat = replace_digits_with_floats(scientific_notation(saveat), var_names)
  }

  # Collect all arguments
  argg <- c(
    as.list(environment()),
    list(...))[unique(passed_arg)]

  # Overwrite simulation specifications
  sfm$sim_specs = sfm$sim_specs %>% utils::modifyList(argg)

  sfm = validate_xmile(sfm)

  return(sfm)
}





#' Remove variable from stock-and-flow model
#'
#' @inheritParams build
#'
#' @return Updated stock-and-flow Model
#' @noRd
#'
erase_var = function(sfm, name){

  # Erase specified variables
  sfm$model$variables = lapply(sfm$model$variables, function(x) {
      # Remove variable from model
      x = x[!names(x) %in% name]

      # Remove variable from to, from
      lapply(x, function(y){

        if (is_defined(y$to)){
          if (y$to %in% name) y$to = NULL
        }
        if (is_defined(y$from)){
          if (y$from %in% name) y$from = NULL
        }
        return(y)
      })
    })

  sfm = validate_xmile(sfm)

  return(sfm)
}



#' Report whether any names were changed
#'
#' @param old_names Vector with old names
#' @param new_names Vector with new names
#'
#' @returns NULL
#' @noRd
report_name_change = function(old_names, new_names){

  # Warning if specified name changed
  idx = old_names != new_names
  if (any(idx)){
    warning(paste0(ifelse(sum(idx) > 1, "Name was", "Names were"),
                    " changed to be syntactically valid and/or avoid overlap: ",
                    paste0( paste0(old_names[idx], " -> ", new_names[idx]), collapse = ", ")))
  }

  return(invisible())
}







#' Create, modify or remove variables
#'
#' Variables in stock-and-flow models may be stocks, flows, constants, auxiliaries, or graphical functions. Add, change, or erase a variable in a stock-and-flow model.
#'
#' @section Stocks: Stocks define the state of the system. They accumulate material or information over time, such as people, products, or beliefs, which creates memory and inertia in the system. As such, stocks need not be tangible. Stocks are variables that can increase and decrease, and can be measured at a single moment in time. The value of a stock is increased or decreased by flows. A stock may have multiple inflows and multiple outflows. The net change in a stock is the sum of its inflows minus the sum of its outflows.
#'
#' The obligatory properties of a stock are "name", "type", and "eqn". Optional additional properties are "units", "label", "doc", "non_negative", "conveyor", "len".
#'
#' @section Flows: Flows move material and information through the system. Stocks can only decrease or increase through flows. A flow must flow from and/or flow to a stock. If a flow is not flowing from a stock, the source of the flow is outside of the model boundary. Similarly, if a flow is not flowing to a stock, the destination of the flow is outside the model boundary. Flows are defined in units of material or information moved over time, such as birth rates, revenue, and sales.
#'
#' The obligatory properties of a flow are "name", "type", "eqn", and either "from", "to", or both. Optional additional properties are "units", "label", "doc", "non_negative".
#'
#' @section Constants: Constants are variables that do not change over the course of the simulation - they are time-independent. These may be numbers, but also functions. They can depend only on other constants.
#'
#' The obligatory properties of a constant are "name", "type", and "eqn". Optional additional properties are "units", "label", "doc", "non_negative".
#'
#' @section Auxiliaries: Auxiliaries are dynamic variables that change over time. They are used for intermediate calculations in the system, and can depend on other flows, auxiliaries, constants, and stocks.
#'
#' The obligatory properties of an auxiliary are "name", "type", and "eqn". Optional additional properties are "units", "label", "doc", "non_negative".
#'
#' @section Graphical functions: Graphical functions, also known as table or lookup functions, are interpolation functions used to define the desired output (y) for a specified input (x). They are defined by a set of x- and y-domain points, which are used to create a piecewise linear function. The interpolation method defines the behaviour of the graphical function between x-points ("constant" to return the value of the previous x-point, "linear" to linearly interpolate between defined x-points), and the extrapolation method defines the behaviour outside of the x-points ("NA" to return NA values outside of defined x-points, "nearest" to return the value of the closest x-point).
#'
#' The obligatory properties of an auxiliary are "name", "type", "xpts", and "ypts". "xpts" and "ypts" must be of the same length. Optional additional properties are "units", "label", "doc", "source", "interpolation", "extrapolation".
#'
#' @param sfm Stock-and-flow model, object of class sdbuildR_xmile.
#' @param name Variable name. Character vector.
#' @param type Type of building block(s); one of 'stock', 'flow', 'constant', 'aux', or 'gf'). Does not need to be specified to modify an existing variable.
#' @param change_name New name for variable (optional). Defaults to NULL to indicate no change.
#' @param change_type New type for variable (optional). Defaults to NULL to indicate no change.
#' @param erase If TRUE, remove variable from model. Defaults to FALSE.
#' @param label Name of variable used for plotting. Defaults to the same as name.
#' @param eqn Equation (or initial value in the case of stocks). Defaults to "0.0".
#' @param conveyor Boolean indicating whether the Stock is a conveyor. Defaults to FALSE.
#' @param len Numerical value indicating the delay length of the conveyor Stock. Defaults to NULL.
#' @param to Target of flow. Must be a stock in the model. Defaults to NULL to indicate no target.
#' @param from Source of flow. Must be a stock in the model. Defaults to NULL to indicate no source.
#' @param units Unit of variable, such as 'meter'. Defaults to "1" (no units).
#' @param non_negative If TRUE, variable is enforced to be non-negative (i.e. strictly 0 or positive). Defaults to FALSE.
#' @param xpts Only for graphical function: vector of x-domain points. Must be of the same length as ypts.
#' @param ypts Only for graphical function: vector of y-domain points. Must be of the same length as xpts.
#' @param source Only for graphical function: name of variable which will serve as the input to the graphical function. Necessary to specify if units are used. Defaults to NULL.
#' @param interpolation Only for graphical function: interpolation method. Must be either "constant" or "linear". Defaults to "linear".
#' @param extrapolation Only for graphical function: extrapolation method. Must be either "nearest" or "NA". Defaults to "nearest".
#' @param doc Description of variable (optional). Defaults to "".
#'
#' @return Updated stock-and-flow model.
#' @seealso [xmile()]
#' @export
#'
#' @examples
#'
#'# First initialize an empty model:
#'sfm = xmile()
#'summary(sfm)
#'
#'# Add two stocks. Specify their initial values in the "eqn" property, as well
#'# as their plotting label
#'sfm = sfm %>%
#'  build("predator", "stock", eqn = 10, label = "Predator") %>%
#'  build("prey", "stock", eqn = 50, label = "Prey")
#'
#'# Plot the stock-and-flow diagram
#'plot(sfm)
#'
#'# Add four flows: the births and deaths of both the predators and prey. The
#'# "eqn" property of flows represents the rate of the flow. In addition, we
#'# specify which stock the flow is coming from ("from") or flowing to ("to").
#'sfm = sfm %>%
#'  build("predator_births", "flow", eqn = "delta*prey*predator",
#'        label = "Predator Births", to = "predator") %>%
#'  build("predator_deaths", "flow", eqn = "gamma*predator",
#'        label = "Predator Deaths", from = "predator") %>%
#'  build("prey_births", "flow", eqn = "alpha*prey",
#'        label = "Prey Births", to = "prey") %>%
#'  build("prey_deaths", "flow", eqn = "beta*prey*predator",
#'        label = "Prey Deaths", from = "prey")
#'plot(sfm)
#'
#'# The flows make use of four other variables: "delta", "gamma", "alpha", and
#'# "beta". Define these as constants in a vectorized manner for efficiency:
#'sfm = sfm %>%
#'  build(c("delta", "gamma", "alpha", "beta"), "constant",
#'        eqn = c(.025, .5, .5, .05),
#'        label = c("Delta", "Gamma", "Alpha", "Beta"),
#'        doc = c("Birth rate of predators", "Death rate of predators",
#'                "Birth rate of prey", "Death rate of prey by predators"))
#'
#'# We now have a complete predator-prey model which is ready to be simulated
#'sim = simulate(sfm)
#'plot(sim)
#'
build = function(sfm, name, type,
                 eqn = "0.0",
                 units = "1",
                 label = name,
                 doc = "",
                 change_name = NULL,
                 change_type = NULL,
                 erase = FALSE,
                 # Stock conveyor arguments
                 conveyor = FALSE, len = "",
                 # leakage = .1, leakage_type = "linear", **to do
                 # Flow arguments
                 to = NULL, from = NULL,
                 # Rarely used arguments
                 non_negative = FALSE,
                 # min = NULL, max = NULL,
                 # Graphical function arguments
                 xpts = NULL, ypts = NULL,
                 source = NULL,
                 interpolation = "linear",
                 extrapolation = "nearest"
                 ){

  # Basic check
  if (missing(sfm)){
    stop("No model specified!")
  }

  if (missing(name)){
    stop("name must be specified!")
  }

  if (!(all(is.character(name)))){
    stop("name must be a character!")
  }

  if (!(all(nzchar(name)))){
    stop("All entries in name must have characters!")
  }

  # Remove variable from model
  if (!is.null(erase)){
    if (length(erase) != 1){
      stop("erase must be a single logical value!")
    }

    if (!is.logical(erase)){
      stop("erase must be TRUE or FALSE!")
    }
  }

  # Get names dataframe
  check_xmile(sfm)
  names_df = get_names(sfm)
  var_names = names_df$name

  # Find variables which already exist
  idx_exist = name %in% var_names

  # Check if name does not exists and type is missing
  if (missing(type)){

    # If type is not specified, all names have to exist
    if (any(!idx_exist)){
      stop(sprintf("The variable%s %s %s not exist in your model! If you're trying to add a new variable, specify type (one of 'stock', 'flow', 'constant', 'aux', 'gf').",
                   ifelse(length(name[!idx_exist]) > 1, "s", ""),
                   paste0(name[!idx_exist], collapse = ", "),
                   ifelse(length(name[!idx_exist]) > 1, "do", "does") ))

    }

    # Find corresponding building block
    type = names_df[match(name, names_df$name), "type"]

  } else if (!missing(type)){

    type = tolower(type)
    if (!all(type %in% c("stock", "flow", "constant", "aux", "gf"))){
      stop("type needs to be one of 'stock', 'flow', 'constant', 'aux', or 'gf'!")
    }
    type = ensure_length(type, name)

    # If type is specified, and name already exists, but it doesn't match that type, stop
    match_type = names_df[match(name, names_df$name), "type"]

    nonmatching_type = idx_exist & type != match_type

    if (any(nonmatching_type)){

      if (erase){
        stop(paste0("These variables exist in your model but not as the type specified:\n- ",
                    paste0(paste0(name[nonmatching_type], " (type: ", match_type[nonmatching_type], ")"), collapse = ", ") ))
      } else {
        stop(paste0("These variables already exist in your model, but not as the type specified. Either omit the type to modify the variable, or specify a unique variable name to add a new variable of that type:\n- ",
                   paste0(paste0(name[nonmatching_type], " (type: ", match_type[nonmatching_type], ")"), collapse = ", ") ))
      }
    }

    # Ensure names are valid of new variables
    if (any(!idx_exist)){

      # Create syntactically valid, unique names (this also avoids overlap with previous names, but we stopped the function already if this is the case)
      new_names = create_R_names(name[!idx_exist], names_df)

      # Warning if specified name changed
      report_name_change(name[!idx_exist], new_names)

      # Change name
      name[!idx_exist] = new_names

    }
  }

  # Remove variable
  if (erase){

    # For erase, all names have to exist
    if (any(!idx_exist)){
      stop(sprintf("The variable%s %s %s not exist in your model!",
                   ifelse(length(name[!idx_exist]) > 1, "s", ""),
                   paste0(name[!idx_exist], collapse = ", "),
                   ifelse(length(name[!idx_exist]) > 1, "do", "does") ))

    }

    sfm = erase_var(sfm, name)
    return(sfm)
  }

  # Check change name of variable
  if (!is.null(change_name)){
    if (length(change_name) > 1 | length(name) > 1){
      stop("You can only change the name of one variable at a time!")
    }
  }

  # Check change type of variable
  if (!is.null(change_type)){

    if (length(change_type) > 1 | length(name) > 1){
      stop("You can only change the type of one variable at a time!")
    }

    change_type = tolower(change_type)
    if (!all(change_type %in% c("stock", "flow", "constant", "aux", "gf"))){
      stop("change_type needs to be one of 'stock', 'flow', 'constant', 'aux', or 'gf'!")
    }
  }


  # Get properties per building block
  keep_prop = get_building_block_prop()

  # Get names of passed arguments
  passed_arg = names(as.list(match.call())[-1]) %>%
    # Remove some arguments
    setdiff(c("sfm",
              # "name", "type",
              "erase", "change_name", "change_type"))

  # Check whether appropriate properties were passed for this variable type; issue warning if not
  appr_prop = Reduce(intersect, keep_prop[type])
  idx_inappr = !(passed_arg %in% appr_prop)
  if (any(idx_inappr)){
    warning(sprintf("These properties are not appropriate for %s specified type%s (%s):\n- %s\nThese will be ignored.",
                    ifelse(length(unique(type)) > 1, "all", "the"),
                    ifelse(length(unique(type)) > 1, "s", ""),
                    paste0(unique(type), collapse = ", "), paste0(passed_arg[idx_inappr], collapse = ", ")))
  }

  # Flow properties
  if (!is.null(to)){
    if (!inherits(to, "character")){
      stop("to must be a character!")
    }

    if (length(name) == 1 & length(to) > 1){
      stop("A flow may only have one target!")
    }
    to = ensure_length(to, name)
  }

  if (!is.null(from)){
    if (!inherits(from, "character")){
      stop("from must be a character!")
    }

    if (length(name) == 1 & length(from) > 1){
      stop("A flow may only have one source!")
    }
    from = ensure_length(from, name)
  }




  # Graphical functions
  if (any(type == "gf")){
    if (length(name) != 1){
      stop("Vectorized building is not supported for graphical functions. Please ony specify one graphical function at a time.")
    }

    if (is.null(xpts) & !is.null(ypts)){
      stop("xpts must be specified for graphical functions!")
    }

    if (is.null(ypts) & !is.null(xpts)){
      stop(" ypts must be specified for graphical functions!")
    }

    if (!is.null(xpts) & !is.null(ypts)){

      # Split xpts and ypts temporarily to check length
      if (inherits(xpts, "character")){
        xpts = strsplit(xpts, ",")[[1]]
        xpts[1] = gsub("^c\\(", "", xpts[1])
        xpts[length(xpts)] = gsub("\\)$", "", xpts[length(xpts)])
      }

      if (inherits(ypts, "character")){
        ypts = strsplit(ypts, ",")[[1]]
        ypts[1] = gsub("^c\\(", "", ypts[1])
        ypts[length(ypts)] = gsub("\\)$", "", ypts[length(ypts)])
      }

      if (length(xpts) != length(ypts)){
        # Ensure length of xpts and ypts for graphical functions is the same
        stop(paste0("For graphical functions, the length of xpts must match that of ypts.\n",
                    paste0("The length of xpts is ", length(xpts), "; the length of ypts is ", length(ypts), collapse = "\n")))
      }


      if (!inherits(xpts, "character")){
        xpts = paste0("c(", paste0(xpts, collapse = ", "), ")")
      }

      if (!inherits(ypts, "character")){
        ypts = paste0("c(", paste0(ypts, collapse = ", "), ")")
      }

    }

    interpolation = tolower(interpolation)

    if (length(interpolation) > 1){
      stop("interpolation must be a single value!")
    }

    if (!interpolation %in% c("linear", "constant")){
      stop(sprintf("interpolation must be 'linear' or 'constant'!"))
    }

    if (length(extrapolation) > 1){
      stop("extrapolation must be a single value!")
    }

    if (!extrapolation %in% c("nearest", "NA")){
      stop(sprintf("extrapolation must be either 'nearest' or 'NA'!"))
    }

    if (!is.null(source)){
      if (!inherits(source, "character")){
        stop("source must be a character!")
      }

      # Ensure source is a single value
      if (length(source) > 1){
        stop("source must be a single value!")
      }

    }
  }


  # **if conveyor is specified len must be specified


  # If overwriting name with change_name
  if (!is.null(change_name)){

    # Ensure new name is syntactically valid
    chosen_new_name = change_name
    change_name = create_R_names(change_name, names_df)
    report_name_change(chosen_new_name, change_name)

    # Overwrite name
    variable_names = names(sfm$model$variables[[type]])
    variable_names[variable_names == name] = change_name
    names(sfm$model$variables[[type]]) = variable_names
    sfm$model$variables[[type]][[change_name]][["name"]] = change_name

    # Overwrite label in case it was the same as the old name
    if (sfm$model$variables[[type]][[change_name]][["label"]] == name){
      sfm$model$variables[[type]][[change_name]][["label"]] = change_name
    }


    # Replace references to name with change_name everywhere (eqn, from, to)
    sfm$model$variables = lapply(sfm$model$variables, function(y){
      lapply(y, function(x){

        if (is_defined(x$eqn)){
          idx_df = get_range_names(x$eqn, name,
                                   names_with_brackets = FALSE)
          if (nrow(idx_df) > 0){
            # Reverse indices to replace correctly
            for (i in rev(1:nrow(idx_df))){
              stringr::str_sub(x$eqn, idx_df[i, "start"], idx_df[i, "end"]) = change_name
            }

            # Update julia translation
            idx_df = get_range_names(x$eqn_julia, name,
                                     names_with_brackets = FALSE)
            if (nrow(idx_df) > 0){
              # Reverse indices to replace correctly
              for (i in rev(1:nrow(idx_df))){
                stringr::str_sub(x$eqn_julia, idx_df[i, "start"], idx_df[i, "end"]) = change_name
              }
            }
          }
        }
        if (is_defined(x$from)){
          if (x$from == name) x$from = change_name
        }
        if (is_defined(x$to)){
          if (x$to == name) x$to = change_name
        }
        return(x)
      })
    })

    name = change_name

    # Update
    var_names = get_model_var(sfm)
    idx_exist = name %in% var_names

    # Redo equation (in case of delay variables, the names need to be updated to get the correct suffix, e.g. "a" -> "b" needs new delay names "b_delay1_acc1", etc.; in addition, some types can't have delays)

    if (!"eqn" %in% passed_arg){
      eqn = sfm$model$variables[[type]][[name]][["eqn"]]
      passed_arg = c(passed_arg, "eqn")
    }
  }

  # Change type of building block
  if (!is.null(change_type)){

    if (type != change_type){

      old_prop = sfm$model$variables[[type]][[name]]

      updated_defaults = formals(build) %>% utils::modifyList(old_prop)
      updated_defaults = updated_defaults[names(updated_defaults) %in% keep_prop[[change_type]]]
      updated_defaults = updated_defaults[!lengths(updated_defaults) == 0]

      # Remove old part
      sfm$model$variables[[type]][name] = NULL

      # Add new part
      sfm$model$variables[[change_type]][[name]] = updated_defaults

      type = change_type

      # Redo equation (in case of delay variables, the names need to be updated to get the correct suffix, e.g. "a" -> "b" needs new delay names "b_delay1_acc1", etc.; in addition, some types can't have delays)
      if (!"eqn" %in% passed_arg){
        eqn = sfm$model$variables[[type]][[name]][["eqn"]]
        passed_arg = c(passed_arg, "eqn")
      }
    }




  }



  # Only need regex_units if any of the following are passed
  if (any(c("eqn", "units", "len") %in% passed_arg)){
    regex_units = get_regex_units()
  }


  if ("eqn" %in% passed_arg){
    if (any(is.null(eqn))){
      warning("Equation cannot be NULL! Setting empty equations to 0...")
      eqn[is.null(eqn)] = "0.0"
    }

    if (any(!nzchar(eqn))){
      warning("Equation cannot be empty! Setting empty equations to 0...")
      eqn[!nzchar(eqn)] = "0.0"
    }

    # Change all equations to characters
    if (!is.null(eqn)){
      eqn = as.character(eqn)
    }

    if (any(grepl("^[ ]*function[ ]*\\(", eqn))){
      stop("Model variables cannot be functions! To add a custom function, use macro().")
    }


    # Ensure units are cleaned in u() in eqn
    # eqn = sapply(eqn, function(x){clean_unit_in_u(x, regex_units)}) %>% unname()
    eqn = clean_unit_in_u(eqn, regex_units)
    eqn = ensure_length(eqn, name)

    # Convert to julia - note that with delay() and past(), an intermediary property is added; with delayN() and smoothN(), a func property (nested list) is added
    eqn_julia = lapply(1:length(name), function(i){convert_equations_julia(sfm, type[i], name[i], eqn[i], var_names,
                                                                           regex_units = regex_units,
                                                                           debug = FALSE)}) %>% unname()

    # Remove old func list
    for (i in length(name)){
      sfm$model$variables[[type[i]]][[name[i]]][["func"]] = NULL
    }

  }

  # Units
  if (!is.null(units)){
    if (!inherits(units, "character")){
      units = as.character(units)
    }

    # Set empty unit to 1
    if (any(!nzchar(units))){
      units[!nzchar(units)] = "1"
    }

    # Units are not supported well in R, so translate to julia directly
    units = sapply(units, function(x){clean_unit(x, regex_units)}) %>% unname()
    units = ensure_length(units, name)
  }

  # # Minimum and maximum constraint
  # if (!is.null(min)){
  #
  #   min = clean_unit_in_u(min, regex_units)
  #   # min = sapply(min, function(x){clean_unit_in_u(x, regex_units)}) %>% unname()
  #
  #   min_julia = sapply(min, function(x){convert_equations_julia(sfm, "min", "min", x, var_names,
  #                                                               regex_units = regex_units,
  #                                                               debug = FALSE)$eqn_julia}) %>% unname()
  #   min = ensure_length(min, name)
  #   min_julia = ensure_length(min_julia, name)
  #   passed_arg = c(passed_arg, "min_julia")
  # }
  #
  # if (!is.null(max)){
  #   max = clean_unit_in_u(max, regex_units)
  #
  #   max_julia = sapply(max, function(x){convert_equations_julia(sfm, "max", "max", x, var_names,
  #                                                               regex_units = regex_units,
  #                                                               debug = FALSE)$eqn_julia}) %>% unname()
  #   max = ensure_length(max, name)
  #   max_julia = ensure_length(max_julia, name)
  #   passed_arg = c(passed_arg, "max_julia")
  # }

  if ("conveyor" %in% passed_arg){
    if (!inherits(conveyor, "logical")){
      stop("conveyor must be either TRUE or FALSE!")
    }
    conveyor = ensure_length(conveyor, name)
  }

  if ("len" %in% passed_arg){
    #** len can also be a unit
    # if (!inherits(len, "numeric")){
    #   stop("len must be a numeric!")
    # }
    len = clean_unit_in_u(len, regex_units)
    len = ensure_length(len, name)
  }

  if ("non_negative" %in% passed_arg){
    if (!all(is.logical(non_negative))){
      stop("non_negative must be either TRUE or FALSE!")
    }
    non_negative = ensure_length(non_negative, name)
  }

  if ("label" %in% passed_arg){
    if (!inherits(label, "character")){
      stop("label must be a character!")
    }
    label = ensure_length(label, name)
  }

  if ("doc" %in% passed_arg){
    if (!inherits(doc, "character")){
      stop("doc must be a character!")
    }
    doc = ensure_length(doc, name)
  }


  # Collect all arguments in environment but only keep those that were passed
  argg <- c(as.list(environment()))[unique(passed_arg)]
  # argg[["name"]] = name
  argg[["type"]] = type # Keep type for ease

  # Create nested 3-level list with all model entries
  new_element = argg %>%
    purrr::transpose() %>%
    lapply(list) %>%
    # Create three named levels: type, name, properties
    purrr::imap(function(x, y){
      # Make sure each model element only has appropriate entries

      keep_prop_y = keep_prop[[type[y]]]
      keep_x = x[[1]][names(x[[1]]) %in% keep_prop_y]

      # Add converted julia equation
      if ("eqn" %in% passed_arg){

        keep_x = keep_x %>% utils::modifyList(eqn_julia[[y]])
      }

      list(keep_x) %>% stats::setNames(name[y])

      }) %>% stats::setNames(type)

  # Add elements to model (in for-loop, as otherwise not all elements are added)
  for (i in seq_along(name)){
    sfm$model$variables = sfm$model$variables %>%
      utils::modifyList(new_element[i])
  }

  sfm = validate_xmile(sfm)

  return(sfm)

}




#' Get possible variable properties per building block type
#'
#' @return List with default properties per building block type
#' @noRd
#'
get_building_block_prop = function(){
  list(
    "stock" = c("type", "name", "eqn", "units", "label", "doc",
                "non_negative", "conveyor", "len",
                "eqn_julia"),
    "flow" = c("type", "name", "eqn", "to", "from", "units", "label", "doc",
               "non_negative",
               "eqn_julia"),
    "constant" = c("type", "name", "eqn", "units", "label", "doc",
      "non_negative",
      "eqn_julia"),
    "aux" = c("type", "name", "eqn", "units", "label", "doc",
              "non_negative",
              "eqn_julia"),
    "gf" = c("type", "name", "units", "label",  "xpts", "ypts", "source", "interpolation", "extrapolation", "doc")
  ) %>% return()
}



#' Quickly get names of model variables
#'
#' @inheritParams build
#'
#' @noRd
#' @returns Vector with names of model variables
get_model_var = function(sfm){
  c(unname(unlist(lapply(sfm$model$variables, names))), names(sfm$macro))
}


#' Create dataframe with stock-and-flow model variables, types, labels, and units
#'
#' @inheritParams build
#'
#' @return Dataframe
#' @noRd
#'
get_names <- function(sfm) {

  # Return empty dataframe if no variables
  nr_var <- sum(lengths(sfm$model$variables))
  if (nr_var == 0) {
    names_df <- data.frame(
      type = character(0),
      name = character(0),
      label = character(0),
      units = character(0)
    )
    return(names_df)
  }

  # Building blocks to check
  blocks <- c("stock", "aux", "constant", "flow", "gf")
  entries <- list()

  # Collect variable information
  for (block in blocks) {
    if (!is.null(sfm$model$variables[[block]])) {
      for (var in sfm$model$variables[[block]]) {
        if (!is.null(var$name)) {
          entries[[length(entries) + 1]] <- list(
            type = block,
            name = var$name,
            label = var$label,
            units = var$units
          )
        }
      }
    }
  }

  # Convert to dataframe
  if (length(entries) > 0) {
    names_df <- do.call(rbind, lapply(entries, as.data.frame, stringsAsFactors = FALSE))
  } else {
    column_names <- c("type", "name", "label", "units")
    names_df <- as.data.frame(matrix(NA, nrow = 1, ncol = length(column_names)))
    colnames(names_df) <- column_names
  }

  # Add macros if any
  if (!is.null(sfm$macro) && length(names(sfm$macro)) > 0) {
    macro_df <- data.frame(
      type = "macro",
      name = names(sfm$macro),
      label = names(sfm$macro),
      units = "",
      stringsAsFactors = FALSE
    )
    names_df <- rbind(names_df, macro_df)
  }

  rownames(names_df) <- NULL
  return(names_df)
}



#' Get exported function names from a package
#'
#' @param package package name
#'
#' @returns Vector with names of exported functions
#' @noRd
get_exported_functions <- function(package) {
  # Load the package namespace (does not attach to search path)
  ns <- getNamespace(package)

  # Get all exported objects
  exports <- getNamespaceExports(package)

  # Filter for functions
  functions <- exports[sapply(exports, function(x) {
    is.function(get(x, envir = ns))
  })]

  # Return sorted for consistency
  sort(functions)
}

#' Create syntactically valid, unique names for use in R and Julia
#'
#' @param create_names Vector of strings with names to transform to valid names
#' @param names_df Dataframe with at least the column name
#' @param protected Optional vector of protected names
#'
#' @return Translated names
#' @noRd
#'
create_R_names = function(create_names, names_df, protected = c()){

  # Define protected names: these cannot be used as variable names
  protected_names = c(

    # Reserved words in R
    "if",
    "else", "repeat", "function", "return", "while", "for", "in", "next", "break", "TRUE", "FALSE", # already protected
    "T", "F",
    # "NULL", "Inf", "NaN", "NA", "NA_integer_", "NA_real_", "NA_complex_", "NA_character_", # already protected
    "time", # used as first variable in simulation dataframe #"Time", "TIME",
    # "constraints",
    # Add Julia keywords
    "baremodule", "begin", "break", "catch", "const", "continue", "do",
    "else", "elseif", "end", "export", "false", "finally",
    "global", "error", "throw",
    "import", "let", "local", "macro", "module", "quote", "return", "struct", "true", "try", "catch", "using",

    "Missing", "missing", "Nothing", "nothing",
    # "?", "]",

    # Add R custom functions
    get_exported_functions("sdbuildR"),

    # Add julia custom functions
    names(get_func_julia()),
    # These are variables in the ode and cannot be model element names
    unname(unlist(P[!names(P) %in% c("sim_df_name", "change_prefix", "conveyor_suffix", "delayN_suffix", "delay_suffix", "delay_order_suffix", "delay_length_suffix", "past_suffix", "past_length_suffix", "fix_suffix", "fix_length_suffix")])), protected,
    as.character(stats::na.omit(names_df$name))
  ) %>% unique()

  stock_names = names_df[names_df$type == "stock", "name"]
  if (length(stock_names) > 0){
    if (all(!is.null(stock_names) & !is.na(stock_names))){
      protected_names = c(protected_names,
                          paste0(P$change_prefix, stock_names))
    }
  }

  # Make syntactically valid and unique names out of character vectors; Insight Maker allows names to be double, so make unique
  new_names = make.names(c(protected_names, trimws(create_names)), unique = TRUE)
  # For Julia translation, remove names with a period
  new_names = stringr::str_replace_all(new_names, "\\.", "_")
  # This may cause overlap in names, so repeat
  new_names = make.names(new_names, unique = TRUE)
  new_names = stringr::str_replace_all(new_names, "\\.", "_")
  new_names = make.names(new_names, unique = TRUE)[-seq_along(protected_names)]  # Remove protected names



  # If any names end in a suffix used by sdbuildR, add _
  pattern = paste0(P$conveyor_suffix, "$|", P$delay_suffix, "[0-9]+$|", P$past_suffix, "[0-9]+$|",
                   P$fix_suffix, "$|",
                   P$fix_length_suffix, "$|",
                   P$conveyor_suffix, "$|", P$delayN_suffix, "[0-9]+", P$delayN_acc_suffix, "[0-9]+$|", P$smoothN_suffix, "[0-9]+", P$delayN_acc_suffix, "[0-9]+$")

  idx = grepl(new_names, pattern = pattern)
  new_names[idx] = paste0(new_names[idx], "_")
  # e.g. names cannot end with _delay[0-9]+$ or _delay[0-9]+_acc[0-9]+$

  return(new_names)
}




#' Apply purrr::map() and unlist() whilst preserving NULL
#'
#' @param x List
#' @param element_name String, desired name of list
#' @param change_null_to String, what to change NULL to
#'
#' @return Vector
#' @noRd
#'
get_map = function(x, element_name, change_null_to = ""){

  if (length(x) == 0){
    return(c())
  }

  # x_list = x %>% purrr::map(element_name)
  x_list = lapply(x, `[[`, element_name)
  # Unlist preserving NULL
  x_list[sapply(x_list, function(x){is.null(x) | length(x) == 0 })] = change_null_to
  return(unlist(x_list))
}



#' Generate code to build stock-and-flow model
#'
#' Create R code to rebuild an existing stock-and-flow model. This may help to understand how a model is built, or to modify an existing one.
#'
#' @inheritParams build
#' @inheritParams simulate
#'
#' @return String with code to build stock-and-flow model from scratch.
#' @export
#'
#' @examples
#' sfm = xmile("SIR")
#' get_build_code(sfm)
get_build_code = function(sfm, format_code = TRUE){

  check_xmile(sfm)
  defaults = formals(build)
  defaults = defaults[!names(defaults) %in% c("sfm", "name", "type", "label", "...")]

  # Simulation specifications - careful here. If a default is 100.0, this will be turned into 100. Need to have character defaults to preserve digits.
  defaults_sim_specs = formals(sim_specs)
  defaults_sim_specs = defaults_sim_specs[!names(defaults_sim_specs) %in% c("name", "caption", "created", "...")]
  sim_specs_list = sfm$sim_specs

  # Remove defaults
  default_elements = defaults_sim_specs[names(sim_specs_list)]
  default_elements = default_elements[!lengths(default_elements) == 0]

  idx = unlist(default_elements) == unlist(sim_specs_list[names(default_elements)])
  sim_specs_list[names(which(idx))] = NULL
  sim_specs_list <- lapply(sim_specs_list, function(z) if (is.character(z)) paste0("'", z, "'") else z)
  sim_specs_str = paste0(names(sim_specs_list), " = ", unname(sim_specs_list), collapse = ", ")
  sim_specs_str = paste0(" %>%\n\t\tsim_specs(", sim_specs_str, ")")

  # Model units
  if (length(sfm$model_units) > 0){
    model_units_str = lapply(sfm$model_units, function(x){
        x = x %>% purrr::map_if(is.character, \(z) paste0("'", z, "'"))
        sprintf("model_units(%s)", paste0(names(x), " = ", unname(x), collapse = ", "))
      }) %>% unlist() %>% paste0(., collapse = "%%>%%\n\t\t")
    model_units_str = paste0(" %>%\n\t\t", model_units_str)
  } else {
    model_units_str = ""
  }

  # Macros
  if (length(sfm$macro) > 0){
    macro_str = lapply(sfm$macro, function(x){

        # Remove properties containing "_julia"
        x[grepl("_julia", names(x))] = NULL

        x <- lapply(x, function(z) if (is.character(z)) paste0("'", z, "'") else z)
        sprintf("macro(%s)", paste0(names(x), " = ", unname(x), collapse = ", "))
      }) %>% unlist() %>% paste0(., collapse = "%%>%%\n\t\t")
    macro_str = paste0(" %>%\n\t\t", macro_str)
  } else {
    macro_str = ""
  }

  # Header string
  x = sfm$header
  # x = x %>% purrr::map_if(function(y){is.character(y) | inherits(y, "POSIXt")}, \(z) paste0("'", z, "'"))
  x = lapply(x, function(z) if (is.character(z) | inherits(z, "POSIXt")) paste0("'", z, "'") else z)

  header_str = paste0(" %>%\n\t\theader(", paste0(names(x), " = ", unname(x), collapse = ", "), ")")

  # Variables
  if (length(unlist(sfm$model$variables)) > 0){
    var_str = lapply(sfm$model$variables, function(x){
        lapply(x, function(y){
          z = y
          z$name = NULL
          z$type = NULL
          z$func = NULL

          # Remove defaults
          default_elements = defaults[names(z)]
          default_elements = default_elements[!lengths(default_elements) == 0]
          idx = unlist(default_elements) != unlist(z[names(default_elements)])
          z = z[names(which(idx))]

          # Remove properties containing "_julia"
          z[grepl("_julia", names(z))] = NULL

          # z = z %>% purrr::map_if(is.character, \(a) paste0("'", a, "'"))
          z = lapply(z, function(a) if (is.character(a)) paste0("'", a, "'") else a)

          sprintf("build('%s', '%s'%s)",
                  y$name, y$type, ifelse(length(z) > 0, paste0(", ", paste0(names(z), " = ", unname(z), collapse = ", ")), "") )
        })
      })
    var_str = var_str[lengths(var_str) > 0]
    var_str = paste0(" %>%\n\t\t", paste0(unlist(var_str), collapse = " %>%\n\t\t"))
  } else {
    var_str = ""
  }

  script = sprintf("sfm = xmile()%s%s%s%s%s", sim_specs_str, header_str, var_str, macro_str, model_units_str)

  # Format code
  if (format_code & requireNamespace("styler", quietly = TRUE)){
    suppressWarnings(suppressMessages(
      script <- styler::style_text(script)
    ))
  }

  return(script)
}



#' Debug stock-and-flow model
#'
#' Check for common formulation problems in a stock-and-flow model.
#'
#' The following problems are detected:
#' - An absence of stocks
#' - Flows without a source (`from`) or target (`to`)
#' - Flows connected to a stock that does not exist
#' - Connected stocks and flows without both having units or no units
#' - Undefined variable references in equations
#' - Circularities in equations
#' - Missing unit definitions
#'
#' The following potential problems are detected:
#' - Absence of flows
#' - Stocks without inflows or outflows
#' - Equations with a value of 0
#' - Static variables depending on dynamic variables
#'
#' @inheritParams build
#' @param quietly If TRUE, don't print problems. Defaults to FALSE.
#'
#' @returns Logical value indicating whether any problems were detected.
#' @export
#'
#' @examples
#' # No issues
#' sfm = xmile("SIR")
#' debugger(sfm)
#'
#' # Detect absence of stocks or flows
#' sfm = xmile()
#' debugger(sfm)
#'
#' # Detect stocks without inflows or outflows
#' sfm = xmile() %>% build("Prey", "stock")
#' debugger(sfm)
#'
#' # Detect circularities in equation definitions
#' sfm = xmile() %>% build("Prey", "stock", eqn = "Predator") %>%
#'  build("Predator", "stock", eqn = "Prey")
#' debugger(sfm)
#'
debugger = function(sfm, quietly = FALSE){

  check_xmile(sfm)

  if (!is.logical(quietly)){
    stop("quietly must be TRUE or FALSE!")
  }

  problems = c()
  potential_problems = c()

  constant_names = names(sfm$model$variables$constant)
  aux_names = names(sfm$model$variables$aux)
  stock_names = names(sfm$model$variables$stock)
  flow_df = get_flow_df(sfm)
  flow_names = flow_df$name
  names_df = get_names(sfm)

  ### Check whether all Stocks have inflows and/or outflows
  if (length(stock_names) > 0 & nrow(flow_df) > 0){

    idx = stock_names %in% flow_df$to | stock_names %in% flow_df$from

    if (any(!idx)){
      potential_problems = c(potential_problems, paste0("* These stocks are not connected to any flows:\n- ",
                     paste0(stock_names[!idx], collapse = ", ")))
    }

  } else if (length(stock_names) == 0){
    problems = c(problems, "* Your model has no stocks.")
  }


  ### Check whether all flows either have a from or to property
  if (length(flow_names) > 0){
    idx = !nzchar(flow_df$from) & !nzchar(flow_df$to)

    if (any(idx)){
      problems = c(problems, paste0("* These flows are not connected to any stock:\n- ",
                     paste0(flow_names[idx], collapse = ", "), "\nConnect a flow to a stock using 'to' and/or 'from' in build()."))
    }

    ### Find whether the from and to stocks exist
    idx_to = (!flow_df$to %in% stock_names) & nzchar(flow_df$to)
    idx_from = (!flow_df$from %in% stock_names) & nzchar(flow_df$from)

    if (any(idx_to) | any(idx_from)){
      problems = c(problems, paste0("* These flows are connected to a stock that does not exist:\n - ",
                     paste0(c(flow_names[idx_to], flow_names[idx_from]), collapse = ", ")))
    }

    ### Find whether both flows and stocks have units
    flows_units = names_df[match(flow_names, names_df$name), "units"]
    stock_units = names_df[match(stock_names, names_df$name), "units"]

  } else {
    potential_problems = c(potential_problems, "* Your model has no flows.")
  }


  ### Check equations with zero
  zero_eqn = lapply(unname(sfm$model$variables), function(y){
    lapply(y, function(x){
      if (is_defined(x$eqn)){
        if (x$eqn == "0" | x$eqn == "0.0"){
          return(x$name)
        }
      }
      return(NULL)
    })
    }) %>% unlist() %>% purrr::compact()

  if (length(zero_eqn) > 0){
    potential_problems = c(potential_problems, paste0("* These variables have an equation of 0:\n- ", paste0(unname(zero_eqn), collapse = ", ")))
  }

  ### Detect undefined variable references in equations
  out = detect_undefined_var(sfm)
  if (out$issue){
    problems = c(problems,  paste0("* ", out$msg))
  }

  ### Detect whether static variables depend on dynamic ones
  out = static_depend_on_dyn(sfm)
  if (out$issue){
    potential_problems = c(potential_problems, paste0("* ",  out$msg))
  }

  ### Detect circularities in equations
  out = order_equations(sfm, print_msg = FALSE)
  if (out$static$issue){
    problems = c(problems,
                    paste0("* Ordering static equations failed. ", out$static$msg, collapse = ""))
  }
  if (out$dynamic$issue){
    problems = c(problems,
                    paste0("* Ordering dynamic equations failed. ", out$dynamic$msg, collapse = ""))

  }


  ### Find missing unit definitions
  regex_units = get_regex_units()

  # Check whether all units are defined
  add_model_units = detect_undefined_units(sfm,
                                     new_eqns = c(sfm$model$variables %>%
                                                    lapply(function(x){lapply(x, `[[`, "eqn_julia")}) %>% unlist(),
                                                  # sfm$global$eqn_julia,
                                                  unlist(lapply(sfm$macro, `[[`, "eqn_julia"))),
                                     new_units = sfm$model$variables %>%
                                       lapply(function(x){lapply(x, `[[`, "units")}) %>% unlist(),
                                     regex_units= regex_units, R_or_Julia = "Julia")
  if (length(add_model_units) > 0){
    problems = c(problems, paste0("* These units are not defined:\n- ",
                                  paste0(names(add_model_units), collapse = ", ")))
  }



  if (!quietly & length(problems) > 0){
    message("Problems:")
    message(paste0(problems, collapse = "\n\n"))
  } else if (!quietly){
    message("No problems detected!")
  }

  if (!quietly & length(potential_problems) > 0){
    prefix = ifelse(!quietly & length(problems) > 0, "\n", "")
    message(paste0(prefix, "Potentially problematic:"))
    message(paste0(potential_problems, collapse = "\n\n"))
  }


  if (quietly){
    return(list(problems = paste0(problems, collapse = "\n\n"),
                potential_problems = paste0(potential_problems, collapse = "\n\n"))
           )
  } else {
    return(invisible())
  }
}



#' Check whether static variables (stock's initial values, constants) depend on dynamic variables
#'
#' @inheritParams build
#'
#' @noRd
#' @returns Logical value
static_depend_on_dyn = function(sfm){

  # Check whether a stock depends on a dynamic variable, give warning
  dependencies = sfm$model$variables[c("stock", "constant")] %>%
    unname() %>% purrr::list_flatten() %>%
    purrr::map("eqn") %>%
    find_dependencies(sfm, ., only_model_var = TRUE)

  names_df = get_names(sfm)
  dynamic_var = names_df[names_df$type %in% c("aux", "flow"), "name"]

  static_with_dyn_dep = lapply(dependencies, function(x){
    x[x %in% dynamic_var]
  }) %>% purrr::compact()

  if (length(static_with_dyn_dep)){

    static_with_dyn_dep = sapply(static_with_dyn_dep, paste0, collapse = ", ")
    stock_or_constant = names_df[match(names(static_with_dyn_dep), names_df$name), "type"]

    msg = paste0(c("Simulation impossible: static variables depend on dynamic variables!",
                 paste0(
                   paste0("- ",
      ifelse(stock_or_constant == "stock", "The initial value of stock ", "The constant "),
      names(static_with_dyn_dep), " depends on ", static_with_dyn_dep), collapse = "\n")),
      collapse = "\n")

    # for (i in seq_along(static_with_dyn_dep)){
    #   stock_or_constant = names_df[names_df$name == names(static_with_dyn_dep)[i], "type"]
    #
    #   message(paste0(
    #     ifelse(stock_or_constant == "stock", "The initial value of stock ", "The constant "),
    #     names(static_with_dyn_dep)[i], " depends on ", paste0(static_with_dyn_dep[[i]], collapse = ", ")))
    # }
    return(list(issue = TRUE, msg = msg))
  } else {
    return(list(issue = FALSE))
  }

}







#' Convert stock-and-flow model to dataframe
#'
#' Create a dataframe with properties of all model variables, model units, and macros. Specify the variable types, variable names, and/or properties to get a subset of the dataframe.
#'
#' @inheritParams plot.sdbuildR_xmile
#' @param type Variable types to retain in the dataframe. Must be one or more of 'stock', 'flow', 'constant', 'aux', 'gf', 'model_units', or 'macro'. Defaults to NULL to include all variable types.
#' @param name Variable names to retain in the dataframe. Defaults to NULL to include all variables.
#' @param properties Variable properties to retain in the dataframe. Defaults to NULL to include all properties.
#' @param row.names NULL or a character vector giving the row names for the data frame. Missing values are not allowed.
#' @param optional Ignored parameter.
#'
#' @returns Dataframe with properties of all model variables, model units, and macros.
#' @export
#' @method as.data.frame sdbuildR_xmile
#'
#' @examples as.data.frame(xmile("SIR"))
#'
#' # Only show stocks
#' as.data.frame(xmile("SIR"), type = "stock")
#'
#' # Only show name and equation
#' as.data.frame(xmile("SIR"), properties = c("name", "eqn"))
as.data.frame.sdbuildR_xmile = function(x,
                                        row.names = NULL, optional = FALSE,
                                        type = NULL, name = NULL, properties = NULL, ...){

  check_xmile(x)
  sfm = x

  # nr_var = length(sfm$model$variables %>% purrr::flatten())
  # if (nr_var == 0){
  #   stop("Your model contains no variables!")
  # }

  # Only keep specified types
  if (!is.null(type)){
    type = Filter(nzchar, unique(tolower(type)))

    if (length(type) == 0){
      stop("At least one type must be specified!")
    }

    # Remove trailing "s" from plural types to show a bit more grace to the user
    type[type != "model_units"] = gsub("s$", "", type[type != "model_units"])
    type[type == "model_unit"] = "model_units"

    if (!all(type %in% c("stock", "flow", "constant", "aux", "gf", "model_units", "macro"))){
      stop("type needs to be one or more of 'stock', 'flow', 'constant', 'aux', 'gf', 'model_units', or 'macro'!")
    }
  }

  df = data.frame()

  # Add model variables
  nr_var = sum(lengths(sfm$model$variables))
  if ((is.null(type) | any(c("stock", "flow", "constant", "aux", "gf") %in% type)) & nr_var > 0){

    if (!is.null(type)){
      sfm$model$variables = sfm$model$variables[type[type %in% c("stock", "flow", "constant", "aux", "gf")]]
    }

    # Remove func
    sfm$model$variables = lapply(sfm$model$variables, function(y){
      lapply(y, function(x){
      x["translated_func"] = NULL
      x["func"] = NULL

      if (x$type == "gf"){
        x$xpts = paste0(x$xpts, collapse = ", ")
        x$ypts = paste0(x$ypts, collapse = ", ")
      }

      return(x)
    })
    })

    # Create dataframe with model variable properties
    model_df = lapply(sfm$model$variables %>% purrr::compact(), function(x){
        as.data.frame(do.call(dplyr::bind_rows, x))
      }) %>% do.call(dplyr::bind_rows, .)
    df = dplyr::bind_rows(df, model_df)

  }

  # Add model units
  if ((is.null(type) | "model_units" %in% type) & length(sfm$model_units) > 0){
    units_df = as.data.frame(do.call(dplyr::bind_rows, sfm$model_units))
    units_df$prefix = NULL
    units_df$type = "model_units"
    df = dplyr::bind_rows(df, units_df)
  }

  # Add macros
  if ((is.null(type) | "macro" %in% type) & length(sfm$macro) > 0){

    # Remove func
    sfm$macro = lapply(sfm$macro, function(x){
        # x["translated_func"] = NULL
        x["func"] = NULL
        return(x)
    })

    macro_df = as.data.frame(do.call(dplyr::bind_rows, sfm$macro))
    macro_df$type = "macro"
    df = dplyr::bind_rows(df, macro_df)
  }

  if (nrow(df) == 0) return(df)

  # Only keep specified names
  if (!is.null(name)){
    # Check if names exist
    name = Filter(nzchar, unique(name))

    if (length(name) == 0){
      stop("At least one name must be specified!")
    }

    idx_exist = name %in% df$name
    if (!all(idx_exist)){
      stop(sprintf("The variable%s %s %s not exist in your model!",
                   ifelse(length(name[!idx_exist]) > 1, "s", ""),
                   paste0(name[!idx_exist], collapse = ", "),
                   ifelse(length(name[!idx_exist]) > 1, "do", "does") ))
    }
    df = df[df$name %in% name, , drop = FALSE]
    if (nrow(df) == 0) return(df)
  }

  # Only keep specified properties
  if (!is.null(properties)){
    # Check if properties exist
    properties = Filter(nzchar, unique(tolower(properties)))
    if (length(properties) == 0){
      stop("At least one property must be specified!")
    }

    existing_prop = Reduce(union, get_building_block_prop())
    idx_exist = properties %in% existing_prop
    prop_in_df = properties %in% names(df)

    if (!all(idx_exist)){
      stop(sprintf("%s %s!",
                   paste0(properties[!idx_exist], collapse = ", "),
                   ifelse(length(properties[!idx_exist]) > 1, "are not existing properties", "is not an existing property")))
    }

    # if (!all(prop_in_df)){
    #   stop(sprintf("%s %s!",
    #                paste0(properties[!prop_in_df], collapse = ", "),
    #                ifelse(length(properties[!prop_in_df]) > 1, "are not existing properties", "is not an existing property")))
    # }

    df = df[, names(df) %in% properties, drop = FALSE]
    if (nrow(df) == 0) return(df)
  }

  # Reorder columns
  order_first = c("type", "name", "eqn", "units", "label", "to", "from", "non_negative")

  # Get columns to prioritize (in order_first order)
  cols_first <- intersect(order_first, names(df))
  # Get remaining columns (in original order)
  cols_rest <- setdiff(names(df), order_first)
  # Combine columns (handles character(0) safely)
  new_cols <- c(cols_first, cols_rest)
  # Reorder data frame
  df <- df[, new_cols, drop = FALSE]

  # Make sure that for all columns, at least one row is not NA or empty
  # This is especially necessary when only interested in one type, e.g. macro or model_units

  # Convert empty strings to NA and keep columns with at least one non-NA
  df[] <- lapply(df, function(x) {
    x[x == ""] <- NA
    x
  })
  df <- df[, colSums(!is.na(df)) > 0, drop = FALSE]

  # Handle row.names if provided
  if (!is.null(row.names)) {
    if (length(row.names) != nrow(df)) {
      stop("Length of row.names (", length(row.names), ") does not match number of rows (", nrow(df), ")")
    }
    rownames(df) <- row.names
  }

  return(df)
}
