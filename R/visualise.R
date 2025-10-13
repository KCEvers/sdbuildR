#' Save plot to a file
#'
#' Save a plot of a stock-and-flow diagram or a simulation to a specified file path. Note that saving plots requires additional packages to be installed (see below).
#'
#' @param pl Plot object.
#' @param file File path to save plot to, including a file extension. For plotting a stock-and-flow model, the file extension can be one of png, pdf, svg, ps, eps, webp. For plotting a simulation, the file extension can be one of png, pdf, jpg, jpeg, webp. If no file extension is specified, it will default to png.
#' @param width Width of image in units.
#' @param height Height of image in units.
#' @param units Units in which width and heigth are specified. Either "cm", "in", or "px".
#' @param dpi Resolution of image. Only used if units is not "px".
#'
#' @returns Returns `NULL` invisibly.
#' @export
#' @family simulate
#'
#' @examples
#'
#' # Only if dependencies are installed
#' if (require("DiagrammeRsvg", quietly = TRUE) &
#'   require("rsvg", quietly = TRUE)) {
#'   sfm <- xmile("SIR")
#'   file <- tempfile(fileext = ".png")
#'   export_plot(plot(sfm), file)
#'
#'   # Remove plot
#'   file.remove(file)
#' }
#'
#' \dontrun{ # requires internet
#' # Only if dependencies are installed
#' if (require("htmlwidgets", quietly = TRUE) &
#'   require("webshot2", quietly = TRUE)) {
#'   #     # Requires Chrome to save plotly plot:
#'   sim <- simulate(sfm)
#'   export_plot(plot(sim), file)
#'
#'   # Remove plot
#'   file.remove(file)
#' }
#' }
export_plot <- function(pl, file, width = 3, height = 4, units = "cm", dpi = 300) {
  # Auto-detect format
  format <- tolower(tools::file_ext(file))

  # Convert dimensions to pixels
  if (units == "in") {
    width <- width * dpi
    height <- height * dpi
  } else if (units == "cm") {
    width <- width * dpi / 2.54
    height <- height * dpi / 2.54
  }

  if ("grViz" %in% class(pl)) {
    if (!nzchar(format)) {
      # stop("No file extension specified! Choose one of png, pdf, svg, ps, eps, webp.")
      # Default to png
      format <- "png"
      file <- paste0(file, ".", format)
    }

    export_diagram(pl, file, format,
      width = width, height = height
    )
  } else if ("plotly" %in% class(pl)) {
    if (!nzchar(format)) {
      # stop("No file extension specified! Choose one of png, pdf, jpg, jpeg, webp.")
      # Default to png
      format <- "png"
      file <- paste0(file, ".", format)
    }

    export_plotly(pl, file,
      format = format,
      width = width, height = height
    )
  } else {
    stop("export_plot does not support plot object of class ", class(pl))
  }

  return(invisible())
}


#' Export diagram
#'
#' @inheritParams export_plot
#' @param format Output format.
#'
#' @returns Returns `NULL` invisibly.
#' @noRd
#'
export_diagram <- function(pl, file, format, width, height) {
  if (!requireNamespace("rsvg", quietly = TRUE)) {
    stop("rsvg needs to be installed!")
  }

  if (!requireNamespace("DiagrammeRsvg", quietly = TRUE)) {
    stop("DiagrammeRsvg needs to be installed!")
  }

  temp <- charToRaw(DiagrammeRsvg::export_svg(pl))

  if (format == "webp") {
    rsvg::rsvg_webp(temp, file, width = width, height = height)
  } else if (format == "png") {
    rsvg::rsvg_png(temp, file, width = width, height = height)
  } else if (format == "pdf") {
    rsvg::rsvg_pdf(temp, file, width = width, height = height)
  } else if (format == "svg") {
    rsvg::rsvg_svg(temp, file, width = width, height = height)
  } else if (format == "ps") {
    rsvg::rsvg_ps(temp, file, width = width, height = height)
  } else if (format == "eps") {
    rsvg::rsvg_eps(temp, file, width = width, height = height)
  } else {
    stop("format ", format, " not supported")
  }

  return(invisible())
}



#' Export plotly object
#'
#' @inheritParams export_plot
#' @param format Output format.
#'
#' @returns Returns `NULL` invisibly.
#' @noRd
#'
export_plotly <- function(pl, file, format, width, height) {
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
    stop("htmlwidgets needs to be installed!")
  }

  if (!requireNamespace("webshot2", quietly = TRUE)) {
    stop("webshot2 needs to be installed!")
  }

  # Create temporary HTML file
  temp_html <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(pl, temp_html, selfcontained = TRUE)

  # Set webshot2 parameters based on format
  webshot_params <- list(
    url = temp_html,
    file = file,
    vwidth = width,
    vheight = height,
    delay = 1,
    quiet = TRUE # Doesn't seem to work
  )

  # Format-specific settings
  if (!format %in% c("jpg", "jpeg", "webp", "png", "pdf")) {
    warning("Format '", format, "' may not be supported by webshot2. Trying anyway...")
  }

  # Overwrite quiet option temporarily
  old_option <- getOption("webshot.quiet")
  options("webshot.quiet" = TRUE)
  on.exit({
    if (is.null(old_option)) {
      options("webshot.quiet" = NULL)
    } else {
      options("webshot.quiet" = old_option)
    }
  })

  # Convert to specified format
  do.call(webshot2::webshot, webshot_params)

  # Cleanup
  file.remove(temp_html)

  return(invisible())
}




#' Plot stock-and-flow diagram
#'
#' Visualize a stock-and-flow diagram using DiagrammeR. Stocks are represented as boxes. Flows are represented as arrows between stocks and/or double circles, where the latter represent what it outside of the model boundary. Thin grey edges indicate dependencies between variables. By default, constants (indicated by italic labels) are not shown. Hover over the variables to see their equations.
#'
#' @param x Stock-and-flow model of class sdbuildR_xmile
#' @param vars Variables to plot. Defaults to NULL to plot all variables.
#' @param format_label If TRUE, apply default formatting (removing periods and underscores) to labels if labels are the same as variable names.
#' @param wrap_width Width of text wrapping for labels. Must be an integer. Defaults to 20.
#' @param font_size Font size. Defaults to 18.
#' @param font_family Font name. Defaults to "Georgia".
#' @param stock_col Colour of stocks. Defaults to "#83d3d4".
#' @param flow_col Colour of flows. Defaults to "#f48153".
#' @param dependency_col Colour of dependency arrows. Defaults to "#999999".
#' @param show_dependencies If TRUE, show dependencies between variables. Defaults to TRUE.
#' @param show_constants If TRUE, show constants. Defaults to FALSE.
#' @param show_aux If TRUE, show auxiliary variables. Defaults to TRUE.
#' @param minlen Minimum length of edges; must be an integer. Defaults to 2.
#' @param ... Optional arguments
#'
#' @return Stock-and-flow diagram plotted with DiagrammeR()
#' @export
#' @family build
#' @method plot sdbuildR_xmile
#' @seealso [insightmaker_to_sfm()], [xmile()], [plot.sdbuildR_sim()]
#'
#' @examples
#' sfm <- xmile("SIR")
#' plot(sfm)
#'
#' # Don't show constants or auxiliaries
#' plot(sfm, show_constants = FALSE, show_aux = FALSE)
#'
#' # Only show specific variables
#' plot(sfm, vars = "Susceptible")
plot.sdbuildR_xmile <- function(x,
                                vars = NULL,
                                format_label = TRUE,
                                wrap_width = 20,
                                font_size = 18,
                                font_family = "Times New Roman",
                                stock_col = "#83d3d4",
                                flow_col = "#f48153",
                                dependency_col = "#999999",
                                show_dependencies = TRUE,
                                show_constants = FALSE,
                                show_aux = TRUE,
                                minlen = 2,
                                ...) {
  sfm <- x
  rm(x)
  check_xmile(sfm)

  # Get property dataframe
  df <- as.data.frame(sfm, properties = c("type", "name", "label", "eqn"))

  # Check whether there are any variables
  if (nrow(df) == 0) {
    stop("Your model contains no variables!")
  }

  # Get dependencies
  dep <- find_dependencies(sfm)
  flow_df <- get_flow_df(sfm)

  if (!is.null(vars)) {
    if (!is.character(vars)) {
      stop("vars must be a character vector!")
    }

    vars <- unique(vars)

    if (length(vars) == 0) {
      stop("vars cannot be of length zero!")
    }

    # Check whether specified variables are in the model
    idx <- !(vars %in% df[["name"]])
    if (any(idx)) {
      stop(paste0(
        paste0(vars[idx], collapse = ", "),
        ifelse(sum(idx) == 1, " is not a variable", " are not variables"),
        " in the model! Model variables: ",
        paste0(df[["name"]], collapse = ", ")
      ))
    }

    # # Add dependencies of vars
    # vars <- c(vars, unname(unlist(dep[vars])))
    #
    # # For all stocks in vars, also include their inflows and outflows,
    # # and the stocks that those inflows and outflows are connected to.
    # stock_vars <- vars[vars %in% df[df[["type"]] == "stock", "name"]]
    # if (length(stock_vars) > 0){
    #   idx <- flow_df[["to"]] %in% stock_vars | flow_df[["from"]] %in% stock_vars
    #   if (any(idx)){
    #     vars <- unique(c(vars, unname(unlist(flow_df[idx, ])) ))
    #   }
    # }

    # Only keep these variables in flow_df, dep, and df
    df <- df[df[["name"]] %in% vars, , drop = FALSE]
    dep <- dep[names(dep) %in% vars]
    flow_df <- flow_df[flow_df[["name"]] %in% vars, , drop = FALSE]

    # Set stocks not in vars to ''
    flow_df[["to"]][!flow_df[["to"]] %in% vars] <- ""
    flow_df[["from"]][!flow_df[["from"]] %in% vars] <- ""

    # Set show_aux to TRUE if any variables are aux
    if (any(vars %in% df[df[["type"]] == "aux", "name"])) {
      show_aux <- TRUE
    }

    # Set show_constants to TRUE if any variables are constants
    if (any(vars %in% df[df[["type"]] == "constant", "name"])) {
      show_constants <- TRUE
    }
  }

  if (format_label) {
    df[["label"]] <- ifelse(df[["name"]] == df[["label"]],
      stringr::str_replace_all(
        df[["label"]],
        c("_" = " ", "\\." = " ", "  " = " ")
      ), df[["label"]]
    )
  }

  # Text wrap to prevent long names
  df[["label"]] <- gsub("'", "\\\\'", df[["label"]])
  df[["label"]] <- stringr::str_wrap(df[["label"]], width = wrap_width)
  dict <- stats::setNames(df[["label"]], df[["name"]])

  # Get equations and remove quotation marks from unit strings
  dict_eqn <- stats::setNames(stringr::str_replace_all(
    df[["eqn"]],
    c("'" = "", "\"" = "")
  ), df[["name"]])

  # Categorize variables by type
  stock_names <- df[df[["type"]] == "stock", "name"]
  flow_names <- df[df[["type"]] == "flow", "name"]
  aux_names <- df[df[["type"]] == "aux", "name"]
  const_names <- df[df[["type"]] == "constant", "name"]

  # Filter based on show parameters
  if (!show_constants) const_names <- character(0)
  if (!show_aux) aux_names <- character(0)

  plot_var <- c(stock_names, flow_names)
  if (show_constants) {
    plot_var <- c(plot_var, const_names)
  }
  if (show_aux) {
    plot_var <- c(plot_var, aux_names)
  }

  # Prepare stock nodes
  if (length(stock_names) > 0) {
    stock_nodes <- sprintf(
      "%s [id=%s,label='%s',tooltip = 'eqn = %s',shape=box,style=filled,fillcolor='%s',fontsize=%s,fontname='%s']",
      paste0("'", stock_names, "'"),
      paste0("'", stock_names, "'"),
      dict[stock_names],
      dict_eqn[stock_names],
      stock_col, font_size, font_family
    )
  } else {
    stock_nodes <- ""
  }

  # Prepare auxiliary nodes
  if (length(aux_names) > 0) {
    aux_nodes <- sprintf(
      "%s [id=%s,label='%s',tooltip = 'eqn = %s',shape=plaintext,fontsize=%s,fontname='%s', width=0.6, height=0.3]",
      paste0("'", aux_names, "'"),
      paste0("'", aux_names, "'"),
      dict[aux_names],
      dict_eqn[aux_names],
      font_size - 2, font_family
    )
  } else {
    aux_nodes <- ""
  }

  # Prepare constant nodes (italic font)
  if (length(const_names) > 0) {
    # Format labels: convert \n to <BR/> and add italics
    formatted_labels <- vapply(dict[const_names], function(label) {
      label_with_html_breaks <- gsub("\n", "<BR/>", label, fixed = TRUE)
      paste0("<I>", label_with_html_breaks, "</I>")
    }, character(1), USE.NAMES = FALSE)

    const_nodes <- sprintf(
      "%s [id=%s,label=<%s>, tooltip = 'eqn = %s',
                         shape=plaintext,fontsize=%s,fontname='%s',
                         width=0.6, height=0.3]",
      paste0("'", const_names, "'"),
      paste0("'", const_names, "'"),
      formatted_labels,
      dict_eqn[const_names],
      font_size - 2, font_family
    )
  } else {
    const_nodes <- ""
  }


  # Create rank groupings based on dependencies
  rank_groups <- list()

  # Reverse dependencies, because we want to couple constants/aux with the first variable that depends on them
  dep_rev <- reverse_dep(dep)

  # Helper function
  get_first_dep <- function(var_names, dep_rev, plot_var) {
    if (length(var_names) == 0) {
      return(list())
    }

    dep_var <- dep_rev[var_names]

    # Only keep those in plot_var
    dep_var <- lapply(dep_var, function(x) {
      intersect(x, plot_var)
    })

    dep_var <- dep_var[lengths(dep_var) != 0]

    if (length(dep_var) > 0) {
      lapply(dep_var, `[[`, 1)
    } else {
      list()
    }
  }

  if (length(const_names) > 0) {
    dep_var <- get_first_dep(const_names, dep_rev, plot_var)
    rank_groups <- append(rank_groups, dep_var)
  }

  if (length(aux_names) > 0) {
    dep_var <- get_first_dep(aux_names, dep_rev, plot_var)
    rank_groups <- append(rank_groups, dep_var)
  }


  # Create rank statements
  rank_statements <- ""
  if (length(rank_groups) > 0) {
    rank_statements <- vapply(names(rank_groups), function(rank_node) {
      vars_in_rank <- rank_groups[[rank_node]]
      sprintf(
        "      {rank=same; %s }",
        paste0("'", c(rank_node, vars_in_rank), "'", collapse = "; ")
      )
    }, character(1), USE.NAMES = FALSE)
    rank_statements <- paste(rank_statements, collapse = "\n")
  }

  cloud_nodes <- flow_edges <- flow_nodes <- dependency_edges <- legend_nodes <- legend_edges <- ""

  if (length(flow_names) > 0) {
    # # Create dataframe with direction of flows
    # flow_df <- get_flow_df(sfm)

    # If the flow is to a stock that doesn't exist, remove
    flow_df[["from"]] <- ifelse(flow_df[["from"]] %in% stock_names,
      flow_df[["from"]], ""
    )
    flow_df[["to"]] <- ifelse(flow_df[["to"]] %in% stock_names,
      flow_df[["to"]], ""
    )
    flow_df <- as.matrix(flow_df)

    # Fill in NA in flow_df with numbered clouds
    idxs <- which(flow_df == "")
    cloud_names <- character(0)
    if (length(idxs) > 0) {
      cloud_names <- paste0("Cloud", seq_along(idxs))
      flow_df[idxs] <- cloud_names

      # Find whether cloud is a source or a sink
      # flow_df has three columns; idxs are row-based
      labels <- cloud_names
      labels[idxs <= (nrow(flow_df) * 2)] <- "Unspecified source"
      labels[idxs > (nrow(flow_df) * 2)] <- "Unspecified sink"

      # External environment is represented as a cloud
      cloud_nodes <- sprintf(
        "%s [label='', tooltip = %s,shape=doublecircle, fixedsize=true, width = .25, height = .25, orientation=15]",
        paste0("'", cloud_names, "'"),
        paste0("'", labels, "'")
      )
    }

    # Create intermediate flow nodes (small nodes that flows pass through)
    flow_nodes <- sprintf(
      "%s [id=%s,label='%s', tooltip = 'eqn = %s', shape = plaintext, fontsize=%s, fontname='%s', width=0.6, height=0.3]",
      paste0("'", flow_names, "'"),
      paste0("'", flow_names, "'"),
      dict[flow_names],
      dict_eqn[flow_names],
      font_size - 2,
      font_family
    )

    # Create edges: from -> flow_node -> to
    flow_edges <- c()
    for (i in seq_len(nrow(flow_df))) {
      flow_name <- flow_df[i, "name"]
      flow_node <- flow_name
      from_node <- flow_df[i, "from"]
      to_node <- flow_df[i, "to"]

      # Edge from source to flow node
      flow_edges <- c(flow_edges, sprintf(
        "%s -> %s [arrowhead='none', color='%s', penwidth=1.1, minlen=%s, splines=false]",
        paste0("'", from_node, "'"),
        paste0("'", flow_node, "'"),
        paste0(
          "black:",
          # flow_col,":",
          flow_col, ":black"
        ),
        minlen
      ))

      # Edge from flow node to destination
      flow_edges <- c(flow_edges, sprintf(
        "%s -> %s [arrowhead='normal', color='%s', arrowsize=1.5, penwidth=1.1, minlen=%s, splines=ortho]",
        paste0("'", flow_node, "'"),
        paste0("'", to_node, "'"),
        paste0(
          "black:",
          # flow_col,":",
          flow_col, ":black"
        ),
        minlen
      ))
    }
  }

  # Add dependency arrows if requested
  if (show_dependencies) {
    # Only keep dependencies in plot_var
    dep <- lapply(dep, function(x) {
      intersect(x, plot_var)
    })

    # Only keep entries in plot_var
    dep <- dep[names(dep) %in% plot_var]

    if (length(dep) > 0) {
      dependency_edges <- unlist(lapply(names(dep), function(x) {
        if (length(dep[[x]]) > 0) {
          vapply(dep[[x]], function(y) {
            sprintf(
              "%s -> %s [color='%s', arrowsize=0.8, penwidth=1, splines=true, constraint=false, tailport='_']",
              paste0("'", y, "'"),
              paste0("'", x, "'"),
              dependency_col
            )
          }, character(1), USE.NAMES = FALSE)
        }
      }))
    }
  }

  # Compile string for diagram
  viz_str <- sprintf(
    "
    digraph sfm {

      graph [layout = dot, rankdir = LR, center=true, outputorder='edgesfirst', pad=.1, nodesep= .3]

      # Rank groupings
      %s

      # Define stock nodes
      %s

      # Define flow nodes (intermediate nodes for flows)
      %s

      # Define external cloud nodes
      %s

      # Define flow edges (stock -> flow_node -> stock)
      %s

      # Define dependency edges
      %s

      # Define auxiliary nodes
      %s

      # Define constant nodes
      %s
    }
          ",
    rank_statements,
    stock_nodes |> rev() |> paste0(collapse = "\n\t\t"),
    flow_nodes |> paste0(collapse = "\n\t\t"),
    cloud_nodes |> paste0(collapse = "\n\t\t"),
    flow_edges |> paste0(collapse = "\n\t\t"),
    dependency_edges |> paste0(collapse = "\n\t\t"),
    aux_nodes |> paste0(collapse = "\n\t\t"),
    const_nodes |> paste0(collapse = "\n\t\t")
  )

  pl <- DiagrammeR::grViz(viz_str)

  return(pl)
}



#' Prepare for plotting simulation
#'
#' @param type_sim Either "sim" or "ensemble"
#' @param df Dataframe to plot
#' @param constants Constants to plot
#' @inheritParams plot.sdbuildR_sim
#' @inheritParams build
#'
#' @returns List
#' @noRd
#'
prep_plot <- function(sfm, type_sim, df, constants, add_constants, vars, palette, colors, wrap_width) {
  # Get names of stocks and non-stock variables
  names_df <- get_names(sfm)

  if (!is.null(vars)) {
    if (!is.character(vars)) {
      stop("vars must be a character vector!")
    }

    vars <- unique(vars)

    if (length(vars) == 0) {
      stop("vars cannot be of length zero!")
    }
  }

  # If vars is specified and it contains a constant, set add_constants = TRUE
  if (!is.null(vars)) {
    constant_names <- names_df[
      names_df[["type"]] %in% c("constant", "gf"),
      "name"
    ]
    vars_constants <- intersect(constant_names, vars)
    constants_not_in_vars <- setdiff(constant_names, vars_constants)

    # Overwrite
    add_constants <- length(vars_constants) > 0

    # If not all constants should be added, only select those in vars
    if (add_constants) {
      # Remove constants not in vars
      names_df <- names_df[!(names_df[["name"]] %in% constants_not_in_vars), ,
        drop = FALSE
      ]

      if (type_sim == "sim") {
        constants <- constants[vars_constants]
      } else if (type_sim == "ensemble") {
        constants <- constants[!(constants[["variable"]] %in% constants_not_in_vars), ,
          drop = FALSE
        ]
      }
    }
  }

  # Add constants
  if (add_constants) {
    if (length(constants) > 0) {
      if (type_sim == "sim") {
        # Ensure functions are not added
        idx_func <- vapply(constants, is.function, logical(1), USE.NAMES = FALSE)
        constants <- constants[!idx_func]

        # Remove from names
        names_df <- names_df[!names_df[["name"]] %in% names(idx_func[idx_func]), ,
          drop = FALSE
        ]

        # Duplicate long format for each constant
        if (length(constants) > 0) {
          # Find time vector from first variable
          times <- df[df[["variable"]] == df[["variable"]][1], "time"]
          temp <- lapply(names(constants), function(y) {
            data.frame(
              time = times, variable = y,
              value = constants[[y]]
            )
          }) |>
            do.call(rbind, args = _) |>
            as.data.frame()
          df <- dplyr::bind_rows(df, temp)
          rm(temp)
        }
      } else if (type_sim == "ensemble") {
        # Find time vector from first variable
        times <- df[df[["variable"]] == df[["variable"]][1], "time"]

        # Duplicate each row length(times) times
        temp <- constants[rep(seq_len(nrow(constants)), each = length(times)), ]

        # Add the times column
        temp$time <- rep(times, nrow(constants))

        # Clean up row names
        rownames(temp) <- NULL

        df <- dplyr::bind_rows(df, temp)
        rm(temp)
      }
    }
  }


  # Keep only specified variables
  if (!is.null(vars)) {
    # Check whether specified variables are in the model
    idx <- !(vars %in% names_df[["name"]])
    if (any(idx)) {
      stop(paste0(
        paste0(vars[idx], collapse = ", "),
        ifelse(sum(idx) == 1, " is not a variable", " are not variables"),
        " in the model! Model variables: ",
        paste0(names_df[["name"]], collapse = ", ")
      ))
    }

    # Check if variables are in the model but not in the dataframe
    idx <- !(vars %in% df[["variable"]])
    if (any(idx)) {
      stop(paste0(
        paste0(vars[idx], collapse = ", "),
        ifelse(sum(idx) == 1, " is", " are"),
        " in the model, but not in the simulated dataframe. Run simulate() with only_stocks = FALSE."
      ))
    }

    names_df <- names_df[match(vars, names_df[["name"]]), , drop = FALSE]
    df <- df[df[["variable"]] %in% vars, , drop = FALSE]
    highlight_these_names <- vars
  } else {
    # If no vars were specified, highlight stocks
    highlight_these_names <- names_df[names_df[["type"]] == "stock", "name"]
  }

  # Check labels are unique
  if (nrow(names_df) != length(unique(names_df[["label"]]))) {
    labels <- names_df[["label"]]
    dup_indices <- which(labels %in% labels[duplicated(labels) |
      duplicated(labels, fromLast = TRUE)])

    # Relabel, otherwise plotting will go wrong with recoded variables
    names_df[dup_indices, "label"] <- paste0(
      names_df[dup_indices, "label"], "(",
      names_df[dup_indices, "name"], ")"
    )
  }


  # Ensure only variables which are in the dataframe are included
  names_df <- names_df[names_df[["name"]] %in% unique(df[["variable"]]), ,
    drop = FALSE
  ]

  # Create dictionary with stock and non-stock names and labels
  highlight_names <- names_df[match(highlight_these_names, names_df[["name"]]), , drop = FALSE]
  highlight_names <- stats::setNames(highlight_names[["name"]], highlight_names[["label"]])
  nonhighlight_names <- names_df[!names_df[["name"]] %in% highlight_these_names, , drop = FALSE]
  nonhighlight_names <- stats::setNames(nonhighlight_names[["name"]], nonhighlight_names[["label"]])

  # Wrap names to prevent long names from squishing the plot
  names(highlight_names) <- stringr::str_wrap(names(highlight_names), width = wrap_width)
  names(nonhighlight_names) <- stringr::str_wrap(names(nonhighlight_names), width = wrap_width)

  # Split dataframe into stocks and non-stocks
  df_highlight <- df[df[["variable"]] %in% unname(highlight_names), , drop = FALSE]
  df_nonhighlight <- df[df[["variable"]] %in% unname(nonhighlight_names), , drop = FALSE]

  # Change labels of variables
  df_highlight[["variable"]] <- factor(
    df_highlight[["variable"]],
    levels = unname(highlight_names),
    labels = names(highlight_names)
  )
  df_nonhighlight[["variable"]] <- factor(
    df_nonhighlight[["variable"]],
    levels = unname(nonhighlight_names),
    labels = names(nonhighlight_names)
  )

  # Create colours
  nr_var <- length(unique(df[["variable"]]))

  gen_colors <- FALSE
  if (!is.null(colors)) {
    if (length(colors) < nr_var) {
      stop(paste0("Length of colors (", length(colors), ") must be equal to the number of variables in the simulation dataframe (", nr_var, ").\nUsing palette instead..."))
      gen_colors <- TRUE
    }
  } else {
    gen_colors <- TRUE
  }

  if (gen_colors) {
    # Minimum number of variables needed for color palette generation
    if (nr_var < 3) {
      nr_var_c <- 3
    } else {
      nr_var_c <- nr_var
    }

    colors <- grDevices::hcl.colors(n = nr_var_c, palette = palette)
  }

  # Cut number of colors to number of variables
  colors <- colors[seq_len(nr_var)]

  return(list(
    highlight_names = highlight_names,
    nonhighlight_names = nonhighlight_names,
    df_highlight = df_highlight,
    df_nonhighlight = df_nonhighlight,
    colors = colors
  ))
}



#' Plot timeseries of simulation
#'
#' Visualize simulation results of a stock-and-flow model. Plot the evolution of stocks over time, with the option of also showing other model variables.
#'
#' @param x Output of simulate().
#' @param add_constants If TRUE, include constants in plot. Defaults to FALSE.
#' @param vars Variables to plot. Defaults to NULL to plot all variables.
#' @param palette Colour palette. Must be one of hcl.pals().
#' @param colors Vector of colours. If NULL, the color palette will be used. If specified, will override palette. The number of colours must be equal to the number of variables in the simulation dataframe. Defaults to NULL.
#' @param font_family Font family. Defaults to "Times New Roman".
#' @param font_size Font size. Defaults to 16.
#' @param wrap_width Width of text wrapping for labels. Must be an integer. Defaults to 25.
#' @param showlegend Whether to show legend. Must be TRUE or FALSE. Defaults to TRUE.
#' @param ... Optional parameters
#'
#' @return Plot object
#' @export
#' @family simulate
#' @seealso [simulate()], [as.data.frame.sdbuildR_sim()], [plot.sdbuildR_xmile()]
#' @method plot sdbuildR_sim
#'
#' @examples
#' sfm <- xmile("SIR")
#' sim <- simulate(sfm)
#' plot(sim)
#'
#' # The default plot title and axis labels can be changed like so:
#' plot(sim, main = "Simulated trajectory", xlab = "Time", ylab = "Value")
#'
#' # Add constants to the plot
#' plot(sim, add_constants = TRUE)
#'
#' # Plot all model variables
#' sim <- simulate(sfm, only_stocks = FALSE)
#' plot(sim, add_constants = TRUE)
#'
plot.sdbuildR_sim <- function(x,
                              add_constants = FALSE,
                              vars = NULL,
                              palette = "Dark 2",
                              colors = NULL,
                              font_family = "Times New Roman",
                              font_size = 16,
                              wrap_width = 25,
                              showlegend = TRUE,
                              ...) {
  if (missing(x)) {
    stop("No simulation data provided! Use simulate() to run a simulation.")
  }

  # Check whether it is an xmile object
  if (!inherits(x, "sdbuildR_sim")) {
    stop("This is not an object of class sdbuildR_sim! Simulate a stock-and-flow model with simulate().")
  }

  if (x[["success"]] == FALSE) {
    stop("Simulation failed!")
  }

  if (nrow(x[["df"]]) == 0) {
    stop("Dataframe has no rows!")
  }

  if (!is.logical(showlegend)) {
    stop("showlegend must be TRUE or FALSE!")
  }

  dots <- list(...)
  main <- if (!"main" %in% names(dots)) {
    x[["sfm"]][["header"]][["name"]]
  } else {
    dots[["main"]]
  }

  xlab <- if (!"xlab" %in% names(dots)) {
    matched_time_unit <- find_matching_regex(x[["sfm"]][["sim_specs"]][["time_units"]], get_regex_time_units())
    paste0("Time (", matched_time_unit, ")")
  } else {
    dots[["xlab"]]
  }

  ylab <- if (!"ylab" %in% names(dots)) {
    ""
  } else {
    dots[["ylab"]]
  }

  out <- prep_plot(
    x[["sfm"]], "sim", x[["df"]], x[["constants"]], add_constants,
    vars, palette, colors, wrap_width
  )
  highlight_names <- out[["highlight_names"]]
  nonhighlight_names <- out[["nonhighlight_names"]]
  df_highlight <- out[["df_highlight"]]
  df_nonhighlight <- out[["df_nonhighlight"]]
  colors <- out[["colors"]]

  x_col <- "time"

  # Initialize plotly object
  pl <- plotly::plot_ly()

  # Add traces for non-stock variables (visible = "legendonly")
  if (length(nonhighlight_names) > 0) {
    pl <- plotly::add_trace(pl,
      data = df_nonhighlight,
      x = ~ get(x_col),
      y = ~value,
      color = ~variable,
      legendgroup = ~variable,
      showlegend = showlegend,
      colors = colors,
      type = "scatter",
      mode = "lines",
      visible = "legendonly"
    )
  }

  # Add traces for stock variables (visible = TRUE)
  if (length(highlight_names) > 0) {
    pl <- plotly::add_trace(pl,
      data = df_highlight,
      x = ~ get(x_col),
      y = ~value,
      color = ~variable,
      legendgroup = ~variable,
      showlegend = showlegend,
      type = "scatter",
      mode = "lines",
      colors = colors,
      visible = TRUE
    )
  }

  # Customize layout
  pl <- plotly::layout(pl,
    # As the most important things are at the top, reverse the trace order
    legend = list(
      traceorder = "reversed",
      font = list(size = ceiling(font_size * .85))
    ),
    title = main,
    xaxis = list(title = xlab, font = list(size = font_size)),
    yaxis = list(title = ylab, font = list(size = font_size)),
    font = list(family = font_family, size = font_size),
    margin = list(t = 50, b = 50, l = 50, r = 50) # Increase top margin to 100 pixels
  )

  # If there is only one trace, legend doesn't show
  if (showlegend & (length(highlight_names) + length(nonhighlight_names)) == 1) {
    pl <- plotly::layout(pl, showlegend = TRUE)
  }

  # Set x-axis limits if specified
  if ("xlim" %in% names(dots)) {
    pl <- plotly::layout(pl,
      xaxis = list(range = dots[["xlim"]])
    )
  }

  if ("ylim" %in% names(dots)) {
    pl <- plotly::layout(pl,
      yaxis = list(range = dots[["ylim"]])
    )
  }


  return(pl)
}





#' Plot timeseries of ensemble
#'
#' Visualize ensemble simulation results of a stock-and-flow model. Either summary statistics or individual trajectories can be plotted. When multiple conditions j are specified, a grid of subplots is plotted. See `ensemble()` for examples.
#'
#' @param x Output of ensemble().
#' @param type Type of plot. Either "summary" for a summary plot with mean or median lines and confidence intervals, or "sims" for individual simulation trajectories with mean or median lines. Defaults to "summary".
#' @param i Indices of the individual trajectories to plot if type = "sims". Defaults to 1:10. Including a high number of trajectories will slow down plotting considerably.
#' @param j Indices of the condition to plot. Defaults to 1:9. If only one condition is specified, the plot will not be a grid of subplots.
#' @param nrows Number of rows in the plot grid. Defaults to ceiling(sqrt(n_conditions)).
#' @param shareX If TRUE, share the x-axis across subplots. Defaults to TRUE.
#' @param shareY If TRUE, share the y-axis across subplots. Defaults to TRUE.
#' @param palette Colour palette. Must be one of hcl.pals().
#' @param colors Vector of colours. If NULL, the color palette will be used. If specified, will override palette. The number of colours must be equal to the number of variables in the simulation dataframe. Defaults to NULL.
#' @param font_family Font family. Defaults to "Times New Roman".
#' @param font_size Font size. Defaults to 16.
#' @param wrap_width Width of text wrapping for labels. Must be an integer. Defaults to 25.
#' @param showlegend Whether to show legend. Must be TRUE or FALSE. Defaults to TRUE.
#' @param j_labels Whether to plot labels indicating the condition of the subplot.
#' @param central_tendency Central tendency to use for the mean line. Either "mean", "median", or FALSE to not plot the central tendency. Defaults to "mean".
#' @param central_tendency_width Line width of central tendency. Defaults to 3.
#' @param ... Optional parameters
#' @inheritParams plot.sdbuildR_sim
#'
#' @return Plot object with ensemble simulation
#' @export
#' @family simulate
#' @seealso [ensemble()]
#' @method plot sdbuildR_ensemble
#'
plot.sdbuildR_ensemble <- function(x,
                                   type = c("summary", "sims")[1],
                                   i = seq(1, min(c(x[["n"]], 10))),
                                   j = seq(1, min(c(x[["n_conditions"]], 9))),
                                   vars = NULL,
                                   add_constants = FALSE,
                                   nrows = ceiling(sqrt(max(j))),
                                   shareX = TRUE,
                                   shareY = TRUE,
                                   palette = "Dark 2",
                                   colors = NULL,
                                   font_family = "Times New Roman",
                                   font_size = 16,
                                   wrap_width = 25,
                                   showlegend = TRUE,
                                   j_labels = TRUE,
                                   central_tendency = c("mean", "median", FALSE)[1],
                                   central_tendency_width = 3,
                                   ...) {
  if (missing(x)) {
    stop("No simulation data provided! Use simulate() to run a simulation.")
  }

  # Check whether it is an xmile object
  if (!inherits(x, "sdbuildR_ensemble")) {
    stop("This is not an object of class sdbuildR_ensemble! Generate an ensemble of simulations with ensemble().")
  }

  if (x[["success"]] == FALSE) {
    stop("Ensemble simulation failed!")
  }

  if (!is.logical(showlegend)) {
    stop("showlegend must be TRUE or FALSE!")
  }

  if (!is.logical(j_labels)) {
    stop("j_labels must be TRUE or FALSE!")
  }

  # Check type
  type <- trimws(tolower(type))
  type <- ifelse(type == "sim", "sims", type)
  if (!type %in% c("summary", "sims")) {
    stop("type must be one of 'summary' or 'sims'.")
  }

  # Check central tendency
  if (!isFALSE(central_tendency)) {
    central_tendency <- trimws(tolower(central_tendency))
    if (!central_tendency %in% c("mean", "median")) {
      stop("central_tendency must be 'mean', 'median', or FALSE.")
    }
  }

  # Get passed arguments
  passed_arg <- names(as.list(match.call())[-1])

  dots <- list(...)

  sub <- if (!"sub" %in% names(dots)) {
    if (type == "summary") {
      sub <- paste0(
        ifelse(isFALSE(central_tendency), "",
          stringr::str_to_title(central_tendency)
        ), " with [",
        min(x[["quantiles"]]), ", ", max(x[["quantiles"]]),
        "] confidence interval of ", x[["n"]], " simulation",
        ifelse(x[["n"]] == 1, "", "s")
      )
    } else if (type == "sims") {
      sub <- paste0(
        ifelse(isFALSE(central_tendency), "",
          stringr::str_to_title(central_tendency)
        ),
        " with ", length(i), "/", x[["n"]], " simulation",
        ifelse(x[["n"]] == 1, "", "s")
      )
    }
  }


  main <- if (!"main" %in% names(dots)) {
    paste0("Ensemble of ", x[["sfm"]][["header"]][["name"]])
  } else {
    dots[["main"]]
  }
  main <- paste0(main, "\n<span style='font-size:", font_size, "px;'>", sub, "</span>")

  xlab <- if (!"xlab" %in% names(dots)) {
    matched_time_unit <- find_matching_regex(x[["sfm"]][["sim_specs"]][["time_units"]], get_regex_time_units())
    paste0("Time (", matched_time_unit, ")")
  } else {
    dots[["xlab"]]
  }

  ylab <- if (!"ylab" %in% names(dots)) {
    ""
  } else {
    dots[["ylab"]]
  }

  alpha <- if (!"alpha" %in% names(dots)) {
    0.3
  } else {
    dots[["alpha"]]
  }

  if (!is.null(x[["summary"]])) {
    summary_df <- x[["summary"]]
  } else {
    stop("No summary data available!")
  }

  # Check if j is a valid index
  if ("j" %in% passed_arg) {
    if (length(j) == 0) {
      stop("j must be a non-empty vector of indices.")
    }

    if (is.numeric(j)) {
      if (any(j < 1 | j > x[["n_conditions"]])) {
        if (x[["n_conditions"]] == 1) {
          stop(paste0("There is only one condition. Set j = 1."))
        } else {
          stop(paste0("j must be a vector with integers between 1 and ", x[["n_conditions"]], "."))
        }
      }
    } else {
      stop("j must be a numeric vector.")
    }
  }

  # Ensure there aren't more rows than j
  nrows <- min(nrows, length(j))
  ncols <- ceiling(length(j) / nrows)

  # Whether to create subplots or not
  create_subplots <- length(j) > 1

  # To plot individual simulation trajectories, extract df
  if (type == "sims") {
    if (!is.null(x[["df"]])) {
      df <- x[["df"]]

      # Check if i is a valid index
      if ("i" %in% passed_arg) {
        if (length(i) == 0) {
          stop("i must be a non-empty vector of indices.")
        }

        if (is.numeric(i)) {
          if (any(i < 1 | i > x[["n"]])) {
            if (x[["n"]] == 1) {
              stop(paste0("There is only one simulation. Set i = 1."))
            } else {
              stop(paste0("i must be a vector with integers between 1 and ", x[["n"]], "."))
            }
          }
        } else {
          stop("i must be a numeric vector.")
        }
      }

      # Filter condition
      df <- df[df[["i"]] %in% i, , drop = FALSE]
    } else {
      stop("No simulation data available! Run ensemble() with return_sims = TRUE.")
    }
  } else if (type == "summary") {
    if ("i" %in% passed_arg) {
      message("i is not used when type = 'summary'. Set type = 'sims' to plot individual trajectories.")
    }

    df <- NULL
  }

  # Prepare for plotting
  out <- prep_plot(x[["sfm"]], "ensemble", summary_df,
    constants = x[["constants"]][["summary"]], add_constants = add_constants,
    vars = vars, palette = palette, colors = colors, wrap_width = wrap_width
  )
  highlight_names <- out[["highlight_names"]]
  nonhighlight_names <- out[["nonhighlight_names"]]
  summary_df_highlight <- out[["df_highlight"]]
  summary_df_nonhighlight <- out[["df_nonhighlight"]]
  colors <- out[["colors"]]

  if (type == "sims") {
    out <- prep_plot(x[["sfm"]], "ensemble", df,
      constants = x[["constants"]][["df"]], add_constants = add_constants,
      vars = vars, palette = palette, colors = colors, wrap_width = wrap_width
    )
    df_highlight <- out[["df_highlight"]]
    df_nonhighlight <- out[["df_nonhighlight"]]
  } else {
    df_highlight <- df_nonhighlight <- NULL
  }

  # Find qlow and qhigh
  q_cols <- colnames(summary_df)[grepl("^q", colnames(summary_df))]
  q_num <- as.numeric(gsub("^q", "", q_cols))
  q_low <- q_cols[which.min(q_num)]
  q_high <- q_cols[which.max(q_num)]

  # Check whether there are multiple time points
  mode <- ifelse(length(unique(summary_df[["time"]])) == 1, "markers", "lines")


  # Plot
  if (!create_subplots) {
    j_idx <- 1
    j_name <- j[j_idx]
    pl <- plot_ensemble_helper(
      j_idx = j_idx,
      j_name = j_name, j = j, j_labels = j_labels,
      type = type,
      create_subplots = create_subplots,
      summary_df_highlight = summary_df_highlight,
      summary_df_nonhighlight = summary_df_nonhighlight,
      df_highlight = df_highlight,
      df_nonhighlight = df_nonhighlight,
      central_tendency = central_tendency,
      central_tendency_width = central_tendency_width,
      q_low = q_low,
      q_high = q_high,
      mode = mode,
      colors = colors,
      showlegend = showlegend,
      dots = dots,
      main = main,
      xlab = xlab, ylab = ylab,
      font_family = font_family,
      font_size = font_size,
      alpha = alpha
    )
  } else {
    # Create a list of plotly objects for each condition
    pl_list <- list()
    for (j_idx in seq_along(j)) {
      j_name <- j[j_idx]

      pl_list[[j_idx]] <- plot_ensemble_helper(
        j_idx = j_idx,
        j_name = j_name, j = j, j_labels = j_labels,
        type = type,
        create_subplots = create_subplots,
        summary_df_highlight = summary_df_highlight[summary_df_highlight[["j"]] == j_name, , drop = FALSE],
        summary_df_nonhighlight = summary_df_nonhighlight[summary_df_nonhighlight[["j"]] == j_name, , drop = FALSE],
        df_highlight = df_highlight[df_highlight[["j"]] == j_name, , drop = FALSE],
        df_nonhighlight = df_nonhighlight[df_nonhighlight[["j"]] == j_name, , drop = FALSE],
        central_tendency = central_tendency,
        central_tendency_width = central_tendency_width,
        q_low = q_low,
        q_high = q_high,
        mode = mode,
        colors = colors,
        showlegend = showlegend,
        dots = dots,
        main = main,
        xlab = xlab, ylab = ylab,
        font_family = font_family,
        font_size = font_size,
        alpha = alpha
      )
    }

    pl <- plotly::subplot(pl_list,
      nrows = nrows,
      shareX = shareX,
      shareY = shareY,
      titleY = FALSE,
      titleX = FALSE
      # margin sets vertical spacing between subplots
      # margin = 0.05
    ) |>
      plotly::layout(
        title = list(text = main), # , font = list(size = font_size)),
        # xaxis = list(title = list(text=xlab, standoff = 15), position = 0.5),
        yaxis = list(title = list(text = ylab)),
        font = list(family = font_family, size = font_size),
        margin = list(t = 100, b = 50, l = 50, r = 50), # Increase top margin to 100 pixels
        legend = list(
          orientation = "h", # orientation
          x = 0.5, # Center horizontally
          y = -0.2, # Below the plot
          xanchor = "center",
          yanchor = "top"
        )
      )

    if (nzchar(xlab)) {
      pl <- pl |>
        plotly::layout(
          annotations = list(
            list(
              text = xlab,
              font = list(
                family = font_family
                # size = font_size
              ),
              xref = "paper",
              yref = "paper",
              xanchor = "center", yanchor = "top",
              x = 0.5, y = -.1,
              showarrow = FALSE
            )
          )
        )
    }
  }

  return(pl)
}



#' Helper function to plot ensemble simulation results
#'
#' @param j_idx Index of the condition to plot.
#' @param j_name Name of the condition to plot.
#' @param j Index of the condition to plot. Used to determine whether to show the legend.
#' @param type Type of plot. Must be one of "summary" or "sims". Defaults to "summary". If "summary", the plot will show the mean and confidence intervals of the simulation results. If "sims", the plot will show all individual simulation runs in i.
#' @param create_subplots If TRUE, create subplots for each condition. If FALSE, plot all conditions in one plot.
#' @param summary_df_highlight Dataframe with summary statistics of the ensemble simulation results (stocks). Must contain columns "j", "variable", "mean", and confidence interval columns (e.g., "q0.025", "q0.975").
#' @param summary_df_nonhighlight Dataframe with summary statistics of the ensemble simulation results (non-stocks). Must contain columns "j", "variable", "mean", and confidence interval columns (e.g., "q0.025", "q0.975").
#' @param df_highlight Dataframe with individual simulation results (stocks). Must contain columns "i", "j", "variable", and "value". Only used if type = "sims".
#' @param df_nonhighlight Dataframe with individual simulation results (non-stocks). Must contain columns "i", "j", "variable", and "value". Only used if type = "sims".
#' @param q_low Column name for the lower bound of the confidence interval (e.g., "q0.025").
#' @param q_high Column name for the upper bound of the confidence interval (e.g., "q0.975").
#' @param mode Plotting mode. Either "lines" if there are multiple time points or "markers" for a single time point.
#' @param colors Vector of colours. If NULL, the color palette will be used. If specified, will override palette. The number of colours must be equal to the number of variables in the simulation dataframe. Defaults to NULL.
#' @param dots List of additional parameters passed to the plotly functions.
#' @param main Main title of the plot. Defaults to the name of the stock-and-flow model and the number of simulations.
#' @param xlab Label on x-axis.
#' @param ylab Label on y-axis.
#' @param alpha Opacity of the confidence bands or individual trajectories. Defaults to 0.3.
#' @inheritParams plot.sdbuildR_ensemble
#'
#' @returns Plotly object
#' @noRd
plot_ensemble_helper <- function(j_idx, j_name, j, j_labels,
                                 type, create_subplots,
                                 summary_df_highlight,
                                 summary_df_nonhighlight,
                                 df_highlight,
                                 df_nonhighlight,
                                 central_tendency,
                                 central_tendency_width,
                                 q_low, q_high,
                                 mode,
                                 colors, showlegend, dots,
                                 main, xlab, ylab,
                                 font_family, font_size, alpha) {
  # Only show legend if it's the last subplot
  showlegend <- ifelse(j_name != max(j), FALSE, showlegend)
  x_col <- "time"

  plot_highlight <- nrow(summary_df_highlight) > 0
  plot_nonhighlight <- nrow(summary_df_nonhighlight) > 0
  nr_var <- length(colors)

  # Initialize plotly object
  pl <- plotly::plot_ly()

  if (type == "summary") {
    if (mode == "lines") {
      if (plot_nonhighlight) {
        pl <- plotly::add_ribbons(pl,
          data = summary_df_nonhighlight,
          x = ~ get(x_col),
          ymin = ~ get(q_low),
          ymax = ~ get(q_low),
          color = ~variable,
          legendgroup = ~variable,
          fillcolor = ~variable,
          opacity = alpha,
          type = "scatter",
          mode = mode,
          colors = colors,
          # Add traces for non-stock variables (visible = "legendonly")
          showlegend = showlegend,
          visible = "legendonly"
        )
      }

      # First plot confidence bands
      if (plot_highlight) {
        pl <- plotly::add_ribbons(pl,
          data = summary_df_highlight,
          x = ~ get(x_col),
          ymin = ~ get(q_low),
          ymax = ~ get(q_high),
          color = ~variable,
          legendgroup = ~variable,
          fillcolor = ~variable,
          opacity = alpha,
          type = "scatter",
          mode = mode,
          colors = colors,
          showlegend = showlegend,
          # Add traces for stock variables (visible = TRUE)
          visible = TRUE
        )
      }
    }
  } else if (type == "sims") {
    # Add traces for non-stock variables (visible = "legendonly")
    if (plot_nonhighlight) {
      pl <- plotly::add_trace(pl,
        data = df_nonhighlight,
        x = ~ get(x_col),
        y = ~value,
        color = ~variable,
        legendgroup = ~variable,
        type = "scatter",
        mode = mode,
        opacity = alpha,
        colors = colors,
        split = ~ interaction(variable, i), # ensures each line is treated separately
        showlegend = FALSE,
        visible = "legendonly"
      )
    }

    if (plot_highlight) {
      pl <- plotly::add_trace(pl,
        data = df_highlight,
        x = ~ get(x_col),
        y = ~value,
        color = ~variable,
        legendgroup = ~variable,
        type = "scatter",
        mode = mode,
        opacity = alpha,
        colors = colors,
        split = ~ interaction(variable, i), # ensures each line is treated separately
        showlegend = FALSE,
        # Add traces for stock variables (visible = TRUE)
        visible = TRUE
      )
    }
  }

  # When central_tendency = FALSE, we still want a legend
  if (isFALSE(central_tendency)) {
    # Overwrite with default
    central_tendency <- "mean"

    # Same trick does not work for mode == "markers" to not plot central_tendency
    if (mode == "markers") {
      # When mode == "markers" and the only value available is Infinity, no trace is plotted but the legend still shows up, which is exactly what we want when central_tendency is FALSE

      summary_df_highlight[[central_tendency]] <- Inf
      summary_df_nonhighlight[[central_tendency]] <- Inf
    } else if (mode == "lines") {
      # When only one time point is available and mode == "lines", no trace is plotted but the legend still shows up, which is exactly what we want when central_tendency is FALSE
      summary_df_highlight <- summary_df_highlight[summary_df_highlight[["time"]] == summary_df_highlight[["time"]][1], ]
      summary_df_nonhighlight <- summary_df_nonhighlight[summary_df_nonhighlight[["time"]] == summary_df_nonhighlight[["time"]][1], ]
    }
  }

  # Plot mean/median points/lines
  if (mode == "lines") {
    if (plot_nonhighlight) {
      pl <- plotly::add_trace(pl,
        data = summary_df_nonhighlight,
        x = ~ get(x_col),
        y = ~ get(central_tendency),
        color = ~variable,
        legendgroup = ~variable,
        type = "scatter",
        mode = mode,
        colors = colors,
        showlegend = showlegend,
        line = list(width = central_tendency_width), # thicker line for mean
        visible = "legendonly"
      )
    }

    if (plot_highlight) {
      pl <- plotly::add_trace(pl,
        data = summary_df_highlight,
        x = ~ get(x_col),
        y = ~ get(central_tendency),
        color = ~variable,
        legendgroup = ~variable,
        type = "scatter",
        mode = mode,
        line = list(width = central_tendency_width), # thicker line for mean
        showlegend = showlegend,
        colors = colors,
        visible = TRUE
      )
    }
  } else if (mode == "markers" & type == "summary") {
    if (plot_nonhighlight) {
      pl <- plotly::add_trace(pl,
        data = summary_df_nonhighlight,
        x = ~ get(x_col),
        y = ~ get(central_tendency),
        color = ~variable,
        legendgroup = ~variable,
        type = "scatter",
        error_y = ~ list(
          symmetric = FALSE,
          arrayminus = get(central_tendency) - get(q_low),
          array = get(q_high) - get(central_tendency),
          color = colors
        ),
        mode = mode,
        colors = colors,
        showlegend = showlegend,
        marker = list(size = central_tendency_width * 3), # thicker line for mean
        visible = "legendonly"
      )
    }

    if (plot_highlight) {
      pl <- plotly::add_trace(pl,
        data = summary_df_highlight,
        x = ~ get(x_col),
        y = ~ get(central_tendency),
        color = ~variable,
        legendgroup = ~variable,
        type = "scatter",
        error_y = ~ list(
          symmetric = FALSE,
          arrayminus = get(central_tendency) - get(q_low),
          array = get(q_high) - get(central_tendency),
          color = colors
        ),
        mode = mode,
        marker = list(size = central_tendency_width * 3), # thicker line for mean
        showlegend = showlegend,
        colors = colors,
        visible = TRUE
      )
    }
  } else if (mode == "markers" & type == "sims") {
    if (plot_nonhighlight) {
      pl <- plotly::add_trace(pl,
        data = summary_df_nonhighlight,
        x = ~ get(x_col),
        y = ~ get(central_tendency),
        color = ~variable,
        legendgroup = ~variable,
        type = "scatter",
        mode = mode,
        colors = colors,
        showlegend = showlegend,
        marker = list(size = central_tendency_width * 3), # thicker line for mean
        visible = "legendonly"
      )
    }

    if (plot_highlight) {
      pl <- plotly::add_trace(pl,
        data = summary_df_highlight,
        x = ~ get(x_col),
        y = ~ get(central_tendency),
        color = ~variable,
        legendgroup = ~variable,
        type = "scatter",
        mode = mode,
        marker = list(size = central_tendency_width * 3), # thicker line for mean
        showlegend = showlegend,
        colors = colors,
        visible = TRUE
      )
    }
  }



  # Customize layout
  pl <- plotly::layout(pl,
    margin = list(t = 50, b = 50, l = 50, r = 50),
    # xaxis = list(tickfont = list(size = ceiling(font_size *.75))),
    # yaxis = list(tickfont = list(size = ceiling(font_size *.75))),
    # As the most important things are at the top, reverse the trace order
    legend = list(
      traceorder = "reversed",
      font = list(size = ceiling(font_size * .85))
    )
  )

  # If there is only one trace, legend doesn't show
  if (showlegend & (nr_var == 1)) {
    pl <- plotly::layout(pl, showlegend = TRUE)
  }


  if (!create_subplots) {
    pl <- plotly::layout(pl,
      title = main,
      xaxis = list(title = xlab),
      yaxis = list(title = ylab),
      font = list(family = font_family, size = font_size)
    )
  }


  # Add subplot title
  if (create_subplots & j_labels) {
    pl <- plotly::layout(pl,
      annotations = list(
        list(
          text = paste0("j = ", j_name),
          font = list(
            family = font_family,
            size = ceiling(font_size * .75)
          ),
          bgcolor = "white",
          xref = "paper",
          yref = "paper",
          xanchor = "center", yanchor = "top",
          x = 0.5, y = 1,
          showarrow = FALSE
        )
      )
    )
  }

  # Set x-axis limits if specified
  if ("xlim" %in% names(dots)) {
    pl <- plotly::layout(pl, xaxis = list(range = dots[["xlim"]]))
  }

  # Set y-axis limits if specified
  if ("ylim" %in% names(dots)) {
    pl <- plotly::layout(pl, yaxis = list(range = dots[["ylim"]]))
  }

  return(pl)
}
