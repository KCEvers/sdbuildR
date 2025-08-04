#' Convert all R equations to Julia code
#'
#' @inheritParams build
#' @inheritParams simulate_julia
#' @inheritParams clean_unit
#'
#' @return Updated sfm
#' @noRd
#'
convert_equations_julia_wrapper <- function(sfm, regex_units) {
  # Get variable names
  var_names <- get_model_var(sfm)

  sfm[["model"]][["variables"]] <- lapply(
    sfm[["model"]][["variables"]],
    function(x) {
      lapply(x, function(y) {
        if (is_defined(y[["eqn"]])) {
          out <- convert_equations_julia(sfm, y[["type"]], y[["name"]], y[["eqn"]], var_names,
            regex_units = regex_units
          )
          y <- utils::modifyList(y, out)
        }
        return(y)
      })
    }
  )

  # Macros
  sfm[["macro"]] <- lapply(sfm[["macro"]], function(x) {
    # If a name is defined, assign macro to that name (necessary for correct conversion of functions)
    if (nzchar(x[["name"]])) {
      x[["eqn_julia"]] <- paste0(x[["name"]], " = ", x[["eqn"]])
    } else {
      x[["eqn_julia"]] <- x[["eqn"]]
    }

    out <- convert_equations_julia(sfm, "macro", "macro", x[["eqn_julia"]], var_names,
      regex_units = regex_units
    )
    x <- utils::modifyList(x, out)

    return(x)
  })

  return(sfm)
}



#' Transform R code to Julia code
#'
#' @inheritParams build
#' @inheritParams convert_equations_IM
#' @inheritParams clean_unit
#'
#' @return Dataframe with transformed eqn
#' @importFrom rlang .data
#' @noRd
#'
convert_equations_julia <- function(sfm, type, name, eqn, var_names, regex_units) {
  if (.sdbuildR_env[["P"]][["debug"]]) {
    # print("")
    # print(type)
    # print(name)
    print(eqn)
  }

  if (length(eqn) > 1) {
    stop("eqn must be of length 1!")
  }

  default_out <- list(
    eqn_julia = "0.0",
    func = list()
  )

  # Check whether eqn is empty or NULL
  if (is.null(eqn) || !nzchar(eqn)) {
    return(default_out)
  }

  if (eqn == "0" | eqn == "0.0") {
    return(default_out)
  }

  # Try to parse the code
  out <- tryCatch(
    {
      parse(text = eqn)
      TRUE
    },
    error = function(e) {
      return(e)
    }
  )

  if ("error" %in% class(out)) {
    stop(paste0("Parsing equation of ", name, " failed:\n", out[["message"]]))
  }

  if (any(grepl("%%", eqn))) {
    stop("The modulus operator a %% b is not supported in sdbuildR. Please use mod(a, b) instead.")
  }

  if (any(grepl("na\\.rm", eqn))) {
    stop("na.rm is not supported as an argument in sdbuildR. Please use na.omit(x) instead.")
  }

  # Remove comments we don't keep these
  eqn <- remove_comments(eqn)[["eqn"]]

  # If equation is now empty, don't run rest of functions but set equation to zero
  if (!nzchar(eqn) | eqn == "0" | eqn == "0.0") {
    return(default_out)
  } else {
    # Ensure there is no scientific notation
    eqn <- scientific_notation(eqn)

    # Step 2. Syntax (bracket types, destructuring assignment, time units {1 Month})
    eqn <- eqn |>
      # Translate vector brackets, i.e. c() -> []
      vector_to_square_brackets(var_names) |>
      # Ensure integers are floats
      # Julia can throw InexactError errors in case e.g. an initial condition is defined as an integer
      replace_digits_with_floats(var_names)


    # # Destructuring assignment, e.g. x, y <- {a, b}
    # **to do
    # conv_destructuring_assignment()


    # print("eqn")
    # print(eqn)

    # Step 3. Statements (if, for, while, functions, try)
    eqn <- convert_all_statements_julia(eqn, var_names)

    # Step 4. Operators (booleans, logical operators, addition of strings)
    eqn <- eqn |>
      # # Convert addition of strings to paste0
      # conv_addition_of_strings(var_names) |>
      # # Replace logical operators (true, false, = (but not if in function()))
      replace_op_julia(var_names) #|>
    # # Replace range, e.g. range(0, 10, 2) -> 0:2:10
    # replace_range_julia(var_names)

    # Step 5. Replace R functions to Julia functions
    conv_list <- convert_builtin_functions_julia(type, name, eqn, var_names)
    eqn <- conv_list[["eqn"]]
    add_Rcode <- conv_list[["add_Rcode"]]


    # **to do:
    #     <<- --> global
    # <- -> =

    # Remove spaces in front of new lines
    eqn <- stringr::str_replace_all(eqn, "[ ]*\n", "\n")

    # Replace single with double quotation marks
    eqn <- stringr::str_replace_all(eqn, "\'", "\"")

    # Clean units again to ensure no scientific notation is used when necessary; do this at the end to avoid the scientific notation messing up other parts
    eqn <- clean_unit_in_u(eqn, regex_units)

    # Units: replace u("") with u""
    eqn <- stringr::str_replace_all(eqn, "\\bu\\([\"|'](.*?)[\"|']\\)", "u\"\\1\"")

    # # # If it is a multi-line statement, surround by brackets in case they aren't macros
    # # eqn = trimws(eqn)
    # if (stringr::str_detect(eqn, stringr::fixed("\n")) & !stringr::str_starts(eqn, "begin") & !stringr::str_ends(eqn, "end")) {
    #   eqn = paste0("begin\n", eqn, "\nend")
    # }

    # if (.sdbuildR_env[["P"]][["debug"]]){print(eqn)}

    out <- append(list(eqn_julia = eqn), add_Rcode)

    return(out)
  }
}





#' Get indices of digits in string
#'
#' @inheritParams convert_equations_IM
#'
#' @returns Dataframe with start and end indices of digits
#' @noRd
#'
get_range_digits <- function(eqn, var_names) {
  # Get indices in variable names or quotations to exclude later
  idxs_exclude <- get_seq_exclude(eqn, var_names, names_with_brackets = FALSE)

  # Locate all integers
  # idx_df = as.data.frame(stringr::str_locate_all(eqn, "(?<![a-zA-Z0-9\\.:punct:])[0-9]+(?![a-zA-Z0-9\\.:punct:])")[[1]])
  # Remove :punct: -> !"#%&'()*+,-./:; -> this skips e.g. 1:10
  idx_df <- as.data.frame(stringr::str_locate_all(eqn, "(?<![a-zA-Z0-9\\.])[0-9]+(?![a-zA-Z0-9\\.])")[[1]])

  if (nrow(idx_df) > 0) {
    # Remove matches within variable names or quotations
    idx_df <- idx_df[!(idx_df[["start"]] %in% idxs_exclude | idx_df[["end"]] %in% idxs_exclude), ]

    if (nrow(idx_df) > 0) {
      # Extract substrings vectorized
      sub_formulas <- stringr::str_sub(eqn, idx_df[["start"]], idx_df[["end"]])

      # Filter idx_df where substrings contain only digits
      idx_df <- idx_df[grepl("^[0-9]+$", sub_formulas), ]
    }
  }

  return(idx_df)
}



# eqn <- "1e+09 + a"
# eqn <- "1e-09 + a"
# eqn <- ".01e09 + a + .03e-02"


#' Replace digits with floats in string
#'
#' @inheritParams convert_equations_IM
#'
#' @returns Updated string
#' @noRd
#'
replace_digits_with_floats <- function(eqn, var_names) {
  idx_df <- get_range_digits(eqn, var_names)

  if (nrow(idx_df) > 0) {
    # Replace digit with float in each case
    for (i in rev(idx_df[["end"]])) {
      eqn <- stringr::str_c(stringr::str_sub(eqn, 1, i), ".0", stringr::str_sub(eqn, i + 1, -1))
    }
  }

  return(eqn)
}


# replace_range_Julia = function(eqn, var_names) {
#
#   # ** to do
#
#
#   paired_idxs = get_range_all_pairs(eqn, var_names, add_custom = "seq()", type = "seq")
#   paired_idxs
#
#   # Split eqn with indices from paired_idxs
#   # The default third argument for seq() in R is by, but in Julia it is length
#
#   bracket_arg = stringr::str_sub(eqn, idx_func$start_bracket + 1, idx_func$end - 1)
#   arg = parse_args(bracket_arg, var_names)
#
#
#   stringr::str_replace_all(., ", by", ", step")
#   stringr::str_replace_all(., ", length.out", ", length")
#
#
#
#
#   # Replace range, e.g. range(0, 10, 2) -> 0:2:10
#   eqn = stringr::str_replace_all(eqn, "range\\((.*?), (.*?), (.*?)\\)", "\\1:\\3:\\2")
#
#   return(eqn)
#
# }



#' Translate R operators to Julia
#'
#' @inheritParams convert_equations_IM
#' @return Updated eqn
#' @importFrom rlang .data
#' @noRd
#'
replace_op_julia <- function(eqn, var_names) {
  # Define logical operators in R and replacements in Julia
  logical_op_words <- c("TRUE" = "true", "FALSE" = "false", "T" = "true", "F" = "false", "NULL" = "nothing", "NA" = "missing")
  # Cannot be preceded or followed by a letter
  names(logical_op_words) <- paste0("\\b", stringr::str_escape(names(logical_op_words)), "\\b")


  # **to do: 1 is not true in Julia

  logical_op_signs <- c(
    # Default: broadcast operations # Add spaces everywhere to clear confusion with floats
    "*" = " .* ",
    "/" = " ./ ",
    "+" = " .+ ",
    "^" = " .^ ",
    # "<" = " .< ",
    # ">" = " .> ",
    "<=" = " .<= ",
    ">=" = " .>= ",
    "==" = " .== ",
    "!=" = " .!= ",

    # Modulus operator - new function in Julia
    "%%" = "\\u2295",

    # Remainder operator
    "%REM%" = "%",

    # Assignment
    "<-" = " = ",
    # Pipe operator
    # "%>%" = " |> ",
    # Matrix algebra
    "%*%" = " * ",
    "%in%" = " in "
    # "%%" = "mod"
    # "$"
  )
  #

  names(logical_op_signs) <- paste0("(?<![\\.%])", stringr::str_escape(names(logical_op_signs)))
  logical_op <- c(logical_op_words, logical_op_signs)

  # Add additional operators to replace, which require special regex to
  logical_op <- c(
    logical_op,
    c("(?<!<)-(?!>)" = " .- "),
    c("(?<!\\.|%)<(?!-|=)" = " .< "),
    c("(?<!\\.|-|%)>(?!=)" = " .> "),
    c("(?<!&)&(?!&)" = " && ")
  )

  # Find indices of logical operators
  idxs_logical_op <- stringr::str_locate_all(eqn, names(logical_op))
  idxs_logical_op

  if (length(unlist(idxs_logical_op)) > 0) {
    # Get match and replacement
    df_logical_op <- as.data.frame(do.call(rbind, idxs_logical_op))
    df_logical_op[["match"]] <- stringr::str_sub(eqn, df_logical_op[["start"]], df_logical_op[["end"]])
    df_logical_op[["replacement"]] <- rep(unname(logical_op), sapply(idxs_logical_op, nrow))
    df_logical_op <- df_logical_op[order(df_logical_op[["start"]]), ]
    df_logical_op

    # Remove those that are in quotation marks or names
    idxs_exclude <- get_seq_exclude(eqn, var_names)

    if (nrow(df_logical_op) > 0) df_logical_op <- df_logical_op[!(df_logical_op[["start"]] %in% idxs_exclude | df_logical_op[["end"]] %in% idxs_exclude), ]
    # Remove matches that are the same as the logical operator
    if (nrow(df_logical_op) > 0) df_logical_op <- df_logical_op[df_logical_op[["replacement"]] != df_logical_op[["match"]], ]

    if (nrow(df_logical_op) > 0) {
      # Replace in reverse order; no nested functions, so we can replace them in one go
      for (i in rev(1:nrow(df_logical_op))) {
        stringr::str_sub(eqn, df_logical_op[i, ][["start"]], df_logical_op[i, ][["end"]]) <- df_logical_op[i, ][["replacement"]]
      }
      # Remove double spaces
      eqn <- stringr::str_replace_all(eqn, "[ ]+", " ")
    }
  }

  return(eqn)
}



#' Find all round brackets
#'
#' Helper for convert_all_statements_julia()
#'
#' @param df Dataframe with indices
#' @param round_brackets Dataframe with indices of round brackets
#' @inheritParams convert_equations_julia
#'
#' @returns Modified dataframe
#' @noRd
#'
find_round <- function(df, round_brackets, eqn, var_names) {
  statements <- c("if", "else if", "for", "while", "function")
  if (df[["statement"]] %in% c(statements, toupper(statements))) {
    matching <- round_brackets[match(df[["end"]], round_brackets[["start"]]), ]
    start_round <- matching[["start"]]
    end_round <- matching[["end"]]
  } else {
    start_round <- end_round <- NA
  }

  start_word <- end_word <- func_name <- NA

  if (df[["statement"]] %in% c("function", "FUNCTION")) {
    # Get words before statement
    words <- get_words(stringr::str_sub(eqn, 1, df[["start"]] - 1))
    if (nrow(words) > 0) {
      # Pick last word
      word <- words[nrow(words), ]
      start_word <- word[["start"]]
      end_word <- word[["end"]]
      func_name <- word[["word"]]
    }
  }
  return(cbind(df, data.frame(
    start_round = start_round, end_round = end_round,
    start_word = start_word, end_word = end_word,
    func_name = func_name
  )))
}



#' Find all curly brackets
#'
#' Helper for convert_all_statements_julia()
#'
#' @param df Dataframe with indices
#' @param paired_idxs Dataframe with indices
#'
#' @returns Modified dataframe
#' @noRd
#'
find_curly <- function(df, paired_idxs) {
  statements <- c("if", "else if", "for", "while", "function")
  if (df[["statement"]] %in% c(statements, toupper(statements))) {
    matching <- paired_idxs[which(paired_idxs[["start"]] > df[["end_round"]])[1], ]
  } else {
    matching <- paired_idxs[which(paired_idxs[["start"]] == df[["end"]])[1], ]
  }
  start_curly <- matching[["start"]]
  end_curly <- matching[["end"]]
  return(cbind(df, data.frame(start_curly = start_curly, end_curly = end_curly)))
}


#' Convert all statement syntax from R to Julia
#' Wrapper around convert_statement()
#'
#' @inheritParams convert_equations_IM
#'
#' @return Updated eqn
#' @noRd
#'
convert_all_statements_julia <- function(eqn, var_names) {
  eqn_old <- eqn


  # ** to do: R's local() is equivalent to Julia's let end I believe
  # result <- local({
  #   set.seed(1)
  #   v <- rnorm(10, 0, 1)
  #   min(v)
  # })
  # print(result)        # e.g., -1.023122
  # print(exists("v"))   # FALSE


  # If curly brackets surround entire eqn, replace and surround with begin ... end
  if (stringr::str_sub(eqn, 1, 1) == "{" & stringr::str_sub(eqn, nchar(eqn), nchar(eqn)) == "}") {
    stringr::str_sub(eqn, nchar(eqn), nchar(eqn)) <- "\nend"
    stringr::str_sub(eqn, 1, 1) <- "begin\n"
  }

  # Only if there are curly brackets in the equation, look for statements
  if (grepl("\\{", eqn)) {
    done <- FALSE
    i <- 1 # counter

    # Define regular expressions for statements, accounting for whitespace
    statement_regex <- c(
      "for" = "for[ ]*\\(",
      "if" = "if[ ]*\\(",
      "while" = "while[ ]*\\(", "else" = "[ ]*else[ ]*\\{",
      "else if" = "[ ]*else if[ ]*\\(", "function" = "function[ ]*\\("
    )

    while (!done) {
      # Create sequence of indices of curly brackets; update each iteration
      paired_idxs <- get_range_all_pairs(eqn, var_names, type = "curly")

      # Look for statements
      idx_statements <- stringr::str_locate_all(eqn, unname(statement_regex))
      df_statements <- as.data.frame(do.call(rbind, idx_statements))
      df_statements[["statement"]] <- rep(names(statement_regex), sapply(idx_statements, nrow))

      # # Remove those matches that are in quotation marks or names
      idxs_exclude <- get_seq_exclude(eqn, var_names, type = "quot")
      if (nrow(df_statements) > 0) df_statements <- df_statements[!(df_statements[["start"]] %in% idxs_exclude | df_statements[["end"]] %in% idxs_exclude), ]

      if (!(nrow(paired_idxs) > 0 & nrow(df_statements) > 0)) {
        done <- TRUE
      } else {
        # Sort by start index
        paired_idxs <- paired_idxs[order(paired_idxs[["start"]]), ]

        # Get all round brackets
        round_brackets <- get_range_all_pairs(eqn, var_names, type = "round")

        df_statements <- df_statements[order(df_statements[["start"]]), ]

        # Step 1: Group by 'end' and keep row with minimum 'start' value for each group
        df_grouped <- split(df_statements, df_statements[["end"]])
        df_min_rows <- do.call(rbind, lapply(df_grouped, function(group) {
          min_start_idx <- which.min(group[["start"]])
          group[min_start_idx, ]
        }))

        # Step 2: Add row numbers as 'id' column
        df_min_rows[["id"]] <- seq_len(nrow(df_min_rows))

        # Step 3: Apply find_round function to each row
        df_with_round <- do.call(rbind, lapply(seq_len(nrow(df_min_rows)), function(i) {
          row_data <- df_min_rows[i, ]
          result <- find_round(row_data, round_brackets, eqn, var_names)
          result[["id"]] <- i # Preserve the id
          result
        }))

        # Step 4: Apply find_curly function to each row
        df_statements <- do.call(rbind, lapply(seq_len(nrow(df_with_round)), function(i) {
          row_data <- df_with_round[i, ]
          result <- find_curly(row_data, paired_idxs)
          result[["id"]] <- i # Preserve the id
          result
        }))

        # Remove row names that might have been created by rbind
        rownames(df_statements) <- NULL

        # Add lead_start column (equivalent to dplyr::lead with default = 0)
        lead_start <- c(df_statements[["start"]][-1], 0) - 1
        df_statements[["lead_start"]] <- lead_start

        # Add next_statement column (equivalent to dplyr::if_else with dplyr::lead)
        lead_statement <- c(df_statements[["statement"]][-1], NA)
        df_statements[["next_statement"]] <- ifelse(
          df_statements[["end_curly"]] == df_statements[["lead_start"]],
          lead_statement,
          NA
        )


        df_statements

        if (nrow(df_statements) == 0) {
          done <- TRUE
        } else {
          # # At first iteration, replace all with uppercase versions, as the statement names are the same in R and Julia. This is necessart because someone may have enclosed their if statement etc. in extra round brackets, such that it still matches
          if (i == 1) {
            # Replace all statement names with uppercase versions
            for (i in 1:nrow(df_statements)) {
              stringr::str_sub(eqn, df_statements[i, "start"], df_statements[i, "end"]) <- toupper(stringr::str_sub(eqn, df_statements[i, "start"], df_statements[i, "end"]))
            }
            statement_regex <- toupper(statement_regex)
            i <- i + 1
            next
          }


          # Start with first pair
          pair <- df_statements[1, ]
          pair |> as.data.frame()

          if (pair[["statement"]] %in% c("if")) {
            if (pair[["next_statement"]] %in% c("else if", "else")) {
              stringr::str_sub(eqn, pair[["end_curly"]], pair[["end_curly"]]) <- ""
            } else {
              stringr::str_sub(eqn, pair[["end_curly"]], pair[["end_curly"]]) <- "end"
            }
            stringr::str_sub(eqn, pair[["start_curly"]], pair[["start_curly"]]) <- ""
            stringr::str_sub(eqn, pair[["end_round"]], pair[["end_round"]]) <- " "
            stringr::str_sub(eqn, pair[["start_round"]], pair[["start_round"]]) <- " "
            stringr::str_sub(eqn, pair[["start"]], pair[["end"]] - 1) <- tolower(stringr::str_sub(eqn, pair[["start"]], pair[["end"]] - 1)) # replace statement, not opening bracket
          } else if (pair[["statement"]] %in% c("else if")) {
            if (pair[["next_statement"]] %in% c("else if", "else")) {
              stringr::str_sub(eqn, pair[["end_curly"]], pair[["end_curly"]]) <- ""
            } else {
              stringr::str_sub(eqn, pair[["end_curly"]], pair[["end_curly"]]) <- "end"
            }
            stringr::str_sub(eqn, pair[["start_curly"]], pair[["start_curly"]]) <- ""
            stringr::str_sub(eqn, pair[["end_round"]], pair[["end_round"]]) <- " "
            stringr::str_sub(eqn, pair[["start"]], pair[["end"]]) <- "elseif " # also captures opening round bracket
          } else if (pair[["statement"]] %in% c("else")) {
            stringr::str_sub(eqn, pair[["end_curly"]], pair[["end_curly"]]) <- "end"
            stringr::str_sub(eqn, pair[["start_curly"]], pair[["start_curly"]]) <- ""
            stringr::str_sub(eqn, pair[["start"]], pair[["end"]] - 1) <- tolower(stringr::str_sub(eqn, pair[["start"]], pair[["end"]] - 1)) # replace statement, not opening bracket
          } else if (pair[["statement"]] %in% c("for", "while")) {
            stringr::str_sub(eqn, pair[["end_curly"]], pair[["end_curly"]]) <- "end"
            stringr::str_sub(eqn, pair[["start_curly"]], pair[["start_curly"]]) <- ""
            stringr::str_sub(eqn, pair[["end_round"]], pair[["end_round"]]) <- " "
            stringr::str_sub(eqn, pair[["start_round"]], pair[["start_round"]]) <- " "
            stringr::str_sub(eqn, pair[["start"]], pair[["end"]] - 1) <- tolower(stringr::str_sub(eqn, pair[["start"]], pair[["end"]] - 1)) # replace statement, not opening bracket
          } else if (pair[["statement"]] %in% c("function")) {
            stringr::str_sub(eqn, pair[["end_curly"]], pair[["end_curly"]]) <- "end"
            stringr::str_sub(eqn, pair[["start_curly"]], pair[["start_curly"]]) <- ""

            # Parse arguments
            arg <- parse_args(stringr::str_sub(eqn, pair[["start_round"]] + 1, pair[["end_round"]] - 1))

            # All default arguments have to be at the end; if not, throw error
            contains_name <- stringr::str_detect(arg, "=")
            arg_split <- stringr::str_split_fixed(arg, "=", n = 2)
            names_arg <- ifelse(contains_name, arg_split[, 1], NA) |> trimws()

            # error when there are non-default arguments between default argumens or when default argument is not at the end
            if (any(!is.na(names_arg))) {
              if (any(diff(which(!is.na(names_arg))) > 1) | max(which(!is.na(names_arg))) != length(names_arg)) {
                stop(paste0("Please change the function definition of ", pair[["func_name"]], ". All arguments with defaults have to be placed at the end of the function arguments."))
              }
            }

            arg <- paste0(arg, collapse = ", ") |>
              # Varargs (Variable Arguments): , ... -> ...
              stringr::str_replace_all(",[ ]*\\.\\.\\.", "...")

            stringr::str_sub(eqn, pair[["start_word"]], pair[["end_round"]]) <- paste0(
              "function ", pair[["func_name"]],
              # # To mimic R's flexibility in positional and keyword arguments, we use keyword arguments for all arguments in Julia
              # "(;",
              # For consistency, we use NO keyword arguments for all arguments in Julia, so no ; in function statements
              "(",
              arg, ")"
            )
          }
        }
      }
    }
  }


  ### Convert one liner functions
  # Get start of new sentences
  idxs_newline <- rbind(
    data.frame(start = 1, end = 1),
    stringr::str_locate_all(eqn, "\n")[[1]] |> as.data.frame(),
    data.frame(start = nchar(eqn) + 1, end = nchar(eqn) + 1)
  )

  # For each new line, find first two words
  x <- idxs_newline[["end"]]
  pairs <- lapply(seq(length(x) - 1), function(i) {
    # Get surrounding words
    pair <- data.frame(start = x[i], end = x[i + 1] - 1)
    pair[["match"]] <- stringr::str_sub(eqn, pair[["start"]], pair[["end"]])
    words <- get_words(pair[["match"]])
    pair[["first_word"]] <- ifelse(nrow(words) > 0, words[1, "word"], "")
    pair[["second_word"]] <- ifelse(nrow(words) > 1, words[2, "word"], "")

    # If second word is function, replace
    if (pair[["second_word"]] == "function") {
      pair[["match"]] <- pair[["match"]] |>
        stringr::str_replace(
          paste0(pair[["second_word"]], "[ ]*\\("),
          # Edit: DON'T turn everything into keyword argument
          paste0(pair[["first_word"]], "(")
        ) |>
        # Replace assignment operator too
        stringr::str_replace(
          paste0(stringr::str_escape(pair[["first_word"]]), "[ ]*(=|<-)"),
          paste0(pair[["second_word"]], " ")
        )

      # A new line needs to be added for Julia after the function name and brackets
      # Get all round brackets
      round_brackets <- get_range_all_pairs(pair[["match"]], var_names, type = "round")

      # Find first opening bracket
      chosen_bracket <- round_brackets[["start"]] == min(round_brackets[["start"]])
      end_idx <- round_brackets[chosen_bracket, ][["end"]]

      # Parse arguments
      arg <- parse_args(stringr::str_sub(pair[["match"]], round_brackets[chosen_bracket, "start"] + 1, end_idx - 1))

      # All default arguments have to be at the end; if not, throw error
      contains_name <- stringr::str_detect(arg, "=")
      arg_split <- stringr::str_split_fixed(arg, "=", n = 2)
      names_arg <- ifelse(contains_name, arg_split[, 1], NA) |> trimws()

      # error when there are non-default arguments between default argumens or when default argument is not at the end
      if (any(!is.na(names_arg))) {
        if (any(diff(which(!is.na(names_arg))) > 1) | max(which(!is.na(names_arg))) != length(names_arg)) {
          stop(paste0("Please change the function definition of ", pair[["first_word"]], ". All arguments with defaults have to be placed at the end of the function arguments."))
        }
      }

      stringr::str_sub(pair[["match"]], end_idx, end_idx) <- ")\n"

      # Add end at the end
      pair[["match"]] <- paste0(pair[["match"]], "\nend")
    }
    return(pair)
  })

  eqn <- unlist(lapply(pairs, `[[`, "match")) |> paste0(collapse = "")

  return(eqn)
}





#' Create list of default arguments
#'
#' @param arg List with parsed arguments
#'
#' @returns List with named default arguments
#' @noRd
#'
create_default_arg <- function(arg) {
  # Find names and values of arguments
  contains_value <- stringr::str_detect(arg, "=")
  arg_split <- stringr::str_split_fixed(arg, "=", n = 2)
  values_arg <- ifelse(contains_value, arg_split[, 1], NA) |> trimws()
  names_arg <- ifelse(contains_value, arg_split[, 2], arg_split[, 1]) |> trimws()
  default_arg <- lapply(as.list(stats::setNames(values_arg, names_arg)), as.character)

  return(default_arg)
}


#' Get regular expressions for Julia functions
#'
#' @noRd
#' @return Dataframe
get_syntax_julia <- function() {
  # https://www.johnmyleswhite.com/notebook/2012/04/09/comparing-julia-and-rs-vocabularies/
  # https://docs.julialang.org/en/v1/manual/noteworthy-differences/

  # Custom function to replace each (nested) function; necessary because regex in stringr unfortunately doesn't seem to handle nested functions
  conv_df <- matrix(
    c(

      # Statistics
      "min", "min", "syntax1", "", "", FALSE,
      "max", "max", "syntax1", "", "", FALSE,
      "pmin", "min", "syntax1", "", "", FALSE,
      "pmax", "max", "syntax1", "", "", FALSE,
      "mean", "Statistics.mean", "syntax1", "", "", FALSE,
      "median", "Statistics.median", "syntax1", "", "", FALSE,
      "prod", "prod", "syntax1", "", "", FALSE, # ** to do: na.rm!
      "sum", "sum", "syntax1", "", "", FALSE,
      "sd", "Statistics.std", "syntax1", "", "", FALSE,
      "cor", "Statistics.cor", "syntax1", "", "", FALSE,
      "cov", "Statistics.cov", "syntax1", "", "", FALSE,
      "var", "Statistics.var", "syntax1", "", "", FALSE,
      "range", "extrema", "syntax1", "", "", FALSE,
      "as.logical", "Bool", "syntax1", "", "", TRUE,
      "seq", "range", "syntax_seq", "", "", FALSE,
      "seq.int", "range", "syntax_seq", "", "", FALSE,
      "seq_along", "range", "syntax_seq", "", "", FALSE,
      "seq_len", "range", "syntax_seq", "", "", FALSE,
      "sample", "StatsBase.sample", "syntax_sample", "", "", FALSE,
      "sample.int", "StatsBase.sample", "syntax_sample", "", "", FALSE,


      # ** mad()
      # Statistics.median(abs.(A .- Statistics.median(A)))


      # **to do: "seq"; Julia:
      # range(start, stop, length)
      # range(start, stop; length, step)
      # range(start; length, stop, step)
      # range(;start, length, stop, step)

      # "shuffle", "", "syntax1", "", "" , TRUE,


      # ** to do: cummax, cummin
      "cumsum", "cumsum", "syntax1", "", "", FALSE,
      "cumprod", "cumprod", "syntax1", "", "", FALSE,
      "diff", "diff", "syntax1", "", "", FALSE,
      "abs", "abs", "syntax1", "", "", TRUE,
      "sign", "sign", "syntax1", "", "", TRUE,
      "cos", "cos", "syntax1", "", "", TRUE,
      "sin", "sin", "syntax1", "", "", TRUE,
      "tan", "tan", "syntax1", "", "", TRUE,
      "acos", "acos", "syntax1", "", "", TRUE,
      "asin", "asin", "syntax1", "", "", TRUE,
      "atan", "atan", "syntax1", "", "", TRUE,
      "cospi", "cospi", "syntax1", "", "", TRUE,
      "sinpi", "sinpi", "syntax1", "", "", TRUE,
      "tanpi", "tanpi", "syntax1", "", "", TRUE,
      "nchar", "length", "syntax1", "", "", FALSE,
      "cor", "cor", "syntax1", "", "", FALSE,
      "floor", "floor", "syntax1", "", "", TRUE,
      "ceiling", "ceil", "syntax1", "", "", TRUE,
      "round", "round_", "syntax1", "", "", TRUE,
      "trunc", "trunc", "syntax1", "", "", TRUE,

      # Find
      # "which", "findall", "syntax1", "", "",

      # findmax(arr): Returns (max_value, index).
      # findmin(arr): Returns (min_value, index).

      "which.min", "argmin", "syntax1", "", "", FALSE,
      "which.max", "argmax", "syntax1", "", "", FALSE,
      "exp", "exp", "syntax1", "", "", TRUE,
      "expm1", "expm1", "syntax1", "", "", TRUE,
      # "log", "log", "syntax1", "", "", TRUE, # **to do, put base first!
      # "logb", "logb", "syntax1", "", "", TRUE,
      "log2", "log2", "syntax1", "", "", TRUE,
      "log10", "log10", "syntax1", "", "", TRUE,
      "sqrt", "sqrt", "syntax1", "", "", TRUE,
      "dim", "size", "syntax1", "", "", FALSE,
      "nrow", "size", "syntax1", "", "1", FALSE,
      "ncol", "size", "syntax1", "", "2", FALSE,
      "cbind", "hcat", "syntax1", "", "", FALSE,
      "rbind", "vcat", "syntax1", "", "", FALSE,

      # Matrix functions
      "diag", "LinearAlgebra.diag", "syntax1", "", "", FALSE,
      "upper.tri", "LinearAlgebra.UpperTriangular", "syntax1", "", "", FALSE,
      "lower.tri", "LinearAlgebra.LowerTriangular", "syntax1", "", "", FALSE,
      "norm", "LinearAlgebra.norm", "syntax1", "", "", FALSE,
      "det", "LinearAlgebra.det", "syntax1", "", "", FALSE,
      "t", "transpose", "syntax1", "", "", FALSE,
      "rev", "reverse", "syntax1", "", "", FALSE,
      "print", "println", "syntax1", "", "", FALSE,
      "na.omit", "skipmissing", "syntax1", "", "", FALSE,
      "eigen", "eig", "syntax1", "", "", FALSE,
      "getcd", "getcwd", "syntax1", "", "", FALSE,
      "setwd", "setcwd", "syntax1", "", "", FALSE,
      "Filter", "filter", "syntax1", "", "", TRUE,
      "which", "findall", "syntax1", "", "", FALSE,
      "class", "typeof", "syntax1", "", "", FALSE,

      # **
      # "pracma::logspace", "logrange", "syntax1", "", "",
      # "isTRUE", "", "syntax1", "", "",
      # "isFALSE", "", "syntax1", "", "",


      # "", "isapprox", "syntax1", "", "",
      #
      # # String manipulation
      "grep", "match", "syntax1", "", "", FALSE,
      "strsplit", "split", "syntax1", "", "", FALSE,
      "paste0", "join", "syntax1", "", "", FALSE,
      "toupper", "uppercase", "syntax1", "", "", TRUE,
      "tolower", "lowercase", "syntax1", "", "", TRUE,
      "stringr::str_to_title", "uppercasefirst", "syntax1", "", "", TRUE,


      # "sprintf", "", "syntax1", "", "",
      # "paste", "", "syntax1", "", "",
      # "paste0", "", "syntax1", "", "",

      # Sets
      # to do: Julia's isdisjoint, issubset, issetequal.
      "union", "union", "syntax1", "", "", FALSE,
      "intersect", "intersect", "syntax1", "", "", FALSE,
      "setdiff", "setdiff", "syntax1", "", "", FALSE,
      "setequal", "setequal", "syntax1", "", "", FALSE,

      # is....()
      "rlang::is_empty", "isempty", "syntax1", "", "", FALSE,
      "all", "all", "syntax1", "", "", FALSE,
      "any", "any", "syntax1", "", "", FALSE,
      "is.infinite", "isinf", "syntax1", "", "", TRUE,
      "is.finite", "isfinite", "syntax1", "", "", TRUE,
      "is.nan", "ismissing", "syntax1", "", "", TRUE,

      # https://docs.julialang.org/en/v1/base/collections
      # Julia: indexin, sortperm, findfirst
      "sort", "sort", "syntax1", "", "", FALSE,


      # Complex numbers
      "Re", "real", "syntax1", "", "", TRUE,
      "Im", "imag", "syntax1", "", "", TRUE,
      "Mod", "", "syntax1", "", "", TRUE,
      "Arg", "", "syntax1", "", "", TRUE,
      "Conj", "conj", "syntax1", "", "", TRUE,

      # as.complex(x, ...)
      # is.complex(x)
      # Re(z)
      # Im(z)
      # Mod(z)
      # Arg(z)
      # Conj(z)
      # imag, reim, complex, isreal, Real.

      # Custom functions
      "logistic", "logistic", "syntax1", "", "", TRUE,
      "logit", "logit", "syntax1", "", "", TRUE,
      "expit", "expit", "syntax1", "", "", TRUE,
      "convert_u", "convert_u", "syntax1", "", "", TRUE,
      "drop_u", "Unitful.ustrip", "syntax1", "", "", TRUE,

      # step() is already an existing function in Julia, so we use make_step() instead
      "step", "make_step", "syntax1", paste0(.sdbuildR_env[["P"]][["times_name"]], ", ", .sdbuildR_env[["P"]][["time_units_name"]]), "", FALSE,
      "pulse", "pulse", "syntax1", paste0(.sdbuildR_env[["P"]][["times_name"]], ", ", .sdbuildR_env[["P"]][["time_units_name"]]), "", FALSE,
      "ramp", "ramp", "syntax1", paste0(.sdbuildR_env[["P"]][["times_name"]], ", ", .sdbuildR_env[["P"]][["time_units_name"]]), "", FALSE,
      "seasonal", "seasonal", "syntax1", paste0(.sdbuildR_env[["P"]][["times_name"]], ", ", .sdbuildR_env[["P"]][["timestep_name"]]), "", FALSE,
      "length_IM", "length", "syntax1", "", "", FALSE,
      "delay", "retrieve_delay", "delay", "", "", FALSE,
      "past", "retrieve_past", "past", "", "", FALSE,
      "delayN", "compute_delayN", "delayN", "", "", FALSE,
      "smoothN", "compute_smoothN", "smoothN", "", "", FALSE,

      # Random Number Functions (13)
      "runif", "rand", "syntaxD", "Distributions.Uniform", "", FALSE,
      "rnorm", "rand", "syntaxD", "Distributions.Normal", "", FALSE,
      "rlnorm", "rand", "syntaxD", "Distributions.LogNormal", "", FALSE,
      "rbool", "rbool", "syntax1", "", "", FALSE,
      "rbinom", "rand", "syntaxD", "Distributions.Binomial", "", FALSE,
      "rnbinom", "rand", "syntaxD", "Distributions.NegativeBinomial", "", FALSE,
      "rpois", "rand", "syntaxD", "Distributions.Poisson", "", FALSE,
      # "EnvStats::rtri", "", "syntaxD", "", "", FALSE,
      "rexp", "rand", "syntaxD", "Distributions.Exponential", "", FALSE,
      "rgamma", "rand", "syntaxD", "Distributions.Gamma", "", FALSE,
      "rbeta", "rand", "syntaxD", "Distributions.Beta", "", FALSE,
      "rcauchy", "rand", "syntaxD", "Distributions.Cauchy", "", FALSE,
      "rchisq", "rand", "syntaxD", "Distributions.Chisq", "", FALSE,
      "rgeom", "rand", "syntaxD", "Distributions.Geometric", "", FALSE,
      "rf", "rand", "syntaxD", "Distributions.FDist", "", FALSE,
      # "rhyper", "rand", "syntaxD", "Distributions.", "", FALSE,
      # "rlogis", "rand", "syntaxD", "Distributions.", "", FALSE,
      "rmultinom", "rand", "syntaxD", "Distributions.Multinomial", "", FALSE,
      # "rsignrank", "rand", "syntaxD", "Distributions.", "", FALSE,
      "rt", "rand", "syntaxD", "Distributions.TDist", "", FALSE,
      "rweibull", "rand", "syntaxD", "Distributions.Weibull", "", FALSE,
      # "rwilcox", "rand", "syntaxD", "Distributions.", "", FALSE,
      # "rbirthday", "rand", "syntaxD", "Distributions.", "", FALSE,
      # "rtukey", "rand", "syntaxD", "Distributions.", "", FALSE,
      "rdist", "rdist", "syntax1", "", "", FALSE,
      "set.seed", "Random.seed!", "syntax1", "", "", FALSE,


      # Statistical Distributions (20)
      "punif", "Distributions.cdf.", "syntaxD", "Distributions.Uniform", "", FALSE,
      "dunif", "Distributions.pdf.", "syntaxD", "Distributions.Uniform", "", FALSE,
      "qunif", "Distributions.quantile.", "syntaxD", "Distributions.Uniform", "", FALSE,
      "pnorm", "Distributions.cdf.", "syntaxD", "Distributions.Normal", "", FALSE,
      "dnorm", "Distributions.pdf.", "syntaxD", "Distributions.Normal", "", FALSE,
      "qnorm", "Distributions.quantile.", "syntaxD", "Distributions.Normal", "", FALSE,
      "plnorm", "Distributions.cdf.", "syntaxD", "Distributions.LogNormal", "", FALSE,
      "dlnorm", "Distributions.pdf.", "syntaxD", "Distributions.LogNormal", "", FALSE,
      "qlnorm", "Distributions.quantile.", "syntaxD", "Distributions.LogNormal", "", FALSE,
      "pbinom", "Distributions.cdf.", "syntaxD", "Distributions.Binomial", "", FALSE,
      "dbinom", "Distributions.pdf.", "syntaxD", "Distributions.Binomial", "", FALSE,
      "qbinom", "Distributions.quantile.", "syntaxD", "Distributions.Binomial", "", FALSE,
      "pnbinom", "Distributions.cdf.", "syntaxD", "Distributions.NegativeBinomial", "", FALSE,
      "dnbinom", "Distributions.pdf.", "syntaxD", "Distributions.NegativeBinomial", "", FALSE,
      "qnbinom", "Distributions.quantile.", "syntaxD", "Distributions.NegativeBinomial", "", FALSE,
      "pgamma", "Distributions.cdf.", "syntaxD", "Distributions.Gamma", "", FALSE,
      "dgamma", "Distributions.pdf.", "syntaxD", "Distributions.Gamma", "", FALSE,
      "qgamma", "Distributions.quantile.", "syntaxD", "Distributions.Gamma", "", FALSE,
      "pbeta", "Distributions.cdf.", "syntaxD", "Distributions.Beta", "", FALSE,
      "dbeta", "Distributions.pdf.", "syntaxD", "Distributions.Beta", "", FALSE,
      "qbeta", "Distributions.quantile.", "syntaxD", "Distributions.Beta", "", FALSE,
      "pcauchy", "Distributions.cdf.", "syntaxD", "Distributions.Cauchy", "", FALSE,
      "dcauchy", "Distributions.pdf.", "syntaxD", "Distributions.Cauchy", "", FALSE,
      "qcauchy", "Distributions.quantile.", "syntaxD", "Distributions.Cauchy", "", FALSE,
      "pgeom", "Distributions.cdf.", "syntaxD", "Distributions.Geometric", "", FALSE,
      "dgeom", "Distributions.pdf.", "syntaxD", "Distributions.Geometric", "", FALSE,
      "qgeom", "Distributions.quantile.", "syntaxD", "Distributions.Geometric", "", FALSE,
      "dmultinom", "Distributions.pdf.", "syntaxD", "Distributions.Multinomial", "", FALSE,
      "pweibull", "Distributions.cdf.", "syntaxD", "Distributions.Weibull", "", FALSE,
      "dweibull", "Distributions.pdf.", "syntaxD", "Distributions.Weibull", "", FALSE,
      "qweibull", "Distributions.quantile.", "syntaxD", "Distributions.Weibull", "", FALSE,
      "pt", "Distributions.cdf.", "syntaxD", "Distributions.TDist", "", FALSE,
      "dt", "Distributions.pdf.", "syntaxD", "Distributions.TDist", "", FALSE,
      "qt", "Distributions.quantile.", "syntaxD", "Distributions.TDist", "", FALSE,
      "pf", "Distributions.cdf.", "syntaxD", "Distributions.FDist", "", FALSE,
      "df", "Distributions.pdf.", "syntaxD", "Distributions.FDist", "", FALSE,
      "qf", "Distributions.quantile.", "syntaxD", "Distributions.FDist", "", FALSE,
      "pchisq", "Distributions.cdf.", "syntaxD", "Distributions.Chisq", "", FALSE,
      "dchisq", "Distributions.pdf.", "syntaxD", "Distributions.Chisq", "", FALSE,
      "qchisq", "Distributions.quantile.", "syntaxD", "Distributions.Chisq", "", FALSE,
      "pexp", "Distributions.cdf.", "syntaxD", "Distributions.Exponential", "", FALSE,
      "dexp", "Distributions.pdf.", "syntaxD", "Distributions.Exponential", "", FALSE,
      "qexp", "Distributions.quantile.", "syntaxD", "Distributions.Exponential", "", FALSE,
      "ppois", "Distributions.cdf.", "syntaxD", "Distributions.Poisson", "", FALSE,
      "dpois", "Distributions.pdf.", "syntaxD", "Distributions.Poisson", "", FALSE,
      "qpois", "Distributions.quantile.", "syntaxD", "Distributions.Poisson", "", FALSE,


      # Complete replacements
      "next", "continue", "syntax0", "", "", FALSE,
      "stop", "error", "syntax0", "", "", FALSE
    ),



    # http://adv-r.had.co.nz/Vocabulary.html
    # str
    #
    # # Important operators and assignment
    # %in%, match
    # =, <-, <<-
    #   $, [, [[, head, tail, subset
    #           with
    #           assign, get
    #
    #           # Comparison
    #           all.equal, identical
    #           !=, ==, >, >=, <, <=
    #             is.na, complete.cases
    #           is.finite
    #
    #           # Basic math
    #           *, +, -, /, ^, %%, %/%

    #         signif
    #
    #           rle
    #
    #           # Functions to do with functions
    #           missing
    #           on.exit
    #           return, invisible
    #
    #           # Logical & sets
    #           &, |, !, xor
    #           which
    #
    #           # Vectors and matrices
    #           c, matrix
    #           # automatic coercion rules character > numeric > logical
    #           length, dim, ncol, nrow
    #           cbind, rbind
    #           names, colnames, rownames
    #           sweep
    #           as.matrix, data.matrix
    #
    #           # Making vectors
    #           c
    #           rep, rep_len
    #           seq, seq_len, seq_along
    #           sample
    #           choose, factorial, combn
    #           (is/as).(character/numeric/logical/...)
    #
    #           # Lists & data.frames
    #           list, unlist
    #           data.frame, as.data.frame
    #           split
    #           expand.grid
    #
    #           # Control flow
    #           if, &&, || (short circuiting)
    #           for, while
    #           next, break
    #           switch
    #
    #           # Apply & friends
    #           lapply, sapply, vapply
    #           apply
    #           tapply
    #           replicate
    #
    #           # Date time
    #           ISOdate, ISOdatetime, strftime, strptime, date
    #           difftime
    #           julian, months, quarters, weekdays
    #           library(lubridate)
    #
    #           # Character manipulation
    #           grep, agrep
    #           gsub
    #           strsplit
    #           chartr
    #           substr
    #           paste
    #           trimws
    #           library(stringr)
    #
    #           # Factors
    #           factor, levels, nlevels
    #           reorder, relevel
    #           cut, findInterval
    #           interaction
    #           options(stringsAsFactors = FALSE)
    #
    #           # Array manipulation
    #           array
    #           dim
    #           dimnames
    #           aperm
    #           library(abind)
    #
    #           # Ordering and tabulating
    #           duplicated, unique
    #           merge
    #           order, rank, quantile
    #           table, ftable
    #
    #
    #
    #           # Random variables
    #           (q, p, d, r) * (beta, binom, cauchy, chisq, exp, f, gamma, geom,
    #                           hyper, lnorm, logis, multinom, nbinom, norm, pois, signrank, t,
    #                           unif, weibull, wilcox, birthday, tukey)
    #
    #           # Matrix algebra
    #           crossprod, tcrossprod
    #           eigen, qr, svd
    #           %*%, %o%, outer
    #           rcond
    #           solve
    ncol = 6, byrow = TRUE,
    dimnames = list(NULL, c("R", "julia", "syntax", "add_first_arg", "add_second_arg", "add_broadcast"))
  )

  # Convert to dataframe
  conv_df <- as.data.frame(conv_df, stringsAsFactors = FALSE)

  conv_df

  # **to do: add optionally that someone uses Base::


  # Create syntax_df by copying conv_df
  syntax_df <- conv_df

  # Add and modify columns
  syntax_df[["R_first_iter"]] <- syntax_df[["R"]]
  syntax_df[["R_regex_first_iter"]] <- ifelse(
    syntax_df[["syntax"]] == "syntax0",
    paste0("(?<!\\.)\\b", syntax_df[["R"]], "\\b"),
    paste0("(?<!\\.)\\b", syntax_df[["R"]], "\\(")
  )
  syntax_df[["R"]] <- paste0(syntax_df[["R"]], "_replace")
  syntax_df[["R_regex"]] <- ifelse(
    syntax_df[["syntax"]] == "syntax0",
    paste0("(?<!\\.)\\b", syntax_df[["R"]], "\\b"),
    paste0("(?<!\\.)\\b", syntax_df[["R"]], "\\(")
  )

  return(list(syntax_df = syntax_df, conv_df = conv_df))
}



#' Convert R built-in functions to Julia
#'
#' @return List with transformed eqn and list with additional R code needed to make the eqn function
#' @inheritParams convert_equations_IM
#' @noRd
#' @importFrom rlang .data
#'
convert_builtin_functions_julia <- function(type, name, eqn, var_names) {
  add_Rcode <- list(func = list())

  if (grepl("[[:alpha:]]", eqn)) {
    # Dataframe with regular expressions for each built-in Insight Maker function
    out <- get_syntax_julia()
    syntax_df <- out[["syntax_df"]]
    conv_df <- out[["conv_df"]]

    # Preparation for first iteration
    done <- FALSE
    i <- 1
    R_regex <- syntax_df[["R_regex_first_iter"]]

    while (!done) {
      # Remove those matches that are in quotation marks or names
      idxs_exclude <- get_seq_exclude(eqn, var_names)

      # Update location indices of functions in eqn
      idx_df <- lapply(seq_along(R_regex), function(i) {
        matches <- gregexpr(R_regex[i], eqn, perl = TRUE, ignore.case = FALSE)[[1]]

        if (matches[1] == -1) {
          return(NULL) # Return NULL instead of empty data.frame
        } else {
          # Use cbind instead of dplyr::bind_cols for speed
          cbind(
            syntax_df[rep(i, length(matches)), , drop = FALSE],
            data.frame(
              start = as.integer(matches),
              end = as.integer(matches + attr(matches, "match.length") - 1)
            )
          )
        }
      })

      # Remove NULL entries
      idx_df <- idx_df[!sapply(idx_df, is.null)]

      if (length(idx_df) == 0) {
        done <- TRUE
        next
      }

      idx_df <- do.call(rbind, idx_df)

      if (nrow(idx_df) > 0) idx_df <- idx_df[!(idx_df[["start"]] %in% idxs_exclude | idx_df[["end"]] %in% idxs_exclude), ]
      if (nrow(idx_df) == 0) {
        done <- TRUE
        next
      }


      # For the first iteration, add _replace to all detected functions, so we don't end in an infinite loop (some Julia and R functions have the same name)
      if (i == 1 & nrow(idx_df) > 0) {
        idx_df <- idx_df[order(idx_df[["start"]]), ]
        idx_df[["R_regex"]] <- stringr::str_replace_all(
          idx_df[["R_regex"]],
          stringr::fixed(c("(?<!\\.)\\b" = "", "\\(" = "(", "\\)" = ")"))
        )

        for (j in rev(1:nrow(idx_df))) {
          stringr::str_sub(eqn, idx_df[j, "start"], idx_df[j, "end"]) <- idx_df[j, ][["R_regex"]] #|>
        }
      }

      if (i == 1) {
        # print(eqn)
        R_regex <- syntax_df[["R_regex"]]
        i <- i + 1
        # Stop first iteration
        next
      }


      if (nrow(idx_df) == 0) {
        done <- TRUE
      } else {
        # To find the arguments within round brackets, find all indices of matching '', (), [], c()
        paired_idxs <- get_range_all_pairs(eqn, var_names, add_custom = "paste0()")
        paired_idxs

        # If there are brackets in the eqn:
        if (nrow(paired_idxs) > 0) {
          # Match the opening bracket of each function to round brackets in paired_idxs
          idx_funcs <- merge(
            paired_idxs[paired_idxs[["type"]] == "round", ],
            idx_df,
            by.x = "start",
            by.y = "end"
          )
          idx_funcs[["start_bracket"]] <- idx_funcs[["start"]]
          idx_funcs[["start"]] <- idx_funcs[["start.y"]]


          df2 <- idx_df[idx_df[["syntax"]] == "syntax1b", ]
          # Add start_bracket column to prevent errors
          df2[["start_bracket"]] <- df2[["start"]]
          # Add back syntax1b which does not need brackets
          idx_funcs <- dplyr::bind_rows(idx_funcs, df2)
          idx_funcs <- idx_funcs[order(idx_funcs[["end"]]), ]
          idx_funcs
        } else {
          # If there are no brackets in the eqn:
          idx_funcs <- idx_df
          # Add start_bracket column to prevent errors
          idx_funcs[["start_bracket"]] <- idx_funcs[["start"]]
        }

        # Start with most nested function
        idx_funcs_ordered <- idx_funcs
        idx_funcs_ordered[["is_nested_around"]] <- any(idx_funcs_ordered[["start"]] < idx_funcs[["start"]] &
          idx_funcs_ordered[["end"]] > idx_funcs[["end"]])
        idx_funcs_ordered <- idx_funcs_ordered[order(idx_funcs_ordered[["is_nested_around"]]), ]
        idx_func <- idx_funcs_ordered[1, ] # Select first match

        if (.sdbuildR_env[["P"]][["debug"]]) {
          print("idx_func")
          print(idx_func)
        }

        # Extract argument between brackets (excluding brackets)
        bracket_arg <- stringr::str_sub(eqn, idx_func[["start_bracket"]] + 1, idx_func[["end"]] - 1)

        # print("bracket_arg")
        # print(bracket_arg)

        arg <- parse_args(bracket_arg)
        named_arg <- sort_args(arg, idx_func[["R_first_iter"]], var_names = var_names)
        arg <- unname(unlist(named_arg))

        # print("arg")
        # print(arg)

        # Indices of replacement in eqn
        start_idx <- idx_func[["start"]]
        end_idx <- idx_func[["end"]]

        if (idx_func[["syntax"]] == "syntax0") {
          # arg = paste0(arg, collapse = ", ")
          replacement <- idx_func[["julia"]]
        } else if (idx_func[["syntax"]] == "syntax1") {
          arg <- paste0(arg, collapse = ", ")

          replacement <- sprintf(
            "%s%s(%s%s%s%s%s)",
            idx_func[["julia"]],
            ifelse(idx_func[["add_broadcast"]], ".", ""),
            idx_func[["add_first_arg"]],
            ifelse(nzchar(idx_func[["add_first_arg"]]) & nzchar(arg), ", ", ""),
            arg,
            idx_func[["add_second_arg"]],
            ifelse(nzchar(idx_func[["add_second_arg"]]) & nzchar(arg), ", ", "")
          )
        } else if (idx_func[["syntax"]] == "delay") {
          if (type %in% c("stock", "gf", "constant", "macro")) {
            stop(paste0("Adjust equation of ", name, ": delay() cannot be used for a ", type, "."))
          }

          # Check arguments
          arg[2] <- trimws(arg[2])
          if (arg[2] == "0" || arg[2] == "0.0" || arg[2] == "0L") {
            stop(paste0("Adjust equation of ", name, ": the delay length in delay() must be greater than 0."))
          }

          func_name <- paste0(name, .sdbuildR_env[["P"]][["delay_suffix"]], length(add_Rcode[["func"]][[idx_func[["syntax"]]]]) + 1)
          arg3 <- ifelse(length(arg) > 2, arg[3], "nothing")

          replacement <- paste0(
            idx_func[["julia"]], "(",
            arg[1], ", ",
            arg[2], ", ",
            arg3, ", ",
            .sdbuildR_env[["P"]][["time_name"]],
            # Symbols are faster
            ", :", arg[1],
            ", ",
            .sdbuildR_env[["P"]][["intermediaries"]], ", ",
            .sdbuildR_env[["P"]][["model_setup_name"]], ".",
            .sdbuildR_env[["P"]][["intermediary_names"]], ")"
          )

          add_Rcode[["func"]][[idx_func[["syntax"]]]][[func_name]] <- list(
            var = arg[1],
            length = arg[2],
            initial = arg3
          )
        } else if (idx_func[["syntax"]] == "past") {
          if (type %in% c("stock", "gf", "constant", "macro")) {
            stop(paste0("Adjust equation of ", name, ": past() cannot be used for a ", type, "."))
          }

          # Check arguments
          arg[2] <- trimws(arg[2])
          if (arg[2] == "0" || arg[2] == "0.0" || arg[2] == "0L") {
            stop(paste0("Adjust equation of ", name, ": the past interval in past() must be greater than 0."))
          }

          arg2 <- ifelse(length(arg) > 1, arg[2], "nothing")
          func_name <- paste0(name, .sdbuildR_env[["P"]][["past_suffix"]], length(add_Rcode[["func"]][[idx_func[["syntax"]]]]) + 1)
          replacement <- paste0(
            idx_func[["julia"]], "(",
            arg[1], ", ",
            arg2, ", nothing, ",
            .sdbuildR_env[["P"]][["time_name"]],
            # Symbols are faster
            ", :", arg[1],
            ", ",
            .sdbuildR_env[["P"]][["intermediaries"]], ", ",
            .sdbuildR_env[["P"]][["model_setup_name"]], ".",
            .sdbuildR_env[["P"]][["intermediary_names"]], ")"
          )
          add_Rcode[["func"]][[idx_func[["syntax"]]]][[func_name]] <- list(
            var = arg[1],
            length = arg2
          )
        } else if (idx_func[["syntax"]] == "delayN") {
          if (type %in% c("stock", "gf", "constant", "macro")) {
            stop(paste0("Adjust equation of ", name, ": delayN() cannot be used for a ", type, "."))
          }

          # Check arguments
          arg[2] <- trimws(arg[2])
          if (arg[2] == "0" || arg[2] == "0.0" || arg[2] == "0L") {
            stop(paste0("Adjust equation of ", name, ": the delay length in delayN() must be greater than 0."))
          }

          if (arg[3] == "0" || arg[3] == "0.0" || arg[3] == "0L") {
            stop(paste0("Adjust equation of ", name, ": the delay order in delayN() must be greater than 0."))
          }

          arg4 <- ifelse(length(arg) > 3, arg[4], arg[1])

          # Number delayN() as there may be multiple
          func_name <- paste0(name, .sdbuildR_env[["P"]][["delayN_suffix"]], length(add_Rcode[["func"]][[idx_func[["syntax"]]]]) + 1)

          replacement <- paste0(func_name, .sdbuildR_env[["P"]][["outflow_suffix"]])
          setup <- paste0(
            "setup_delayN(", arg4, ", ", arg[2], ", ", arg[3],
            # ", \"", func_name, "\")")
            # Symbols are faster
            ", :", func_name, ")"
          )
          compute <- paste0(
            idx_func[["julia"]], "(",
            arg[1], ", ",
            func_name, ", ",
            arg[2], ", ",
            arg[3], ")"
          )

          update <- paste0(func_name, ".update")

          add_Rcode[["func"]][[idx_func[["syntax"]]]][[func_name]] <- list(
            name = name,
            setup = setup,
            compute = compute,
            update = update,
            type = idx_func[["julia"]],
            var = arg[1],
            length = arg[2],
            order = arg[3],
            initial = arg4
          )
        } else if (idx_func[["syntax"]] == "smoothN") {
          if (type %in% c("stock", "gf", "constant", "macro")) {
            stop(paste0("Adjust equation of ", name, ": smoothN() cannot be used for a ", type, "."))
          }

          # Check arguments
          arg[2] <- trimws(arg[2])
          if (arg[2] == "0" || arg[2] == "0.0" || arg[2] == "0L") {
            stop(paste0("Adjust equation of ", name, ": the smoothing time in smoothN() must be greater than 0."))
          }

          arg[3] <- trimws(arg[3])
          if (arg[3] == "0" || arg[3] == "0.0" || arg[3] == "0L") {
            stop(paste0("Adjust equation of ", name, ": the smoothing order in smoothN() must be greater than 0."))
          }

          arg4 <- ifelse(length(arg) > 3, arg[4], arg[1])

          # Number smoothN() as there may be multiple
          func_name <- paste0(name, .sdbuildR_env[["P"]][["smoothN_suffix"]], length(add_Rcode[["func"]][[idx_func[["syntax"]]]]) + 1)

          replacement <- paste0(func_name, .sdbuildR_env[["P"]][["outflow_suffix"]])
          setup <- paste0(
            "setup_smoothN(", arg4, ", ", arg[2], ", ", arg[3],
            # ", \"", func_name, "\")")
            # Symbols are faster
            ", :", func_name, ")"
          )
          compute <- paste0(
            idx_func[["julia"]], "(",
            arg[1], ", ",
            func_name, ", ",
            arg[2], ", ",
            arg[3], ")"
          )

          update <- paste0(func_name, ".update")

          add_Rcode[["func"]][[idx_func[["syntax"]]]][[func_name]] <- list(
            name = name,
            setup = setup,
            compute = compute,
            update = update,
            type = idx_func[["julia"]],
            var = arg[1],
            length = arg[2],
            order = arg[3],
            initial = arg4
          )
        } else if (idx_func[["syntax"]] == "syntaxD") {
          # Convert random number generation
          replacement <- conv_distribution(
            arg,
            idx_func[["julia"]],
            idx_func[["add_first_arg"]]
          )
        } else if (idx_func[["syntax"]] == "syntax_seq") {
          # Convert sequence
          replacement <- conv_seq(
            named_arg,
            idx_func[["R_first_iter"]],
            idx_func[["julia"]]
          )
        } else if (idx_func[["syntax"]] == "syntax_sample") {
          # Convert sequence
          replacement <- conv_sample(
            named_arg,
            idx_func[["R_first_iter"]],
            idx_func[["julia"]]
          )
        }

        if (.sdbuildR_env[["P"]][["debug"]]) {
          print(stringr::str_sub(eqn, start_idx, end_idx))
          print(replacement)
          print("")
        }

        # Replace eqn
        stringr::str_sub(eqn, start_idx, end_idx) <- replacement
      }
    }
  }
  return(list(eqn = eqn, add_Rcode = add_Rcode))
}





#' Sort arguments in function call according to default order
#'
#' @param arg Vector with arguments in strings
#' @param func_name String with name of R function
#' @param default_arg Either NULL or named list of default arguments
#' @inheritParams convert_builtin_functions_julia
#'
#' @noRd
#' @returns List with named and sorted arguments
#'
sort_args <- function(arg, func_name, default_arg = NULL, var_names = NULL) {
  # If default arguments are not provided, assume func_name is an R function
  if (is.null(default_arg)) {
    # Find default arguments of R function
    # Assume Julia and R arguments are the same, with the same order
    default_arg <- do.call(formals, list(func_name)) |> as.list()
    varargs <- any(names(default_arg) == "...")
    default_arg <- default_arg[names(default_arg) != "..."] # Remove ellipsis

    # formals(seq) is empty for some reason
    if (func_name == "seq") {
      default_arg <- list(
        "from" = "1.0", "to" = "1.0", "by" = NULL,
        "length.out" = NULL, "along.with" = NULL
      )
    } else if (func_name == "seq_along") {
      default_arg <- list("along.with" = "NULL")
    } else if (func_name == "seq_len") {
      default_arg <- list("length.out" = "1.0")
    }

    # default_arg_Julia = JuliaCall::julia_eval(sprintf("using Distributions; params(%s())", Julia_func))
  }

  # Find names and values of arguments
  contains_name <- stringr::str_detect(arg, "=")
  arg_split <- stringr::str_split_fixed(arg, "=", n = 2)
  names_arg <- trimws(ifelse(contains_name, arg_split[, 1], NA))
  values_arg <- trimws(ifelse(contains_name, arg_split[, 2], arg_split[, 1]))

  # For some functions, there are no default arguments, so there is no need to sort them
  if (length(default_arg) == 0) {
    arg_R <- stats::setNames(values_arg, names_arg)
  } else {
    # Check whether all argument names are in the allowed argument names in case of no dots argument (...)
    idx <- !names_arg %in% names(default_arg) & !is.na(names_arg)
    if (!varargs & any(idx)) {
      stop(paste0(
        "Argument",
        ifelse(sum(idx) > 1, "s ", " "),
        paste0(names_arg[idx], collapse = ", "),
        ifelse(sum(idx) > 1, " are", " is"),
        " not allowed for function ", func_name, ". Allowed arguments: ",
        paste0(names(default_arg), collapse = ", "), "."
      ))
    }

    # Check if there are too many arguments
    if (!varargs & length(arg) > length(default_arg)) {
      stop(paste0(
        "Too many arguments for function ", func_name, ". Allowed arguments: ",
        paste0(names(default_arg), collapse = ", "), "."
      ))
    }

    # Add names to unnamed arguments; note that R can mix named and default arguments, e.g. runif(max = 10, 20, min = 1). Julia cannot if they're not keyword arguments!
    idx <- which(!contains_name & nzchar(values_arg)) # Find unnamed arguments which have values
    standard_order <- names(default_arg)
    if (length(idx) > 0 && length(standard_order) > 0) {
      new_names <- setdiff(standard_order, stats::na.omit(names_arg)) # names which are missing from the passed argument names
      names_arg[idx] <- new_names[1:length(idx)] # Assign new names to unnamed arguments; only select as many as there are unnamed arguments
    }


    # Check for missing obligatory arguments
    # obligatory arguments without a default (class == "name" or is.symbol, e.g. n in formals(rnorm) is a symbol)
    obligatory_args <- unlist(lapply(default_arg, is.symbol))
    idx <- !names(default_arg[obligatory_args]) %in% names_arg

    if (any(idx)) {
      stop(paste0(
        "Obligatory argument",
        ifelse(sum(idx) > 1, "s ", " "),
        paste0(names(default_arg[obligatory_args])[idx], collapse = ", "),
        ifelse(sum(idx) > 1, " are", " is"),
        " missing for function ", func_name, "."
      ))
    }

    # Overwrite default arguments with specified arguments & remove NULL arguments
    default_arg_list <- default_arg[!obligatory_args | unlist(lapply(default_arg, is.null))]
    arg_R <- utils::modifyList(default_arg_list, as.list(stats::setNames(values_arg, names_arg)))

    # Sort order of arguments according to default order
    order_arg <- c(names(default_arg), setdiff(names(arg_R), names(default_arg)))
    arg_R <- arg_R[order_arg]

    # print("arg_R")
    # print(arg_R)

    # Check if any of the arguments are calls - these will need to be evaluated
    if (any(sapply(arg_R, class) == "call")) {
      arg_R_num <- lapply(arg_R, function(x) {
        if (!is.call(x)) {
          if (!grepl("'|\"", x) & !is.na(suppressWarnings(as.numeric(x)))) {
            x <- as.numeric(x)
          }
        }
        return(x)
      })

      # Parse in case of default arguments like scale = 1/rate
      for (name in names(arg_R)) {
        if (is.language(arg_R[[name]]) && !is.name(arg_R[[name]])) {
          # Evaluate the expression in the context of merged_args
          env <- list2env(arg_R_num, parent = baseenv())
          # arg_R[[name]] <- eval(arg_R_correct_class[[name]], envir = env)

          # Substitute values into the expression
          arg_R[[name]] <- deparse(eval(bquote(substitute(.(arg_R[[name]]), env))))
        }
      }
    }

    # Ensure digits become floats for Julia
    for (name in names(arg_R)) {
      if (!is.null(arg_R[[name]])) {
        arg_R[[name]] <- replace_digits_with_floats(arg_R[[name]], var_names)
      }
    }
  }

  arg_R <- lapply(arg_R, as.character)

  return(arg_R)
}




#' Convert random number generation in R to Julia
#'
#' @inheritParams sort_args
#' @param julia_func String with Julia function
#' @param distribution String with Julia distribution call
#'
#' @returns String with Julia code
#' @noRd
#'
conv_distribution <- function(arg, julia_func, distribution) {
  # If n = 1, don't include it, as rand(..., 1) generates a vector. n is the first argument.

  julia_str <- sprintf(
    "%s(%s(%s), %d)",
    julia_func, distribution,
    # Don't include names of arguments
    paste0(arg[-1], collapse = ", "), as.numeric(arg[1])
  )

  if (as.numeric(arg[1]) == 1 & julia_func == "rand") {
    julia_str <- sprintf(
      "%s(%s(%s))",
      julia_func, distribution,
      # Don't include names of arguments
      paste0(arg[-1], collapse = ", ")
    )
  } else if (julia_func == "Distributions.cdf.") {
    # log = TRUE
    if (arg[length(arg)] == "TRUE") {
      julia_str <- sprintf(
        "log%s(%s(%s), %d)",
        julia_func, distribution,
        # Don't include names of arguments; skip log
        paste0(arg[-c(1, length(arg) - 1, length(arg))], collapse = ", "), as.numeric(arg[1])
      )
    } else {
      julia_str <- sprintf(
        "%s(%s(%s), %d)",
        julia_func, distribution,
        # Don't include names of arguments; skip log
        paste0(arg[-c(1, length(arg) - 1, length(arg))], collapse = ", "), as.numeric(arg[1])
      )
    }
  } else if (julia_func == "Distributions.pdf.") {
    # log.p = TRUE
    if (arg[length(arg)] == "TRUE") {
      julia_str <- sprintf(
        "log%s(%s(%s), %d)",
        julia_func, distribution,
        # Don't include names of arguments; skip lower.tail and log.p
        paste0(arg[-c(1, length(arg))], collapse = ", "), as.numeric(arg[1])
      )
    } else {
      julia_str <- sprintf(
        "%s(%s(%s), %d)",
        julia_func, distribution,
        # Don't include names of arguments; skip lower.tail and log.p
        paste0(arg[-c(1, length(arg))], collapse = ", "), as.numeric(arg[1])
      )
    }
  } else if (julia_func == "Distributions.quantile.") {
    # log = TRUE
    if (arg[length(arg)] == "TRUE") {
      julia_str <- sprintf(
        "invlogcdf(%s(%s), %d)",
        distribution,
        # Don't include names of arguments; skip lower.tail and log.p
        paste0(arg[-c(1, length(arg) - 1, length(arg))], collapse = ", "), as.numeric(arg[1])
      )
    } else {
      julia_str <- sprintf(
        "%s(%s(%s), %d)",
        julia_func, distribution,
        # Don't include names of arguments; skip lower.tail and log.p
        paste0(arg[-c(1, length(arg) - 1, length(arg))], collapse = ", "), as.numeric(arg[1])
      )
    }
  }


  return(julia_str)
}


#' Convert sequence in R to Julia
#'
#' @inheritParams sort_args
#' @param R_func String with R function, e.g. "seq", "seq_along"
#' @param julia_func String with Julia function
#'
#' @returns String with Julia code
#' @noRd
#'
conv_seq <- function(arg, R_func, julia_func) {
  # print("arg in conv_seq")
  # print(arg)

  if (R_func == "seq_along") {
    julia_str <- paste0(julia_func, "(1.0, length(", arg[["along.with"]], "))")
  } else if (R_func == "seq_len") {
    julia_str <- paste0(julia_func, "(1.0, ", arg[["length.out"]], ")")
  } else {
    # If nothing is specified, specify by
    if (!is_defined(arg[["by"]]) && !is_defined(arg[["length.out"]]) && !is_defined(arg[["along.with"]])) {
      arg[["by"]] <- "1.0" # Default value for by
    }

    if (is_defined(arg[["by"]])) {
      julia_str <- sprintf(
        "%s(%s, %s, step=%s)",
        julia_func, arg[["from"]], arg[["to"]], arg[["by"]]
      )
    } else if (is_defined(arg[["length.out"]])) {
      # Julia throws an error in this case
      if (as.numeric(arg[["length.out"]]) == 1 & as.numeric(arg[["from"]]) != as.numeric(arg[["to"]])) {
        julia_str <- arg[["from"]]
      } else {
        # length.out should be an integer
        julia_str <- sprintf(
          "%s(%s, %s, round_(%s))",
          julia_func, arg[["from"]], arg[["to"]], arg[["length.out"]]
        )
      }
    } else if (is_defined(arg[["along.with"]])) {
      julia_str <- sprintf(
        "%s(%s, %s, length(%s))",
        julia_func, arg[["from"]], arg[["to"]], arg[["along.with"]]
      )
    }
  }

  return(julia_str)
}



#' Convert R sample() to Julia StatsBase.sample()
#'
#' @inheritParams conv_seq
#'
#' @returns String with Julia code
#' @noRd
conv_sample <- function(arg, R_func, julia_func) {
  # print("arg in conv_sample")
  # print(arg)

  # Order in StatsBase.sample() is different
  if (R_func == "sample.int") {
    arg[["x"]] <- paste0("seq(1.0, ", arg[["n"]], ")")
  }

  arg[["replace"]] <- ifelse(tolower(arg[["replace"]]) == "true", "true", "false")

  if (is_defined(arg[["prob"]])) {
    julia_str <- sprintf(
      "%s(%s, StatsBase.pweights(%s), round_(%s), replace=%s)",
      julia_func, arg[["x"]], arg[["prob"]], arg[["size"]], arg[["replace"]]
    )
  } else {
    julia_str <- sprintf(
      "%s(%s, round_(%s), replace=%s)",
      julia_func, arg[["x"]], arg[["size"]], arg[["replace"]]
    )
  }

  return(julia_str)
}


#' Translate vector bracket syntax from R to square brackets in Julia
#'
#' @inheritParams convert_equations_IM
#' @return Updated eqn
#' @noRd
#'
vector_to_square_brackets <- function(eqn, var_names) {
  # Get indices of all enclosures
  paired_idxs <- get_range_all_pairs(eqn, var_names,
    type = "vector",
    names_with_brackets = FALSE
  )

  # Remove those that are preceded by a letter
  if (nrow(paired_idxs) > 0) paired_idxs <- paired_idxs[!stringr::str_detect(stringr::str_sub(eqn, paired_idxs[["start"]] - 1, paired_idxs[["start"]] - 1), "[[:alpha:]]"), ]

  if (nrow(paired_idxs) > 0) {
    # First replace all closing brackets with ]
    chars <- strsplit(eqn, "", fixed = TRUE)[[1]]
    chars[paired_idxs[["end"]]] <- "]"
    eqn <- paste0(chars, collapse = "")

    # Order paired_idxs by start position
    paired_idxs <- paired_idxs[order(paired_idxs[["start"]]), ]

    # Replace opening brackets c( with [
    for (j in rev(1:nrow(paired_idxs))) {
      # Replace c( with [
      stringr::str_sub(eqn, paired_idxs[j, "start"], paired_idxs[j, "start"] + 1) <- "["
    }
  }

  return(eqn)
}






#' Remove scientific notation from string
#'
#' @inheritParams convert_equations_IM
#' @param task String with either "remove" or "add" to remove or add scientific notation
#' @param digits_max Number of digits after which to use scientific notation; ignored if task = "remove"; defaults to 15
#'
#' @returns Updated eqn
#' @noRd
#'
scientific_notation <- function(eqn, task = c("remove", "add")[1], digits_max = 15) {
  eqn <- as.character(eqn)

  if (task == "remove") {
    scientific <- FALSE
    # Regex for scientific notation
    pattern <- "-?(?:\\d+\\.?\\d*|\\.\\d+)[eE][+-]?\\d+"
  } else if (task == "add") {
    scientific <- TRUE
    # pattern = "\\d+"
    pattern <- "-?(?:\\d+\\.?\\d*|\\.\\d+)"
  }

  # Function to reformat scientific notation to fixed format
  reformat_scientific <- function(match) {
    # Convert digit match to numeric
    num <- as.numeric(match)
    # print(match)

    # Keep any white space padding
    leading_whitespace <- stringr::str_extract(match, "^[ ]*")
    following_whitespace <- stringr::str_extract(match, "[ ]*$ ")

    # Format to scientific notation if maximum digits are exceeded
    if (task == "add") {
      if (nchar(format(num, scientific = FALSE)) > digits_max) {
        replacement <- paste0(
          ifelse(is.na(leading_whitespace), "", leading_whitespace),
          format(num, scientific = TRUE, trim = TRUE),
          ifelse(is.na(following_whitespace), "", following_whitespace)
        )
      } else {
        # Change nothing otherwise
        replacement <- match
      }
    } else if (task == "remove") {
      replacement <- paste0(
        ifelse(is.na(leading_whitespace), "", leading_whitespace),
        format(num, scientific = FALSE),
        ifelse(is.na(following_whitespace), "", following_whitespace)
      )
    }

    return(replacement) # Convert back to fixed string
  }

  # Replace scientific notation in the string
  eqn <- stringr::str_replace_all(
    eqn,
    pattern = pattern,
    replacement = reformat_scientific
  )

  return(eqn)
}
