

#' Convert all R equations to Julia code
#'
#' @inheritParams build
#' @inheritParams simulate_Julia
#' @inheritParams clean_unit
#'
#' @return Updated sfm
#'
convert_equations_Julia_wrapper = function(sfm, regex_units, debug = TRUE){

  # Get variable names
  # names_df = get_names(sfm)
  var_names = get_model_var(sfm)

  sfm$model$variables = lapply(sfm$model$variables,
                               function(x){
    lapply(x, function(y){
      # print(y$eqn)

      if (is_defined(y$eqn)){
        # y$eqn_Julia = convert_equations_Julia(sfm, y$type, y$name, y$eqn, var_names,
        #                                       regex_units = regex_units,
        #                                       debug = debug)
        out = convert_equations_Julia(sfm, y$type, y$name, y$eqn, var_names,
                                              regex_units = regex_units,
                                              debug = debug)
        y = y %>% utils::modifyList(out)
      }
      # print(y$eqn_Julia)
      return(y)
    })
  })

  # Macros
  sfm$macro = lapply(sfm$macro, function(x){

    # If a name is defined, assign macro to that name (necessary for correct conversion of functions)
    if (nzchar(x$name)){
      x$eqn_Julia = paste0(x$name, " = ", x$eqn)
    } else {
      x$eqn_Julia = x$eqn
    }
    # x$eqn_Julia = convert_equations_Julia(sfm, "macro", "macro", x$eqn_Julia, var_names,
    #                                       regex_units = regex_units, debug = debug)
    out = convert_equations_Julia(sfm, "macro", "macro", x$eqn_Julia, var_names,
                                          regex_units = regex_units, debug = debug)
    x = x %>% utils::modifyList(out)

    return(x)
  })


  if (is_defined(sfm$global$eqn)){
    sfm$global$eqn_Julia = convert_equations_Julia(sfm, "global", "global", sfm$global$eqn, var_names,
                                            regex_units = regex_units, debug = debug)$eqn_Julia
  }

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
#'
convert_equations_Julia = function(sfm, type, name, eqn, var_names, regex_units, debug = TRUE){

  if (debug) {
    # print("")
    # print(type)
    # print(name)
    print(eqn)
  }

  default_out = list(eqn_Julia = "0.0", translated_func = c(),
                     intermediary = c(), func = list())

  # Check whether eqn is empty or NULL
  if (is.null(eqn) || !nzchar(eqn)){
    return(default_out)
  }

  if (eqn == "0" | eqn == "0.0"){
    return(default_out)
  }

  # Try to parse the code
  out = tryCatch(
    {
      parse(text = eqn)
      TRUE
    },
    error = function(e) {
      return(e)
    }
  )

  if ("error" %in% class(out)){
    stop(paste0("Parsing equation of ", name, " failed:\n", out$message))
  }

  if (any(grepl("%%", eqn))){
    stop("The modulus operator a %% b is not supported in sdbuildR. Please use mod(a, b) instead.")
  }

  if (any(grepl("na\\.rm", eqn))){
    stop("na.rm is not supported as an argument in sdbuildR. Please use na.omit(x) instead.")
  }

  # Remove comments we don't keep these
  eqn = remove_comments(eqn)$eqn

  # If equation is now empty, don't run rest of functions but set equation to zero
  if (!nzchar(eqn) | eqn == "0" | eqn == "0.0"){
    return(default_out)
  } else {

    # Ensure there is no scientific notation
    eqn = scientific_notation(eqn)

    # Step 2. Syntax (bracket types, destructuring assignment, time units {1 Month})
    eqn = eqn %>%
      # Translate vector brackets, i.e. c() -> []
      vector_to_square_brackets(., var_names) %>%
      # Ensure integers are floats
      # Julia can throw InexactError errors in case e.g. an initial condition is defined as an integer
      replace_digits_with_floats(., var_names)


    # # Destructuring assignment, e.g. x, y <- {a, b}
    # **to do
    # conv_destructuring_assignment()


    # print("eqn")
    # print(eqn)

    # Step 3. Statements (if, for, while, functions, try)
    eqn = convert_all_statements_Julia(eqn, var_names, debug)

    # Step 4. Operators (booleans, logical operators, addition of strings)
    eqn = eqn %>%
      # # Convert addition of strings to paste0
      # conv_addition_of_strings(., var_names) %>%
      # # Replace logical operators (true, false, = (but not if in function()))
      replace_op_Julia(., var_names) #%>%
    # # Replace range, e.g. range(0, 10, 2) -> 0:2:10
    # replace_range_Julia(., var_names)

    # Step 5. Replace R functions to Julia functions
    conv_list = convert_builtin_functions_Julia(type, name, eqn, var_names, debug = debug)
    eqn = conv_list$eqn
    add_Rcode = conv_list$add_Rcode
    translated_func = conv_list$translated_func


    # **to do:
    #     <<- --> global
    # <- -> =

    # Remove spaces in front of new lines
    eqn = stringr::str_replace_all(eqn, "[ ]*\n", "\n")

    # Replace single with double quotation marks
    eqn = stringr::str_replace_all(eqn, "\'", "\"")

    # Clean units again to ensure no scientific notation is used when necessary; do this at the end to avoid the scientific notation messing up other parts
    eqn = clean_unit_in_u(eqn, regex_units)

    # Units: replace u("") with u""
    eqn = stringr::str_replace_all(eqn, "\\bu\\([\"|'](.*?)[\"|']\\)", "u\"\\1\"")

    # # # If it is a multi-line statement, surround by brackets in case they aren't macros
    # # eqn = trimws(eqn)
    # if (stringr::str_detect(eqn, stringr::fixed("\n")) & !stringr::str_starts(eqn, "begin") & !stringr::str_ends(eqn, "end")) {
    #   eqn = paste0("begin\n", eqn, "\nend")
    # }

    # if (debug){print(eqn)}

    out = list(eqn_Julia = eqn, translated_func = translated_func) %>%
      append(add_Rcode)

    return(out)
  }

}





#' Get indices of digits in string
#'
#' @inheritParams convert_equations_IM
#'
#' @returns Dataframe with start and end indices of digits
#' @export
#'
get_range_digits = function(eqn, var_names){

  # Get indices in variable names or quotations to exclude later
  idxs_exclude = get_seq_exclude(eqn, var_names, names_with_brackets = FALSE)

  # Locate all integers
  # idx_df = stringr::str_locate_all(eqn, "[\\.]*[0-9]+[\\.[0-9]+]*")[[1]] %>% as.data.frame()
  idx_df = stringr::str_locate_all(eqn, "(?<![a-zA-Z0-9\\.:punct:])[0-9]+(?![a-zA-Z0-9\\.:punct:])")[[1]] %>% as.data.frame()

  if (nrow(idx_df) > 0){

    # Remove matches within variable names or quotations
    idx_df = idx_df[!(idx_df$start %in% idxs_exclude | idx_df$end %in% idxs_exclude), ]

    if (nrow(idx_df) > 0){

      # Only keep the digits in these matches
      # keep_idxs = sapply(1:nrow(idx_df), function(i){
      #   # Get substring
      #   sub_formula = stringr::str_sub(eqn, idx_df[i, "start"], idx_df[i, "end"])
      #
      #   # Don't keep if substring doesn't only contain digits
      #   grepl("^[0-9]+$", sub_formula)
      # })
      # idx_df = idx_df[keep_idxs, ]

      # Extract substrings vectorized
      sub_formulas <- stringr::str_sub(eqn, idx_df$start, idx_df$end)

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
#' @export
#'
replace_digits_with_floats = function(eqn, var_names){

  idx_df = get_range_digits(eqn, var_names)

  if (nrow(idx_df) > 0){
    # Replace digit with float in each case
    for (i in rev(idx_df$end)) {
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


# eqn = "c(9,4,5,'8','T','*', 4) + A - B^2 & TRUE + c(F,F,T) "

#' Translate R operators to Julia
#'
#' @inheritParams convert_equations_IM
#' @return Updated eqn
#' @importFrom rlang .data
#'
replace_op_Julia <- function(eqn, var_names) {

  # Define logical operators in R and replacements in Julia
  logical_op_words = c("TRUE" = "true", "FALSE" = "false", "T" = "true", "F" = "false", "NULL" = "nothing", "NA" = "missing")
  # Cannot be preceded or followed by a letter
  names(logical_op_words) = paste0("\\b", stringr::str_escape(names(logical_op_words)), "\\b")


  # **to do: 1 is not true in Julia

  logical_op_signs = c(
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
   "%>%" = " |> ",
  # Matrix algebra
    "%*%" = " * ",
    "%in%" = " in "
  # "%%" = "mod"
  # "$"
  )
  #

  names(logical_op_signs) = paste0("(?<![\\.%])", stringr::str_escape(names(logical_op_signs)))
  logical_op = c(logical_op_words, logical_op_signs)

  # Add additional operators to replace, which require special regex to
  logical_op = c(logical_op,
                 c("(?<!<)-(?!>)" = " .- "),
                 c("(?<!\\.|%)<(?!-|=)" = " .< "),
                 c("(?<!\\.|-|%)>(?!=)" = " .> "),
                 c("(?<!&)&(?!&)" = " && "))

  # Find indices of logical operators
  idxs_logical_op = stringr::str_locate_all(eqn, names(logical_op))
  idxs_logical_op

  if (length(unlist(idxs_logical_op)) > 0){

    # Get match and replacement
    df_logical_op = as.data.frame(do.call(rbind, idxs_logical_op))
    df_logical_op$match = stringr::str_sub(eqn, df_logical_op$start, df_logical_op$end)
    df_logical_op$replacement = rep(unname(logical_op), sapply(idxs_logical_op, nrow))
    df_logical_op = df_logical_op[order(df_logical_op$start), ]
    df_logical_op

    # Remove those that are in quotation marks or names
    idxs_exclude = get_seq_exclude(eqn, var_names)

    if (nrow(df_logical_op) > 0) df_logical_op = df_logical_op[!(df_logical_op$start %in% idxs_exclude | df_logical_op$end %in% idxs_exclude), ]
    # Remove matches that are the same as the logical operator
    if (nrow(df_logical_op) > 0) df_logical_op = df_logical_op[df_logical_op$replacement != df_logical_op$match, ]

    if (nrow(df_logical_op) > 0){
      # Replace in reverse order; no nested functions, so we can replace them in one go
      for (i in rev(1:nrow(df_logical_op))){
        stringr::str_sub(eqn, df_logical_op[i, ]$start, df_logical_op[i, ]$end) = df_logical_op[i, ]$replacement
      }
      # Remove double spaces
      eqn = stringr::str_replace_all(eqn, "[ ]+", " ")
    }

  }

  return(eqn)
}



#' Find all round brackets
#'
#' Helper for convert_all_statements_Julia()
#'
#' @param df Dataframe with indices
#' @param round_brackets Dataframe with indices of round brackets
#' @inheritParams convert_equations_Julia
#'
#' @returns Modified dataframe
#'
find_round = function(df, round_brackets, eqn, var_names){

  statements = c("if", "else if", "for", "while", "function")
  if (df$statement %in% c(statements, toupper(statements))){
    matching = round_brackets[match(df$end, round_brackets$start), ]
    start_round = matching$start
    end_round = matching$end
  } else {
    start_round = end_round = NA
  }
  if (df$statement %in% c("function", "FUNCTION")){

    # Get words before statement
    words = get_words(stringr::str_sub(eqn, 1, df$start - 1))
    if (nrow(words) > 0){
      # Pick last word
      word = words[nrow(words), ]
      start_word = word$start
      end_word = word$end
      func_name = word$word
    }
  } else {
    start_word = end_word = func_name = NA
  }
  return(cbind(df, data.frame(start_round = start_round, end_round = end_round,
                              start_word = start_word, end_word = end_word,
                              func_name = func_name)))
}



#' Find all curly brackets
#'
#' Helper for convert_all_statements_Julia()
#'
#' @param df Dataframe with indices
#' @param paired_idxs Dataframe with indices
#'
#' @returns Modified dataframe
#'
find_curly = function(df, paired_idxs){
  statements = c("if", "else if", "for", "while", "function")
  if (df$statement %in% c(statements, toupper(statements))){
    matching = paired_idxs[which(paired_idxs$start > df$end_round)[1], ]
  } else {
    matching = paired_idxs[which(paired_idxs$start == df$end)[1], ]
  }
  start_curly = matching$start
  end_curly = matching$end
  return(cbind(df, data.frame(start_curly = start_curly, end_curly = end_curly)))
}


#' Convert all statement syntax from R to Julia
#' Wrapper around convert_statement()
#'
#' @inheritParams convert_equations_IM
#'
#' @return Updated eqn
#'
convert_all_statements_Julia = function(eqn, var_names, debug){

  eqn_old = eqn


  # ** to do: R's local() is equivalent to Julia's let end I believe
  # result <- local({
  #   set.seed(1)
  #   v <- rnorm(10, 0, 1)
  #   min(v)
  # })
  # print(result)        # e.g., -1.023122
  # print(exists("v"))   # FALSE


  # If curly brackets surround entire eqn, replace and surround with begin ... end
  if (stringr::str_sub(eqn, 1, 1) == "{" & stringr::str_sub(eqn, nchar(eqn), nchar(eqn)) == "}"){
    stringr::str_sub(eqn, nchar(eqn), nchar(eqn)) = "\nend"
    stringr::str_sub(eqn, 1, 1) = "begin\n"
  }

  # Only if there are curly brackets in the equation, look for statements
  if (grepl("\\{", eqn)){
    done = FALSE
    i = 1 # counter

    # Define regular expressions for statements, accounting for whitespace
    statement_regex = c("for" = "for[ ]*\\(",
                        "if" = "if[ ]*\\(",
                        "while" = "while[ ]*\\(", "else" = "[ ]*else[ ]*\\{",
                        "else if" = "[ ]*else if[ ]*\\(", "function" = "function[ ]*\\(")

    while (!done) {

    # Create sequence of indices of curly brackets; update each iteration
    paired_idxs = get_range_all_pairs(eqn, var_names, type = "curly")

    # Look for statements
    idx_statements = stringr::str_locate_all(eqn, unname(statement_regex))
    df_statements = as.data.frame(do.call(rbind, idx_statements))
    df_statements$statement = rep(names(statement_regex), sapply(idx_statements, nrow))

    # # Remove those matches that are in quotation marks or names
    idxs_exclude = get_seq_exclude(eqn, var_names, type = "quot")
    if (nrow(df_statements) > 0) df_statements = df_statements[!(df_statements$start %in% idxs_exclude | df_statements$end %in% idxs_exclude), ]

    if (!(nrow(paired_idxs) > 0 & nrow(df_statements) > 0)) {
      done = TRUE
    } else {

      # Sort by start index
      paired_idxs = paired_idxs[order(paired_idxs$start), ]

      # Get all round brackets
      round_brackets = get_range_all_pairs(eqn, var_names, type = "round")

      # print(df_statements)

      # Add round brackets and curly brackets to df_statements
      df_statements = df_statements %>%
        dplyr::arrange(.data$start) %>%
        # else if and if will have matching ending characters, choose one with longest statement
        dplyr::group_by(.data$end) %>%
        dplyr::slice_min(.data$start, n = 1) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(id = 1:nrow(.)) %>% dplyr::group_by(.data$id) %>%
        # Find matching round brackets
        dplyr::group_modify(~ find_round(.x, round_brackets, eqn, var_names)) %>%
        # Find matching curly brackets, pass df_statements to check if there is another statement after the curly bracket
        dplyr::group_modify(~ find_curly(.x, paired_idxs)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(lead_start = dplyr::lead(.data$start, default = 0) - 1,
                      # Check if there is another statement after the curly bracket
                      next_statement = dplyr::if_else(.data$end_curly == .data$lead_start,
                                                      dplyr::lead(.data$statement), NA))

      df_statements

      if (nrow(df_statements) == 0) {
        done = TRUE
      } else {

        # # At first iteration, replace all with uppercase versions, as the statement names are the same in R and Julia. This is necessart because someone may have enclosed their if statement etc. in extra round brackets, such that it still matches
        if (i == 1){

          # Replace all statement names with uppercase versions
          for (i in 1:nrow(df_statements)){
            stringr::str_sub(eqn, df_statements[i, "start"], df_statements[i, "end"]) = toupper(stringr::str_sub(eqn, df_statements[i, "start"], df_statements[i, "end"]))
          }
          statement_regex = toupper(statement_regex)
          i = i + 1
          next
        }


        # Start with first pair
        pair = df_statements[1, ]
        pair %>% as.data.frame()

        if (pair$statement %in% c("if")) {
          if (pair$next_statement %in% c("else if", "else")){
            stringr::str_sub(eqn, pair$end_curly, pair$end_curly) = ""
          } else {
            stringr::str_sub(eqn, pair$end_curly, pair$end_curly) = "end"
          }
          stringr::str_sub(eqn, pair$start_curly, pair$start_curly) = ""
          stringr::str_sub(eqn, pair$end_round, pair$end_round) = " "
          stringr::str_sub(eqn, pair$start_round, pair$start_round) = " "
          stringr::str_sub(eqn, pair$start, pair$end-1) = tolower(stringr::str_sub(eqn, pair$start, pair$end-1)) # replace statement, not opening bracket

        } else if (pair$statement %in% c("else if")) {
          if (pair$next_statement %in% c("else if", "else")){
            stringr::str_sub(eqn, pair$end_curly, pair$end_curly) = ""
          } else {
            stringr::str_sub(eqn, pair$end_curly, pair$end_curly) = "end"
          }
          stringr::str_sub(eqn, pair$start_curly, pair$start_curly) = ""
          stringr::str_sub(eqn, pair$end_round, pair$end_round) = " "
          stringr::str_sub(eqn, pair$start, pair$end) = "elseif " # also captures opening round bracket

        } else if (pair$statement %in% c("else")) {
          stringr::str_sub(eqn, pair$end_curly, pair$end_curly) = "end"
          stringr::str_sub(eqn, pair$start_curly, pair$start_curly) = ""
          stringr::str_sub(eqn, pair$start, pair$end-1) = tolower(stringr::str_sub(eqn, pair$start, pair$end-1)) # replace statement, not opening bracket

        } else if (pair$statement %in% c("for", "while")) {
          stringr::str_sub(eqn, pair$end_curly, pair$end_curly) = "end"
          stringr::str_sub(eqn, pair$start_curly, pair$start_curly) = ""
          stringr::str_sub(eqn, pair$end_round, pair$end_round) = " "
          stringr::str_sub(eqn, pair$start_round, pair$start_round) = " "
          stringr::str_sub(eqn, pair$start, pair$end-1) = tolower(stringr::str_sub(eqn, pair$start, pair$end-1)) # replace statement, not opening bracket

        } else if (pair$statement %in% c("function")) {
          stringr::str_sub(eqn, pair$end_curly, pair$end_curly) = "end"
          stringr::str_sub(eqn, pair$start_curly, pair$start_curly) = ""

          # Parse arguments
          arg = parse_args(stringr::str_sub(eqn, pair$start_round+1, pair$end_round-1))

          # All default arguments have to be at the end; if not, throw error
          contains_name = stringr::str_detect(arg, "=")
          arg_split = stringr::str_split_fixed(arg, "=", n = 2)
          names_arg = ifelse(contains_name, arg_split[, 1], NA) %>% trimws()

          # error when there are non-default arguments between default argumens or when default argument is not at the end
          if (any(!is.na(names_arg))){
            if (any(diff(which(!is.na(names_arg))) > 1) | max(which(!is.na(names_arg))) != length(names_arg) ){
            stop(paste0("Please change the function definition of ", pair$func_name, ". All arguments with defaults have to be placed at the end of the function arguments."))
            }

          }

          arg = paste0(arg, collapse = ", ") %>%
            # Varargs (Variable Arguments): , ... -> ...
            stringr::str_replace_all(",[ ]*\\.\\.\\.", "...")

          stringr::str_sub(eqn, pair$start_word, pair$end_round) = paste0("function ", pair$func_name,
                                                                              # # To mimic R's flexibility in positional and keyword arguments, we use keyword arguments for all arguments in Julia
                                                                              # "(;",
                                                                          # For consistency, we use NO keyword arguments for all arguments in Julia, so no ;
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
  idxs_newline = rbind(data.frame(start = 1, end = 1),
                       stringr::str_locate_all(eqn, "\n")[[1]] %>% as.data.frame(),
                       data.frame(start = nchar(eqn) + 1, end = nchar(eqn) + 1))

  # For each new line, find first two words
  x = idxs_newline$end
  pairs = lapply(seq(length(x) - 1), function(i){

    # Get surrounding words
    pair = data.frame(start = x[i], end = x[i + 1] - 1)
    pair$match = stringr::str_sub(eqn, pair$start, pair$end)
    words = get_words(pair$match)
    pair$first_word = ifelse(nrow(words) > 0, words[1,"word"], "")
    pair$second_word = ifelse(nrow(words) > 1, words[2,"word"], "")

    # If second word is function, replace
    if (pair$second_word == "function"){

      pair$match = pair$match %>%
        stringr::str_replace(paste0(pair$second_word, "[ ]*\\("),
                             # Edit: DON'T turn everything into keyword argument
                             paste0(pair$first_word, "(") ) %>%
        # Replace assignment operator too
        stringr::str_replace(paste0(stringr::str_escape(pair$first_word), "[ ]*(=|<-)"),
                             paste0(pair$second_word, " "))

      # A new line needs to be added for Julia after the function name and brackets
      # Get all round brackets
      round_brackets = get_range_all_pairs(pair$match, var_names, type = "round")

      # Find the bracket which opens with ;, insert new line after closing bracket
      # end_idx = round_brackets[which(grepl("^\\(;", round_brackets$match))[1], ]$end

      # Find first opening bracket
      chosen_bracket = round_brackets$start == min(round_brackets$start)
      end_idx = round_brackets[chosen_bracket, ]$end

      # Parse arguments
      arg = parse_args(stringr::str_sub(pair$match, round_brackets[chosen_bracket, "start"] + 1, end_idx - 1))

      # All default arguments have to be at the end; if not, throw error
      contains_name = stringr::str_detect(arg, "=")
      arg_split = stringr::str_split_fixed(arg, "=", n = 2)
      names_arg = ifelse(contains_name, arg_split[, 1], NA) %>% trimws()

      # error when there are non-default arguments between default argumens or when default argument is not at the end
      if (any(!is.na(names_arg))){
        if (any(diff(which(!is.na(names_arg))) > 1) | max(which(!is.na(names_arg))) != length(names_arg) ){
          stop(paste0("Please change the function definition of ", pair$first_word, ". All arguments with defaults have to be placed at the end of the function arguments."))
        }
      }

      stringr::str_sub(pair$match, end_idx, end_idx) = ")\n"

      # # Add end at the end, careful of comments
      # mat = stringr::str_split_fixed(pair$match, "#", n = 2)
      # pair$match = paste0(mat[,1], "\nend ",
      #                     # Add comment sign back in
      #                     ifelse(nzchar(mat[,2]), "#", ""), mat[,2])

      # Add end at the end
      pair$match = paste0(pair$match, "\nend")

    }
    return(pair)

  })

  # eqn = pairs %>% purrr::map_vec("match") %>% paste0(collapse = "")
  eqn = unlist(lapply(pairs, `[[`, "match")) %>% paste0(collapse = "")

  return(eqn)

}





#' Create list of default arguments
#'
#' @param arg List with parsed arguments
#'
#' @returns List with named default arguments
#'
create_default_arg = function(arg){
  # Find names and values of arguments
  contains_value = stringr::str_detect(arg, "=")
  arg_split = stringr::str_split_fixed(arg, "=", n = 2)
  values_arg = ifelse(contains_value, arg_split[, 1], NA) %>% trimws()
  names_arg = ifelse(contains_value, arg_split[, 2], arg_split[, 1]) %>% trimws()


  default_arg = lapply(as.list(stats::setNames(values_arg, names_arg)), as.character)

  return(default_arg)
}



#' Ensure all uses of custom functions have NO keyword arguments and the right order
#'
#' @inheritParams build
#' @inheritParams convert_equations_IM
#'
#' @returns Updated stock-and-flow model
order_arg_in_func_wrapper = function(sfm, var_names){


  # Collect all custom defined functions and their arguments
  all_eqns = c(lapply(sfm$macro, `[[`, "eqn_Julia"),
               sfm$global$eqn_Julia,
               sfm$model$variables %>% purrr::map_depth(2, "eqn_Julia")) %>% unlist() %>%
    unname() %>% paste0(collapse = "\n")
  idxs_exclude = get_seq_exclude(all_eqns, var_names)

  # Get arguments function
  paired_idxs = get_range_all_pairs(all_eqns, var_names, type = "round")


  # Get function name
  paired_idxs$func_name = trimws(stringr::str_match(stringr::str_sub(all_eqns, 1, paired_idxs$start-1) , "function (.*?)$")[,2])
  # Pull arguments from match
  paired_idxs$bracket_arg = stringr::str_replace_all(paired_idxs$match, c("\\(;" = "", "\\)$" = ""))


}



#' Add keyword arguments to custom functions in all equations
#'
#' @inheritParams build
#' @inheritParams convert_equations_IM
#'
#' @returns Updated stock-and-flow model
#'
add_keyword_arg_wrapper = function(sfm, var_names){

  # Replace all function calls with named arguments
  # names_df = get_names(sfm)
  all_eqns = c(lapply(sfm$macro, `[[`, "eqn_Julia"),
               sfm$global$eqn_Julia,
               sfm$model$variables %>% purrr::map_depth(2, "eqn_Julia")) %>% unlist() %>%
    unname() %>% paste0(collapse = "\n")
  idxs_exclude = get_seq_exclude(all_eqns, var_names)

  # Get arguments function
  paired_idxs = get_range_all_pairs(all_eqns, var_names, type = "round")

  if (nrow(paired_idxs) > 0){

    # Select only function definitions
    paired_idxs = paired_idxs[grepl("\\(;", paired_idxs$match), ]

    if (nrow(paired_idxs) > 0){

      # Get function name
      paired_idxs$func_name = trimws(stringr::str_match(stringr::str_sub(all_eqns, 1, paired_idxs$start-1) , "function (.*?)$")[,2])
      # Pull arguments from match
      paired_idxs$bracket_arg = stringr::str_replace_all(paired_idxs$match, c("\\(;" = "", "\\)$" = ""))

      # Create default arguments
      default_arg_list = lapply(1:nrow(paired_idxs), function(i){
          # Parse arguments
          parse_args(paired_idxs[i, "bracket_arg"]) %>%
          create_default_arg(.)}) %>% stats::setNames(paired_idxs$func_name)

      for (i in 1:nrow(paired_idxs)){
        sfm$model$variables =  sfm$model$variables %>% purrr::map_depth(2, function(x){
          if (is_defined(x$eqn_Julia)){
            x$eqn_Julia = add_keyword_arg(x$eqn_Julia, var_names, paired_idxs[i, "func_name"], default_arg_list[[i]])
          }
          return(x)
        })

        sfm$macro = lapply(sfm$macro, function(x){
          x$eqn_Julia = add_keyword_arg(x$eqn_Julia, var_names, paired_idxs[i, "func_name"], default_arg_list[[i]])
          return(x)
        })

        sfm$global$eqn_Julia = add_keyword_arg(sfm$global$eqn_Julia, var_names, paired_idxs[i, "func_name"], default_arg_list[[i]])


      }

    }
  }
  return(sfm)

}



#' Add keyword arguments to custom functions in equation
#'
#' @inheritParams convert_equations_IM
#' @param func_name String with function name
#' @param default_arg List with default arguments
#'
#' @returns Updated eqn
#'
add_keyword_arg = function(eqn, var_names, func_name, default_arg){

  # Find all function calls in eqn
  paired_idxs = get_range_all_pairs(eqn, var_names, add_custom = paste0(func_name, "()"), type = func_name)

  if (nrow(paired_idxs) > 0){

    paired_idxs$bracket_arg = stringr::str_replace_all(paired_idxs$match, paste0("^", func_name, "\\("), "") %>%
                      stringr::str_replace_all(paste0("\\)$"), "")
    # Ensure it doesn't start with ; - this is the function definition
    paired_idxs = paired_idxs[!grepl("^;", paired_idxs$bracket_arg), ]

    if (nrow(paired_idxs) > 0){

      # For each match, extract and sort arguments, and place back with named arguments; reverse order to not mess up the indices
      for (i in rev(seq.int(nrow(paired_idxs)))){
        # Parse arguments
        arg = parse_args(paired_idxs[i, ]$bracket_arg)

        # Sort arguments
        arg_sorted = sort_args(arg, func_name, default_arg = default_arg)

        # Replace in eqn with names
        arg_str = paste0(paste0(names(arg_sorted), " = ", unname(arg_sorted)), collapse = ", ")
        # NA means there is no default value

        stringr::str_sub(eqn, paired_idxs[i, "start"] + nchar(paste0(func_name, "(")), paired_idxs[i, "end"] - 1) = arg_str
      }
    }
  }

  return(eqn)

}



#' Get regular expressions for Julia functions
#'
#' @return Dataframe
get_syntax_Julia = function(){

  # https://www.johnmyleswhite.com/notebook/2012/04/09/comparing-julia-and-rs-vocabularies/
# https://docs.julialang.org/en/v1/manual/noteworthy-differences/

  # Custom function to replace each (nested) function; necessary because regex in stringr unfortunately doesn't seem to handle nested functions
  conv_df = matrix(c(

    # Statistics
    "min", "min", "syntax1", "", "", F,
    "max", "max", "syntax1", "", "", F,
    "pmin", "min", "syntax1", "", "", F,
    "pmax", "max", "syntax1", "", "", F,
    "mean", "Statistics.mean", "syntax1", "", "", F,
    "median", "Statistics.median", "syntax1", "", "", F,
    "prod", "prod", "syntax1", "", "", F, # ** to do: na.rm!
    "sum", "sum", "syntax1", "", "", F,
    "sd", "Statistics.std", "syntax1", "", "", F,
    "cor", "Statistics.cor", "syntax1", "", "", F,
    "cov", "Statistics.cov", "syntax1", "", "", F,
    "var", "Statistics.var", "syntax1", "", "", F,
    "range", "extrema", "syntax1", "", "", F,

    "as.logical", "Bool", "syntax1", "", "" , T,


    # ** mad()
    # Statistics.median(abs.(A .- Statistics.median(A)))


    # **to do: "seq"; Julia:
    # range(start, stop, length)
    # range(start, stop; length, step)
    # range(start; length, stop, step)
    # range(;start, length, stop, step)

    # "shuffle", "", "syntax1", "", "" , T,


    # ** to do: cummax, cummin
    "cumsum", "cumsum", "syntax1", "", "", F,
    "cumprod", "cumprod", "syntax1", "", "", F,
    "diff", "diff", "syntax1", "", "", F,
    "abs", "abs", "syntax1", "", "", T,
    "sign", "sign", "syntax1", "", "", T,

    "cos", "cos", "syntax1", "", "", T,
    "sin", "sin", "syntax1", "", "", T,
    "tan", "tan", "syntax1", "", "", T,
    "acos", "acos", "syntax1", "", "", T,
    "asin", "asin", "syntax1", "", "", T,
    "atan", "atan", "syntax1", "", "", T,
    "cospi", "cospi", "syntax1", "", "", T,
    "sinpi", "sinpi", "syntax1", "", "", T,
    "tanpi", "tanpi", "syntax1", "", "", T,


    "nchar", "length", "syntax1", "", "", F,
    "cor", "cor", "syntax1", "", "", F,
    "floor", "floor", "syntax1", "", "", T,
    "ceiling", "ceil", "syntax1", "", "", T,
    "round", "round", "syntax1", "", "", T,
    "trunc", "trunc", "syntax1", "", "", T,

    # Find
    # "which", "findall", "syntax1", "", "",

    # findmax(arr): Returns (max_value, index).
    # findmin(arr): Returns (min_value, index).

    "which.min", "argmin", "syntax1", "", "", F,
    "which.max", "argmax", "syntax1", "", "", F,

    "exp", "exp", "syntax1", "", "", T,
    "expm1", "expm1", "syntax1", "", "", T,
    # "log", "log", "syntax1", "", "", T, # **to do, put base first!
    # "logb", "logb", "syntax1", "", "", T,
    "log2", "log2", "syntax1", "", "", T,
    "log10", "log10", "syntax1", "", "", T,
    "sqrt", "sqrt", "syntax1", "", "", T,


    "dim", "size", "syntax1", "", "", F,
    "nrow", "size", "syntax1", "", "1", F,
    "ncol", "size", "syntax1", "", "2", F,
    "cbind", "hcat", "syntax1", "", "", F,
    "rbind", "vcat", "syntax1", "", "", F,

    # Matrix functions
    "diag", "LinearAlgebra.diag", "syntax1", "", "", F,
    "upper.tri", "LinearAlgebra.UpperTriangular", "syntax1", "", "", F,
    "lower.tri", "LinearAlgebra.LowerTriangular", "syntax1", "", "", F,
    "norm", "LinearAlgebra.norm", "syntax1", "", "", F,
    "det", "LinearAlgebra.det", "syntax1", "", "", F,

    "t", "transpose", "syntax1", "", "", F,
    "rev", "reverse", "syntax1", "", "", F,
    "print", "println", "syntax1", "", "", F,

    "na.omit", "skipmissing", "syntax1", "", "", F,

    "eigen", "eig", "syntax1", "", "", F,
    "getcd", "getcwd", "syntax1", "", "", F,
    "setwd", "setcwd", "syntax1", "", "", F,


    "Filter", "filter", "syntax1", "", "", T,
    "which", "findall", "syntax1", "", "", F,
    "class", "typeof", "syntax1", "", "", F,

    # **
    # "pracma::logspace", "logrange", "syntax1", "", "",
    # "isTRUE", "", "syntax1", "", "",
    # "isFALSE", "", "syntax1", "", "",


    # "", "isapprox", "syntax1", "", "",
    #
    # # String manipulation
    "grep", "match", "syntax1", "", "", F,
    "strsplit", "split", "syntax1", "", "", F,
    "paste0", "join", "syntax1", "", "", F,
    "toupper", "uppercase", "syntax1", "", "", T,
    "tolower", "lowercase", "syntax1", "", "", T,
    "stringr::str_to_title", "uppercasefirst", "syntax1", "", "", T,


    # "sprintf", "", "syntax1", "", "",
    # "paste", "", "syntax1", "", "",
    # "paste0", "", "syntax1", "", "",

    # Sets
    # to do: Julia's isdisjoint, issubset, issetequal.
    "union", "union", "syntax1", "", "", F,
    "intersect", "intersect", "syntax1", "", "", F,
    "setdiff", "setdiff", "syntax1", "", "", F,
    "setequal", "setequal", "syntax1", "", "", F,

    # is....()
    "rlang::is_empty", "isempty", "syntax1", "", "", F,
    "all", "all", "syntax1", "", "", F,
    "any", "any", "syntax1", "", "", F,

    "is.infinite", "isinf", "syntax1", "", "", T,
    "is.finite", "isfinite", "syntax1", "", "", T,
    "is.nan", "ismissing", "syntax1", "", "", T,

    # https://docs.julialang.org/en/v1/base/collections
    # Julia: indexin, sortperm, findfirst
    "sort", "sort", "syntax1", "", "", F,


    # Complex numbers
    "Re", "real", "syntax1", "", "", T,
    "Im", "imag", "syntax1", "", "", T,
    "Mod", "", "syntax1", "", "", T,
    "Arg", "", "syntax1", "", "", T,
    "Conj", "conj", "syntax1", "", "", T,

    # as.complex(x, ...)
    # is.complex(x)
    # Re(z)
    # Im(z)
    # Mod(z)
    # Arg(z)
    # Conj(z)
    # imag, reim, complex, isreal, Real.

    # Custom functions
    "sigmoid", "sigmoid", "syntax1", "", "", T,
    "logit", "logit", "syntax1", "", "", T,
    "expit", "expit", "syntax1", "", "", T,
    "drop_u", "Unitful.ustrip", "syntax1", "", "", T,
    # ** to do: check units go well
    "seasonal", "seasonal", "syntax1", "", "", T,
    "IM_length", "length", "syntax1", "", "", T,

    "delay", "retrieve_past", "delay", "", "", T,
    "past", "retrieve_past", "past", "", "", T,
    "delayN", "compute_delayN", "delayN", "", "", T,
    "smoothN", "compute_smoothN", "smoothN", "", "", T,

    # Random Number Functions (13)
    "runif", "rand", "syntaxD", "Distributions.Uniform", "", F,
    "rnorm", "rand", "syntaxD", "Distributions.Normal", "", F,
    "rlnorm", "rand", "syntaxD", "Distributions.LogNormal", "", F,
    "rbool", "rbool", "syntax1", "", "", F,
    "rbinom", "rand", "syntaxD", "Distributions.Binomial", "", F,
    "rnbinom", "rand", "syntaxD", "Distributions.NegativeBinomial", "", F,
    "rpois", "rand", "syntaxD", "Distributions.Poisson", "", F,
    # "EnvStats::rtri", "", "syntaxD", "", "", F,
    "rexp", "rand", "syntaxD", "Distributions.Exponential", "", F,
    "rgamma", "rand", "syntaxD", "Distributions.Gamma", "", F,
    "rbeta", "rand", "syntaxD", "Distributions.Beta", "", F,
    "rcauchy", "rand", "syntaxD", "Distributions.Cauchy", "", F,
    "rchisq", "rand", "syntaxD", "Distributions.Chisq", "", F,
    "rgeom", "rand", "syntaxD", "Distributions.Geometric", "", F,
    "rf", "rand", "syntaxD", "Distributions.FDist", "", F,
    # "rhyper", "rand", "syntaxD", "Distributions.", "", F,
    # "rlogis", "rand", "syntaxD", "Distributions.", "", F,
    "rmultinom", "rand", "syntaxD", "Distributions.Multinomial", "", F,
    # "rsignrank", "rand", "syntaxD", "Distributions.", "", F,
    "rt", "rand", "syntaxD", "Distributions.TDist", "", F,
    "rweibull", "rand", "syntaxD", "Distributions.Weibull", "", F,
    # "rwilcox", "rand", "syntaxD", "Distributions.", "", F,
    # "rbirthday", "rand", "syntaxD", "Distributions.", "", F,
    # "rtukey", "rand", "syntaxD", "Distributions.", "", F,
    "rdist", "rdist", "syntax1", "", "", F,
    "set.seed", "Random.seed!", "syntax1", "", "", F,


    # Statistical Distributions (20)
    "punif", "Distributions.cdf.", "syntaxD", "Distributions.Uniform", "", F,
    "dunif", "Distributions.pdf.", "syntaxD", "Distributions.Uniform", "", F,
    "qunif", "Distributions.quantile.", "syntaxD", "Distributions.Uniform", "", F,

    "pnorm", "Distributions.cdf.", "syntaxD", "Distributions.Normal", "", F,
    "dnorm", "Distributions.pdf.", "syntaxD", "Distributions.Normal", "", F,
    "qnorm", "Distributions.quantile.", "syntaxD", "Distributions.Normal", "", F,

    "plnorm", "Distributions.cdf.", "syntaxD", "Distributions.LogNormal", "", F,
    "dlnorm", "Distributions.pdf.", "syntaxD", "Distributions.LogNormal", "", F,
    "qlnorm", "Distributions.quantile.", "syntaxD", "Distributions.LogNormal", "", F,

    "pbinom", "Distributions.cdf.", "syntaxD", "Distributions.Binomial", "", F,
    "dbinom", "Distributions.pdf.", "syntaxD", "Distributions.Binomial", "", F,
    "qbinom", "Distributions.quantile.", "syntaxD", "Distributions.Binomial", "", F,

    "pnbinom", "Distributions.cdf.", "syntaxD", "Distributions.NegativeBinomial", "", F,
    "dnbinom", "Distributions.pdf.", "syntaxD", "Distributions.NegativeBinomial", "", F,
    "qnbinom", "Distributions.quantile.", "syntaxD", "Distributions.NegativeBinomial", "", F,

    "pgamma", "Distributions.cdf.", "syntaxD", "Distributions.Gamma", "", F,
    "dgamma", "Distributions.pdf.", "syntaxD", "Distributions.Gamma", "", F,
    "qgamma", "Distributions.quantile.", "syntaxD", "Distributions.Gamma", "", F,

    "pbeta", "Distributions.cdf.", "syntaxD", "Distributions.Beta", "", F,
    "dbeta", "Distributions.pdf.", "syntaxD", "Distributions.Beta", "", F,
    "qbeta", "Distributions.quantile.", "syntaxD", "Distributions.Beta", "", F,

    "pcauchy", "Distributions.cdf.", "syntaxD", "Distributions.Cauchy", "", F,
    "dcauchy", "Distributions.pdf.", "syntaxD", "Distributions.Cauchy", "", F,
    "qcauchy", "Distributions.quantile.", "syntaxD", "Distributions.Cauchy", "", F,

    "pgeom", "Distributions.cdf.", "syntaxD", "Distributions.Geometric", "", F,
    "dgeom", "Distributions.pdf.", "syntaxD", "Distributions.Geometric", "", F,
    "qgeom", "Distributions.quantile.", "syntaxD", "Distributions.Geometric", "", F,

    "dmultinom", "Distributions.pdf.", "syntaxD", "Distributions.Multinomial", "", F,

    "pweibull", "Distributions.cdf.", "syntaxD", "Distributions.Weibull", "", F,
    "dweibull", "Distributions.pdf.", "syntaxD", "Distributions.Weibull", "", F,
    "qweibull", "Distributions.quantile.", "syntaxD", "Distributions.Weibull", "", F,

    "pt", "Distributions.cdf.", "syntaxD", "Distributions.TDist", "", F,
    "dt", "Distributions.pdf.", "syntaxD", "Distributions.TDist", "", F,
    "qt", "Distributions.quantile.", "syntaxD", "Distributions.TDist", "", F,

    "pf", "Distributions.cdf.", "syntaxD", "Distributions.FDist", "", F,
    "df", "Distributions.pdf.", "syntaxD", "Distributions.FDist", "", F,
    "qf", "Distributions.quantile.", "syntaxD", "Distributions.FDist", "", F,

    "pchisq", "Distributions.cdf.", "syntaxD", "Distributions.Chisq", "", F,
    "dchisq", "Distributions.pdf.", "syntaxD", "Distributions.Chisq", "", F,
    "qchisq", "Distributions.quantile.", "syntaxD", "Distributions.Chisq", "", F,

    "pexp", "Distributions.cdf.", "syntaxD", "Distributions.Exponential", "", F,
    "dexp", "Distributions.pdf.", "syntaxD", "Distributions.Exponential", "", F,
    "qexp", "Distributions.quantile.", "syntaxD", "Distributions.Exponential", "", F,

    "ppois", "Distributions.cdf.", "syntaxD", "Distributions.Poisson", "", F,
    "dpois", "Distributions.pdf.", "syntaxD", "Distributions.Poisson", "", F,
    "qpois", "Distributions.quantile.", "syntaxD", "Distributions.Poisson", "", F,


    # Complete replacements
    "next", "continue", "syntax0", "", "", F,
    "stop", "error", "syntax0", "", "", F ),



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

    ncol = 6, byrow = TRUE, dimnames = list(NULL, c("R", "Julia", "syntax", "add_first_arg", "add_second_arg", "add_broadcast"))
  )

  # Convert to dataframe
  conv_df <- as.data.frame(conv_df, stringsAsFactors = FALSE)

  # %>% t() %>%
    # magrittr::set_colnames(c(
    #   "R",
    #   "Julia",
    #   "syntax",
    #   "add_first_arg",
    #   "add_second_arg",
    #   "add_broadcast"
    # )) %>%
    # magrittr::set_rownames(NULL) %>%
    # as.data.frame()
  conv_df

  # **to do: add optionally that someone uses Base::


  # Create syntax_df by copying conv_df
  syntax_df <- conv_df

  # Add and modify columns
  syntax_df$R_first_iter <- syntax_df$R
  syntax_df$R_regex_first_iter <- ifelse(
    syntax_df$syntax == "syntax0",
    paste0("(?<!\\.)\\b", syntax_df$R, "\\b"),
    paste0("(?<!\\.)\\b", syntax_df$R, "\\(")
  )
  syntax_df$R <- paste0(syntax_df$R, "_replace")
  syntax_df$R_regex <- ifelse(
    syntax_df$syntax == "syntax0",
    paste0("(?<!\\.)\\b", syntax_df$R, "\\b"),
    paste0("(?<!\\.)\\b", syntax_df$R, "\\(")
  )

  return(list(syntax_df = syntax_df, conv_df = conv_df))
}



#' Convert R built-in functions to Julia
#'
#' @return List with transformed eqn and list with additional R code needed to make the eqn function
#' @inheritParams convert_equations_IM
#' @importFrom rlang .data
#'
convert_builtin_functions_Julia <- function(type, name, eqn, var_names, debug) {

  translated_func = c()
  # add_Rcode_list = list()

  # add_Rcode_list = list(list(list(intermediary = c(), func = list())) %>% stats::setNames(name)) %>%
    # stats::setNames(type)
  add_Rcode = list(intermediary = c(), func = list())

  if (grepl("[[:alpha:]]", eqn)){

    # Dataframe with regular expressions for each built-in Insight Maker function
    out = get_syntax_Julia()
    syntax_df = out$syntax_df
    conv_df = out$conv_df

    # intermediary = compute = update = setup = c()

    # Preparation for first iteration
    done = FALSE
    i = 1
    R_regex = syntax_df$R_regex_first_iter

    while (!done) {

      # which_regex = ifelse(i == 1, "R_regex_first_iter", "R_regex")

      # Remove those matches that are in quotation marks or names
      idxs_exclude = get_seq_exclude(eqn, var_names)

      # Update location indices of functions in eqn
      idx_df <- lapply(1:length(R_regex), function(i) {
        matches <- gregexpr(R_regex[i], eqn, perl = TRUE, ignore.case = FALSE)[[1]]
        # matches <- gregexpr(syntax_df[i, ][[which_regex]], eqn, ignore.case = FALSE)[[1]]
        if (matches[1] == -1) {
          return(data.frame(start = integer(), end = integer()))
        } else {
          # dplyr::bind_cols instead of cbind(), as cbind() gives rownames for short variable have been discarded warning
          dplyr::bind_cols(syntax_df[i, ],
                data.frame(
                  start = as.integer(matches),
                  end = as.integer(matches + attr(matches, "match.length") - 1)
                ))
        }
      }) %>% do.call(rbind, .) %>% as.data.frame()

      # idx_df <- idx_df[!((idx_df$start %in% idxs_exclude) | (idx_df$end %in% idxs_exclude)), ]
      if (nrow(idx_df) > 0) idx_df = idx_df[!(idx_df$start %in% idxs_exclude | idx_df$end %in% idxs_exclude), ]
      if (nrow(idx_df) == 0){
        done = TRUE
        next
      }


      # For the first iteration, add _replace to all detected functions, so we don't end in an infinite loop (some Julia and R functions have the same name)
      if (i == 1 & nrow(idx_df) > 0){

        idx_df <- idx_df[order(idx_df$start), ]
        idx_df$R_regex <- stringr::str_replace_all(idx_df$R_regex,
                                            stringr::fixed(c("(?<!\\.)\\b" = "", "\\(" = "(", "\\)" = ")")))

        for (j in rev(1:nrow(idx_df))){
          stringr::str_sub(eqn, idx_df[j, "start"], idx_df[j, "end"]) = idx_df[j, ]$R_regex #%>%
        }
      }

      if (i == 1){
        # print(eqn)
        R_regex = syntax_df$R_regex
        i = i + 1
        # Stop first iteration
        next
      }


      if (nrow(idx_df) == 0) {
        done = TRUE
      } else {

        # To find the arguments within round brackets, find all indices of matching '', (), [], c()
        paired_idxs = get_range_all_pairs(eqn, var_names, add_custom = "paste0()")
        paired_idxs

        # If there are brackets in the eqn:
        if (nrow(paired_idxs) > 0){
          # Match the opening bracket of each function to round brackets in paired_idxs
          idx_funcs = merge(
            paired_idxs[paired_idxs$type == "round", ],
            idx_df,
            by.x = "start",
            by.y = "end"
          ) %>% dplyr::rename(start_bracket = "start", start = "start.y") %>%
            # Add back syntax1b which does not need brackets
            dplyr::bind_rows(idx_df[idx_df$syntax == "syntax1b", ] %>%
                               # Add start_bracket column to prevent errors
                               dplyr::mutate(start_bracket = .data$start)) %>%
            dplyr::arrange(.data$end)
          idx_funcs
        } else {
          # If there are no brackets in the eqn:
          idx_funcs = idx_df
          # Add start_bracket column to prevent errors
          idx_funcs$start_bracket = idx_funcs$start
        }

        # Start with most nested function
        idx_funcs_ordered = idx_funcs %>% dplyr::rowwise() %>%
          dplyr::mutate(is_nested_around = any(.data$start < idx_funcs$start &
                                                 .data$end > idx_funcs$end)) %>% dplyr::ungroup() %>%
          dplyr::arrange(.data$is_nested_around) %>% as.data.frame()
        idx_func = idx_funcs_ordered[1, ] # Select first match

        if (debug){
          print("idx_func")
          print(idx_func)
        }

        # Extract argument between brackets (excluding brackets)
        bracket_arg = stringr::str_sub(eqn, idx_func$start_bracket + 1, idx_func$end - 1)

        # print("bracket_arg")
        # print(bracket_arg)

        arg = parse_args(bracket_arg)
        arg = sort_args(arg, idx_func$R_first_iter)
        arg = unname(unlist(arg))

        # print("arg")
        # print(arg)

        # Indices of replacement in eqn
        start_idx = idx_func$start
        end_idx = idx_func$end

        if (idx_func$syntax == "syntax0") {

          # arg = paste0(arg, collapse = ", ")
          replacement = idx_func$Julia

        } else if (idx_func$syntax == "syntax1") {

          arg = paste0(arg, collapse = ", ")

          replacement = sprintf(
            "%s%s(%s%s%s%s%s)",
            idx_func$Julia,
            ifelse(idx_func$add_broadcast, ".", ""),
            idx_func$add_first_arg,
            ifelse(nzchar(idx_func$add_first_arg) & nzchar(arg), ", ", ""),
            arg,
            idx_func$add_second_arg,
            ifelse(nzchar(idx_func$add_second_arg) & nzchar(arg), ", ", "")
          )

        } else if (idx_func$syntax == "delay"){

          if (type %in% c("stock", "gf", "constant", "macro")){
            stop(paste0("Adjust equation of ", name, ": delay() cannot be used for a ", type, "."))
          }

          # if (length(arg) > 3 | length(arg) < 2){
          #   stop(paste0("Adjust equation of ", name, ": delay() requires at least two and at most three arguments: the variable to delay and the time delay (and optionally the default value); see ?delay."))
          # }

          replacement = paste0(idx_func$Julia, "(",
                               arg[1], ", ",
                               arg[2], ", ",
                               ifelse(length(arg) > 2, arg[3], "nothing"), ", ",
                               P$time_name, ", \"", arg[1], "\", \"single\", ",
                               P$intermediaries, ", ", P$intermediary_names, ")")
          add_Rcode[["intermediary"]] = c(add_Rcode[["intermediary"]], arg[1])

        } else if (idx_func$syntax == "past"){


          if (type %in% c("stock", "gf", "constant", "macro")){
            stop(paste0("Adjust equation of ", name, ": past() cannot be used for a ", type, "."))
          }

          # if (length(arg) > 2 | length(arg) == 1 && !nzchar(arg[1]) ){
          #   stop(paste0("Adjust equation of ", name, ": past() requires at least one and at most two arguments: the variable to retrieve (and optionally the past interval); see ?past."))
          # }


          replacement = paste0(idx_func$Julia, "(",
                               arg[1], ", ",
                               arg[2], ", nothing, ",
                               P$time_name, ", \"", arg[1], "\", \"interval\", ",
                               P$intermediaries, ", ", P$intermediary_names, ")")
          add_Rcode[["intermediary"]] = c(add_Rcode[["intermediary"]], arg[1])

        } else if (idx_func$syntax == "delayN"){


          if (type %in% c("stock", "gf", "constant", "macro")){
            stop(paste0("Adjust equation of ", name, ": delayN() cannot be used for a ", type, "."))
          }

          # if (length(arg) > 4 | length(arg) < 3){
          #   stop(paste0("Adjust equation of ", name, ": delayN() requires at least three and at most four arguments: the variable to delay, the length of the delay, and the order of the delay (and optionally the initial value); see ?delayN."))
          # }

          arg4 = ifelse(length(arg) > 3, arg[4], arg[1])
          # Number delayN() as there may be multiple
          name_delay = paste0(name, P$delayN_suffix, length(add_Rcode[["func"]]) + 1)
          replacement = paste0(name_delay, ".outflow")
          setup = paste0("setup_delayN(", arg4, ", ", arg[2], ", ", arg[3], ", \"", name_delay, "\")")
          compute = paste0(idx_func$Julia, "(",
                                    arg[1], ", ",
                           name_delay, ", ",
                                    arg[2], ", ",
                                    arg[3], ")")

          update = paste0(name_delay, ".update")

          add_Rcode[["func"]] = append(add_Rcode[["func"]],
                             list(list(setup = setup,
                                       compute = compute,
                                       update = update,
                                       type = idx_func$Julia,
                                       var = arg[1],
                                       length = arg[2],
                                       order = arg[3],
                                       initial = arg4

                                       )) %>% stats::setNames(name_delay))


        } else if (idx_func$syntax == "smoothN"){


          if (type %in% c("stock", "gf", "constant", "macro")){
            stop(paste0("Adjust equation of ", name, ": smoothN() cannot be used for a ", type, "."))
          }

          # if (length(arg) > 4 | length(arg) < 3){
          #   stop(paste0("Adjust equation of ", name, ": smoothN() requires at least three and at most four arguments: the variable to smooth, the smoothing time, and the smoothing order (and optionally the initial value); see ?smoothN."))
          # }

          arg4 = ifelse(length(arg) > 3, arg[4], arg[1])
          # Number smoothN() as there may be multiple
          name_delay = paste0(name, P$delayN_suffix, length(add_Rcode[["func"]]) + 1)
          replacement = paste0(name_delay, ".outflow")
          setup = paste0("setup_smoothN(", arg4, ", ", arg[2], ", ", arg[3], ", \"", name_delay, "\")")
          compute = paste0(idx_func$Julia, "(",
                           arg[1], ", ",
                           name_delay, ", ",
                           arg[2], ", ",
                           arg[3], ")")

          update = paste0(name_delay, ".update")

          add_Rcode[["func"]] = append(add_Rcode[["func"]],
                                            list(list(setup = setup,
                                                      compute = compute,
                                                      update = update,
                                                      type = idx_func$Julia,
                                                      var = arg[1],
                                                      length = arg[2],
                                                      order = arg[3],
                                                      initial = arg4
                                                      )) %>% stats::setNames(name_delay))


        } else if (idx_func$syntax == "syntaxD"){

          # Convert random number generation
          replacement = conv_distribution(arg,
                                          # idx_func$R_first_iter,
                                          idx_func$Julia,
                                          idx_func$add_first_arg)
        }

        if (debug){
          print(stringr::str_sub(eqn, start_idx, end_idx))
          print(replacement)
          print("")
        }

        # Replace eqn
        stringr::str_sub(eqn, start_idx, end_idx) = replacement

        translated_func = c(translated_func, idx_func$R)
      }
    }
    # add_Rcode = list(list(add_Rcode) %>% stats::setNames(name)) %>%
      # stats::setNames(type)

}
  return(list(eqn = eqn, add_Rcode = add_Rcode, translated_func = translated_func))

}





#' Sort arguments in function call according to default order
#'
#' @param arg Vector with arguments in strings
#' @param func_name String with name of R function
#' @param default_arg Either NULL or named list of default arguments
#'
#' @returns List with named and sorted arguments
#'
sort_args = function(arg, func_name, default_arg = NULL){

  # If default arguments are not provided, assume func_name is an R function
  if (is.null(default_arg)){

    # Find default arguments of R function
    # Assume Julia and R arguments are the same, with the same order
    default_arg = do.call(formals, list(func_name)) %>% as.list()
    varargs = any(names(default_arg) == "...")
    default_arg = default_arg[names(default_arg) != "..."] # Remove ellipsis

    # default_arg_Julia = JuliaCall::julia_eval(sprintf("using Distributions; params(%s())", Julia_func))
  }

  # Find names and values of arguments
  contains_name = stringr::str_detect(arg, "=")
  arg_split = stringr::str_split_fixed(arg, "=", n = 2)
  names_arg = trimws(ifelse(contains_name, arg_split[, 1], NA))
  values_arg = trimws(ifelse(contains_name, arg_split[, 2], arg_split[, 1]))

  # For some functions, there are no default arguments, so there is no need to sort them
  if (length(default_arg) == 0){

    arg_R = values_arg

  } else {

    # Check whether all argument names are in the allowed argument names in case of no ...
    idx = !names_arg %in% names(default_arg) & !is.na(names_arg)
    if (!varargs & any(idx)){
      stop(paste0("Argument",
                  ifelse(sum(idx) > 1, "s ", " "),
                  paste0(names_arg[idx], collapse = ", "),
                  ifelse(sum(idx) > 1, " are", " is"),
                  " not allowed for function ", func_name, ". Allowed arguments: ",
                  paste0(names(default_arg), collapse = ", "), "."))
    }

    # Check if there are too many arguments
    if (!varargs & length(arg) > length(default_arg)){
      stop(paste0("Too many arguments for function ", func_name, ". Allowed arguments: ",
                  paste0(names(default_arg), collapse = ", "), "."))
    }

    # Add names to unnamed arguments; note that R can mix named and default arguments, e.g. runif(max = 10, 20, min = 1) - Julia cannot if they're not keyword arguments!
    idx = which(!contains_name & nzchar(values_arg)) # Find unnamed arguments which have values
    standard_order = names(default_arg)
    new_names = setdiff(standard_order, stats::na.omit(names_arg))
    names_arg[idx] = new_names[idx]


    # Check for missing obligatory arguments
    # obligatory arguments without a default (class == "name" or is.symbol, e.g. n in formals(rnorm) is a symbol)
    obligatory_args = unlist(lapply(default_arg, is.symbol))
    idx = !names(default_arg[obligatory_args]) %in% names_arg

    if (any(idx)){
      stop(paste0("Obligatory argument",
                  ifelse(sum(idx) > 1, "s ", " "),
                  paste0(names(default_arg[obligatory_args])[idx], collapse = ", "),
                  ifelse(sum(idx) > 1, " are", " is"),
                  " missing for function ", func_name, "."))
    }

    # Overwrite default arguments with specified arguments & remove NULL arguments
    default_arg_list = default_arg[!obligatory_args | unlist(lapply(default_arg, is.null))]
    arg_R = default_arg_list %>%
      utils::modifyList(as.list(stats::setNames(values_arg, names_arg)))

    # Sort order of arguments according to default order
    order_arg = c(names(default_arg), setdiff(names(arg_R), names(default_arg)))
    arg_R = arg_R[order_arg]


    # print("arg_R")
    # print(arg_R)

    # Check if any of the arguments are calls - these will need to be evaluated
    if (any(sapply(arg_R, class) == "call")){

      arg_R_num = lapply(arg_R, function(x) {
        if (!is.call(x)) {
          if (!grepl("'|\"", x) & !is.na(suppressWarnings(as.numeric(x)))) {
            x = as.numeric(x)
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
  }


  arg_R = lapply(arg_R, as.character)

  return(arg_R)
}




#' Sort arguments in function call according to default order
#'
#' @param arg Vector with arguments in strings
#' @param func_name String with name of R function
#' @param default_arg Either NULL or named list of default arguments
#'
#' @returns List with named and sorted arguments
#'
sort_arg_old = function(arg, func_name, default_arg = NULL){

  # If default arguments are not provided, assume func_name is an R function
  if (is.null(default_arg)){

    # Find default arguments of R function
    # Assume Julia and R arguments are the same, with the same order
    default_arg = do.call(formals, list(func_name)) %>% as.list()
    varargs = any(names(default_arg) == "...")
    default_arg = default_arg[names(default_arg) != "..."] # Remove ellipsis

    # default_arg_Julia = JuliaCall::julia_eval(sprintf("using Distributions; params(%s())", Julia_func))
  }

  # Find names and values of arguments
  contains_name = stringr::str_detect(arg, "=")
  arg_split = stringr::str_split_fixed(arg, "=", n = 2)
  names_arg = ifelse(contains_name, arg_split[, 1], NA) %>% trimws()
  values_arg = ifelse(contains_name, arg_split[, 2], arg_split[, 1]) %>% trimws()

  # For some functions, there are no default arguments, so all names should be stripped.
  if (length(default_arg) == 0){

    arg_R = values_arg

  } else {

    # Check whether all argument names are in the allowed argument names
    idx = !names_arg %in% names(default_arg) & !is.na(names_arg)
    if (any(idx) & !varargs){
      stop(paste0("Argument",
                  ifelse(sum(idx) > 1, "s ", " "),
                  paste0(names_arg[idx], collapse = ", "),
                  ifelse(sum(idx) > 1, " are", " is"),
                  " not allowed for function ", func_name, ". Allowed arguments: ",
                  paste0(names(default_arg), collapse = ", "), "."))
    }

    # Check if there are too many arguments
    if (!varargs & length(arg) > length(default_arg)){
      stop(paste0("Too many arguments for function ", func_name, ". Allowed arguments: ",
                  paste0(names(default_arg), collapse = ", "), "."))
    }

    # Add names to unnamed arguments; note that R can mix named and default arguments, e.g. runif(max = 10, 20, min = 1) - Julia cannot if they're not keyword arguments!
    idxs = which(!contains_name & nzchar(values_arg)) # Unnamed arguments which do have values
    standard_order = names(default_arg)
    new_names = setdiff(standard_order, stats::na.omit(names_arg))
    names_arg[idxs] = new_names[idxs]


    # Check for missing obligatory arguments

    # obligatory arguments without a default (class == "name" or is.symbol, e.g. n in formals(rnorm) is a symbol)
    obligatory_args = unlist(lapply(default_arg, is.symbol))
    idx = !names(default_arg[obligatory_args]) %in% names_arg

    if (any(idx)){
      stop(paste0("Obligatory argument",
                  ifelse(sum(idx) > 1, "s ", " "),
                  paste0(names(default_arg[obligatory_args])[idx], collapse = ", "),
                  ifelse(sum(idx) > 1, " are", " is"),
                  " missing for function ", func_name, "."))
    }

    # Overwrite default arguments with specified arguments
    default_arg_list = default_arg[!obligatory_args | unlist(lapply(default_arg, is.null))]
    arg_R = default_arg_list %>% # remove NULL arguments
      # arg_R = default_arg[unlist(lapply(default_arg, class)) != "name"] %>%
      # arg_R = default_arg[(default_arg %>% purrr::map_vec(class) != "name")] %>%
      utils::modifyList(as.list(stats::setNames(values_arg, names_arg)))
    # %>%
      # lapply(deparse)
    # lapply(as.character) # as.character converts e.g. 1/rate to "/"    "1"    "rate"

    # In case not all arguments are in default arguments
    order_arg = c(names(default_arg), setdiff(names(arg_R), names(default_arg)))
    arg_R = arg_R[order_arg] # Sort arguments according to default order
    arg_R_correct_class = arg_R


    # print("arg_R")
    # print(arg_R)

    for (name in names(arg_R_correct_class)) {
      if ((!class(default_arg[[name]]) %in% c("name", "call")) & !is.null(arg_R_correct_class[[name]])){
        class(arg_R_correct_class[[name]]) <- class(default_arg[[name]])
      }
    }


    # Parse in case of default arguments like scale = 1/rate
    for (name in names(arg_R)) {
      if (is.language(arg_R[[name]]) && !is.name(arg_R[[name]])) {
        # Evaluate the expression in the context of merged_args
        env <- as.environment(arg_R)
        env <- list2env(arg_R_correct_class, parent = baseenv())
        # arg_R[[name]] <- eval(arg_R_correct_class[[name]], envir = env)

        # Substitute values into the expression
        arg_R[[name]] <- eval(bquote(substitute(.(arg_R[[name]]), env)))

      }
    }

    arg_R = lapply(arg_R, function(x) {
      if (is.call(x)) {
        return(deparse(x))
      } else {
        return(as.character(x))
      }
    })
  }

  return(arg_R)
}





#' Convert random number generation in R to Julia
#'
#' @inheritParams sort_args
#' @param Julia_func String with Julia function
#' @param distribution String with Julia distribution call
#'
#' @returns String with Julia code
#'
conv_distribution = function(arg, Julia_func, distribution){


  # If n = 1, don't include it, as rand(..., 1) generates a vector. n is the first argument.

  Julia_str = sprintf("%s(%s(%s), %d)",
                      Julia_func, distribution,
                      # Don't include names of arguments
                      paste0(arg[-1], collapse = ", "), as.numeric(arg[1]))

  if (as.numeric(arg[1]) == 1 & Julia_func == "rand"){
    Julia_str = sprintf("%s(%s(%s))",
                        Julia_func, distribution,
                        # Don't include names of arguments
                        paste0(arg[-1], collapse = ", "))

  } else if (Julia_func == "Distributions.cdf."){

    # log = TRUE
    if (arg[length(arg)] == "TRUE"){
      Julia_str = sprintf("log%s(%s(%s), %d)",
                          Julia_func, distribution,
                          # Don't include names of arguments; skip log
                          paste0(arg[-c(1,  length(arg)-1, length(arg))], collapse = ", "), as.numeric(arg[1]))
    } else {

      Julia_str = sprintf("%s(%s(%s), %d)",
                          Julia_func, distribution,
                          # Don't include names of arguments; skip log
                          paste0(arg[-c(1,  length(arg)-1, length(arg))], collapse = ", "), as.numeric(arg[1]))
    }

  } else if (Julia_func == "Distributions.pdf."){

    # log.p = TRUE
    if (arg[length(arg)] == "TRUE"){
      Julia_str = sprintf("log%s(%s(%s), %d)",
                          Julia_func, distribution,
                          # Don't include names of arguments; skip lower.tail and log.p
                          paste0(arg[-c(1, length(arg))], collapse = ", "), as.numeric(arg[1]))
    } else {
      Julia_str = sprintf("%s(%s(%s), %d)",
                          Julia_func, distribution,
                          # Don't include names of arguments; skip lower.tail and log.p
                          paste0(arg[-c(1, length(arg))], collapse = ", "), as.numeric(arg[1]))
    }

  } else if (Julia_func == "Distributions.quantile."){

    # log = TRUE
    if (arg[length(arg)] == "TRUE"){
      Julia_str = sprintf("invlogcdf(%s(%s), %d)",
                          distribution,
                          # Don't include names of arguments; skip lower.tail and log.p
                          paste0(arg[-c(1, length(arg)-1, length(arg))], collapse = ", "), as.numeric(arg[1]))
    } else {
      Julia_str = sprintf("%s(%s(%s), %d)",
                          Julia_func, distribution,
                          # Don't include names of arguments; skip lower.tail and log.p
                          paste0(arg[-c(1, length(arg)-1, length(arg))], collapse = ", "), as.numeric(arg[1]))
    }
  }


  return(Julia_str)
}



#' Translate vector bracket syntax from R to square brackets in Julia
#'
#' @inheritParams convert_equations_IM
#' @return Updated eqn
#'
vector_to_square_brackets = function(eqn, var_names) {
  # print(eqn)

  done = FALSE
  while (!done) {

    # Get indices of all enclosures
    paired_idxs = get_range_all_pairs(eqn, var_names, type = "vector", names_with_brackets = FALSE)

    if (nrow(paired_idxs) > 0) {

      # Take first vector, replace with square brackets
      chosen_pair = paired_idxs[1, ]
      stringr::str_sub(eqn, chosen_pair$start, chosen_pair$end) = stringr::str_c("[", stringr::str_sub(eqn, chosen_pair$start+2, chosen_pair$end-1), "]")

    } else {
      done = TRUE
    }
  }

  return(eqn)

}





#' Split string per unit
#'
#' @param x String
#'
#' @returns List
split_units = function(x){

  idxs_word_df = stringr::str_locate_all(x, "([a-zA-Z_][a-zA-Z_\\.0-9 ]*)")[[1]] %>% as.data.frame()

  # If there are no words, x does not need to be split
  if (nrow(idxs_word_df) == 0){
    x_split = list(x)
  } else {
    split_df = rbind(idxs_word_df,
                     data.frame(start = idxs_word_df$end[-nrow(idxs_word_df)] + 1,
                                end = idxs_word_df$start[-1] - 1))
    split_df = split_df[order(split_df$start), ]

    # Make sure beginning characters are included
    if (split_df$start[1] > 1){
      split_df = rbind(data.frame(start = 1, end = split_df$start[1] - 1),
                       split_df) %>% as.data.frame()
    }

    # Make sure final characters are included
    if (split_df$end[nrow(split_df)] < nchar(x)){
      split_df = rbind(split_df, data.frame(start = split_df$end[nrow(split_df)] + 1, end = nchar(x))) %>% as.data.frame()
    }


    x_split = lapply(1:nrow(split_df), function(i){
      stringr::str_sub(x, split_df[i, "start"], split_df[i, "end"])
    })
  }

  return(x_split)
}




#' Replace written powers ("squared", "cubed") in string
#'
#' @param x String with unit
#'
#' @return Cleaned string with unit
#'
replace_written_powers = function(x){

  # Prepare regular expressions for detecting written powers
  regex_written_powers = stringr::regex(c("square[d]?", "cubic", "cube[d]?"), ignore_case = TRUE)
  regex_replacement = stringr::regex(c("^square[d]? (.*?)$" = "\\1\\^2",
                                       "^(.*?) square[d]?$" = "\\1\\^2",
                                       "^cubic (.*?)$" = "\\1\\^3",
                                       "^cube[d]? (.*?)$" = "\\1\\^3"), ignore_case = TRUE)

  while ( any(stringr::str_detect(x, regex_written_powers))){
    # Find indices of words
    idxs_words = stringr::str_locate_all(x, "[a-zA-Z][a-zA-Z 0-9\\._]*")[[1]]

    # Find indices of powers
    idxs_power = x %>% stringr::str_locate_all(regex_written_powers) %>% do.call(rbind, .)

    # Get first word with powers
    keep_idx = idxs_words[idxs_words[, "start"] <= idxs_power[1, "start"] & idxs_words[, "end"] > idxs_power[1, "start"] ,]

    sub_string = stringr::str_sub(x, keep_idx["start"], keep_idx["end"])
    replacement = sub_string %>% stringr::str_replace_all(regex_replacement)
    stringr::str_sub(x, keep_idx["start"], keep_idx["end"]) = replacement

  }
  return(x)
}



#' Remove scientific notation from string
#'
#' @inheritParams convert_equations_IM
#' @param task String with either "remove" or "add" to remove or add scientific notation
#' @param digits_max Number of digits after which to use scientific notation; ignored if task = "remove"; defaults to 15
#'
#' @returns Updated eqn
#'
scientific_notation = function(eqn, task = c("remove", "add")[1], digits_max = 15){

  eqn = as.character(eqn)

  if (task == "remove"){
    scientific = FALSE
    # pattern = "-?(?:\\d+\\.?\\d*|.\\d+)[eE][+-]?\\d+"
    pattern = "-?(?:\\d+\\.?\\d*|\\.\\d+)[eE][+-]?\\d+"
  } else if (task == "add"){
    scientific = TRUE
    # pattern = "\\d+"
    pattern = "-?(?:\\d+\\.?\\d*|\\.\\d+)"

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
    if (task == "add"){
      if (nchar(format(num, scientific = FALSE)) > digits_max){
        replacement = paste0(ifelse(is.na(leading_whitespace), "", leading_whitespace),
                             format(num, scientific = TRUE, trim = TRUE),
                             ifelse(is.na(following_whitespace), "", following_whitespace) )
      } else {
      # Change nothing otherwise
        replacement = match
      }

    } else if (task == "remove"){
      replacement = paste0(ifelse(is.na(leading_whitespace), "", leading_whitespace),
                           format(num, scientific = FALSE),
                                                 ifelse(is.na(following_whitespace), "", following_whitespace) )
    }

    return(replacement) # Convert back to fixed string
  }

  # Replace scientific notation in the string
  eqn <- stringr::str_replace_all(
    eqn,
    pattern = pattern,  # Regex for scientific notation
    replacement = reformat_scientific
  )

  return(eqn)
}



#' Clean unit contained in u('')
#'
#' @param x Equation containing potentially multiple u()
#' @inheritParams clean_unit
#'
#' @returns Cleaned equation
#' @export
#'
#' @examples
#' clean_unit_in_u("u('10 Meters') + u('Kilograms per sec') + u('10 pounds squared')",
#' get_regex_units())
clean_unit_in_u = function(x, regex_units){
  stringr::str_replace_all(x, "\\bu\\([\"|'](.*?)[\"|']\\)", function(y){clean_unit(y, regex_units)})
}


#' Convert units in string to Julia
#'
#' @param x String
#' @param regex_units Named vector with regular expressions in R as names and units in Julia as entries
#' @param ignore_case Boolean; if TRUE, ignore case when matching units
#' @param include_translation Boolean; if TRUE, add translation per unit to returned value
#' @param unit_name Boolean; if TRUE, x is a custom unit name and should be more rigorously cleaned
#'
#' @returns Updated string
clean_unit <- function(x, regex_units, ignore_case = FALSE, include_translation = FALSE, unit_name = FALSE) {

  if (x == "1") {
    x_new = x
    x_parts = x %>% stats::setNames(x)

  } else {

    # Ensure there is no scientific notation
    x = scientific_notation(x, task = "remove")

    # Remove double spaces and trim
    x <- gsub("\\s+", " ", trimws(x))

    # Replace squared -> ^2 and cubed -> ^3
    x = unname(sapply(x, replace_written_powers))

    # Replace "per" with "/"
    x <- gsub("[[:space:]\\)][Pp]er[[:space:]\\(]", "/", x, ignore.case = ignore_case)

    # Split unit into separate parts
    x_split = split_units(x)
    x_split_clean = lapply(x_split, trimws)
    idx = lapply(x_split_clean,
                 stringr::str_detect,
                 stringr::regex(names(regex_units), ignore_case = ignore_case)) %>%
      lapply(which) %>%
      vapply(function(y) if (length(y) > 0) y[1] else NA_real_, numeric(1))

    # Concatenate parts
    x_parts = ifelse(!is.na(idx), unname(regex_units[idx]), unlist(x_split_clean)) %>%
      # Replace punctuation with underscore
      # sapply(function(y){gsub("[@#&$!^%*~{}|:;<>?`]", "_", y)}) %>%
      # sapply(function(y){gsub("[@#&\\$!%~\\{\\}\\|:;\\?`\\\\]", "_", y)}) %>%
      sapply(function(y){gsub("@|#|&|\\$|!|%|~|\\{|\\}|\\||:|;|\\?|`|\\\\", "_", y)}) %>%
      # Replace space between numbers with "*"
      sapply(function(y){gsub("([0-9]) ([0-9])", "\\1*\\2", y)}) %>%
      # Remove all spaces
      sapply(function(y){gsub("[[:space:]]", "", y)}) %>%
      stats::setNames(unlist(x_split_clean))
    x_new = paste0(x_parts, collapse = "")

    # # Replace punctuation with underscore
    # x_new <- gsub("[@#&$!]", "_", x_new)
    #
    # # Replace space between numbers with "*"
    # x_new <- gsub("([0-9]) ([0-9])", "\\1*\\2", x_new)
    #
    # # Remove all spaces
    # x_new <- gsub("[[:space:]]", "", x_new)

    # Add back scientific notation in case there are too many digits
    x_new = scientific_notation(x_new, task = "add")
  }

  # If x is a unit name, clean more rigorously
  if (unit_name){
    x_new = gsub("\\+|\\^|\\*|<|>|-|\\.|\\(|\\)|/", "_", x_new)

    # Unit names cannot lead with a number
    if (grepl("^[0-9]", x_new)){
      x_new = paste0("_", x_new)
    }
  }

  if (include_translation){
    return(list(x_parts = x_parts, x_new = x_new))
  } else {
    return(x_new)
  }
}






#' Find missing unit definitions
#'
#' @inheritParams build
#' @inheritParams clean_unit
#' @param new_eqns String or vector with new equations potentially containing unit strings
#' @param new_units String or vector with units of variables
#' @param R_or_Julia String with either "R" or "Julia" to indicate which regular expression to use.
#'
#' @returns List with models units to add to sfm
#'
detect_undefined_units = function(sfm, new_eqns, new_units, regex_units, R_or_Julia = "Julia"){

  # Add undefined units to custom units
  units_in_model = c(sfm$sim_specs$time_units,
                     new_units,
                     # Extract units from equations
                     new_eqns %>%
                       stringr::str_extract_all(.,
                     ifelse(R_or_Julia == "Julia", "\\bu[\"|'](.*?)[\"|']", "\\bu\\([\"|'](.*?)[\"|']\\)"))
  ) %>%
    unlist() %>%
    # lapply(scientific_notation) %>%
    lapply(split_units) %>% unlist() %>%
    unique() %>%
    Filter(nzchar, .)  %>%
    # Only keep entries with letters in them
    Filter(function(x){stringr::str_detect(x, "[a-zA-Z]")}, .)

  # Find units to define: ones not already included in Julia
  existing_units = names(sfm$model_units)
  units_to_define = setdiff(units_in_model,
                            c(existing_units,
                              unname(regex_units)
                              ))

  if (length(units_to_define) > 0){

    add_model_units = lapply(units_to_define, function(u){
        list(name = u, eqn = "1", doc = "", prefix = FALSE)
      }) %>% stats::setNames(units_to_define)


  } else {
    add_model_units = list()
  }

  return(add_model_units)
}




#' View all standard units
#'
#' `get_units()` yields a dataframe with all standard units in Julia's Unitful package and added custom units by sdbuildR.
#'
#' @returns Dataframe with units in Julia
#' @export
#'
get_units = function(){

  units_df = matrix(c(
    "The meter, the SI base unit of length.", "m",      "Meter",     "",    TRUE,
    "The second, the SI base unit of time.", "s",      "Second",     "",    TRUE,
    "The ampere, the SI base unit of electric current.", "A",      "Ampere",     "",   TRUE,
    "The kelvin, the SI base unit of thermodynamic temperature.", "K",      "Kelvin",    "",       TRUE,
    "The candela, the SI base unit of luminous intensity.", "cd",     "Candela",    "",      TRUE,
    "The gram, the SI base unit for weight.", "g",      "Gram",        "",    TRUE,
    "The mole, the SI base unit for amount of substance.", "mol",    "Mole",         "",  TRUE,
    # Angles and solid angles
    "The steradian, a unit of spherical angle. There are 4pi sr in a sphere.", "sr",      "Steradian", "", TRUE,
    "The radian, a unit of angle. There are 2pi rad in a circle.", "rad",     "Radian", "", #** IM: 180/pi
      TRUE,
    "The degree, a unit of angle. There are 360 degrees in a circle.", "deg",  "Degree",      "pi/180", FALSE,

    # SI and related units
    "The hertz, an SI unit of frequency, defined as 1 s^-1.", "Hz",   "Hertz",           "1/s", TRUE,
    "The newton, an SI unit of force, defined as 1 kg * m / s^2.", "N", "Newton", "1kg*m/s^2", TRUE,
    "The pascal, an SI unit of pressure, defined as 1 N / m^2.", "Pa",   "Pascal", "1N/m^2", TRUE,
    "The joule, an SI unit of energy, defined as 1 N * m.", "J", "Joule", "1N*m", TRUE,
    "The watt, an SI unit of power, defined as 1 J / s.", "W",   "Watt", "1J/s", TRUE,
    "The coulomb, an SI unit of electric charge, defined as 1 A * s.", "C", "Coulomb", "1A*s", TRUE,
    "The volt, an SI unit of electric potential, defined as 1 W / A.", "V", "Volt", "1W/A", TRUE,
    "The ohm, an SI unit of electrical resistance, defined as 1 V / A.", "Ohm", "Ohm", "1V/A", TRUE,
    "The siemens, an SI unit of electrical conductance, defined as 1 Ohm^-1", "S", "Siemens", "1/Ohm", TRUE,
    "The farad, an SI unit of electrical capacitance, defined as 1 s^4 * A^2 / (kg * m^2).", "F", "Farad", "1s^4*A^2/(kg*m^2)", TRUE,
    "The henry, an SI unit of electrical inductance, defined as 1 J / A^2.", "H", "Henry", "1J/(A^2)", TRUE,
    "The tesla, an SI unit of magnetic B-field strength, defined as 1 kg / (A * s^2).", "T", "Tesla", "1kg/(A*s^2)", TRUE,
    "The weber, an SI unit of magnetic flux, defined as 1 kg * m^2 / (A * s^2).", "Wb", "Weber", "1kg*m^2/(A*s^2)", TRUE,
    "The lumen, an SI unit of luminous flux, defined as 1 cd * sr.", "lm", "Lumen", "1cd*sr", TRUE,
    "The lux, an SI unit of illuminance, defined as 1 lm / m^2.", "lx", "Lux", "1lm/m^2", TRUE,
    "The becquerel, an SI unit of radioactivity, defined as 1 nuclear decay per s.", "Bq", "Becquerel", "1/s", TRUE,
    "The gray, an SI unit of ionizing radiation dose, defined as the absorption of 1 J per kg of matter.", "Gy", "Gray", "1J/kg", TRUE,
    "The sievert, an SI unit of the biological effect of an ionizing radiation dose.", "Sv", "Sievert", "1J/kg", TRUE,
    "The katal, an SI unit of catalytic activity, defined as 1 mol of catalyzed", "kat", "Katal", "1mol/s", TRUE,
    "Percent, a unit meaning parts per hundred. Printed as \"%\".", "%", "Percent", "1//100", FALSE,
    "Permille, a unit meaning parts per thousand. Printed as %", "permille", "Permille", "1//1000", FALSE,
    "Permyriad, a unit meaning parts per ten thousand.", "pertenthousand", "Pertenthousand", "1//10000", FALSE,
    "Percentmille, a unit meaning parts per hundred thousand.", "pcm", "Percentmille", "1//100000", FALSE,
    "Permillion, a unit meaning parts per million.", "ppm", "Permillion", "1//1000000", FALSE,
    "Perbillion, a unit meaning parts per billion (in the short-scale sense), i.e., 10^-9.", "ppb", "Perbillion", "1//1000000000", FALSE,
    "Pertrillion, a unit meaning parts per trillion (in the short-scale sense), i.e., 10^-12.","ppt",  "Pertrillion", "1//1000000000000", FALSE,
    "Perquadrillion, a unit meaning parts per quadrillion (in the short-scale sense), i.e., 10^-15.", "ppq", "Perquadrillion",  "1//1000000000000000", FALSE,

    # Temperature
    "The degree Celsius, an SI unit of temperature, defined such that 0 degrees C = 273.15 K.","degC", "Celsius", "(27315//100)K", FALSE,

    # Common units of time
    "The minute, a unit of time defined as 60 s. The full name `minute` is used instead of the symbol `min` to avoid confusion with the Julia function `min`.", "minute", "Minute", "60s", FALSE,
    "The hour, a unit of time defined as 60 minutes.", "hr", "Hour", "3600s", FALSE,
    "The day, a unit of time defined as 24 hr.", "d", "Day", "86400s", FALSE,
    "The week, a unit of time, defined as 7 d.", "wk", "Week", "604800s", FALSE,
    "The year, a unit of time, defined as 365.25 d.", "yr", "Year", "31557600s", TRUE,
    "Revolutions per second, a unit of rotational speed, defined as 2pi rad / s.","rps", "RevolutionsPerSecond", "2pi*rad/s", FALSE,
    "Revolutions per minute, a unit of rotational speed, defined as 2pi rad / minute.", "rpm", "RevolutionsPerMinute", "2pi*rad/minute", FALSE,

    # Area
    # The hectare is used more frequently than any other power-of-ten of an are.
    "The are, a metric unit of area, defined as 100 m^2.", "a", "Are", "100m^2", FALSE,
    "The hectare, a metric unit of area, defined as 100 a.", "ha", "Hectare", "", FALSE,
    "The barn, a metric unit of area, defined as 100 fm^2.", "b", "Barn", "100fm^2", TRUE,

    # Volume
    # `l` is also an acceptable symbol for liters
    "The liter, a metric unit of volume, defined as 1000 cm^3.", "L", "Liter", "m^3//1000", TRUE, # const l = L)

    # Molarity
    "A unit for measuring molar concentration, equal to 1 mol/L.", "M", "Molar", "1mol/L", TRUE,
    # Energy
    "A quantity equal to the elementary charge, the charge of a single electron, with a value of exactly 1.602,176,634 * 10^-19 C. The letter `q` is used instead of `e` to avoid confusion with Euler's number.", "q", "", "1.602_176_634e-19*C", FALSE,        # CODATA 2018; `e` means 2.718...
    "The electron-volt, a unit of energy, defined as q*V.", "eV", "eV", "q*V", TRUE,
    # For convenience
    "A unit for convenience in angular frequency, equal to 2pi Hz.", "AngHertz", "AngHertz", "2pi/s", TRUE,
    "The bar, a metric unit of pressure, defined as 100 kPa.", "bar", "Bar", "100000Pa", TRUE,
    "The standard atmosphere, a unit of pressure, defined as 101,325 Pa.", "atm", "Atmosphere", "101325Pa", TRUE,
    "The torr, a unit of pressure, defined as 1/760 atm.", "Torr", "Torr", "101325Pa//760", TRUE,

    # Constants (2018 CODATA values)        (uncertainties in final digits)
    "A quantity representing the speed of light in a vacuum, defined as exactly 2.997,924,58 * 10^8 m/s.", "c0", "", "299_792_458*m/s", FALSE,
    "The speed of light in a vacuum, a unit of speed, defined as exactly 2.997,924,58 * 10^8 m/s.", "c", "SpeedOfLight", "1c0", FALSE,
    "A quantity representing the vacuum permeability constant, defined as 4pi * 10^-7 H / m.", "magnetic_constant", "magnetic constant", "4pi*(1//10)^7*H/m", FALSE,
    "A quantity representing the vacuum permittivity constant, defined as 1 / (mu0 * c^2).", "electric_constant", "electric constant", "", FALSE,
    "A quantity representing the impedance of free space, a constant defined as mu0 * c.", "Z0", "impedance of free space", "", FALSE,
    "A quantity representing the universal gravitational constant, equal to 6.674,30 * 10^-11 m^3 / (kg * s^2) (the CODATA 2018 recommended value).", "G", "gravitational constant", "6.674_30e-11*m^3/kg/s^2", FALSE,
    "A quantity representing the nominal acceleration due to gravity in a vacuum near the surface of the earth, defined by standard to be exactly 9.806,65 m / s^2.", "gn", "standard acceleration of gravity", "9.80665*m/s^2", FALSE,
    "A quantity representing Planck's constant, defined as exactly 6.626,070,15 * 10^-34 J * s.", "h", "Planck constant", "6.626_070_15e-34*J*s", FALSE,
    "A quantity representing the reduced Planck constant, defined as h / 2pi.", "reduced_Planck_constant", "hbar", "h/2pi", FALSE, #hbar is already a unit -> prefix + bar
    "A quantity representing the superconducting magnetic flux quantum, defined as h / (2 * q).", "superconducting_magnetic_flux_quantum", "Superconducting magnetic flux quantum", "h/(2q)", FALSE,
    "A quantity representing the rest mass of an electron, equal to 9.109,383,7015 * 10^-31 kg (the CODATA 2018 recommended value).", "me", "electron rest mass", "9.109_383_7015e-31*kg", FALSE,
    "A quantity representing the rest mass of a neutron, equal to 1.674,927,498,04 * 10^-27 kg (the CODATA 2018 recommended value).", "mn", "neutron rest mass", "1.674_927_498_04e-27*kg", FALSE,
    "A quantity representing the rest mass of a proton, equal to 1.672,621,923,69 * 10^-27 kg (the CODATA 2018 recommended value).", "mp", "proton rest mass", "1.672_621_923_69e-27*kg", FALSE,
    "A quantity representing the Bohr magneton, equal to q * hbar / (2 * me).", "Bohr_magneton", "Bohr magneton", "q*hbar/(2*me)", FALSE,
    "A quantity representing Avogadro's constant, defined as exactly 6.022,140,76 * 10^23 / mol.", "Na", "Avogadro constant", "6.022_140_76e23/mol", FALSE,
    "A quantity representing the Boltzmann constant, defined as exactly 1.380,649 * 10^-23 J / K.", "k", "Boltzmann constant", "1.380_649e-23*(J/K)", FALSE,
    "A quantity representing the molar gas constant, defined as Na * k.", "R", "molar gas constant", "Na*k", FALSE,
    "A quantity representing the Stefan-Boltzmann constant, defined as pi^2 * k^4 / (60 * hbar^3 * c^2).",
      "Stefan_Boltzmann_constant", "Stefan-Boltzmann constant", "pi^2*k^4/(60*hbar^3*c^2)", FALSE,
    "A quantity representing the Rydberg constant, equal to 1.097,373,156,8160 * 10^-7 / m (the CODATA 2018 recommended value).", "Rydberg_constant", "Rydberg constant", "10_973_731.568_160/m", FALSE,
    "The unified atomic mass unit, or dalton, a unit of mass defined as 1/12 the mass of an unbound neutral atom of carbon-12, equal to 1.660,539,066,60 * 10^-27 kg (the CODATA 2018 recommended value).", "u", "UnifiedAtomicMassUnit", "1.660_539_066_60e-27*kg", FALSE,

    # Acceleration
    "The nominal acceleration due to gravity in a vacuum near the surface of the earth, a unit of acceleration, defined by standard to be exactly 9.806,65 m / s^2.", "ge", "EarthGravity", "gn", FALSE,


    # CGS units
    "The gal, a CGS unit of acceleration, defined as 1 cm / s^2.", "Gal", "Gal", "1cm/s^2",  TRUE,
    "The dyne, a CGS unit of force, defined as 1 g * cm / s^2.", "dyn", "Dyne", "1g*cm/s^2", TRUE,
    "The erg, a CGS unit of energy, defined as 1 dyn * cm.", "erg", "Erg", "1g*cm^2/s^2", TRUE,
    "The barye, a CGS unit of pressure, defined as 1 dyn / cm^2.","Ba", "Barye", "1g/cm/s^2", TRUE,
    "The poise, a CGS unit of dynamic viscosity, defined as 1 dyn * s / cm^2.", "P", "Poise", "1g/cm/s", TRUE,
    "The stokes, a CGS unit of kinematic viscosity, defined as 1 cm^2 / s.", "St", "Stokes", "1cm^2/s", TRUE,
    "The gauss, a CGS unit of magnetic B-field strength, defined as 1 Mx / cm^2.", "Gauss", "Gauss", "(1//10_000)*T", TRUE,
    "The oersted, a CGS unit of magnetic H-field strength, defined as 1000 A / (4pi * m).", "Oe", "Oersted", "(1_000/4pi)*A/m", TRUE,
    "The maxwell, a CGS unit of magnetic flux, defined as 1 Gauss * cm^2.", "Mx", "Maxwell", "(1//100_000_000)*Wb", TRUE,


    #########
    # Shared Imperial / US customary units

    # Length
    #key: Symbol    Display    Name                 Equivalent to           10^n prefixes?
    "The inch, a US customary unit of length defined as 2.54 cm.", "inch", "Inch", "(254//10000)*m", FALSE,
    "The mil, a US customary unit of length defined as 1/1000 inch.", "mil", "Mil", "(1//1000)*inch", FALSE,
    "The foot, a US customary unit of length defined as 12 inch.", "ft", "Foot", "12inch",  FALSE,
    "The yard, a US customary unit of length defined as 3 ft.","yd", "Yard", "3ft", FALSE,
    "The mile, a US customary unit of length defined as 1760 yd.", "mi", "Mile", "1760yd", FALSE,
    "The angstrom, a metric unit of length defined as 1/10 nm.", "angstrom", "Angstrom", "(1//10)*nm", FALSE,

    # Area
    "The acre, a US customary unit of area defined as 4840 yd^2","ac", "Acre", "(316160658//78125)*m^2", FALSE,

    # Temperatures
    "The rankine, a US customary unit of temperature defined as 5/9 K.", "Ra", "Rankine", "(5//9)*K", FALSE,
    "The degree Fahrenheit, a US customary unit of temperature, defined such that 0 degrees F = 459.67 Ra.", "degF", "Fahrenheit", "(45967//100)Ra", FALSE,

    # Masses
    "The pound-mass, a US customary unit of mass defined as exactly 0.453,592,37 kg.", "lb", "Pound", "0.45359237kg", FALSE, # is exact
    "The ounce, a US customary unit of mass defined as 1/16 lb.", "oz", "Ounce", "lb//16", FALSE,
    "The slug, a US customary unit of mass defined as 1 lbf * s^2 / ft.", "slug", "Slug", "1lb*ge*s^2/ft", FALSE,
    "The dram, a US customary unit of mass defined as 1/16 oz.", "dr", "Dram", "oz//16", FALSE,
    "The grain, a US customary unit of mass defined as 1/7000 lb.", "gr", "Grain", "(32//875)*dr", FALSE,

    # Force
    "The pound-force, a US customary unit of force defined as 1 lb * ge.", "lbf", "PoundsForce", "1lb*ge", FALSE,

    # Energy
    # Use ISO 31-4 for BTU definition
    "The calorie, a unit of energy defined as exactly 4.184 J.", "cal", "Calorie", "4.184J", TRUE,
    "The British thermal unit, a US customary unit of heat defined by ISO 31-4 as exactly 1055.06 J.", "btu", "BritishThermalUnit", "1055.06J", FALSE,
    "Pounds per square inch, a US customary unit of pressure defined as 1 lbf / inch^2.", "psi", "PoundsPerSquareInch", "1lbf/inch^2", FALSE,

    # Custom units

    "The common year, a unit of time, defined as 365 d.", "common_yr", "Common year", "365d", FALSE,
    "The common quarter, a unit of time, defined as 365/4 d.", "common_quarter", "Common quarter", "365/4d", FALSE,
    "The common month, a unit of time, defined as 365/12 d.", "common_month", "Common month", "365/12d", FALSE,
    "The quarter, a unit of time, defined as 1/4yr.", "quarter", "Quarter", "1/4yr", FALSE,
    "The month, a unit of time, defined as 1/12yr.", "month", "Month", "1/12yr", FALSE,

    "The US fluid ounce, to measure liquids", "fl_oz", "Fluid ounce", "29.5735295625mL", FALSE,
    "The US liquid quart, unit of capacity", "quart", "Quart", "946.35cm^3", FALSE,
    "The tonne, a metric unit of mass", "tonne", "Tonne", "1000kg", FALSE,
    "The US short ton", "ton", "Ton", "907.18474kg", FALSE,
    "The US gallon, a unit of volume", "US_gal", "Gallon", "0.003785411784m^3", FALSE,
    "The atom, used in chemistry to quantify microscopic particles", "atom", "Atom", "1/6.02214076e23mol", FALSE,
    "The molecule, used in scientific models to count chemical entities", "molecule", "Molecule", "1/6.02214076e23mol", FALSE,
    "The euro, the official currency of the Eurozone", "EUR", "Euro", "1", FALSE,
    "The dollar, the currency of the United States", "USD", "Dollar", "1", FALSE,
    "The pound, the currency of the United Kingdom", "GBP", "PoundSterling", "1", FALSE),

   ncol = 5, byrow = TRUE, dimnames = list(NULL, c("description", "name", "full_name", "definition", "prefix")))


  # %>% t() %>% magrittr::set_rownames(NULL) %>%
  #   magrittr::set_colnames(c("description", "name", "full_name", "definition", "prefix")) %>%
  #   as.data.frame()

  return(units_df)

}


#' Get regular expressions for time units in Julia
#'
#' @returns Named vector with regular expressions as names and units as entries
#'
#' @export
get_regex_time_units = function(){

  # Get units dataframe and only keep time units
  units_df = get_units()
  units_df = units_df[nzchar(units_df[, "full_name"]) & units_df[, "name"] %in% c("s", "minute", "hr", "d", "wk", "month", "quarter", "yr", "common_month", "common_quarter", "common_yr"), ]


    # Create regular expressions
  regex_time_units_Julia = sapply(units_df[, "full_name"],
                                  function(x){
    paste0("^[", toupper(stringr::str_sub(x, 1, 1)), "|", tolower(stringr::str_sub(x, 1, 1)), "]",
           stringr::str_sub(x, 2, nchar(x)), "[s]?$")
  })

  # Get named list with regular expressions
  regex_time_units_Julia = stats::setNames(units_df[, "name"], unname(regex_time_units_Julia))

  # Units that allow for prefixes
  idx = which(units_df[, "prefix"] == "TRUE")

  # Prefixes
  si_prefix_matrix = unit_prefixes()
  si_prefixes = si_prefix_matrix[, "symbol"] %>% stats::setNames(si_prefix_matrix[, "prefix"])


  add_prefixes = lapply(seq_along(regex_time_units_Julia[idx]), function(i){
    name = names(regex_time_units_Julia[idx])[i]
    x = regex_time_units_Julia[idx][i]
    stats::setNames(
      paste0(unname(si_prefixes), unname(x)),
      paste0("^[", toupper(stringr::str_sub(names(si_prefixes), 1, 1)), "|",
             tolower(stringr::str_sub(names(si_prefixes), 1, 1)), "]",

             stringr::str_sub(names(si_prefixes), 2, -1L),
             # Start at index 2 to skip opening ^
             stringr::str_sub(name, 2, -1L)))
  }) %>% unname() %>% unlist()

  regex_time_units_Julia = c(regex_time_units_Julia, add_prefixes)

  regex_time_units_Julia = c(regex_time_units_Julia,
                        # Add extras
                        "^[S|s]ec$" = "s",
                        "^[M|m]in$" = "minute",
                        "^[C|c]ommon month[s]?$" = "common_month",
                        "^[C|c]ommon quarter[s]?$" = "common_quarter",
                        "^[C|c]ommon year[s]?$" = "common_yr"
  )

  # Only keep ones with characters in entry and name %>% Filter(nzchar, .)
  regex_time_units_Julia = regex_time_units_Julia[nzchar(names(regex_time_units_Julia)) & nzchar(unname(regex_time_units_Julia))]

  return(regex_time_units_Julia)

}



#' Show unit prefixes
#'
#' @returns Matrix with unit prefixes, symbols, and power-of-ten scale
#' @export
#'
#' @examples unit_prefixes()
unit_prefixes = function(){
  # Define the SI prefixes, symbols, and scales (with scales as 10^exponent)
  si_prefix_matrix <- matrix(
    c(
      "yotta", "Y", "10^24",
      "zetta", "Z", "10^21",
      "exa", "E", "10^18",
      "peta", "P", "10^15",
      "tera", "T", "10^12",
      "giga", "G", "10^9",
      "mega", "M", "10^6",
      "kilo", "k", "10^3",
      "hecto", "h", "10^2",
      "deka", "da", "10^1",
      "deci", "d", "10^-1",
      "centi", "c", "10^-2",
      "milli", "m", "10^-3",
      "micro", "\\u03BC", "10^-6", # mu
      "nano", "n", "10^-9",
      "pico", "p", "10^-12",
      "femto", "f", "10^-15",
      "atto", "a", "10^-18",
      "zepto", "z", "10^-21",
      "yocto", "y", "10^-24"
    ),
    ncol = 3,
    byrow = TRUE,
    dimnames = list(NULL, c("prefix", "symbol", "scale"))
  )

  return(si_prefix_matrix)
}


#' Get regular expressions for units in Julia
#'
#' @inheritParams build
#' @returns Named vector with regular expressions as names and units as entries
#'
#' @export
get_regex_units = function(sfm = NULL){

  # Get units dataframe
  units_df = get_units()
  units_df = units_df[nzchar(units_df[, "full_name"]), ]

  # Units which should not have a suffix s ([s]?), e.g. inch
  no_s_suffix = c("Hertz", "Siemens", "Henry", "Lux", "Percent",
                  "Permille", "Pertenthousand", "Percentmille", "Permillion", "Perbillion", "Pertrillion", "Perquadrillion", "Celsius",
                  "AngHertz","SpeedOfLight", "magnetic constant", "electric constant","impedance of free space", "gravitational constant", "standard acceleration of gravity", "Planck constant", "Superconducting magnetic flux quantum", "electron rest mass", "neutron rest mass", "proton rest mass", "Bohr magneton", "Avogadro constant", "Boltzmann constant", "molar gas constant", "Stefan-Boltzmann constant","Rydberg constant", "UnifiedAtomicMassUnit", "EarthGravity", "Stokes", "Gauss", "Inch", "Mil", "Foot","Fahrenheit", "PoundsPerSquareInch")

  regex_units = sapply(units_df[, "full_name"],
                       function(x){
    paste0("^[", toupper(stringr::str_sub(x, 1, 1)), "|", tolower(stringr::str_sub(x, 1, 1)), "]",
           stringr::str_sub(x, 2, nchar(x)), ifelse(x %in% no_s_suffix, "$", "[s]?$"))
  })

  # Get named list with regular expressions
  regex_units = stats::setNames(units_df[, "name"], unname(regex_units))

  # Units that allow for prefixes
  idx = which(units_df[, "prefix"] == "TRUE")


  # Prefixes
  si_prefix_matrix = unit_prefixes()
  si_prefixes = stats::setNames(si_prefix_matrix[, "symbol"], si_prefix_matrix[, "prefix"])

  add_prefixes = lapply(seq_along(regex_units[idx]), function(i){
    name = names(regex_units[idx])[i]
    x = regex_units[idx][i]
    stats::setNames(
      paste0(unname(si_prefixes), unname(x)),
      paste0("^[", toupper(stringr::str_sub(names(si_prefixes), 1, 1)), "|",
             tolower(stringr::str_sub(names(si_prefixes), 1, 1)), "]",

             stringr::str_sub(names(si_prefixes), 2, -1L),
             # Start at index 2 to skip opening ^
             stringr::str_sub(name, 2, -1L)))
  }) %>% unname() %>% unlist()

  regex_units = c(regex_units, add_prefixes)

  regex_units = c(regex_units,
                        # Add extras
                        "^feet$" = "ft",
                        "^[S|s]ec$" = "s",
                  "^[M|m]in$" = "minute",
                  "^[C|c]ommon month[s]?$" = "common_month",
                  "^[C|c]ommon quarter[s]?$" = "common_quarter",
                  "^[C|c]ommon year[s]?$" = "common_yr",

                  "^[I|i]nches$" = "inch",
                        "^[M|m]etre[s]?$" = "m",
                        "^[A|a]cre feet$" = "ac",
                        "^[A|a]cre foot$" = "ac",
                  "^[D|d]egree[s]? [K|k]elvin$" = "K",
                  "^[D|d]egree[s]? [F|f]ahrenheit$" = "degF",
                  "^[D|d]egree[s]? [C|c]elsius$" = "degC",

                        "^BTU$" = "btu",
                        "^\\$" = "USD",
                        "^\\u20AC$" = "EUR",
                        "^\\u00A3$" = "GBP",
                        # "^[F|f]luid [ounce[s]?]?$" = "fl_oz",
                        # "^[Q|q]uarts$" = "quarts",
                        # "^[T|t]onne[s]?$" = "tonne",
                        # "^[T|t]on[s]?$" = "ton",
                        # "^[A|a]tom[s]?$" = "atom",
                        # "^[M|m]olecule[s]?$" = "molecule",
                        # "^[E|e]uro[s]?$" = "euro",
                        # "^[D|d]ollar[s]?$" = "dollar",

                        "^[H|h]enries$" = "H",
                        "^[L|l]uxes$" = "lx",
                        "^[P|p]ercentage[s]?$" = "%",
                        "^[U|u]nitless$" = "1",
                        "^[D|d]imensionless$" = "1",
                        "^[D|d]mnl$" = "1",
                        "^[N|n]o[ ]?[U|u]nit[s]?$" = "1"
  )

  # If there are custom units added with power of ten prefixes enabled, add regular expressions
  if (!is.null(sfm)){
    if (length(sfm$model_units) > 0){
      # Only if there are any units with power-of-ten
      prefix = unlist(lapply(sfm$model_units, `[[`, "prefix"))
      if (any(prefix)){
        unit_names = names(sfm$model_units)
        add_custom_regex = lapply(unit_names[prefix], function(unit_name){

        stats::setNames(
          paste0(unname(si_prefixes), unit_name),
          paste0("^[", toupper(stringr::str_sub(names(si_prefixes), 1, 1)), "|",
                 tolower(stringr::str_sub(names(si_prefixes), 1, 1)), "]",
                 stringr::str_sub(names(si_prefixes), 2, -1L),
                 unit_name)
            )
        }) %>% unlist()
        regex_units = c(regex_units, add_custom_regex)

      }
    }
  }


  # Only keep ones with characters in entry and name %>% Filter(nzchar, .)
  regex_units = regex_units[nzchar(names(regex_units)) & nzchar(unname(regex_units))]

  return(regex_units)

}





#' Get list of standard custom units in Julia
#'
#' @returns List with custom units in Julia
custom_units = function(){

  # The "month" unit in Insight Maker is 365/12, which is not the same as in the units package, where it is 365.25/12. Add new unit "common_month".


  list("common_yr" = list(name = "common_yr", eqn = "365d", prefix = FALSE),
       "common_quarter" = list(name = "common_quarter", eqn = "365/4*d", prefix = FALSE),
       "common_month" = list(name = "common_month", eqn = "365/12*d", prefix = FALSE),
       "quarter" = list(name = "quarter", eqn = "1/4*yr", prefix = FALSE),
       "month" = list(name = "month", eqn = "1/12*yr", prefix = FALSE),
       "quart" = list(name = "quart", eqn = "946.35cm^3", prefix = FALSE),
       "tonne" = list(name = "tonne", eqn = "1000kg", prefix = FALSE),
       "ton" = list(name = "ton", eqn = "907.18474kg", prefix = FALSE),
       "atom" = list(name = "atom", eqn = "1/6.02214076e23mol", prefix = FALSE),
       "molecule" = list(name = "molecule", eqn = "1/6.02214076e23mol", prefix = FALSE),


       "US_gal" = list(name = "US_gal", eqn = "0.003785411784m^3", prefix = FALSE),
       "fl_oz" = list(name = "fluidOunce", eqn = "29.5735295625mL", prefix = FALSE),
       "EUR" = list(name = "EUR", eqn = "1", prefix = FALSE),
       "USD" = list(name = "USD", eqn = "1", prefix = FALSE),
       "GBP" = list(name = "GBP", eqn = "1", prefix = FALSE),

       # Unicode characters are not accepted in R, so create new units
       # This is necessary, because otherwise these units will be translated to unicode characters for users, which can create problems in R. Alternatively, the units are translated to e.g. \\u00B0, which is not nice for the user.
       # "deg" = list(name = "deg", eqn = "1.0\\u00B0", prefix = FALSE),
       # "Ohm" = list(name = "Ohm", eqn = "1.0\\u2126", prefix = FALSE),
       # "hbar" = list(name = "hbar", eqn = "1.0\\u0127", prefix = FALSE),
       # "superconducting_magnetic_flux_quantum" = list(name = "superconducting_magnetic_flux_quantum", eqn = "1.0\\u03A60", prefix = FALSE),
       # "degF" = list(name = "degF", eqn = "1.0\\u00B0F", prefix = FALSE),
       # "degC" = list(name = "degC", eqn = "1.0\\u00B0C", prefix = FALSE),
       # "Stefan_Boltzmann_constant" = list(name = "Stefan_Boltzmann_constant", eqn = "1.0\\u03C3", prefix = FALSE),
       # "AngHertz" = list(name = "AngHertz", eqn = "1.0Hz2\\u03C0", prefix = FALSE),
       # "magnetic_constant" = list(name = "magnetic_constant", eqn = "1.0\\u03C30", prefix = FALSE),
       # "electric_constant" = list(name = "electric_constant", eqn = "1.0\\u03F50", prefix = FALSE)

       "deg" = list(name = "deg", eqn = "pi/180", prefix = FALSE),
       "Ohm" = list(name = "Ohm", eqn = "1V/A", prefix = FALSE),
       "reduced_Planck_constant" = list(name = "reduced_Planck_constant", eqn = "h/2pi", prefix = FALSE),
       "superconducting_magnetic_flux_quantum" = list(name = "superconducting_magnetic_flux_quantum", eqn = "h/(2q)", prefix = FALSE),
       "degF" = list(name = "degF", eqn = "(45967//100)Ra", prefix = FALSE),
       "degC" = list(name = "degC", eqn = "(27315//100)K", prefix = FALSE),
       "Stefan_Boltzmann_constant" = list(name = "Stefan_Boltzmann_constant", eqn = "pi^2*k^4/(60*reduced_Planck_constant^3*c^2)", prefix = FALSE),
       "AngHertz" = list(name = "AngHertz", eqn = "2pi/s", prefix = FALSE),
       "magnetic_constant" = list(name = "magnetic_constant", eqn = "4pi*(1//10)^7*H/m", prefix = FALSE),
       "electric_constant" = list(name = "electric_constant", eqn = "1/(\\u03BC0*c^2)", prefix = FALSE),
       "Bohr_magneton" = list(name = "Bohr_magneton", eqn = "q*reduced_Planck_constant/(2*me)", prefix = FALSE),
       "Rydberg_constant" = list(name = "Rydberg_constant", eqn = "10_973_731.568_160/m", prefix = FALSE)


       ) %>% return()
}
