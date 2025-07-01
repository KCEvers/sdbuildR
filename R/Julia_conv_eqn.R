

#' Convert all R equations to Julia code
#'
#' @inheritParams build
#' @inheritParams simulate_julia
#' @inheritParams clean_unit
#'
#' @return Updated sfm
#' @noRd
#'
convert_equations_julia_wrapper = function(sfm, regex_units, debug = TRUE){

  # Get variable names
  var_names = get_model_var(sfm)

  sfm[["model"]][["variables"]] = lapply(sfm[["model"]][["variables"]],
                               function(x){
    lapply(x, function(y){
      # print(y$eqn)

      if (is_defined(y[["eqn"]])){
        out = convert_equations_julia(sfm, y[["type"]], y[["name"]], y[["eqn"]], var_names,
                                              regex_units = regex_units,
                                              debug = debug)
        y = utils::modifyList(y, out)
      }
      # print(y$eqn_julia)
      return(y)
    })
  })

  # Macros
  sfm[["macro"]] = lapply(sfm[["macro"]], function(x){

    # If a name is defined, assign macro to that name (necessary for correct conversion of functions)
    if (nzchar(x[["name"]])){
      x[["eqn_julia"]] = paste0(x[["name"]], " = ", x[["eqn"]])
    } else {
      x[["eqn_julia"]] = x[["eqn"]]
    }

    out = convert_equations_julia(sfm, "macro", "macro", x[["eqn_julia"]], var_names,
                                          regex_units = regex_units, debug = debug)
    x = utils::modifyList(x, out)

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
convert_equations_julia = function(sfm, type, name, eqn, var_names, regex_units, debug = TRUE){

  if (debug) {
    # print("")
    # print(type)
    # print(name)
    print(eqn)
  }

  default_out = list(eqn_julia = "0.0",
                     # translated_func = c(),
                     # intermediary = c(),
                     func = list())

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
    stop(paste0("Parsing equation of ", name, " failed:\n", out[["message"]]))
  }

  if (any(grepl("%%", eqn))){
    stop("The modulus operator a %% b is not supported in sdbuildR. Please use mod(a, b) instead.")
  }

  if (any(grepl("na\\.rm", eqn))){
    stop("na.rm is not supported as an argument in sdbuildR. Please use na.omit(x) instead.")
  }

  # Remove comments we don't keep these
  eqn = remove_comments(eqn)[["eqn"]]

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
    eqn = convert_all_statements_julia(eqn, var_names, debug)

    # Step 4. Operators (booleans, logical operators, addition of strings)
    eqn = eqn %>%
      # # Convert addition of strings to paste0
      # conv_addition_of_strings(., var_names) %>%
      # # Replace logical operators (true, false, = (but not if in function()))
      replace_op_julia(., var_names) #%>%
    # # Replace range, e.g. range(0, 10, 2) -> 0:2:10
    # replace_range_julia(., var_names)

    # Step 5. Replace R functions to Julia functions
    conv_list = convert_builtin_functions_julia(type, name, eqn, var_names, debug = debug)
    eqn = conv_list[["eqn"]]
    add_Rcode = conv_list[["add_Rcode"]]
    # translated_func = conv_list$translated_func


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

    out =  append(list(eqn_julia = eqn), add_Rcode)

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
get_range_digits = function(eqn, var_names){

  # Get indices in variable names or quotations to exclude later
  idxs_exclude = get_seq_exclude(eqn, var_names, names_with_brackets = FALSE)

  # Locate all integers
  # idx_df = stringr::str_locate_all(eqn, "[\\.]*[0-9]+[\\.[0-9]+]*")[[1]] %>% as.data.frame()
  idx_df = stringr::str_locate_all(eqn, "(?<![a-zA-Z0-9\\.:punct:])[0-9]+(?![a-zA-Z0-9\\.:punct:])")[[1]] %>% as.data.frame()

  if (nrow(idx_df) > 0){

    # Remove matches within variable names or quotations
    idx_df = idx_df[!(idx_df[["start"]] %in% idxs_exclude | idx_df[["end"]] %in% idxs_exclude), ]

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
replace_digits_with_floats = function(eqn, var_names){

  idx_df = get_range_digits(eqn, var_names)

  if (nrow(idx_df) > 0){
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


# eqn = "c(9,4,5,'8','T','*', 4) + A - B^2 & TRUE + c(F,F,T) "

#' Translate R operators to Julia
#'
#' @inheritParams convert_equations_IM
#' @return Updated eqn
#' @importFrom rlang .data
#' @noRd
#'
replace_op_julia <- function(eqn, var_names) {

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
    df_logical_op[["match"]] = stringr::str_sub(eqn, df_logical_op[["start"]], df_logical_op[["end"]])
    df_logical_op[["replacement"]] = rep(unname(logical_op), sapply(idxs_logical_op, nrow))
    df_logical_op = df_logical_op[order(df_logical_op[["start"]]), ]
    df_logical_op

    # Remove those that are in quotation marks or names
    idxs_exclude = get_seq_exclude(eqn, var_names)

    if (nrow(df_logical_op) > 0) df_logical_op = df_logical_op[!(df_logical_op[["start"]] %in% idxs_exclude | df_logical_op[["end"]] %in% idxs_exclude), ]
    # Remove matches that are the same as the logical operator
    if (nrow(df_logical_op) > 0) df_logical_op = df_logical_op[df_logical_op[["replacement"]] != df_logical_op[["match"]], ]

    if (nrow(df_logical_op) > 0){
      # Replace in reverse order; no nested functions, so we can replace them in one go
      for (i in rev(1:nrow(df_logical_op))){
        stringr::str_sub(eqn, df_logical_op[i, ][["start"]], df_logical_op[i, ][["end"]]) = df_logical_op[i, ][["replacement"]]
      }
      # Remove double spaces
      eqn = stringr::str_replace_all(eqn, "[ ]+", " ")
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
find_round = function(df, round_brackets, eqn, var_names){

  statements = c("if", "else if", "for", "while", "function")
  if (df[["statement"]] %in% c(statements, toupper(statements))){
    matching = round_brackets[match(df[["end"]], round_brackets[["start"]]), ]
    start_round = matching[["start"]]
    end_round = matching[["end"]]
  } else {
    start_round = end_round = NA
  }

  start_word = end_word = func_name = NA

   if (df$statement %in% c("function", "FUNCTION")){

    # Get words before statement
    words = get_words(stringr::str_sub(eqn, 1, df[["start"]] - 1))
    if (nrow(words) > 0){
      # Pick last word
      word = words[nrow(words), ]
      start_word = word[["start"]]
      end_word = word[["end"]]
      func_name = word[["word"]]
    }
  }
  return(cbind(df, data.frame(start_round = start_round, end_round = end_round,
                              start_word = start_word, end_word = end_word,
                              func_name = func_name)))
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
find_curly = function(df, paired_idxs){
  statements = c("if", "else if", "for", "while", "function")
  if (df[["statement"]] %in% c(statements, toupper(statements))){
    matching = paired_idxs[which(paired_idxs[["start"]] > df[["end_round"]])[1], ]
  } else {
    matching = paired_idxs[which(paired_idxs[["start"]] == df[["end"]])[1], ]
  }
  start_curly = matching[["start"]]
  end_curly = matching[["end"]]
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
convert_all_statements_julia = function(eqn, var_names, debug){

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
    if (nrow(df_statements) > 0) df_statements = df_statements[!(df_statements[["start"]] %in% idxs_exclude | df_statements[["end"]] %in% idxs_exclude), ]

    if (!(nrow(paired_idxs) > 0 & nrow(df_statements) > 0)) {
      done = TRUE
    } else {

      # Sort by start index
      paired_idxs = paired_idxs[order(paired_idxs[["start"]]), ]

      # Get all round brackets
      round_brackets = get_range_all_pairs(eqn, var_names, type = "round")

      # print(df_statements)
#
#       # Add round brackets and curly brackets to df_statements
#       df_statements = df_statements %>%
#         dplyr::arrange(.data$start) %>%
#         # else if and if will have matching ending characters, choose one with longest statement
#         dplyr::group_by(.data$end) %>%
#         dplyr::slice_min(.data$start, n = 1) %>%
#         dplyr::ungroup() %>%
#         dplyr::mutate(id = 1:nrow(.)) %>% dplyr::group_by(.data$id) %>%
#         # Find matching round brackets
#         dplyr::group_modify(~ find_round(.x, round_brackets, eqn, var_names)) %>%
#         # Find matching curly brackets, pass df_statements to check if there is another statement after the curly bracket
#         dplyr::group_modify(~ find_curly(.x, paired_idxs)) %>%
#         dplyr::ungroup() %>%
#         dplyr::mutate(lead_start = dplyr::lead(.data$start, default = 0) - 1,
#                       # Check if there is another statement after the curly bracket
#                       next_statement = dplyr::if_else(.data$end_curly == .data$lead_start,
#                                                       dplyr::lead(.data$statement), NA))
#
#       df_statements

      #
            # Add round brackets and curly brackets to df_statements
            df_statements = df_statements[order(df_statements[["start"]]), ]
            df_statements = df_statements %>%
              # else if and if will have matching ending characters, choose one with longest statement
              dplyr::group_by(.data$end) %>%
              dplyr::slice_min(.data$start, n = 1) %>%
              dplyr::ungroup() %>%
              dplyr::mutate(id = 1:nrow(.)) %>% dplyr::group_by(.data$id) %>%
              # Find matching round brackets
              dplyr::group_modify(~ find_round(.x, round_brackets, eqn, var_names)) %>%
              # Find matching curly brackets, pass df_statements to check if there is another statement after the curly bracket
              dplyr::group_modify(~ find_curly(.x, paired_idxs)) %>%
              dplyr::ungroup()
            df_statements[["lead_start"]] = dplyr::lead(df_statements$start, default = 0) - 1
                            # Check if there is another statement after the curly bracket
            df_statements[["next_statement"]] = dplyr::if_else(df_statements[["end_curly"]] == df_statements[["lead_start"]],
                                                            dplyr::lead(df_statements[["statement"]]), NA)

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

        if (pair[["statement"]] %in% c("if")) {
          if (pair[["next_statement"]] %in% c("else if", "else")){
            stringr::str_sub(eqn, pair[["end_curly"]], pair[["end_curly"]]) = ""
          } else {
            stringr::str_sub(eqn, pair[["end_curly"]], pair[["end_curly"]]) = "end"
          }
          stringr::str_sub(eqn, pair[["start_curly"]], pair[["start_curly"]]) = ""
          stringr::str_sub(eqn, pair[["end_round"]], pair[["end_round"]]) = " "
          stringr::str_sub(eqn, pair[["start_round"]], pair[["start_round"]]) = " "
          stringr::str_sub(eqn, pair[["start"]], pair[["end"]]-1) = tolower(stringr::str_sub(eqn, pair[["start"]], pair[["end"]]-1)) # replace statement, not opening bracket

        } else if (pair$statement %in% c("else if")) {
          if (pair$next_statement %in% c("else if", "else")){
            stringr::str_sub(eqn, pair[["end_curly"]], pair[["end_curly"]]) = ""
          } else {
            stringr::str_sub(eqn, pair[["end_curly"]], pair[["end_curly"]]) = "end"
          }
          stringr::str_sub(eqn, pair[["start_curly"]], pair[["start_curly"]]) = ""
          stringr::str_sub(eqn, pair[["end_round"]], pair[["end_round"]]) = " "
          stringr::str_sub(eqn, pair[["start"]], pair[["end"]]) = "elseif " # also captures opening round bracket

        } else if (pair$statement %in% c("else")) {
          stringr::str_sub(eqn, pair[["end_curly"]], pair[["end_curly"]]) = "end"
          stringr::str_sub(eqn, pair[["start_curly"]], pair[["start_curly"]]) = ""
          stringr::str_sub(eqn, pair[["start"]], pair[["end"]]-1) = tolower(stringr::str_sub(eqn, pair[["start"]], pair[["end"]]-1)) # replace statement, not opening bracket

        } else if (pair$statement %in% c("for", "while")) {
          stringr::str_sub(eqn, pair[["end_curly"]], pair[["end_curly"]]) = "end"
          stringr::str_sub(eqn, pair[["start_curly"]], pair[["start_curly"]]) = ""
          stringr::str_sub(eqn, pair[["end_round"]], pair[["end_round"]]) = " "
          stringr::str_sub(eqn, pair[["start_round"]], pair[["start_round"]]) = " "
          stringr::str_sub(eqn, pair[["start"]], pair[["end"]]-1) = tolower(stringr::str_sub(eqn, pair[["start"]], pair[["end"]]-1)) # replace statement, not opening bracket

        } else if (pair$statement %in% c("function")) {
          stringr::str_sub(eqn, pair[["end_curly"]], pair[["end_curly"]]) = "end"
          stringr::str_sub(eqn, pair[["start_curly"]], pair[["start_curly"]]) = ""

          # Parse arguments
          arg = parse_args(stringr::str_sub(eqn, pair[["start_round"]]+1, pair[["end_round"]]-1))

          # All default arguments have to be at the end; if not, throw error
          contains_name = stringr::str_detect(arg, "=")
          arg_split = stringr::str_split_fixed(arg, "=", n = 2)
          names_arg = ifelse(contains_name, arg_split[, 1], NA) %>% trimws()

          # error when there are non-default arguments between default argumens or when default argument is not at the end
          if (any(!is.na(names_arg))){
            if (any(diff(which(!is.na(names_arg))) > 1) | max(which(!is.na(names_arg))) != length(names_arg) ){
            stop(paste0("Please change the function definition of ", pair[["func_name"]], ". All arguments with defaults have to be placed at the end of the function arguments."))
            }

          }

          arg = paste0(arg, collapse = ", ") %>%
            # Varargs (Variable Arguments): , ... -> ...
            stringr::str_replace_all(",[ ]*\\.\\.\\.", "...")

          stringr::str_sub(eqn, pair[["start_word"]], pair[["end_round"]]) = paste0("function ", pair[["func_name"]],
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
  x = idxs_newline[["end"]]
  pairs = lapply(seq(length(x) - 1), function(i){

    # Get surrounding words
    pair = data.frame(start = x[i], end = x[i + 1] - 1)
    pair[["match"]] = stringr::str_sub(eqn, pair[["start"]], pair[["end"]])
    words = get_words(pair[["match"]])
    pair[["first_word"]] = ifelse(nrow(words) > 0, words[1,"word"], "")
    pair[["second_word"]] = ifelse(nrow(words) > 1, words[2,"word"], "")

    # If second word is function, replace
    if (pair[["second_word"]] == "function"){

      pair[["match"]] = pair[["match"]] %>%
        stringr::str_replace(paste0(pair[["second_word"]], "[ ]*\\("),
                             # Edit: DON'T turn everything into keyword argument
                             paste0(pair[["first_word"]], "(") ) %>%
        # Replace assignment operator too
        stringr::str_replace(paste0(stringr::str_escape(pair[["first_word"]]), "[ ]*(=|<-)"),
                             paste0(pair[["second_word"]], " "))

      # A new line needs to be added for Julia after the function name and brackets
      # Get all round brackets
      round_brackets = get_range_all_pairs(pair[["match"]], var_names, type = "round")

      # Find the bracket which opens with ;, insert new line after closing bracket
      # end_idx = round_brackets[which(grepl("^\\(;", round_brackets$match))[1], ]$end

      # Find first opening bracket
      chosen_bracket = round_brackets$start == min(round_brackets$start)
      end_idx = round_brackets[chosen_bracket, ]$end

      # Parse arguments
      arg = parse_args(stringr::str_sub(pair[["match"]], round_brackets[chosen_bracket, "start"] + 1, end_idx - 1))

      # All default arguments have to be at the end; if not, throw error
      contains_name = stringr::str_detect(arg, "=")
      arg_split = stringr::str_split_fixed(arg, "=", n = 2)
      names_arg = ifelse(contains_name, arg_split[, 1], NA) %>% trimws()

      # error when there are non-default arguments between default argumens or when default argument is not at the end
      if (any(!is.na(names_arg))){
        if (any(diff(which(!is.na(names_arg))) > 1) | max(which(!is.na(names_arg))) != length(names_arg) ){
          stop(paste0("Please change the function definition of ", pair[["first_word"]], ". All arguments with defaults have to be placed at the end of the function arguments."))
        }
      }

      stringr::str_sub(pair[["match"]], end_idx, end_idx) = ")\n"

      # # Add end at the end, careful of comments
      # mat = stringr::str_split_fixed(pair[["match"]], "#", n = 2)
      # pair[["match"]] = paste0(mat[,1], "\nend ",
      #                     # Add comment sign back in
      #                     ifelse(nzchar(mat[,2]), "#", ""), mat[,2])

      # Add end at the end
      pair[["match"]] = paste0(pair[["match"]], "\nend")

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
#' @noRd
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
#' @noRd
#'
#' @returns Updated stock-and-flow model
order_arg_in_func_wrapper = function(sfm, var_names){

  # Collect all custom defined functions and their arguments
  all_eqns = c(lapply(sfm[["macro"]], `[[`, "eqn_julia"),
               # sfm$global$eqn_julia,
               sfm[["model"]][["variables"]] %>%
                 purrr::map_depth(2, "eqn_julia")) %>% unlist() %>%
    unname() %>% paste0(collapse = "\n")
  idxs_exclude = get_seq_exclude(all_eqns, var_names)

  # Get arguments function
  paired_idxs = get_range_all_pairs(all_eqns, var_names, type = "round")


  # Get function name
  paired_idxs[["func_name"]] = trimws(stringr::str_match(stringr::str_sub(all_eqns, 1, paired_idxs[["start"]]-1) , "function (.*?)$")[,2])
  # Pull arguments from match
  paired_idxs[["bracket_arg"]] = stringr::str_replace_all(paired_idxs[["match"]], c("\\(;" = "", "\\)$" = ""))


}



#' Add keyword arguments to custom functions in all equations
#'
#' @inheritParams build
#' @inheritParams convert_equations_IM
#'
#' @noRd
#' @returns Updated stock-and-flow model
#'
add_keyword_arg_wrapper = function(sfm, var_names){

  # Replace all function calls with named arguments
  all_eqns = c(lapply(sfm[["macro"]], `[[`, "eqn_julia"),
               sfm[["model"]][["variables"]] %>%
                 purrr::map_depth(2, "eqn_julia")) %>% unlist() %>%
    unname() %>% paste0(collapse = "\n")
  idxs_exclude = get_seq_exclude(all_eqns, var_names)

  # Get arguments function
  paired_idxs = get_range_all_pairs(all_eqns, var_names, type = "round")

  if (nrow(paired_idxs) > 0){

    # Select only function definitions
    paired_idxs = paired_idxs[grepl("\\(;", paired_idxs[["match"]]), ]

    if (nrow(paired_idxs) > 0){

      # Get function name
      paired_idxs$func_name = trimws(stringr::str_match(stringr::str_sub(all_eqns, 1, paired_idxs$start-1) , "function (.*?)$")[,2])
      # Pull arguments from match
      paired_idxs[["bracket_arg"]] = stringr::str_replace_all(paired_idxs[["match"]], c("\\(;" = "", "\\)$" = ""))

      # Create default arguments
      default_arg_list = lapply(1:nrow(paired_idxs), function(i){
          # Parse arguments
          parse_args(paired_idxs[i, "bracket_arg"]) %>%
          create_default_arg(.)}) %>% stats::setNames(paired_idxs$func_name)

      for (i in 1:nrow(paired_idxs)){
        sfm[["model"]][["variables"]] =  sfm[["model"]][["variables"]] %>% purrr::map_depth(2, function(x){
          if (is_defined(x[["eqn_julia"]])){
            x[["eqn_julia"]] = add_keyword_arg(x[["eqn_julia"]], var_names, paired_idxs[i, "func_name"], default_arg_list[[i]])
          }
          return(x)
        })

        sfm[["macro"]] = lapply(sfm[["macro"]], function(x){
          x[["eqn_julia"]] = add_keyword_arg(x[["eqn_julia"]], var_names, paired_idxs[i, "func_name"], default_arg_list[[i]])
          return(x)
        })

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
#' @noRd
#'
add_keyword_arg = function(eqn, var_names, func_name, default_arg){

  # Find all function calls in eqn
  paired_idxs = get_range_all_pairs(eqn, var_names, add_custom = paste0(func_name, "()"), type = func_name)

  if (nrow(paired_idxs) > 0){

    paired_idxs[["bracket_arg"]] = stringr::str_replace_all(paired_idxs[["match"]], paste0("^", func_name, "\\("), "") %>%
                      stringr::str_replace_all(paste0("\\)$"), "")
    # Ensure it doesn't start with ; - this is the function definition
    paired_idxs = paired_idxs[!grepl("^;", paired_idxs[["bracket_arg"]]), ]

    if (nrow(paired_idxs) > 0){

      # For each match, extract and sort arguments, and place back with named arguments; reverse order to not mess up the indices
      for (i in rev(seq.int(nrow(paired_idxs)))){
        # Parse arguments
        arg = parse_args(paired_idxs[i, ][["bracket_arg"]])

        # Sort arguments
        arg_sorted = sort_args(arg, func_name, default_arg = default_arg, var_names = var_names)

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
#' @noRd
#' @return Dataframe
get_syntax_julia = function(){

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
    "round", "round_", "syntax1", "", "", T,
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
    "logistic", "logistic", "syntax1", "", "", T,
    "logit", "logit", "syntax1", "", "", T,
    "expit", "expit", "syntax1", "", "", T,
    "convert_u", "convert_u", "syntax1", "", "", T,
    "drop_u", "Unitful.ustrip", "syntax1", "", "", T,

    # step() is already an existing function in Julia, so we use make_step() instead
    "step", "make_step", "syntax1", "", "", F,
    "pulse", "pulse", "syntax1", "", "", F,
    "ramp", "ramp", "syntax1", "", "", F,
    "seasonal", "seasonal", "syntax1", "", "", F,

    "IM_length", "length", "syntax1", "", "", F,

    "delay", "retrieve_delay", "delay", "", "", F,
    "past", "retrieve_past", "past", "", "", F,
    "delayN", "compute_delayN", "delayN", "", "", F,
    "smoothN", "compute_smoothN", "smoothN", "", "", F,

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

    ncol = 6, byrow = TRUE, dimnames = list(NULL, c("R", "julia", "syntax", "add_first_arg", "add_second_arg", "add_broadcast"))
  )

  # Convert to dataframe
  conv_df <- as.data.frame(conv_df, stringsAsFactors = FALSE)

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
#' @noRd
#' @importFrom rlang .data
#'
convert_builtin_functions_julia <- function(type, name, eqn, var_names, debug) {

  # translated_func = c()

  add_Rcode = list(func = list())

  if (grepl("[[:alpha:]]", eqn)){

    # Dataframe with regular expressions for each built-in Insight Maker function
    out = get_syntax_julia()
    syntax_df = out$syntax_df
    conv_df = out$conv_df

    # intermediary = compute = update = setup = c()

    # Preparation for first iteration
    done = FALSE
    i = 1
    R_regex = syntax_df$R_regex_first_iter

    while (!done) {

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

      # idx_df <- idx_df[!((idx_df[["start"]] %in% idxs_exclude) | (idx_df[["end"]] %in% idxs_exclude)), ]
      if (nrow(idx_df) > 0) idx_df = idx_df[!(idx_df[["start"]] %in% idxs_exclude | idx_df[["end"]] %in% idxs_exclude), ]
      if (nrow(idx_df) == 0){
        done = TRUE
        next
      }


      # For the first iteration, add _replace to all detected functions, so we don't end in an infinite loop (some Julia and R functions have the same name)
      if (i == 1 & nrow(idx_df) > 0){

        idx_df <- idx_df[order(idx_df[["start"]]), ]
        idx_df$R_regex <- stringr::str_replace_all(idx_df[["R_regex"]],
                                            stringr::fixed(c("(?<!\\.)\\b" = "", "\\(" = "(", "\\)" = ")")))

        for (j in rev(1:nrow(idx_df))){
          stringr::str_sub(eqn, idx_df[j, "start"], idx_df[j, "end"]) = idx_df[j, ][["R_regex"]] #%>%
        }
      }

      if (i == 1){
        # print(eqn)
        R_regex = syntax_df[["R_regex"]]
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
            paired_idxs[paired_idxs[["type"]] == "round", ],
            idx_df,
            by.x = "start",
            by.y = "end"
          )
          idx_funcs[["start_bracket"]] = idx_funcs[["start"]]
          idx_funcs[["start"]] = idx_funcs[["start.y"]]


          df2 = idx_df[idx_df[["syntax"]] == "syntax1b", ]
          # Add start_bracket column to prevent errors
          df2[["start_bracket"]] = df2[["start"]]
          # Add back syntax1b which does not need brackets
          idx_funcs = dplyr::bind_rows(idx_funcs, df2)
          idx_funcs = idx_funcs[order(idx_funcs[["end"]]), ]
          idx_funcs


          # # Match the opening bracket of each function to round brackets in paired_idxs
          # idx_funcs = merge(
          #   paired_idxs[paired_idxs$type == "round", ],
          #   idx_df,
          #   by.x = "start",
          #   by.y = "end"
          # ) %>% dplyr::rename(start_bracket = "start", start = "start.y") %>%
          #   # Add back syntax1b which does not need brackets
          #   dplyr::bind_rows(idx_df[idx_df$syntax %in% c("syntax0b", "syntax1b"), ] %>%
          #                      # Add start_bracket column to prevent errors
          #                      dplyr::mutate(start_bracket = .data$start)) %>%
          #   dplyr::arrange(.data$end)
          # idx_funcs

        } else {
          # If there are no brackets in the eqn:
          idx_funcs = idx_df
          # Add start_bracket column to prevent errors
          idx_funcs$start_bracket = idx_funcs$start
        }

        # Start with most nested function
        idx_funcs_ordered = idx_funcs
        idx_funcs_ordered[["is_nested_around"]] = any(idx_funcs_ordered[["start"]] < idx_funcs[["start"]] &
                                                   idx_funcs_ordered[["end"]] > idx_funcs[["end"]])
        idx_funcs_ordered = idx_funcs_ordered[order(idx_funcs_ordered[["is_nested_around"]]), ]
        idx_func = idx_funcs_ordered[1, ] # Select first match

        if (debug){
          print("idx_func")
          print(idx_func)
        }

        # Extract argument between brackets (excluding brackets)
        bracket_arg = stringr::str_sub(eqn, idx_func[["start_bracket"]] + 1, idx_func[["end"]] - 1)

        # print("bracket_arg")
        # print(bracket_arg)

        arg = parse_args(bracket_arg)
        arg = sort_args(arg, idx_func[["R_first_iter"]], var_names = var_names)
        arg = unname(unlist(arg))

        # print("arg")
        # print(arg)

        # Indices of replacement in eqn
        start_idx = idx_func[["start"]]
        end_idx = idx_func[["end"]]

        if (idx_func[["syntax"]] == "syntax0") {

          # arg = paste0(arg, collapse = ", ")
          replacement = idx_func[["julia"]]

        } else if (idx_func[["syntax"]] == "syntax1") {

          arg = paste0(arg, collapse = ", ")

          replacement = sprintf(
            "%s%s(%s%s%s%s%s)",
            idx_func[["julia"]],
            ifelse(idx_func[["add_broadcast"]], ".", ""),
            idx_func[["add_first_arg"]],
            ifelse(nzchar(idx_func[["add_first_arg"]]) & nzchar(arg), ", ", ""),
            arg,
            idx_func[["add_second_arg"]],
            ifelse(nzchar(idx_func[["add_second_arg"]]) & nzchar(arg), ", ", "")
          )

        } else if (idx_func[["syntax"]] == "delay"){

          if (type %in% c("stock", "gf", "constant", "macro")){
            stop(paste0("Adjust equation of ", name, ": delay() cannot be used for a ", type, "."))
          }

          # Check arguments
          arg[2] = trimws(arg[2])
          if (arg[2] == "0" || arg[2] == "0.0" || arg[2] == "0L"){
            stop(paste0("Adjust equation of ", name, ": the delay length in delay() must be greater than 0."))
          }

          func_name = paste0(name, P$delay_suffix, length(add_Rcode[["func"]][[idx_func[["syntax"]]]]) + 1)
          arg3 = ifelse(length(arg) > 2, arg[3], "nothing")

          replacement = paste0(idx_func[["julia"]], "(",
                               arg[1], ", ",
                               arg[2], ", ",
                               arg3, ", ",
                               P[["time_name"]],
                               # ", \"", arg[1], "\", \"single\", ",
                               # Symbols are faster
                               ", :", arg[1],
                               # ", \"single\", ",
                               ", ",

                               P[["intermediaries"]], ", ", P[["intermediary_names"]], ")")

          add_Rcode[["func"]][[idx_func[["syntax"]]]][[func_name]] = list(var = arg[1],
                                                                     length = arg[2],
                                                                     initial = arg3
          )

        } else if (idx_func[["syntax"]] == "past"){


          if (type %in% c("stock", "gf", "constant", "macro")){
            stop(paste0("Adjust equation of ", name, ": past() cannot be used for a ", type, "."))
          }

          # Check arguments
          arg[2] = trimws(arg[2])
          if (arg[2] == "0" || arg[2] == "0.0" || arg[2] == "0L"){
            stop(paste0("Adjust equation of ", name, ": the past interval in past() must be greater than 0."))
          }

          arg2 = ifelse(length(arg) > 1, arg[2], "nothing")
          func_name = paste0(name, P[["past_suffix"]], length(add_Rcode[["func"]][[idx_func[["syntax"]]]]) + 1)
          replacement = paste0(idx_func[["julia"]], "(",
                               arg[1], ", ",
                               arg2, ", nothing, ",
                               P[["time_name"]],
                               # ", \"", arg[1], "\", \"interval\", ",
                               # Symbols are faster
                               ", :", arg[1],
                               # ", \"interval\", ",
                               ", ",
                               P[["intermediaries"]], ", ", P[["intermediary_names"]], ")")
          add_Rcode[["func"]][[idx_func[["syntax"]]]][[func_name]] = list(var = arg[1],
                                                                     length = arg2)


        } else if (idx_func[["syntax"]] == "delayN"){


          if (type %in% c("stock", "gf", "constant", "macro")){
            stop(paste0("Adjust equation of ", name, ": delayN() cannot be used for a ", type, "."))
          }

          # Check arguments
          arg[2] = trimws(arg[2])
          if (arg[2] == "0" || arg[2] == "0.0" || arg[2] == "0L"){
            stop(paste0("Adjust equation of ", name, ": the delay length in delayN() must be greater than 0."))
          }

          if (arg[3] == "0" || arg[3] == "0.0" || arg[3] == "0L"){
            stop(paste0("Adjust equation of ", name, ": the delay order in delayN() must be greater than 0."))
          }

          arg4 = ifelse(length(arg) > 3, arg[4], arg[1])

          # Number delayN() as there may be multiple
          func_name = paste0(name, P[["delayN_suffix"]], length(add_Rcode[["func"]][[idx_func[["syntax"]]]]) + 1)

          replacement = paste0(func_name, P[["outflow_suffix"]])
          setup = paste0("setup_delayN(", arg4, ", ", arg[2], ", ", arg[3],
                         # ", \"", func_name, "\")")
                         # Symbols are faster
                         ", :", func_name, ")")
          compute = paste0(idx_func[["julia"]], "(",
                                    arg[1], ", ",
                           func_name, ", ",
                                    arg[2], ", ",
                                    arg[3], ")")

          update = paste0(func_name, ".update")

          add_Rcode[["func"]][[idx_func[["syntax"]]]][[func_name]] = list(setup = setup,
                                                                     compute = compute,
                                                                     update = update,
                                                                     type = idx_func[["julia"]],
                                                                     var = arg[1],
                                                                     length = arg[2],
                                                                     order = arg[3],
                                                                     initial = arg4)



        } else if (idx_func[["syntax"]] == "smoothN"){


          if (type %in% c("stock", "gf", "constant", "macro")){
            stop(paste0("Adjust equation of ", name, ": smoothN() cannot be used for a ", type, "."))
          }

          # Check arguments
          arg[2] = trimws(arg[2])
          if (arg[2] == "0" || arg[2] == "0.0" || arg[2] == "0L"){
            stop(paste0("Adjust equation of ", name, ": the smoothing time in smoothN() must be greater than 0."))
          }

          arg[3] = trimws(arg[3])
          if (arg[3] == "0" || arg[3] == "0.0" || arg[3] == "0L"){
            stop(paste0("Adjust equation of ", name, ": the smoothing order in smoothN() must be greater than 0."))
          }

          arg4 = ifelse(length(arg) > 3, arg[4], arg[1])

          # Number smoothN() as there may be multiple
          func_name = paste0(name, P[["smoothN_suffix"]], length(add_Rcode[["func"]][[idx_func[["syntax"]]]]) + 1)

          replacement = paste0(func_name, P[["outflow_suffix"]])
          setup = paste0("setup_smoothN(", arg4, ", ", arg[2], ", ", arg[3],
                         # ", \"", func_name, "\")")
                         # Symbols are faster
                         ", :", func_name, ")")
          compute = paste0(idx_func[["julia"]], "(",
                           arg[1], ", ",
                           func_name, ", ",
                           arg[2], ", ",
                           arg[3], ")")

          update = paste0(func_name, ".update")

          # add_Rcode[["func"]] = append(add_Rcode[["func"]],
          #                                   list(list(setup = setup,
          #                                             compute = compute,
          #                                             update = update,
          #                                             type = idx_func[["julia"]],
          #                                             var = arg[1],
          #                                             length = arg[2],
          #                                             order = arg[3],
          #                                             initial = arg4
          #                                             )) %>% stats::setNames(func_name))

          add_Rcode[["func"]][[idx_func[["syntax"]]]][[func_name]] = list(setup = setup,
                                                                     compute = compute,
                                                                     update = update,
                                                                     type = idx_func[["julia"]],
                                                                     var = arg[1],
                                                                     length = arg[2],
                                                                     order = arg[3],
                                                                     initial = arg4
          )

        } else if (idx_func[["syntax"]] == "syntaxD"){

          # Convert random number generation
          replacement = conv_distribution(arg,
                                          # idx_func$R_first_iter,
                                          idx_func[["julia"]],
                                          idx_func[["add_first_arg"]])
        }

        if (debug){
          print(stringr::str_sub(eqn, start_idx, end_idx))
          print(replacement)
          print("")
        }

        # Replace eqn
        stringr::str_sub(eqn, start_idx, end_idx) = replacement

        # translated_func = c(translated_func, idx_func$R)
      }
    }
    # add_Rcode = list(list(add_Rcode) %>% stats::setNames(name)) %>%
      # stats::setNames(type)

}
  return(list(eqn = eqn, add_Rcode = add_Rcode
              # , translated_func = translated_func
              ))

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
sort_args = function(arg, func_name, default_arg = NULL, var_names = NULL){

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

    # Check whether all argument names are in the allowed argument names in case of no dots argument (...)
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
    if (length(idx) > 0 && length(standard_order) > 0){
      new_names = setdiff(standard_order, stats::na.omit(names_arg)) # names which are missing from the passed argument names
      names_arg[idx] = new_names[1:length(idx)] # Assign new names to unnamed arguments; only select as many as there are unnamed arguments
    }


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
    arg_R = utils::modifyList(default_arg_list, as.list(stats::setNames(values_arg, names_arg)))

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

    # Ensure digits become floats for Julia
    for (name in names(arg_R)) {
      if (!is.null(arg_R[[name]])){
        arg_R[[name]] <- replace_digits_with_floats(arg_R[[name]], var_names)
      }
    }

  }

  arg_R = lapply(arg_R, as.character)

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
conv_distribution = function(arg, julia_func, distribution){


  # If n = 1, don't include it, as rand(..., 1) generates a vector. n is the first argument.

  julia_str = sprintf("%s(%s(%s), %d)",
                      julia_func, distribution,
                      # Don't include names of arguments
                      paste0(arg[-1], collapse = ", "), as.numeric(arg[1]))

  if (as.numeric(arg[1]) == 1 & julia_func == "rand"){
    julia_str = sprintf("%s(%s(%s))",
                        julia_func, distribution,
                        # Don't include names of arguments
                        paste0(arg[-1], collapse = ", "))

  } else if (julia_func == "Distributions.cdf."){

    # log = TRUE
    if (arg[length(arg)] == "TRUE"){
      julia_str = sprintf("log%s(%s(%s), %d)",
                          julia_func, distribution,
                          # Don't include names of arguments; skip log
                          paste0(arg[-c(1,  length(arg)-1, length(arg))], collapse = ", "), as.numeric(arg[1]))
    } else {

      julia_str = sprintf("%s(%s(%s), %d)",
                          julia_func, distribution,
                          # Don't include names of arguments; skip log
                          paste0(arg[-c(1,  length(arg)-1, length(arg))], collapse = ", "), as.numeric(arg[1]))
    }

  } else if (julia_func == "Distributions.pdf."){

    # log.p = TRUE
    if (arg[length(arg)] == "TRUE"){
      julia_str = sprintf("log%s(%s(%s), %d)",
                          julia_func, distribution,
                          # Don't include names of arguments; skip lower.tail and log.p
                          paste0(arg[-c(1, length(arg))], collapse = ", "), as.numeric(arg[1]))
    } else {
      julia_str = sprintf("%s(%s(%s), %d)",
                          julia_func, distribution,
                          # Don't include names of arguments; skip lower.tail and log.p
                          paste0(arg[-c(1, length(arg))], collapse = ", "), as.numeric(arg[1]))
    }

  } else if (julia_func == "Distributions.quantile."){

    # log = TRUE
    if (arg[length(arg)] == "TRUE"){
      julia_str = sprintf("invlogcdf(%s(%s), %d)",
                          distribution,
                          # Don't include names of arguments; skip lower.tail and log.p
                          paste0(arg[-c(1, length(arg)-1, length(arg))], collapse = ", "), as.numeric(arg[1]))
    } else {
      julia_str = sprintf("%s(%s(%s), %d)",
                          julia_func, distribution,
                          # Don't include names of arguments; skip lower.tail and log.p
                          paste0(arg[-c(1, length(arg)-1, length(arg))], collapse = ", "), as.numeric(arg[1]))
    }
  }


  return(julia_str)
}



#' Translate vector bracket syntax from R to square brackets in Julia
#'
#' @inheritParams convert_equations_IM
#' @return Updated eqn
#' @noRd
#'
vector_to_square_brackets = function(eqn, var_names) {

  # Get indices of all enclosures
  paired_idxs = get_range_all_pairs(eqn, var_names, type = "vector",
                                    names_with_brackets = FALSE)

  # Remove those that are preceded by a letter
  if (nrow(paired_idxs) > 0) paired_idxs = paired_idxs[!stringr::str_detect(stringr::str_sub(eqn, paired_idxs$start - 1, paired_idxs$start - 1), "[[:alpha:]]"), ]

  if (nrow(paired_idxs) > 0) {

    # First replace all closing brackets with ]
    chars <- strsplit(eqn, "", fixed = TRUE)[[1]]
    chars[paired_idxs$end] <- "]"
    eqn = paste0(chars, collapse = "")

    # Order paired_idxs by start position
    paired_idxs = paired_idxs[order(paired_idxs$start), ]

    # Replace opening brackets c( with [
    for (j in rev(1:nrow(paired_idxs))) {
      # Replace c( with [
      stringr::str_sub(eqn, paired_idxs[j, "start"], paired_idxs[j, "start"] + 1) = "["
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
scientific_notation = function(eqn, task = c("remove", "add")[1], digits_max = 15){

  eqn = as.character(eqn)

  if (task == "remove"){
    scientific = FALSE
    # Regex for scientific notation
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
    pattern = pattern,
    replacement = reformat_scientific
  )

  return(eqn)
}


