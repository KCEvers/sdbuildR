# Replacement functions for InsightMaker to R

#' Transform InsightMaker eqn to R code
#'
#' @param eqn String with InsightMaker eqn, but translated R names
#' @param var_names Dataframe with type, name, label and units per variable
#' @param name R name of variable to which the eqn belongs
#' @param type Name of model element to which the eqn belongs
#' @param debug Boolean; whether to be talkative; defaults to FALSE
#' @inheritParams clean_unit
#'
#' @return Dataframe with transformed eqn and additional R code needed to make the eqn function
#' @importFrom rlang .data
#'
convert_equations_IM = function(
                        # Simulation time unit used for converting all time units to the same unit
                        # Name of model element, only needed in step, ramp, pulse functions
                        type,
                        name,
                        eqn,
                        var_names,
                        regex_units,
                        debug = FALSE) {
  if (debug) {
    print("")
    # print(type)
    print(name)
    print(eqn)
  }

  # Check whether eqn is empty or NULL
  if (is.null(eqn) || !nzchar(eqn)){
    return(eqn)
  }

  # Don't translate interpolation functions, have already been converted to R code
  # if (stringr::str_detect(name, stringr::fixed(sprintf("(%s)", P$time_name)))){
  # if (stringr::str_detect(eqn, stringr::fixed("stats::approxfun"))){
  # if (type == "Converter"){
  #     return(data.frame(conv_property = "convert_equations_IM", value = eqn, name = name, type = type) )
  # }


  # If equation is now empty, don't run rest of functions but set equation to zero
  if (!nzchar(eqn)){
    eqn = "0.0"
    translated_func = c()
    add_Rcode = list()
  } else {

  # # If the eqn is only comments, space characters, and/or curly brackets, put 0 as default value
  # # Remove comments (everything after '#')
  # formula_no_comments <- strsplit(eqn, "\n")[[1]] %>% sub("#.*$", "", .) %>% stringr::str_replace_all(c("\\s" = "", "\\{" = "", "\\}" = ""))
  # if (all(!nzchar(formula_no_comments))){
  #   eqn = paste0("0 # Default of 0, eqn is not well-defined\n", eqn)
  # }

    # Step 2. Syntax (bracket types, destructuring assignment, time units {1 Month})
    eqn = eqn %>%
      # Replace curly brackets {} with c()
      curly_to_vector_brackets(., var_names) %>%
      # Destructuring assignment, e.g. x, y <- {a, b}
      conv_destructuring_assignment()


    # Step 3. Statements (if, for, while, functions, try)
    eqn = convert_all_statements(eqn, var_names, debug)

    # Step 4. Operators (booleans, logical operators, addition of strings)
    eqn = eqn %>%
      # Convert addition of strings to paste0
      convert_addition_of_strings(., var_names) %>%
      # Replace logical operators (true, false, = (but not if in function()))
      replace_op_IM(., var_names) %>%
      # Replace range, e.g. 0:2:10; replace other colons : with =
      replace_colon(., var_names)

    # Step 5. Replace built-in functions
    conv_list = convert_builtin_functions_IM(type, name, eqn, var_names, debug = debug)
    eqn = conv_list$eqn
    add_Rcode = conv_list$add_Rcode
    translated_func = conv_list$translated_func

    # Ensure units which need scientific notation have it
    eqn = clean_unit_in_u(eqn, regex_units)

    # Replace two consecutive newlines and trim white space
    eqn = stringr::str_replace_all(trimws(eqn), "\\\n[ ]*\\\n", "\n")


    # If it is a multi-line statement, surround by brackets in case they aren't macros
    eqn = trimws(eqn)
    if (stringr::str_detect(eqn, stringr::fixed("\n")) & name != "global") {
      eqn = paste0("{\n", eqn, "\n}")
    }


    if (debug){print(eqn)}
  }

  out = list(list(list(eqn = eqn,
                       translated_func = translated_func)) %>%
               stats::setNames(name)) %>%
    stats::setNames(type) %>%
    append(add_Rcode)

  return(out)
}



#' Replace comment characters from InsightMaker to R
#'
#' @inheritParams convert_equations_IM
#'
#' @return Updated eqn
#'
replace_comments = function(eqn) {
  comment_char = c("//", "/*", "*/")
  replacements = c("#", "#", "\n")

  done = FALSE

  while (!done) {
    idxs_comments_ = stringr::str_locate_all(eqn, stringr::fixed(comment_char))
    idxs_comments = as.data.frame(do.call(rbind, idxs_comments_))

    if (nrow(idxs_comments) == 0){
      done = TRUE
      next
    }

    idxs_comments$char = rep(comment_char, sapply(idxs_comments_, nrow))

    # Remove those that are in comments (#) or quotation marks
    idxs_exclude = get_seq_exclude(eqn, type = "quot", names_with_brackets = TRUE)
    idxs_comments = idxs_comments[!(idxs_comments$start %in% idxs_exclude | idxs_comments$end %in% idxs_exclude), ]

    if (nrow(idxs_comments) == 0){
      done = TRUE
    } else {
      idx_comments = idxs_comments[1,]
      replacement = replacements[match(idx_comments$char, comment_char)]
      stringr::str_sub(eqn, idx_comments[["start"]], idx_comments[["end"]]) = replacement
    }

  }
  return(eqn)
}



#' Get start and end indices of all comments
#'
#' @inheritParams convert_equations_IM
#' @return Dataframe with start and end indices of all comments in eqn
#'
get_range_comments <- function(eqn) {

  # Get indices of string which are in comments
  idxs_comments = stringr::str_locate_all(eqn, "#")[[1]][, "start"]

  # Get indices of new line (where comment ends), and append length of string
  idxs_newline = unname(stringr::str_locate_all(eqn, "\n")[[1]][, 1]) %>%
    c(., nchar(eqn) + 1) # Add 1 because of subtraction later

  if (length(idxs_comments) > 0) {

    # Create sequence from comment character to next closest new line or the closest new comment
    pair_comments = lapply(idxs_comments, function(i) {
      c(i, min(
        # Next closest comment
        idxs_comments[idxs_comments > i][1],
        # Next closest newline
        idxs_newline[idxs_newline > i][1], na.rm = T) - 1) # Subtract 1 to not include it
    }) %>% do.call(rbind, .) %>% magrittr::set_colnames(c("start", "end")) %>%
      as.data.frame()

  } else {
    pair_comments = data.frame()
  }
  return(pair_comments)
}



#' Clean equation and extract comments
#'
#' @inheritParams convert_equations_IM
#'
#' @returns List with cleaned eqn and extracted comments
#'
remove_comments = function(eqn){

  if (grepl("#", eqn)){

    # Find indices of comments
    comment_df = get_range_comments(eqn)

    # Get indices of comments
    # seq_idxs_range = unlist(purrr::map2(comment_df[,"start"], comment_df[,"end"], seq))
    seq_idxs_range <- unlist(mapply(seq, comment_df[,"start"], comment_df[,"end"], SIMPLIFY = FALSE))

    split_formula = strsplit(eqn, "")[[1]]

    # Get comments as documentation and remove comments from eqn
    doc = trimws(paste0(split_formula[seq_idxs_range], collapse = ""))
    eqn = trimws(paste0(split_formula[-seq_idxs_range], collapse = ""))

    if (!nzchar(eqn)){
      eqn = "0.0"
    }

  } else {
    doc = ""
  }
  return(list(eqn = eqn,
              doc = doc))
}


#' Translate InsightMaker destructuring assignment to R
#'
#' @inheritParams convert_equations_IM
#'
#' @return Updated eqn
#'
conv_destructuring_assignment = function(eqn) {
  done = FALSE

  while (!done) {
    # In-line function assignment without using the word function
    idxs_inline = stringr::str_locate_all(eqn,
                                              stringr::regex(
                                                "^(.*?)\\)[ ]*[^%]<-",
                                                dotall = F,
                                                multiline = T
                                              ))[[1]]
    idxs_inline

    if (nrow(idxs_inline) == 0) {
      done = TRUE
    } else {

      idx_inline = idxs_inline[1, ]
      # Change to one-liner function
      stringr::str_sub(eqn, idx_inline["start"], idx_inline["end"]) =
        stringr::str_sub(eqn, idx_inline["start"], idx_inline["end"]) %>%
              stringr::str_replace("[ ]*<-[ ]*", " ") %>%
        stringr::str_replace("\\(", " <- function(")  # Replace first opening bracket; important to change it to <- and not =, as otherwise = will be replaced by ==

    }


    # Check whether any string uses destructuring assignment
    idxs_assignment = stringr::str_locate_all(eqn,
                                              stringr::regex(
                                                # Match shouldn't contain square brackets, e.g. res[row, col] <- total
                                                # "^[ ]*[^\\{\\}]+,[^\\{\\}]+[^%]<-",
                                                "^[ ]*(.*?),(.*?)[^%]<-",
                                                dotall = F,
                                                multiline = T
                                              ))[[1]]
    idxs_assignment

    # Check for comma before <-
    if (nrow(idxs_assignment) == 0) {
      done = TRUE
    } else {

      idx_assignment = idxs_assignment[1, ]
      assignment_to = stringr::str_sub(eqn, idx_assignment["start"], idx_assignment["end"] - 2)

      # Ensure there are no brackets before <-
      if (!(stringr::str_detect(assignment_to, "\\[|\\{") & stringr::str_detect(assignment_to, "\\]|\\}"))){

        stringr::str_sub(eqn, idx_assignment["start"], idx_assignment["end"]) =
          # Enclose variable names before <- in c()
          paste0(stringr::str_extract(assignment_to, "^[ ]*"), # Leading spaces
                 "c(", trimws(assignment_to), # Replace <- with zeallot's %<-%
          ") %<-%")
      }

    }
  }

  return(eqn)
}





#' Translate InsightMaker colon operator to R
#'
#' @inheritParams convert_equations_IM
#' @return Updated eqn
#'
replace_colon = function(eqn, var_names) {

  # Replace range including by argument
  eqn = stringr::str_replace_all(eqn,
                           "([0-9.-]+):([0-9.-]+):([0-9.-]+)",
                           "seq\\(\\1, \\3, by = \\2\\)")

  # Get indices of all colons
  idxs_colon = stringr::str_locate_all(eqn, ":")[[1]][,"start"]

  # Remove those that are in quotation marks or names
  idxs_exclude = get_seq_exclude(eqn, var_names, names_with_brackets = TRUE)
  idxs_colon = setdiff(idxs_colon, idxs_exclude)

  # Don't keep those that are bordered by numbers
  idxs_range = stringr::str_locate_all(eqn, "([0-9.-]+):([0-9.-]+)")[[1]]
  # seq_idxs_range = unlist(purrr::map2(idxs_range[,"start"], idxs_range[,"end"], seq))
  seq_idxs_range <- unlist(mapply(seq, idxs_range[,"start"], idxs_range[,"end"], SIMPLIFY = FALSE))

  # Only keep those between vector brackets
  paired_idxs = get_range_all_pairs(eqn, var_names, type = "vector", names_with_brackets = TRUE)
  # for (i in 1:nrow(paired_idxs)){
  #   print(stringr::str_sub(eqn, paired_idxs[i,"start"],paired_idxs[i,"end"]))
  # }
  # seq_paired_idxs = unlist(purrr::map2(paired_idxs$start, paired_idxs$end, seq))
  seq_paired_idxs <- unlist(mapply(seq, paired_idxs$start, paired_idxs$end, SIMPLIFY = FALSE))

  # Replace colons that are not used to define a range with "="
  replace_idxs = idxs_colon[(idxs_colon %in% seq_paired_idxs) & !(idxs_colon %in% seq_idxs_range)]

  for (i in replace_idxs){
    stringr::str_sub(eqn, i, i) = "="
    # print(stringr::str_sub(eqn, i-10, i+10))
  }

  return(eqn)
}




#' Translate curly bracket syntax from InsightMaker to R
#'
#' @inheritParams convert_equations_IM
#' @return Updated eqn
#'
curly_to_vector_brackets = function(eqn, var_names) {
  # print(eqn)

  # Curly brackets can be:
  # - Indexers, e.g. "b{1}.length()"
  # if it is adjacent to [a-zA-Z0-9] or {}, it's indexing
  # - Vectors
  # - Nested lists

  done = F
  max_iter = 1000
  i = 1
  while (!done & i < max_iter) {

    # Get indices of all enclosures
    paired_idxs = get_range_all_pairs(eqn, var_names, add_custom = "list()", names_with_brackets = TRUE)

    # Check nesting
    if (nrow(paired_idxs) > 0) {

      # Create dataframe with properties per pair
      paired_idxs_prop = paired_idxs
      paired_idxs_prop$id = 1:nrow(paired_idxs_prop)

      # paired_idxs_prop = paired_idxs_prop %>%  dplyr::rowwise() %>%
      #   # Find which pairs this pair is nested within; this will be in order of highest to lowest level
      #   dplyr::mutate(
      #     nested_within = which(
      #       paired_idxs$start < .data$start & paired_idxs$end > .data$end
      #     ) %>% paste0(collapse = ","),
      #     nr_nesting_levels = strsplit(nested_within, ",")[[1]] %>% length(),
      #     nested_around = which(
      #       paired_idxs$start > .data$start &  paired_idxs$end < .data$end
      #     ) %>% paste0(collapse = ","),
      #     # First character to the left
      #     left_adjacent_char = stringr::str_sub(eqn, .data$start - 1, .data$start - 1),
      #     # Preceding string
      #     preceding_str = stringr::str_extract(
      #       stringr::str_sub(eqn, 1, .data$start - 1),
      #       "\\b[\\w\\.\\\\]+$"# All word characters, periods . and slashes \\
      #     ),
      #     is_start_of_string = .data$start == 1,
      #     # string =  stringr::str_sub(eqn, .data$start - 10, .data$start + 10),
      #     # right_adjacent_char = stringr::str_sub(eqn, .data$end - 1, .data$end - 1),
      #     # is_end_of_string = .data$end == stringr::str_length(eqn)
      #   ) %>%
      #   dplyr::ungroup()
      # paired_idxs_prop %>% as.data.frame()

        # Find which pairs this pair is nested within; this will be in order of highest to lowest level
        paired_idxs_prop$nested_within = which(
          paired_idxs$start < paired_idxs_prop$start & paired_idxs$end > paired_idxs_prop$end
        ) %>% paste0(collapse = ",")
        paired_idxs_prop$nr_nesting_levels = length(strsplit(paired_idxs_prop$nested_within, ",")[[1]])
        paired_idxs_prop$nested_around = which(
          paired_idxs$start > paired_idxs_prop$start &  paired_idxs$end < paired_idxs_prop$end
        ) %>% paste0(collapse = ",")
        # First character to the left
        paired_idxs_prop$left_adjacent_char = stringr::str_sub(eqn, paired_idxs_prop$start - 1, paired_idxs_prop$start - 1)
        # Preceding string
        paired_idxs_prop$preceding_str = stringr::str_extract(
          stringr::str_sub(eqn, 1, paired_idxs_prop$start - 1),
          "\\b[\\w\\.\\\\]+$"# All word characters, periods . and slashes \\
        )
        paired_idxs_prop$is_start_of_string = paired_idxs_prop$start == 1
        # string =  stringr::str_sub(eqn, .data$start - 10, .data$start + 10),
        # right_adjacent_char = stringr::str_sub(eqn, .data$end - 1, .data$end - 1),
        # is_end_of_string = .data$end == stringr::str_length(eqn)



      if (any(paired_idxs_prop$type == "curly")) {
        # Start with most nested string
        chosen_pair = paired_idxs_prop[paired_idxs_prop$type == "curly", ]
        chosen_pair = chosen_pair[order(chosen_pair$start, decreasing = TRUE), ][1, ]

        # Find type of enclosure of the lowest order bracket it is nested within
        if (nzchar(chosen_pair$nested_within)) {
          nested_within = paired_idxs_prop[paired_idxs_prop$id == as.numeric(
              strsplit(chosen_pair$nested_within, ",")[[1]] %>% dplyr::last()
            ), "type"]
        } else {
          nested_within = ""
        }

        # Find type of enclosure of the highest order bracket it is nested around
        if (nzchar(chosen_pair$nested_around)) {
          nested_around = paired_idxs_prop[paired_idxs_prop$id == as.numeric(strsplit(
              chosen_pair$nested_around, ","
            )[[1]][[1]]), "type"]
        } else {
          nested_around = ""
        }

        # It is a vector if it is not nested around anything
        x_is_vector = (!nzchar(nested_around) |
                         (nested_around %in% c("quot", "round"))) &
          (!nzchar(nested_within) |
             (
               nested_within %in% c("curly", "round", "vector", "square")
             ))
        # It is a list if...
        x_is_list = nested_around %in% c("curly", "list", "vector")
        # It is an indexer if...
        x_is_indexer = stringr::str_detect(chosen_pair$left_adjacent_char,
                                           "[a-zA-Z0-9._\\}\\)]") &
          (!chosen_pair$is_start_of_string) &
          !(grepl("\\n", chosen_pair$preceding_str, fixed = T))

        start_idx = chosen_pair$start
        end_idx = chosen_pair$end
        string = stringr::str_sub(eqn, start_idx + 1, end_idx - 1)

        # print(string)

        if (x_is_indexer) {
          replacement = paste0("[", string, "]")
        } else if (x_is_vector) {
          replacement = paste0("c(", string, ")")
        } else if (x_is_list) {
          replacement = paste0("list(", string, ")")
        } else {
          replacement =  stringr::str_sub(eqn, start_idx, end_idx) # No replacement
        }

        # print(replacement)

        # Replace in string
        stringr::str_sub(eqn, start_idx, end_idx) = replacement
        i = i + 1
      } else {
        done = TRUE
      }
    } else {
      done = TRUE
    }
  }

  return(eqn)
}






#' Translate InsightMaker operators to R
#'
#' @inheritParams convert_equations_IM
#' @return Updated eqn
#' @importFrom rlang .data
#'
replace_op_IM <- function(eqn, var_names) {

  # InsightMaker uses "and", "AND", "or", "OR", "not", "NOT"
  logical_op = c(
    # Insight Maker is case-insensitive
    "and" = "&",
    "or" = "|",
    "not" = "!",
    "true" = "TRUE",
    "false" = "FALSE",
    # mod in Insight Maker is actually the remainder operator!
    "mod" = "%REM%",
    # Euler
    "e" = "exp(1)"
  )
  # Operator words cannot be preceded or followed by a letter
  names(logical_op) = paste0("\\b", names(logical_op), "\\b")
  logical_op = c(logical_op, c("(?<!=|<|>)=(?!=)" = "==", "<>" = "!="))

  # Find indices of logical operators
  idxs_logical_op = stringr::str_locate_all(eqn, stringr::regex(names(logical_op), ignore_case = TRUE))

  if (length(unlist(idxs_logical_op)) > 0){

    # Get match and replacement
    df_logical_op = as.data.frame(do.call(rbind, idxs_logical_op))
    df_logical_op$match = stringr::str_sub(eqn, df_logical_op$start, df_logical_op$end)
    df_logical_op$replacement = rep(unname(logical_op), sapply(idxs_logical_op, nrow))
    df_logical_op = df_logical_op[order(df_logical_op$start), ]

    # Remove those that are in quotation marks or names
    idxs_exclude = get_seq_exclude(eqn, var_names, names_with_brackets = TRUE)
    if (nrow(df_logical_op) > 0) df_logical_op = df_logical_op[!(df_logical_op$start %in% idxs_exclude | df_logical_op$end %in% idxs_exclude), ]

    # Remove matches that are the same as the logical operator
    if (nrow(df_logical_op) > 0) df_logical_op = df_logical_op[df_logical_op$replacement != df_logical_op$match, ]

    if (nrow(df_logical_op) > 0){

      # In case of "=", remove those that are in function(...), as these are for default assignment of arguments and should stay as =
      paired_idxs = get_range_all_pairs(eqn, var_names, type = "round", names_with_brackets = TRUE)
      end_function_words = get_words(eqn)
      end_function_words = end_function_words[end_function_words$word == "function", "end"]

      if (nrow(paired_idxs) > 0 & length(end_function_words) > 0){
        function_brackets = paired_idxs
        function_brackets = function_brackets[function_brackets$start %in% (end_function_words + 1), ]
        idxs_exclude <- unlist(mapply(seq, function_brackets$start, function_brackets$end, SIMPLIFY = FALSE))

        if (nrow(df_logical_op) > 0) df_logical_op = df_logical_op[!(df_logical_op$start %in% idxs_exclude | df_logical_op$end %in% idxs_exclude), ]
      }

      if (nrow(df_logical_op) > 0){

        # Replace in reverse order; no nested functions, so we can replace them in one go
        for (i in rev(1:nrow(df_logical_op))){
          stringr::str_sub(eqn, df_logical_op[i, ]$start, df_logical_op[i, ]$end) = df_logical_op[i, ]$replacement
        }
        # Remove double spaces
        eqn = stringr::str_replace_all(eqn, "[ ]+", " ")
      }
    }

  }

  # Translate assignment operator <- to =. This can't be done above as then = would be translated to ==. Insight Maker does not use = as an assignment operator.
  df_logical_op = as.data.frame(stringr::str_locate_all(eqn, "<-")[[1]])
  df_logical_op

  if (nrow(df_logical_op) > 0){

    # Remove those that are in quotation marks or names
    idxs_exclude = get_seq_exclude(eqn, var_names, names_with_brackets = TRUE)
    df_logical_op = df_logical_op[!(df_logical_op$start %in% idxs_exclude | df_logical_op$end %in% idxs_exclude), ]

    if (nrow(df_logical_op) > 0){
      # Replace in reverse order; no nested functions, so we can replace them in one go
      for (i in rev(1:nrow(df_logical_op))){
        stringr::str_sub(eqn, df_logical_op[i, ]$start, df_logical_op[i, ]$end) = "="
      }
    }
  }

  return(eqn)
}




#' Extract start and end indices of all words
#'
#' @inheritParams convert_equations_IM
#'
#' @return Dataframe with start and end indices of all words as well as extracted words
#'
get_words = function(eqn){

  # An existing function stringr::word() extracts words but treats e.g. "return(a)" as one word
  idxs_word = stringr::str_locate_all(eqn, "([a-zA-Z_\\.0-9]+)")[[1]] %>% as.data.frame()

  if (nrow(idxs_word) > 0) idxs_word$word = stringr::str_sub(eqn, idxs_word$start, idxs_word$end)

  return(idxs_word)
}


#' Convert statement syntax from InsightMaker to R
#'
#' @param line String with line of code
#' @inheritParams convert_equations_IM
#'
#' @return Updated line
#'
convert_statement = function(line, var_names, debug){

  # # Split by first comment #
  # mat = stringr::str_split_fixed(line, "#", n = 2)
  # equation = mat[,1]
  # comment = mat[,2]
  equation = line

  # Count amount of whitespace at beginning of string
  leading_whitespace <- stringr::str_extract(equation, "^[ ]*") %>% nchar() %>% stringr::str_dup(" ", .)
  equation = trimws(equation)

  # Check for final comma - anything appended needs to come before the comma
  final_char = stringr::str_extract(trimws(equation), ".$") %>% ifelse(!is.na(.), ., "")
  if (final_char == ","){
    equation = stringr::str_replace(equation, ",[ ]*$", "") # Remove comma from equation
    comma = ","
  } else {
    comma = ""
  }

  # Remove then (might not only be in if and else if, but also in a separate line)
  equation = stringr::str_replace(equation, stringr::regex("\\bthen\\b", ignore_case = TRUE), "")


  # To find statements (e.g. for, if), extract first and second word
  words = get_words(equation)
  first_word_orig = ifelse(nrow(words) > 0, words[1,"word"], "")
  second_word_orig = ifelse(nrow(words) > 1, words[2,"word"], "")
  first_word = first_word_orig %>% tolower()
  second_word = second_word_orig %>% tolower()

  # Change equation
  statement = ""
  closing_statement = ""

  if (first_word == "function"){
    # A function declaration starts with function without brackets, e.g. Function Square(x).
    # In R, the variable name needs to come BEFORE function, so switch statement and first word of equation
    statement = second_word_orig

    # When there is no second word, i.e. function(), don't change the equation
    if (nzchar(second_word_orig)){
      equation = stringr::str_replace(equation, sprintf("%s ", first_word_orig), "") %>%
        stringr::str_replace(second_word_orig, " <- function") # Remove statement # Add equals sign
    }

  } else if ((first_word == "end" & second_word %in% c("loop", "if", "function"))){
    statement = "}"
    equation = stringr::str_replace(equation, sprintf("^%s %s", first_word_orig, second_word_orig), "") # Remove statement

  } else if (first_word %in% c("for", "while", "if", "return", "throw") | (first_word == "else" & second_word == "if")){
    statement = first_word
    equation = stringr::str_replace(equation, sprintf("^[ ]*%s", first_word_orig), "") # Remove statement from equation

    if (first_word %in% c("while", "if", "for") | (first_word == "else" & second_word == "if")){
      closing_statement = "{"
    }

    # Add "}" to else if
    if (first_word == "else" & second_word == "if"){
      statement = sprintf("} %s %s", first_word, second_word)
      equation = stringr::str_replace(equation, sprintf("^[ ]*%s", second_word_orig), "") # Remove statement from equation
    }

    # Add round brackets around equation
    equation = trimws(equation)
    equation = ifelse(stringr::str_starts(equation, "\\(") & stringr::str_ends(equation, "\\)"), equation,
                      paste0("(", equation, ")")) %>% ifelse(!(first_word %in% c("return", "throw")), paste0(" ", .), .)

    # Replace ranges in for-loop
    if (first_word == "for"){
      equation = stringr::str_replace(equation, stringr::regex("from (.*?) to (.*?) by (.*?)\\)", ignore_case = T),
                                      "in seq(\\1, \\2, by = \\3))") %>%
        stringr::str_replace(stringr::regex("from (.*?) to (.*?)\\)", ignore_case = T),
                             "in seq(\\1, \\2))")
    }

  } else if (first_word == "else"){
    statement = paste0("} ", first_word, " {")
    equation = stringr::str_replace(equation, sprintf("^%s", first_word_orig), "") # Remove statement from equation

    # Try-statement
  } else if (first_word == "try"){

    statement =  "tryCatch({"
    equation = stringr::str_replace(equation, sprintf("^%s", first_word_orig), "") # Remove statement from equation

  } else if (first_word == "catch"){
    statement = "}, error = function("
    closing_statement = "){"
    equation = stringr::str_replace(equation, sprintf("^%s", first_word_orig), "") # Remove statement from equation

  } else if (first_word == "end" & second_word == "try"){
    statement = "})"
    equation = stringr::str_replace(equation, sprintf("^%s %s", first_word_orig, second_word_orig), "") # Remove statement from equation
  }


  # Left-over: Make sure all functions are in the right lower case; don't do this with simple replacement as "function" might be part of a variable name
  words = get_words(equation)
  words_function = words
  words_function = words_function[tolower(words_function$word) == "function", ]

  if (nrow(words_function) > 0){
    for (i in 1:nrow(words_function)){
      stringr::str_sub(equation, words_function[i,"start"], words_function[i,"end"]) = "function"
    }
  }

  # Make sure all functions have a closing statement "{"
  paired_idxs = get_range_all_pairs(equation, var_names, type = "round", names_with_brackets = TRUE) # Extract all round brackets
  if (nrow(paired_idxs) > 0 & nrow(words) > 0){
    # Pick bracket that ends the string
    start_bracket = paired_idxs
    start_bracket = start_bracket[start_bracket$end == stringr::str_length(equation), "start"]
    # Get last word before vector brackets
    last_word = words
    last_word = last_word[(last_word$end + 1) == ifelse(length(start_bracket), start_bracket, 0), "word"]
    last_word = ifelse(length(last_word) > 0, last_word, "")
    if (last_word == "function") {
      closing_statement = "{"
    }
  }

  join_str = c(leading_whitespace = leading_whitespace,
               statement = statement,
               equation = equation,
               closing_statement = closing_statement,
               comma = comma) %>% stringr::str_c(collapse = "")

  # comment = sprintf("%s%s%s", ifelse(nzchar(join_str) & nzchar(comment), " ", ""), ifelse(nzchar(comment), "#", ""), comment)
  # join_str = paste0(c(join_str), comment, collapse = "")

  if (debug){
    print("Converting statements:")
    print(line)
    print(join_str)
    print("")
  }
  return(join_str)
}


#' Convert all statement syntax from InsightMaker to R
#' Wrapper around convert_statement()
#'
#' @inheritParams convert_equations_IM
#'
#' @return Updated eqn
#'
convert_all_statements = function(eqn, var_names, debug){

  # InsightMaker doesn't require users to specify an Else-statement in If ... Else If ... End If -> if no condition evaluates to TRUE, the output is zero. Add "Else\n0\n" in these lines.
  # Find all if end if
  formula_split = stringr::str_split(eqn, "\n")[[1]]
  idx_end_if = which(stringr::str_detect(formula_split, stringr::regex("\\bend if\\b", ignore_case = T)))

  # For each end if, check whether first preceding line with "Then" or "Else" or "If"
  for (i in idx_end_if){

    last_idx = sapply(c("else", "else if", "if"),
           function(x){
             idx = which(stringr::str_detect(formula_split[1:(i-1)], stringr::regex(sprintf("\\b%s\\b", x), ignore_case = TRUE)))
             if (length(idx) > 0){
               return(max(idx))
             } else {
               return(0)
             }
           })
    last_idx

    # Add else-statement
    if (last_idx[["else if"]] == max(last_idx) | last_idx[["if"]] == max(last_idx)){
      # If the last found statement is else if or if, add "end if"
      formula_split[i] = paste("\nelse\n0\n", formula_split[i])
    }

  }
  eqn =  stringr::str_c(formula_split, collapse = "\n")

  formula_new = unlist(lapply(stringr::str_split(eqn, "\n")[[1]], convert_statement, var_names, debug)) %>% stringr::str_c(collapse = "\n")
  return(formula_new)

}


#' Find indices of enclosures such as (), c(), {}
#'
#' @param eqn Character string containing the equation.
#' @param var_names Character vector of variable names to exclude.
#' @param opening String with opening character, e.g., "c(".
#' @param closing String with closing character, e.g., ")".
#' @param names_with_brackets Logical, whether variable names include brackets.
#'
#' @return Data frame with indices of enclosures and nesting properties.
#' @importFrom stringr str_locate_all str_sub
get_range_pairs <- function(eqn, var_names, opening = "c(", closing = ")", names_with_brackets = FALSE) {

  # Find all pairs of "bare" brackets first, e.g. () instead of c(), as to get all matching pairs of c() we need to exclude other (). The bare brackets are the last character of opening and the last character of closing
  opening_bare <- substr(opening, nchar(opening), nchar(opening))
  closing_bare <- substr(closing, nchar(closing), nchar(closing))

  # Find all bare bracket indices
  open_locs <- stringr::str_locate_all(eqn, stringr::fixed(opening_bare))[[1]][, 1]
  close_locs <- stringr::str_locate_all(eqn, stringr::fixed(closing_bare))[[1]][, 1]

  # Exclude indices in quotes or variable names
  exclude_idxs <- get_seq_exclude(eqn, var_names, names_with_brackets)
  open_locs <- open_locs[!open_locs %in% exclude_idxs]
  close_locs <- close_locs[!close_locs %in% exclude_idxs]

  # Check for mismatched brackets
  if (length(open_locs) != length(close_locs)) {
    stop(sprintf("Missing brackets in equation:\n%s\n %s were found but %s",
                 eqn,
                 if (length(open_locs) > length(close_locs))
                   sprintf("%d %s", length(open_locs), opening_bare)
                 else
                   sprintf("%d %s", length(close_locs), closing_bare),
                 if (length(open_locs) > length(close_locs))
                   sprintf("%d %s", length(close_locs), closing_bare)
                 else
                   sprintf("%d %s", length(open_locs), opening_bare)))
  }

  # Return empty data frame if no brackets
  if (length(open_locs) == 0 || length(close_locs) == 0) {
    return(data.frame(pair = integer(), start = integer(), end = integer(),
                      id = integer(), nested_around = character(), nested_within = character(), match = character()))
  }

  # Pair brackets using a stack-based approach
  stack <- integer()
  pairs <- matrix(NA, nrow = length(open_locs), ncol = 2)
  pair_id <- 0
  for (i in seq_along(sort(c(open_locs, close_locs)))) {
    idx <- sort(c(open_locs, close_locs))[i]
    if (idx %in% open_locs) {
      stack <- c(stack, idx)
    } else {
      pair_id <- pair_id + 1
      pairs[pair_id, ] <- c(stack[length(stack)], idx)
      stack <- stack[-length(stack)]
    }
  }

  # Create pair data frame
  pair_df <- data.frame(pair = seq_len(pair_id), start = pairs[, 1], end = pairs[, 2])

  # Filter for specific opening (e.g., "c(" instead of just "(")
  if (opening != opening_bare) {
    opening_strip <- substr(opening, 1, nchar(opening) - 1)
    matches <- stringr::str_sub(eqn, pair_df$start - nchar(opening_strip), pair_df$start - 1) == opening_strip
    pair_df <- pair_df[matches, ]
    pair_df$start <- pair_df$start - nchar(opening_strip)
  }

  # Return empty data frame if no valid pairs
  if (nrow(pair_df) == 0) {
    return(data.frame(pair = integer(), start = integer(), end = integer(),
                      id = integer(), nested_around = character(), nested_within = character(), match = character()))
  }

  # Add nesting information
  pair_df$id <- seq_len(nrow(pair_df))
  pair_df$match <- stringr::str_sub(eqn, pair_df$start, pair_df$end)
  pair_df$nested_around <- vapply(seq_len(nrow(pair_df)), function(i) {
    paste(which(pair_df$start < pair_df$start[i] & pair_df$end > pair_df$end[i]), collapse = ",")
  }, character(1))
  pair_df$nested_within <- vapply(seq_len(nrow(pair_df)), function(i) {
    paste(which(pair_df$start > pair_df$start[i] & pair_df$end < pair_df$end[i]), collapse = ",")
  }, character(1))

  return(pair_df)
}




#' Get indices of all quotation marks
#'
#' @inheritParams convert_equations_IM
#' @return Dataframe with indices of quotation marks in eqn
#'
get_range_quot <- function(eqn) {
  # Get indices of quotation marks (no such thing as nested quotation marks, luckily, so we only need to find indices of all quotation marks and consecutive ones belong together)
  # idx_quot = stringr::str_locate_all(eqn, "'")[[1]][, 1]
  # idx_quot_escape = stringr::str_locate_all(eqn, "\"")[[1]][, 1]
  pair_quotation_marks = data.frame()

  idx_quot_single <- gregexpr("'", eqn)[[1]]  # Match both single and double quotes
  idx_quot_escape <- gregexpr("\"", eqn)[[1]]  # Match both single and double quotes

  # Concatenate (don't sort, important to match right ' and \")
  idx_quot <- c(idx_quot_single, idx_quot_escape)
  idx_quot = idx_quot[idx_quot != -1]

  # -1 is returned for no match
  if (length(idx_quot) > 0) {

    # Remove those that are in comments (#)
    # Find indices of comments
    comment_df = get_range_comments(eqn)

    # Remove matches of quotations within comments
    if (nrow(comment_df) > 0){
      # idxs_comments = unlist(purrr::map2(comment_df[,"start"], comment_df[,"end"], seq))
      idxs_comments <- unlist(mapply(seq, comment_df[,"start"], comment_df[,"end"], SIMPLIFY = FALSE))

      idx_quot = setdiff(idx_quot, idxs_comments)
    }

    if (length(idx_quot) > 0) {
      # Create dataframe with start and end indices of quotation marks
      pair_quotation_marks = data.frame(start = idx_quot[seq(1, length(idx_quot), by = 2)],
                                        end = idx_quot[seq(2, length(idx_quot), by = 2)])
    }
  }

  return(pair_quotation_marks)
}


#' Get indices of all pairs of square, round, and vector brackets and quotation marks
#'
#' @inheritParams convert_equations_IM
#' @inheritParams get_range_names
#' @param add_custom String with custom enclosure to look for, e.g. "paste0()", defaults to NULL
#' @param type Vector with types of enclosures to look for
#'
#' @return Dataframe with indices per type of bracket
#' @importFrom rlang .data
#'
get_range_all_pairs = function(eqn, var_names,
                               add_custom = NULL,
                               type = c("square", "curly", "round", "vector", "quot"),
                               names_with_brackets = FALSE) {

  pair_square_brackets = data.frame()
  pair_curly_brackets = data.frame()
  pair_round_brackets = data.frame()
  pair_vector_brackets = data.frame()
  pair_quotation_marks = data.frame()
  pair_custom = data.frame()

  # Get start and end indices of paired []
  if ("square" %in% type){
    pair_square_brackets = get_range_pairs(eqn, var_names, opening = "[", closing = "]", names_with_brackets = names_with_brackets)
    if (nrow(pair_square_brackets) > 0) pair_square_brackets$type = "square"
  }

  # Get start and end indices of paired {}
  if ("curly" %in% type){
    pair_curly_brackets = get_range_pairs(eqn, var_names, opening = "{", closing = "}", names_with_brackets = names_with_brackets)
    if (nrow(pair_curly_brackets) > 0) pair_curly_brackets$type = "curly"
  }

  # Get start and end indices of ()
  if ("round" %in% type){
    pair_round_brackets = get_range_pairs(eqn, var_names, opening = "(", closing = ")", names_with_brackets = names_with_brackets)
    if (nrow(pair_round_brackets) > 0) pair_round_brackets$type = "round"
  }

  # Get start and end indices of paired c()
  if ("vector" %in% type){
    pair_vector_brackets = get_range_pairs(eqn, var_names, opening = "c(", closing = ")", names_with_brackets = names_with_brackets)
    if (nrow(pair_vector_brackets) > 0) pair_vector_brackets$type = "vector"
  }

  # Get start and end indices of paired ''
  if ("quot" %in% type){
    pair_quotation_marks = get_range_quot(eqn)
    if (nrow(pair_quotation_marks) > 0) pair_quotation_marks$type = "quot"
  }

  # Custom element to look for, e.g. add_custom = "paste0()"
  if (!is.null(add_custom)) {
    l = nchar(add_custom)
    name_custom = substr(add_custom, 1, l - 2)
    opening = substr(add_custom, 1, l - 1) # Extract second to last character
    closing = substr(add_custom, l, l) # Extract last character
    type = c(type, name_custom)
    pair_custom = get_range_pairs(eqn, var_names, opening = opening, closing = closing)
    if (nrow(pair_custom) > 0) pair_custom$type = name_custom

  } else {
    pair_custom = data.frame()
  }

  paired_idxs = dplyr::bind_rows(
    pair_square_brackets,
    pair_curly_brackets,
    pair_round_brackets,
    pair_vector_brackets,
    pair_quotation_marks,
    pair_custom
  ) %>% magrittr::set_rownames(NULL)

  if (nrow(paired_idxs) > 0) {

    # Filter by type
    paired_idxs <- paired_idxs[paired_idxs$type %in% type, ]

    # Arrange by start
    paired_idxs <- paired_idxs[order(paired_idxs$start), ]

    # 3. For each unique end, keep the row with the smallest start
    # Split by end
    split_by_end <- split(paired_idxs, paired_idxs$end)

    # Keep the row with the minimum start for each end
    paired_idxs <- do.call(rbind, lapply(split_by_end, function(group) {
      group[which.min(group$start), ]
    }))

    # 4. Reset row names and ensure it's a data frame
    rownames(paired_idxs) <- NULL
    return(paired_idxs)

  } else {
    return(data.frame())
  }
}




#' Get sequence of indices of to exclude
#'
#' @inheritParams convert_equations_IM
#' @inheritParams get_range_all_pairs
#' @inheritParams get_range_names
#'
#' @return Sequence of indices
#'
get_seq_exclude = function(eqn,
                           var_names = NULL,
                           type = c("quot", "names"),
                           names_with_brackets = FALSE) {

  pair_quotation_marks = data.frame()
  pair_names = data.frame()

  if ("quot" %in% type) {
    # Get start and end indices of paired ''
    pair_quotation_marks = get_range_quot(eqn)
    if (nrow(pair_quotation_marks) > 0) pair_quotation_marks$type = "quot"
  }

  if ("names" %in% type) {
    # Get start and end indices of variable names
    pair_names = get_range_names(eqn, var_names, names_with_brackets = names_with_brackets)
    if (nrow(pair_names) > 0) pair_names$type = "names"
  }

  comb = dplyr::bind_rows(pair_quotation_marks, pair_names)

  # Create sequence
  if (nrow(comb) > 0) {
    paired_seq = lapply(1:nrow(comb), function(i) {
      seq(comb[i, ][["start"]], comb[i, ][["end"]])
    }) %>% unlist() %>% unique() %>% sort()
  } else {
    paired_seq = c()
  }

  return(paired_seq)
}





#' Get start and end indices of each name
#'
#' @param var_names Vector with variable names
#' @param names_with_brackets Boolean; whether to add square bracket around the variable names
#' @inheritParams convert_equations_IM
#'
#' @return Dataframe with start and end indices of each name
#'
get_range_names = function(eqn, var_names, names_with_brackets = FALSE) {

  idxs_df = data.frame()

  if (length(var_names) > 0){
    # Save original names
    original_names = var_names

    # If names are surrounded by square brackets, add these to the names
    if (names_with_brackets){
      var_names = paste0("[", var_names, "]")
    }

    # Add surrounding word boundaries and escape special characters
    R_names = paste0("\\b", stringr::str_escape(var_names), "\\b")
    idxs_names = stringr::str_locate_all(eqn, R_names)

    if (length(unlist(idxs_names)) > 0){

      # Create indices dataframe with detected variable names
      idxs_df = as.data.frame(do.call(rbind, idxs_names))
      idxs_df$name = rep(original_names, sapply(idxs_names, nrow))

      # Remove matches in characters
      idxs_exclude = get_seq_exclude(eqn, type = "quot", names_with_brackets = names_with_brackets)

      if (nrow(idxs_df) > 0) idxs_df = idxs_df[!(idxs_df$start %in% idxs_exclude | idxs_df$end %in% idxs_exclude), ]

    }
  }

  return(idxs_df)
}


#' Get regular expressions for built-in Insight Maker functions
#'
#' @return Dataframe
get_syntax_IM = function(){
  # Custom function to replace each (nested) function; necessary because regex in stringr unfortunately doesn't seem to handle nested functions
  conv_df = matrix(c(
    # Mathematical Functions (27)
    "Round", "IM_round", "syntax1", F, T, "",
    "Ceiling", "ceiling", "syntax1", F, T, "",
    "Floor", "floor", "syntax1", F, T, "",
    "Cos", "cos", "syntax1", F, T, "",
    "ArcCos", "acos", "syntax1", F, T, "",
    "Sin", "sin", "syntax1", F, T, "",
    "ArcSin", "asin", "syntax1", F, T, "",
    "Tan", "tan", "syntax1", F, T, "",
    "ArcTan", "atan", "syntax1", F, T, "",
    "Log", "log10", "syntax1", F, T, "",
    "Ln", "log", "syntax1", F, T, "",
    "Exp", "exp", "syntax1", F, T, "",
    "Sum", "sum", "syntax1", T, T, "",
    "Product", "prod", "syntax1", T, T, "",
    "Max", "max", "syntax1", T, T, "",
    "Min", "min", "syntax1", T, T, "",
    "Mean", "mean", "syntax1", T, T, "",
    "Median", "median", "syntax1", T, T, "",
    "StdDev", "sd", "syntax1", T, T, "",
    "Abs", "abs", "syntax1", T, T, "",
    "Sqrt", "sqrt", "syntax1", F, T, "",
    "Sign", "sign", "syntax1", F, T, "",
    "Logit", "logit", "syntax1", F, T, "",
    "Expit", "expit", "syntax1", F, T, "",

    # Random Number Functions (13)
    "Rand", "runif", "syntax1", F, F, "1",
    "RandNormal", "rnorm", "syntax1", F, F, "1",
    "RandLognormal", "rlnorm", "syntax1", F, F, "1",
    "RandBoolean", "rbool", "syntax1", F, F, "",
    "RandBinomial", "rbinom", "syntax1", F, F, "1",
    "RandNegativeBinomial", "rnbinom", "syntax1", F, F, "1",
    "RandPoisson", "rpois", "syntax1", F, F, "1",
    "RandTriangular", "EnvStats::rtri", "syntax1", F, F, "1",
    "RandExp", "rexp", "syntax1", F, F, "1",
    "RandGamma", "rgamma", "syntax1", F, F, "1",
    "RandBeta", "rbeta", "syntax1", F, F, "1",
    "RandDist", "rdist", "syntax1", F, F, "1",
    "setRandSeed", "set.seed", "syntax1", F, T, "",

    # **to do: custom julia functions
    # e.g. qnorm
    # Distributions.quantile.(Normal(0, 1), range(1/n, 1-1/n, length = n))


    # Statistical Distributions (20)
    "CDFNormal", "pnorm", "syntax1", F, T, "",
    "PDFNormal", "dnorm", "syntax1", F, T, "",
    "InvNormal", "qnorm", "syntax1", F, T, "",
    "CDFLognormal", "plnorm", "syntax1", F, T, "",
    "PDFLognormal", "dlnorm", "syntax1", F, T, "",
    "InvLognormal", "qlnorm", "syntax1", F, T, "",
    "CDFt", "pt", "syntax1", F, T, "",
    "PDFt", "dt", "syntax1", F, T, "",
    "Invt", "qt", "syntax1", F, T, "",
    "CDFF", "pf", "syntax1", F, T, "",
    "PDFF", "df", "syntax1", F, T, "",
    "InvF", "qf", "syntax1", F, T, "",
    "CDFChiSquared", "pchisq", "syntax1", F, T, "",
    "PDFChiSquared", "dchisq", "syntax1", F, T, "",
    "InvChiSquared", "qchisq", "syntax1", F, T, "",
    "CDFExponential", "pexp", "syntax1", F, T, "",
    "PDFExponential", "dexp", "syntax1", F, T, "",
    "InvExponential", "qexp", "syntax1", F, T, "",
    "CDFPoisson", "ppois", "syntax1", F, T, "",
    "PMFPoisson", "dpois", "syntax1", F, T, "",

    # User Input Functions (3)
    "Alert", "print", "syntax1", F, T, "",
    "Prompt", "readline", "syntax1", F, T, "",
    "Confirm", "readline", "syntax1", F, T, "",

    # String Functions (10)
    "Range", "substr_i", "syntax2", F, T, "",
    "Split", "strsplit", "syntax2", F, T, "",
    "UpperCase", "toupper", "syntax2", F, T, "",
    "LowerCase", "tolower", "syntax2", F, T, "",
    "Join", "stringr::str_flatten", "syntax2", F, T, "",
    "Trim", "trimws", "syntax2", F, T, "",
    "Parse", "as.numeric", "syntax2", F, T, "",

    # Vector Functions (20)
    "Length", "IM_length", "syntax2", F, T, "",
    # "Join", "c", "syntax1", ),
    # "Flatten", "purrr::flatten", "syntax2", ),
    "Unique", "unique", "syntax2", F, T, "",
    "Union", "union", "syntax2", F, T, "",
    "Intersection", "intersect", "syntax2", F, T, "",
    "Difference", "symdiff", "syntax2", F, T, "",
    "Sort", "sort", "syntax2", F, T, "",
    "Reverse", "rev", "syntax2", F, T, "",
    "Sample", "sample", "syntax2", F, T, "",
    "IndexOf", "indexof", "syntax2", F, T, "",
    "Contains", "IM_contains", "syntax2", F, T, "",
    "Keys", "names", "syntax2", F, T, "",
    "Values", "unname", "syntax2", F, T, "",
    # Map() and Filter() have syntax 2 ({}.Map(), but need an additional edit. First apply syntax 2 and then syntax 3
    "Map", "IMMAP", "syntax2", F, T, "",
    "Filter", "IMFILTER", "syntax2", F, T, "",
    "IMMAP", "conv_IMMAP", "syntax3", F, T, "",
    "IMFILTER", "conv_IMFILTER", "syntax3", F, T, "",
    # General Functions (6)
    "IfThenElse", "ifelse", "syntax1", F, T, "",
    # "Pause", "", , ), # no R equivalent
    "Stop", "stop", "syntax1", F, T, "",
    # Syntax 3
    # "Unitless", "convert_unitless", "syntax3", F, T, "",
    "Unitless", "drop_u", "syntax1", F, T, "",
    "PastValues", "conv_past_values", "syntax3", F, T, "",
    "PastMax", "conv_past_values", "syntax3", F, T, "",
    "PastMin", "conv_past_values", "syntax3", F, T, "",
    "PastMedian", "conv_past_values", "syntax3", F, T, "",
    "PastMean", "conv_past_values", "syntax3", F, T, "",
    "PastStdDev", "conv_past_values", "syntax3", F, T, "",
    "PastCorrelation", "conv_past_values", "syntax3", F, T, "",
    "Delay1", "conv_delayN", "syntax3", F, T, "",
    "Delay3", "conv_delayN", "syntax3", F, T, "",
    "DelayN", "conv_delayN", "syntax3", F, T, "",
    "Smooth", "conv_delayN", "syntax3", F, T, "",
    "SmoothN", "conv_delayN", "syntax3", F, T, "",
    "Delay", "conv_delay", "syntax3", F, T, "",
    "Fix", "conv_fix", "syntax3", F, T, "",
    "Staircase", "conv_step", "syntax3", F, T, "", # synonym for Step()
    "Step", "conv_step", "syntax3", F, T, "",
    "Pulse", "conv_pulse", "syntax3", F, T, "",
    "Ramp", "conv_ramp", "syntax3", F, T, "",
    "Seasonal", "conv_seasonal", "syntax3", F, F, sprintf("%s, %s", P$times_name, P$time_units_name),
    "Lookup", "conv_lookup", "syntax3", F, T, "",
    "Repeat", "conv_repeat", "syntax3", F, T, "",

    "Seconds", sprintf("drop_u(setunit(%s, \"s\"))", P$time_name), "syntax0", F, F, "",
    "Minutes", sprintf("drop_u(setunit(%s, \"minute\"))", P$time_name), "syntax0", F, F, "",
    "Hours", sprintf("drop_u(setunit(%s, \"hr\"))", P$time_name), "syntax0", F, F, "",
    "Days", sprintf("drop_u(setunit(%s, \"d\"))", P$time_name), "syntax0", F, F, "",
    "Weeks", sprintf("drop_u(setunit(%s, \"wk\"))", P$time_name), "syntax0", F, F, "",
    "Months", sprintf("drop_u(setunit(%s, \"common_month\"))", P$time_name), "syntax0", F, F,"",
    "Quarters", sprintf("drop_u(setunit(%s, \"common_quarter\"))", P$time_name), "syntax0", F, F, "",
    "Years", sprintf("drop_u(setunit(%s, \"common_yr\"))", P$time_name), "syntax0", F, F, "",


    "Time", P$time_name, "syntax0", F, F, "",
    "TimeStart", paste0(P$times_name, "[1]"), "syntax0", F, F, "",
    "TimeStep", P$timestep_name, "syntax0", F, F, "",
    "TimeEnd", paste0(P$times_name, "[2]"), "syntax0", F, F, "",
    "TimeLength", paste0("(", P$times_name, "[2] - ", P$times_name, "[1])"), "syntax0", F, F, "",
    # For agent-based modelling functions, issue a warning that these will not be translated
    ".FindAll()", "", "syntax4", F, T, "",
    ".FindState(", "", "syntax4", F, T, "",
    ".FindNotState(", "", "syntax4", F, T, "",
    ".FindIndex(", "", "syntax4", F, T, "",
    ".FindNearby(", "", "syntax4", F, T, "",
    ".FindNearest(", "", "syntax4", F, T, "",
    ".FindFurthest(", "", "syntax4", F, T, "",
    ".Value(", "", "syntax4", F, T, "",
    ".SetValue(", "", "syntax4", F, T, "",
    ".Location()", "", "syntax4", F, T, "",
    ".Index()", "", "syntax4", F, T, "",
    ".Location(", "", "syntax4", F, T, "",
    ".SetLocation(", "", "syntax4", F, T, "",
    "Distance(", "", "syntax4", F, T, "",
    ".Move(", "", "syntax4", F, T, "",
    ".MoveTowards(", "", "syntax4", F, T, "",
    ".Connected()", "", "syntax4", F, T, "",
    ".Connect(", "", "syntax4", F, T, "",
    ".Unconnect(", "", "syntax4", F, T, "",
    ".ConnectionWeight(", "", "syntax4", F, T, "",
    ".SetConnectionWeight(", "", "syntax4", F, T, "",
    ".PopulationSize()", "", "syntax4", F, T, "",
    ".Add(", "", "syntax4", F, T, "",
    ".Remove()", "", "syntax4", F, T, "",
    "Width(", "", "syntax4", F, T, "",
    "Height(", "", "syntax4", F, T, ""),
    ncol = 6, byrow = TRUE, dimnames = list(NULL, c("InsightMaker", "R", "syntax", "add_c()", "needs_brackets", "add_first_arg"))

  )

  # Convert to dataframe
  conv_df <- as.data.frame(conv_df, stringsAsFactors = FALSE)

  # Filter out syntax4
  df <- conv_df[conv_df$syntax != "syntax4", ]

  # Initialize new columns
  df$InsightMaker_first_iter <- df$InsightMaker
  df$InsightMaker_regex_first_iter <- ifelse(
    df$syntax %in% c("syntax0", "syntax1", "syntax3"),
    paste0("\\b", df$InsightMaker, "\\("),
    paste0("\\.", df$InsightMaker, "\\(")
  )
  df$InsightMaker <- paste0(df$InsightMaker, "_replace")
  df$InsightMaker_regex <- ifelse(
    df$syntax %in% c("syntax0", "syntax1", "syntax3"),
    paste0("\\b", df$InsightMaker, "\\("),
    paste0("\\.", df$InsightMaker, "\\(")
  )

  # Create additional rows for syntax0b and syntax1b
  additional_rows <- conv_df[conv_df$syntax %in% c("syntax0", "syntax1") & !as.logical(conv_df$needs_brackets), ]
  if (nrow(additional_rows) > 0) {
    additional_rows$InsightMaker_first_iter <- additional_rows$InsightMaker
    additional_rows$InsightMaker_regex_first_iter <- paste0("\\b", additional_rows$InsightMaker, "\\b")
    additional_rows$InsightMaker <- paste0(additional_rows$InsightMaker, "_replace")
    additional_rows$InsightMaker_regex <- paste0("\\b", additional_rows$InsightMaker, "\\b")
    additional_rows$syntax <- paste0(additional_rows$syntax, "b")

    # Combine rows
    syntax_df <- rbind(df, additional_rows)
  } else {
    syntax_df <- df
  }

  # Reset row names
  rownames(syntax_df) <- NULL

  return(list(syntax_df = syntax_df, conv_df = conv_df))
}



#' Convert InsightMaker built-in functions to R
#'
#' @return List with transformed eqn and list with additional R code needed to make the eqn function
#' @inheritParams convert_equations_IM
#' @importFrom rlang .data
#'
convert_builtin_functions_IM <- function(type, name, eqn, var_names, debug) {

  # If there are no letters in the eqn, don't run function
  translated_func = c()
  add_Rcode_list = add_Rcode = list()

  if (grepl("[[:alpha:]]", eqn)){

    # Dataframe with regular expressions for each built-in Insight Maker function
    out = get_syntax_IM()
    syntax_df = out$syntax_df
    conv_df = out$conv_df

    done = FALSE
    i = 1
    IM_regex = syntax_df$InsightMaker_regex_first_iter
    ignore_case_arg = TRUE

  while (!done) {

    # Remove those matches that are in quotation marks or names
    idxs_exclude = get_seq_exclude(eqn, var_names, names_with_brackets = TRUE)

    idx_df <- lapply(1:nrow(syntax_df), function(i) {
      matches <- gregexpr(IM_regex[i], eqn, ignore.case = ignore_case_arg)[[1]]

      if (matches[1] == -1) {
        return(data.frame(start = integer(), end = integer()))
      } else {
        dplyr::bind_cols(syntax_df[i, ],
              data.frame(
                start = as.integer(matches),
                end = as.integer(matches + attr(matches, "match.length") - 1)
              ))
      }
    }) %>% do.call(rbind, .)

    if (nrow(idx_df) == 0){
      done = TRUE
      next
    }

    idx_df = idx_df %>% as.data.frame() %>%
      # Double matches in case of functions that don't need brackets, e.g. Days() -> select one with longest end, as we want to match Days() over Days
      dplyr::group_by(.data$InsightMaker, .data$start) %>%
      dplyr::slice_max(order_by = .data$end, n = 1) %>%
      dplyr::ungroup()

    if (nrow(idx_df) > 0) idx_df = idx_df[!(idx_df$start %in% idxs_exclude | idx_df$end %in% idxs_exclude), ]

    # For the first iteration, add _replace to all detected functions, so we don't end in an infinite loop (some InsightMaker and R functions have the same name)
    if (i == 1 & nrow(idx_df) > 0){

      idx_df <- idx_df[order(idx_df$start), ]
      idx_df$InsightMaker_regex <- stringr::str_replace_all(idx_df$InsightMaker_regex,
                                                 stringr::fixed(c("\\b" = "", "\\(" = "(", "\\)" = ")")))

      for (j in rev(1:nrow(idx_df))){
          stringr::str_sub(eqn, idx_df[j, "start"], idx_df[j, "end"]) = idx_df[j, ]$InsightMaker_regex #%>%
      }
    }

    if (i == 1){
      # print(eqn)
      ignore_case_arg = FALSE
      IM_regex = syntax_df$InsightMaker_regex
      i = i + 1
      # Stop first iteration
      next
    }

    if (nrow(idx_df) == 0) {
      done = TRUE
    } else {

      # To find the arguments within round brackets, find all indices of matching '', (), [], c()
      paired_idxs = get_range_all_pairs(eqn, var_names, add_custom = "paste0()", names_with_brackets = TRUE)
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
          dplyr::bind_rows(idx_df[idx_df$syntax %in% c("syntax0b", "syntax1b"), ] %>%
                             # Add start_bracket column to prevent errors
                             dplyr::mutate(start_bracket = .data$start)) %>%
          dplyr::arrange(.data$end)
        idx_funcs
      } else {
        # If there are no brackets in the eqn, add start_bracket column to prevent errors
        idx_funcs = idx_df
        idx_funcs$start_bracket = idx_funcs$start
      }

      # Start with most nested function
      idx_funcs_ordered = idx_funcs %>% dplyr::rowwise() %>%
        dplyr::mutate(is_nested_around = any(.data$start < idx_funcs$start &
                                               .data$end > idx_funcs$end)) %>% dplyr::ungroup() %>%
        dplyr::arrange(.data$is_nested_around) %>% as.data.frame()

      # Select first match
      idx_func = idx_funcs_ordered[1, ]

      # Remove _replace in replacement function
      idx_func$InsightMaker = stringr::str_replace(idx_func$InsightMaker, "_replace$", "")
      idx_func

      # Extract argument between brackets (excluding brackets)
      bracket_arg = stringr::str_sub(eqn, idx_func$start_bracket + 1, idx_func$end - 1)
      arg = parse_args(bracket_arg)

      # Replace entire string, no arguments
      if (idx_func$syntax %in% c("syntax0", "syntax0b")) {
        replacement = idx_func$R

        # Indices of replacement in eqn
        start_idx = idx_func$start
        end_idx = idx_func$end

      } else if (idx_func$syntax == "syntax1") {

        # Add vector brackets if needed
        if (as.logical(idx_func[["add_c()"]]) & length(arg) > 1) {
          arg = paste0("c(", paste0(arg, collapse = ", "), ")")
        } else {
          arg = paste0(arg, collapse = ", ")
        }

        replacement = sprintf(
          "%s(%s%s%s)",
          idx_func$R,
          idx_func$add_first_arg,
          ifelse(nzchar(idx_func$add_first_arg) & nzchar(arg), ", ", ""),
          arg
        )

        # Indices of replacement in eqn
        start_idx = idx_func$start
        end_idx = idx_func$end

      } else if (idx_func$syntax == "syntax1b"){

        replacement = sprintf(
          "%s(%s)",
          idx_func$R,
          idx_func$add_first_arg)

        # Indices of replacement in eqn
        start_idx = idx_func$start
        end_idx = idx_func$end

      } else if (idx_func$syntax == "syntax2") {

        # Extract argument before function
        prefunc_arg = extract_prefunc_args(eqn, var_names, start_func = idx_func$start, names_with_brackets = TRUE)
        start_idx = idx_func$start - stringr::str_length(prefunc_arg)

        replacement = sprintf(
          "%s(%s%s%s%s%s)",
          idx_func$R,
          idx_func$add_first_arg,
          ifelse(nzchar(idx_func$add_first_arg), ", ", ""),
          prefunc_arg,
          ifelse(nzchar(arg[1]), ", ", ""),
          paste0(arg, collapse = ", ")
        )

        # End index of replacement in eqn
        end_idx = idx_func$end

      } else if (idx_func$syntax == "syntax3") {

        # If it's the first function of this kind, no id is needed
        match_idx = length(translated_func[translated_func == idx_func$InsightMaker]) + 1
        match_idx = ifelse(match_idx == 1, "", match_idx)

        # Get environment and create list of arguments needed by the function
        envir = environment()
        call_args = eval(parse(text = idx_func$R)) %>%
          # Get the formal arguments needed by the function
          formals() %>% as.list %>%
          # Add own arguments
          utils::modifyList(list(
            func = idx_func$InsightMaker %>% tolower,
            arg = arg
          )) %>%
          # Add other arguments from environment
          purrr::imap(function(x, y) {
            if (is.name(x)) {
              return(envir[[y]])
            } else {
              return(x)
            }
          })
        rm(envir)
        call_args

        out = do.call(idx_func$R, call_args)

        # Indices of replacement in eqn
        start_idx = idx_func$start
        end_idx = idx_func$end
        replacement = out$replacement
        add_Rcode = out$add_Rcode
        add_Rcode_list = append(add_Rcode_list, add_Rcode)

        # Add newly created variables to names_df so that they are safe from replacement, e.g. if a variable contains the word "Time"
        add_names = unname(sapply(add_Rcode, names))
        var_names = c(var_names, add_names)

      }

      if (debug){
        print(stringr::str_sub(eqn, start_idx, end_idx))
        print(replacement)
        print("")
      }

      # Replace eqn
      stringr::str_sub(eqn, start_idx, end_idx) = replacement

      translated_func = c(translated_func, idx_func$InsightMaker)
    }
  }
  add_Rcode = add_Rcode_list

  # Syntax 4: Agent-based functions, which are not translated but flagged
  syntax4 = conv_df[conv_df$syntax == "syntax4", "InsightMaker"]
  idx_ABM = stringr::str_detect(eqn, stringr::fixed(syntax4))

  if (any(idx_ABM)) {
    message("Agent-Based Modelling functions were found, and won't be translated: ")
    message(paste0(syntax4[idx_ABM], ")"))
  }
  }

  return(list(eqn = eqn, add_Rcode = add_Rcode, translated_func = translated_func))

}





#' Split arguments to function by comma
#'
#' @param bracket_arg String with arguments, excluding surrounding brackets
#'
#' @return Vector with arguments
#'
parse_args = function(bracket_arg){
  # Split arguments by comma; in order to not split arguments which contain a comma (e.g. c(1,2,3)), find all brackets and quotation marks, and don't include commas within these

  # Find indices of commas
  idxs_commas = unname(stringr::str_locate_all(bracket_arg, ",")[[1]][, 1])

  # If there's no commas, there's only one argument
  if (length(idxs_commas) == 0) {
    args = bracket_arg
  } else {

    # Create sequence of indices between brackets/quotation marks, and check whether comma is between them
    paired_idxs = get_range_all_pairs(bracket_arg, var_names = NULL)
    paired_idxs_seq <- unlist(mapply(seq,paired_idxs$start, paired_idxs$end, SIMPLIFY = FALSE))

    idxs_commas = idxs_commas[!idxs_commas %in% paired_idxs_seq]

    # Only keep commas which are not between brackets
    # Start and end positions based on indices
    starts <- c(1, idxs_commas + 1)
    ends <- c(idxs_commas - 1, stringr::str_length(bracket_arg))

    # Split bracket argument by indices
    args = mapply(stringr::str_sub, bracket_arg, starts, ends) %>% trimws() %>% unname()
  }

  return(args)

}



#' Extract arguments before function (object-oriented syntax)
#'
#' @inheritParams convert_equations_IM
#' @inheritParams get_range_names
#' @param start_func Index of start of function
#'
#' @return String with arguments before function
#'
extract_prefunc_args = function(eqn, var_names, start_func, names_with_brackets){

  # Get all enclosing elements before start of function
  prefunc_brackets = get_range_all_pairs(eqn, var_names, add_custom = "paste0()", names_with_brackets = names_with_brackets)

  prefunc_brackets = prefunc_brackets[prefunc_brackets$type != "square",
                                      prefunc_brackets$end == (start_func - 1), ]

  if (nrow(prefunc_brackets) > 0) {
    # Second argument is whatever is between brackets
    prefunc_arg = stringr::str_sub(eqn,
                                   prefunc_brackets$start,
                                   # Keep c()
                                   prefunc_brackets$end)

  } else {

    # If there are no brackets around the argument preceding . (e.g. .length()), extract string before

    # prefunc_arg = stringr::str_extract(
    #   stringr::str_sub(eqn, 1, start_func - 1) %>% stringi::stri_reverse(),
    #   "[\\w\\.\\[\\]]+" # Don't match square brackets as we don't only want to extract indexers
    # ) %>% stringi::stri_reverse() # Reverse to get preceding string

    idx_prefunc_arg = stringr::str_locate(
      stringr::str_sub(eqn, 1, start_func - 1) ,
      "[\\w\\.\\[\\]]+$" # Don't match square brackets as we don't only want to extract indexers
    )

    prefunc_arg = stringr::str_sub(stringr::str_sub(eqn, 1, start_func - 1),
                                   idx_prefunc_arg[1,"start"], idx_prefunc_arg[1, "end"])

  }

  return(prefunc_arg)
}




#' Convert InsightMaker Filter() function to R
#'
#' @inheritParams conv_delayN
#' @return Transformed eqn
#'
conv_IMFILTER <- function(func, arg) {
  x_in_function = stringr::str_detect(arg[2], "\\bx\\b")
  key_in_function = stringr::str_detect(arg[2], "\\bkey\\b")

  # The filter function contains a logical statement, but Insightmaker accepts both x = y and x == y to test whether x is the same as y
  arg[2] = stringr::str_replace_all(arg[2], "[\\b\\s]*=[\\b\\s]", " == ")

  if (!key_in_function) {
    replacement = sprintf("Filter(%s, function(x) %s)", arg[1], arg[2])
  } else {
    # In case "key" is a part of the filter statement, use custom function, as Filter does not accept two arguments
    replacement = sprintf("IM_filter(%s, function(x,key) %s)", arg[1], arg[2])
  }

  return(list(replacement = replacement))

}


#' Convert InsightMaker Map() function to R
#'
#' @inheritParams conv_delayN
#' @return Transformed eqn
#'
conv_IMMAP <- function(func, arg) {
  x_in_function = stringr::str_detect(arg[2], "\\bx\\b")
  key_in_function = stringr::str_detect(arg[2], "\\bkey\\b")
  function_in_function = stringr::str_detect(arg[2], "\\bfunction\\b")

  # ** to do: reverse order of function and argument, e.g.
  # {Min, Median, Max}.Map(x(7, 5, 8, 1, 6)) # = {1, 6, 8}


  if (function_in_function){
    replacement = sprintf("sapply(%s, %s)", arg[1], arg[2])
  } else if (!key_in_function) {
    replacement = sprintf("sapply(%s, function(x) %s)", arg[1], arg[2])
  } else {
    # To return a named vector, we cannot pass only indices, even if the function only uses x and not key
    replacement = sprintf("%s %%>%% mapply(function(x, key){%s}, ., names(.))",
                                 arg[1],
                                 arg[2])
  }

  return(list(replacement = replacement))
}




#' Convert Insight Maker's Repeat() function to R
#'
#' @inheritParams conv_delayN
#' @return Transformed eqn
#'
conv_repeat <- function(func, arg) {
  x_in_function = stringr::str_detect(arg[1], "\\bx\\b")
  key_in_function = stringr::str_detect(arg[1], "\\bkey\\b")
  names_second_arg = try(eval(parse(text = sprintf("names(%s)", arg[2]))), silent = T)
  named_second_arg = !(is.null(names_second_arg) |
                         "try-error" %in% class(names_second_arg))

  # InsightMaker's Repeats performs two different actions:
  # 1. Apply the function in the first argument using the index of the second argument as x

  if (!x_in_function & !key_in_function) {
    replacement = sprintf("rep(%s, %s)", arg[1], arg[2])

  } else if (!named_second_arg) {
    replacement = sprintf("sapply(seq.int(%s), function(x) %s)", arg[2], arg[1])
  } else if (named_second_arg) {
    # 2. Create a named vector

    if (key_in_function & !x_in_function) {
      replacement = sprintf("sapply(%s, function(key) %s)", arg[2], arg[1])
    } else {
      # To return a named vector, we cannot pass only indices, even if the function only uses x and not key
      replacement = sprintf(
        "%s %%>%% stats::setNames(mapply(function(x, key) %s, seq_along(.), .), .)",
        arg[2],
        arg[1]
      )

    }
  }
  return(list(replacement = replacement))
}


#' Convert InsightMaker's Lookup() to R
#'
#' Lookup() is a linear interpolation function, equivalent to R's approx().
#'
#' @inheritParams conv_delayN
#' @inheritParams convert_equations_IM
#'
#' @return Transformed eqn
#'
conv_lookup <- function(func, arg, name) {

  func_name_str = sprintf("%s_lookup", name)
  arg[1] = stringr::str_replace_all(arg[1], c("^\\[" = "", "\\]$" = ""))
  add_Rcode = list(gf = list(list(xpts = arg[2], ypts = arg[3],
                                  source = arg[1],
                                  interpolation = "linear",
                                  extrapolation = "nearest")) %>%
                     stats::setNames(func_name_str))

  replacement = sprintf("[%s]([%s])", func_name_str, arg[1])

  return(list(replacement = replacement,
              add_Rcode = add_Rcode))
}


#' Convert InsightMaker's addition of strings to R
#'
#' InsightMaker allows for strings to be concatenated by +, whereas R doesn't and uses paste0() instead.
#'
#' @inheritParams convert_equations_IM
#' @return Transformed eqn
#'
convert_addition_of_strings = function(eqn, var_names) {

  # First get rid of all spaces next to + in order to identify neighbours
  eqn = stringr::str_replace_all(eqn, " \\+", "+") %>% stringr::str_replace_all("\\+ ", "+")

  done = F

  # Use a while loop to iteratively convert + to paste0 if it neighbours quotation marks; if there's a variable [x] that is defined as a eqn, we unfortunately can't know here
  while (!done) {
    # Find all instances of "+" and of quotation marks
    idxs_plus = unname(stringr::str_locate_all(eqn, "\\+")[[1]][, 1])

    if (length(idxs_plus) == 0) {
      done = T
    } else {
      # Find all indices of all elements like [], c(), ''
      paired_idxs = get_range_all_pairs(eqn, var_names, add_custom = "paste0()", names_with_brackets = TRUE)

      # Are + left or right adjacent to any quotation dplyr::groups
      # Right adjacent to
      right_adjacents = match(idxs_plus, paired_idxs$end + 1)

      # Left adjacent to
      left_adjacents = match(idxs_plus, paired_idxs$start - 1)

      # Condition: It needs to be adjacent to some paired_idxs, and either right or left needs to be in quotation marks
      cond = which(!is.na(right_adjacents) &
                     !is.na(left_adjacents) &
                     (((paired_idxs[right_adjacents, "type"] == "quot") + (paired_idxs[left_adjacents, "type"] == "quot")
                     ) > 0))[1]

      # Choose first + that meets the condition
      idxs_plus_adj = idxs_plus[cond]
      right_adjacent = right_adjacents[cond]
      left_adjacent = left_adjacents[cond]

      if (is.na(idxs_plus_adj)) {
        done = T
      } else {
        left_arg = stringr::str_sub(eqn, paired_idxs[right_adjacent, ]$start, paired_idxs[right_adjacent, ]$end)
        right_arg = stringr::str_sub(eqn, paired_idxs[left_adjacent, ]$start, paired_idxs[left_adjacent, ]$end)

        # In case left or right argument is in paste0() already, remove paste0()
        l = stringr::str_length("paste0(")
        if (stringr::str_sub(left_arg, 1, l) == "paste0(") {
          left_arg = stringr::str_sub(left_arg, l + 1, stringr::str_length(left_arg) - 1)
        }
        if (stringr::str_sub(right_arg, 1, l) == "paste0(") {
          right_arg = stringr::str_sub(right_arg, l + 1, stringr::str_length(right_arg) - 1)
        }

        # Overwrite eqn
        stringr::str_sub(eqn, paired_idxs[right_adjacent, ]$start, paired_idxs[left_adjacent, ]$end) = sprintf("paste0(%s, %s)", left_arg, right_arg)

      }
    }
  }

  # # **removed scientific notation, so remove
  # # Add spaces back unless it is preceded by an e
  # eqn = gsub("(?<![E|e])\\+", " + ", eqn, perl = TRUE)
  eqn = stringr::str_replace_all(eqn, "\\+", " + ")



  return(eqn)
}


#' Check whether a eqn consists of only a primitive between brackets
#'
#' @inheritParams convert_equations_IM
#' @return Boolean
#'
check_only_primitive = function(eqn) {
  # A eqn only contains a primitive when there is one pair of square brackets and they are located at the beginning and end
  opening = stringr::str_locate_all(eqn, "\\[")[[1]][, 1]
  closing = stringr::str_locate_all(eqn, "\\]")[[1]][, 1]

  return(
    length(opening) == 1 &
      length(closing == 1) &
      opening[1] == 1 & closing[1] == stringr::str_length(eqn)
  )
}


#' Convert InsightMaker's Delay() to R
#'
#' @inheritParams conv_past_values
#'
#' @return List with transformed eqn and additional R code needed to make the eqn function
conv_delay = function(func, arg, name){

  # Fixed delays are treated the same as PastValues()
  replacement = sprintf("delay(%s, %s%s)",
                        arg[1],
                        arg[2],
                        ifelse(length(arg) == 3, paste0(", ", arg[3]), ""))
  return(
    list(
      replacement = replacement,
      add_Rcode = list()
    )
  )
}


#' Convert InsightMaker's PastValues and friends to R
#'
#'
#' @return List with transformed eqn and additional R code needed to make the eqn function
#' @inheritParams convert_equations_IM
#' @inheritParams conv_delayN
#'
conv_past_values = function(func, arg, name) {

  # If there is one, get the function that is applied to PastValues, e.g. min in PastMin()
  applied_func = func %>% stringr::str_replace("past", "")
  if (applied_func == "stddev") {
    applied_func = "sd"
  } else if (applied_func == "correlation") {
    applied_func = "cor"
  } else if (applied_func == "values") {
    # No applied function, pastvalues simply retrieves all past values
    applied_func = ""
  }

  # The optional second (third in case of PastCorrelation()) argument is how long we need to look back in history
  arg_nr_past_length = ifelse(applied_func == "cor", 3, 2)
  if (length(arg) < arg_nr_past_length) {
    past_length = ""
  } else {
    past_length = arg[arg_nr_past_length]
  }

  # Construct replacement
  if (applied_func == "cor"){
    replacement = sprintf("%s(past(%s%s), past(%s%s))",
            applied_func,
            arg[1],
            ifelse(nzchar(past_length), paste0(", ", past_length), ""),
            arg[2],
            ifelse(nzchar(past_length), paste0(", ", past_length), "")
            )
  } else {
    replacement = sprintf("%spast(%s%s)%s",
            ifelse(nzchar(applied_func), paste0(applied_func, "("), ""),
            arg[1],
            ifelse(nzchar(past_length), paste0(", ", past_length), ""),
            ifelse(nzchar(applied_func), ")", ""))
  }

  return(
    list(
      replacement = replacement,
      add_Rcode = list()
    )
  )
}


#' Convert InsightMaker's DelayN() and SmoothN() family
#'
#' @param func String with name of InsightMaker function
#' @param arg Arguments passed to InsightMaker function
#'
#' @return List with transformed eqn and additional R code needed to make the eqn function
#' @importFrom rlang .data
#'
conv_delayN = function(func, arg) {

  # Get order of delay
  if (func == "smooth" | func == "delay1") {
    delay_order = "1"
    delay_0 = ifelse(length(arg) == 3, arg[3], "")
  } else if (func == "delay3") {
    delay_order = "3"
    delay_0 = ifelse(length(arg) == 3, arg[3], "")
  } else {
    delay_order = arg[3]
    delay_0 = ifelse(length(arg) == 4, arg[4], "")
  }
  delay_length = arg[2]

  replacement = sprintf("delayN(%s, %s, %s%s)",
                        arg[1], delay_length, delay_order,
                        ifelse(nzchar(delay_0), paste0(", ", delay_0), ""))

  return(
    list(
      replacement = replacement,
      add_Rcode = list()
    )
  )

}


#' Convert InsightMaker's Fix() to R
#'
#' @return List with transformed eqn and additional R code needed to make the eqn function
#' @inheritParams convert_equations_IM
#' @inheritParams conv_delayN
#'
conv_fix = function(func, arg, name, type) {


  # ** to do
  add_Rcode = list()

  if (length(arg) == 1) {
    # In case no period is specified, never update variable
    fix_length = "Inf"
  } else {
    fix_length = arg[2]
  }
  # Strip the primitive (arg[1]) of brackets, as we don't want to add any units later, and removing brackets ensures it won't be replaced
  arg[1] = stringr::str_replace_all(arg[1], c("^\\[" = "", "\\]$" = ""))
  archive_var = arg[1]

  # Add "past" as a list element to keep track of obligatory variables
  add_Rcode = append(add_Rcode,
                     list(list(list(past = archive_var)) %>% stats::setNames(name)) %>%
                       stats::setNames(type))

  # If the fix_length is a primitive, no need to add a parameter
  if (check_only_primitive(fix_length)) {
    fix_length_name = fix_length
  } else {
    fix_length_name = sprintf("%s_fix_length", name)
    add_Rcode = list(constant = list(list(eqn = fix_length)) %>% stats::setNames(fix_length_name))
    fix_length_name = paste0("[", fix_length_name, "]")
  }

  replacement = sprintf(
    "ifelse(%s %%%% %s == 0, %s, get_past(environment(), '[%s]', %s, %s, '%s', '%s', 'fixed'))",
    P$time_name,
    fix_length_name,
    arg[1],
    name,
    P$timestep_name, P$time_units_name,
    P$time_name, P$times_name
  )

    return(
    list(
      replacement = replacement,
      add_Rcode = add_Rcode
    )
  )
}


# Ramp, pulse, step functions
# These are functions that translate to forcings in deSolve::ode (not events, as these functions may also be used to change variables other than state variables/stocks). These functions need to be defined with an interpolation function at the beginning of the script, and are used in the ODE as func(t).

# Convert step, pulse, and ramp InsightMaker functions to corresponding R functions. Rewrite equation eqn and define interpolation function to be put at beginning of the script


#' Convert InsightMaker's Step() function to R
#'
#' @param h_step Height of step, defaults to 1
#' @param match_idx Index of the number of times the same function has been called in the same eqn
#'
#' @return List with transformed eqn and list with additional R code needed to make the eqn function
#' @inheritParams conv_delayN
#' @inheritParams convert_equations_IM
#'
conv_step = function(func, arg, match_idx, name,  # Default settings of InsightMaker
                     h_step = "1") {

  # Name of function is the type (step, pulse, ramp), the number, and which model element it belongs to
  func_name_str = sprintf("%s_%s%s", name, func, # If there is only one match, don't number function
                          as.character(match_idx))
  replacement = sprintf("[%s](%s)", func_name_str, P$time_name)
  # Step(Start, Height=1), e.g. Step({2 Years}, 100)

  # Clean start time by converting to simulation time units
  start_t_step = arg[1]

  # Define interpolation function
  h_step = ifelse(is.na(arg[2]), h_step, arg[2])
  # Function definition to put at beginning of script
  func_def_str = sprintf(
    "step(start_t_step = %s, h_step = %s)",
    # P$times_name,
    start_t_step,
    h_step
    # P$time_units_name, P$time_units_name

  )
  add_Rcode = list(aux = list(list(eqn = func_def_str
                                   # , source = P$time_name
                                   )) %>% stats::setNames(func_name_str))

  return(list(replacement = replacement,
              add_Rcode = add_Rcode
              ))

}


#' Convert InsightMaker's Pulse() function to R
#'
#' @param h_pulse Height of pulse, defaults to 1
#' @param w_pulse Width of pulse in duration (i.e. time), defaults to 0 to indicate an instantaneous pulse
#' @param repeat_interval Interval at which to repeat pulse, defaults to "NULL" to indicate no repetition
#'
#' @return List with transformed eqn and list with additional R code needed to make the eqn function
#' @inheritParams convert_equations_IM
#' @inheritParams conv_step
#'
conv_pulse = function(func,
                      arg,
                      match_idx,
                      name,
                      # Default settings of InsightMaker
                      h_pulse = "1",
                      w_pulse = "0",
                      repeat_interval = "NULL") {

  # Name of function is the type (step, pulse, ramp), the number, and which model element it belongs to
  func_name_str = sprintf("%s_%s%s", name, func, # If there is only one match, don't number function
                          as.character(match_idx))
  replacement = sprintf("[%s](%s)", func_name_str, P$time_name)

  # Pulse(Time, Height, Width=0, Repeat=-1), e.g. Pulse({5 Years}, 10, 1, {10 Years})

  # Clean start time by converting to simulation time units
  start_t_pulse = arg[1]

  # Define interpolation function
  h_pulse = ifelse(is.na(arg[2]), h_pulse, arg[2])
  w_pulse = ifelse(is.na(arg[3]), w_pulse, arg[3])
  repeat_interval = ifelse(is.na(arg[4]), repeat_interval, arg[4])

  # Function definition to put at beginning of script
  func_def_str = sprintf(
    "pulse(start_t_pulse = %s(%s, %s), h_pulse = %s, w_pulse = %s(%s, %s), repeat_interval = %s)",
    # P$times_name,
    P$setunit_func,
    start_t_pulse, P$time_units_name,
    h_pulse,
    P$setunit_func,
    w_pulse, P$time_units_name,
    repeat_interval
    # P$time_units_name, P$time_units_name
  )
  add_Rcode = list(aux = list(list(eqn = func_def_str
                                   # , source = P$time_name
                                   )) %>% stats::setNames(func_name_str))

  return(list(replacement = replacement,
              add_Rcode = add_Rcode
              ))

}


#' Convert InsightMaker's Ramp() function to R
#'
#' @param h_ramp End height of ramp, defaults to 1
#'
#' @return List with transformed eqn and list with additional R code needed to make the eqn function
#' @inheritParams convert_equations_IM
#' @inheritParams conv_step
#'
conv_ramp = function(func, arg, match_idx, name, # Default settings of InsightMaker
                     h_ramp = "1") {


  # Name of function is the type (step, pulse, ramp), the number, and which model element it belongs to
  func_name_str = sprintf("%s_%s%s", name, func, # If there is only one match, don't number function
                          as.character(match_idx))
  replacement = sprintf("[%s](%s)", func_name_str, P$time_name)

  # Ramp(Start, Finish, Height=1), e.g. Ramp({3 Years}, {8 Years}, -50)

  # Clean start time by converting to simulation time units
  start_t_ramp = arg[1]
  end_t_ramp = arg[2]

  # Define interpolation function
  h_ramp = ifelse(is.na(arg[3]), h_ramp, arg[3])

  # Function definition to put at beginning of script
  func_def_str = sprintf(
    "ramp(start_t_ramp = %s(%s, %s), end_t_ramp = %s(%s, %s), start_h_ramp = 0, end_h_ramp = %s)",
    # P$times_name,
    P$setunit_func,
    start_t_ramp, P$time_units_name,
    P$setunit_func,
    end_t_ramp, P$time_units_name,
    h_ramp
    # P$time_units_name, P$time_units_name
  )
  add_Rcode = list(aux = list(list(eqn = func_def_str
                                   # , source = P$time_name
                                   )) %>% stats::setNames(func_name_str))

  return(list(replacement = replacement,
              add_Rcode = add_Rcode
              ))

}



#' Convert InsightMaker's Seasonal() function to R
#'
#' @param period Period of wave in years, defaults to 1
#' @param shift Time in years at which the wave peaks, defaults to 0
#'
#' @return List with transformed eqn and list with additional R code needed to make the eqn function
#' @inheritParams convert_equations_IM
#' @inheritParams conv_step
#'
conv_seasonal = function(func, arg, match_idx, name, period = "u(\"1common_yr\")", shift ="u(\"0common_yr\")") {

  # # Name of function is the type (step, pulse, ramp), the number, and which model element it belongs to
  # func_name_str = sprintf("%s_%s%s", name, func, # If there is only one match, don't number function
  #                         as.character(match_idx))
  # replacement = sprintf("[%s](%s)", func_name_str, P$time_name)

  # Seasonal(Peak=0)

  # If an argument is specified, it's the peak time
  if (nzchar(arg)) {
    # If there are only numbers and a period in there, add unit
    if (grepl("^[0-9]+\\.?[0-9]*$", arg[1]) | grepl("^[\\.?[0-9]*$", arg[1])){
      shift = paste0("u(\"", arg[1], "common_yr\")")
    } else {
      shift = arg[1]
    }
  }
  replacement = paste0("seasonal(", P$time_name, ", period = ", period, ", shift = ", shift, ")")

  # # Function definition to put at beginning of script
  # func_def_str = sprintf(
  #   "seasonal(%s, peak_time = %s, %s = %s)",
  #   P$times_name,
  #   peak_time, P$time_units_name, P$time_units_name
  # )
  # add_Rcode = list(aux = list(list(eqn = func_def_str
  #                                  # , source = P$time_name
  #                                  )) %>%
  #                    stats::setNames(func_name_str))
  add_Rcode = list()

  return(list(replacement = replacement,
              add_Rcode = add_Rcode
              ))

}

