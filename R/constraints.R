#' Generate constraints to use in ODE
#' Turn constraints on the minimum and maximum value a variable can take into a logical statement to evaluate in the ODE. Constraint variables can be stocks, flows, or any other variable defined in the ODE.
#'
#' @param constraints_def Named list of minimum and maximum constraints per variable
#'
#' @return Vector of strings, specifying the statements to evaluate
#' @export
#'
#' @examples
#' constraints_def = list("Moose" = c(min = 0, max = 300), "Wolves" = c(max = 300))
#' get_logical_constraints(constraints_def) # c("Moose < 0", "Moose > 300", "Wolves > 300")
get_logical_constraints = function(constraints_def){
  constraints_def %>% purrr::imap(.,
                              function(x, i){
                                if (!is.na(x["min"])){
                                  min_constraint = paste0(i, " < ", x["min"])
                                } else {
                                  min_constraint = list()
                                }
                                if (!is.na(x["max"])){
                                  max_constraint = paste0(i, " > ", x["max"])
                                } else {
                                  max_constraint = list()
                                }
                                return(list(min_constraint, max_constraint))

                              }) %>% unlist() %>% unname
}


#' Check constraint violation in ODE
#'
#' @param constraints Vector of strings, specifying the statements to evaluate, as returned by get_logical_constraints()
#' @param envir Environment
#' @param current_time Current time in ODE
#'
#' @return Nothing if constraints are not violated; otherwise a message will be printed with the constraints that were violated, and the simulation will terminate
#' @export
#'
#' @examples
check_constraints = function(constraints, envir, current_time){

  # Evaluate constraint
  constraints_eval = sapply(constraints, function(x){eval(parse(text = x), envir = envir)})

  # Stop function if constraint is broken
  if (any(is.na(constraints_eval)) | any(constraints_eval)){
    msg = paste0("Constraint violated at time ", current_time,
                 "\n", paste0(names(constraints_eval), ": ", unname(constraints_eval), collapse = "\n"))
    message(cat(msg))
    stop()
  }
  return()
}
