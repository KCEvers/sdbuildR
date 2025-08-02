
# delay() and past() are not supported as they require keeping track of history, which is very slow in R.

#' Compute the delayN accumulator
#'
#' Internal function to compute the delayN accumulator.
#'
#' @param input Input value to process.
#' @param accumulator Accumulator values.
#' @param length Length of the delay.
#' @param order Order of the delay.
#'
#' @returns A list containing the outflow value and the updated accumulator.
#' @export
#' @family internal
#'
compute_delayN <- function(input, accumulator, length, order) {
  order <- round(order)
  d_accumulator <- numeric(order)
  exit_rate_stage <- accumulator / (length / order)

  d_accumulator[1] <- input - exit_rate_stage[1]

  if (order > 1) {
    for (ord in 2:order) {
      d_accumulator[ord] <- exit_rate_stage[ord - 1] - exit_rate_stage[ord]
    }
  }

  outflow <- exit_rate_stage[order]  # last stage is the outflow
  return(list(outflow = outflow, update = d_accumulator))
}


#' Compute the smoothN accumulator
#'
#' Internal function to compute the smoothN accumulator.
#'
#' @param input Input value to process.
#' @param state Current state of the accumulator.
#' @param length Length of the smoothing.
#' @param order Order of the smoothing.
#'
#' @returns A list containing the outflow value and the updated state.
#' @family internal
#' @export
#'
compute_smoothN <- function(input, state, length, order) {
  order <- round(order)
  d_state <- numeric(order)
  adjustment_rate <- (input - state[1]) / (length / order)
  d_state[1] <- adjustment_rate

  if (order > 1) {
    for (ord in 2:order) {
      d_state[ord] <- (state[ord - 1] - state[ord]) / (length / order)
    }
  }

  outflow <- state[order]  # output is the last state
  return(list(outflow = outflow, update = d_state))
}


#' Setup a delayN accumulator
#'
#' Internal function to set up a delayN accumulator.
#'
#' @param initial Initial value for the accumulator. If NULL, the first value of the variable to delay is used. Defaults to NULL.
#' @param length Length of the delay.
#' @param order Order of the delay.
#' @param name Name of the accumulator.
#'
#' @returns A named vector of initial values for the accumulator.
#' @family internal
#' @export
#'
setup_delayN <- function(initial, length, order, name) {
  order <- round(order)
  value <- initial * length / order
  acc_names <- paste0(name, .sdbuildR_env[["P"]][["acc_suffix"]], seq_len(order))
  accumulator <- stats::setNames(rep(value, order), acc_names)
  return(accumulator)
}

#' Setup a smoothN accumulator
#'
#' Internal function to set up a smoothN accumulator.
#'
#' @param initial Initial value for the accumulator. If NULL, the first value of the variable to smooth is used. Defaults to NULL.
#' @param length Length of the smoothing.
#' @param order Order of the smoothing.
#' @param name Name of the accumulator.
#'
#' @returns A named vector of initial values for the accumulator.
#' @family internal
#' @export
#'
setup_smoothN <- function(initial, length, order, name) {
  order <- round(order)
  value <- initial #* length / order
  acc_names <- paste0(name, .sdbuildR_env[["P"]][["acc_suffix"]], seq_len(order))
  accumulator <- stats::setNames(rep(value, order), acc_names)
  return(accumulator)
}


#' Retrieve values from the past
#'
#' Function in development; currently unusable. Note that this function is only usable for simulations run with Julia.
#'
#' @param variable Variable of which to access past values.
#' @param length Interval from the current time point to look back to. If NULL, all available past values are returned. Defaults to NULL.
#'
#' @return Past values
#' @family delay
#' @export
#'
#' @examples
past = function(variable, length = NULL){

}


#' Retrieve a single value from the past
#'
#' Also known as a pipeline or fixed delay. Note that this function is only usable for simulations run with Julia.
#'
#' @inheritParams past
#' @param length Length of the delay.
#' @param default_value Value to return when the delay length has not passed yet. If NULL, the value at the first time point is returned. Defaults to NULL.
#'
#' @return Past value
#' @family delay
#' @export
#'
#' @examples
#'
#'
delay = function(variable, length, default_value = NULL){

}


#' Delay of order N
#'
#' Delay of order N, which is a more complex delay that can be used to model processes with multiple stages or orders of delay. Note that this function is only usable for simulations run with Julia.
#'
#' @param variable Variable to delay.
#' @param length Length of the delay.
#' @param order Order of the delay.
#' @param initial Value to initialize the accumulator with. If NULL, the first value of the variable to delay is used. Defaults to NULL.
#'
#' @return Delayed variable
#' @family delay
#' @export
#'
#' @examples
delayN = function(variable, length, order, initial = NULL){

}


#' Smoothing of order N
#'
#' Smoothing of order N, which is a more complex smoothing that can be used to model processes with multiple stages or orders of smoothing. Note that this function is only usable for simulations run with Julia.
#'
#' @param variable Variable to smooth
#' @param length Length of smoothing.
#' @param order Order of smoothing.
#' @param initial Value to initialize the accumulator with. If NULL, the first value of the variable to smooth is used. Defaults to NULL.
#'
#' @return Smoothed variable
#' @family delay
#' @export
#'
#' @examples
smoothN = function(variable, length, order, initial = NULL){

}
