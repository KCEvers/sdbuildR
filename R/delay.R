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
#' @examplesIf not_on_cran()
#'
past <- function(variable, length = NULL) {

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
#' @examplesIf not_on_cran()
#'
delay <- function(variable, length, default_value = NULL) {

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
#' @examplesIf not_on_cran()
#'
delayN <- function(variable, length, order, initial = NULL) {

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
#' @examplesIf not_on_cran()
#'
smoothN <- function(variable, length, order, initial = NULL) {

}
