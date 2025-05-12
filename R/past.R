

#' Retrieve a single value from the past
#'
#' This function is coming soon, but is now unusable.
#'
#' @inheritParams past
#' @param length Length of the delay.
#' @param default_value Value to return when the delay length has not passed yet. If NULL, the value at the first time point is returned. Defaults to NULL.
#'
#' @return Past value
#' @export
#'
#' @examples
#'
#'
delay = function(variable, length, default_value = NULL){

  # time_name = deparse(substitute(t))
  # var_name = deparse(substitute(name))
  #
  #   # if (inherits(length, "units")){
  #   #   if (!is.null(time_units)){
  #   #     length = drop_if_units(units::set_units(length, time_units))
  #   #   } else {
  #       length = drop_if_units(length)
  #   #   }
  #   # }
  #
  # # Make sure that the past variable is up to date with the value of var_name at the current time step; use <<- for global assignment; this mimicks InsightMaker's update of the current value if it is accessed (https://github.com/scottfr/simulation/blob/main/src/Primitives.js, line 196: "/* Add current value to array if needed */")
  # archive[which(dplyr::near(archive[[time_name]], t)), var_name] <<- name
  #
  # # Time to retrieve
  # retrieve_t = t - length
  # first_t = archive[[time_name]][1]
  #
  #   # If trying to retrieve a value in the "past" (i.e. before times[1]), use default value if specified, otherwise use first time point
  #   if (retrieve_t < first_t){
  #     if(!is.null(default_value)){
  #       return(default_value)
  #     } else {
  #       start_t_idx = end_t_idx = 1
  #     }
  #
  #   } else {
  #     # Find indices around time to retrieve
  #     # idx_nearest = which(abs(past[[time_name]]-retrieve_t)==min(abs(past[[time_name]]-retrieve_t))) # Will have length greater than 1 in case multiple values are equally close
  #     idx_nearest = find_nearest_idx(haystack = archive[[time_name]], needle = retrieve_t)
  #     start_t_idx = idx_nearest[1] # Find first closest value
  #     end_t_idx = idx_nearest[length(idx_nearest)] # Find last closest value
  #   }
  #
  # # Linear approximation - in case retrieve_t has an exact match in past[[time_name]], the exact value will be returned
  # xn = archive[start_t_idx:end_t_idx, time_name]
  # yn = archive[start_t_idx:end_t_idx, var_name]
  # if (length(yn) == 1){ # In case start_t_idx == end_t_idx
  #   past_values = yn
  # } else {
  #
  #   # In the case of NAs, stats::approx() throws an error
  #   if (sum(!is.na(yn)) <= 1){
  #     return(NA)
  #   } else {
  #     past_values = stats::approx(xn, yn, retrieve_t)$y
  #   }
  # }
  #
  # return(past_values)

}




#' Retrieve an interval of values from the past
#'
#' This function is coming soon, but is now unusable.

#' @param variable Variable of which to access past values.
#' @param length Interval from the current time point to look back to. If NULL, all available past values are returned. Defaults to NULL.
#'
#' @return Past values
#' @export
#'
#' @examples
past = function(variable, length = NULL){

}



#' Delay of order N
#'
#' This function is coming soon, but is now unusable.
#'
#' @param variable Variable to delay.
#' @param length Length of the delay.
#' @param order Order of the delay.
#' @param default_value Value to initialize the accumulator with. If NULL, the first value of the variable to delay is used. Defaults to NULL.
#'
#' @return Delayed variable
#' @export
#'
#' @examples
delayN = function(variable, length, order, default_value = NULL){

}


#' Smoothing of order N
#'
#' This function is coming soon, but is now unusable.
#'
#' @param variable Variable to smooth
#' @param length Length of smoothing.
#' @param order Order of smoothing.
#' @param default_value Value to initialize the accumulator with. If NULL, the first value of the variable to smooth is used. Defaults to NULL.
#'
#' @return Smoothed variable
#' @export
#'
#' @examples
smoothN = function(variable, length, order, default_value = NULL){

}
