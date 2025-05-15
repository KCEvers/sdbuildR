
#' Convert Insight Maker's Round() function to R
#'
#' This function ensures that rounding in R matches rounding in Insight Maker. R base's round() rounds .5 to 0, whereas round_IM() rounds .5 to 1.
#'
#' Source: https://stackoverflow.com/questions/12688717/round-up-from-5/12688836#12688836
#'
#' @param x Value
#' @param digits Number of digits; optional, defaults to 0
#'
#' @return Rounded value
#' @export
#'
#' @examples round_IM(.5)
round_IM = function(x, digits = 0) {

  return(ifelse(x %% 1 == 0.5 | x %% 1 == -0.5,
         ceiling(x),
         round(x, digits)))
}


#' Logit function
#'
#' @param p Probability, numerical value between 0 and 1
#'
#' @return Logit
#' @export
#'
#' @examples
#' logit(.1)
logit = function(p){
  return(log(p/(1-p)))
}


#' Expit function
#'
#' Inverse of the logit function
#'
#' @param x Numerical value
#'
#' @return Numerical value
#' @export
#'
#' @examples
#' expit(1)
expit = function(x){
  return(1 / (1+exp(-x)))
}



#' Generate random logical value
#'
#' Equivalent of RandBoolean() in Insight Maker
#'
#' @param p Probability of TRUE, numerical value between 0 and 1
#'
#' @return Boolean
#' @export
#'
#' @examples
#' rbool(.5)
rbool = function(p){
  return(stats::runif(1) < p)
}


#' Generate random number from custom distribution
#'
#' Equivalent of RandDist() in Insight Maker
#'
#' @param a Vector to draw sample from
#' @param b Vector of probabilities
#'
#' @return One sample from custom distribution
#' @export
#'
#' @examples
#' rdist(c(1,2,3), c(.5, .25, .25))
rdist = function(a, b){
  return(sample(a, size = 1, prob = b))
}


#' Find index of needle (value) in haystack (vector)
#'
#' Equivalent of .IndexOf() in Insight Maker
#'
#' @param haystack Vector or string to search through
#' @param needle Value to search for
#'
#' @return Index
#' @export
#'
#' @examples
#' indexof(c("a", "b", "c"), "b") # 2
#' indexof("haystack", "hay") # 1
indexof = function(haystack, needle){

  if (length(haystack) == 1 & is.character(haystack)){
    matches = stringr::str_locate(haystack, stringr::fixed(needle))
    positions = unname(matches[,"start"][1]) # First match

    if (!is.na(positions)){
      return(positions)
    } else {
      return(0) # Return 0 if there is no match
    }
  } else {
    # Returns first occurrence of match, 0 if no match
    result <- which(haystack == needle)
    if (length(result) == 0) {
      return(0)  # Return 0 if the value is not found
    } else {
      return(result[1]) # Return the position of the first occurrence of the element
    }
  }
}


#' Length of vector or string
#'
#' Equivalent of .Length() in Insight Maker, which returns the number of elements when performed on a vector, but returns the number of characters when performed on a string
#'
#' @param x A vector or a string
#'
#' @return The number of elements in x if x is a vector; the number of characters in x if x is a string
#' @export
#'
#' @examples
#' IM_length(c("a", "b", "c")) # 3
#' IM_length("abcdef") # 6
#' IM_length(c("abcdef")) # 6
IM_length = function(x){
  if (length(x) == 1 & is.character(x)){
    return(stringr::str_length(x))
  } else {
    return(length(x))
  }
}


#' Check whether needle (value) is in haystack (vector or string)
#'
#' Equivalent of .Contains() in Insight Maker
#'
#' @param haystack Vector or string to search through
#' @param needle Value to search for
#'
#' @return Boolean
#' @export
#'
#' @examples
#' IM_contains(c("a", "b", "c"), "d") # FALSE
#' IM_contains(c("abcdef"), "bc") # TRUE
IM_contains = function(haystack, needle){
  if (length(haystack) == 1 & is.character(haystack)){
    return(grepl(needle, haystack, fixed = TRUE))
  } else {
    return(needle %in% haystack)
  }
}


#' Extract characters from string by index
#'
#' Equivalent of .Range() in Insight Maker
#'
#' @param string String to extract from
#' @param idxs Integer or vector of integers indexing which characters to extract
#'
#' @return Substring
#' @export
#'
#' @examples
#' substr_i("InsightMaker", 3) # "s"
#' substr_i("InsightMaker", c(1, 5, 10)) # "Igk"
substr_i = function(string, idxs){
  paste0(strsplit(string,"")[[1]][idxs], collapse = "")
}


#' Filter vector based on logical function
#'
#' Equivalent of .Filter() in Insight Maker when using arguments x and key in the filter function
#'
#' @param y Named vector
#' @param condition_func Function which takes arguments x and key and outputs a boolean value
#'
#' @return Vector with elements which meet condition_func
#' @export
#'
#' @examples
#' IM_filter(c(a = 1, b = 2, c = 3), function(x, key){x > 1})
#' # b c
#' # 2 3
#' IM_filter(c(a = 1, b = 2, c = 3), function(x, key){x > 1 & key != "b"})
#' # c
#' # 3
IM_filter = function(y, condition_func){
  purrr::map2(y, names(y), function(x,key){if (condition_func(x, key)){return(x)}}) %>%
    purrr::compact() %>% unlist()
}



#' Create ramp function
#'
#' Equivalent of Ramp() in Insight Maker
#'
#' @param start_t_ramp Start time of ramp
#' @param end_t_ramp End time of ramp
#' @param start_h_ramp Start height of ramp, defaults to 0
#' @param end_h_ramp End height of ramp, defaults to 1
#'
#' @export
#' @return Interpolation function
#'
ramp <- function(start_t_ramp, end_t_ramp, start_h_ramp = 0, end_h_ramp = 1){

  # if (!is.null(time_units)){
  #   if (inherits(start_t_ramp, "units")){
  #     start_t_ramp = drop_if_units(units::set_units(start_t_ramp, time_units))
  #   }
  #   if (inherits(end_t_ramp, "units")){
  #     end_t_ramp = drop_if_units(units::set_units(end_t_ramp, time_units))
  #   }
  #   start_h_ramp = drop_if_units(start_h_ramp)
  #   end_h_ramp = drop_if_units(end_h_ramp)
  # }
  #
  # # Check whether start ramp is in times
  # if (start_t_ramp < times[1] | start_t_ramp > times[length(times)]){
  #   warning(paste0("start_t_ramp (", start_t_ramp, ") is not in times [", times[1], ", ", times[length(times)], "]"))
  # }

  # Create dataframe with signal
  signal = data.frame(times = c(start_t_ramp, end_t_ramp),
                      y = c(start_h_ramp, end_h_ramp))

  # If the ramp is after the start of signal, add a zero at the start
  if (min(start_t_ramp) > times[1]){
    signal = rbind(data.frame(times = times[1], y = 0), signal)
  }

  # # If the ramp ends before the end of the signal, add height of ramp at the end
  # if (max(end_t_ramp) < dplyr::last(times)){
  #   signal = rbind(signal, data.frame(times = dplyr::last(times), y = end_h_ramp))
  # }

  # In Insight Maker, at start_t_ramp, the signal is still 0, but here it starts at start_h_ramp
  # No need to append the end value as the signal stays at the end value of ramp

  # Create linear approximation function
  input = stats::approxfun(signal, rule = 2, method = "linear")
  return(input)
}



#' Create pulse function
#'
#' This function is coming soon, but is now unusable.
#'
#' Equivalent of Pulse() in Insight Maker
#'
#' @param start_t_pulse Start time of pulse
#' @param h_pulse Height of pulse, defaults to 1
#' @param w_pulse Width of pulse in duration (i.e. time). This cannot be equal to or less than 0; to indicate an instantaneous pulse, specify the simulation step size
#' @param repeat_interval Interval at which to repeat pulse, defaults to NULL to indicate no repetition
#'
#' @export
#' @return Interpolation function
#'
pulse <- function(start_t_pulse, h_pulse = 1, w_pulse = 1, repeat_interval = NULL){

  if (w_pulse <= 0){
    stop(paste0("The width of the pulse (w_pulse) cannot be equal to or less than 0; to indicate an 'instantaneous pulse', specify the simulation step size (", P$timestep_name, ")"))
  }

  # # Remove any units if set
  # if (!is.null(time_units)){
  #   if (inherits(start_t_pulse, "units")){
  #     start_t_pulse = drop_if_units(units::set_units(start_t_pulse, time_units))
  #   }
  #   if (inherits(w_pulse, "units")){
  #     w_pulse = drop_if_units(units::set_units(w_pulse, time_units))
  #   }
  #   if (inherits(repeat_interval, "units")){
  #     repeat_interval = drop_if_units(units::set_units(repeat_interval, time_units))
  #   }
  #   h_pulse = drop_if_units(h_pulse)
  # }
#
#   # Check whether start pulse is in times
#   if (start_t_pulse < times[1] | start_t_pulse > times[length(times)]){
#     warning(paste0("start_t_pulse (", start_t_pulse, ") is not in times [", times[1], ", ", times[length(times)], "]"))
#   }

  # Define time and indices of pulses
  start_ts = seq(start_t_pulse, times[length(times)],
                 # If the number of repeat is NULL, ensure no repeats
                 by = ifelse(is.null(repeat_interval), times[length(times)] + 1,
                             repeat_interval))
  end_ts = start_ts + w_pulse

  signal = rbind(
        data.frame(times = start_ts, y = h_pulse),
        data.frame(times = end_ts, y = 0))

  # If pulse is after the start of signal, add a zero at the start
  if (min(start_ts) > times[1]){
    signal = rbind(signal, data.frame(times = times[1], y = 0))
  }

  # If pulse does not cover end of signal, add a zero at the end
  # (I don't fully understand why this is necessary, but otherwise it gives incorrect results with repeat_interval <= 0 in Julia, so for consistency's sake)
  if (max(end_ts) < times[length(times)]){
    signal = rbind(signal, data.frame(times = times[length(times)], y = 0))
  }

  # signal = signal %>%
  #   dplyr::arrange(times)
  signal = signal[order(signal$times), ]

  # Create linear approximation function, use constant interpolation to get a block shape even at finer sampling times
  input = stats::approxfun(signal, rule = 2, method = 'constant')
  return(input)
}




#' Create step function
#'
#' This function is coming soon, but is now unusable.
#'
#' Equivalent of Step() in Insight Maker
#'
#' @param start_t_step Start time of step
#' @param h_step Height of step, defaults to 1
#'
#' @export
#' @return Interpolation function
#'
step <- function(start_t_step, h_step = 1){

  # if (!is.null(time_units)){
  #   if (inherits(start_t_step, "units")){
  #     start_t_step = drop_if_units(units::set_units(start_t_step, time_units))
  #   }
  #   h_step = drop_if_units(h_step)
  # }
  #
  # # Check whether step is in times
  # if (start_t_step < times[1] | start_t_step > times[length(times)]){
  #   warning(paste0("start_t_step (", start_t_step, ") is not in times [", times[1], ", ", times[length(times)], "]"))
  # }

  # Create dataframe with signal
  signal = data.frame(times = c(start_t_step, times[length(times)]), y = c(h_step, h_step))

  if (start_t_step > times[1]){
    signal = rbind(data.frame(times = times[1], y = 0), signal)
  }

  # Create linear approximation function
  input = stats::approxfun(signal, rule = 2, method = "constant")
  return(input)
}


#' Annual sine wave with an amplitude of 1
#'
#' This function is coming soon, but is now unusable.
#'
#' Equivalent of Seasonal() in Insight Maker
#'
#' @param t Current time
#' @param period Duration of wave in years
#' @param shift Timing of wave peak in years, defaults to 0
#'
#' @return Seasonal wave
#' @export
#'
#' @examples
#' times = seq(0, 48, by = .1)
#' season = seasonal(times)
#' plot(season, type = "l")
#' season = seasonal(times, shift = 6)
#' plot(season, type = "l")
seasonal = function(t, period = 1, shift = 0){

  return(cos(2 * pi * (t - shift) / period))

}



#' Safely check whether x is less than zero, preserving units
#'
#' @param x Value
#'
#' @return x if x is greater than 0, 0 otherwise
#' @export
#'
#' @examples nonnegative(NA)
#' nonnegative(-1)
#'
nonnegative = function(x){
  # Safe comparison to zero
  if (is.na(x)){
    return(x)
  } else {
    return(max(c(0, x)))
  }
}



#' Remainder
#'
#' Note that modulus and remainder are not the same in case either a or b is negative. If you work with negative numbers, modulus is always non-negative (it matches the sign of the divisor).
#'
#' @param a Dividend
#' @param b Divisor
#'
#' @returns Remainder
#' @export
#'
#' @examples
#' a = 7; b = 3
#' rem(a, b)
#' mod(a, b)
#' a = -7; b = 3
#' rem(a, b)
#' mod(a, b)
#' a = 7; b = -3
#' rem(a, b)
#' mod(a, b)
#' a = -7; b = -3
#' rem(a, b)
#' mod(a, b)
rem <- function(a, b) {
  return(a - b * trunc(a / b))
}


#' Modulus
#'
#' Note that modulus and remainder are not the same in case either a or b is negative. If you work with negative numbers, modulus is always non-negative (it matches the sign of the divisor).
#'
#' @inheritParams rem
#'
#' @returns Modulus
#' @export
#'
#' @examples
#'
#' # mod(a, b) is the same as a %% b
#'
#' a = 7; b = 3
#' rem(a, b)
#' mod(a, b)
#' a = -7; b = 3
#' rem(a, b)
#' mod(a, b)
#' a = 7; b = -3
#' rem(a, b)
#' mod(a, b)
#' a = -7; b = -3
#' rem(a, b)
#' mod(a, b)
mod = function(a, b){
  return(a %% b)
}



#' Remainder operator
#'
#' This is a wrapper for the rem() function. It is used to match Insight Maker's mod, which is the remainder operator.
#'
#' @inheritParams rem
#'
#' @returns Remainder
#' @export
#'
#' @examples
#' -7 %REM% 3
`%REM%` <- function(a, b) {
  rem(a, b)
}


#' Shuffle vector
#'
#' @param x Vector to shuffle
#'
#' @returns Shuffled x
#' @export
#'
#' @examples
#' shuffle(1:10)
shuffle = function(x){
  return(sample(x, length(x), replace = F))
}



#' Sigmoid function (i.e. logistic function)
#'
#' @param x Value to compute sigmoid of
#' @param slope Slope of sigmoid function, defaults to 1
#' @param midpoint Midpoint of sigmoid function where the output is .5, defaults to 0
#'
#' @returns Sigmoid of x
#' @export
#'
#' @examples sigmoid(0)
#' sigmoid(1, slope = 5, midpoint = 0.5)
sigmoid <- function(x, slope = 1, midpoint = 0) {
  stopifnot("slope must be numeric!" = is.numeric(slope))
  stopifnot("midpoint must be numeric!" = is.numeric(midpoint))

  return(1 / (1 + exp(-slope*(x-midpoint))))
}



#' Function to save dataframe at specific times
#'
#' Internal function used to save the dataframe at specific times in case saveat != dt in the simulation specifications.
#'
#' @param df Dataframe
#' @param time_col Name of the time column
#' @param new_times Vector of new times to save the dataframe at
#'
#' @returns Dataframe with new times and interpolated values
#' @export
#'
saveat_func <- function(df, time_col, new_times) {
  # Extract the time column (first column)
  time <- df[[time_col]]

  # Get the columns to interpolate (all except the first)
  cols_to_interpolate <- setdiff(names(df), time_col)

  # Interpolate each column (except the first) at new_times
  interpolated <- lapply(cols_to_interpolate, function(col) {
    stats::approx(x = time, y = df[[col]], xout = new_times, method = "linear")$y
  })

  # Combine results into a new data frame
  result <- data.frame(new_times)
  names(result) <- time_col
  for (i in seq_along(cols_to_interpolate)) {
    result[[cols_to_interpolate[i]]] <- interpolated[[i]]
  }

  return(result)
}

