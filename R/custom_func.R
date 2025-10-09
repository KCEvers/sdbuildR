#' Round Half-Up (as in Insight Maker)
#'
#' R rounds .5 to 0, whereas Insight Maker rounds .5 to 1. This function is the equivalent of Insight Maker's Round() function.
#'
#' Source: https://stackoverflow.com/questions/12688717/round-up-from-5/12688836#12688836
#'
#' @param x Value
#' @param digits Number of digits; optional, defaults to 0
#'
#' @return Rounded value
#' @family custom
#' @export
#'
#' @examples
#' round_IM(.5) # 1
#' round(.5) # 0
#' round_IM(-0.5) # 0
#' round(-0.5) # 0
#' round_IM(1.5) # 2
#' round(1.5) # 2
round_IM <- function(x, digits = 0) {
  return(ifelse(x %% 1 == 0.5 | x %% 1 == -0.5,
    ceiling(x),
    round(x, digits)
  ))
}


#' Logit function
#'
#' @param p Probability, numerical value between 0 and 1
#'
#' @return Logit
#' @family custom
#' @export
#'
#' @examples
#' logit(.1)
logit <- function(p) {
  return(log(p / (1 - p)))
}


#' Expit function
#'
#' Inverse of the logit function
#'
#' @param x Numerical value
#'
#' @return Numerical value
#' @family custom
#' @export
#'
#' @examples
#' expit(1)
expit <- function(x) {
  return(1 / (1 + exp(-x)))
}



#' Generate random logical value
#'
#' Equivalent of RandBoolean() in Insight Maker
#'
#' @param p Probability of TRUE, numerical value between 0 and 1
#'
#' @return Boolean
#' @family custom
#' @export
#'
#' @examples
#' rbool(.5)
rbool <- function(p) {
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
#' @family custom
#' @export
#'
#' @examples
#' rdist(c(1, 2, 3), c(.5, .25, .25))
rdist <- function(a, b) {
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
#' @family custom
#' @export
#'
#' @examples
#' indexof(c("a", "b", "c"), "b") # 2
#' indexof("haystack", "hay") # 1
#' indexof("haystack", "m") # 0
indexof <- function(haystack, needle) {
  if (length(haystack) == 1 & is.character(haystack)) {
    matches <- stringr::str_locate(haystack, stringr::fixed(needle))
    positions <- unname(matches[, "start"][1]) # First match

    if (!is.na(positions)) {
      return(positions)
    } else {
      return(0) # Return 0 if there is no match
    }
  } else {
    # Returns first occurrence of match, 0 if no match
    result <- which(haystack == needle)
    if (length(result) == 0) {
      return(0) # Return 0 if the value is not found
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
#' @family custom
#' @export
#'
#' @examples
#' length_IM(c("a", "b", "c")) # 3
#' length_IM("abcdef") # 6
length_IM <- function(x) {
  if (length(x) == 1 & is.character(x)) {
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
#' @family custom
#' @export
#'
#' @examples
#' contains_IM(c("a", "b", "c"), "d") # FALSE
#' contains_IM(c("abcdef"), "bc") # TRUE
contains_IM <- function(haystack, needle) {
  if (length(haystack) == 1 & is.character(haystack)) {
    return(grepl(needle, haystack, fixed = TRUE))
  } else {
    return(needle %in% haystack)
  }
}




#' Create ramp function
#'
#' Create a ramp function that increases linearly from 0 to a specified height at a specified start time, and stays at this height after the specified end time.
#'
#' Equivalent of Ramp() in Insight Maker
#'
#' @param times Vector of simulation times
#' @param start Start time of ramp
#' @param finish End time of ramp
#' @param height End height of ramp, defaults to 1
#'
#' @export
#' @return Interpolation function
#' @family input
#' @seealso [step()], [pulse()], [seasonal()]
#' @examples
#' # Create a simple model with a ramp function
#' sfm <- xmile() |>
#'   build("a", "stock") |>
#'   # Specify the global variable "times" as simulation times
#'   build("input", "constant", eqn = "ramp(times, 20, 30, 3)") |>
#'   build("inflow", "flow", eqn = "input(t)", to = "a")
#'
#' \dontshow{
#' sfm <- sim_specs(sfm, save_at = 1, dt = .1)
#' }
#'
#' sim <- simulate(sfm, only_stocks = FALSE)
#' plot(sim)
#'
#' # To create a decreasing ramp, set the height to a negative value
#' sfm <- build(sfm, "input", eqn = "ramp(times, 20, 30, -3)")
#'
#' sim <- simulate(sfm, only_stocks = FALSE)
#' plot(sim)
#'
ramp <- function(times, start, finish, height = 1) {
  if (finish < start) {
    stop("The finish time of the ramp cannot be before the start time. To specify a decreasing ramp, set the height to a negative value.")
  }

  if (start < times[1]) {
    warning("Start of ramp before beginning of simulation time.")
  }

  if (start > times[length(times)]){
    warning("Start of ramp after end of simulation time.")

    # In this case, no need to compute signal
    signal <- data.frame(times = times[c(1, length(times))], y = c(0, 0))
    input <- stats::approxfun(signal, rule = 2, method = "constant")
    return(input)
  } else if (finish < times[1]){
    warning("End of ramp before beginning of simulation time.")

    # In this case, no need to compute signal
    signal <- data.frame(times = times[c(1, length(times))], y = c(height, height))
    input <- stats::approxfun(signal, rule = 2, method = "constant")
    return(input)
  }

  # Create dataframe with signal
  signal <- data.frame(
    times = c(start, finish),
    y = c(0, height)
  )

  # If the ramp is after the start of signal, add a zero at the start
  if (start > times[1]) {
    signal <- rbind(data.frame(times = times[1], y = 0), signal)
  }

  # # If the ramp ends before the end of the signal, add height of ramp at the end
  # if (max(finish) < dplyr::last(times)){
  #   signal = rbind(signal, data.frame(times = dplyr::last(times), y = height))
  # }

  # Create linear approximation function
  input <- stats::approxfun(signal, rule = 2, method = "linear")
  return(input)
}



#' Create pulse function
#'
#' Create a pulse function that jumps from zero to a specified height at a specified time, and returns to zero after a specified width. The pulse can be repeated at regular intervals.
#'
#' Equivalent of Pulse() in Insight Maker
#'
#' @param times Vector of simulation times
#' @param start Start time of pulse in simulation time units.
#' @param height Height of pulse. Defaults to 1.
#' @param width Width of pulse in simulation time units. This cannot be equal to or less than 0. To indicate an instantaneous pulse, specify the simulation step size.
#' @param repeat_interval Interval at which to repeat pulse. Defaults to NULL to indicate no repetition.
#'
#' @export
#' @return Interpolation function
#' @seealso [step()], [ramp()], [seasonal()]
#' @family input
#' @examples
#' # Create a simple model with a pulse function
#' # that starts at time 5, jumps to a height of 2
#' # with a width of 1, and does not repeat
#' sfm <- xmile() |>
#'   build("a", "stock") |>
#'   # Specify the global variable "times" as simulation times
#'   build("input", "constant", eqn = "pulse(times, 5, 2, 1)") |>
#'   build("inflow", "flow", eqn = "input(t)", to = "a")
#'
#' \dontshow{
#' sfm <- sim_specs(sfm, dt = .1)
#' }
#'
#' sim <- simulate(sfm, only_stocks = FALSE)
#' plot(sim)
#'
#' # Create a pulse that repeats every 5 time units
#' sfm <- build(sfm, "input", eqn = "pulse(times, 5, 2, 1, 5)")
#'
#' sim <- simulate(sfm, only_stocks = FALSE)
#' plot(sim)
#'
pulse <- function(times, start, height = 1, width = 1, repeat_interval = NULL) {
  if (width <= 0) {
    stop(paste0("The width of the pulse cannot be equal to or less than 0. To indicate an 'instantaneous' pulse, specify the simulation step size (", .sdbuildR_env[["P"]][["timestep_name"]], ")."))
  }

  if (start < times[1]) {
    warning("Start of pulse before beginning of simulation time.")
  }

  if (start > times[length(times)]){
    warning("Start of pulse after end of simulation time.")

    # In this case, no need to compute signal
    signal <- data.frame(times = times[c(1, length(times))], y = c(0, 0))
    input <- stats::approxfun(signal, rule = 2, method = "constant")
    return(input)
  }


  # Define time and indices of pulses
  if (is.null(repeat_interval)) {
    signal <- rbind(
      data.frame(times = start, y = height),
      data.frame(times = start + width, y = 0)
    )
  } else {
    start_ts <- seq(start, times[length(times)], by = repeat_interval)

    # When width is equal or greater than repeat interval, it's basically continuously 1
    if (width >= repeat_interval){
      warning("width (", width, ") >= repeat_interval (", repeat_interval, ") creates a continuous pulse.")

      signal <- data.frame(times = start_ts, y = height)
    } else {
      signal <- rbind(
        data.frame(times = start_ts, y = height),
        data.frame(times = start_ts + width, y = 0)
      )
    }
  }

  # If pulse is after the start of signal, add a zero at the start
  if (start > times[1]) {
    signal <- rbind(signal, data.frame(times = times[1], y = 0))
  }

  # If pulse does not cover end of signal, add a zero at the end
  # (I don't fully understand why this is necessary, but otherwise it gives incorrect results with repeat_interval <= 0 in Julia, so for consistency's sake)
  if (max(signal[["times"]]) < times[length(times)]) {
    signal <- rbind(signal,
                    data.frame(times = times[length(times)], y = 0))
  }

  signal <- signal[order(signal[["times"]]), ]

  # Create linear approximation function, use constant interpolation to get a block shape even at finer sampling times
  input <- stats::approxfun(signal, rule = 2, method = "constant")
  return(input)
}




#' Create step function
#'
#' Create a step function that jumps from zero to a specified height at a specified time, and remains at that height until the end of the simulation time.
#'
#' Equivalent of Step() in Insight Maker
#'
#' @param times Vector of simulation times
#' @param start Start time of step
#' @param height Height of step, defaults to 1
#'
#' @export
#' @return Interpolation function
#' @seealso [ramp()], [pulse()], [seasonal()]
#' @family input
#' @examples
#' # Create a simple model with a step function
#' # that jumps at time 50 to a height of 5
#' sfm <- xmile() |>
#'   build("a", "stock") |>
#'   # Specify the global variable "times" as simulation times
#'   build("input", "constant", eqn = "step(times, 50, 5)") |>
#'   build("inflow", "flow", eqn = "input(t)", to = "a")
#'
#' \dontshow{
#' sfm <- sim_specs(sfm, dt = .1)
#' }
#'
#' sim <- simulate(sfm, only_stocks = FALSE)
#' plot(sim)
#'
#' # Negative heights are also possible
#' sfm <- build(sfm, "input", eqn = "step(times, 50, -10)")
#'
#' sim <- simulate(sfm, only_stocks = FALSE)
#' plot(sim)
step <- function(times, start, height = 1) {

  if (start < times[1]) {
    warning("Start of step before beginning of simulation time.")
  }

  if (start > times[length(times)]){
    warning("Start of step after end of simulation time.")

    # In this case, no need to compute signal
    signal <- data.frame(times = times[c(1, length(times))], y = c(0, 0))
    input <- stats::approxfun(signal, rule = 2, method = "constant")
    return(input)
  }

  # Create dataframe with signal
  signal <- data.frame(times = start, y = height)

  # In rare cases, the start is the same time as the end of times, so add in if()
  if (start != times[length(times)]) {
    signal <- rbind(data.frame(times = times[length(times)], y = height), signal)
  }

  if (start >= times[1]) {
    signal <- rbind(data.frame(times = times[1], y = 0), signal)
  }

  # Create linear approximation function
  input <- stats::approxfun(signal, rule = 2, method = "constant")
  return(input)
}


#' Create a seasonal wave function
#'
#' Create a seasonal wave function that oscillates between -1 and 1, with a specified period and shift. The wave peaks at the specified shift time.
#'
#' Equivalent of Seasonal() in Insight Maker
#'
#' @param times Vector of simulation times
#' @param period Duration of wave in simulation time units. Defaults to 1.
#' @param shift Timing of wave peak in simulation time units. Defaults to 0.
#'
#' @return Seasonal wave
#' @family input
#' @seealso [step()], [pulse()], [ramp()]
#' @export
#'
#' @examples
#' # Create a simple model with a seasonal wave
#' sfm <- xmile() |>
#'   build("a", "stock") |>
#'   # Specify the global variable "times" as simulation times
#'   build("input", "constant", eqn = "seasonal(times, 10, 0)") |>
#'   build("inflow", "flow", eqn = "input(t)", to = "a")
#'
#' sim <- simulate(sfm, only_stocks = FALSE)
#' plot(sim)
#'
seasonal <- function(times, period = 1, shift = 0) {
  if (period <= 0) {
    stop("The period of the seasonal wave must be greater than 0.")
  }

  # Create linear approximation function - define wave in advance so that the period and shift argument do not need to be kept
  signal <- cos(2 * pi * (times - shift) / period)
  input <- stats::approxfun(x = times, y = signal, rule = 2, method = "linear")
  return(input)
}






#' Safely check whether x is less than zero
#'
#' If using Julia, units are preserved
#'
#' @param x Value
#'
#' @return x if x is greater than 0, 0 otherwise
#' @family internal
#' @export
#'
#' @examples nonnegative(NA)
#' nonnegative(-1)
#'
nonnegative <- function(x) {
  # Safe comparison to zero
  if (is.na(x)) {
    return(x)
  } else {
    return(max(c(0, x)))
  }
}



#' Remainder and modulus
#'
#' Remainder and modulus operators. The modulus and remainder are not the same in case either a or b is negative. If you work with negative numbers, modulus is always non-negative (it matches the sign of the divisor).
#'
#' @param a Dividend
#' @param b Divisor
#'
#' @returns Remainder
#' @family custom
#' @export
#' @rdname rem_mod
#'
#' @examples
#' # Modulus and remainder are the same when a and b are positive
#' a <- 7
#' b <- 3
#' rem(a, b)
#' mod(a, b)
#' # Modulus and remainder are NOT when either a or b is negative
#' a <- -7
#' b <- 3
#' rem(a, b)
#' mod(a, b)
#' a <- 7
#' b <- -3
#' rem(a, b)
#' mod(a, b)
#' # Modulus and remainder are the same when both a and b are negative
#' a <- -7
#' b <- -3
#' rem(a, b)
#' mod(a, b)
#'
#' # Alternative way of computing the remainder:
#' a %REM% b
rem <- function(a, b) {
  return(a - b * trunc(a / b))
}



#' @export
#' @rdname rem_mod
mod <- function(a, b) {
  return(a %% b)
}




#' @export
#' @rdname rem_mod
`%REM%` <- function(a, b) {
  rem(a, b)
}



#' Logistic function
#'
#' @param x Value
#' @param slope Slope of logistic function. Defaults to 1.
#' @param midpoint Midpoint of logistic function where the output is .5. Defaults to 0.
#' @param upper Maximal value returned by logistic function. Defaults to 1.
#'
#' @returns f(x), where f is the logistic function
#' @family custom
#' @export
#'
#' @examples logistic(0)
#' logistic(1, slope = 5, midpoint = 0.5)
logistic <- function(x, slope = 1, midpoint = 0, upper = 1) {
  stopifnot("slope must be numeric!" = is.numeric(slope))
  stopifnot("midpoint must be numeric!" = is.numeric(midpoint))
  stopifnot("upper must be numeric!" = is.numeric(upper))

  return(upper / (1 + exp(-slope * (x - midpoint))))
}




#' Internal function to save dataframe at specific times
#'
#' Internal function used to save the dataframe at specific times in case save_at is not equal to dt in the simulation specifications.
#'
#' @param df Dataframe
#' @param time_col Name of the time column
#' @param new_times Vector of new times to save the dataframe at
#'
#' @returns Dataframe with new times and interpolated values
#' @family internal
#' @export
#'
saveat_func <- function(df, time_col, new_times) {
  # Extract the time column (first column)
  time <- df[[time_col]]

  # Get the columns to interpolate (all except the first)
  cols_to_interpolate <- setdiff(names(df), time_col)

  # Interpolate each column (except the first) at new_times
  interpolated <- lapply(cols_to_interpolate, function(col) {
    stats::approx(x = time, y = df[[col]], xout = new_times, method = "linear")[["y"]]
  })

  # Combine results into a new data frame
  result <- data.frame(new_times)
  names(result) <- time_col
  for (i in seq_along(cols_to_interpolate)) {
    result[[cols_to_interpolate[i]]] <- interpolated[[i]]
  }

  return(result)
}
