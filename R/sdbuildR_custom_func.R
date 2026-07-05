#' Round values half-up (as in Insight Maker)
#'
#' R rounds .5 to 0, whereas Insight Maker rounds .5 to 1. This function is the equivalent of Insight Maker's Round() function.
#'
#' @param x Value
#' @param digits Number of digits; optional, defaults to 0
#'
#' @returns Rounded value
#' @concept convenience
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
  ifelse(x %% 1 == 0.5 | x %% 1 == -0.5,
    ceiling(x),
    round(x, digits)
  )
}


#' Logit function
#'
#' @param p Probability, numerical value between 0 and 1
#'
#' @returns Numerical value
#' @concept convenience
#' @export
#'
#' @examples
#' logit(.1)
logit <- function(p) {
  log(p / (1 - p))
}


#' Expit function
#'
#' Inverse of the logit function
#'
#' @param x Numerical value
#'
#' @returns Numerical value
#' @concept convenience
#' @export
#'
#' @examples
#' expit(1)
expit <- function(x) {
  1 / (1 + exp(-x))
}


#' Generate random logical value
#'
#' Equivalent of RandBoolean() in Insight Maker
#'
#' @param p Probability of TRUE, numerical value between 0 and 1
#'
#' @returns Logical value
#' @concept convenience
#' @export
#'
#' @examples
#' rbool(.5)
rbool <- function(p) {
  stats::runif(1) < p
}


#' Generate random number from custom distribution
#'
#' Equivalent of RandDist() in Insight Maker
#'
#' @param a Vector to draw sample from
#' @param b Vector of probabilities
#'
#' @returns One sample from custom distribution
#' @concept convenience
#' @export
#'
#' @examples
#' rdist(c(1, 2, 3), c(.5, .25, .25))
rdist <- function(a, b) {
  sample(a, size = 1, prob = b)
}


#' Find index of value in vector or string
#'
#' Equivalent of .IndexOf() in Insight Maker.
#'
#' @param haystack Vector or string to search through
#' @param needle Value to search for
#'
#' @returns Index, integer
#' @concept convenience
#' @export
#'
#' @examples
#' indexof(c("a", "b", "c"), "b") # 2
#' indexof("haystack", "hay") # 1
#' indexof("haystack", "m") # 0
indexof <- function(haystack, needle) {
  if (length(haystack) == 1 && is.character(haystack)) {
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
#' @returns The number of elements in x if x is a vector; the number of characters in x if x is a string
#' @concept convenience
#' @export
#'
#' @examples
#' length_IM(c("a", "b", "c")) # 3
#' length_IM("abcdef") # 6
length_IM <- function(x) {
  if (length(x) == 1 && is.character(x)) {
    return(nchar(x, type = "chars", allowNA = TRUE, keepNA = TRUE))
  } else {
    return(length(x))
  }
}


#' Check whether value is in vector or string
#'
#' Equivalent of .Contains() in Insight Maker.
#'
#' @param haystack Vector or string to search through
#' @param needle Value to search for
#'
#' @returns Logical value
#' @concept convenience
#' @export
#'
#' @examples
#' contains_IM(c("a", "b", "c"), "d") # FALSE
#' contains_IM(c("abcdef"), "bc") # TRUE
contains_IM <- function(haystack, needle) {
  if (length(haystack) == 1 && is.character(haystack)) {
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
#' @returns Ramp interpolation function
#' @concept input
#' @seealso [step()], [pulse()], [seasonal()]
#' @examples
#' # Create a simple model with a ramp function
#' sfm <- stockflow() |>
#'   update("a", "stock") |>
#'   # Specify the global variable "times" as simulation times
#'   update("input", "constant", eqn = "ramp(times, 20, 30, 3)") |>
#'   update("inflow", "flow", eqn = "input(t)", to = "a")
#'
#' \dontshow{
#' sfm <- sim_settings(sfm, save_by = 1, dt = .1)
#' }
#'
#' sim <- simulate(sfm, only_stocks = FALSE)
#' plot(sim)
#'
#' # To create a decreasing ramp, set the height to a negative value
#' sfm <- update(sfm, "input", eqn = "ramp(times, 20, 30, -3)")
#'
#' sim <- simulate(sfm, only_stocks = FALSE)
#' plot(sim)
#'
ramp <- function(times, start, finish, height = 1) {
  if (finish < start) {
    cli::cli_abort(c(
      "Invalid ramp parameters.",
      "x" = "The {.arg finish} time cannot be before the {.arg start} time.",
      "i" = "To create a decreasing ramp, set {.arg height} to a negative value.",
      ">" = "Adjust {.arg finish} to be greater than {.arg start}, or use a negative {.arg height}."
    ))
  }

  if (start < times[1]) {
    cli::cli_warn(c(
      "Ramp starts before simulation time.",
      "i" = "The {.arg start} time {.val {start}} is before the simulation start at {.val {times[1]}}.",
      ">" = "Consider adjusting the {.arg start} parameter to be within the simulation time range."
    ))
  }

  if (start > times[length(times)]) {
    cli::cli_warn(c(
      "Ramp starts after simulation time.",
      "i" = "The {.arg start} time {.val {start}} is after the simulation ends at {.val {times[length(times)]}}.",
      ">" = "The ramp will have no effect on the simulation."
    ))

    # In this case, no need to compute signal
    signal <- data.frame(times = times[c(1, length(times))], y = c(0, 0))
    input <- stats::approxfun(signal, rule = 2, method = "constant")
    return(input)
  } else if (finish < times[1]) {
    cli::cli_warn(c(
      "Ramp finishes before simulation time.",
      "i" = "The {.arg finish} time {.val {finish}} is before the simulation start at {.val {times[1]}}.",
      ">" = "The ramp will be at its final {.arg height} for the entire simulation."
    ))

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
  input
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
#' @returns Pulse interpolation function
#' @seealso [step()], [ramp()], [seasonal()]
#' @concept input
#' @examples
#' # Create a simple model with a pulse function
#' # that starts at time 5, jumps to a height of 2
#' # with a width of 1, and does not repeat
#' sfm <- stockflow() |>
#'   update("a", "stock") |>
#'   # Specify the global variable "times" as simulation times
#'   update("input", "constant", eqn = "pulse(times, 5, 2, 1)") |>
#'   update("inflow", "flow", eqn = "input(t)", to = "a")
#'
#' \dontshow{
#' sfm <- sim_settings(sfm, dt = .1)
#' }
#'
#' sim <- simulate(sfm, only_stocks = FALSE)
#' plot(sim)
#'
#' # Create a pulse that repeats every 5 time units
#' sfm <- update(sfm, "input", eqn = "pulse(times, 5, 2, 1, 5)")
#'
#' sim <- simulate(sfm, only_stocks = FALSE)
#' plot(sim)
#'
pulse <- function(times, start, height = 1, width = 1, repeat_interval = NULL) {
  if (width <= 0) {
    cli::cli_abort(c(
      "Invalid {.arg width} parameter.",
      "x" = "The {.arg width} parameter cannot be equal to or less than {.val {0}}.",
      "i" = "To create an instantaneous pulse, use the simulation step size {.code {P[[\"timestep_name\"]]}}.",
      ">" = "Set {.arg width} to a positive value."
    ))
  }

  if (start < times[1]) {
    cli::cli_warn(c(
      "Pulse starts before simulation time.",
      "i" = "The {.arg start} time {.val {start}} is before the simulation start at {.val {times[1]}}.",
      ">" = "Consider adjusting the {.arg start} parameter to be within the simulation time range."
    ))
  }

  if (start > times[length(times)]) {
    cli::cli_warn(c(
      "Pulse starts after simulation time.",
      "i" = "The {.arg start} time {.val {start}} is after the simulation ends at {.val {times[length(times)]}}.",
      ">" = "The pulse will have no effect on the simulation."
    ))

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
    if (width >= repeat_interval) {
      cli::cli_warn(c(
        "Pulse configuration creates continuous output.",
        "i" = "The {.arg width} ({.val {width}}) is greater than or equal to {.arg repeat_interval} ({.val {repeat_interval}}).",
        "!" = "This creates a continuous pulse instead of discrete pulses.",
        ">" = "Consider reducing {.arg width} or increasing {.arg repeat_interval}."
      ))

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
    signal <- rbind(
      signal,
      data.frame(times = times[length(times)], y = 0)
    )
  }

  signal <- signal[order(signal[["times"]]), ]

  # Create linear approximation function, use constant interpolation to get a block shape even at finer sampling times
  input <- stats::approxfun(signal, rule = 2, method = "constant")
  input
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
#' @returns Step interpolation function
#' @seealso [ramp()], [pulse()], [seasonal()]
#' @concept input
#' @examples
#' # Create a simple model with a step function
#' # that jumps at time 50 to a height of 5
#' sfm <- stockflow() |>
#'   update("a", "stock") |>
#'   # Specify the global variable "times" as simulation times
#'   update("input", "constant", eqn = "step(times, 50, 5)") |>
#'   update("inflow", "flow", eqn = "input(t)", to = "a")
#'
#' \dontshow{
#' sfm <- sim_settings(sfm, dt = .1)
#' }
#'
#' sim <- simulate(sfm, only_stocks = FALSE)
#' plot(sim)
#'
#' # Negative heights are also possible
#' sfm <- update(sfm, "input", eqn = "step(times, 50, -10)")
#'
#' sim <- simulate(sfm, only_stocks = FALSE)
#' plot(sim)
step <- function(times, start, height = 1) {
  if (start < times[1]) {
    cli::cli_warn(c(
      "Step starts before simulation time.",
      "i" = "The {.arg start} time {.val {start}} is before the simulation start at {.val {times[1]}}.",
      ">" = "Consider adjusting the {.arg start} parameter to be within the simulation time range."
    ))
  }

  if (start > times[length(times)]) {
    cli::cli_warn(c(
      "Step starts after simulation time.",
      "i" = "The {.arg start} time {.val {start}} is after the simulation ends at {.val {times[length(times)]}}.",
      ">" = "The step will have no effect on the simulation."
    ))

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
  input
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
#' @returns Seasonal interpolation function
#' @concept input
#' @seealso [step()], [pulse()], [ramp()]
#' @export
#'
#' @examples
#' # Create a simple model with a seasonal wave
#' sfm <- stockflow() |>
#'   update("a", "stock") |>
#'   # Specify the global variable "times" as simulation times
#'   update("input", "constant", eqn = "seasonal(times, 10, 0)") |>
#'   update("inflow", "flow", eqn = "input(t)", to = "a")
#'
#' sim <- simulate(sfm, only_stocks = FALSE)
#' plot(sim)
#'
seasonal <- function(times, period = 1, shift = 0) {
  if (period <= 0) {
    cli::cli_abort(c(
      "Invalid {.arg period} parameter.",
      "x" = "The {.arg period} must be greater than {.val {0}}.",
      ">" = "Set {.arg period} to a positive value."
    ))
  }

  # Create linear approximation function - define wave in advance so that the period and shift argument do not need to be kept
  signal <- cos(2 * pi * (times - shift) / period)
  input <- stats::approxfun(x = times, y = signal, rule = 2, method = "linear")
  input
}


#' Check whether x is less than zero
#'
#' Check whether x is less than zero.
#'
#' @param x Value
#'
#' @returns x if x is greater than 0, 0 otherwise
#' @concept internal
#' @export
#'
#' @examples
#' nonnegative(NA)
#' nonnegative(-1)
#'
nonnegative <- function(x) {
  # # Safe comparison to zero
  # if (is.na(x)) {
  #   return(x)
  # } else {
  return(max(c(0, x)))
  # }
}


#' Remainder and modulus
#'
#' Remainder and modulus operators. The modulus and remainder are not the same in case either a or b is negative. If you work with negative numbers, modulus is always non-negative (it matches the sign of the divisor).
#'
#' @param a Dividend
#' @param b Divisor
#'
#' @returns Remainder
#' @concept convenience
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
  a - b * trunc(a / b)
}


#' @export
#' @concept convenience
#' @rdname rem_mod
mod <- function(a, b) {
  a %% b
}


#' @export
#' @concept convenience
#' @rdname rem_mod
`%REM%` <- function(a, b) {
  rem(a, b)
}


#' Logistic function
#'
#' Computes the logistic (i.e., sigmoid) function with configurable slope, midpoint, and upper asymptote.
#'
#' @param x Value at which to evaluate the function
#' @param slope Slope of logistic function at the midpoint. Defaults to 1.
#' @param midpoint Midpoint of logistic function where the output is `upper/2`. Defaults to 0.
#' @param upper Upper asymptote (maximal value) of the logistic function. Defaults to 1.
#'
#' @returns Numeric value given by \deqn{f(x) = \frac{upper}{1 + e^{-slope \cdot (x - midpoint)}}}
#'
#' @details
#' The logistic function is a smooth S-shaped curve bounded between 0 and `upper`.
#' It transitions from near 0 to near `upper` around the `midpoint`, with the steepness
#' of this transition controlled by `slope`.
#'
#' @concept convenience
#' @export
#'
#' @examples
#' logistic(0)
#' # equivalent:
#' sigmoid(0)
#'
#' # Adjust parameters
#' logistic(0, slope = 5, midpoint = 0.5, upper = 10)
#'
#' # Visualize different slopes
#' curve(logistic(x, slope = 1), from = -5, to = 5, ylab = "f(x)", ylim = c(0, 1))
#' curve(logistic(x, slope = 5), add = TRUE, col = "blue")
#' curve(logistic(x, slope = 50), add = TRUE, col = "red")
#' legend("topleft",
#'   legend = c("slope = 1", "slope = 5", "slope = 50"),
#'   col = c("black", "blue", "red"), lty = 1
#' )
logistic <- function(x, slope = 1, midpoint = 0, upper = 1) {
  if (!is.numeric(slope)) {
    cli::cli_abort(c(
      "Invalid {.arg slope} parameter.",
      "x" = "The {.arg slope} parameter must be numeric."
    ))
  }

  if (!is.numeric(midpoint)) {
    cli::cli_abort(c(
      "Invalid {.arg midpoint} parameter.",
      "x" = "The {.arg midpoint} parameter must be numeric."
    ))
  }

  if (!is.numeric(upper)) {
    cli::cli_abort(c(
      "Invalid {.arg upper} parameter.",
      "x" = "The {.arg upper} parameter must be numeric."
    ))
  }

  # # Use numerically stable computation
  # # For large positive z: result ≈ upper
  # # For large negative z: result ≈ 0
  # # Avoid underflow by using pmin/pmax to keep result strictly between 0 and upper
  # z <- slope * (x - midpoint)

  # # Compute logistic with numerical stability
  # # When z is very large, exp(-z) underflows to 0, so we use upper directly
  # # When z is very small, exp(z) might overflow, so we handle carefully
  # result <- upper / (1 + exp(-pmin(pmax(z, -500), 500)))

  # # Ensure result is strictly less than upper and greater than 0 (no exact bounds)
  # # Use nextafter equivalent: subtract smallest positive float to ensure strict inequality
  # result <- pmin(result, upper * (1 - .Machine$double.eps))
  # result <- pmax(result, upper * .Machine$double.eps)
  # result
  upper / (1 + exp(-slope * (x - midpoint)))
}


#' @rdname logistic
#' @concept convenience
#' @export
sigmoid <- logistic


#' Hill function
#'
#' Computes the Hill function with configurable slope, midpoint, and upper asymptote.
#'
#' @param x Value at which to evaluate the function
#' @param slope Slope of Hill function at the midpoint. Defaults to 1.
#' @param midpoint Midpoint of Hill function where the output is `upper/2`. Defaults to 0.5.
#' @param upper Upper asymptote (maximal value) of the Hill function. Defaults to 1.
#'
#' @returns Numeric value given by \deqn{f(x) = \frac{upper \cdot x^{slope}}{midpoint^{slope} + x^{slope}}}
#'
#' @details
#' The Hill function is a smooth S-shaped curve (when slope > 1) bounded between 0 and `upper`.
#' It transitions from near 0 to near `upper` around the `midpoint`, with the steepness
#' of this transition controlled by `slope`. See \url{https://en.wikipedia.org/wiki/Hill_equation_%28biochemistry%29} for more details.
#'
#' @concept convenience
#' @export
#'
#' @examples
#' hill(0)
#'
#' # Adjust parameters
#' hill(0, slope = 5, midpoint = 0.5, upper = 10)
#'
hill <- function(x, slope = 1, midpoint = 0.5, upper = 1) {
  if (!is.numeric(slope)) {
    cli::cli_abort(c(
      "Invalid {.arg slope} parameter.",
      "x" = "The {.arg slope} parameter must be numeric."
    ))
  }

  if (!is.numeric(midpoint)) {
    cli::cli_abort(c(
      "Invalid {.arg midpoint} parameter.",
      "x" = "The {.arg midpoint} parameter must be numeric."
    ))
  }

  if (!is.numeric(upper)) {
    cli::cli_abort(c(
      "Invalid {.arg upper} parameter.",
      "x" = "The {.arg upper} parameter must be numeric."
    ))
  }

  upper * x^slope / (midpoint^slope + x^slope)
}


#' Generalized Ricker function
#'
#' Computes the generalized Ricker function, a smooth hump-shaped curve that rises
#' from zero, peaks, and then decays back towards zero. It is commonly used to
#' describe humped (non-monotonic) dependencies, such as stock-recruitment
#' relationships in ecology or size-dependent predation.
#'
#' @param x Value at which to evaluate the function. Because the curve involves a fractional power of `x`, `x` is expected to be non-negative.
#' @param location Value of `x` at which the function reaches its peak. Defaults to 1.
#' @param upper Maximal value (height) of the function, attained at the peak. Defaults to 1.
#' @param shape Exponent controlling the width of the peak: values above 1 narrow
#'   the peak, values below 1 broaden it. `shape = 1` gives the standard Ricker
#'   function. Defaults to 1.
#' @param a,b Coefficients of the equivalent expanded form
#'   \eqn{f(x) = a \cdot x^{shape} \cdot e^{-b \cdot x}}. Optional alternative to
#'   `location` and `upper`: when both `a` and `b` are supplied, they take
#'   precedence and set `location = shape / b` and `upper = a \cdot (location / e)^{shape}`.
#'   With `shape = 1` this is the standard Ricker parameterization
#'   \eqn{f(x) = a \cdot x \cdot e^{-b \cdot x}}. Supplying only one of them, or
#'   combining them with an explicit `location` or `upper`, is an error. Default
#'   to `NULL`.
#'
#' @returns Numeric value given by \deqn{f(x) = upper \cdot \left(\frac{x}{location} \cdot e^{1 - x / location}\right)^{shape}}
#'
#' @details
#' The generalized Ricker function (Persson et al., 1998) is defined as:
#' 
#' \deqn{f(x) = upper \cdot \left(\frac{x}{location} \cdot e^{1 - x / location}\right)^{shape}}
#' 
#' with a power parameter (\eqn{\alpha}, or `shape`) that broadens or narrows the
#' peak. The function peaks at `x = location`, where it attains the value `upper`,
#' for any `shape`.
#'
#' Expanding the expression shows that it is equivalent to:
#'
#' \deqn{f(x) = a \cdot x^{shape} \cdot e^{-b \cdot x}}
#'
#' with coefficients
#'
#' \deqn{a = upper \cdot (e / location)^{shape}}
#' \deqn{b = shape / location}
#'
#' or equivalently
#'
#' \deqn{location = shape / b}
#' \deqn{upper = a \cdot (location / e)^{shape}.}
#'
#' Note that \eqn{e} is the base of the natural logarithm (i.e., `exp(1)`). When
#' `shape = 1`, the power on `x` is 1 and this reduces to the standard Ricker
#' function \eqn{f(x) = a \cdot x \cdot e^{-b \cdot x}}, with \eqn{a = upper \cdot e / location}
#' and \eqn{b = 1 / location}.
#'
#' See Bolker, B. M. (2008). \emph{Ecological Models and Data in R}. Princeton
#' University Press, Section 8.1.
#'
#' @concept convenience
#' @export
#'
#' @examples
#' ricker(1)
#'
#' # Adjust parameters
#' ricker(2, location = 2, upper = 10, shape = 1)
#'
#' # Use the expanded form f(x) = a * x^shape * exp(-b * x) instead.
#' # With shape = 1 this is the standard Ricker f(x) = a * x * exp(-b * x).
#' ricker(3, a = 2.5, b = 0.4)
#' # equivalent to:
#' ricker(3, location = 1 / 0.4, upper = 2.5 * (1 / 0.4 / exp(1)))
#'
#' # The mapping holds for any shape, e.g. f(x) = a * x^2 * exp(-b * x)
#' ricker(3, a = 2.5, b = 0.4, shape = 2)
#'
#' # Visualize different peak widths
#' curve(ricker(x, location = 2), from = 0, to = 10, ylab = "f(x)", ylim = c(0, 1.5))
#' curve(ricker(x, location = 2, shape = 0.5), add = TRUE, col = "blue")
#' curve(ricker(x, location = 2, shape = 3), add = TRUE, col = "red")
#' legend("topright",
#'   legend = c("shape = 1", "shape = 0.5", "shape = 3"),
#'   col = c("black", "blue", "red"), lty = 1
#' )
ricker <- function(x, location = 1, upper = 1, shape = 1, a = NULL, b = NULL) {
  if (!is.numeric(shape)) {
    cli::cli_abort(c(
      "Invalid {.arg shape} parameter.",
      "x" = "The {.arg shape} parameter must be numeric."
    ))
  }

  # Standard (a, b) parameterization, when supplied, overrides location/upper.
  # These are the coefficients of the generalized form f(x) = a * x^shape * exp(-b * x).
  if (!is.null(a) || !is.null(b)) {
    if (is.null(a) || is.null(b)) {
      cli::cli_abort(c(
        "Incomplete standard Ricker parameterization.",
        "x" = "Both {.arg a} and {.arg b} must be supplied together.",
        "i" = "Either use {.arg location} and {.arg upper}, or supply both {.arg a} and {.arg b}."
      ))
    }

    if (!missing(location) || !missing(upper)) {
      cli::cli_abort(c(
        "Conflicting Ricker parameterizations.",
        "x" = "Supply either {.arg location} and {.arg upper}, or {.arg a} and {.arg b}, not both."
      ))
    }

    if (!is.numeric(a)) {
      cli::cli_abort(c(
        "Invalid {.arg a} parameter.",
        "x" = "The {.arg a} parameter must be numeric."
      ))
    }

    if (!is.numeric(b)) {
      cli::cli_abort(c(
        "Invalid {.arg b} parameter.",
        "x" = "The {.arg b} parameter must be numeric."
      ))
    }

    # Convert the generalized coefficients to the peak parameterization
    location <- shape / b
    upper <- a * (location / exp(1))^shape
  }

  if (!is.numeric(location)) {
    cli::cli_abort(c(
      "Invalid {.arg location} parameter.",
      "x" = "The {.arg location} parameter must be numeric."
    ))
  }

  if (!is.numeric(upper)) {
    cli::cli_abort(c(
      "Invalid {.arg upper} parameter.",
      "x" = "The {.arg upper} parameter must be numeric."
    ))
  }

  upper * (x / location * exp(1 - x / location))^shape
}

#' Internal function to save data frame at specific times
#'
#' Internal function used to save the data frame at specific times in case the save interval (`save_by`) is not equal to dt in the simulation specifications.
#'
#' @param df data.frame in wide format
#' @param time_col Name of the time column
#' @param new_times Vector of new times to save the data frame at
#'
#' @returns Interpolated data.frame. The data frame has columns \code{time} followed by
#'   one column per variable.
#' @concept internal
#' @export
#' @examples
#' # Recommended: Use save_by in sim_settings() to downsample simulations
#' sfm <- stockflow("sir") |> sim_settings(dt = 0.01, save_by = 1)
#' sim <- simulate(sfm)
#' df <- as.data.frame(sim)
#' nrow(df) # Returns only times at intervals of 1
#' head(df)
#'
#' # The saveat_func() is the underlying function used by simulate()
#' # Direct use is not recommended, but shown here for completeness:
#' sfm <- sfm |> sim_settings(save_by = 0.01)
#' sim <- simulate(sfm)
#' df <- as.data.frame(sim)
#' nrow(df) # Many more rows
#'
#' # Manual downsampling (not recommended - use save_by instead)
#' new_times <- seq(min(df$time), max(df$time), by = 1)
#' df_wide <- as.data.frame(sim, direction = "wide")
#' df_manual <- saveat_func(df_wide, "time", new_times)
#' nrow(df_manual)
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

  result
}
