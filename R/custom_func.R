### Custom functions to match InsightMaker functions

#' Convert InsightMaker's Round() function to R
#'
#' This function ensures that rounding in R matches rounding in InsightMaker. R base's round() rounds .5 to 0, whereas round_IM() rounds .5 to 1.
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
  # posneg = sign(x)
  # z = abs(x)*10^digits
  # z = z + 0.5 + sqrt(.Machine$double.eps)
  # z = trunc(z)
  # z = z/10^digits
  # z*posneg
#
#   if (inherits(x, "units")){
#     x_has_units = TRUE
#     x_units = units(x)
#     x = drop_if_units(x)
#   } else {
#     x_has_units = FALSE
#   }

  y = ifelse(x %% 1 == 0.5 | x %% 1 == -0.5,
         ceiling(x),
         round(x, digits))

  # # Add units back in
  # if (x_has_units){
  #   y = units::set_units(y, x_units)
  # }
  return(y)
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

  # p = drop_if_unitless(p)

  return(log(p/(1-p)))
}


#' Expit function; inverse of the logit function
#'
#' @param x Numerical value
#'
#' @return Numerical value
#' @export
#'
#' @examples
#' expit(1)
expit = function(x){

  # x = drop_if_unitless(x)
  return(1 / (1+exp(-x)))
}



#' Generate random boolean value
#' Equivalent of RandBoolean() in InsightMaker
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
  # return(sample(c(T,F), size = 1, prob = c(p, 1-p)))
}


#' Generate random number from custom distribution
#' Equivalent of RandDist() in InsightMaker
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
#' Equivalent of .IndexOf() in InsightMaker
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
#' Equivalent of .Length() in InsightMaker, which returns the number of elements when performed on a vector, but returns the number of characters when performed on a string
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
#' Equivalent of .Contains() in InsightMaker
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
#' Equivalent of .Range() in InsightMaker
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
#' Equivalent of .Filter() in InsightMaker when using arguments x and key in the filter function
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




# **to do: have to create separate function for pulse/ramp/steo so that unit isnt set
# and when people access e.g. lottery winnings, lottery winnings should be = lottery winnings pulse (t)


#' Create ramp function
#' Equivalent of Ramp() in InsightMaker
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
  if (min(start_t_ramp) > dplyr::first(times)){
    signal = rbind(data.frame(times = dplyr::first(times), y = 0), signal)
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
#' Equivalent of Pulse() in InsightMaker
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
  start_ts = seq(start_t_pulse, dplyr::last(times),
                 # If the number of repeat is NULL, ensure no repeats
                 by = ifelse(is.null(repeat_interval), dplyr::last(times) + 1,
                             repeat_interval))
  end_ts = start_ts + w_pulse

  signal = rbind(
        data.frame(times = start_ts, y = h_pulse),
        data.frame(times = end_ts, y = 0))

  # If pulse is after the start of signal, add a zero at the start
  if (min(start_ts) > dplyr::first(times)){
    signal = rbind(signal, data.frame(times = dplyr::first(times), y = 0))
  }

  # If pulse does not cover end of signal, add a zero at the end
  # (I don't fully understand why this is necessary, but otherwise it gives incorrect results with repeat_interval <= 0 in Julia, so for consistency's sake)
  if (max(end_ts) < dplyr::last(times)){
    signal = rbind(signal, data.frame(times = dplyr::last(times), y = 0))
  }

  signal = signal %>%
    dplyr::arrange(times)

  # Create linear approximation function, use constant interpolation to get a block shape even at finer sampling times
  input = stats::approxfun(signal, rule = 2, method = 'constant')
  return(input)
}




#' Create step function
#' Equivalent of Step() in InsightMaker
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

  if (start_t_step > dplyr::first(times)){
    signal = rbind(data.frame(times = dplyr::first(times), y = 0), signal)
  }

  # Create linear approximation function
  input = stats::approxfun(signal, rule = 2, method = "constant")
  return(input)
}


#' Annual sine wave with an amplitude of 1
#' Equivalent of Seasonal() in InsightMaker
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
  # } else if (inherits(x, "units")){
  #   if (drop_if_units(x) < 0){
  #     return(x - x) # Preserve units, x - x will evaluate to 0 (with units) in case it is negative
  #   } else {
  #     return(x)
  #   }
  } else {
    return(max(c(0, x)))
  }
}





# Wolf.birth.rate = pluck_from_ode(ode_func, "Wolf.birth.rate", xstart, pars, times[1])


#' Initialize variable from ODE
#'
#' Some initial conditions and parameters need a variable from the ODE to be defined. Find the minimal set of equations in ode_func to define var_name.
#'
#' @param var_name String; desired variable name
#' @param envir Environment
#'
#' @return Value
#' @export
#'
#' @examples
pluck_from_ode_old = function(var_name, envir){

  ode_func = get(P$ode_func_name, envir = envir)
  xstart = get(P$initial_value_name, envir = envir)
  pars = get(P$parameter_name, envir = envir)


  # Convert the body of the function to character lines
  func_code <- deparse(body(ode_func), width.cutoff = 500)

  # Initialize a list to store dependencies for each line
  dependencies_list <- list()

  # Loop through each line in the function body
  for (i in seq_along(func_code)) {
    # Parse the line as an expression
    expr <- tryCatch(parse(text = func_code[i]), error = function(e) NULL)

    # If parsing was successful, extract the variable names used in the line
    if (!is.null(expr)) {
      vars <- all.vars(expr)
      dependencies_list[[i]] <- vars
    } else {
      dependencies_list[[i]] <- NA  # If parsing fails, mark it as NA
    }
  }
  dependencies_list

  # Find which variables are defined in each equation
  defined_var = dependencies_list %>% purrr::map(1)
  defined_var[sapply(defined_var, is.null)] = ""
  defined_var = unlist(defined_var)

  # Only include defined variables in dependencies
  dependencies_list = lapply(dependencies_list, function(x){
    if (all(is.na(x))){
      return(x)
    } else {
      intersect(x, defined_var)
    }
    })

  # Equation number which we're ultimately interested in
  eq_var_name = which(defined_var == var_name)

  # We need at least eq_var_name, but we also need all equations that eq_var_name is dependent on, and the ones that those equations are dependent on, and so on...

  deep_dependencies = dependencies_list[[eq_var_name]]
  if (length(deep_dependencies) == 0){
    necessary_idx_eq = eq_var_name
  } else {
    done = F
    while (!done){
      old = deep_dependencies

      # Get dependencies of dependencies
      deep_dependencies = c(deep_dependencies,
                              unname(unlist(dependencies_list[match(deep_dependencies, defined_var)]))) %>%
        Filter(nzchar, .) %>% unique() %>%
        # Remove dependency in S to prevent stock_units from being needed (used before with() statement in ode_func)
        setdiff(., P$state_name)

      # While-loop ends if there is no change
      if (setequal(old, deep_dependencies)){ done = T}
    }
    necessary_idx_eq = sort(match(deep_dependencies, defined_var))
  }

  # defined_var %in% dependencies_list[[eq_var_name]] & seq_along(defined_var) <= eq_var_name

  # Keep the equations above eq_var_name which define the dependencies of eq_var_name
  necessary_eq = func_code[necessary_idx_eq] %>% trimws() %>% paste0(collapse = "\n")

  # print(necessary_eq)

  # Some equations need S (e.g. DelayN())
  # S = unlist(xstart)

  # Unwrap the list elements into the current environment
  list2env(get(P$parameter_name), envir = envir)
  list2env(get(P$initial_value_name), envir = envir)
  assign(P$state_name, unlist(xstart), envir = envir) # # Some equations need S (e.g. DelayN())
  assign(P$time_name, get(P$times_name, envir = envir)[1], envir = envir)

  chosen_var = {
    eval(parse(text = necessary_eq), envir = envir)
    return(eval(parse(text = var_name), envir = envir)) # Only keep variable of interest
  }
  # chosen_var = with( c(S, pars), {
  #   eval(parse(text = necessary_eq), envir = envir)
  #   return(eval(parse(text = var_name)))
  # })

  return(chosen_var)
}



#' Create unit
#'
#' Units are only supported in Julia, not in R.
#'
#' @param unit_def Unit definition; e.g. u('s')
#'
#' @returns Unit
#' @export
#'
#' @examples u('s')
u = function(unit_def){
  return(unit_def)
}



#' Drop unit
#'
#' Units are only supported in Julia, not in R.
#'
#' @param x Variable with unit
#'
#' @returns Unit
#' @export
#'
#' @examples
drop_u = function(x){
  return(x)
}



#' Set unit of x
#'
#' Units are only supported in Julia, not in R.
#'
#' @param x Variable
#' @inheritParams u
#'
#' @returns x
#' @export
#'
#' @examples
#' setunit(8, u('wk')) # 8 weeks
#'
#'
setunit = function(x, unit_def){
  return(x)
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


# **to do: sigmoid etc.
# logrange https://docs.julialang.org/en/v1/base/math/#Base.logrange
