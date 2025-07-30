
#' Specify unit in equations
#'
#' Flexibly use units in equations by enclosing them in `u()`. Note that units are only supported in Julia, not in R.
#'
#' Unit strings are converted to their standard symbols using regular expressions. This means that you can easily specify units without knowing their standard symbols. For example, u('kilograms per meters squared') will become 'kg/m^2'. You can use title-case for unit names, but letters cannot all be uppercase if this is not the standard symbol. For example, 'kilogram' works, but 'KILOGRAM' does not. This is to ensure that the right unit is detected.
#'
#' @param unit_str Unit string; e.g. '3 seconds'
#'
#' @returns Specified unit (only in Julia)
#'
#' @seealso [model_units()], [unit_prefixes()], [convert_u()], [drop_u()]
#' @family units
#' @export
#'
#' @examples
#' # Use units in equations
#' sfm = xmile() %>%
#'  build("a", "constant", eqn = "u('10kilometers') - u('3meters')",
#'  units = "centimeters")
#'
#' # Units can also be set by multiplying a number with a unit
#' sfm = xmile() %>%
#'  build("a", "constant", eqn = "10 * u('kilometers') - u('3meters')")
#'
#' # Addition and subtraction is only allowed between matching units
#'sfm = xmile() %>%
#'  build("a", "constant", eqn = "u('3seconds') + u('1hour')")
#'
#' # Division, multiplication, and exponentiation are allowed between different units
#' sfm = xmile() %>%
#'  build("a", "constant", eqn = "u('10grams') / u('1minute')")
#'
#' # Use custom units in equations
#' sfm = xmile() %>%
#'   model_units("BMI", eqn = "kilograms/meters^2", doc = "Body Mass Index") %>%
#'   build("weight_gain", "flow", eqn = "u('2 BMI / year')", units = "BMI/year")
#'
#' # Unit strings are often needed in flows to ensure dimensional consistency
#' sfm = xmile() %>% sim_specs(stop = 1, time_units = "days") %>%
#'   build("consumed_food", "stock", eqn = "1", units = "kilocalories") %>%
#'   build("eating", "flow", eqn = "u('750kilocalories') / u('6hours')",
#'   units = "kilocalories/day", to = "consumed_food")
#'
u = function(unit_str){
  return(unit_str)
}



#' Drop unit in equation
#'
#' In rare cases, it may be desirable to drop the units of a variable within an equation. Use `drop_unit()` to render a variable unitless. See `?u()` for more information on the rules of specifying units. Note that units are only supported in Julia, not in R.
#'
#' @param x Variable with unit
#'
#' @returns Unitless variable
#' @seealso [model_units()], [unit_prefixes()], [u()], [convert_u()]
#' @family units
#' @export
#'
#' @examples
#' # For example, the cosine function only accepts unitless arguments or
#' # arguments with units in radians or degrees
#' sfm = xmile() %>%
#' build("a", "constant", eqn = "10", units = "minutes") %>%
#' build("b", "constant", eqn = "cos(drop_u(a))")
drop_u = function(x){
  return(x)
}



#' Convert unit in equation
#'
#' In rare cases, it may be desirable to change the units of a variable within an equation. Use `convert_u()` to convert a variable to another matching unit. See `?u()` for more information on the rules of specifying units. Note that units are only supported in Julia, not in R.
#'
#' @param x Variable
#' @param unit_def Unit definition, e.g. u('seconds')
#'
#' @returns Variable with new unit
#' @seealso [model_units()], [unit_prefixes()], [u()], [drop_u()]
#' @family units
#' @export
#'
#' @examples
#' # Change the unit of rate from minutes to hours
#'sfm = xmile() %>%
#'build("rate", "constant", eqn = "10", units = "minutes") %>%
#'build("change", "flow",
#'      eqn = "(room_temperature - coffee_temperature) / convert_u(rate, u('hour'))")
#'
convert_u = function(x, unit_def){
  return(x)
}


#' Split string per unit
#'
#' @param x String
#'
#' @noRd
#' @returns List
split_units = function(x){

  idxs_word_df = stringr::str_locate_all(x, "([a-zA-Z_][a-zA-Z_\\.0-9 ]*)")[[1]] %>% as.data.frame()

  # If there are no words, x does not need to be split
  if (nrow(idxs_word_df) == 0){
    x_split = list(x)
  } else {
    split_df = rbind(idxs_word_df,
                     data.frame(start = idxs_word_df[["end"]][-nrow(idxs_word_df)] + 1,
                                end = idxs_word_df[["start"]][-1] - 1))
    split_df = split_df[order(split_df[["start"]]), ]

    # Make sure beginning characters are included
    if (split_df[["start"]][1] > 1){
      split_df = rbind(data.frame(start = 1, end = split_df[["start"]][1] - 1),
                       split_df) %>% as.data.frame()
    }

    # Make sure final characters are included
    if (split_df[["end"]][nrow(split_df)] < nchar(x)){
      split_df = rbind(split_df, data.frame(start = split_df[["end"]][nrow(split_df)] + 1, end = nchar(x))) %>% as.data.frame()
    }

    # Split string
    x_split = lapply(1:nrow(split_df), function(i){
      stringr::str_sub(x, split_df[i, "start"], split_df[i, "end"])
    })
  }

  return(x_split)
}




#' Replace written powers ("squared", "cubed") in string
#'
#' @param x String with unit
#'
#' @return Cleaned string with unit
#' @noRd
#'
replace_written_powers = function(x){

  # Prepare regular expressions for detecting written powers
  powers = c("square", "cubic", "cube", "quartic", "quintic")
  regex_written_powers = stringr::regex(
    paste0("\\b", c("square[d]?", "cubic", "cube[d]?", "quartic", "quintic"), "\\b"),                                      ignore_case = TRUE)

  if (any(stringr::str_detect(x, regex_written_powers))){

    # Find all words
    idxs_words = get_words(x)

    if (nrow(idxs_words) == 0) return(x)

    # Find indices of powers
    idxs_power = stringr::str_locate_all(x, regex_written_powers)
    df_power = as.data.frame(do.call(rbind, idxs_power))
    df_power[["power"]] = rep(powers, sapply(idxs_power, nrow))
    df_power = df_power[order(df_power[, "start"]), ]

    if (nrow(idxs_words) == nrow(df_power)) return(x)

    for (i in rev(1:nrow(df_power))){
      pre = which(idxs_words[, "end"] < df_power[i, "start"] & nzchar(idxs_words[, "word"]))
      pre = ifelse(length(pre) > 0, pre[length(pre)], NA) # select last
      post = which(idxs_words[, "start"] > df_power[i, "end"] & nzchar(idxs_words[, "word"]))[1] # select first
      idx_power_word = which(idxs_words["start"] == df_power[i, "start"])

      if (df_power[i, "power"] %in% c("square", "cube")){

        # If there is a preceding word, use that
        if (!is.na(pre)){
          idxs_words[pre, "word"] = paste0(idxs_words[pre, "word"], ifelse(df_power[i, "power"] == "square", "^2", "^3"))
          idxs_words[idx_power_word, "word"] = "" # Remove power word
        } else {
          idxs_words[post, "word"] = paste0(idxs_words[post, "word"], ifelse(df_power[i, "power"] == "square", "^2", "^3"))
          idxs_words[idx_power_word, "word"] = "" # Remove power word
        }
      } else if (df_power[i, "power"] %in% c("cubic", "quartic", "quintic")){
        power_exp = switch(df_power[i, "power"],
                                 cubic = "^3",
                                 quartic = "^4",
                                 quintic = "^5")
        idxs_words[post, "word"] = paste0(idxs_words[post, "word"], power_exp)
        idxs_words[idx_power_word, "word"] = "" # Remove power word
      }

    }

    # Paste string together again
    y = idxs_words[, "word"]
    # Remove empty words
    x = paste0(y[y != ""], collapse = " ")
  }
  return(x)

}


#' Clean unit contained in u('')
#'
#' @param x Equation containing potentially multiple u()
#' @inheritParams clean_unit
#'
#' @returns Cleaned equation
#' @noRd
#'
#' @examples
#' clean_unit_in_u("u('10 Meters') + u('Kilograms per sec') + u('10 pounds squared')",
#' get_regex_units())
clean_unit_in_u = function(x, regex_units){
  # old:
  # stringr::str_replace_all(x, "\\bu\\([\"|'](.*?)[\"|']\\)", function(y){clean_unit(y, regex_units)})

  # Extract all u('...') patterns
  matches <- stringr::str_extract_all(x, "\\bu\\([\"|'](.*?)[\"|']\\)")[[1]]

  if (length(matches) == 0) return(x)

  # Remove surrounding u('')
  matches_no_u = sapply(matches, function(y){stringr::str_sub(y, 4, nchar(y)-2)}, USE.NAMES = FALSE)

  # Throw error if a match includes u(''): units cannot be nested
  if (any(stringr::str_detect(matches_no_u, "u\\([\"|']"))) {
    stop("Nested units u(' u('') ') are not allowed. Please remove the u() from the unit string.")
  }


  # Clean all matches at once
  cleaned <- paste0("u(\"", sapply(matches_no_u, clean_unit, regex_units = regex_units, USE.NAMES = FALSE), "\")")

  # Replace back
  result <- x
  for (i in seq_along(matches)) {
    # Need to do str_replace_all, because there might be multiple occurrences of the same unit and otherwise it always uses the first match
    result <- stringr::str_replace_all(result, stringr::fixed(matches[i]), cleaned[i])
  }

  return(result)
}


#' Convert units in string to Julia
#'
#' @param x String
#' @param regex_units Named vector with regular expressions in R as names and units in Julia as entries
#' @param ignore_case Boolean; if TRUE, ignore case when matching units
#' @param include_translation Boolean; if TRUE, add translation per unit to returned value
#' @param unit_name Boolean; if TRUE, x is a custom unit name and should be more rigorously cleaned
#' @noRd
#'
#' @returns Updated string
clean_unit <- function(x, regex_units, ignore_case = FALSE, include_translation = FALSE, unit_name = FALSE) {

  if (x == "1") {
    x_new = x
    x_parts = stats::setNames(x, x)

  } else {

    # Ensure there is no scientific notation
    x = scientific_notation(x, task = "remove")

    # Remove double spaces and trim
    x <- gsub("\\s+", " ", trimws(x))

    # Replace squared -> ^2 and cubed -> ^3
    x = sapply(x, replace_written_powers, USE.NAMES = FALSE)

    # Replace "per" with "/"
    x <- gsub("[[:space:]\\)][Pp]er[[:space:]\\(]", "/", x, ignore.case = ignore_case)

    # Split unit into separate parts
    x_split = split_units(x)
    x_split_clean = lapply(x_split, trimws)
    idx = lapply(x_split_clean,
                 stringr::str_detect,
                 stringr::regex(names(regex_units), ignore_case = ignore_case)) %>%
      lapply(which) %>%
      vapply(function(y) if (length(y) > 0) y[1] else NA_real_, numeric(1))

    # Concatenate parts
    x_parts = ifelse(!is.na(idx), unname(regex_units[idx]), unlist(x_split_clean)) %>%
      # Replace punctuation with underscore
      # sapply(function(y){gsub("[@#&$!^%*~{}|:;<>?`]", "_", y)}) %>%
      # sapply(function(y){gsub("[@#&\\$!%~\\{\\}\\|:;\\?`\\\\]", "_", y)}) %>%
      sapply(function(y){gsub("@|#|&|\\$|!|%|~|\\{|\\}|\\||:|;|\\?|`|\\\\", "_", y)}) %>%
      # Replace space between numbers with "*"
      sapply(function(y){gsub("([0-9]) ([0-9])", "\\1*\\2", y)}) %>%
      # Remove all spaces
      sapply(function(y){gsub("[[:space:]]", "", y)}) %>%
      stats::setNames(unlist(x_split_clean))
    x_new = paste0(x_parts, collapse = "")

    # # Replace punctuation with underscore
    # x_new <- gsub("[@#&$!]", "_", x_new)
    #
    # # Replace space between numbers with "*"
    # x_new <- gsub("([0-9]) ([0-9])", "\\1*\\2", x_new)
    #
    # # Remove all spaces
    # x_new <- gsub("[[:space:]]", "", x_new)

    # Add back scientific notation in case there are too many digits
    x_new = scientific_notation(x_new, task = "add")
  }

  # If x is a unit name, clean more rigorously
  if (unit_name){
    x_new = gsub("\\+|\\^|\\*|<|>|-|\\.|\\(|\\)|/", "_", x_new)

    # Unit names cannot lead with a number
    if (grepl("^[0-9]", x_new)){
      x_new = paste0("_", x_new)
    }
  }

  if (include_translation){
    return(list(x_parts = x_parts, x_new = x_new))
  } else {
    return(x_new)
  }
}






#' Find missing unit definitions
#'
#' @inheritParams build
#' @inheritParams clean_unit
#' @param new_eqns String or vector with new equations potentially containing unit strings
#' @param new_units String or vector with units of variables
#' @param R_or_Julia String with either "R" or "Julia" to indicate which regular expression to use.
#'
#' @noRd
#' @returns List with models units to add to sfm
#'
detect_undefined_units = function(sfm, new_eqns, new_units, regex_units, R_or_Julia = "Julia"){

  # Add undefined units to custom units
  units_in_model = c(sfm[["sim_specs"]][["time_units"]],
                     new_units,
                     # Extract units from equations
                     new_eqns %>%
                       stringr::str_extract_all(.,
                                                ifelse(R_or_Julia == "Julia", "\\bu[\"|'](.*?)[\"|']", "\\bu\\([\"|'](.*?)[\"|']\\)"))
  ) %>%
    unlist() %>%
    # lapply(scientific_notation) %>%
    lapply(split_units) %>% unlist() %>%
    unique() %>%
    Filter(nzchar, .)  %>%
    # Only keep entries with letters in them
    Filter(function(x){stringr::str_detect(x, "[a-zA-Z]")}, .)

  # Find units to define: ones not already included in Julia
  existing_units = names(sfm[["model_units"]])
  units_to_define = setdiff(units_in_model,
                            c(existing_units,
                              unname(regex_units)
                            ))

  if (length(units_to_define) > 0){

    add_model_units = lapply(units_to_define, function(u){
      list(name = u, eqn = "1", doc = "", prefix = FALSE)
    }) %>% stats::setNames(units_to_define)


  } else {
    add_model_units = list()
  }

  return(add_model_units)
}




#' View all standard units
#'
#' `get_units()` yields a dataframe with all standard units in Julia's Unitful package and added custom units by sdbuildR.
#'
#' @returns Dataframe with units in Julia
#' @family units
#' @export
#'
get_units = function(){

  units_df = matrix(c(
    "The meter, the SI base unit of length.", "m",      "Meter",     "",    TRUE,
    "The second, the SI base unit of time.", "s",      "Second",     "",    TRUE,
    "The ampere, the SI base unit of electric current.", "A",      "Ampere",     "",   TRUE,
    "The kelvin, the SI base unit of thermodynamic temperature.", "K",      "Kelvin",    "",       TRUE,
    "The candela, the SI base unit of luminous intensity.", "cd",     "Candela",    "",      TRUE,
    "The gram, the SI base unit for weight.", "g",      "Gram",        "",    TRUE,
    "The mole, the SI base unit for amount of substance.", "mol",    "Mole",         "",  TRUE,
    # Angles and solid angles
    "The steradian, a unit of spherical angle. There are 4pi sr in a sphere.", "sr",      "Steradian", "", TRUE,
    "The radian, a unit of angle. There are 2pi rad in a circle.", "rad",     "Radian", "", #** IM: 180/pi
    TRUE,
    "The degree, a unit of angle. There are 360 degrees in a circle.", "deg",  "Degree",      "pi/180", FALSE,

    # SI and related units
    "The hertz, an SI unit of frequency, defined as 1 s^-1.", "Hz",   "Hertz",           "1/s", TRUE,
    "The newton, an SI unit of force, defined as 1 kg * m / s^2.", "N", "Newton", "1kg*m/s^2", TRUE,
    "The pascal, an SI unit of pressure, defined as 1 N / m^2.", "Pa",   "Pascal", "1N/m^2", TRUE,
    "The joule, an SI unit of energy, defined as 1 N * m.", "J", "Joule", "1N*m", TRUE,
    "The watt, an SI unit of power, defined as 1 J / s.", "W", "Watt", "1J/s", TRUE,
    "The coulomb, an SI unit of electric charge, defined as 1 A * s.", "C", "Coulomb", "1A*s", TRUE,
    "The volt, an SI unit of electric potential, defined as 1 W / A.", "V", "Volt", "1W/A", TRUE,
    "The ohm, an SI unit of electrical resistance, defined as 1 V / A.", "Ohm", "Ohm", "1V/A", TRUE,
    "The siemens, an SI unit of electrical conductance, defined as 1 Ohm^-1", "S", "Siemens", "1/Ohm", TRUE,
    "The farad, an SI unit of electrical capacitance, defined as 1 s^4 * A^2 / (kg * m^2).", "F", "Farad", "1s^4*A^2/(kg*m^2)", TRUE,
    "The henry, an SI unit of electrical inductance, defined as 1 J / A^2.", "H", "Henry", "1J/(A^2)", TRUE,
    "The tesla, an SI unit of magnetic B-field strength, defined as 1 kg / (A * s^2).", "T", "Tesla", "1kg/(A*s^2)", TRUE,
    "The weber, an SI unit of magnetic flux, defined as 1 kg * m^2 / (A * s^2).", "Wb", "Weber", "1kg*m^2/(A*s^2)", TRUE,
    "The lumen, an SI unit of luminous flux, defined as 1 cd * sr.", "lm", "Lumen", "1cd*sr", TRUE,
    "The lux, an SI unit of illuminance, defined as 1 lm / m^2.", "lx", "Lux", "1lm/m^2", TRUE,
    "The becquerel, an SI unit of radioactivity, defined as 1 nuclear decay per s.", "Bq", "Becquerel", "1/s", TRUE,
    "The gray, an SI unit of ionizing radiation dose, defined as the absorption of 1 J per kg of matter.", "Gy", "Gray", "1J/kg", TRUE,
    "The sievert, an SI unit of the biological effect of an ionizing radiation dose.", "Sv", "Sievert", "1J/kg", TRUE,
    "The katal, an SI unit of catalytic activity, defined as 1 mol of catalyzed", "kat", "Katal", "1mol/s", TRUE,
    "Percent, a unit meaning parts per hundred. Printed as \"%\".", "%", "Percent", "1//100", FALSE,
    "Permille, a unit meaning parts per thousand. Printed as %", "permille", "Permille", "1//1000", FALSE,
    "Permyriad, a unit meaning parts per ten thousand.", "pertenthousand", "Pertenthousand", "1//10000", FALSE,
    "Percentmille, a unit meaning parts per hundred thousand.", "pcm", "Percentmille", "1//100000", FALSE,
    "Permillion, a unit meaning parts per million.", "ppm", "Permillion", "1//1000000", FALSE,
    "Perbillion, a unit meaning parts per billion (in the short-scale sense), i.e., 10^-9.", "ppb", "Perbillion", "1//1000000000", FALSE,
    "Pertrillion, a unit meaning parts per trillion (in the short-scale sense), i.e., 10^-12.","ppt",  "Pertrillion", "1//1000000000000", FALSE,
    "Perquadrillion, a unit meaning parts per quadrillion (in the short-scale sense), i.e., 10^-15.", "ppq", "Perquadrillion",  "1//1000000000000000", FALSE,

    # Temperature
    "The degree Celsius, an SI unit of temperature, defined such that 0 degrees C = 273.15 K.","degC", "Celsius", "(27315//100)K", FALSE,

    # Common units of time
    "The minute, a unit of time defined as 60 s. The full name `minute` is used instead of the symbol `min` to avoid confusion with the Julia function `min`.", "minute", "Minute", "60s", FALSE,
    "The hour, a unit of time defined as 60 minutes.", "hr", "Hour", "3600s", FALSE,
    "The day, a unit of time defined as 24 hr.", "d", "Day", "86400s", FALSE,
    "The week, a unit of time, defined as 7 d.", "wk", "Week", "604800s", FALSE,
    "The year, a unit of time, defined as 365.25 d.", "yr", "Year", "31557600s", TRUE,
    "Revolutions per second, a unit of rotational speed, defined as 2pi rad / s.","rps", "RevolutionsPerSecond", "2pi*rad/s", FALSE,
    "Revolutions per minute, a unit of rotational speed, defined as 2pi rad / minute.", "rpm", "RevolutionsPerMinute", "2pi*rad/minute", FALSE,

    # Area
    # The hectare is used more frequently than any other power-of-ten of an are.
    "The are, a metric unit of area, defined as 100 m^2.", "a", "Are", "100m^2", FALSE,
    "The hectare, a metric unit of area, defined as 100 a.", "ha", "Hectare", "", FALSE,
    "The barn, a metric unit of area, defined as 100 fm^2.", "b", "Barn", "100fm^2", TRUE,

    # Volume
    # `l` is also an acceptable symbol for liters
    "The liter, a metric unit of volume, defined as 1000 cm^3.", "L", "Liter", "m^3//1000", TRUE, # const l = L)

    # Molarity
    "A unit for measuring molar concentration, equal to 1 mol/L.", "M", "Molar", "1mol/L", TRUE,
    # Energy
    "A quantity equal to the elementary charge, the charge of a single electron, with a value of exactly 1.602,176,634 * 10^-19 C. The letter `q` is used instead of `e` to avoid confusion with Euler's number.", "q", "", "1.602_176_634e-19*C", FALSE,        # CODATA 2018; `e` means 2.718...
    "The electron-volt, a unit of energy, defined as q*V.", "eV", "eV", "q*V", TRUE,
    # For convenience
    "A unit for convenience in angular frequency, equal to 2pi Hz.", "AngHertz", "AngHertz", "2pi/s", TRUE,
    "The bar, a metric unit of pressure, defined as 100 kPa.", "bar", "Bar", "100000Pa", TRUE,
    "The standard atmosphere, a unit of pressure, defined as 101,325 Pa.", "atm", "Atmosphere", "101325Pa", TRUE,
    "The torr, a unit of pressure, defined as 1/760 atm.", "Torr", "Torr", "101325Pa//760", TRUE,

    # Constants (2018 CODATA values)        (uncertainties in final digits)
    "A quantity representing the speed of light in a vacuum, defined as exactly 2.997,924,58 * 10^8 m/s.", "c0", "", "299_792_458*m/s", FALSE,
    "The speed of light in a vacuum, a unit of speed, defined as exactly 2.997,924,58 * 10^8 m/s.", "c", "SpeedOfLight", "1c0", FALSE,
    "A quantity representing the vacuum permeability constant, defined as 4pi * 10^-7 H / m.", "magnetic_constant", "magnetic constant", "4pi*(1//10)^7*H/m", FALSE,
    "A quantity representing the vacuum permittivity constant, defined as 1 / (mu0 * c^2).", "electric_constant", "electric constant", "", FALSE,
    "A quantity representing the impedance of free space, a constant defined as mu0 * c.", "Z0", "impedance of free space", "", FALSE,
    "A quantity representing the universal gravitational constant, equal to 6.674,30 * 10^-11 m^3 / (kg * s^2) (the CODATA 2018 recommended value).", "G", "gravitational constant", "6.674_30e-11*m^3/kg/s^2", FALSE,
    "A quantity representing the nominal acceleration due to gravity in a vacuum near the surface of the earth, defined by standard to be exactly 9.806,65 m / s^2.", "gn", "standard acceleration of gravity", "9.80665*m/s^2", FALSE,
    "A quantity representing Planck's constant, defined as exactly 6.626,070,15 * 10^-34 J * s.", "h", "Planck constant", "6.626_070_15e-34*J*s", FALSE,
    "A quantity representing the reduced Planck constant, defined as h / 2pi.", "reduced_Planck_constant", "hbar", "h/2pi", FALSE, #hbar is already a unit -> prefix + bar
    "A quantity representing the superconducting magnetic flux quantum, defined as h / (2 * q).", "superconducting_magnetic_flux_quantum", "Superconducting magnetic flux quantum", "h/(2q)", FALSE,
    "A quantity representing the rest mass of an electron, equal to 9.109,383,7015 * 10^-31 kg (the CODATA 2018 recommended value).", "me", "electron rest mass", "9.109_383_7015e-31*kg", FALSE,
    "A quantity representing the rest mass of a neutron, equal to 1.674,927,498,04 * 10^-27 kg (the CODATA 2018 recommended value).", "mn", "neutron rest mass", "1.674_927_498_04e-27*kg", FALSE,
    "A quantity representing the rest mass of a proton, equal to 1.672,621,923,69 * 10^-27 kg (the CODATA 2018 recommended value).", "mp", "proton rest mass", "1.672_621_923_69e-27*kg", FALSE,
    "A quantity representing the Bohr magneton, equal to q * hbar / (2 * me).", "Bohr_magneton", "Bohr magneton", "q*hbar/(2*me)", FALSE,
    "A quantity representing Avogadro's constant, defined as exactly 6.022,140,76 * 10^23 / mol.", "Na", "Avogadro constant", "6.022_140_76e23/mol", FALSE,
    "A quantity representing the Boltzmann constant, defined as exactly 1.380,649 * 10^-23 J / K.", "k", "Boltzmann constant", "1.380_649e-23*(J/K)", FALSE,
    "A quantity representing the molar gas constant, defined as Na * k.", "R", "molar gas constant", "Na*k", FALSE,
    "A quantity representing the Stefan-Boltzmann constant, defined as pi^2 * k^4 / (60 * hbar^3 * c^2).",
    "Stefan_Boltzmann_constant", "Stefan-Boltzmann constant", "pi^2*k^4/(60*hbar^3*c^2)", FALSE,
    "A quantity representing the Rydberg constant, equal to 1.097,373,156,8160 * 10^-7 / m (the CODATA 2018 recommended value).", "Rydberg_constant", "Rydberg constant", "10_973_731.568_160/m", FALSE,
    "The unified atomic mass unit, or dalton, a unit of mass defined as 1/12 the mass of an unbound neutral atom of carbon-12, equal to 1.660,539,066,60 * 10^-27 kg (the CODATA 2018 recommended value).", "u", "UnifiedAtomicMassUnit", "1.660_539_066_60e-27*kg", FALSE,

    # Acceleration
    "The nominal acceleration due to gravity in a vacuum near the surface of the earth, a unit of acceleration, defined by standard to be exactly 9.806,65 m / s^2.", "ge", "EarthGravity", "gn", FALSE,


    # CGS units
    "The gal, a CGS unit of acceleration, defined as 1 cm / s^2.", "Gal", "Gal", "1cm/s^2",  TRUE,
    "The dyne, a CGS unit of force, defined as 1 g * cm / s^2.", "dyn", "Dyne", "1g*cm/s^2", TRUE,
    "The erg, a CGS unit of energy, defined as 1 dyn * cm.", "erg", "Erg", "1g*cm^2/s^2", TRUE,
    "The barye, a CGS unit of pressure, defined as 1 dyn / cm^2.","Ba", "Barye", "1g/cm/s^2", TRUE,
    "The poise, a CGS unit of dynamic viscosity, defined as 1 dyn * s / cm^2.", "P", "Poise", "1g/cm/s", TRUE,
    "The stokes, a CGS unit of kinematic viscosity, defined as 1 cm^2 / s.", "St", "Stokes", "1cm^2/s", TRUE,
    "The gauss, a CGS unit of magnetic B-field strength, defined as 1 Mx / cm^2.", "Gauss", "Gauss", "(1//10_000)*T", TRUE,
    "The oersted, a CGS unit of magnetic H-field strength, defined as 1000 A / (4pi * m).", "Oe", "Oersted", "(1_000/4pi)*A/m", TRUE,
    "The maxwell, a CGS unit of magnetic flux, defined as 1 Gauss * cm^2.", "Mx", "Maxwell", "(1//100_000_000)*Wb", TRUE,


    #########
    # Shared Imperial / US customary units

    # Length
    #key: Symbol    Display    Name                 Equivalent to           10^n prefixes?
    "The inch, a US customary unit of length defined as 2.54 cm.", "inch", "Inch", "(254//10000)*m", FALSE,
    "The mil, a US customary unit of length defined as 1/1000 inch.", "mil", "Mil", "(1//1000)*inch", FALSE,
    "The foot, a US customary unit of length defined as 12 inch.", "ft", "Foot", "12inch",  FALSE,
    "The yard, a US customary unit of length defined as 3 ft.","yd", "Yard", "3ft", FALSE,
    "The mile, a US customary unit of length defined as 1760 yd.", "mi", "Mile", "1760yd", FALSE,
    "The angstrom, a metric unit of length defined as 1/10 nm.", "angstrom", "Angstrom", "(1//10)*nm", FALSE,

    # Area
    "The acre, a US customary unit of area defined as 4840 yd^2","ac", "Acre", "(316160658//78125)*m^2", FALSE,

    # Temperatures
    "The rankine, a US customary unit of temperature defined as 5/9 K.", "Ra", "Rankine", "(5//9)*K", FALSE,
    "The degree Fahrenheit, a US customary unit of temperature, defined such that 0 degrees F = 459.67 Ra.", "degF", "Fahrenheit", "(45967//100)Ra", FALSE,

    # Masses
    "The pound-mass, a US customary unit of mass defined as exactly 0.453,592,37 kg.", "lb", "Pound", "0.45359237kg", FALSE, # is exact
    "The ounce, a US customary unit of mass defined as 1/16 lb.", "oz", "Ounce", "lb//16", FALSE,
    "The slug, a US customary unit of mass defined as 1 lbf * s^2 / ft.", "slug", "Slug", "1lb*ge*s^2/ft", FALSE,
    "The dram, a US customary unit of mass defined as 1/16 oz.", "dr", "Dram", "oz//16", FALSE,
    "The grain, a US customary unit of mass defined as 1/7000 lb.", "gr", "Grain", "(32//875)*dr", FALSE,

    # Force
    "The pound-force, a US customary unit of force defined as 1 lb * ge.", "lbf", "PoundsForce", "1lb*ge", FALSE,

    # Energy
    # Use ISO 31-4 for BTU definition
    "The calorie, a unit of energy defined as exactly 4.184 J.", "cal", "Calorie", "4.184J", TRUE,
    "The British thermal unit, a US customary unit of heat defined by ISO 31-4 as exactly 1055.06 J.", "btu", "BritishThermalUnit", "1055.06J", FALSE,
    "Pounds per square inch, a US customary unit of pressure defined as 1 lbf / inch^2.", "psi", "PoundsPerSquareInch", "1lbf/inch^2", FALSE,

    # Custom units

    "The common year, a unit of time, defined as 365 d.", "common_yr", "Common year", "365d", FALSE,
    "The common quarter, a unit of time, defined as 365/4 d.", "common_quarter", "Common quarter", "365/4d", FALSE,
    "The common month, a unit of time, defined as 365/12 d.", "common_month", "Common month", "365/12d", FALSE,
    "The quarter, a unit of time, defined as 1/4yr.", "quarter", "Quarter", "1/4yr", FALSE,
    "The month, a unit of time, defined as 1/12yr.", "month", "Month", "1/12yr", FALSE,

    "The US fluid ounce, to measure liquids", "fl_oz", "Fluid ounce", "29.5735295625mL", FALSE,
    "The US liquid quart, unit of capacity", "quart", "Quart", "946.35cm^3", FALSE,
    "The tonne, a metric unit of mass", "tonne", "Tonne", "1000kg", FALSE,
    "The US short ton", "ton", "Ton", "907.18474kg", FALSE,
    "The US gallon, a unit of volume", "US_gal", "Gallon", "0.003785411784m^3", FALSE,
    "The atom, used in chemistry to quantify microscopic particles", "atom", "Atom", "1/6.02214076e23mol", FALSE,
    "The molecule, used in scientific models to count chemical entities", "molecule", "Molecule", "1/6.02214076e23mol", FALSE,
    "The euro, the official currency of the Eurozone", "EUR", "Euro", "1", FALSE,
    "The dollar, the currency of the United States", "USD", "Dollar", "1", FALSE,
    "The pound, the currency of the United Kingdom", "GBP", "PoundSterling", "1", FALSE),

    ncol = 5, byrow = TRUE, dimnames = list(NULL, c("description", "name", "full_name", "definition", "prefix")))

  return(units_df)

}


#' Get regular expressions for time units in Julia
#'
#' @returns Named vector with regular expressions as names and units as entries
#'
#' @family units
#' @export
get_regex_time_units = function(){

  # Get units dataframe and only keep time units
  units_df = get_units()
  units_df = units_df[nzchar(units_df[, "full_name"]) & units_df[, "name"] %in% c("s", "minute", "hr", "d", "wk", "month", "quarter", "yr", "common_month", "common_quarter", "common_yr"), ]


  # Create regular expressions
  regex_time_units_julia = sapply(units_df[, "full_name"],
                                  function(x){
                                    paste0("^[", toupper(stringr::str_sub(x, 1, 1)), "|", tolower(stringr::str_sub(x, 1, 1)), "]",
                                           stringr::str_sub(x, 2, nchar(x)), "[s]?$")
                                  })

  # Get named list with regular expressions
  regex_time_units_julia = stats::setNames(units_df[, "name"], unname(regex_time_units_julia))

  # Units that allow for prefixes
  idx = which(units_df[, "prefix"] == "TRUE")

  # Prefixes
  si_prefix_matrix = unit_prefixes()
  si_prefixes = si_prefix_matrix[, "symbol"] %>% stats::setNames(si_prefix_matrix[, "prefix"])


  add_prefixes = lapply(seq_along(regex_time_units_julia[idx]), function(i){
    name = names(regex_time_units_julia[idx])[i]
    x = regex_time_units_julia[idx][i]
    stats::setNames(
      paste0(unname(si_prefixes), unname(x)),
      paste0("^[", toupper(stringr::str_sub(names(si_prefixes), 1, 1)), "|",
             tolower(stringr::str_sub(names(si_prefixes), 1, 1)), "]",

             stringr::str_sub(names(si_prefixes), 2, -1L),
             # Start at index 2 to skip opening ^
             stringr::str_sub(name, 2, -1L)))
  }) %>% unname() %>% unlist()

  regex_time_units_julia = c(regex_time_units_julia, add_prefixes)

  regex_time_units_julia = c(regex_time_units_julia,
                             # Add extras
                             "^[S|s]ec$" = "s",
                             "^[M|m]in$" = "minute",
                             "^[C|c]ommon month[s]?$" = "common_month",
                             "^[C|c]ommon quarter[s]?$" = "common_quarter",
                             "^[C|c]ommon year[s]?$" = "common_yr"
  )

  # Only keep ones with characters in entry and name %>% Filter(nzchar, .)
  regex_time_units_julia = regex_time_units_julia[nzchar(names(regex_time_units_julia)) & nzchar(unname(regex_time_units_julia))]

  return(regex_time_units_julia)

}



#' Show unit prefixes
#'
#' @returns Matrix with unit prefixes, symbols, and power-of-ten scale
#' @family units
#' @export
#'
#' @examples unit_prefixes()
unit_prefixes = function(){
  # Define the SI prefixes, symbols, and scales (with scales as 10^exponent)
  si_prefix_matrix <- matrix(
    c(
      "yotta", "Y", "10^24",
      "zetta", "Z", "10^21",
      "exa", "E", "10^18",
      "peta", "P", "10^15",
      "tera", "T", "10^12",
      "giga", "G", "10^9",
      "mega", "M", "10^6",
      "kilo", "k", "10^3",
      "hecto", "h", "10^2",
      "deka", "da", "10^1",
      "deci", "d", "10^-1",
      "centi", "c", "10^-2",
      "milli", "m", "10^-3",
      "micro", "\\u03BC", "10^-6", # mu
      "nano", "n", "10^-9",
      "pico", "p", "10^-12",
      "femto", "f", "10^-15",
      "atto", "a", "10^-18",
      "zepto", "z", "10^-21",
      "yocto", "y", "10^-24"
    ),
    ncol = 3,
    byrow = TRUE,
    dimnames = list(NULL, c("prefix", "symbol", "scale"))
  )

  return(si_prefix_matrix)
}


#' Get regular expressions for units in Julia
#'
#' @inheritParams build
#' @returns Named vector with regular expressions as names and units as entries
#'
#' @family units
#' @export
get_regex_units = function(sfm = NULL){

  # Get units dataframe
  units_df = get_units()
  units_df = units_df[nzchar(units_df[, "full_name"]), ]

  # Units which should not have a suffix s ([s]?), e.g. inch
  no_s_suffix = c("Hertz", "Siemens", "Henry", "Lux", "Percent",
                  "Permille", "Pertenthousand", "Percentmille", "Permillion", "Perbillion", "Pertrillion", "Perquadrillion", "Celsius",
                  "AngHertz","SpeedOfLight", "magnetic constant", "electric constant","impedance of free space", "gravitational constant", "standard acceleration of gravity", "Planck constant", "Superconducting magnetic flux quantum", "electron rest mass", "neutron rest mass", "proton rest mass", "Bohr magneton", "Avogadro constant", "Boltzmann constant", "molar gas constant", "Stefan-Boltzmann constant","Rydberg constant", "UnifiedAtomicMassUnit", "EarthGravity", "Stokes", "Gauss", "Inch", "Mil", "Foot","Fahrenheit", "PoundsPerSquareInch")

  regex_units = sapply(units_df[, "full_name"],
                       function(x){
                         paste0("^[", toupper(stringr::str_sub(x, 1, 1)), "|", tolower(stringr::str_sub(x, 1, 1)), "]",
                                stringr::str_sub(x, 2, nchar(x)), ifelse(x %in% no_s_suffix, "$", "[s]?$"))
                       })

  # Get named list with regular expressions
  regex_units = stats::setNames(units_df[, "name"], unname(regex_units))

  # Units that allow for prefixes
  idx = which(units_df[, "prefix"] == "TRUE")


  # Prefixes
  si_prefix_matrix = unit_prefixes()
  si_prefixes = stats::setNames(si_prefix_matrix[, "symbol"], si_prefix_matrix[, "prefix"])

  add_prefixes = lapply(seq_along(regex_units[idx]), function(i){
    name = names(regex_units[idx])[i]
    x = regex_units[idx][i]
    stats::setNames(
      paste0(unname(si_prefixes), unname(x)),
      paste0("^[", toupper(stringr::str_sub(names(si_prefixes), 1, 1)), "|",
             tolower(stringr::str_sub(names(si_prefixes), 1, 1)), "]",

             stringr::str_sub(names(si_prefixes), 2, -1L),
             # Start at index 2 to skip opening ^
             stringr::str_sub(name, 2, -1L)))
  }) %>% unname() %>% unlist()

  regex_units = c(regex_units, add_prefixes)

  regex_units = c(regex_units,
                  # Add extras
                  "^feet$" = "ft",
                  "^[S|s]ec$" = "s",
                  "^[M|m]in$" = "minute",
                  "^[C|c]ommon month[s]?$" = "common_month",
                  "^[C|c]ommon quarter[s]?$" = "common_quarter",
                  "^[C|c]ommon year[s]?$" = "common_yr",

                  "^[I|i]nches$" = "inch",
                  "^[M|m]etre[s]?$" = "m",
                  "^[A|a]cre feet$" = "ac",
                  "^[A|a]cre foot$" = "ac",
                  "^[D|d]egree[s]? [K|k]elvin$" = "K",
                  "^[D|d]egree[s]? [F|f]ahrenheit$" = "degF",
                  "^[D|d]egree[s]? [C|c]elsius$" = "degC",

                  "^BTU$" = "btu",
                  "^\\$" = "USD",
                  "^\\u20AC$" = "EUR",
                  "^\\u00A3$" = "GBP",
                  # "^[F|f]luid [ounce[s]?]?$" = "fl_oz",
                  # "^[Q|q]uarts$" = "quarts",
                  # "^[T|t]onne[s]?$" = "tonne",
                  # "^[T|t]on[s]?$" = "ton",
                  # "^[A|a]tom[s]?$" = "atom",
                  # "^[M|m]olecule[s]?$" = "molecule",
                  # "^[E|e]uro[s]?$" = "euro",
                  # "^[D|d]ollar[s]?$" = "dollar",

                  "^[H|h]enries$" = "H",
                  "^[L|l]uxes$" = "lx",
                  "^[P|p]ercentage[s]?$" = "%",
                  "^[U|u]nitless$" = "1",
                  "^[D|d]imensionless$" = "1",
                  "^[D|d]mnl$" = "1",
                  "^[N|n]o[ ]?[U|u]nit[s]?$" = "1"
  )

  # If there are custom units added with power of ten prefixes enabled, add regular expressions
  if (!is.null(sfm)){
    if (length(sfm[["model_units"]]) > 0){
      # Only if there are any units with power-of-ten
      prefix = unlist(lapply(sfm[["model_units"]], `[[`, "prefix"))
      if (any(prefix)){
        unit_names = names(sfm[["model_units"]])
        add_custom_regex = lapply(unit_names[prefix], function(unit_name){

          stats::setNames(
            paste0(unname(si_prefixes), unit_name),
            paste0("^[", toupper(stringr::str_sub(names(si_prefixes), 1, 1)), "|",
                   tolower(stringr::str_sub(names(si_prefixes), 1, 1)), "]",
                   stringr::str_sub(names(si_prefixes), 2, -1L),
                   unit_name)
          )
        }) %>% unlist()
        regex_units = c(regex_units, add_custom_regex)

      }
    }
  }


  # Only keep ones with characters in entry and name %>% Filter(nzchar, .)
  regex_units = regex_units[nzchar(names(regex_units)) & nzchar(unname(regex_units))]

  return(regex_units)

}





#' Get list of standard custom units in Julia
#'
#' @returns List with custom units in Julia
#' @noRd
custom_units = function(){

  # The "month" unit in Insight Maker is 365/12, which is not the same as in the units package, where it is 365.25/12. Add new unit "common_month".


  list("common_yr" = list(name = "common_yr", eqn = "365d", prefix = FALSE),
       "common_quarter" = list(name = "common_quarter", eqn = "365/4*d", prefix = FALSE),
       "common_month" = list(name = "common_month", eqn = "365/12*d", prefix = FALSE),
       "quarter" = list(name = "quarter", eqn = "1/4*yr", prefix = FALSE),
       "month" = list(name = "month", eqn = "1/12*yr", prefix = FALSE),
       "quart" = list(name = "quart", eqn = "946.35cm^3", prefix = FALSE),
       "tonne" = list(name = "tonne", eqn = "1000kg", prefix = FALSE),
       "ton" = list(name = "ton", eqn = "907.18474kg", prefix = FALSE),
       "atom" = list(name = "atom", eqn = "1/6.02214076e23mol", prefix = FALSE),
       "molecule" = list(name = "molecule", eqn = "1/6.02214076e23mol", prefix = FALSE),


       "US_gal" = list(name = "US_gal", eqn = "0.003785411784m^3", prefix = FALSE),
       "fl_oz" = list(name = "fluidOunce", eqn = "29.5735295625mL", prefix = FALSE),
       "EUR" = list(name = "EUR", eqn = "1", prefix = FALSE),
       "USD" = list(name = "USD", eqn = "1", prefix = FALSE),
       "GBP" = list(name = "GBP", eqn = "1", prefix = FALSE),

       # Unicode characters are not accepted in R, so create new units
       # This is necessary, because otherwise these units will be translated to unicode characters for users, which can create problems in R. Alternatively, the units are translated to e.g. \\u00B0, which is not nice for the user.
       # "deg" = list(name = "deg", eqn = "1.0\\u00B0", prefix = FALSE),
       # "Ohm" = list(name = "Ohm", eqn = "1.0\\u2126", prefix = FALSE),
       # "hbar" = list(name = "hbar", eqn = "1.0\\u0127", prefix = FALSE),
       # "superconducting_magnetic_flux_quantum" = list(name = "superconducting_magnetic_flux_quantum", eqn = "1.0\\u03A60", prefix = FALSE),
       # "degF" = list(name = "degF", eqn = "1.0\\u00B0F", prefix = FALSE),
       # "degC" = list(name = "degC", eqn = "1.0\\u00B0C", prefix = FALSE),
       # "Stefan_Boltzmann_constant" = list(name = "Stefan_Boltzmann_constant", eqn = "1.0\\u03C3", prefix = FALSE),
       # "AngHertz" = list(name = "AngHertz", eqn = "1.0Hz2\\u03C0", prefix = FALSE),
       # "magnetic_constant" = list(name = "magnetic_constant", eqn = "1.0\\u03C30", prefix = FALSE),
       # "electric_constant" = list(name = "electric_constant", eqn = "1.0\\u03F50", prefix = FALSE)

       "deg" = list(name = "deg", eqn = "pi/180", prefix = FALSE),

       #    # Use lowercase ohm because Ohm is already taken as an abbreviation and throws an error upon compiling the package

       "ohm" = list(name = "ohm", eqn = "1V/A", prefix = FALSE),
       "reduced_Planck_constant" = list(name = "reduced_Planck_constant", eqn = "h/2pi", prefix = FALSE),
       "superconducting_magnetic_flux_quantum" = list(name = "superconducting_magnetic_flux_quantum", eqn = "h/(2q)", prefix = FALSE),
       "degF" = list(name = "degF", eqn = "(45967//100)Ra", prefix = FALSE),
       "degC" = list(name = "degC", eqn = "(27315//100)K", prefix = FALSE),
       "Stefan_Boltzmann_constant" = list(name = "Stefan_Boltzmann_constant", eqn = "pi^2*k^4/(60*reduced_Planck_constant^3*c^2)", prefix = FALSE),
       "anghertz" = list(name = "anghertz", eqn = "2pi/s", prefix = FALSE),
       "magnetic_constant" = list(name = "magnetic_constant", eqn = "4pi*(1//10)^7*H/m", prefix = FALSE),
       "electric_constant" = list(name = "electric_constant", eqn = "1/(\\u03BC0*c^2)", prefix = FALSE),
       "Bohr_magneton" = list(name = "Bohr_magneton", eqn = "q*reduced_Planck_constant/(2*me)", prefix = FALSE),
       "Rydberg_constant" = list(name = "Rydberg_constant", eqn = "10_973_731.568_160/m", prefix = FALSE)


  ) %>% return()
}
