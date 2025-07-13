
#' Create stock-and-flow model from model library
#'
#' Create a stock-and-flow model from a template in the model library. The function will return a stock-and-flow model ready to be simulated and plotted, or modified in any way you like.
#'
#' @param name Name of model; one of "logistic_model", "SIR", "predator-prey", "Crielaard2022", "coffee_cup", "bank_account", "Lorenz", "Rossler", "vanderPol", "Duffing", "Chua", "spruce_budworm"
#'
#' @noRd
#' @return Stock-and-flow model of class sdbuildR_xmile.
#'
template = function(name){

  model_names = c("logistic_model", "SIR", "predator-prey", "Crielaard2022",
                  # Meadows
                  "coffee_cup", "bank_account",
                  "Lorenz", "Rossler", "vanderPol", "Duffing", "Chua",
                  "spruce_budworm")

  if (missing(name)){
    message(sprintf("Choose one of the available templates:\n\n%s", paste0(model_names, collapse = "\n")))
    return(invisible())
  }

  if (!is.character(name)){
    stop("name must be a string!")
  }

  if (length(name) != 1){
    stop("name must be of length 1!")
  }

  if (!name %in% model_names){
    stop(sprintf("%s is not an available template. The available templates are:\n\n%s", name,
                 paste0(model_names, collapse = "\n")))
  }


  if (name == "logistic_model"){

    sfm = xmile() %>% header(name = "Logistic model") %>%
      sim_specs(stop = 200) %>%
      build("X", "stock", eqn = ".01", label = "Population size") %>%
      # build("flow", "flow", eqn = "r * X * (1 - X / K)", to = "X", label = "Net change") %>%
      build("inflow", "flow", eqn = "r * X", to = "X", label = "Births") %>%
      build("outflow", "flow", eqn = "r * X^2 / K", from = "X", label = "Deaths") %>%
      build("r", "constant", eqn = "0.1", label = "Growth rate") %>%
      build("K", "constant", eqn = "1", label = "Carrying capacity")

  } else if (name == "SIR"){
    # Chapter 5 Duggan: SIR Aggregate.R & Chapter 7. Screening.R
    # https://github.com/JimDuggan/SDMR/blob/master/models/05%20Chapter/R/01%20SIR%20Aggregate.R
    # https://github.com/JimDuggan/SDMR/blob/master/models/07%20Chapter/R/02%20Screening.R

    sfm = xmile() %>% header(name = "Susceptible-Infected-Recovered (SIR)") %>%
      sim_specs(start = 0, stop = 20) %>%
      build("Susceptible", "stock", eqn = "99999") %>%
      build("Infected", "stock", eqn = "1") %>%
      build("Recovered", "stock", eqn = "0.0") %>%
      build("Beta", "aux", eqn = "Effective_Contact_Rate / Total_Population") %>%
      build("Lambda", "aux", eqn = "Beta * Infected") %>%
      build("Infection_Rate", "flow", eqn = "Susceptible * Lambda", from = "Susceptible", to = "Infected") %>%
      build("Recovery_Rate", "flow", eqn = "Infected / Delay", from = "Infected", to = "Recovered") %>%
      build("Total_Population", "constant", eqn = "100000") %>%
      build("Effective_Contact_Rate", "constant", eqn = "2") %>%
      build("Delay", "constant", eqn = "2")

  } else if (name == "predator-prey"){

    sfm = xmile() %>% header(name = "Predator-Prey Dynamics (Lotka-Volterra)") %>%
      sim_specs(method = "euler", stop = 500) %>%
      build("predator", "stock", eqn = 10, label = "Predator") %>%
      build("prey", "stock", eqn = 50, label = "Prey") %>%
      build("predator_births", "flow", eqn = "delta*prey*predator",
            label = "Births", to = "predator") %>%
      build("predator_deaths", "flow", eqn = "gamma*predator",
            label = "Deaths", from = "predator") %>%
      build("prey_births", "flow", eqn = "alpha*prey",
            label = "Births", to = "prey") %>%
      build("prey_deaths", "flow", eqn = "beta*prey*predator",
            label = "Deaths", from = "prey") %>%
      build(c("delta", "gamma", "alpha", "beta"), "constant",
            eqn = c(.025, .5, .5, .05),
            label = c("Delta", "Gamma", "Alpha", "Beta"),
            doc = c("Birth rate of predators", "Death rate of predators",
                    "Birth rate of prey", "Death rate of prey by predators"))

  } else if (name == "Crielaard2022"){

    sfm = xmile() %>% header(name = "Eating Behaviour (Crielaard et al., 2022)",
                doi = "10.1037/met0000484") %>%
      sim_specs(time_units = "days", stop = 100) %>%
      build("Food_intake", "stock", eqn = "runif(1)",
            label = "Food intake") %>%
      build("Hunger", "stock", eqn = "runif(1)") %>%
      build("Compensatory_behaviour", "stock", eqn = "runif(1)",
            label = "Compensatory behaviour") %>%

      build("Losing_energy_by_compensatory_behavior", "flow",
            eqn = "(0.3 * Compensatory_behaviour) * ((1 - Hunger) / 1)",
            label = "Losing energy by compensatory behavior", to = "Hunger") %>%
      build("Feeling_hunger", "flow",
            eqn = "((0.8 * Hunger^a0) * Food_intake) * ((1 - Food_intake) / 1)",
            label = "Feeling hunger", to = "Food_intake") %>%
      build("Satiety", "flow",
            eqn = "(1.3 * (Food_intake)) * ((1 - Food_intake) / 1)",
            from = "Food_intake") %>%
      build("Food_intake_reduces_hunger", "flow", eqn = "((1.4 * Food_intake^a0) * Hunger) * ((1 - Hunger) / 1)", label = "Food intake reduces hunger",
            from = "Hunger") %>%
      build("Compensating_for_having_eaten", "flow",
            eqn = "(Sig(a2 * Food_intake)) * ((1 - Compensatory_behaviour) / 1)",
            label = "Compensating for having eaten",
            to = "Compensatory_behaviour") %>%
      build("Satisfaction_with_hungry_feeling", "flow",
            eqn = "(1.3 * Hunger * Compensatory_behaviour) * ((1 - Compensatory_behaviour) / 1)",
            label = "Satisfaction with hungry feeling", from = "Compensatory_behaviour") %>%
      build("Effect_of_eating_triggers", "flow",
            eqn = "(a1 * Food_intake) * ((1 - Food_intake) / 1)",
            to = "Food_intake", label = "Effect of eating triggers") %>%
      build("Effect_of_compensatory_behavior", "flow",
            eqn = "(2 * Compensatory_behaviour * Food_intake) * ((1 - Food_intake) / 1)",
            from = "Food_intake",
            label = "Effect of compensatory behavior") %>%
      build(c("a0", "a1", "a2"), "constant", eqn = c(1.31, 1.5, 0.38)) %>%
      macro(name = "Sig", eqn = "function(x) 1 / (1 + exp(1)^(-x))")

  } else if (name == "coffee_cup"){

    sfm = xmile() %>% header(name = "Coffee cup", caption = "Coffee cup cooling or heating from Meadows' Thinking in Systems (Chapter 1)") %>%
      sim_specs(stop = 100, dt = 1, time_units = "minute") %>%
      build("coffee_temperature", "stock", eqn = "100", units = "Celsius", label = "Coffee temperature") %>%
      build("cooling", "flow", eqn = "discrepancy * .1 / u('min')", units = "Celsius/min", to = "coffee_temperature", label = "Cooling or heating") %>%
      build("discrepancy", "aux", eqn = "room_temperature - coffee_temperature", units = "Celsius", label = "Discrepancy") %>%
      build("room_temperature", "constant", eqn = "18", units = "Celsius", label = "Room temperature")

  } else if (name == "bank_account"){

    sfm = xmile() %>% header(name = "Bank account with interest", caption = "Bank account with compounding interest from Meadows' Thinking in Systems (Chapter 1)") %>%
      sim_specs(start = 0, stop = 12, dt = 1, time_units = "year") %>%
      build("money_in_bank_account", "stock", eqn = "100",
            label = "Money in bank account", units = "dollar") %>%
      build("interest_added", "flow",
            eqn = "money_in_bank_account * interest_rate / u('1year')",
            label = "Adding interest",
            units = "dollar/year", to = "money_in_bank_account") %>%
      build("interest_rate", "constant", eqn = ".02", label = "Interest rate",
            units = "1")

  } else if (name == "Lorenz"){

    sfm = xmile() %>% header(name = "Lorenz Attractor",
                caption = "Lorenz Attractor system for chaotic dynamics") %>%
      sim_specs(stop = 50, time_units = "hours") %>%
      # Stocks
      build("x", "stock", eqn = "1") %>%
      build("y", "stock", eqn = "1") %>%
      build("z", "stock", eqn = "1") %>%
      # Flows (differential equations)
      build("dx_dt", "flow", eqn = "sigma * (y - x)", to = "x", label = "Rate of change of X") %>%
      build("dy_dt", "flow", eqn = "x * (rho - z) - y", to = "y", label = "Rate of change of Y") %>%
      build("dz_dt", "flow", eqn = "x * y - beta * z",  to = "z", label = "Rate of change of Z") %>%
      # Parameters
      build("sigma", "constant", eqn = "10") %>%
      build("rho", "constant", eqn = "28") %>%
      build("beta", "constant", eqn = "8/3")

  } else if (name == "Rossler"){

  sfm = xmile() %>% header(name = "Rossler Attractor",
              caption = "Chaotic Rossler system in 3D") %>%
    sim_specs(stop = 100, time_units = "hours") %>%
    # Stocks
    build("x", "stock", eqn = "1") %>%
    build("y", "stock", eqn = "1") %>%
    build("z", "stock", eqn = "1") %>%
    # Flows
    build("dx_dt", "flow", eqn = "-y - z", to = "x", label = "Rate of change of X") %>%
    build("dy_dt", "flow", eqn = "x + a * y", to = "y", label = "Rate of change of Y") %>%
    build("dz_dt", "flow", eqn = "b + z * (x - c)", to = "z", label = "Rate of change of Z") %>%
    # Parameters
    build("a", "constant", eqn = "0.2") %>%
    build("b", "constant", eqn = "0.2") %>%
    build("c", "constant", eqn = "5.7")

  } else if (name == "vanderPol"){

    sfm = xmile() %>% header(name = "Van der Pol Oscillator",
                caption = "Nonlinear oscillator with limit cycle behavior") %>%
      sim_specs(stop = 50, time_units = "hours") %>%
      # Stocks
      build("x", "stock", eqn = "0.1", label = "Position") %>%
      build("y", "stock", eqn = "0", label = "Velocity") %>%
      # Flows
      build("dx_dt", "flow", eqn = "y", to = "x", label = "Rate of change of position") %>%
      build("dy_dt", "flow", eqn = "mu * (1 - x^2) * y - x", to = "y", label = "Rate of change of velocity") %>%
      # Parameters
      build("mu", "constant", eqn = "1",label = "Damping parameter")

  } else if (name == "Duffing"){

    # **cos() throws error with units
    sfm = xmile() %>% header(name = "Duffing Oscillator",
                caption = "Nonlinear oscillator with forcing") %>%
      sim_specs(stop = 100, time_units = "hours") %>%
      # Stocks
      build("x", "stock", eqn = "0.1", label = "Position") %>%
      build("y", "stock", eqn = "0", label = "Velocity") %>%
      # Flows
      build("dx_dt", "flow", eqn = "y", to = "x",
            label = "Rate of change of position") %>%
      build("dy_dt", "flow", eqn = "-delta * y - alpha * x - beta * x^3 + gamma * cos(omega * t)",
           to = "y", label = "Rate of change of velocity") %>%
      # Parameters
      build("delta", "constant", eqn = "0.3", label = "Damping coefficient") %>%
      build("alpha", "constant", eqn = "-1", label = "Linear stiffness") %>%
      build("beta", "constant", eqn = "1", label = "Nonlinear stiffness") %>%
      build("gamma", "constant", eqn = "0.5", label = "Forcing amplitude") %>%
      build("omega", "constant", eqn = "1.2",  label = "Forcing frequency")

  } else if (name == "Chua"){

    sfm = xmile() %>% header(name = "Chua's Circuit", caption = "Chaotic electronic circuit model") %>%
      sim_specs(stop = 50, time_units = "hours") %>%
      # Stocks
      build("x", "stock", eqn = "0.1", label = "Voltage 1") %>%
      build("y", "stock", eqn = "0", label = "Voltage 2") %>%
      build("z", "stock", eqn = "0", label = "Current") %>%
      # Flows
      build("dx_dt", "flow", eqn = "alpha * (y - x - fx)", to = "x", label = "Rate of change of x") %>%
      build("dy_dt", "flow", eqn = "x - y + z", to = "y", label = "Rate of change of y") %>%
      build("dz_dt", "flow", eqn = "-beta * y", to = "z", label = "Rate of change of z") %>%
      build("fx", "aux", eqn = "m1 * x + 0.5 * (m0 - m1) * (abs(x + 1) - abs(x - 1))",
            label = "Nonlinear resistor") %>%
      # Parameters
      build("alpha", "constant", eqn = "15.6", label = "Parameter alpha") %>%
      build("beta", "constant", eqn = "28", label = "Parameter beta") %>%
      build("m0", "constant", eqn = "-1.143", label = "Nonlinear slope m0") %>%
      build("m1", "constant", eqn = "-0.714", label = "Nonlinear slope m1")

  } else if (name == "spruce_budworm"){

    sfm = xmile() %>%
      build("N", "stock", eqn = .5) %>%
      build("inflow", "flow", eqn = "r * N * (1 - N/K)", to = "N") %>%
      build("outflow", "flow", eqn = "(B * N^2) / (A^2 + N^2)", from = "N") %>%
      build(c("r", "K", "B", "A"), "constant", eqn = c(.5, 1000, 10000, 1))

  }


  return(sfm)
}

