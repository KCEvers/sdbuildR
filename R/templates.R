#' Template model registry
#'
#' Single source of truth for the model library. Each entry describes one
#' template version: its `name`, semantic `version`, a short `description` (used
#' to auto-generate the `stockflow()` documentation), and a `build` closure that
#' constructs the model. To add a template, add one entry here; the available
#' names, version matching, and documentation all follow automatically.
#'
#' Builders must call `stockflow()` with no `template` argument to avoid
#' recursion.
#'
#' @noRd
template_registry <- function() {
  list(
    list(
      name = "logistic_model",
      version = "1.0.0",
      description = "Population growth with carrying capacity",
      build = function() {
        stockflow() |>
          meta(name = "Logistic model") |>
          sim_settings(stop = 200) |>
          update("X", "stock", eqn = ".01", label = "Population size") |>
          update("births", "flow", eqn = "r * X", to = "X", label = "Births") |>
          update("deaths", "flow", eqn = "r * X^2 / K", from = "X", label = "Deaths") |>
          update("r", "constant", eqn = "0.1", label = "Growth rate") |>
          update("K", "constant", eqn = "1", label = "Carrying capacity")
      }
    ),
    list(
      name = "sir",
      version = "1.0.0",
      description = "Epidemic model (Susceptible-Infected-Recovered)",
      build = function() {
        stockflow() |>
          meta(name = "Susceptible-Infected-Recovered (SIR)") |>
          sim_settings(start = 0, stop = 20, time_units = "weeks") |>
          update("susceptible", "stock", eqn = "99999", label = "Susceptible") |>
          update("infected", "stock", eqn = 1, label = "Infected") |>
          update("recovered", "stock", eqn = 0, label = "Recovered") |>
          update("contact_rate", "constant", eqn = 2, label = "Contact rate") |>
          update("recovery_rate", "constant", eqn = 0.1, label = "Recovery rate") |>
          update("infection_rate", "constant", eqn = "contact_rate / total_population", label = "Infection rate") |>
          update("new_infections", "flow", eqn = "infection_rate * susceptible * infected", from = "susceptible", to = "infected", label = "New infections") |>
          update("new_recoveries", "flow", eqn = "recovery_rate * infected", from = "infected", to = "recovered", label = "New recoveries") |>
          update("total_population", "constant", eqn = "susceptible + infected + recovered", label = "Total population")
      }
    ),
    list(
      name = "predator_prey",
      version = "1.0.0",
      description = "Lotka-Volterra dynamics",
      build = function() {
        stockflow() |>
          meta(name = "Predator-Prey Dynamics (Lotka-Volterra)") |>
          sim_settings(method = "euler", stop = 500) |>
          update("predator", "stock", eqn = 10, label = "Predator") |>
          update("prey", "stock", eqn = 50, label = "Prey") |>
          update("predator_births", "flow",
            eqn = "delta*prey*predator",
            label = "Births", to = "predator"
          ) |>
          update("predator_deaths", "flow",
            eqn = "gamma*predator",
            label = "Deaths", from = "predator"
          ) |>
          update("prey_births", "flow",
            eqn = "alpha*prey",
            label = "Births", to = "prey"
          ) |>
          update("prey_deaths", "flow",
            eqn = "beta*prey*predator",
            label = "Deaths", from = "prey"
          ) |>
          update(c("delta", "gamma", "alpha", "beta"), "constant",
            eqn = c(.025, .5, .5, .05),
            label = c("Delta", "Gamma", "Alpha", "Beta"),
            doc = c(
              "Birth rate of predators", "Death rate of predators",
              "Birth rate of prey", "Death rate of prey by predators"
            )
          )
      }
    ),
    list(
      name = "cusp",
      version = "1.0.0",
      description = "Cusp catastrophe model",
      build = function() {
        stockflow() |>
          meta(name = "Cusp Catastrophe") |>
          sim_settings(method = "euler", stop = 500) |>
          update("x", "stock", eqn = .1) |>
          update("dxdt", "flow",
            eqn = "a + b*x - x^3 + rnorm(1, dt)",
            to = "x"
          ) |>
          update("a", "constant", eqn = 2, label = "Normal variable") |>
          update("b", "constant", eqn = 2, label = "Splitting variable")
      }
    ),
    list(
      name = "crielaard2022",
      version = "1.0.0",
      description = "Eating behavior (doi: 10.1037/met0000484)",
      build = function() {
        stockflow() |>
          meta(
            name = "Eating Behaviour (Crielaard et al., 2022)",
            doi = "10.1037/met0000484"
          ) |>
          sim_settings(time_units = "days", stop = 100) |>
          update("Food_intake", "stock",
            eqn = "runif(1)",
            label = "Food intake"
          ) |>
          update("Hunger", "stock", eqn = "runif(1)") |>
          update("Compensatory_behaviour", "stock",
            eqn = "runif(1)",
            label = "Compensatory behaviour"
          ) |>
          update("Losing_energy_by_compensatory_behavior", "flow",
            eqn = "(0.3 * Compensatory_behaviour) * ((1 - Hunger) / 1)",
            label = "Losing energy by compensatory behavior", to = "Hunger"
          ) |>
          update("Feeling_hunger", "flow",
            eqn = "((0.8 * Hunger^a0) * Food_intake) * ((1 - Food_intake) / 1)",
            label = "Feeling hunger", to = "Food_intake"
          ) |>
          update("Satiety", "flow",
            eqn = "(1.3 * (Food_intake)) * ((1 - Food_intake) / 1)",
            from = "Food_intake"
          ) |>
          update("Food_intake_reduces_hunger", "flow",
            eqn = "((1.4 * Food_intake^a0) * Hunger) * ((1 - Hunger) / 1)", label = "Food intake reduces hunger",
            from = "Hunger"
          ) |>
          update("Compensating_for_having_eaten", "flow",
            eqn = "(Sig(a2 * Food_intake)) * ((1 - Compensatory_behaviour) / 1)",
            label = "Compensating for having eaten",
            to = "Compensatory_behaviour"
          ) |>
          update("Satisfaction_with_hungry_feeling", "flow",
            eqn = "(1.3 * Hunger * Compensatory_behaviour) * ((1 - Compensatory_behaviour) / 1)",
            label = "Satisfaction with hungry feeling", from = "Compensatory_behaviour"
          ) |>
          update("Effect_of_eating_triggers", "flow",
            eqn = "(a1 * Food_intake) * ((1 - Food_intake) / 1)",
            to = "Food_intake", label = "Effect of eating triggers"
          ) |>
          update("Effect_of_compensatory_behavior", "flow",
            eqn = "(2 * Compensatory_behaviour * Food_intake) * ((1 - Food_intake) / 1)",
            from = "Food_intake",
            label = "Effect of compensatory behavior"
          ) |>
          update(c("a0", "a1", "a2"), "constant", eqn = c(1.31, 1.5, 0.38)) |>
          custom_func(name = "Sig", eqn = "function(x) 1 / (1 + exp(1)^(-x))")
      }
    ),
    list(
      name = "coffee_cup",
      version = "1.0.0",
      description = "Temperature equilibration (Meadows)",
      build = function() {
        stockflow() |>
          meta(name = "Coffee cup", caption = "Coffee cup cooling or heating from Meadows' Thinking in Systems (Chapter 1)") |>
          sim_settings(stop = 100, dt = 1, time_units = "minute", language = "Julia") |>
          update("coffee_temperature", "stock", eqn = "100", label = "Coffee temperature") |>
          update("cooling", "flow", eqn = "discrepancy * .1", to = "coffee_temperature", label = "Cooling or heating") |>
          update("discrepancy", "aux", eqn = "room_temperature - coffee_temperature", label = "Discrepancy") |>
          update("room_temperature", "constant", eqn = "18", label = "Room temperature")
      }
    ),
    list(
      name = "bank_account",
      version = "1.0.0",
      description = "Compound interest (Meadows)",
      build = function() {
        stockflow() |>
          meta(
            name = "Bank account with interest",
            caption = "Bank account with compounding interest from Meadows' Thinking in Systems (Chapter 1)"
          ) |>
          sim_settings(start = 0, stop = 12, dt = 1, time_units = "year", language = "Julia") |>
          update("money_in_bank_account", "stock",
            eqn = "100",
            label = "Money in bank account"
          ) |>
          update("interest_added", "flow",
            eqn = "money_in_bank_account * interest_rate",
            label = "Adding interest",
            to = "money_in_bank_account"
          ) |>
          update("interest_rate", "constant",
            eqn = ".02", label = "Interest rate"
          )
      }
    ),
    list(
      name = "lorenz",
      version = "1.0.0",
      description = "Lorenz attractor (chaotic)",
      build = function() {
        stockflow() |>
          meta(
            name = "Lorenz Attractor",
            caption = "Lorenz Attractor system for chaotic dynamics"
          ) |>
          sim_settings(stop = 50, time_units = "hours") |>
          # Stocks
          update("x", "stock", eqn = "1") |>
          update("y", "stock", eqn = "1") |>
          update("z", "stock", eqn = "1") |>
          # Flows (differential equations)
          update("dx_dt", "flow", eqn = "sigma * (y - x)", to = "x", label = "Rate of change of X") |>
          update("dy_dt", "flow", eqn = "x * (rho - z) - y", to = "y", label = "Rate of change of Y") |>
          update("dz_dt", "flow", eqn = "x * y - beta * z", to = "z", label = "Rate of change of Z") |>
          # Parameters
          update("sigma", "constant", eqn = "10") |>
          update("rho", "constant", eqn = "28") |>
          update("beta", "constant", eqn = "8/3")
      }
    ),
    list(
      name = "rossler",
      version = "1.0.0",
      description = "Rossler attractor (chaotic)",
      build = function() {
        stockflow() |>
          meta(
            name = "Rossler Attractor",
            caption = "Chaotic Rossler system in 3D"
          ) |>
          sim_settings(stop = 100, time_units = "hours") |>
          # Stocks
          update("x", "stock", eqn = "1") |>
          update("y", "stock", eqn = "1") |>
          update("z", "stock", eqn = "1") |>
          # Flows
          update("dx_dt", "flow", eqn = "-y - z", to = "x", label = "Rate of change of X") |>
          update("dy_dt", "flow", eqn = "x + a * y", to = "y", label = "Rate of change of Y") |>
          update("dz_dt", "flow", eqn = "b + z * (x - c)", to = "z", label = "Rate of change of Z") |>
          # Parameters
          update("a", "constant", eqn = "0.2") |>
          update("b", "constant", eqn = "0.2") |>
          update("c", "constant", eqn = "5.7")
      }
    ),
    list(
      name = "vanderpol",
      version = "1.0.0",
      description = "Van der Pol oscillator",
      build = function() {
        stockflow() |>
          meta(
            name = "Van der Pol Oscillator",
            caption = "Nonlinear oscillator with limit cycle behavior"
          ) |>
          sim_settings(stop = 50, time_units = "hours") |>
          # Stocks
          update("x", "stock", eqn = "0.1", label = "Position") |>
          update("y", "stock", eqn = "0", label = "Velocity") |>
          # Flows
          update("dx_dt", "flow", eqn = "y", to = "x", label = "Rate of change of position") |>
          update("dy_dt", "flow", eqn = "mu * (1 - x^2) * y - x", to = "y", label = "Rate of change of velocity") |>
          # Parameters
          update("mu", "constant", eqn = "1", label = "Damping parameter")
      }
    ),
    list(
      name = "duffing",
      version = "1.0.0",
      description = "Forced Duffing oscillator",
      build = function() {
        stockflow() |>
          meta(
            name = "Duffing Oscillator",
            caption = "Nonlinear oscillator with forcing"
          ) |>
          sim_settings(stop = 100, time_units = "hours") |>
          # Stocks
          update("x", "stock", eqn = "0.1", label = "Position") |>
          update("y", "stock", eqn = "0", label = "Velocity") |>
          # Flows
          update("dx_dt", "flow",
            eqn = "y", to = "x",
            label = "Rate of change of position"
          ) |>
          update("dy_dt", "flow",
            eqn = "-delta * y - alpha * x - beta * x^3 + gamma * cos(omega * t)",
            to = "y", label = "Rate of change of velocity"
          ) |>
          # Parameters
          update("delta", "constant", eqn = "0.3", label = "Damping coefficient") |>
          update("alpha", "constant", eqn = "-1", label = "Linear stiffness") |>
          update("beta", "constant", eqn = "1", label = "Nonlinear stiffness") |>
          update("gamma", "constant", eqn = "0.5", label = "Forcing amplitude") |>
          update("omega", "constant", eqn = "1.2", label = "Forcing frequency")
      }
    ),
    list(
      name = "chua",
      version = "1.0.0",
      description = "Chua's circuit (chaotic)",
      build = function() {
        stockflow() |>
          meta(name = "Chua's Circuit", caption = "Chaotic electronic circuit model") |>
          sim_settings(stop = 50, time_units = "hours") |>
          # Stocks
          update("x", "stock", eqn = "0.1", label = "Voltage 1") |>
          update("y", "stock", eqn = "0", label = "Voltage 2") |>
          update("z", "stock", eqn = "0", label = "Current") |>
          # Flows
          update("dx_dt", "flow", eqn = "alpha * (y - x - fx)", to = "x", label = "Rate of change of x") |>
          update("dy_dt", "flow", eqn = "x - y + z", to = "y", label = "Rate of change of y") |>
          update("dz_dt", "flow", eqn = "-beta * y", to = "z", label = "Rate of change of z") |>
          update("fx", "aux",
            eqn = "m1 * x + 0.5 * (m0 - m1) * (abs(x + 1) - abs(x - 1))",
            label = "Nonlinear resistor"
          ) |>
          # Parameters
          update("alpha", "constant", eqn = "15.6", label = "Parameter alpha") |>
          update("beta", "constant", eqn = "28", label = "Parameter beta") |>
          update("m0", "constant", eqn = "-1.143", label = "Nonlinear slope m0") |>
          update("m1", "constant", eqn = "-0.714", label = "Nonlinear slope m1")
      }
    ),
    list(
      name = "burnout",
      version = "1.0.0",
      description = "Toy model of burnout as used in Evers et al. (under review)",
      build = function() {
        stockflow() |>
          sim_settings(start = "0.0", stop = round(365/2), dt = "0.01", time_units = "days", only_stocks = FALSE) |>
          meta(name = "Burnout") |>
          stock("energy", eqn = 0.3, label = "Energy") |>
          constant("recovery_rate", eqn = 0.3, label = "Recovery Rate") |>
          flow("recovery", eqn = "recovery_rate", to = "energy", label = "Recovery") |>
          flow("depletion", eqn = "depletion_rate * energy", from = "energy", label = "Depletion") |>
          constant("depletion_rate", eqn = 0.05, label = "Depletion Rate") |>
          aux("net_flow", eqn = "recovery - depletion", label = "Net flow to energy")
      }
    ),
    list(name = "burnout",
      version = "2.0.0",
      description = "Toy model of burnout as used in Evers et al. (under review)",
    build = function() {
      stockflow() |>
        sim_settings(start = "0.0", stop = round(365/2), dt = "0.01", time_units = "days", only_stocks = FALSE) |>
        meta(name = "Burnout") |>
        stock("energy", eqn = 0.3, label = "Energy") |>
        stock("recovery_rate", eqn = 0.3, label = "Recovery Rate") |>
        flow("erosion", eqn = "recovery_rate * depletion", from = "recovery_rate", label = "Erosion") |>
        flow("recovery", eqn = "recovery_rate", to = "energy", label = "Recovery") |>
        flow("depletion", eqn = "depletion_rate * energy", from = "energy", label = "Depletion") |>
        constant("depletion_rate", eqn = 0.05, label = "Depletion Rate") |>
        aux("net_flow", eqn = "recovery - depletion", label = "Net flow to energy")
    }
    ),
    list(
      name = "fitzhugh_nagumo",
      version = "1.0.0",
      description = "Two-variable excitable neuron model",
      urls = c(
        "https://en.wikipedia.org/wiki/FitzHugh%E2%80%93Nagumo_model",
        "https://doi.org/10.1016/S0006-3495(61)86902-6"
      ),
      # walkthrough = c(
      #   "Membrane voltage changes quickly through a cubic activation curve, while recovery changes more slowly.",
      #   "External current can push the system from rest into spikes or repeated oscillations."
      # ),
      # behaviours = c(
      #   "Excitable rest: small perturbations decay, larger perturbations trigger a spike-like excursion.",
      #   "Sustained oscillation when input current is high enough."
      # ),
      # scenarios = c(
      #   "Resting: input_current = 0.2 returns to equilibrium.",
      #   "Spiking: input_current = 0.8 produces repeated excursions.",
      #   "Slower recovery: tau = 20 lengthens the refractory phase."
      # ),
      build = function() {
        stockflow() |>
          meta(name = "FitzHugh-Nagumo", caption = "Excitable neuron model with fast voltage and slow recovery") |>
          sim_settings(start = 0, stop = 120, dt = 0.01, time_units = "ms") |>
          update("voltage", "stock", eqn = "-1", label = "Voltage") |>
          update("recovery", "stock", eqn = "1", label = "Recovery") |>
          update("dvoltage_dt", "flow",
            eqn = "voltage - voltage^3 / 3 - recovery + input_current",
            to = "voltage", label = "Voltage change"
          ) |>
          update("drecovery_dt", "flow",
            eqn = "(voltage + a - b * recovery) / tau",
            to = "recovery", label = "Recovery change"
          ) |>
          update(c("a", "b", "tau", "input_current"), "constant",
            eqn = c("0.7", "0.8", "12.5", "0.5")
          )
      }
    )
    # list(
    #   name = "jdr",
    #   version = "1.0.0",
    #   description = "Job Demands-Resources Theory as formalized in Evers et al. (under review)",
    #   build = function() {
    #     stockflow() |>
    #       sim_settings(start = "0.0", stop = round(182.5), dt = "0.01", seed = "123", time_units = "day", only_stocks = FALSE, save_by = 1) |>
    #       meta(name = "Job Demands and Resources (JD-R) Theory", created = "2026-05-25 10:42:07.289319") |>
    #       stock("demands", eqn = "runif(1, 0.01, 2)", label = "Job Demands") |>
    #       stock("energy", eqn = "runif(1, 0.01, 2)", label = "Energy") |>
    #       stock("engagement", eqn = "runif(1, 0.01, 2)", label = "Work Engagement") |>
    #       stock("resources", eqn = "runif(1, 0.01, 2)", label = "Job Resources") |>
    #       flow("effort", eqn = "effort_rate / (1 + engagement) * energy * demands / (1 + resources)", from = "energy", label = "Effort") |>
    #       flow("engagement_decay", eqn = "engagement_decay_rate * engagement / (1 + energy)", from = "engagement", label = "Decay") |>
    #       flow("exo_demands", eqn = "exo_demand_rate * exp(-s_slope * demands)", to = "demands", label = "Exogenous tasks") |>
    #       flow("exo_resources", eqn = "exo_resource_rate * exp(-s_slope * resources)", to = "resources", label = "Exogenous support") |>
    #       flow("motivation", eqn = "motivation_rate * energy * hill(resources, m_slope) * demands", to = "engagement", label = "Motivation") |>
    #       flow("proactive", eqn = "proactive_rate * hill(engagement, m_slope)", to = "resources", label = "Proactive behaviour") |>
    #       flow("recovery", eqn = "recovery_rate * energy * exp(-s_slope * energy)", to = "energy", label = "Recovery") |>
    #       flow("resource_decay", eqn = "resource_decay_rate * resources / (1 + energy)", from = "resources", label = "Decay") |>
    #       flow("undermining", eqn = "undermining_rate * energy * exp(-e_slope * energy)", to = "demands", label = "Self-undermining") |>
    #       flow("work", eqn = "work_rate * energy * demands * (1 + engagement)", from = "demands", label = "Work") |>
    #       constant("e_slope", eqn = 10, label = "Extreme Slope") |>
    #       constant("effort_rate", eqn = 0.5, label = "Effort Rate") |>
    #       constant("engagement_decay_rate", eqn = 0.2, label = "Engagement Decay Rate") |>
    #       constant("exo_demand_rate", eqn = 0.1, label = "New task rate") |>
    #       constant("exo_resource_rate", eqn = 0.1, label = "New resource rate") |>
    #       constant("m_slope", eqn = 3, label = "Medium Slope") |>
    #       constant("motivation_rate", eqn = 0.3, label = "Motivation Rate") |>
    #       constant("proactive_rate", eqn = 0.2, label = "Proactive Behaviour Rate") |>
    #       constant("recovery_rate", eqn = 0.3, label = "Recovery Rate") |>
    #       constant("resource_decay_rate", eqn = 0.1, label = "Resource Decay Rate") |>
    #       constant("s_slope", eqn = 5, label = "Steep Slope") |>
    #       constant("undermining_rate", eqn = 5, label = "Self-undermining Rate") |>
    #       constant("work_rate", eqn = 0.5, label = "Demand Reduction Rate") |>
    #       aux("performance", eqn = "engagement + energy", label = "Job Performance")
    #   }
    # )
  )
}


#' Validate the version argument for template loading
#'
#' Lenient: accepts `NULL`, a number, a character string, the output of
#' [numeric_version()]/[package_version()], or any single value coercible to a
#' version string. Strips an optional leading `"v"` (e.g. `"v1.0.0"`). Returns a
#' plain version string (or `NULL`), or aborts with a helpful message.
#'
#' @param version Version specification (already resolved from NSE).
#' @param arg_name Name of argument for error messages.
#' @noRd
.validate_version_arg <- function(version, arg_name = "version") {
  if (is.null(version)) {
    return(NULL)
  }
  if (length(version) != 1) {
    cli::cli_abort(c(
      "Invalid {.arg {arg_name}} argument.",
      "x" = "The {.arg {arg_name}} argument must be a single value."
    ))
  }

  version <- sub("^[vV]", "", trimws(as.character(version)))
  ok <- tryCatch(
    {
      numeric_version(version)
      TRUE
    },
    error = function(e) FALSE
  )
  if (!nzchar(version) || !ok) {
    cli::cli_abort(c(
      "Invalid {.arg {arg_name}} argument.",
      "x" = "{.val {version}} is not a valid semantic version (e.g. {.val 1}, {.val 1.2.1})."
    ))
  }

  version
}


#' Match a requested version against available template versions
#'
#' Prefix match using [numeric_version()]: `version = 1` selects the highest
#' `1.x.y`, `version = "1.2"` the highest `1.2.y`, `version = "1.2.1"` exact.
#' Development versions with a 4th component (e.g. `1.0.0.9000`) are matched the
#' same way. Returns the highest matching version string, or aborts.
#'
#' @param version Validated version string.
#' @param available Character vector of available version strings.
#' @param template Template name, for error messages.
#' @noRd
.match_template_version <- function(version, available, template) {
  r <- unclass(numeric_version(version))[[1]]
  ok <- vapply(available, function(x) {
    u <- unclass(numeric_version(x))[[1]]
    length(u) >= length(r) && all(u[seq_along(r)] == r)
  }, logical(1))

  if (!any(ok)) {
    cli::cli_abort(c(
      "x" = "Version '{version}' of template '{template}' not found.",
      "i" = "Available versions: {.val {available}}"
    ))
  }

  as.character(max(numeric_version(available[ok])))
}


#' Generate the `@param template` roxygen block from the registry
#'
#' Used via the roxygen `@eval` tag in [stockflow()] so the documented list of
#' templates is derived automatically from `template_registry()`.
#'
#' @noRd
template_param_doc <- function() {
  reg <- template_registry()
  items <- vapply(reg, function(e) {
    sprintf("  \\item \\strong{%s}: %s (v%s)", e$name, e$description, e$version)
  }, character(1))

  c(
    "@param template Name of the template to load (case-insensitive). If `NULL`,",
    "an empty stock-and-flow model is created with default simulation parameters",
    "and a default meta. If specified, `template` should be one of:",
    "\\itemize{",
    items,
    "}"
  )
}


#' Create stock-and-flow model from model library
#'
#' Create a stock-and-flow model from a template in the model library. The function will return a stock-and-flow model ready to be simulated and plotted, or modified in any way you like.
#'
#' @param template Name of model.
#' @param version Optional resolved version string (or `NULL` for the latest).
#'
#' @noRd
#' @returns Stock-and-flow model of class [`stockflow`][stockflow], or, when
#'   `template` is missing, a character vector of available template names.
#'
templates <- function(template, version = NULL) {
  reg <- template_registry()
  model_names <- unique(vapply(reg, function(e) e$name, character(1)))

  # List template names for internal use
  if (missing(template)) {
    return(model_names)
  }

  if (!is.character(template)) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg template} argument.",
      "i" = "The {.arg template} argument must be {.cls character}."
    ))
  }

  if (length(template) != 1) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg template} length.",
      "i" = "The {.arg template} argument must be a single {.cls character} string."
    ))
  }

  # Case-insensitive template matching
  template <- tolower(template)

  if (!template %in% model_names) {
    cli::cli_abort(c(
      "x" = "Template '{template}' not found.",
      "i" = "Available templates: {.val {model_names}}"
    ))
  }

  version <- .validate_version_arg(version)

  entries <- Filter(function(e) e$name == template, reg)
  available <- vapply(entries, function(e) e$version, character(1))
  chosen <- if (is.null(version)) {
    as.character(max(numeric_version(available)))
  } else {
    .match_template_version(version, available, template)
  }

  entry <- entries[[which(available == chosen)[1]]]
  object <- entry$build()
  object <- meta(object, version = chosen)

  object
}
