test_that("downloading and simulating Insight Maker models works", {
  # Requires internet
  testthat::skip_on_cran()

  URL <- "https:"

  expect_error(
    insightmaker_to_sfm(),
    "Either URL or file needs to be specified"
  )

  expect_error(
    insightmaker_to_sfm(URL = URL),
    "This is not a URL to an Insight Maker model"
  )

  expect_error(
    insightmaker_to_sfm(file = "test.InsightMaker"),
    "Your file refers to a file that does not exist"
  )

  expect_error(
    insightmaker_to_sfm(file = "test.InsightMaker2"),
    "Your file does not have the file extension \\.InsightMaker"
  )

  expect_error(
    insightmaker_to_sfm(URL = URL, file = ""),
    "Either URL or file needs to be specified, not both"
  )

  sfm_list <- list()

  testthat::skip_if_not(JuliaConnectoR::juliaSetupOk())

  URL <- "https://insightmaker.com/insight/3xgsvC7QKgPktHWZuXyGAl/Clone-of-Global-Climate-Change"
  sfm_list[[1]] <- sfm <- expect_no_error(insightmaker_to_sfm(URL = URL))
  df <- expect_no_error(as.data.frame(sfm))
  expect_equal(nrow(df) > 0, TRUE)
  expect_equal("macro" %in% df$type, TRUE)
  expect_equal("model_units" %in% df$type, TRUE)

  # Contains graphical functions; check whether xpts and ypts were concatenated
  expect_equal("xpts" %in% names(df), TRUE)
  expect_equal("ypts" %in% names(df), TRUE)

  expect_error(
    simulate(sfm |> sim_specs(language = "R")),
    "The model contains unit strings u\\(''\\), which are not supported for simulations in R"
  )

  URL <- "https://insightmaker.com/insight/5LxQr0waZGgBcPJcNTC029/Crielaard-et-al-2022"
  sfm_list[[2]] <- sfm <- expect_no_error(insightmaker_to_sfm(URL = URL))
  df <- expect_no_error(as.data.frame(sfm))
  expect_equal(sfm[["model_units"]], list())
  expect_equal(nrow(df) > 0, TRUE)
  expect_equal("macro" %in% df$type, TRUE)

  sim <- expect_no_error(simulate(sfm |> sim_specs(
    language = "R", start = 0,
    dt = 0.1, stop = 10
  )))
  expect_equal(sim$success, TRUE)
  expect_equal(nrow(sim$df) > 0, TRUE)

  URL <- "https://insightmaker.com/insight/75PvtT7zp43wI7ofBOM9Sm/Clone-of-HYSTERESIS"
  sfm_list[[3]] <- sfm <- expect_no_error(insightmaker_to_sfm(URL = URL))
  df <- expect_no_error(as.data.frame(sfm))
  expect_equal(nrow(df) > 0, TRUE)
  expect_equal("macro" %in% df$type, TRUE)

  # This model uses unit strings u(''), which are not supported in R
  expect_error(
    simulate(sfm |> sim_specs(language = "R")),
    "The model contains unit strings u\\(''\\), which are not supported for simulations in R"
  )

  lapply(sfm_list, function(sfm) {
    # For some models with units, save_at and save_from create error
    sim <- expect_no_error(simulate(sfm |> sim_specs(
      language = "Julia",
      # dt = 0.1, start = 0,
      # stop = 10
    )))
    expect_equal(sim$success, TRUE)
    expect_equal(nrow(sim$df) > 0, TRUE)
  })
})


test_that("translating Insight Maker models works (cran)", {
  # Get path to the cran folder
  folder <- test_path("testdata", "insightmaker", "cran")

  # Get all .InsightMaker files in the folder
  model_files <- list.files(
    path = folder,
    pattern = "\\.InsightMaker$",
    full.names = TRUE
  )

  for (file in model_files) {
    # print(basename(file))
    sfm <- expect_no_error({
      # Suppress potential warnings about old Insight Maker version
      # or large dt
      suppressWarnings({
        insightmaker_to_sfm(
          file = file,
          keep_nonnegative_flow = TRUE,
          keep_nonnegative_stock = TRUE,
          keep_solver = TRUE
        )
      })
    })

    df <- expect_no_error(as.data.frame(sfm))
    expect_equal(nrow(df) > 0, TRUE)
    expect_true(all(
      c(
        "eqn",
        "eqn_insightmaker",
        "eqn_julia",
        "name_insightmaker",
        "units_insightmaker",
        "id_insightmaker"
      ) %in% names(df)
    ))
    expect_no_error(expect_no_warning(expect_no_message(plot(sfm))))
    sim <- expect_no_error(simulate(sfm |> sim_specs(dt = 0.01, save_at = 1),
      only_stocks = FALSE
    ))
    expect_equal(sim$success, TRUE)
    expect_equal(nrow(sim$df) > 0, TRUE)
    expect_no_error(expect_no_warning(expect_no_message(plot(sim))))
  }
})



test_that("translating Insight Maker models works (validation)", {
  skip_on_cran()
  testthat::skip_if_not(JuliaConnectoR::juliaSetupOk())

  # Get path to the cran folder
  folder <- test_path("testdata", "insightmaker", "validation")

  # print("folder")
  # print(folder)
  # print(list.files(path = test_path("testdata", "insightmaker"), include.dirs = TRUE))

  skip_if_not(dir.exists(folder), "Validation test files not available")

  # Get all .InsightMaker files in the folder
  model_files <- list.files(
    path = folder,
    pattern = "\\.InsightMaker$",
    full.names = TRUE
  )

  use_julia()
  for (file in model_files) {
    # print(basename(file))
    sfm <- expect_no_error({
      # Suppress potential warnings about old Insight Maker version
      # or large dt
      suppressWarnings({
        insightmaker_to_sfm(
          file = file,
          keep_nonnegative_flow = TRUE,
          keep_nonnegative_stock = TRUE,
          keep_solver = TRUE
        )
      })
    })

    df <- expect_no_error(as.data.frame(sfm))
    expect_equal(nrow(df) > 0, TRUE)
    expect_true(all(
      c(
        "eqn",
        "eqn_insightmaker",
        "eqn_julia",
        "name_insightmaker",
        "units_insightmaker",
        "id_insightmaker"
      ) %in% names(df)
    ))
    expect_no_error(expect_no_warning(expect_no_message(plot(sfm))))
    sim <- expect_no_error(simulate(sfm |> sim_specs(dt = 0.01, save_at = 1),
      only_stocks = FALSE
    ))
    expect_equal(sim$success, TRUE)
    expect_equal(nrow(sim$df) > 0, TRUE)
    expect_no_error(expect_no_warning(expect_no_message(plot(sim))))
  }
})


#   # model_list = c(
#   #   # 'A_Business_Model', # unit # delay
#   #                'A_Simple_National_Income_Macroeconomic_Model_Continuous_Time',
#   #                'Accidental_Adversaries',
#   #                'Addiction_Cycle_System',
#   #                'Adding_Agriculture',
#   #                'Aggression',
#   #                'Antisocial_behavior_and_aversive_responses',
#   #                'Asteroid_impact_simulator',
#   #                'Attractiveness_Principle',
#   #                # 'Automobile_Leasing_Strategy', # delay
#   #                'Balancing_an_Inverted_Pendulum',
#   #                'Basic_Model_Tyson_Lynx_and_Hare',
#   #                'basic_warehouse_stock_control',
#   #                'Bio103_Growth_Models',
#   #                'Bipolar_II_dynamics',
#   #                'Bossel_Z110_Logistic_growth_with_stock_dependent_harvest',
#   #                'Bossel_Z202_Van_der_Pol_Oscillator',
#   #                'Bossel_Z203_Brusselator',
#   #                'Bossel_Z301_Regional_Water_Balance',
#   #                'Buffet_Tragedy_of_the_Commons_application',
#   #                'Burnout_Dynamics',
#   #                'C_N_bacteria_DOM',
#   #                'Chaotic_Bistable_Oscillator',
#   #                'Clinical_Process_Overview',
#   #                'Clone_of_House_Heating_Dynamics',
#   #                'Clone_of_Z602_Population_with_four_age_groups',
#   #                'Competition_for_Resources',
#   #                'Crielaard_2022',
#   #                'D_model_curve_di_Richards_con_ln_alpha_lag_mu',
#   #                'Dependence',
#   #                'Diffusion_of_Innovation_Bass_Model',
#   #                # 'Diffusion_of_Medical_Technology', # delay
#   #                'Double_Loop_Control_Theory_by_William_T_Powers',
#   #                'Drifting_Goals',
#   #                'E_coli_life_cycle_model',
#   #                'ED_Weekend_Flows_Senior_Roster',
#   #                'Engineeing_Effectiveness_vs_Technical_Debt',
#   #                'Escalation',
#   #                'Fall_of_a_balloon_in_air',
#   #                'Fall_with_drag_force',
#   #                'Fern_Population_Model',
#   #                'Fixes_That_Fail',
#   #                'Global_Climate_Change',
#   #                'Glucose_Regulation_Diabetes_Simulation',
#   #                'Goodwin_Business_Cycle',
#   #                'Goodwin_Model',
#   #                'Growth_and_Underinvestment',
#   #                'honeybee_hive_population_model',
#   #                # 'Hyperinflation_Simulation', # delay
#   #                'HYSTERESIS',
#   #                'I_O_psychology_internship',
#   #                'Influence_of_Surface_Temperature_on_Albedo_and_Greenhouse_Effect',
#   #                # 'Inventory_Simulation', # delay
#   #                'Kepler_Ellipsen',
#   #                'Limits_to_Action_Archetype',
#   #                # 'Littles_Law', # delay
#   #                'Lorenz_Attractor',
#   #                'MGMT_S_5012_Shifting_the_Burden_Archetype',
#   #                'Milgram_Experiment',
#   #                'Minsky_Financial_Instability_Model',
#   #                # 'My_Insight', # error ** to do, add robustness pulse ramp step to start/finish after/before times, also in Julia
#   #                'Outpatient_Clinics_Patient_Flow',
#   #                'OVERSHOOT_GROWTH_INTO_TURBULENCE',
#   #                # 'Pesticide_Use_in_Central_America_Model', # delay
#   #                'PHY201_Lab_2_Projectile_with_Air_Drag_V2',
#   #                'Policy_Horizon_Model',
#   #                'Population_w_Agriculture',
#   #                'REM_221_Z301_Regional_Water_Balance',
#   #                'REM_221_Z404_Prey_and_two_Predator_Populations',
#   #                'REM_221_Z409_Fishery_dynamics',
#   #                'REM_221_Z412_Tourism_Dynamics',
#   #                'Romeo_Juliet',
#   #                'Rotating_Pendulum',
#   #                'SEIRD_01_COVID_19_spread',
#   #                'Simple_harmonic_oscillator_with_damping',
#   #                # 'SIR_model_with_stochastic_events', # error
#   #                'Source_Sink_2',
#   #                'Spring_and_fall_bloom',
#   #                'Spring_Mass_Model',
#   #                'Stanford_Prison_Experiment',
#   #                'Subtropical_forest_succession',
#   #                'Success_to_the_Successful',
#   #                'Sucking_thumb_Limits_to_Growth_application',
#   #                'Sustainable_Ecotourism',
#   #                'System_Zoo_409',
#   #                'System_Zoo_Z104_Exponential_delay',
#   #                'System_Zoo_Z105_Time_dependent_growth',
#   #                'System_Zoo_Z109_ex_6_Whale_population',
#   #                'System_Zoo_Z415_Resource_Extraction_and_Recycling',
#   #                'Technology_and_Healthcare_Costs_and_Outcomes_3',
#   #                # 'Technology_Learning_Curve', # delay
#   #                'THE_BUTTERFLY_EFFECT',
#   #                'The_Ecology_of_Medical_Care',
#   #                # 'The_effect_of_Supply_and_Demand_on_the_Housing_Market_Assignment_3_43323871', # delay
#   #                'The_Logistic_Map',
#   #                # 'The_probability_density_function_PDF_of_the_normal_distribution_or_Bell_Curve_Gaussian_Distribution_by_Guy_Lakeman', # delay
#   #                'The_Rossler_Chaotic_Attractor',
#   #                'The_Science_of_Inequality',
#   #                'The_SIC_Susceptible_Infected_Concern_Model',
#   #                'Tragedy_of_the_Commons',
#   #                'Urine_flow_rate',
#   #                'Using_Systems_thinking_for_technology_in_education',
#   #                'Very_Simple_Ecosystem_Model_with_Evapotranspiration_VSEM_ET',
#   #                'Viral_Growth',
#   #                'Wind_Resistance_Model',
#   #                'Wolves_Rabbits_Carrots_Ecosystem',
#   #                'YellowstoneEcoClassModel_ISD_OWL',
#   #                'Z308_Forest_dynamics',
#   #                'Z504_Market_and_Price_System_Zoo_3',
#   #                'Z605_Miniworld'
#   #                )
