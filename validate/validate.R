library(dplyr)
library(insightmakeR1)
library(foreach)
library(doParallel)

# Validate package sdbuildR
directory = "C:/Users/kevers1/Documents/PhD/insightmakeR1/validate/models"

# Read all URLs and write .InsightMaker file
URLs = c(
  # Han van der Maas: Romeo & Juliet
  "https://insightmaker.com/insight/4ZFHRxfRKTCVBRbGvglBqG/Romeo-Juliet",
  # Geoff McDonnell: Chaotic Bistable Oscillator
  "https://insightmaker.com/insight/1Oa8oRxADhfd5McGN3wbf8/Chaotic-Bistable-Oscillator",
  # # Andrew E Long: Basic Model, Tyson Lynx and Hare
  "https://insightmaker.com/insight/6v9JyFsseEvLKRzXS4VEWC/Basic-Model-Tyson-Lynx-and-Hare", # reproduced if keep_nonnegative_flow = T; different result if flows are allowed to become negative
  # Pau Fonseca:  SEIRD 01 COVID-19 spread
  "https://insightmaker.com/insight/2nPWRjZGqerpxMtCvVAtmW/SEIRD-01-COVID-19-spread",
  # Steve Conrad: REM 221 - Z412 Tourism Dynamics
  "https://insightmaker.com/insight/7kYUgStCpb4tDvfxo0f31q/REM-221-Z412-Tourism-Dynamics", # clone and re-add variables to plot
  # Steve Conrad: REM 221 - Z404 Prey and two Predator Populations
  "https://insightmaker.com/insight/2g49A0QKGKjFxTXPsyy9Tb/REM-221-Z404-Prey-and-two-Predator-Populations", # incorrect reproduction
  # Andrew E Long: The Rossler Chaotic Attractor
  "https://insightmaker.com/insight/2hsC7OKMcuOt7tj2RfW0SL/The-Rossler-Chaotic-Attractor",
  # Henny van Dongen: System Zoo Z415 Resource Extraction and Recycling
  "https://insightmaker.com/insight/3nI10pemqbZyVulGuR0c1O/System-Zoo-Z415-Resource-Extraction-and-Recycling",
  # Muhammad Ali: Clone of ENV221 - Z418 - Sustainable Use of a renewable resource
  # "https://insightmaker.com/insight/01S7O7p7skEABhpukBkTfp/Clone-of-ENV221-Z418-Sustainable-Use-of-a-renewable-resource", # doesn't work even for simulation package
  # Lisa Belyea; Very Simple Ecosystem Model with Evapotranspiration (VSEM-ET)
  "https://insightmaker.com/insight/6zZF5JHLkENegrNgvhXaB2/Very-Simple-Ecosystem-Model-with-Evapotranspiration-VSEM-ET", # not reproduced because of dgits in pi -> seasonal function
  # R          java          diff
  # [1,]  1.000000e+00  1.000000e+00  0.000000e+00
  # [2,]  9.510565e-01  9.510565e-01 -2.322043e-11
  # [3,]  8.090170e-01  8.090170e-01 -4.416834e-11
  # [4,]  5.877853e-01  5.877853e-01 -6.079215e-11
  # [5,]  3.090170e-01  3.090170e-01 -7.146528e-11
  # [6,]  1.210202e-13  7.526413e-11 -7.514311e-11
  # [7,] -3.090170e-01 -3.090170e-01 -7.146572e-11
  # [8,] -5.877853e-01 -5.877853e-01 -6.060807e-11
  # [9,] -8.090170e-01 -8.090170e-01 -4.416800e-11
  # [10,] -9.510565e-01 -9.510565e-01 -2.314982e-11
  # [11,] -1.000000e+00 -1.000000e+00  0.000000e+00
  # [12,] -9.510565e-01 -9.510565e-01  2.315026e-11
  # [13,] -8.090170e-01 -8.090170e-01  4.416789e-11
  # [14,] -5.877853e-01 -5.877853e-01  6.060830e-11
  # [15,] -3.090170e-01 -3.090170e-01  7.146517e-11
  # [16,] -1.811401e-14 -7.493385e-11  7.491574e-11
  # [17,]  3.090170e-01  3.090170e-01  7.124934e-11
  # [18,]  5.877853e-01  5.877853e-01  6.060841e-11
  # [19,]  8.090170e-01  8.090170e-01  4.403478e-11
  # [20,]  9.510565e-01  9.510565e-01  2.315059e-11
  # [21,]  1.000000e+00  1.000000e+00  0.000000e+00


  # Geoff McDonnell: Rotating Pendulum
  "https://insightmaker.com/insight/7uFEovUGymu77GO8FEp8Cn/Rotating-Pendulum",
  # Michael Scallan: Clone of Z602 Population with four age groups # error
  "https://insightmaker.com/insight/2k4n9nSLWwZ2faP9DgUbnS/Clone-of-Z602-Population-with-four-age-groups", # custom units! # units in Fertility don't work: [Children per Woman]*{1 year}/[Parents Age Classes] -> unitless * year / year should evaluate to 1/year?
  # Fertility <- set_units(Children.per.Woman(set_units(Point.in.Time, "common_year")) * set_units(1, "common_year") / Parents.Age.Classes, "1/common_year")
# Kyra Evers: Crielaard 2022
  "https://insightmaker.com/insight/5LxQr0waZGgBcPJcNTC029/Crielaard-2022",
  # Ash Moran: System Zoo Z109 ex 6: Whale population
  "https://insightmaker.com/insight/1vQMMepYQSN2khMUS6ORxl/System-Zoo-Z109-ex-6-Whale-population", # custom units! # problem of values in flows not needs units, see equation births
  # Guy Lakeman: 2014 Weather & Climate Extreme Loss of Arable Land and Ocean Fertility - The World3+ Model: Forecaster
  # "https://insightmaker.com/insight/5x8XTOZ4CougkAIl71QRLf/Clone-of-2014-Weather-Climate-Extreme-Loss-of-Arable-Land-and-Ocean-Fertility-The-World3-Model-Forecaster", #    # circular def with Service.output -> Delayed.Labor.Utilization.Fract -> Delayed.Labor.Utilization.Fract_delay -> Labor.Utilization.Fraction -> Jobs -> Potential.Service.Jobs -> Service.Output.Per.Capita -> Service.Output

  # "https://insightmaker.com/insight/GN8BlLTr167kQ5nF0Xbys/2014-Weather-Climate-Extreme-Loss-of-Arable-Land-and-Ocean-Fertility-The-World3-Model-Forecaster", # delay and smooth # # There's an error here, where in the URL, there's a link between Births (id: 208) and Population 15 to 44 (id: 110) which is supposed to be bi-directional but not encoded as such. When you clone the model this is updated. # model too big to work? problem with sorting equations
  # Rob koch: Using Systems thinking for technology in education
  "https://insightmaker.com/insight/2jN539hiEIfuxW9EEur6o9/Using-Systems-thinking-for-technology-in-education",
  # Geoff McDonnell: Diffusion of Innovation Bass Model
  "https://insightmaker.com/insight/6xyML8FVlYtLGrHN8CeFQa/Diffusion-of-Innovation-Bass-Model",
  # Geoff McDonnell: Technology Learning Curve
  "https://insightmaker.com/insight/5qKUhEgm59dFR9SqLlM0j8/Technology-Learning-Curve",
  # Scott Fortmann-Roe: Global Climate Change # error
  "https://insightmaker.com/insight/34ijXHsL8uVmuT2vnYv7En/Global-Climate-Change", # error: macros # custom units! # eart.energy ; e0 circular defnition? pluck_from_ode results in NA
  # Christina Cheung: Influence of Surface Temperature on Albedo and Greenhouse Effect # Units!
  "https://insightmaker.com/insight/6u2G5l9tGzD73cWnZAoabS/Influence-of-Surface-Temperature-on-Albedo-and-Greenhouse-Effect",
  # Roberto Forero: Story of ED Flows without Separate Areas
  "https://insightmaker.com/insight/2IBvMYSwzz6LYcFHUfvCOh/Story-of-ED-Flows-without-Separate-Areas", # not reproduced
  # Geoff McDonnell: ED Flows with Acute and FastTrack Areas
  "https://insightmaker.com/insight/1DZDy4dgXVDgZi1gpmR5YQ/ED-Flows-with-Acute-and-FastTrack-Areas",  # not reproduced
  # Keming Wang: Outpatient Clinics Patient Flow
  "https://insightmaker.com/insight/MvIPW00zkLCrkg04MfFoN/Outpatient-Clinics-Patient-Flow",
  # Geoff McDonnell: Burnout Dynamics
  "https://insightmaker.com/insight/14Mq6mI9axnY3MwbStlNFF/Burnout-Dynamics", # constraint broken, went to NA
  # Geoff McDonnell: Clinical Process Overview
  "https://insightmaker.com/insight/1t0bFODnfq6NDzXNT9Sx3Y/Clinical-Process-Overview",
  # Geoff McDonnell: ED Weekend Flows Senior Roster
  "https://insightmaker.com/insight/7yIER82cuED9PlWsBl0vtt/ED-Weekend-Flows-Senior-Roster",
  # mistake in modulus:
  # > set_units(12.25, "hour") %% 24
  # -11.75 [h]
  # Alex Neroni: Asteroid impact simulator
  "https://insightmaker.com/insight/4GG7aripEpg68NsQiOzvJT/Asteroid-impact-simulator", # xstart relies on dynamic parameter
  # Geoff McDonnell: Lorenz Attractor
  "https://insightmaker.com/insight/2GiYWAAc0hWlNwzWuKJN9B/Lorenz-Attractor",
  # Edythe: E coli life cycle model
  "https://insightmaker.com/insight/71VQ6W1TVZI6nxSMAY3YNS/E-coli-life-cycle-model", # make_step_func has dynamic arguments but is in constants -> move to ode_func?
  # Geoff McDonnell: The Science of Inequality
  "https://insightmaker.com/insight/2UahlHF2A33SKGeYq9ALmZ/The-Science-of-Inequality",
  # Guy Lakeman: OVERSHOOT GROWTH INTO TURBULENCE
  "https://insightmaker.com/insight/kWbS9pJh3TKdv3RYvNUkA/OVERSHOOT-GROWTH-INTO-TURBULENCE",
  # Guy Lakeman: WORLD2020 to PLANET2020
  # "https://insightmaker.com/insight/2cKJzcj6NZhSgdBBuhtccS/WORLD2020-to-PLANET2020", # update bidirectional
  # "https://insightmaker.com/insight/6kNkEWStEGw5CaOibUXaY3/Clone-of-WORLD2020-to-PLANET2020", # still population 15 to 44 issue
  # Scott Fortmann-Roe: Viral Growth
  "https://insightmaker.com/insight/OVFVEU0Y3RL8Z79iBFYcR/Viral-Growth",
  # Scott Fortmann-Roe: Policy Horizon Model
  "https://insightmaker.com/insight/5sX7qbqx88s5sGW2awww59/Policy-Horizon-Model", # problem of no operations between no units and unitless, equation Problem.Resolution.Rate
  # Scott Fortmann-Roe: The SIC (Susceptible-Infected-Concern) Model
  "https://insightmaker.com/insight/2kejhnX6cF5RJN7J1oC5N3/The-SIC-Susceptible-Infected-Concern-Model",
  # Alain Plante: Z605 Miniworld
  "https://insightmaker.com/insight/5qpcJK9cVjuJEOqzXcL8xr/Z605-Miniworld",
  # Ash Moran: System Zoo Z105: Time-dependent growth
 "https://insightmaker.com/insight/43bMIGsAYM6mD0LkmRxYIi/System-Zoo-Z105-Time-dependent-growth",
 # Henny van Dongen: Z504 Market and Price - System Zoo 3
 "https://insightmaker.com/insight/2K6wvKT4lhMATWCOgMpXEb/Z504-Market-and-Price-System-Zoo-3", # request operations between unitless and no units, demand.discrepancy
 # Ash Moran: System Zoo Z104: Exponential delay
 "https://insightmaker.com/insight/1bDhMmaxKiaON3HRqCVyo3/System-Zoo-Z104-Exponential-delay",
 # Steve Conrad: REM 221 - Z409 Fishery dynamics
 "https://insightmaker.com/insight/ugPKRnX5pUkLxOC4q4gx4/REM-221-Z409-Fishery-dynamics",
 # Lisa Belyea: Z308 Forest dynamics
 "https://insightmaker.com/insight/2ZFHLyo5kJMZjJTTtr5jKy/Z308-Forest-dynamics",
 # Henny van Dongen: Simplified and changed Z504 Market and Price - System Zoo 3
 "https://insightmaker.com/insight/5XqW2n5MsUstxLQnUVNzsp/Simplified-and-changed-Z504-Market-and-Price-System-Zoo-3",
 # Libby Boissy: Sustainable Ecotourism
 "https://insightmaker.com/insight/3CKpiqGWpOPt4Xmlb0DFUr/Sustainable-Ecotourism",
 # Geoff McDonnell: Dependence
  "https://insightmaker.com/insight/77YcOL6heuYhkZCwDgUtfl/Dependence",
 # Alfred Aenishaenslin: Bossel: Z110: Logistic growth with stock-dependent harvest
 "https://insightmaker.com/insight/3NVygSlxikH0WufhHnXOQj/Bossel-Z110-Logistic-growth-with-stock-dependent-harvest",
 # Steve Conrad: REM 221 - Z301 Regional Water Balance
 "https://insightmaker.com/insight/4GtjK0urhLez825nwpFrVy/REM-221-Z301-Regional-Water-Balance",
 # Geoff McDonnell: Escalation
 "https://insightmaker.com/insight/6xCAytF8lvgBl5lhJBxSQJ/Escalation",
 # Eduardo Enrique Escamilla: Bipolar II dynamics
 "https://insightmaker.com/insight/4fLYHSvEIukhLo0Rm4quz6/Bipolar-II-dynamics",
 # Alfred Aenishaenslin: Bossel: Z203 Brusselator
 "https://insightmaker.com/insight/6fwLP8mWXoeoLN1PQgHY4s/Bossel-Z203-Brusselator",
 # Alfred Aenishaenslin: Bossel: Z202 Van der Pol Oscillator
 "https://insightmaker.com/insight/7bsPnvVVkX0rCAa1IMlcnt/Bossel-Z202-Van-der-Pol-Oscillator",
 # Dr. Scott: System Zoo 409
 "https://insightmaker.com/insight/4CJOEAqsQKpSSZbCSgbErA/System-Zoo-409",
 # Geoff McDonnell: House Heating Dynamics
  # "https://insightmaker.com/insight/1b8kjLaJzsPb0l6R1Jf4kN/House-Heating-Dynamics", # solar heating gain, bidirectional link update
 "https://insightmaker.com/insight/76dBtZZIeBiu5vgk4PvQiY/Clone-of-House-Heating-Dynamics",
 # Alyssa Zhao: A Business Model # not reproduced: probably precision issue
 "https://insightmaker.com/insight/2QGhZudDQQF5e3wUBEAD75/A-Business-Model", # dependency on variable in ODE for setting initial value of delay
 # Alfred Aenishaenslin: Bossel: Z301 Regional Water Balance
 "https://insightmaker.com/insight/3TpHeSjqHM6BuHKgFLi32Z/Bossel-Z301-Regional-Water-Balance",
 # J: Buffet - Tragedy of the Commons application
 "https://insightmaker.com/insight/5AqF74CCXXSi3XwxXotOlC/Buffet-Tragedy-of-the-Commons-application",
 # J: Sucking thumb - Limits to Growth application
 "https://insightmaker.com/insight/2BagISaqPxlxhyEEAVRMvh/Sucking-thumb-Limits-to-Growth-application",
 # DIEGO GOZER: MGMT S-5012 - Shifting the Burden Archetype
"https://insightmaker.com/insight/5xjUMQrN4ewZucU2NeVT7L/MGMT-S-5012-Shifting-the-Burden-Archetype",
 # Gene Bellinger: Limits to Action Archetype
 "https://insightmaker.com/insight/5zj3IOF1S3AGkLltAGSW7N/Limits-to-Action-Archetype",
 # Edythe: Fixes That Fail
 "https://insightmaker.com/insight/4ET66ir2gDDjYO7tF30pcj/Fixes-That-Fail",
 # Gene Bellinger: Drifting Goals
 "https://insightmaker.com/insight/4iO69FoQ8bIfScKO1j280L/Drifting-Goals",
 # Gene Bellinger: Escalation
 "https://insightmaker.com/insight/3vJyt3ojd0zZB7Yt7JsQAW/Escalation",
 # Gene Bellinger: Growth and Underinvestment
"https://insightmaker.com/insight/1eQSvqwEBhTnqbFUipW5cx/Growth-and-Underinvestment",
 # Gene Bellinger: Accidental Adversaries
"https://insightmaker.com/insight/2DnkFfW7kFUat8zWsAENq4/Accidental-Adversaries",
 # Gene Bellinger Success to the Successful
 "https://insightmaker.com/insight/379kDfaPae99V1dOKPqWQw/Success-to-the-Successful",
# Michelle Bowman: Glucose Regulation & Diabetes Simulation
"https://insightmaker.com/insight/5kenuBllImJcSrTsPvogzs/Glucose-Regulation-Diabetes-Simulation",
# Christopher DiCarlo: Wind Resistance Model
"https://insightmaker.com/insight/77C9XTvvb66nQnGepYAn5m/Wind-Resistance-Model",
# Christopher DiCarlo: Spring-Mass Model
"https://insightmaker.com/insight/SdeQdNdWce06bqUV6x1Qj/Spring-Mass-Model",
# Hans Niedderer: Fall of a balloon in air
"https://insightmaker.com/insight/3mYXqJJOeOY9hv593ppszY/Fall-of-a-balloon-in-air",
# Alfredo Louro: Simple harmonic oscillator with damping
"https://insightmaker.com/insight/pdMvlu0kLyFzWzEhhKhj0/Simple-harmonic-oscillator-with-damping", # Second_Squared
# Werner Maurer: Urine flow rate
"https://insightmaker.com/insight/9EVFPL7SwCquieRp2dBzF/Urine-flow-rate",
# Pia Lübke: 3-Körper-Problem mit Reibung
# "https://insightmaker.com/insight/cH7FKRLiz1ZvQpYuhGwDE/3-K-rper-Problem-mit-Reibung", # vectorized model!
# Guy Lakeman: The probability density function (PDF) of the normal distribution or Bell Curve Gaussian Distribution by Guy Lakeman
"https://insightmaker.com/insight/53TUJu95bc2aQxKc8x0FZ8/The-probability-density-function-PDF-of-the-normal-distribution-or-Bell-Curve-Gaussian-Distribution-by-Guy-Lakeman",
# Guy Lakeman: THE BUTTERFLY EFFECT
"https://insightmaker.com/insight/5weFDR1M20YaE6NyofpIRh/THE-BUTTERFLY-EFFECT",
# Guy Lakeman: The Logistic Map
"https://insightmaker.com/insight/1Am80tHGcU8urb7j9r0pvi/The-Logistic-Map",
# Guy Lakeman: HYSTERESIS # error
"https://insightmaker.com/insight/6MAJFvPfUzUA4lbB48UivP/HYSTERESIS", # issue with units in weeks() -> they use a function F with as input weeks(), get invalid unit comparison
# Gene Bellinger: Tragedy of the Commons
"https://insightmaker.com/insight/3iQWfmAjLIJJtQR9dqN6u6/Tragedy-of-the-Commons",
# Gene Bellinger: Attractiveness Principle
"https://insightmaker.com/insight/4zWMYCCn8rb9x4uLKjH3Pq/Attractiveness-Principle",
# Geoff McDonnell: Diffusion of Medical Technology
"https://insightmaker.com/insight/mRgLQuj7aHhOmxNf9zhLU/Diffusion-of-Medical-Technology",
# Geoff McDonnell: Technology and Healthcare Costs and Outcomes 3
"https://insightmaker.com/insight/3DJvzG3dhjRiZxmIXVOnER/Technology-and-Healthcare-Costs-and-Outcomes-3",
# Geoff McDonnell: Double Loop Control Theory by William T Powers
"https://insightmaker.com/insight/6kIixhlOzvKCQPvT0fab4V/Double-Loop-Control-Theory-by-William-T-Powers",
# https://insightmaker.com/insight/3hTR9PYhQhrhguYa8DBHtR/Wolves-Rabbits-Carrots-Ecosystem
"https://insightmaker.com/insight/3hTR9PYhQhrhguYa8DBHtR/Wolves-Rabbits-Carrots-Ecosystem",
# Noel Urban: C:N-bacteria-DOM
"https://insightmaker.com/insight/59ab9y2IOrThbU4MnkaGXY/C-N-bacteria-DOM",
# Ryan Nakhle: The effect of Supply and Demand on the Housing Market Assignment 3 (43323871)
"https://insightmaker.com/insight/1QL265Ex7fjMTqR4hewLkd/The-effect-of-Supply-and-Demand-on-the-Housing-Market-Assignment-3-43323871",
# Geoff McDonnell: The Ecology of Medical Care
"https://insightmaker.com/insight/5RGz4vA7Num4747hzrpFTd/The-Ecology-of-Medical-Care",
# Mitchell Bassil: Real Estate Simulation Assignment - Mitchell Bassil 43290264
# "https://insightmaker.com/insight/1Tv9tTJvOf7NbaA6v3dnxT/Real-Estate-Simulation-Assignment-Mitchell-Bassil-43290264", # constraint violated - doesn't work even for simulation package
# Eugenio Parente: D-model (curve di Richards) con -ln(alpha)=lag*mu
"https://insightmaker.com/insight/6G3s10hbWkyECivkThcYX2/D-model-curve-di-Richards-con-ln-alpha-lag-mu", # not reproduced because of number precision (I think?)
# Jonathan Sandoe: honeybee hive population model
"https://insightmaker.com/insight/4z3zxrSSWTBR2YLWxIO64b/honeybee-hive-population-model",
# https://insightmaker.com/insight/4y8MpF98Zk61aBOy5RRqAg/Bio103-Growth-Models
"https://insightmaker.com/insight/4y8MpF98Zk61aBOy5RRqAg/Bio103-Growth-Models",
# Gene Bellinger: Adding Agriculture
"https://insightmaker.com/insight/5lBHvgHPR3lsBl26sajcXF/Adding-Agriculture",
# Osman Murat Anlı: Population w Agriculture
"https://insightmaker.com/insight/1oGV6SOdbYyvoAJySyJnl9/Population-w-Agriculture",
# Geoff McDonnell: Goodwin Business Cycle
"https://insightmaker.com/insight/6Cpye5jXaR6aho8YY5pW18/Goodwin-Business-Cycle",
# Geoff McDonnell: Minsky Financial Instability Model
"https://insightmaker.com/insight/7cBpdXaMEFKgw7lpcYLfv0/Minsky-Financial-Instability-Model",
# Silvan: Goodwin Model
"https://insightmaker.com/insight/2IoN2mc7hjH27tOx4i48nQ/Goodwin-Model",
# Geoff McDonnell: A Simple National Income Macroeconomic Model Continuous Time
"https://insightmaker.com/insight/6H2uFGAJIeT1yLHhDOfh3P/A-Simple-National-Income-Macroeconomic-Model-Continuous-Time",
# Lisa Belyea: Subtropical forest succession
"https://insightmaker.com/insight/39ev8P0kkWOu7ZQ4tsB6P6/Subtropical-forest-succession",
# Hans Røy: Spring and fall bloom
"https://insightmaker.com/insight/2Y2VsPhcFGBNUNdu8ODHjg/Spring-and-fall-bloom",
# Kevin Collins: YellowstoneEcoClassModel - ISD OWL
"https://insightmaker.com/insight/47y8NSJ0QarNk5Obeeor5G/YellowstoneEcoClassModel-ISD-OWL",
# Milena Lauschner Lopes: Fall with drag force
"https://insightmaker.com/insight/SaKHM3dOn6o24SNdtD37h/Fall-with-drag-force",
# Niels Johnson-Laird: PHY201 - Lab 2 - Projectile with Air Drag (V2)
"https://insightmaker.com/insight/EMAuZnBO0aiMCEkgrOMni/PHY201-Lab-2-Projectile-with-Air-Drag-V2",
# Wolfgang Thomaser: Kepler Ellipsen
"https://insightmaker.com/insight/3EpRD7NyanXvmhv5uaVZZx/Kepler-Ellipsen",
# Miguel Angel Niño Zambrano: Ejemplo 7: Retraso de Material Nivel 3 Cosecha Usando Funciones Históricas
"https://insightmaker.com/insight/4roBSgXWOtORIPQXw65mrc/Ejemplo-7-Retraso-de-Material-Nivel-3-Cosecha-Usando-Funciones-Hist-ricas",
# Elaine McCormick: Addiction Cycle System
"https://insightmaker.com/insight/59xbZlmkwIBWq5WuX4dpZg/Addiction-Cycle-System",
# Aleix Morgadas: Engineeing Effectiveness vs Technical Debt
"https://insightmaker.com/insight/4rnr9uqaqMneumUI2AaK78/Engineeing-Effectiveness-vs-Technical-Debt",
# Chaitanya TSK: Automobile Leasing Strategy
"https://insightmaker.com/insight/h0r8J3OgA1LhKwBKaehQm/Automobile-Leasing-Strategy",
# Ashish Mendiratta: Little's Law
"https://insightmaker.com/insight/2vUgrcSEBhqnde6yZQ7vdo/Littles-Law",
# Sung Yoon: 5f. Vendor Managed Inventory # error
"https://insightmaker.com/insight/2yZYSUSM5QuvBpOo8mF5mr/5f-Vendor-Managed-Inventory",
# Vincent Cate: Hyperinflation Simulation
"https://insightmaker.com/insight/54qQFW5Qt5Li4HEg05Vz5l/Hyperinflation-Simulation",

# Stochastic models
# Andrew E Long SIR model with stochastic events
"https://insightmaker.com/insight/5JC9o87MwwnZLM4epQnXzw/SIR-model-with-stochastic-events", # stochastic
# Geoff McDonnell: Balancing an Inverted Pendulum
"https://insightmaker.com/insight/4hsoljgmcipfND3hXD3fr4/Balancing-an-Inverted-Pendulum", # stochastic
# Kevin T Shoemaker: Source_Sink_2 # error
"https://insightmaker.com/insight/5cv9Iq3EEr9m2vBjVlVbQN/Source-Sink-2", # stochastic
# A Man Has No Name: Fern Population Model
"https://insightmaker.com/insight/1ZbnycySjZ50P0znZhM2WC/Fern-Population-Model", # stock with only comments # stochastic
# anton petrov: basic warehouse stock control # error
"https://insightmaker.com/insight/5DfLnWXeS8GZztY9nx8mbw/basic-warehouse-stock-control", # stochastic
# Chris Alford: Pesticide Use in Central America Model
"https://insightmaker.com/insight/6Xn1iRvbiLHfi4qilmhJ1d/Pesticide-Use-in-Central-America-Model", # stochastic
# Ben: Inventory Simulation # error
"https://insightmaker.com/insight/4uHyb1k0wbrjViAPqNJtsS/Inventory-Simulation"
)
print(sprintf("Number of URLs: %d", length(URLs)))

filepaths_IM = sapply(URLs, function(URL){
  print(URL)
  # extract_XML_from_URL(URL = URL, filepath_IM = NULL, directory = directory)
  # url_to_IM(URL = URL, filepath_IM = NULL, directory = directory)
  # URL to .InsightMaker file
  URL_XML = url_to_IM(URL, filepath_IM = NULL, directory = directory)

  return(URL_XML$filepath_IM)
  })
filepaths_IM

# Rewrite all models to have Stocks with NonNegative == "false"
out = lapply(filepaths_IM, function(filepath_IM){

  # Read text from .InsightMaker file
  xml_file = readLines(filepath_IM)

  # Find Stocks
  start_stock = stringr::str_locate_all(xml_file, stringr::fixed("<Stock"))[[1]]
  end_stock = stringr::str_locate_all(xml_file, stringr::fixed("Stock>"))[[1]]
  nr_stock = nrow(start_stock)

  i = 1
  # Use while loop because replacement ("false") is not of the same length as the original ("true")
  while (i <= nr_stock){
    sub_string = stringr::str_sub(xml_file, start_stock[i, "start"], end_stock[i, "end"])

    # Find whether Stock has NonNegative property set to true
    idx_nonnegative = stringr::str_locate(sub_string, stringr::fixed("NonNegative=\"true\""))

    # Replace with NonNegative = true
    if (!is.na(idx_nonnegative[, "start"])){
      stringr::str_sub(sub_string, idx_nonnegative[, "start"], idx_nonnegative[, "end"]) = "NonNegative=\"false\""
      stringr::str_sub(xml_file, start_stock[i, "start"], end_stock[i, "end"]) = sub_string
    }

    i = i + 1
  }

  # Overwrite
  writeLines(xml_file, filepath_IM)

})


# In Visual Studio Code, navigate to "C:/Users/kevers1/Documents/PhD/insightmakeR1/validate" and run "node run_simulation.js" to simulate all models via the JavaScript simulation package and save output


# Convert JSON files to dataframes
json_filepaths = stringr::str_replace_all(filepaths_IM, "\\.InsightMaker$", ".json")
json_euler_filepaths = stringr::str_replace_all(filepaths_IM, "\\.InsightMaker$", "_euler.json")

convert_json_to_df = function(json_filepath){
  print(json_filepath)

  tryCatch({
    # Read the JSON file
    data <- rjson::fromJSON(file = json_filepath)

    # Create dictionary out of name-id mapping
    rename_dict = unlist(data[["_nameIdMapping"]])
    rename_dict = stats::setNames(names(rename_dict), unname(rename_dict))

    # Create dataframe of simulated timeseries
    df = cbind(time = data[["_data"]][["times"]],
                data[["_data"]][["data"]] %>% purrr::map(rbind) %>%
                 do.call(rbind, .) %>%
      as.data.frame())

    # Save as .RDS file
    simulation_filepath <- sub("\\.json$", ".RDS", json_filepath)
    output = list(df = df, nameIdMapping = rename_dict, stochastic = data[["_data"]][["stochastic"]])
    # output %>% purrr::map(head) %>% print()
    saveRDS(output, simulation_filepath)

    if (file.exists(simulation_filepath)) print(simulation_filepath)
    simulation_filepath

  }, error = function(e){
    print(e)
    return(e)
  }
  )
}

simulation_filepaths = lapply(json_filepaths, convert_json_to_df)
simulation_euler_filepaths = lapply(json_euler_filepaths, convert_json_to_df)


# Parallel
# Create a cluster
cl <- makeCluster(detectCores() - 4) # Detect the number of available cores

# Register the cluster
doParallel::registerDoParallel(cl)

# Generate R models with sdbuildR
sdbuildR_filepaths = stringr::str_replace_all(filepaths_IM, ".InsightMaker", "_sdbuildR.RDS")

out = foreach::foreach(i = 1:length(filepaths_IM),
                                          filepath_IM = filepaths_IM,
                                          sdbuildR_filepath = sdbuildR_filepaths,
                                          .packages = c("insightmakeR1", "dplyr")) %dopar% {

    print("")
    message(paste0("Filepath number ", i))
    # filepath_IM = filepaths_IM[i]
    print(filepath_IM)

    tryCatch({
      sfm = insightmaker_to_sfm(filepath_IM = filepath_IM)
      print(summary(sfm))
      names_df = get_names(sfm)
      sim = simulate(sfm)
      print(head(sim$df, n = 1))
      # output = insightmaker_to_sfm(filepath_IM = filepath_IM, directory = directory,
      #                            seed_number = 123, open_script = F, debug = F, overwrite = T,
      #                            keep_nonnegative_flow = T,
      #                            keep_nonnegative_stock = T, # !
      #                            keep_units = T, keep_solver = F, check_script = T)
      saveRDS(list(sfm = sfm, names_df = names_df) %>% utils::modifyList(sim), sdbuildR_filepath)
      return(TRUE)
    }, error = function(e) {
      return(FALSE)
    })

  }

out %>% purrr::map(length)
out %>% purrr::map(names)




# test
i = 102
simulation_filepath = simulation_filepaths[[i]]
sdbuildR_filepath = sdbuildR_filepaths[i]
simulation_euler_filepath = simulation_euler_filepaths[[i]]


# Check equality between timeseries generated by simulation package and by sdbuildR
compare_output = function(sdbuildR_filepath, simulation_filepath, simulation_euler_filepath){

  print(sdbuildR_filepath)
  print(simulation_filepath)
  print(simulation_euler_filepath)

  if (!(file.exists(as.character(simulation_filepath)) & file.exists(as.character(sdbuildR_filepath)))){
    return(list(equal = 0, message = "Both files do not exist!",
                simulation_filepath = simulation_filepath,
                sdbuildR_filepath = sdbuildR_filepath))
  } else if (!file.exists(as.character(sdbuildR_filepath))){
    return(list(equal = 0, message = "The sdbuildR_filepath does not exist!",
                simulation_filepath = simulation_filepath,
                sdbuildR_filepath = sdbuildR_filepath))
  } else if (!file.exists(as.character(simulation_filepath))){
    return(list(equal = 0, message = "The simulation_filepath does not exist!",
                simulation_filepath = simulation_filepath,
                sdbuildR_filepath = sdbuildR_filepath))
  } else {

    # Read files
    R_sim = readRDS(sdbuildR_filepath)
    java_sim = readRDS(simulation_filepath)
    java_sim_euler = tryCatch(readRDS(simulation_euler_filepath),
                              error = function(e) {
                                return(e)
                                }) # Euler version

    # Check properties
    non_negative_stock = !is.null(R_sim$sfm$behavior$stock$non_negative)
    non_negative_flow = !is.null(R_sim$sfm$behavior$flow$non_negative)
    ode_method = R_sim$sfm$sim_specs$method
    original_ode_method = R_sim$sfm$sim_specs$method_InsightMaker


    # In case of non-negative stocks, the Insight Maker output can only be reproduced with Euler integration. Compare R_sim to java_sim_euler
    if (!is.null(original_ode_method)){
      if (original_ode_method == "rk4" & ode_method == "euler" & non_negative_stock){
        print("Compare to euler!")
        java_sim = java_sim_euler
      }
    }


    if (is.null(R_sim$df)){
      return(list(equal = 0, message = "sdbuildR translation was unsuccessful!",
                  simulation_filepath = simulation_filepath,
                  sdbuildR_filepath = sdbuildR_filepath))
    }


    # Add parameters
    R_sim$df = R_sim$df %>% cbind(R_sim$pars %>%
      # Remove elements of length 1
      purrr::keep(\(x) length(x) == 1 & !inherits(x, "function")) %>%
        purrr::map(drop_if_units))

    # Make sure column variables are the same in IM and R simulated data
    # names_df = R_sim$names_df
    replace_dict = R_sim$sfm$model$variables %>%
      purrr::imap(function(x, building_block){
        purrr::map(x, function(y){
          if (building_block == "gf"){
            list(y$id) %>% stats::setNames(paste0(y$name, "(", y$source, ")")) %>% return()
          } else {
            list(y$id) %>% stats::setNames(y$name) %>% return()
          }
        })
      }) %>% purrr::compact() %>%
      purrr::flatten() %>% purrr::flatten() %>% unlist()

    # Rename columns of java simulation according to id
    java_sim_df = java_sim$df %>% dplyr::rename(dplyr::any_of(replace_dict))
    cols = intersect(colnames(java_sim_df), colnames(R_sim$df))
    missing_col = setdiff(colnames(java_sim_df), cols)

    # Ensure all variables are numerical
    R_sim_df = R_sim$df %>% dplyr::select(dplyr::all_of(cols)) %>%
      dplyr::mutate_if(is.logical, ~ . * 1) %>%
      dplyr::mutate_all(~ as.numeric(as.character(.)))
    java_sim_df = java_sim_df %>% dplyr::select(dplyr::all_of(cols)) %>%
      dplyr::mutate_if(is.logical, ~ . * 1) %>%
      dplyr::mutate_all(~ as.numeric(as.character(.)))

    if (all(dim(java_sim_df) == dim(R_sim_df))){
      simple_check = all.equal(java_sim_df, R_sim_df,
                             tolerance = 1e-5) # Lower tolerance because of differences in precision in the representation of numbers
      simple_check

      if (all(simple_check == T)){
        print("Dataframes are equal!")
        return(list(equal = 1,
                    simulation_filepath = simulation_filepath,
                    sdbuildR_filepath = sdbuildR_filepath))
      }
    } else {
      simple_check = 0
    }

    # If they are not equal, provide more detailed information
    equal_nrow = nrow(java_sim_df) == nrow(R_sim_df)
    equal_ncol = ncol(java_sim_df) == ncol(R_sim_df)


    # Check whether first row is the same
    first_row_equal = java_sim_df[1,] == R_sim_df[1,]
    if (all(stats::na.omit(first_row_equal)) & !anyNA(first_row_equal)){
      first_row_equal = 1
    }

    if (equal_nrow){

      # Compare each column
      eucl_dist = sapply(colnames(java_sim_df),
                         function(name){
                           IM = java_sim_df[[name]]
                           sim = R_sim_df[[name]]

                           # Calculate Euclidean distance, ignoring NAs
                           sqrt(sum((IM - sim)^2, na.rm = TRUE))
                         })


      which(abs(java_sim_df - R_sim_df) >  1e-5, arr.ind=TRUE)

      java_sim_df[236,]
      R_sim_df[236,]

      # Find first row where they are no longer the same
      # first_discrepancy_row_nr = which(rowSums(java_sim_df != R_sim_df) > 0)[1]
      first_discrepancy_row_nr = which(java_sim_df != R_sim_df, arr.ind = TRUE)
      if (nrow(first_discrepancy_row_nr) == 0){
        first_discrepancy_row_nr = 1
      } else {
        first_discrepancy_row_nr = first_discrepancy_row_nr[1,]
      }


    } else {
      eucl_dist = c()
      first_discrepancy_row_nr = c()
    }

    # Check NA and infinite values
    nr_NA_IM = colSums(apply(java_sim_df, 2, is.na))
    nr_NA_sim = colSums(apply(R_sim_df, 2, is.na))
    nr_inf_IM = colSums(apply(java_sim_df, 2, is.infinite))
    nr_inf_sim = colSums(apply(R_sim_df, 2, is.infinite))

    if (all(nr_NA_IM == 0)){nr_NA_IM = 0}
    if (all(nr_NA_sim == 0)){nr_NA_sim = 0}
    if (all(nr_inf_IM == 0)){nr_inf_IM = 0}
    if (all(nr_inf_sim == 0)){nr_inf_sim = 0}

    # Merge head of dataframes for easy comparison
    head_merged = dplyr::full_join(head(java_sim_df), head(R_sim_df), suffix = c(".js",".R"), by = "time") %>%
      dplyr::select(order(colnames(.))) %>%
      dplyr::select("time", everything())

    tail_merged = dplyr::full_join(tail(java_sim_df), tail(R_sim_df), suffix = c(".js",".R"), by = "time") %>%
      dplyr::select(order(colnames(.))) %>%
      dplyr::select("time", everything())


    print("Dataframes were not (exactly) equal.")
    return(list(equal = 0,
                simulation_filepath = simulation_filepath,
                sdbuildR_filepath = sdbuildR_filepath,
                ode_method = ode_method,
                original_ode_method = ifelse(!is.null(original_ode_method), original_ode_method, ode_method),
                non_negative_stock = non_negative_stock,
                non_negative_flow = non_negative_flow,
                missing_col = missing_col,
                simple_check = simple_check,
                stochastic = java_sim$stochastic,
                head_merged = head_merged,
                tail_merged = tail_merged,
               equal_nrow = equal_nrow,
               equal_ncol = equal_ncol,
               eucl_dist = eucl_dist,
               # first_row_equal = first_row_equal,
               first_discrepancy_row_nr = first_discrepancy_row_nr,
               dim_IM = dim(java_sim_df),
               dim_sim = dim(R_sim_df),
               nr_NA_IM = nr_NA_IM,
               nr_NA_sim = nr_NA_sim,
               nr_inf_IM = nr_inf_IM,
               nr_inf_sim = nr_inf_sim))
  }
}

comparison = mapply(compare_output, sdbuildR_filepaths,
                    simulation_filepaths, simulation_euler_filepaths) %>% unname()
comparison


equal_sim = comparison %>% purrr::map("equal") %>% unlist()
sum(equal_sim)
length(equal_sim)



comp_df = comparison %>% purrr::imap(function(x, idx){

  print(idx)
  if (is.null(x$message)){
    df = data.frame(idx = idx,
      equal = x$equal,
                    filename = basename(x$simulation_filepath)
                    # simulation_filepath = x$simulation_filepath, sdbuildR_filepath = x$sdbuildR_filepath
                    )

    if (x$equal == 0){
      df$non_negative_stock = x$non_negative_stock
      df$non_negative_flow = x$non_negative_flow
      df$ode_method = x$ode_method
      df$original_ode_method = x$original_ode_method
      df$stochastic = x$stochastic
      df$equal_nrow = x$equal_nrow
      df$equal_ncol = x$equal_ncol
      df$first_row_equal = x$first_row_equal
    }

    return(df)
  }
  }) %>%
  do.call(dplyr::bind_rows, .) %>% as.data.frame()

head(comp_df)

sum(comp_df$equal)
length(comp_df$equal)
View(comp_df %>% dplyr::filter(.data$equal == 0, .data$stochastic == FALSE))

URLs[comparison %>% purrr::map(function(x){!is.null(x$message)}) %>% unlist() %>% which()] %>% unname()


URLs[c(11,16,71,102)]
comparison %>% purrr::map("message")


# **t do: check for stochastic
