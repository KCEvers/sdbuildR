# init.jl - Script to initialize Julia environment for sdbuildR

# Load packages
using CSV
using DataFrames
using DiffEqCallbacks
using Distributions
using OrdinaryDiffEqLowOrderRK
using OrdinaryDiffEqRosenbrock
using OrdinaryDiffEqTsit5
using OrdinaryDiffEqVerner
using Random
using SciMLBase
using Statistics
using StatsBase
using SystemDynamicsBuildR

# Add initialization of sdbuildR
init_sdbuildR = true

