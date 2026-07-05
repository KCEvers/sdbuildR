# init.jl - Script to initialize Julia environment for sdbuildR

# Load packages
using CSV
using DataFrames
using DiffEqCallbacks
using Distributions
using OrdinaryDiffEq
using OrdinaryDiffEqLowOrderRK
using Random
using SciMLBase
using Statistics
using StatsBase
using SystemDynamicsBuildR

# Add initialization of sdbuildR
init_sdbuildR = true

