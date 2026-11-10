#title: "WOWAir Bankruptcy Impacts Master Script"
#author: "Luke Miller"
#date: "2026-04-23"
set.seed(12345)

# Load Data
source("Scripts/Setup and Data.R")

# Helper Functions
source("Scripts/Helper Functions.R")

# General Visuals & Tables
source("Scripts/Tourism Specialization.R")
source("Scripts/Callibration and Information Graphics.R")

# Primary Outcome
source("Scripts/Primary Arrivals.R")
source("Scripts/All Arrivals.R")
source("Scripts/Remove TS Elements.R") # keep here so it can use the preferred BSTS total-passenger output

# Labor Outcomes
# Labor.R creates the manuscript labor table and relative-impact CI figure.
source("Scripts/Labor.R")

 # Sensitivities
# Do Results Depend on Choice of controls (X) used
source("Scripts/Variable Choice.R")

# Do Results Depend on Control Country (I)
source("Scripts/Country Choice.R")

# TWFE
source("Scripts/DID.R")

# Placebo Analysis
# Other Countries
source("Scripts/Placebo - Other Countries.R")

# Other Countries
source("Scripts/Placebo - Years Prior.R")

# Insulated Industry
source("Scripts/Placebo - Fishing.R")

# Compile Placebos into a Single Table
source("Scripts/Placebos - Compile.R")

# Removes TS Elements
# source("Scripts/No TS Elements.R")

# Additional Analysis - Not included in paper
source("Scripts/Exports and Imports.R")
source("Scripts/Hotel Stays.R")
source("Scripts/Transportation Measures.R")
source("Scripts/Additional Outcome Tables.R")


# Additional Analysis - Response to Referees:
# Passengers By Nationality:
# This script creates the nationality route and confidence-interval figure.
source("Scripts/Passengers by Nationality.R")

# Flight Counts US
source("Scripts/BTS T100 Monthly Airline Traffic.R")

# Testing to see if results are carried by one to 2 indicators only 
# i.e., removing ONLY arrivals from other countries as predictors
# source("Scripts/Less Predictive Covariates.R")
  # Error here needs debugged

source("Scripts/Rename Primary Figures.R")

# Make a Word-friendly Excel copy of the paper tables.
source("Scripts/Export Tables to Excel.R")
