# Rename Tables, Markdowns, and Graphics

# Table 1: Summary Table
file.copy(from = "Manually Created Tables/summary_table.tex", to   = "Paper/Tables/Table 1.tex", overwrite = T)

# Table 2: Varying Information Availability
file.copy(from = "Manually Created Tables/information_asymmetry.tex", to   = "Paper/Tables/Table 2.tex", overwrite = T)

# Table 3: Iceland Sectoral Employment Concentration
file.copy(from = "Main Figures and Tables/specialization.tex", to   = "Paper/Tables/Table 3.tex", overwrite = T)

# Table 4: Placebo Counterfactual Analysis
file.copy(from = "Main Figures and Tables/placebo_impact_table.tex", to   = "Paper/Tables/Table 4.tex", overwrite = T)

# Table 5: Choice of Control Countries
file.copy(from = "Main Figures and Tables/choice_of_control_countries.tex", to   = "Paper/Tables/Table 5.tex", overwrite = T)

# Table 6: Projected Change in Tourism From Different Coefficient Groups
file.copy(from = "Main Figures and Tables/choice_of_variables.tex", to   = "Paper/Tables/Table 6.tex", overwrite = T)

# Table 7: Forecasted Impacts Without Time-Series
file.copy(from = "Main Figures and Tables/drop_time_series.tex", to   = "Paper/Tables/Table 7.tex", overwrite = T)

# Table 8: Two-Way F.E. Treatment Estimate
file.copy(from = "Main Figures and Tables/DID_table.tex", to   = "Paper/Tables/Table 8.tex", overwrite = T)


# Figures

# Figure 1: Recent Information Bias
file.copy(from = "Main Figures and Tables/recent_information_bias.png", to   = "Paper/Figures/Figure 1.png", overwrite = T)

# Figure 2: Posterior Inclusion Probabilities
file.copy(from = "Main Figures and Tables/coefficients_arrivals_plot.png", to   = "Paper/Figures/Figure 2.png", overwrite = T)

# Figure 3: Cumulative effects across pre-periods
file.copy(from = "Main Figures and Tables/time_callibration.png", to   = "Paper/Figures/Figure 3.png", overwrite = T)

# Figure 4: Decline in Airline Passengers
file.copy(from = "Main Figures and Tables/primary_arrivals_plot.png", to   = "Paper/Figures/Figure 4.png", overwrite = T)

# Figure 5: Monthly Arrivals Relative to Counterfactual 
file.copy(from = "Main Figures and Tables/arrivals_by_month.png", to   = "Paper/Figures/Figure 5.png", overwrite = T)

# Figure 6: Impacts on Passenger Counts by Nationality
file.copy(from = "Main Figures and Tables/passengers_by_nationality.png", to   = "Paper/Figures/Figure 6.png", overwrite = T)

# Figure 7: (combines a .tex and a .png)
# Figure 7a:
file.copy(from = "Main Figures and Tables/national_labor_impact_plot.png", to   = "Paper/Figures/Figure 7a.png", overwrite = T)

# Figure 7b:
file.copy(from = "Main Figures and Tables/comprehensive_labor_impact_table.tex", to   = "Paper/Figures/Figure 7b.tex", overwrite = T)

# Figure 8: Monthly Differences in Estimated and Realized Labor Levels
file.copy(from = "Main Figures and Tables/sectoral_labor_impact_plot.png", to   = "Paper/Figures/Figure 8.png", overwrite = T)

# Figure 9: Projected Monthly Differences (No Time-Series Elements)
file.copy(from = "Main Figures and Tables/dropped_time_series_differences.png", to   = "Paper/Figures/Figure 9.png", overwrite = T)


# Figure 10: Projected Cumulative Differences (No Time-Series Elements)
file.copy(from = "Main Figures and Tables/dropped_time_series_cumul_difs.png", to   = "Paper/Figures/Figure 10.png", overwrite = T)


# Move any .png or .tex files from folder a to folder B: 
from <- getwd()   # current working directory
to   <- file.path(getwd(), "Additional Tables and Figures")

# create destination if it doesn't exist
dir.create(to, showWarnings = FALSE)

files <- list.files(from, pattern = "\\.(png|tex|md)$", full.names = TRUE)

file.rename(files, file.path(to, basename(files)))

