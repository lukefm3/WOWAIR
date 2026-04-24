variable_choice <- rbind(total_t, national_t,foreign_t,
                         fishing_impact_all,
                         country_placebo_g, country_placebo_f, country_placebo_d, country_placebo_n,
                         year_prior_1, year_prior_2)

variable_choice$Indicator <- rownames(variable_choice)
variable_choice <- variable_choice %>% select(Indicator, everything())

kable(variable_choice, caption = "Placebo Impact", format = "markdown", row.names = FALSE)
writeLines(kable(variable_choice, caption = "Placebo Impact", format = "markdown", row.names = FALSE), "placebo_table.md")


# add stars to the "Relative Impact (%)" column (keep numeric shown to 2 d.p.)
vc_out <- variable_choice %>%
  mutate(
    `Relative Impact (%)` =
      sprintf("%.2f%s", `Relative Impact (%)`, pstars(`p-value`)),

    `9-Month Cumulative Impact` =
      formatC(`9-Month Cumulative Impact`, format = "f", big.mark = ",", digits = 0),

    `95% CI Upper` =
      formatC(`95% CI Upper`, format = "f", big.mark = ",", digits = 0),

    `95% CI Lower` =
      formatC(`95% CI Lower`, format = "f", big.mark = ",", digits = 0),

    `RMSE (priors)` =
      formatC(`RMSE (priors)`, format = "f", big.mark = ",", digits = 0)
  )

col_names <- c(
  "Indicator",
  "Relative Impact (%)",
  "9-Month Cumulative Impact",
  "95% CI Upper",
  "95% CI Lower",
  "p-value",
  "RMSE (priors)"
)

latex_tbl <- vc_out %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    escape = FALSE,              # keep the *** in the table
    caption = "Impacts and Fit Statistics",
    col.names = col_names,
    align = c("l", rep("r", 6))  # 7 columns total
  ) %>%
  kable_styling(latex_options = c("hold_position"))


# collapse to one string so regex replacements are easier
latex_tbl<- paste(latex_tbl, collapse = "\n")

# keep only the tabular block
latex_tbl <- str_extract(latex_tbl, "(?s)\\\\begin\\{tabular\\}\\[t\\]\\{lrrrrrr\\}.*?\\\\end\\{tabular\\}")

# remove all \addlinespace
latex_tbl <- str_replace_all(latex_tbl, "\n\\\\addlinespace", "")
latex_tbl <- str_replace_all(latex_tbl, "\\\\addlinespace\n", "")
latex_tbl<- str_replace_all(latex_tbl, "\\\\addlinespace", "")

# change All Indicators to Fish Landings and insert midrule before it
latex_tbl <- str_replace_all(
  latex_tbl,
  "\nAll Indicators",
  "\n\\\\midrule\nFish Landings"
)

# insert midrule before Greenland
latex_tbl <- str_replace_all(
  latex_tbl,
  "\\\\\\\\\nGreenland",
  "\\\\\\\\\n\\\\midrule\nGreenland"
)

# insert midrule before Impact in January 2018
latex_tbl <- str_replace_all(
  latex_tbl,
  "\\\\\\\\\nImpact in January 2018",
  "\\\\\\\\\n\\\\midrule\nImpact in January 2018"
)

# escape % so LaTeX compiles
latex_tbl <- str_replace_all(latex_tbl, "%", "\\\\%")

# write to .tex
writeLines(latex_tbl, "Main Figures and Tables/placebo_impact_table.tex")






