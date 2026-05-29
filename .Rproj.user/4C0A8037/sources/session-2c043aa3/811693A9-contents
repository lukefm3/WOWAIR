library(dplyr)
library(lubridate)
library(tibble)
library(knitr)
library(kableExtra)

iceland <- month[,grepl("iceland",names(month))]
total_in_employment_total_iceland

colnames(iceland)

iceland$total_in_employment_total_iceland

iceland$total_in_employment_total_iceland

iceland$employ


month %>% filter(year(date)==2010) %>% summarize(mean(total_tourism_employees_iceland,na.rm=T)/mean(total_in_employment_total_iceland,na.rm=T))



# helper functions
pct_change <- function(new, old) {
  ((new - old) / old) * 100
}

year_mean <- function(data, var, yr) {
  data %>%
    filter(year(date) == yr) %>%
    summarize(val = mean({{ var }}, na.rm = TRUE)) %>%
    pull(val)
}

# build the underlying values
specialization_raw <- tibble(
  Indicator = c(
    "Monthly Foreign Visitors Per Worker",
    "Tourism Employees as \\% of All Employees",
    "Transportation Employees as \\% of All Employees",
    "Air Transport Employees as \\% of All Employees",
    "Accommodation Employees as \\% of All Employees",
    "Food and Bev Employees as \\% of All Employees",
    "Tour Services Employees as \\% of All Employees",
    "Other Tourism Employees as \\% of All Employees"
  ),
  
  `2010` = c(
    year_mean(month, departures_foreign_iceland, 2010) /
      year_mean(month, total_in_employment_total_iceland, 2010),
    
    year_mean(month, total_tourism_employees_iceland, 2010) /
      year_mean(month, total_in_employment_total_iceland, 2010) * 100,
    
    year_mean(month, employees__transport_iceland, 2010) /
      year_mean(month, total_in_employment_total_iceland, 2010) * 100,
    
    year_mean(month, air_employees_iceland, 2010) /
      year_mean(month, total_in_employment_total_iceland, 2010) * 100,
    
    year_mean(month, employees_Accommodation_iceland, 2010) /
      year_mean(month, total_in_employment_total_iceland, 2010) * 100,
    
    year_mean(month, food_bev_employees_iceland, 2010) /
      year_mean(month, total_in_employment_total_iceland, 2010) * 100,
    
    year_mean(month, tour_services_employees_iceland, 2010) /
      year_mean(month, total_in_employment_total_iceland, 2010) * 100,
    
    year_mean(month, other_tourism_employees_iceland, 2010) /
      year_mean(month, total_in_employment_total_iceland, 2010) * 100
  ),
  
  `2018` = c(
    year_mean(month, departures_foreign_iceland, 2018) /
      year_mean(month, total_in_employment_total_iceland, 2018),
    
    year_mean(month, total_tourism_employees_iceland, 2018) /
      year_mean(month, total_in_employment_total_iceland, 2018) * 100,
    
    year_mean(month, employees__transport_iceland, 2018) /
      year_mean(month, total_in_employment_total_iceland, 2018) * 100,
    
    year_mean(month, air_employees_iceland, 2018) /
      year_mean(month, total_in_employment_total_iceland, 2018) * 100,
    
    year_mean(month, employees_Accommodation_iceland, 2018) /
      year_mean(month, total_in_employment_total_iceland, 2018) * 100,
    
    year_mean(month, food_bev_employees_iceland, 2018) /
      year_mean(month, total_in_employment_total_iceland, 2018) * 100,
    
    year_mean(month, tour_services_employees_iceland, 2018) /
      year_mean(month, total_in_employment_total_iceland, 2018) * 100,
    
    year_mean(month, other_tourism_employees_iceland, 2018) /
      year_mean(month, total_in_employment_total_iceland, 2018) * 100
  ),
  
  `2019` = c(
    year_mean(month, departures_foreign_iceland, 2019) /
      year_mean(month, total_in_employment_total_iceland, 2019),
    
    year_mean(month, total_tourism_employees_iceland, 2019) /
      year_mean(month, total_in_employment_total_iceland, 2019) * 100,
    
    year_mean(month, employees__transport_iceland, 2019) /
      year_mean(month, total_in_employment_total_iceland, 2019) * 100,
    
    year_mean(month, air_employees_iceland, 2019) /
      year_mean(month, total_in_employment_total_iceland, 2019) * 100,
    
    year_mean(month, employees_Accommodation_iceland, 2019) /
      year_mean(month, total_in_employment_total_iceland, 2019) * 100,
    
    year_mean(month, food_bev_employees_iceland, 2019) /
      year_mean(month, total_in_employment_total_iceland, 2019) * 100,
    
    year_mean(month, tour_services_employees_iceland, 2019) /
      year_mean(month, total_in_employment_total_iceland, 2019) * 100,
    
    year_mean(month, other_tourism_employees_iceland, 2019) /
      year_mean(month, total_in_employment_total_iceland, 2019) * 100
  )
) %>%
  mutate(
    `% Change, 2010-2018` = pct_change(`2018`, `2010`),
    `% Change, 2018-2019` = pct_change(`2019`, `2018`)
  )

# format for display
specialization_out <- specialization_raw %>%
  mutate(
    `2010` = c(
      sprintf("%.2f", `2010`[1]),
      sprintf("%.2f\\%%", `2010`[-1])
    ),
    `2018` = c(
      sprintf("%.2f", `2018`[1]),
      sprintf("%.2f\\%%", `2018`[-1])
    ),
    `2019` = c(
      sprintf("%.2f", `2019`[1]),
      sprintf("%.2f\\%%", `2019`[-1])
    ),
    `% Change, 2010-2018` = sprintf("%.2f\\%%", `% Change, 2010-2018`),
    `% Change, 2018-2019` = sprintf("%.2f\\%%", `% Change, 2018-2019`)
  )

# make key rows bold
specialization_out$Indicator[c(1, 2)] <- paste0("\\textbf{", specialization_out$Indicator[c(1, 2)], "}")
specialization_out[1, 2:6] <- lapply(specialization_out[1, 2:6], function(x) paste0("\\textbf{", x, "}"))
specialization_out[2, 2:6] <- lapply(specialization_out[2, 2:6], function(x) paste0("\\textbf{", x, "}"))

# LaTeX table
latex_tbl <- specialization_out %>%
  kable(
    format = "latex",
    booktabs = FALSE,
    escape = FALSE,
    align = "l",
    col.names = c(
      "Indicator",
      "2010",
      "2018",
      "2019",
      "\\% Change, 2010-2018",
      "\\% Change, 2018-2019"
    ),
    caption = "Iceland Sectoral Employment Concentration"
  ) %>%
  kable_styling(latex_options = c("hold_position"), full_width = FALSE) %>%
  pack_rows("\\textbf{Tourists per Worker}", 1, 1, escape = FALSE) %>%
  pack_rows("\\textbf{Employment Breakdown By Tourism Sector}", 2, 8, escape = FALSE)




# Split into lines
latex_lines <- strsplit(latex_tbl, "\n")[[1]]

# Remove only exact wrapper/caption/centering lines
latex_lines <- latex_lines[
  !grepl("^\\\\begin\\{table\\}", latex_lines) &
    !grepl("^\\\\end\\{table\\}", latex_lines) &
    !grepl("^\\\\centering$", latex_lines) &
    !grepl("^\\\\caption\\{", latex_lines)
]

# Save cleaned table
writeLines(latex_lines, "Main Figures and Tables/specialization.tex")


