# Additional outcome tables

dir.create("Main Figures and Tables", showWarnings = FALSE, recursive = TRUE)

table_from_impact_avg_yoy <- function(results, name) {
  rmse <- calculate_rmse(results)
  response <- as.numeric(results$series$response)
  prior_year_index <- (length(response) - 20):(length(response) - 12)
  prior_year_mean <- mean(response[prior_year_index], na.rm = TRUE)
  quantity_digits <- ifelse(grepl("Rate", name), 2, 0)

  temp_table <- data.frame(
    "Relative Impact (%)" = round(results$summary$RelEffect[1] * 100, 2),
    "Average Monthly Impact" = round(results$summary$AbsEffect[1], quantity_digits),
    "Monthly Avg. Predicted" = round(results$summary$Pred[1], quantity_digits),
    "Monthly Avg. Observed" = round(results$summary$Actual[1], quantity_digits),
    "YoY Actual Change (%)" = round((results$summary$Actual[1] / prior_year_mean - 1) * 100, 4),
    "p-value" = round(results$summary$p[1], 4),
    "RMSE (priors)" = round(rmse, quantity_digits),
    check.names = FALSE
  )
  row.names(temp_table)[1] <- name
  temp_table
}

run_named_impact <- function(outcome_col, label, predictor_list) {
  if (!outcome_col %in% colnames(month)) {
    stop("Missing outcome column: ", outcome_col)
  }

  complete <- month %>% select(any_of(predictor_list))
  complete2 <- month %>% select(all_of(outcome_col))
  colnames(complete2) <- "outcome_iceland"

  icelandcolumns <- grep("iceland", colnames(complete))
  if (length(icelandcolumns) > 0) {
    complete <- complete[, -icelandcolumns, drop = FALSE]
  }

  complete <- cbind(complete, complete2)
  complete <- complete %>%
    filter(date > "2013-12-31") %>%
    filter(date < "2019-12-31")
  complete <- complete[, colSums(is.na(complete)) == 0, drop = FALSE] %>%
    relocate(outcome_iceland) %>%
    relocate(date)

  temp <- ts(
    data = complete[-c(1)],
    start = 2014,
    end = 2020,
    frequency = 12
  ) %>% head(-1)

  pre.period <- as.yearmon(c("2014-01-01", "2019-01-01"))
  post.period <- as.yearmon(c("2019-04-01", "2020-01-01"))

  impact <- CausalImpact(
    temp,
    pre.period,
    post.period,
    model.args = list(niter = 5000, nseasons = 12, season.duration = 1)
  )

  table_from_impact_avg_yoy(impact, label)
}

latex_escape_additional <- function(x) {
  x <- gsub("\\", "\\textbackslash{}", x, fixed = TRUE)
  x <- gsub("&", "\\&", x, fixed = TRUE)
  x <- gsub("%", "\\%", x, fixed = TRUE)
  x
}

format_additional_whole <- function(x) {
  formatC(round(as.numeric(x)), format = "d", big.mark = ",")
}

format_additional_decimal <- function(x, digits = 2) {
  formatC(as.numeric(x), format = "f", digits = digits, big.mark = ",")
}

format_additional_quantity <- function(x, outcome) {
  ifelse(grepl("Rate", outcome), format_additional_decimal(x, 2), format_additional_whole(x))
}

render_additional_table <- function(variable_choice, caption, label, note, path) {
  variable_choice$Outcome <- row.names(variable_choice)
  variable_choice <- variable_choice %>% select(Outcome, everything())

  out <- variable_choice %>%
    mutate(
      `Relative Impact (%)` = sprintf("%.2f%s", `Relative Impact (%)`, pstars(`p-value`)),
      `Average Monthly Impact` = format_additional_quantity(`Average Monthly Impact`, Outcome),
      `Monthly Avg. Predicted` = format_additional_quantity(`Monthly Avg. Predicted`, Outcome),
      `Monthly Avg. Observed` = format_additional_quantity(`Monthly Avg. Observed`, Outcome),
      `YoY Actual Change (%)` = format_additional_decimal(`YoY Actual Change (%)`, 2),
      `p-value` = formatC(as.numeric(`p-value`), format = "f", digits = 4),
      `RMSE (priors)` = format_additional_quantity(`RMSE (priors)`, Outcome),
      Outcome = latex_escape_additional(Outcome)
    ) %>%
    select(
      Outcome, `Relative Impact (%)`, `Average Monthly Impact`,
      `Monthly Avg. Predicted`, `Monthly Avg. Observed`,
      `YoY Actual Change (%)`, `p-value`, `RMSE (priors)`
    )

  row_to_tex <- function(r) paste0(paste(r, collapse = " & "), " \\\\")
  header_line <- "Outcome & Relative Impact (\\%) & Avg. Monthly Impact & Monthly Avg. Predicted & Monthly Avg. Observed & YoY Actual Change (\\%) & p-value & RMSE (pre-period)\\\\"

  lines <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    paste0("\\caption{", caption, "}\\label{", label, "}"),
    "\\resizebox{\\textwidth}{!}{%",
    "\\begin{tabular}[t]{lrrrrrrr}",
    "\\toprule",
    header_line,
    "\\midrule",
    apply(out, 1, row_to_tex),
    "\\bottomrule",
    "\\end{tabular}",
    "}",
    "\\begin{minipage}{\\textwidth}",
    paste0("\\raggedright\\scriptsize \\textit{Note:} ", note),
    "\\end{minipage}",
    "\\end{table}"
  )

  writeLines(lines, path)
}

tourism_activity_specs <- data.frame(
  outcome_col = c(
    "traffic_capital_iceland",
    "rental_cars_iceland",
    "overnight_stays_iceland"
  ),
  label = c(
    "Capital-Area Road Traffic",
    "Registered Rental Cars",
    "Overnight Stays"
  ),
  stringsAsFactors = FALSE
)

tourism_activity_tables <- lapply(
  seq_len(nrow(tourism_activity_specs)),
  function(i) {
    run_named_impact(
      tourism_activity_specs$outcome_col[i],
      tourism_activity_specs$label[i],
      tourism_list
    )
  }
)

tourism_activity_table <- do.call(rbind, tourism_activity_tables)
render_additional_table(
  tourism_activity_table,
  "Additional Tourism Activity Effects of the WOW Air Bankruptcy",
  "tab:tourism_activity_impact",
  "Rows report counterfactual estimates for Icelandic visitor-adjacent activity measures. Capital-area road traffic measures road traffic in the capital region; registered rental cars measures the rental-car stock; overnight stays measures overnight stays in Iceland. Average monthly impact is measured in the original units of each row. Stars denote statistical significance: *** $p<0.01$, ** $p<0.05$, * $p<0.10$.",
  "Main Figures and Tables/tourism_activity_impact_table.tex"
)

demographic_labor_specs <- data.frame(
  outcome_col = c(
    "15_24_employed_iceland",
    "25_74_employed_iceland",
    "16_24_unemployed_iceland",
    "25_74_unemployed_iceland",
    "16_24_unemployment_rate_iceland",
    "25_74_unemployment_rate_iceland",
    "employment_net_tourism_national_iceland",
    "employment_net_tourism_immigrant_iceland",
    "tourism_aggregate_national_iceland",
    "tourism_aggregate_immigrant_iceland"
  ),
  label = c(
    "Employed Persons, Age 15--24",
    "Employed Persons, Age 25--74",
    "Unemployed Persons, Age 16--24",
    "Unemployed Persons, Age 25--74",
    "Unemployment Rate, Age 16--24",
    "Unemployment Rate, Age 25--74",
    "Employment Net of Tourism, National",
    "Employment Net of Tourism, Immigrant",
    "Tourism Employment, National",
    "Tourism Employment, Immigrant"
  ),
  stringsAsFactors = FALSE
)

demographic_labor_specs <- demographic_labor_specs %>%
  filter(outcome_col %in% colnames(month))

demographic_labor_tables <- lapply(
  seq_len(nrow(demographic_labor_specs)),
  function(i) {
    run_named_impact(
      demographic_labor_specs$outcome_col[i],
      demographic_labor_specs$label[i],
      labor_list
    )
  }
)

demographic_labor_table <- do.call(rbind, demographic_labor_tables)
render_additional_table(
  demographic_labor_table,
  "Demographic Labor-Market Effects of the WOW Air Bankruptcy",
  "tab:demographic_labor_impact",
  "Rows report available age and national-or-immigrant employment outcomes from the monthly data. Gender-specific employment fields were not present in the working monthly dataset. Average monthly impact is measured in workers for employment and unemployment rows and percentage points for unemployment-rate rows. Stars denote statistical significance: *** $p<0.01$, ** $p<0.05$, * $p<0.10$.",
  "Main Figures and Tables/demographic_labor_impact_table.tex"
)
