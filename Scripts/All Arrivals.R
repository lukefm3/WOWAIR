complete <- month[,c(tourism_list)]
complete2 <- as.data.frame(complete[4]) # change this to pick out your indicator,
#then just pick your dates
colnames(complete2) <- "arrivals_iceland"
# list of remaining columns
icelandcolumns <- grep('iceland', colnames(complete))
complete <- complete[,-c(icelandcolumns)]

complete <- cbind(complete,complete2)

complete <- complete %>% filter(date > '2013-12-31') %>% filter(date < '2019-12-31') # to do arrivals 
complete <- complete[ , colSums(is.na(complete))==0] %>% relocate(arrivals_iceland) %>% relocate(date)
temp<- ts(data = complete[-c(1)],
          start = 2014,
          end = 2020,
          frequency = 12) %>% head(-1)

pre.period <- as.yearmon(c("2014-01-01", "2019-01-01"))

post.period <- as.yearmon(c("2019-04-01", "2019-12-31"))

impact_temp <- CausalImpact(temp,pre.period,post.period, model.args=list(niter=5000, nseasons = 12, season.duration = 1))
paste("Estimated Relative Impact for Foreign Travel was ", impact_temp$summary$RelEffect[1])
foreign <- plot(impact_temp, "original") + ggtitle("Foreign Departures") + panel_theme + ylab("Departures")
foreign_t <- table_func(impact_temp, "Foreign Passengers")


complete <- month[,c(tourism_list)]
complete2 <- as.data.frame(complete[5]) # change this to pick out your indicator,
#then just pick your dates
colnames(complete2) <- "arrivals_iceland"
# list of remaining columns
icelandcolumns <- grep('iceland', colnames(complete))
complete <- complete[,-c(icelandcolumns)]

complete <- cbind(complete,complete2)

complete <- complete %>% filter(date > '2013-12-31') %>% filter(date < '2019-12-31') # to do arrivals 
complete <- complete[ , colSums(is.na(complete))==0] %>% relocate(arrivals_iceland) %>% relocate(date)
temp<- ts(data = complete[-c(1)],
          start = 2014,
          end = 2020,
          frequency = 12) %>% head(-1)

pre.period <- as.yearmon(c("2014-01-01", "2019-01-01"))

post.period <- as.yearmon(c("2019-04-01", "2019-12-31"))

impact_temp <- CausalImpact(temp,pre.period,post.period, model.args=list(niter=5000, nseasons = 12, season.duration = 1))
paste("Estimated Relative Impact for Domestic Travel was ", impact_temp$summary$RelEffect[1])
domestic <- plot(impact_temp, "original") + ggtitle("Icelandic Citizen Departures") + panel_theme + ylab("Departures")
national_t <- table_func(impact_temp, "Icelandic Passengers")


complete <- month[,c(tourism_list)]
complete2 <- as.data.frame(complete[6]) # change this to pick out your indicator,
#then just pick your dates
colnames(complete2) <- "arrivals_iceland"
# list of remaining columns
icelandcolumns <- grep('iceland', colnames(complete))
complete <- complete[,-c(icelandcolumns)]

complete <- cbind(complete,complete2)

complete <- complete %>% filter(date > '2013-12-31') %>% filter(date < '2019-12-31') # to do arrivals 
complete <- complete[ , colSums(is.na(complete))==0] %>% relocate(arrivals_iceland) %>% relocate(date)
temp<- ts(data = complete[-c(1)],
          start = 2014,
          end = 2020,
          frequency = 12) %>% head(-1)

pre.period <- as.yearmon(c("2014-01-01", "2019-01-01"))

post.period <- as.yearmon(c("2019-04-01", "2019-12-31"))

impact_temp <- CausalImpact(temp,pre.period,post.period, model.args=list(niter=5000, nseasons = 12, season.duration = 1))
paste("Estimated Relative Impact for Total Travel was ", impact_temp$summary$RelEffect[1])

total <- plot(impact_temp, "original") + ggtitle("Total Departures") + panel_theme + ylab("Departures")
total_t <- table_func(impact_temp, "Total Passengers")

#Combine Pointwise Graphs for all three above
passenger_impact<- grid.arrange(total, foreign, domestic)
ggsave("Main Figures and Tables/passengers_by_nationality.png",passenger_impact, width=8,height=5, units="in")

passenger_tables <- rbind(total_t, national_t, foreign_t)
passenger_tables$Indicator <- rownames(passenger_tables)
passenger_tables <- passenger_tables %>% select(Indicator, everything())

passenger_pstars <- function(p) {
  ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", "")))
}

format_whole <- function(x) {
  formatC(round(as.numeric(x)), format = "d", big.mark = ",")
}

passenger_display <- passenger_tables %>%
  rename(
    `Passenger Series` = Indicator,
    `RMSE (pre-period)` = `RMSE (priors)`
  ) %>%
  mutate(
    `Relative Impact (%)` = sprintf(
      "%.2f%s",
      as.numeric(`Relative Impact (%)`),
      passenger_pstars(as.numeric(`p-value`))
    ),
    `9-Month Cumulative Impact` = format_whole(`9-Month Cumulative Impact`),
    `95% CI Upper` = format_whole(`95% CI Upper`),
    `95% CI Lower` = format_whole(`95% CI Lower`),
    `p-value` = formatC(as.numeric(`p-value`), format = "f", digits = 4),
    `RMSE (pre-period)` = format_whole(`RMSE (pre-period)`)
  ) %>%
  select(
    `Passenger Series`, `Relative Impact (%)`, `9-Month Cumulative Impact`,
    `95% CI Upper`, `95% CI Lower`, `p-value`, `RMSE (pre-period)`
  )

latex_out <- kable(
  passenger_display,
  format = "latex",
  booktabs = TRUE,
  escape = FALSE,
  row.names = FALSE,
  col.names = c(
    "Passenger Series", "Relative Impact (\\%)", "9-Month Cumulative Impact",
    "95\\% CI Upper", "95\\% CI Lower", "p-value", "RMSE (pre-period)"
  ),
  align = c("l", rep("r", 6))
)

# Split into lines
latex_lines <- strsplit(latex_out, "\n")[[1]]

# Remove only exact wrapper/caption/centering lines
latex_lines <- latex_lines[
  !grepl("^\\\\begin\\{table\\}", latex_lines) &
    !grepl("^\\\\end\\{table\\}", latex_lines) &
    !grepl("^\\\\centering$", latex_lines) &
    !grepl("^\\\\caption\\{", latex_lines)
]

# Save cleaned table
writeLines(
  latex_lines,
  "Main Figures and Tables/passenger_nationality.tex"
)

passenger_table <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Passenger Effects of the WOW Air Bankruptcy}\\label{tab:passenger_impact}",
  "\\resizebox{\\textwidth}{!}{%",
  latex_lines,
  "}",
  "\\begin{minipage}{\\textwidth}",
  "\\raggedright\\scriptsize \\textit{Note:} Relative impact is the April--December 2019 difference between observed and counterfactual departures divided by the counterfactual. Stars denote statistical significance: *** $p<0.01$, ** $p<0.05$, * $p<0.10$.",
  "\\end{minipage}",
  "\\end{table}"
)

writeLines(
  passenger_table,
  "Main Figures and Tables/passenger_impact_table.tex"
)

