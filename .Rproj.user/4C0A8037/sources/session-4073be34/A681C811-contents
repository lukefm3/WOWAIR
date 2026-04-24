#Section 2: Labor Level Impacts

#National First:
  
  #{r Total Taxable Wages Economy Wide}
complete <- month[,c(labor_list)] # Restart the formula
complete2 <- as.data.frame(complete[,15]) # change this to pick out your indicator, then just pick your dates
colnames(complete2) <- "taxable_wages_iceland"
# list of remaining columns
icelandcolumns <- grep('iceland', colnames(complete))
complete <- complete[,-c(icelandcolumns)]

complete <- cbind(complete,complete2)

complete <- complete %>% filter(date > '2013-12-31') %>% filter(date < '2019-12-31') 

complete <- complete[ , colSums(is.na(complete))==0] %>% relocate(taxable_wages_iceland) %>% relocate(date)

temp <- ts(data = complete[-c(1)],
           start = 2014,
           end = 2020,
           frequency = 12) %>% head(-1)
pre.period <- as.yearmon(c("2014-01-01", "2019-01-01"))
post.period <- as.yearmon(c("2019-04-01", "2020-01-01"))

impact_temp <- CausalImpact(temp,pre.period,post.period, model.args=list(niter=5000, nseasons = 12, season.duration = 1))

#plot(impact_temp) + ggtitle("Taxable Wages Post-Bankruptcy")
w1 <- plot(impact_temp, "pointwise") + ggtitle("Taxable Wages") + panel_theme + ylab("ISK (M)")

w1_table <- table_func_avg_yoy(impact_temp, "Taxable Wages (Full Economy)")

#

#{r Total Employed Economy Wide}
complete <- month[,c(labor_list)] # Restart the formula
complete2 <- as.data.frame(complete[,36]) # change this to pick out your indicator, then just pick your dates
colnames(complete2) <- "Employees_total_iceland"
# list of remaining columns
icelandcolumns <- grep('iceland', colnames(complete))
complete <- complete[,-c(icelandcolumns)]

complete <- cbind(complete,complete2)

complete <- complete %>% filter(date > '2013-12-31') %>% filter(date < '2019-12-31') 

complete <- complete[ , colSums(is.na(complete))==0] %>% relocate(Employees_total_iceland) %>% relocate(date)

temp <- ts(data = complete[-c(1)],
           start = 2014,
           end = 2020,
           frequency = 12) %>% head(-1)
pre.period <- as.yearmon(c("2014-01-01", "2019-01-01"))
post.period <- as.yearmon(c("2019-04-01", "2020-01-01"))

impact_temp <- CausalImpact(temp,pre.period,post.period, model.args=list(niter=5000, nseasons = 12, season.duration = 1))

e1 <- plot(impact_temp, "pointwise") + ggtitle("Total Employees") +   panel_theme + ylab("Persons")

e1_table <- table_func_avg_yoy(impact_temp, "Total Employees")

#

#{r Total Unemployed Economy Wide}
complete <- month[,c(labor_list)] # Restart the formula
complete2 <- as.data.frame(complete[,35]) # change this to pick out your indicator, then just pick your dates
colnames(complete2) <- "taxable_wages_iceland"
# list of remaining columns
icelandcolumns <- grep('iceland', colnames(complete))
complete <- complete[,-c(icelandcolumns)]

complete <- cbind(complete,complete2)

complete <- complete %>% filter(date > '2013-12-31') %>% filter(date < '2019-12-31') 

complete <- complete[ , colSums(is.na(complete))==0] %>% relocate(taxable_wages_iceland) %>% relocate(date)

temp <- ts(data = complete[-c(1)],
           start = 2014,
           end = 2020,
           frequency = 12) %>% head(-1)
pre.period <- as.yearmon(c("2014-01-01", "2019-01-01"))
post.period <- as.yearmon(c("2019-04-01", "2020-01-01"))

impact_temp <- CausalImpact(temp,pre.period,post.period, model.args=list(niter=5000, nseasons = 12, season.duration = 1))

#plot(impact_temp) + ggtitle("Taxable Wages Post-Bankruptcy")
u1 <- plot(impact_temp, "pointwise") + ggtitle("Unemployed Persons") +   panel_theme + ylab("ISK (M)")

u1_table <- table_func_avg_yoy(impact_temp, "Unemployed Persons")

#

#{r Total Unemployment Rate Economy Wide}

complete <- month[,c(labor_list)] # Restart the formula
complete2 <- as.data.frame(complete[,2]) # change this to pick out your indicator, then just pick your dates
colnames(complete2) <- "Employees_total_iceland"
# list of remaining columns
icelandcolumns <- grep('iceland', colnames(complete))
complete <- complete[,-c(icelandcolumns)]

complete <- cbind(complete,complete2)

complete <- complete %>% filter(date > '2013-12-31') %>% filter(date < '2019-12-31') 

complete <- complete[ , colSums(is.na(complete))==0] %>% relocate(Employees_total_iceland) %>% relocate(date)

temp <- ts(data = complete[-c(1)],
           start = 2014,
           end = 2020,
           frequency = 12) %>% head(-1)
pre.period <- as.yearmon(c("2014-01-01", "2019-01-01"))
post.period <- as.yearmon(c("2019-04-01", "2020-01-01"))

impact_temp <- CausalImpact(temp,pre.period,post.period, model.args=list(niter=5000, nseasons = 12, season.duration = 1))

u2 <- plot(impact_temp, "pointwise") + ggtitle("Unemployment Rate") +   panel_theme + ylab("Unemployment Rate (%)")

u2_table <- table_func_avg_yoy(impact_temp, "Unemployment Rate")

#

#{r Total Employed Net of Air Economy Wide}
complete <- month %>%
  mutate(net_tourism_employees_iceland = employees_tourism_iceland - air_employees_iceland) %>%
  select(any_of(labor_list), net_tourism_employees_iceland)

complete2 <- as.data.frame(complete[,151]) # change this to pick out your indicator, then just pick your dates
colnames(complete2) <- "Employees_total_iceland"
# list of remaining columns
icelandcolumns <- grep('iceland', colnames(complete))
complete <- complete[,-c(icelandcolumns)]

complete <- cbind(complete,complete2)

complete <- complete %>% filter(date > '2013-12-31') %>% filter(date < '2019-12-31') 

complete <- complete[ , colSums(is.na(complete))==0] %>% relocate(Employees_total_iceland) %>% relocate(date)

temp <- ts(data = complete[-c(1)],
           start = 2014,
           end = 2020,
           frequency = 12) %>% head(-1)
pre.period <- as.yearmon(c("2014-01-01", "2019-01-01"))
post.period <- as.yearmon(c("2019-04-01", "2020-01-01"))

impact_temp <- CausalImpact(temp,pre.period,post.period, model.args=list(niter=5000, nseasons = 12, season.duration = 1))

enet <- plot(impact_temp, "pointwise") + ggtitle("Total Employees") +    panel_theme + ylab("Persons")

enet_table <- table_func_avg_yoy(impact_temp, "Total Employees Not in Airline Industry")

#

#{r National Labor Impact}
employee_impact<- grid.arrange(u1,u2, ncol=2)

ggsave("Main Figures and Tables/national_labor_plot.png",employee_impact, width=10,height=5, units="in")
variable_choice <- rbind(u1_table, u2_table)


variable_choice$Indicator <- rownames(variable_choice)
variable_choice <- variable_choice %>% select(Indicator, everything())

kable(variable_choice, caption = "Forecasted National Level Labor Impact", format = "latex", row.names = FALSE)
writeLines(kable(variable_choice, caption = "Forecasted National Level Labor Impact", format = "latex", row.names = FALSE), "Main Figures and Tables/national_labor_table.md")

# add stars to the "Relative Impact (%)" column (keep numeric shown to 2 d.p.)
vc_out <- variable_choice %>%
  mutate(`Relative Impact (%)` =
           sprintf("%.2f%s", `Relative Impact (%)`, pstars(`p-value`)))

# build LaTeX table; escape=FALSE so the asterisks are kept as-is
latex_tbl <- vc_out %>%
  kable(format = "latex", booktabs = TRUE, escape = FALSE,
        caption = "Impacts and Fit Statistics",
        col.names = c("Indicator","Relative Impact (%)",
                      "Avg. Monthly Impact",
                      "Monthly Avg. Predicted","Monthly Avg. Observed",
                      "YoY Actual Change (%)","p-value","RMSE (priors)"),
        align = c("l", rep("r", 7))) %>%
  kable_styling(latex_options = c("hold_position", "striped"))

# write to a .tex file you can \input{} in LaTeX
writeLines(latex_tbl, "Main Figures and Tables/Labor_Impacts_National.tex")
#

#Figure consists of w1,e1,u1,u2

#{r Tourism Wages tourism}
complete <- month[,c(labor_list)] # Restart the formula
complete2 <- as.data.frame(complete[,21]) # change this to pick out your indicator, then just pick your dates
colnames(complete2) <- "taxable_wages_iceland"
# list of remaining columns
icelandcolumns <- grep('iceland', colnames(complete))
complete <- complete[,-c(icelandcolumns)]

complete <- cbind(complete,complete2)

complete <- complete %>% filter(date > '2013-12-31') %>% filter(date < '2019-12-31') 

complete <- complete[ , colSums(is.na(complete))==0] %>% relocate(taxable_wages_iceland) %>% relocate(date)

temp <- ts(data = complete[-c(1)],
           start = 2014,
           end = 2020,
           frequency = 12) %>% head(-1)
pre.period <- as.yearmon(c("2014-01-01", "2019-01-01"))
post.period <- as.yearmon(c("2019-04-01", "2020-01-01"))

impact_temp <- CausalImpact(temp,pre.period,post.period, model.args=list(niter=5000, nseasons = 12, season.duration = 1))


w2 <- plot(impact_temp, "pointwise") + 
  ggtitle("Tourism Wages") + 
  panel_theme + ylab("ISK (M)")


w2_table <- table_func_avg_yoy(impact_temp, "Tourism Wages")
#

#{r Tourism Employees}
complete <- month[,c(labor_list)] # Restart the formula
complete2 <- as.data.frame(complete[,22]) # change this to pick out your indicator, then just pick your dates
colnames(complete2) <- "tourism_employees_iceland"
# list of remaining columns
icelandcolumns <- grep('iceland', colnames(complete))
complete <- complete[,-c(icelandcolumns)]

complete <- cbind(complete,complete2)

complete <- complete %>% filter(date > '2013-12-31') %>% filter(date < '2019-12-31') 

complete <- complete[ , colSums(is.na(complete))==0] %>% relocate(tourism_employees_iceland) %>% relocate(date)

temp <- ts(data = complete[-c(1)],
           start = 2014,
           end = 2020,
           frequency = 12) %>% head(-1)
pre.period <- as.yearmon(c("2014-01-01", "2019-01-01"))
post.period <- as.yearmon(c("2019-04-01", "2020-01-01"))

impact_temp <- CausalImpact(temp,pre.period,post.period, model.args=list(niter=5000, nseasons = 12, season.duration = 1))


e2 <- plot(impact_temp, "pointwise") + ggtitle("Tourism Employees") +    panel_theme + ylab("Persons")

e2_table <- table_func_avg_yoy(impact_temp, "Tourism Employees")

#

#{r Transportation Employees}
complete <- month[,c(labor_list)] # Restart the formula
complete2 <- as.data.frame(complete[,18]) # change this to pick out your indicator, then just pick your dates
colnames(complete2) <- "temp"
# list of remaining columns
icelandcolumns <- grep('iceland', colnames(complete))
complete <- complete[,-c(icelandcolumns)]

complete <- cbind(complete,complete2)

complete <- complete %>% filter(date > '2013-12-31') %>% filter(date < '2019-12-31') 

complete <- complete[ , colSums(is.na(complete))==0] %>% relocate(temp) %>% relocate(date)

temp <- ts(data = complete[-c(1)],
           start = 2014,
           end = 2020,
           frequency = 12) %>% head(-1)
pre.period <- as.yearmon(c("2014-01-01", "2019-01-01"))
post.period <- as.yearmon(c("2019-04-01", "2020-01-01"))

impact_temp <- CausalImpact(temp,pre.period,post.period, model.args=list(niter=5000, nseasons = 12, season.duration = 1))

e3 <- plot(impact_temp, "pointwise") + ggtitle("Transportation Employees") + panel_theme + ylab("Persons")

e3_table <- table_func_avg_yoy(impact_temp, "Transportation Employees")

#

#{r Transportation Wages}
complete <- month[,c(labor_list)] # Restart the formula
complete2 <- as.data.frame(complete[,17]) # change this to pick out your indicator, then just pick your dates
colnames(complete2) <- "temp"
# list of remaining columns
icelandcolumns <- grep('iceland', colnames(complete))
complete <- complete[,-c(icelandcolumns)]

complete <- cbind(complete,complete2)

complete <- complete %>% filter(date > '2013-12-31') %>% filter(date < '2019-12-31') 

complete <- complete[ , colSums(is.na(complete))==0] %>% relocate(temp) %>% relocate(date)

temp <- ts(data = complete[-c(1)],
           start = 2014,
           end = 2020,
           frequency = 12) %>% head(-1)
pre.period <- as.yearmon(c("2014-01-01", "2019-01-01"))
post.period <- as.yearmon(c("2019-04-01", "2020-01-01"))

impact_temp <- CausalImpact(temp,pre.period,post.period, model.args=list(niter=5000, nseasons = 12, season.duration = 1))

w3 <- plot(impact_temp, "pointwise") + ggtitle("Transportation Wages") + panel_theme + ylab("ISK (M)")

w3_table <- table_func_avg_yoy(impact_temp, "Transportation Wages")

#

#{r Transportation Wages Net of Airline}
complete <- month[,c(labor_list)] # Restart the formula
complete2 <- as.data.frame(complete[,17]) # change this to pick out your indicator, then just pick your dates
colnames(complete2) <- "temp"
# list of remaining columns
icelandcolumns <- grep('iceland', colnames(complete))
complete <- complete[,-c(icelandcolumns)]

complete <- cbind(complete,complete2)

complete <- complete %>% filter(date > '2013-12-31') %>% filter(date < '2019-12-31') 

complete <- complete[ , colSums(is.na(complete))==0] %>% relocate(temp) %>% relocate(date)

temp <- ts(data = complete[-c(1)],
           start = 2014,
           end = 2020,
           frequency = 12) %>% head(-1)
pre.period <- as.yearmon(c("2014-01-01", "2019-01-01"))
post.period <- as.yearmon(c("2019-04-01", "2020-01-01"))

impact_temp <- CausalImpact(temp,pre.period,post.period, model.args=list(niter=5000, nseasons = 12, season.duration = 1))

w3 <- plot(impact_temp, "pointwise") + ggtitle("Transportation Wages") + panel_theme + ylab("ISK (M)")

w3_table <- table_func_avg_yoy(impact_temp, "Transportation Wages")

#

#{r Employees net tourism}
complete <- month[,c(labor_list)] # Restart the formula
complete2 <- as.data.frame(complete[,c(36,22)]) # change this to pick out your indicator, then just pick your dates
complete2 <- complete2 %>% mutate(
  net_tourism_employees_iceland = complete2[,1]- complete2[,2]
) 
complete2 <- complete2 %>% select(net_tourism_employees_iceland)
colnames(complete2) <- "Employees_total_iceland"
# list of remaining columns
icelandcolumns <- grep('iceland', colnames(complete))
complete <- complete[,-c(icelandcolumns)]

complete <- cbind(complete,complete2)

complete <- complete %>% filter(date > '2013-12-31') %>% filter(date < '2019-12-31') 

complete <- complete[ , colSums(is.na(complete))==0] %>% relocate(Employees_total_iceland) %>% relocate(date)

temp <- ts(data = complete[-c(1)],
           start = 2014,
           end = 2020,
           frequency = 12) %>% head(-1)
pre.period <- as.yearmon(c("2014-01-01", "2019-01-01"))
post.period <- as.yearmon(c("2019-04-01", "2020-01-01"))

impact_temp <- CausalImpact(temp,pre.period,post.period, model.args=list(niter=5000, nseasons = 12, season.duration = 1))

temp1 <- plot(impact_temp, "pointwise") + ggtitle("Total Employees") +  panel_theme + ylab("Persons")

temp1_table <- table_func_avg_yoy(impact_temp, "Total Non-Tourism Employees")

#


#{r Taxable Wage Accommodation}

complete <- month[,c(labor_list)] # Restart the formula
complete2 <- as.data.frame(complete[,19]) # change this to pick out your indicator, then just pick your dates
colnames(complete2) <- "temp"
# list of remaining columns
icelandcolumns <- grep('iceland', colnames(complete))
complete <- complete[,-c(icelandcolumns)]

complete <- cbind(complete,complete2)

complete <- complete %>% filter(date > '2013-12-31') %>% filter(date < '2019-12-31') 

complete <- complete[ , colSums(is.na(complete))==0] %>% relocate(temp) %>% relocate(date)

temp <- ts(data = complete[-c(1)],
           start = 2014,
           end = 2020,
           frequency = 12) %>% head(-1)
pre.period <- as.yearmon(c("2014-01-01", "2019-01-01"))
post.period <- as.yearmon(c("2019-04-01", "2020-01-01"))

impact_temp <- CausalImpact(temp,pre.period,post.period, model.args=list(niter=5000, nseasons = 12, season.duration = 1))

w4 <- plot(impact_temp, "pointwise") + ggtitle("Accommodation Wages") +  panel_theme + ylab("ISK (M)")

w4_table <- table_func_avg_yoy(impact_temp, "Accommodation Wages")

#

#{r Accommodation Employees}
complete <- month[,c(labor_list)] # Restart the formula
complete2 <- as.data.frame(complete[,20]) # change this to pick out your indicator, then just pick your dates
colnames(complete2) <- "temp"
# list of remaining columns
icelandcolumns <- grep('iceland', colnames(complete))
complete <- complete[,-c(icelandcolumns)]

complete <- cbind(complete,complete2)

complete <- complete %>% filter(date > '2013-12-31') %>% filter(date < '2019-12-31') 

complete <- complete[ , colSums(is.na(complete))==0] %>% relocate(temp) %>% relocate(date)

temp <- ts(data = complete[-c(1)],
           start = 2014,
           end = 2020,
           frequency = 12) %>% head(-1)
pre.period <- as.yearmon(c("2014-01-01", "2019-01-01"))
post.period <- as.yearmon(c("2019-04-01", "2020-01-01"))

impact_temp <- CausalImpact(temp,pre.period,post.period)

e4 <- plot(impact_temp, "pointwise") + ggtitle("Accommodation Employees") +  panel_theme + ylab("Persons")
e4_table <- table_func_avg_yoy(impact_temp, "Accommodation Employees")

#

#{r Tourism Minus Air Employees}
complete <- month %>%
  mutate(net_tourism_employees_iceland = employees_tourism_iceland - air_employees_iceland) %>%
  select(any_of(labor_list), net_tourism_employees_iceland)

complete2 <- as.data.frame(complete[,151]) # change this to pick out your indicator, then just pick your dates
colnames(complete2) <- "tourism_employees_iceland"
# list of remaining columns
icelandcolumns <- grep('iceland', colnames(complete))
complete <- complete[,-c(icelandcolumns)]

complete <- cbind(complete,complete2)

complete <- complete %>% filter(date > '2013-12-31') %>% filter(date < '2019-12-31') 

complete <- complete[ , colSums(is.na(complete))==0] %>% relocate(tourism_employees_iceland) %>% relocate(date)

temp <- ts(data = complete[-c(1)],
           start = 2014,
           end = 2020,
           frequency = 12) %>% head(-1)
pre.period <- as.yearmon(c("2014-01-01", "2019-01-01"))
post.period <- as.yearmon(c("2019-04-01", "2020-01-01"))

impact_temp <- CausalImpact(temp,pre.period,post.period, model.args=list(niter=5000, nseasons = 12, season.duration = 1))


net_employees <- plot(impact_temp, "pointwise") + ggtitle("Tourism Employees Excl. Airline") + panel_theme + ylab("Persons")

net_employees_table <- table_func_avg_yoy(impact_temp, "Tourism Employees Excl. Airline")


#

#{r Sectoral Employee Impact}
employee_impact<- grid.arrange(e2,e3,net_employees, e4,w2 ,w3,w4,ncol=2)

ggsave(  "Main Figures and Tables/sectoral_labor_impact_plot.png",
         employee_impact,
         width = 12,
         height = 8.5,
         units = "in",
         dpi = 500
)

variable_choice <- rbind(e2_table,net_employees_table,e3_table, e4_table)

variable_choice$Indicator <- rownames(variable_choice)
variable_choice <- variable_choice %>% select(Indicator, everything())

kable(variable_choice, caption = "Forecasted Employment Growth Impact", format = "markdown", row.names = FALSE)
writeLines(kable(variable_choice, caption = "Forecasted Employment Growth Impact", format = "markdown", row.names = FALSE), "Main Figures and Tables/sectoral_labor_impact_table.md")


# add stars to the "Relative Impact (%)" column (keep numeric shown to 2 d.p.)
vc_out <- variable_choice %>%
  mutate(`Relative Impact (%)` =
           sprintf("%.2f%s", `Relative Impact (%)`, pstars(`p-value`)))

# build LaTeX table; escape=FALSE so the asterisks are kept as-is
latex_tbl <- vc_out %>%
  kable(format = "latex", booktabs = TRUE, escape = FALSE,
        caption = "Impacts and Fit Statistics",
        col.names = c("Indicator","Relative Impact (%)",
                      "Avg. Monthly Impact",
                      "Monthly Avg. Predicted","Monthly Avg. Observed",
                      "YoY Actual Change (%)","p-value","RMSE (priors)"),
        align = c("l", rep("r", 7))) %>%
  kable_styling(latex_options = c("hold_position", "striped"))

# write to a .tex file you can \input{} in LaTeX
writeLines(latex_tbl, "Main Figures and Tables/sector_impacts.tex")

#

#{r Wages}
employee_impact<- grid.arrange(w2,w3,w4,ncol=2)

ggsave("Main Figures and Tables/wage_impact_plot.png",employee_impact, width=10,height=5, units="in")

variable_choice <- rbind(w1_table, w2_table, w3_table, w4_table)

variable_choice$Indicator <- rownames(variable_choice)
variable_choice <- variable_choice %>% select(Indicator, everything())

kable(variable_choice, caption = "Forecasted Employment Growth Impact", format = "markdown", row.names = FALSE)
writeLines(kable(variable_choice, caption = "Forecasted Employment Growth Impact", format = "markdown", row.names = FALSE), "Main Figures and Tables/wage_impact_table.md")


# add stars to the "Relative Impact (%)" column (keep numeric shown to 2 d.p.)
vc_out <- variable_choice %>%
  mutate(`Relative Impact (%)` =
           sprintf("%.2f%s", `Relative Impact (%)`, pstars(`p-value`)))

# build LaTeX table; escape=FALSE so the asterisks are kept as-is
latex_tbl <- vc_out %>%
  kable(format = "latex", booktabs = TRUE, escape = FALSE,
        caption = "Impacts and Fit Statistics",
        col.names = c("Indicator","Relative Impact (%)",
                      "Avg. Monthly Impact",
                      "Monthly Avg. Predicted","Monthly Avg. Observed",
                      "YoY Actual Change (%)","p-value","RMSE (priors)"),
        align = c("l", rep("r", 7))) %>%
  kable_styling(latex_options = c("hold_position", "striped"))

# write to a .tex file you can \input{} in LaTeX
writeLines(latex_tbl, "Main Figures and Tables/wage_impacts.tex")
#


#Notes on Sources: 
##  Section 1 Wages & Employment: TEK02003: Taxable wage & number employed by economic activity
#Section 2: Further Tourism Breakdown: FYR03004
#Section 3: Registered Employe Persons: vin10001: Select Domestic & Abroad Employed...
#* All Age employed, all age unemployed- need to clarify, not references   
#Section 4: Ages Breakdown - this is the survey data
#Section5: Tourism Further Breakdown: SAM08051 taken from statbank, Name of file: Number of employees in activities related to tourism 2008

#Sectoral Employees These are NOT estimated from Surveys. 

#{r air_employees_iceland}

complete <- month[,c(labor_list)] # Restart the formula
complete2 <- as.data.frame(complete[,48]) # change this to pick out your indicator, then just pick your dates
colnames(complete2) <- "temp"
# list of remaining columns
icelandcolumns <- grep('iceland', colnames(complete))
complete <- complete[,-c(icelandcolumns)]

complete <- cbind(complete,complete2)

complete <- complete %>% filter(date > '2013-12-31') %>% filter(date < '2019-12-31') 

complete <- complete[ , colSums(is.na(complete))==0] %>% relocate(temp) %>% relocate(date)

temp <- ts(data = complete[-c(1)],
           start = 2014,
           end = 2020,
           frequency = 12) %>% head(-1)
pre.period <- as.yearmon(c("2014-01-01", "2019-01-01"))
post.period <- as.yearmon(c("2019-04-01", "2020-01-01"))

impact_temp <- CausalImpact(temp,pre.period,post.period, model.args=list(niter=5000, nseasons = 12, season.duration = 1))

plot(impact_temp) + ggtitle("Airline Employees Post-Bankruptcy")
e6 <- plot(impact_temp, "pointwise") + ggtitle("Air Employees") +  panel_theme + ylab("Persons")

e6_table <- table_func_avg_yoy(impact_temp, "Air Transport Employees")
#

#{r Accommodation Employees _ Type2}

complete <- month[,c(labor_list)] # Restart the formula
complete2 <- as.data.frame(complete[,49]) # change this to pick out your indicator, then just pick your dates
colnames(complete2) <- "temp"
# list of remaining columns
icelandcolumns <- grep('iceland', colnames(complete))
complete <- complete[,-c(icelandcolumns)]

complete <- cbind(complete,complete2)

complete <- complete %>% filter(date > '2013-12-31') %>% filter(date < '2019-12-31') 

complete <- complete[ , colSums(is.na(complete))==0] %>% relocate(temp) %>% relocate(date)

temp <- ts(data = complete[-c(1)],
           start = 2014,
           end = 2020,
           frequency = 12) %>% head(-1)
pre.period <- as.yearmon(c("2014-01-01", "2019-01-01"))
post.period <- as.yearmon(c("2019-04-01", "2020-01-01"))

impact_temp <- CausalImpact(temp,pre.period,post.period, model.args=list(niter=5000, nseasons = 12, season.duration = 1))

e7 <- plot(impact_temp, "pointwise") + ggtitle("Accommodation Employees") +  panel_theme + ylab("Persons")
e7_table <- table_func_avg_yoy(impact_temp, "Accommodation Employees")

#

#{r food_bev_employees_iceland}
complete <- month[,c(labor_list)] # Restart the formula
complete2 <- as.data.frame(complete[,50]) # change this to pick out your indicator, then just pick your dates
colnames(complete2) <- "temp"
# list of remaining columns
icelandcolumns <- grep('iceland', colnames(complete))
complete <- complete[,-c(icelandcolumns)]

complete <- cbind(complete,complete2)

complete <- complete %>% filter(date > '2013-12-31') %>% filter(date < '2019-12-31') 

complete <- complete[ , colSums(is.na(complete))==0] %>% relocate(temp) %>% relocate(date)

temp <- ts(data = complete[-c(1)],
           start = 2014,
           end = 2020,
           frequency = 12) %>% head(-1)
pre.period <- as.yearmon(c("2014-01-01", "2019-01-01"))
post.period <- as.yearmon(c("2019-04-01", "2020-01-01"))

impact_temp <- CausalImpact(temp,pre.period,post.period, model.args=list(niter=5000, nseasons = 12, season.duration = 1))

e8 <- plot(impact_temp, "pointwise") + ggtitle("Food & Beverage Employees") +  panel_theme + ylab("Persons")
e8_table <- table_func_avg_yoy(impact_temp, "Food & Beverage Employees")
#

#{r tour_services_employees_iceland }

complete <- month # Restart the formula
complete2 <- as.data.frame(complete[,71]) # change this to pick out your indicator, then just pick your dates
colnames(complete2) <- "temp"
# list of remaining columns
icelandcolumns <- grep('iceland', colnames(complete))
complete <- complete[,-c(icelandcolumns)]

complete <- cbind(complete,complete2)

complete <- complete %>% filter(date > '2013-12-31') %>% filter(date < '2019-12-31') 

complete <- complete[ , colSums(is.na(complete))==0] %>% relocate(temp) %>% relocate(date)

temp <- ts(data = complete[-c(1)],
           start = 2014,
           end = 2020,
           frequency = 12) %>% head(-1)
pre.period <- as.yearmon(c("2014-01-01", "2019-01-01"))
post.period <- as.yearmon(c("2019-04-01", "2020-01-01"))

impact_temp <- CausalImpact(temp,pre.period,post.period, model.args=list(niter=5000, nseasons = 12, season.duration = 1))

e9 <- plot(impact_temp, "pointwise") + ggtitle("Tour Services Employees") +  panel_theme + ylab("Persons")
e9_table <- table_func_avg_yoy(impact_temp, "Tour Services Employees")


#

#{r other_tourism_employees_iceland}
#72
complete <- month # Restart the formula
complete2 <- as.data.frame(complete[,72]) # change this to pick out your indicator, then just pick your dates
colnames(complete2) <- "temp"
# list of remaining columns
icelandcolumns <- grep('iceland', colnames(complete))
complete <- complete[,-c(icelandcolumns)]

complete <- cbind(complete,complete2)

complete <- complete %>% filter(date > '2011-12-31') %>% filter(date < '2019-12-31') 

complete <- complete[ , colSums(is.na(complete))==0] %>% relocate(temp) %>% relocate(date)

temp <- ts(data = complete[-c(1)],
           start = 2014,
           end = 2020,
           frequency = 12) %>% head(-1)
pre.period <- as.yearmon(c("2014-01-01", "2019-01-01"))
post.period <- as.yearmon(c("2019-04-01", "2020-01-01"))

impact_temp <- CausalImpact(temp,pre.period,post.period, model.args=list(niter=5000, nseasons = 12, season.duration = 1))

e10 <- plot(impact_temp, "pointwise") + ggtitle("Other Tourism Employees") +   panel_theme + ylab("Persons")

e10_table <- table_func_avg_yoy(impact_temp, "Other Tourism Employees")


#

#{r Tourism Labor Impact}
employee_impact<- grid.arrange(e2,e6,net_employees, e7,e8,e9,e10,ncol=2)

ggsave("Main Figures and Tables/tourism_labor_impact_plot.png",employee_impact, width=10,height=5, units="in")

variable_choice <- rbind(e6_table, e7_table, e8_table, e9_table, e10_table)

variable_choice$Indicator <- rownames(variable_choice)
variable_choice <- variable_choice %>% select(Indicator, everything())

kable(variable_choice, caption = "Forecasted Tourism Labor Impact", format = "markdown", row.names = FALSE)
writeLines(kable(variable_choice, caption = "Forecasted Tourism Labor Impact", format = "markdown", row.names = FALSE), "Main Figures and Tables/tourism_labor_impact_table.md")


# add stars to the "Relative Impact (%)" column (keep numeric shown to 2 d.p.)
vc_out <- variable_choice %>%
  mutate(`Relative Impact (%)` =
           sprintf("%.2f%s", `Relative Impact (%)`, pstars(`p-value`)))

# build LaTeX table; escape=FALSE so the asterisks are kept as-is
latex_tbl <- vc_out %>%
  kable(format = "latex", booktabs = TRUE, escape = FALSE,
        caption = "Impacts and Fit Statistics",
        col.names = c("Indicator","Relative Impact (%)",
                      "Avg. Monthly Impact",
                      "Monthly Avg. Predicted","Monthly Avg. Observed",
                      "YoY Actual Change (%)","p-value","RMSE (priors)"),
        align = c("l", rep("r", 7))) %>%
  kable_styling(latex_options = c("hold_position", "striped"))

# write to a .tex file you can \input{} in LaTeX
writeLines(latex_tbl, "Main Figures and Tables/tourism_sector_impacts.tex")

#


#Comprehensive Table For Labor Impacts: labor_impact_2

#{r}
library(stringr)

# -------- keep your helpers (unchanged) --------
strip_tex_to_rows <- function(path) {
  txt <- readLines(path, warn = FALSE)
  
  b <- grep("\\\\begin\\{tabular\\}", txt)[1]
  e <- grep("\\\\end\\{tabular\\}",  txt)[1]
  if (is.na(b) || is.na(e)) stop("Couldn't find tabular in: ", path)
  txt <- txt[(b + 1):(e - 1)]
  
  drop_pat <- "^\\s*\\\\(toprule|midrule|bottomrule|addlinespace|caption|centering|hline|cline|rowcolor)"
  txt <- txt[!grepl(drop_pat, txt)]
  txt <- sub("%.*$", "", txt)
  txt <- trimws(txt)
  txt <- txt[nzchar(txt)]
  
  while (any(grepl("\\\\cellcolor\\{[^}]+\\}\\{[^}]*\\}", txt))) {
    txt <- gsub("\\\\cellcolor\\{[^}]+\\}\\{([^}]*)\\}", "\\1", txt)
  }
  
  txt <- txt[grepl("&", txt) & grepl("\\\\\\\\", txt)]
  txt <- gsub("\\\\\\\\$", "", txt)
  txt
}

split_row <- function(line) {
  parts <- strsplit(line, "\\s*&\\s*", perl = TRUE)[[1]]
  parts <- gsub("^\\{(.*)\\}$", "\\1", parts)
  parts <- trimws(parts)
  
  # Repair "Food" + "Beverage Employees" split across two columns
  if (length(parts) == 9) {
    parts <- c(paste(parts[1], parts[2], sep = " \\& "), parts[3:9])
  }
  if (length(parts) > 8) parts <- parts[1:8]
  parts
}

tex_tab_to_df <- function(path) {
  rows  <- strip_tex_to_rows(path)
  cells <- lapply(rows, split_row)
  
  cells <- Filter(function(v) length(v) >= 8 && v[1] != "Indicator", cells)
  cells <- lapply(cells, function(v) if (length(v) < 8) c(v, rep("", 8 - length(v))) else v)
  
  m  <- do.call(rbind, cells)
  df <- as.data.frame(m, stringsAsFactors = FALSE)
  names(df) <- c(
    "Indicator",
    "Relative Impact (%)",
    "Average Monthly Impact",
    "Monthly Avg. Predicted",
    "Monthly Avg. Observed",
    "YoY Actual Change (%)",
    "p-value",
    "RMSE (priors)"
  )
  
  # normalize common typos
  df$Indicator <- gsub("Accomodation", "Accommodation", df$Indicator, ignore.case = TRUE)
  df
}


# 
# # -------- read your FOUR input files in the order you want --------
t_nat   <- tex_tab_to_df("Main Figures and Tables/Labor_Impacts_National.tex")   # National impacts
t_sect  <- tex_tab_to_df("Main Figures and Tables/sector_impacts.tex")           # By sector
t_tour  <- tex_tab_to_df("Main Figures and Tables/tourism_sector_impacts.tex")   # Tourism-specific
t_wage  <- tex_tab_to_df("Main Figures and Tables/wage_impacts.tex")             # Wages

# -------- combine & write --------
make_combined_latex_4 <- function(t1, t2, t3, t4, outfile = "Main Figures and Tables/combined_table.tex") {
  hdr <- "Indicator & Relative Impact (\\%) & Average Monthly Impact & Monthly Avg. Predicted & Monthly Avg. Observed & YoY Actual Change (\\%) & p-value & RMSE (priors)\\\\"
  to_rows <- function(df) apply(df, 1, function(r) paste(paste(r, collapse = " & "), "\\\\", sep = " "))

  lines <- c(
    "\\begin{table}",
    "%\\caption{Foregone Labor Growth}",
    "\\resizebox{\\textwidth}{!}{%",
    "\\centering",
    "\\begin{tabular}[t]{l|l|r|r|r|r|r|r}",
    "\\midrule",
    hdr,
    "\\midrule",
    "\\multicolumn{8}{c}{National Level Indicators} \\\\",
    "\\midrule",
    to_rows(t1),
    "\\midrule",
    "\\multicolumn{8}{c}{Employed Persons By Job Sector} \\\\",
    "\\midrule",
    to_rows(t2),
    "\\midrule",
    "\\multicolumn{8}{c}{Tourism-Specific Indicators} \\\\",
    "\\midrule",
    to_rows(t3),
    "\\midrule",
    "\\midrule",
    "\\multicolumn{8}{c}{Taxable Wages By Job Sector, ISK (M)} \\\\",
    "\\midrule",
    to_rows(t4),
    "\\midrule",
    "\\end{tabular}",
    "}",
    "\\label{tab:labor_impact}",
    "\\begin{minipage}{\\textwidth}",
    "\\tiny",
    "Notes: The number of stars denotes the significance level of the coefficient estimates based on p-values, where *** indicates $p<0.01$, ** indicates $p<0.05$, and * indicates $p<0.10$.",
    "\\end{minipage}",
    "\\end{table}"
  )

  writeLines(lines, outfile)
  message("Wrote: ", normalizePath(outfile))
}

# write it
make_combined_latex_4(t_nat, t_sect, t_tour, t_wage, outfile = "combined_table.tex")


#


#V2: Comprehensive Table For Labor Impacts: labor_impact_2

#Unemployed Persons u1_table
#Unemployment Rate u2_table

#Tourism Employees e2_table
#Tourism Net Airline net_employees_table
#Transportation e3_table
#Accommodation e4_table
#Air Transport e6_table

#Economy Wide Taxable Wages w1_table
#Tourism Wages  w2_table
#Transportation Wages w3_table
#Accommodation Wages w4_table

#{r Comprehensive Labor Impact Table}
variable_choice <- rbind(u1_table, u2_table, temp1_table,
                         e2_table, net_employees_table, e3_table, e4_table, e6_table,e8_table, e10_table,
                         w1_table, w2_table, w3_table, w4_table)

variable_choice$Indicator <- rownames(variable_choice)
variable_choice <- variable_choice %>% select(Indicator, everything())

kable(variable_choice, caption = "Comprehensive Labor Impact", format = "markdown", row.names = FALSE)
writeLines(kable(variable_choice, caption = "Comprehensive Labor Impact", format = "markdown", row.names = FALSE), "Main Figures and Tables/comprehensive_labor_impact_table.md")


# add stars to the "Relative Impact (%)" column (keep numeric shown to 2 d.p.)
vc_out <- variable_choice %>%
  mutate(`Relative Impact (%)` =
           sprintf("%.2f%s", `Relative Impact (%)`, pstars(`p-value`)))


# --- helpers ---------------------------------------------------------------
latex_escape <- function(x) {
  x %>%
    str_replace_all("&", "\\\\&")
}

# Fix common typos/casing for Indicators
fix_indicator <- function(x) {
  x %>%
    str_replace("^Accommodation", "Accommodation") %>%
    str_replace("^Accommodation", "Accommodation") %>%
    str_replace("Accommodation Employees", "Accommodation Employees") %>%
    str_replace("Accommodation Wages", "Accommodation Wages")
}

# --- shape your vc_out -----------------------------------------------------
df <- vc_out %>%
  rename(
    `Average Monthly Impact`   = starts_with("Average Monthly Impact"),
    `Monthly Avg. Predicted`   = `Monthly Avg. Predicted`,
    `Monthly Avg. Observed`    = `Monthly Avg. Observed`,
    `YoY Actual Change (%)`    = `YoY Actual Change (%)`,
    `p-value`                  = `p-value`,
    `RMSE (priors)`            = `RMSE (priors)`
  ) %>%
  mutate(
    Indicator = fix_indicator(Indicator),
    Indicator = latex_escape(Indicator),
    `YoY Actual Change (%)` = round(`YoY Actual Change (%)`,2)
  ) %>%
  select(
    Indicator, `Relative Impact (%)`, `Average Monthly Impact`,
    `Monthly Avg. Predicted`, `Monthly Avg. Observed`,
    `YoY Actual Change (%)`, `p-value`, `RMSE (priors)`
  )

# --- desired order for each section (will keep only those that exist) ------
nat_ord <- c("Unemployed Persons", "Unemployment Rate")

sector_emp_ord <- c(
  "Tourism Employees",
  "Tourism Employees Net Airline",
  "Transportation Employees",
  "Accommodation Employees"
)

tourism_spec_ord <- c(
  "Air Transport Employees",
  "Accommodation Employees",
  "Food \\& Beverage Employees",
  "Tour Services Employees",
  "Other Tourism Employees"
)

wage_ord <- c(
  "Taxable Wages (Full Economy)",
  "Tourism Wages",
  "Transportation Wages",
  "Accommodation Wages"
  # in case you want to show it here too
)

# helper to pick rows in a given order if present
pick_in_order <- function(d, wanted) {
  present <- wanted[wanted %in% d$Indicator]
  d %>% filter(Indicator %in% present) %>%
    mutate(.ord = match(Indicator, present)) %>%
    arrange(.ord) %>% select(-.ord)
}

t_nat  <- pick_in_order(df, nat_ord)
t_sect <- pick_in_order(df, sector_emp_ord)
t_tour <- pick_in_order(df, tourism_spec_ord)
t_wage <- pick_in_order(df, wage_ord)

# --- render to LaTeX exactly like your target ------------------------------
row_to_tex <- function(r) paste0(paste(r, collapse = " & "), " \\\\")

make_block <- function(title, d) {
  if (nrow(d) == 0) return(character())
  c(
    "\\midrule",
    paste0("\\multicolumn{8}{c}{", title, "} \\\\"),
    "\\midrule",
    apply(d, 1, row_to_tex)
  )
}

header_line <- "Indicator & Relative Impact (\\%) & Average Monthly Impact & Monthly Avg. Predicted & Monthly Avg. Observed & YoY Actual Change (\\%) & p-value & RMSE (priors)\\\\"

lines <- c(
  "\\begin{table}",
  "%\\caption{Foregone Labor Growth}",
  "\\resizebox{\\textwidth}{!}{%",
  "\\centering",
  "\\begin{tabular}[t]{l|l|r|r|r|r|r|r}",
  "\\midrule",
  header_line,
  make_block("National Level Indicators", t_nat),
  make_block("Employed Persons By Job Sector", t_sect),
  make_block("Tourism-Specific Indicators", t_tour),
  "\\midrule",
  "\\midrule",
  paste0("\\multicolumn{8}{c}{Taxable Wages By Job Sector, ISK (M)} \\\\"),
  "\\midrule",
  if (nrow(t_wage) > 0) apply(t_wage, 1, row_to_tex) else character(),
  "\\midrule",
  "\\end{tabular}",
  "}",
  "\\label{tab:labor_impact}",
  "\\begin{minipage}{\\textwidth}",
  "\\tiny",
  "Notes: The number of stars denotes the significance level of the coefficient estimates based on p-values, where *** indicates $p<0.01$, ** indicates $p<0.05$, and * indicates $p<0.10$.",
  "\\end{minipage}",
  "\\end{table}"
)

writeLines(lines, "Main Figures and Tables/comprehensive_labor_impact_table.tex")

#
