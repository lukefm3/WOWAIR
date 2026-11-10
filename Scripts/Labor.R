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
w1_table_v2 <- table_func_avg_yoy_v2(impact_temp, "Taxable Wages (Full Economy)")

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
u1 <- plot(impact_temp, "pointwise") + ggtitle("Unemployed Persons") +   panel_theme + ylab("Persons")

u1_table <- table_func_avg_yoy(impact_temp, "Unemployed Persons")
u1_table_v2 <- table_func_avg_yoy_v2(impact_temp, "Unemployed Persons")

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
u2_table_v2 <- table_func_avg_yoy_v2(impact_temp, "Unemployment Rate")

#

#{r Total Employed Net of Air Economy Wide}
complete <- month %>%
  mutate(total_employees_net_air_iceland = registered_employed_persons_iceland - air_employees_iceland) %>%
  select(any_of(labor_list), total_employees_net_air_iceland)

complete2 <- complete %>% select(total_employees_net_air_iceland)
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


employee_impact <- (u1 | u2) +
  plot_layout(widths = c(1, 1)) &
  theme(
    plot.margin = margin(5, 5, 5, 5),
    aspect.ratio = 0.65
  )

ggsave(
  "Main Figures and Tables/national_labor_plot.png",
  employee_impact,
  width = 10,
  height = 3.5,
  units = "in",
  dpi = 500
)


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
w2_table_v2 <- table_func_avg_yoy_v2(impact_temp, "Tourism Wages")
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
e2_table_v2 <- table_func_avg_yoy_v2(impact_temp, "Tourism Employees")

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
e3_table_v2 <- table_func_avg_yoy_v2(impact_temp, "Transportation Employees")

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
w3_table_v2 <- table_func_avg_yoy_v2(impact_temp, "Transportation Wages")

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
temp1_table_v2 <- table_func_avg_yoy_v2(impact_temp, "Total Non-Tourism Employees")

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
w4_table_v2 <- table_func_avg_yoy_v2(impact_temp, "Accommodation Wages")

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
e4_table_v2 <- table_func_avg_yoy_v2(impact_temp, "Accommodation Employees")

#

#{r Tourism Minus Air Employees}
complete <- month %>%
  mutate(net_tourism_employees_iceland = employees_tourism_iceland - air_employees_iceland) %>%
  select(any_of(labor_list), net_tourism_employees_iceland)

complete2 <- complete %>% select(net_tourism_employees_iceland)
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


net_employees <- plot(impact_temp, "pointwise") + ggtitle("Tourism Employees, Net of Air Transport") + panel_theme + ylab("Persons")

net_employees_table <- table_func_avg_yoy(impact_temp, "Tourism Employees, Net of Air Transport")
net_employees_table_v2 <- table_func_avg_yoy_v2(impact_temp, "Tourism Employees, Net of Air Transport")


#

#{r Transportation Minus Air Employees}
complete <- month %>%
  mutate(transportation_net_air_employees_iceland = employees__transport_iceland - air_employees_iceland) %>%
  select(any_of(labor_list), transportation_net_air_employees_iceland)

complete2 <- complete %>% select(transportation_net_air_employees_iceland)
colnames(complete2) <- "transportation_employees_iceland"
# list of remaining columns
icelandcolumns <- grep('iceland', colnames(complete))
complete <- complete[,-c(icelandcolumns)]

complete <- cbind(complete,complete2)

complete <- complete %>% filter(date > '2013-12-31') %>% filter(date < '2019-12-31')

complete <- complete[ , colSums(is.na(complete))==0] %>% relocate(transportation_employees_iceland) %>% relocate(date)

temp <- ts(data = complete[-c(1)],
           start = 2014,
           end = 2020,
           frequency = 12) %>% head(-1)
pre.period <- as.yearmon(c("2014-01-01", "2019-01-01"))
post.period <- as.yearmon(c("2019-04-01", "2020-01-01"))

impact_temp <- CausalImpact(temp,pre.period,post.period, model.args=list(niter=5000, nseasons = 12, season.duration = 1))

transport_net_air_employees <- plot(impact_temp, "pointwise") + ggtitle("Transportation Employees, Net of Air Transport") + panel_theme + ylab("Persons")

transport_net_air_employees_table <- table_func_avg_yoy(impact_temp, "Transportation Employees, Net of Air Transport")
transport_net_air_employees_table_v2 <- table_func_avg_yoy_v2(impact_temp, "Transportation Employees, Net of Air Transport")


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

variable_choice <- rbind(e2_table,net_employees_table,e3_table,transport_net_air_employees_table, e4_table)

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
e6_table_v2 <- table_func_avg_yoy_v2(impact_temp, "Air Transport Employees")
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
e8_table_v2 <- table_func_avg_yoy_v2(impact_temp, "Food & Beverage Employees")
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
e10_table_v2 <- table_func_avg_yoy_v2(impact_temp, "Other Tourism Employees")


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
                         e2_table, net_employees_table, e3_table, transport_net_air_employees_table, e4_table, e6_table,e8_table, e10_table,
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
    str_replace("Taxable Wages \\(Full Economy\\)", "Taxable Wages, Full Economy") %>%
    str_replace("Tourism Employees Excl\\. Airline", "Tourism Employees, Net of Air Transport") %>%
    str_replace("Accommodation Employees", "Accommodation Employees") %>%
    str_replace("Accommodation Wages", "Accommodation Wages")
}

format_whole <- function(x) {
  formatC(round(as.numeric(x)), format = "d", big.mark = ",")
}

format_decimal <- function(x, digits = 2) {
  formatC(as.numeric(x), format = "f", digits = digits, big.mark = ",")
}

format_labor_quantity <- function(x, indicator) {
  ifelse(indicator == "Unemployment Rate", format_decimal(x, 2), format_whole(x))
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
    `Average Monthly Impact` = format_labor_quantity(`Average Monthly Impact`, Indicator),
    `Monthly Avg. Predicted` = format_labor_quantity(`Monthly Avg. Predicted`, Indicator),
    `Monthly Avg. Observed` = format_labor_quantity(`Monthly Avg. Observed`, Indicator),
    `YoY Actual Change (%)` = format_decimal(`YoY Actual Change (%)`, 2),
    `p-value` = formatC(as.numeric(`p-value`), format = "f", digits = 4),
    `RMSE (priors)` = format_labor_quantity(`RMSE (priors)`, Indicator),
    Indicator = latex_escape(Indicator)
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
  "Tourism Employees, Net of Air Transport",
  "Transportation Employees",
  "Transportation Employees, Net of Air Transport",
  "Accommodation Employees",
  "Air Transport Employees",
  "Food \\& Beverage Employees",
  "Other Tourism Employees"
)

tourism_spec_ord <- character()

wage_ord <- c(
  "Taxable Wages, Full Economy",
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
    paste0("\\multicolumn{8}{l}{\\textbf{", title, "}}\\\\"),
    "\\midrule",
    apply(d, 1, row_to_tex)
  )
}

header_line <- "Outcome & Relative Impact (\\%) & Avg. Monthly Impact & Monthly Avg. Predicted & Monthly Avg. Observed & YoY Actual Change (\\%) & p-value & RMSE (pre-period)\\\\"

lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Labor-Market and Wage Effects of the WOW Air Bankruptcy}\\label{tab:labor_impact}",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}[t]{lrrrrrrr}",
  "\\toprule",
  header_line,
  make_block("National labor indicators", t_nat),
  make_block("Employment by sector", t_sect),
  "\\midrule",
  paste0("\\multicolumn{8}{l}{\\textbf{Taxable wages by sector, ISK millions}}\\\\"),
  "\\midrule",
  if (nrow(t_wage) > 0) apply(t_wage, 1, row_to_tex) else character(),
  "\\bottomrule",
  "\\end{tabular}",
  "}",
  "\\begin{minipage}{\\textwidth}",
  "\\raggedright\\scriptsize \\textit{Note:} Average monthly impact is measured in workers for employment and unemployment, percentage points for the unemployment rate, and ISK millions for wages. Stars denote statistical significance: *** $p<0.01$, ** $p<0.05$, * $p<0.10$.",
  "\\end{minipage}",
  "\\end{table}"
)

writeLines(lines, "Main Figures and Tables/comprehensive_labor_impact_table.tex")

# Manuscript labor table and nine-month relative-impact CI figure.
# Compact manuscript version with nine-month cumulative relative-impact CIs.
# The original table objects and formatter above remain available elsewhere.
labor_v2 <- rbind(
  u1_table_v2, u2_table_v2, e2_table_v2, net_employees_table_v2,
  e3_table_v2, transport_net_air_employees_table_v2, e4_table_v2,
  e6_table_v2, e8_table_v2, e10_table_v2,
  w1_table_v2, w2_table_v2, w3_table_v2, w4_table_v2
)
labor_v2$Outcome <- rownames(labor_v2)
labor_v2$Outcome <- fix_indicator(labor_v2$Outcome)
# Existing R sessions may hold v2 rows created before the monthly columns were
# restored. Fill those fields from the already fitted legacy rows, with no refit.
monthly_columns <- c("Average Monthly Impact", "Monthly Avg. Predicted",
                     "Monthly Avg. Observed", "RMSE (priors)")
if (!all(monthly_columns %in% names(labor_v2))) {
  labor_old <- rbind(
    u1_table, u2_table, e2_table, net_employees_table, e3_table,
    transport_net_air_employees_table, e4_table, e6_table, e8_table,
    e10_table, w1_table, w2_table, w3_table, w4_table
  )
  old_outcomes <- fix_indicator(rownames(labor_old))
  old_rows <- match(labor_v2$Outcome, old_outcomes)
  if (anyNA(old_rows)) stop("Could not align the saved monthly labor results.")
  monthly_source <- grep("^Average Monthly Impact", names(labor_old), value = TRUE)[1]
  labor_v2[["Average Monthly Impact"]] <- labor_old[[monthly_source]][old_rows]
  for (column in monthly_columns[-1]) {
    labor_v2[[column]] <- labor_old[[column]][old_rows]
  }
}
# The legacy LaTeX table order escapes "&"; the model labels are still plain text.
labor_order <- c(nat_ord, gsub("\\", "", sector_emp_ord, fixed = TRUE), wage_ord)
missing_labor_outcomes <- setdiff(labor_order, labor_v2$Outcome)
if (length(missing_labor_outcomes)) {
  stop("Missing v2 labor outcomes: ", paste(missing_labor_outcomes, collapse = ", "))
}
labor_v2 <- labor_v2[match(labor_order, labor_v2$Outcome), , drop = FALSE]

# Produce a standalone interval figure alongside this table's generation code.
labor_plot <- labor_v2
labor_plot$Outcome <- factor(labor_plot$Outcome, levels = rev(labor_plot$Outcome))
labor_plot$Panel <- factor(
  rep(c("National labor indicators", "Employment by sector", "Taxable wages by sector"),
      times = c(length(nat_ord), length(sector_emp_ord), length(wage_ord))),
  levels = c("National labor indicators", "Employment by sector", "Taxable wages by sector")
)
labor_panel_dividers <- data.frame(
  Panel = factor(
    c("National labor indicators", "Employment by sector"),
    levels = levels(labor_plot$Panel)
  ),
  yintercept = 0.25
)
labor_interval_plot <- ggplot2::ggplot(
  labor_plot, ggplot2::aes(x = `Relative Impact (%)`, y = Outcome)
) +
  ggplot2::geom_vline(xintercept = 0, color = "grey60", linewidth = 0.4) +
  ggplot2::geom_segment(
    ggplot2::aes(x = `95% CI Lower (%)`, xend = `95% CI Upper (%)`, yend = Outcome),
    linewidth = 0.8, color = "#326987"
  ) +
  ggplot2::geom_point(size = 2.5, color = "#B23A48") +
  ggplot2::geom_hline(
    data = labor_panel_dividers,
    ggplot2::aes(yintercept = yintercept),
    inherit.aes = FALSE, color = "grey85", linewidth = 0.4
  ) +
  ggplot2::facet_grid(Panel ~ ., scales = "free_y", space = "free_y") +
  ggplot2::coord_cartesian(clip = "off") +
  ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%")) +
  ggplot2::labs(
    x = "Cumulative relative impact (%)", y = NULL
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    panel.grid.major.y = ggplot2::element_blank(),
    panel.spacing.y = grid::unit(1.2, "lines"),
    strip.text.y = ggplot2::element_text(angle = 0, face = "bold"),
    plot.title = ggplot2::element_text(face = "bold"),
    plot.margin = ggplot2::margin(12, 18, 12, 12)
  )
ggplot2::ggsave(
  "Main Figures and Tables/labor_relative_impact_95ci.png",
  labor_interval_plot, width = 10, height = 8, units = "in", dpi = 300
)

format_labor_percent <- function(x) formatC(x, format = "f", digits = 2)
format_labor_p <- function(x) formatC(x, format = "f", digits = 4)
format_labor_quantity_v2 <- function(x, outcome) {
  if (grepl("Rate", outcome)) formatC(as.numeric(x), format = "f", digits = 2) else
    formatC(round(as.numeric(x)), format = "d", big.mark = ",")
}
labor_row_v2 <- function(row) {
  paste0(
    latex_escape(row[["Outcome"]]), " & ",
    format_labor_percent(as.numeric(row[["Relative Impact (%)"]])),
    pstars(as.numeric(row[["p-value"]])), "\\hspace{0.6em}[",
    formatC(as.numeric(row[["95% CI Lower (%)"]]), format = "f", digits = 1), ", ",
    formatC(as.numeric(row[["95% CI Upper (%)"]]), format = "f", digits = 1), "] & ",
    format_labor_percent(as.numeric(row[["YoY Actual Change (%)"]])), " & ",
    format_labor_quantity_v2(row[["Average Monthly Impact"]], row[["Outcome"]]), " & ",
    format_labor_quantity_v2(row[["Monthly Avg. Predicted"]], row[["Outcome"]]), " & ",
    format_labor_quantity_v2(row[["Monthly Avg. Observed"]], row[["Outcome"]]), " & ",
    format_labor_p(as.numeric(row[["p-value"]])), " & ",
    format_labor_quantity_v2(row[["RMSE (priors)"]], row[["Outcome"]]), " \\\\"
  )
}
labor_block_v2 <- function(title, rows) {
  c("\\midrule", paste0("\\multicolumn{8}{l}{\\textbf{", title, "}}\\\\"),
    apply(rows, 1, labor_row_v2))
}
labor_lines_v2 <- c(
  "\\begin{table}[htbp]", "\\centering",
  "\\caption{Labor-Market and Wage Effects of the WOW Air Bankruptcy}\\label{tab:labor_impact}",
  "\\resizebox{\\textwidth}{!}{%", "\\begin{tabular}[t]{llrrrrrr}", "\\toprule",
  "Outcome & \\shortstack[l]{9-Month Relative Impact (\\%)\\\\and 95\\% CI} & YoY Actual Change (\\%) & Avg. Monthly Impact & Monthly Avg. Predicted & Monthly Avg. Observed & p-value & RMSE (pre-period)\\\\",
  labor_block_v2("National labor indicators", labor_v2[seq_along(nat_ord), , drop = FALSE]),
  labor_block_v2("Employment by sector", labor_v2[length(nat_ord) + seq_along(sector_emp_ord), , drop = FALSE]),
  labor_block_v2("Taxable wages by sector", labor_v2[length(nat_ord) + length(sector_emp_ord) + seq_along(wage_ord), , drop = FALSE]),
  "\\bottomrule", "\\end{tabular}", "}",
  "\\begin{minipage}{\\textwidth}",
  "\\raggedright\\scriptsize \\textit{Note:} Relative impact and its 95\\% interval compare cumulative observed and counterfactual outcomes over April--December 2019. YoY actual change compares the observed nine-month average with the same months of 2018. Monthly quantities and RMSE are measured in workers for employment and unemployment, percentage points for the unemployment rate, and ISK millions for wages. Stars denote statistical significance: *** $p<0.01$, ** $p<0.05$, * $p<0.10$.",
  "\\end{minipage}", "\\end{table}"
)
writeLines(labor_lines_v2, "Main Figures and Tables/comprehensive_labor_impact_table.tex")

#
