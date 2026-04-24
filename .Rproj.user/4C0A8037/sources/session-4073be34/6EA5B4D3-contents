
#Placebo Check: Fishing

#{r Fishing Placebo Check All Indicators}
complete <- month # Restart the formula
complete2 <- as.data.frame(complete[46]) # change this to pick out your indicator,
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

pre.period <- as.yearmon(c("2014-01-01", "2019-03-01"))

post.period <- as.yearmon(c("2019-04-01", "2019-12-31"))

impact_temp <- CausalImpact(temp,pre.period,post.period, model.args=list(niter=5000, nseasons = 12, season.duration = 1))

fishing_impact_all <- table_func(impact_temp, "All Indicators")

coeff_plot(impact_temp)

#

#{r Fishing Placebo Check Passengers}
complete <- month[,c(passenger_list)] # Restart the formula
complete2 <- as.data.frame(complete[32]) # change this to pick out your indicator,
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

pre.period <- as.yearmon(c("2014-01-01", "2019-03-01"))

post.period <- as.yearmon(c("2019-04-01", "2019-12-31"))

impact_temp <- CausalImpact(temp,pre.period,post.period, model.args=list(niter=5000, nseasons = 12, season.duration = 1))

fishing_impact_pass <- table_func(impact_temp, "Passengers")

#

#{r Fishing Placebo Check Tourism}
complete <- month[,c(tourism_list)] # Restart the formula
complete2 <- as.data.frame(complete[32]) # change this to pick out your indicator,
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

pre.period <- as.yearmon(c("2014-01-01", "2019-03-01"))

post.period <- as.yearmon(c("2019-04-01", "2019-12-31"))

impact_temp <- CausalImpact(temp,pre.period,post.period, model.args=list(niter=5000, nseasons = 12, season.duration = 1))

fishing_impact_tour <- table_func(impact_temp, "Tourism")

#

#{r Fishing Placebo Check Labor}
complete <- month[,c(labor_list)] # Restart the formula
complete2 <- as.data.frame(complete[32]) # change this to pick out your indicator,
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

pre.period <- as.yearmon(c("2014-01-01", "2019-03-01"))

post.period <- as.yearmon(c("2019-04-01", "2019-12-31"))

impact_temp <- CausalImpact(temp,pre.period,post.period, model.args=list(niter=5000, nseasons = 12, season.duration = 1))

fishing_impact_labor <- table_func(impact_temp, "Labor")

#

#{r Fishing By Fishing}
complete <- month[,c(other_list)] # Restart the formula
complete2 <- as.data.frame(complete[32]) # change this to pick out your indicator,
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

pre.period <- as.yearmon(c("2014-01-01", "2019-03-01"))

post.period <- as.yearmon(c("2019-04-01", "2019-12-31"))

impact_temp <- CausalImpact(temp,pre.period,post.period, model.args=list(niter=5000, nseasons = 12, season.duration = 1))

fishing_impact_other <- table_func(impact_temp, "Fishing Impact Predicted by Other Indicators")
#

#{r Fishing Placebo Table}
variable_choice <- rbind(fishing_impact_all, fishing_impact_pass, fishing_impact_tour, fishing_impact_labor, fishing_impact_other)

temp <- as.data.frame(t(colMeans(variable_choice[, , drop = FALSE])))
rownames(temp) <- c("Mean")

# Calculate the mean of each column
variable_choice <- rbind(variable_choice,temp)

variable_choice$Indicator <- rownames(variable_choice)
variable_choice <- variable_choice %>% select(Indicator, everything())

kable(variable_choice, caption = "Forecasted Impacts of Closure on Fish Landing Weight by Different Indicators", format = "markdown", row.names = FALSE)
writeLines(kable(variable_choice, caption = "Forecasted Impacts of Closure on Fish Landing Weight by Different Indicators", format = "markdown", row.names = FALSE), "Additional Tables and Figures/choice_of_variables.md")
#

