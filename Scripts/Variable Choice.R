#  Arrivals_All_Indicators
complete <- month
complete2 <- as.data.frame(complete[14]) # change this to pick out your indicator,
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

coefficients_1 <- table_func(impact_temp, "All Indicators")

# Arrivals_Passenger_List
complete <- month[,c(passenger_list)]
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

coefficients_2 <- table_func(impact_temp, "Passenger Indicators")

# Arrivals_Tourism_List
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

coefficients_3 <- table_func(impact_temp, "Tourism Indicators")

# Arrivals_Labor_List
complete <- month[,c(labor_list)]
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

coefficients_4 <- table_func(impact_temp, "Labor Indicators")

# Arrivals_Other_List
complete <- month[,c(other_list)]
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

coefficients_5 <- table_func(impact_temp, "Other Indicators")

# Variable Choice Table
variable_choice <- rbind(coefficients_1, coefficients_2, coefficients_3, coefficients_4, coefficients_5)

temp <- as.data.frame(t(colMeans(variable_choice[, , drop = FALSE])))
rownames(temp) <- c("Mean")

# Calculate the mean of each column
variable_choice <- rbind(variable_choice,temp)


variable_choice$Indicator <- rownames(variable_choice)
variable_choice <- variable_choice %>% select(Indicator, everything())



# round columns 2 to 2 decimlal places and 6 to 4 decimals. round columns 3 to 5 and 7 to 0 decimal places
variable_choice <-  variable_choice %>%
  mutate(across(2, ~ round(.x, 2))) %>%
  mutate(across(c(3:5, 7), ~ round(.x, 0))) %>%
  mutate(across(6, ~ round(.x, 4)))

variable_choice <-  variable_choice %>%
  mutate(`Relative Impact (%)` =
           sprintf("%.2f%s", `Relative Impact (%)`, pstars(`p-value`)))




writeLines(kable(variable_choice, caption = "Forecasted Change in Tourism From Different Coefficient Groups", format = "latex", row.names = FALSE), "Main Figures and Tables/choice_of_variables.tex")


