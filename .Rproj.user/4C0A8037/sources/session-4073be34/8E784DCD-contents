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
writeLines(kable(passenger_tables, caption = "Forecasted Change in Departures by Nationality", format = "latex", row.names = FALSE), "Main Figures and Tables/passenger_nationality.tex")



